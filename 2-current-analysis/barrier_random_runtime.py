"""Shared randomized barrier runtime for zero-shot, context and anchor experiments.

Fisher-Yates uses runtime system randomness. A pending prompt/order survives
failures and restarts. All model variants share it and use model seed 42.
Matching is automatic, with no predefined aliases; first five unique valid
labels are retained. The canonical IDs never depend on display positions.
"""
import argparse
import csv
import datetime
import fcntl
import hashlib
import io
import json
import random
import re
import unicodedata
import uuid
from collections import defaultdict
from difflib import SequenceMatcher
from pathlib import Path
import requests

MATCH_THRESHOLD = 0.85
MIN_MATCH_MARGIN = 0.05

def configure(output_folder, name_prefix, barriers, model_variants, prompt_builder,
              condition, anchor_type="", anchor_level="", model_seed=42):
    """Configure one condition/anchor arm; no model calls or output writes."""
    global OUTPUT_FOLDER, RAW_LOG_DIR, ORDER_LOG_DIR, OUTPUT_CSV, COUNTER_FILE
    global PENDING_FILE, LOCK_FILE, MODEL_VARIANTS, BARRIERS, PROMPT_BUILDER
    global EXPERIMENT_META, SEED, LOG_PREFIX
    OUTPUT_FOLDER = Path(output_folder)
    LOG_PREFIX = name_prefix + "BarrierSelect"
    RAW_LOG_DIR = OUTPUT_FOLDER / (LOG_PREFIX + "RawLogs")
    ORDER_LOG_DIR = OUTPUT_FOLDER / (LOG_PREFIX + "OrderLogs")
    OUTPUT_CSV = OUTPUT_FOLDER / (LOG_PREFIX + "Responses.csv")
    arm_token = "".join(word.capitalize() for word in re.findall(r"[A-Za-z0-9]+", anchor_type))
    COUNTER_FILE = RAW_LOG_DIR / f"{LOG_PREFIX}{arm_token}Counter.txt"
    PENDING_FILE = RAW_LOG_DIR / f"{LOG_PREFIX}{arm_token}PendingOrder.json"
    # All arms share one CSV, so they also share this process lock.
    LOCK_FILE = OUTPUT_FOLDER / ("." + LOG_PREFIX + ".lock")
    BARRIERS = dict(barriers)
    MODEL_VARIANTS = list(model_variants)
    if len(BARRIERS) != 11 or set(BARRIERS) != set(range(1, 12)):
        raise ValueError("Expected the canonical 11-barrier catalog")
    if len(set(MODEL_VARIANTS)) != 12 or len(MODEL_VARIANTS) != 12:
        raise ValueError("Expected 12 distinct model variants")
    if len({normalize_barrier_label(label) for label in BARRIERS.values()}) != 11:
        raise ValueError("Canonical labels are ambiguous after normalization")
    PROMPT_BUILDER = prompt_builder
    EXPERIMENT_META = {
        "condition": condition, "anchor_type": anchor_type,
        "anchor_level": anchor_level,
    }
    SEED = model_seed


def question(order):
    return PROMPT_BUILDER(order)


FIELDS = [
    "condition", "anchor_type", "anchor_level",
    "row_id", "base_model", "variant_id", "model", "barrier_id",
    "displayed_position", "official_label", "model_label", "label_status",
    "is_hallucinated", "iteration", "timestamp", "attempt_id", "model_seed",
    "match_method", "match_similarity", "match_margin",
]

def fisher_yates(values):
    """Shuffle a copy at prompt construction, using unseeded system randomness."""
    items = list(values)
    rng = random.SystemRandom()
    for index in range(len(items) - 1, 0, -1):
        replacement_index = rng.randrange(index + 1)
        items[index], items[replacement_index] = items[replacement_index], items[index]
    return items

def normalize_barrier_label(label):
    normalized = unicodedata.normalize("NFKC", label).casefold()
    return " ".join(
        "".join(char if char.isalnum() else " " for char in normalized).split()
    )

def match_barrier_label(label):
    normalized = normalize_barrier_label(label)
    scores = sorted(
        ((SequenceMatcher(None, normalized, normalize_barrier_label(official)).ratio(), key)
         for key, official in BARRIERS.items()),
        reverse=True,
    )
    best_score, best_id = scores[0]
    second_score = scores[1][0] if len(scores) > 1 else 0.0
    gap = best_score - second_score
    exact_id = next(
        (key for key, official in BARRIERS.items()
         if normalized == normalize_barrier_label(official)), None
    )
    if exact_id is not None:
        matched_id, method = exact_id, "exact_normalized"
    elif best_score >= MATCH_THRESHOLD and gap >= MIN_MATCH_MARGIN:
        matched_id, method = best_id, "similarity"
    else:
        matched_id, method = None, "unmatched"
    matched_score = (
        SequenceMatcher(None, normalized, normalize_barrier_label(BARRIERS[matched_id])).ratio()
        if matched_id is not None else best_score
    )
    return {
        "barrier_id": matched_id,
        "official_label": BARRIERS[matched_id] if matched_id is not None else "",
        "model_label": label,
        "label_status": "matched" if matched_id is not None else "unmatched",
        "match_method": method,
        "match_similarity": matched_score,
        "match_margin": gap,
        "suggested_barrier_id": best_id,
        "suggested_label": BARRIERS[best_id],
    }

def extract_barrier_info(response_text):
    """Parse all entries by label; retain raw spelling and matching diagnostics."""
    entries = []
    for line_number, line in enumerate(response_text.splitlines(), 1):
        if not line.strip():
            continue
        label = re.sub(
            r"^\s*(?:[-*•]\s+|\d{1,2}\s*[.)\-:–—]\s*)", "", line
        ).strip()
        entry = match_barrier_label(label)
        entry["response_line"] = line_number
        entries.append(entry)
    return entries

def first_five_valid(entries):
    """Keep response order; skip unmatched/duplicate entries and audit all extras."""
    selected, seen = [], set()
    for entry in entries:
        barrier_id = entry["barrier_id"]
        if entry["label_status"] != "matched":
            entry["selection_status"] = "unmatched"
        elif barrier_id in seen:
            entry["selection_status"] = "duplicate"
        else:
            seen.add(barrier_id)
            if len(selected) < 5:
                entry["selection_status"] = "selected"
                selected.append(entry)
            else:
                entry["selection_status"] = "extra_valid"
    return selected

def valid_selection(entries):
    return (
        len(entries) == 5
        and all(entry["label_status"] == "matched" for entry in entries)
        and len({entry["barrier_id"] for entry in entries}) == 5
    )

def query_model(prompt, model_name):
    try:
        with requests.post(
            "http://localhost:11434/api/generate",
            json={"model": model_name, "prompt": prompt,
                  "stream": True, "options": {"seed": SEED}},
            stream=True, timeout=120,
        ) as response:
            response.raise_for_status()
            parts = []
            done = False
            for line in response.iter_lines():
                if not line:
                    continue
                event = json.loads(line.decode("utf-8"))
                if event.get("error"):
                    raise ValueError(event["error"])
                parts.append(event.get("response", ""))
                done = done or event.get("done", False)
            if not done:
                raise ValueError("Ollama stream ended without a completion event")
            return "".join(parts)
    except (requests.RequestException, ValueError) as exc:
        print(f"Failed to query {model_name}: {exc}", flush=True)
        return ""

def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()

def atomic_write(path, text):
    temporary = path.with_name(f".{path.name}.{uuid.uuid4().hex}.tmp")
    try:
        temporary.write_text(text, encoding="utf-8")
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)

def write_json(path, value):
    atomic_write(path, json.dumps(value, indent=2, ensure_ascii=False) + "\n")

def read_results():
    """Validate committed iterations and recover a stale counter after a restart."""
    rows = []
    if OUTPUT_CSV.exists():
        with OUTPUT_CSV.open(newline="", encoding="utf-8") as handle:
            reader = csv.DictReader(handle)
            if reader.fieldnames != FIELDS:
                raise ValueError("Unexpected CSV schema; do not mix old and randomized outputs")
            rows = list(reader)
    # One anchor CSV contains all arms. Its counters and iteration numbers are per arm.
    scoped_rows = [row for row in rows if all(
        row[key] == value for key, value in EXPERIMENT_META.items()
    )]
    groups = defaultdict(list)
    for row in scoped_rows:
        groups[int(row["iteration"])].append(row)
    if sorted(groups) != list(range(len(groups))):
        raise ValueError("Saved iterations are not contiguous from zero")
    for iteration, batch in groups.items():
        if len(batch) != len(MODEL_VARIANTS) * 5:
            raise ValueError(f"Iteration {iteration} has incomplete saved results")
        if len({row["attempt_id"] for row in batch}) != 1:
            raise ValueError(f"Iteration {iteration} mixes attempts")
        by_model = defaultdict(list)
        for row in batch:
            model_name = f"{row['base_model']}-{row['model']}"
            if (model_name not in MODEL_VARIANTS
                    or row["official_label"] != BARRIERS.get(int(row["barrier_id"]))
                    or row["model_seed"] != str(SEED)
                    or row["label_status"] != "matched"):
                raise ValueError("Invalid saved barrier selection")
            by_model[model_name].append(row)
        if set(by_model) != set(MODEL_VARIANTS) or any(
            len(batch_rows) != 5 or len({row["barrier_id"] for row in batch_rows}) != 5
            for batch_rows in by_model.values()
        ):
            raise ValueError(f"Iteration {iteration} lacks five unique selections per model")
    completed = len(groups)
    counter = int(COUNTER_FILE.read_text().strip()) if COUNTER_FILE.exists() else 0
    if counter != completed:
        if counter == completed - 1:
            print("Recovering counter after a previously committed CSV write.", flush=True)
            atomic_write(COUNTER_FILE, str(completed))
        else:
            raise ValueError("Counter and saved CSV disagree; inspect before proceeding")
    # A crash after committing CSV must not cause the completed iteration to repeat.
    if PENDING_FILE.exists():
        pending = json.loads(PENDING_FILE.read_text())
        if pending["iteration"] < completed:
            committed = groups.get(pending["iteration"], [])
            attempt_id = pending.get("last_attempt_id")
            if not committed or {row["attempt_id"] for row in committed} != {attempt_id}:
                raise ValueError("Stale pending order does not match committed results")
            log = ORDER_LOG_DIR / f"{attempt_id}.json"
            record = json.loads(log.read_text())
            record.update(status="success", recovered_after_restart=True)
            write_json(log, record)
            PENDING_FILE.unlink()
    return completed, rows

def pending_prompt(iteration):
    if PENDING_FILE.exists():
        pending = json.loads(PENDING_FILE.read_text())
    else:
        order = fisher_yates(BARRIERS)
        pending = {
            "iteration": iteration, "order": order,
            "prompt": question(order), "created_at": now(),
            "model_seed": SEED, "model_variants": MODEL_VARIANTS,
            "algorithm": "Fisher-Yates", "shuffle_source": "SystemRandom",
            "attempts": 0, "experiment": EXPERIMENT_META,
        }
        write_json(PENDING_FILE, pending)
    order = pending["order"]
    if (pending["iteration"] != iteration
            or len(order) != len(BARRIERS) or set(order) != set(BARRIERS)
            or pending["prompt"] != question(order)
            or pending["model_seed"] != SEED
            or pending["model_variants"] != MODEL_VARIANTS
            or pending.get("experiment") != EXPERIMENT_META):
        raise ValueError("Pending state conflicts with this script; inspect before proceeding")
    return pending

def run_attempt(iteration, previous_rows):
    pending = pending_prompt(iteration)
    attempt_id = uuid.uuid4().hex
    pending["attempts"] += 1
    pending["last_attempt_id"] = attempt_id
    write_json(PENDING_FILE, pending)
    timestamp = now()
    record = {
        **EXPERIMENT_META,
        "attempt_id": attempt_id, "iteration": iteration,
        "attempt_number": pending["attempts"], "timestamp": timestamp,
        "status": "started", "algorithm": pending["algorithm"],
        "shuffle_source": pending["shuffle_source"], "shuffle_seed": None,
        "model_seed": SEED, "model_variants": MODEL_VARIANTS,
        "order": [
            {"displayed_position": position, "barrier_id": barrier_id,
             "official_label": BARRIERS[barrier_id]}
            for position, barrier_id in enumerate(pending["order"], 1)
        ],
        "prompt": pending["prompt"],
        "prompt_sha256": hashlib.sha256(pending["prompt"].encode()).hexdigest(),
    }
    order_path = ORDER_LOG_DIR / f"{attempt_id}.json"
    write_json(order_path, record)  # Persist the actual order before any model request.
    positions = {barrier_id: i for i, barrier_id in enumerate(pending["order"], 1)}
    print(f"Iteration {iteration}; attempt {pending['attempts']}; order {pending['order']}", flush=True)
    raw_responses, structured, failed_models = {}, {}, []
    record["parser_policy"] = {
        "selection_rule": "first_five_unique_valid_in_response_order",
        "similarity_metric": "SequenceMatcher on normalized full labels",
        "threshold": MATCH_THRESHOLD, "minimum_margin": MIN_MATCH_MARGIN,
        "aliases": {},
        "matching_policy": "automatic_similarity_without_predefined_aliases",
    }
    record["selection_audit"] = {}
    for model_name in MODEL_VARIANTS:
        print(f"Querying expert: {model_name}", flush=True)
        raw = query_model(pending["prompt"], model_name)
        raw_responses[model_name] = raw
        entries = extract_barrier_info(raw)
        selected = first_five_valid(entries)
        structured[model_name] = selected
        record["selection_audit"][model_name] = {
            "parsed_entries": len(entries),
            "selected_count": len(selected),
            "entries": entries,
        }
        if not valid_selection(selected):
            failed_models.append(model_name)
            print(f"Rejected {model_name}: fewer than five unique matched labels.", flush=True)
    raw_path = RAW_LOG_DIR / f"{LOG_PREFIX}RandOrderIter{iteration:02d}Attempt{attempt_id}.txt"
    atomic_write(raw_path, "".join(
        f"\n--- {model_name} ---\n{response.strip()}\n"
        for model_name, response in raw_responses.items()
    ))
    record.update(raw_log=raw_path.name, finished_at=now(), failed_models=failed_models)
    if failed_models:
        record["status"] = "failed"
        failure = dict(record, raw_responses=raw_responses, structured_responses=structured)
        write_json(RAW_LOG_DIR / f"{LOG_PREFIX}FailedAttempt{attempt_id}.json", failure)
        write_json(order_path, record)
        print("Attempt failed; pending order retained for retry.", flush=True)
        return False
    rows = []
    for model_name, entries in structured.items():
        base_model, role = model_name.split("-", 1)
        for entry in entries:
            barrier_id = entry["barrier_id"]
            rows.append({
                **EXPERIMENT_META,
                "row_id": f"{attempt_id}_{model_name}_b{barrier_id}",
                "base_model": base_model, "variant_id": f"{base_model}_{role}",
                "model": role, "barrier_id": barrier_id,
                "displayed_position": positions[barrier_id],
                "official_label": entry["official_label"], "model_label": entry["model_label"],
                "label_status": "matched", "is_hallucinated": False,
                "iteration": iteration, "timestamp": timestamp,
                "attempt_id": attempt_id, "model_seed": SEED,
                "match_method": entry["match_method"],
                "match_similarity": entry["match_similarity"],
                "match_margin": entry["match_margin"],
            })
    record["status"] = "validated"
    write_json(order_path, record)
    output = io.StringIO(newline="")
    writer = csv.DictWriter(output, fieldnames=FIELDS)
    writer.writeheader()
    writer.writerows(previous_rows + rows)
    atomic_write(OUTPUT_CSV, output.getvalue())
    atomic_write(COUNTER_FILE, str(iteration + 1))
    record["status"] = "success"
    write_json(order_path, record)
    PENDING_FILE.unlink()
    print(f"Complete: {iteration + 1} successful iterations; saved {len(rows)} selections.", flush=True)
    return True

def main(until_successes=None):
    for directory in [OUTPUT_FOLDER, RAW_LOG_DIR, ORDER_LOG_DIR]:
        directory.mkdir(parents=True, exist_ok=True)
    with LOCK_FILE.open("a") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise RuntimeError("Another randomized-barrier launcher is already running") from None
        while True:
            completed, rows = read_results()
            if until_successes is not None and completed >= until_successes:
                print(f"Complete: {completed} successful iterations.", flush=True)
                return
            run_attempt(completed, rows)
            if until_successes is None:
                return
