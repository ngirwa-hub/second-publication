"""Runtime Fisher-Yates barrier order; fixed Ollama generation seed 42.

Linux/Bash: one attempt (all 12 variants):
python3 -u /home/ngirwa/TiiS/variability-barrier-list/zeroshotBarrierSelect_seed42_randOrder.py

Continue until 50 successful iterations (run in tmux, with Ollama running):
python3 -u /home/ngirwa/TiiS/variability-barrier-list/zeroshotBarrierSelect_seed42_randOrder.py --until-successes 50

The shuffle uses system randomness, independently of Ollama's seed.
A pending iteration keeps its saved order and prompt through failures/restarts.
Every retry queries all 12 variants. Ctrl+C stops the launcher without discarding
its pending order. Fixed-seed invalid answers can repeat; inspect failure logs
if the successful counter stops advancing. Never edit pending state mid-run.
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

SEED = 42  # Ollama generation only; not used to initialize the shuffle.
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_FOLDER = SCRIPT_DIR / "outputs" / "randOrder-seed42"
RAW_LOG_DIR = OUTPUT_FOLDER / "zeroshotBarrierSelectRawLogs"
ORDER_LOG_DIR = OUTPUT_FOLDER / "zeroshotBarrierSelectOrderLogs"
OUTPUT_CSV = OUTPUT_FOLDER / "zeroshotBarrierSelectResponses_seed42_randOrder.csv"
COUNTER_FILE = OUTPUT_FOLDER / "zeroshotBarrierSelectCounter.txt"
PENDING_FILE = OUTPUT_FOLDER / "zeroshotBarrierSelectPendingOrder.json"
LOCK_FILE = OUTPUT_FOLDER / ".experiment.lock"

MODEL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter"
]

BARRIERS = {
    1: "power losses, quality and safety issues",
    2: "reduced reliability in DC devices",
    3: "lack of use-cases in which DC is advantageous",
    4: "uncertain utility interaction (net metering, utility ownership, and agreed standards)",
    5: "lack of pilot projects",
    6: "public perception of DC and readiness to 'champion' installations from DC projects",
    7: "incompatibility of DC systems components",
    8: "misconception and lack of knowledge leads to lengthy/expensive design and permit process",
    9: "lack of enough trained personnel in DC systems",
    10: "uncertain regulatory roadmap",
    11: "high costs of DC solutions"
}

def context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies.\n"
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
    )

def instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and respond according to your expert role.\n"
        "You must select exactly five (5) barriers from the list provided.\n"
        "Do not select more or fewer than five.\n"
        "You must copy and use the full barrier labels exactly as listed.\n"
        "Do not paraphrase, rephrase, or modify any barrier wording.\n"
        "Only select from the list — do not add new barriers.\n"
        "The list is shown in random order and does not reflect importance.\n"
        "No justification is required. Return only five lines, each starting with '- ', "
        "followed by one selected barrier label. Do not number the selections."
    )


def fisher_yates(values):
    """Shuffle a copy at prompt construction, using unseeded system randomness."""
    items = list(values)
    rng = random.SystemRandom()
    for index in range(len(items) - 1, 0, -1):
        replacement_index = rng.randrange(index + 1)
        items[index], items[replacement_index] = items[replacement_index], items[index]
    return items


def question(order):
    barrier_list = "\n".join(f"- {BARRIERS[barrier_id]}" for barrier_id in order)
    return f"{context()}\n\n{instructions()}\n\nBarriers:\n{barrier_list}"


def normalize_barrier_label(label):
    normalized = unicodedata.normalize("NFKC", label).casefold()
    return " ".join(
        "".join(char if char.isalnum() else " " for char in normalized).split()
    )


# Normalized character-similarity matching; diagnostic score is in [0, 1].
MATCH_THRESHOLD = 0.85
MIN_MATCH_MARGIN = 0.05  # Reject ambiguous best/second-best fuzzy matches.


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


FIELDS = [
    "row_id", "base_model", "variant_id", "model", "barrier_id",
    "displayed_position", "official_label", "model_label", "label_status",
    "is_hallucinated", "iteration", "timestamp", "attempt_id", "model_seed",
    "match_method", "match_similarity", "match_margin",
]


def read_results():
    """Validate committed iterations and recover a stale counter after a restart."""
    rows = []
    if OUTPUT_CSV.exists():
        with OUTPUT_CSV.open(newline="", encoding="utf-8") as handle:
            reader = csv.DictReader(handle)
            if reader.fieldnames != FIELDS:
                raise ValueError("Unexpected CSV schema; do not mix old and randomized outputs")
            rows = list(reader)
    groups = defaultdict(list)
    for row in rows:
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
            "attempts": 0,
        }
        write_json(PENDING_FILE, pending)
    order = pending["order"]
    if (pending["iteration"] != iteration
            or len(order) != len(BARRIERS) or set(order) != set(BARRIERS)
            or pending["prompt"] != question(order)
            or pending["model_seed"] != SEED
            or pending["model_variants"] != MODEL_VARIANTS):
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
    raw_path = RAW_LOG_DIR / f"zeroshotBarrierSelectRandOrderIter{iteration:02d}Attempt{attempt_id}.txt"
    atomic_write(raw_path, "".join(
        f"\n--- {model_name} ---\n{response.strip()}\n"
        for model_name, response in raw_responses.items()
    ))
    record.update(raw_log=raw_path.name, finished_at=now(), failed_models=failed_models)
    if failed_models:
        record["status"] = "failed"
        failure = dict(record, raw_responses=raw_responses, structured_responses=structured)
        write_json(RAW_LOG_DIR / f"zeroshotBarrierSelectFailedAttempt{attempt_id}.json", failure)
        write_json(order_path, record)
        print("Attempt failed; pending order retained for retry.", flush=True)
        return False
    rows = []
    for model_name, entries in structured.items():
        base_model, role = model_name.split("-", 1)
        for entry in entries:
            barrier_id = entry["barrier_id"]
            rows.append({
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--until-successes", type=int, default=None,
                        help="Keep retrying until the successful counter reaches this value")
    args = parser.parse_args()
    if args.until_successes is not None and args.until_successes < 1:
        parser.error("--until-successes must be positive")
    main(args.until_successes)
