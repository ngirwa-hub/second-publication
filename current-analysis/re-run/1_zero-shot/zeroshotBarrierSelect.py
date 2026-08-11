r"""
1..50 | % { python .\1_zero-shot\zeroshotBarrierSelect.py }

# sample starter:
1..3 | % { python .\1_zero-shot\zeroshotBarrierSelect.py }


"""
import os
import csv
import requests
import json
import datetime
from difflib import SequenceMatcher
from pathlib import Path
import re
import unicodedata

# Folder setup
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_FOLDER = SCRIPT_DIR / "zeroshotResponses"
RAW_LOG_DIR = OUTPUT_FOLDER / "zeroshotBarrierSelectRawLogs"
RAW_LOG_DIR.mkdir(parents=True, exist_ok=True)
OUTPUT_CSV = OUTPUT_FOLDER / "zeroshotBarrierSelectResponses.csv"
COUNTER_FILE = OUTPUT_FOLDER / "zeroshotBarrierSelectCounter.txt"

def filename_token(value):
    return "".join(part.capitalize() for part in re.findall(r"[A-Za-z0-9]+", value))

# Expert variants (each must exist in Ollama with `ollama create`)
MODEL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter"
]

# Barriers
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
VALID_BARRIER_IDS = set(BARRIERS.keys())

# Prompt components
def context():
    return (
        "- The expert elicitation is based on the XXXXXX project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies.\n"
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
    )

def instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and respond according to your expert role.\n"
        "You must select exactly five (5) barriers from the list provided.\n"
        "Do not select more or fewer than five.\n"
        "⚠️ You must copy and use the barrier entries exactly as listed — including the barrier number and full label.\n"
        "Do not paraphrase, rephrase, or modify any barrier wording.\n"
        "Only select from the list — do not add new barriers.\n"
        "The list is shown in random order and does not reflect importance.\n"
        "No justification is required. List the five selected barriers clearly."
    )

def question():
    barrier_list = "\n".join([f"{k}. {v}" for k, v in BARRIERS.items()])
    return f"{context()}\n\n{instructions()}\n\nBarriers:\n{barrier_list}"

# Call model variant
def query_model(prompt, model_name):
    try:
        response = requests.post(
            "http://localhost:11434/api/generate",
            json={"model": model_name, "prompt": prompt, "stream": True},
            stream=True,
            timeout=120
        )
        if response.status_code == 200:
            full_text = ""
            for line in response.iter_lines():
                if line:
                    result = json.loads(line.decode("utf-8"))
                    full_text += result.get("response", "")
            return full_text
        print(f"Failed to query model {model_name}. Status code: {response.status_code}")
        return ""
    except requests.RequestException as exc:
        print(f"Failed to query model {model_name}: {exc}")
        return ""

# Match labels
def is_similar(a, b, threshold=0.85):
    return SequenceMatcher(None, a.lower().strip(), b.lower().strip()).ratio() >= threshold

def find_best_match(label):
    best_score = 0
    best_id = None
    best_label = ""
    for k, v in BARRIERS.items():
        score = SequenceMatcher(None, label.lower().strip(), v.lower().strip()).ratio()
        if score > best_score:
            best_score = score
            best_id = k
            best_label = v
    return best_score, best_id, best_label

def normalize_barrier_label(label):
    normalized = unicodedata.normalize("NFKC", label).casefold()
    normalized = "".join(char if char.isalnum() else " " for char in normalized)
    return " ".join(normalized.split())

# Parse LLM response
def extract_barrier_info(response_text):
    selected_barriers = []
    official_by_label = {
        normalize_barrier_label(label): (barrier_id, label)
        for barrier_id, label in BARRIERS.items()
    }

    for raw_line in response_text.splitlines():
        match = re.match(r"^\s*(\d{1,2})\s*[\.\)\-:–—]\s*(.+?)\s*$", raw_line)
        if not match:
            continue

        model_barrier_id = int(match.group(1))
        model_label = match.group(2).strip()
        normalized_label = normalize_barrier_label(model_label)
        exact_match = official_by_label.get(normalized_label)
        similarity, suggested_id, suggested_label = find_best_match(model_label)

        if exact_match is None:
            barrier_id = None
            official_label = ""
            label_status = "unmatched"
            barrier_id_status = "unverified_id"
        else:
            barrier_id, official_label = exact_match
            label_status = "matched"
            barrier_id_status = (
                "correct_id"
                if model_barrier_id == barrier_id
                else "corrected_from_label"
            )

        selected_barriers.append({
            "barrier_id": barrier_id,
            "model_barrier_id": model_barrier_id,
            "model_label": model_label,
            "official_label": official_label,
            "label_status": label_status,
            "barrier_id_status": barrier_id_status,
            "similarity": float(similarity),
            "suggested_barrier_id": suggested_id,
            "suggested_label": suggested_label
        })
    return selected_barriers

# Main run
def main(rounds=1):
    # Load counter
    if os.path.exists(COUNTER_FILE):
        with open(COUNTER_FILE, "r") as f:
            try:
                iteration_counter = int(f.read().strip())
            except ValueError:
                iteration_counter = 0
    else:
        iteration_counter = 0

    prompt = question()
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filename_timestamp = timestamp.replace("_", "T")
    for i in range(rounds):
        run_id = f"zeroshotBarrierSelectRawLogIter{iteration_counter:02d}At{filename_timestamp}"
        all_rows = []
        run_logs = []
        raw_responses = {}
        structured_responses = {}
        all_valid = True

        for model_name in MODEL_VARIANTS:
            base_model, role = model_name.split("-", 1)
            print(f"Querying expert: {role} ({model_name})")
            response = query_model(prompt, model_name)
            raw_responses[model_name] = response
            if not response.strip():
                print(f"Warning: No response from {model_name}")
                all_valid = False
                continue

            barriers = extract_barrier_info(response)
            barriers = barriers[:5]
            structured_responses[model_name] = barriers
            valid_selection = (
                len(barriers) == 5
                and all(barrier["label_status"] == "matched" for barrier in barriers)
                and len({barrier["barrier_id"] for barrier in barriers}) == 5
            )
            if not valid_selection:
                print(f"Warning: {model_name} did not return 5 unique, matched barriers in its first 5 entries")
                all_valid = False

            variant_id = f"{base_model}_{role}"
            for b in barriers:
                row_id = f"{variant_id}_{iteration_counter:02d}_b{b['barrier_id']}"
                all_rows.append({
                    "row_id": row_id,
                    "base_model": base_model,
                    "variant_id": variant_id,
                    "model": role,
                    "barrier_id": b["barrier_id"],
                    "model_barrier_id": b["model_barrier_id"],
                    "official_label": b["official_label"],
                    "model_label": b["model_label"],
                    "label_status": b["label_status"],
                    "barrier_id_status": b["barrier_id_status"],
                    "is_hallucinated": b["label_status"] != "matched",
                    "iteration": iteration_counter,
                    "timestamp": timestamp
                })

            run_logs.append(f"\n--- {variant_id} ---\n{response.strip()}\n")

        # Save raw logs
        with open(RAW_LOG_DIR / f"{run_id}.txt", "w", encoding="utf-8") as logf:
            logf.write("".join(run_logs))

        if all_valid:
            write_header = not os.path.exists(OUTPUT_CSV)
            with open(OUTPUT_CSV, "a", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(f, fieldnames=[
                    "row_id", "base_model", "variant_id", "model",
                    "barrier_id", "model_barrier_id", "official_label", "model_label",
                    "label_status", "barrier_id_status", "is_hallucinated",
                    "iteration", "timestamp"
                ])
                if write_header:
                    writer.writeheader()
                writer.writerows(all_rows)

            iteration_counter += 1
            with open(COUNTER_FILE, "w") as f:
                f.write(str(iteration_counter))
            print(f"Run complete. Saved {len(all_rows)} rows.")
        else:
            # Save fallback for manual inspection
            json_path = RAW_LOG_DIR / f"zeroshotBarrierSelectFailedAt{filename_timestamp}.json"
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump({
                    "question": prompt,
                    "raw_responses": raw_responses,
                    "structured_responses": structured_responses,
                    "error_reason": "One or more variants did not return 5 unique, matched barriers in its first 5 entries."
                }, f, indent=4, ensure_ascii=False)
            print(f"Warning: Incomplete run. Saved diagnostics to {json_path}")

if __name__ == "__main__":
    main(rounds=1)
