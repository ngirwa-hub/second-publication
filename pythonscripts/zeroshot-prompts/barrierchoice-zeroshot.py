import os
import csv
import requests
import json
import datetime
from difflib import SequenceMatcher

# Base model family (for logging, tracking, filenames)
base_model = "mistral"

# Folder setup
OUTPUT_FOLDER = "expert_responses"
RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs")
os.makedirs(RAW_LOG_DIR, exist_ok=True)
OUTPUT_CSV = os.path.join(OUTPUT_FOLDER, "barrier_choice_all.csv")
COUNTER_FILE = f"barrierChoice_{base_model}_counter.txt"

# Expert variants (each must exist in Ollama with `ollama create`)
EXPERTS = {
    "generalist": "generalist",
    "generalist2": "generalist2",
    "normative": "normative",
    "normative2": "normative2",
    "subject_matter": "subject_matter",
    "subject_matter2": "subject_matter2"
}

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
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies.\n"
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
    response = requests.post(
        "http://localhost:11434/api/generate",
        json={"model": model_name, "prompt": prompt, "stream": True},
        stream=True
    )
    if response.status_code == 200:
        full_text = ""
        for line in response.iter_lines():
            if line:
                result = json.loads(line.decode("utf-8"))
                full_text += result.get("response", "")
        return full_text
    else:
        print(f"❌ Failed to query model {model_name}. Status code: {response.status_code}")
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

# Parse LLM response
def extract_barrier_info(response_text):
    lines = response_text.split("\n")
    selected_barriers = []
    for line in lines:
        if "." not in line:
            continue
        parts = line.split(".", 1)
        if len(parts) != 2:
            continue
        try:
            barrier_id = int(parts[0].strip())
            model_label = parts[1].strip()
        except ValueError:
            continue

        similarity, matched_label_id, matched_label = find_best_match(model_label)
        label_status = "matched" if similarity >= 0.85 else "hallucinated_label"

        if label_status == "hallucinated_label":
            barrier_id_status = "outside_range_id"
            official_label = ""
        else:
            official_label = matched_label
            if barrier_id not in BARRIERS:
                barrier_id_status = "outside_range_id"
            elif barrier_id == matched_label_id:
                barrier_id_status = "correct_id"
            else:
                barrier_id_status = "wrong_id_matchable"

        selected_barriers.append({
            "barrier_id": barrier_id,
            "model_label": model_label,
            "official_label": official_label,
            "label_status": label_status,
            "barrier_id_status": barrier_id_status
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

    for i in range(rounds):
        run_id = f"barrierChoice_{base_model}_{iteration_counter:02d}_{timestamp}"
        all_rows = []
        run_logs = []
        raw_responses = {}
        structured_responses = {}
        all_valid = True

        for role, model_name in EXPERTS.items():
            print(f"🔍 Querying expert: {role} ({model_name})")
            response = query_model(prompt, model_name)
            raw_responses[role] = response
            if not response.strip():
                print(f"⚠️ No response from {model_name}")
                all_valid = False
                continue

            barriers = extract_barrier_info(response)
            structured_responses[role] = barriers
            if len(barriers) < 5:
                print(f"⚠️ {model_name} returned only {len(barriers)} barriers")
                all_valid = False

            variant_id = f"{role}_{base_model.replace(':', '').replace('-', '')}"
            for b in barriers:
                row_id = f"{variant_id}_{iteration_counter:02d}_b{b['barrier_id']}"
                all_rows.append({
                    "row_id": row_id,
                    "base_model": base_model,
                    "variant_id": variant_id,
                    "model": role,
                    "barrier_id": b["barrier_id"],
                    "official_label": b["official_label"],
                    "model_label": b["model_label"],
                    "label_status": b["label_status"],
                    "barrier_id_status": b["barrier_id_status"],
                    "is_hallucinated": b["label_status"] == "hallucinated_label",
                    "iteration": iteration_counter,
                    "timestamp": timestamp
                })

            run_logs.append(f"\n--- {variant_id} ---\n{response.strip()}\n")

        # Save raw logs
        with open(os.path.join(RAW_LOG_DIR, f"{run_id}.txt"), "w", encoding="utf-8") as logf:
            logf.write("".join(run_logs))

        if all_valid:
            write_header = not os.path.exists(OUTPUT_CSV)
            with open(OUTPUT_CSV, "a", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(f, fieldnames=[
                    "row_id", "base_model", "variant_id", "model",
                    "barrier_id", "official_label", "model_label",
                    "label_status", "barrier_id_status", "is_hallucinated",
                    "iteration", "timestamp"
                ])
                if write_header:
                    writer.writeheader()
                writer.writerows(all_rows)

            iteration_counter += 1
            with open(COUNTER_FILE, "w") as f:
                f.write(str(iteration_counter))
            print(f"✅ Run complete. Saved {len(all_rows)} rows.")
        else:
            # Save fallback for manual inspection
            json_path = os.path.join(OUTPUT_FOLDER, f"bchoice_failed_{timestamp}_{base_model}.json")
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump({
                    "question": prompt,
                    "raw_responses": raw_responses,
                    "structured_responses": structured_responses,
                    "error_reason": "One or more variants returned fewer than 5 barriers."
                }, f, indent=4, ensure_ascii=False)
            print(f"⚠️ Incomplete run. Saved diagnostics to {json_path}")

if __name__ == "__main__":
    main(rounds=1)
