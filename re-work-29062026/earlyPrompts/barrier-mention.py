import os
import requests
import json
import re
import datetime
import csv

# Timestamp
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

# Expert variants
EXPERTS = {
    "generalist": "generalist",
    "generalist2": "generalist2",
    "normative": "normative",
    "normative2": "normative2",
    "subject_matter": "subject_matter",
    "subject_matter2": "subject_matter2"
}

# Output paths
OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)
RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs")
os.makedirs(RAW_LOG_DIR, exist_ok=True)
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "barrier_mention_all.csv")

# Counter for iteration tracking
counter_file = "barrierMention_counter.txt"
if os.path.exists(counter_file):
    with open(counter_file, "r") as f:
        try:
            iteration_counter = int(f.read().strip())
        except ValueError:
            iteration_counter = 0
else:
    iteration_counter = 0

# Ensure CSV has headers
if not os.path.exists(MASTER_CSV):
    with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["row_id", "base_model", "variant_id", "model", "barrier_title", "explanation", "iteration", "timestamp", "truncated", "is_duplicate"])
        writer.writeheader()

# Context and instructions

def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies. "
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
    )

def load_barrier_list():
    return [
        "power losses, quality and safety issues",
        "reduced reliability in DC devices",
        "lack of use-cases in which DC is advantageous",
        "uncertain utility interaction (net metering, utility ownership, and agreed standards)",
        "lack of pilot projects",
        "public perception of DC and readiness to 'champion' installations from DC projects",
        "incompatibility of DC systems components",
        "misconception and lack of knowledge leads to lengthy/expensive design and permit process",
        "lack of enough trained personnel in DC systems",
        "uncertain regulatory roadmap",
        "high costs of DC solutions"
    ]

def load_instructions():
    barrier_list=load_barrier_list()
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Propose new barriers to DC adoption.\n"
        "Be concise. Start with a short title, then give a brief explanation."
        f"Do not repeat barriers already listed in: {', '.join(barrier_list)}\n"
    )

def load_question():
    return (
        "Provide up to 5 new barriers to DC adoption that are NOT in the list.\n"
        "Consider numbering each item. \n"
        "Each item must include a short title and a brief explanation."
    )

def query_expert(model, question):
    response = requests.post(
        "http://localhost:11434/api/generate",
        json={"model": model, "prompt": question},
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
        return ""

def extract_custom_barriers(response_text):
    pattern = re.findall(r"(?m)^\s*(\d+)[\).:-]?\s*(.*?)\n\s*(.+)", response_text.strip())
    results = []
    seen_titles = set()
    for idx, title, explanation in pattern:
        normalized_title = re.sub(r'\W+', '', title.strip().lower())
        is_duplicate = normalized_title in seen_titles
        results.append({
            "title": title.strip(),
            "explanation": explanation.strip(),
            "is_duplicate": is_duplicate
        })
        seen_titles.add(normalized_title)
    return results

# Main execution
if __name__ == "__main__":
    context = "\n\n".join([load_context(), load_instructions()])
    base_model = "phi4"  # Change per model
    question = load_question()
    full_prompt = f"{context}\n\nQuestion:\n{question}"

    all_valid = True
    structured_responses = {}
    raw_responses = {}

    for role, model in EXPERTS.items():
        print(f"🔍 Querying {role}...")
        response = query_expert(model, full_prompt)
        raw_responses[role] = response
        extracted = extract_custom_barriers(response)

        if len(extracted) < 5:
            print(f"⚠️ {role} returned {len(extracted)} custom barriers (expected at least 5).")
            all_valid = False
        else:
            structured_responses[role] = extracted

    if all_valid:
        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["row_id", "base_model", "variant_id", "model", "barrier_title", "explanation", "iteration", "timestamp", "truncated", "is_duplicate"])
            for role, barriers in structured_responses.items():
                variant_id = f"{base_model}_{role}"
                full_count = len(barriers)
                truncated_flag = "yes" if full_count > 5 else "no"
                for i, b in enumerate(barriers[:5], start=1):
                    row_id = f"{variant_id}_{iteration_counter:02d}_b{i}"
                    writer.writerow({
                        "row_id": row_id,
                        "base_model": base_model,
                        "variant_id": variant_id,
                        "model": role,
                        "barrier_title": b["title"],
                        "explanation": b["explanation"],
                        "iteration": iteration_counter,
                        "timestamp": timestamp,
                        "truncated": truncated_flag,
                        "is_duplicate": b["is_duplicate"]
                    })
        print(f"✅ Results appended to: {MASTER_CSV}")
        # Save raw responses to .txt files (one per variant)
        for role, response_text in raw_responses.items():
            variant_id = f"{base_model}_{role}"
            txt_path = os.path.join(RAW_LOG_DIR, f"{variant_id}_{iteration_counter:02d}_{timestamp}.txt")
            with open(txt_path, "w", encoding="utf-8") as f:
                f.write(response_text)
        iteration_counter += 1
        with open(counter_file, "w") as f:
            f.write(str(iteration_counter))
    else:
        fallback_path = os.path.join(RAW_LOG_DIR, f"barrier_mention_failed_{timestamp}.json")
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({
                "question": question,
                "structured_responses": structured_responses,
                "raw_responses": raw_responses,
                "error_reason": "One or more model variants returned fewer than 5 barriers."
            }, f, indent=4, ensure_ascii=False)
        print(f"⚠️ Invalid response structure saved to: {fallback_path}")
