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
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "solution_propose_all.csv")

# Counter for iteration tracking
counter_file = "solution_propose_counter.txt"
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
        writer = csv.DictWriter(f, fieldnames=["row_id", "base_model", "variant_id", "model", "proposal_title", "explanation", "iteration", "timestamp"])
        writer.writeheader()

# Context and instructions

def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies. "
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
    )

def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Propose new DC solutions NOT already listed.\n"
        "Each should include a short name/title and a 1–3 line explanation.\n"
        "Number your proposals. Max 5."
    )

def load_question():
    return (
        "Please propose up to 5 additional DC solutions you believe would be valuable for ports, industry, data centers, or buildings."
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

def extract_proposals(response_text):
    pattern = re.findall(r"(?m)^\s*(\d+)[\).:-]?\s*(.*?)\n\s*(.+)", response_text.strip())
    results = []
    for idx, title, explanation in pattern:
        if title.strip() and explanation.strip():
            results.append({"title": title.strip(), "explanation": explanation.strip()})
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
        extracted = extract_proposals(response)

        if not (1 <= len(extracted) <= 5):
            print(f"⚠️ {role} returned {len(extracted)} proposals (expected 1–5).")
            all_valid = False

        structured_responses[role] = extracted

    if all_valid:
        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["row_id", "base_model", "variant_id", "model", "proposal_title", "explanation", "iteration", "timestamp"])
            for role, proposals in structured_responses.items():
                variant_id = f"{base_model}_{role}"
                for i, p in enumerate(proposals, start=1):
                    row_id = f"{variant_id}_{iteration_counter:02d}_p{i}"
                    writer.writerow({
                        "row_id": row_id,
                        "base_model": base_model,
                        "variant_id": variant_id,
                        "model": role,
                        "proposal_title": p["title"],
                        "explanation": p["explanation"],
                        "iteration": iteration_counter,
                        "timestamp": timestamp
                    })
        print(f"✅ Results appended to: {MASTER_CSV}")
        iteration_counter += 1
        with open(counter_file, "w") as f:
            f.write(str(iteration_counter))
    else:
        fallback_path = os.path.join(OUTPUT_FOLDER, f"solution_propose_failed_{timestamp}.json")
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({
                "question": question,
                "structured_responses": structured_responses,
                "raw_responses": raw_responses,
                "error_reason": "One or more model variants returned fewer than 1 or more than 5 valid solution proposals."
            }, f, indent=4, ensure_ascii=False)
        print(f"⚠️ Invalid response structure saved to: {fallback_path}")
