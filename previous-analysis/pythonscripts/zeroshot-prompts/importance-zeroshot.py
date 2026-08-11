import os
import requests
import json
import re
import datetime
import csv

# ==== Setup ====
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

EXPERTS = {
    "generalist": "generalist",
    "generalist2": "generalist2",
    "normative": "normative",
    "normative2": "normative2",
    "subject_matter": "subject_matter",
    "subject_matter2": "subject_matter2"
}

RATING_LABELS = {
    0: "Not able to respond",
    1: "Not important",
    2: "Somewhat important",
    3: "Important",
    4: "Very important"
}

DC_SOLUTIONS = [
    "Smart and sustainable DC cables",
    "DC connectors",
    "Static protection system",
    "Semiconductor-based circuit breaker",
    "Protection DC system design tool",
    "DC-DC converter",
    "LVAC-LVDC interlink converter",
    "DC measurement device",
    "DC solution design tool",
    "Network design tool for DC solutions",
    "Solid-state circuit breaker"
]

OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "importance_all.csv")

RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs")
os.makedirs(RAW_LOG_DIR, exist_ok=True)

# ==== Context ====
def load_context():
    return (
        "- The Shift2DC project promotes DC technologies in ports, industry, data centers, and buildings.\n"
        "- Evaluate the importance of the listed DC solution.\n"
    )

def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "You must evaluate the importance of each DC solution listed.\n\n"
        "❗ Format requirements:\n"
        "Each response must use a plain-text 3-line block:\n"
        "Line 1: <DC Solution Name>\n"
        "Line 2: Rating: <a number from 0 to 4>\n"
        "Line 3: Justification: <one short sentence>\n\n"
        "⚠️ Invalid formats will be rejected. Only clean 3-line blocks will be accepted.\n"
        "✅ Begin directly with the requested DC solution, using plain lines only.\n"
    )

def attach_scale(text):
    return text + (
        "\n\nImportance Scale:\n"
        "0 - Not able to respond\n"
        "1 - Not important\n"
        "2 - Somewhat important\n"
        "3 - Important\n"
        "4 - Very important"
    )

def query_expert(model, prompt):
    print(f"🔍 Querying expert variant: {model}")
    response = requests.post(
        "http://localhost:11434/api/generate",
        json={"model": model, "prompt": prompt},
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
        print(f"❌ Error from model {model}: {response.status_code}")
        return ""

# ==== Extraction ====
def extract_single_block(response_text, dc_solution):
    solution = dc_solution
    rating = None
    justification = ""

    # Look for rating
    match = re.search(r"Rating\s*[:\-]?\s*([0-4])\b", response_text, re.IGNORECASE)
    if match:
        rating = int(match.group(1))

    # Look for justification
    just_match = re.search(r"Justification\s*[:\-]?\s*(.*)", response_text, re.IGNORECASE)
    if just_match:
        justification = just_match.group(1).strip()

    return {
        "solution": solution,
        "rating": rating,
        "justification": justification
    }

# ==== Counter ====
def get_iteration(solution, base_model):
    file = f"imp_counter_{solution.replace(' ', '_')}_{base_model}.txt"
    if os.path.exists(file):
        with open(file, "r") as f:
            try:
                return int(f.read().strip()), file
            except:
                return 0, file
    else:
        return 0, file

# ==== Main Execution ====
base_model = "phi4"  # Change this base model
target_solution = DC_SOLUTIONS[10]  # Update index per run

context = "\n\n".join([load_context(), load_instructions()])
question = f"Evaluate the importance of {target_solution}, considering the provided context and rating scale.\nGive your response in the required format.\n"
full_prompt = f"{context}\n\n{attach_scale(question)}"

iteration, counter_file = get_iteration(target_solution, base_model)

results = {}
raw_responses = {}
success_map = {}

for role, model in EXPERTS.items():
    response = query_expert(model, full_prompt)
    raw_responses[role] = response

    if target_solution.lower() in response.lower():
        parsed = extract_single_block(response, target_solution)
        results[role] = [parsed]
        success_map[role] = parsed["rating"] is not None
    else:
        results[role] = []
        success_map[role] = False

all_success = all(success_map.values())

if all_success:
    try:
        if not os.path.exists(MASTER_CSV):
            with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(f, fieldnames=[
                    "row_id", "base_model", "variant_id", "model",
                    "dc_solution", "rating", "label", "iteration", "timestamp", "justification"
                ])
                writer.writeheader()

        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=[
                "row_id", "base_model", "variant_id", "model",
                "dc_solution", "rating", "label", "iteration", "timestamp", "justification"
            ])
            for model, entries in results.items():
                for entry in entries:
                    variant_id = f"{base_model}_{model}"
                    row_id = f"{variant_id}_{iteration:02d}_{timestamp}"
                    writer.writerow({
                        "row_id": row_id,
                        "base_model": base_model,
                        "variant_id": variant_id,
                        "model": model,
                        "dc_solution": entry["solution"],
                        "rating": entry["rating"],
                        "label": RATING_LABELS.get(entry["rating"], ""),
                        "iteration": iteration,
                        "timestamp": timestamp,
                        "justification": entry["justification"]
                    })
                    print(f"✅ Saved structured response for: {variant_id}")


        txt_path = os.path.join(RAW_LOG_DIR, f"{base_model}_{target_solution.replace(' ', '_').lower()}_iter{iteration:02d}_{timestamp}.txt")
        with open(txt_path, "w", encoding="utf-8") as txt_file:
            for variant, text in raw_responses.items():
                txt_file.write(f"\n--- {variant} ---\n{text.strip()}\n")
        print(f"📄 Raw response saved to: {txt_path}")

        with open(counter_file, "w") as f:
            f.write(str(iteration + 1))
        print(f"🔁 Current iteration: {iteration}")
        print(f"🔁 Iteration counter updated to {iteration + 1} (for next run)")


    except Exception as e:
        print(f"❌ Error while saving results: {e}")
        print("⛔ Iteration counter NOT incremented.")

else:
    fallback_path = os.path.join(OUTPUT_FOLDER, f"importance_failed_{timestamp}.json")
    with open(fallback_path, "w", encoding="utf-8") as f:
        json.dump({
            "target_solution": target_solution,
            "responses": raw_responses,
            "structured_results": results,
            "success_map": success_map,
            "error_reason": "One or more variants did not provide a complete or valid response."
        }, f, indent=4, ensure_ascii=False)
    print(f"⚠️ Not all variants returned valid responses. Fallback saved to: {fallback_path}")
