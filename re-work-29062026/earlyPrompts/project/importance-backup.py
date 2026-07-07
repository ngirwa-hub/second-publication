import os
import requests
import json
import re
import datetime
import csv
from collections import Counter

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

RATING_LABELS = {
    0: "Not able to respond",
    1: "Not important",
    2: "Somewhat important",
    3: "Important",
    4: "Very important"
}

EXPECTED_SOLUTIONS = [
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

# Output paths
OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "importance_all.csv")
EXTRA_CSV = os.path.join(OUTPUT_FOLDER, "importance_extras.csv")

# Load context and instructions

def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project...\n"
        "- The proposed DC solutions are:\n"
        + "\n".join([f" {i+1}. {sol}" for i, sol in enumerate(EXPECTED_SOLUTIONS)])
    )

def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "For each DC solution listed, provide:\n"
        "A rating line using the format: Rating: <number between 0 and 4>\n"
        "A short justification explaining your rating.\n\n"
        "For each solution, respond using ONLY this exact structure:\n\n"
        "<DC solution Name>\n"
        "Rating: <0–4>\n"
        "Justification: <Your explanation>\n\n"
        "Do NOT add explanations, summaries, introductions, or reasoning outside this structure.\n"
        "Only return in the format provided."
    )

def attach_scale(text, scale_title="Importance Scale"):
    return text + (
        f"\n\n{scale_title}:\n"
        "0 - Not able to respond\n"
        "1 - Not important\n"
        "2 - Somewhat important\n"
        "3 - Important\n"
        "4 - Very important"
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
        print(f"Error from model {model}: {response.status_code}")
        return ""

def extract_ratings_and_justifications(response_text, variant_id, base_model):
    structured = []
    extras = []
    seen = set()

    blocks = re.split(r"\n(?=\s*[^\n]+\n\s*Rating\s*[:\-]?\s*\d)", response_text.strip())

    for block in blocks:
        lines = block.strip().splitlines()
        if len(lines) < 2:
            continue

        solution = lines[0].strip()

        rating = None
        for line in lines[1:]:
            rating_match = re.search(r"Rating\s*[:\-]?\s*([0-4])\b", line, re.IGNORECASE)
            if rating_match:
                rating = int(rating_match.group(1))
                break

        justification_lines = []
        justification_found = False
        for line in lines:
            if re.match(r"Justification\s*[:\-]?", line, re.IGNORECASE):
                justification_found = True
                cleaned = re.sub(r"Justification\s*[:\-]?\s*", "", line, flags=re.IGNORECASE)
                justification_lines.append(cleaned)
            elif justification_found:
                justification_lines.append(line)

        justification = "\n".join(justification_lines).strip()

        if solution in EXPECTED_SOLUTIONS and solution not in seen:
            seen.add(solution)
            structured.append({
                "solution": solution,
                "rating": rating,
                "justification": justification,
                "raw_response": response_text
            })
        elif solution not in EXPECTED_SOLUTIONS or solution in seen:
            extras.append({
                "base_model": base_model,
                "variant_id": variant_id,
                "solution": solution,
                "rating": rating,
                "justification": justification,
                "timestamp": timestamp
            })

    return structured, extras

def run_experts(prompt, base_model):
    all_structured = {}
    all_extras = []
    for role, model in EXPERTS.items():
        print(f"\n🔍 Querying {role} expert...")
        response = query_expert(model, prompt)
        variant_id = f"{base_model}_{role}"
        if response:
            structured, extras = extract_ratings_and_justifications(response, variant_id, base_model)
            all_structured[role] = structured
            all_extras.extend(extras)
        else:
            print(f"⚠️ No response from {role} expert.")
            all_structured[role] = []
    return all_structured, all_extras

# Iteration counter per model
iteration_counter_file = lambda base_model: f"importance_counter_{base_model}.txt"
def get_iteration(base_model):
    path = iteration_counter_file(base_model)
    if os.path.exists(path):
        with open(path, "r") as f:
            try:
                return int(f.read().strip())
            except ValueError:
                return 0
    return 0

def save_iteration(base_model, count):
    path = iteration_counter_file(base_model)
    with open(path, "w") as f:
        f.write(str(count))

if __name__ == "__main__":
    base_model = "deepseek-r1"  # Change per model
    iteration_counter = get_iteration(base_model)

    # Prompt construction
    context = "\n\n".join([load_context(), load_instructions()])
    base_question = (

        "Considering all the listed DC solutions (listed in no particular order of usability, or innovation):\n"
        " - Smart and sustainable DC cables\n"
        " - DC connectors\n"
        " - Static protection system\n"
        " - Semiconductor-based circuit breaker\n"
        " - Protection DC system design tool\n"
        " - DC-DC converter\n"
        " - LVAC-LVDC interlink converter\n"
        " - DC measurement device\n"
        " - DC solution design tool\n"
        " - Network design tool for DC solutions\n"
        " - Solid-state circuit breaker\n"
        "Question: Evaluate the importance of each of the listed DC solutions, considering the provided context and rating scale provided."
    )
    question_with_scale = attach_scale(base_question)
    full_prompt = f"{context}\n\nQuestion:\n{question_with_scale}"

    results, extras = run_experts(full_prompt, base_model)

    all_success = all(len(r) == 11 for r in results.values())

    # Write successful responses
    if all_success:
        if not os.path.exists(MASTER_CSV):
            with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(f, fieldnames=["row_id", "base_model", "variant_id", "model", "dc_solution", "rating", "label", "iteration", "timestamp", "justification"])
                writer.writeheader()

        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["row_id", "base_model", "variant_id", "model", "dc_solution", "rating", "label", "iteration", "timestamp", "justification"])
            for model, responses in results.items():
                variant_id = f"{base_model}_{model}"
                for entry in responses:
                    dc_sol=entry["solution"].replace(" ", "_").lower().replace("-", "_").lower()
                    row_id = f"{variant_id}_{dc_sol}_{iteration_counter:02d}_{timestamp}"
                    label = RATING_LABELS.get(entry["rating"], "")
                    writer.writerow({
                        "row_id": row_id,
                        "base_model": base_model,
                        "variant_id": variant_id,
                        "model": model,
                        "dc_solution": dc_sol,
                        "rating": entry["rating"],
                        "label": label,
                        "iteration": iteration_counter,
                        "timestamp": timestamp,
                        "justification": entry["justification"]
                    })
        save_iteration(base_model, iteration_counter + 1)
        print(f"✅ Results appended to: {MASTER_CSV}")

    else:
        # Save fallback JSON
        fallback_path = os.path.join(OUTPUT_FOLDER, f"importance_failed_{timestamp}.json")
        failures = {k: len(v) for k, v in results.items() if len(v) != 11}
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({
                "question": question_with_scale,
                "structured_responses": results,
                "error_reason": f"One or more models did not return 11 solutions: {failures}"
            }, f, indent=4, ensure_ascii=False)
        print(f"⚠️ Incomplete responses saved to: {fallback_path}")

    # Save extras (even if run succeeded)
    if extras:
        save_header = not os.path.exists(EXTRA_CSV)
        with open(EXTRA_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["base_model", "variant_id", "solution", "rating", "justification", "timestamp"])
            if save_header:
                writer.writeheader()
            writer.writerows(extras)

