import os
import requests
import json
import re
import datetime
import csv
#IMPORTANT: base model has been defined in two places, so make sure to change both places
#IMPORTANT: base model has been defined in two places, so make sure to change both places
#IMPORTANT: base model has been defined in two places, so make sure to change both places


# Timestamp for filenames
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

# Define expert variants
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
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "feasibility_all.csv")

# Load shared elicitation context
def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies. "
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
        "- The proposed DC solutions are;\n"
        " 1. Smart and sustainable DC cables\n"
        " 2. DC connectors\n"
        " 3. Static protection system\n"
        " 4. Semiconductor-based circuit breaker\n"
        " 5. Protection DC system design tool\n"
        " 6. DC-DC converter\n"
        " 7. LVAC-LVDC interlink converter\n"
        " 8. DC measurement device\n"
        " 9. DC solution design tool\n"
        " 10. Network design tool for DC solutions\n"
        " 11. Solid-state circuit breaker\n"
    )
def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and answer according to your expert role.\n"
        "Be clear, concise, and do not justify your response.\n\n"

        "You MUST use ONLY the following feasibility scale:\n"
        "0 - Not able to respond\n"
        "1 - Not feasible\n"
        "2 - Somewhat feasible\n"
        "3 - Feasible\n"
        "4 - Very feasible\n\n"
        "❗ Do NOT invent or use any number outside this scale.\n"
        "❌ 5, 6, 'Highly feasible', or anything else is INVALID.\n"
        "Responses with invalid ratings will be rejected.\n\n"
        "Your response must be ONE LINE ONLY:\n"
        "Format: <rating number> - <matching label from the scale>\n"
        "Do NOT include explanations, reasoning, or <think> sections.\n"
    )

def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and answer according to your expert role.\n"
        "Be clear, concise, and no need for justification of your response\n"
        "Please use ONLY the provided feasibility rating scale.\n"
        "Respond with one line only, beginning with the rating number (0–4) followed by the matching label from the feasibility scale.\n"
        "Do not include explanations or extra context, or texts, or your thinking process like the <think>.....</think>.\n"
        "THE ONLY FEASIBILITY SCALE TO USE is: <0 - Not able to respond, 1 - Not feasible, 2 - Somewhat feasible, 3 - Feasible, 4 - Very feasible>"
        "Format your answer: <rating> - <label> \n"
    )

def attach_scale(question_text, scale_title="Feasibility Scale"):
    scale = (
        f"\n\n{scale_title}:\n"
        "0- Not able to respond\n"
        "1 - Not feasible\n"
        "2 - Somewhat feasible\n"
        "3 - Feasible\n"
        "4 - Very feasible\n"
    )
    return question_text + scale

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

import re

# Define allowed labels and mapping
RATING_LABELS = {
    0: "Not able to respond",
    1: "Not feasible",
    2: "Somewhat feasible",
    3: "Feasible",
    4: "Very feasible"
}
LABEL_TO_RATING = {v.lower(): k for k, v in RATING_LABELS.items()}

def extract_single_rating(response_text):
    lines = [line.strip() for line in response_text.strip().splitlines() if line.strip()]
    rating_label_matches = []
    label_only_matches = []

    # First pass: search for "X - Label" pattern in all lines
    for line in lines:
        match = re.match(
            r"^\s*([0-4])\s*[-–:]?\s*(Very feasible|Feasible|Somewhat feasible|Not feasible|Not able to respond)\s*$",
            line,
            re.IGNORECASE
        )
        if match:
            rating = int(match.group(1))
            label = match.group(2).strip().capitalize()
            rating_label_matches.append({"rating": rating, "label": label})

    # If structured match found, return the last one
    if rating_label_matches:
        return rating_label_matches[-1]

    # Second pass: fallback — look for known label only
    for line in lines:
        label_match = re.search(
            r"\b(Very feasible|Feasible|Somewhat feasible|Not feasible|Not able to respond)\b",
            line,
            re.IGNORECASE
        )
        if label_match:
            label = label_match.group(1).strip().capitalize()
            rating = LABEL_TO_RATING.get(label.lower())
            label_only_matches.append({"rating": rating, "label": label})

    # If label-only match found, return the last one
    if label_only_matches:
        return label_only_matches[-1]

    # If nothing found
    return {"rating": None, "label": ""}

raw_responses = {}
def run_experts(full_prompt):
    all_structured = {}
    for role, model in EXPERTS.items():
        print(f"\n🔍 Querying {role} expert...")
        response = query_expert(model, full_prompt)
        raw_responses[role] = response
        if response:
            extracted = extract_single_rating(response)
            all_structured[role] = extracted
        else:
            print(f"⚠️ No response from {role} expert.")
            all_structured[role] = {"rating": None, "label": ""}
    return all_structured

# Define base_model before using it
base_model = "gemma3-12b"  # Change this to your desired base model

# Counter file for tracking iterations
iteration_counter_file = f"feasibility_counter_{base_model}.txt"
if os.path.exists(iteration_counter_file):
    with open(iteration_counter_file, "r") as f:
        try:
            iteration_counter = int(f.read().strip())
        except ValueError:
            print("⚠️ Invalid counter value in file, starting from 0.")
            iteration_counter = 0
else:
    print("⚠️ No counter file found, starting from 0.")
    iteration_counter = 0

# Ensure master CSV exists with header
if not os.path.exists(MASTER_CSV):
    with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["row_id", "variant_id", "base_model", "model", "rating", "label", "iteration", "timestamp", "raw_response"])
        writer.writeheader()

# Main execution
if __name__ == "__main__":
    base_model = "gemma3-12b"  # Change this to your desired base model
    context_sections = [load_context(), load_instructions()]
    context = "\n\n".join(context_sections)

    base_question = (
        "Question: How feasible is the use of DC solutions for the target sectors described in the Shift2DC project?\n"
        "Key considerations:\n"
        "- Consider all the listed DC solutions within the context of the Shift2DC project.\n"
        "- Provide an overall assessment of the feasibility of these DC solutions in the target sectors.\n"
        "⚠️-Use ONLY the provided rating scale <0–4> (the feasibility Scale).\n"
        "- Do not provide justification for your choice.\n"
        "- Focus on the overall feasibility rather than assessing each solution individually.\n"
        )
    question_with_scale = attach_scale(base_question)
    full_prompt = f"{context}\n\nQuestion:\n{question_with_scale}"

    results = run_experts(full_prompt)

    # Save raw responses as .txt file for this run
    raw_log_path = os.path.join(OUTPUT_FOLDER, f"feasibility_raw_{base_model}_iter{iteration_counter:02d}_{timestamp}.txt")
    with open(raw_log_path, "w", encoding="utf-8") as f:
        for model, raw in raw_responses.items():
            f.write(f"\n--- {model} ---\n{raw.strip()}\n")
    print(f"📝 Raw responses saved to: {raw_log_path}")


    all_success = all(result["rating"] is not None for result in results.values())

    if all_success:
        # Append to master CSV
        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["row_id", "variant_id", "base_model", "model", "rating", "label", "iteration", "timestamp", "raw_response"])
            for model, result in results.items():
                variant_id = f"{base_model}_{model}"
                row_id = f"{variant_id}_{iteration_counter:02d}_{timestamp}"
                writer.writerow({
                    "row_id": row_id,
                    "variant_id": variant_id,
                    "base_model": base_model,
                    "model": model,
                    "rating": result["rating"],
                    "label": result["label"],
                    "iteration": iteration_counter,
                    "timestamp": timestamp,
                    "raw_response": raw_responses.get(model, "")
                })
        print(f"✅ Results appended to: {MASTER_CSV}")
        # Increment and store counter
        iteration_counter += 1
        with open(iteration_counter_file, "w") as f:
            f.write(str(iteration_counter))
    else:
        # Save full JSON if any model failed
        fallback_path = os.path.join(OUTPUT_FOLDER, f"feasibility_failed_{timestamp}.json")
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({
                "question": question_with_scale,
                "structured_responses": results,
                "raw_responses": raw_responses
            }, f, indent=4, ensure_ascii=False)
        print(f"⚠️ Incomplete responses saved to: {fallback_path}")


#IMPORTANT: base model has been defined in two places, so make sure to change both places
