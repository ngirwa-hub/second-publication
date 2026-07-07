r"""
1..50 | % { python .\1_zero-shot\zeroshotFeasibility.py }

"""
import os
import requests
import json
import re
import datetime
import csv
from pathlib import Path


# Timestamp for filenames
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
filename_timestamp = timestamp.replace("_", "T")

def filename_token(value):
    return "".join(part.capitalize() for part in re.findall(r"[A-Za-z0-9]+", value))

# Define expert variants
MODEL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter"
]

# Output paths
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_FOLDER = SCRIPT_DIR / "zeroshotResponses"
RAW_LOG_DIR = OUTPUT_FOLDER / "zeroshotFeasibilityRawLogs"
RAW_LOG_DIR.mkdir(parents=True, exist_ok=True)
MASTER_CSV = OUTPUT_FOLDER / "zeroshotFeasibilityResponses.csv"

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
    try:
        response = requests.post(
            "http://localhost:11434/api/generate",
            json={"model": model, "prompt": question},
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
        print(f"Error from model {model}: {response.status_code}")
        return ""
    except requests.RequestException as exc:
        print(f"Error querying model {model}: {exc}")
        return ""

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
    pattern = re.compile(
        r"^\s*([0-4])\s*[-–:]\s*"
        r"(Very feasible|Feasible|Somewhat feasible|Not feasible|Not able to respond)\s*$",
        re.IGNORECASE
    )

    for line in lines:
        match = pattern.match(line)
        if not match:
            continue

        rating = int(match.group(1))
        expected_label = RATING_LABELS[rating]
        if match.group(2).strip().lower() == expected_label.lower():
            return {"rating": rating, "label": expected_label}

    return {"rating": None, "label": ""}

raw_responses = {}
def run_experts(full_prompt):
    all_structured = {}
    for model_name in MODEL_VARIANTS:
        print(f"\nQuerying expert variant: {model_name}")
        response = query_expert(model_name, full_prompt)
        raw_responses[model_name] = response
        if response:
            extracted = extract_single_rating(response)
            all_structured[model_name] = extracted
        else:
            print(f"Warning: No response from {model_name}.")
            all_structured[model_name] = {"rating": None, "label": ""}
    return all_structured

# Counter file for tracking iterations
iteration_counter_file = OUTPUT_FOLDER / "zeroshotFeasibilityCounter.txt"
if os.path.exists(iteration_counter_file):
    with open(iteration_counter_file, "r") as f:
        try:
            iteration_counter = int(f.read().strip())
        except ValueError:
            print("Warning: Invalid counter value in file, starting from 0.")
            iteration_counter = 0
else:
    print("Warning: No counter file found, starting from 0.")
    iteration_counter = 0

# Ensure master CSV exists with header
if not os.path.exists(MASTER_CSV):
    with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["row_id", "variant_id", "base_model", "model", "rating", "label", "iteration", "timestamp", "raw_response"])
        writer.writeheader()

# Main execution
if __name__ == "__main__":
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
    raw_log_path = RAW_LOG_DIR / f"zeroshotFeasibilityRawLogIter{iteration_counter:02d}At{filename_timestamp}.txt"
    with open(raw_log_path, "w", encoding="utf-8") as f:
        for model, raw in raw_responses.items():
            f.write(f"\n--- {model} ---\n{raw.strip()}\n")
    print(f"Raw responses saved to: {raw_log_path}")


    all_success = all(result["rating"] is not None for result in results.values())

    if all_success:
        # Append to master CSV
        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["row_id", "variant_id", "base_model", "model", "rating", "label", "iteration", "timestamp", "raw_response"])
            for model_name, result in results.items():
                base_model, role = model_name.split("-", 1)
                variant_id = f"{base_model}_{role}"
                row_id = f"{variant_id}_{iteration_counter:02d}_{timestamp}"
                writer.writerow({
                    "row_id": row_id,
                    "variant_id": variant_id,
                    "base_model": base_model,
                    "model": role,
                    "rating": result["rating"],
                    "label": result["label"],
                    "iteration": iteration_counter,
                    "timestamp": timestamp,
                    "raw_response": raw_responses.get(model_name, "")
                })
        print(f"Results appended to: {MASTER_CSV}")
        # Increment and store counter
        iteration_counter += 1
        with open(iteration_counter_file, "w") as f:
            f.write(str(iteration_counter))
    else:
        # Save full JSON if any model failed
        fallback_path = RAW_LOG_DIR / f"zeroshotFeasibilityFailedAt{filename_timestamp}.json"
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({
                "question": question_with_scale,
                "structured_responses": results,
                "raw_responses": raw_responses
            }, f, indent=4, ensure_ascii=False)
        print(f"Warning: Incomplete responses saved to: {fallback_path}")
