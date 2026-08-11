r"""
1..50 | % { python .\1_zero-shot\zeroshotImportance.py }
"""

import os
import requests
import json
import re
import datetime
import csv
from pathlib import Path

# ==== Setup ====
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
filename_timestamp = timestamp.replace("_", "T")

def filename_token(value):
    return "".join(part.capitalize() for part in re.findall(r"[A-Za-z0-9]+", value))

MODEL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter"
]

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

SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_FOLDER = SCRIPT_DIR / "zeroshotResponses"
OUTPUT_FOLDER.mkdir(parents=True, exist_ok=True)
MASTER_CSV = OUTPUT_FOLDER / "zeroshotImportanceResponses.csv"

RAW_LOG_DIR = OUTPUT_FOLDER / "zeroshotImportanceRawLogs"
RAW_LOG_DIR.mkdir(parents=True, exist_ok=True)

# ==== Context ====
def load_context():
    return (
        "- The XXXXXX project promotes DC technologies in ports, industry, data centers, and buildings.\n"
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
    rating = None
    justification = ""

    # Preferred plain-text fields. The configured target remains authoritative
    # even when the model shortens or reformats the solution name.
    rating_match = re.search(
        r"^\s*Rating\s*[:\-]?\s*([0-4])\b",
        response_text,
        re.IGNORECASE | re.MULTILINE
    )
    if rating_match:
        rating = int(rating_match.group(1))

    justification_match = re.search(
        r"^\s*Justification\s*[:\-]?\s*(.+)$",
        response_text,
        re.IGNORECASE | re.MULTILINE
    )
    if justification_match:
        justification = justification_match.group(1).strip()

    # Accept Markdown-table or pipe-delimited rows, including:
    # Solution | 4 | explanation
    # Solution | Rating: 4 | Justification: explanation
    if rating is None:
        for line in response_text.splitlines():
            if "|" not in line:
                continue

            cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
            if not cells or all(re.fullmatch(r":?-{3,}:?", cell) for cell in cells):
                continue

            for index, cell in enumerate(cells):
                table_rating = re.fullmatch(
                    r"(?:Rating\s*[:\-]?\s*)?([0-4])",
                    cell,
                    re.IGNORECASE
                )
                if not table_rating:
                    continue

                rating = int(table_rating.group(1))
                if index + 1 < len(cells):
                    justification = re.sub(
                        r"^\s*Justification\s*[:\-]?\s*",
                        "",
                        cells[index + 1],
                        flags=re.IGNORECASE
                    ).strip()
                break

            if rating is not None:
                break

    return {
        "solution": dc_solution,
        "rating": rating,
        "justification": justification
    }

# ==== Counter ====
def get_iteration(solution):
    file = OUTPUT_FOLDER / f"zeroshotImportance{filename_token(solution)}Counter.txt"
    if os.path.exists(file):
        with open(file, "r") as f:
            try:
                return int(f.read().strip()), file
            except:
                return 0, file
    else:
        return 0, file

# ==== Main Execution ====
target_solution = DC_SOLUTIONS[0]  # Update index per run

context = "\n\n".join([load_context(), load_instructions()])
question = f"Evaluate the importance of {target_solution}, considering the provided context and rating scale.\nGive your response in the required format.\n"
full_prompt = f"{context}\n\n{attach_scale(question)}"

iteration, counter_file = get_iteration(target_solution)

results = {}
raw_responses = {}
success_map = {}

for model_name in MODEL_VARIANTS:
    response = query_expert(model_name, full_prompt)
    raw_responses[model_name] = response

    parsed = extract_single_block(response or "", target_solution)
    results[model_name] = [parsed] if parsed["rating"] is not None else []
    success_map[model_name] = parsed["rating"] is not None

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
            for model_name, entries in results.items():
                base_model, role = model_name.split("-", 1)
                for entry in entries:
                    variant_id = f"{base_model}_{role}"
                    row_id = f"{variant_id}_{iteration:02d}_{timestamp}"
                    writer.writerow({
                        "row_id": row_id,
                        "base_model": base_model,
                        "variant_id": variant_id,
                        "model": role,
                        "dc_solution": entry["solution"],
                        "rating": entry["rating"],
                        "label": RATING_LABELS.get(entry["rating"], ""),
                        "iteration": iteration,
                        "timestamp": timestamp,
                        "justification": entry["justification"]
                    })
                    print(f"✅ Saved structured response for: {variant_id}")


        qualifiers = filename_token(target_solution)
        txt_path = RAW_LOG_DIR / f"zeroshotImportance{qualifiers}RawLogIter{iteration:02d}At{filename_timestamp}.txt"
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
    qualifiers = filename_token(target_solution)
    fallback_path = RAW_LOG_DIR / f"zeroshotImportance{qualifiers}FailedAt{filename_timestamp}.json"
    with open(fallback_path, "w", encoding="utf-8") as f:
        json.dump({
            "target_solution": target_solution,
            "responses": raw_responses,
            "structured_results": results,
            "success_map": success_map,
            "error_reason": "One or more variants did not provide a complete or valid response."
        }, f, indent=4, ensure_ascii=False)
    print(f"⚠️ Not all variants returned valid responses. Fallback saved to: {fallback_path}")
