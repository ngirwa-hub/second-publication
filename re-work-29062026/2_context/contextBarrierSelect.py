r"""
1..50 | % { python .\2_context\contextBarrierSelect.py }
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
OUTPUT_FOLDER = SCRIPT_DIR / "contextResponses"
RAW_LOG_DIR = OUTPUT_FOLDER / "contextBarrierSelectRawLogs"
RAW_LOG_DIR.mkdir(parents=True, exist_ok=True)
OUTPUT_CSV = OUTPUT_FOLDER / "contextBarrierSelectResponses.csv"
COUNTER_FILE = OUTPUT_FOLDER / "contextBarrierSelectCounter.txt"

# ==== CONFIGURATION ====
#no phi4
MODEL_VARIANTS = [
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter",
    "phi4-generalist", "phi4-normative", "phi4-subject-matter"
]


#load project context
def load_project():
    return (
    "## Project description\n"
    "- The project is part of the Shift2DC initiative. It focuses on establishing guidelines and roadmaps to enable the widespread adoption of DC solutions, especially medium- and low-voltage DC. The project will develop multiple technologies, including software tools, simulation tools, and hardware solutions such as cables and converters.\n"
    "- Because the project is still early, it has not yet engaged end users extensively. Instead, it examines topics reported in the literature by gathering input from experts in DC systems. Experts are drawn from several areas, including ports, buildings, manufacturing, and utilities, all aligned with the project’s energy and DC expertise.\n"
    "- The project is organized around:\n\n"
    "- An overview of DC grids\n"
    "- Development of tools and IT solution architectures (selected as enablers)\n"
    "- Prototypes (including items with high TRLs)\n"
    "- Simulation tools\n"
    "- Four demonstrations across application areas selected by the commission: data centers, buildings, industry, and ports\n"
    "- The project also includes feasibility studies and recommendations, culminating in DC roadmaps based on the results."
     )


#load demonstration context
def load_demonstration():
    return (
    "Four demonstrators are planned across data centers, buildings, industry, and ports, described as follows\n"
    "## Data center\nA data center demonstrator is planned in Germany. The focus is on edge data centers and how data centers can be integrated, particularly with renewables and heat reuse. The demonstrator also considers powering the office via DC, not only the computing equipment. A micro data center will be tested in Germany with a small installation assembled from data center components. The installation includes vehicle-to-grid (V2G) and solar panels, and it will operate as a real micro data center.\n"
    "## Building\nA residential building demonstrator in France will test how existing DC technologies can be integrated into a DC grid. The buildings demonstrator includes a live setup and focuses on DC-related technologies, including distributed photovoltaic (DPV) systems.\n"
    "## Industry\nTwo industry use cases are planned. One manufacturing demonstrator is located in Aachen, Germany, where technologies will be integrated and large facilities will be connected. Another industry demonstrator is in Phoenix Contact, focusing on accommodating renewables and chargers, with a scale around 100 kW. A separate industry/living-lab setup in Aachen connects two buildings using DC technology.\n"
    "## Port\nA port demonstrator is planned in Funchal. The objective is to assess the feasibility of DC for onshore power supply in port environments. The port demonstrator is small and includes a digital twin to explore scaling of DC in the port. In addition, the demonstrator tests multiple technologies and considers end-user perspectives, including people passing by (e.g., tourists) who may not realize the system is DC. The installation includes approximately 10 kW of PV and 11 kW of V2G chargers." 
 )


def load_elicitation():
    return (
    "## Expert elicitation protocol\n"
    "- The elicitation process is part of the EU-funded Shift2DC project and focuses on users’ perceptions of medium- and low-voltage DC technologies.\n"
    "- Because the project is still in its first year and there are few direct current users at present, the study uses experts instead of relying on user data.\n"
    "- The goal is to gain insights into the challenges and opportunities for medium- and low-voltage DC, including areas where there is still uncertainty and mixed views in the literature.\n"
    "- The protocol is designed to gather qualitative and quantitative input from experts through a set of prepared questions.\n"
    "- The elicitation will be used to document expert opinions, and to converge on areas of agreement and identify areas of disagreement.\n"
    "- The session includes brief introductions, a short overview of the broader project kept general rather than limited to the Shift2DC project, an explanation of the expert elicitation protocol and its process, and concluding remarks.\n"
    "- Topics selected for discussion include barriers to adoption, costs, time, risks of delay, and related general considerations."
    )


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


def instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and respond according to your expert role.\n"
        "You must select exactly five (5) barriers from the list provided.\n"
        "Do not select more or fewer than five (5).\n"
        "⚠️ You must copy and use the barrier entries exactly as listed — including the barrier number and full label.\n"
        "Do not paraphrase, rephrase, or modify any barrier wording.\n"
        "Only select from the list — do not add new barriers.\n"
        "The list is shown in random order and does not reflect importance.\n"
        "No justification is required. List the five selected barriers clearly."
    )

def question():
    barrier_list = "\n".join([f"{k}. {v}" for k, v in BARRIERS.items()])
    context = "\n".join([load_project(), load_demonstration(), load_elicitation(), instructions()])
    return f"{context}\n\nBarriers:\n{barrier_list}"

def query_expert(model, prompt):
    print(f"🔍 Querying: {model}")
    try:
        response = requests.post(
            "http://localhost:11434/api/generate",
            json={"model": model, "prompt": prompt},
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
        else:
            print(f"❌ Error from {model}: {response.status_code}")
            return ""
    except Exception as e:
        print(f"❌ Exception while querying {model}: {e}")
        return ""

def find_best_match(label):
    best_score, best_id, best_label = 0, None, ""
    for k, v in BARRIERS.items():
        score = SequenceMatcher(None, label.lower().strip(), v.lower().strip()).ratio()
        if score > best_score:
            best_score, best_id, best_label = score, k, v
    return best_score, best_id, best_label

def normalize_barrier_label(label):
    normalized = unicodedata.normalize("NFKC", label).casefold()
    normalized = "".join(char if char.isalnum() else " " for char in normalized)
    return " ".join(normalized.split())

def find_best_prefix_match(label, minimum_similarity=0.85, minimum_margin=0.05):
    normalized_label = normalize_barrier_label(label)
    candidates = []

    for barrier_id, official_label in BARRIERS.items():
        normalized_official = normalize_barrier_label(official_label)
        generated_prefix = normalized_label[:len(normalized_official)]
        similarity = SequenceMatcher(
            None, generated_prefix, normalized_official
        ).ratio()
        candidates.append((similarity, barrier_id, official_label))

    candidates.sort(reverse=True, key=lambda candidate: candidate[0])
    best_similarity, best_id, best_label = candidates[0]
    second_similarity = candidates[1][0] if len(candidates) > 1 else 0.0
    is_accepted = (
        best_similarity >= minimum_similarity
        and best_similarity - second_similarity >= minimum_margin
    )
    return is_accepted, best_similarity, best_id, best_label

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
        (
            fallback_accepted,
            similarity,
            suggested_id,
            suggested_label
        ) = find_best_prefix_match(model_label)

        if exact_match is not None:
            barrier_id, official_label = exact_match
            label_status = "matched"
            barrier_id_status = (
                "correct_id"
                if model_barrier_id == barrier_id
                else "corrected_from_label"
            )
        elif fallback_accepted:
            barrier_id = suggested_id
            official_label = suggested_label
            label_status = "matched_by_fuzzy_prefix"
            barrier_id_status = (
                "correct_id_fuzzy_prefix"
                if model_barrier_id == barrier_id
                else "corrected_from_fuzzy_prefix"
            )
        else:
            barrier_id = None
            official_label = ""
            label_status = "unmatched"
            barrier_id_status = "unverified_id"

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

    truncated = len(selected_barriers) > 5
    #if len(selected_barriers) > 5:
        #selected_barriers = selected_barriers[:5]
        #truncated = True

    return selected_barriers, truncated

def main(rounds=1):
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

    for _ in range(rounds):
        run_id = f"contextBarrierSelectRawLogIter{iteration_counter:02d}At{filename_timestamp}"
        all_rows = []
        run_logs = []
        raw_responses = {}
        structured_responses = {}
        all_valid = True

        for variant_name in MODEL_VARIANTS:
            # Parse base_model and role
            if variant_name.startswith("llama"):
                base_model = "llama"
            elif variant_name.startswith("mistral"):
                base_model = "mistral"
            elif variant_name.startswith("gemma3"):
                base_model = "gemma3"
            elif variant_name.startswith("phi4"):
                base_model = "phi4"
            else:
                base_model = "unknown"

            role = variant_name.split("-", 1)[-1]
            print(f"Querying expert: {role} ({base_model})")
            response = query_expert(variant_name, prompt)
            raw_responses[variant_name] = response

            if not response.strip():
                print(f"⚠️ No response from {variant_name}")
                all_valid = False
                continue

            barriers, truncated = extract_barrier_info(response)
            barriers = barriers[:5]

            valid_selection = (
                len(barriers) == 5
                and all(
                    barrier["label_status"] in {
                        "matched", "matched_by_fuzzy_prefix"
                    }
                    for barrier in barriers
                )
                and len({barrier["barrier_id"] for barrier in barriers}) == 5
            )
            if not valid_selection:
                print(f"⚠️ {variant_name} did not return 5 unique, matched barriers in its first 5 entries")
                all_valid = False

            structured_responses[variant_name] = barriers
            variant_id = f"{role}_{base_model}"

            for i, b in enumerate(barriers):
                row_id = f"{variant_id}_{iteration_counter:02d}_b{b['barrier_id']}"
                truncated_flag = "yes" if i >= 5 else "no"
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
                    "is_hallucinated": b["label_status"] == "unmatched",
                    "iteration": iteration_counter,
                    "timestamp": timestamp,
                    "truncated": truncated_flag
                })

            run_logs.append(f"\n--- {variant_id} ---\n{response.strip()}\n")

        # --- File writing happens after all MODEL_VARIANTS processed ---
        with open(os.path.join(RAW_LOG_DIR, f"{run_id}.txt"), "w", encoding="utf-8") as logf:
            logf.write("".join(run_logs))

        if all_valid:
            write_header = not os.path.exists(OUTPUT_CSV)
            with open(OUTPUT_CSV, "a", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(f, fieldnames=[
                    "row_id", "base_model", "variant_id", "model",
                    "barrier_id", "model_barrier_id", "official_label", "model_label",
                    "label_status", "barrier_id_status", "is_hallucinated",
                    "iteration", "timestamp", "truncated"
                ])
                if write_header:
                    writer.writeheader()
                writer.writerows(all_rows)

            iteration_counter += 1
            with open(COUNTER_FILE, "w") as f:
                f.write(str(iteration_counter))
            print(f"\u2705 Run complete. Saved {len(all_rows)} rows.")
        else:
            json_path = RAW_LOG_DIR / f"contextBarrierSelectFailedAt{filename_timestamp}.json"
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump({
                    "question": prompt,
                    "raw_responses": raw_responses,
                    "structured_responses": structured_responses,
                    "error_reason": "One or more variants did not return 5 unique, matched barriers in its first 5 entries."
                }, f, indent=4, ensure_ascii=False)
            print(f"\u26a0\ufe0f Incomplete run. Saved diagnostics to {json_path}")
 
if __name__ == "__main__":
    main(rounds=1)
