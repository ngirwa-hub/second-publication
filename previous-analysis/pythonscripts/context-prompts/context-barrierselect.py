import os
import csv
import requests
import json
import datetime
from difflib import SequenceMatcher

# Folder setup
OUTPUT_FOLDER = "expert_responses"
RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs")
os.makedirs(RAW_LOG_DIR, exist_ok=True)
OUTPUT_CSV = os.path.join(OUTPUT_FOLDER, "context_barrier_choice_all.csv")
COUNTER_FILE = f"context_some_barrierChoice_counter.txt"

# ==== CONFIGURATION ====
#no phi4
MODEL_VARIANTS = [
    "llama-generalist", "llama-generalist2", "llama-normative", "llama-normative2", "llama-subject_matter", "llama-subject_matter2",
    "mistral-generalist", "mistral-generalist2", "mistral-normative", "mistral-normative2", "mistral-subject_matter", "mistral-subject_matter2",
    "gemma3-generalist", "gemma3-generalist2", "gemma3-normative", "gemma3-normative2", "gemma3-subject_matter", "gemma3-subject_matter2"
]


#load project context
def load_project():
    return (
        "- The Shift to Direct Current (Shift2DC) project is one of two initiatives selected under a recent call focused on advancing direct current (DC) technologies.\n"
        "- The objective of this call is to establish guidelines for the widespread application of low and medium voltage DC systems.\n"
        "- The project will deliver 30 DC-related solutions, including software tools, simulation platforms, and hardware components such as cables and converters.\n"
        "- Several demonstrators are planned to test and showcase these solutions in real-world settings.\n"
        "- The project adopts a comprehensive approach, addressing technical barriers, regulatory frameworks, stakeholder engagement, and user perspectives.\n"
    )


#load demonstration context
def load_demonstration():
    return (
        "- The Shift2DC project includes four key demonstration areas: ports, industry, data centers, and buildings.\n"
        "- Two of these areas—data centers and industry—feature physical demonstrators where technologies will be implemented and tested on-site.\n"
        "- The data center demonstration is located in Germany and focuses on edge data centers. It explores how DC can be integrated to support renewable energy use, heat reuse, and powering not only the computing infrastructure but also office spaces.\n"
        "- The industry demonstration involves a functioning factory environment where DC technologies will be piloted.\n"
        "- Live demonstrations will also take place in buildings, while the port demonstration includes a small-scale testbed supported by a digital twin to explore DC scalability in port operations.\n"
        "- In the port use case, one focus is to assess DC as a viable alternative for onshore power supply, especially in light of varying vessel frequency standards (50 Hz vs. 60 Hz).\n"
        "- The port demonstration also considers powering port operations—such as forklifts and electric vehicles—through a DC microgrid using hardware-in-the-loop simulations.\n"
        "- Finally, the project will gather perspectives not only from experts but also from end-user observers, such as tourists, to better understand public awareness and acceptance of DC technologies.\n"
    )


def load_elicitation():
    return (
        "- This expert elicitation aims to collect expert insights on the feasibility, importance, challenges, and opportunities associated with proposed DC solutions.\n"
        "- Expert elicitation is a structured technique that draws on the knowledge and judgment of subject-matter experts to inform complex decision-making.\n"
        "- The process covers a series of predefined topics. Experts are asked to respond to targeted questions, and their responses will be analyzed to identify areas of agreement, divergence, and uncertainty.\n"
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

        similarity, matched_id, matched_label = find_best_match(model_label)
        label_status = "matched" if similarity >= 0.85 else "hallucinated_label"

        if label_status == "hallucinated_label":
            barrier_id_status = "outside_range_id"
            official_label = ""
        else:
            official_label = matched_label
            if barrier_id not in BARRIERS:
                barrier_id_status = "outside_range_id"
            elif barrier_id == matched_id:
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

    for _ in range(rounds):
        run_id = f"context_barrierChoice_{iteration_counter:02d}_{timestamp}"
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

            if len(barriers) < 5:
                print(f"⚠️ {variant_name} returned only {len(barriers)} barriers")
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
                    "official_label": b["official_label"],
                    "model_label": b["model_label"],
                    "label_status": b["label_status"],
                    "barrier_id_status": b["barrier_id_status"],
                    "is_hallucinated": b["label_status"] == "hallucinated_label",
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
                    "barrier_id", "official_label", "model_label",
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
            json_path = os.path.join(RAW_LOG_DIR, f"context_barrierChoice_failed_{timestamp}.json")
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump({
                    "question": prompt,
                    "raw_responses": raw_responses,
                    "structured_responses": structured_responses,
                    "error_reason": "One or more variants returned fewer than 5 barriers."
                }, f, indent=4, ensure_ascii=False)
            print(f"\u26a0\ufe0f Incomplete run. Saved diagnostics to {json_path}")
 
if __name__ == "__main__":
    main(rounds=1)
