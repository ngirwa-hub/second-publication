import os
import csv
import requests
import json
import datetime
import re
from difflib import SequenceMatcher

# =========================
# Paths & constants
# =========================
OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)
RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs_barriers")
os.makedirs(RAW_LOG_DIR, exist_ok=True)
OUTPUT_CSV = os.path.join(OUTPUT_FOLDER, "barriers_biased_all.csv")  # <— single merged CSV for all biased arms

timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

# =========================
# Model variants
# =========================
MODEL_VARIANTS = [
    "phi4-generalist", "phi4-generalist2", "phi4-normative", "phi4-normative2", "phi4-subject_matter", "phi4-subject_matter2",
    "llama-generalist", "llama-generalist2", "llama-normative", "llama-normative2", "llama-subject_matter", "llama-subject_matter2",
    "mistral-generalist", "mistral-generalist2", "mistral-normative", "mistral-normative2", "mistral-subject_matter", "mistral-subject_matter2",
    "gemma3-generalist", "gemma3-generalist2", "gemma3-normative", "gemma3-normative2", "gemma3-subject_matter", "gemma3-subject_matter2"
]

# =========================
# Bias arms (general DC microgrids)
# =========================
BIASED_ARMS = ["BIAS_WORD", "BIAS_EXAMPLE", "BIAS_NUM_LOW", "BIAS_NUM_HIGH"]

def build_barrier_frame(bias_type: str) -> dict:
    """One-line prime per run + minimal metadata."""
    meta = {"bias_type": bias_type, "anchor_level": "", "frame_text": ""}
    if bias_type == "BIAS_WORD":
        meta["frame_text"] = (
            "Context: European deployments and standardization efforts indicate workable LV/MV-DC microgrids across sectors."
        )
    elif bias_type == "BIAS_EXAMPLE":
        meta["frame_text"] = (
            "Example: DC microgrids integrating PV, storage, and DC loads (e.g., ±380 V DC bus) are reported in operational testbeds."
        )
    elif bias_type == "BIAS_NUM_LOW":
        meta["anchor_level"] = "low"
        meta["frame_text"] = (
            "Some peer-reviewed studies report ~2–6% distribution-efficiency improvement with DC microgrids under efficient baselines."
        )
    elif bias_type == "BIAS_NUM_HIGH":
        meta["anchor_level"] = "high"
        meta["frame_text"] = (
            "Other analyses report ~10–18% distribution-efficiency improvement in modeled DC microgrid scenarios with PV and storage."
        )
    return meta

# =========================
# Context blocks (same as neutral)
# =========================
def load_project():
    return (
        "- The Shift to Direct Current (Shift2DC) project is one of two initiatives selected under a recent call focused on advancing direct current (DC) technologies.\n"
        "- The objective of this call is to establish guidelines for the widespread application of low and medium voltage DC systems.\n"
        "- The project will deliver 30 DC-related solutions, including software tools, simulation platforms, and hardware components such as cables and converters.\n"
        "- Several demonstrators are planned to test and showcase these solutions in real-world settings.\n"
        "- The project adopts a comprehensive approach, addressing technical barriers, regulatory frameworks, stakeholder engagement, and user perspectives.\n"
    )

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

# =========================
# Barriers list
# =========================
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

def build_prompt(bias_line: str) -> str:
    barrier_list = "\n".join([f"{k}. {v}" for k, v in BARRIERS.items()])
    context = "\n".join([load_project(), load_demonstration(), load_elicitation(), instructions()])
    return f"{context}\n\n{bias_line}\n\nBarriers:\n{barrier_list}"

# =========================
# Query & parse
# =========================
def query_expert(model, prompt):
    print(f"🔍 Querying: {model}")
    try:
        r = requests.post(
            "http://localhost:11434/api/generate",
            json={"model": model, "prompt": prompt},
            stream=True,
            timeout=120
        )
        if r.status_code == 200:
            full = ""
            for line in r.iter_lines():
                if line:
                    obj = json.loads(line.decode("utf-8"))
                    full += obj.get("response", "")
            return full
        else:
            print(f"❌ Error from {model}: {r.status_code}")
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
    """
    Parse lines like:
      '1. high costs of DC solutions'
      '3) lack of pilot projects'
      '7 - incompatibility of DC systems components'
      '10: uncertain regulatory roadmap'
    Returns (selected, truncated_flag). Does NOT truncate.
    """
    selected = []
    seen_ids = set()
    truncated = False

    for rawline in response_text.splitlines():
        line = rawline.strip()
        if not line:
            continue

        m = re.match(r"^\s*(\d{1,2})\s*[\.\)\-:–—]\s*(.+?)\s*$", line)
        if not m:
            continue

        try:
            barrier_id = int(m.group(1))
        except ValueError:
            continue

        model_label = m.group(2).strip()
        if barrier_id not in VALID_BARRIER_IDS:
            sim, mid, mlabel = find_best_match(model_label)
            label_status = "matched" if sim >= 0.85 else "hallucinated_label"
            barrier_id_status = "outside_range_id"
            official_label = mlabel if label_status == "matched" else ""
        else:
            sim, mid, mlabel = find_best_match(model_label)
            if sim >= 0.85:
                label_status = "matched"
                official_label = mlabel
                barrier_id_status = "correct_id" if mid == barrier_id else "wrong_id_matchable"
            else:
                label_status = "hallucinated_label"
                official_label = ""
                barrier_id_status = "correct_id"

        # de-duplicate by barrier_id (keep first occurrence)
        if barrier_id in seen_ids:
            continue
        seen_ids.add(barrier_id)

        selected.append({
            "barrier_id": barrier_id,
            "model_label": model_label,
            "official_label": official_label,
            "label_status": label_status,
            "barrier_id_status": barrier_id_status,
            "similarity": float(sim)
        })

    # mark run as 'truncated' if more than 5 were provided, but DO NOT trim
    if len(selected) > 5:
        truncated = True

    return selected, truncated
# =========================
# Main
# =========================
def main(rounds=1):
    # arm selection
    ARM = os.getenv("BIAS_TYPE", "BIAS_WORD")
    assert ARM in BIASED_ARMS, f"Unknown BIAS_TYPE={ARM}. Choose one of {BIASED_ARMS}"
    frame_meta = build_barrier_frame(ARM)

    # counter per arm (keeps runs isolated)
    counter_file = f"barriers_{ARM}_counter.txt"
    if os.path.exists(counter_file):
        try:
            with open(counter_file, "r") as f:
                iteration_counter = int(f.read().strip())
        except ValueError:
            iteration_counter = 0
    else:
        iteration_counter = 0

    # prompt with bias framing
    prompt = build_prompt(frame_meta["frame_text"])

    for _ in range(rounds):
        run_id = f"barriers_{ARM}_{iteration_counter:02d}_{timestamp}"
        all_rows = []
        run_logs = []
        raw_responses = {}
        structured_responses = {}
        #all_valid = True

        for variant_name in MODEL_VARIANTS:
            # base & role
            base_model, role = variant_name.split("-", 1)

            print(f"Querying expert: {role} ({base_model})")
            response = query_expert(variant_name, prompt)
            raw_responses[variant_name] = response

            if not (response or "").strip():
                print(f"⚠️ No response from {variant_name}")
                all_valid = False
                continue

            barriers, truncated = extract_barrier_info(response)

            if len(barriers) < 5:
                print(f"⚠️ {variant_name} returned only {len(barriers)} barriers after parsing")
                all_valid = False

            structured_responses[variant_name] = barriers
            variant_id = f"{base_model}_{role}"

            for i, b in enumerate(barriers):
                row_id = f"{ARM}_{variant_id}_{iteration_counter:02d}_b{b['barrier_id']}_{timestamp}"
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
                    "similarity": round(b.get("similarity", 0.0), 3),
                    "iteration": iteration_counter,
                    "timestamp": timestamp,
                    "truncated": "yes" if i >= 5 else "no",
                    "condition": "BIASED",
                    "bias_type": ARM,
                    "anchor_level": frame_meta["anchor_level"]
                })

            run_logs.append(f"\n--- {variant_name} ---\n{(response or '').strip()}\n")

        # write raw transcript
        with open(os.path.join(RAW_LOG_DIR, f"{run_id}.txt"), "w", encoding="utf-8") as logf:
            logf.write("".join(run_logs))

        # write CSV if at least one model failed? Your call. Here: only write if ALL valid.
        if all_valid:
            write_header = not os.path.exists(OUTPUT_CSV)
            header = [
                "row_id","base_model","variant_id","model",
                "barrier_id","official_label","model_label",
                "label_status","barrier_id_status","is_hallucinated", "similarity",
                "iteration","timestamp","truncated",
                "condition","bias_type","anchor_level"
            ]
            with open(OUTPUT_CSV, "a", newline="", encoding="utf-8") as f:
                w = csv.DictWriter(f, fieldnames=header)
                if write_header:
                    w.writeheader()
                w.writerows(all_rows)

            # bump counter
            iteration_counter += 1
            with open(counter_file, "w") as f:
                f.write(str(iteration_counter))
            print(f"✅ Run complete. Saved {len(all_rows)} rows → {OUTPUT_CSV}")
        else:
            # diagnostics
            json_path = os.path.join(RAW_LOG_DIR, f"barriers_{ARM}_failed_{timestamp}.json")
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump({
                    "prompt": prompt,
                    "raw_responses": raw_responses,
                    "structured_responses": structured_responses,
                    "error_reason": "One or more variants returned fewer than 5 barriers after parsing."
                }, f, indent=4, ensure_ascii=False)
            print(f"⚠️ Incomplete run. Diagnostics → {json_path}")

if __name__ == "__main__":
    main(rounds=1)
