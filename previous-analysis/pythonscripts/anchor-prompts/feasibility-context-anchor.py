import os
import requests
import json
import re
import datetime
import csv

# Timestamp for filenames
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

# ---- ALL VARIANTS ----
ALL_VARIANTS = [
    "phi4-generalist", "phi4-generalist2", "phi4-normative", "phi4-normative2", "phi4-subject_matter", "phi4-subject_matter2",
    "llama-generalist", "llama-generalist2", "llama-normative", "llama-normative2", "llama-subject_matter", "llama-subject_matter2",
    "mistral-generalist", "mistral-generalist2", "mistral-normative", "mistral-normative2", "mistral-subject_matter", "mistral-subject_matter2",
    "gemma3-generalist", "gemma3-generalist2", "gemma3-normative", "gemma3-normative2", "gemma3-subject_matter", "gemma3-subject_matter2",
]

#load project context
def load_project():
    return (
        "- The XXXXXX project is one of two initiatives selected under a recent call focused on advancing direct current (DC) technologies.\n"
        "- The objective of this call is to establish guidelines for the widespread application of low and medium voltage DC systems.\n"
        "- The project will deliver 30 DC-related solutions, including software tools, simulation platforms, and hardware components such as cables and converters.\n"
        "- Several demonstrators are planned to test and showcase these solutions in real-world settings.\n"
        "- The project adopts a comprehensive approach, addressing technical barriers, regulatory frameworks, stakeholder engagement, and user perspectives.\n"
    )


#load demonstration context
def load_demonstration():
    return (
        "- The XXXXXX project includes four key demonstration areas: ports, industry, data centers, and buildings.\n"
        "- Two of these areas—data centers and industry—feature physical demonstrators where technologies will be implemented and tested on-site.\n"
        "- The data center demonstration is located in XXXXXX and focuses on edge data centers. It explores how DC can be integrated to support renewable energy use, heat reuse, and powering not only the computing infrastructure but also office spaces.\n"
        "- The industry demonstration involves a functioning factory environment where DC technologies will be piloted.\n"
        "- Live demonstrations will also take place in buildings, while the port demonstration includes a small-scale testbed supported by a digital twin to explore DC scalability in port operations.\n"
        "- In the port use case, one focus is to assess DC as a viable alternative for onshore power supply, especially in light of varying vessel frequency standards (50 Hz vs. 60 Hz).\n"
        "- The port demonstration also considers powering port operations—such as forklifts and electric vehicles—through a DC microgrid using hardware-in-the-loop simulations.\n"
        "- Finally, the project will gather perspectives not only from experts but also from end-users observers, such as tourists, to better understand public awareness and acceptance of DC technologies.\n"
    )


def load_elicitation():
    return (
        "- This expert elicitation aims to collect expert insights on the feasibility, importance, challenges, and opportunities associated with proposed DC solutions.\n"
        "- Expert elicitation is a structured technique that draws on the knowledge and judgment of subject-matter experts to inform complex decision-making.\n"
        "- The process covers a series of predefined topics. Experts are asked to respond to targeted questions, and their responses will be analyzed to identify areas of agreement, divergence, and uncertainty.\n"
    )


def load_solutions():
    return (
        "- The XXXXXX project includes 30 proposed DC solutions. This expert elicitation focuses on 11 of them, described as follows:\n"
        " 1. Smart and sustainable DC cables: These are designed with durable, environmentally friendly materials, ensuring comparable or improved longevity relative to AC cables. The cables prioritize ergonomic use, mimicking standard AC cable handling to simplify adoption across all demonstration sites.\n"
        " 2. DC connectors: The project is developing two types of connectors—passive and smart. Passive connectors offer enhanced physical robustness, while smart connectors incorporate microelectronics to handle higher voltages and currents. These will be demonstrated in buildings and potentially in ports.\n"
        " 3. Static protection system: This solution includes advanced fault detection and ultra-fast protection devices that can rapidly identify and isolate DC faults within microgrids.\n"
        " 4. Semiconductor-based circuit breaker: Designed to overcome the absence of natural current zero-crossing in DC systems, this breaker ensures rapid fault interruption. It complies with Current/OS and ODCA standards and incorporates adaptive protection schemes, real-time monitoring, and communication with central controllers.\n"
        " 5. Protection DC system design tool: A software tool that facilitates the design and simulation of protection systems for DC grids.\n"
        " 6. DC-DC converter: Enables power flow control among DC appliances. The converter features real-time monitoring of voltage, current, and temperature, as well as load balancing and peak shaving functionalities for improved grid performance.\n"
        " 7. LVAC-LVDC interlink converter: A low-voltage AC-DC converter designed to bridge AC and DC systems. It includes built-in droop control to reduce the number of downstream devices needed in the DC architecture.\n"
        " 8. DC measurement device: Developed by Phoenix Contact, this solution enables easy retrofit installation and accurate, reproducible measurements without requiring on-site calibration. It aggregates multiple sensors into a single robust measurement system.\n"
        " 9. DC solution design tool: A user-friendly, possibly open-source software tool supporting the design of DC systems. It accommodates models for technologies in the XXXXXX project and aligns with both Current-OS and ODCA frameworks. The tool emphasizes drag-and-drop usability and supports electrical sizing and techno-economic analysis.\n"
        "10. Network design tool for DC solutions: A simulation tool that integrates static models of key DC devices and supports multiple DC ecosystem configurations.\n"
        "11. Solid-state circuit breaker: A high-speed circuit breaker capable of fault detection and isolation in microseconds. It emphasizes thermal and current handling realism and includes real-time monitoring and IoT-enabled communication features.\n"
    )

# Load general instructions
def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Read the context carefully. Evaluate overall feasibility of the DC solutions.\n"
        "Please consider the provided context and answer according to your expert role.\n"
        "You MUST rate overall feasibility using this 0–4 scale and do not provide justification.\n"
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

#attach scale
#scale
def attach_scale(question_text, scale_title="Feasibility Scale"):
    scale = (
        f"\n\n{scale_title}:\n"
        "0 - Not able to respond\n"
        "1 - Not feasible\n"
        "2 - Somewhat feasible\n"
        "3 - Feasible\n"
        "4 - Very feasible\n"
    )
    return question_text + scale

def get_iteration(counter_file):
    if os.path.exists(counter_file):
        with open(counter_file, "r") as f:
            try:
                return int(f.read().strip())
            except:
                return 0
    else:
        return 0

def increment_counter(counter_file, current_iter):
    with open(counter_file, "w") as f:
        f.write(str(current_iter + 1))

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

def extract_single_rating(response_text):
    lines = [line.strip() for line in response_text.strip().splitlines() if line.strip()]
    for line in lines:
        match = re.match(
            r"^\s*([0-4])\s*[-–:]?\s*(Very feasible|Feasible|Somewhat feasible|Not feasible|Not able to respond)\b.*$",
            line, re.IGNORECASE
        )

        if match:
            rating = int(match.group(1))
            label = match.group(2).strip().capitalize()
            return {"rating": rating, "label": label}
    return {"rating": None, "label": ""}

# === BIASED ARMS===
BIASED_ARMS = ["BIAS_WORD", "BIAS_EXAMPLE", "BIAS_NUM_LOW", "BIAS_NUM_HIGH"]

def build_feasibility_frame(bias_type: str) -> dict:
    """
    Returns the single frame line to inject and minimal metadata.
    Using general DC microgrids phrasing.
    """
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

if __name__ == "__main__":
    # --- choose one biased arm (run one arm per process) ---
    ARM = os.getenv("BIAS_TYPE", "BIAS_WORD")
    assert ARM in BIASED_ARMS, f"Unknown BIAS_TYPE={ARM}. Choose one of {BIASED_ARMS}"

    # --- compose context and question (unchanged) ---
    context = "\n\n".join([
        load_project(),
        load_demonstration(),
        load_solutions(),
        load_elicitation(),
        load_instructions()
    ])
    question = (
        "Question: How feasible is the use of DC solutions for the target sectors described in the XXXXXX project?\n"
        "Key considerations:\n"
        "- Consider all the listed DC solutions within the context of the XXXXXX project.\n"
        "- Provide an overall assessment of the feasibility of these DC solutions in the target sectors.\n"
        "⚠️- Use ONLY the provided rating scale <0–4> (the feasibility Scale).\n"
        "- Do not provide justification for your choice.\n"
        "- Focus on the overall feasibility rather than assessing each solution individually.\n"
    )

    # --- per-arm counter (so runs stay balanced across iterations) ---
    iteration_counter_file = f"feas_{ARM}_counter.txt"
    iteration_counter = get_iteration(iteration_counter_file)

    # --- build and inject the single frame line (general DC microgrids) ---
    frame_meta = build_feasibility_frame(ARM)
    full_prompt = f"{context}\n\n{frame_meta['frame_text']}\n\n{attach_scale(question)}"

    # --- outputs ---
    OUTPUT_FOLDER = "expert_responses"
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, f"raw_logs_{ARM}")
    os.makedirs(RAW_LOG_DIR, exist_ok=True)
    MASTER_CSV = os.path.join(OUTPUT_FOLDER, "feasibility_biased_all.csv")

    # --- query all variants ---
    raw_responses, results = {}, {}
    for variant in ALL_VARIANTS:
        response = query_expert(variant, full_prompt)
        raw_responses[variant] = response
        results[variant] = extract_single_rating(response) if response else {"rating": None, "label": ""}

    all_success = all(r["rating"] is not None for r in results.values())

    if all_success:
        # raw log per arm+iteration
        raw_log_path = os.path.join(RAW_LOG_DIR, f"feas_{ARM}_iter{iteration_counter:02d}_{timestamp}.txt")
        with open(raw_log_path, "w", encoding="utf-8") as f:
            for v, raw in raw_responses.items():
                f.write(f"\n--- {v} ---\n{(raw or '').strip()}\n")

        # CSV header (no demo columns)
        header = [
            "row_id","base_model","variant_id","model",
            "rating","label","iteration","timestamp","raw_response",
            "condition","bias_type","anchor_level"
        ]
        if not os.path.exists(MASTER_CSV):
            with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
                csv.DictWriter(f, fieldnames=header).writeheader()

        # append rows
        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=header)
            for variant, res in results.items():
                base_model, role = variant.split("-", 1)
                variant_id = f"{base_model}_{role}"
                row_id = f"{ARM}_{base_model}_{role}_{iteration_counter:02d}_{timestamp}"

                w.writerow({
                    "row_id": row_id,
                    "base_model": base_model,
                    "variant_id": variant_id,
                    "model": role,
                    "rating": res["rating"],
                    "label": res["label"] or "",
                    "iteration": iteration_counter,
                    "timestamp": timestamp,
                    "raw_response": raw_responses.get(variant, ""),
                    "condition": "BIASED",
                    "bias_type": ARM,
                    "anchor_level": frame_meta["anchor_level"]
                })

        increment_counter(iteration_counter_file, iteration_counter)
        print(f"✅ {ARM}: all succeeded. Counter → {iteration_counter + 1}. Appended → {MASTER_CSV}")

    else:
        fallback_path = os.path.join(OUTPUT_FOLDER, f"feasibility_{ARM}_failed_{timestamp}.json")
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({"structured_responses": results, "raw_responses": raw_responses},
                      f, indent=4, ensure_ascii=False)
        print(f"⚠️ {ARM}: some variants invalid. Fallback → {fallback_path}")
