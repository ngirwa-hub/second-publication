r"""
$env:ANCHOR_TYPE="ANCHOR_WORD"; 1..10 | % { python .\3_anchor\anchorFeasibility.py }

$env:ANCHOR_TYPE="ANCHOR_EXAMPLE"; 1..10 | % { python .\3_anchor\anchorFeasibility.py }

$env:ANCHOR_TYPE="ANCHOR_NUM_LOW"; 1..10 | % { python .\3_anchor\anchorFeasibility.py }

$env:ANCHOR_TYPE="ANCHOR_NUM_HIGH"; 1..10 | % { python .\3_anchor\anchorFeasibility.py }

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

# --- outputs ---
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_FOLDER = SCRIPT_DIR / "anchorResponses"
RAW_LOG_DIR = OUTPUT_FOLDER / "anchorFeasibilityRawLogs"
RAW_LOG_DIR.mkdir(parents=True, exist_ok=True)
MASTER_CSV = OUTPUT_FOLDER / "anchorFeasibilityResponses.csv"

# ---- ALL VARIANTS ----
ALL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter"
]

#load project context
def load_project():
    return (
    "## Project description\n"
    "- The project is part of the XXXXXX initiative. It focuses on establishing guidelines and roadmaps to enable the widespread adoption of DC solutions, especially medium- and low-voltage DC. The project will develop multiple technologies, including software tools, simulation tools, and hardware solutions such as cables and converters.\n"
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
    "## Data center\nA data center demonstrator is planned in XXXXXX. The focus is on edge data centers and how data centers can be integrated, particularly with renewables and heat reuse. The demonstrator also considers powering the office via DC, not only the computing equipment. A micro data center will be tested in Germany with a small installation assembled from data center components. The installation includes vehicle-to-grid (V2G) and solar panels, and it will operate as a real micro data center.\n"
    "## Building\nA residential building demonstrator in XXXXXX will test how existing DC technologies can be integrated into a DC grid. The buildings demonstrator includes a live setup and focuses on DC-related technologies, including distributed photovoltaic (DPV) systems.\n"
    "## Industry\nTwo industry use cases are planned. One manufacturing demonstrator is located in XXXXXX, XXXXXX, where technologies will be integrated and large facilities will be connected. Another industry demonstrator is in Phoenix Contact, focusing on accommodating renewables and chargers, with a scale around 100 kW. A separate industry/living-lab setup in Aachen connects two buildings using DC technology.\n"
    "## Port\nA port demonstrator is planned in XXXXXX. The objective is to assess the feasibility of DC for onshore power supply in port environments. The port demonstrator is small and includes a digital twin to explore scaling of DC in the port. In addition, the demonstrator tests multiple technologies and considers end-user perspectives, including people passing by (e.g., tourists) who may not realize the system is DC. The installation includes approximately 10 kW of PV and 11 kW of V2G chargers." 
 )


def load_elicitation():
    return (
    "## Expert elicitation protocol\n"
    "- The elicitation process is part of the XX-funded XXXXXX project and focuses on users’ perceptions of medium- and low-voltage DC technologies.\n"
    "- Because the project is still in its first year and there are few direct current users at present, the study uses experts instead of relying on user data.\n"
    "- The goal is to gain insights into the challenges and opportunities for medium- and low-voltage DC, including areas where there is still uncertainty and mixed views in the literature.\n"
    "- The protocol is designed to gather qualitative and quantitative input from experts through a set of prepared questions.\n"
    "- The elicitation will be used to document expert opinions, and to converge on areas of agreement and identify areas of disagreement.\n"
    "- The session includes brief introductions, a short overview of the broader project kept general rather than limited to the XXXXXX project, an explanation of the expert elicitation protocol and its process, and concluding remarks.\n"
    "- Topics selected for discussion include barriers to adoption, costs, time, risks of delay, and related general considerations."
    )


def load_solutions():
    return (
        "- The XXXXXX project includes 30 proposed DC solutions. This expert elicitation focuses on 11 of them, described as follows:\n"
        "1. Smart and sustainable DC cables: These are designed with durable, environmentally friendly materials, ensuring comparable or improved longevity relative to AC cables. The cables prioritize ergonomic use, mimicking standard AC cable handling to simplify adoption across all demonstration sites.\n"
        " 2. DC connectors: The project is developing two types of connectors—passive and smart. Passive connectors offer enhanced physical robustness, while smart connectors incorporate microelectronics to handle higher voltages and currents. n"
        " 3. Static protection system: This solution includes advanced fault detection and ultra-fast protection devices that can rapidly identify and isolate DC faults within microgrids.\n"
        " 4. Semiconductor-based circuit breaker: Designed to overcome the absence of natural current zero-crossing in DC systems, this breaker ensures rapid fault interruption. It complies with Current/OS and ODCA standards and incorporates adaptive protection schemes, real-time monitoring, and communication with central controllers.\n"
        " 5. Protection DC system design tool: A software tool that facilitates the design and simulation of protection systems for DC grids.\n"
        " 6. DC-DC converter: Enables power flow control among DC appliances. The converter features real-time monitoring of voltage, current, and temperature, as well as load balancing and peak shaving functionalities for improved grid performance.\n"
        " 7. LVAC-LVDC interlink converter: A low-voltage AC-DC converter designed to bridge AC and DC systems. It includes built-in droop control to reduce the number of downstream devices needed in the DC architecture.\n"
        " 8. DC measurement device: Developed by XXXXXX XXXXXX, this solution enables easy retrofit installation and accurate, reproducible measurements without requiring on-site calibration. It aggregates multiple sensors into a single robust measurement system.\n"
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

# === ANCHORED ARMS===
ANCHORED_ARMS = ["ANCHOR_WORD", "ANCHOR_EXAMPLE", "ANCHOR_NUM_LOW", "ANCHOR_NUM_HIGH"]

def build_feasibility_frame(anchor_type: str) -> dict:
    """
    Returns the single frame line to inject and minimal metadata.
    Using general DC microgrids phrasing.
    """
    meta = {"anchor_type": anchor_type, "anchor_level": "", "frame_text": ""}

    if anchor_type == "ANCHOR_WORD":
        meta["frame_text"] = (
            "Context: European deployments and standardization efforts indicate workable LV/MV-DC microgrids across sectors."
        )

    elif anchor_type == "ANCHOR_EXAMPLE":
        meta["frame_text"] = (
            "Example: DC microgrids integrating PV, storage, and DC loads (e.g., ±380 V DC bus) are reported in operational testbeds."
        )

    elif anchor_type == "ANCHOR_NUM_LOW":
        meta["anchor_level"] = "low"
        meta["frame_text"] = (
            "Some peer-reviewed studies report ~2–6% distribution-efficiency improvement with DC microgrids under efficient baselines."
        )

    elif anchor_type == "ANCHOR_NUM_HIGH":
        meta["anchor_level"] = "high"
        meta["frame_text"] = (
            "Other analyses report ~10–18% distribution-efficiency improvement in modeled DC microgrid scenarios with PV and storage."
        )

    return meta

if __name__ == "__main__":
    # --- choose one biased arm (run one arm per process) ---
    ARM = os.getenv("ANCHOR_TYPE", "ANCHOR_WORD")
    assert ARM in ANCHORED_ARMS, f"Unknown ANCHOR_TYPE={ARM}. Choose one of {ANCHORED_ARMS}"

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
    arm_token = filename_token(ARM)
    iteration_counter_file = OUTPUT_FOLDER / f"anchorFeasibility{arm_token}Counter.txt"
    iteration_counter = get_iteration(iteration_counter_file)

    # --- build and inject the single frame line (general DC microgrids) ---
    frame_meta = build_feasibility_frame(ARM)
    full_prompt = f"{context}\n\n{frame_meta['frame_text']}\n\n{attach_scale(question)}"

    # --- query all variants ---
    raw_responses, results = {}, {}
    for variant in ALL_VARIANTS:
        response = query_expert(variant, full_prompt)
        raw_responses[variant] = response
        results[variant] = extract_single_rating(response) if response else {"rating": None, "label": ""}

    all_success = all(r["rating"] is not None for r in results.values())

    if all_success:
        # raw log per arm+iteration
        raw_log_path = RAW_LOG_DIR / f"anchorFeasibility{arm_token}RawLogIter{iteration_counter:02d}At{filename_timestamp}.txt"
        with open(raw_log_path, "w", encoding="utf-8") as f:
            for v, raw in raw_responses.items():
                f.write(f"\n--- {v} ---\n{(raw or '').strip()}\n")

        # CSV header (no demo columns)
        header = [
            "row_id","base_model","variant_id","model",
            "rating","label","iteration","timestamp","raw_response",
            "condition","anchor_type","anchor_level"
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
                    "condition": "ANCHORED",
                    "anchor_type": ARM,
                    "anchor_level": frame_meta["anchor_level"]
                })

        increment_counter(iteration_counter_file, iteration_counter)
        print(f"✅ {ARM}: all succeeded. Counter → {iteration_counter + 1}. Appended → {MASTER_CSV}")

    else:
        fallback_path = RAW_LOG_DIR / f"anchorFeasibility{arm_token}FailedAt{filename_timestamp}.json"
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({"structured_responses": results, "raw_responses": raw_responses},
                      f, indent=4, ensure_ascii=False)
        print(f"⚠️ {ARM}: some variants invalid. Fallback → {fallback_path}")
