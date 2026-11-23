"""ANCHORED: runtime randomized barrier order, automatic matching, seed 42.

Linux/Bash commands (run in tmux; keep Ollama running in another window):
python3 -u /home/ngirwa/TiiS/second-publication/2-current-analysis/23_anchor/23_anchorBarrierSelect.py --anchor-type ANCHOR_WORD

# Example: 10 successful iterations for one anchor arm:
python3 -u /home/ngirwa/TiiS/second-publication/2-current-analysis/23_anchor/23_anchorBarrierSelect.py --anchor-type ANCHOR_WORD --until-successes 10
# Other arms: ANCHOR_EXAMPLE, ANCHOR_NUM_LOW, ANCHOR_NUM_HIGH.

A failed iteration retains its prompt/order across retries and restarts.
The launcher stops at the requested successful counter, not an attempt count.
Fixed-seed invalid responses can repeat. Ctrl+C retains the pending order.
CSV: 23_anchor-raw/23_anchorBarrierSelectResponses.csv
Counters/pending state: this question's RawLogs subfolder.
Each attempt's actual order, full prompt, matching policy and selections are audited.
"""
import argparse
import os
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR.parent))
import barrier_random_runtime as runtime

SEED = 42
OUTPUT_FOLDER = SCRIPT_DIR / "23_anchorResponses"
CSV_FOLDER = SCRIPT_DIR / "23_anchor-raw"
CSV_FOLDER.mkdir(parents=True, exist_ok=True)
RAW_LOG_DIR = OUTPUT_FOLDER / "23_anchorBarrierSelectRawLogs"
ORDER_LOG_DIR = OUTPUT_FOLDER / "23_anchorBarrierSelectOrderLogs"
OUTPUT_CSV = CSV_FOLDER / "23_anchorBarrierSelectResponses.csv"

MODEL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter"
]

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

def instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and respond according to your expert role.\n"
        "You must select exactly five (5) barriers from the list provided.\n"
        "Do not select more or fewer than five (5).\n"
        "⚠️ You must copy and use the barrier entries exactly as listed — including the full label.\n"
        "Do not paraphrase, rephrase, or modify any barrier wording.\n"
        "Only select from the list — do not add new barriers.\n"
        "The list is shown in random order and does not reflect importance.\n"
        "No justification is required. Return only five lines, each starting with '- ' followed by one selected full barrier label. Do not number the selections."
    )

ANCHORED_ARMS = ["ANCHOR_WORD", "ANCHOR_EXAMPLE", "ANCHOR_NUM_LOW", "ANCHOR_NUM_HIGH"]

def build_barrier_frame(anchor_type: str) -> dict:
    """One-line prime per run + minimal metadata."""
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

def build_prompt(anchor_line: str, order) -> str:
    barrier_list = "\n".join(f"- {BARRIERS[barrier_id]}" for barrier_id in order)
    context = "\n".join([load_project(), load_demonstration(), load_elicitation(), instructions()])
    return f"{context}\n\n{anchor_line}\n\nBarriers:\n{barrier_list}"

def main(until_successes=None, anchor_type=None):
    arm = anchor_type or os.getenv("ANCHOR_TYPE", "ANCHOR_WORD")
    if arm not in ANCHORED_ARMS:
        raise ValueError(f"Unknown ANCHOR_TYPE={arm}")
    frame_meta = build_barrier_frame(arm)
    runtime.configure(
        OUTPUT_FOLDER, "23_anchor", BARRIERS, MODEL_VARIANTS,
        lambda order: build_prompt(frame_meta["frame_text"], order),
        condition="ANCHORED", anchor_type=arm,
        anchor_level=frame_meta["anchor_level"], model_seed=SEED,
    )
    runtime.OUTPUT_CSV = OUTPUT_CSV
    runtime.main(until_successes)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--until-successes", type=int, default=None,
                        help="Keep retrying until this successful counter is reached")
    parser.add_argument("--anchor-type", choices=ANCHORED_ARMS, default=None,
                        help="Overrides ANCHOR_TYPE; default is ANCHOR_WORD")
    args = parser.parse_args()
    if args.until_successes is not None and args.until_successes < 1:
        parser.error("--until-successes must be positive")
    main(args.until_successes, args.anchor_type)
