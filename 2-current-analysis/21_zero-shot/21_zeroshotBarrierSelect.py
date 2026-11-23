"""ZERO_SHOT: runtime randomized barrier order, automatic matching, seed 42.

Linux/Bash commands (run in tmux; keep Ollama running in another window):
python3 -u /home/ngirwa/TiiS/second-publication/2-current-analysis/21_zero-shot/21_zeroshotBarrierSelect.py

# Continue until 50 successful iterations:
python3 -u /home/ngirwa/TiiS/second-publication/2-current-analysis/21_zero-shot/21_zeroshotBarrierSelect.py --until-successes 50

A failed iteration retains its prompt/order across retries and restarts.
The launcher stops at the requested successful counter, not an attempt count.
Fixed-seed invalid responses can repeat. Ctrl+C retains the pending order.
CSV: 21_zero-shot-raw/21_zeroshotBarrierSelectResponses.csv
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
OUTPUT_FOLDER = SCRIPT_DIR / "21_zeroshotResponses"
CSV_FOLDER = SCRIPT_DIR / "21_zero-shot-raw"
RAW_LOG_DIR = OUTPUT_FOLDER / "21_zeroshotBarrierSelectRawLogs"
ORDER_LOG_DIR = OUTPUT_FOLDER / "21_zeroshotBarrierSelectOrderLogs"
OUTPUT_CSV = CSV_FOLDER / "21_zeroshotBarrierSelectResponses.csv"
COUNTER_FILE = RAW_LOG_DIR / "21_zeroshotBarrierSelectCounter.txt"

MODEL_VARIANTS = [
    "phi4-generalist", "phi4-normative", "phi4-subject-matter",
    "gemma3-generalist", "gemma3-normative", "gemma3-subject-matter",
    "llama-generalist", "llama-normative", "llama-subject-matter",
    "mistral-generalist", "mistral-normative", "mistral-subject-matter"
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

def context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies.\n"
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
    )

def instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and respond according to your expert role.\n"
        "You must select exactly five (5) barriers from the list provided.\n"
        "Do not select more or fewer than five.\n"
        "⚠️ You must copy and use the barrier entries exactly as listed — including the full label.\n"
        "Do not paraphrase, rephrase, or modify any barrier wording.\n"
        "Only select from the list — do not add new barriers.\n"
        "The list is shown in random order and does not reflect importance.\n"
        "No justification is required. Return only five lines, each starting with '- ' followed by one selected full barrier label. Do not number the selections."
    )

def question(order):
    barrier_list = "\n".join(f"- {BARRIERS[barrier_id]}" for barrier_id in order)
    return f"{context()}\n\n{instructions()}\n\nBarriers:\n{barrier_list}"

def main(until_successes=None):
    runtime.configure(
        OUTPUT_FOLDER, "21_zeroshot", BARRIERS, MODEL_VARIANTS,
        question, condition="ZERO_SHOT", model_seed=SEED,
    )
    CSV_FOLDER.mkdir(parents=True, exist_ok=True)
    runtime.OUTPUT_CSV = OUTPUT_CSV
    runtime.main(until_successes)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--until-successes", type=int, default=None,
                        help="Keep retrying until this successful counter is reached")
    args = parser.parse_args()
    if args.until_successes is not None and args.until_successes < 1:
        parser.error("--until-successes must be positive")
    main(args.until_successes)
