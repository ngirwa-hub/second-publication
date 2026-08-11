r"""
1..50 | % { python .\2_context\contextImportance.py }
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
OUTPUT_FOLDER = SCRIPT_DIR / "contextResponses"
OUTPUT_FOLDER.mkdir(parents=True, exist_ok=True)
MASTER_CSV = OUTPUT_FOLDER / "contextImportanceResponses.csv"

RAW_LOG_DIR = OUTPUT_FOLDER / "contextImportanceRawLogs"
RAW_LOG_DIR.mkdir(parents=True, exist_ok=True)

target_solution = DC_SOLUTIONS[0]  # Update index per run

# ==== Context ====
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

# Load DC solutions context
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

    rating_match = re.search(
        r"^\s*Rating\s*[:\-]?\s*([0-4])\b",
        response_text,
        re.IGNORECASE | re.MULTILINE
    )
    if rating_match:
        rating = int(rating_match.group(1))

    if rating is None:
        inline_rating_match = re.search(
            rf"^\s*(?:[-*]\s*)?\*{{0,2}}\s*"
            rf"{re.escape(dc_solution)}\s*\*{{0,2}}\s*[:\-]\s*"
            r"Rating\s*[:\-]?\s*([0-4])\b",
            response_text,
            re.IGNORECASE | re.MULTILINE
        )
        if inline_rating_match:
            rating = int(inline_rating_match.group(1))

    if rating is None:
        solution_rating_match = re.search(
            rf"^\s*(?:[-*]\s*)?\*{{0,2}}\s*"
            rf"{re.escape(dc_solution)}\s*\*{{0,2}}\s*[:\-]\s*"
            r"([0-4])\b",
            response_text,
            re.IGNORECASE | re.MULTILINE
        )
        if solution_rating_match:
            rating = int(solution_rating_match.group(1))

    justification_match = re.search(
        r"^\s*Justification\s*[:\-]?\s*(.+)$",
        response_text,
        re.IGNORECASE | re.MULTILINE
    )
    if justification_match:
        justification = justification_match.group(1).strip()

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
    file = OUTPUT_FOLDER / f"contextImportance{filename_token(solution)}Counter.txt"
    if os.path.exists(file):
        with open(file, "r") as f:
            try:
                return int(f.read().strip()), file
            except:
                return 0, file
    else:
        return 0, file



context = "\n\n".join([load_project(), load_demonstration(), load_solutions(), load_elicitation(), load_instructions()])
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
        txt_path = RAW_LOG_DIR / f"contextImportance{qualifiers}RawLogIter{iteration:02d}At{filename_timestamp}.txt"
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
    fallback_path = RAW_LOG_DIR / f"contextImportance{qualifiers}FailedAt{filename_timestamp}.json"
    with open(fallback_path, "w", encoding="utf-8") as f:
        json.dump({
            "target_solution": target_solution,
            "responses": raw_responses,
            "structured_results": results,
            "success_map": success_map,
            "error_reason": "One or more variants did not provide a complete or valid response."
        }, f, indent=4, ensure_ascii=False)
    print(f"⚠️ Not all variants returned valid responses. Fallback saved to: {fallback_path}")
