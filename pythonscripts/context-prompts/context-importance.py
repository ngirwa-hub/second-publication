import os
import requests
import json
import re
import datetime
import csv

# ==== Setup ====
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

EXPERTS = {
    "generalist": "generalist",
    "generalist2": "generalist2",
    "normative": "normative",
    "normative2": "normative2",
    "subject_matter": "subject_matter",
    "subject_matter2": "subject_matter2"
}

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

OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "context_importance_all.csv")

RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs")
os.makedirs(RAW_LOG_DIR, exist_ok=True)

base_model = "phi4"  # Change this base model
target_solution = DC_SOLUTIONS[10]  # Update index per run

# ==== Context ====
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
        "- Finally, the project will gather perspectives not only from experts but also from end-users observers, such as tourists, to better understand public awareness and acceptance of DC technologies.\n"
    )

# Load DC solutions context
def load_solutions():
    return (
        "- The Shift2DC project includes 30 proposed DC solutions. This expert elicitation focuses on 11 of them, described as follows:\n"
        " 1. Smart and sustainable DC cables: These are designed with durable, environmentally friendly materials, ensuring comparable or improved longevity relative to AC cables. The cables prioritize ergonomic use, mimicking standard AC cable handling to simplify adoption across all demonstration sites.\n"
        " 2. DC connectors: The project is developing two types of connectors—passive and smart. Passive connectors offer enhanced physical robustness, while smart connectors incorporate microelectronics to handle higher voltages and currents. These will be demonstrated in buildings and potentially in ports.\n"
        " 3. Static protection system: This solution includes advanced fault detection and ultra-fast protection devices that can rapidly identify and isolate DC faults within microgrids.\n"
        " 4. Semiconductor-based circuit breaker: Designed to overcome the absence of natural current zero-crossing in DC systems, this breaker ensures rapid fault interruption. It complies with Current/OS and ODCA standards and incorporates adaptive protection schemes, real-time monitoring, and communication with central controllers.\n"
        " 5. Protection DC system design tool: A software tool that facilitates the design and simulation of protection systems for DC grids.\n"
        " 6. DC-DC converter: Enables power flow control among DC appliances. The converter features real-time monitoring of voltage, current, and temperature, as well as load balancing and peak shaving functionalities for improved grid performance.\n"
        " 7. LVAC-LVDC interlink converter: A low-voltage AC-DC converter designed to bridge AC and DC systems. It includes built-in droop control to reduce the number of downstream devices needed in the DC architecture.\n"
        " 8. DC measurement device: Developed by Phoenix Contact, this solution enables easy retrofit installation and accurate, reproducible measurements without requiring on-site calibration. It aggregates multiple sensors into a single robust measurement system.\n"
        " 9. DC solution design tool: A user-friendly, possibly open-source software tool supporting the design of DC systems. It accommodates models for technologies in the Shift2DC project and aligns with both Current-OS and ODCA frameworks. The tool emphasizes drag-and-drop usability and supports electrical sizing and techno-economic analysis.\n"
        "10. Network design tool for DC solutions: A simulation tool that integrates static models of key DC devices and supports multiple DC ecosystem configurations.\n"
        "11. Solid-state circuit breaker: A high-speed circuit breaker capable of fault detection and isolation in microseconds. It emphasizes thermal and current handling realism and includes real-time monitoring and IoT-enabled communication features.\n"
    )

#load elicitation context
def load_elicitation():
    return (
        "- This expert elicitation aims to collect expert insights on the feasibility, importance, challenges, and opportunities associated with proposed DC solutions.\n"
        "- Expert elicitation is a structured technique that draws on the knowledge and judgment of subject-matter experts to inform complex decision-making.\n"
        "- The process covers a series of predefined topics. Experts are asked to respond to targeted questions, and their responses will be analyzed to identify areas of agreement, divergence, and uncertainty.\n"
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
    blocks = response_text.strip().split("\n\n")
    for block in blocks:
        lines = [line.strip() for line in block.strip().splitlines()]
        if len(lines) == 3 and lines[0].lower() == dc_solution.lower():
            rating_match = re.search(r"Rating\s*[:\-]?\s*([0-4])", lines[1], re.IGNORECASE)
            just_match = re.search(r"Justification\s*[:\-]?\s*(.*)", lines[2], re.IGNORECASE)
            return {
                "solution": dc_solution,
                "rating": int(rating_match.group(1)) if rating_match else None,
                "justification": just_match.group(1).strip() if just_match else ""
            }
    return {"solution": dc_solution, "rating": None, "justification": ""}

# ==== Counter ====
def get_iteration(solution, base_model):
    file = f"context_imp_counter_{solution.replace(' ', '_')}_{base_model}.txt"
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

iteration, counter_file = get_iteration(target_solution, base_model)

results = {}
raw_responses = {}
success_map = {}

for role, model in EXPERTS.items():
    response = query_expert(model, full_prompt)
    raw_responses[role] = response

    if target_solution.lower() in response.lower():
        parsed = extract_single_block(response, target_solution)
        if parsed["rating"] is None:
            print(f"⚠️ {role} included the solution name but did not follow the required format.")
        results[role] = [parsed]
        success_map[role] = parsed["rating"] is not None
    else:
        print(f"⛔ {role} did not mention the target solution: '{target_solution}'")
        results[role] = []
        success_map[role] = False

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
            for model, entries in results.items():
                for entry in entries:
                    variant_id = f"{base_model}_{model}"
                    row_id = f"{variant_id}_{iteration:02d}_{timestamp}"
                    writer.writerow({
                        "row_id": row_id,
                        "base_model": base_model,
                        "variant_id": variant_id,
                        "model": model,
                        "dc_solution": entry["solution"],
                        "rating": entry["rating"],
                        "label": RATING_LABELS.get(entry["rating"], ""),
                        "iteration": iteration,
                        "timestamp": timestamp,
                        "justification": entry["justification"]
                    })
                    print(f"✅ Saved structured response for: {variant_id}")

        txt_path = os.path.join(RAW_LOG_DIR, f"context_{base_model}_{target_solution.replace(' ', '_').lower()}_iter{iteration:02d}_{timestamp}.txt")
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
    fallback_path = os.path.join(OUTPUT_FOLDER, f"context_importance_failed_{timestamp}.json")
    with open(fallback_path, "w", encoding="utf-8") as f:
        json.dump({
            "target_solution": target_solution,
            "responses": raw_responses,
            "structured_results": results,
            "success_map": success_map,
            "error_reason": "One or more variants did not provide a complete or valid response."
        }, f, indent=4, ensure_ascii=False)
    print(f"⚠️ Not all variants returned valid responses. Fallback saved to: {fallback_path}")
