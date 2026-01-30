import os
import requests
import json
import re
import datetime
import csv

# Timestamp for filenames
timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

# Define expert variants
EXPERTS = {
    "generalist": "generalist",
    "generalist2": "generalist2",
    "normative": "normative",
    "normative2": "normative2",
    "subject_matter": "subject_matter",
    "subject_matter2": "subject_matter2"
}

# Output paths
OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)
RAW_LOG_DIR = os.path.join(OUTPUT_FOLDER, "raw_logs")
os.makedirs(RAW_LOG_DIR, exist_ok=True)
MASTER_CSV = os.path.join(OUTPUT_FOLDER, "context_feasibility_all.csv")

#base model
base_model = "phi4"  # Change this to your desired base model

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


def load_elicitation():
    return (
        "- This expert elicitation aims to collect expert insights on the feasibility, importance, challenges, and opportunities associated with proposed DC solutions.\n"
        "- Expert elicitation is a structured technique that draws on the knowledge and judgment of subject-matter experts to inform complex decision-making.\n"
        "- The process covers a series of predefined topics. Experts are asked to respond to targeted questions, and their responses will be analyzed to identify areas of agreement, divergence, and uncertainty.\n"
    )


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

# Load general instructions
def load_instructions():
    return (
        "You are participating in an expert elicitation exercise."
        "Read the context carefully. Evaluate overall feasibility of the DC solutions.\n"
        "Please consider the provided context and answer according to your expert role.\n"
        "You MUST rate overall feasibility using this 0–4 scale and do not provide justification."
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

def query_expert(model, question):
    response = requests.post(
        "http://localhost:11434/api/generate",
        json={"model": model, "prompt": question},
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
        print(f"Error from model {model}: {response.status_code}")
        return ""

# Define allowed labels and mapping
RATING_LABELS = {
    0: "Not able to respond",
    1: "Not feasible",
    2: "Somewhat feasible",
    3: "Feasible",
    4: "Very feasible"
}
LABEL_TO_RATING = {v.lower(): k for k, v in RATING_LABELS.items()}

def extract_single_rating(response_text):
    lines = [line.strip() for line in response_text.strip().splitlines() if line.strip()]
    rating_label_matches = []
    label_only_matches = []

    # First pass: search for "X - Label" pattern in all lines
    for line in lines:
        match = re.match(
            r"^\s*([0-4])\s*[-–:]?\s*(Very feasible|Feasible|Somewhat feasible|Not feasible|Not able to respond)\s*$",
            line,
            re.IGNORECASE
        )
        if match:
            rating = int(match.group(1))
            label = match.group(2).strip().capitalize()
            rating_label_matches.append({"rating": rating, "label": label})

    # If structured match found, return the last one
    if rating_label_matches:
        return rating_label_matches[-1]

    # Second pass: fallback — look for known label only
    for line in lines:
        label_match = re.search(
            r"\b(Very feasible|Feasible|Somewhat feasible|Not feasible|Not able to respond)\b",
            line,
            re.IGNORECASE
        )
        if label_match:
            label = label_match.group(1).strip().capitalize()
            rating = LABEL_TO_RATING.get(label.lower())
            label_only_matches.append({"rating": rating, "label": label})

    # If label-only match found, return the last one
    if label_only_matches:
        return label_only_matches[-1]

    # If nothing found
    return {"rating": None, "label": ""}

raw_responses = {}
def run_experts(full_prompt):
    all_structured = {}
    for role, model in EXPERTS.items():
        print(f"\n🔍 Querying {role} expert...")
        response = query_expert(model, full_prompt)
        raw_responses[role] = response
        if response:
            extracted = extract_single_rating(response)
            all_structured[role] = extracted
        else:
            print(f"⚠️ No response from {role} expert.")
            all_structured[role] = {"rating": None, "label": ""}
    return all_structured

# Counter file for tracking iterations
iteration_counter_file = f"context_feasibility_counter_{base_model}.txt"
if os.path.exists(iteration_counter_file):
    with open(iteration_counter_file, "r") as f:
        try:
            iteration_counter = int(f.read().strip())
        except ValueError:
            print("⚠️ Invalid counter value in file, starting from 0.")
            iteration_counter = 0
else:
    print("⚠️ No counter file found, starting from 0.")
    iteration_counter = 0

# Ensure master CSV exists with header
if not os.path.exists(MASTER_CSV):
    with open(MASTER_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["row_id", "variant_id", "base_model", "model", "rating", "label", "iteration", "timestamp", "raw_response"])
        writer.writeheader()

# Main execution
if __name__ == "__main__":
    context_sections = [load_project(), load_demonstration(), load_solutions(), load_elicitation(), load_instructions()]
    context = "\n\n".join(context_sections)

    base_question = (
        "Question: How feasible is the use of DC solutions for the target sectors described in the Shift2DC project?\n"
        "Key considerations:\n"
        "- Consider all the listed DC solutions within the context of the Shift2DC project.\n"
        "- Provide an overall assessment of the feasibility of these DC solutions in the target sectors.\n"
        "⚠️- Use ONLY the provided rating scale <0–4> (the feasibility Scale).\n"
        "- Do not provide justification for your choice.\n"
        "- Focus on the overall feasibility rather than assessing each solution individually.\n"
        )
    question_with_scale = attach_scale(base_question)
    full_prompt = f"{context}\n\nQuestion:\n{question_with_scale}"

    results = run_experts(full_prompt)

    all_success = all(result["rating"] is not None for result in results.values())

    if all_success:
        
        # Save raw responses as .txt file for this run
        raw_log_path = os.path.join(RAW_LOG_DIR, f"context_feasibility_raw_{base_model}_iter{iteration_counter:02d}_{timestamp}.txt")
        with open(raw_log_path, "w", encoding="utf-8") as f:
            for model, raw in raw_responses.items():
                f.write(f"\n--- {model} ---\n{raw.strip()}\n")
        print(f"📝 Raw responses saved to: {raw_log_path}")

        # Append to master CSV
        with open(MASTER_CSV, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=["row_id", "variant_id", "base_model", "model", "rating", "label", "iteration", "timestamp", "raw_response"])
            for model, result in results.items():
                variant_id = f"{base_model}_{model}"
                row_id = f"{variant_id}_{iteration_counter:02d}_{timestamp}"
                writer.writerow({
                    "row_id": row_id,
                    "variant_id": variant_id,
                    "base_model": base_model,
                    "model": model,
                    "rating": result["rating"],
                    "label": result["label"],
                    "iteration": iteration_counter,
                    "timestamp": timestamp,
                    "raw_response": raw_responses.get(model, "")
                })
        print(f"✅ Results appended to: {MASTER_CSV}")
        # Increment and store counter
        iteration_counter += 1
        with open(iteration_counter_file, "w") as f:
            f.write(str(iteration_counter))
    else:
        # Save full JSON if any model failed
        fallback_path = os.path.join(RAW_LOG_DIR, f"context_feasibility_failed_{timestamp}.json")
        with open(fallback_path, "w", encoding="utf-8") as f:
            json.dump({
                "question": question_with_scale,
                "structured_responses": results,
                "raw_responses": raw_responses
            }, f, indent=4, ensure_ascii=False)
        print(f"⚠️ Incomplete responses saved to: {fallback_path}")

