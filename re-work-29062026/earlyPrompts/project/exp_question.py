import os
import requests
import json

# Define your three experts (model aliases in Ollama)
EXPERTS = {
    "generalist": "generalist",
    "normative": "normative",
    "sme": "subject_matter"
}

# Ensure the output folder exists
OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)

# Load shared elicitation context
def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies. "
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
        "- The proposed direct current (DC) solutions are;\n 1. Smart and sustainable DC cables: Cables to provide the interface between DC sources and loads, conforms Current-OS specifications, consider aspects such as electrothermal aging, ergonomics, and environmental impact;\n 2. DC connectors: DC connectors manage safe connection and disconnection in DC grids, using active or passive arc extinguishing methods to prevent damage and ensure user safety;\n 3. Static protection system: Provides ultra-fast protection for DC microgrids, quickly isolating faults and ensuring system reliability using advanced detection and solid-state breakers;\n 4. semiconductor-based circuit breaker: A solid-state circuit breaker to quickly and reliably protect DC grids, overcoming the challenge of no natural zero-crossing in DC currents;\n 5. Protection DC system design tool: Enable the design of protection system for DC grids;\n 6. DC-DC converter: Power-flow-control between DC appliances;\n 7. LVAC-LVDC interlink converter: Active-front-end with droop-control capabilities on the DC side;\n 8. DC measurement device: Current measurements up to 1000 A DC, Voltage measurement up to 1500 V DC;\n 9. DC solution design tool: Evaluate different DC architectures and compare with conventional AC radial networks;\n 10. Network design tool for DC solutions: Integrates static models of some specific DC devices, Supports different DC ecosystems;\n 11. Solid-state circuit breaker: Real-time monitoring and communication capabilities, adaptive protection scheme, adjusting to varying grid conditions.\n"
    )

# Load general instructions
def load_instructions():
    return (
        "You are participating in an expert elicitation exercise.\n"
        "Please consider the provided context and answer according to your expert role.\n"
        "Be clear, concise, and justify your reasoning.\n"
        "Use the provided rating scale when applicable."
    )

# Rating scale to be attached per question
def attach_scale(question_text, scale_title="Importance Scale"):
    scale = (
        f"\n\n{scale_title}:\n"
        "1 - Not important\n"
        "2 - Somewhat important\n"
        "3 - Important\n"
        "4 - Very important\n"
        "5 - Not able to respond\n\n"

    )
    return question_text + scale

# Query a single expert via Ollama
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

# Run all experts in parallel
def run_experts(full_prompt):
    results = {}
    for role, model in EXPERTS.items():
        print(f"Querying {role} expert...")
        response = query_expert(model, full_prompt)
        if response:
            results[role] = response
        else:
            print(f"⚠️ No response from {role} expert.")
    return results

# MAIN EXECUTION
if __name__ == "__main__":
    context = load_context()
    instructions = load_instructions()

    base_question = (
    "Considering all the listed DC solutions: smart and sustainable DC cables, DC-DC converters, static protection system, semiconductor-based circuit breaker, protection DC system design tool, LVAC-LVDC interlink converter, DC measurement device, DC solution design tool, solid-state circuit breaker, DC-DC connector, and network design tool for DC solutions,\n"
    "evaluate how important these innovations are for the target sectors described in the Shift2DC project."
    )

    question_with_scale = attach_scale(base_question)

    # Combine context, instructions, and question into final prompt
    full_prompt = f"""{context}\n\n{instructions}\n\nQuestion:\n{question_with_scale}"""

    # Get responses from all experts
    responses = run_experts(full_prompt)

    # Save to file
    if responses:
        output_data = {
            "question": question_with_scale,
            "responses": responses
        }
        output_path = os.path.join(OUTPUT_FOLDER, "import_question1.json")
        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(output_data, f, indent=4, ensure_ascii=False)
        print(f"✅ Responses saved to: {output_path}")
    else:
        print("⚠️ No responses received — something might have gone wrong.")
