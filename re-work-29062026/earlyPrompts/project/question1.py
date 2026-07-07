# question.py – Expert Elicitation Framework using phi-4 and Ollama

import requests
import json
from datetime import datetime

OLLAMA_URL = "http://localhost:11434/api/generate"
MODEL_NAME = "phi"

# --- CONTEXTUAL KNOWLEDGE ---
def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies. "
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
        "- The proposed direct current (DC) solutions are;\n 1. Smart and sustainable DC cables: Cables to provide the interface between DC sources and loads, conforms Current-OS specifications, consider aspects such as electrothermal aging, ergonomics, and environmental impact;\n 2. DC connectors: DC connectors manage safe connection and disconnection in DC grids, using active or passive arc extinguishing methods to prevent damage and ensure user safety;\n 3. Static protection system: Provides ultra-fast protection for DC microgrids, quickly isolating faults and ensuring system reliability using advanced detection and solid-state breakers;\n 4. semiconductor-based circuit breaker: A solid-state circuit breaker to quickly and reliably protect DC grids, overcoming the challenge of no natural zero-crossing in DC currents;\n 5. Protection DC system design tool: Enable the design of protection system for DC grids;\n 6. DC-DC converter: Power-flow-control between DC appliances;\n 7. LVAC-LVDC interlink converter: Active-front-end with droop-control capabilities on the DC side;\n 8. DC measurement device: Current measurements up to 1000 A DC, Voltage measurement up to 1500 V DC;\n 9. DC solution design tool: Evaluate different DC architectures and compare with conventional AC radial networks;\n 10. Network design tool for DC solutions: Integrates static models of some specific DC devices, Supports different DC ecosystems;\n 11. Solid-state circuit breaker: Real-time monitoring and communication capabilities, adaptive protection scheme, adjusting to varying grid conditions.\n"
    )

# --- RESPONSE INSTRUCTIONS ---
def load_instructions():
    return (
        "Please answer each question using the background information provided.\n"
        "Respond in a way that reflects your assigned role (generalist, normative analyst, or subject-matter expert).\n"
        "When rating importance, use the following scale:\n"
        "- Not important\n"
        "- Somewhat important\n"
        "- Important\n"
        "- Very important\n"
    )

# --- PROMPT BUILDER ---
def build_prompt(question, perspective, context, instructions):
    persona_roles = {
        "generalist": "You are a generalist assistant with broad knowledge across disciplines.",
        "normative": "You are a policy-oriented assistant who considers ethical, legal, and societal perspectives.",
        "expert": "You are a technical subject-matter expert in renewable energy and DC technologies."
    }

    role = persona_roles.get(perspective, "You are a helpful assistant.")

    return f"""{role}

Background:
{context}

Instructions:
{instructions}

Question:
{question}

Answer:"""

# --- TEMPERATURE CONTROL PER AGENT ---
def get_temperature(perspective):
    return {
        "generalist": 0.7,
        "normative": 0.8,
        "expert": 0.65
    }.get(perspective, 0.7)

# --- QUERY OLLAMA ---
def query_phi(prompt, perspective):
    temperature = get_temperature(perspective)
    response = requests.post(
        OLLAMA_URL,
        json={
            "model": MODEL_NAME,
            "prompt": prompt,
            "temperature": temperature,
            "stream": False
        }
    )
    return response.json().get("response", "").strip()

# --- MAIN EXECUTION ---
def main():
    context = load_context()
    instructions = load_instructions()

    questions = [
        "How important is HVDC for long-distance renewable energy transmission?",
        "What are the key barriers to implementing DC microgrids in rural regions?",
        "How feasible is LVDC deployment in residential buildings?"
    ]

    perspectives = ["generalist", "normative", "expert"]
    all_results = []

    for question in questions:
        result = {
            "question": question,
            "responses": {}
        }
        for perspective in perspectives:
            prompt = build_prompt(question, perspective, context, instructions)
            response = query_phi(prompt, perspective)
            result["responses"][perspective] = response

        all_results.append(result)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_file = f"elicitation_results_{timestamp}.json"

    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(all_results, f, indent=2, ensure_ascii=False)

    print(f"Results saved to {output_file}")


if __name__ == "__main__":
    main()
