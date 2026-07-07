import os
import requests
import json
import re

# Define your model variants (model aliases in Ollama)
EXPERTS = {
    "generalist": "generalist:latest",
    "generalist2": "generalist2:latest",
    "normative": "normative:latest",
    "normative2": "normative2:latest",
    "subject_matter": "subject_matter:latest",
    "subject_matter2": "subject_matter2:latest"
}

# Ensure the output folder exists
OUTPUT_FOLDER = "expert_responses"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)

# Load shared elicitation context
def load_context():
    return (
        "- The expert elicitation is based on the Shift2DC project, which aims to accelerate the transition to a sustainable energy system by promoting the use of direct current (DC) technologies. "
        "- The project focuses on the development and demonstration of DC solutions in four sectors: ports, industry, data centers, and buildings."
        "- The proposed direct current (DC) solutions are;\n"
        " 1. Smart and sustainable DC cables\n"
        " 2. DC connectors\n"
        " 3. Static protection system\n"
        " 4. Semiconductor-based circuit breaker\n"
        " 5. Protection DC system design tool\n"
        " 6. DC-DC converter\n"
        " 7. LVAC-LVDC interlink converter\n"
        " 8. DC measurement device\n"
        " 9. DC solution design tool\n"
        " 10. Network design tool for DC solutions\n"
        " 11. Solid-state circuit breaker\n"
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
        "0 - Not able to respond\n"
        "1 - Not important\n"
        "2 - Somewhat important\n"
        "3 - Important\n"
        "4 - Very important\n" 
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

# Check if all 11 DC solutions are addressed
def validate_replies(response_text):
    dc_solutions = [
        "smart and sustainable DC cables",
        "DC connectors",
        "static protection system",
        "semiconductor-based circuit breaker",
        "protection DC system design tool",
        "DC-DC converter",
        "LVAC-LVDC interlink converter",
        "DC measurement device",
        "DC solution design tool",
        "network design tool for DC solutions",
        "solid-state circuit breaker"
    ]

    # Check if each solution is mentioned in the response
    for solution in dc_solutions:
        if solution.lower() not in response_text.lower():
            return False
    return True

# Format the raw response to ensure one DC solution per line

def format_response(response_text):
    # Remove extra newlines and ensure one DC solution per line
    # Add a newline before each numbered DC solution (e.g., "1.", "2.", etc.)
    formatted_response = re.sub(r"\s*\n*(\d+\.\s)", r"\n\1", response_text)
    return formatted_response.strip()

# MAIN EXECUTION
if __name__ == "__main__":
    context = load_context()
    instructions = load_instructions()

    base_question = (
        "Considering all the listed DC solutions: smart and sustainable DC cables, DC-DC converters, static protection system, "
        "semiconductor-based circuit breaker, protection DC system design tool, LVAC-LVDC interlink converter, DC measurement device, "
        "DC solution design tool, solid-state circuit breaker, DC-DC connector, and network design tool for DC solutions,\n"
        "evaluate how important each of these innovations are for the target sectors described in the Shift2DC project."
    )

    question_with_scale = attach_scale(base_question)
    full_prompt = f"""{context}\n\n{instructions}\n\nQuestion:\n{question_with_scale}"""

    # Ask the user to select a model
    print("Available models:")
    for role in EXPERTS.keys():
        print(f"- {role}")
    selected_role = input("\nEnter the role of the model you want to query (e.g., 'generalist'): ").strip()

    if selected_role not in EXPERTS:
        print(f"⚠️ Invalid role: {selected_role}")
    else:
        model_name = EXPERTS[selected_role]
        print(f"\n🔍 Querying the '{selected_role}' model ({model_name})...")
        response = query_expert(model_name, full_prompt)

    if response:
        print(f"\n[DEBUG] Raw response from {selected_role}:\n{response[:2000]}...\n")

        # Validate the response
        if validate_replies(response):
            # Format the response for better readability
            formatted_response = format_response(response)

        # Ensure actual newlines are preserved
            formatted_response = formatted_response.replace("\\n", "\n")

            output_data = {
                "question": question_with_scale,
                "raw_response": formatted_response
            }
            output_path = os.path.join(OUTPUT_FOLDER, f"importance_{selected_role}.json")
            with open(output_path, "w", encoding="utf-8") as f:
                json.dump(output_data, f, indent=4, ensure_ascii=False)
            print(f"✅ Valid response saved to: {output_path}")
        else:
            print(f"⚠️ The response from '{selected_role}' did not address all 11 DC solutions. Ignoring the response.")