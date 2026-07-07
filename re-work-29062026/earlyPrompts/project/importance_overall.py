import os
import requests
import json
import re

# Define your three experts (model aliases in Ollama)
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
        "Be clear, concise, and no need for justification\n"
        "Consider using the provided rating scale."
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

# Extract a single overall rating from free-text response
def extract_single_rating(response_text):
    # This pattern matches exactly: "4 - Very feasible" (even with space or hyphen variations)
    match = re.search(r"^\s*([1-5])\s*[-–:]?\s*(.+)$", response_text.strip(), re.IGNORECASE | re.MULTILINE)

    if match:
        rating = int(match.group(1))
        label = match.group(2).strip().capitalize()
        return {"rating": rating, "label": label}
    else:
        print("⚠️ Rating pattern not matched in response:")
        print(response_text[:100])  # Print first 100 chars for debugging
        return {"rating": None, "label": ""}




# Run all experts in parallel
def run_experts(full_prompt):
    all_structured = {}

    for role, model in EXPERTS.items():
        print(f"\n🔍 Querying {role} expert...")
        response = query_expert(model, full_prompt)

        if response:
            # DEBUG: Print the raw output before parsing
            print(f"\n[DEBUG] Raw response from {role}:\n{response[:2000]}...\n")

            extracted = extract_single_rating(response)
            all_structured[role] = extracted

            if not extracted:
                print(f"⚠️ No structured data extracted for {role}.")
        else:
            print(f"⚠️ No response from {role} expert.")

    return all_structured


# MAIN EXECUTION
if __name__ == "__main__":
    context = load_context()
    instructions = load_instructions()

    base_question = (
        "Considering all the listed DC solutions: smart and sustainable DC cables, DC-DC converters, static protection system, "
        "semiconductor-based circuit breaker, protection DC system design tool, LVAC-LVDC interlink converter, DC measurement device, "
        "DC solution design tool, solid-state circuit breaker, DC-DC connector, and network design tool for DC solutions,\n"
        "How important is the use of DC solutions for the target sectors described in the Shift2DC project?\n"
        "Do not provide justification for your choice\n\n"
        "do not assess each solution separately, but rather the overall importance of the DC solutions in the context of the Shift2DC project.\n"
    )

    question_with_scale = attach_scale(base_question)
    full_prompt = f"""{context}\n\n{instructions}\n\nQuestion:\n{question_with_scale}"""

    results = run_experts(full_prompt)

    if results:
        output_data = {
            "question": question_with_scale,
            "structured_responses": results
        }
        output_path = os.path.join(OUTPUT_FOLDER, "importance_overall6.json")
        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(output_data, f, indent=4, ensure_ascii=False)
        print(f"✅ Structured responses saved to: {output_path}")
    else:
        print("⚠️ No structured responses received.")
