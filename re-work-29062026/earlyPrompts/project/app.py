from flask import Flask, render_template, request, jsonify
import os
import json
import re
from datetime import datetime
import requests

app = Flask(__name__)

# Define your six model variants
EXPERTS = {
    "generalist": "generalist",
    "generalist2": "generalist",
    "normative": "normative",
    "normative2": "normative",
    "sme": "subject_matter",
    "sme2": "subject_matter"
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
        "1 - Not important\n"
        "2 - Somewhat important\n"
        "3 - Important\n"
        "4 - Very important\n"
        "5 - Not able to respond\n"
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

# Extract structured ratings and justifications from free-text response
def extract_ratings_and_justifications(response_text):
    structured = []
    blocks = re.split(r"\n\s*\d{1,2}\.\s+\*\*(.*?)\*\*", response_text)

    for i in range(1, len(blocks), 2):
        solution = blocks[i].strip()
        body = blocks[i + 1] if i + 1 < len(blocks) else ""

        # Try multiple rating patterns
        rating_match = re.search(r"(Importance(?: Rating)?|Rating)\s*[:\-]?\s*(\d)", body, re.IGNORECASE)
        rating = int(rating_match.group(2)) if rating_match else None

        # Try multiple justification/reasoning patterns
        justification_match = re.search(r"(Justification|Reasoning)\s*[:\-]?\s*(.*?)(?=(\\n\\s*\\n|\\Z))", body, re.IGNORECASE | re.DOTALL)
        justification = justification_match.group(2).strip() if justification_match else body.strip()

        if solution and rating:
            structured.append({
                "solution": solution,
                "rating": rating,
                "justification": justification
            })

    return structured

# Query all experts and process their responses
def run_experts(full_prompt):
    all_structured = {}

    for role, model in EXPERTS.items():
        print(f"\n🔍 Querying {role} expert...")
        response = query_expert(model, full_prompt)

        if response:
            # DEBUG: Print the raw output before parsing
            print(f"\n[DEBUG] Raw response from {role}:\n{response[:2000]}...\n")

            extracted = extract_ratings_and_justifications(response)
            all_structured[role] = extracted

            if not extracted:
                print(f"⚠️ No structured data extracted for {role}.")
        else:
            print(f"⚠️ No response from {role} expert.")

    return all_structured

# Save responses to a JSON file
def save_responses_to_json(question, responses):
    # Create a unique filename based on the current timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(OUTPUT_FOLDER, f"responses_{timestamp}.json")

    # Save the question and responses to the JSON file
    data = {
        "question": question,
        "structured_responses": responses
    }
    with open(filename, "w", encoding="utf-8") as json_file:
        json.dump(data, json_file, indent=4, ensure_ascii=False)

    print(f"✅ Structured responses saved to: {filename}")
    return filename

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/prompt', methods=['POST'])
def prompt():
    data = request.json
    prompt_text = data.get('prompt')
    if not prompt_text:
        return jsonify({"error": "Prompt is required"}), 400

    # Load context and instructions
    context = load_context()
    instructions = load_instructions()

    # Attach the rating scale to the question
    question_with_scale = attach_scale(prompt_text)
    full_prompt = f"""{context}\n\n{instructions}\n\nQuestion:\n{question_with_scale}"""

    # Query all experts
    results = run_experts(full_prompt)

    # Save the results to a JSON file
    if results:
        filename = save_responses_to_json(question_with_scale, results)
        return jsonify({"message": "Responses saved successfully", "file": filename, "responses": results})
    else:
        return jsonify({"error": "No structured responses received"}), 500

if __name__ == '__main__':
    app.run(debug=True)