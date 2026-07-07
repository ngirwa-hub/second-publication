import json
import os
from datetime import datetime

# List of model variants
models = ["generalist2", "generalist", "normative2", "normative", "subject_matter2", "subject_matter"]

def prompt_all_models(prompt_text):
    # Simulate interaction with all models (replace with actual API calls)
    responses = []
    for model_name in models:
        response = {
            "model": model_name,
            "prompt": prompt_text,
            "reply": f"Simulated reply from {model_name} for prompt: {prompt_text}"
        }
        responses.append(response)

    # Save the responses to a JSON file
    save_responses_to_json(prompt_text, responses)

    return responses

def save_responses_to_json(prompt_text, responses):
    # Create a directory for saving JSON files if it doesn't exist
    output_dir = "output"
    os.makedirs(output_dir, exist_ok=True)

    # Generate a unique filename based on the current timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(output_dir, f"responses_{timestamp}.json")

    # Save the prompt and responses to the JSON file
    data = {
        "prompt": prompt_text,
        "responses": responses
    }
    with open(filename, "w") as json_file:
        json.dump(data, json_file, indent=4)

    print(f"Responses saved to {filename}")