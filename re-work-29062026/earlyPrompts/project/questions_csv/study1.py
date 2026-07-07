import pandas as pd
import json

# Load the CSV file
#VALIDATE THE JSON FILE
with open("c:\\Users\\FEEL\\Downloads\\images\\study1.json", "r", encoding="utf-8") as f:
    try:
        json_data = json.load(f)
        print("✅ JSON is valid!")
    except json.JSONDecodeError as e:
        print(f"❌ JSON is invalid: {e}")
csv_file = "c:\\Users\\FEEL\\Downloads\\images\\study1.csv"
data = pd.read_csv(csv_file, encoding="cp1252")

#fill the NAN with empty string
data.fillna("N/A", inplace=True)

# Extract the questions (column headers) and participant answers
questions = data.columns[1:]  # Skip the first column (Participant ID)
participants = data["Participant ID"]

# Structure the data
structured_data = {}
for question in questions:  # Iterate over each question
    structured_data[question] = {
        "question": question,
        "answers": [
            {"participant": participant, "answer": answer}
            for participant, answer in zip(participants, data[question])
        ]
    }

# Save to JSON file
output_file = "c:\\Users\\FEEL\\Downloads\\images\\study1.json"
with open(output_file, "w", encoding="utf-8") as f:
    json.dump(structured_data, f, indent=4, ensure_ascii=False)

print(f"✅ Data successfully saved to {output_file}")