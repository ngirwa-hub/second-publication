#reserve feasibility with rich in contents
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

#load project context
def load_project():
    return (
        "-The Shift to Direct Current (Shift2DC) project, very briefly is one of the two projects that was accepted in a recent call for DC. "
        "-So our objective is actually of this call was to establishing guidelines for the widespread of the application of DC especially low voltage and medium voltage."
        "-We will develop 30 DC solutions; software tools, simulation tools, also hardware tools like cables and converters. "
        "-We have a few demonstrators like the other project has. This is just a very general overview of the project. "
        "-We are looking at a whole spectrum of the ecosystem from right now we are looking at barriers, opportunities, regulation, and users."
    )
#load demonstration context
def load_demonstration():
    return (
        "-Application areas or demonstrators, of the Shift2DC project are ports, industry, data center, and industry. "
        "-Two of them we are actually tackling in a very physical demonstrator. "
        "-For example, the data centers, we actually have center in Germany where we'll test this micro data center in Germany, where we want to look at mostly edge data centers and how can DC be integrated there, especially from a perspective of integrating renewables and heat reuse, and if we can have the office also powered by DC, not just the actual computer. "
        "-In industry we also have a factory in industry where we will test the technologies. Then we have the buildings and the ports where we are looking more on this. Even buildings we will have a live demonstrator also. "
        "-The port is just a small demonstrator with a few technologies and then a digital twin on top of it that we want to scale the see how it would scale the DC in the ports and the key objective is to test some of the technologies. "
        "-We also want to see to what extent the sea can be an option for onshore power supply. There are several open issues. "
        "-One of the most critical is people talk a lot of the difference in frequency of the vessels that 50 hertz, 60 hertz DC could be a very interesting alternative here but not just that even to power the operations of the ports like the forklifts and the cars electric vehicles if this can be used using a dc microgrid so this will be a digital twin demonstrator so it's more on a simulation hardware in the loop."
        "-So once we have this we're going to talk about the end users what are their perspectives and not just the users of DC but people that actually will be passing by and not knowing it's DC for example tourists in the port they don't know it's a DC system and we will intrigue them knowing what are their opinions about it."
    )
# Load DC solutions context
def load_solutions():
    return (
        "- The proposed direct current (DC) solutions are;\n"
        " 1. Smart and sustainable DC cables: So we have the DC cables are out there. What we want to do is add this sustainable components and smartness. By sustainable we mean the right materials that they last long or at least the same as an AC cable. We also want it to be like if you're using this cable you don't have to learn how to use a new cable. It should be using this not very different from using any in AC cable. So this is one of the few that we are developing and it will probably be demonstrated in all the demos. Not just a cable but a sustainable cable that does not age as fast as the others and with ergonomics that we can use the cable more or less as we use a normal AC cable.\n"
        " 2. DC connectors: These connectors we are developing in the project, there are two types of connectors. The passive connectors where it's more about let's make the cable or the actual connector more resilient like we make it with higher capacity than the load that it will handle or then using micro-electronic technology actually embedded in the connectors that we can withstand higher currents and voltages. They are improving and we will demonstrate in the buildings and in maybe ports. DC connectors which is the idea is to enable the connection of the components to the loads and the grid. \n"
        " 3. Static protection system: a potential solution could involve the development and integration of an advanced fault detection method and ultra-fast protection device that can rapidly identify and isolate DC faults within the microgrid. \n"
        " 4. Semiconductor-based circuit breaker: A solid-state circuit breaker (SSCB) is proposed to quickly and reliably protect DC grids, will enable rapid and reliable interruption of fault currents, overcoming the limitations posed by the absence of natural zero-crossing in DC currents. The SSCB will be capable of detecting and isolating faults within the DC grid following Current/OS and ODCA specifications, ensuring compliance with industry standards. Furthermore, it will be designed to communicate and exchange necessary information with the central controller of the DC grid, facilitating coordinated protection and control. The protection architecture will be further enhanced by incorporating an adaptive protection scheme, which dynamically adjusts to varying grid conditions to ensure optimal performance.\n"
        " 5. Protection DC system design tool: Enable the design of protection system for DC grids\n"
        " 6. DC-DC converter: power flow control between DC appliances. It has a high level of monitoring and controllability. Due to its monitoring capability, the proposed solution enhances the performance and reliability of DC grids by allowing the real-time tracking of key parameters such as voltage, current, and temperature. Thanks to its high controllability, the smart power distribution unit (SPDU) can also contribute to load balancing and peak shaving, improving overall grid stability and ensuring a more resilient and sustainable energy infrastructure.\n"
        " 7. LVAC-LVDC interlink converter: another one that we are developing is for low voltage, a low voltage AC-DC interlink converter. So this is pretty much just to make the bridge between the LV – the AC and the DC worlds in low voltage. This is being developed by one of our partners, so it's not a very big device. It's not a lot of power looking at the 700 DC volts and one of the things that we find important is the built-in droop control on the DC side that we'd like this will help actually all the distribution that we need to do if you have something already at the source this could help us actually minimize the number of devices that we need to install in the DC side.\n"
        " 8. DC measurement device: In this development, Phoenix Contact targets a solution with the named attributes, especially easy retrofit installation, and reproducible measurements at reasonable accuracy without the need for calibration at the client site. So the idea is to create several measuring devices and then combine all the measurements like an ensemble. Such that we can create a device that is robust to noise and that does not require a lot of calibration on the client side. \n"
        " 9. DC solution design tool: this is a more software-based tool, we are aware that there are already a few tools for designing the DC systems, mostly not open source. The idea here is we want to build something that is open source or at least has a community version, maybe not with all the features, but that can be used by the community. And our objective is all these tools that we are developing in the project, if we can actually have models, software models to have in this tool, and support the different rules of these two ecosystems, Current-OS and ODCA, because they are both partners in the project, like they are both collaborating with us. It's very easy to engage in these two entities one important thing is it's about being user friendly, there's a big effort on developing a user interface that we can actually use it without being programmers and even without knowing a lot about these technologies more like a MATLAB or LabVIEW that we can drag and drop blocks, and make life easier. And of course, the idea is to create the outputs of electrical sizing, techno-economic analysis.\n"
        " 10. Network design tool for DC solutions: Integrates static models of some specific DC devices, Supports different DC eco-systems.\n"
        " 11. Solid-state circuit breaker: the idea is we already have some breakers, but here the idea is to make them really fast. At least this is the most important points, the fault detection and the isolation in the level of microseconds, and do it to a level of currents and thermal performance that it's realistic. So we are not looking at more heavy power scenarios that might be a bit unrealistic. One of the things that is interesting is that they want to add this real-time monitoring and communications. It will be more like also an IoT-enabled device, not simply a solid state breaker, but also with monitoring and communication capabilities.\n"
    )

#load elicitation context
def load_elicitation():
    return (
        "- Expert elicitation: So our objective here is to gather your insights on the feasibility, importance, challenges, and opportunities for this DC solutions. "
        "The expert elicitation is actually this technique where we rely on experts. "
        "- We gathered a few topics of interest and we will make questions and you have to provide us some answers and in the end we will write down and try to converge and find some areas of convergence, areas of disagreements."
        
    )

#load_question_context
def load_question():
    return (
        "-Types of questions: So We will be asking more or less four types of questions. "
        "-One will be on the feasibility of the solutions and the project objectives, how feasible you think this is, the importance of each of the DC solutions presented, the barriers to adopt the DC solutions, and then we have a general discussion like for open discussion for questions."
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
def attach_scale(question_text, scale_title="Feasibility Scale"):
    scale = (
        f"\n\n{scale_title}:\n"
        "0- Not able to respond\n"
        "1 - Not feasible\n"
        "2 - Somewhat feasible\n"
        "3 - Feasible\n"
        "4 - Very feasible\n"
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
    context_sections=[
        load_project(),
        load_demonstration(),
        load_solutions(),
        load_elicitation(),
        load_question(),
    ]
    context = "\n\n".join(context_sections)
    print("🔍 Loading project context...")

    instructions = load_instructions()

    base_question = (
        "Based on the context provided.\n"
        "Also considering all the listed DC solutions: smart and sustainable DC cables, DC-DC converters, static protection system, "
        "semiconductor-based circuit breaker, protection DC system design tool, LVAC-LVDC interlink converter, DC measurement device, "
        "DC solution design tool, solid-state circuit breaker, DC-DC connector, and network design tool for DC solutions,\n"
        "How feasible is the use of DC solutions for the target sectors described in the Shift2DC project?\n"
        "Do not provide justification for your choice\n\n"
        "do not assess each solution separately, but rather the overall feasibility of the DC solutions in the context of the Shift2DC project.\n"
    )

    question_with_scale = attach_scale(base_question)
    full_prompt = f"""{context}\n\n{instructions}\n\nQuestion:\n{question_with_scale}"""

    results = run_experts(full_prompt)

    if results:
        output_data = {
            "question": question_with_scale,
            "structured_responses": results
        }
        output_path = os.path.join(OUTPUT_FOLDER, "feasibility.json")
        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(output_data, f, indent=4, ensure_ascii=False)
        print(f"✅ Structured responses saved to: {output_path}")
    else:
        print("⚠️ No structured responses received.")
