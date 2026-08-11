#!/usr/bin/env python3
"""
Script target: cleaning transcript-derived context sections with through an OpenAI API call.

This script reads the raw context blocks from: transcription.md

It extracts the three top-level sections:
    - elicitation_process
    - project_description
    - demonstrator_description

For each section, it asks an OpenAI model to:
    - restore punctuation and sentence boundaries
    - correct grammar and disfluencies
    - lightly paraphrase into clear text
    - preserve factual meaning without inventing details

Outputs:
    - cleaned_contexts.json
    - cleaned_contexts.md

Both files are written under:
    second-publication/meetingTranscript/cleaned-context/

# run command:
/Users/HP/Documents/.venv312/bin/python /Users/HP/Documents/second-publication/meetingTranscript/text-cleaning.py --runs 3

"""

from __future__ import annotations

import argparse
import datetime
import json
import os
import re
import sys
from pathlib import Path
from typing import Dict, List

from openai import OpenAI
from pydantic import BaseModel, Field


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_INPUT_PATH = SCRIPT_DIR / "transcription.md"
DEFAULT_OUTPUT_DIR = SCRIPT_DIR / "cleaned-context"
DEFAULT_ENV_PATH = Path("/Users/HP/Documents/apis/.env.openai")
DEFAULT_TEMPERATURE = 0.2

SECTION_ORDER = [
    "elicitation_process",
    "project_description",
    "demonstrator_description",
]


class CleanedSection(BaseModel):
    title: str = Field(description="The original top-level section title.")
    cleaned_text: str = Field(
        description=(
            "Cleaned markdown text for the section. Keep it concise, factual, and "
            "publication-ready. Use bullet points when that improves clarity. "
            "If the section contains demonstrator subheadings, preserve them."
        )
    )


SYSTEM_PROMPT = """You are cleaning transcript-derived research context.

Your task is editorial cleanup only. Do not analyze, expand, or add new facts.

Requirements:
1. Restore punctuation, sentence boundaries, capitalization, and grammar.
2. Remove transcript artifacts, false starts, filler, repeated fragments, and speaker/meta labels.
3. Lightly paraphrase for clarity, but keep the original meaning, scope, and factual content.
4. Preserve important facts such as project goals, locations, domains, technologies, scales, and distinctions between demonstrators.
5. Merge overlapping statements when they clearly repeat the same point.
6. The raw section may combine parallel material from multiple meetings or transcript versions. Do not produce repeated content in the cleaned result. Harmonize overlapping passages into a single coherent description for that section.
7. If two passages say nearly the same thing, keep the clearest phrasing once rather than preserving both.
8. If one passage adds a concrete detail to another overlapping passage, integrate that detail into one unified statement instead of writing two separate repeated statements.
9. Within each top-level section, produce one consolidated output block, not a transcript-by-transcript rewrite.
10. Do not invent details, interpretations, citations, or claims not supported by the raw text.
11. Write clean markdown text that can be reused directly in a Python prompt string later.
12. Keep the tone neutral and informational.
13. For demonstrator descriptions, preserve any existing site subheadings and keep each site distinct.
14. Return only the structured response.
"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Clean transcript-based context blocks with the OpenAI API."
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=DEFAULT_INPUT_PATH,
        help=f"Path to transcription markdown file. Default: {DEFAULT_INPUT_PATH}",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Directory for cleaned outputs. Default: {DEFAULT_OUTPUT_DIR}",
    )
    parser.add_argument(
        "--model",
        default=os.getenv("OPENAI_MODEL", "gpt-5.4-nano"),
        help="OpenAI model to use. Defaults to OPENAI_MODEL or gpt-5.4-nano.",
    )
    parser.add_argument(
        "--env-file",
        type=Path,
        default=DEFAULT_ENV_PATH,
        help=f"Path to env file containing OPENAI_API_KEY. Default: {DEFAULT_ENV_PATH}",
    )
    parser.add_argument(
        "--temperature",
        type=float,
        default=DEFAULT_TEMPERATURE,
        help=f"Sampling temperature. Default: {DEFAULT_TEMPERATURE}",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=1,
        help="Number of separate runs to execute and save as different files. Default: 1",
    )
    return parser.parse_args()


def load_markdown(path: Path) -> str:
    if not path.exists():
        raise FileNotFoundError(f"Input file not found: {path}")
    return path.read_text(encoding="utf-8")


def load_api_key_from_env_file(env_file: Path) -> None:
    if os.getenv("OPENAI_API_KEY"):
        return

    if not env_file.exists():
        return

    for raw_line in env_file.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("export "):
            line = line[len("export ") :].strip()
        if "=" not in line:
            continue
        key, value = line.split("=", 1)
        if key.strip() != "OPENAI_API_KEY":
            continue
        os.environ["OPENAI_API_KEY"] = value.strip().strip("'").strip('"')
        return


def extract_top_level_sections(markdown: str) -> Dict[str, str]:
    pattern = re.compile(r"^#\s+([^\n]+)\s*$", re.MULTILINE)
    matches = list(pattern.finditer(markdown))

    if not matches:
        raise ValueError("No top-level sections were found in the markdown file.")

    sections: Dict[str, str] = {}
    for index, match in enumerate(matches):
        title = match.group(1).strip()
        start = match.end()
        end = matches[index + 1].start() if index + 1 < len(matches) else len(markdown)
        body = markdown[start:end].strip()
        sections[title] = body

    missing = [name for name in SECTION_ORDER if name not in sections]
    if missing:
        raise ValueError(
            "Missing required sections in transcription.md: " + ", ".join(missing)
        )

    return sections


def build_user_prompt(section_title: str, raw_text: str) -> str:
    return f"""Clean the following transcript-derived section.

Section title: {section_title}

Cleaning target:
- produce clear, concise, publication-ready context
- preserve facts and distinctions
- lightly paraphrase only when needed for clarity
- deduplicate repeated material across transcript excerpts
- harmonize overlapping September and October content into one coherent section
- remove source labels such as file names and "From ..." markers
- keep markdown formatting simple and reusable

Raw section text:
<<<RAW_SECTION
{raw_text}
RAW_SECTION>>>
"""


def clean_section(
    client: OpenAI,
    model: str,
    temperature: float,
    section_title: str,
    raw_text: str,
) -> CleanedSection:
    response = client.responses.parse(
        model=model,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": build_user_prompt(section_title, raw_text)},
        ],
        temperature=temperature,
        text_format=CleanedSection,
    )

    parsed = response.output_parsed
    if parsed is None:
        raise ValueError(f"No structured output returned for section: {section_title}")

    return parsed


def write_json(path: Path, cleaned_sections: List[CleanedSection]) -> None:
    payload = {
        section.title: section.cleaned_text
        for section in cleaned_sections
    }
    path.write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding="utf-8")


def write_markdown(path: Path, cleaned_sections: List[CleanedSection]) -> None:
    parts: List[str] = []
    for section in cleaned_sections:
        parts.append(f"# {section.title}\n\n{section.cleaned_text.strip()}")
    path.write_text("\n\n".join(parts).strip() + "\n", encoding="utf-8")


def build_output_paths(output_dir: Path, run_index: int, timestamp: str) -> tuple[Path, Path]:
    suffix = f"run{run_index:02d}_{timestamp}"
    json_path = output_dir / f"cleaned_contexts_{suffix}.json"
    markdown_path = output_dir / f"cleaned_contexts_{suffix}.md"
    return json_path, markdown_path


def main() -> int:
    args = parse_args()
    load_api_key_from_env_file(args.env_file)

    if not os.getenv("OPENAI_API_KEY"):
        print(
            f"OPENAI_API_KEY is not set and was not found in {args.env_file}.",
            file=sys.stderr,
        )
        return 1

    markdown = load_markdown(args.input)
    sections = extract_top_level_sections(markdown)

    args.output_dir.mkdir(parents=True, exist_ok=True)

    client = OpenAI()
    if args.runs < 1:
        print("--runs must be at least 1.", file=sys.stderr)
        return 1

    for run_index in range(1, args.runs + 1):
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        cleaned_sections: List[CleanedSection] = []

        print(f"Starting run {run_index}/{args.runs}")
        for section_name in SECTION_ORDER:
            print(f"Cleaning section: {section_name}")
            cleaned = clean_section(
                client=client,
                model=args.model,
                temperature=args.temperature,
                section_title=section_name,
                raw_text=sections[section_name],
            )
            cleaned_sections.append(cleaned)

        json_path, markdown_path = build_output_paths(
            args.output_dir,
            run_index,
            timestamp,
        )

        write_json(json_path, cleaned_sections)
        write_markdown(markdown_path, cleaned_sections)

        print(f"Wrote JSON output to: {json_path}")
        print(f"Wrote Markdown output to: {markdown_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
