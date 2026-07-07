#!/usr/bin/env python3
"""
Target: Measure semantic fidelity of cleaned context files with BERTScore.

Default behavior:
- Read the original section blocks from `transcription.md`
- Read all `cleaned_contexts_*.json` files from `cleaned-context/`
- Compute BERTScore for each matching section:
  - elicitation_process
  - project_description
  - demonstrator_description
- Save per-section and aggregate results to CSV and JSON

Notes:
- `candidate` = cleaned text
- `reference` = original text
- Higher recall suggests more of the source meaning was retained
- Higher precision suggests the cleaned text stays closer to the source

# install bert command: 
/Users/HP/Documents/.venv312/bin/pip install bert-score

# run command: 
python second-publication/meetingTranscript/semanticFidelity.py
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from pathlib import Path
from typing import Dict, Iterable, List

try:
    from bert_score import score as bertscore_score
except ImportError as exc:  # pragma: no cover
    raise SystemExit(
        "Missing dependency: bert-score. Install it first, for example with "
        "`pip install bert-score`."
    ) from exc


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_SOURCE_PATH = SCRIPT_DIR / "transcription.md"
DEFAULT_CLEANED_DIR = SCRIPT_DIR / "cleaned-context"
DEFAULT_OUTPUT_DIR = SCRIPT_DIR / "semantic-fidelity"

SECTION_ORDER = [
    "elicitation_process",
    "project_description",
    "demonstrator_description",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compute BERTScore semantic fidelity for cleaned context files."
    )
    parser.add_argument(
        "--source",
        type=Path,
        default=DEFAULT_SOURCE_PATH,
        help=f"Path to the source transcription markdown. Default: {DEFAULT_SOURCE_PATH}",
    )
    parser.add_argument(
        "--cleaned-dir",
        type=Path,
        default=DEFAULT_CLEANED_DIR,
        help=f"Directory containing cleaned_contexts_*.json files. Default: {DEFAULT_CLEANED_DIR}",
    )
    parser.add_argument(
        "--cleaned-file",
        type=Path,
        action="append",
        default=[],
        help="Specific cleaned JSON file(s) to score. Can be passed multiple times.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Directory for semantic fidelity outputs. Default: {DEFAULT_OUTPUT_DIR}",
    )
    parser.add_argument(
        "--model-type",
        default="roberta-large",
        help="Backbone model for BERTScore. Default: roberta-large",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=8,
        help="Batch size for BERTScore. Default: 8",
    )
    parser.add_argument(
        "--rescale-with-baseline",
        action="store_true",
        help="Use BERTScore baseline rescaling.",
    )
    return parser.parse_args()


def read_text(path: Path) -> str:
    if not path.exists():
        raise FileNotFoundError(f"File not found: {path}")
    return path.read_text(encoding="utf-8")


def extract_top_level_sections(markdown: str) -> Dict[str, str]:
    pattern = re.compile(r"^#\s+([^\n]+)\s*$", re.MULTILINE)
    matches = list(pattern.finditer(markdown))
    if not matches:
        raise ValueError("No top-level sections found in the source markdown.")

    sections: Dict[str, str] = {}
    for index, match in enumerate(matches):
        title = match.group(1).strip()
        start = match.end()
        end = matches[index + 1].start() if index + 1 < len(matches) else len(markdown)
        sections[title] = markdown[start:end].strip()

    missing = [name for name in SECTION_ORDER if name not in sections]
    if missing:
        raise ValueError(
            "Missing required sections in source markdown: " + ", ".join(missing)
        )
    return sections


def normalize_source_text(text: str) -> str:
    text = re.sub(r"^From\s+`[^`]+`:\s*$", "", text, flags=re.MULTILINE)
    text = re.sub(r"^##\s+", "", text, flags=re.MULTILINE)
    text = text.replace("```", "")
    text = text.replace("`", "")
    lines: List[str] = []
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        if line.startswith('"') and line.endswith('"') and len(line) >= 2:
            line = line[1:-1].strip()
        lines.append(line)
    return "\n".join(lines).strip()


def normalize_cleaned_text(text: str) -> str:
    return re.sub(r"\n{3,}", "\n\n", text.strip())


def load_cleaned_json(path: Path) -> Dict[str, str]:
    payload = json.loads(read_text(path))
    if not isinstance(payload, dict):
        raise ValueError(f"Cleaned JSON must contain a top-level object: {path}")

    missing = [name for name in SECTION_ORDER if name not in payload]
    if missing:
        raise ValueError(
            f"Cleaned file {path} is missing required sections: {', '.join(missing)}"
        )

    return {key: str(value).strip() for key, value in payload.items()}


def discover_cleaned_files(cleaned_dir: Path) -> List[Path]:
    if not cleaned_dir.exists():
        raise FileNotFoundError(f"Cleaned directory not found: {cleaned_dir}")
    return sorted(cleaned_dir.glob("cleaned_contexts_*.json"))


def score_pairs(
    candidates: List[str],
    references: List[str],
    model_type: str,
    batch_size: int,
    rescale_with_baseline: bool,
):
    precision, recall, f1 = bertscore_score(
        candidates,
        references,
        lang="en",
        model_type=model_type,
        batch_size=batch_size,
        rescale_with_baseline=rescale_with_baseline,
        verbose=False,
    )
    return precision, recall, f1


def mean(values: Iterable[float]) -> float:
    values = list(values)
    return sum(values) / len(values) if values else 0.0


def main() -> int:
    args = parse_args()

    source_sections = extract_top_level_sections(read_text(args.source))
    normalized_source = {
        section: normalize_source_text(source_sections[section])
        for section in SECTION_ORDER
    }

    cleaned_files = [path.resolve() for path in args.cleaned_file] or discover_cleaned_files(
        args.cleaned_dir
    )
    if not cleaned_files:
        print("No cleaned JSON files found to score.", file=sys.stderr)
        return 1

    args.output_dir.mkdir(parents=True, exist_ok=True)
    rows: List[Dict[str, object]] = []

    for cleaned_path in cleaned_files:
        print(f"Scoring: {cleaned_path.name}")
        cleaned_sections = load_cleaned_json(cleaned_path)

        references = [normalized_source[section] for section in SECTION_ORDER]
        candidates = [
            normalize_cleaned_text(cleaned_sections[section]) for section in SECTION_ORDER
        ]

        precision, recall, f1 = score_pairs(
            candidates=candidates,
            references=references,
            model_type=args.model_type,
            batch_size=args.batch_size,
            rescale_with_baseline=args.rescale_with_baseline,
        )

        file_rows: List[Dict[str, object]] = []
        for index, section in enumerate(SECTION_ORDER):
            row = {
                "cleaned_file": cleaned_path.name,
                "section": section,
                "precision": round(float(precision[index]), 6),
                "recall": round(float(recall[index]), 6),
                "f1": round(float(f1[index]), 6),
                "candidate_chars": len(candidates[index]),
                "reference_chars": len(references[index]),
                "length_ratio": round(
                    len(candidates[index]) / len(references[index]), 6
                ) if references[index] else 0.0,
                "model_type": args.model_type,
                "rescale_with_baseline": args.rescale_with_baseline,
            }
            rows.append(row)
            file_rows.append(row)

        aggregate_row = {
            "cleaned_file": cleaned_path.name,
            "section": "__overall__",
            "precision": round(mean(row["precision"] for row in file_rows), 6),
            "recall": round(mean(row["recall"] for row in file_rows), 6),
            "f1": round(mean(row["f1"] for row in file_rows), 6),
            "candidate_chars": sum(int(row["candidate_chars"]) for row in file_rows),
            "reference_chars": sum(int(row["reference_chars"]) for row in file_rows),
            "length_ratio": round(
                sum(int(row["candidate_chars"]) for row in file_rows)
                / sum(int(row["reference_chars"]) for row in file_rows),
                6,
            ),
            "model_type": args.model_type,
            "rescale_with_baseline": args.rescale_with_baseline,
        }
        rows.append(aggregate_row)

    csv_path = args.output_dir / "bertscore_semantic_fidelity.csv"
    json_path = args.output_dir / "bertscore_semantic_fidelity.json"

    fieldnames = [
        "cleaned_file",
        "section",
        "precision",
        "recall",
        "f1",
        "candidate_chars",
        "reference_chars",
        "length_ratio",
        "model_type",
        "rescale_with_baseline",
    ]

    with csv_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    json_path.write_text(json.dumps(rows, indent=2), encoding="utf-8")

    print(f"Wrote CSV output to: {csv_path}")
    print(f"Wrote JSON output to: {json_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
