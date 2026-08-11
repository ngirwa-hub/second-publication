#!/usr/bin/env python3
"""
Compute BERTScore semantic fidelity between author raw and author edited versions.

Default inputs:
- author-review-raw.json
- author-review-edited.jsonl

The script:
- loads the three expected sections
- normalizes string-vs-array representations into plain text
- compares edited text (candidate) against raw text (reference)
- writes per-section and overall results to CSV and JSON

# command: 
/Users/HP/Documents/.venv312/bin/python /Users/HP/Documents/second-publication/meetingTranscript/semanticFidelityAuthorVersions.py --model-type roberta-large
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
DEFAULT_RAW_PATH = SCRIPT_DIR / "author-review-raw.json"
DEFAULT_EDITED_PATH = SCRIPT_DIR / "author-review-edited.jsonl"
DEFAULT_OUTPUT_DIR = SCRIPT_DIR / "semantic-fidelity"

SECTION_ORDER = [
    "elicitation_process",
    "project_description",
    "demonstrator_description",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compute BERTScore between author raw and author edited section files."
    )
    parser.add_argument(
        "--raw",
        type=Path,
        default=DEFAULT_RAW_PATH,
        help=f"Path to author raw JSON. Default: {DEFAULT_RAW_PATH}",
    )
    parser.add_argument(
        "--edited",
        type=Path,
        default=DEFAULT_EDITED_PATH,
        help=f"Path to author edited JSON/JSONL-style file. Default: {DEFAULT_EDITED_PATH}",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Directory for outputs. Default: {DEFAULT_OUTPUT_DIR}",
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


def load_json_like(path: Path) -> Dict[str, object]:
    content = read_text(path).strip()
    if not content:
        raise ValueError(f"File is empty: {path}")

    try:
        payload = json.loads(content)
    except json.JSONDecodeError as exc:
        raise ValueError(f"Invalid JSON content in {path}: {exc}") from exc

    if not isinstance(payload, dict):
        raise ValueError(f"Top-level content must be a JSON object: {path}")

    missing = [name for name in SECTION_ORDER if name not in payload]
    if missing:
        raise ValueError(
            f"Missing required sections in {path.name}: {', '.join(missing)}"
        )
    return payload


def normalize_section_value(value: object) -> str:
    if isinstance(value, str):
        text = value
    elif isinstance(value, list):
        if not all(isinstance(item, str) for item in value):
            raise ValueError("Section arrays must contain only strings.")
        text = "".join(value)
    else:
        raise ValueError("Section value must be either a string or a list of strings.")

    text = text.strip()
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text


def mean(values: Iterable[float]) -> float:
    values = list(values)
    return sum(values) / len(values) if values else 0.0


def main() -> int:
    args = parse_args()

    raw_payload = load_json_like(args.raw)
    edited_payload = load_json_like(args.edited)

    references = [normalize_section_value(raw_payload[name]) for name in SECTION_ORDER]
    candidates = [normalize_section_value(edited_payload[name]) for name in SECTION_ORDER]

    precision, recall, f1 = bertscore_score(
        candidates,
        references,
        lang="en",
        model_type=args.model_type,
        batch_size=args.batch_size,
        rescale_with_baseline=args.rescale_with_baseline,
        verbose=False,
    )

    args.output_dir.mkdir(parents=True, exist_ok=True)

    rows: List[Dict[str, object]] = []
    for index, section in enumerate(SECTION_ORDER):
        row = {
            "raw_file": args.raw.name,
            "edited_file": args.edited.name,
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

    rows.append(
        {
            "raw_file": args.raw.name,
            "edited_file": args.edited.name,
            "section": "__overall__",
            "precision": round(mean(row["precision"] for row in rows), 6),
            "recall": round(mean(row["recall"] for row in rows), 6),
            "f1": round(mean(row["f1"] for row in rows), 6),
            "candidate_chars": sum(int(row["candidate_chars"]) for row in rows),
            "reference_chars": sum(int(row["reference_chars"]) for row in rows),
            "length_ratio": round(
                sum(int(row["candidate_chars"]) for row in rows)
                / sum(int(row["reference_chars"]) for row in rows),
                6,
            ),
            "model_type": args.model_type,
            "rescale_with_baseline": args.rescale_with_baseline,
        }
    )

    csv_path = args.output_dir / "bertscore_author_versions.csv"
    json_path = args.output_dir / "bertscore_author_versions.json"

    fieldnames = [
        "raw_file",
        "edited_file",
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
