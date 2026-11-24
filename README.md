This repository contains [previous-analysis](previous-analysis/), [current-analysis](current-analysis/), [2-current-analysis](2-current-analysis/), and [variability-tests](variability-tests/).

## Repository Structure
### 1. Independent files
- These are files in the root of the repository supporting and referenced in the paper:

  - `EE-Protocol-first.pdf` : the elicitation protocol covered the second session with the DC experts
  - `EE-Protocol-second.pdf` : the elicitation protocol covered the first session with the DC experts
  - `Online-appendix.pdf` : contains extra figures and tables referenced in the paper
  - `Online-appendix2.pdf` : contains discussions raised during first and second sessions with the DC experts

### 2. Previous analysis: 
- Overall path: `previous-analysis`
- Contains prompt scripts, files, and analysis scripts from the first submission. - - These files support reproducibility for the experiments reported in the second manuscript.

  - `analysis/`: analysis scripts and generated outputs reported in the second manuscript.
  - `modelfiles/`: model-definition files used to generate the LLM expert personas.
  - `pythonscripts/`: Python scripts used to prompt the model variants.

### 3. Current analysis: 
- Overall path: `current-analysis`
- Contains the revised prompt scripts, files, and analysis scripts used after incorporating reviewer comments.

- `meetingTranscript/`: data-preparation files for the content used in the `context` and `anchor` experiments.
  - `transcription.md`: raw sections for the project description, demonstrations, and elicitation instructions.
  - `text-cleaning.py`: script that cleans the transcript and writes outputs to `cleaned-context/`.
  - `semanticFidelity.py`: first BERTScore run; outputs are saved in `semantic-fidelity/` as `.tex`, `.csv`, and `.json`.
  - `semanticFidelityAuthorVersions.py`: author-version BERTScore run; outputs are also saved in `semantic-fidelity/`.
  - `visuals-textcleaning.ipynb`: computes the final LaTeX table `text_cleaning_fidelity_table.tex`.
- `re-run/`: variants creation, prompting, and analysis for the current manuscript.
  - `modelfiles/`: model files used in the re-run prompts.
  - `1_zero-shot/`, `2_context/`, and `3_anchor/`: scripts and outputs for the three experimental conditions.
  - `feasibility`, `importance`, and `barrierSelect`: question sets used within each condition.
  - `zeroshotResponses/`: zero-shot response outputs.
  - `zeroshotBarrierSelectRaw/`: raw barrier-selection responses for the zero-shot condition.
  - `zero-shot-raw/`: example CSV access path for zero-shot outputs.

#### Guide to the analyses:
##### RQ1: 
Run dependencies: `pandas`; `tidyverse` (for R scripts); `scales`.

###### RQ1-barrier:
Overall path: `current-analysis/re-run/rq1-rerun/`
- Data-preparation notebook: `rq1-barrier-analysis.ipynb`
- Input CSV: `barriers-zeroshot-working/barriers-humanllm-responses.csv`
- Analysis script: `barrier-select-rq1.R`
- Generated selection-share table: `barriers-zeroshot-working/barrier_selection_rates.csv`
- Generated figures: `rerun-rq1_barriers.png` and `rerun-rq1_barriers.eps` in `barriers-zeroshot-working/`

Rscript `barrier-select-rq1.R`
- Outputs: full selection rate and slopegraph in: `barriers-zeroshot-working/ `

###### RQ1-feasibility:
Overall path: `current-analysis/re-run/rq1-rerun/`

- Optional data-preparation notebook: `rq1-feasibility-analysis.ipynb`
- Combined analysis input: `feas-zeroshot-working/feasibility-humanllm-responses.csv`
- Analysis script: `rq1-rerun/feasibility-rq1.R`
- Generated figures: `rerun-rq1_feas_bplot.png` and `rerun-rq1_feas_bplot.eps` in `feas-zeroshot-working/`

Rscript `feasibility-rq1.R`
- Outputs: `feas-zeroshot-working/`

###### RQ1-importance:
Overall path: `current-analysis/re-run/rq1-rerun/`

- Optional data-preparation notebook: `rq1-importance-analysis.ipynb`
- Combined analysis input: `imp-zeroshot-working/importance-humanllm-responses.csv`
- Analysis script: `rq1-rerun/importance-rq1.R`
- Generated figures: `rerun-rq1_importance_boxplot.png` and `rerun-rq1_importance_boxplot.eps`; 
  - Saved in: `imp-zeroshot-working/`

Run from `current-analysis/re-run/rq1-rerun/` 
Rscript `importance-rq1.R`
- Outputs: `imp-zeroshot-working/`

##### RQ2:
###### RQ2-barrier:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Optional data-preparation notebook: `rq2-barrier-analysis.ipynb`
- Combined analysis input: `barriers-ctx-zeroshot-working/barriers-ctx-zeroshot-responses.csv`
- Analysis script: `rq2-rerun/barrier-select-rq2.R`
- Generated tables: `top5_overlap_summary_by_family.csv`, `top5_union_per_barrier_with_jaccard.csv`, and `jaccard_top5_barriers.tex` in `barriers-ctx-zeroshot-working/`
- Generated figures: `slopegraph_top5_union_by_family.png` and `slopegraph_top5_union_by_family.eps` in `barriers-ctx-zeroshot-working/`

Rscript `barrier-select-rq2.R`
- Outputs: `barriers-ctx-zeroshot-working/`

###### RQ2-feasibility:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Combined input: `feas-ctx-zeroshot-working/feas-ctx-zeroshot-responses.csv`
- Analysis scripts: `feasibility-analysis/`

Run the following commands in order from `current-analysis/re-run/rq2-rerun/`:
```bash
Rscript "feasibility-analysis/feasibility-rq2-thres.R"
Rscript "feasibility-analysis/ppc-feasibility-rq2-thres.R"
Rscript "feasibility-analysis/feas-rq2-sensitivity.R"
```

- Primary model: `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/`;
- PPCs: `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/posterior-predictive-checks/`, and 
- Prior-sensitivity: `feas-ctx-zeroshot-working/bayesian-results/prior-sensitivity/`

###### RQ2-importance:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Combined input: `imp-ctx-zeroshot-working/imp-ctx-zeroshot-responses.csv`
- Analysis scripts: `imp-rq2-analysis/`

Run the following commands in order from `current-analysis/re-run/rq2-rerun/`:
```bash
Rscript "imp-rq2-analysis/importance-rq2.R"
Rscript "imp-rq2-analysis/ppc-importance-rq2.R"
Rscript "imp-rq2-analysis/imp-rq2-sensitivity.R"
```

- Primary model: `imp-ctx-zeroshot-working/bayesian-results/base-solution-threshold/`; 
- PPCs: `imp-ctx-zeroshot-working/bayesian-results/base-solution-threshold/posterior-predictive-checks/`, and 
- Prior-sensitivity: `imp-ctx-zeroshot-working/bayesian-results/prior-sensitivity/`


##### RQ3:

###### RQ3-barrier:
Overall path: `current-analysis/re-run/rq3-rerun/`

- Combined input: `barriers-ctx-anchor-working/barriers-ctx-anchor-responses.csv`
- Analysis script: `rq3-rerun/barrier-select-rq3.R`

Run from `current-analysis/re-run/rq3-rerun/`:
```bash
Rscript "barrier-select-rq3.R"
```

- Outputs: `barriers-ctx-anchor-working/`

###### RQ3-feasibility:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Combined input: `feas-ctx-zeroshot-working/feas-ctx-zeroshot-responses.csv`
- Analysis-scripts: `feasibility-analysis/`

Run the following commands in order from `current-analysis/re-run/rq2-rerun/`:
```bash
Rscript "feasibility-analysis/feasibility-rq2-thres.R"
Rscript "feasibility-analysis/ppc-feasibility-rq2-thres.R"
Rscript "feasibility-analysis/feas-rq2-sensitivity.R"
```

- Primary model: `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/`, 
- PPCs: `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/posterior-predictive-checks/`, and 
- Prior-sensitivity: `feas-ctx-zeroshot-working/bayesian-results/prior-sensitivity/`


###### RQ3-importance:
Overall path: `current-analysis/re-run/rq3-rerun/`

- Combined input: `imp-ctx-anchor-working/imp-ctx-anchor-responses.csv`
- Analysis scripts: `imp-rq3-analysis/`

Run the following commands in order from `current-analysis/re-run/rq3-rerun/`:
```bash
Rscript "imp-rq3-analysis/importance-rq3.R"
Rscript "imp-rq3-analysis/ppc-importance-rq3.R"
Rscript "imp-rq3-analysis/imp-rq3-sensitivity.R"
```

- Primary model: `imp-ctx-anchor-working/bayesian-results/base-solution-threshold/`, 
- PPCs: `imp-ctx-anchor-working/bayesian-results/base-solution-threshold/posterior-predictive-checks/`, and, 
- Anchor-pattern: `imp-ctx-anchor-working/bayesian-results/anchor-response-patterns/` and 
  - `imp-ctx-anchor-working/bayesian-results/rq3-importance-anchor-response-patterns/`, and 
- Prior-sensitivity: `imp-ctx-anchor-working/bayesian-results/prior-sensitivity/`

### 4. Seeded reruns: [2-current-analysis](2-current-analysis/)

This folder replicates the experiments in [current-analysis](current-analysis/), using Ollama generation seed 42 for feasibility, importance, and barrier selection. Barrier-selection prompts also use Fisher–Yates shuffling to present all 11 barriers in a randomly generated order for each iteration. The shuffle uses system randomness independently of the generation seed. See the [analysis guide above](#guide-to-the-analyses) for the shared methods and workflow.

- Prompt scripts, response CSVs, and logs: [Zero-shot](2-current-analysis/21_zero-shot/), [Context](2-current-analysis/22_context/), and [Anchor](2-current-analysis/23_anchor/).
- Analysis notebooks, R scripts, and result folders: [RQ1](2-current-analysis/rq1-rerun/), [RQ2](2-current-analysis/rq2-rerun/), and [RQ3](2-current-analysis/rq3-rerun/).

### 5. Variability tests: [variability-tests](variability-tests/)

These tests assessed whether changing the presentation order of the 11 DC solutions (feasibility question) or the 11 barriers (barrier-selection question), and introducing generation seed 42, produced different responses from the original baseline. Each condition contains 50 iterations for all 12 variants: four base models with three expert roles each.

| Condition | DC-solution order / feasibility outputs | Barrier order / selection outputs |
|---|---|---|
| Original baseline, no explicit generation seed | [Baseline](variability-tests/variability-feasibility-sol/baseline-run/) | [Baseline](variability-tests/variability-barrier-list/baseline-run/) |
| Baseline order, generation seed 42 | [Seeded baseline](variability-tests/variability-feasibility-sol/outputs/baseline-seed42/) | [Seeded baseline](variability-tests/variability-barrier-list/outputs/baseline-seed42/) |
| Fixed reordered list: Run1, no explicit generation seed | [Run1](variability-tests/variability-feasibility-sol/outputs/run1/) | [Run1](variability-tests/variability-barrier-list/outputs/run1/) |
| Fixed reordered list: Run2, no explicit generation seed | [Run2](variability-tests/variability-feasibility-sol/outputs/run2/) | [Run2](variability-tests/variability-barrier-list/outputs/run2/) |
| Fixed reordered list: Run3, no explicit generation seed | [Run3](variability-tests/variability-feasibility-sol/outputs/run3/) | [Run3](variability-tests/variability-barrier-list/outputs/run3/) |
| Run1 order, generation seed 42 | [Seeded Run1](variability-tests/variability-feasibility-sol/outputs/run1-seed42/) | [Seeded Run1](variability-tests/variability-barrier-list/outputs/run1-seed42/) |
| Fisher–Yates order generated per iteration, generation seed 42 | — | [Randomized order](variability-tests/variability-barrier-list/outputs/randOrder-seed42/) |

- [Feasibility comparison notebook](variability-tests/variability-feasibility-sol/feasibility-shuffle.ipynb): compares rating distributions; generated PNG/EPS figures are in the [experiment folder](variability-tests/variability-feasibility-sol/).
- [Barrier comparison notebook](variability-tests/variability-barrier-list/barrier-selection-shuffle.ipynb): compares selection shares; tables, figures, and ordering-analysis outputs are in [analysis-results](variability-tests/variability-barrier-list/analysis-results/).
