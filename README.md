This repository is organized into two main folders: `previous-analysis` and `current-analysis`.

## Repository Structure
### Independent files
These are files in the root of the repository supporting and referenced in the paper:

  - Elicitation protocol first session: EE-Protocol-first.pdf
  - Elicitation protocol second session: EE-Protocol-second.pdf
  - Online appendix: Online-appendix.pdf
  - Online appendix 2: Online-appendix2.pdf

### 2. Previous analysis: `previous-analysis`
Contains prompt scripts, files, and analysis scripts from the first submission to the TiiS journal. These files support reproducibility for the experiments reported in the second manuscript.

  - `analysis/`: analysis scripts and generated outputs reported in the second manuscript.
  - `modelfiles/`: model-definition files used to generate the LLM expert personas.
  - `pythonscripts/`: Python scripts used to prompt the model variants.

### 3. Current analysis: `current-analysis`
Contains the revised prompt scripts, files, and analysis scripts used after incorporating reviewer comments.

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
