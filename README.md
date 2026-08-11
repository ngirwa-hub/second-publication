This repository contains two main folders: previous-analysis and current-analysis. 

1. `previous-analysis`: contains prompt scripts, files, and analysis scripts from the first submission to the TiiS journal. The files that support the reproducibility of the experiments presented in our second manuscript. 

- The folder structures are as follows: 
    1.1. `analysis folder`: contains all the analysis scripts and the generated outputs that have been reported in the second manuscript
    1.2. `modefiles folder`: contains the files for generating the LLM experts used in the prompts to produce the data analyzed in this manuscript
    1.3. `pythonscripts folder`: contains the Python scripts used to prompt the model variants. 


2. `current-analysis`: contains prompt scripts, files, and analysis scripts from the second submission to the TiiS journal after incorporating comments provided by the reviewers. 
The folder structure and targets are as follows: 
    2.1. Data preparations are in the `second-publication/current-analysis/meetingTranscript/` folder:
        - This mainly targeted on preparations of the content that was used for the `context` and the `anchored` experiments
            2.1.1. For the `context` experiment: the scripts and files are in `second-publication/meetingTranscript/`
                A. Raw sections (project, demonstrations, elicitation instructions): `transcription.md`
                B. GPT run script: `text-cleaning.py`. 
                  - Input: `transcription.md`, and output were saved in `cleaned-context/` folder.
                C. First BERTScore run, script: `semanticFidelity.py` 
                  - Output saved: `semantic-fidelity/` folder in three formats (i.e., .tex, .csv, and .json) 
                  - Higher `F1-score` saved in: `author-review-raw.json`.
                D. Author BERTScore run, script: `semanticFidelityAuthorVersions.py` with outputs saved in three formats as well (i.e., .tex, .csv, and .json) 
                  - Outputs saved: `semantic-fidelity/` 
                  - Final Latex table computed: `visuals-textcleaning.ipynb`; 
                  - Saved: `text_cleaning_fidelity_table.tex`.

  2.2. Variants creation, prompting, and analysis can be found in `second-publication/current-analysis/re-run/` folder:
    A. Modelfiles: `modelfiles/` 
    B. Scripts per condition: `1_zero-shot/`, `2_context/`, and `3_anchor/`
      - Questions folder: `feasibility`, `importance`, and `barrierSelect`. 
      - Counter-files (folder structures): `re-run/1_zero-shot/zeroshotResponses/` 
      - Raw-responses: `re-run/1_zero-shot/zeroshotResponses/zeroshotBarrierSelectRaw/`  
    C. CSVs example access path using zero-shot: `re-run/zero-shot-raw/`

## 2.3. Guide to the analyses:
### RQ1: 
Run dependencies: `pandas`; `tidyverse` (for R scripts); `scales`.

#### RQ1-barrier:
Overall path: `current-analysis/re-run/rq1-rerun/`
- Data-preparation notebook: `rq1-barrier-analysis.ipynb`
- Input CSV: `barriers-zeroshot-working/barriers-humanllm-responses.csv`
- Analysis script: `barrier-select-rq1.R`
- Generated selection-share table: `barriers-zeroshot-working/barrier_selection_rates.csv`
- Generated figures: `rerun-rq1_barriers.png` and `rerun-rq1_barriers.eps` in `barriers-zeroshot-working/`

Rscript `barrier-select-rq1.R`
- Outputs: full selection rate and slopegraph in: `barriers-zeroshot-working/ `

#### RQ1-feasibility:
Overall path: `current-analysis/re-run/rq1-rerun/`

- Optional data-preparation notebook: `rq1-feasibility-analysis.ipynb`
- Combined analysis input: `feas-zeroshot-working/feasibility-humanllm-responses.csv`
- Analysis script: `rq1-rerun/feasibility-rq1.R`
- Generated figures: `rerun-rq1_feas_bplot.png` and `rerun-rq1_feas_bplot.eps` in `feas-zeroshot-working/`

Rscript `feasibility-rq1.R`
- Outputs: `feas-zeroshot-working/`

#### RQ1-importance:
Overall path: `current-analysis/re-run/rq1-rerun/`

- Optional data-preparation notebook: `rq1-importance-analysis.ipynb`
- Combined analysis input: `imp-zeroshot-working/importance-humanllm-responses.csv`
- Analysis script: `rq1-rerun/importance-rq1.R`
- Generated figures: `rerun-rq1_importance_boxplot.png` and `rerun-rq1_importance_boxplot.eps`; 
  - Saved in: `imp-zeroshot-working/`

Run from `current-analysis/re-run/rq1-rerun/` 
Rscript `importance-rq1.R`
- Outputs: `imp-zeroshot-working/`

### RQ2:
#### RQ2-barrier:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Optional data-preparation notebook: `rq2-barrier-analysis.ipynb`
- Combined analysis input: `barriers-ctx-zeroshot-working/barriers-ctx-zeroshot-responses.csv`
- Analysis script: `rq2-rerun/barrier-select-rq2.R`
- Generated tables: `top5_overlap_summary_by_family.csv`, `top5_union_per_barrier_with_jaccard.csv`, and `jaccard_top5_barriers.tex` in `barriers-ctx-zeroshot-working/`
- Generated figures: `slopegraph_top5_union_by_family.png` and `slopegraph_top5_union_by_family.eps` in `barriers-ctx-zeroshot-working/`

Rscript `barrier-select-rq2.R`
- Outputs: `barriers-ctx-zeroshot-working/`

#### RQ2-feasibility:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Combined input: `feas-ctx-zeroshot-working/feas-ctx-zeroshot-responses.csv`
- Analysis scripts: `feasibility-analysis/`

Run the following commands in order from `current-analysis/re-run/rq2-rerun/`:
```bash
Rscript "feasibility-analysis/feasibility-rq2-thres.R"
Rscript "feasibility-analysis/ppc-feasibility-rq2-thres.R"
Rscript "feasibility-analysis/feas-rq2-sensitivity.R"
```

- Outputs: primary model in `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/`;
- PPCs in `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/posterior-predictive-checks/`, and 
- Prior-sensitivity outputs in `feas-ctx-zeroshot-working/bayesian-results/prior-sensitivity/`

#### RQ2-importance:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Combined input: `imp-ctx-zeroshot-working/imp-ctx-zeroshot-responses.csv`
- Analysis scripts: `imp-rq2-analysis/`

Run the following commands in order from `current-analysis/re-run/rq2-rerun/`:
```bash
Rscript "imp-rq2-analysis/importance-rq2.R"
Rscript "imp-rq2-analysis/ppc-importance-rq2.R"
Rscript "imp-rq2-analysis/imp-rq2-sensitivity.R"
```

- Outputs: primary model in `imp-ctx-zeroshot-working/bayesian-results/base-solution-threshold/`; 
- PPCs in `imp-ctx-zeroshot-working/bayesian-results/base-solution-threshold/posterior-predictive-checks/`, and 
- Prior-sensitivity outputs in `imp-ctx-zeroshot-working/bayesian-results/prior-sensitivity/`


### RQ3:

#### RQ3-barrier:
Overall path: `current-analysis/re-run/rq3-rerun/`

- Combined input: `barriers-ctx-anchor-working/barriers-ctx-anchor-responses.csv`
- Analysis script: `rq3-rerun/barrier-select-rq3.R`

Run from `current-analysis/re-run/rq3-rerun/`:
```bash
Rscript "barrier-select-rq3.R"
```

- Outputs: `barriers-ctx-anchor-working/`

#### RQ2-feasibility:
Overall path: `current-analysis/re-run/rq2-rerun/`

- Combined input: `feas-ctx-zeroshot-working/feas-ctx-zeroshot-responses.csv`
- Analysis scripts: `feasibility-analysis/`

Run the following commands in order from `current-analysis/re-run/rq2-rerun/`:
```bash
Rscript "feasibility-analysis/feasibility-rq2-thres.R"
Rscript "feasibility-analysis/ppc-feasibility-rq2-thres.R"
Rscript "feasibility-analysis/feas-rq2-sensitivity.R"
```

- Outputs: `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/`, 
- PPCs: `feas-ctx-zeroshot-working/bayesian-results/base-model-threshold-trial/posterior-predictive-checks/`, and 
- Prior-sensitivity: `feas-ctx-zeroshot-working/bayesian-results/prior-sensitivity/`


#### RQ3-importance:
Overall path: `current-analysis/re-run/rq3-rerun/`

- Combined input: `imp-ctx-anchor-working/imp-ctx-anchor-responses.csv`
- Analysis scripts: `imp-rq3-analysis/`

Run the following commands in order from `current-analysis/re-run/rq3-rerun/`:
```bash
Rscript "imp-rq3-analysis/importance-rq3.R"
Rscript "imp-rq3-analysis/ppc-importance-rq3.R"
Rscript "imp-rq3-analysis/imp-rq3-sensitivity.R"
```

- Outputs: primary model in `imp-ctx-anchor-working/bayesian-results/base-solution-threshold/`, 
- PPCs in `imp-ctx-anchor-working/bayesian-results/base-solution-threshold/posterior-predictive-checks/`, and, 
- anchor-pattern outputs in `imp-ctx-anchor-working/bayesian-results/anchor-response-patterns/` and 
  - `imp-ctx-anchor-working/bayesian-results/rq3-importance-anchor-response-patterns/`, and 
- Prior-sensitivity outputs in `imp-ctx-anchor-working/bayesian-results/prior-sensitivity/`
