This repository contains [previous-analysis](previous-analysis/), [current-analysis](current-analysis/), [2-current-analysis](2-current-analysis/), and [variability-tests](variability-tests/).

## Repository Structure
### 1. Independent files
- These are files in the root of the repository supporting and referenced in the paper:

  - `EE-Protocol-first.pdf` : the elicitation protocol covered the second session with the DC experts
  - `EE-Protocol-second.pdf` : the elicitation protocol covered the first session with the DC experts
  - `Online-appendix2.pdf` : contains discussions raised during first and second sessions with the DC experts

### 2. Previous analysis: 
- Overall path: `previous-analysis`
- Contains prompt scripts, files, and analysis scripts from the first submission. - - These files support reproducibility for the experiments reported in the second manuscript.

  - `analysis/`: analysis scripts and generated outputs reported in the second manuscript.
  - `modelfiles/`: model-definition files used to generate the LLM expert personas.
  - `pythonscripts/`: Python scripts used to prompt the model variants.

### 3. Earlier analysis: [current-analysis](current-analysis/)

This folder retains the earlier revised experiments and results for comparison with the seeded reruns. Prompt scripts and RQ1–RQ3 analyses are under `re-run/`; model definitions are in `re-run/modelfiles/`. The `meetingTranscript/` folder contains transcript cleaning, author review, BERTScore checks, and the context-preparation outputs. The current experimental and analysis workflow is documented under [2-current-analysis](#4-seeded-reruns-2-current-analysis) below.

### 4. Seeded reruns: [2-current-analysis](2-current-analysis/)

This is the main workflow for the seeded experiments and their RQ1–RQ3 analyses. It uses four base models, each with generalist, normative, and subject-matter variants. All generation requests use seed 42. Zero-shot and context have 50 iterations per variant and question; importance is elicited separately for each DC solution. Anchors have 10 iterations per variant and arm: word, example, high numeric, and low numeric. Fixed-seed repetitions assess repeatability, not variability across independently sampled generation seeds.

#### Prompt scripts, responses, and logs

Paths in this table are relative to `2-current-analysis/`.

| Condition | Python prompt scripts | Response CSV folder | Raw-response and order logs |
|---|---|---|---|
| [Zero-shot](2-current-analysis/21_zero-shot/) | `21_zero-shot/21_zeroshotFeasibility.py`, `21_zeroshotImportance.py`, `21_zeroshotBarrierSelect.py` in the same folder | `21_zero-shot/21_zero-shot-raw/` | `21_zero-shot/21_zeroshotResponses/` |
| [Context](2-current-analysis/22_context/) | `22_context/22_contextFeasibility.py`, `22_contextImportance.py`, `22_contextBarrierSelect.py` in the same folder | `22_context/22_context-raw/` | `22_context/22_contextResponses/` |
| [Anchor](2-current-analysis/23_anchor/) | `23_anchor/23_anchorFeasibility.py`, `23_anchorImportance.py`, `23_anchorBarrierSelect.py` in the same folder | `23_anchor/23_anchor-raw/` | `23_anchor/23_anchorResponses/` |

Each CSV folder contains the corresponding `FeasibilityResponses.csv`, `ImportanceResponses.csv`, and `BarrierSelectResponses.csv`, with the condition prefix shown in the script names. The balanced anchor-feasibility input, `23_anchorFeasibilityResponses-40iter.csv`, is prepared by the RQ3 feasibility notebook.

[barrier_random_runtime.py](2-current-analysis/barrier_random_runtime.py) is shared by the three barrier launchers. It generates a Fisher–Yates order using system randomness for each new iteration, independently of generation seed 42. All variants receive the same order within an iteration. Retries retain the pending prompt and order. The condition-specific response folders contain `*RawLogs/` and `*BarrierSelectOrderLogs/`, recording responses, presentation orders, and matching information.

Context preparation remains documented in [current-analysis/meetingTranscript](current-analysis/meetingTranscript/). The exploratory checks that informed the seeded and shuffled runs are described under [Variability tests](#5-variability-tests-variability-tests).

#### Analysis workflow

The notebooks prepare combined response CSVs and generate reporting tables and figures. R scripts perform the analyses. Run preparation cells first, then the relevant R scripts, and finally the notebook's reporting cells. For the RQ2 and RQ3 rating analyses, the sequence is primary model, posterior predictive checks (PPCs), then prior sensitivity. Rerunning reporting cells alone does not refit a model. Some files retain Linux absolute paths; check those paths before running on another computer.

#### RQ1: expert responses versus zero-shot LLM responses

Overall path: [2-current-analysis/rq1-rerun](2-current-analysis/rq1-rerun/). Expert response CSVs are in `raw-responses/`; LLM inputs come from `../21_zero-shot/21_zero-shot-raw/`. Paths below are relative to `rq1-rerun/`. Run the R scripts from that folder.

| Question | Preparation notebook | Analysis script | Combined input and output folder |
|---|---|---|---|
| Feasibility | `21_rq1-feasibility-analysis.ipynb` | `21_feasibility-rq1.R` | `feas-zeroshot-working/feasibility-humanllm-responses.csv`; figures in the same folder |
| Importance | `21_rq1-importance-analysis.ipynb` | `21_importance-rq1.R` | `imp-zeroshot-working/importance-humanllm-responses.csv`; figures in the same folder |
| Barriers | `21_rq1-barrier-analysis.ipynb` | `21_barrier-select-rq1.R` | `barriers-zeroshot-working/barriers-humanllm-responses.csv`; selection rates, ranks, comparisons, and figures in the same folder |

The [revised expert consistency check](2-current-analysis/rq1-rerun/experts-consistency/revised-consistency-check.ipynb) assesses consortium and industry responses separately and pooled. It reports ordinal Krippendorff's alpha for importance and nominal alpha for barriers, with expert-bootstrap confidence intervals. Feasibility reports group percentages and a two-sided Fisher's exact test. Whole-expert permutation tests compare importance and barrier response distributions; Holm adjustment covers the three question-level comparisons. These tests assess group/session differences, not differences between alpha coefficients. The combined table is [agreement_overall_summary.tex](2-current-analysis/rq1-rerun/experts-consistency/latex-tables/agreement_overall_summary.tex); detailed question tables are in the same `latex-tables/` folder.

#### RQ2: context versus zero-shot

Overall path: [2-current-analysis/rq2-rerun](2-current-analysis/rq2-rerun/). The notebooks combine responses from `21_zero-shot/` and `22_context/`. Paths below are relative to `rq2-rerun/`.

| Question | Preparation and reporting notebook | Combined analysis input |
|---|---|---|
| Feasibility | `rq2-feas-analysis.ipynb` | `feas-ctx-zeroshot-working/22_feas-ctx-zeroshot-responses.csv` |
| Importance | `rq2-imp-analysis.ipynb` | `imp-ctx-zeroshot-working/22_imp-ctx-zeroshot-responses.csv` |
| Barriers | `rq2-barrier-analysis.ipynb` | `barriers-ctx-zeroshot-working/22_barriers-ctx-zeroshot-responses.csv` |

- Feasibility scripts, in order: `feasibility-analysis/22_feasibility-rq2-thres.R`, `22_ppc-feasibility-rq2-thres.R`, and `22_feas-rq2-sensitivity.R` in that script folder.
- Importance scripts, in order: `imp-rq2-analysis/22_importance-rq2.R`, `22_ppc-importance-rq2.R`, and `22_imp-rq2-sensitivity.R` in that script folder.
- Barrier script: `22_barrier-select-rq2.R`. Outputs include top-five overlap summaries and the rank slopegraph in `barriers-ctx-zeroshot-working/`.

Rating outputs are in the corresponding working folder. Primary results are under `bayesian-results/base-model-threshold-trial/` for feasibility and `bayesian-results/base-solution-threshold/` for importance. PPCs are in each primary-result folder's `posterior-predictive-checks/`; sensitivity outputs are under `bayesian-results/prior-sensitivity/`. Notebook-generated LaTeX tables and figures are saved in the working folders.

#### RQ3: anchors versus context

Overall path: [2-current-analysis/rq3-rerun](2-current-analysis/rq3-rerun/). The notebooks combine context responses with the four anchor arms. Paths below are relative to `rq3-rerun/`.

| Question | Preparation and reporting notebook | Combined analysis input |
|---|---|---|
| Feasibility | `rq3-feas-analysis.ipynb` | `feas-ctx-anchor-working/23_feas-ctx-anchor-responses.csv` |
| Importance | `rq3-imp-analysis.ipynb` | `imp-ctx-anchor-working/23_imp-ctx-anchor-responses.csv` |
| Barriers | `rq3-barrier-analysis.ipynb` | `barriers-ctx-anchor-working/23_barriers-ctx-anchor-responses.csv` |

- Feasibility scripts, in order: `rq3-feasibility-analysis/23_feasibility-rq3.R`, `23_ppc-feasibility-rq3.R`, and `23_feas-rq3-sensitivity.R` in that script folder.
- Importance scripts, in order: `rq3-imp-analysis/23_importance-rq3.R`, `23_ppc-importance-rq3.R`, and `23_imp-rq3-sensitivity.R` in that script folder.
- Barrier script: `23_barrier-select-rq3.R`. Selection rates, pooled and arm-specific top-five Jaccard summaries, and rank slopegraphs are saved in `barriers-ctx-anchor-working/`.

Rating outputs are saved in their working folders: primary results under `bayesian-results/base-model-threshold/` for feasibility and `bayesian-results/base-solution-threshold/` for importance, PPCs in their `posterior-predictive-checks/` subfolders, and sensitivity results under `bayesian-results/prior-sensitivity/`. Each rating-script folder also contains `23_anchor-patterns-*-rq3.R` for post-estimation endpoint analyses. Their tables are under `feas-ctx-anchor-working/bayesian-results/23_rq3_feasibility-anchor-response-patterns/latex-tables/` and `imp-ctx-anchor-working/bayesian-results/rq3-importance-anchor-response-patterns/latex-tables/`.

### 5. Variability tests: [variability-tests](variability-tests/)

These exploratory checks compare the original baseline with reordered DC-solution lists for the overall feasibility question and reordered barrier lists for barrier selection. They also compare configurations with no explicit generation seed against configurations using seed 42. Each condition has 50 iterations for each of the 12 model variants. The checks assess response repeatability and list-order sensitivity; they are not a systematic assessment across multiple generation seeds or prompt paraphrases.

| Condition | DC-solution order / feasibility outputs | Barrier order / selection outputs |
|---|---|---|
| Original baseline, no explicit generation seed | [Baseline](variability-tests/variability-feasibility-sol/baseline-run/) | [Baseline](variability-tests/variability-barrier-list/baseline-run/) |
| Baseline order, generation seed 42 | [Seeded baseline](variability-tests/variability-feasibility-sol/outputs/baseline-seed42/) | [Seeded baseline](variability-tests/variability-barrier-list/outputs/baseline-seed42/) |
| Fixed reordered list: Run1, no explicit generation seed | [Run1](variability-tests/variability-feasibility-sol/outputs/run1/) | [Run1](variability-tests/variability-barrier-list/outputs/run1/) |
| Fixed reordered list: Run2, no explicit generation seed | [Run2](variability-tests/variability-feasibility-sol/outputs/run2/) | [Run2](variability-tests/variability-barrier-list/outputs/run2/) |
| Fixed reordered list: Run3, no explicit generation seed | [Run3](variability-tests/variability-feasibility-sol/outputs/run3/) | [Run3](variability-tests/variability-barrier-list/outputs/run3/) |
| Run1 order, generation seed 42 | [Seeded Run1](variability-tests/variability-feasibility-sol/outputs/run1-seed42/) | [Seeded Run1](variability-tests/variability-barrier-list/outputs/run1-seed42/) |
| Fisher–Yates order generated per iteration, generation seed 42 | — | [Randomized order](variability-tests/variability-barrier-list/outputs/randOrder-seed42/) |

- [Feasibility comparison notebook](variability-tests/variability-feasibility-sol/feasibility-shuffle.ipynb): compares rating distributions, medians, and interquartile ranges across list orders and seed configurations. Scripts and generated PNG/EPS figures are in the [experiment folder](variability-tests/variability-feasibility-sol/). Feasibility medians were unchanged across the tested unseeded list orders, although distributions varied. 

- [Barrier comparison notebook](variability-tests/variability-barrier-list/barrier-selection-shuffle.ipynb): compares aggregate selection shares across fixed and randomized orders. Descriptive figures, the barrier dictionary, and presentation-order tables are in [analysis-results](variability-tests/variability-barrier-list/analysis-results/). The randomized condition records 50 distinct orders for 50 successful iterations; per-iteration logs are in its `zeroshotBarrierSelectOrderLogs/` folder. 