# ---- Empirical P(rating >= 3) by scenario & base_model (LLM) ----
library(dplyr)
library(readr)

indir  <- "clean"          # input folder
outdir <- "clean/feas_out" # output folder
in_csv <- "feasibility_bianeu_clean.csv"

# ensure output directory exists
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# --- LLM cell-variation check (by base_model × scenario × rating) ---
emp_llm <- readr::read_csv(file.path(indir, in_csv)) %>%
  dplyr::filter(source == "LLM") %>%
  dplyr::mutate(high = as.integer(rating >= 3)) %>%
  dplyr::group_by(scenario, base_model) %>%
  dplyr::summarise(n = dplyr::n(), p_ge3 = mean(high), .groups = "drop")
readr::write_csv(emp_llm, file.path(outdir, "feas_emp_pge3_by_scenario_base_model.csv"))
