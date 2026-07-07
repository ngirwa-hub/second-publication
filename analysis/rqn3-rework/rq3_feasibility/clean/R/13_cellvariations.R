library(dplyr)
library(readr)

indir  <- "clean"          # input folder
outdir <- "clean/feas_out" # output folder
in_csv <- "feasibility_bianeu_clean.csv"

# ensure output directory exists
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# --- LLM cell-variation check (by base_model × scenario × rating) ---
feas_llm_cell_variation <- readr::read_csv(file.path(indir, in_csv)) %>%
  dplyr::filter(source == "LLM") %>%
  dplyr::mutate(rating = as.integer(rating)) %>%
  dplyr::count(base_model, scenario, rating, name = "n") %>%
  dplyr::arrange(base_model, scenario, rating)

readr::write_csv(
  feas_llm_cell_variation,
  file.path(outdir, "feas_llm_cell_variation_check.csv")
)
message("✅ Wrote: ", file.path(outdir, "feas_llm_cell_variation_check.csv"))

# --- Unique base models seen this run (sanity list) ---
feas_unique_base_models <- readr::read_csv(file.path(indir, in_csv)) %>%
  dplyr::filter(source == "LLM") %>%
  dplyr::distinct(base_model) %>%
  dplyr::arrange(base_model)

readr::write_csv(
  feas_unique_base_models,
  file.path(outdir, "feas_unique_base_models_seen_by_run.csv")
)
message("✅ Wrote: ", file.path(outdir, "feas_unique_base_models_seen_by_run.csv"))
