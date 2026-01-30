# 03_tables.R
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(forcats)
})

outdir <- "clean/feas_out"
feas <- readRDS(file.path(outdir, "feas_clean.rds"))

# 1) sanity counts (same idea as your *_counts_check.csv)
feas %>%
  count(source, condition, scenario) %>%
  arrange(source, condition, scenario) %>%
  write_csv(file.path(outdir, "feasibility_bianeu_counts_check.csv"))

# 2) medians by source × scenario
feas %>%
  group_by(source, scenario) %>%
  summarise(n = n(), median_rating = median(as.integer(as.character(rating_ord))), .groups="drop") %>%
  write_csv(file.path(outdir, "feas_median_by_source_scenario.csv"))

# 3) sparsity report (LLM only)
feas_llm <- feas %>% filter(source=="LLM")
sparsity <- feas_llm %>%
  count(base_model, scenario, rating_ord, name="n") %>%
  complete(base_model, scenario,
           rating_ord = factor(0:4, ordered=TRUE, levels=0:4),
           fill = list(n = 0)) %>%
  group_by(base_model, scenario) %>%
  summarise(n_obs = sum(n), n_pos_levels = sum(n>0), min_cell = min(n), .groups="drop")

write_csv(sparsity, file.path(outdir, "feas_llm_sparsity_check.csv"))
message("Wrote tables to: ", normalizePath(outdir))
message("✅ Wrote: ", file.path(outdir, "feasibility_bianeu_counts_check.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_median_by_source_scenario.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_llm_sparsity_check.csv"))
