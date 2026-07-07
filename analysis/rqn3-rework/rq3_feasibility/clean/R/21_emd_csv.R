# ================================
# 01_compute_emd.R
# Computes Wasserstein/EMD (1D) between HUMAN(NEU) and LLM (NEU/BIASED)
# Outputs CSVs:
#   - human_vs_llm_emd_by_model.csv
#   - human_vs_llm_emd_overall.csv
# ================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
})

# ---- Config ----
INFILE <- "clean/feasibility_bianeu_clean.csv"
OUTDIR <- "clean/feas_out"
if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE)

# ---- Load & tidy ----
dat <- readr::read_csv(INFILE, show_col_types = FALSE) %>%
  mutate(
    source     = toupper(trimws(as.character(source))),
    condition  = toupper(trimws(as.character(condition))),
    cond2      = if_else(condition == "NEU", "NEU", "BIASED"),
    rating     = suppressWarnings(as.numeric(as.character(rating))),
    rating     = pmin(pmax(rating, 0), 4),               # clamp to [0,4]
    base_model = if_else(source == "HUMAN", "HUMAN", as.character(base_model))
  ) %>%
  filter(!is.na(rating), source %in% c("HUMAN","LLM"))

rating_levels <- 0:4

# ---- Wasserstein-1 (Earth Mover) on ordinal 0..4 ----
w1_ord <- function(counts_a, counts_b) {
  pa <- counts_a / sum(counts_a); pb <- counts_b / sum(counts_b)
  sum(abs(cumsum(pa) - cumsum(pb)))  # unit bin width
}

# ---- HUMAN NEU distribution ----
dist_human_neu <- dat %>%
  filter(source == "HUMAN", cond2 == "NEU") %>%
  count(rating) %>%
  complete(rating = rating_levels, fill = list(n = 0)) %>%
  arrange(rating)

if (sum(dist_human_neu$n) == 0) stop("No HUMAN NEU ratings found.")
human_n <- sum(dist_human_neu$n)

# ---- LLM per base_model distributions (NEU / BIASED) ----
dist_llm_bm <- dat %>%
  filter(source == "LLM") %>%
  count(base_model, cond2, rating, name = "n") %>%
  complete(base_model, cond2, rating = rating_levels, fill = list(n = 0)) %>%
  arrange(base_model, cond2, rating)

emd_by_model <- dist_llm_bm %>%
  group_by(base_model, cond2) %>%
  summarise(
    emd   = w1_ord(dist_human_neu$n, n),
    n_llm = sum(n),
    .groups = "drop"
  ) %>%
  mutate(n_human = human_n) %>%
  arrange(base_model, cond2)

# ---- OVERALL (pool LLM across base_models) ----
dist_llm_overall <- dat %>%
  filter(source == "LLM") %>%
  count(cond2, rating, name = "n") %>%
  complete(cond2, rating = rating_levels, fill = list(n = 0)) %>%
  arrange(cond2, rating)

emd_overall <- dist_llm_overall %>%
  group_by(cond2) %>%
  summarise(
    emd   = w1_ord(dist_human_neu$n, n),
    n_llm = sum(n),
    .groups = "drop"
  ) %>%
  mutate(n_human = human_n) %>%
  arrange(cond2)

# ---- Save CSVs ----
out_by_model <- file.path(OUTDIR, "human_vs_llm_emd_by_model.csv")
out_overall  <- file.path(OUTDIR, "human_vs_llm_emd_overall.csv")
readr::write_csv(emd_by_model, out_by_model)
readr::write_csv(emd_overall,  out_overall)

message("✅ Wrote:\n  - ", out_by_model, "\n  - ", out_overall)
