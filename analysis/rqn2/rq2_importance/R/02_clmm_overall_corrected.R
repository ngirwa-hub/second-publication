# 02_fit_clmm_overall_corrected.R
if (!requireNamespace("ordinal", quietly = TRUE)) install.packages("ordinal")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

library(ordinal)
library(tidyverse)
library(broom)

# helpers
to_or <- function(est, se){
  lo <- est - 1.96*se; hi <- est + 1.96*se
  tibble(OR = exp(est), OR_l95 = exp(lo), OR_u95 = exp(hi))
}

# --- BEFORE reading df_prepped.rds ---
out_dir <- "clmm_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "summaries"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "effects"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "probs"),     recursive = TRUE, showWarnings = FALSE)

cat("WD:", normalizePath(getwd(), winslash="\\", mustWork = FALSE), "\n")
cat("Output root:", normalizePath(out_dir, winslash="\\", mustWork = FALSE), "\n")

# helper: safe write with message + existence check
save_csv <- function(x, path) {
  readr::write_csv(x, path)
  cat("Wrote:", normalizePath(path, winslash="\\", mustWork = FALSE), "\n")
  if (!file.exists(path)) stop("File not written: ", path)
}

# --- READ DATA ---
df_path <- file.path(out_dir, "df_prepped.rds")
stopifnot(file.exists(df_path))
df <- readRDS(df_path)

# MAKE SURE THESE ARE FACTORS so levels() isn't NULL
df <- df |>
  dplyr::mutate(
    condition   = if (!is.factor(condition)) factor(condition)   else condition,
    base_model  = if (!is.factor(base_model)) factor(base_model) else base_model,
    dc_solution = if (!is.factor(dc_solution)) factor(dc_solution) else dc_solution
  )

# sanity
str(df$condition); str(df$base_model); str(df$dc_solution)

# --- FIT OVERALL MODEL (unchanged) ---
m_overall <- clmm(
  rating ~ condition + base_model + condition:base_model +
    (1 | pair_id) + (1 | dc_solution),
  data = df, link = "logit", Hess = TRUE
)

# --- SAVE SUMMARY (ensure sink closes even on error) ---
sum_path <- file.path(out_dir, "summaries", "overall_summary.txt")
con <- file(sum_path, open = "wt"); on.exit(close(con), add = TRUE)
sink(con)
cat("=== OVERALL CLMM SUMMARY ===\n"); print(summary(m_overall))
cat("\n=== VarCorr ===\n"); print(VarCorr(m_overall))
sink()  # close
cat("Wrote:", normalizePath(sum_path, winslash="\\", mustWork = FALSE), "\n")
stopifnot(file.exists(sum_path))

# --- EFFECTS CSV ---
co <- broom::tidy(m_overall, conf.int = FALSE)

coef_est_se <- function(term_name){
  row <- dplyr::filter(co, term == term_name)
  if (nrow(row)==0) c(est=0, se=0) else c(est=row$estimate[1], se=row$std.error[1])
}

bm_levels <- levels(df$base_model)
stopifnot(length(bm_levels) > 0)
ref_bm   <- bm_levels[1]

cond_main <- coef_est_se("conditionCONTEXT")  # adjust if your level is not "CONTEXT"

bm_effects <- purrr::map_dfr(bm_levels, function(bm){
  inter <- coef_est_se(paste0("conditionCONTEXT:base_model", bm))
  est <- cond_main["est"] + inter["est"]
  se  <- sqrt(cond_main["se"]^2 + inter["se"]^2)  # assumes independence; OK for quick summary
  tibble::tibble(base_model = bm) |> dplyr::bind_cols(to_or(est, se))
})

eff_path <- file.path(out_dir, "effects", "overall_condition_OR_by_base_model.csv")
save_csv(bm_effects, eff_path)

# --- PREDICTION GRIDS (ensure non-empty) ---
lvl_or_unique <- function(x) if (is.factor(x)) levels(x) else unique(x)

nd_overall <- expand.grid(
  condition  = lvl_or_unique(df$condition),
  base_model = ref_bm,
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
stopifnot(nrow(nd_overall) > 0)

pr_overall <- predict_probs_clmm(m_overall, nd_overall, geq_k = 3)
save_csv(pr_overall$probs_long, file.path(out_dir, "probs", "probs_by_condition_overall_refBM.csv"))
save_csv(pr_overall$probs_geq,  file.path(out_dir, "probs", "prob_ge3_by_condition_overall_refBM.csv"))

nd_bm <- expand.grid(
  condition  = lvl_or_unique(df$condition),
  base_model = lvl_or_unique(df$base_model),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
stopifnot(nrow(nd_bm) > 0)

pr_bm <- predict_probs_clmm(m_overall, nd_bm, geq_k = 3)
avg_probs_geq <- pr_bm$probs_geq |>
  dplyr::group_by(condition) |>
  dplyr::summarise(prob_geq = mean(prob_geq), .groups="drop")

save_csv(avg_probs_geq, file.path(out_dir, "probs", "prob_ge3_by_condition_avgBM.csv"))

# --- PER BASE × SOLUTION PREDICTIONS ---
grid_bm_sol <- expand.grid(
  condition   = lvl_or_unique(df$condition),
  base_model  = lvl_or_unique(df$base_model),
  dc_solution = lvl_or_unique(df$dc_solution),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
stopifnot(nrow(grid_bm_sol) > 0)

pr_bm_sol <- predict_probs_clmm(m_overall, grid_bm_sol, geq_k = 3)

save_csv(
  pr_bm_sol$probs_long,
  file.path(out_dir, "probs", "probs_by_condition_base_model_solution_FROM_OVERALL.csv")
)
save_csv(
  pr_bm_sol$probs_geq,
  file.path(out_dir, "probs", "prob_ge3_by_condition_base_model_solution_FROM_OVERALL.csv")
)

# --- SAVE MODEL ---
mdl_path <- file.path(out_dir, "m_overall.rds")
saveRDS(m_overall, mdl_path)
cat("Wrote model:", normalizePath(mdl_path, winslash="\\", mustWork = FALSE), "\n")

# Final listing to prove files exist
cat("\nFiles under", normalizePath(out_dir, winslash="\\", mustWork = FALSE), ":\n")
print(list.files(out_dir, recursive = TRUE, full.names = TRUE))
