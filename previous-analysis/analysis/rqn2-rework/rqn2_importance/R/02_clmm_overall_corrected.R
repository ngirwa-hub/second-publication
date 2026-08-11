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

predict_probs_clmm <- function(mod, newdata, geq_k = 3, add_dc_solution_re = FALSE) {
  mf <- mod$model
  if ("condition" %in% names(mf) && "condition" %in% names(newdata)) {
    newdata$condition <- factor(newdata$condition, levels = levels(mf$condition))
  }
  if ("base_model" %in% names(mf) && "base_model" %in% names(newdata)) {
    newdata$base_model <- factor(newdata$base_model, levels = levels(mf$base_model))
  }
  if ("dc_solution" %in% names(mf) && "dc_solution" %in% names(newdata)) {
    newdata$dc_solution <- factor(newdata$dc_solution, levels = levels(mf$dc_solution))
  }

  resp <- mf[[1]]
  K <- length(levels(resp))
  stopifnot(K >= 2)
  cats <- 0:(K - 1)

  cf <- coef(mod)
  if (length(cf) < (K - 1)) stop("Model coef shorter than thresholds imply.")
  theta <- as.numeric(cf[seq_len(K - 1)])
  beta <- as.numeric(cf[-seq_len(K - 1)])
  names(beta) <- names(cf)[-seq_len(K - 1)]

  TT <- stats::delete.response(stats::terms(mod))
  X <- stats::model.matrix(TT, newdata, contrasts.arg = mod$contrasts)

  miss <- setdiff(names(beta), colnames(X))
  if (length(miss)) {
    X <- cbind(
      X,
      matrix(0, nrow = nrow(X), ncol = length(miss), dimnames = list(NULL, miss))
    )
  }
  X <- X[, names(beta), drop = FALSE]
  eta <- as.numeric(X %*% beta)

  if (isTRUE(add_dc_solution_re) && "dc_solution" %in% names(newdata)) {
    re <- try(suppressWarnings(ranef(mod)$dc_solution), silent = TRUE)
    if (!inherits(re, "try-error") && !is.null(re)) {
      col_ok <- intersect(c("(Intercept)", "Intercept"), colnames(re))
      if (length(col_ok)) {
        ri <- re[as.character(newdata$dc_solution), col_ok[1]]
        ri[is.na(ri)] <- 0
        eta <- eta + as.numeric(ri)
      }
    }
  }

  Fmat <- plogis(outer(theta, eta, function(th, e) th - e))
  Fmat <- t(Fmat)

  N <- nrow(newdata)
  P <- matrix(NA_real_, nrow = N, ncol = K)
  P[, 1] <- Fmat[, 1]
  if (K > 2) P[, 2:(K - 1)] <- Fmat[, 2:(K - 1)] - Fmat[, 1:(K - 2)]
  P[, K] <- 1 - Fmat[, K - 1]

  row_sums <- rowSums(P)
  stopifnot(all(is.finite(row_sums)))
  if (any(abs(row_sums - 1) > 1e-8)) warning("Row probabilities not summing to 1.")

  dfP <- as.data.frame(P)
  colnames(dfP) <- paste0("cat_", cats)

  probs_long <- dfP |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(dplyr::starts_with("cat_"), names_to = "category", values_to = "prob") |>
    dplyr::mutate(category = as.integer(gsub("cat_", "", category))) |>
    dplyr::left_join(dplyr::mutate(newdata, row = dplyr::row_number()), by = "row") |>
    dplyr::select(-row)

  ge_idx <- which(cats >= geq_k)
  probs_geq <- newdata |>
    dplyr::mutate(
      prob_geq = if (length(ge_idx)) rowSums(P[, ge_idx, drop = FALSE]) else 0,
      threshold_k = geq_k
    )

  list(probs_long = probs_long, probs_geq = probs_geq)
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

# --- SAVE SUMMARY (avoid sink/connection edge cases) ---
sum_path <- file.path(out_dir, "summaries", "overall_summary.txt")
summary_lines <- c(
  "=== OVERALL CLMM SUMMARY ===",
  capture.output(summary(m_overall)),
  "",
  "=== VarCorr ===",
  capture.output(VarCorr(m_overall))
)
writeLines(summary_lines, con = sum_path, useBytes = TRUE)
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

pr_bm_sol_with_re <- predict_probs_clmm(
  m_overall, grid_bm_sol, geq_k = 3, add_dc_solution_re = TRUE
)
save_csv(
  pr_bm_sol_with_re$probs_long,
  file.path(out_dir, "probs", "probs_by_condition_base_model_solution_FROM_OVERALL_WITH_RE.csv")
)
save_csv(
  pr_bm_sol_with_re$probs_geq,
  file.path(out_dir, "probs", "prob_ge3_by_condition_base_model_solution_FROM_OVERALL_WITH_RE.csv")
)

# --- SAVE MODEL ---
mdl_path <- file.path(out_dir, "m_overall.rds")
saveRDS(m_overall, mdl_path)
cat("Wrote model:", normalizePath(mdl_path, winslash="\\", mustWork = FALSE), "\n")

# Final listing to prove files exist
cat("\nFiles under", normalizePath(out_dir, winslash="\\", mustWork = FALSE), ":\n")
print(list.files(out_dir, recursive = TRUE, full.names = TRUE))
