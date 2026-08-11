# 02_fit_clmm_overall.R
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
# --- patched helper: no dplyr::set_names needed ---
predict_probs_clmm <- function(mod, newdata, geq_k = 3) {
  # Coerce factor levels to match the model
  mf <- mod$model
  if ("condition"   %in% names(mf) && "condition"   %in% names(newdata))
    newdata$condition   <- factor(newdata$condition,   levels = levels(mf$condition))
  if ("base_model"  %in% names(mf) && "base_model"  %in% names(newdata))
    newdata$base_model  <- factor(newdata$base_model,  levels = levels(mf$base_model))
  if ("dc_solution" %in% names(mf) && "dc_solution" %in% names(newdata))
    newdata$dc_solution <- factor(newdata$dc_solution, levels = levels(mf$dc_solution))

  # Number of categories from the fitted response
  resp <- mf[[1]]
  K    <- length(levels(resp))
  stopifnot(K >= 2)
  cats <- 0:(K - 1)

  # Split coef vector: first K-1 are thresholds, rest are betas
  cf <- coef(mod)
  if (length(cf) < (K - 1)) stop("Model coef shorter than thresholds imply.")
  theta <- as.numeric(cf[seq_len(K - 1)])
  beta  <- as.numeric(cf[-seq_len(K - 1)])
  names(beta) <- names(cf)[-seq_len(K - 1)]

  # Fixed-effects model matrix for newdata (no random effects)
  TT <- stats::delete.response(stats::terms(mod))
  X  <- stats::model.matrix(TT, newdata, contrasts.arg = mod$contrasts)

  # Align columns: add missing beta cols as zeros; drop extras
  miss <- setdiff(names(beta), colnames(X))
  if (length(miss)) {
    X <- cbind(X, matrix(0, nrow = nrow(X), ncol = length(miss),
                         dimnames = list(NULL, miss)))
  }
  X <- X[, names(beta), drop = FALSE]

  # Linear predictor
  eta <- as.numeric(X %*% beta)

  # Cumulative probs with logit link: F_k = logistic(theta_k - eta)
  Fmat <- plogis(outer(theta, eta, function(th, e) th - e))  # (K-1) x N
  Fmat <- t(Fmat)                                            # N x (K-1)

  # Category probabilities
  N <- nrow(newdata)
  P <- matrix(NA_real_, nrow = N, ncol = K)
  P[, 1] <- Fmat[, 1]
  if (K > 2) P[, 2:(K - 1)] <- Fmat[, 2:(K - 1)] - Fmat[, 1:(K - 2)]
  P[, K] <- 1 - Fmat[, K - 1]

  # Sanity: rows sum to 1
  rs <- rowSums(P)
  if (!all(is.finite(rs))) stop("Non-finite probabilities produced.")
  if (any(abs(rs - 1) > 1e-8)) warning("Row probabilities not summing to 1 (tolerance).")

  # Tidy outputs (use base setNames to avoid dplyr::set_names)
  dfP <- as.data.frame(P)
  colnames(dfP) <- paste0("cat_", cats)

  probs_long <- dfP |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(dplyr::starts_with("cat_"),
                        names_to = "category", values_to = "prob") |>
    dplyr::mutate(category = as.integer(gsub("cat_", "", category))) |>
    dplyr::left_join(dplyr::mutate(newdata, row = dplyr::row_number()), by = "row") |>
    dplyr::select(-row)

  ge_idx <- which(cats >= geq_k)
  if (length(ge_idx) == 0) warning("geq_k is above the largest category; prob_geq will be 0.")

  probs_geq <- newdata |>
    dplyr::mutate(prob_geq = if (length(ge_idx)) rowSums(P[, ge_idx, drop = FALSE]) else 0,
                  threshold_k = geq_k)
  # sanity check: each row sums to 1
  row_sums <- rowSums(P)
  stopifnot(all(abs(row_sums - 1) < 1e-8))

  list(probs_long = probs_long, probs_geq = probs_geq)
}

# I/O
out_dir <- "clmm_outputs"
df <- readRDS(file.path(out_dir, "df_prepped.rds"))
dir.create(file.path(out_dir, "summaries"), showWarnings = FALSE)
dir.create(file.path(out_dir, "effects"),   showWarnings = FALSE)
dir.create(file.path(out_dir, "probs"),     showWarnings = FALSE)

# ---- overall model ----
m_overall <- clmm(
  rating ~ condition + base_model + condition:base_model +
    (1 | pair_id) + (1 | dc_solution),
  data = df, link = "logit", Hess = TRUE
)

# save summary
sink(file.path(out_dir, "summaries", "overall_summary.txt"))
cat("=== OVERALL CLMM SUMMARY ===\n"); print(summary(m_overall))
cat("\n=== VarCorr ===\n"); print(VarCorr(m_overall))
sink()

# OR by base_model (from single overall fit)
co <- broom::tidy(m_overall, conf.int = FALSE)
bm_levels <- levels(df$base_model)
ref_bm <- bm_levels[1]

coef_est_se <- function(term_name){
  row <- filter(co, term == term_name)
  if (nrow(row)==0) c(est=0, se=0) else c(est=row$estimate[1], se=row$std.error[1])
}
cond_main <- coef_est_se("conditionCONTEXT")

bm_effects <- map_dfr(bm_levels, function(bm){
  inter <- coef_est_se(paste0("conditionCONTEXT:base_model", bm))
  est <- cond_main["est"] + inter["est"]
  se  <- sqrt(cond_main["se"]^2 + inter["se"]^2)
  tibble(base_model = bm) |> bind_cols(to_or(est, se))
})

write_csv(bm_effects, file.path(out_dir, "effects", "overall_condition_OR_by_base_model.csv"))

# population-level probabilities by condition (at ref base_model)
nd_overall <- expand.grid(condition = levels(df$condition),
                          base_model = ref_bm,
                          KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

pr_overall <- predict_probs_clmm(m_overall, nd_overall, geq_k = 3)
write_csv(pr_overall$probs_long, file.path(out_dir, "probs", "probs_by_condition_overall_refBM.csv"))
write_csv(pr_overall$probs_geq,  file.path(out_dir, "probs", "prob_ge3_by_condition_overall_refBM.csv"))

# optional: average across base models
nd_bm <- expand.grid(condition = levels(df$condition),
                     base_model = bm_levels,
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
pr_bm <- predict_probs_clmm(m_overall, nd_bm, geq_k = 3)
avg_probs_geq <- pr_bm$probs_geq %>%
  group_by(condition) %>% summarise(prob_geq = mean(prob_geq), .groups="drop")
write_csv(avg_probs_geq, file.path(out_dir, "probs", "prob_ge3_by_condition_avgBM.csv"))

# save the model to reuse if needed
saveRDS(m_overall, file.path(out_dir, "m_overall.rds"))
cat("Overall model done.\n")

# --- NEW: per-base_model × dc_solution predictions from the OVERALL model ---
# Uses the robust helper `predict_probs_clmm()` you already defined above.
# These are population-level predictions (random effects set to zero).

grid_bm_sol <- expand.grid(
  condition   = levels(df$condition),
  base_model  = levels(df$base_model),
  dc_solution = levels(df$dc_solution),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)

pr_bm_sol <- predict_probs_clmm(m_overall, grid_bm_sol, geq_k = 3)

# Save tidy per-category probabilities and P(rating >= 3)
readr::write_csv(
  pr_bm_sol$probs_long,
  file.path(out_dir, "probs", "probs_by_condition_base_model_solution_FROM_OVERALL.csv")
)
readr::write_csv(
  pr_bm_sol$probs_geq,
  file.path(out_dir, "probs", "prob_ge3_by_condition_base_model_solution_FROM_OVERALL.csv")
)

cat("Wrote overall per-base×solution predictions to: ",
    normalizePath(file.path(out_dir, "probs"), winslash="\\", mustWork = FALSE), "\n", sep = "")
