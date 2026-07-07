# 03_fit_clmm_by_base_model_solution.R
if (!requireNamespace("ordinal", quietly = TRUE)) install.packages("ordinal")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

library(ordinal)
library(tidyverse)
library(broom)

# ---------- helpers ----------
to_or <- function(est, se){
  lo <- est - 1.96*se; hi <- est + 1.96*se
  tibble(OR = exp(est), OR_l95 = exp(lo), OR_u95 = exp(hi))
}

# population-level predicted probabilities for clmm (no random effects)
predict_probs_clmm <- function(mod, newdata, geq_k = 3) {
  # coerce factor levels to match model
  mf <- mod$model
  if ("condition"   %in% names(mf) && "condition"   %in% names(newdata))
    newdata$condition   <- factor(newdata$condition,   levels = levels(mf$condition))
  if ("base_model"  %in% names(mf) && "base_model"  %in% names(newdata))
    newdata$base_model  <- factor(newdata$base_model,  levels = levels(mf$base_model))
  if ("dc_solution" %in% names(mf) && "dc_solution" %in% names(newdata))
    newdata$dc_solution <- factor(newdata$dc_solution, levels = levels(mf$dc_solution))

  # number of categories from fitted response
  resp <- mf[[1]]
  K    <- length(levels(resp))
  stopifnot(K >= 2)
  cats <- 0:(K - 1)

  # split coefs: first K-1 are thresholds, rest are betas
  cf <- coef(mod)
  if (length(cf) < (K - 1)) stop("Model coef shorter than thresholds imply.")
  theta <- as.numeric(cf[seq_len(K - 1)])
  beta  <- as.numeric(cf[-seq_len(K - 1)])
  names(beta) <- names(cf)[-seq_len(K - 1)]

  # fixed-effects design for newdata
  TT <- stats::delete.response(stats::terms(mod))
  X  <- stats::model.matrix(TT, newdata, contrasts.arg = mod$contrasts)

  miss <- setdiff(names(beta), colnames(X))
  if (length(miss)) {
    X <- cbind(X, matrix(0, nrow = nrow(X), ncol = length(miss),
                         dimnames = list(NULL, miss)))
  }
  X <- X[, names(beta), drop = FALSE]

  # linear predictor
  eta <- as.numeric(X %*% beta)

  # cumulative probs: F_k = logistic(theta_k - eta)
  Fmat <- plogis(outer(theta, eta, function(th, e) th - e))  # (K-1) x N
  Fmat <- t(Fmat)                                            # N x (K-1)

  # category probs
  N <- nrow(newdata)
  P <- matrix(NA_real_, nrow = N, ncol = K)
  P[, 1] <- Fmat[, 1]
  if (K > 2) P[, 2:(K - 1)] <- Fmat[, 2:(K - 1)] - Fmat[, 1:(K - 2)]
  P[, K] <- 1 - Fmat[, K - 1]

  # tidy outputs
  dfP <- as.data.frame(P); colnames(dfP) <- paste0("cat_", cats)
  probs_long <- dfP |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(dplyr::starts_with("cat_"),
                        names_to = "category", values_to = "prob") |>
    dplyr::mutate(category = as.integer(gsub("cat_", "", category))) |>
    dplyr::left_join(dplyr::mutate(newdata, row = dplyr::row_number()), by = "row") |>
    dplyr::select(-row)

  ge_idx <- which(cats >= geq_k)
  probs_geq <- newdata |>
    dplyr::mutate(prob_geq = if (length(ge_idx)) rowSums(P[, ge_idx, drop = FALSE]) else 0,
                  threshold_k = geq_k)

  list(probs_long = probs_long, probs_geq = probs_geq)
}

write_csv_safely <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(path)) file.remove(path)
  readr::write_csv(x, path)
  message("Wrote: ", normalizePath(path, winslash="\\", mustWork = FALSE),
          "  (nrow=", nrow(x), ")")
}

# ---------- I/O ----------
out_dir <- "clmm_outputs"
message("Working directory: ", getwd())
message("Output dir: ", normalizePath(out_dir, winslash="\\", mustWork = FALSE))

df <- readRDS(file.path(out_dir, "df_prepped.rds"))
dir.create(file.path(out_dir, "summaries"), showWarnings = FALSE)
dir.create(file.path(out_dir, "effects"),   showWarnings = FALSE)
dir.create(file.path(out_dir, "probs"),     showWarnings = FALSE)

bm_list <- split(df, df$base_model)
all_bm_solution_OR <- list()

for (bm in names(bm_list)) {
  d_bm <- bm_list[[bm]] %>%
    mutate(dc_solution = droplevels(dc_solution),
           pair_id     = droplevels(pair_id))

  # within-base-model CLMM with solution-specific condition effects
  m_bm <- clmm(
    rating ~ condition + condition:dc_solution + (1 | pair_id) + (1 | dc_solution),
    data = d_bm, link = "logit", Hess = TRUE
  )

  # summary
  sink(file.path(out_dir, "summaries", paste0("summary_bm_", bm, ".txt")))
  cat("=== CLMM SUMMARY — base_model:", bm, "===\n")
  print(summary(m_bm))
  cat("\n=== VarCorr ===\n"); print(VarCorr(m_bm))
  sink()

  # OR per solution (CONTEXT vs ZEROSHOT)
  co_bm <- broom::tidy(m_bm, conf.int = FALSE)
  sol_levels <- levels(d_bm$dc_solution)

  get_est_se <- function(term){
    r <- dplyr::filter(co_bm, term == term)
    if (nrow(r)==0) c(est=0,se=0) else c(est=r$estimate[1], se=r$std.error[1])
  }
  cond_main <- get_est_se("conditionCONTEXT")

  sol_or <- purrr::map_dfr(sol_levels, function(sol){
    inter <- get_est_se(paste0("conditionCONTEXT:dc_solution", sol))
    est <- cond_main["est"] + inter["est"]
    se  <- sqrt(cond_main["se"]^2 + inter["se"]^2)
    tibble(base_model = bm, dc_solution = sol) |> bind_cols(to_or(est, se))
  })

  write_csv_safely(sol_or, file.path(out_dir, "effects", paste0("condition_OR_by_solution_bm_", bm, ".csv")))
  all_bm_solution_OR[[bm]] <- sol_or

  # probabilities by condition × solution (population-level)
  nd_sol <- expand.grid(condition = levels(d_bm$condition),
                        dc_solution = sol_levels,
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  pr_sol <- predict_probs_clmm(m_bm, nd_sol, geq_k = 3)

  write_csv_safely(pr_sol$probs_long, file.path(out_dir, "probs", paste0("probs_by_condition_solution_bm_", bm, ".csv")))
  write_csv_safely(pr_sol$probs_geq,  file.path(out_dir, "probs", paste0("prob_ge3_by_condition_solution_bm_", bm, ".csv")))
}

bind_rows(all_bm_solution_OR) %>%
  arrange(base_model, dc_solution) %>%
  write_csv_safely(file.path(out_dir, "effects", "condition_OR_by_solution_ALL_base_models.csv"))

cat("Per–base model fits done. Outputs in:", normalizePath(out_dir), "\n")
