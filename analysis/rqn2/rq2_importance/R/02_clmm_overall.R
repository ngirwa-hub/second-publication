# 02_fit_clmm_overall.R
if (!requireNamespace("ordinal", quietly = TRUE)) install.packages("ordinal")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

library(ordinal)
library(tidyverse)
library(broom)

message("Working directory: ", getwd())

write_csv_safely <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(path)) file.remove(path)   # force overwrite (Excel must be closed!)
  readr::write_csv(x, path)
  message("Wrote: ", normalizePath(path, winslash="\\", mustWork = FALSE),
          "  (rows=", nrow(x), ")")
}

stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")


# helpers
to_or <- function(est, se){
  lo <- est - 1.96*se; hi <- est + 1.96*se
  tibble(OR = exp(est), OR_l95 = exp(lo), OR_u95 = exp(hi))
}
# Set add_dc_solution_re = TRUE to add the dc_solution random intercept (if available).
predict_probs_clmm <- function(mod, newdata, geq_k = 3, add_dc_solution_re = FALSE) {
  # 1) Ensure factor levels in newdata match the model's
  mf <- mod$model
  if ("condition"   %in% names(mf) && "condition"   %in% names(newdata))
    newdata$condition   <- factor(newdata$condition,   levels = levels(mf$condition))
  if ("base_model"  %in% names(mf) && "base_model"  %in% names(newdata))
    newdata$base_model  <- factor(newdata$base_model,  levels = levels(mf$base_model))
  if ("dc_solution" %in% names(mf) && "dc_solution" %in% names(newdata))
    newdata$dc_solution <- factor(newdata$dc_solution, levels = levels(mf$dc_solution))

  # 2) thresholds first (robust): infer K from the fitted response
  resp <- mf[[1]]
  K <- length(levels(resp)); stopifnot(K >= 2)
  cf   <- coef(mod)
  if (length(cf) < (K - 1)) stop("coef shorter than thresholds imply.")
  theta <- as.numeric(cf[seq_len(K - 1)])
  beta  <- as.numeric(cf[-seq_len(K - 1)]); names(beta) <- names(cf)[-seq_len(K - 1)]

  # 3) Build fixed-effects model matrix X for newdata (marginal part only)
  TT <- stats::delete.response(stats::terms(mod))
  X  <- stats::model.matrix(TT, newdata, contrasts.arg = mod$contrasts)

  # Align columns: add missing betas as 0, drop extras
  miss <- setdiff(names(beta), colnames(X))
  if (length(miss)) {
    X <- cbind(X, matrix(0, nrow = nrow(X), ncol = length(miss),
                         dimnames = list(NULL, miss)))
  }
  X <- X[, names(beta), drop = FALSE]

  # 4) Linear predictor (population level)
  eta <- as.numeric(X %*% beta)

  # Optionally add dc_solution random intercept (if present)
  if (isTRUE(add_dc_solution_re) && "dc_solution" %in% names(newdata)) {
    re <- try(suppressWarnings(ranef(mod)$dc_solution), silent = TRUE)
    if (!inherits(re, "try-error") && !is.null(re)) {
      # rownames(re) should be the levels; column "(Intercept)" often present
      col_ok <- intersect(c("(Intercept)", "Intercept"), colnames(re))
      if (length(col_ok)) {
        ri <- re[as.character(newdata$dc_solution), col_ok[1]]
        ri[is.na(ri)] <- 0
        eta <- eta + as.numeric(ri)
      }
    }
  }

  # 5) Cumulative probs with logit link: F_k = logistic(theta_k - eta)
  Fmat <- plogis(outer(theta, eta, FUN = function(th, e) th - e)) # (K-1) x N
  Fmat <- t(Fmat)  # N x (K-1)

  # 6) Category probabilities: p1=F1; pk=Fk-F(k-1); pK=1-F(K-1)
  N <- nrow(newdata)
  P <- matrix(NA_real_, nrow = N, ncol = K)
  P[, 1]      <- Fmat[, 1]
  if (K > 2) P[, 2:(K - 1)] <- Fmat[, 2:(K - 1)] - Fmat[, 1:(K - 2)]
  P[, K]      <- 1 - Fmat[, K - 1]

  # Label categories as 0..K-1 (matches your later code)
  cats <- 0:(K - 1)
  colnames(P) <- as.character(cats)

  dfP <- as.data.frame(P)

  probs_long <- dfP |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(-row, names_to = "category", values_to = "prob") |>
    dplyr::left_join(dplyr::mutate(newdata, row = dplyr::row_number()), by = "row") |>
    dplyr::select(-row)

  ge_idx <- which(cats >= geq_k)
  probs_geq <- newdata |>
    dplyr::mutate(prob_geq = if (length(ge_idx)) rowSums(P[, ge_idx, drop = FALSE]) else 0,
                  threshold_k = geq_k)

  list(probs_long = probs_long, probs_geq = probs_geq)
}

# I/O
out_dir<- "clmm_outputs"
rds_path <- file.path(out_dir, "df_prepped.rds")

if (!file.exists(rds_path)) {
  stop(sprintf("RDS not found at: %s\n(getwd() = %s)", rds_path, getwd()))
}

df <- readRDS(file.path(rds_path))
dir.create(file.path(out_dir, "summaries"), showWarnings = FALSE)
dir.create(file.path(out_dir, "effects"),   showWarnings = FALSE)
dir.create(file.path(out_dir, "probs"),     showWarnings = FALSE)

# ---- overall model ----
m_overall <- clmm(
  rating ~ condition + base_model + condition:base_model +
    (1 | pair_id) + (1 | dc_solution),
  data = df, link = "logit", Hess = TRUE
)
# ---- overall model ----
m_overall <- clmm(
  rating ~ condition + base_model + condition:base_model +
    (1 | pair_id) + (1 | dc_solution),
  data = df, link = "logit", Hess = TRUE
)

# === AUDIT: log what model we used ===
model_class <- paste(class(m_overall), collapse = "; ")
K <- length(levels(m_overall$model[[1]]))  # number of rating categories seen in the fit
info_lines <- c(
  "=== OVERALL MODEL INFO ===",
  paste0("model_class: ", model_class),
  paste0("link: logit; type: CLMM; K_levels: ", K),
  paste0("nobs: ", nrow(m_overall$model)),
  paste0("formula: ", deparse(formula(m_overall)))
)
cat(paste(info_lines, collapse = "\n"), "\n")

# also save to a small txt for traceability
dir.create(file.path(out_dir, "summaries"), showWarnings = FALSE, recursive = TRUE)
writeLines(info_lines, con = file.path(out_dir, "summaries", "overall_model_info.txt"))

add_re <- TRUE  # set to FALSE if you want population-level only

# ... later, when predicting per base×solution:
pr_bm_sol <- predict_probs_clmm(m_overall, grid_bm_sol, geq_k = 3, add_dc_solution_re = add_re)

# append this flag to the info file
write(paste0("predictions_add_dc_solution_RE: ", add_re),
      file = file.path(out_dir, "summaries", "overall_model_info.txt"),
      append = TRUE)

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

write_csv_safely(bm_effects, file.path(out_dir,"effects","overall_condition_OR_by_base_model.csv"))

# population-level probabilities by condition (at ref base_model)
nd_overall <- expand.grid(condition = levels(df$condition),
                          base_model = ref_bm,
                          KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

pr_overall <- predict_probs_clmm(m_overall, nd_overall, geq_k = 3)
write_csv_safely(pr_overall$probs_long, file.path(out_dir,"probs","probs_by_condition_overall_refBM.csv"))
write_csv_safely(pr_overall$probs_geq,  file.path(out_dir,"probs","prob_ge3_by_condition_overall_refBM.csv"))

# optional: average across base models
nd_bm <- expand.grid(condition = levels(df$condition),
                     base_model = bm_levels,
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
pr_bm <- predict_probs_clmm(m_overall, nd_bm, geq_k = 3)
avg_probs_geq <- pr_bm$probs_geq %>%
  group_by(condition) %>% summarise(prob_geq = mean(prob_geq), .groups="drop")
write_csv_safely(avg_probs_geq, file.path(out_dir, "probs", "prob_ge3_by_condition_avgBM.csv"))

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

# NEW: per-base × solution predictions from the OVERALL model
grid_bm_sol <- expand.grid(
  condition   = levels(df$condition),
  base_model  = levels(df$base_model),
  dc_solution = levels(df$dc_solution),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
pr_bm_sol <- predict_probs_clmm(m_overall, grid_bm_sol, geq_k = 3, add_dc_solution_re = TRUE)

write_csv_safely(
  pr_bm_sol$probs_geq,
  file.path(out_dir, "probs", "prob_ge3_by_condition_base_model_solution_FROM_OVERALL_WITH_RE.csv")
)
write_csv_safely(
  pr_bm_sol$probs_long,
  file.path(out_dir, "probs", "probs_by_condition_base_model_solution_FROM_OVERALL_WITH_RE.csv")
)

cat("Wrote overall per-base×solution predictions to: ",
    normalizePath(file.path(out_dir, "probs"), winslash="\\", mustWork = FALSE), "\n", sep = "")

chk <- readr::read_csv("clmm_outputs/probs/prob_ge3_by_condition_base_model_solution_FROM_OVERALL_WITH_RE.csv", show_col_types = FALSE)

# count unique values per (condition, base_model)
chk %>%
  dplyr::group_by(condition, base_model) %>%
  dplyr::summarise(n_solutions = dplyr::n(),
                   n_unique_probs = dplyr::n_distinct(prob_geq),
                   .groups = "drop")
