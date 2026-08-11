# ==== FIT CLMM + ORs per base_model (CONTEXT vs ZEROSHOT) ====

suppressPackageStartupMessages({
  library(ordinal)
  library(broom)
  library(dplyr)
  library(readr)
})

out_dir <- "feas_clmm_outputs"
df <- readRDS(file.path(out_dir, "df_prepped_feas.rds"))

write_csv_safely <- function(x, path){
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) file.remove(path)
  readr::write_csv(x, path)
  message("Wrote: ", normalizePath(path, winslash="\\", mustWork=FALSE), " (rows=", nrow(x), ")")
}

# 0) Clean factors and set condition reference
df <- droplevels(df)
df$condition <- stats::relevel(df$condition, ref = "ZEROSHOT")

#================================check for pair existence=================================================#

df %>%
  count(base_model, condition) %>%                # counts per BM × condition
  tidyr::pivot_wider(names_from = condition,
                     values_from = n, values_fill = 0) %>%
  mutate(has_both = ZEROSHOT > 0 & CONTEXT > 0) %>%
  arrange(desc(has_both))

#flag missing base models
bm_missing <- df %>%
  count(base_model, condition) %>%
  tidyr::pivot_wider(names_from = condition,
                     values_from = n, values_fill = 0) %>%
  filter(!(ZEROSHOT > 0 & CONTEXT > 0)) %>%
  pull(base_model)

bm_missing
#write also this in csv
write_csv_safely( tibble(base_model = bm_missing),  file.path(out_dir, "summaries", "feasibility_base_model_missing.csv"))
#stop if base model is missing
if (length(bm_missing)) { stop("These base_model(s) lack both conditions: ",  paste(bm_missing, collapse = ", ")) }

tab <- with(df, table(base_model, condition))
tab <- rowSums(tab == 0)  # >0 means that base_model is missing at least one condition
#save this info in csv
write_csv_safely( as.data.frame(tab), file.path(out_dir, "summaries", "feasibility_base_model_condition_coverage.csv"))
#================================================================================#

# 1) Find base_models with BOTH conditions; pick one as reference
bm_cov <- df %>%
  group_by(base_model) %>%
  summarise(n_cond = n_distinct(condition), n = n(), .groups = "drop")

bm_valid <- bm_cov %>% filter(n_cond == 2) %>% pull(base_model) %>% as.character()
if (length(bm_valid) == 0) stop("No base_model has both conditions; cannot estimate ORs.")

ref_bm <- bm_valid[1]  # choose the first valid as reference
df$base_model <- relevel(droplevels(df$base_model), ref = ref_bm)

message("Reference base_model set to: ", ref_bm)

# 2) Fit CLMM (overall feasibility)
m <- clmm(
  rating ~ condition + base_model + condition:base_model + (1 | pair_id),
  data = df, link = "logit", Hess = TRUE
)

# 3) Save summary
dir.create(file.path(out_dir, "summaries"), showWarnings = FALSE, recursive = TRUE)
sink(file.path(out_dir, "summaries", "feas_overall_summary.txt"))
cat("=== FEASIBILITY CLMM (overall) ===\n")
cat("Reference base_model: ", ref_bm, "\n\n")
print(summary(m))
sink()

# 4) ORs per base_model (CONTEXT vs ZEROSHOT), NA-safe
co <- broom::tidy(m, conf.int = FALSE)

get_est_se <- function(tbl, term_name){
  row <- dplyr::filter(tbl, term == term_name)
  if (nrow(row) == 0 || !is.finite(row$estimate[1])) {
    c(est = NA_real_, se = NA_real_)
  } else {
    # allow se==0: we'll output OR with NA CIs
    se <- if (!is.finite(row$std.error[1])) NA_real_ else row$std.error[1]
    c(est = row$estimate[1], se = se)
  }
}

to_or <- function(est, se){
  if (is.na(est)) return(tibble(OR = NA_real_, OR_l95 = NA_real_, OR_u95 = NA_real_))
  if (is.na(se) || se == 0) {
    # point estimate only
    return(tibble(OR = exp(est), OR_l95 = NA_real_, OR_u95 = NA_real_))
  }
  lo <- est - 1.96*se; hi <- est + 1.96*se
  tibble(OR = exp(est), OR_l95 = exp(lo), OR_u95 = exp(hi))
}

cond_main <- get_est_se(co, "conditionCONTEXT")

or_by_bm <- purrr::map_dfr(levels(df$base_model), function(bm) {
  if (!(bm %in% bm_valid) || is.na(cond_main["est"])) {
    tibble(base_model = bm, OR = NA_real_, OR_l95 = NA_real_, OR_u95 = NA_real_)
  } else {
    inter <- get_est_se(co, paste0("conditionCONTEXT:base_model", bm))
    est_inter <- if (is.na(inter["est"])) 0 else inter["est"]  # 0 for ref_bm
    se_inter  <- if (is.na(inter["se"]))  0 else inter["se"]
    est <- cond_main["est"] + est_inter
    # combine SEs only if both available; else NA CIs
    se  <- if (!is.na(cond_main["se"]) && !is.na(se_inter)) sqrt(cond_main["se"]^2 + se_inter^2) else NA_real_
    tibble(base_model = bm) %>% dplyr::bind_cols(to_or(est, se))
  }
})

dir.create(file.path(out_dir, "effects"), showWarnings = FALSE, recursive = TRUE)
write_csv_safely(or_by_bm, file.path(out_dir, "effects", "feas_overall_OR_by_base_model.csv"))

# (Optional) print a quick coverage note so you know why any NA remains
na_bm <- or_by_bm %>% filter(is.na(OR)) %>% pull(base_model)
if (length(na_bm)) message("OR NA for base_model(s) without both conditions: ", paste(na_bm, collapse = ", "))
#================================================================================================#
message("Feasibility CLMM finished. One OR per base_model written to effects/feas_overall_OR_by_base_model.csv")
# ==== Predicted probabilities for feasibility: P(Y>=3) ====

# Safe writer (use the same helper you already pasted)
if (!exists("write_csv_safely")) {
  write_csv_safely <- function(x, path){
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(path)) file.remove(path)
    readr::write_csv(x, path)
    message("Wrote: ", normalizePath(path, winslash="\\", mustWork = FALSE), " (rows=", nrow(x), ")")
  }
}

# Helper using fitted thresholds & fixed effects (population-level: no random effects)
predict_probs_clmm <- function(mod, newdata, geq_k = 3){
  mf <- mod$model
  # align factor levels
  if ("condition"  %in% names(newdata)) newdata$condition  <- factor(newdata$condition,  levels = levels(mf$condition))
  if ("base_model" %in% names(newdata)) newdata$base_model <- factor(newdata$base_model, levels = levels(mf$base_model))

  # thresholds first K-1, then betas
  resp  <- mf[[1]]                         # ordered response
  K     <- length(levels(resp))
  cf    <- coef(mod)
  stopifnot(length(cf) >= (K-1))
  theta <- as.numeric(cf[seq_len(K-1)])
  beta  <- as.numeric(cf[-seq_len(K-1)]); names(beta) <- names(cf)[-seq_len(K-1)]

  # fixed-effects design matrix
  TT <- stats::delete.response(stats::terms(mod))
  X  <- stats::model.matrix(TT, newdata, contrasts.arg = mod$contrasts)
  miss <- setdiff(names(beta), colnames(X))
  if (length(miss)) {
    X <- cbind(X, matrix(0, nrow=nrow(X), ncol=length(miss), dimnames=list(NULL, miss)))
  }
  X <- X[, names(beta), drop = FALSE]

  eta  <- as.numeric(X %*% beta)
  Fmat <- plogis(outer(theta, eta, function(th,e) th - e))  # (K-1) x N
  Fmat <- t(Fmat)

  N <- nrow(newdata)
  P <- matrix(NA_real_, nrow = N, ncol = K)
  P[,1] <- Fmat[,1]
  if (K > 2) P[,2:(K-1)] <- Fmat[,2:(K-1)] - Fmat[,1:(K-2)]
  P[,K] <- 1 - Fmat[,K-1]

  ge_idx <- which(0:(K-1) >= geq_k)
  out <- tibble::as_tibble(newdata) |>
    dplyr::mutate(prob_geq = if (length(ge_idx)) rowSums(P[, ge_idx, drop = FALSE]) else 0,
                  threshold_k = geq_k)
  out
}

dir.create(file.path(out_dir, "probs"), showWarnings = FALSE, recursive = TRUE)

# (A) By condition, averaged across base models
nd_cond_bm <- expand.grid(
  condition  = levels(df$condition),
  base_model = levels(df$base_model),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
pr_all <- predict_probs_clmm(m, nd_cond_bm, geq_k = 3)
feas_prob_ge3_by_condition_avgBM <- pr_all |>
  dplyr::group_by(condition) |>
  dplyr::summarise(prob_geq = mean(prob_geq), .groups = "drop")

write_csv_safely(
  feas_prob_ge3_by_condition_avgBM,
  file.path(out_dir, "probs", "feas_prob_ge3_by_condition_avgBM.csv")
)

# (B) By base_model × condition (one row per base_model per condition)
feas_prob_ge3_by_condition_base_model <- predict_probs_clmm(
  m,
  expand.grid(
    condition  = levels(df$condition),
    base_model = levels(df$base_model),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  ),
  geq_k = 3
)

write_csv_safely(
  feas_prob_ge3_by_condition_base_model,
  file.path(out_dir, "probs", "feas_prob_ge3_by_condition_base_model.csv")
)
with(df, addmargins(table(base_model, condition)))
