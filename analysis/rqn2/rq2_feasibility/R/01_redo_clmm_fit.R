# ==== FIT CLMM + ORs per base_model (CONTEXT vs ZEROSHOT) ====

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
needed <- c("lme4", "ordinal", "broom.mixed")
to_install <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

library(lme4)
library(ordinal)
library(broom.mixed)


out_dir <- "trial_outputs"
df <- readRDS(file.path(out_dir, "df_prepped_feas.rds"))

write_csv_safely <- function(x, path){
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) file.remove(path)
  readr::write_csv(x, path)
  message("Wrote: ", normalizePath(path, winslash="\\", mustWork=FALSE), " (rows=", nrow(x), ")")
}

# both conditions present?
df %>%
  count(base_model, condition) %>%
  pivot_wider(names_from = condition, values_from = n, values_fill = 0) %>%
  mutate(has_both = ZEROSHOT > 0 & CONTEXT > 0)

df <- droplevels(df)
df$condition  <- relevel(df$condition, "ZEROSHOT")
# choose a reference base_model that has both conditions:
ref_bm <- with(df, names(sort(table(base_model[condition=="ZEROSHOT"]), decreasing=TRUE)))[1]
df$base_model <- relevel(droplevels(df$base_model), ref = ref_bm)

# Interaction model (your current)
m_feas_clmm_int <- clmm(rating ~ condition * base_model + (1|pair_id),
                        data=df, link="logit", Hess=TRUE)
                        # 3) Save summary
dir.create(file.path(out_dir), showWarnings = FALSE, recursive = TRUE)
sink(file.path(out_dir, "feas_overall_summary_clmm_int_redo.txt"))
cat("=== FEASIBILITY CLMM (overall) ===\n")
cat("Reference base_model: ", ref_bm, "\n\n")
print(summary(m_feas_clmm_int))
sink()

# Simpler (no interaction) if needed
m_feas_clmm_main <- clmm(rating ~ condition + base_model + (1|pair_id),
                         data=df, link="logit", Hess=TRUE)

# 3) Save summary
dir.create(file.path(out_dir), showWarnings = FALSE, recursive = TRUE)
sink(file.path(out_dir, "feas_overall_summary_clmm_main_redo.txt"))
cat("=== FEASIBILITY CLMM (overall) ===\n")
cat("Reference base_model: ", ref_bm, "\n\n")
print(summary(m_feas_clmm_main))
sink()


df_bin <- df %>% mutate(y_ge3 = as.integer(as.integer(as.character(rating)) >= 3))
df_bin$condition  <- relevel(df_bin$condition, "ZEROSHOT")
# ensure ref base_model has both conditions
bm_cov <- df_bin %>% count(base_model, condition) %>%
  pivot_wider(names_from=condition, values_from=n, values_fill=0)
ref_bm <- bm_cov %>% filter(ZEROSHOT>0, CONTEXT>0) %>% pull(base_model) %>% as.character() %>% .[1]
df_bin$base_model <- relevel(droplevels(df_bin$base_model), ref = ref_bm)

# Fit GLMM
m_feas_glmm <- glmer(y_ge3 ~ condition * base_model + (1|pair_id),
                     data = df_bin, family = binomial())

# 3) Save summary
dir.create(file.path(out_dir), showWarnings = FALSE, recursive = TRUE)
sink(file.path(out_dir, "feas_overall_summary_glmm_redo.txt"))
cat("=== FEASIBILITY GLMM (overall) ===\n")
cat("Reference base_model: ", ref_bm, "\n\n")
print(summary(m_feas_glmm))
sink()

# Build per-base_model ORs (CONTEXT vs ZEROSHOT)
co <- broom.mixed::tidy(m_feas_glmm, effects="fixed", conf.int=TRUE, conf.method="Wald")

get_est_se <- function(term) {
  row <- dplyr::filter(co, term == !!term)
  if (nrow(row)==0) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
  c(row$estimate[1], row$std.error[1], row$conf.low[1], row$conf.high[1])
}

cond_est <- get_est_se("conditionCONTEXT")

or_by_bm <- lapply(levels(df_bin$base_model), function(bm){
  inter <- get_est_se(paste0("conditionCONTEXT:base_model", bm))
  est   <- (ifelse(is.na(cond_est[1]), NA_real_, cond_est[1])) + (ifelse(is.na(inter[1]), 0, inter[1]))
  se    <- if (is.na(cond_est[2]) || is.na(inter[2])) NA_real_ else sqrt(cond_est[2]^2 + inter[2]^2)
  # Wald CI on log-odds:
  lo <- if (is.na(se)) NA_real_ else est - 1.96*se
  hi <- if (is.na(se)) NA_real_ else est + 1.96*se
  data.frame(base_model=bm,
             OR=exp(est), OR_l95=exp(lo), OR_u95=exp(hi), check.names=FALSE)
}) %>% bind_rows()

# Save
readr::write_csv(or_by_bm, "trial_outputs/effects/feas_overall_OR_by_base_model_glmm_binary_ge3.csv")
