# --- Predicted P(Y>=3) using emmeans probabilities (from CSV, no in-memory deps) ---

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ordinal)  # clmm
  library(emmeans)
  library(tidyr)
})

# ---- Paths ----
indir   <- "clean"                 # folder containing the input CSV
in_csv  <- "feasibility_bianeu_clean.csv"
out_dir <- "clean/feas_out"        # output folder

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Read data from disk ----
dat <- readr::read_csv(file.path(indir, in_csv), show_col_types = FALSE) %>%
  filter(source == "LLM")

# ---- Ensure types needed by clmm/emmeans ----
# rating_ord must be an ordered factor with levels 0 < 1 < 2 < 3 < 4
# (adjust the column name if your ordinal response is named differently)
if (!is.ordered(dat$rating_ord)) {
  # Coerce safely; cover numeric or character inputs
  dat <- dat %>%
    mutate(
      rating_ord = as.character(rating_ord),
      rating_ord = factor(rating_ord, levels = as.character(0:4), ordered = TRUE)
    )
}

# condition should be NEU vs BIASED.
# If you actually use "scenario" categories and derive condition, uncomment this:
# dat <- dat %>%
#   mutate(condition = ifelse(scenario == "NEU", "NEU", "BIASED"))

# Make sure base_model and condition are factors (helps emmeans labeling)
dat <- dat %>%
  mutate(
    base_model = factor(base_model),
    condition  = factor(condition)  # expected to already be NEU / BIASED
  )

# ---- Fit the ordinal model from the file-backed data ----
has_id_key <- "id_key" %in% names(dat)

fit_int <- tryCatch({
  if (has_id_key) {
    clmm(rating_ord ~ base_model * condition + (1 | id_key),
         data = dat, link = "logit", Hess = TRUE)
  } else {
    clm(rating_ord ~ base_model * condition,
        data = dat, link = "logit", Hess = TRUE)
  }
}, error = function(e) e)

if (inherits(fit_int, "error")) {
  stop("Model fit failed: ", conditionMessage(fit_int))
}

# ---- Get per-category probabilities by base_model × condition ----
emm_prob <- as.data.frame(
  emmeans(fit_int, ~ rating_ord | base_model * condition, mode = "prob")
)

# ---- Sum probs for categories 3 and 4 → P(Y >= 3) ----
pge3 <- emm_prob %>%
  dplyr::filter(rating_ord %in% c("3", "4")) %>%
  dplyr::group_by(base_model, condition) %>%
  dplyr::summarise(p_ge3 = sum(prob), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = condition, values_from = p_ge3) %>%
  dplyr::mutate(diff_p_ge3 = BIASED - NEU)

# ---- Save to disk ----
readr::write_csv(pge3, file.path(out_dir, "feas_pred_pge3.csv"))
message("✅ Wrote: ", file.path(out_dir, "feas_pred_pge3.csv"))
