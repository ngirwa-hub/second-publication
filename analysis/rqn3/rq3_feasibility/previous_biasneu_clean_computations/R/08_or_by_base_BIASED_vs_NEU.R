# === 07_or_by_base_BIASED_vs_NEU.R ===
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(forcats)
  library(ordinal)   # clmm
  library(emmeans)   # contrasts
})

# --- paths ---
in_csv  <- "clean/feasibility_bianeu_clean.csv"   # <- your merged file
outdir  <- "clean/feas_out"
outfile <- file.path(outdir, "feas_or_BIASED_vs_NEU_by_base_model.csv")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# --- load & coerce types ---
dat <- read_csv(in_csv, show_col_types = FALSE) %>%
  mutate(
    source     = factor(source, levels = c("HUMAN","LLM")),
    base_model = factor(as.character(base_model)),
    rating     = as.integer(rating),
    rating_ord = factor(rating, ordered = TRUE, levels = 0:4),
    condition  = toupper(as.character(condition)),
    cond2      = if_else(condition == "NEU", "NEU", "BIASED"),
    cond2      = factor(cond2, levels = c("NEU","BIASED"))
  )

# LLM only for this analysis
dat_llm <- dat %>%
  filter(source == "LLM") %>%
  droplevels()

# --- fit one CLMM with interaction (random intercept by id_key) ---
fit <- clmm(rating_ord ~ cond2 * base_model + (1 | id_key),
            data = dat_llm, link = "logit", Hess = TRUE)

# --- emmeans: BIASED - NEU within each base_model (on link scale) ---
emm  <- emmeans(fit, ~ cond2 | base_model)  # link (logit) scale
# ensure order is NEU, BIASED (it is, from factor above)
ct   <- contrast(emm,
                 method = list("BIASED - NEU" = c(-1, 1)),
                 by = "base_model")

tab  <- summary(ct) %>%
  as.data.frame() %>%
  transmute(
    contrast = contrast,
    base_model = as.character(base_model),
    estimate = estimate,
    SE       = SE,
    df       = df,          # typically Inf (asymptotic z)
    z.ratio  = z.ratio,
    p.value  = p.value
  )

write_csv(tab, outfile)
message("✅ Wrote: ", outfile)

