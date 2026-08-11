# =========================
# 17_interaction_summary.R
# Fits CLMM (effect coding) + writes summary + Tables 1/2/3 (.tex/.png/.txt)
# =========================

# ---- Paths (edit if needed) ----
indir   <- "clean"
in_csv  <- "feasibility_bianeu_clean.csv"
outdir  <- "clean/feas_out"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---- Packages ----
need <- c("dplyr","tibble","emmeans","ordinal","gt","kableExtra","stringr","forcats","readr")
to_install <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(need, library, character.only = TRUE))
if (!requireNamespace("webshot2", quietly = TRUE)) install.packages("webshot2", repos = "https://cloud.r-project.org")

# ---- Helpers ----
save_as_latex <- function(df, file, caption = NULL){
  tab <- knitr::kable(
    df, format = "latex", booktabs = TRUE, longtable = FALSE,
    caption = caption, escape = TRUE, linesep = ""
  ) |>
    kableExtra::kable_styling(latex_options = c("hold_position", "striped"))
  kableExtra::save_kable(tab, file)
}
save_as_png <- function(df, file, title = NULL, subtitle = NULL){
  tbl <- df |>
    gt::gt() |>
    gt::tab_header(title = title, subtitle = subtitle) |>
    gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 2) |>
    gt::cols_align(align = "left", columns = dplyr::everything())
  gt::gtsave(tbl, filename = file)  # requires webshot2
}
save_as_txt <- function(df, file, header = NULL){
  con <- file(file, open = "wt")
  on.exit(close(con), add = TRUE)
  if (!is.null(header)) writeLines(header, con)
  capture.output(print(df, row.names = FALSE), file = con)
}

# Standardize emmeans output; ensure OR & OR-scale CIs (auto-detect CI scale)
std_emm <- function(x){
  df <- as.data.frame(x)
  nm <- names(df)
  # OR column
  if ("odds.ratio" %in% nm) df$OR <- df$odds.ratio
  else if ("ratio" %in% nm) df$OR <- df$ratio
  else if ("response" %in% nm) df$OR <- df$response
  else if ("estimate" %in% nm) df$OR <- exp(df$estimate) else
    stop("No OR-like column found.")
  # CI columns -> auto-detect link vs OR scale
  if (all(c("lower.CL","upper.CL") %in% nm)) {
    if (any(df$lower.CL <= 0 | df$upper.CL <= 0, na.rm = TRUE)) {
      df$LCL <- exp(df$lower.CL); df$UCL <- exp(df$upper.CL)
    } else {
      df$LCL <- df$lower.CL; df$UCL <- df$upper.CL
    }
  } else if (all(c("estimate","SE") %in% nm)) {
    df$LCL <- exp(df$estimate - 1.96*df$SE)
    df$UCL <- exp(df$estimate + 1.96*df$SE)
  } else {
    df$LCL <- NA_real_; df$UCL <- NA_real_
  }
  if (!("p.value" %in% nm)) df$p.value <- NA_real_
  df
}
fmt_or_table <- function(df, cols_order){
  df |>
    dplyr::transmute(
      !!!rlang::syms(cols_order),
      OR = round(OR, 2),
      `95% CI` = paste0("(", round(LCL, 2), ", ", round(UCL, 2), ")"),
      p  = if ("p.value" %in% names(df))
              format.pval(df$p.value, digits = 3, eps = 1e-4)
           else NA_character_
    )
}

# Robust ordered rating (handles 0–4 or 1–4; numeric/char/factor)
make_ordered_rating <- function(x) {
  xn <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.finite(xn))) {
    lv <- sort(unique(xn))
    factor(xn, levels = lv, ordered = TRUE)
  } else if (is.ordered(x)) x else factor(x, ordered = TRUE)
}

# ---- Build data (feas_llm_i) ----
if (!exists("feas_llm_i")) {
  message("feas_llm_i not found in environment; reading from CSV...")
  dat <- readr::read_csv(file.path(indir, in_csv), show_col_types = FALSE)

  req_cols <- c("source","base_model","scenario","rating","id_key")
  miss <- setdiff(req_cols, names(dat))
  if (length(miss)) stop("Missing required column(s): ", paste(miss, collapse = ", "))

  dat <- dat |>
    dplyr::filter(source == "LLM") |>
    dplyr::mutate(
      scenario = forcats::fct(
        scenario,
        levels = c("NEU","BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH")
      ),
      rating_ord = make_ordered_rating(rating),
      id_key     = if (!is.factor(id_key)) factor(id_key) else id_key
    )

  # keep base_models with rating variation
  bm_keep <- dat |>
    dplyr::group_by(base_model) |>
    dplyr::summarise(n_unique = dplyr::n_distinct(rating), .groups = "drop") |>
    dplyr::filter(n_unique > 1) |>
    dplyr::pull(base_model)

  feas_llm_i <- dat |>
    dplyr::filter(base_model %in% bm_keep) |>
    dplyr::mutate(
      base_model = forcats::fct_drop(forcats::as_factor(base_model)),
      scenario   = forcats::fct_drop(scenario)
    )
  if (length(unique(feas_llm_i$base_model)) < 2) {
    stop("After filtering, fewer than 2 base models remain. Cannot fit interaction.")
  }
}

# ---- Fit CLMM with effect coding ----
op <- options(contrasts = c("contr.sum","contr.poly"))
on.exit(options(op), add = TRUE)

fit_ec <- ordinal::clmm(
  rating_ord ~ scenario * base_model + (1 | id_key),
  data = feas_llm_i, link = "logit", Hess = TRUE, nAGQ = 5
)

# ---- Model summary (TXT) ----
sum_txt <- file.path(outdir, "feasfit_llm_effectcoded_summary.txt")
capture.output({
  cat("==== CLMM (logit) with Effect Coding (Sum-to-Zero) ====\n")
  cat("Formula: rating_ord ~ scenario * base_model + (1 | id_key)\n\n")
  cat("Rows: ", nrow(feas_llm_i), "\n")
  cat("Base models: ", paste(levels(feas_llm_i$base_model), collapse = ", "), "\n")
  cat("Scenarios: ", paste(levels(feas_llm_i$scenario), collapse = ", "), "\n\n")
  print(summary(fit_ec))
  cat("\nNOTES:\n",
      "- Effect coding: coefficients are deviations around grand means (no single reference).\n",
      "- For direct comparisons, see Tables 2 & 3 (emmeans pairwise, OR scale).\n", sep = "")
}, file = sum_txt)

# ---- Table 1: Fixed effects (effect-coded) with OR & Wald 95% CI ----
coef_mat <- as.data.frame(summary(fit_ec)$coefficients)
t1_df <- coef_mat |>
  tibble::rownames_to_column("term") |>
  dplyr::mutate(
    OR     = exp(Estimate),
    LCL    = exp(Estimate - 1.96 * `Std. Error`),
    UCL    = exp(Estimate + 1.96 * `Std. Error`),
    `Estimate (log-odds)` = round(Estimate, 3),
    SE     = round(`Std. Error`, 3),
    OR     = round(OR, 2),
    `95% CI` = paste0("(", round(LCL, 2), ", ", round(UCL, 2), ")"),
    z      = round(`z value`, 2),
    p      = format.pval(`Pr(>|z|)`, digits = 3, eps = 1e-4)
  ) |>
  dplyr::select(term, `Estimate (log-odds)`, SE, OR, `95% CI`, z, p)

t1_tex <- file.path(outdir, "table1_fixed_effects_effectcoded_OR.tex")
t1_png <- file.path(outdir, "table1_fixed_effects_effectcoded_OR.png")
t1_txt <- file.path(outdir, "table1_fixed_effects_effectcoded_OR.txt")
save_as_latex(t1_df, t1_tex,
  caption = "Fixed effects (effect coding): odds ratios with Wald 95\\% CI (relative to grand mean)")
save_as_png(t1_df, t1_png,
  title = "Fixed effects (effect coding)", subtitle = "ORs with Wald 95% CI (vs grand mean)")
save_as_txt(t1_df, t1_txt,
  header = "Fixed effects (effect coding): OR (Wald 95% CI). Coefficients are deviations from grand mean.\n")

# ---- Table 2: base_model pairwise within each scenario (OR, 95% CI) ----
emm_bm_by_scen <- emmeans::emmeans(fit_ec, ~ base_model | scenario)
bm_pairs   <- emmeans::contrast(emm_bm_by_scen, method = "pairwise")
bm_pairs_resp  <- summary(bm_pairs, infer = TRUE, type = "response")
bm_tbl         <- std_emm(bm_pairs_resp) |> fmt_or_table(cols_order = c("scenario","contrast"))

bm_tex <- file.path(outdir, "table2_bm_pairsby_scenario_OR.tex")
bm_png <- file.path(outdir, "table2_bm_pairsby_scenario_OR.png")
bm_txt <- file.path(outdir, "table2_bm_pairsby_scenario_OR.txt")
save_as_latex(bm_tbl, bm_tex,
  caption = "All-pairs base\\_model comparisons within each scenario (odds ratios, 95\\% CI)")
save_as_png(bm_tbl, bm_png, title = "Base Model Pairwise by Scenario", subtitle = "Odds Ratios (95% CI)")
save_as_txt(bm_tbl, bm_txt, header = "Base model pairwise comparisons within each scenario (OR, 95% CI)\n")

# ---- Table 3: selected scenario contrasts within each base_model (OR, 95% CI, z, p) ----
emm_scen_by_bm <- emmeans::emmeans(fit_ec, ~ scenario | base_model)

# IMPORTANT: These vectors assume scenario levels are exactly:
# c("NEU","BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH")
my_contrasts <- list(
  "BIAS_WORD - NEU"     = c(-1, +1,  0,  0,  0),
  "BIAS_EXAMPLE - NEU"  = c(-1,  0, +1,  0,  0),
  "BIAS_NUM_LOW - NEU"  = c(-1,  0,  0, +1,  0),
  "BIAS_NUM_HIGH - NEU" = c(-1,  0,  0,  0, +1)
)

scen_contr <- emmeans::contrast(emm_scen_by_bm, method = my_contrasts, by = "base_model")

# Summarize on LINK (log-odds) scale; include SE and p
df <- as.data.frame(summary(scen_contr, infer = TRUE, type = "link", adjust = "holm"))

# --- Build CI on link scale robustly ---
LCL_link <- if ("lower.CL"  %in% names(df)) df$lower.CL  else
            if ("asymp.LCL" %in% names(df)) df$asymp.LCL else
            if (all(c("estimate","SE") %in% names(df))) df$estimate - 1.96*df$SE else
            stop("No CI columns and cannot compute from estimate/SE.")
UCL_link <- if ("upper.CL"  %in% names(df)) df$upper.CL  else
            if ("asymp.UCL" %in% names(df)) df$asymp.UCL else
            if (all(c("estimate","SE") %in% names(df))) df$estimate + 1.96*df$SE else
            stop("No CI columns and cannot compute from estimate/SE.")

# Detect test-stat column (z or t)
stat_nm <- intersect(c("z.ratio","t.ratio"), names(df))
stat_nm <- if (length(stat_nm)) stat_nm[1] else NA_character_
# df: from summary(scen_contr, infer = TRUE, type = "link", adjust = "holm")
# stat_nm: one of "z.ratio" or "t.ratio" if present

# --- Compute raw p from the test statistic; then Holm-adjust within each base_model ---
df <- df |>
  dplyr::mutate(
    z_val = if (!is.na(stat_nm)) .data[[stat_nm]] else NA_real_,
    p_raw = if (!is.na(stat_nm)) 2 * pnorm(abs(z_val), lower.tail = FALSE) else NA_real_
  ) |>
  dplyr::group_by(base_model) |>
  dplyr::mutate(p_holm = if (all(is.na(p_raw))) NA_real_ else p.adjust(p_raw, method = "holm")) |>
  dplyr::ungroup()


sc_tbl <- df |>
  dplyr::mutate(
    OR        = round(exp(estimate), 2),
    `95% CI`  = paste0("(", round(exp(LCL_link), 2), ", ", round(exp(UCL_link), 2), ")"),
    z         = round(z_val, 2),
    p_show    = dplyr::case_when(
                  "p.value" %in% names(df) & !is.na(p.value) ~ p.value,
                  TRUE ~ p_raw
                ),
    p         = format.pval(p_show, digits = 3, eps = 1e-4),
    p_holm    = ifelse(is.na(p_holm), NA_character_, format.pval(p_holm, digits = 3, eps = 1e-4))
  ) |>
  dplyr::select(base_model, contrast, OR, `95% CI`, z, p, p_holm)

# Save
sc_tex <- file.path(outdir, "table3_selected_scenario_pairs_by_bm_OR.tex")
sc_png <- file.path(outdir, "table3_selected_scenario_pairs_by_bm_OR.png")
sc_txt <- file.path(outdir, "table3_selected_scenario_pairs_by_bm_OR.txt")

save_as_latex(sc_tbl, sc_tex,
  caption = "Selected scenario contrasts within each base\\_model (odds ratios, 95\\% CI, z, p; Holm-adjusted)")
save_as_png(sc_tbl, sc_png,
  title = "Selected Scenario Contrasts by Base Model",
  subtitle = "Odds Ratios (95% CI), z and p values")
save_as_txt(sc_tbl, sc_txt,
  header = "Selected scenario contrasts within each base model (OR, 95% CI, z, p; Holm-adjusted)\n")

# ---- Done ----
message("✅ Wrote:\n",
        "  - ", sum_txt, "\n",
        "  - ", t1_tex, "\n  - ", t1_png, "\n  - ", t1_txt, "\n",
        "  - ", bm_tex, "\n  - ", bm_png, "\n  - ", bm_txt, "\n",
        "  - ", sc_tex, "\n  - ", sc_png, "\n  - ", sc_txt, "\n")
