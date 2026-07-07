# ===== Packages =====
libs <- c("dplyr","stringr","tibble","knitr","kableExtra","gt","tools")
to_install <- libs[!sapply(libs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(libs, library, character.only = TRUE))
if (!requireNamespace("webshot2", quietly = TRUE)) install.packages("webshot2")

# ===== Helpers =====
detect_delim <- function(path) {
  first <- readLines(path, n = 1, warn = FALSE)
  if (grepl("\t", first)) "\t"
  else if (grepl(";", first)) ";"
  else ","
}

read_contrasts_csv <- function(path) {
  delim <- detect_delim(path)
  df <- readr::read_delim(path, delim = delim, show_col_types = FALSE)

  # standardize names to lowercase dot style
  std_names <- tolower(gsub("[^A-Za-z0-9]+","\\.", names(df)))
  names(df) <- std_names

  # rename common variants to canonical names
  ren <- c(
    "base.model"="base_model", "basemodel"="base_model",
    "std.error"="se", "std.err"="se",
    "p"="p.value", "pval"="p.value", "pvalue"="p.value",
    "odds.ratio"="or", "ratio"="or", "response"="or",
    "lower.cl"="lcl", "upper.cl"="ucl",
    "asymp.lcl"="lcl", "asymp.ucl"="ucl"
  )
  for (i in seq_along(ren)) {
    old <- names(ren)[i]; new <- unname(ren[i])
    if (old %in% names(df) && !(new %in% names(df))) names(df)[names(df) == old] <- new
  }

  # sanity: required columns
  req_any <- c("contrast","base_model")
  missing <- setdiff(req_any, names(df))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse=", "))

  # if estimate missing but OR exists -> estimate = log(OR)
  if (!("estimate" %in% names(df))) {
    if ("or" %in% names(df)) df$estimate <- log(df$or)
  }
  if (!("estimate" %in% names(df))) {
    stop("Need either 'estimate' (log-odds) or 'odds.ratio'/'ratio'/'response' column.")
  }

  # default df column if absent
  if (!("df" %in% names(df))) df$df <- Inf

  # ensure SE exists if we will need Wald CI
  if (!("se" %in% names(df))) {
    if (!all(c("lcl","ucl") %in% names(df))) {
      stop("No 'SE' and no 'lower.CL/upper.CL' (or asymp.LCL/UCL) to form CIs.")
    }
  }

  df
}

save_as_latex <- function(df, file, caption = NULL){
  kb <- knitr::kable(df, format = "latex", booktabs = TRUE,
                     caption = caption, align = "lrrrrrl")
  kb <- kableExtra::kable_styling(kb, latex_options = c("hold_position","striped"))
  kableExtra::save_kable(kb, file)
}

save_as_png <- function(df, file, title = NULL, subtitle = NULL){
  gt_tbl <- df |>
    gt::gt() |>
    gt::tab_header(title = title, subtitle = subtitle) |>
    gt::fmt_number(columns = where(is.numeric), decimals = 2) |>
    gt::cols_align(align = "left", columns = everything())
  gt::gtsave(gt_tbl, filename = file)
}

# ===== Main: build tables from a CSV =====
make_bias_vs_neu_table_from_csv <- function(
  csv_path = "clean/feas_out/feas_or_BIASED_vs_NEU_by_base_model.csv",
  contrast_pattern = "BIASED - NEU",
  outdir = "clean/feas_out",
  stem = "bias_vs_neu_by_model",
  caption = "BIASED vs NEU within each base_model (Cumulative OR, Wald 95% CI)",
  also_png = TRUE,
  rename_map = NULL
) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # 1) Read & filter ---------------------------------------------------------
  raw <- read_contrasts_csv(csv_path)
  df <- raw %>%
    dplyr::filter(stringr::str_detect(contrast, fixed(contrast_pattern, ignore_case = TRUE)))
  if (nrow(df) == 0) {
    stop("No rows match '", contrast_pattern, "' in column 'contrast'. Check spelling/case.")
  }

  # 2) Optional renaming (does NOT affect any calculations) ------------------
  if (!is.null(rename_map)) {
    df$base_model <- dplyr::recode(df$base_model, !!!rename_map, .default = df$base_model)
  }

  # 3) OR & CI, unconditionally ---------------------------------------------
  # OR from estimate if present; else if 'or' column exists, use it directly
  OR <- if ("estimate" %in% names(df)) exp(df$estimate) else df$or

have_ci_cols <- all(c("lcl","ucl") %in% names(df))
if (have_ci_cols) {
  # AUTO-DETECT CI SCALE:
  # If any bound ≤ 0, it cannot be an OR (ORs are > 0) → treat as link-scale and exponentiate.
  if (any(df$lcl <= 0 | df$ucl <= 0, na.rm = TRUE)) {
    LOR <- exp(df$lcl)
    UOR <- exp(df$ucl)
  } else {
    LOR <- df$lcl
    UOR <- df$ucl
  }
} else {
  # Build Wald CI on link scale and exponentiate
  if (!("se" %in% names(df))) {
    stop("No CI columns and no SE column. Provide either lcl/ucl or SE to form CIs.")
  }
  crit <- ifelse(is.finite(df$df), qt(0.975, df$df), 1.96)
  LOR <- exp(df$estimate - crit * df$se)
  UOR <- exp(df$estimate + crit * df$se)
}

  # 4) p-values & Holm -------------------------------------------------------
  p_raw  <- if ("p.value" %in% names(df)) df$p.value else NA_real_
  p_holm <- if (all(is.finite(p_raw))) p.adjust(p_raw, method = "holm") else rep(NA_real_, length(p_raw))

  # 5) Pretty columns ---------------------------------------------------------
  pct_odds  <- (OR - 1) * 100
  direction <- ifelse(OR > 1, "increase", ifelse(OR < 1, "decrease", "no change"))
  dir_sig   <- ifelse(!is.na(p_holm) & p_holm < 0.05,
                      ifelse(OR > 1, "significant increase", "significant decrease"),
                      "no clear change")

  tab <- tibble::tibble(
    base_model = df$base_model,
    OR         = round(OR, 2),
    `95% CI`   = paste0("(", round(LOR, 2), "–", round(UOR, 2), ")"),
    `p (raw)`  = if (all(is.na(p_raw))) rep("NA", length(OR)) else format.pval(p_raw, digits = 3, eps = 1e-4),
    `p (Holm)` = if (all(is.na(p_holm))) rep("NA", length(OR)) else format.pval(p_holm, digits = 3, eps = 1e-4),
    `%Δ odds`  = sprintf("%+d%%", round(pct_odds)),
    Direction  = dir_sig
  ) %>%
    dplyr::arrange(dplyr::desc(Direction != "no clear change"),
                   dplyr::desc(abs(OR - 1)))

  # 6) Write outputs ----------------------------------------------------------
  tex_path <- file.path(outdir, paste0(stem, "bm_effect_table.tex"))
  png_path <- file.path(outdir, paste0(stem, "bm_effect_table.png"))
  txt_path <- file.path(outdir, paste0(stem, "bm_effect_table.txt"))

  save_as_latex(tab, tex_path, caption = caption)
  if (also_png) save_as_png(tab, png_path,
                            title = "Biased vs Neutral by Base Model",
                            subtitle = "Cumulative OR (95% CI), Holm-adjusted p")

  lines <- sprintf(
    "%s — %s: OR = %.2f %s, p = %s; Holm = %s — %s",
    tab$base_model, contrast_pattern, tab$OR, tab$`95% CI`,
    tab$`p (raw)`, tab$`p (Holm)`, tab$Direction
  )
  writeLines(c(paste0("Results for ", contrast_pattern), lines), txt_path)

  message("✅ Wrote:\n  - ", tex_path,
          if (also_png) paste0("\n  - ", png_path) else "",
          "\n  - ", txt_path)

  return(tab)
}

# ===== Example usage =====
# Point this to your CSV/TSV with columns:
# contrast, base_model, estimate, SE, df, z.ratio, p.value  (LCL/UCL optional)
# csv_path <- "clean/feas_out/my_contrasts.csv"
# out_tab <- make_bias_vs_neu_table_from_csv(csv_path,
#                                            contrast_pattern = "BIASED - NEU",
#                                            outdir = "clean/feas_out",
#                                            stem = "bias_vs_neu_by_model")
# out_tab
# replace with your actual file path if different
csv_path <- "clean/feas_out/feas_or_BIASED_vs_NEU_by_base_model.csv"

out_tab <- make_bias_vs_neu_table_from_csv(
  csv_path,
  contrast_pattern = "BIASED - NEU",   # must match the text in your CSV's 'contrast' column
  outdir = "clean/feas_out",
  stem = "bias_vs_neu_by_model"        # file name stem for outputs
)

print(out_tab)  # <-- shows the table in the terminal
# ---- RUN: point to your CSV and run the table builder ----
# Adjust this path if your file lives elsewhere:
csv_path <- "clean/feas_out/feas_or_BIASED_vs_NEU_by_base_model.csv"

# (Optional) pretty display names for base models
rename_map <- c(
  "phi4"        = "Phi4",
  "gemma3-12b"  = "Gemma3:12B",
  "llama-pro"   = "LlaMa-Pro",
  "mistral"     = "Mistral7B"
)

# Build tables (LaTeX/PNG/TXT) and print to console
out_tab <- make_bias_vs_neu_table_from_csv(
  csv_path            = csv_path,
  contrast_pattern    = "BIASED - NEU",       # must match the text in your 'contrast' column
  outdir              = "clean/feas_out",
  stem                = "bias_vs_neu_by_model",
  caption             = "BIASED vs NEU within each base_model (Cumulative OR, Wald 95% CI)",
  also_png            = TRUE,
  rename_map          = rename_map            # remove if you don't want renaming
)

print(out_tab)

# Helpful message so you know where files went
message("Wrote: ",
        "clean/feas_out/bias_vs_neu_by_model.tex, ",
        "clean/feas_out/bias_vs_neu_by_model.png, ",
        "clean/feas_out/bias_vs_neu_by_model.txt")
