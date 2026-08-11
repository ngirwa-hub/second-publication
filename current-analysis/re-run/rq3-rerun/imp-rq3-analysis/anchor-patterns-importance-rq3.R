# Run with:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" && Rscript "imp-rq3-analysis/anchor-patterns-importance-rq3.R"
#
# Post-estimation analysis for RQ3 importance:
#   1. word/example upper-end response concentration, P(Y = 4);
#   2. one primary-prior CSV and one primary-prior LaTeX table.
#
# Only ratings 3 and 4 occur in the importance data and are retained by
# the fitted ordinal models. P(Y = 1) is therefore not an estimated
# category, so P(Y = 1) + P(Y = 4) cannot be derived for importance.
# The valid importance endpoint estimand is P(Y = 4).
#
# This script does not refit a model or perform prior sensitivity. It
# reuses the completed primary fit from importance-rq3.R. Numeric-anchor
# ordinal effects are already reported by the main importance pipeline
# and are not paired here.

required_packages <- c(
  "brms",
  "dplyr",
  "tidyr",
  "readr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# ------------------------------------------------------------------
# 1. Resolve paths and define the analysis structure
# ------------------------------------------------------------------

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)

if (length(script_argument) != 1) {
  stop("Run this analysis with Rscript.")
}

script_dir <- dirname(
  normalizePath(
    sub("^--file=", "", script_argument),
    mustWork = TRUE
  )
)

rq3_dir <- dirname(script_dir)
working_dir <- file.path(
  rq3_dir,
  "imp-ctx-anchor-working"
)
primary_dir <- file.path(
  working_dir,
  "bayesian-results",
  "base-solution-threshold"
)
output_dir <- file.path(
  working_dir,
  "bayesian-results",
  "rq3-importance-anchor-response-patterns"
)
latex_dir <- file.path(output_dir, "latex-tables")

input_path <- file.path(
  working_dir,
  "imp-ctx-anchor-responses.csv"
)
primary_fit_path <- file.path(
  primary_dir,
  "rq3_importance_base_solution_threshold_fit.rds"
)

required_paths <- c(
  input_path,
  primary_fit_path
)
missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0) {
  stop(
    "Required RQ3 importance inputs are missing:\n",
    paste(missing_paths, collapse = "\n")
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(latex_dir, recursive = TRUE, showWarnings = FALSE)

base_model_levels <- c("gemma3", "llama", "mistral", "phi4")
comparison_levels <- c(
  "CONTEXT",
  "ANCHOR_EXAMPLE",
  "ANCHOR_WORD",
  "ANCHOR_NUM_HIGH",
  "ANCHOR_NUM_LOW"
)
extremity_anchor_levels <- c("ANCHOR_EXAMPLE", "ANCHOR_WORD")

# ------------------------------------------------------------------
# 2. Reconstruct and validate the analysis data
# ------------------------------------------------------------------

raw <- readr::read_csv(input_path, show_col_types = FALSE)

required_columns <- c(
  "row_id",
  "rating",
  "condition",
  "anchor_type",
  "base_model",
  "dc_solution",
  "iteration"
)
missing_columns <- setdiff(required_columns, names(raw))

if (length(missing_columns) > 0) {
  stop(
    "The importance CSV is missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- raw |>
  mutate(
    condition = toupper(trimws(as.character(condition))),
    anchor_type = toupper(trimws(as.character(anchor_type))),
    base_model = tolower(trimws(as.character(base_model))),
    dc_solution = trimws(as.character(dc_solution)),
    rating_value = suppressWarnings(as.integer(rating)),
    comparison = case_when(
      condition == "CONTEXT" ~ "CONTEXT",
      condition == "ANCHOR" &
        anchor_type %in% comparison_levels[-1] ~ anchor_type,
      TRUE ~ NA_character_
    )
  )

if (anyNA(df$comparison)) {
  stop("Some importance rows could not be assigned to a comparison.")
}

if (!setequal(unique(df$base_model), base_model_levels)) {
  stop("Expected exactly gemma3, llama, mistral, and phi4.")
}

if (anyNA(df$dc_solution) || any(df$dc_solution == "")) {
  stop("DC-solution names must not be missing or empty.")
}

solution_levels <- sort(unique(df$dc_solution))

if (length(solution_levels) != 11) {
  stop("Expected exactly 11 DC solutions.")
}

if (!setequal(unique(df$rating_value), c(3L, 4L))) {
  stop(
    "Upper-end analysis requires the observed importance categories 3 and 4."
  )
}

df <- df |>
  mutate(
    comparison = factor(comparison, levels = comparison_levels),
    base_model = factor(base_model, levels = base_model_levels),
    dc_solution = factor(dc_solution, levels = solution_levels),
    base_solution = interaction(
      base_model,
      dc_solution,
      drop = TRUE,
      sep = "__"
    ),
    anchor_example = as.integer(
      comparison == "ANCHOR_EXAMPLE"
    ),
    anchor_word = as.integer(
      comparison == "ANCHOR_WORD"
    ),
    anchor_num_high = as.integer(
      comparison == "ANCHOR_NUM_HIGH"
    ),
    anchor_num_low = as.integer(
      comparison == "ANCHOR_NUM_LOW"
    ),
    rating = ordered(rating_value, levels = c(3L, 4L))
  )

base_solution_levels <- levels(df$base_solution)

if (length(base_solution_levels) != 44) {
  stop("Expected exactly 44 base-model/DC-solution groups.")
}

validate_fit_data <- function(fit, prior_name) {
  required_fit_columns <- c(
    "rating",
    "base_solution",
    "anchor_example",
    "anchor_word",
    "anchor_num_high",
    "anchor_num_low"
  )

  missing_fit_columns <- setdiff(
    required_fit_columns,
    names(fit$data)
  )

  if (length(missing_fit_columns) > 0) {
    stop(
      "The ", prior_name, " fit is missing stored variables: ",
      paste(missing_fit_columns, collapse = ", ")
    )
  }

  current_identity <- df |>
    transmute(
      rating = as.integer(as.character(rating)),
      base_solution = as.character(base_solution),
      anchor_example,
      anchor_word,
      anchor_num_high,
      anchor_num_low
    )

  fit_identity <- fit$data |>
    transmute(
      rating = suppressWarnings(
        as.integer(as.character(rating))
      ),
      base_solution = as.character(base_solution),
      anchor_example = as.integer(anchor_example),
      anchor_word = as.integer(anchor_word),
      anchor_num_high = as.integer(anchor_num_high),
      anchor_num_low = as.integer(anchor_num_low)
    )

  if (!isTRUE(
    all.equal(
      as.data.frame(current_identity),
      as.data.frame(fit_identity),
      check.attributes = FALSE
    )
  )) {
    stop(
      "The current importance CSV does not match the data stored in the ",
      prior_name,
      " fitted model. Refit the corresponding pipeline before continuing."
    )
  }
}

# ------------------------------------------------------------------
# 3. Calculate word/example upper-end probability contrasts
# ------------------------------------------------------------------

probability_summary <- function(draws) {
  interval <- unname(quantile(draws, c(0.025, 0.975)))
  c(
    median = median(draws),
    l95 = interval[1],
    u95 = interval[2]
  )
}

group_mapping <- df |>
  distinct(base_model, dc_solution, base_solution) |>
  arrange(base_model, dc_solution)

extremity_grid <- tidyr::crossing(
  group_mapping,
  comparison = c("CONTEXT", extremity_anchor_levels)
) |>
  mutate(
    base_model = factor(base_model, levels = base_model_levels),
    dc_solution = factor(dc_solution, levels = solution_levels),
    base_solution = factor(
      as.character(base_solution),
      levels = base_solution_levels
    ),
    comparison = factor(comparison, levels = comparison_levels),
    anchor_example = as.integer(
      comparison == "ANCHOR_EXAMPLE"
    ),
    anchor_word = as.integer(
      comparison == "ANCHOR_WORD"
    ),
    anchor_num_high = 0L,
    anchor_num_low = 0L
  )

extract_upper_end_contrasts <- function(
    fit,
    prior_name,
    effect_prior_sd) {
  validate_fit_data(fit, prior_name)

  category_levels <- levels(fit$data$rating)

  if (!identical(category_levels, c("3", "4"))) {
    stop(
      "The ", prior_name,
      " fit does not retain importance categories 3 and 4."
    )
  }

  epred <- brms::posterior_epred(
    fit,
    newdata = extremity_grid
  )

  if (length(dim(epred)) != 3 || dim(epred)[3] != 2) {
    stop(
      "Unexpected posterior_epred dimensions for the ",
      prior_name,
      " importance fit."
    )
  }

  highest_rating_index <- match("4", category_levels)

  bind_rows(lapply(seq_len(nrow(group_mapping)), function(group_index) {
    current_base_solution <- as.character(
      group_mapping$base_solution[group_index]
    )
    current_model <- as.character(
      group_mapping$base_model[group_index]
    )
    current_solution <- as.character(
      group_mapping$dc_solution[group_index]
    )

    context_index <- which(
      as.character(extremity_grid$base_solution) ==
        current_base_solution &
        as.character(extremity_grid$comparison) == "CONTEXT"
    )

    bind_rows(lapply(extremity_anchor_levels, function(current_anchor) {
      anchor_index <- which(
        as.character(extremity_grid$base_solution) ==
          current_base_solution &
          as.character(extremity_grid$comparison) == current_anchor
      )

      if (length(context_index) != 1 || length(anchor_index) != 1) {
        stop("Failed to identify a unique importance contrast row.")
      }

      context_probability <-
        epred[, context_index, highest_rating_index]
      anchor_probability <-
        epred[, anchor_index, highest_rating_index]
      difference <- anchor_probability - context_probability

      context_summary <- probability_summary(context_probability)
      anchor_summary <- probability_summary(anchor_probability)
      difference_summary <- probability_summary(difference)
      probability_gt_0 <- mean(difference > 0)
      probability_lt_0 <- mean(difference < 0)

      conclusion <- case_when(
        difference_summary["l95"] > 0 &&
          probability_gt_0 >= 0.95 ~
          "greater upper-end concentration under Anchor",
        difference_summary["u95"] < 0 &&
          probability_lt_0 >= 0.95 ~
          "reduced upper-end concentration under Anchor",
        TRUE ~ "uncertain"
      )

      tibble(
        prior_name = prior_name,
        effect_prior_sd = effect_prior_sd,
        base_model = current_model,
        dc_solution = current_solution,
        base_solution = current_base_solution,
        anchor_type = current_anchor,
        comparison = paste0(current_anchor, " vs CONTEXT"),
        response_pattern = "upper-end probability: P(Y=4)",
        context_probability_median = context_summary["median"],
        context_probability_l95 = context_summary["l95"],
        context_probability_u95 = context_summary["u95"],
        anchor_probability_median = anchor_summary["median"],
        anchor_probability_l95 = anchor_summary["l95"],
        anchor_probability_u95 = anchor_summary["u95"],
        difference_median = difference_summary["median"],
        difference_l95 = difference_summary["l95"],
        difference_u95 = difference_summary["u95"],
        posterior_probability_difference_gt_0 = probability_gt_0,
        posterior_probability_difference_lt_0 = probability_lt_0,
        conclusion = conclusion
      )
    }))
  }))
}

message("Processing importance upper-end contrasts from the primary fit")

primary_fit <- readRDS(primary_fit_path)

primary_extremity <- extract_upper_end_contrasts(
  fit = primary_fit,
  prior_name = "primary",
  effect_prior_sd = 1.50
) |>
  arrange(base_model, dc_solution, anchor_type)

rm(primary_fit)
invisible(gc())

# ------------------------------------------------------------------
# 4. Build the single primary-prior LaTeX table
# ------------------------------------------------------------------

model_labels <- c(
  gemma3 = "Gemma3:12B",
  llama = "LLaMa-Pro",
  mistral = "Mistral",
  phi4 = "Phi-4"
)

anchor_labels <- c(
  ANCHOR_WORD = "Word",
  ANCHOR_EXAMPLE = "Example"
)

conclusion_labels <- c(
  "greater upper-end concentration under Anchor" = "Greater",
  "reduced upper-end concentration under Anchor" = "Reduced",
  "uncertain" = "Uncertain"
)

format_probability <- function(value) {
  if (value >= 0.9995) {
    return("$>0.999$")
  }
  if (value <= 0.0005) {
    return("$<0.001$")
  }
  sprintf("%.3f", value)
}

latex_table <- primary_extremity |>
  mutate(
    model_label = unname(model_labels[base_model]),
    anchor_label = unname(anchor_labels[anchor_type]),
    anchor_order = match(anchor_type, names(anchor_labels)),
    model_order = match(base_model, names(model_labels)),
    probability_label = vapply(
      posterior_probability_difference_gt_0,
      format_probability,
      character(1)
    ),
    conclusion_label = unname(conclusion_labels[conclusion])
  ) |>
  arrange(model_order, dc_solution, anchor_order)

full_latex_rows <- paste0(
  latex_table$model_label,
  " & ", latex_table$dc_solution,
  " & ", latex_table$anchor_label,
  " & ", sprintf("%.3f", latex_table$context_probability_median),
  " & ", sprintf("%.3f", latex_table$anchor_probability_median),
  " & ", sprintf("%+.3f", latex_table$difference_median),
  " & [", sprintf("%.3f", latex_table$difference_l95),
  ", ", sprintf("%.3f", latex_table$difference_u95), "]",
  " & ", latex_table$probability_label,
  " & ", latex_table$conclusion_label,
  " \\\\"
)

full_latex_lines <- c(
  "\\begin{landscape}",
  "\\begin{longtable}{lllcccccc}",
  paste0(
    "\\caption{RQ3 importance: primary-prior upper-end probability contrasts for the word and example anchors.} ",
    "\\label{tab:rq3_importance_upper_end_appendix} \\\\"
  ),
  "\\toprule",
  "Model & DC solution & Anchor & Context $P(Y=4)$ & Anchor $P(Y=4)$ & $\\Delta$ & 95\\% CrI & $P(\\Delta>0)$ & Interpretation \\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\caption[]{RQ3 importance: primary-prior upper-end probability contrasts for the word and example anchors.} \\\\ ",
  "\\toprule",
  "Model & DC solution & Anchor & Context $P(Y=4)$ & Anchor $P(Y=4)$ & $\\Delta$ & 95\\% CrI & $P(\\Delta>0)$ & Interpretation \\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\multicolumn{9}{r}{Continued on next page} \\\\ ",
  "\\midrule",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot",
  full_latex_rows,
  "\\midrule",
  paste0(
    "\\multicolumn{9}{p{0.96\\linewidth}}{\\footnotesize\\textit{Note.} ",
    "$\\Delta$ is the posterior anchor-minus-Context difference in $P(Y=4)$. ",
    "Only ratings 3 and 4 were observed in the importance data; consequently, ",
    "$P(Y=1)$ was not estimated by the fitted importance model.}\\\\"
  ),
  "\\end{longtable}",
  "\\end{landscape}"
)

greater_latex_table <- latex_table |>
  filter(conclusion == "greater upper-end concentration under Anchor")

if (nrow(greater_latex_table) == 0) {
  stop("No primary importance contrasts were classified as greater.")
}

greater_latex_rows <- paste0(
  greater_latex_table$model_label,
  " & ", greater_latex_table$dc_solution,
  " & ", greater_latex_table$anchor_label,
  " & ", sprintf("%.3f", greater_latex_table$context_probability_median),
  " & ", sprintf("%.3f", greater_latex_table$anchor_probability_median),
  " & ", sprintf("%+.3f", greater_latex_table$difference_median),
  " & [", sprintf("%.3f", greater_latex_table$difference_l95),
  ", ", sprintf("%.3f", greater_latex_table$difference_u95), "]",
  " & ", greater_latex_table$probability_label,
  " & ", greater_latex_table$conclusion_label,
  " \\\\"
)

greater_latex_lines <- c(
  "\\begin{landscape}",
  "\\begin{longtable}{lllcccccc}",
  paste0(
    "\\caption{RQ3 importance: primary-prior upper-end probability contrasts classified as greater under the word and example anchors.} ",
    "\\label{tab:rq3_importance_upper_end_greater} \\\\"
  ),
  "\\toprule",
  "Model & DC solution & Anchor & Context $P(Y=4)$ & Anchor $P(Y=4)$ & $\\Delta$ & 95\\% CrI & $P(\\Delta>0)$ & Interpretation \\\\ ",
  "\\midrule",
  "\\endfirsthead",
  "\\caption[]{RQ3 importance: primary-prior upper-end probability contrasts classified as greater under the word and example anchors.} \\\\ ",
  "\\toprule",
  "Model & DC solution & Anchor & Context $P(Y=4)$ & Anchor $P(Y=4)$ & $\\Delta$ & 95\\% CrI & $P(\\Delta>0)$ & Interpretation \\\\ ",
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\multicolumn{9}{r}{Continued on next page} \\\\ ",
  "\\midrule",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot",
  greater_latex_rows,
  "\\midrule",
  paste0(
    "\\multicolumn{9}{p{0.96\\linewidth}}{\\footnotesize\\textit{Note.} ",
    "$\\Delta$ is the posterior anchor-minus-Context difference in $P(Y=4)$. ",
    "Only contrasts classified as greater upper-end concentration under the anchor are included. ",
    "Only ratings 3 and 4 were observed in the importance data; consequently, ",
    "$P(Y=1)$ was not estimated by the fitted importance model.}\\\\"
  ),
  "\\end{longtable}",
  "\\end{landscape}"
)

# ------------------------------------------------------------------
# 5. Save the primary upper-end CSV and two LaTeX tables
# ------------------------------------------------------------------

readr::write_csv(
  primary_extremity,
  file.path(
    output_dir,
    "rq3_importance_primary_upper_end_contrasts.csv"
  )
)

writeLines(
  full_latex_lines,
  file.path(
    latex_dir,
    "rq3_importance_upper_end_appendix.tex"
  ),
  useBytes = TRUE
)

writeLines(
  greater_latex_lines,
  file.path(
    latex_dir,
    "rq3_importance_upper_end_greater_only.tex"
  ),
  useBytes = TRUE
)

cat("\nPrimary importance upper-end contrasts:\n")
print(primary_extremity)

message("\nRQ3 importance upper-end analysis complete.")
message("Results saved in: ", output_dir)
