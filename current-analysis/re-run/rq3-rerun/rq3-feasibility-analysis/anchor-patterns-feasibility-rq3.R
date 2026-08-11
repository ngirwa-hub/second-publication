# Run with:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" && Rscript "rq3-feasibility-analysis/anchor-patterns-feasibility-rq3.R"
#
# Post-estimation analysis for RQ3 feasibility:
#   1. word/example anchor-associated endpoint concentration, defined as
#      P(Y = 1) + P(Y = 4);
#   2. one primary-prior CSV and one primary-prior LaTeX table.
#
# This script does not refit a model or perform prior sensitivity. It
# reuses the completed primary fit from feasibility-rq3.R. Numeric-anchor
# ordinal effects are already reported by the main feasibility pipeline
# and are therefore not recomputed or paired here.

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

# 1. Resolve paths and define the analysis structure
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
  "feas-ctx-anchor-working"
)
primary_dir <- file.path(
  working_dir,
  "bayesian-results",
  "base-model-threshold"
)
output_dir <- file.path(
  working_dir,
  "bayesian-results",
  "rq3-feasibility-anchor-response-patterns"
)
latex_dir <- file.path(output_dir, "latex-tables")

input_path <- file.path(
  working_dir,
  "feas-ctx-anchor-responses.csv"
)
primary_fit_path <- file.path(
  primary_dir,
  "rq3_feasibility_base_model_threshold_fit.rds"
)

required_paths <- c(
  input_path,
  primary_fit_path
)
missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0) {
  stop(
    "Required RQ3 feasibility inputs are missing:\n",
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
  "iteration"
)
missing_columns <- setdiff(required_columns, names(raw))

if (length(missing_columns) > 0) {
  stop(
    "The feasibility CSV is missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- raw |>
  mutate(
    condition = toupper(trimws(as.character(condition))),
    anchor_type = toupper(trimws(as.character(anchor_type))),
    base_model = tolower(trimws(as.character(base_model))),
    rating_value = suppressWarnings(as.integer(rating)),
    comparison = case_when(
      condition == "CONTEXT" ~ "CONTEXT",
      condition == "ANCHOR" &
        anchor_type %in% comparison_levels[-1] ~ anchor_type,
      TRUE ~ NA_character_
    )
  )

if (anyNA(df$comparison)) {
  stop("Some feasibility rows could not be assigned to a comparison.")
}

if (!setequal(unique(df$base_model), base_model_levels)) {
  stop("Expected exactly gemma3, llama, mistral, and phi4.")
}

if (!setequal(unique(df$rating_value), 1:4)) {
  stop(
    "Endpoint analysis requires observed feasibility categories 1, 2, 3, and 4."
  )
}

df <- df |>
  mutate(
    comparison = factor(comparison, levels = comparison_levels),
    base_model = factor(base_model, levels = base_model_levels),
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
    rating = ordered(rating_value, levels = 1:4)
  )

validate_fit_data <- function(fit, prior_name) {
  required_fit_columns <- c(
    "rating",
    "base_model",
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
      base_model = as.character(base_model),
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
      base_model = as.character(base_model),
      anchor_example = as.integer(anchor_example),
      anchor_word = as.integer(anchor_word),
      anchor_num_high = as.integer(anchor_num_high),
      anchor_num_low = as.integer(anchor_num_low)
    )

  # Compare the stored values rather than tibble/data.frame metadata.
  if (!isTRUE(
    all.equal(
      as.data.frame(current_identity),
      as.data.frame(fit_identity),
      check.attributes = FALSE
    )
  )) {
    stop(
      "The current feasibility CSV does not match the data stored in the ",
      prior_name,
      " fitted model. Refit the primary feasibility model before continuing."
    )
  }
}

# ------------------------------------------------------------------
# 3. Calculate word/example endpoint-probability contrasts
# ------------------------------------------------------------------

probability_summary <- function(draws) {
  interval <- unname(quantile(draws, c(0.025, 0.975)))
  c(
    median = median(draws),
    l95 = interval[1],
    u95 = interval[2]
  )
}

extremity_grid <- tidyr::expand_grid(
  base_model = base_model_levels,
  comparison = c("CONTEXT", extremity_anchor_levels)
) |>
  mutate(
    base_model = factor(base_model, levels = base_model_levels),
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

extract_extremity_contrasts <- function(
    fit,
    prior_name,
    effect_prior_sd) {
  validate_fit_data(fit, prior_name)

  category_levels <- levels(fit$data$rating)

  if (!identical(category_levels, as.character(1:4))) {
    stop(
      "The ", prior_name,
      " fit does not retain feasibility categories 1, 2, 3, and 4."
    )
  }

  epred <- brms::posterior_epred(
    fit,
    newdata = extremity_grid
  )

  if (length(dim(epred)) != 3 || dim(epred)[3] != 4) {
    stop(
      "Unexpected posterior_epred dimensions for the ",
      prior_name,
      " feasibility fit."
    )
  }

  low_endpoint_index <- match("1", category_levels)
  high_endpoint_index <- match("4", category_levels)

  bind_rows(lapply(base_model_levels, function(current_model) {
    context_index <- which(
      as.character(extremity_grid$base_model) == current_model &
        as.character(extremity_grid$comparison) == "CONTEXT"
    )

    bind_rows(lapply(extremity_anchor_levels, function(current_anchor) {
      anchor_index <- which(
        as.character(extremity_grid$base_model) == current_model &
          as.character(extremity_grid$comparison) == current_anchor
      )

      if (length(context_index) != 1 || length(anchor_index) != 1) {
        stop("Failed to identify a unique feasibility contrast row.")
      }

      context_probability <-
        epred[, context_index, low_endpoint_index] +
        epred[, context_index, high_endpoint_index]

      anchor_probability <-
        epred[, anchor_index, low_endpoint_index] +
        epred[, anchor_index, high_endpoint_index]

      difference <- anchor_probability - context_probability

      context_summary <- probability_summary(context_probability)
      anchor_summary <- probability_summary(anchor_probability)
      difference_summary <- probability_summary(difference)
      probability_gt_0 <- mean(difference > 0)
      probability_lt_0 <- mean(difference < 0)

      conclusion <- case_when(
        difference_summary["l95"] > 0 &&
          probability_gt_0 >= 0.95 ~
          "greater endpoint concentration under Anchor",
        difference_summary["u95"] < 0 &&
          probability_lt_0 >= 0.95 ~
          "reduced endpoint concentration under Anchor",
        TRUE ~ "uncertain"
      )

      tibble(
        prior_name = prior_name,
        effect_prior_sd = effect_prior_sd,
        base_model = current_model,
        anchor_type = current_anchor,
        comparison = paste0(current_anchor, " vs CONTEXT"),
        response_pattern = "endpoint probability: P(Y=1) + P(Y=4)",
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

message("Processing feasibility endpoint contrasts from the primary fit")

primary_fit <- readRDS(primary_fit_path)

primary_extremity <- extract_extremity_contrasts(
  fit = primary_fit,
  prior_name = "primary",
  effect_prior_sd = 1.50
) |>
  arrange(base_model, anchor_type)

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

conclusion_labels <- c(
  "greater endpoint concentration under Anchor" = "Greater",
  "reduced endpoint concentration under Anchor" = "Reduced",
  "uncertain" = "Uncertain"
)

format_endpoint_cell <- function(
    difference_median,
    difference_l95,
    difference_u95,
    conclusion) {
  sprintf(
    "%+.3f [%.3f, %.3f]; %s",
    difference_median,
    difference_l95,
    difference_u95,
    unname(conclusion_labels[conclusion])
  )
}

latex_table <- primary_extremity |>
  mutate(
    Model = unname(model_labels[base_model]),
    Anchor = recode(
      anchor_type,
      ANCHOR_EXAMPLE = "Example",
      ANCHOR_WORD = "Word"
    ),
    Cell = format_endpoint_cell(
      difference_median,
      difference_l95,
      difference_u95,
      conclusion
    ),
    Model = factor(Model, levels = unname(model_labels))
  ) |>
  select(Model, Anchor, Cell) |>
  tidyr::pivot_wider(names_from = Anchor, values_from = Cell) |>
  arrange(Model)

latex_lines <- c(
  "\\begin{table*}[tbp]",
  "\\caption{RQ3 feasibility: anchor-associated endpoint changes under the example and word anchors.}",
  "\\label{tab:rq3-feasibility-anchor-patterns}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Model & Example $\\Delta_{\\mathrm{endpoint}}$ [95\\% CrI] & Word $\\Delta_{\\mathrm{endpoint}}$ [95\\% CrI] \\\\",
  "\\midrule",
  paste0(
    as.character(latex_table$Model),
    " & ", latex_table$Example,
    " & ", latex_table$Word,
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{2pt}",
  "\\begin{minipage}{\\linewidth}",
  paste0(
    "\\footnotesize\\textit{Note.} ",
    "$\\Delta_{\\mathrm{endpoint}}$ is the posterior anchor-minus-Context difference in ",
    "$P(Y=1)+P(Y=4)$. Positive values indicate greater endpoint concentration under the anchor, ",
    "whereas negative values indicate reduced endpoint concentration. ",
    "A 95\\% credible interval containing zero was classified as uncertain."
  ),
  "\\end{minipage}",
  "\\end{table*}"
)

# ------------------------------------------------------------------
# 5. Save the primary endpoint CSV and LaTeX table
# ------------------------------------------------------------------

readr::write_csv(
  primary_extremity,
  file.path(
    output_dir,
    "rq3_feasibility_primary_extremity_contrasts.csv"
  )
)

writeLines(
  latex_lines,
  file.path(
    latex_dir,
    "rq3_feasibility_endpoint_contrasts.tex"
  ),
  useBytes = TRUE
)

cat("\nPrimary feasibility endpoint contrasts:\n")
print(primary_extremity)

message("\nRQ3 feasibility endpoint analysis complete.")
message("Results saved in: ", output_dir)
