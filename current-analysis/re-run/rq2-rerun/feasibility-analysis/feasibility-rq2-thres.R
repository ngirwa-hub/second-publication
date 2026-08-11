# Run command: cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq2-rerun" && Rscript "feasibility-analysis/feasibility-rq2-thres.R"
# Bayesian ordinal regression threshold trial:
# For rq2 overall feasibility -- Context versus Zero-shot
# This model estimates a separate ordinal-threshold vector for each base model and preserves the original shared-threshold fit.

required_packages <- c("brms", "posterior", "dplyr", "tidyr", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(brms)
  library(posterior)
  library(dplyr)
  library(tidyr)
  library(readr)
})

options(
  mc.cores = min(4L, parallel::detectCores()),
  contrasts = c("contr.sum", "contr.poly")
)

# Resolve paths from this script's location rather than the terminal's
# current working directory.
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

rq2_dir <- dirname(script_dir)

input_path <- file.path(
  rq2_dir,
  "feas-ctx-zeroshot-working",
  "feas-ctx-zeroshot-responses.csv"
)

output_dir <- file.path(
  rq2_dir,
  "feas-ctx-zeroshot-working",
  "bayesian-results",
  "base-model-threshold-trial"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

df <- readr::read_csv(input_path, show_col_types = FALSE) |>
  mutate(
    condition = toupper(trimws(condition)),
    base_model = tolower(trimws(base_model)),
    rating_value = suppressWarnings(as.integer(rating))
  )

required_columns <- c(
  "rating",
  "condition",
  "base_model",
  "iteration"
)

missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (any(!df$condition %in% c("ZEROSHOT", "CONTEXT"))) {
  stop("Unexpected condition values.")
}

if (any(is.na(df$rating_value)) ||
    any(!df$rating_value %in% 0:4)) {
  stop("Ratings must be integers from 0 to 4.")
}

base_model_levels <- c("gemma3", "llama", "mistral", "phi4")
base_model_levels <- base_model_levels[
  base_model_levels %in% unique(df$base_model)
]

df <- df |>
  mutate(
    condition = factor(
      condition,
      levels = c("ZEROSHOT", "CONTEXT")
    ),
    base_model = factor(
      base_model,
      levels = base_model_levels
    ),
    rating = ordered(
      rating_value,
      levels = sort(unique(rating_value))
    )
  )

# Confirm the number of observations and rating distribution.
audit <- df |>
  count(base_model, condition, rating_value, name = "n") |>
  arrange(base_model, condition, rating_value)

readr::write_csv(
  audit,
  file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_data_audit.csv"
  )
)

priors <- c(
  prior(normal(0, 1.5), class = "b"), # regression coefficients centered at 0 with SD = 1.5
  prior(normal(0, 2), class = "Intercept") # the threshold prior
)

# Four observed categories require three thresholds. The gr argument estimates a different three-threshold vector for each base model.
threshold_formula <- brms::bf(
  rating | thres(3, gr = base_model) ~
    condition * base_model
)

fit <- brms::brm(
  formula = threshold_formula,
  data = df,
  family = cumulative(
    link = "logit",
    threshold = "flexible"
  ),
  prior = priors,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 20260727,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  ),
  file = file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_fit"
  ),
  file_refit = "on_change"
)

writeLines(
  capture.output(print(summary(fit))),
  file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_summary.txt"
  )
)

draw_diagnostics <- posterior::summarise_draws(
  posterior::as_draws_array(fit)
)

readr::write_csv(
  as.data.frame(draw_diagnostics),
  file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_draw_diagnostics.csv"
  )
)

# Construct one Zero-shot and one Context row per base model.
contrast_grid <- tibble(
  base_model = factor(
    base_model_levels,
    levels = base_model_levels
  )
)

zeroshot_data <- contrast_grid |>
  mutate(
    condition = factor(
      "ZEROSHOT",
      levels = levels(df$condition)
    )
  )

context_data <- contrast_grid |>
  mutate(
    condition = factor(
      "CONTEXT",
      levels = levels(df$condition)
    )
  )

eta_zeroshot <- brms::posterior_linpred(
  fit,
  newdata = zeroshot_data,
  transform = FALSE
)

eta_context <- brms::posterior_linpred(
  fit,
  newdata = context_data,
  transform = FALSE
)

delta_draws <- eta_context - eta_zeroshot

results <- bind_rows(
  lapply(seq_along(base_model_levels), function(i) {
    log_odds <- delta_draws[, i]
    odds_ratio <- exp(log_odds)

    tibble(
      base_model = base_model_levels[i],
      comparison = "CONTEXT vs ZEROSHOT",
      threshold_structure = "base-model-specific",
      log_odds_median = median(log_odds),
      log_odds_l95 = unname(quantile(log_odds, 0.025)),
      log_odds_u95 = unname(quantile(log_odds, 0.975)),
      OR_median = median(odds_ratio),
      OR_l95 = unname(quantile(odds_ratio, 0.025)),
      OR_u95 = unname(quantile(odds_ratio, 0.975)),
      posterior_probability_OR_gt_1 = mean(odds_ratio > 1),
      posterior_probability_OR_lt_1 = mean(odds_ratio < 1)
    )
  })
)

readr::write_csv(
  results,
  file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_effects.csv"
  )
)

print(results)
message("RQ2 feasibility base-model-threshold trial complete.")
message("Results saved in: ", output_dir)
