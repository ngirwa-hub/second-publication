# Run with:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" && Rscript "imp-rq3-analysis/importance-rq3.R"
#
# Bayesian cumulative-logit ordinal model: RQ3 importance -- each anchor type versus Context for every base model and DC solution.
#
# The model:
# - pools model variants and iterations within each base-model/solution
#   group;
# - does not assume iteration-wise pairing or use a random intercept;
# - retains the unequal numbers of Context and anchor observations;
# - estimates a separate ordinal threshold for each base-model/solution
#   group;
# - estimates 176 direct anchor-versus-Context effects:
#   four anchors x four base models x eleven DC solutions;
# - gives every direct contrast the same Normal(0, 1.5) prior.

required_packages <- c(
  "brms",
  "posterior",
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
  library(posterior)
  library(dplyr)
  library(tidyr)
  library(readr)
})

options(mc.cores = min(4L, parallel::detectCores()))

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

rq3_dir <- dirname(script_dir)

input_path <- file.path(
  rq3_dir,
  "imp-ctx-anchor-working",
  "imp-ctx-anchor-responses.csv"
)

output_dir <- file.path(
  rq3_dir,
  "imp-ctx-anchor-working",
  "bayesian-results",
  "base-solution-threshold"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

base_model_levels <- c(
  "gemma3",
  "llama",
  "mistral",
  "phi4"
)

anchor_levels <- c(
  "ANCHOR_EXAMPLE",
  "ANCHOR_WORD",
  "ANCHOR_NUM_HIGH",
  "ANCHOR_NUM_LOW"
)

comparison_levels <- c(
  "CONTEXT",
  anchor_levels
)

# ------------------------------------------------------------------
# 1. Read and validate the combined data
# ------------------------------------------------------------------

raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE
)

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
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- raw |>
  mutate(
    condition = toupper(
      trimws(as.character(condition))
    ),
    anchor_type = toupper(
      trimws(as.character(anchor_type))
    ),
    base_model = tolower(
      trimws(as.character(base_model))
    ),
    dc_solution = trimws(
      as.character(dc_solution)
    ),
    rating_value = suppressWarnings(
      as.integer(rating)
    ),
    comparison = case_when(
      condition == "CONTEXT" ~ "CONTEXT",
      condition == "ANCHOR" &
        anchor_type %in% anchor_levels ~ anchor_type,
      TRUE ~ NA_character_
    )
  )

if (anyNA(df$comparison)) {
  stop(
    "Every row must be Context or one of the four expected ",
    "anchor types."
  )
}

if (anyNA(df$base_model) ||
    !setequal(unique(df$base_model), base_model_levels)) {
  stop(
    "Expected exactly these base models: ",
    paste(base_model_levels, collapse = ", "),
    "."
  )
}

if (anyNA(df$dc_solution) ||
    any(df$dc_solution == "")) {
  stop("DC-solution names must not be missing or empty.")
}

solution_levels <- sort(unique(df$dc_solution))

if (length(solution_levels) != 11) {
  stop(
    "Expected exactly 11 DC solutions. Found: ",
    length(solution_levels),
    "."
  )
}

if (anyNA(df$rating_value) ||
    any(!df$rating_value %in% 0:4)) {
  stop("Ratings must be integers from 0 to 4.")
}

rating_levels <- sort(unique(df$rating_value))

if (length(rating_levels) != 2) {
  stop(
    "This model uses one threshold and therefore expects exactly ",
    "two observed rating categories. Found: ",
    paste(rating_levels, collapse = ", "),
    "."
  )
}

df <- df |>
  mutate(
    comparison = factor(
      comparison,
      levels = comparison_levels
    ),
    base_model = factor(
      base_model,
      levels = base_model_levels
    ),
    dc_solution = factor(
      dc_solution,
      levels = solution_levels
    ),
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
    rating = ordered(
      rating_value,
      levels = rating_levels
    )
  )

base_solution_levels <- levels(df$base_solution)

if (length(base_solution_levels) != 44) {
  stop(
    "Expected 44 base-model/solution groups. Found: ",
    length(base_solution_levels),
    "."
  )
}

comparison_coverage <- df |>
  distinct(base_solution, comparison) |>
  count(base_solution, name = "n_comparisons")

if (any(comparison_coverage$n_comparisons != 5)) {
  stop(
    "Every base-model/solution group must contain Context and ",
    "all four anchor types."
  )
}

# ------------------------------------------------------------------
# 2. Save transparent data and group audits
# ------------------------------------------------------------------

audit <- df |>
  count(
    base_model,
    dc_solution,
    base_solution,
    comparison,
    rating_value,
    name = "n"
  ) |>
  group_by(
    base_model,
    dc_solution,
    comparison
  ) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  arrange(
    base_model,
    dc_solution,
    comparison,
    rating_value
  )

readr::write_csv(
  audit,
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_data_audit.csv"
  )
)

group_mapping <- df |>
  distinct(
    base_solution,
    base_model,
    dc_solution
  ) |>
  arrange(base_model, dc_solution)

readr::write_csv(
  group_mapping,
  file.path(
    output_dir,
    "rq3_importance_base_solution_mapping.csv"
  )
)

# ------------------------------------------------------------------
# 3. Fit the primary base-solution-specific-threshold model
# ------------------------------------------------------------------

priors <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(normal(0, 2), class = "Intercept")
)

# Context has all four anchor indicators set to zero. Each coefficient
# is therefore one direct anchor-versus-Context log-odds effect.
threshold_formula <- brms::bf(
  rating | thres(1, gr = base_solution) ~
    base_solution:anchor_example +
    base_solution:anchor_word +
    base_solution:anchor_num_high +
    base_solution:anchor_num_low
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
    adapt_delta = 0.995,
    max_treedepth = 15
  ),
  file = file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_fit"
  ),
  file_refit = "on_change"
)

writeLines(
  capture.output(print(summary(fit))),
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_summary.txt"
  )
)

# ------------------------------------------------------------------
# 4. Save sampling diagnostics
# ------------------------------------------------------------------

draw_diagnostics <- posterior::summarise_draws(
  posterior::as_draws_array(fit)
)

readr::write_csv(
  as.data.frame(draw_diagnostics),
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_draw_diagnostics.csv"
  )
)

rhat_values <- draw_diagnostics$rhat[
  is.finite(draw_diagnostics$rhat)
]
bulk_ess_values <- draw_diagnostics$ess_bulk[
  is.finite(draw_diagnostics$ess_bulk)
]
tail_ess_values <- draw_diagnostics$ess_tail[
  is.finite(draw_diagnostics$ess_tail)
]

sampler_parameters <- nuts_params(fit)

diagnostic_summary <- tibble(
  max_rhat = ifelse(
    length(rhat_values) > 0,
    max(rhat_values),
    NA_real_
  ),
  min_bulk_ess = ifelse(
    length(bulk_ess_values) > 0,
    min(bulk_ess_values),
    NA_real_
  ),
  min_tail_ess = ifelse(
    length(tail_ess_values) > 0,
    min(tail_ess_values),
    NA_real_
  ),
  divergences = sum(
    sampler_parameters$Parameter == "divergent__" &
      sampler_parameters$Value == 1
  ),
  max_treedepth_hits = sum(
    sampler_parameters$Parameter == "treedepth__" &
      sampler_parameters$Value >= 15
  )
)

readr::write_csv(
  diagnostic_summary,
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_diagnostic_summary.csv"
  )
)

# ------------------------------------------------------------------
# 5. Estimate all 176 direct anchor-versus-Context effects
# ------------------------------------------------------------------

contrast_grid <- tidyr::expand_grid(
  base_model = base_model_levels,
  dc_solution = solution_levels,
  anchor_type = anchor_levels
) |>
  mutate(
    base_model = factor(
      base_model,
      levels = base_model_levels
    ),
    dc_solution = factor(
      dc_solution,
      levels = solution_levels
    ),
    base_solution = factor(
      as.character(
        interaction(
          base_model,
          dc_solution,
          drop = TRUE,
          sep = "__"
        )
      ),
      levels = base_solution_levels
    ),
    anchor_example = as.integer(
      anchor_type == "ANCHOR_EXAMPLE"
    ),
    anchor_word = as.integer(
      anchor_type == "ANCHOR_WORD"
    ),
    anchor_num_high = as.integer(
      anchor_type == "ANCHOR_NUM_HIGH"
    ),
    anchor_num_low = as.integer(
      anchor_type == "ANCHOR_NUM_LOW"
    )
  )

if (nrow(contrast_grid) != 176 ||
    anyNA(contrast_grid$base_solution)) {
  stop("Failed to construct the 176-row contrast grid.")
}

context_data <- tibble(
  base_solution = contrast_grid$base_solution,
  anchor_example = rep(0L, nrow(contrast_grid)),
  anchor_word = rep(0L, nrow(contrast_grid)),
  anchor_num_high = rep(0L, nrow(contrast_grid)),
  anchor_num_low = rep(0L, nrow(contrast_grid))
)

anchor_data <- contrast_grid |>
  select(
    base_solution,
    anchor_example,
    anchor_word,
    anchor_num_high,
    anchor_num_low
  )

eta_context <- brms::posterior_linpred(
  fit,
  newdata = context_data,
  transform = FALSE
)

eta_anchor <- brms::posterior_linpred(
  fit,
  newdata = anchor_data,
  transform = FALSE
)

delta_draws <- eta_anchor - eta_context

results <- bind_rows(
  lapply(seq_len(nrow(contrast_grid)), function(i) {
    log_odds <- delta_draws[, i]
    odds_ratio <- exp(log_odds)

    log_interval <- unname(
      quantile(log_odds, c(0.025, 0.975))
    )
    or_interval <- unname(
      quantile(odds_ratio, c(0.025, 0.975))
    )

    probability_gt_1 <- mean(odds_ratio > 1)
    probability_lt_1 <- mean(odds_ratio < 1)

    conclusion <- case_when(
      or_interval[1] > 1 &&
        probability_gt_1 >= 0.95 ~ "higher under Anchor",
      or_interval[2] < 1 &&
        probability_lt_1 >= 0.95 ~ "lower under Anchor",
      TRUE ~ "uncertain"
    )

    tibble(
      base_model = as.character(
        contrast_grid$base_model[i]
      ),
      dc_solution = as.character(
        contrast_grid$dc_solution[i]
      ),
      base_solution = as.character(
        contrast_grid$base_solution[i]
      ),
      anchor_type = contrast_grid$anchor_type[i],
      comparison = paste0(
        contrast_grid$anchor_type[i],
        " vs CONTEXT"
      ),
      threshold_structure = "base-model-solution-specific",
      log_odds_median = median(log_odds),
      log_odds_l95 = log_interval[1],
      log_odds_u95 = log_interval[2],
      OR_median = median(odds_ratio),
      OR_l95 = or_interval[1],
      OR_u95 = or_interval[2],
      posterior_probability_OR_gt_1 = probability_gt_1,
      posterior_probability_OR_lt_1 = probability_lt_1,
      conclusion = conclusion
    )
  })
)

if (nrow(results) != 176) {
  stop(
    "Expected 176 posterior contrasts. Produced: ",
    nrow(results),
    "."
  )
}

readr::write_csv(
  results,
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_anchor_effects.csv"
  )
)

print(results, n = nrow(results))
print(diagnostic_summary)

message(
  "RQ3 importance base-solution-threshold analysis complete."
)
message("Results saved in: ", output_dir)
