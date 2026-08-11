# Run after the primary model and PPC:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq2-rerun" && Rscript "imp-rq2-analysis/imp-rq2-sensitivity.R"
#
# Prior-sensitivity analysis for RQ2 importance.
#
# The same cumulative-logit model with base-model/DC-solution-specific
# thresholds is evaluated under three priors on every direct
# Context-versus-Zero-shot log odds ratio:
#   regularizing: Normal(0, 0.75)
#   primary:      Normal(0, 1.50)
#   weak:         Normal(0, 2.50)
#
# The completed primary fit from importance-rq2.R is reused. Only the
# regularizing and weak alternatives are newly fitted. Data, likelihood,
# thresholds, contrasts, MCMC settings, and PPC procedure are otherwise
# held constant.

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
  "imp-ctx-zeroshot-working",
  "imp-ctx-zeroshot-responses.csv"
)

primary_dir <- file.path(
  rq2_dir,
  "imp-ctx-zeroshot-working",
  "bayesian-results",
  "base-solution-threshold"
)

primary_fit_path <- file.path(
  primary_dir,
  "rq2_importance_base_solution_threshold_fit.rds"
)

output_dir <- file.path(
  rq2_dir,
  "imp-ctx-zeroshot-working",
  "bayesian-results",
  "prior-sensitivity"
)

if (!file.exists(input_path)) {
  stop("Input CSV not found: ", input_path)
}

if (!file.exists(primary_fit_path)) {
  stop(
    "Primary fitted model not found: ",
    primary_fit_path,
    "\nRun importance-rq2.R successfully before sensitivity analysis."
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

base_model_levels <- c(
  "gemma3",
  "llama",
  "mistral",
  "phi4"
)

# ------------------------------------------------------------------
# 1. Reconstruct and verify the primary analysis data
# ------------------------------------------------------------------

raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE
)

required_columns <- c(
  "row_id",
  "rating",
  "condition",
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

observed <- raw |>
  transmute(
    row_id = as.character(row_id),
    base_model = tolower(
      trimws(as.character(base_model))
    ),
    dc_solution = trimws(
      as.character(dc_solution)
    ),
    condition = toupper(
      trimws(as.character(condition))
    ),
    iteration = suppressWarnings(
      as.integer(iteration)
    ),
    rating_value = suppressWarnings(
      as.integer(rating)
    )
  )

if (anyNA(observed$base_model) ||
    anyNA(observed$dc_solution) ||
    anyNA(observed$condition) ||
    any(observed$dc_solution == "")) {
  stop("The current CSV contains invalid grouping values.")
}

if (!setequal(
  unique(observed$base_model),
  base_model_levels
)) {
  stop("The expected four base models are not all present.")
}

if (any(!observed$condition %in% c(
  "ZEROSHOT",
  "CONTEXT"
))) {
  stop("Condition must contain only ZEROSHOT and CONTEXT.")
}

if (anyNA(observed$iteration) ||
    anyNA(observed$rating_value) ||
    any(!observed$rating_value %in% 0:4)) {
  stop("The CSV contains invalid iteration or rating values.")
}

solution_levels <- sort(unique(observed$dc_solution))
rating_levels <- sort(unique(observed$rating_value))

if (length(solution_levels) != 11) {
  stop("Expected 11 DC solutions.")
}

if (length(rating_levels) != 3) {
  stop(
    "Expected exactly three observed rating categories. Found: ",
    paste(rating_levels, collapse = ", "),
    "."
  )
}

df <- observed |>
  mutate(
    condition = factor(
      condition,
      levels = c("ZEROSHOT", "CONTEXT")
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
    context_indicator = as.integer(
      condition == "CONTEXT"
    ),
    rating = ordered(
      rating_value,
      levels = rating_levels
    )
  )

base_solution_levels <- levels(df$base_solution)

if (length(base_solution_levels) != 44) {
  stop("Expected 44 base-model/solution groups.")
}

coverage <- df |>
  distinct(base_solution, condition) |>
  count(base_solution, name = "n_conditions")

if (nrow(coverage) != 44 ||
    any(coverage$n_conditions != 2)) {
  stop(
    "Every base-model/solution group must contain both conditions."
  )
}

primary_fit <- readRDS(primary_fit_path)

required_fit_columns <- c(
  "rating",
  "base_solution",
  "context_indicator"
)

missing_fit_columns <- setdiff(
  required_fit_columns,
  names(primary_fit$data)
)

if (length(missing_fit_columns) > 0) {
  stop(
    "The primary fit is missing stored model variables: ",
    paste(missing_fit_columns, collapse = ", "),
    "."
  )
}

if (nrow(primary_fit$data) != nrow(df)) {
  stop(
    "The primary fit and current CSV contain different row counts."
  )
}

current_model_data <- df |>
  transmute(
    rating = rating_value,
    base_solution = as.character(base_solution),
    context_indicator = context_indicator
  )

fit_model_data <- primary_fit$data |>
  transmute(
    rating = suppressWarnings(
      as.integer(as.character(rating))
    ),
    base_solution = as.character(base_solution),
    context_indicator = suppressWarnings(
      as.integer(context_indicator)
    )
  )

if (!isTRUE(
  all.equal(
    as.data.frame(current_model_data),
    as.data.frame(fit_model_data),
    check.attributes = FALSE
  )
)) {
  stop(
    "The current CSV does not match the data in the primary fit. ",
    "Refit importance-rq2.R before sensitivity analysis."
  )
}

data_audit <- df |>
  count(
    base_model,
    dc_solution,
    condition,
    rating_value,
    name = "n"
  ) |>
  group_by(
    base_model,
    dc_solution,
    condition
  ) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  arrange(
    base_model,
    dc_solution,
    condition,
    rating_value
  )

readr::write_csv(
  data_audit,
  file.path(
    output_dir,
    "rq2_importance_sensitivity_data_audit.csv"
  )
)

# ------------------------------------------------------------------
# 2. Define the common model, contrasts, and prior specifications
# ------------------------------------------------------------------

sensitivity_formula <- brms::bf(
  rating | thres(2, gr = base_solution) ~
    base_solution:context_indicator
)

prior_specifications <- tibble(
  prior_name = c(
    "regularizing",
    "primary",
    "weak"
  ),
  effect_prior_sd = c(
    0.75,
    1.50,
    2.50
  ),
  fit_source = c(
    "sensitivity refit",
    "reused primary model",
    "sensitivity refit"
  )
)

readr::write_csv(
  prior_specifications,
  file.path(
    output_dir,
    "rq2_importance_prior_specifications.csv"
  )
)

contrast_grid <- tidyr::expand_grid(
  base_model = base_model_levels,
  dc_solution = solution_levels
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
    )
  )

if (nrow(contrast_grid) != 44 ||
    anyNA(contrast_grid$base_solution)) {
  stop("Failed to construct the 44-row contrast grid.")
}

zeroshot_data <- tibble(
  base_solution = contrast_grid$base_solution,
  context_indicator = rep(0L, nrow(contrast_grid))
)

context_data <- tibble(
  base_solution = contrast_grid$base_solution,
  context_indicator = rep(1L, nrow(contrast_grid))
)

ppc_data <- df |>
  transmute(
    base_model = as.character(base_model),
    dc_solution = as.character(dc_solution),
    condition = as.character(condition),
    rating = rating_value
  )

group_grid <- ppc_data |>
  distinct(
    base_model,
    dc_solution,
    condition
  ) |>
  mutate(
    condition = factor(
      condition,
      levels = c("ZEROSHOT", "CONTEXT")
    )
  ) |>
  arrange(
    base_model,
    dc_solution,
    condition
  )

if (nrow(group_grid) != 88) {
  stop("Expected 88 PPC cells.")
}

# ------------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------------

extract_effects <- function(current_fit, current_prior_name) {
  eta_zeroshot <- brms::posterior_linpred(
    current_fit,
    newdata = zeroshot_data,
    transform = FALSE
  )

  eta_context <- brms::posterior_linpred(
    current_fit,
    newdata = context_data,
    transform = FALSE
  )

  delta_draws <- eta_context - eta_zeroshot

  bind_rows(
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
          probability_gt_1 >= 0.95 ~ "higher in Context",
        or_interval[2] < 1 &&
          probability_lt_1 >= 0.95 ~ "lower in Context",
        TRUE ~ "uncertain"
      )

      tibble(
        prior_name = current_prior_name,
        base_model = as.character(
          contrast_grid$base_model[i]
        ),
        dc_solution = as.character(
          contrast_grid$dc_solution[i]
        ),
        base_solution = as.character(
          contrast_grid$base_solution[i]
        ),
        comparison = "CONTEXT vs ZEROSHOT",
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
}

extract_diagnostics <- function(
  current_fit,
  current_prior_name,
  current_prior_sd
) {
  draw_summary <- posterior::summarise_draws(
    posterior::as_draws_array(current_fit)
  )

  rhat_values <- draw_summary$rhat[
    is.finite(draw_summary$rhat)
  ]
  bulk_ess_values <- draw_summary$ess_bulk[
    is.finite(draw_summary$ess_bulk)
  ]
  tail_ess_values <- draw_summary$ess_tail[
    is.finite(draw_summary$ess_tail)
  ]

  sampler_parameters <- nuts_params(current_fit)

  tibble(
    prior_name = current_prior_name,
    effect_prior_sd = current_prior_sd,
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
}

extract_ppc <- function(current_fit, current_prior_name) {
  set.seed(20260727)

  yrep <- brms::posterior_predict(
    current_fit,
    ndraws = 1000
  )

  if (ncol(yrep) != nrow(ppc_data)) {
    stop(
      "Posterior predictions do not match the analysis rows for ",
      current_prior_name,
      "."
    )
  }

  predicted_values <- sort(unique(as.vector(yrep)))

  if (all(predicted_values %in% rating_levels)) {
    yrep_ratings <- yrep
  } else if (
    all(predicted_values %in% seq_along(rating_levels))
  ) {
    yrep_ratings <- matrix(
      rating_levels[as.integer(yrep)],
      nrow = nrow(yrep),
      ncol = ncol(yrep)
    )
  } else {
    stop(
      "Unexpected predicted rating categories under ",
      current_prior_name,
      "."
    )
  }

  output <- bind_rows(
    lapply(seq_len(nrow(group_grid)), function(group_index) {
      current_base_model <-
        group_grid$base_model[group_index]
      current_solution <-
        group_grid$dc_solution[group_index]
      current_condition <- as.character(
        group_grid$condition[group_index]
      )

      row_indices <- which(
        ppc_data$base_model == current_base_model &
          ppc_data$dc_solution == current_solution &
          ppc_data$condition == current_condition
      )

      bind_rows(
        lapply(rating_levels, function(rating_category) {
          observed_proportion <- mean(
            ppc_data$rating[row_indices] == rating_category
          )

          simulated_proportions <- rowMeans(
            yrep_ratings[, row_indices, drop = FALSE] ==
              rating_category
          )

          predictive_interval <- unname(
            quantile(
              simulated_proportions,
              c(0.025, 0.975)
            )
          )

          tibble(
            prior_name = current_prior_name,
            base_model = current_base_model,
            dc_solution = current_solution,
            condition = current_condition,
            rating = rating_category,
            n_observed = length(row_indices),
            observed_proportion = observed_proportion,
            predicted_median = median(
              simulated_proportions
            ),
            predicted_l95 = predictive_interval[1],
            predicted_u95 = predictive_interval[2],
            observed_within_predictive_95 =
              observed_proportion >= predictive_interval[1] &
              observed_proportion <= predictive_interval[2],
            absolute_difference = abs(
              observed_proportion -
                median(simulated_proportions)
            )
          )
        })
      )
    })
  )

  rm(yrep, yrep_ratings)
  gc(verbose = FALSE)

  output
}

# ------------------------------------------------------------------
# 4. Reuse or fit each prior specification
# ------------------------------------------------------------------

all_effects <- list()
all_diagnostics <- list()
all_ppc <- list()

for (prior_index in seq_len(nrow(prior_specifications))) {
  prior_name <- prior_specifications$prior_name[prior_index]
  effect_prior_sd <-
    prior_specifications$effect_prior_sd[prior_index]

  prior_output_dir <- file.path(
    output_dir,
    prior_name
  )

  dir.create(
    prior_output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  cat(
    "\nProcessing prior:",
    prior_name,
    "(SD =",
    effect_prior_sd,
    ")\n"
  )

  if (prior_name == "primary") {
    current_fit <- primary_fit
  } else {
    current_priors <- c(
      brms::set_prior(
        paste0(
          "normal(0, ",
          effect_prior_sd,
          ")"
        ),
        class = "b"
      ),
      prior(normal(0, 2), class = "Intercept")
    )

    current_fit <- brms::brm(
      formula = sensitivity_formula,
      data = df,
      family = cumulative(
        link = "logit",
        threshold = "flexible"
      ),
      prior = current_priors,
      chains = 4,
      iter = 4000,
      warmup = 2000,
      seed = 20260727,
      control = list(
        adapt_delta = 0.995,
        max_treedepth = 15
      ),
      file = file.path(
        prior_output_dir,
        paste0(
          "rq2_importance_",
          prior_name,
          "_fit"
        )
      ),
      file_refit = "on_change"
    )
  }

  writeLines(
    capture.output(print(summary(current_fit))),
    file.path(
      prior_output_dir,
      paste0(
        "rq2_importance_",
        prior_name,
        "_summary.txt"
      )
    )
  )

  prior_effects <- extract_effects(
    current_fit,
    prior_name
  )

  prior_diagnostics <- extract_diagnostics(
    current_fit,
    prior_name,
    effect_prior_sd
  )

  prior_ppc <- extract_ppc(
    current_fit,
    prior_name
  )

  if (nrow(prior_effects) != 44) {
    stop(
      "Expected 44 effects under ",
      prior_name,
      "."
    )
  }

  if (nrow(prior_ppc) != 264) {
    stop(
      "Expected 264 PPC checks under ",
      prior_name,
      "."
    )
  }

  readr::write_csv(
    prior_effects,
    file.path(
      prior_output_dir,
      paste0(
        "rq2_importance_",
        prior_name,
        "_effects.csv"
      )
    )
  )

  readr::write_csv(
    prior_diagnostics,
    file.path(
      prior_output_dir,
      paste0(
        "rq2_importance_",
        prior_name,
        "_diagnostics.csv"
      )
    )
  )

  readr::write_csv(
    prior_ppc,
    file.path(
      prior_output_dir,
      paste0(
        "rq2_importance_",
        prior_name,
        "_ppc_rating_proportions.csv"
      )
    )
  )

  all_effects[[prior_name]] <- prior_effects
  all_diagnostics[[prior_name]] <- prior_diagnostics
  all_ppc[[prior_name]] <- prior_ppc

  if (prior_name != "primary") {
    rm(current_fit)
    gc(verbose = FALSE)
  }
}

# ------------------------------------------------------------------
# 5. Combine and compare the three prior specifications
# ------------------------------------------------------------------

combined_effects <- bind_rows(all_effects)
combined_diagnostics <- bind_rows(all_diagnostics)
combined_ppc <- bind_rows(all_ppc)

readr::write_csv(
  combined_effects,
  file.path(
    output_dir,
    "rq2_importance_prior_sensitivity_effects.csv"
  )
)

readr::write_csv(
  combined_diagnostics,
  file.path(
    output_dir,
    "rq2_importance_prior_sensitivity_diagnostics.csv"
  )
)

readr::write_csv(
  combined_ppc,
  file.path(
    output_dir,
    "rq2_importance_prior_sensitivity_ppc_all.csv"
  )
)

ppc_summary <- combined_ppc |>
  group_by(prior_name) |>
  summarise(
    category_checks = n(),
    checks_within_predictive_95 = sum(
      observed_within_predictive_95
    ),
    proportion_within_predictive_95 = mean(
      observed_within_predictive_95
    ),
    median_absolute_difference = median(
      absolute_difference
    ),
    maximum_absolute_difference = max(
      absolute_difference
    ),
    .groups = "drop"
  )

readr::write_csv(
  ppc_summary,
  file.path(
    output_dir,
    "rq2_importance_prior_sensitivity_ppc_summary.csv"
  )
)

robustness <- combined_effects |>
  select(
    prior_name,
    base_model,
    dc_solution,
    base_solution,
    comparison,
    OR_median,
    OR_l95,
    OR_u95,
    posterior_probability_OR_gt_1,
    posterior_probability_OR_lt_1,
    conclusion
  ) |>
  pivot_wider(
    names_from = prior_name,
    values_from = c(
      OR_median,
      OR_l95,
      OR_u95,
      posterior_probability_OR_gt_1,
      posterior_probability_OR_lt_1,
      conclusion
    ),
    names_glue = "{.value}_{prior_name}"
  ) |>
  mutate(
    direction_regularizing = case_when(
      OR_median_regularizing > 1 ~ "above 1",
      OR_median_regularizing < 1 ~ "below 1",
      TRUE ~ "equal to 1"
    ),
    direction_primary = case_when(
      OR_median_primary > 1 ~ "above 1",
      OR_median_primary < 1 ~ "below 1",
      TRUE ~ "equal to 1"
    ),
    direction_weak = case_when(
      OR_median_weak > 1 ~ "above 1",
      OR_median_weak < 1 ~ "below 1",
      TRUE ~ "equal to 1"
    ),
    direction_stable =
      direction_regularizing == direction_primary &
      direction_primary == direction_weak,
    conclusion_stable =
      conclusion_regularizing == conclusion_primary &
      conclusion_primary == conclusion_weak
  )

if (nrow(robustness) != 44) {
  stop(
    "Expected 44 rows in the prior-robustness comparison."
  )
}

readr::write_csv(
  robustness,
  file.path(
    output_dir,
    "rq2_importance_prior_sensitivity_robustness.csv"
  )
)

robustness_summary <- robustness |>
  summarise(
    contrasts = n(),
    direction_stable = sum(direction_stable),
    conclusion_stable = sum(conclusion_stable),
    direction_stability_rate = mean(direction_stable),
    conclusion_stability_rate = mean(conclusion_stable)
  )

readr::write_csv(
  robustness_summary,
  file.path(
    output_dir,
    "rq2_importance_prior_sensitivity_robustness_summary.csv"
  )
)

cat("\nPrior-sensitivity sampling diagnostics:\n")
print(combined_diagnostics)

cat("\nPrior-sensitivity PPC summary:\n")
print(ppc_summary)

cat("\nPrior-sensitivity robustness summary:\n")
print(robustness_summary)

message("RQ2 importance prior-sensitivity analysis complete.")
message("Results saved in: ", output_dir)
