# Run after inspecting the primary threshold model and its PPC:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" && Rscript "rq3-feasibility-analysis/feas-rq3-sensitivity.R"
#
# Prior-sensitivity analysis for RQ3 overall feasibility:
# each anchor type versus Context.
#
# The same base-model-specific-threshold model is fitted under three different priors for the 16 anchor-versus-Context effects:
#   regularizing: Normal(0, 0.75)
#   primary:      Normal(0, 1.50)
#   weak:         Normal(0, 2.50)
#
# Data, likelihood, threshold prior, contrasts, and PPC procedure are
# held constant. Each of the 16 direct contrasts receives the same prior.

required_packages <- c(
  "brms",
  "posterior",
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2"
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
  library(ggplot2)
})

options(mc.cores = min(4L, parallel::detectCores()))

# Resolve paths from this script's location.
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
  "feas-ctx-anchor-working",
  "feas-ctx-anchor-responses.csv"
)

output_dir <- file.path(
  rq3_dir,
  "feas-ctx-anchor-working",
  "bayesian-results",
  "prior-sensitivity"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

comparison_levels <- c(
  "CONTEXT",
  "ANCHOR_EXAMPLE",
  "ANCHOR_WORD",
  "ANCHOR_NUM_HIGH",
  "ANCHOR_NUM_LOW"
)

anchor_levels <- comparison_levels[
  comparison_levels != "CONTEXT"
]

base_model_levels <- c(
  "gemma3",
  "llama",
  "mistral",
  "phi4"
)

# ------------------------------------------------------------------
# 1. Read and validate data
# ------------------------------------------------------------------

df <- readr::read_csv(input_path, show_col_types = FALSE) |>
  mutate(
    condition = toupper(trimws(as.character(condition))),
    anchor_type = toupper(trimws(as.character(anchor_type))),
    base_model = tolower(trimws(as.character(base_model))),
    rating_value = suppressWarnings(as.integer(rating)),
    comparison = case_when(
      condition == "CONTEXT" ~ "CONTEXT",
      condition == "ANCHOR" &
        anchor_type %in% anchor_levels ~ anchor_type,
      TRUE ~ NA_character_
    )
  )

required_columns <- c(
  "rating",
  "condition",
  "anchor_type",
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

if (anyNA(df$comparison)) {
  stop("Some rows could not be assigned to an RQ3 comparison group.")
}

if (anyNA(df$rating_value) ||
    any(!df$rating_value %in% 0:4)) {
  stop("Ratings must be integers from 0 to 4.")
}

if (!setequal(unique(df$base_model), base_model_levels)) {
  stop("Expected exactly gemma3, llama, mistral, and phi4.")
}

if (!setequal(unique(df$comparison), comparison_levels)) {
  stop("Expected Context and all four anchor types.")
}

rating_levels <- sort(unique(df$rating_value))

if (length(rating_levels) != 4) {
  stop(
    "Expected exactly four observed rating categories. Found: ",
    paste(rating_levels, collapse = ", ")
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

data_audit <- df |>
  count(
    base_model,
    comparison,
    rating_value,
    name = "n"
  ) |>
  group_by(base_model, comparison) |>
  mutate(proportion = n / sum(n)) |>
  ungroup() |>
  arrange(base_model, comparison, rating_value)

readr::write_csv(
  data_audit,
  file.path(
    output_dir,
    "rq3_feasibility_sensitivity_data_audit.csv"
  )
)

# ------------------------------------------------------------------
# 2. Common model and prior specifications
# ------------------------------------------------------------------

sensitivity_formula <- brms::bf(
  rating | thres(3, gr = base_model) ~
    base_model:anchor_example +
    base_model:anchor_word +
    base_model:anchor_num_high +
    base_model:anchor_num_low
)

prior_specifications <- tibble(
  prior_name = c("regularizing", "primary", "weak"),
  effect_prior_sd = c(0.75, 1.50, 2.50),
  adapt_delta = c(0.99, 0.995, 0.99)
)

readr::write_csv(
  prior_specifications,
  file.path(
    output_dir,
    "rq3_feasibility_prior_specifications.csv"
  )
)

contrast_grid <- tidyr::expand_grid(
  base_model = base_model_levels,
  anchor_type = anchor_levels
) |>
  mutate(
    base_model = factor(
      base_model,
      levels = base_model_levels
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

context_data <- tibble(
  base_model = contrast_grid$base_model,
  anchor_example = rep(0L, nrow(contrast_grid)),
  anchor_word = rep(0L, nrow(contrast_grid)),
  anchor_num_high = rep(0L, nrow(contrast_grid)),
  anchor_num_low = rep(0L, nrow(contrast_grid))
)

anchor_data <- contrast_grid |>
  select(
    base_model,
    anchor_example,
    anchor_word,
    anchor_num_high,
    anchor_num_low
  )

all_effects <- list()
all_diagnostics <- list()
all_ppc <- list()

# ------------------------------------------------------------------
# 3. Fit and assess each prior
# ------------------------------------------------------------------

for (prior_index in seq_len(nrow(prior_specifications))) {
  prior_name <- prior_specifications$prior_name[prior_index]
  effect_prior_sd <-
    prior_specifications$effect_prior_sd[prior_index]
  current_adapt_delta <-
    prior_specifications$adapt_delta[prior_index]

  message(
    "\nFitting ",
    prior_name,
    " prior: Normal(0, ",
    effect_prior_sd,
    ")"
  )

  prior_output_dir <- file.path(output_dir, prior_name)
  ppc_output_dir <- file.path(
    prior_output_dir,
    "posterior-predictive-checks"
  )

  dir.create(
    ppc_output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  current_priors <- c(
    brms::set_prior(
      paste0("normal(0, ", effect_prior_sd, ")"),
      class = "b"
    ),
    prior(normal(0, 2), class = "Intercept")
  )

  fit <- brms::brm(
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
      adapt_delta = current_adapt_delta,
      max_treedepth = 15
    ),
    file = file.path(
      prior_output_dir,
      paste0(
        "rq3_feasibility_",
        prior_name,
        "_fit"
      )
    ),
    file_refit = "on_change"
  )

  writeLines(
    capture.output(print(summary(fit))),
    file.path(
      prior_output_dir,
      paste0(
        "rq3_feasibility_",
        prior_name,
        "_summary.txt"
      )
    )
  )

  # ---------------------------------------------------------------
  # 3a. Sampling diagnostics
  # ---------------------------------------------------------------

  draw_diagnostics <- posterior::summarise_draws(
    posterior::as_draws_array(fit)
  )

  readr::write_csv(
    as.data.frame(draw_diagnostics),
    file.path(
      prior_output_dir,
      paste0(
        "rq3_feasibility_",
        prior_name,
        "_draw_diagnostics.csv"
      )
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
    prior_name = prior_name,
    effect_prior_sd = effect_prior_sd,
    adapt_delta = current_adapt_delta,
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

  all_diagnostics[[prior_name]] <- diagnostic_summary

  # ---------------------------------------------------------------
  # 3b. All 16 anchor-versus-Context effects
  # ---------------------------------------------------------------

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

  effect_results <- bind_rows(
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
        prior_name = prior_name,
        effect_prior_sd = effect_prior_sd,
        base_model = as.character(
          contrast_grid$base_model[i]
        ),
        anchor_type = contrast_grid$anchor_type[i],
        comparison = paste0(
          contrast_grid$anchor_type[i],
          " vs CONTEXT"
        ),
        threshold_structure = "base-model-specific",
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

  all_effects[[prior_name]] <- effect_results

  readr::write_csv(
    effect_results,
    file.path(
      prior_output_dir,
      paste0(
        "rq3_feasibility_",
        prior_name,
        "_effects.csv"
      )
    )
  )

  # ---------------------------------------------------------------
  # 3c. Posterior predictive category proportions
  # ---------------------------------------------------------------

  set.seed(20260727)

  yrep <- brms::posterior_predict(
    fit,
    ndraws = 1000
  )

  indicator_sum <- with(
    fit$data,
    anchor_example +
      anchor_word +
      anchor_num_high +
      anchor_num_low
  )

  if (any(!indicator_sum %in% c(0, 1))) {
    stop(
      "Invalid fitted-data anchor indicators under prior ",
      prior_name,
      "."
    )
  }

  observed <- fit$data |>
    transmute(
      base_model = as.character(base_model),
      comparison = case_when(
        anchor_example == 1 ~ "ANCHOR_EXAMPLE",
        anchor_word == 1 ~ "ANCHOR_WORD",
        anchor_num_high == 1 ~ "ANCHOR_NUM_HIGH",
        anchor_num_low == 1 ~ "ANCHOR_NUM_LOW",
        TRUE ~ "CONTEXT"
      ),
      rating = as.integer(as.character(rating))
    )

  if (ncol(yrep) != nrow(observed)) {
    stop(
      "Posterior-predictive columns do not match observed rows for ",
      prior_name,
      "."
    )
  }

  if (!all(yrep %in% rating_levels)) {
    stop(
      "Posterior-predictive ratings contain unexpected categories for ",
      prior_name,
      "."
    )
  }

  groups <- observed |>
    distinct(base_model, comparison) |>
    mutate(
      comparison = factor(
        comparison,
        levels = comparison_levels
      )
    ) |>
    arrange(base_model, comparison)

  ppc_distribution <- bind_rows(
    lapply(seq_len(nrow(groups)), function(group_index) {
      current_base_model <- groups$base_model[group_index]
      current_comparison <- as.character(
        groups$comparison[group_index]
      )

      row_indices <- which(
        observed$base_model == current_base_model &
          observed$comparison == current_comparison
      )

      bind_rows(
        lapply(rating_levels, function(rating_category) {
          observed_proportion <- mean(
            observed$rating[row_indices] == rating_category
          )

          simulated_proportions <- rowMeans(
            yrep[, row_indices, drop = FALSE] == rating_category
          )

          predictive_interval <- unname(
            quantile(
              simulated_proportions,
              c(0.025, 0.975)
            )
          )

          tibble(
            prior_name = prior_name,
            effect_prior_sd = effect_prior_sd,
            base_model = current_base_model,
            comparison = current_comparison,
            rating = rating_category,
            n_observed = length(row_indices),
            observed_proportion = observed_proportion,
            predicted_mean = mean(simulated_proportions),
            predicted_median = median(simulated_proportions),
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

  all_ppc[[prior_name]] <- ppc_distribution

  readr::write_csv(
    ppc_distribution,
    file.path(
      ppc_output_dir,
      paste0(
        "rq3_feasibility_",
        prior_name,
        "_ppc_rating_proportions.csv"
      )
    )
  )

  plot_data <- ppc_distribution |>
    mutate(
      comparison = factor(
        comparison,
        levels = comparison_levels
      )
    )

  distribution_plot <- ggplot(
    plot_data,
    aes(
      x = factor(rating),
      y = predicted_median
    )
  ) +
    geom_errorbar(
      aes(
        ymin = predicted_l95,
        ymax = predicted_u95
      ),
      width = 0.15,
      colour = "#0072B2",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        colour = "Posterior prediction",
        shape = "Posterior prediction"
      ),
      size = 2.2
    ) +
    geom_point(
      aes(
        y = observed_proportion,
        colour = "Observed proportion",
        shape = "Observed proportion"
      ),
      stroke = 1.1,
      size = 2.8
    ) +
    scale_colour_manual(
      name = NULL,
      breaks = c(
        "Posterior prediction",
        "Observed proportion"
      ),
      values = c(
        "Posterior prediction" = "#0072B2",
        "Observed proportion" = "#D55E00"
      )
    ) +
    scale_shape_manual(
      name = NULL,
      breaks = c(
        "Posterior prediction",
        "Observed proportion"
      ),
      values = c(
        "Posterior prediction" = 16,
        "Observed proportion" = 4
      )
    ) +
    facet_grid(
      rows = vars(base_model),
      cols = vars(comparison)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +
    labs(
      title = paste(
        "RQ3 feasibility prior sensitivity:",
        prior_name,
        "prior"
      ),
      subtitle = "Base-model-specific thresholds",
      x = "Rating category",
      y = "Proportion"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 8
      ),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  plot_stem <- file.path(
    ppc_output_dir,
    paste0(
      "rq3_feasibility_",
      prior_name,
      "_ppc_distributions"
    )
  )

  ggsave(
    filename = paste0(plot_stem, ".png"),
    plot = distribution_plot,
    width = 16,
    height = 9,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    filename = paste0(plot_stem, ".eps"),
    plot = distribution_plot,
    width = 16,
    height = 9,
    dpi = 300,
    bg = "white",
    device = grDevices::cairo_ps,
    onefile = FALSE,
    fallback_resolution = 600
  )

  message("Completed prior specification: ", prior_name)
}

# ------------------------------------------------------------------
# 4. Combine all specifications
# ------------------------------------------------------------------

combined_effects <- bind_rows(all_effects) |>
  arrange(base_model, anchor_type, effect_prior_sd)

combined_diagnostics <- bind_rows(all_diagnostics) |>
  arrange(effect_prior_sd)

combined_ppc <- bind_rows(all_ppc) |>
  arrange(
    prior_name,
    base_model,
    comparison,
    rating
  )

ppc_summary <- combined_ppc |>
  group_by(prior_name, effect_prior_sd) |>
  summarise(
    category_checks_passed = sum(
      observed_within_predictive_95
    ),
    category_checks_total = n(),
    median_absolute_difference = median(
      absolute_difference
    ),
    maximum_absolute_difference = max(
      absolute_difference
    ),
    .groups = "drop"
  ) |>
  arrange(effect_prior_sd)

robustness_summary <- combined_effects |>
  group_by(base_model, anchor_type) |>
  summarise(
    conclusions = paste(
      paste0(prior_name, ": ", conclusion),
      collapse = "; "
    ),
    conclusion_stable =
      dplyr::n_distinct(conclusion) == 1,
    OR_median_min = min(OR_median),
    OR_median_max = max(OR_median),
    minimum_probability_OR_gt_1 = min(
      posterior_probability_OR_gt_1
    ),
    maximum_probability_OR_gt_1 = max(
      posterior_probability_OR_gt_1
    ),
    .groups = "drop"
  )

readr::write_csv(
  combined_effects,
  file.path(
    output_dir,
    "rq3_feasibility_prior_sensitivity_effects.csv"
  )
)

readr::write_csv(
  combined_diagnostics,
  file.path(
    output_dir,
    "rq3_feasibility_prior_sensitivity_diagnostics.csv"
  )
)

readr::write_csv(
  combined_ppc,
  file.path(
    output_dir,
    "rq3_feasibility_prior_sensitivity_ppc_all.csv"
  )
)

readr::write_csv(
  ppc_summary,
  file.path(
    output_dir,
    "rq3_feasibility_prior_sensitivity_ppc_summary.csv"
  )
)

readr::write_csv(
  robustness_summary,
  file.path(
    output_dir,
    "rq3_feasibility_prior_sensitivity_robustness.csv"
  )
)

cat("\nCombined condition-effect results:\n")
print(combined_effects)

cat("\nSampling diagnostics:\n")
print(combined_diagnostics)

cat("\nPosterior predictive summary:\n")
print(ppc_summary)

cat("\nConclusion stability across priors:\n")
print(robustness_summary)

message("\nRQ3 feasibility prior-sensitivity analysis complete.")
message("Results saved in: ", output_dir)
