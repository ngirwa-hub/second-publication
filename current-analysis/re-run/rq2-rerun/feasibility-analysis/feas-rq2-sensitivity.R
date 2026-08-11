# Run with: cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq2-rerun" && Rscript "feasibility-analysis/feas-rq2-sensitivity.R"

required_packages <- c(
  "brms",
  "posterior",
  "dplyr",
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
  library(readr)
  library(ggplot2)
})

options(mc.cores = min(4L, parallel::detectCores()))

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
  "prior-sensitivity"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Read and validate the data
df <- readr::read_csv(input_path, show_col_types = FALSE) |>
  mutate(
    condition = toupper(trimws(as.character(condition))),
    base_model = tolower(trimws(as.character(base_model))),
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

if (anyNA(df$condition) ||
    any(!df$condition %in% c("ZEROSHOT", "CONTEXT"))) {
  stop("Condition must contain only ZEROSHOT and CONTEXT.")
}

if (anyNA(df$rating_value) ||
    any(!df$rating_value %in% 0:4)) {
  stop("Ratings must be integers from 0 to 4.")
}

base_model_levels <- c("gemma3", "llama", "mistral", "phi4")
unexpected_base_models <- setdiff(
  unique(df$base_model),
  base_model_levels
)

if (length(unexpected_base_models) > 0) {
  stop(
    "Unexpected base_model values: ",
    paste(unexpected_base_models, collapse = ", ")
  )
}

base_model_levels <- base_model_levels[
  base_model_levels %in% unique(df$base_model)
]

if (length(base_model_levels) != 4) {
  stop(
    "Expected all four base models: gemma3, llama, mistral, phi4."
  )
}

rating_levels <- sort(unique(df$rating_value))

if (length(rating_levels) != 4) {
  stop(
    "This model uses three thresholds and therefore expects exactly ",
    "four observed rating categories. Found: ",
    paste(rating_levels, collapse = ", ")
  )
}

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
    # Zero-shot is 0 and Context is 1. With the formula below, each
    # base-model coefficient is its direct Context-vs-Zero-shot
    # log odds ratio rather than a sum of treatment-coded terms.
    context_indicator = if_else(
      condition == "CONTEXT",
      1,
      0
    ),
    rating = ordered(
      rating_value,
      levels = rating_levels
    )
  )

data_audit <- df |>
  count(base_model, condition, rating_value, name = "n") |>
  arrange(base_model, condition, rating_value)

readr::write_csv(
  data_audit,
  file.path(output_dir, "rq2_feasibility_sensitivity_data_audit.csv")
)

# ------------------------------------------------------------------
# 2. Define the common model and the three effect priors
# ------------------------------------------------------------------

# The group-specific thresholds describe the Zero-shot rating
# distribution for each base model. Each base_model:context_indicator
# coefficient is that base model's change from Zero-shot to Context.
sensitivity_formula <- brms::bf(
  rating | thres(3, gr = base_model) ~
    base_model:context_indicator
)

prior_specifications <- tibble(
  prior_name = c("regularizing", "primary", "weak"),
  effect_prior_sd = c(0.75, 1.50, 2.50),
  adapt_delta = c(0.99, 0.995, 0.99)
)

readr::write_csv(
  prior_specifications,
  file.path(output_dir, "rq2_feasibility_prior_specifications.csv")
)

all_effects <- list()
all_diagnostics <- list()
all_ppc <- list()

# ------------------------------------------------------------------
# 3. Fit, diagnose, contrast, and check each prior specification
# ------------------------------------------------------------------

for (prior_index in seq_len(nrow(prior_specifications))) {
  prior_name <- prior_specifications$prior_name[prior_index]
  effect_prior_sd <-
    prior_specifications$effect_prior_sd[prior_index]
  current_adapt_delta <-
    prior_specifications$adapt_delta[prior_index]

  message(
    "\nFitting prior specification: ",
    prior_name,
    " [Normal(0, ",
    effect_prior_sd,
    ")]"
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
    # set_prior() is used here because the prior SD changes in the
    # loop and must be inserted as a numeric value, not as an
    # unevaluated R variable name.
    brms::set_prior(
      paste0("normal(0, ", effect_prior_sd, ")"),
      class = "b"
    ),
    # Held fixed so this first sensitivity analysis isolates the
    # effect of changing the condition-effect prior.
    prior(
      normal(0, 2),
      class = "Intercept"
    )
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
        "rq2_feasibility_",
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
        "rq2_feasibility_",
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
        "rq2_feasibility_",
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

  divergences <- sum(
    sampler_parameters$Parameter == "divergent__" &
      sampler_parameters$Value == 1
  )

  treedepth_hits <- sum(
    sampler_parameters$Parameter == "treedepth__" &
      sampler_parameters$Value >= 15
  )

  diagnostic_summary <- tibble(
    prior_name = prior_name,
    effect_prior_sd = effect_prior_sd,
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
    divergences = divergences,
    max_treedepth_hits = treedepth_hits
  )

  all_diagnostics[[prior_name]] <- diagnostic_summary

  # ---------------------------------------------------------------
  # 3b. Context-versus-Zero-shot contrasts
  # ---------------------------------------------------------------

  contrast_grid <- tibble(
    base_model = factor(
      base_model_levels,
      levels = base_model_levels
    )
  )

  zeroshot_data <- contrast_grid |>
    mutate(context_indicator = 0)

  context_data <- contrast_grid |>
    mutate(context_indicator = 1)

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

  effect_results <- bind_rows(
    lapply(seq_along(base_model_levels), function(i) {
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
        prior_name = prior_name,
        effect_prior_sd = effect_prior_sd,
        base_model = base_model_levels[i],
        comparison = "CONTEXT vs ZEROSHOT",
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
        "rq2_feasibility_",
        prior_name,
        "_effects.csv"
      )
    )
  )

  # ---------------------------------------------------------------
  # 3c. Posterior predictive category-proportion checks
  # ---------------------------------------------------------------

  set.seed(20260727)

  yrep <- brms::posterior_predict(
    fit,
    ndraws = 1000
  )

  observed <- fit$data |>
    transmute(
      base_model = as.character(base_model),
      # brms keeps only variables used by the model formula in
      # fit$data. Reconstruct the display label from the retained
      # 0/1 condition indicator.
      condition = if_else(
        context_indicator == 1,
        "CONTEXT",
        "ZEROSHOT"
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
      "Posterior-predictive ratings contain unexpected categories ",
      "for ",
      prior_name,
      "."
    )
  }

  groups <- observed |>
    distinct(base_model, condition) |>
    arrange(base_model, condition)

  ppc_distribution <- bind_rows(
    lapply(seq_len(nrow(groups)), function(group_index) {
      current_base_model <- groups$base_model[group_index]
      current_condition <- groups$condition[group_index]

      row_indices <- which(
        observed$base_model == current_base_model &
          observed$condition == current_condition
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
            condition = current_condition,
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
        "rq2_feasibility_",
        prior_name,
        "_ppc_rating_proportions.csv"
      )
    )
  )

  distribution_plot <- ggplot(
    ppc_distribution,
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
      size = 2.5
    ) +
    geom_point(
      aes(
        y = observed_proportion,
        colour = "Observed proportion",
        shape = "Observed proportion"
      ),
      stroke = 1.2,
      size = 3
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
      cols = vars(condition)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +
    labs(
      title = paste(
        "RQ2 feasibility prior sensitivity:",
        prior_name,
        "prior"
      ),
      subtitle = "Base-model-specific thresholds",
      x = "Rating category",
      y = "Proportion"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
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
      "rq2_feasibility_",
      prior_name,
      "_ppc_distributions"
    )
  )

  ggsave(
    filename = paste0(plot_stem, ".png"),
    plot = distribution_plot,
    width = 10,
    height = 9,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    filename = paste0(plot_stem, ".eps"),
    plot = distribution_plot,
    width = 10,
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
# 4. Combine and compare all three specifications
# ------------------------------------------------------------------

combined_effects <- bind_rows(all_effects) |>
  arrange(base_model, effect_prior_sd)

combined_diagnostics <- bind_rows(all_diagnostics) |>
  arrange(effect_prior_sd)

combined_ppc <- bind_rows(all_ppc) |>
  arrange(prior_name, base_model, condition, rating)

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
  group_by(base_model) |>
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
    "rq2_feasibility_prior_sensitivity_effects.csv"
  )
)

readr::write_csv(
  combined_diagnostics,
  file.path(
    output_dir,
    "rq2_feasibility_prior_sensitivity_diagnostics.csv"
  )
)

readr::write_csv(
  combined_ppc,
  file.path(
    output_dir,
    "rq2_feasibility_prior_sensitivity_ppc_all.csv"
  )
)

readr::write_csv(
  ppc_summary,
  file.path(
    output_dir,
    "rq2_feasibility_prior_sensitivity_ppc_summary.csv"
  )
)

readr::write_csv(
  robustness_summary,
  file.path(
    output_dir,
    "rq2_feasibility_prior_sensitivity_robustness.csv"
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

message("\nRQ2 feasibility prior-sensitivity analysis complete.")
message("Results saved in: ", output_dir)
