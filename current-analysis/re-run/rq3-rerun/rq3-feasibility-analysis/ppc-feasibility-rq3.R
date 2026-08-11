# Run after feasibility-rq3.R: cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" && Rscript "rq3-feasibility-analysis/ppc-feasibility-rq3.R"
# Posterior predictive checks for the RQ3 feasibility model with
# base-model-specific ordinal thresholds.

required_packages <- c(
  "brms",
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
  library(dplyr)
  library(readr)
  library(ggplot2)
})

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

trial_dir <- file.path(
  rq3_dir,
  "feas-ctx-anchor-working",
  "bayesian-results",
  "base-model-threshold"
)

fit_path <- file.path(
  trial_dir,
  "rq3_feasibility_base_model_threshold_fit.rds"
)

output_dir <- file.path(
  trial_dir,
  "posterior-predictive-checks"
)

if (!file.exists(fit_path)) {
  stop(
    "Fitted threshold model not found: ",
    fit_path,
    ". Run feasibility-rq3.R first."
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

comparison_levels <- c(
  "CONTEXT",
  "ANCHOR_EXAMPLE",
  "ANCHOR_WORD",
  "ANCHOR_NUM_HIGH",
  "ANCHOR_NUM_LOW"
)

# ------------------------------------------------------------------
# 1. Load and verify the CSV and fitted model
# ------------------------------------------------------------------

raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE
)

fit <- readRDS(fit_path)

cat("Rows in original CSV:", nrow(raw), "\n")
cat("Rows used by model:", nrow(fit$data), "\n")

if (nrow(raw) != nrow(fit$data)) {
  stop(
    "The saved model and current CSV contain different numbers of rows."
  )
}

indicator_sum <- with(
  fit$data,
  anchor_example +
    anchor_word +
    anchor_num_high +
    anchor_num_low
)

if (any(!indicator_sum %in% c(0, 1))) {
  stop(
    "The fitted model contains invalid anchor-indicator combinations."
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

raw_check <- raw |>
  mutate(
    condition = toupper(trimws(as.character(condition))),
    anchor_type = toupper(trimws(as.character(anchor_type)))
  ) |>
  transmute(
    base_model = tolower(trimws(as.character(base_model))),
    comparison = case_when(
      condition == "CONTEXT" ~ "CONTEXT",
      condition == "ANCHOR" ~ anchor_type,
      TRUE ~ NA_character_
    ),
    rating = suppressWarnings(as.integer(rating))
  )

data_values_match <- isTRUE(
  all.equal(
    as.data.frame(raw_check),
    as.data.frame(observed),
    check.attributes = FALSE
  )
)

if (!data_values_match) {
  stop(
    "The current CSV does not match the data stored in the fitted ",
    "threshold model. Refit feasibility-rq3.R before running this PPC."
  )
}

if (anyNA(observed$rating) ||
    anyNA(observed$comparison)) {
  stop("The fitted model data contain invalid values.")
}

rating_values <- sort(unique(observed$rating))

if (length(rating_values) != 4) {
  stop("Expected exactly four observed rating categories.")
}

# ------------------------------------------------------------------
# 2. Generate replicated ratings from the posterior
# ------------------------------------------------------------------

set.seed(20260727)

yrep <- brms::posterior_predict(
  fit,
  ndraws = 1000
)

cat(
  "Posterior-predictive matrix:",
  nrow(yrep),
  "simulations x",
  ncol(yrep),
  "observations\n"
)

if (ncol(yrep) != nrow(observed)) {
  stop(
    "Posterior-predictive columns do not match observed rows."
  )
}

if (!all(yrep %in% rating_values)) {
  stop(
    "Posterior-predictive ratings contain unexpected categories."
  )
}

# ------------------------------------------------------------------
# 3. Category-proportion checks for all 20 groups
# ------------------------------------------------------------------

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
      lapply(rating_values, function(rating_category) {
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

readr::write_csv(
  ppc_distribution,
  file.path(
    output_dir,
    "rq3_feasibility_base_model_threshold_ppc_rating_proportions.csv"
  )
)

# ------------------------------------------------------------------
# 4. Supplementary median-rating checks
# ------------------------------------------------------------------

ppc_medians <- bind_rows(
  lapply(seq_len(nrow(groups)), function(group_index) {
    current_base_model <- groups$base_model[group_index]
    current_comparison <- as.character(
      groups$comparison[group_index]
    )

    row_indices <- which(
      observed$base_model == current_base_model &
        observed$comparison == current_comparison
    )

    observed_median <- median(
      observed$rating[row_indices]
    )

    simulated_medians <- apply(
      yrep[, row_indices, drop = FALSE],
      1,
      median
    )

    predictive_interval <- unname(
      quantile(
        simulated_medians,
        c(0.025, 0.975)
      )
    )

    tibble(
      base_model = current_base_model,
      comparison = current_comparison,
      n_observed = length(row_indices),
      observed_median = observed_median,
      predicted_median = median(simulated_medians),
      predicted_l95 = predictive_interval[1],
      predicted_u95 = predictive_interval[2],
      observed_within_predictive_95 =
        observed_median >= predictive_interval[1] &
        observed_median <= predictive_interval[2]
    )
  })
)

readr::write_csv(
  ppc_medians,
  file.path(
    output_dir,
    "rq3_feasibility_base_model_threshold_ppc_median_ratings.csv"
  )
)

# ------------------------------------------------------------------
# 5. Save the numerical assessment
# ------------------------------------------------------------------

ppc_summary <- tibble(
  category_checks_passed = sum(
    ppc_distribution$observed_within_predictive_95
  ),
  category_checks_total = nrow(ppc_distribution),
  category_checks_failed = sum(
    !ppc_distribution$observed_within_predictive_95
  ),
  median_absolute_difference = median(
    ppc_distribution$absolute_difference
  ),
  maximum_absolute_difference = max(
    ppc_distribution$absolute_difference
  ),
  median_checks_passed = sum(
    ppc_medians$observed_within_predictive_95
  ),
  median_checks_total = nrow(ppc_medians)
)

readr::write_csv(
  ppc_summary,
  file.path(
    output_dir,
    "rq3_feasibility_base_model_threshold_ppc_summary.csv"
  )
)

# ------------------------------------------------------------------
# 6. Plot observed and predicted category proportions
# ------------------------------------------------------------------

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
    title = "Posterior Predictive Check: RQ3 Feasibility",
    subtitle = "Thresholds Grouped by Model",
    x = "Rating Category",
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
  output_dir,
  "rq3_feasibility_base_model_threshold_ppc_distributions"
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

failed_checks <- ppc_distribution |>
  filter(!observed_within_predictive_95)

cat("\nRQ3 feasibility PPC summary:\n")
print(ppc_summary)

cat("\nCategory checks outside the 95% predictive interval:\n")

if (nrow(failed_checks) == 0) {
  cat("None\n")
} else {
  print(
    failed_checks |>
      select(
        base_model,
        comparison,
        rating,
        observed_proportion,
        predicted_median,
        predicted_l95,
        predicted_u95
      )
  )
}

message("RQ3 feasibility threshold-model PPC complete.")
message("Results saved in: ", output_dir)
