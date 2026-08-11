# Run with: cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq2-rerun" && Rscript "feasibility-analysis/ppc-feasibility-rq2-thres.R"
# Posterior predictive checks for the RQ2 feasibility model with base-model-specific ordinal thresholds.

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

trial_dir <- file.path(
  rq2_dir,
  "feas-ctx-zeroshot-working",
  "bayesian-results",
  "base-model-threshold-trial"
)

fit_path <- file.path(
  trial_dir,
  "rq2_feasibility_base_model_threshold_fit.rds"
)

output_dir <- file.path(
  trial_dir,
  "posterior-predictive-checks"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------------
# 1. Load and verify the original data and fitted model
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

observed <- fit$data |>
  transmute(
    base_model = as.character(base_model),
    condition = as.character(condition),
    rating = as.integer(as.character(rating))
  )

raw_check <- raw |>
  transmute(
    base_model = tolower(trimws(as.character(base_model))),
    condition = toupper(trimws(as.character(condition))),
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
    "The current CSV does not match the data stored in the threshold ",
    "model. Refit the threshold model before running this PPC."
  )
}

if (anyNA(observed$rating)) {
  stop("The fitted model data contain non-numeric rating levels.")
}

rating_values <- sort(unique(observed$rating))

cat(
  "Observed rating categories:",
  paste(rating_values, collapse = ", "),
  "\n"
)

# 2. Generate replicated ratings from the fitted posterior
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

# 3. Compare category proportions within base model and condition
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

readr::write_csv(
  ppc_distribution,
  file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_ppc_rating_proportions.csv"
  )
)

# 4. Compare median ratings as a supplementary ordinal check
ppc_medians <- bind_rows(
  lapply(seq_len(nrow(groups)), function(group_index) {
    current_base_model <- groups$base_model[group_index]
    current_condition <- groups$condition[group_index]

    row_indices <- which(
      observed$base_model == current_base_model &
        observed$condition == current_condition
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
      condition = current_condition,
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
    "rq2_feasibility_base_model_threshold_ppc_median_ratings.csv"
  )
)

# 5. Plot observed and posterior-predicted category proportions
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
      colour = "Posterior Prediction",
      shape = "Posterior Prediction"
    ),
    size = 2.5
  ) +
  geom_point(
    aes(
      y = observed_proportion,
      colour = "Observed Proportion",
      shape = "Observed Proportion"
    ),
    stroke = 1.2,
    size = 3
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = c(
      "Posterior Prediction",
      "Observed Proportion"
    ),
    values = c(
      "Posterior Prediction" = "#0072B2",
      "Observed Proportion" = "#D55E00"
    )
  ) +
  scale_shape_manual(
    name = NULL,
    breaks = c(
      "Posterior Prediction",
      "Observed Proportion"
    ),
    values = c(
      "Posterior Prediction" = 16,
      "Observed Proportion" = 4
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
      "Posterior Predictive Check:",
      "RQ2 Feasibility"
    ),
    subtitle = "Thresholds Grouped by Model",
    x = "Rating Category",
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

ggsave(
  filename = file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_ppc_distributions.png"
  ),
  plot = distribution_plot,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    output_dir,
    "rq2_feasibility_base_model_threshold_ppc_distributions.eps"
  ),
  plot = distribution_plot,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white",
  device = grDevices::cairo_ps,
  onefile = FALSE,
  fallback_resolution = 600
)

# ------------------------------------------------------------------
# 6. Print the numerical PPC assessment
# ------------------------------------------------------------------

failed_distribution_checks <- ppc_distribution |>
  filter(!observed_within_predictive_95)

cat(
  "\nCategory checks within the 95% predictive interval:",
  sum(ppc_distribution$observed_within_predictive_95),
  "of",
  nrow(ppc_distribution),
  "\n"
)

cat("\nCategory checks outside the 95% predictive interval:\n")

if (nrow(failed_distribution_checks) == 0) {
  cat("None\n")
} else {
  print(
    failed_distribution_checks |>
      select(
        base_model,
        condition,
        rating,
        observed_proportion,
        predicted_median,
        predicted_l95,
        predicted_u95
      )
  )
}

cat("\nMedian-rating posterior predictive checks:\n")
print(ppc_medians)

cat(
  "\nThreshold-model PPC outputs saved in:\n",
  output_dir,
  "\n"
)
