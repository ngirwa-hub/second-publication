#!/usr/bin/env Rscript
# Barrier-adjusted primacy/recency analysis of the randomized barrier experiment.
# Run from Linux or source this file in RStudio:
# Rscript /home/ngirwa/TiiS/second-publication/variability-tests/variability-barrier-list/p-r-effect.R
# Optional: --bootstrap 1000 --output-dir /your/output/folder --validate-only
# Required packages (installed separately, never automatically by this script):
# install.packages(c("jsonlite", "brglm2"), repos = "https://cloud.r-project.org")
#
# One binomial logistic model is fitted per base model, pooling its responses:
# selected/not selected ~ barrier identity + Early/Middle/Late presentation group.
# Early=1:3, Middle=4:8 (reference), Late=9:11. The full eleven-item presentations
# are reconstructed from successful iteration logs, including unselected items.
# Mean bias reduction handles complete separation (Phi-4 never selected B2).
# Whole-iteration bootstrap resampling retains within-iteration dependence.
# Predictions average equally over all eleven barrier identities.
# CIs are pointwise 95% percentile intervals, exploratory and not multiplicity-adjusted.

options(stringsAsFactors = FALSE, warn = 1)

script_directory <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }
  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(dirname(normalizePath(frame$ofile, mustWork = TRUE)))
    }
  }
  normalizePath("/home/ngirwa/TiiS/second-publication/variability-tests/variability-barrier-list", mustWork = TRUE)
}

SCRIPT_DIR <- script_directory()
DATA_DIR <- file.path(SCRIPT_DIR, "outputs", "randOrder-seed42")
OUTPUT_DIR <- file.path(SCRIPT_DIR, "analysis-results")
N_BOOT <- 1000L
ANALYSIS_SEED <- 42L  # Bootstrap only; separate from Ollama generation seed.
EXPECTED_ITERATIONS <- 50L
VALIDATE_ONLY <- FALSE
args <- commandArgs(trailingOnly = TRUE)
i <- 1L
while (i <= length(args)) {
  arg <- args[[i]]
  if (arg == "--validate-only") {
    VALIDATE_ONLY <- TRUE
  } else if (arg == "--help") {
    cat("Usage: Rscript p-r-effect.R [--bootstrap N] [--data-dir PATH] [--output-dir PATH] [--validate-only]\n")
    quit(status = 0L)
  } else if (arg %in% c("--bootstrap", "--data-dir", "--output-dir")) {
    if (i == length(args)) stop("Missing value after ", arg)
    i <- i + 1L
    value <- args[[i]]
    if (arg == "--bootstrap") N_BOOT <- suppressWarnings(as.integer(value))
    if (arg == "--data-dir") DATA_DIR <- value
    if (arg == "--output-dir") OUTPUT_DIR <- value
  } else {
    stop("Unknown argument: ", arg)
  }
  i <- i + 1L
}
if (is.na(N_BOOT) || N_BOOT < 20L) stop("--bootstrap must be an integer of at least 20.")
if (!VALIDATE_ONLY && N_BOOT < 1000L) warning("Fewer than 1000 bootstrap replicates: use this for a smoke test, not final interpretation.")
needed <- if (VALIDATE_ONLY) "jsonlite" else c("jsonlite", "brglm2")
missing_packages <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
       ". Install them with install.packages(c(\"jsonlite\", \"brglm2\"), repos=\"https://cloud.r-project.org\").")
}

MODEL_NAMES <- c(phi4 = "Phi-4", gemma3 = "Gemma 3", llama = "Llama", mistral = "Mistral")
ROLES <- c("generalist", "normative", "subject-matter")
GROUPS <- c("Early", "Middle", "Late")
BARRIERS <- c(
  "power losses, quality and safety issues",
  "reduced reliability in DC devices",
  "lack of use-cases in which DC is advantageous",
  "uncertain utility interaction (net metering, utility ownership, and agreed standards)",
  "lack of pilot projects",
  "public perception of DC and readiness to 'champion' installations from DC projects",
  "incompatibility of DC systems components",
  "misconception and lack of knowledge leads to lengthy/expensive design and permit process",
  "lack of enough trained personnel in DC systems",
  "uncertain regulatory roadmap",
  "high costs of DC solutions"
)

require_true <- function(test, message) {
  if (!isTRUE(test)) stop(message, call. = FALSE)
}
integer_column <- function(x, name) {
  value <- suppressWarnings(as.numeric(x))
  require_true(all(is.finite(value) & value == floor(value)), paste("Invalid integer values in", name))
  as.integer(value)
}
write_table <- function(x, name) {
  write.csv(x, file.path(OUTPUT_DIR, name), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

input_csv <- file.path(DATA_DIR, "zeroshotBarrierSelectResponses_seed42_randOrder.csv")
require_true(file.exists(input_csv), paste("Missing randomized barrier CSV:", input_csv))
selected <- read.csv(input_csv, colClasses = "character", check.names = FALSE, na.strings = character(), fileEncoding = "UTF-8-BOM")
required_columns <- c("row_id", "base_model", "model", "iteration", "barrier_id", "displayed_position", "official_label", "attempt_id", "model_seed", "label_status")
require_true(all(required_columns %in% names(selected)), "Randomized CSV lacks required columns.")
require_true(nrow(selected) == 4L * 3L * EXPECTED_ITERATIONS * 5L, "Expected 3000 selections from 50 complete iterations.")
require_true(!anyDuplicated(selected$row_id), "Duplicate row IDs in randomized CSV.")
require_true(all(selected$base_model %in% names(MODEL_NAMES)), "Unknown base model.")
require_true(all(selected$model %in% ROLES), "Unknown response role.")
require_true(all(selected$model_seed == "42" & selected$label_status == "matched"), "Saved rows contain an unexpected model seed or unmatched labels.")
selected$iteration <- integer_column(selected$iteration, "iteration")
selected$barrier_id <- integer_column(selected$barrier_id, "barrier_id")
selected$displayed_position <- integer_column(selected$displayed_position, "displayed_position")
require_true(setequal(selected$iteration, 0:(EXPECTED_ITERATIONS - 1L)), "Expected iteration IDs 0 through 49.")
canonical_id <- match(selected$official_label, BARRIERS)
require_true(!anyNA(canonical_id) && all(canonical_id == selected$barrier_id), "Official labels disagree with the canonical barrier dictionary.")
require_true(all(selected$displayed_position %in% 1:11), "Invalid presentation positions.")
response_key <- paste(selected$base_model, selected$model, selected$iteration, sep = "|")
require_true(all(table(response_key) == 5L), "Every response must have exactly five saved selections.")
require_true(!anyDuplicated(paste(response_key, selected$barrier_id, sep = "|")), "Repeated barrier within a response.")
for (model in names(MODEL_NAMES)) {
  for (role in ROLES) {
    ids <- selected$iteration[selected$base_model == model & selected$model == role]
    require_true(setequal(ids, 0:(EXPECTED_ITERATIONS - 1L)), "A model has incomplete pooled responses.")
  }
}

# Only successful attempts linked to the saved CSV define the 50 presentations.
order_rows <- list()
policy_rows <- list()
input_paths <- input_csv
for (iteration in sort(unique(selected$iteration))) {
  block <- selected[selected$iteration == iteration, , drop = FALSE]
  attempt_id <- unique(block$attempt_id)
  require_true(length(attempt_id) == 1L, paste("Mixed attempt IDs in iteration", iteration))
  order_path <- file.path(DATA_DIR, "zeroshotBarrierSelectOrderLogs", paste0(attempt_id, ".json"))
  require_true(file.exists(order_path), paste("Missing order log:", order_path))
  record <- jsonlite::fromJSON(order_path, simplifyVector = FALSE)
  require_true(identical(record$status, "success") && record$iteration == iteration && identical(record$attempt_id, attempt_id), "Order-log metadata disagrees with the CSV.")
  require_true(record$model_seed == 42, "Order log uses an unexpected model seed.")
  order <- data.frame(
    iteration = iteration, attempt_id = attempt_id,
    barrier_id = vapply(record$order, function(x) as.integer(x$barrier_id), integer(1)),
    displayed_position = vapply(record$order, function(x) as.integer(x$displayed_position), integer(1)),
    official_label = vapply(record$order, function(x) x$official_label, character(1))
  )
  require_true(nrow(order) == 11L && setequal(order$barrier_id, 1:11) && identical(order$displayed_position, 1:11), "Order log is not a complete permutation of the 11 barriers.")
  require_true(all(order$official_label == BARRIERS[order$barrier_id]), "Canonical labels disagree with order log.")
  expected_bullets <- paste(paste0("- ", order$official_label), collapse = "\n")
  prompt_parts <- strsplit(record$prompt, "\n\nBarriers:\n", fixed = TRUE)[[1]]
  require_true(length(prompt_parts) == 2L && identical(prompt_parts[[2]], expected_bullets), "Recorded prompt does not contain its logged barrier order.")
  require_true(all(block$displayed_position == order$displayed_position[match(block$barrier_id, order$barrier_id)]), "Selected rows disagree with logged presentation positions.")
  order_rows[[length(order_rows) + 1L]] <- order
  policy <- record$parser_policy
  policy_rows[[length(policy_rows) + 1L]] <- data.frame(
    iteration = iteration,
    threshold = if (is.null(policy$threshold)) NA_real_ else policy$threshold,
    predefined_aliases = length(policy$aliases),
    selection_rule = if (is.null(policy$selection_rule)) "not recorded" else policy$selection_rule
  )
  input_paths <- c(input_paths, order_path)
}
orders <- do.call(rbind, order_rows)
parser_policies <- do.call(rbind, policy_rows)
require_true(nrow(orders) == 550L, "Expected 550 presentation records.")

# Reconstruct zeros as well as selected items for every pooled response.
responses <- unique(selected[c("base_model", "model", "iteration", "attempt_id")])
analysis <- merge(responses, orders, by = c("iteration", "attempt_id"), all.x = TRUE, sort = FALSE)
require_true(nrow(analysis) == 4L * 3L * EXPECTED_ITERATIONS * 11L, "Incomplete reconstruction of presented but unselected barriers.")
selection_keys <- paste(selected$base_model, selected$model, selected$iteration, selected$barrier_id, sep = "|")
analysis_keys <- paste(analysis$base_model, analysis$model, analysis$iteration, analysis$barrier_id, sep = "|")
analysis$selected <- as.integer(analysis_keys %in% selection_keys)
analysis$position_group <- ifelse(analysis$displayed_position <= 3L, "Early", ifelse(analysis$displayed_position <= 8L, "Middle", "Late"))
require_true(sum(analysis$selected) == 3000L, "Reconstructed selected count does not match source CSV.")
require_true(all(tapply(analysis$selected, paste(analysis$base_model, analysis$model, analysis$iteration), sum) == 5L), "Reconstructed response does not have five selected barriers.")
analysis$presented <- 1L

# Pool within each model/iteration/barrier; grouped binomial data preserve the
# same logistic likelihood as the expanded selected/not-selected records.
pooled <- aggregate(cbind(selected, presented) ~ base_model + iteration + barrier_id + displayed_position + position_group, data = analysis, FUN = sum)
require_true(all(pooled$presented == 3L), "Unequal response contribution in pooled data.")
pooled$barrier <- factor(pooled$barrier_id, levels = 1:11)
pooled$position_group <- factor(pooled$position_group, levels = c("Middle", "Early", "Late"))

validation <- do.call(rbind, lapply(names(MODEL_NAMES), function(model) {
  x <- analysis[analysis$base_model == model, , drop = FALSE]
  counts <- tapply(x$selected, factor(x$barrier_id, levels = 1:11), sum)
  constant <- which(counts == 0L | counts == 150L)
  data.frame(
    model = MODEL_NAMES[[model]], successful_iterations = length(unique(x$iteration)),
    pooled_responses = nrow(x) / 11L, presented_opportunities = nrow(x),
    selected = sum(x$selected), not_selected = sum(1L - x$selected),
    constant_outcome_barriers = paste(paste0("B", constant), collapse = "; ")
  )
}))

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_table(validation, "validation_summary.csv")
write_table(orders, "presentation_orders.csv")
write_table(parser_policies, "parser_policy_provenance.csv")
write_table(pooled[c("base_model", "iteration", "barrier_id", "displayed_position", "position_group", "selected", "presented")], "analysis_dataset.csv")
write_table(data.frame(barrier_id = paste0("B", 1:11), label = BARRIERS), "barrier_dictionary.csv")
write_table(data.frame(file = input_paths, md5 = unname(tools::md5sum(input_paths))), "input_checksums.csv")
cat("Validated 50 shared randomized presentations; 150 pooled responses per model.\n")
if (VALIDATE_ONLY) {
  writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, "sessionInfo.txt"))
  cat("Validation-only outputs saved to:", OUTPUT_DIR, "\n")
} else {
  fit_model <- function(x, frequencies = NULL) {
    if (is.null(frequencies)) frequencies <- rep(1, nrow(x))
    x$.bootstrap_weight <- frequencies
    x <- x[x$.bootstrap_weight > 0, , drop = FALSE]
    messages <- character()
    fit <- withCallingHandlers(
      glm(cbind(selected, presented - selected) ~ barrier + position_group,
          data = x, family = binomial(link = "logit"), weights = .bootstrap_weight,
          method = brglm2::brglmFit,
          control = brglm2::brglmControl(type = "AS_mean", maxit = 100)),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    require_true(isTRUE(fit$converged) && all(is.finite(coef(fit))) && !anyNA(coef(fit)), "Logistic model failed to converge with finite coefficients.")
    attr(fit, "fit_warnings") <- unique(messages)
    fit
  }

  prediction_grid <- data.frame(
    barrier = factor(rep(1:11, times = 3L), levels = 1:11),
    position_group = factor(rep(GROUPS, each = 11L), levels = c("Middle", "Early", "Late"))
  )
  adjusted_probabilities <- function(fit) {
    probability <- as.numeric(predict(fit, newdata = prediction_grid, type = "response"))
    require_true(all(is.finite(probability)), "Invalid adjusted probabilities.")
    setNames(colMeans(matrix(probability, nrow = 11L, ncol = 3L)), GROUPS)
  }
  contrasts_from <- function(probability) {
    c("Early minus Middle" = unname(probability["Early"] - probability["Middle"]),
      "Late minus Middle" = unname(probability["Late"] - probability["Middle"]),
      "Early minus Late" = unname(probability["Early"] - probability["Late"]))
  }
  interval <- function(values) as.numeric(quantile(values, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE))

  # Same iteration draws are used across the four independently fitted models.
  set.seed(ANALYSIS_SEED)
  iteration_ids <- sort(unique(pooled$iteration))
  draws <- replicate(N_BOOT, sample(iteration_ids, length(iteration_ids), replace = TRUE))
  probability_rows <- contrast_rows <- diagnostic_rows <- list()
  saved_models <- bootstrap_samples <- list()

  for (model in names(MODEL_NAMES)) {
    cat("Fitting", MODEL_NAMES[[model]], "and", N_BOOT, "iteration-bootstrap replicates...\n")
    x <- pooled[pooled$base_model == model, , drop = FALSE]
    fit <- fit_model(x)
    estimate <- adjusted_probabilities(fit)
    point_contrasts <- contrasts_from(estimate)
    boot_prob <- matrix(NA_real_, nrow = N_BOOT, ncol = 3L, dimnames = list(NULL, GROUPS))
    failure_messages <- character()
    for (b in seq_len(N_BOOT)) {
      frequency <- tabulate(match(draws[, b], iteration_ids), nbins = length(iteration_ids))
      weights <- frequency[match(x$iteration, iteration_ids)]
      result <- tryCatch(adjusted_probabilities(fit_model(x, weights)), error = function(e) e)
      if (inherits(result, "error")) {
        failure_messages <- c(failure_messages, conditionMessage(result))
      } else {
        boot_prob[b, ] <- result
      }
      if (b %% 100L == 0L) cat(" ", MODEL_NAMES[[model]], ":", b, "/", N_BOOT, "\n")
    }
    valid <- complete.cases(boot_prob)
    require_true(sum(valid) >= ceiling(0.95 * N_BOOT), paste(MODEL_NAMES[[model]], "has too many unsuccessful bootstrap fits; inspect the data."))
    boot_contrasts <- t(apply(boot_prob, 1L, contrasts_from))
    for (group in GROUPS) {
      ci <- interval(boot_prob[, group])
      probability_rows[[length(probability_rows) + 1L]] <- data.frame(
        base_model = model, model = MODEL_NAMES[[model]], position_group = group,
        positions = switch(group, Early = "1-3", Middle = "4-8", Late = "9-11"),
        probability_pct = 100 * estimate[[group]], ci_lower_pct = 100 * ci[1L], ci_upper_pct = 100 * ci[2L],
        successful_bootstrap_fits = sum(valid)
      )
    }
    for (contrast in names(point_contrasts)) {
      ci <- interval(boot_contrasts[, contrast])
      interpretation <- if (contrast == "Early minus Middle") {
        if (ci[1L] > 0) "Higher early than middle selection (primacy-consistent)" else if (ci[2L] < 0) "Lower early than middle selection" else "No clear early-middle difference"
      } else if (contrast == "Late minus Middle") {
        if (ci[1L] > 0) "Higher late than middle selection (recency-consistent)" else if (ci[2L] < 0) "Lower late than middle selection" else "No clear late-middle difference"
      } else {
        if (ci[1L] > 0) "Higher early than late selection" else if (ci[2L] < 0) "Higher late than early selection" else "No clear early-late difference"
      }
      contrast_rows[[length(contrast_rows) + 1L]] <- data.frame(
        model = MODEL_NAMES[[model]], contrast = contrast,
        difference_pp = 100 * point_contrasts[[contrast]],
        ci_lower_pp = 100 * ci[1L], ci_upper_pp = 100 * ci[2L],
        interpretation = interpretation
      )
    }
    saved_models[[model]] <- fit
    bootstrap_samples[[model]] <- boot_prob
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.frame(
      model = MODEL_NAMES[[model]], estimator = "Mean-bias-reduced binomial logistic regression",
      converged = fit$converged, pooled_binomial_rows = nrow(x), iteration_clusters = length(iteration_ids),
      requested_bootstrap_fits = N_BOOT, successful_bootstrap_fits = sum(valid),
      fit_warnings = paste(attr(fit, "fit_warnings"), collapse = "; "),
      bootstrap_error_messages = paste(unique(failure_messages), collapse = "; ")
    )
  }

  probabilities <- do.call(rbind, probability_rows)
  contrasts <- do.call(rbind, contrast_rows)
  diagnostics <- do.call(rbind, diagnostic_rows)
  write_table(probabilities, "adjusted_selection_probabilities.csv")
  write_table(contrasts, "position_contrasts.csv")
  write_table(diagnostics, "model_diagnostics.csv")
  saveRDS(saved_models, file.path(OUTPUT_DIR, "fitted_models.rds"))
  saveRDS(list(iteration_draws = draws, probabilities = bootstrap_samples, seed = ANALYSIS_SEED), file.path(OUTPUT_DIR, "iteration_bootstrap.rds"))

  plot_model <- function(model, small = FALSE) {
    z <- probabilities[probabilities$base_model == model, , drop = FALSE]
    z <- z[match(GROUPS, z$position_group), , drop = FALSE]
    par(mar = if (small) c(4, 4.5, 3, 1) else c(5, 5, 3.5, 1))
    plot(1:3, z$probability_pct, xlim = c(0.65, 3.35), ylim = c(0, 100),
         xaxt = "n", xlab = "Presentation position", ylab = "Adjusted selection probability (%)",
         main = MODEL_NAMES[[model]], pch = 19, col = "#205A81", cex = 1.2)
    axis(1, at = 1:3, labels = c("Early\n(1-3)", "Middle\n(4-8)", "Late\n(9-11)"))
    abline(h = seq(0, 100, 20), col = "grey90", lty = 1)
    arrows(1:3, z$ci_lower_pct, 1:3, z$ci_upper_pct, angle = 90, code = 3, length = 0.05, col = "#205A81", lwd = 2)
    points(1:3, z$probability_pct, pch = 19, col = "#205A81", cex = 1.2)
  }
  for (model in names(MODEL_NAMES)) {
    pdf(file.path(OUTPUT_DIR, paste0(model, "_primacy_recency.pdf")), width = 6.5, height = 5)
    plot_model(model)
    dev.off()
    if (capabilities("cairo")) {
      png(file.path(OUTPUT_DIR, paste0(model, "_primacy_recency.png")), width = 1600, height = 1250, res = 240, type = "cairo")
      plot_model(model)
      dev.off()
    }
  }
  pdf(file.path(OUTPUT_DIR, "all_models_primacy_recency.pdf"), width = 10, height = 8)
  par(mfrow = c(2, 2))
  for (model in names(MODEL_NAMES)) plot_model(model, small = TRUE)
  dev.off()

  method_notes <- c(
    "ANALYSIS: primacy and recency in the randomized barrier experiment",
    paste("Input CSV:", normalizePath(input_csv)),
    "Each base model is fitted separately; all its responses are pooled.",
    "All 11 shown barriers are reconstructed for every response, including nonselections.",
    "Model: cbind(selected, presented-selected) ~ barrier + position_group; binomial logit.",
    "Presentation groups: Early 1-3; Middle 4-8 (reference); Late 9-11.",
    "Estimator: brglm2 mean-bias-reduced logistic regression (AS_mean), for all models consistently.",
    "Reason: Phi-4 never selected B2, causing separation in ordinary maximum-likelihood logistic regression.",
    "Adjusted probabilities average predictions equally across the same 11 canonical barrier identities.",
    paste("Uncertainty:", N_BOOT, "whole-iteration bootstrap resamples; seed", ANALYSIS_SEED),
    "Each resampled iteration retains its full order and all pooled responses. Individual barrier rows are not resampled independently.",
    "Intervals: pointwise 95% percentile bootstrap CIs; exploratory, not multiplicity-adjusted.",
    "Early-Middle and Late-Middle contrasts assess primacy- and recency-consistent patterns; Early-Late directly compares the ends.",
    "These are selection probabilities (0-100%), not shares of the 750 selections.",
    "Interpretation concerns the saved generation-and-selection pipeline: only the first five valid unique entries were retained.",
    "Fuzzy corrections, truncation of extra selections, conditioning on successful attempts, and parser-policy changes can influence findings.",
    "The source audit has five successful iterations with predefined aliases, followed by automatic similarity matching; see parser_policy_provenance.csv.",
    "No significance p-values are generated; inspect contrast estimates, intervals, and their practical size.",
    "Sources:",
    "https://search.r-project.org/CRAN/refmans/brglm2/html/brglmFit.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html"
  )
  writeLines(method_notes, file.path(OUTPUT_DIR, "method_notes.txt"))
  writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, "sessionInfo.txt"))
  cat("\nAdjusted probabilities:\n")
  print(probabilities[c("model", "position_group", "probability_pct", "ci_lower_pct", "ci_upper_pct")], row.names = FALSE, digits = 4)
  cat("\nPosition contrasts (percentage points):\n")
  print(contrasts, row.names = FALSE, digits = 4)
  cat("\nOutputs saved to:", normalizePath(OUTPUT_DIR), "\n")
}
