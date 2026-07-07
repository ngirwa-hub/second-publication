# --- PER–BASE-MODEL ORs (coef-based; independent of `ratio`) ---
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ordinal)
})

# Expect `feas` and `outdir` already defined earlier in your script.
feas_llm <- feas %>% dplyr::filter(source == "LLM") %>% droplevels()
bm_levels <- levels(feas_llm$base_model)
if (is.null(bm_levels)) bm_levels <- unique(as.character(feas_llm$base_model))

rows <- list()

for (bm in bm_levels) {
  dat_b <- feas_llm %>% dplyr::filter(base_model == bm) %>% droplevels()
  note <- NA_character_

  # minimal checks that often cause failures
  if (dplyr::n_distinct(dat_b$scenario) < 2) note <- "only_one_scenario"
  if (dplyr::n_distinct(dat_b$rating_ord) < 2) note <- "no_rating_variation"

  if (is.na(note)) {
    fit_b <- try(
      clmm(rating_ord ~ scenario + (1 | id_key),
           data = dat_b, link = "logit", Hess = TRUE),
      silent = TRUE
    )

    if (!inherits(fit_b, "try-error")) {
      co <- as.data.frame(coef(summary(fit_b)))
      co$term <- rownames(co)
      sc <- co[grepl("^scenario", co$term), , drop = FALSE]

      if (nrow(sc)) {
        sc$scenario <- sub("^scenario", "", sc$term)
        out <- sc %>%
          dplyr::transmute(
            base_model = bm,
            contrast  = paste0(scenario, " - NEU"),
            scenario  = scenario,
            OR        = exp(Estimate),
            CI_low    = exp(Estimate - 1.96*`Std. Error`),
            CI_high   = exp(Estimate + 1.96*`Std. Error`),
            z         = `z value`,
            p         = 2*pnorm(-abs(`z value`)),
            note      = NA_character_
          )
        rows[[bm]] <- out
      } else {
        note <- "no_scenario_coefs"
      }
    } else {
      note <- "fit_error"
    }
  }

  # if anything went wrong, still emit rows so this base_model appears in the table
  if (is.na(note)) next
  rows[[paste0(bm, "_na")]] <- tibble::tibble(
    base_model = bm,
    contrast   = c("BIAS_WORD - NEU","BIAS_EXAMPLE - NEU","BIAS_NUM_LOW - NEU","BIAS_NUM_HIGH - NEU"),
    scenario   = c("BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH"),
    OR = NA_real_, CI_low = NA_real_, CI_high = NA_real_, z = NA_real_, p = NA_real_,
    note = note
  )
}

res_bm <- dplyr::bind_rows(rows) %>%
  dplyr::arrange(base_model, factor(scenario, levels = c("BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH")))

readr::write_csv(res_bm, file.path(outdir, "feas_or_by_scenario_by_base_model.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_or_by_scenario_by_base_model.csv"))

# ---- 5.3 Diagnostics: distributions per base model × scenario ----
suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2) })

dist_tab <- feas %>%
  filter(source == "LLM") %>% mutate(rating = as.integer(rating)) %>% count(base_model, scenario, rating, name = "n") %>% group_by(base_model, scenario) %>% mutate(prop = n / sum(n)) %>% ungroup() %>% arrange(base_model, scenario, rating)
readr::write_csv(dist_tab, file.path(outdir, "feas_dist_by_base_scenario.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_dist_by_base_scenario.csv"))
# Define pretty labels for base models (facet titles)
base_labels <- c(
  "gemma3-12b" = "Gemma3:12B",
  "llama-pro"  = "LLaMA-Pro",
  "mistral"    = "Mistral",
  "phi4"       = "Phi4"
)

# Define pretty labels for scenarios (x-axis)
scenario_labels <- c(
  "BIAS_EXAMPLE"  = "Example Bias",
  "BIAS_NUM_HIGH" = "High-Numeric Bias",
  "BIAS_NUM_LOW"  = "Low-Numeric Bias",
  "BIAS_WORD"     = "Word Bias",
  "NEU"           = "Neutral"
)

p_dist <- ggplot(
  dist_tab,
  aes(x = scenario, y = prop, fill = factor(rating, levels = 0:4))
) +
  geom_col() +
  facet_wrap(~ base_model, ncol = 2, labeller = labeller(base_model = base_labels)) +
  scale_x_discrete(labels = scenario_labels) +
  labs(
    x = NULL,
    y = "Proportion",
    fill = "Rating",
    title = "Feasibility Rating Distribution by Base Model & Scenario"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.6)  # centers the main title
  )

ggsave(
  file.path(outdir, "feas_stack_by_base_scenario.png"),
  p_dist,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)
message("✅ Wrote: ", file.path(outdir, "feas_stack_by_base_scenario.png"))

