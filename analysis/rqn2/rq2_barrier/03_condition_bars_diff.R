# install.packages(c("tidyverse"))  # run once
library(tidyverse)
library(patchwork)
library(dplyr)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
options(dplyr.summarise.inform = FALSE)
# =======================
# OPTIONS — tweak freely
# =======================
csv_path <- "merged_barriers_zero_context.csv"
baseline <- "zeroshot"   # baseline condition for the diff panel (e.g., "neu")

# Optional display orders (use if present in your data)
cond_order <- c("zeroshot","context","anchor","inverse order","inverse_order","inverse")
fam_order  <- c("phi4","llama","mistral","gemma3")

# Plot cosmetics
decimals <- 1        # label decimals on bars
show_ci  <- TRUE     # 95% CI whiskers on bars and diff panel

# =======================
# LOAD & PREP
# =======================
df <- readr::read_csv(csv_path, show_col_types = FALSE) |>
  rename_with(tolower) |>
  mutate(
    condition   = tolower(condition),
    barrier_id  = as.integer(barrier_id),
    iteration   = as.integer(iteration),
    base_family = case_when(
      str_starts(tolower(base_model), "llama")   ~ "llama",
      str_starts(tolower(base_model), "phi4")    ~ "phi4",
      str_starts(tolower(base_model), "mistral") ~ "mistral",
      str_starts(tolower(base_model), "gemma3")  ~ "gemma3",
      TRUE ~ tolower(base_model)
    )
  )

# Orders (keep your preferred order first, then any extras)
conditions <- c(cond_order[cond_order %in% unique(df$condition)],
                setdiff(unique(df$condition), cond_order))
families   <- c(fam_order[fam_order %in% unique(df$base_family)],
                setdiff(unique(df$base_family), fam_order))
barriers   <- sort(unique(df$barrier_id))

# =======================
# PER-ITERATION SELECTION RATES
# =======================
# Count selections per base_family × condition × iteration × barrier
counts <- df |>
  count(base_family, condition, iteration, barrier_id, name = "n_sel")

# Denominator per base_family × condition × iteration
totals <- df |>
  count(base_family, condition, iteration, name = "total_sel")

# Full grid so barriers not selected in an iteration appear as 0
grid <- tidyr::crossing(
  totals |> distinct(base_family, condition, iteration),
  tibble(barrier_id = barriers)
)

rates <- grid |>
  left_join(counts, by = c("base_family","condition","iteration","barrier_id")) |>
  left_join(totals, by = c("base_family","condition","iteration")) |>
  mutate(n_sel = replace_na(n_sel, 0L),
         selection_rate = n_sel / total_sel)

# =======================
# SUMMARIES YOU ASKED FOR
# =======================
# (A) Per base_family × condition × barrier (keep for later)
family_summ <- rates |>
  group_by(base_family, condition, barrier_id) |>
  summarise(
    mean_pct = 100*mean(selection_rate, na.rm=TRUE),
    sd_rate  = sd(selection_rate, na.rm=TRUE),          # SD on [0,1]
    n_iter   = n_distinct(iteration),
    .groups="drop"
  ) |>
  mutate(
    ci95_pct   = ifelse(n_iter > 0, 100*1.96*(sd_rate/sqrt(n_iter)), NA_real_),
    base_family = factor(base_family, levels = families),
    condition   = factor(condition,   levels = conditions),
    barrier_id  = factor(barrier_id,  levels = barriers)
  )

# (B) Per condition × barrier (aggregate across base families)
# Pool all per-iteration rates across families by using a composite "unit"
cond_rates <- rates |>
  mutate(unit = paste(base_family, iteration)) |>
  group_by(condition, barrier_id) |>
  summarise(
    mean_pct = 100*mean(selection_rate, na.rm=TRUE),
    sd_rate  = sd(selection_rate, na.rm=TRUE),
    n_units  = n_distinct(unit),
    .groups="drop"
  ) |>
  mutate(
    ci95_pct  = ifelse(n_units > 0, 100*1.96*(sd_rate/sqrt(n_units)), NA_real_),
    condition = factor(condition, levels = conditions),
    barrier_id = factor(barrier_id, levels = barriers)
  )

# Write the two CSVs you requested
readr::write_csv(cond_rates |> arrange(condition, barrier_id),
                 "selection_rates_by_condition.csv")
readr::write_csv(family_summ |> arrange(base_family, condition, barrier_id),
                 "selection_rates_by_base_family.csv")

# =======================
# PLOTS
# =======================
# 1) GROUPED BAR CHART — x = barrier code, bars = CONDITIONS (aggregated across families)
pd <- position_dodge(width = 0.8)

p_grouped <- ggplot(cond_rates, aes(x = barrier_id, y = mean_pct, fill = condition)) +
  geom_col(position = pd, width = 0.75) +
  {if (show_ci) geom_errorbar(aes(ymin = mean_pct - ci95_pct,
                                  ymax = mean_pct + ci95_pct),
                              position = pd, width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf(paste0("%.", decimals, "f"), mean_pct)),
            position = position_dodge(width = 0.8),
            vjust = -0.25, size = 3) +
  labs(x = "Barrier code", y = "Selection rate (%)", fill = "Condition",
       title = "Grouped Barcharts - Context vs Zeroshot") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "top")

# 2) SIDE DIFF PANEL — Δ% = (condition − baseline), per barrier (aggregated across families)
stopifnot(baseline %in% levels(cond_rates$condition))

# Build diff vs baseline for all non-baseline conditions
base_tbl <- cond_rates |>
  filter(condition == baseline) |>
  transmute(barrier_id, mean_base = mean_pct, sd_base = sd_rate, n_base = n_units)

diff_multi <- cond_rates |>
  filter(condition != baseline) |>
  left_join(base_tbl, by = "barrier_id") |>
  mutate(
    diff_pct = mean_pct - mean_base,                        # (cond − baseline)
    # Welch SE on the [0,1] scale ⇒ multiply by 100 for % CI
    se = sqrt( (sd_rate^2)/pmax(1, n_units) + (sd_base^2)/pmax(1, n_base) ),
    ci95 = 100*1.96*se
  )

p_diff <- ggplot(diff_multi, aes(x = barrier_id, y = diff_pct, fill = condition)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(position = "dodge", width = 0.7, alpha = 0.95) +
  {if (show_ci) geom_errorbar(aes(ymin = diff_pct - ci95, ymax = diff_pct + ci95),
                              position = position_dodge(width = 0.7),
                              width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf("%+.1f", diff_pct)),
            position = position_dodge(width = 0.7),
            vjust = -0.25, size = 3) +
  labs(x = "Barrier", y = paste0("Δ mean % (context − ", baseline, ")"),
       title = "Context vs Zeroshot") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# Combine side-by-side
combo <- p_grouped | p_diff + plot_layout(widths = c(3, 1.6))
print(combo)
ggsave("grouped_by_condition_plus_diff.png", combo, width = 18, height = 8, dpi = 300)
