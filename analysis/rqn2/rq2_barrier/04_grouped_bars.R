# install.packages(c("tidyverse"))  # run once
library(tidyverse)
library(patchwork)
library(dplyr)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(dplyr))
options(dplyr.summarise.inform = FALSE)
# =======================
# OPTIONS — tweak freely
# =======================
csv_path <- "merged_barriers_zero_context.csv"  # <- set your path here

# Optional display orders (used if present in your data)
cond_order <- c("zeroshot","neu","anchor","inverse order","inverse_order","inverse")
fam_order  <- c("phi4","llama","mistral","gemma3")

# Plot cosmetics
decimals <- 1        # label decimals on bars
show_ci  <- TRUE     # 95% CI whiskers on bars

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
# (A) Per base_family × condition × barrier  —— keep for later plotting per base model
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

# (B) Per condition × barrier (aggregated across base families)
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
# GROUPED BAR CHART — x = barrier code, bars = CONDITIONS (aggregated across families)
# =======================
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
       title = "Grouped bars (bars = conditions, aggregated across base models)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "top")

print(p_grouped)
ggsave("grouped_by_condition_only.png", p_grouped, width = 14, height = 6, dpi = 300)
