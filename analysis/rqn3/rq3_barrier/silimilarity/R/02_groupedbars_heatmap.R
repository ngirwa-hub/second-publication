# install.packages(c("tidyverse","patchwork"))  # run once if needed
library(tidyverse)
library(patchwork)
options(dplyr.summarise.inform = FALSE)

# =======================
# OPTIONS — tweak freely
# =======================
csv_path <- "../merged_barrier_anchorctx_final.csv"  # <-- set your path
baseline_arm <- "context"                            # baseline for Δ heatmap
decimals <- 1                                        # label decimals on bars
show_ci  <- TRUE                                     # 95% CI whiskers on bars

# Desired order of arms in plots/CSVs
arm_order <- c("context", "word", "example", "low-numeric", "high-numeric")

# Optional order of base families (used only for facet consistency later if needed)
fam_order  <- c("phi4","llama","mistral","gemma3")

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

# Keep only the two main conditions mentioned: context and anchor
df <- df |>
  filter(condition %in% c("context","anchor"))

# ---- Derive ANCHOR arms robustly from several optional columns ----
na_chr <- rep(NA_character_, nrow(df))
anchor_raw <- tolower(dplyr::coalesce(
  if ("anchor_arm"  %in% names(df)) df$anchor_arm  else na_chr,
  if ("anchor_type" %in% names(df)) df$anchor_type else na_chr,
  if ("bias_type"   %in% names(df)) df$bias_type   else na_chr,
  if ("scenario"    %in% names(df)) df$scenario    else na_chr
))

df <- df |>
  mutate(
    arm = case_when(
      condition == "context" ~ "context",
      condition == "anchor" & str_detect(anchor_raw, "word")        ~ "word",
      condition == "anchor" & str_detect(anchor_raw, "exam")        ~ "example",
      condition == "anchor" & str_detect(anchor_raw, "low")         ~ "low-numeric",
      condition == "anchor" & str_detect(anchor_raw, "high")        ~ "high-numeric",
      condition == "anchor" ~ "anchor-other",
      TRUE ~ "other"
    )
  )

# Final arm ordering (append any unexpected levels at the end)
arms_present <- unique(df$arm)
arms <- c(arm_order[arm_order %in% arms_present],
          setdiff(arms_present, arm_order))

# Base families ordering
families <- c(fam_order[fam_order %in% unique(df$base_family)],
              setdiff(unique(df$base_family), fam_order))

barriers <- sort(unique(df$barrier_id))
stopifnot(baseline_arm %in% arms)

# =======================
# PER-ITERATION SELECTION RATES (by base_family × arm × iteration × barrier)
# =======================
counts <- df |> count(base_family, arm, iteration, barrier_id, name = "n_sel")
totals <- df |> count(base_family, arm, iteration, name = "total_sel")

# Ensure every barrier is present for every (family, arm, iteration)
grid <- tidyr::crossing(
  totals |> distinct(base_family, arm, iteration),
  tibble(barrier_id = barriers)
)

rates <- grid |>
  left_join(counts, by = c("base_family","arm","iteration","barrier_id")) |>
  left_join(totals, by = c("base_family","arm","iteration")) |>
  mutate(n_sel = replace_na(n_sel, 0L),
         selection_rate = n_sel / total_sel)

# =======================
# SUMMARIES / EXPORTS
# =======================
# (A) by base_family × arm × barrier
family_summ <- rates |>
  group_by(base_family, arm, barrier_id) |>
  summarise(
    mean_pct = 100*mean(selection_rate, na.rm=TRUE),
    sd_rate  = sd(selection_rate, na.rm=TRUE),
    n_iter   = n_distinct(iteration),
    .groups="drop"
  ) |>
  mutate(
    ci95_pct    = ifelse(n_iter > 0, 100*1.96*(sd_rate/sqrt(n_iter)), NA_real_),
    base_family = factor(base_family, levels = families),
    arm         = factor(arm,         levels = arms),
    barrier_id  = factor(barrier_id,  levels = barriers)
  )

# (B) aggregated across families: arm × barrier
# Treat each (base_family, iteration) as a unit
cond_rates <- rates |>
  mutate(unit = paste(base_family, iteration)) |>
  group_by(arm, barrier_id) |>
  summarise(
    mean_pct = 100*mean(selection_rate, na.rm=TRUE),
    sd_rate  = sd(selection_rate, na.rm=TRUE),
    n_units  = n_distinct(unit),
    .groups="drop"
  ) |>
  mutate(
    ci95_pct  = ifelse(n_units > 0, 100*1.96*(sd_rate/sqrt(n_units)), NA_real_),
    arm       = factor(arm, levels = arms),
    barrier_id = factor(barrier_id, levels = barriers)
  )

# Write CSVs
readr::write_csv(cond_rates |> arrange(arm, barrier_id),
                 "rq3_selection_rates_by_arm.csv")
readr::write_csv(family_summ |> arrange(base_family, arm, barrier_id),
                 "rq3_selection_rates_by_base_family_and_arm.csv")

# =======================
# LEFT: GROUPED BARS (bars = arms, aggregated across base models)
# =======================
pd <- position_dodge(width = 0.8)
p_grouped <- ggplot(cond_rates, aes(x = barrier_id, y = mean_pct, fill = arm)) +
  geom_col(position = pd, width = 0.75) +
  {if (show_ci) geom_errorbar(aes(ymin = mean_pct - ci95_pct,
                                  ymax = mean_pct + ci95_pct),
                              position = pd, width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf(paste0("%.", decimals, "f"), mean_pct)),
            position = position_dodge(width = 0.8), vjust = -0.25, size = 3) +
  labs(x = "Barrier code", y = "Selection rate (%)", fill = "Arm",
       title = "Grouped anchor aggregated across base models") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "top")

# =======================
# RIGHT: Δ HEATMAP (changes vs baseline_arm, aggregated)
# =======================
base_tbl <- cond_rates |>
  filter(arm == baseline_arm) |>
  select(barrier_id, mean_base = mean_pct)

delta <- cond_rates |>
  filter(arm != baseline_arm) |>
  left_join(base_tbl, by = "barrier_id") |>
  mutate(diff_pct = mean_pct - mean_base,
         arm = droplevels(arm),
         base_family = factor("ALL", levels = "ALL"))

# symmetric color limits across all facets; red = decrease, green = increase
lim <- max(abs(delta$diff_pct), na.rm = TRUE); if (!is.finite(lim)) lim <- 1

p_heat <- ggplot(delta, aes(x = barrier_id, y = base_family, fill = diff_pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%+.*f", decimals, diff_pct)), size = 3) +
  facet_wrap(~ arm, ncol = 1) +
  scale_fill_gradient2(
    low = "#d73027", mid = "#f7f7f7", high = "#1a9850",
    midpoint = 0, limits = c(-lim, lim),
    name = paste0("Δ pp vs ", baseline_arm)
  ) +
  labs(x = "Barrier code", y = NULL, title = "Anchor vs Context") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title.position = "plot",
        legend.position = "right")

# =======================
# COMBINE & SAVE
# =======================
combo <- p_grouped | p_heat + plot_layout(widths = c(3.2, 1.6))
print(combo)
ggsave("grouped_by_arm_plus_delta_heatmap.png", combo, width = 18, height = 8, dpi = 300)

# Also export the delta table (aggregate mode)
readr::write_csv(delta |> select(arm, barrier_id, diff_pct) |> arrange(arm, barrier_id),
                 "delta_vs_baseline_by_arm.csv")
