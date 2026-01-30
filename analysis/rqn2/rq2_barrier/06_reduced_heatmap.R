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
csv_path <- "merged_barriers_zero_context.csv"   # <-- set your path
baseline <- "zeroshot"                                # baseline for Δ heatmap

# Bars panel tweaks
decimals   <- 1          # labels on bars
show_ci    <- TRUE       # 95% CI whiskers on bars
bar_ylim   <- 35         # *** compress bar heights by capping y-axis at this % (set NA to auto)
bar_title  <- "Grouped bars (bars = conditions; aggregated across base models)"

# Heatmap tweaks
annotate_heat <- TRUE
annot_dec     <- 1
legend_pos    <- "right" # "right" / "bottom"
# diverging colors: RED = decrease, GREEN/BLUE = increase
col_low <- "#d73027"; col_mid <- "#f7f7f7"; col_high <- "#1a9850"

# Layout / export
left_width  <- 3.0
right_width <- 1.4
base_height <- 5.0       # base figure height; will grow a bit with # of facets
height_per_facet <- 0.8  # extra height per additional heatmap facet
outfile <- "grouped_by_condition_plus_delta_heatmap_tight.png"

# Optional display orders
cond_order <- c("zeroshot","neu","anchor","inverse order","inverse_order","inverse")
fam_order  <- c("phi4","llama","mistral","gemma3")
# =======================

# LOAD & PREP
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

conditions <- c(cond_order[cond_order %in% unique(df$condition)],
                setdiff(unique(df$condition), cond_order))
families   <- c(fam_order[fam_order %in% unique(df$base_family)],
                setdiff(unique(df$base_family), fam_order))
barriers   <- sort(unique(df$barrier_id))
stopifnot(baseline %in% conditions)

# PER-ITERATION SELECTION RATES
counts <- df |> count(base_family, condition, iteration, barrier_id, name = "n_sel")
totals <- df |> count(base_family, condition, iteration, name = "total_sel")

grid <- tidyr::crossing(
  totals |> distinct(base_family, condition, iteration),
  tibble(barrier_id = barriers)
)

rates <- grid |>
  left_join(counts, by = c("base_family","condition","iteration","barrier_id")) |>
  left_join(totals, by = c("base_family","condition","iteration")) |>
  mutate(n_sel = replace_na(n_sel, 0L),
         selection_rate = n_sel / total_sel)

# SUMMARIES + CSVs
# (A) by base_family × condition × barrier
family_summ <- rates |>
  group_by(base_family, condition, barrier_id) |>
  summarise(
    mean_pct = 100*mean(selection_rate, na.rm=TRUE),
    sd_rate  = sd(selection_rate, na.rm=TRUE),
    n_iter   = n_distinct(iteration),
    .groups="drop"
  ) |>
  mutate(
    ci95_pct   = ifelse(n_iter > 0, 100*1.96*(sd_rate/sqrt(n_iter)), NA_real_),
    base_family = factor(base_family, levels = families),
    condition   = factor(condition,   levels = conditions),
    barrier_id  = factor(barrier_id,  levels = barriers)
  )

# (B) aggregated across families: condition × barrier (bars panel)
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

# write CSVs
readr::write_csv(cond_rates |> arrange(condition, barrier_id),
                 "selection_rates_by_condition.csv")
readr::write_csv(family_summ |> arrange(base_family, condition, barrier_id),
                 "selection_rates_by_base_family.csv")

# LEFT: GROUPED BARS (bars = conditions)
pd <- position_dodge(width = 0.8)
pd <- position_dodge(width = 0.8)

# --- compute label y-position: mean + (CI if shown) + small pad ---
annot_pad <- 1.0  # in percentage points; bump to 1.5–2.0 if needed
cond_rates_lab <- cond_rates |>
  mutate(label_y = mean_pct + ifelse(isTRUE(show_ci), ci95_pct, 0) + annot_pad)

p_grouped <- ggplot(cond_rates, aes(x = barrier_id, y = mean_pct, fill = condition)) +
  geom_col(position = pd, width = 0.75) +
  {if (show_ci) geom_errorbar(aes(ymin = mean_pct - ci95_pct,
                                  ymax = mean_pct + ci95_pct),
                              position = pd, width = 0.25, alpha = 0.7)} +
  geom_text(data = cond_rates_lab,
            aes(y = label_y, label = sprintf("%.1f", mean_pct)),
            position = position_dodge(width = 0.8),
            vjust = 0, size = 3) +
  labs(x = "Barrier code", y = "Selection rate (%)", fill = "Condition") +
  # add top headroom so labels clear the facet strip
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    legend.position = "top",
    # --- strip tweaks (only visible if you facet this panel) ---
    strip.placement = "outside",
    strip.text = element_text(size = 9, margin = margin(t = 2, r = 2, b = 2, l = 2)),
    strip.background = element_rect(fill = NA, colour = NA),
    # a bit of breathing room so labels aren't clipped
    plot.margin = margin(6, 6, 6, 6)
  ) +
  # allow text to render outside the panel if it still overlaps
  coord_cartesian(clip = "off")

# compress bar heights by limiting y-axis
if (!is.na(bar_ylim)) {
  p_grouped <- p_grouped + coord_cartesian(ylim = c(0, bar_ylim), expand = FALSE)
}

# RIGHT: Δ HEATMAP (changes vs baseline, aggregate across families)
base_tbl <- cond_rates |>
  filter(condition == baseline) |>
  select(barrier_id, mean_base = mean_pct)

delta <- cond_rates |>
  filter(condition != baseline) |>
  left_join(base_tbl, by = "barrier_id") |>
  mutate(diff_pct = mean_pct - mean_base,
         condition = droplevels(condition),
         base_family = factor("ALL", levels = "ALL"))

# symmetric color limit; red = decrease, green/blue = increase
lim <- max(abs(delta$diff_pct), na.rm = TRUE); if (!is.finite(lim)) lim <- 1

# symmetric color limits; red = decrease, green = increase
lim <- max(abs(delta$diff_pct), na.rm = TRUE); if (!is.finite(lim)) lim <- 1

p_heat <- ggplot(delta, aes(x = barrier_id, y = base_family, fill = diff_pct)) +
  geom_tile(color = "white") +
  {if (annotate_heat) geom_text(aes(label = sprintf("%+.*f", annot_dec, diff_pct)), size = 3)} +
  facet_wrap(~ condition, ncol = 1) +               # vertical facets (stacked)
  scale_fill_gradient2(
    low = col_low, mid = col_mid, high = col_high,
    midpoint = 0, limits = c(-lim, lim),
    name = paste0("Δ pp vs ", baseline)
  ) +
  labs(x = "Barrier code", y = NULL) +              # no panel title
  theme_minimal(base_size = 12) +
  theme(
    # --- make each facet SHORT ---
    aspect.ratio = 0.12,                            # ↓ try 0.10–0.15 for tighter/looser
    # tidy strips so they don’t eat space
    strip.placement = "outside",
    strip.text = element_text(size = 8, margin = margin(t = 1, r = 2, b = 1, l = 2)),
    strip.background = element_rect(fill = NA, colour = NA),
    # legend carries the title; keep it compact
    legend.position = legend_pos,                   # "right" or "bottom"
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.margin = margin(5, 5, 5, 0)
  )
p_heat <- ggplot(delta, aes(x = barrier_id, y = base_family, fill = diff_pct)) +
  geom_tile(color = "white", height = 0.6)  # 0<height≤1; smaller = shorter tiles
p_heat <- p_heat +
  theme(aspect.ratio = 0.12)
# COMBINE & SAVE
# n_facets <- nlevels(delta$condition)
# fig_height <- base_height + max(0, n_facets - 1) * height_per_facet

# new: keep the left bars tall; we’ll compress the heatmap internally
fig_height <- 7  # adjust to taste; bars stay readable


combo <- p_grouped | p_heat + plot_layout(widths = c(left_width, right_width))
print(combo)
ggsave(outfile, combo, width = 18, height = fig_height, dpi = 300)
