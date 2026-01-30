# install.packages(c("tidyverse","patchwork"))  # run once

library(tidyverse)
library(patchwork)

# =======================
# OPTIONS — tweak freely
# =======================
csv_path <- "merged_barriers_zero_context.csv"
pair_conds <- c("ZEROSHOT","NEU")     # which two to compare in the diff panel
fam_rename <- c(phi4="Phi4", llama="LLaMa-Pro", mistral="Mistral", gemma3="Gemma3:12B")
cond_order <- c("ZEROSHOT","NEU") #add also for "BIASED","INVERSE_ORDER" later
fam_order  <- c("phi4","llama","mistral","gemma3")
decimals   <- 1                       # annotation decimals
show_ci    <- TRUE                    # error bars on bars & diff
palette    <- c("#5E60CE","#4EA699","#F3722C","#577590")  # 4 families

# =======================
# Load & prepare
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

# what's present (order with your preferences on top)
conditions <- c(cond_order[cond_order %in% unique(df$condition)],
                setdiff(unique(df$condition), cond_order))
families   <- c(fam_order[fam_order %in% unique(df$base_family)],
                setdiff(unique(df$base_family), fam_order))
barriers   <- sort(unique(df$barrier_id))

# =======================
# Selection-rate per iteration, then mean% + CI
# =======================
counts <- df |>
  count(base_family, condition, iteration, barrier_id, name = "n_sel")
totals <- df |>
  count(base_family, condition, iteration, name = "total_sel")

# full grid so unselected barriers register as 0
grid <- tidyr::crossing(
  totals |> distinct(base_family, condition, iteration),
  tibble(barrier_id = barriers)
)

rates <- grid |>
  left_join(counts, by = c("base_family","condition","iteration","barrier_id")) |>
  left_join(totals, by = c("base_family","condition","iteration")) |>
  mutate(n_sel = replace_na(n_sel, 0L),
         selection_rate = n_sel / total_sel)

summ <- rates |>
  group_by(base_family, condition, barrier_id) |>
  summarise(
    mean_pct = 100*mean(selection_rate, na.rm=TRUE),
    sd_rate  = sd(selection_rate, na.rm=TRUE),  # SD on [0,1]
    n_iter   = n_distinct(iteration),
    .groups="drop"
  ) |>
  mutate(
    ci95_pct = ifelse(n_iter > 0, 100*1.96*(sd_rate/sqrt(n_iter)), NA_real_),
    base_family = factor(base_family, levels = families),
    condition   = factor(condition,   levels = conditions),
    barrier_id  = factor(barrier_id,  levels = barriers)
  )

# =======================
# (1) GROUPED BAR CHART — grouped by CONDITION
# =======================
pd <- position_dodge(width = 0.8)

p_grouped <- ggplot(summ, aes(x = barrier_id, y = mean_pct, fill = base_family)) +
  geom_col(position = pd, width = 0.75) +
  {if (show_ci) geom_errorbar(aes(ymin = mean_pct - ci95_pct,
                                  ymax = mean_pct + ci95_pct),
                              position = pd, width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf(paste0("%.", decimals, "f"), mean_pct)),
            position = position_dodge(width = 0.8),
            vjust = -0.25, size = 3) +
  facet_wrap(~ condition, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = setNames(palette, levels(summ$base_family)),
                    labels = function(l) unname(fam_rename[l])) +
  labs(x = "Barrier code", y = "Mean selection rate (%)", fill = "Base model",
       title = "Grouped bars by condition") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "top")

# =======================
# (2) DIFF PANEL — Δ% = zeroshot − neu, by base model × barrier
# =======================
if (!all(pair_conds %in% levels(summ$condition))) {
  stop(sprintf("pair_conds not found: %s",
               paste(setdiff(pair_conds, levels(summ$condition)), collapse=", ")))
}

diff_df <- summ |>
  filter(condition %in% pair_conds) |>
  select(base_family, barrier_id, condition, mean_pct, sd_rate, n_iter) |>
  pivot_wider(names_from = condition,
              values_from = c(mean_pct, sd_rate, n_iter),
              names_sep = "__") |>
  mutate(
    diff_pct = .data[[sprintf("mean_pct__%s", pair_conds[1])]] -
               .data[[sprintf("mean_pct__%s", pair_conds[2])]],
    se = sqrt( (.data[[sprintf("sd_rate__%s", pair_conds[1])]]^2) /
                 pmax(1, .data[[sprintf("n_iter__%s", pair_conds[1])]]) +
               (.data[[sprintf("sd_rate__%s", pair_conds[2])]]^2) /
                 pmax(1, .data[[sprintf("n_iter__%s", pair_conds[2])]]) ),
    ci95 = 100*1.96*se,
    sign = if_else(diff_pct >= 0, "increase", "decrease")
  ) |>
  mutate(
    base_family = factor(base_family, levels = families),
    barrier_id  = factor(barrier_id,  levels = barriers)
  )

pd2 <- position_dodge(width = 0.75)

p_diff <- ggplot(diff_df, aes(x = barrier_id, y = diff_pct, fill = base_family)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(position = pd2, width = 0.7, alpha = 0.95) +
  {if (show_ci) geom_errorbar(aes(ymin = diff_pct - ci95,
                                  ymax = diff_pct + ci95),
                              position = pd2, width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf("%+.1f", diff_pct)),
            position = position_dodge(width = 0.75),
            vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(palette, levels(diff_df$base_family)),
                    labels = function(l) unname(fam_rename[l])) +
  labs(x = "Barrier", y = paste0("Δ mean % (", pair_conds[1], " − ", pair_conds[2], ")"),
       title = "Shift") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# =======================
# COMBINE side-by-side & SAVE
# =======================
combo <- p_grouped | p_diff + plot_layout(widths = c(3, 1.5))
print(combo)

ggsave("bars_by_condition_plus_diff.png", combo, width = 16, height = 6, dpi = 300)

# Also drop out the summary tables if you want to inspect values:
# write_csv(summ,    "summ_by_family_condition_barrier.csv")
# write_csv(diff_df, "diff_zeroshot_minus_neu_by_family_barrier.csv")
