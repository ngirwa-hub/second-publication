# install.packages(c("tidyverse","patchwork"))  # run once
library(tidyverse)
library(patchwork)

# ========= OPTIONS =========
csv_path   <- "merged_barriers_zero_context.csv"
baseline   <- "zeroshot"                                     # baseline condition
palette    <- c("#5E60CE","#4EA699","#F3722C","#577590")# 4 families
fam_rename <- c(phi4="Phi-4", llama="LLaMA", mistral="Mistral", gemma3="Gemma-3")
cond_order <- c("zeroshot","neu","anchor","inverse order","inverse_order","inverse")
fam_order  <- c("phi4","llama","mistral","gemma3")
decimals   <- 1
show_ci    <- TRUE
# ==========================

# --- Load & prep ---
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

# --- Per-iteration selection rates ---
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

# ---------------- (1) GROUPED BARS by CONDITION ----------------
pd <- position_dodge(width = 0.8)
p_grouped <- ggplot(summ, aes(x = barrier_id, y = mean_pct, fill = base_family)) +
  geom_col(position = pd, width = 0.75) +
  {if (show_ci) geom_errorbar(aes(ymin = mean_pct - ci95_pct, ymax = mean_pct + ci95_pct),
                              position = pd, width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf(paste0("%.", decimals, "f"), mean_pct)),
            position = position_dodge(width = 0.8), vjust = -0.25, size = 3) +
  facet_wrap(~ condition, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = setNames(palette, levels(summ$base_family)),
                    labels = function(l) unname(fam_rename[l])) +
  labs(x = "Barrier code", y = "Mean selection rate (%)", fill = "Base model",
       title = "Grouped bars by condition") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "top")

# ---------------- (2) MULTI-DIFF vs BASELINE ----------------
stopifnot(baseline %in% levels(summ$condition))

base_tbl <- summ |> filter(condition == baseline) |>
  transmute(base_family, barrier_id, mean_base = mean_pct,
            sd_base = sd_rate, n_base = n_iter)

diff_multi <- summ |>
  filter(condition != baseline) |>
  left_join(base_tbl, by = c("base_family","barrier_id")) |>
  mutate(
    diff_pct = mean_pct - mean_base,                         # (cond - baseline)
    se = sqrt( (sd_rate^2)/pmax(1, n_iter) + (sd_base^2)/pmax(1, n_base) ),
    ci95 = 100*1.96*se,                                      # convert to percentage scale
    condition = droplevels(condition),
    base_family = factor(base_family, levels = families),
    barrier_id  = factor(barrier_id,  levels = barriers)
  )

pd2 <- position_dodge(width = 0.75)
p_diff_multi <- ggplot(diff_multi, aes(x = barrier_id, y = diff_pct, fill = base_family)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(position = pd2, width = 0.7, alpha = 0.95) +
  {if (show_ci) geom_errorbar(aes(ymin = diff_pct - ci95, ymax = diff_pct + ci95),
                              position = pd2, width = 0.25, alpha = 0.7)} +
  geom_text(aes(label = sprintf("%+.1f", diff_pct)),
            position = position_dodge(width = 0.75), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(palette, levels(diff_multi$base_family)),
                    labels = function(l) unname(fam_rename[l])) +
  labs(x = "Barrier", y = paste0("Δ mean % (condition − ", baseline, ")"),
       title = "Shift vs baseline") +
  facet_wrap(~ condition, ncol = 1, scales = "free_y") +  # one facet per NON-baseline condition
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# ---------------- Combine & save ----------------
combo <- p_grouped | p_diff_multi + plot_layout(widths = c(3, 1.6))
print(combo)
ggsave("bars_by_condition_plus_multidiff.png", combo, width = 18, height = 8, dpi = 300)
