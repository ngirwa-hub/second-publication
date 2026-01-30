# ================================
# 23_emd_plots_annotated.R
# Annotated multi-panel plots for EMD results
# Requires: the CSVs from 01_compute_emd.R
#   - clean/feas_out/human_vs_llm_emd_by_model.csv
#   - clean/feas_out/human_vs_llm_emd_overall.csv
# Produces:
#   - clean/feas_out/emd_annotated_panel.png
#   - clean/feas_out/emd_annotated_panel.pdf
# ================================

suppressPackageStartupMessages({
  need <- c("dplyr","readr","ggplot2","ggrepel","patchwork")
  to_install <- need[!sapply(need, requireNamespace, quietly = TRUE)]
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  lapply(need, library, character.only = TRUE)
})

OUTDIR <- "clean/feas_out"
by_model_csv <- file.path(OUTDIR, "human_vs_llm_emd_by_model.csv")
overall_csv  <- file.path(OUTDIR, "human_vs_llm_emd_overall.csv")

by_model <- readr::read_csv(by_model_csv, show_col_types = FALSE)
overall  <- readr::read_csv(overall_csv,  show_col_types = FALSE)

# Ensure ordering of conditions
by_model <- by_model %>%
  mutate(cond2 = factor(cond2, levels = c("NEU","BIASED")))
overall  <- overall  %>%
  mutate(cond2 = factor(cond2, levels = c("NEU","BIASED")))

# Identify which condition is closer (smaller EMD) per base model
closer_tbl <- by_model %>%
  group_by(base_model) %>%
  slice_min(order_by = emd, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(base_model, closest_condition = cond2)

by_model <- by_model %>%
  left_join(closer_tbl, by = "base_model") %>%
  mutate(is_closest = cond2 == closest_condition)

# ---- Pretty base-model display names ----
bm_map <- c(
  "phi4"        = "Phi4",
  "llama-pro"   = "LLaMa-Pro",
  "mistral"     = "Mistral",
  "gemma3-12b"  = "Gemma3:12B"
)

by_model <- by_model %>%
  mutate(base_model = recode(tolower(base_model), !!!bm_map))

# keep a stable facet order (optional)
by_model <- by_model %>%
  mutate(base_model = factor(base_model,
    levels = c("Phi4","LLaMa-Pro","Mistral","Gemma3:12B")
  ))


# ---------- Plot A: Faceted EMD per base model (NEU vs BIASED), annotated ----------
pA <- ggplot(by_model, aes(x = cond2, y = emd, group = base_model)) +
  geom_line(aes(group = 1), linewidth = 0.5, color = "grey50") +
  geom_point(aes(fill = cond2, size = is_closest), shape = 21, stroke = 0.4) +
  scale_size_manual(values = c(`FALSE` = 2.2, `TRUE` = 3.2), guide = "none") +
  geom_text(aes(label = sprintf("%.3f", emd)), vjust = -0.6, size = 3) +
  facet_wrap(~ base_model, ncol = 2, scales = "free_y") +
  labs(
  title = "EMD vs Human by Base Model",
  x = "LLM Condition", y = "EMD (Wasserstein-1)"
) +
theme_minimal(base_size = 11) +
theme(
  panel.grid.minor = element_blank(),
  legend.position = "bottom",
  plot.margin = margin(10, 40, 10, 10),
  plot.title = element_text(hjust = 0.5)   # <-- centers the title
) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.28))) +
coord_cartesian(clip = "off")



# ---------- Plot B: Overall (pooled LLM) ----------
pB <- ggplot(overall, aes(x = cond2, y = emd, group = 1)) +
  geom_line(color = "grey50") +
  geom_point(aes(fill = cond2), shape = 21, size = 3, stroke = 0.4) +
  geom_text(aes(label = sprintf("%.3f", emd)),
            vjust = -0.6, size = 3) +
  labs(title = "Overall EMD - Pooled (LLM)",
       x = "LLM Condition", y = "EMD (Wasserstein-1)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

# ---------- Plot C: Delta (BIASED − NEU) per base model ----------
# Negative = BIASED closer; Positive = NEU closer
delta_tbl <- by_model %>%
  select(base_model, cond2, emd) %>%
  tidyr::pivot_wider(names_from = cond2, values_from = emd) %>%
  mutate(delta = BIASED - NEU) %>%
  arrange(desc(delta))

# delta_tbl already built as:
# delta_tbl <- by_model |>
#   select(base_model, cond2, emd) |>
#   tidyr::pivot_wider(names_from = cond2, values_from = emd) |>
#   mutate(delta = BIASED - NEU) |>
#   arrange(desc(delta))

# Prepare label helpers (center text & pick readable color)
delta_plot <- delta_tbl |>
  dplyr::mutate(
    lab      = sprintf("%+.3f", delta),
    y_lab    = delta/2,                               # center of each bar (after coord_flip)
    col_lab  = ifelse(abs(delta) >= 0.06, "white", "black")  # white on longer bars
  )

pC <- ggplot(delta_plot, aes(x = reorder(base_model, delta), y = delta)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey50") +
  geom_col(aes(fill = delta > 0), width = 0.55) +    # control thickness here
  geom_text(aes(y = y_lab, label = lab, color = col_lab),
            size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_identity() +                            # use colors as provided
  coord_flip(clip = "off") +                          # no clipping, but we won't need big margins
  scale_fill_manual(values = c("TRUE" = "#bbbbff", "FALSE" = "#ffbbbb"),
                    labels = c("FALSE" = "BIASED closer", "TRUE" = "NEU closer"),
                    guide = "none") +
  labs(title = "Delta EMD per Base Model (BIASED − NEU)",
       subtitle = "Negative = BIASED closer to human; Positive = NEU closer",
       x = NULL, y = "Δ EMD (BIASED − NEU)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)            # center title
  )

# ---------- Combine as subplots with patchwork ----------
# Layout: A on top; B and C side-by-side on bottom
panel <- pA / (pB | pC) +
  plot_annotation(
    title = "Closeness of LLM to Human (NEU) using Wasserstein/EMD",
    subtitle = "Panels: A) per base model • B) overall pooled • C) ΔEMD (BIASED − NEU)",
    theme = theme(plot.title = element_text(face = "bold"))
  )

# ---------- Save ----------
png_file <- file.path(OUTDIR, "emd_annotated_panel.png")
pdf_file <- file.path(OUTDIR, "emd_annotated_panel.pdf")
ggplot2::ggsave(png_file, panel, width = 10, height = 9, dpi = 200)
ggplot2::ggsave(pdf_file, panel, width = 10, height = 9)

message("✅ Saved plots:\n  - ", png_file, "\n  - ", pdf_file)
