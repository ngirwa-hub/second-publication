# Heatmap: scenarios (rows) × ratings 2–4 (cols) grouped by base_model (strips on top)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(forcats)
  library(ggplot2); library(readr); library(scales)
})

# -------------------- CONFIG (customize here) --------------------
indir        <- "feas_out"
in_rds       <- file.path(indir, "feas_clean.rds")
outdir       <- file.path(indir, "clmm_outputs")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ratings to display on the x-axis (you asked for 2,3,4)
ratings_plot <- 1:4

# scenario ordering and pretty labels (edit as you like)
scenario_order <- c("CONTEXT","ANCHOR_WORD","ANCHOR_EXAMPLE","ANCHOR_NUM_LOW","ANCHOR_NUM_HIGH")
scenario_labels <- c(
  CONTEXT         = "Context",
  ANCHOR_WORD     = "Anchor: Word",
  ANCHOR_EXAMPLE  = "Anchor: Example",
  ANCHOR_NUM_LOW  = "Anchor: Num Low",
  ANCHOR_NUM_HIGH = "Anchor: Num High"
)

# base model ordering and pretty labels (edit as you like)
# (If you don't set base_order, it will use the order in the data)
base_order <- NULL   # e.g., c("gemma3-12b","llama-pro","mistral","phi4")
base_labels <- c(
  "gemma3-12b" = "Gemma3:12B",
  "llama-pro"  = "LLaMA-Pro",
  "mistral"    = "Mistral",
  "phi4"       = "Phi-4"
)
# ----------------------------------------------------------------

# load
feas <- readRDS(in_rds)

# ensure columns/types
if (!"base_model" %in% names(feas)) feas$base_model <- "ALL"
feas$base_model <- as.character(feas$base_model)
feas$rating     <- as.integer(feas$rating)
feas$scenario   <- as.character(feas$scenario)

# enforce scenario order
feas$scenario <- fct_relevel(feas$scenario, scenario_order)

# optional base_model order
if (!is.null(base_order)) {
  feas$base_model <- factor(feas$base_model, levels = base_order)
} else {
  feas$base_model <- factor(feas$base_model)
}

# -------- build the distribution (normalize within base_model × scenario over ratings 2–4) --------
df_heat <- feas %>%
  filter(scenario %in% scenario_order, rating %in% ratings_plot) %>%
  count(base_model, scenario, rating, name = "n") %>%
  # include 0% tiles so no rating (2/3/4) disappears for a given base_model × scenario
  complete(base_model, scenario, rating = ratings_plot, fill = list(n = 0)) %>%
  group_by(base_model, scenario) %>%
  mutate(denom_24 = sum(n, na.rm = TRUE),
         prop     = ifelse(denom_24 > 0, n / denom_24, 0)) %>%
  ungroup() %>%
  mutate(
    scenario_f = fct_relevel(as.character(scenario), scenario_order),
    rating_f   = factor(rating, levels = ratings_plot),
    prop_lab   = percent(prop, accuracy = 1)
  )

# save table used for the heatmap (handy to inspect)
write_csv(df_heat %>% select(base_model, scenario = scenario_f, rating = rating_f, n, prop),
          file.path(outdir, "heatmap_scenario_x_rating_by_base_model.csv"))

# facet labeller for base models (pretty names where provided)
bm_labeller <- if (length(base_labels)) {
  labeller(base_model = base_labels)
} else {
  label_value
}

# ------------------------------ plot ------------------------------
p_heat <- ggplot(df_heat, aes(x = rating_f, y = scenario_f, fill = prop)) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = prop_lab), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue", labels = percent) +
  scale_y_discrete(labels = scenario_labels) +   # customize scenario labels
  labs(
    x = "Rating", y = NULL,
    fill = "Share",
    title = "Rating distribution by base model and scenario"
  ) +
  facet_grid(. ~ base_model, switch = "x", scales = "free_x", space = "free_x", labeller = bm_labeller) +
  theme_minimal() +
  theme(
    strip.placement = "outside",           # strips (base model names) above tiles
    strip.background = element_rect(fill = NA, color = NA),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    plot.title   = element_text(hjust = 0.5),
    panel.grid   = element_blank()
  )

ggsave(file.path(outdir, "3D_feas_distr.png"),
       p_heat, width = 10, height = 6, dpi = 300, bg = "white")

message("✅ Wrote: ", file.path(outdir, "heatmap_scenario_x_rating_by_base_model.csv"))
message("✅ Wrote: ", file.path(outdir, "3D_feas_distr.png"))
