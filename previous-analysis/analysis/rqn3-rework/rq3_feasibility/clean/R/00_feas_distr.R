# Heatmap: rating distribution per base_model in pooled (anchor vs context)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(forcats); library(readr); library(stringr)
})

# --- CONFIG you can customize ---
indir        <- "feas_out"
in_rds       <- file.path(indir, "feas_clean.rds")
outdir       <- file.path(indir, "clmm_outputs")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

ratings_show <- 1:4                     # show ratings 1..4 on Y
base_order   <- NULL                    # e.g., c("gemma3","phi4","llama") or leave NULL to autodetect
base_labels  <- NULL                    # e.g., c(gemma3 = "Gemma 3", phi4 = "Phi-4")
pool_labels  <- c(context = "Context", anchor = "Anchor")
x_sep        <- "-"                     # separator in x axis labels (e.g., "gemma3-context")

# --- Load data ---
feas <- readRDS(in_rds)

# Fallback if base_model missing
if (!"base_model" %in% names(feas)) feas$base_model <- "ALL"

# Ensure numeric rating
feas$rating <- as.integer(feas$rating)

# Pool scenarios: CONTEXT vs ANCHOR (everything not CONTEXT)
feas <- feas %>%
  mutate(
    pool = if_else(as.character(scenario) == "CONTEXT", "context", "anchor"),
    base_model = as.character(base_model)
  )

# Optional reordering & relabeling of base_model
if (!is.null(base_order)) {
  feas$base_model <- factor(feas$base_model, levels = base_order)
} else {
  feas$base_model <- factor(feas$base_model)
}
if (!is.null(base_labels)) {
  # carry a display name without changing the factor levels used for grouping
  feas$base_model_disp <- recode(as.character(feas$base_model), !!!base_labels)
} else {
  feas$base_model_disp <- as.character(feas$base_model)
}

# Build x axis like "gemma3-context" / "gemma3-anchor"
feas <- feas %>%
  mutate(x = paste0(base_model, x_sep, pool),
         x_disp = paste0(base_model_disp, x_sep, pool_labels[pool]))

# Compute % distribution over ratings 1..4 *within each x*
df_heat <- feas %>%
  filter(rating %in% ratings_show) %>%
  count(x, x_disp, rating, name = "n") %>%
  group_by(x, x_disp) %>%
  complete(rating = ratings_show, fill = list(n = 0)) %>%
  mutate(total = sum(n),
         prop  = ifelse(total > 0, n / total, 0)) %>%
  ungroup() %>%
  mutate(
    rating_f = factor(rating, levels = ratings_show),                 # y axis
    prop_lab = scales::percent(prop, accuracy = 1)                    # annotation text
  )

# Save the table used for plotting
write_csv(df_heat %>% select(x_disp, rating, prop), file.path(outdir, "heatmap_props_anchor_context.csv"))

# Order x by base_model then pool (context first)
# (If you prefer anchor first, swap the levels below)
pool_level <- c("context", "anchor")
x_levels <- df_heat %>%
  distinct(x, x_disp) %>%
  arrange(match(sub(paste0(".*", x_sep), "", .$x), pool_level)) %>% # sort by pool
  pull(x_disp)
df_heat$x_disp <- factor(df_heat$x_disp, levels = unique(x_levels))

# Plot
p_heat <- ggplot(df_heat, aes(x = x_disp, y = rating_f, fill = prop)) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = prop_lab), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue", labels = scales::percent) +
  labs(
    x = NULL, y = "Rating",
    fill = "Share",
    title = "Rating distribution by base model (Context vs pooled Anchor)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(hjust = 0.5),
    panel.grid   = element_blank()
  )

ggsave(file.path(outdir, "heatmap_anchor_context.png"),
       p_heat, width = 10, height = 6, dpi = 300)

message("✅ Wrote: ", file.path(outdir, "heatmap_anchor_context.png"))
message("✅ Wrote: ", file.path(outdir, "heatmap_props_anchor_context.csv"))
