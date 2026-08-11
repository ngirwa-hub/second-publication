# --- libs
library(tidyverse)
library(scales)

library(conflicted)
library(forcats)
library(purrr)

# Prefer dplyr over stats/ordinal for these:
conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")
conflict_prefer("slice",  "dplyr")   # in case you ever use slice()

# Prefer purrr over scales for discard()
conflict_prefer("discard", "purrr")

# Prefer readr over scales for col_factor() (used in readr col_types)
conflict_prefer("col_factor", "readr")

# ================= USER CONTROLS =================
x_lab    <- "Probability"     # customize x-axis label
y_lab    <- "Model"        # customize y-axis label
title_str<- "Feasibility - Anchor vs Context (Probability \u2265 3)"
outfile  <- "rq3_feas_dumbbells.png"
x_start  <- 0.972          # x-axis start (for zooming in)
x_end    <- 0.999          # x-axis end (for zooming in)
tick_by  <- 0.0045          # x-axis tick interval
tol <- 1e-6
# nice display names
base_label_map <- c(
  "phi4" = "Phi4",
  "gemma3-12b" = "Gemma3:12B",
  "llama-pro" = "LLaMA-Pro",
  "mistral" = "Mistral"
)

anchor_label_map <- c(
  "ANCHOR_WORD"      = "Anchor: Word",
  "ANCHOR_EXAMPLE"   = "Anchor: Example",
  "ANCHOR_NUM_LOW"   = "Anchor: Low numeric",
  "ANCHOR_NUM_HIGH"  = "Anchor: High numeric"
)

# point colors + shapes (keeps your aesthetic spirit)
pal   <- c("Context" = "#1b9e77", "Anchor" = "#d95f02")
shape <- c("Context" = 16, "Anchor" = 17)

# ================= DATA =================
df <- tribble(
  ~base_model, ~scenario,         ~p_ge3,
  "gemma3-12b","CONTEXT",         0.973017762,
  "gemma3-12b","ANCHOR_WORD",     0.973017762,
  "gemma3-12b","ANCHOR_EXAMPLE",  0.973017762,
  "gemma3-12b","ANCHOR_NUM_LOW",  0.973017762,
  "gemma3-12b","ANCHOR_NUM_HIGH", 0.973017762,
  "llama-pro", "CONTEXT",         0.995545981,
  "llama-pro", "ANCHOR_WORD",     0.998801524,
  "llama-pro", "ANCHOR_EXAMPLE",  0.998429261,
  "llama-pro", "ANCHOR_NUM_LOW",  0.998094093,
  "llama-pro", "ANCHOR_NUM_HIGH", 0.998914664,
  "mistral",   "CONTEXT",         0.973017762,
  "mistral",   "ANCHOR_WORD",     0.980007798,
  "mistral",   "ANCHOR_EXAMPLE",  0.973017762,
  "mistral",   "ANCHOR_NUM_LOW",  0.973017762,
  "mistral",   "ANCHOR_NUM_HIGH", 0.973017762,
  "phi4",      "CONTEXT",         0.973017762,
  "phi4",      "ANCHOR_WORD",     0.973017762,
  "phi4",      "ANCHOR_EXAMPLE",  0.973017762,
  "phi4",      "ANCHOR_NUM_LOW",  0.973017762,
  "phi4",      "ANCHOR_NUM_HIGH", 0.973017762
)

# anchors we want (facet order)
anchor_levels <- c("ANCHOR_WORD","ANCHOR_EXAMPLE","ANCHOR_NUM_LOW","ANCHOR_NUM_HIGH")

# ================= PREP =================
# split Context vs Anchor, compute delta = Anchor - Context per base model & anchor
ctx <- df %>% filter(scenario == "CONTEXT") %>%
  select(base_model, p_ctx = p_ge3)

wide <- df %>%
  filter(scenario %in% anchor_levels) %>%
  left_join(ctx, by = "base_model") %>%
  transmute(
    base_model,
    anchor_kind = factor(scenario, levels = anchor_levels, labels = anchor_label_map[anchor_levels]),
    p_ctx,
    p_anchor = p_ge3,
    delta = p_anchor - p_ctx,
    mid   = (p_anchor + p_ctx)/2
  ) %>%
  # reorder base models *within each facet* by delta
  group_by(anchor_kind) %>%
  mutate(base_model_disp = recode(base_model, !!!base_label_map),
         base_model_ord  = fct_reorder(base_model_disp, delta)) %>%
  ungroup()

# long for points
long <- wide %>%
  pivot_longer(c(p_ctx, p_anchor), names_to = "which", values_to = "prob") %>%
  mutate(which = recode(which, p_ctx = "Context", p_anchor = "Anchor"))

# annotate: base model with highest positive shift per facet
tol <- 1e-6
fallback_zeros <- FALSE  # TRUE to annotate 0 when no positive shift in a facet

annot <- wide %>%
  dplyr::group_by(anchor_kind) %>%
  {
    pos <- dplyr::filter(., delta > tol) %>%
      dplyr::arrange(dplyr::desc(delta), dplyr::desc(p_anchor), dplyr::desc(p_ctx), base_model)
    if (nrow(pos) > 0) {
      dplyr::slice(pos, 1L)
    } else if (fallback_zeros) {
      dplyr::arrange(., dplyr::desc(delta), dplyr::desc(p_anchor), dplyr::desc(p_ctx), base_model) %>%
        dplyr::slice(1L)
    } else {
      dplyr::slice(., 0)
    }
  } %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    x_annot = mid,
    label_txt = sprintf("%.3f", delta)
    #x_annot   = pmin(pmax(p_ctx, p_anchor) + 0.002, x_end - 0.001)  # keep inside axis
  )

# ================= PLOT =================
# ================= PLOT =================
p <- ggplot() +
  # segments (dumbbell lines)
  geom_segment(
    data = wide,
    aes(y = base_model_ord, yend = base_model_ord, x = p_ctx, xend = p_anchor),
    linewidth = 2, color = "grey75"
  ) +
  # points
  geom_point(
    data = long,
    aes(x = prob, y = base_model_ord, shape = which, color = which),
    size = 3
  ) +
  scale_color_manual(values = pal, name = NULL) +
  scale_shape_manual(values = shape, name = NULL) +
  # delta labels at the segment midpoint
  geom_text(
    data = annot,
    aes(x = x_annot, y = base_model_ord, label = label_txt),
    hjust = 0.5, vjust = -1.0, size = 3.2, face = "plain", color = "#444444"
  ) +
  # x scale (if you want to drop outside points, keep limits; otherwise remove them)
  scale_x_continuous(
    limits = c(x_start, x_end),                       # remove this line if you prefer zoom-only
    breaks = seq(x_start, x_end, by = tick_by),
    labels = function(x) sprintf("%.3f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(title = title_str, x = x_lab, y = y_lab, face = "plain", family = "") +
  facet_wrap(~ anchor_kind, ncol = 2, scales = "fixed") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "plain"),
    strip.text = element_text(face = "plain"),
    legend.position = "top"
  )

# zoom without dropping data (optional — if you keep limits above, you can omit this)
p <- p + coord_cartesian(xlim = c(x_start, x_end), clip = "off")

print(p)
ggsave(outfile, p, width = 10, height = 7, dpi = 300)
