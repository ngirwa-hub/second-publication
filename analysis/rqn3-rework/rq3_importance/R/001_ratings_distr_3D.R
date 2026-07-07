# --- Heatmap: scenarios (rows) × ratings (cols) grouped by base_model (strips) ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(lubridate)
})

# -------------------- CONFIG (customize here) --------------------
context_csv <- "../importance_neutral_1256hrs.csv"      # neutral
anchor_csv  <- "../importance_biased_llm1254hrs.csv"    # biased w/ bias_type
out_dir     <- "imp_multiarm_outputs"
plots_dir   <- file.path(out_dir, "plots_trial")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# ratings shown on x-axis (change to 0:4, 1:4, 2:4, etc.)
ratings_plot <- 2:4

# scenario ordering and pretty labels
scenario_order <- c("CONTEXT","ANCHOR_WORD","ANCHOR_EXAMPLE","ANCHOR_LOW_NUM","ANCHOR_HIGH_NUM")
scenario_labels <- c(
  CONTEXT         = "Context",
  ANCHOR_WORD     = "Anchor: Word",
  ANCHOR_EXAMPLE  = "Anchor: Example",
  ANCHOR_LOW_NUM  = "Anchor: Num Low",
  ANCHOR_HIGH_NUM = "Anchor: Num High"
)

# facet order and labels for base models
base_order <- c("gemma3-12b","llama-pro","mistral","phi4")
base_labels <- c(
  "gemma3-12b" = "Gemma3:12B",
  "llama-pro"  = "LLaMA-Pro",
  "mistral"    = "Mistral",
  "phi4"       = "Phi-4"
)
# ----------------------------------------------------------------

# --- Read & normalize: CONTEXT (neutral) ---
ctx <- readr::read_csv(context_csv, show_col_types = FALSE) %>%
  mutate(
    condition_raw = toupper(trimws(condition)),
    condition5    = "CONTEXT",
    rating        = suppressWarnings(as.integer(rating)),
    iteration     = suppressWarnings(as.integer(iteration)),
    timestamp     = suppressWarnings(lubridate::ymd_hms(timestamp, quiet = TRUE))
  ) %>%
  select(row_id, base_model, variant_id, model, dc_solution, rating, label,
         iteration, timestamp, source, condition_raw, condition5) %>%
  filter(!is.na(rating), rating >= min(ratings_plot), rating <= max(ratings_plot))

# --- Read & normalize: ANCHOR (biased; keep arms separate) ---
anc <- readr::read_csv(anchor_csv, show_col_types = FALSE) %>%
  mutate(
    condition_raw = toupper(trimws(condition)),
    bias_type     = toupper(trimws(bias_type)),
    condition5    = dplyr::case_when(
      bias_type == "BIAS_WORD"     ~ "ANCHOR_WORD",
      bias_type == "BIAS_EXAMPLE"  ~ "ANCHOR_EXAMPLE",
      bias_type == "BIAS_NUM_LOW"  ~ "ANCHOR_LOW_NUM",
      bias_type == "BIAS_NUM_HIGH" ~ "ANCHOR_HIGH_NUM",
      TRUE ~ NA_character_
    ),
    rating        = suppressWarnings(as.integer(rating)),
    iteration     = suppressWarnings(as.integer(iteration)),
    timestamp     = suppressWarnings(lubridate::ymd_hms(timestamp, quiet = TRUE))
  ) %>%
  select(row_id, base_model, variant_id, model, dc_solution, rating, label,
         iteration, timestamp, source, condition_raw,
         dplyr::any_of(c("bias_type","anchor_level")), condition5) %>%
  filter(!is.na(rating), rating >= min(ratings_plot), rating <= max(ratings_plot),
         !is.na(condition5))

# --- Combine, canonicalize base_model, keep only desired bases ---
canon_map <- c(
  "gemma3-12b" = "gemma3-12b", "gemma3" = "gemma3-12b",
  "llama-pro"  = "llama-pro",  "llama"  = "llama-pro",
  "mistral"    = "mistral",    "Mistral"= "mistral",
  "phi4"       = "phi4",       "Phi4"   = "phi4"
)

df <- bind_rows(ctx, anc) %>%
  mutate(
    base_id  = dplyr::recode(base_model, !!!canon_map, .default = base_model),
    rating   = factor(rating, levels = ratings_plot, ordered = TRUE),
    scenario = factor(condition5, levels = scenario_order)   # keep arms separate
  ) %>%
  filter(base_id %in% base_order)

# --- De-dupe (latest per base × variant × solution × scenario × iteration) ---
df <- df %>%
  arrange(timestamp) %>%
  group_by(base_id, variant_id, dc_solution, scenario, iteration) %>%
  slice_tail(n = 1) %>%
  ungroup()

stopifnot(is.ordered(df$rating))

# --- Build heatmap table: normalize within base_id × scenario over ratings_plot ---
df_heat <- df %>%
  count(base_id, scenario, rating, name = "n") %>%
  tidyr::complete(
    base_id = factor(base_order, levels = base_order),
    scenario = factor(scenario_order, levels = scenario_order),
    rating = factor(ratings_plot, levels = ratings_plot, ordered = TRUE),
    fill = list(n = 0)
  ) %>%
  group_by(base_id, scenario) %>%
  mutate(den = sum(n), prop = ifelse(den > 0, n/den, 0)) %>%
  ungroup() %>%
  mutate(
    rating_f   = factor(as.character(rating), levels = as.character(ratings_plot), ordered = TRUE),
    scenario_f = factor(as.character(scenario), levels = scenario_order),
    prop_lab   = scales::percent(prop, accuracy = 1)
  )

# --- Save the table used for the figure (optional) ---
readr::write_csv(
  df_heat %>% select(base_model = base_id, scenario = scenario_f, rating = rating_f, n, prop),
  file.path(out_dir, "heatmap_scenario_x_rating_by_base_model_FROM_CODE2.csv")
)

# --- Plot (match code 1 aesthetics) ---
bm_labeller <- labeller(base_id = base_labels)

p_heat <- ggplot(df_heat, aes(x = rating_f, y = scenario_f, fill = prop)) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = prop_lab), size = 3) +
  scale_fill_gradient(
    low = "white", high = "steelblue",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), na.value = "grey90"
  ) +
  scale_y_discrete(labels = scenario_labels) +
  labs(
    x = "Rating", y = NULL, fill = "Share",
    title = "Importance - rating distribution by model and scenario"
  ) +
  facet_grid(. ~ base_id, switch = "x", scales = "free_x", space = "free_x",
             labeller = bm_labeller) +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = NA),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title  = element_text(hjust = 0.5),
    panel.grid  = element_blank()
  )

ggsave(file.path(plots_dir, "rq3_imp_heatmap_scenario_x_rating.png"),
       p_heat, width = 10, height = 6, dpi = 300, bg = "white")

message("✅ Wrote: ", file.path(out_dir, "heatmap_scenario_x_rating_by_base_model_FROM_CODE2.csv"))
message("✅ Wrote: ", file.path(plots_dir, "rq3_imp_heatmap_scenario_x_rating.png"))
