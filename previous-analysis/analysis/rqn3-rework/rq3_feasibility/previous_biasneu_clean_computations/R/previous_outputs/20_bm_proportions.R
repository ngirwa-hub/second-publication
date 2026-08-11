# ====== EDIT THESE TWO LINES ONLY ======
CSV_FILE <- "clean/feas_out/feas_dist_by_base_scenario.csv"  # your CSV path
OUT_PNG  <- "clean/feas_out/heatmap_proportions.png"         # set NA to skip saving
# ======================================

# ---- knobs for labels (uniform size) ----
LABEL_SIZE_PT       <- 7.0   # fixed font size for ALL shown labels (try 2.8–3.4)
MIN_PROP_TO_LABEL   <- 0.01  # hide labels if prop < 1% to avoid overflow (set 0.02 for stricter)
SHOW_LT1_AS_TEXT    <- FALSE # if TRUE and prop in (0,1%), draw "<1%"; if FALSE, hide

# ---- tile sizing (appearance) ----
gap_x   <- 0.03   # horizontal gap (0..0.9). Effective width  = 1 - gap_x
gap_y   <- 0.05   # vertical gap   (0..0.9). Effective height = 1 - gap_y
GRID_SIZE <- 0.35 # white border thickness
ASPECT    <- 2.10 # coord_fixed ratio (>1 = taller tiles, <1 = wider tiles)

# ---- optional renames & ordering ----
SCENARIO_LEVELS <- c("NEU","BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH")
RATING_LEVELS   <- 1:4
SCENARIO_RENAME <- c(
  "NEU"="Neutral","BIAS_WORD"="Word Bias","BIAS_EXAMPLE"="Example Bias",
  "BIAS_NUM_LOW"="Low-Num Bias","BIAS_NUM_HIGH"="High-Num Bias"
)
BASEMODEL_RENAME<- c("phi4"="Phi4","gemma3-12b"="Gemma3:12B","llama-pro"="LlaMa-Pro","mistral"="Mistral")

# ---- deps ----
if (!requireNamespace("ggfittext", quietly = TRUE)) install.packages("ggfittext")
suppressPackageStartupMessages({
  pkgs <- c("ggplot2","dplyr","readr","scales","viridisLite","grid","ggfittext")
  need <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

detect_delim <- function(path){
  first <- readLines(path, n = 1, warn = FALSE)
  if (grepl("\t", first)) "\t" else if (grepl(";", first)) ";" else ","
}

# ---- read & prep ----
stopifnot(file.exists(CSV_FILE))
delim <- detect_delim(CSV_FILE)
raw <- readr::read_delim(CSV_FILE, delim = delim, show_col_types = FALSE)

req <- c("base_model","scenario","rating","n")
miss <- setdiff(req, names(raw))
if (length(miss)) stop("Missing required column(s): ", paste(miss, collapse=", "))

df <- raw %>%
  dplyr::mutate(
    scen_code  = factor(scenario, levels = SCENARIO_LEVELS),
    rate_code  = factor(rating,   levels = RATING_LEVELS),
    base_model = as.character(base_model),
    scen_code  = as.character(scen_code)
  )

if (length(SCENARIO_RENAME)) df$scen_lab  <- dplyr::recode(df$scen_code, !!!SCENARIO_RENAME, .default = df$scen_code) else df$scen_lab <- df$scen_code
if (length(BASEMODEL_RENAME)) df$base_lab <- dplyr::recode(df$base_model, !!!BASEMODEL_RENAME, .default = df$base_model) else df$base_lab <- df$base_model

# numeric axes so width/height are honored
scen_levels_lab <- vapply(SCENARIO_LEVELS, function(x) ifelse(x %in% names(SCENARIO_RENAME), SCENARIO_RENAME[[x]], x), "")
x_breaks <- seq_along(RATING_LEVELS); x_labels <- RATING_LEVELS
y_breaks <- seq_along(scen_levels_lab); y_labels <- scen_levels_lab

df <- df %>%
  dplyr::mutate(
    x_id = as.integer(factor(rating, levels = RATING_LEVELS)),            # 1..K
    y_id = as.integer(factor(scen_lab, levels = scen_levels_lab))         # 1..S
  ) %>%
  dplyr::group_by(base_lab, y_id) %>%
  dplyr::mutate(
    tot  = sum(n, na.rm = TRUE),
    prop = if ("prop" %in% names(.)) ifelse(is.na(prop), n/sum(n), prop) else n/sum(n)
  ) %>%
  dplyr::ungroup()

# label text (uniform size; hide zeros; optional "<1%")
df <- df %>%
  dplyr::mutate(
    label_num = 100 * prop,
    label = dplyr::case_when(
      prop <= 0 ~ "",                                           # hide exact zeros
      prop < MIN_PROP_TO_LABEL ~ if (SHOW_LT1_AS_TEXT) "<1%" else "",  # tiny: hide or "<1%"
      TRUE ~ paste0(round(label_num), "%")
    )
  )

# palette
pal_vec <- viridisLite::viridis(256, option = "C", direction = -1)

# tile geometry
tile_w <- 1 - gap_x
tile_h <- 1 - gap_y
df <- df %>%
  dplyr::mutate(
    xmin = x_id - tile_w/2, xmax = x_id + tile_w/2,
    ymin = y_id - tile_h/2, ymax = y_id + tile_h/2
  )

# ---- plot (uniform label size via grow = FALSE) ----
p <- ggplot2::ggplot(df, aes(x = x_id, y = y_id, fill = prop)) +
  ggplot2::geom_tile(width = tile_w, height = tile_h, color = "white", linewidth = GRID_SIZE) +
  ggfittext::geom_fit_text(
    data     = subset(df, label != ""),                   # draw labels only where we want them
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label),
    size     = LABEL_SIZE_PT,   # <- FIXED size for all labels
    min.size = LABEL_SIZE_PT,   # do not shrink
    grow     = FALSE,           # do not grow
    reflow   = FALSE,
    contrast = TRUE,            # auto white/black text
    padding.x = grid::unit(0.2, "mm"),
    padding.y = grid::unit(0.2, "mm")
  ) +
  ggplot2::facet_wrap(~ base_lab, nrow = 1) +
  ggplot2::scale_fill_gradientn(
    colors = pal_vec, limits = c(0,1),
    labels = scales::percent_format(accuracy = 1), name = "Proportion"
  ) +
  ggplot2::scale_x_continuous(
    breaks = x_breaks, labels = x_labels,
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::scale_y_continuous(
    breaks = y_breaks, labels = y_labels,
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::coord_fixed(ratio = ASPECT) +
  ggplot2::labs(x = "Rating", y = NULL, title = "Rating Proportions by Scenario") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

print(p)
if (!is.na(OUT_PNG)) {
  ggplot2::ggsave(OUT_PNG, p, width = 10, height = 3.6, dpi = 300)
  message("Saved: ", OUT_PNG)
}
