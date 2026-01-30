# ---- packages ----
# install.packages(c("tidyverse"))
library(tidyverse)

# ================================
# CONFIG — tweak to your liking
# ================================
csv_path   <- "rq3_selection_rates_by_base_family_and_arm.csv"

# Order of arms on the y-axis within each base model
arm_order  <- c("context", "word", "example", "low-numeric", "high-numeric")

# Order of base families (top-to-bottom)
fam_order  <- c("phi4", "llama", "mistral", "gemma3")

# Pretty display names (optional). Any not listed will keep their original name.
base_label_map <- c(
  phi4   = "Phi4",
  llama  = "LLaMa-Pro",
  mistral= "Mistral",
  gemma3 = "Gemma3:12B"
)

# Plot labels
plot_title <- "Selection rate by base model and anchor arm"
x_label    <- "Barrier code"
fill_label <- "Selection rate (%)"

# ================================
# LABEL / ANNOTATION OPTIONS
# ================================
show_labels       <- TRUE   # turn annotations on/off
label_decimals    <- 0      # 0=integers, 1=one decimal, etc.
label_size        <- 2.5    # text size of the annotations
label_switch_frac <- 0.55   # switch to white text above 55% of max fill

# Arms to include (exclude unexpected ones like 'anchor-other' by default)
plot_arms  <- arm_order

# ================================
# LOAD
# ================================
tab <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
  rename_with(tolower)

# Ensure required columns exist
req <- c("base_family", "arm", "barrier_id", "mean_pct")
missing <- setdiff(req, names(tab))
if (length(missing) > 0) {
  stop("Missing required columns in CSV: ", paste(missing, collapse = ", "))
}

# ================================
# PREP
# ================================
# Keep only desired arms
tab <- tab %>% filter(arm %in% plot_arms)

# Robust barrier code "B1..B11" regardless of how barrier_id is stored
tab <- tab %>%
  mutate(
    barrier_num = suppressWarnings(as.integer(barrier_id)),
    barrier_num = ifelse(
      is.na(barrier_num),
      as.integer(readr::parse_number(as.character(barrier_id))),
      barrier_num
    )
  )

if (any(is.na(tab$barrier_num))) {
  stop("Could not parse some barrier_id values into numbers. Example values: ",
       paste(utils::head(unique(tab$barrier_id), 5), collapse = ", "))
}

# X-axis: B1..B11 in numeric order
barriers_sorted <- sort(unique(tab$barrier_num))
tab <- tab %>%
  mutate(barrier_code = factor(paste0("B", barrier_num),
                               levels = paste0("B", barriers_sorted)))

# Order bases and arms as requested
base_levels <- c(fam_order[fam_order %in% unique(tab$base_family)],
                 setdiff(unique(tab$base_family), fam_order))
arm_levels  <- c(arm_order[arm_order %in% unique(tab$arm)],
                 setdiff(unique(tab$arm), arm_order))

tab <- tab %>%
  mutate(
    base_family = factor(base_family, levels = base_levels),
    arm         = factor(arm,         levels = arm_levels)
  )
# Build label text + readable label color
max_val <- max(tab$mean_pct, na.rm = TRUE)
label_decimals <- 0  # or 1, 2, ...
tab <- tab %>%
  mutate(
    label_txt = ifelse(is.na(mean_pct), "",
                       sprintf(paste0("%.", label_decimals, "f%%"), mean_pct)),
    label_col = ifelse(mean_pct >= label_switch_frac * max_val, "white", "black")
  )

# Build y-axis ordering: for each base, show context first, then other arms
pairs_present <- tab %>%
  transmute(y_id = paste0(as.character(base_family), "-", as.character(arm))) %>%
  distinct() %>% pull(y_id)

y_levels_full <- unlist(lapply(levels(tab$base_family),
                               function(b) paste0(b, "-", levels(tab$arm))))
y_levels <- y_levels_full[y_levels_full %in% pairs_present]



# Display labels with pretty base names
disp_name <- function(b) if (!is.na(base_label_map[[b]])) base_label_map[[b]] else b
y_levels_display <- vapply(y_levels, function(s) {
  parts <- strsplit(s, "-", fixed = TRUE)[[1]]
  paste0(disp_name(parts[1]), "-", parts[2])
}, character(1))
id_to_disp <- setNames(y_levels_display, y_levels)

tab <- tab %>%
  mutate(
    y_id   = paste0(as.character(base_family), "-", as.character(arm)),
    y_disp = factor(id_to_disp[y_id], levels = y_levels_display)
  )

# ================================
# PLOT
# ================================
p <- ggplot(tab, aes(x = barrier_code, y = y_disp, fill = mean_pct)) +
  geom_tile(color = "white") +
  { if (show_labels)
      geom_text(aes(label = label_txt, color = label_col),
                size = label_size, na.rm = TRUE, show.legend = FALSE)
  } +
  scale_color_identity() +
  scale_fill_gradient(low = "white", high = "steelblue", name = fill_label, labels = function(x) paste0(x, "%")) +
  labs(title = plot_title, x = x_label, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5)   # <- centers the title
  )

print(p)

# Optional save
ggsave("rq3_heatmap_by_family_arm.png", p, width = 10, height = 6, dpi = 300)
