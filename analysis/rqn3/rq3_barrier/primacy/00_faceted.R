library(tidyverse)
library(scales)
library(dplyr)
library(forcats)

# reading file structure
read_structure <- function(path, structure_label, delim = ",") {
  readr::read_delim(path, delim = delim, show_col_types = FALSE) %>%
    pivot_longer(
      cols = matches("^(?:[1-9]|10|11)$"),  # columns named 1..11
      names_to = "position",
      values_to = "selection_rate"
    ) %>%
    mutate(
      model = base_model,
      position = as.integer(position),
      structure = structure_label
    ) %>%
    select(model, position, selection_rate, structure)
}

# --- Paths (CSV -> use delim = ",") ---
file_struct1 <- "ordered_list/orderedlist_selection_rates_compressed.csv"
file_struct2 <- "reversed_list/reversedlist_selection_rates_compressed.csv"
file_struct3 <- "unnumbered_list/unnumberedlist_selection_rates_compressed.csv"

# --- Read & combine ---
df <- bind_rows(
  read_structure(file_struct1, "Structure 1", delim = ","),
  read_structure(file_struct2, "Structure 2", delim = ","),
  read_structure(file_struct3, "Structure 3", delim = ",")
)

#to ensur the reversed list is correctly oriented
n_barriers <- 11
df <- df %>%
  mutate(
    position = ifelse(structure == "Structure 2", n_barriers + 1 - position, position)
  )

# --- Rename base models (AFTER df exists) ---
rename_map <- c(
  "gemma3" = "Gemma3:12B",
  "llama"  = "LLaMa-Pro",
  "mistral"= "Mistral",
  "phi4"   = "Phi4"
)
df <- df %>% mutate(model = recode(model, !!!rename_map, .default = as.character(model)))

# --- Optional: control facet (model) order ---
model_order <- c("Gemma3:12B", "Mistral", "LLaMa-Pro", "Phi4")
df <- df %>%
  mutate(
    model = factor(model, levels = model_order),
    structure = factor(structure),
    position = as.integer(position)
  ) %>%
  filter(position >= 1, position <= 11)

# --- Convert to % if values are 0..1 ---
rate_max <- max(df$selection_rate, na.rm = TRUE)
if (rate_max <= 1.05) {
  df <- df %>% mutate(selection_rate = selection_rate * 100)
}

# --- Plot ---
middle_band <- tibble(xmin = 4.5, xmax = 7.5, ymin = -Inf, ymax = Inf)

p <- ggplot(df, aes(x = position, y = selection_rate, color = structure, group = structure)) +
  geom_rect(data = middle_band,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, alpha = 0.06) +
  geom_line(size = 0.9) +
  geom_point(size = 2) +
  #geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.6) +
  facet_wrap(~ model, ncol = 2) +
  scale_x_continuous(breaks = 1:11, minor_breaks = NULL) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%")) +
  scale_color_discrete(
    name = "Prompting structure:",
    labels = c("Structure 1" = "Numbered ordered list",
               "Structure 2" = "Numbered reversed list",
               "Structure 3" = "Unnumbered ordered list")
  ) +
  guides(color = guide_legend(title = "Prompting structure")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",              # move legend (optional)
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    x = "Barrier code",
    y = "Selection rate (%)",
    #title = "Serial-position effects by base model",
    subtitle = "Three prompting structures across 11 barriers presentation"
  ) +
  theme(
    #plot.title = element_text(hjust = 0.5),     # center main title
    plot.subtitle = element_text(hjust = 0.5),  # center subtitle (optional)
    strip.text = element_text(hjust = 0.5)      # center facet titles inside each strip
  )

print(p)
ggsave("rq4_selection_rates_faceted.png", plot = p, width = 8, height = 6, dpi = 300)
