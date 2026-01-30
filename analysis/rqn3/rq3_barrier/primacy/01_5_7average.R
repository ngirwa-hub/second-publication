# --- Load libraries ---
library(tidyverse)
library(knitr)

# --- File paths ---
file_struct1 <- "ordered_list/orderedlist_selection_rates_compressed.csv"
file_struct2 <- "reversed_list/reversedlist_selection_rates_compressed.csv"
file_struct3 <- "unnumbered_list/unnumberedlist_selection_rates_compressed.csv"

# --- Read and combine data ---
read_structure <- function(path, structure_label, delim = ",") {
  readr::read_delim(path, delim = delim, show_col_types = FALSE) %>%
    pivot_longer(
      cols = matches("^(?:[1-9]|10|11)$"),
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

df <- bind_rows(
  read_structure(file_struct1, "Structure 1", delim = ","),
  read_structure(file_struct2, "Structure 2", delim = ","),
  read_structure(file_struct3, "Structure 3", delim = ",")
)

# --- Align reversed list positions ---
n_barriers <- 11
df <- df %>%
  mutate(
    position = ifelse(structure == "Structure 2", n_barriers + 1 - position, position)
  )

# --- Rename base models ---
rename_map <- c(
  "gemma3" = "Gemma3:12B",
  "llama"  = "LLaMa-Pro",
  "mistral"= "Mistral",
  "phi4"   = "Phi4"
)
df <- df %>% mutate(model = recode(model, !!!rename_map, .default = as.character(model)))

# --- Filter for barriers 5, 6, 7 and compute averages ---
avg_table <- df %>%
  filter(position %in% 5:7) %>%
  group_by(model, structure) %>%
  summarise(avg_rate = mean(selection_rate, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    avg_rate = avg_rate * ifelse(max(avg_rate, na.rm = TRUE) <= 1.05, 100, 1),
    structure = recode(structure,
      "Structure 1" = "Ordered",
      "Structure 2" = "Reversed",
      "Structure 3" = "Unnumbered"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = structure,
    values_from = avg_rate
  )

# --- Print LaTeX table ---
print(kable(avg_table, format = "latex", digits = 2, booktabs = TRUE, caption = "Average selection rates for barriers 5-7"))
