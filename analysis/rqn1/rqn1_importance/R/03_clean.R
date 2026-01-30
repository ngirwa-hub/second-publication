# ---- 2) LOAD & CLEAN -------------------------------------------------------
df <- readr::read_csv(FILE, show_col_types = FALSE) %>%
  mutate(
    source = tolower(as.character(source)),
    dc_solution = suppressWarnings(as.integer(dc_solution)),
    rating = suppressWarnings(as.integer(rating))
  ) %>%
  filter(!is.na(dc_solution), !is.na(rating))

if (!is.null(CONDITION) && "condition" %in% names(df)) {
  df <- df %>% filter(condition == CONDITION)
}

# normalize the DC_solution name
if ("dc_solution" %in% names(df) && !"dc_solution_id" %in% names(df)) {
  df <- df %>% dplyr::rename(dc_solution_id = dc_solution)
}

# Keep only relevant columns; others are fine to carry along but not required
need <- c("source","base_model","variant_id","iteration","dc_solution_id","rating")
df <- df %>% select(any_of(need))

# Quick sanity
df %>% count(source)
stopifnot(all(df$rating %in% 0:4))

df <- readr::read_csv(FILE, show_col_types = FALSE)
