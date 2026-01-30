# 01_prep_data.R
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(tidyverse)
library(lubridate)

# ---- I/O ----
in_csv  <- "new_merged_importance_zero_context.csv"
out_dir <- "clmm_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- load & clean ----
df <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
  mutate(
    condition   = toupper(trimws(condition)),
    rating      = suppressWarnings(as.integer(rating))
  ) %>%
  filter(condition %in% c("ZEROSHOT","CONTEXT"),
         !is.na(rating), rating >= 0, rating <= 4)

# keep latest timestamp per key
df <- df %>%
  mutate(timestamp = suppressWarnings(lubridate::ymd_hms(timestamp, quiet = TRUE))) %>%
  arrange(timestamp) %>%
  group_by(base_model, variant_id, dc_solution, condition, iteration) %>%
  slice_tail(n = 1) %>%
  ungroup()

# factors / ordered + pairing key
df <- df %>%
  mutate(
    condition   = factor(condition, levels = c("ZEROSHOT","CONTEXT")),
    base_model  = factor(base_model),
    dc_solution = factor(dc_solution),
    rating      = ordered(rating, levels = 0:4),
    pair_id     = interaction(base_model, variant_id, dc_solution, iteration, drop = TRUE)
  )

# save
saveRDS(df, file.path(out_dir, "df_prepped.rds"))
cat("Saved cleaned data to:", file.path(out_dir, "df_prepped.rds"), "\n")
