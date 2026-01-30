# 01_prep_feasibility.R
suppressPackageStartupMessages({
  library(tidyverse); library(lubridate)
})

in_csv  <- "../merged_feasibility_zeroshot_context.csv"
out_dir <- "trial_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
  mutate(
    condition = toupper(trimws(condition)),
    rating    = suppressWarnings(as.integer(rating))
  ) %>%
  filter(condition %in% c("ZEROSHOT","CONTEXT"),
         !is.na(rating), rating >= 0, rating <= 4) %>%
  mutate(timestamp = suppressWarnings(lubridate::ymd_hms(timestamp, quiet = TRUE))) %>%
  arrange(timestamp) %>%
  # keep last record per (BM, variant, condition, iteration)
  group_by(base_model, variant_id, condition, iteration) %>%
  slice_tail(n = 1) %>% ungroup() %>%
  mutate(
    condition   = factor(condition, levels = c("ZEROSHOT","CONTEXT")),
    base_model  = factor(base_model),
    rating      = ordered(rating, levels = 0:4),
    # pair links ZEROSHOT and CONTEXT for same BM × variant × iteration
    pair_id     = interaction(base_model, variant_id, iteration, drop = TRUE)
  )

saveRDS(df, file.path(out_dir, "df_prepped_feas.rds"))
message("Saved: ", normalizePath(file.path(out_dir, "df_prepped_feas.rds"), winslash="\\", mustWork=FALSE))
