# Distribution per base_model × scenario (CONTEXT + each ANCHOR_*), ratings 1..4, OBSERVED ONLY
suppressPackageStartupMessages({ library(dplyr); library(readr) })

indir  <- "feas_out"
in_rds <- file.path(indir, "feas_clean.rds")
outdir <- file.path(indir, "clmm_outputs")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

feas <- readRDS(in_rds)

# ensure expected cols/types
if (!"base_model" %in% names(feas)) feas$base_model <- "ALL"
feas$base_model <- as.character(feas$base_model)
feas$rating     <- as.integer(feas$rating)
feas$scenario   <- as.character(feas$scenario)

sc_levels <- c("CONTEXT","ANCHOR_WORD","ANCHOR_EXAMPLE","ANCHOR_NUM_LOW","ANCHOR_NUM_HIGH")

dist_anchor_obs <- feas %>%
  filter(scenario %in% sc_levels, rating %in% 1:4) %>%      # only observed ratings 1..4
  count(base_model, scenario, rating, name = "n") %>%
  group_by(base_model, scenario) %>%
  mutate(denom = sum(n), prop = n / denom) %>%              # denom = group total (repeats within group)
  ungroup() %>%
  arrange(base_model, scenario, rating)

readr::write_csv(dist_anchor_obs,
                 file.path(outdir, "feas_dist_by_base_anchor_types.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_dist_by_base_anchor_types.csv"))

