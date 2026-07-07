# packages
library(tidyverse)
library(ordinal)

# clm()
# For BH-FDR we use base R p.adjust()

#data source
FILE <- "merged_importance_zeroshot.csv"

# ---- ANALYSIS KNOBS ----
set.seed(42)
CONDITION <- NULL          # e.g., "ZEROSHOT"; set to NULL to skip filtering
DELTA_OR  <- 1.25          # SESOI bounds on odds ratio scale: [1/DELTA_OR, DELTA_OR]
DELTA     <- log(DELTA_OR) # same on log-odds scale (used in TOST)
B_BOOT    <- 5000          # bootstrap reps for Wasserstein CIs
DELTA_W1  <- 0.20          # SESOI for normalized Wasserstein-1 (0..1) #i have changed this from 0.10 to 0.20
