# ================================
# Jaccard on Top-5 (Context vs Anchors) + LaTeX table
# ================================
# Requires: tidyverse
# Input:  rq3_selection_rates_by_base_family_and_arm.csv
# Outputs:
#   - jaccard_top5_wide.csv   (with top-5 sets and jaccard values)
#   - jaccard_top5_long.csv   (long/tidy form)
#   - jaccard_top5_table.tex  (LaTeX table with Jaccard values only)

library(tidyverse)

# ---------------- Config (tweak freely) ----------------
csv_path        <- "rq3_selection_rates_by_base_family_and_arm.csv"
top_k           <- 5
arm_order       <- c("context", "word", "example", "low-numeric", "high-numeric")
fam_order       <- c("phi4", "llama", "mistral", "gemma3")

# Nice display names for the LaTeX table (fallback to the raw name if missing)
base_label_map  <- c(
  phi4    = "Phi4",
  llama   = "LLaMa-Pro",
  mistral = "Mistral",
  gemma3  = "Gemma3:12B"
)

# LaTeX formatting
latex_caption   <- "Jaccard similarity of top-5 barrier sets: Context vs pooled anchors and vs each anchor arm (by base model)."
latex_label     <- "tab:jaccard_top5"   # \\label{...}
jaccard_digits  <- 3                    # decimals in the LaTeX table

# ------------------------------------------------------

# Helpers
fmt_base <- function(b) if (!is.na(base_label_map[[b]])) base_label_map[[b]] else b
jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}
top_k_codes <- function(df, k = top_k) {
  # expects columns: barrier_num, barrier_code, mean_pct
  df |>
    arrange(desc(mean_pct), barrier_num) |>
    slice_head(n = k) |>
    pull(barrier_code) |>
    as.character()
}
collapse_set <- function(x) paste(x, collapse = ",")

# Load & prep
dat <- readr::read_csv(csv_path, show_col_types = FALSE) |>
  rename_with(tolower)

req <- c("base_family", "arm", "barrier_id", "mean_pct")
missing <- setdiff(req, names(dat))
if (length(missing) > 0) {
  stop("Missing required columns in CSV: ", paste(missing, collapse = ", "))
}

# Barrier number + code B1..B*
dat <- dat |>
  mutate(
    barrier_num = suppressWarnings(as.integer(barrier_id)),
    barrier_num = ifelse(
      is.na(barrier_num),
      as.integer(readr::parse_number(as.character(barrier_id))),
      barrier_num
    )
  )

if (any(is.na(dat$barrier_num))) {
  stop("Could not parse some barrier_id values into numbers. Examples: ",
       paste(utils::head(unique(dat$barrier_id[is.na(dat$barrier_num)]), 5), collapse = ", "))
}

# Canonical barrier codes (B1..B11)
barriers_sorted <- sort(unique(dat$barrier_num))
dat <- dat |>
  mutate(
    barrier_code = factor(paste0("B", barrier_num),
                          levels = paste0("B", barriers_sorted))
  )

# Enforce desired base/arm ordering while keeping any unexpected values at the end
base_levels <- c(fam_order[fam_order %in% unique(dat$base_family)],
                 setdiff(unique(dat$base_family), fam_order))
arm_levels  <- c(arm_order[arm_order %in% unique(dat$arm)],
                 setdiff(unique(dat$arm), arm_order))

dat <- dat |>
  mutate(
    base_family = factor(base_family, levels = base_levels),
    arm         = factor(arm,         levels = arm_levels)
  )

# Arms present
arms_present   <- levels(dat$arm)
anchor_arms    <- setdiff(arms_present, "context")  # anchors = all non-context arms
anchor_arms_in <- intersect(anchor_arms, unique(dat$arm))

# For each base model, compute:
#   - top-5 at context
#   - top-5 at each arm
#   - pooled anchors: mean across all anchor arms (per barrier), then top-5

per_base <- vector("list", length(base_levels))
names(per_base) <- base_levels

for (b in base_levels) {
  d_b <- dat |> filter(base_family == b)

  # Context set
  ctx_df <- d_b |> filter(arm == "context")
  ctx_set <- top_k_codes(ctx_df)

  # Each arm (that exists for this base)
  arm_sets <- list()
  for (a in anchor_arms_in) {
    d_ba <- d_b |> filter(arm == a)
    if (nrow(d_ba) > 0) {
      arm_sets[[a]] <- top_k_codes(d_ba)
    }
  }

  # Pooled anchors (average mean_pct across available anchor arms for this base)
  pool_df <- d_b |>
    filter(arm %in% anchor_arms_in) |>
    group_by(barrier_num, barrier_code) |>
    summarise(mean_pct = mean(mean_pct, na.rm = TRUE), .groups = "drop")
  pool_set <- top_k_codes(pool_df)

  # Jaccard values
  j_pool <- jaccard(ctx_set, pool_set)
  j_each <- vapply(names(arm_sets), function(a) jaccard(ctx_set, arm_sets[[a]]), numeric(1))

  per_base[[b]] <- list(
    base       = as.character(b),
    context    = ctx_set,
    pooled     = pool_set,
    j_pooled   = j_pool,
    arm_sets   = arm_sets,
    j_each     = j_each
  )
}

# -------- CSV outputs (verification) --------
# Wide, one row per base model, with sets and Jaccard numbers
rows <- lapply(per_base, function(x) {
  base <- x$base
  out <- tibble(
    base_family          = base,
    context_top5         = collapse_set(x$context),
    pooled_anchors_top5  = collapse_set(x$pooled),
    jaccard_pooled       = x$j_pooled
  )
  # add each arm's set + jaccard (only if present)
  if (length(x$arm_sets)) {
    for (a in names(x$arm_sets)) {
      out[[paste0(a, "_top5")]]   <- collapse_set(x$arm_sets[[a]])
      out[[paste0("jaccard_", a)]] <- x$j_each[[a]]
    }
  }
  out
})
wide <- bind_rows(rows)

# Tidy/long
long <- list()
for (b in names(per_base)) {
  x <- per_base[[b]]
  # pooled
  long[[length(long) + 1]] <- tibble(
    base_family   = b,
    comparison    = "pooled",
    jaccard       = x$j_pooled,
    context_top5  = collapse_set(x$context),
    comp_top5     = collapse_set(x$pooled)
  )
  # each arm
  if (length(x$arm_sets)) {
    for (a in names(x$arm_sets)) {
      long[[length(long) + 1]] <- tibble(
        base_family   = b,
        comparison    = a,
        jaccard       = x$j_each[[a]],
        context_top5  = collapse_set(x$context),
        comp_top5     = collapse_set(x$arm_sets[[a]])
      )
    }
  }
}
long <- bind_rows(long)

# Write CSVs
readr::write_csv(wide, "jaccard_top5_wide.csv")
readr::write_csv(long, "jaccard_top5_long.csv")

# -------- LaTeX table --------
# Build a wide table of just Jaccard values for pooled + each present arm
# Column order: Base, Pooled, then each anchor arm in arm_order (if present)
cols <- c("pooled", setdiff(arm_order, "context"))
cols <- intersect(cols, c("pooled", unique(long$comparison))) # keep only present

# Prepare data for LaTeX
latex_df <- wide |>
  mutate(Base = vapply(base_family, fmt_base, character(1))) |>
  select(Base, jaccard_pooled, tidyselect::any_of(paste0("jaccard_", cols[cols != "pooled"])))

# Rename columns nicely
colnames(latex_df) <- c(
  "Base model",
  "Pooled anchors",
  if (length(cols) > 1) gsub("^jaccard_", "", colnames(latex_df)[-(1:2)]) else NULL
)

# Format numbers
fmt_num <- function(x) ifelse(is.na(x), "--", formatC(x, format = "f", digits = jaccard_digits))
latex_df[, -1] <- lapply(latex_df[, -1, drop = FALSE], fmt_num)

# Construct LaTeX tabular manually (no extra packages required)
make_latex <- function(df, caption, label) {
  n_cols <- ncol(df)
  align  <- paste0(rep("l", 1), paste0(rep("c", n_cols - 1), collapse = ""), collapse = "")
  header <- paste(colnames(df), collapse = " & ")
  rows   <- apply(df, 1, function(r) paste(r, collapse = " & "))
  body   <- paste(rows, collapse = " \\\\ \n")

  paste0(
    "\\begin{table}[ht]\n\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\begin{tabular}{", align, "}\n\\hline\n",
    header, " \\\\ \\hline\n",
    body, " \\\\ \\hline\n",
    "\\end{tabular}\n\\end{table}\n"
  )
}

latex_code <- make_latex(latex_df, latex_caption, latex_label)

# Save LaTeX table to file and also print to console
writeLines(latex_code, "jaccard_top5_table.tex")
cat(latex_code)
