library(tidyverse)

csv <- "selection_rates_by_base_family.csv"  # <-- path to the data you pasted
k    <- 5               # Top-k
canon_topk <- 1:k       # canonical list Top-k = {1..k}

df <- readr::read_csv(csv, show_col_types = FALSE) %>%
  transmute(
    family    = tolower(base_family),
    condition = toupper(condition),
    barrier   = as.integer(barrier_id),
    pct       = as.numeric(mean_pct)
  ) %>%
  filter(condition %in% c("ZEROSHOT","CONTEXT"))

# Top-k per family × condition (ties broken by pct desc, then barrier ID asc)
topk <- df %>%
  group_by(family, condition) %>%
  arrange(family, condition, desc(pct), barrier) %>%
  mutate(rank = rank(-pct, ties.method = "first")) %>%
  filter(rank <= k) %>%
  summarise(topk = list(barrier), .groups = "drop")

# Pivot to get both condition sets side-by-side
pair <- topk %>%
  tidyr::pivot_wider(names_from = condition, values_from = topk)

# Helper: Jaccard between two integer vectors
jaccard_vec <- function(a, b) length(intersect(a, b)) / length(union(a, b))

# Compute the three Jaccards per family
out <- pair %>%
  rowwise() %>%
  mutate(
    j_ctx_zero = jaccard_vec(CONTEXT, ZEROSHOT),
    j_zero_list = jaccard_vec(ZEROSHOT, canon_topk),
    j_ctx_list  = jaccard_vec(CONTEXT,  canon_topk),
    kept_ctx_zero = paste(sort(intersect(CONTEXT, ZEROSHOT)), collapse = ", "),
    added_ctx     = paste(sort(setdiff(CONTEXT, ZEROSHOT)), collapse = ", "),
    dropped_zero  = paste(sort(setdiff(ZEROSHOT, CONTEXT)), collapse = ", ")
  ) %>%
  ungroup() %>%
  mutate(
    j_ctx_zero = round(j_ctx_zero, 3),
    j_zero_list = round(j_zero_list, 3),
    j_ctx_list  = round(j_ctx_list, 3)
  ) %>%
  # pretty labels
  mutate(family_label = recode(family,
                               phi4="Phi4", llama="LLaMa-Pro",
                               mistral="Mistral", gemma3="Gemma3:12B",
                               .default = family)) %>%
  relocate(family_label, .before = family)

# Save + print
dir.create("outputs", showWarnings = FALSE)
readr::write_csv(out %>% select(family_label, family, j_ctx_zero, j_zero_list, j_ctx_list,
                                kept_ctx_zero, added_ctx, dropped_zero),
                 "outputs/jaccard_top5_primacy_summary.csv")

print(out %>% select(family_label, j_ctx_zero, j_zero_list, j_ctx_list))
