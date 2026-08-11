
# Safety: ensure we have dc_solution_id even if caller passed dc_solution
if ("dc_solution" %in% names(df) && !"dc_solution_id" %in% names(df)) {
  df <- dplyr::rename(df, dc_solution_id = dc_solution)
}

# ---- 04_per_base_per_solution.R ----
bases <- df %>% filter(source == "llm") %>% pull(base_model) %>% unique() %>% sort(na.last = NA)
rows <- list(); kk <- 1L
solutions <- sort(unique(df$dc_solution_id))

for (s in solutions) {
  dS <- df %>% filter(dc_solution_id == s)
  H  <- dS %>% filter(source == "human")
  for (b in bases) {
    B <- dS %>% filter(source == "llm", base_model == b)
    if (nrow(H) == 0 || nrow(B) == 0) {
      rows[[kk]] <- tibble(dc_solution_id = s, base_model = b,
                           n_h = nrow(H), n_b = nrow(B),
                           beta = NA_real_, se = NA_real_, p_tost = NA_real_,
                           w1 = NA_real_, w1_lo = NA_real_, w1_hi = NA_real_,
                           clm_equiv = "NA", w1_equiv = "NA", note = "missing group")
      kk <- kk + 1L; next
    }
    clm_out <- fit_clm_and_tost(H, B, delta = DELTA, downsample = TRUE)
    w1_ci   <- bootstrap_ci_w1(H, B, B = B_BOOT)
    p <- H$rating %>% factor(levels=0:4) %>% table() %>% as.numeric()
    q <- B$rating %>% factor(levels=0:4) %>% table() %>% as.numeric()
    w1 <- wasserstein_ordinal(p, q)

    rows[[kk]] <- tibble(dc_solution_id = s, base_model = b,
                         n_h = nrow(H), n_b = nrow(B),
                         beta = clm_out$beta, se = clm_out$se, p_tost = clm_out$p_tost,
                         w1 = w1, w1_lo = w1_ci[1], w1_hi = w1_ci[2],
                         clm_equiv = ifelse(!is.na(clm_out$p_tost) & clm_out$p_tost < 0.05, "Equivalent", "Not equivalent / NA"),
                         w1_equiv  = w1_equiv_call(w1_ci[1], w1_ci[2]),
                         note = clm_out$note)
    kk <- kk + 1L
  }
}
per_base <- dplyr::bind_rows(rows) %>%
  group_by(dc_solution_id) %>%
  mutate(p_tost_bh = if (all(is.na(p_tost))) NA_real_ else p.adjust(replace_na(p_tost, 1), method = "BH")) %>%
  ungroup()


