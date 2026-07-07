
# Safety: ensure we have dc_solution_id even if caller passed dc_solution
if ("dc_solution" %in% names(df) && !"dc_solution_id" %in% names(df)) {
  df <- dplyr::rename(df, dc_solution_id = dc_solution)
}
# ---- 03_overall_per_solution.R ----
solutions <- sort(unique(df$dc_solution_id))
rows <- vector("list", length(solutions))

for (ii in seq_along(solutions)) {
  s  <- solutions[ii]
  dS <- df %>% filter(dc_solution_id == s)
  H  <- dS %>% filter(source == "human")
  L  <- dS %>% filter(source == "llm")

  if (nrow(H) == 0 || nrow(L) == 0) {
    rows[[ii]] <- tibble(dc_solution_id = s, n_h = nrow(H), n_l = nrow(L),
                         beta = NA_real_, se = NA_real_, p_tost = NA_real_,
                         w1 = NA_real_, w1_lo = NA_real_, w1_hi = NA_real_,
                         clm_equiv = "NA", w1_equiv = "NA", note = "missing group")
    next
  }

  clm_out <- fit_clm_and_tost(H, L, delta = DELTA, downsample = TRUE)  # ← robust to imbalance
  w1_ci   <- bootstrap_ci_w1(H, L, B = B_BOOT)
  p <- H$rating %>% factor(levels=0:4) %>% table() %>% as.numeric()
  q <- L$rating %>% factor(levels=0:4) %>% table() %>% as.numeric()
  w1 <- wasserstein_ordinal(p, q)

  rows[[ii]] <- tibble(
    dc_solution_id = s, n_h = nrow(H), n_l = nrow(L),
    beta = clm_out$beta, se = clm_out$se, p_tost = clm_out$p_tost,
    w1 = w1, w1_lo = w1_ci[1], w1_hi = w1_ci[2],
    clm_equiv = ifelse(!is.na(clm_out$p_tost) & clm_out$p_tost < 0.05, "Equivalent", "Not equivalent / NA"),
    w1_equiv  = w1_equiv_call(w1_ci[1], w1_ci[2]),
    note = clm_out$note
  )
}
overall <- dplyr::bind_rows(rows)
