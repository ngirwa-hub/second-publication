# normalize the DC_solution name
if ("dc_solution" %in% names(df) && !"dc_solution_id" %in% names(df)) {
  df <- df %>% dplyr::rename(dc_solution_id = dc_solution)
}
# ---- 7) PER-SOLUTION LOGS (HUMAN vs LLM POOLED) ---------------------------
solutions <- sort(unique(df$dc_solution_id))

for (s in solutions) {
  dS <- df %>% filter(dc_solution_id == s)
  H  <- dS %>% filter(source == "human")
  L  <- dS %>% filter(source == "llm")

  fname <- file.path(log_dir_overall, sprintf("solution_%s.txt", sanitize_name(s)))
  header <- c(
    sprintf("Solution: %s", s),
    sprintf("N_human = %d, N_llm = %d", nrow(H), nrow(L)),
    sprintf("TOST margin (log-odds) = +/- %.3f (OR bounds [%.3f, %.3f])", DELTA, 1/exp(DELTA), exp(DELTA)),
    sprintf("W1 SESOI (delta_W1) = %.3f", DELTA_W1)
  )

  # frequency table by group
  dtab <- with(dS, table(rating, source))

  # fit CLM + TOST (robust to failures)
  clm_out <- fit_clm_and_tost(H, L)
  fit_obj <- NULL
  if (!is.na(clm_out$beta)) {
    # refit to get an object for full summary (separate from TOST helper’s quick fit)
    dat <- tibble(
      rating = ordered(c(H$rating, L$rating), levels = 0:4),
      group  = c(rep(0, nrow(H)), rep(1, nrow(L)))
    )
    fit_obj <- try(clm(rating ~ group, data = dat, link = "logit", Hess = TRUE), silent = TRUE)
  }

  tost_tbl <- tibble(
    beta = clm_out$beta,
    se   = clm_out$se,
    p_tost = clm_out$p_tost,
    note = clm_out$note
  )

  w1_row <- compute_w1(H, L)
  file <- file.path(log_dir_overall, sprintf("solution_%s.txt", sanitize_name(s)))
  write_clm_txt(file, header, dtab, fit_obj, tost_tbl, w1_row)
}
message("Wrote per-solution logs to: ", log_dir_overall)
