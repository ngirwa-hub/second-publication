# ---- 8) PER-BASE × SOLUTION LOGS (HUMAN vs THAT BASE) ---------------------
bases <- df %>% filter(source == "llm") %>% pull(base_model) %>% unique() %>% sort(na.last = NA)

for (s in solutions) {
  dS <- df %>% filter(dc_solution_id == s)
  H  <- dS %>% filter(source == "human")

  for (b in bases) {
    B <- dS %>% filter(source == "llm", base_model == b)

    fname <- file.path(
      log_dir_perbase,
      sprintf("solution_%s__base_%s.txt", sanitize_name(s), sanitize_name(b))
    )
    header <- c(
      sprintf("Solution: %s | Base model: %s", s, b),
      sprintf("N_human = %d, N_%s = %d", nrow(H), b, nrow(B)),
      sprintf("TOST margin (log-odds) = +/- %.3f (OR bounds [%.3f, %.3f])", DELTA, 1/exp(DELTA), exp(DELTA)),
      sprintf("W1 SESOI (delta_W1) = %.3f", DELTA_W1)
    )

    dtab <- with(bind_rows(
      H %>% mutate(source = "human"),
      B %>% mutate(source = "llm")
    ), table(rating, source))

    clm_out <- fit_clm_and_tost(H, B)
    fit_obj <- NULL
    if (!is.na(clm_out$beta)) {
      dat <- tibble(
        rating = ordered(c(H$rating, B$rating), levels = 0:4),
        group  = c(rep(0, nrow(H)), rep(1, nrow(B)))
      )
      fit_obj <- try(clm(rating ~ group, data = dat, link = "logit", Hess = TRUE), silent = TRUE)
    }

    tost_tbl <- tibble(
      beta = clm_out$beta,
      se   = clm_out$se,
      p_tost = clm_out$p_tost,
      note = clm_out$note
    )

    w1_row <- compute_w1(H, B)
    file <- file.path(log_dir_perbase, sprintf("solution_%s__base_%s.txt", sanitize_name(s), sanitize_name(b)))
    write_clm_txt(file, header, dtab, fit_obj, tost_tbl, w1_row)
  }
}
message("Wrote per-base logs to: ", log_dir_perbase)
