# ---- 6) LOGGING UTILS ------------------------------------------------------
log_dir_overall <- "importance_logs/overall"
log_dir_perbase <- "importance_logs/per_base"
dir.create("importance_logs", showWarnings = FALSE)
dir.create(log_dir_overall, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir_perbase, recursive = TRUE, showWarnings = FALSE)


sanitize_name <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

write_clm_txt <- function(file, header, dtab, fit_obj, tost, w1_row) {
  con <- file(file, open = "wt"); on.exit(close(con), add = TRUE)
  writeLines(header, con)
  writeLines("\n-- Rating counts by group (rows=rating 0..4) --", con)
  capture.output(print(dtab), file = con, append = TRUE)
  if (inherits(fit_obj, "try-error") || is.null(fit_obj)) {
    writeLines("\n-- CLM fit: ERROR/NA --", con)
  } else {
    writeLines("\n-- CLM summary --", con)
    capture.output(print(summary(fit_obj)), file = con, append = TRUE)
  }
  writeLines("\n-- TOST on group (beta) --", con)
  capture.output(print(tost), file = con, append = TRUE)
  writeLines("\n-- Wasserstein-1 (normalized, 0..1) --", con)
  capture.output(print(w1_row), file = con, append = TRUE)
  invisible(TRUE)
}

# small helper to compute W1 + CI from two frames with 'rating'
compute_w1 <- function(H, L) {
  p <- H$rating %>% factor(levels = 0:4) %>% table() %>% as.numeric()
  q <- L$rating %>% factor(levels = 0:4) %>% table() %>% as.numeric()
  w1 <- wasserstein_ordinal(p, q)
  ci <- bootstrap_ci_w1(H, L)
  tibble(w1 = w1, w1_lo = ci[1], w1_hi = ci[2])
}
