# Balanced bootstrap: sample equal counts from each group per replicate
bootstrap_ci_w1 <- function(df_h, df_l, B = B_BOOT) {
  if (nrow(df_h) == 0 || nrow(df_l) == 0) return(c(NA_real_, NA_real_))
  m <- min(nrow(df_h), nrow(df_l))        # balance on the smaller group
  vals <- numeric(B)
  h_idx <- seq_len(nrow(df_h))
  l_idx <- seq_len(nrow(df_l))
  for (b in seq_len(B)) {
    H <- df_h[sample(h_idx, m, replace = TRUE), , drop = FALSE]
    L <- df_l[sample(l_idx, m, replace = TRUE), , drop = FALSE]
    p <- H$rating %>% factor(levels = 0:4) %>% table() %>% as.numeric()
    q <- L$rating %>% factor(levels = 0:4) %>% table() %>% as.numeric()
    vals[b] <- wasserstein_ordinal(p, q)
  }
  stats::quantile(vals, c(0.025, 0.975), names = FALSE) %>% as.numeric()
}
# Fit proportional-odds CLM: rating ~ group (llm=1, human=0),
# with optional LLM downsampling to the human N; returns beta, SE, and TOST p-value.
fit_clm_and_tost <- function(df_h, df_l, delta = DELTA, downsample = FALSE) {
  out <- list(beta = NA_real_, se = NA_real_, p_tost = NA_real_, note = "ok")

  # Need at least 2 observed categories in both groups
  if (dplyr::n_distinct(df_h$rating) < 2L || dplyr::n_distinct(df_l$rating) < 2L) {
    out$note <- "insufficient categories"; return(out)
  }

  # Optional: reduce LLM rows to human N to avoid imbalance effects (one-shot)
  if (isTRUE(downsample) && nrow(df_l) > nrow(df_h)) {
    set.seed(42)
    df_l <- df_l[sample(seq_len(nrow(df_l)), nrow(df_h), replace = FALSE), , drop = FALSE]
  }

  # Build data and fit proportional-odds model
  y <- c(df_h$rating, df_l$rating)
  dat <- tibble::tibble(
    rating = ordered(y, levels = 0:4),
    group  = c(rep(0, nrow(df_h)), rep(1, nrow(df_l)))
  )

  fit <- try(ordinal::clm(rating ~ group, data = dat, link = "logit", Hess = TRUE), silent = TRUE)
  if (inherits(fit, "try-error")) { out$note <- paste("fit_error:", as.character(fit)); return(out) }

  co <- summary(fit)$coefficients
  if (!"group" %in% rownames(co)) { out$note <- "no_group_coef"; return(out) }

  beta <- unname(co["group", "Estimate"])
  se   <- unname(co["group", "Std. Error"])

  # TOST on beta within [-delta, +delta]
  z_lo <- (beta + delta) / se; p_lo <- 1 - stats::pnorm(z_lo)  # test beta > -delta
  z_hi <- (delta - beta) / se; p_hi <- 1 - stats::pnorm(z_hi)  # test beta < +delta
  out$beta <- beta; out$se <- se; out$p_tost <- max(p_lo, p_hi)
  out
}
