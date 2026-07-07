# ---- NEU only: source effect (LLM vs HUMAN), adjust for base_model, cluster by id_key ----
#Humans vs LLM under NEU (are LLMs “higher” than humans?)
feas_neu <- feas %>% dplyr::filter(scenario == "NEU")

fit_src <- clmm(
  rating_ord ~ source + base_model + (1 | id_key),
  data = feas_neu, link = "logit", Hess = TRUE, nAGQ = 5
)
capture.output(summary(fit_src),
               file = file.path(outdir, "feas_fit_source_neu_summary.txt"))

sm <- summary(fit_src)$coefficients
row <- as.data.frame(sm)[rownames(sm) == "sourceLLM", , drop = FALSE]

or_src <- tibble::tibble(
  contrast = "LLM - HUMAN (NEU only)",
  OR      = exp(row$Estimate),
  CI_low  = exp(row$Estimate - 1.96*row$`Std. Error`),
  CI_high = exp(row$Estimate + 1.96*row$`Std. Error`),
  z       = row$`z value`,
  p       = 2*pnorm(-abs(row$`z value`))
) %>% dplyr::mutate(p_fdr = p.adjust(p, method = "BH"))

readr::write_csv(or_src, file.path(outdir, "feas_or_llm_vs_human_neu.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_or_llm_vs_human_neu.csv"))
