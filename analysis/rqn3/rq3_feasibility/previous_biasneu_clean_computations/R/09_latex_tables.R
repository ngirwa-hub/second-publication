# (a) Scenario ORs (LLM)
readr::read_csv(file.path(outdir, "feas_or_by_scenario_llm.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(
    OR_CI = sprintf("%.2f [%.2f, %.2f]", OR, CI_low, CI_high),
    p = scales::pvalue(p), .after = OR
  ) %>%
  dplyr::select(scenario, OR_CI, p) %>%
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("Scenario (vs NEU)", "OR [95% CI]", "p"),
               caption = "Scenario effects on feasibility (LLM only).") %>%
  kableExtra::kable_styling(full_width = FALSE) %>%
  cat(file = file.path(outdir, "table_feas_or_by_scenario.tex"))

# (b) Source OR (NEU only)
readr::read_csv(file.path(outdir, "feas_or_llm_vs_human_neu.csv"), show_col_types = FALSE) %>%
  dplyr::mutate(OR_CI = sprintf("%.2f [%.2f, %.2f]", OR, CI_low, CI_high)) %>%
  dplyr::select(contrast, OR_CI, p) %>%
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("Contrast", "OR [95% CI]", "p"),
               caption = "LLM vs Human under neutral framing.") %>%
  kableExtra::kable_styling(full_width = FALSE) %>%
  cat(file = file.path(outdir, "table_feas_or_llm_vs_human_neu.tex"))

message("✅ Wrote LaTeX tables in: ", outdir)
