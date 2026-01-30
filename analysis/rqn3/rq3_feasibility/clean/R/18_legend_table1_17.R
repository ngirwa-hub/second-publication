library(dplyr); library(stringr); library(tibble)

# 0) Get the factor level names used in the fit
sc_lvls <- levels(feas_llm_i$scenario)
bm_lvls <- levels(feas_llm_i$base_model)

# 1) Legend you can print/save anywhere (optional but nice)
legend_df <- tibble(
  code = c(paste0("scenario", seq_len(length(sc_lvls)-1)),
           paste0("base_model", seq_len(length(bm_lvls)-1))),
  meaning = c(sc_lvls[-length(sc_lvls)], bm_lvls[-length(bm_lvls)]),
  note = c(
    rep("Last scenario level is implied by sum-to-zero", length(sc_lvls)-1),
    rep("Last base_model level is implied by sum-to-zero", length(bm_lvls)-1)
  )
)
# writeLines(capture.output(print(legend_df, row.names = FALSE)), file.path(outdir,"table1_legend.txt"))

# 2) Start from your Table 1 data frame (call it t1_df as in prior code)
t1_aug <- t1_df %>%
  mutate(
    term_pretty = case_when(
      str_detect(term, "^scenario\\d+$") ~ sc_lvls[as.integer(str_match(term, "scenario(\\d+)")[,2])],
      str_detect(term, "^base_model\\d+$") ~ bm_lvls[as.integer(str_match(term, "base_model(\\d+)")[,2])],
      str_detect(term, "^scenario\\d+:base_model\\d+$") ~ {
        m <- str_match(term, "scenario(\\d+):base_model(\\d+)")
        paste0(sc_lvls[as.integer(m[,2])], " : ", bm_lvls[as.integer(m[,3])])
      },
      TRUE ~ term   # thresholds stay as-is: "1|2", "2|3", "3|4"
    )
  )

# 3) Add the implied last levels for scenario & base_model (main effects)
# Pull raw coefficients for main effects (log-odds)
coef_all <- as.data.frame(summary(fit_ec)$coefficients) %>% rownames_to_column("term")

get_implied_row <- function(prefix, level_names, pretty_label){
  # grab the L-1 printed rows for this factor
  sub <- coef_all %>% filter(str_detect(term, paste0("^",prefix,"\\d+$")))
  # implied coefficient on log-odds scale
  est_imp <- -sum(sub$Estimate)
  se_imp  <- NA_real_  # SE for implied effect is not directly reported; keep NA
  OR      <- exp(est_imp)
  tibble(
    term = paste0(prefix, length(level_names)),  # e.g., "scenario5"
    `Estimate (log-odds)` = round(est_imp, 3),
    SE = se_imp,
    OR = round(OR, 2),
    `95% CI` = "(NA, NA)", z = NA, p = NA,
    term_pretty = level_names[length(level_names)]
  )
}

imp_scen <- get_implied_row("scenario", sc_lvls, "Scenario")
imp_bmod <- get_implied_row("base_model", bm_lvls, "Base model")

# 4) Bind implied rows and reorder for readability
t1_selfexp <- t1_aug %>%
  bind_rows(imp_scen, imp_bmod) %>%
  mutate(section = case_when(
    str_detect(term, "\\|") ~ "Thresholds",
    str_detect(term, "^scenario\\d+$") ~ "Scenario (vs grand mean)",
    str_detect(term, "^base_model\\d+$") ~ "Base model (vs grand mean)",
    str_detect(term, "^scenario\\d+:base_model\\d+$") ~ "Interaction (deviation from average scenario effect)",
    TRUE ~ "Other"
  )) %>%
  arrange(factor(section, levels = c("Thresholds","Scenario (vs grand mean)","Base model (vs grand mean)","Interaction (deviation from average scenario effect)")),
          term_pretty) %>%
  select(section, term_pretty, `Estimate (log-odds)`, SE, OR, `95% CI`, z, p)

# Now write this table out in your usual latex/png/txt ways
save_as_latex(t1_selfexp, file.path(outdir,"table_fixed_effects_effectcoded_OR_SELFEXPLAIN.tex"), "Fixed effects (effect coding), labeled with real factor levels")
save_as_png(t1_selfexp,   file.path(outdir,"table_fixed_effects_effectcoded_OR_SELFEXPLAIN.png"),  "Fixed effects (effect coding)", "ORs vs grand mean (real level names)")
save_as_txt(t1_selfexp,   file.path(outdir,"table_fixed_effects_effectcoded_OR_SELFEXPLAIN.txt"),  "Fixed effects (effect coding) — labeled\n")
