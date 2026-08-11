# ================================
# 02_report_emd.R
# Reads EMD CSVs and exports TXT, LaTeX, PNG tables + simple plots
# ================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(tidyr)
  library(gridExtra); library(grid)    # for table PNGs
})

OUTDIR <- "clean/feas_out"
by_model_csv <- file.path(OUTDIR, "human_vs_llm_emd_by_model.csv")
overall_csv  <- file.path(OUTDIR, "human_vs_llm_emd_overall.csv")

by_model <- readr::read_csv(by_model_csv, show_col_types = FALSE)
overall  <- readr::read_csv(overall_csv,  show_col_types = FALSE)

# ---- Pretty versions for output ----
by_model_fmt <- by_model %>%
  mutate(emd = round(emd, 3)) %>%
  arrange(base_model, cond2)

overall_fmt <- overall %>%
  mutate(emd = round(emd, 3)) %>%
  arrange(cond2)

# ---- TXT ----
write.table(by_model_fmt,
            file = file.path(OUTDIR, "human_vs_llm_emd_by_model.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(overall_fmt,
            file = file.path(OUTDIR, "human_vs_llm_emd_overall.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE)

# ---- LaTeX (minimal tabular) ----
latex_tab <- function(df) {
  header <- paste(colnames(df), collapse = " & ")
  rows   <- apply(df, 1, function(r) paste(r, collapse = " & "))
  paste0(
    "\\begin{table}\n\\centering\n\\begin{tabular}{", paste(rep("l", ncol(df)), collapse=""), "}\n\\toprule\n",
    header, " \\\\\n\\midrule\n",
    paste(rows, collapse = " \\\\\n"), " \\\\\n\\bottomrule\n\\end{tabular}\n\\end{table}\n"
  )
}

writeLines(latex_tab(by_model_fmt), file.path(OUTDIR, "human_vs_llm_emd_by_model.tex"))
writeLines(latex_tab(overall_fmt),  file.path(OUTDIR, "human_vs_llm_emd_overall.tex"))

# ---- PNG tables ----
tg1 <- tableGrob(by_model_fmt, rows = NULL)
ggsave(file.path(OUTDIR, "human_vs_llm_emd_by_model.png"), tg1, width = 10, height = 4, dpi = 200)

tg2 <- tableGrob(overall_fmt, rows = NULL)
ggsave(file.path(OUTDIR, "human_vs_llm_emd_overall.png"), tg2, width = 6, height = 2.5, dpi = 200)

# ---- Plots ----
p1 <- by_model %>%
  mutate(cond2 = factor(cond2, levels = c("NEU","BIASED"))) %>%
  ggplot(aes(x = cond2, y = emd, group = base_model, color = base_model)) +
  geom_point() + geom_line() +
  labs(title = "EMD vs Human (NEU) by Base Model",
       x = "LLM Condition", y = "EMD (Wasserstein-1)") +
  theme_minimal()
ggsave(file.path(OUTDIR, "emd_by_model.png"), p1, width = 8, height = 5, dpi = 200)

p2 <- overall %>%
  mutate(cond2 = factor(cond2, levels = c("NEU","BIASED"))) %>%
  ggplot(aes(x = cond2, y = emd, group = 1)) +
  geom_point(size = 3) + geom_line() +
  labs(title = "EMD vs Human (NEU): Overall (LLM pooled)",
       x = "LLM Condition", y = "EMD (Wasserstein-1)") +
  theme_minimal()
ggsave(file.path(OUTDIR, "emd_overall.png"), p2, width = 6, height = 4, dpi = 200)

message("✅ Wrote TXT/LaTeX/PNG tables and plots to: ", OUTDIR)
