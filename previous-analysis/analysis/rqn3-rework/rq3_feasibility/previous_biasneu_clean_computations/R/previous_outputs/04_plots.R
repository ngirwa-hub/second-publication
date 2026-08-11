# 04_plots.R
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2); library(forcats)
})
outdir <- "clean/feas_out"
feas <- readRDS(file.path(outdir, "feas_clean.rds"))

# A) LLM stacked by scenario
p_llm <- feas %>%
  dplyr::filter(source == "LLM", !is.na(scenario)) %>%
  dplyr::mutate(rating_f = factor(as.character(rating_ord), levels = c("0","1","2","3","4"))) %>%
  dplyr::count(scenario, rating_f, .drop = FALSE) %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x = scenario, y = prop, fill = rating_f)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = NULL, y = "Proportion", fill = "Rating",
                title = "Feasibility Ratings by Scenario: LLM Only") +
  ggplot2::scale_x_discrete(drop = FALSE, labels = c(
    NEU           = "Neutral",
    BIAS_WORD     = "Word Bias",
    BIAS_EXAMPLE  = "Example Bias",
    BIAS_NUM_LOW  = "Low-Numeric Bias",
    BIAS_NUM_HIGH = "High-Numeric Bias"
  )) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    plot.title  = ggplot2::element_text(hjust = 0.5),
    plot.title.position = "plot"
  )

ggplot2::ggsave(
  filename = file.path(outdir, "feas_llm_stack_by_scenario-check.png"),
  plot = p_llm, width = 7, height = 4, dpi = 300, bg = "white"
)


# B) NEU human vs LLM
p2 <- feas %>%
  filter(scenario=="NEU") %>%
  mutate(rating_f = factor(as.character(rating_ord), levels=c("0","1","2","3","4"))) %>%
  count(source, rating_f) %>%
  group_by(source) %>% mutate(prop = n/sum(n)) %>% ungroup() %>%
  ggplot(aes(x=source, y=prop, fill=rating_f)) +
  geom_col() +
  labs(x=NULL, y="Proportion", fill="Rating", title="Feasibility Ratings Neutral: Human vs LLM") +
  theme_minimal()
ggsave(file.path(outdir, "feas_neu_human_vs_llm.png"), p2, width=6, height=4, dpi=300, bg="white")

message("Plots saved to: ", normalizePath(outdir))
