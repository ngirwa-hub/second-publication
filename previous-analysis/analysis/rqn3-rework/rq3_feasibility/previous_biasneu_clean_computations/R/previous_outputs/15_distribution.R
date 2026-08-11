# ---- 5.3 Diagnostics: distributions per base model × scenario ----
suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2) })

dist_tab <- feas %>%
  filter(source == "LLM") %>% mutate(rating = as.integer(rating)) %>% count(base_model, scenario, rating, name = "n") %>% group_by(base_model, scenario) %>% mutate(prop = n / sum(n)) %>% ungroup() %>% arrange(base_model, scenario, rating)
readr::write_csv(dist_tab, file.path(outdir, "feas_dist_by_base_scenario.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_dist_by_base_scenario.csv"))

p_dist <- ggplot(dist_tab, aes(x = scenario, y = prop, fill = factor(rating, levels = 0:4))) +
  geom_col() +
  facet_wrap(~ base_model, ncol = 2) +
  labs(
    x = NULL,
    y = "Proportion",
    fill = "Rating",
    title = "Feasibility Rating Distribution by Base Model & Scenario"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(outdir, "feas_stack_by_base_scenario.png"),
  p_dist,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)
message("✅ Wrote: ", file.path(outdir, "feas_stack_by_base_scenario.png"))
