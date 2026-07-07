library(dplyr)

# Your data frame
df <- data.frame(
  base_model = c("gemma3-12b","llama-pro","mistral","phi4"),
  EMD_BIASED = c(0.272727273, 0.168939394, 0.268560606, 0.272727273),
  EMD_NEU    = c(0.272727273, 0.159393939, 0.272727273, 0.272727273)
) %>%
  mutate(Delta = EMD_BIASED - EMD_NEU)

# Round for table display
df_fmt <- df %>%
  mutate(across(c(EMD_BIASED, EMD_NEU, Delta), ~round(.x, 3)))

# Save LaTeX table
library(xtable)
texfile <- "clean/feas_out/emd_delta_table.tex"
print(
  xtable(df_fmt, caption = "Δ EMD (Biased − Neutral) per base model. Positive values indicate Neutral closer to humans, negative values indicate Biased closer."),
  include.rownames = FALSE,
  file = texfile
)

message("✅ Wrote LaTeX table to: ", texfile)
