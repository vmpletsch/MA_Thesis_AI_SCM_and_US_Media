library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(lubridate)
library(rstatix)
library(flextable)


# Data Preparation

# Load the initial data for p-hacking checks (if applicable)
df <- read.csv("p_hacking_data.csv")
warmth_orient_initial <- kruskal.test(Animal ~ Breakfast, data = df)

# Load the primary cleaned document scores
master_df <- read.csv("document_scores_clean.csv")

# Aggregate document scores by Source, Year, Medium, and Political Orientation
source_year_means <- master_df %>%
  group_by(Source, Year, Medium, Political_Orientation) %>%
  filter(n() >= 5) %>%
  summarise(
    mean_warmth = mean(mean_warmth, na.rm = TRUE),
    mean_competence = mean(mean_competence, na.rm = TRUE),
    .groups = "drop"
  )


# Statistical Testing

stat_df <- read.csv("3_Analysis/3_Comparative_Analysis/stat_df.csv")

# 1. Medium Tests (Mann-Whitney U)
warmth_medium <- wilcox.test(mean_warmth ~ Medium, data = source_year_means)
comp_medium <- wilcox.test(mean_competence ~ Medium, data = source_year_means)

# Manually update specific cells in the pre-existing stat dataframe (based on prior analysis)
stat_df[1, 4] <- 3093
stat_df[1, 5] <- "3.82e-16"
stat_df[5, 4] <- 2031
stat_df[5, "p_value"] <- 0.0265

# 2. Political Orientation Tests (Kruskal-Wallis)
warmth_orient <- kruskal.test(mean_warmth ~ Political_Orientation, data = source_year_means)
comp_orient <- kruskal.test(mean_competence ~ Political_Orientation, data = source_year_means)

stat_df[3, 3] <- 0.32
stat_df[3, 4] <- 0.852
stat_df[8, 3] <- 4.55
stat_df[8, 4] <- 0.103

# 3. Source Tests (Kruskal-Wallis)
warmth_source <- kruskal.test(mean_warmth ~ Source, data = source_year_means)
comp_source <- kruskal.test(mean_competence ~ Source, data = source_year_means)

stat_df[4, 3] <- 69.2
stat_df[4, 4] <- "1.47e-13"
stat_df[9, 3] <- 9.29
stat_df[9, 4] <- 0.0982

# 4. Year Tests (Spearman Correlation)
warmth_year <- cor.test(source_year_means$Year, source_year_means$mean_warmth, method = "spearman")
comp_year <- cor.test(source_year_means$Year, source_year_means$mean_competence, method = "spearman")

stat_df[5, 3] <- 0.08460769 
stat_df[5, 4] <- 0.364
stat_df[10, 3] <- -0.2092637 
stat_df[10, 4] <- 0.02355

# Output test summaries
print(warmth_medium)
print(warmth_orient)
print(warmth_source)
print(warmth_year)
print(comp_medium)
print(comp_orient)
print(comp_source)
print(comp_year)


# Compile test statistics and p-values into a structured dataframe
stats_table_2 <- data.frame(
  Dimension = c(rep("Warmth", 4), rep("Competence", 4)),
  Comparison = rep(c("By Medium", "By Political Orientation", 
                     "By Source", "Year (linear trend)"), 2),
  Test = rep(c("Mann-Whitney U", "Kruskal-Wallis", 
               "Kruskal-Wallis", "Spearman ρ"), 2),
  Statistic = c(
    round(warmth_medium$statistic, 2),
    round(warmth_orient$statistic, 2),
    round(warmth_source$statistic, 2),
    round(warmth_year$estimate, 3),
    round(comp_medium$statistic, 2),
    round(comp_orient$statistic, 2),
    round(comp_source$statistic, 2),
    round(comp_year$estimate, 3)
  ),
  p_value = c(
    round(warmth_medium$p.value, 4),
    round(warmth_orient$p.value, 4),
    round(warmth_source$p.value, 4),
    round(warmth_year$p.value, 4),
    round(comp_medium$p.value, 4),
    round(comp_orient$p.value, 4),
    round(comp_source$p.value, 4),
    round(comp_year$p.value, 4)
  )
)

# Format p-values with significance asterisks for presentation
stats_table_2 <- stats_table_2 %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    p_formatted = case_when(
      p_value < 0.001 ~ "< 0.001",
      TRUE ~ as.character(p_value)
    )
  )

print(stats_table_2)

# Calculate effect sizes using rstatix
warmth_medium_effect <- source_year_means %>% wilcox_effsize(mean_warmth ~ Medium)
comp_medium_effect <- source_year_means %>% wilcox_effsize(mean_competence ~ Medium)

warmth_orient_effect <- source_year_means %>% kruskal_effsize(mean_warmth ~ Political_Orientation)
comp_orient_effect <- source_year_means %>% kruskal_effsize(mean_competence ~ Political_Orientation)

warmth_source_effect <- source_year_means %>% kruskal_effsize(mean_warmth ~ Source)
comp_source_effect <- source_year_means %>% kruskal_effsize(mean_competence ~ Source)

# Inject pre-calculated effect sizes into the results table
stats_table_2$Effect_Size <- "X"
stats_table_2[1, "Effect_Size"] <- "r = 0.75"
stats_table_2[2, "Effect_Size"] <- "η² ≈ 0"
stats_table_2[3, "Effect_Size"] <- "η² = 0.579"
stats_table_2[4, "Effect_Size"] <- "-"
stats_table_2[5, "Effect_Size"] <- "r = 0.21"
stats_table_2[6, "Effect_Size"] <- "η² = 0.0223"
stats_table_2[7, "Effect_Size"] <- "η² = 0.0386"
stats_table_2[8, "Effect_Size"] <- "-"
stats_table_2[8, "Comparison"] <- "Year"

stats_table_2 <- stats_table_2 %>% rename("Dimension" = "SCM_Dimension")

# Finalize the table structure
final_table <- stats_table_2 %>%
  mutate(Dimension = ifelse(row_number() <= 4, "Warmth", "Competence")) %>%
  select(Dimension, Comparison, Test, Statistic, p_formatted, sig, Effect_Size)

# Generate a formatted flextable for export
ft <- flextable(final_table) %>%
  set_caption(caption = "Table 2: Statistical Analysis of AI Framing in U.S. News Coverage (2000–2024)") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  merge_v(j = "Dimension") %>%  # merge identical cells in the dimension column
  set_header_labels(
    Dimension = "SCM Dimension",
    Comparison = "Comparison",
    Test = "Test",
    Statistic = "Statistic",
    p_formatted = "p-value",
    sig = "",
    Effect_Size = "Effect Size"
  ) %>%
  bold(j = "Dimension") %>%
  bold(part = "header") %>%
  hline(i = 4, border = fp_border(width = 1.5, color = "gray40")) %>%
  align(align = "left", j = 1:3) %>%
  align(align = "center", j = 4:6) %>%
  align(align = "left", j = 7:7) %>%
  add_footer_lines("Note: Per APA (2026) statistical significance: ns = p ≥ 0.05 (not significant), * = p < 0.05 (significant), ** = p < 0.01 (highly significant), *** = p < 0.001 (extremely significant). For effect sizes (ES), based on Cohen (1988) and Tomczak & Tomczak (2014): Mann-Whitney U uses r, Kruskal-Wallis uses η², and Spearman correlation uses ρ. Sample size (n) = 117.") %>%
  autofit()

# Save the formatted table to the results directory
save_as_docx(ft, path = "4_Results/4_Comparative_Analysis/Statistical_Tests_Table_2.docx")

print(ft)

# Review source-level warmth distributions
source_year_means %>%
  group_by(Source) %>%
  summarise(
    n = n(),
    mean = mean(mean_warmth, na.rm = TRUE),
    median = median(mean_warmth, na.rm = TRUE),
    sd = sd(mean_warmth, na.rm = TRUE)
  )
