library(dplyr)
library(tools)


# load dfs

total_df <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total.csv")
relevant_df <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")

total_df <- total_df %>%
  rename(
    NYT_total = NYT_count,
    CNN_total = CNN_Count,
    WSJ_total = WSJ_count,
    NBC_total = NBC_Count,
    MSNBC_total = MSNBC_Count,
    FOX_total = Fox_Count
  )

relevant_df <- relevant_df %>%
  rename(
    NYT_rel = NYT_rel.y,
    CNN_rel = CNN_rel.y,
    WSJ_rel = WSJ_rel.y,
    NBC_rel = NBC_rel.y,
    MSNBC_rel = MSNBC_rel.y,
    FOX_rel = FOX_rel.y
  )

yearly_combined <- left_join(total_df, relevant_df, by = c("year"))

yearly_combined <- yearly_combined %>%
select(sort(names(.)))

yearly_combined <- yearly_combined %>% relocate(year)

write.csv(yearly_combined, "yearly_combined.csv")

yearly_combined <- read.csv("yearly_combined.csv")

yearly_combined <- yearly_combined %>% select(-Unnamed..0) 

# CNN freq
yearly_combined <- yearly_combined %>%
  mutate(CNN_percent = (CNN_rel / CNN_total) * 100)
yearly_combined$CNN_percent <- sprintf("%.1f%%", yearly_combined$CNN_percent)

# MSNBC freq
yearly_combined <- yearly_combined %>%
  mutate(MSNBC_percent = (MSNBC_rel / MSNBC_total) * 100)
yearly_combined$MSNBC_percent <- sprintf("%.1f%%", yearly_combined$MSNBC_percent)

# FOX freq
yearly_combined <- yearly_combined %>%
  mutate(FOX_percent = (FOX_rel / FOX_total) * 100)
yearly_combined$FOX_percent <- sprintf("%.1f%%", yearly_combined$FOX_percent)

# WSJ freq
yearly_combined <- yearly_combined %>%
  mutate(WSJ_percent = (WSJ_rel / WSJ_total) * 100)
yearly_combined$WSJ_percent <- sprintf("%.1f%%", yearly_combined$WSJ_percent)

# NBC freq
yearly_combined <- yearly_combined %>%
  mutate(NBC_percent = (NBC_rel / NBC_total) * 100)
yearly_combined$NBC_percent <- sprintf("%.1f%%", yearly_combined$NBC_percent)

# NYT freq
yearly_combined <- yearly_combined %>%
  mutate(NYT_percent = (NYT_rel / NYT_total) * 100)
yearly_combined$NYT_percent <- sprintf("%.1f%%", yearly_combined$NYT_percent)


yearly_combined <- yearly_combined %>%
  select(sort(names(.)))

yearly_combined <- yearly_combined %>% relocate(year)

# pivot long

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)  # for percentage formatting

# Load your frequency data
freq_df <- read.csv("freq_df.csv")
freq_df <- freq_df %>%
  mutate(across(ends_with("_percent"), ~ as.numeric(str_remove(., "%"))))


# Reshape to long format
freq_long <- freq_df %>%
  pivot_longer(
    cols = -year,
    names_to = c("source", "metric"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = metric, values_from = value)

# plot
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

# Reshape if needed (assuming your data is in the wide format you showed)
freq_long <- freq_df %>%
  pivot_longer(
    cols = -year,
    names_to = c("source", "metric"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  filter(!is.na(rel))

freq_df <- freq_df %>% select(-Unnamed..0)



# Plot raw counts

ggplot(freq_long, aes(x = year, y = rel, color = source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(data = freq_long %>% filter(year == 2024), 
                  aes(label = source), 
                  nudge_x = 1, segment.color = NA) +
  geom_vline(xintercept = c(2002, 2011, 2017, 2022), linetype = "dashed", color = "grey50") +
  annotate("text", x = 2020, y = 1750, label = "ChatGPT\nRelease", 
           hjust = 0, size = 3, fontface = "italic", color = "grey40") +
annotate("text", x = 2016.8, y = 1450, label = "Transformers", 
         hjust = 1, size = 3, fontface = "italic", color = "grey40")+
  annotate("text", x = 2010.8, y = 1050, label = "Watson", 
           hjust = 1, size = 3, fontface = "italic", color = "grey40")+
  annotate("text", x = 2001.8, y = 550, label = "Roomba", 
           hjust = 1, size = 3, fontface = "italic", color = "grey40")+
  scale_x_continuous(limits = c(2000, 2025), 
                     breaks = seq(2000, 2024, by = 4))+
  scale_color_manual(values = c(
    "CNN" = "#191970",
    "FOX" = "#7A4988",
    "MSNBC" = "#6495ED",
    "NBC" = "#008080",
    "NYT" = "#67032F",
    "WSJ" = "#c74375" 
  )) +
  scale_y_continuous(
    breaks = seq(0, 2000, by = 250), 
    labels = comma,                  
    expand = expansion(mult = c(0, 0.05)) 
  ) +
  labs(
    title = "Salience of AI in Media Discourse",
    subtitle = "Diachronic analysis of total mentions of AI by news organization between 2000–2024",
    x = "Year",
    y = "Number of AI Referencing Publications",
    color = "Source",
  ) +
  theme_minimal(base_size = 12) +
  # Add this to your ggplot object
  coord_cartesian(clip = "off") + 
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title =  element_text(family = "serif", face = "bold", size = 22, color = "#1b1b1b"),
    plot.subtitle = element_text(family = "serif", size = 14, color = "grey30"),
    axis.title = element_text(family = "serif", face = "bold"),
    axis.text = element_text(family = "serif", size = 11),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines (they conflict with your markers)
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "serif"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor.y = element_line(color = "grey95", linetype = "dotted")
  )

 
library(ggplot2)

# This saves the LAST plot you displayed in RStudio
ggsave(
  filename = "AI_Frequency_2000_2024_1.png",
  width = 10,                     # Inches
  height = 7,                     # Inches
  dpi = 300,                      # Professional print/web quality
  bg = "white"                    # Ensures background isn't transparent
)

