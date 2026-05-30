library(dplyr)
library(tidyr)
library(ggplot2)

# Aggregate NYT scores by year, excluding years with insufficient data
nyt_year_means <- document_scores %>%
  filter(Source == "NYT") %>%
  group_by(Year) %>%
  filter(n() >= 5) %>%
  summarise(
    n_docs = n(),
    mean_warmth = mean(mean_warmth, na.rm = TRUE),
    mean_competence = mean(mean_competence, na.rm = TRUE),
    .groups = "drop"
  )

# Plot the trajectory of NYT framing over time
ggplot(nyt_year_means, aes(x = mean_competence, y = mean_warmth)) +
  
  # Set quadrant boundaries at the origin
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
  
  # Connect years chronologically to show trajectory
  geom_path(arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
            color = "gray70", alpha = 0.6, linewidth = 0.5) +
  
  # Plot yearly means with a gradient indicating time
  geom_point(aes(color = Year), size = 4, alpha = 0.85) +
  
  # Label key milestones to maintain readability
  geom_text(data = nyt_year_means %>% filter(Year %in% c(2000, 2011, 2016, 2022, 2024)),
            aes(label = Year), hjust = -0.3, vjust = 0.5, 
            size = 3.5, fontface = "bold") +
  
  # Transition colors from early (light) to recent (dark) years
  scale_color_gradient(low = "#FFCC99", high = "#660000") +
  
  # Annotate quadrants based on the Stereotype Content Model
  annotate("text", x = 0.13, y = 0.13, label = "Admiration",
           color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
  annotate("text", x = -0.08, y = 0.13, label = "Pity",
           color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
  annotate("text", x = 0.13, y = -0.10, label = "Envy / Threat",
           color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
  annotate("text", x = -0.08, y = -0.10, label = "Contempt",
           color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
  
  # Set fixed axis limits for consistency
  scale_x_continuous(limits = c(-0.10, 0.15)) +
  scale_y_continuous(limits = c(-0.12, 0.15)) +
  
  labs(
    title = "AI Framing Trajectory in The New York Times, 2000–2024",
    subtitle = "Annual mean warmth and competence scores",
    x = "Competence",
    y = "Warmth",
    color = "Year"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
