library(tidyverse)
library(lubridate)

# load dfs
cnn_df <- read.csv("cnn_df.csv")

# 1. Convert to date format and extract year
ai_frequency_df <- cnn_df %>%
  mutate(
    date_formatted = as.Date(date), 
    year = year(date_formatted)
  )

# 2. Now you can group and tally
ai_yearly_summary <- ai_frequency_df %>%
  group_by(year) %>%
  tally() # This is a shortcut for summarise(n = n())

# 3. Quick check
head(ai_yearly_summary)



# 1. Create the summary dataframe
ai_frequency_df <- ai_frequency_df %>%
  group_by(year) %>%
  summarise(coverage_count = n()) %>%
  filter(year >= 2000 & year <= 2024) # Optional: narrow your window

# 2. Plot the results
library(ggplot2)

ggplot(ai_frequency_df, aes(x = factor(year), y = coverage_count)) +
  geom_col(fill = "steelblue") +
  labs(title = "AI Coverage in the Media (2000-2024)",
       x = "Year",
       y = "Number of Mentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotates labels so they don't overlap


