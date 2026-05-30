library(tidyverse)
library(lubridate)
library(tidytext)

# Ensure date is correct

cnn_df <- read.csv("cnn_BERT_df.csv")

cnn_df <- cnn_df %>%
  mutate(date = as.Date(date),
         year_month = floor_date(date, "month"))

# 1. Tokenize and remove stop words
tidy_segments <- cnn_df %>%
  unnest_tokens(word, context_clip) %>%
  anti_join(stop_words)

# 2. Join with Sentiment AND Normalize
sentiment_trends <- tidy_segments %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(year_month, sentiment) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    total_sentiment_words = positive + negative,
    net_sentiment_score = (positive - negative) / total_sentiment_words # Normalized score
  )


write_csv(sentiment_trends, "sentiment_analysis.csv")



# 3. Visualize with a Trend Line (LOESS)
# This helps see the 25-year "arc" without being distracted by monthly spikes
ggplot(sentiment_trends, aes(x = year_month, y = net_sentiment_score)) +
  geom_line(alpha = 0.3, color = "gray") + # The raw monthly data
  geom_smooth(method = "loess", span = 0.1, color = "red") + # The 25-year trend
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Sentiment Drift in CNN Transcripts (2000-2025)",
       subtitle = "Normalized Net Sentiment Score (Bing Lexicon)",
       y = "Net Sentiment (Positive - Negative)",
       x = "Year")