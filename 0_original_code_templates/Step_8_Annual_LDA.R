library(dplyr)
library(tidytext)
library(topicmodels)
library(lubridate)
library(tidyr)
library(stringr)


# 1. Load data and extract years
cnn_df <- read.csv("cnn_df.csv") %>%
  mutate(year = year(as.Date(date)))

# 2. Apply Custom Domain Stopwords
data(stop_words)

custom_stop_words <- bind_rows(
  stop_words,
  data.frame(word = c("whitfield", "lot", "begin", "sylvester", "haley", "cold", "video", "clip", "lee", "day", "schneider", 
                      "cnn", "news", "host", "artificial", "intelligence",
                      "anchor", "correspondent", "story", "report", "time",
                      "people", "world", "daryn", "kagan", "jim", "clancy", 
                      "michael", "holmes"), 
             lexicon = "custom")
)

# Unnest and clean tokens
cnn_tokens <- cnn_df %>%
  unnest_tokens(word, context_clip) %>%
  anti_join(custom_stop_words, by = "word") %>%
  # Filter out very short words or purely numeric tokens for cleaner themes
  filter(str_length(word) > 2, !str_detect(word, "^[0-9]+$"))

# 3. Setup Holding Dataframe
annual_topics_table <- data.frame(
  Year = integer(),
  Topic = integer(),
  Top_Terms = character(),
  stringsAsFactors = FALSE
)

# 4. Iterate over Years
years <- sort(unique(cnn_tokens$year))

cat("\nBeginning Isolated Annual LDA Iterations...\n")
pb <- txtProgressBar(min = 0, max = length(years), style = 3)

for (i in seq_along(years)) {
  current_year <- years[i]
  
  # Extract just the data for this specific year
  year_tokens <- cnn_tokens %>%
    filter(year == current_year)
  
  # Ensure there is enough text volume in the year to actually fit a model
  # Dropped from 10 to 2 per your request to include low-density years
  doc_counts <- n_distinct(year_tokens$id)
  if(doc_counts < 2) {
    setTxtProgressBar(pb, i)
    next
  }
  
  # Create localized Document Term Matrix
  dtm_year <- year_tokens %>%
    count(id, word) %>%
    cast_dtm(id, word, n)
  

  # Set K=2 (LDA mathematically requires a minimum of 2 clusters to run)
  k_val <- 2
  
  if (nrow(dtm_year) < 2) {
    setTxtProgressBar(pb, i)
    next
  }
  
  # Train isolated model
  lda_year <- LDA(dtm_year, k = k_val, method = "Gibbs", control = list(seed = 42, iter = 500))
  
  # Find the mathematical "Winner" (the topic most common across all documents this year)
  gamma_year <- tidy(lda_year, matrix = "gamma")
  winning_topic <- gamma_year %>%
    group_by(topic) %>%
    summarize(mean_gamma = mean(gamma), .groups = "drop") %>%
    slice_max(mean_gamma, n = 1, with_ties = FALSE) %>%
    pull(topic)
  
  # Extract the Top 5 unique terms ONLY for the winning topic
  beta_topics <- tidy(lda_year, matrix = "beta")
  top_terms <- beta_topics %>%
    filter(topic == winning_topic) %>%
    slice_max(beta, n = 5, with_ties = FALSE) %>%
    summarize(terms = paste(term, collapse = ", "), .groups = "drop")
    
  # Append results (Mapping it all to "Topic 1" for clean formatting)
  year_results <- data.frame(
    Year = current_year,
    Topic = 1, 
    Top_Terms = top_terms$terms
  )
  
  annual_topics_table <- bind_rows(annual_topics_table, year_results)
  setTxtProgressBar(pb, i)
}
close(pb)


# 5. Output elegant wide table grouped by year
wide_annual_table <- annual_topics_table %>%
  pivot_wider(names_from = Topic, values_from = Top_Terms, names_prefix = "Topic ") %>%
  arrange(Year)

print(wide_annual_table)

write.csv(annual_topics_table, "LDA_topics.csv")
