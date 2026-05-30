library(reticulate)
library(topicdoc)
library(wordcloud2)
library(SnowballC)
library(ggplot2)
library(gt)
library(cld3)
library(topicmodels)
library(textmineR)
library(tidytext)
library(dplyr)

msnbc_df <- read.csv("master_msnbc_df.csv")


data(stop_words)

# Add custom domain stop words so the model doesn't just cluster on your search terms!
custom_stop_words <- bind_rows(
  stop_words,
  data.frame(word = c("ai", "a.i", "artificial", "intelligence", 
                      "cnn", "news", "video", "footage", "clip", "host",
                      "anchor", "correspondent", "story", "report", "israel", "joe", "o'sullivan"), 
             lexicon = "custom")
)

msnbc_df <- cnn_df %>%
  unnest_tokens(word, context_clip)
  
cnn_df <- cnn_df %>%
  anti_join(custom_stop_words, by = "word")


#create DTM then run LDA
dtm <- cnn_df %>%
  count(id, word) %>%
  cast_dtm(id, word, n)
#trying to replicate the same number of the original study's key terms so k=30
lda_model <- LDA(dtm, k = 30,
                 method = "Gibbs",
                 control = list(
                   seed = 42,
                   iter = 1000,
                   verbose = 1))



beta_topics <- tidy(lda_model, matrix = "beta")
#fina gamma
gamma_topics <- tidy(lda_model, matrix = "gamma")


top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)
print(top_terms)


term_totals <- top_terms %>%
  group_by(topic) %>%
  summarize(total_score = sum(beta))
final_table <- top_terms %>%
  mutate(term_with_score = paste0(term, " (", round(beta, 2), ")")) %>%
  group_by(topic) %>%
  summarize(Keywords = paste(term_with_score, collapse = ", ")) %>%
  left_join(term_totals, top_terms, by = "topic")


ks <- 2:31
# Set up progress bar
cat("Calculating Perplexity for k = 2 to 31...\n")
pb <- txtProgressBar(min = 0, max = length(ks), style = 3)

perplexity_values <- numeric(length(ks))
for (i in seq_along(ks)) {
  k_val <- ks[i]
  temp_model <- LDA(dtm, k = k_val,
                    method = "Gibbs",
                    control = list(alpha = 0.01, seed = 42))
                    
  perplexity_values[i] <- perplexity(temp_model, newdata = dtm)
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

#turn into dataframe
perplexity_table <- data.frame(
  Model = "LDA",
  Topics_K = ks,
  Perplexity = perplexity_values
)

#calculate perplexity and coherence score for the k=30 model we already fit!
cat("\nCalculating Perplexity for k=30... (this should take <1 minute)\n")
single_perplexity <- perplexity(lda_model, newdata = dtm)

cat("\nCalculating Coherence Scores for k=30... (this should take <1 minute)\n")
coherence_scores <- topic_coherence(lda_model, dtm)
mean_coherence <- mean(coherence_scores, na.rm = TRUE)

# Create a table specifically comparing perplexity and coherence for the 30-topic model
perp_coh_table <- data.frame(
  Topic = 30,
  Perplexity = single_perplexity,
  Coherence = mean_coherence
)

print(perp_coh_table)


cloud_data <- beta_topics %>%
  filter(topic == 8) %>%
  mutate(freq = beta * 1000) %>%
  select(term, freq)
wordcloud2(cloud_data)


# ---------------------------------------------------------
# Diachronic LDA Analysis (Topics over time)
# ---------------------------------------------------------
library(lubridate)

# Extract distinct documents and their dates from the dataframe
doc_metadata <- cnn_df %>%
  select(id, date) %>%
  distinct() %>%
  mutate(year = year(as.Date(date)))

# Map documents back to their years and calculate average gamma per topic per year
diachronic_lda <- gamma_topics %>%
  mutate(id = as.integer(document)) %>%
  inner_join(doc_metadata, by = "id") %>%
  group_by(year, topic) %>%
  summarize(mean_gamma = mean(gamma), .groups = "drop") %>%
  # Filter down to only the 1 most prevalent topic per year
  group_by(year) %>%
  slice_max(mean_gamma, n = 1, with_ties = FALSE) %>%
  ungroup()

# Create labels for each topic using their top 3 terms
top_terms_labels <- top_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 3) %>%
  summarize(terms = paste(term, collapse = ", "), .groups = "drop") %>%
  mutate(topic_label = paste0("T", topic, ": ", terms))

diachronic_lda <- diachronic_lda %>%
  left_join(top_terms_labels, by = "topic")

# Plot the dominant topic over time
diachronic_plot <- ggplot(diachronic_lda, aes(x = as.factor(year), y = mean_gamma, fill = topic_label)) +
  geom_col(color = "black") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Dominant LDA Topic per Year",
       x = "Year",
       y = "Dominant Topic Proportion (Mean Gamma)",
       fill = "Topic")

# You can display the plot or save it
print(diachronic_plot)
# ggsave("diachronic_topics_over_time.png", diachronic_plot, width = 16, height = 10, bg = "white")