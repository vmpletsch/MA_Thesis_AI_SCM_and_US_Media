library(tidyverse)
library(reticulate)

# Ensure the environment is loaded if running separately
use_condaenv("bert_r") 
transformers <- import("transformers")
tokenizer <- transformers$BertTokenizer$from_pretrained('bert-base-uncased')
model <- transformers$BertModel$from_pretrained('bert-base-uncased')

# Load the embeddings dataframe
cnn_df <- readRDS("/Users/veronicapletsch/Thesis/cnn_BERT_embeddings.rds")

# Extract the word embeddings from the BERT model
word_embeddings <- model$embeddings$word_embeddings$weight$detach()$numpy()
vocab <- tokenizer$get_vocab()

# Create a lookup table of words
# The vocab is a list of word:id mappings. Since the ID corresponds to the 
# zero-indexed row of the word_embeddings matrix, we must sort the names by ID.
vocab_words <- names(sort(unlist(vocab)))

# Pre-normalize the word embedding matrix to make cosine similarity calculations lightning fast
norm_words <- sqrt(rowSums(word_embeddings^2))
norm_words[norm_words == 0] <- 1e-10
normalized_word_embeddings <- word_embeddings / norm_words

# Define a function to get top N nearest words for any 768-dimension vector
get_nearest_words <- function(target_vec, n = 15) {
  # Handle NAs or empty vectors
  if (is.null(target_vec) || sum(target_vec^2) == 0) return(NA_character_)
  
  # Normalize the target vector
  norm_target <- sqrt(sum(target_vec^2))
  target_vec <- target_vec / norm_target
  
  # Calculate cosine similarity: (normalized words) dot (normalized target vector)
  sims <- as.vector(normalized_word_embeddings %*% target_vec)
  
  # Order the similarities descending
  top_indices <- order(sims, decreasing = TRUE)[1:n]
  
  # Get the actual words
  top_words <- vocab_words[top_indices]
  
  # Filter out special BERT tokens ([PAD], [CLS], [SEP], etc.) to keep words clean
  top_words <- top_words[!grepl("^\\[.*\\]$", top_words)]
  
  # Return as a single comma-separated string
  return(paste(top_words, collapse = ", "))
}


# --- 1. CENTROIDS FIRST ---
# Group vec by year and calculate centroid
yearly_centroids <- cnn_df %>%
  group_by(year) %>%
  summarize(centroid = list(colMeans(do.call(rbind, vec), na.rm = TRUE))) %>%
  ungroup()

# Find nearest words for each year's centroid
yearly_centroids <- yearly_centroids %>%
  mutate(top_words = map_chr(centroid, ~get_nearest_words(.x, n = 15)))

print("Top words for Yearly Centroids:")
print(yearly_centroids %>% select(year, top_words))


# --- 2. INDIVIDUAL ARTICLES ---
# Apply the function across each individual vector in cnn_df
# Using .progress = TRUE so you can see how long it takes
cnn_df <- cnn_df %>%
  mutate(nearest_words = map_chr(vec, ~get_nearest_words(.x, n = 10), .progress = TRUE))

# Save your final data containing the embeddings, sentiments, and words
saveRDS(cnn_df, "/Users/veronicapletsch/Thesis/cnn_BERT_embeddings_with_words.rds")

# Save a lightweight CSV version (without the 'vec' column) so it can open safely!
cnn_df %>%
  select(-vec) %>%
  write.csv("/Users/veronicapletsch/Thesis/cnn_BERT_words_results.csv", row.names = FALSE)

print("Done! Saved to cnn_BERT_embeddings_with_words.rds and cnn_BERT_words_results.csv")

# Open visual spreadsheet view safely (dropping the giant "vec" column here so RStudio doesn't crash from the size)
View(yearly_centroids %>% select(-centroid), "Yearly Centroids")
View(cnn_df %>% select(-vec), "CNN Articles with Words")



cnn_bert <- read.csv("cnn_BERT_words_results.csv")