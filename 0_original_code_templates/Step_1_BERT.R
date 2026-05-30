library(tidyverse)
library(lubridate)
library(tidytext)
library(purrr)

# 1. Before touching BERT, you must structure your time data. For a 25-year span, "Year" or "Quarter" is usually the best level of analysis.
# 1. Format dates and create year "bins"

cnn_df <- read.csv("cnn_BERT_df.csv")

cnn_df <- cnn_df %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  filter(!is.na(context_clip))



#Step 4: Generating Embeddings by Time Slice

# 1. Setup
library(reticulate)
use_condaenv("bert_r")

# Import both at the start
transformers <- import("transformers")
torch <- import("torch") 

tokenizer <- transformers$BertTokenizer$from_pretrained('bert-base-uncased')
model <- transformers$BertModel$from_pretrained('bert-base-uncased')

sentiment_analyzer <- transformers$pipeline("sentiment-analysis")

get_embedding <- function(text) {
  # Handle NAs safely
  if (is.na(text)) return(rep(0, 768))
  
  # Tokenize and convert to tensor
  inputs <- tokenizer(text, return_tensors = "pt", truncation = TRUE, padding = TRUE, max_length = 512L)
  
  # Run through BERT with attention mask (no gradient calculation saves memory)
  with(torch$no_grad(), {
    outputs <- model(inputs$input_ids, attention_mask = inputs$attention_mask)
  })
  
  # Use the 'Pooler Output' and cast to a 1D numeric vector instead of a 2D matrix
  embeddings <- as.vector(outputs$pooler_output$numpy())
  return(embeddings)
}

get_sentiment <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  # Truncation prevents errors if text is too long
  res <- sentiment_analyzer(text, truncation = TRUE, max_length = 512L)
  return(as.character(res[[1]]$label)) # Returns "POSITIVE" or "NEGATIVE"
}

#Step 5: calculate embeddings 
# 1. Map the embeddings ONCE using the correct column 'context_clip' instead of 'text'
cnn_df <- cnn_df %>%
  mutate(
    vec = map(context_clip, ~{
      v <- get_embedding(.x)
      return(as.numeric(v)) # Force numeric double here
    }, .progress = TRUE),
    sentiment = map_chr(context_clip, ~get_sentiment(.x), .progress = TRUE)
  )

# (Optional) Retain embedding index tracker
cnn_df$embedding_index <- 1:nrow(cnn_df)

# 2. Calculate the "Average Meaning" (Centroid) per year
yearly_centroids <- cnn_df %>%
  group_by(year) %>%
  summarize(centroid = list(colMeans(do.call(rbind, vec)))) %>%
  ungroup()

# 3. Calculate Cosine Similarity compared to a "Base Year"
# Define our own cosine logic since the 'lsa' package is not installed on your system!
cosine_sim <- function(a, b) sum(a * b) / sqrt(sum(a^2) * sum(b^2))

base_vector <- yearly_centroids$centroid[[1]] # Year 1999

yearly_centroids <- yearly_centroids %>%
  mutate(similarity_to_start = map_dbl(centroid, ~cosine_sim(.x, base_vector)))

# This preserves your vectors as numeric
saveRDS(cnn_df, "cnn_BERT_embeddings.rds")

cnn_df <- readRDS("cnn_BERT_embeddings.rds")


