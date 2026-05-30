library(text2vec)
library(tokenizers)
library(tidyverse)
library(tidytext)
library(ggrepel)
library(word2vec)
library(uwot)


# Load the master dataset
articles <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")

# Ensure an article_id exists for tracking
if (!"article_id" %in% names(articles)) {
  if ("id" %in% names(articles)) {
    articles <- articles %>% rename(article_id = id)
  } else {
    articles <- articles %>% mutate(article_id = row_number())
  }
}

# Ensure date is in proper format
articles$date <- as.Date(articles$date)

# Preprocessing function for text corpus
preprocess_corpus <- function(text_vector) {
  text_vector %>%
    tolower() %>%
    str_replace_all("http\\S+|www\\S+", "") %>%          # Remove URLs
    str_replace_all("\\S+@\\S+", "") %>%                # Remove emails
    str_replace_all("[^a-z0-9\\s-]", " ") %>%           # Keep only alphanumeric and hyphens
    str_replace_all("\\s+", " ") %>%                    # Replace multiple spaces
    str_trim()                                          # Trim leading/trailing whitespace
}

# Apply preprocessing
articles <- articles %>%
  mutate(text_clean = preprocess_corpus(text))


# Detect common phrases using word2vec to create n-grams (e.g., artificial_intelligence)
writeLines(articles$text_clean, "ai_corpus.txt")

phrases <- word2vec_phrases(
  x = "ai_corpus.txt",
  output = "ai_corpus_phrases.txt",
  min_count = 5,
  threshold = 10
)

corpus_with_phrases <- readLines("ai_corpus_phrases.txt")

# Tokenize corpus into words, excluding standard and domain-specific stopwords
tokens <- tokenizers::tokenize_words(
  corpus_with_phrases, 
  lowercase = FALSE,
  stopwords = c(stopwords::stopwords("en"), "said", "also", "would", "could")
)

# Create vocabulary iterator
it <- itoken(tokens, progressbar = TRUE, ids = articles$article_id)
vocab <- create_vocabulary(it)

# Prune vocabulary to remove excessive noise and overly common words
vocab_filtered <- prune_vocabulary(
  vocab,
  term_count_min = 10,
  doc_proportion_max = 0.5,
  doc_proportion_min = 0.001
)

cat(sprintf("Original vocabulary size: %d terms\n", nrow(vocab)))
cat(sprintf("Filtered vocabulary size: %d terms\n", nrow(vocab_filtered)))

# Create Vectorizer and Term Co-occurrence Matrix (TCM)
vectorizer <- vocab_vectorizer(vocab_filtered)
tcm <- create_tcm(
  it = itoken(tokens, progressbar = TRUE),
  vectorizer = vectorizer,
  skip_grams_window = 5,
  skip_grams_window_context = "symmetric",
  weights = c(1, 0.8, 0.6, 0.4, 0.2)
)


glove <- GlobalVectors$new(
  rank = 100,
  x_max = 10,
  learning_rate = 0.05,
  lambda = 0.0,
  shuffle = TRUE
)

# Fit the model
wv_main <- glove$fit_transform(
  tcm, 
  n_iter = 50,
  convergence_tol = 0.001,
  n_threads = 4
)

# Average the word vectors and context vectors
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)
rownames(word_vectors) <- vocab_filtered$term

saveRDS(word_vectors, "ai_media_embeddings_100d.rds")


# Define theoretical frames
innovation_frame <- c("innovation", "breakthrough", "advancement", "progress",
                      "revolutionary", "transformative", "improvement", "efficiency",
                      "optimization", "enhancement", "cutting_edge", "frontier",
                      "pioneering", "novel", "sophisticated")

threat_frame <- c("threat", "risk", "danger", "harmful", "dangerous",
                  "threatening", "unemployment", "displacement", "job_loss",
                  "replace", "eliminate", "control", "manipulation",
                  "surveillance", "bias", "discrimination", "inequality")

# Filter frames to words present in the vocabulary
innovation_frame <- innovation_frame[innovation_frame %in% rownames(word_vectors)]
threat_frame <- threat_frame[threat_frame %in% rownames(word_vectors)]

# Calculate frame vectors (centroid of constituent terms)
innovation_vector <- colMeans(word_vectors[innovation_frame, , drop = FALSE])
threat_vector <- colMeans(word_vectors[threat_frame, , drop = FALSE])

cosine_sim <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# Analyze how specific AI terms fall along the frames
ai_terms <- c("ai", "artificial_intelligence", "machine_learning", 
              "algorithm", "automation", "robot", "neural_network",
              "deep_learning", "chatgpt", "gpt")
ai_terms <- ai_terms[ai_terms %in% rownames(word_vectors)]

analyze_term_framing <- function(term, innovation_vec, threat_vec, embeddings) {
  term_vec <- embeddings[term, ]
  tibble(
    term = term,
    innovation_similarity = cosine_sim(term_vec, innovation_vec),
    threat_similarity = cosine_sim(term_vec, threat_vec),
    net_framing = cosine_sim(term_vec, innovation_vec) - cosine_sim(term_vec, threat_vec)
  )
}

ai_framing_results <- map_df(
  ai_terms, 
  ~analyze_term_framing(.x, innovation_vector, threat_vector, word_vectors)
)

# Visualize general framing
ai_framing_results %>%
  pivot_longer(c(innovation_similarity, threat_similarity),
               names_to = "frame",
               values_to = "similarity") %>%
  mutate(frame = str_remove(frame, "_similarity")) %>%
  ggplot(aes(x = reorder(term, net_framing), y = similarity, fill = frame)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("innovation" = "#134567", "threat" = "#c92042")) +
  labs(
    title = "AI Terms: Frame Positioning in Media Coverage",
    subtitle = sprintf("Based on %d news articles", nrow(articles)),
    x = "AI-related Term",
    y = "Cosine Similarity to Frame",
    fill = "Frame"
  )

ggsave("ai_framing_analysis.png", width = 10, height = 6, dpi = 300)


terms_to_plot <- c(
  ai_terms,
  innovation_frame[1:10],
  threat_frame[1:10],
  "positive", "negative", "beneficial", "harmful", "good", "bad",
  "employment", "productivity", "healthcare", "education"
)
terms_to_plot <- terms_to_plot[terms_to_plot %in% rownames(word_vectors)]

plot_embeddings <- word_vectors[terms_to_plot, ]

set.seed(42)
umap_coords <- umap(
  plot_embeddings,
  n_neighbors = 15,
  n_components = 2,
  metric = "cosine",
  min_dist = 0.1
)

plot_df <- as.data.frame(umap_coords) %>%
  setNames(c("UMAP1", "UMAP2")) %>%
  mutate(
    term = terms_to_plot,
    category = case_when(
      term %in% ai_terms ~ "AI Terms",
      term %in% innovation_frame ~ "Innovation Frame",
      term %in% threat_frame ~ "Threat Frame",
      TRUE ~ "Other"
    )
  )

ggplot(plot_df, aes(x = UMAP1, y = UMAP2, color = category)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(
    aes(label = term),
    size = 3.5,
    max.overlaps = 20,
    box.padding = 0.5,
    segment.color = "grey50"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(
    values = c(
      "AI Terms" = "#3498db",
      "Innovation Frame" = "#2ecc71",
      "Threat Frame" = "#e74c3c",
      "Other" = "#95a5a6"
    )
  ) +
  labs(
    title = "Semantic Space of AI Media Coverage",
    subtitle = "2D projection of 100-dimensional word embeddings",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Category"
  ) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("semantic_space_visualization.png", width = 12, height = 8, dpi = 300)


temporal_analysis <- articles %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year >= 2018 & year <= 2024) %>%
  group_by(year) %>%
  summarise(
    n_articles = n(),
    text_combined = list(text_clean)
  ) %>%
  filter(n_articles >= 100)

train_period_embeddings <- function(texts, rank = 100, n_iter = 30) {
  tokens <- tokenizers::tokenize_words(texts, lowercase = TRUE)
  it <- itoken(tokens)
  vocab <- create_vocabulary(it)
  vocab <- prune_vocabulary(vocab, term_count_min = 5)
  
  vectorizer <- vocab_vectorizer(vocab)
  it <- itoken(tokens)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
  glove <- GlobalVectors$new(rank = rank, x_max = 10)
  wv_main <- glove$fit_transform(tcm, n_iter = n_iter)
  wv_context <- glove$components
  word_vectors <- wv_main + t(wv_context)
  rownames(word_vectors) <- vocab$term
  
  return(word_vectors)
}

temporal_embeddings <- temporal_analysis %>%
  mutate(embeddings = map(text_combined, train_period_embeddings))

analyze_temporal_framing <- function(year_data, term = "ai") {
  embeddings <- year_data$embeddings[[1]]
  year <- year_data$year
  
  if (!term %in% rownames(embeddings)) return(NULL)
  
  innov_terms <- innovation_frame[innovation_frame %in% rownames(embeddings)]
  threat_terms <- threat_frame[threat_frame %in% rownames(embeddings)]
  
  if (length(innov_terms) < 3 | length(threat_terms) < 3) return(NULL)
  
  innov_vec <- colMeans(embeddings[innov_terms, , drop = FALSE])
  threat_vec <- colMeans(embeddings[threat_terms, , drop = FALSE])
  term_vec <- embeddings[term, ]
  
  tibble(
    year = year,
    innovation_sim = cosine_sim(term_vec, innov_vec),
    threat_sim = cosine_sim(term_vec, threat_vec),
    net_framing = cosine_sim(term_vec, innov_vec) - cosine_sim(term_vec, threat_vec)
  )
}

temporal_framing_results <- temporal_embeddings %>%
  rowwise() %>%
  summarise(analyze_temporal_framing(cur_data()))

# Visualize temporal trends
temporal_framing_results %>%
  pivot_longer(c(innovation_sim, threat_sim),
               names_to = "frame",
               values_to = "similarity") %>%
  mutate(frame = str_remove(frame, "_sim")) %>%
  ggplot(aes(x = year, y = similarity, color = frame, group = frame)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  theme_minimal(base_size = 14) +
  scale_color_manual(
    values = c("innovation" = "#2ecc71", "threat" = "#e74c3c"),
    labels = c("Innovation Frame", "Threat Frame")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "How AI Media Framing Changed Over Time",
    subtitle = "Semantic similarity of 'AI' to frame-defining terms",
    x = "Year",
    y = "Cosine Similarity",
    color = "Frame"
  ) +
  theme(legend.position = "bottom")

ggsave("temporal_framing_trends.png", width = 10, height = 6, dpi = 300)


# Analyze framing variations across distinct media sources
source_embeddings <- articles %>%
  filter(source %in% c("NYT", "WSJ", "CNN", "NBC", "MSNBC", "FOX")) %>%
  group_by(source) %>%
  filter(n() >= 100) %>%
  summarise(
    n_articles = n(),
    embeddings = list(train_period_embeddings(text_clean))
  )

source_framing <- source_embeddings %>%
  rowwise() %>%
  mutate(
    framing = list({
      emb <- embeddings[[1]]
      if (!"ai" %in% rownames(emb)) return(NULL)
      
      innov_terms <- innovation_frame[innovation_frame %in% rownames(emb)]
      threat_terms <- threat_frame[threat_frame %in% rownames(emb)]
      
      innov_vec <- colMeans(emb[innov_terms, , drop = FALSE])
      threat_vec <- colMeans(emb[threat_terms, , drop = FALSE])
      ai_vec <- emb["ai", ]
      
      tibble(
        innovation_sim = cosine_sim(ai_vec, innov_vec),
        threat_sim = cosine_sim(ai_vec, threat_vec)
      )
    })
  ) %>%
  unnest(framing)

# Visualize source comparison
source_framing %>%
  pivot_longer(c(innovation_sim, threat_sim),
               names_to = "frame",
               values_to = "similarity") %>%
  mutate(frame = str_remove(frame, "_sim")) %>%
  ggplot(aes(x = source, y = similarity, fill = frame)) +
  geom_col(position = "dodge", width = 0.7) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(
    values = c("innovation" = "#2ecc71", "threat" = "#e74c3c"),
    labels = c("Innovation Frame", "Threat Frame")
  ) +
  labs(
    title = "AI Framing by News Source",
    subtitle = "How different outlets position AI semantically",
    x = "News Source",
    y = "Cosine Similarity",
    fill = "Frame"
  ) +
  theme(legend.position = "bottom")

ggsave("source_comparison.png", width = 8, height = 6, dpi = 300)



# Define framing axis direction and evaluate projection of all terms
frame_axis <- innovation_vector - threat_vector

project_all_terms <- function(embeddings, axis) {
  projections <- apply(embeddings, 1, function(word_vec) {
    sum(word_vec * axis) / sqrt(sum(axis^2))
  })
  
  tibble(
    term = names(projections),
    projection = projections
  ) %>%
    arrange(desc(projection))
}

all_projections <- project_all_terms(word_vectors, frame_axis)

# Visualize term distribution across the theoretical framing axis
ggplot(all_projections, aes(x = projection)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(
    xintercept = all_projections$projection[all_projections$term == "ai"],
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = all_projections$projection[all_projections$term == "ai"],
    y = 50,
    label = "AI",
    color = "red",
    size = 5,
    hjust = -0.2
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Terms on Innovation-Threat Axis",
    subtitle = "Where does 'AI' fall in the media discourse?",
    x = "← Threat Frame | Innovation Frame →",
    y = "Number of Terms"
  )

ggsave("frame_axis_distribution.png", width = 10, height = 6, dpi = 300)