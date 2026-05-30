library(reticulate)
library(tidyverse)

# 1. Setup Environment
conda_remove("bert_r")

# 2. REBUILD it with a specific Python version (3.10 is safest)
conda_create("bert_r", python_version = "3.10")

# 3. INSTALL the "Golden Trio" of versions that definitely work together
# We are pinning huggingface_hub to 0.24.0 (the last version before the break)
py_install(c("huggingface_hub==0.24.0", "transformers==4.40.0", "bertopic"), 
           envname = "bert_r", 
           pip = TRUE)

bertopic <- import("bertopic")

# 2. Initialize Model
# Note: calculate_probabilities is great, but very memory intensive for 8k+ rows.
# If your computer lags, set it to FALSE.
topic_model <- bertopic$BERTopic(
  language = "english", 
  calculate_probabilities = TRUE,
  verbose = TRUE
)

# 3. Fit Model
# Ensure 'docs' is a character vector of your text (cnn_df$context_clip)
cnn_df <- read.csv("cnn_BERT_df.csv")

docs <- as.character(cnn_df$context_clip)
timestamps <- as.integer(substr(cnn_df$date, 1, 4)) # Extract year from the date column

# Fit and Transform
results <- topic_model$fit_transform(docs)

# BERTopic returns a list in R: [[1]] is topics, [[2]] is probabilities
topics <- results[[1]]
probs <- results[[2]]

# 4. Diachronic Analysis (The "Over Time" part)
# nr_bins = 25L matches your 25-year span perfectly (1 bin per year)
topics_over_time_py <- topic_model$topics_over_time(
  docs = docs, 
  timestamps = timestamps, 
  nr_bins = 25L
)

# 5. Bring to R for Visualization
# py_to_r converts the Python DataFrame to an R Tibble/DataFrame
dynamic_topics_r <- py_to_r(topics_over_time_py)


# ---------------------------------------------------------
# 6. Graphing Results
# ---------------------------------------------------------

# Option A: Native R Visualization using ggplot2 (Static & Customizable)
# Filter out the "-1" outlier topic, and select the top 5 most frequently discussed topics
graph_data <- dynamic_topics_r %>%
  filter(Topic != -1)

# Find the top 5 topics overall to prevent extreme clutter
top_5_topics <- graph_data %>%
  group_by(Topic) %>%
  summarize(total = sum(Frequency)) %>%
  slice_max(order_by = total, n = 5) %>%
  pull(Topic)

# Graph the trend using ggplot2
ggplot_data <- graph_data %>%
  filter(Topic %in% top_5_topics) %>%
  # Truncate words for a cleaner legend
  mutate(Topic_Label = paste0("Topic ", Topic, ": ", substr(Words, 1, 30), "..."))

ggplot(ggplot_data, aes(x = Timestamp, y = Frequency, color = as.factor(Topic_Label))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "BERTopic: Topic Evolution Over Time (Top 5 Topics)",
    x = "Year",
    y = "Document Frequency",
    color = "Topic Overview"
  ) +
  theme(legend.position = "bottom", legend.direction = "vertical")


# Option B: BERTopic's built-in interactive graph (Hoverable HTML)
# This will render an interactive Plotly HTML widget.
interactive_fig <- topic_model$visualize_topics_over_time(
  topics_over_time_py, 
  top_n_topics = 10L
)
interactive_fig$show()
