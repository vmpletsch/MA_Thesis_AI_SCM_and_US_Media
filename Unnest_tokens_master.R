library(tidytext)
library(tidyverse)
library(stringr)
library(quanteda)

unnest_df <- read.csv("9_Final_DFs/master_all_df.csv") 

# Build a corpus from your master_df (one row per document, with text column)
master_all_df$id <- seq_len(nrow(master_all_df))
master_all_df <- master_all_df %>% select(-X)

master_all_df <- master_all_df %>%
  mutate(text = str_replace_all(text, regex("A\\.I\\.?", ignore_case = TRUE), "AI"))

corp <- corpus(master_all_df, text_field = "text", docid_field = "id")

toks <- tokens(corp, remove_punct = TRUE)


kwic_results <- kwic(
  toks,
  pattern = phrase(c("artificial intelligence", "AI")),
  window = 100,
  case_insensitive = TRUE
)

write.csv(kwic_results, "kwic.csv")


# Convert to a dataframe and reconstruct context windows
context_windows <- kwic_results %>%
  as_tibble() %>%
  mutate(
    context_clip = str_c(pre, keyword, post, sep = " "),
    id = as.integer(docname)   # convert character to integer
  ) %>%
  select(id, ai_index = from, context_clip) %>%
  left_join(
    master_all_df %>% select(id, source, medium, date, title, AI_mentions),
    by = "id"
  )

write.csv(context_windows, "context_window.csv")

str(context_windows)


context_windows <- context_windows %>%
  mutate(medium = case_when(
    source %in% c("NYT", "WSJ", "NBC") ~ "print",
    source %in% c("CNN", "MSNBC", "FOX") ~ "broadcast",
    TRUE ~ NA_character_
  ))


context_windows <- context_windows %>%
  mutate(political_orientation = case_when(
    source %in% c("NYT", "MSNBC") ~ "left",
    source %in% c("CNN", "NBC") ~ "center",
    source %in% c("WSJ", "FOX") ~ "right",
    TRUE ~ NA_character_
  ))


context_windows <- context_windows %>%
  mutate(date = ymd(date),
         year = year(date))

#relabel 
master_all_df <- master_all_df %>%
  mutate(source = str_to_upper(source))


context_windows <- context_windows %>%
  mutate(source = str_to_upper(source))

unique(master_all_df$source)


unique(context_windows$source)



