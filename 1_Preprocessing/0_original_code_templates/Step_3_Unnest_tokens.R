library(tidytext)
library(tidyverse)
library(stringr)

#df 1
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_1 <- read.csv("relevant_transcripts_df_1.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df <- unnest_df_1 %>%
  mutate(
    # Re-apply the specific regex to match what we filtered by
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions <- word_df %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows <- word_df %>%
  inner_join(ai_positions, by = "id", relationship = "many-to-many") %>%
  # Filter for words strictly within 200 positions before or after the match (400 words total)
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  # Revert the proxy token back to its original format matching the `match_type` variable
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  # Reconstruct the text into a 400-word string for each occurrence
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")

# Optional: View the first couple of clips
head(context_windows)

write_csv(context_windows, "unnested_df_1.csv")



#df 2
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_2 <- read.csv("relevant_transcripts_df_2.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_2 <- unnest_df_2 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_2 <- word_df_2 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_2 <- word_df_2 %>%
  inner_join(ai_positions_2, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")

context_windows_2 <- context_windows_2 %>%
  filter(!id %in% c("1", "2", "5", "10", "11", "12", "157", "415", "1840"))

write_csv(context_windows_2, "unnested_df_2.csv")



#df 3
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_3 <- read.csv("relevant_transcripts_df_3.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_3 <- unnest_df_3 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_3 <- word_df_3 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_3 <- word_df_3 %>%
  inner_join(ai_positions_3, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")

context_windows_3 <- context_windows_3 %>%
  filter(id != "4")

write_csv(context_windows_3, "unnested_df_3.csv")




#df 4
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_4 <- read.csv("relevant_transcripts_df_4.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_4 <- unnest_df_4 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_4 <- word_df_4 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_4 <- word_df_4 %>%
  inner_join(ai_positions_4, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")

context_windows_4 <- context_windows_4 %>%
  filter(!id %in% c("16", "5", "8", "7"))

write_csv(context_windows_4, "unnested_df_4.csv")



#df 5
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_5 <- read.csv("relevant_transcripts_df_5.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_5 <- unnest_df_5 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_5 <- word_df_5 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_5 <- word_df_5 %>%
  inner_join(ai_positions_5, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")


write_csv(context_windows_5, "unnested_df_5.csv")




#df 6
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_6 <- read.csv("relevant_transcripts_df_6.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_6 <- unnest_df_6 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_6 <- word_df_6 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_6 <- word_df_6 %>%
  inner_join(ai_positions_6, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")


write_csv(context_windows_6, "unnested_df_6.csv")





#df 7
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_7 <- read.csv("relevant_transcripts_df_7.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_7 <- unnest_df_7 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_7 <- word_df_7 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_7 <- word_df_7 %>%
  inner_join(ai_positions_7, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")


context_windows_7 <- context_windows_7 %>%
  filter(!id %in% c("148", "204", "207", "214", "215", "154", "153", "150", "149", "151", 
                    "198", "199", "206", "210", "212", "230", "233", "197", "231", "218", 
                    "217", "211", "232", "196", "227", "226", "216"))

write_csv(context_windows_7, "unnested_df_7.csv")



#df 8
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_8 <- read.csv("relevant_transcripts_df_8.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_8 <- unnest_df_8 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_8 <- word_df_8 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_8 <- word_df_8 %>%
  inner_join(ai_positions_8, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")


write_csv(context_windows_8, "unnested_df_8.csv")


#df 9 msnbc
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_msnbc <- read.csv("relevant_transcripts_df_MSNBC.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_9 <- unnest_df_msnbc %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_9 <- word_df_9 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_9 <- word_df_9 %>%
  inner_join(ai_positions_9, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")


write_csv(context_windows_9, "unnested_df_MSNBC.csv")






#df 10 fox
# 1. Load data and add an 'id' column since the CSV doesn't have one
unnest_df_11 <- read.csv("relevant_transcripts_df_FOX.csv") %>%
  mutate(id = row_number())

# 2. Safely capture phrases by temporarily swapping the targeted regex with a proxy word
# This handles both multi-word phrases (like "Artificial Intelligence") and abbreviation punctuation (like "A.I.")
word_df_11 <- unnest_df_11 %>%
  mutate(
    regex_pattern = case_when(
      match_type == "Artificial Intelligence" ~ "artificial intelligence",
      match_type == "AI/A.I." ~ "\\bAI\\b|\\bA\\.I\\.?",
      match_type == "Machine Learning" ~ "machine learning",
      match_type == "GenAI" ~ "GenAI",
      match_type == "AGI" ~ "\\bAGI\\b",
      TRUE ~ "NOMATCH___"
    ),
    # Swap out the match with "aitargetmatchtoken" so unnest_tokens doesn't break it
    transcript_proxy = str_replace_all(
      string = transcript, 
      pattern = regex(regex_pattern, ignore_case = TRUE), 
      replacement = " aitargetmatchtoken "
    )
  ) %>%
  # 3. Break into tokens using the proxy transcript
  group_by(id, source, date, match_type) %>% 
  unnest_tokens(word, transcript_proxy) %>%
  mutate(word_index = row_number()) %>%
  ungroup()

# 4. Identify the positions where our target token appears
ai_positions_11 <- word_df_11 %>%
  filter(word == "aitargetmatchtoken") %>%
  select(id, ai_index = word_index)

# 5. Join positions back and filter for the 200-word window
context_windows_11 <- word_df_11 %>%
  inner_join(ai_positions_11, by = "id", relationship = "many-to-many") %>%
  filter(word_index >= (ai_index - 50) & word_index <= (ai_index + 50)) %>%
  mutate(word = ifelse(word == "aitargetmatchtoken", match_type, word)) %>%
  group_by(id, source, date, match_type, ai_index) %>%
  summarize(context_clip = str_c(word, collapse = " "), .groups = "drop")


write_csv(context_windows_11, "unnested_df_FOX.csv")










