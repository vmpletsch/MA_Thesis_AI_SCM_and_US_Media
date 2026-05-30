library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(dplyr)
library(stringr)


# cnn6_df
cnn_test_6 <- read.csv("cnn6_df.csv")

# Define filter terms 
ai_pattern <- "artificial intelligence|\\bAI\\b|A\\.I\\.?|machine learning|\\bML\\b|GenAI|algorithm|neural net|\\bAGI\\b"

relevant_transcripts_df_6 <- cnn_test_6 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_6 <- relevant_transcripts_df_6 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_6, "relevant_transcripts_df_6.csv")

# cnn_df
cnn_test <- read.csv("cnn_df.csv")

relevant_transcripts_df_1 <- cnn_test %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_1 <- relevant_transcripts_df_1 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_1, "relevant_transcripts_df_1.csv")

# cnn2_df
cnn_test_2 <- read.csv("cnn2_df.csv")

relevant_transcripts_df_2 <- cnn_test_2 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_2 <- relevant_transcripts_df_2 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_2, "relevant_transcripts_df_2.csv")

# cnn3_df
cnn_test_3 <- read.csv("cnn3_df.csv")

relevant_transcripts_df_3 <- cnn_test_3 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_3 <- relevant_transcripts_df_3 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_3, "relevant_transcripts_df_3.csv")

# cnn4_df
cnn_test_4 <- read.csv("cnn4_df.csv")

relevant_transcripts_df_4 <- cnn_test_4 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))


relevant_transcripts_df_4 <- relevant_transcripts_df_4 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_4, "relevant_transcripts_df_4.csv")


# cnn5_df
cnn_test_5 <- read.csv("cnn5_df.csv")

relevant_transcripts_df_5 <- cnn_test_5 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))


relevant_transcripts_df_5 <- relevant_transcripts_df_5 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_5, "relevant_transcripts_df_5.csv")

# cnn7_df
cnn_test_7 <- read.csv("cnn7_df.csv")

relevant_transcripts_df_7 <- cnn_test_7 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_7 <- relevant_transcripts_df_7 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_7, "relevant_transcripts_df_7.csv")

# cnn8_df
cnn_test_8 <- read.csv("cnn8_df.csv")

relevant_transcripts_df_8 <- cnn_test_8 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_8 <- relevant_transcripts_df_8 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_8, "relevant_transcripts_df_8.csv")





# msnbc_df
msnbc_1 <- read.csv("master_msnbc_df.csv")

relevant_transcripts_df_9 <- msnbc_1 %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_9 <- relevant_transcripts_df_9 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_9, "relevant_transcripts_df_9.csv")


# fox_df
fox <- read.csv("fox_df.csv")

relevant_transcripts_df_10 <- fox %>%
  filter(str_detect(transcript, regex(ai_pattern, ignore_case = TRUE)))

relevant_transcripts_df_10 <- relevant_transcripts_df_10 %>%
  mutate(match_type = case_when(
    str_detect(transcript, regex("artificial intelligence", ignore_case = TRUE)) ~ "Artificial Intelligence",
    str_detect(transcript, regex("\\bAI\\b|A\\.I\\.?", ignore_case = TRUE)) ~ "AI/A.I.",
    str_detect(transcript, regex("machine learning", ignore_case = TRUE)) ~ "Machine Learning",
    str_detect(transcript, regex("GenAI", ignore_case = TRUE)) ~ "GenAI",
    str_detect(transcript, regex("neural net", ignore_case = TRUE)) ~ "neural net",
    str_detect(transcript, regex("\\bAGI\\b", ignore_case = TRUE)) ~ "AGI",
    TRUE ~ "Other/No Match" # This catches anything else
  )) %>%
  # Keep only the rows that matched one of our terms
  filter(match_type != "Other/No Match")

write_csv(relevant_transcripts_df_10, "relevant_transcripts_df_10.csv")


fox_df <- read.csv("unnested_df_FOX.csv")


