# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# load NBC
NBC_df <- read.csv("7_NBC/NBC_raw.csv")
NBC_df <- NBC_df %>% select(-author, -url)
NBC_df <- NBC_df %>% rename("date" = "date_published")
NBC_df <- NBC_df %>% mutate(date = mdy(date))

NBC_df2 <- read.csv("NBC_sup.csv")
NBC_df2 <- NBC_df2  %>% rename("date" = "publish_date",
                               "text" = "full_article")

NBC <- rbind(NBC_df, NBC_df2)


# add missing dates

NBC$date[689] <- "2023-12-14"

NBC$date[770] <- "2024-06-10"

NBC$date[952] <- "2023-11-02"

# remove blanks
NBC <- NBC %>%
  filter(row_number() != 479)

NBC <- NBC  %>%
  filter(row_number() != 943)

NBC <- NBC %>%
  filter(row_number() != 1296)

NBC <- NBC %>%
  filter(row_number() != 2101)

write.csv(NBC, "5_MSNBC/filtered_NBC.csv")

NBC <- read.csv("7_NBC/filtered_NBC.csv")


# add source and medium
NBC$source <- "NBC"
NBC$medium <- "article"


# remove date_published 
NBC <- NBC  %>% select (-Unnamed..0)


# relocate source medium to front
NBC <- NBC  %>%
  relocate(date)

NBC <- NBC  %>%
  relocate(medium)

NBC <- NBC  %>%
  relocate(source)

#count AI mentions and save in column

ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"

NBC <- NBC  %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )

# remove all 0 mentions of AI
NBC <- NBC  %>%
  filter(AI_mentions != 0)

# save 
write_csv(NBC, "master_df_NBC.csv")


