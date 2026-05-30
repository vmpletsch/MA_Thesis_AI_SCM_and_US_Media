# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)


fox <- read.csv("4_FOX/fox_df.csv")
fox$title <- "NA"

fox <- fox %>%
  relocate(title)

fox <- fox %>%
  relocate(date)

fox <- fox %>%
  relocate(source)

# supplemental df 1
fox_sup <- read.csv("4_FOX/supp_fox_raw.csv")
fox_sup$source <- "foxnews"
fox_sup <- fox_sup %>% rename("transcript" = "body",
                              "date" = "publish_date")
fox_sup <- fox_sup %>%
  mutate(
    date = mdy(date))

fox_sup <- fox_sup %>%
  filter(year(as.Date(date)) != 2025)

fox_sup <- fox_sup %>%
  filter(year(as.Date(date)) != 2026)

fox_sup <- fox_sup %>%
  filter(year(as.Date(date)) != 1999)

fox_sup <- fox_sup %>%
  relocate(title)

fox_sup <- fox_sup %>%
  relocate(date)

fox_sup <- fox_sup %>%
  relocate(source)

# supplemental df 2
fox_sup_2 <- read.csv("4_FOX/supp_fox.csv")
fox_sup_2$source <- "foxnews"
fox_sup_2 <- fox_sup_2 %>% rename("transcript" = "body",
                              "date" = "publish_date")
fox_sup_2 <- fox_sup_2 %>% 
  filter(year(as.Date(date)) != 2025)

fox_sup_2 <- fox_sup_2 %>% 
  filter(year(as.Date(date)) != 2026)

fox_sup_2 <- fox_sup_2 %>% 
  filter(year(as.Date(date)) != 1999)

fox_sup_2  <- fox_sup_2  %>%
  relocate(title)

fox_sup_2  <- fox_sup_2  %>%
  relocate(date)

fox_sup_2  <- fox_sup_2  %>%
  relocate(source)


# combine all
fox_sup_2 <- fox_sup_2 %>% mutate(date = as.character(date))
fox_sup   <- fox_sup   %>% mutate(date = as.character(date))
fox       <- fox       %>% mutate(date = as.character(date))
fox_df <- bind_rows(fox, fox_sup, fox_sup_2)

write_csv(fox_df, "4_FOX/raw_combined_df_FOX.csv")


# load the raw fox df
fox_df <- read.csv("4_FOX/raw_combined_df_FOX.csv")

# check for AI mention
ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"
# count mentions 
fox_df <- fox_df %>%
  mutate(
    AI_mentions = str_count(transcript, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
fox_df <- fox_df %>%
  filter(AI_mentions != 0)

fox_df <- unique(fox_df)

# add medium
fox_df$medium <- "broadcast"


write_csv(fox_df, "master_df_FOX.csv")