# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# individually load dfs 

# CNN df1
cnn1_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-1.csv")
cnn1_df <- cnn1_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn1_df <- cnn1_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn1_df <- cnn1_df %>% select (-year, -month, -date)
cnn1_df <- cnn1_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn1_df$source <- "CNN"
cnn1_df$medium <- "broadcast"
write.csv(cnn1_df, "3_CNN/filtered_CNN_df/cnn_df.csv")

# CNN df2
cnn2_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-2.csv")
cnn2_df <- cnn2_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn2_df <- cnn2_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn2_df <- cnn2_df %>% select (-year, -month, -date)
cnn2_df <- cnn2_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn2_df$source <- "CNN"
cnn2_df$medium <- "broadcast"
write.csv(cnn2_df, "3_CNN/filtered_CNN_df/cnn2_df.csv")


# CNN df3
cnn3_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-3.csv")
cnn3_df <- cnn3_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn3_df <- cnn3_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn3_df <- cnn3_df %>% select (-year, -month, -date)
cnn3_df <- cnn3_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn3_df$source <- "CNN"
cnn3_df$medium <- "broadcast"
write.csv(cnn3_df, "3_CNN/filtered_CNN_df/cnn3_df.csv")


# CNN df4
cnn4_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-4.csv")
cnn4_df <- cnn4_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn4_df <- cnn4_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn4_df <- cnn4_df %>% select (-year, -month, -date)
cnn4_df <- cnn4_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn4_df$source <- "CNN"
cnn4_df$medium <- "broadcast"
write.csv(cnn4_df, "3_CNN/filtered_CNN_df/cnn4_df.csv")


# CNN df5
cnn5_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-5.csv")
cnn5_df <- cnn5_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn5_df <- cnn5_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn5_df <- cnn5_df %>% select (-year, -month, -date)
cnn5_df <- cnn5_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn5_df$source <- "CNN"
cnn5_df$medium <- "broadcast"

write.csv(cnn5_df, "3_CNN/filtered_CNN_df/cnn5_df.csv")




# CNN df6
cnn6_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-6.csv")
cnn6_df <- cnn6_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn6_df <- cnn6_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn6_df <- cnn6_df %>% select (-year, -month, -date)
cnn6_df <- cnn6_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn6_df$source <- "CNN"
cnn6_df$medium <- "broadcast"
write.csv(cnn6_df, "3_CNN/filtered_CNN_df/cnn6_df.csv")



# CNN df7
cnn7_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-7.csv.gz")
cnn7_df <- cnn7_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn7_df <- cnn7_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn7_df <- cnn7_df %>% select (-year, -month, -date)
cnn7_df <- cnn7_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn7_df$source <- "CNN"
cnn7_df$medium <- "broadcast"
write.csv(cnn7_df, "3_CNN/filtered_CNN_df/cnn7_df.csv")



# CNN df8
cnn8_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-8.csv.gz")
cnn8_df <- cnn8_df  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
cnn8_df <- cnn8_df  %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn8_df <- cnn8_df %>% select (-year, -month, -date)
cnn8_df <- cnn8_df %>% rename ("date" = "date_obj",
                               "title" = "subhead")
cnn8_df$source <- "CNN"
cnn8_df$medium <- "broadcast"
write.csv(cnn8_df, "3_CNN/filtered_CNN_df/cnn8_df.csv")

#load dfs and combine 
cnn1_df <- read.csv("3_CNN/filtered_CNN_df/cnn_df.csv")
cnn2_df <- read.csv("3_CNN/filtered_CNN_df/cnn2_df.csv")
CNN1_df <- rbind(cnn1_df, cnn2_df)
rm(cnn1_df, cnn2_df)
write.csv(CNN1_df, "3_CNN/filtered_CNN_df/combined_CNN_1.csv")

cnn3_df <- read.csv("3_CNN/filtered_CNN_df/cnn3_df.csv")
cnn4_df <- read.csv("3_CNN/filtered_CNN_df/cnn4_df.csv")
CNN2_df <- rbind(cnn3_df, cnn4_df)
rm(cnn3_df, cnn4_df)
write.csv(CNN2_df, "3_CNN/filtered_CNN_df/combined_CNN_2.csv")


cnn5_df <- read.csv("3_CNN/filtered_CNN_df/cnn5_df.csv")
cnn6_df <- read.csv("3_CNN/filtered_CNN_df/cnn6_df.csv")
CNN3_df <- rbind(cnn5_df, cnn6_df)
rm(cnn5_df, cnn6_df)
write.csv(CNN3_df, "3_CNN/filtered_CNN_df/combined_CNN_3.csv")

cnn7_df <- read.csv("3_CNN/filtered_CNN_df/cnn7_df.csv")
cnn8_df <- read.csv("3_CNN/filtered_CNN_df/cnn8_df.csv")
CNN4_df <- rbind(cnn7_df, cnn8_df)
write.csv(CNN4_df, "3_CNN/filtered_CNN_df/combined_CNN_4.csv")

# load ai_pattern 
ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"

# load combined_CNN_1
cnn1_df <- read.csv("3_CNN/filtered_CNN_df/combined_CNN_1.csv")
cnn1_df <- cnn1_df %>% select(-X.1, -X)
cnn1_df <- cnn1_df %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
cnn1_df <- cnn1_df %>%
  filter(AI_mentions != 0)
write.csv(cnn1_df, "3_CNN/filtered_CNN_df/combined_filtered_CNN_1.csv")


# load combined_CNN_2
cnn2_df <- read.csv("3_CNN/filtered_CNN_df/combined_CNN_2.csv")
cnn2_df <- cnn2_df %>% select(-X.1, -X)
cnn2_df <- cnn2_df %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
cnn2_df <- cnn2_df %>%
  filter(AI_mentions != 0)
write.csv(cnn2_df, "3_CNN/filtered_CNN_df/combined_filtered_CNN_2.csv")


# load combined_CNN_3
cnn3_df <- read.csv("3_CNN/filtered_CNN_df/combined_CNN_3.csv")
cnn3_df <- cnn3_df %>% select(-X.1, -X)
cnn3_df <- cnn3_df %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
cnn3_df <- cnn3_df %>%
  filter(AI_mentions != 0)
write.csv(cnn3_df, "3_CNN/filtered_CNN_df/combined_filtered_CNN_3.csv")


# load combined_CNN_4
cnn4_df <- read.csv("3_CNN/filtered_CNN_df/combined_CNN_4.csv")
cnn4_df <- cnn4_df %>% select(-X.1, -X)
cnn_df <- cnn4_df %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
cnn4_df <- cnn_df %>%
  filter(AI_mentions != 0)
write.csv(cnn4_df, "3_CNN/filtered_CNN_df/combined_filtered_CNN_4.csv")

cnn1_df <- read.csv("3_CNN/filtered_CNN_df/combined_filtered_CNN_1.csv")
cnn2_df <- read.csv("3_CNN/filtered_CNN_df/combined_filtered_CNN_2.csv")
cnn3_df <- read.csv("3_CNN/filtered_CNN_df/combined_filtered_CNN_3.csv")
cnn4_df <- read.csv("3_CNN/filtered_CNN_df/combined_filtered_CNN_4.csv")

# combine all 4 dfs
CNN_df <- rbind(cnn1_df, cnn2_df, cnn3_df, cnn4_df)

CNN_df <- CNN_df %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

CNN_df <- CNN_df %>%
  filter(year != 2025)

write_csv(CNN_df, "master_df_CNN.csv")




