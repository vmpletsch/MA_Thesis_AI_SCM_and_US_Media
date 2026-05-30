# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# cnn is incredibly big and needs to be chopped into 8 chunks so that it doesn't crash R, the dfs will slowly be combined to avoid crashes later on
cnn1_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-1.csv")
cnn2_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-2.csv")
cnn3_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-3.csv")
cnn4_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-4.csv")
cnn5_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-5.csv")
cnn6_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-6.csv")
cnn7_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-7.csv.gz")
cnn8_df <- read.csv("3_CNN/1_Raw_CNN_DFs/cnn-8.csv.gz")

# combine all
CNN_df_1 <- rbind(cnn1_df, cnn5_df)
rm(cnn1_df, cnn5_df)

CNN_df_2 <- rbind(cnn3_df, cnn2_df)
rm(cnn3_df, cnn2_df)

CNN_df_3 <- rbind(cnn8_df, cnn6_df)
rm(cnn8_df, cnn6_df)

CNN_df_4 <- rbind(cnn7_df, cnn4_df)
rm(cnn7_df, cnn4_df)
                  
write_csv(CNN_df, "raw_combined_df_CNN.csv")

#CNN_df_1 
# remove unnecessary columns 
CNN_df_1 <-CNN_df_1  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
# count AI mentions and save in column (doing first to remove bulk and avoid R crash)
ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"
# count mentions 
CNN_df_1 <- CNN_df_1 %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
CNN_df_1 <- CNN_df_1 %>%
  filter(AI_mentions != 0)


#CNN_df_2
# remove unnecessary columns 
CNN_df_2 <- CNN_df_2  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
# count AI mentions and save in column (doing first to remove bulk and avoid R crash)
ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"
# count mentions 
CNN_df_2 <- CNN_df_2 %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
CNN_df_2 <- CNN_df_2 %>%
  filter(AI_mentions != 0)



#CNN_df_3
# remove unnecessary columns 
CNN_df_3 <- CNN_df_3  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
# count AI mentions and save in column (doing first to remove bulk and avoid R crash)
ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"
# count mentions 
CNN_df_3 <- CNN_df_3 %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
CNN_df_3 <- CNN_df_3 %>%
  filter(AI_mentions != 0)


# CNN_df_4
# remove unnecessary columns 
CNN_df_4 <- CNN_df_4  %>% select (-url, -program.name, -channel.name, -uid, -duration, -path, -wordcount, -time, -timezone)
# count AI mentions and save in column (doing first to remove bulk and avoid R crash)
ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"
# count mentions 
CNN_df_4 <- CNN_df_4 %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
CNN_df_4 <- CNN_df_4 %>%
  filter(AI_mentions != 0)

# combine all 4 dfs
CNN_df <- rbind(CNN_df_1, CNN_df_2, CNN_df_3, CNN_df_4)
rm(CNN_df_1, CNN_df_2, CNN_df_3, CNN_df_4)

# combined df
# add source and medium
CNN_df$source <- "CNN"
CNN_df$medium <- "broadcast"

# combine into single date column
CNN_df <- CNN_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )

# remove extra date columns
CNN_df <- CNN_df  %>% select (-year, -month, -date)

# reorder columns 
CNN_df<- CNN_df %>% 
  relocate(date_obj)

CNN_df <- CNN_df %>% 
  relocate(medium)

CNN_df <- CNN_df %>% 
  relocate(source)

# rename columns 
CNN_df <- CNN_df %>% rename("title" = "subhead",
                            "date" = "date_obj")

# save
write_csv(CNN_df, "master_df_CNN.csv")


