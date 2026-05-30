# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

#MSNBC

msnbc_df <- read.csv("msnbc-2010--2021.csv")

msnbc2_df <- read.csv("msnbc--2003--2014.csv")

#msnbc_df clean dataframe 
msnbc_df <- msnbc_df %>% rename("source" = "channel.name",
                                "transcript" ="text")


#remove unnecessary columns 
msnbc_df <- msnbc_df %>% select (-show_name, -headline, -guests, -url, -program.name, -uid, -duration,
                                 -year, -month, -date, -time, -timezone, -path, -wordcount, -subhead, -summary)

#msnbc_df clean dataframe 
msnbc_df <- msnbc_df %>% rename("date" = "air_date")


#reorder cols
msnbc_df <- msnbc_df %>% 
  relocate(source)

#msnbc2_df clean dataframe 
msnbc2_df <- msnbc2_df %>% rename("transcript" = "Content", 
                                  "date" = "Date",
                                  "source" = "Source")

#remove unnecessary columns 
msnbc2_df <- msnbc2_df  %>% select (-cite, -Show, -Author, -Location.s., -Dateline, -Section, -Index.Terms, 
                                    -Record.Number, -Length, -Estimated.printed.pages)

#combine msnbc
master_msnbc_df <- rbind(msnbc_df, msnbc2_df)

write_csv(master_msnbc_df, "master_msnbc_df.csv")

msnbc <- read.csv("master_msnbc_df.csv")
cnn <- read.csv("master_cnn_df.csv")
# cnn

# cnn 1
cnn_df <- read.csv("1_Raw_CNN_DFs/cnn-7.csv.gz")
cnn_df$source <- "CNN"
cnn_df <- cnn_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn_df <- cnn_df  %>% select (-date_obj, -month, -year)
cnn_df <- cnn_df %>% 
  relocate(source)
cnn_df <- cnn_df %>% rename("transcript" = "text")
write_csv(cnn_df, "cnn_df_meta_data.csv")

# cnn 2
cnn2_df <- read.csv("1_Raw_CNN_DFs/cnn-8.csv.gz")
cnn2_df$source <- "CNN"
cnn2_df <- cnn2_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn2_df <- cnn2_df  %>% select (-date_obj, -month, -year)
cnn2_df <- cnn2_df %>% 
  relocate(source)
cnn2_df <- cnn2_df %>% rename("transcript" = "text")
write_csv(cnn2_df, "cnn2_df_meta_data.csv")

# cnn 3
cnn3_df <- read.csv("1_Raw_CNN_DFs/cnn-1.csv")
cnn3_df$source <- "CNN"
cnn3_df <- cnn3_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn3_df <- cnn3_df  %>% select (-date_obj, -month, -year)
cnn3_df <- cnn3_df %>% 
  relocate(source)
cnn3_df <- cnn3_df %>% rename("transcript" = "text")
write_csv(cnn3_df, "cnn3_df_meta_data.csv")

# cnn 4
cnn4_df <- read.csv("1_Raw_CNN_DFs/cnn-2.csv")
cnn4_df$source <- "CNN"
cnn4_df <- cnn4_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn4_df <- cnn4_df  %>% select (-date_obj, -month, -year)
cnn4_df <- cnn4_df %>% 
  relocate(source)
cnn4_df <- cnn4_df %>% rename("transcript" = "text")
write_csv(cnn4_df, "cnn4_df_meta_data.csv")

# cnn 5
cnn5_df <- read.csv("1_Raw_CNN_DFs/cnn-3.csv")
cnn5_df$source <- "CNN"
cnn5_df <- cnn5_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn5_df <- cnn5_df  %>% select (-date_obj, -month, -year)
cnn5_df <- cnn5_df %>% 
  relocate(source)
cnn5_df <- cnn5_df %>% rename("transcript" = "text")
write_csv(cnn5_df, "cnn5_df_meta_data.csv")

# cnn 6
cnn6_df <- read.csv("1_Raw_CNN_DFs/cnn-4.csv")
cnn6_df$source <- "CNN"
cnn6_df <- cnn6_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn6_df <- cnn6_df  %>% select (-date_obj, -month, -year)
cnn6_df <- cnn6_df %>% 
  relocate(source)
cnn6_df <- cnn6_df %>% rename("transcript" = "text")
write_csv(cnn6_df, "cnn6_df_meta_data.csv")

# cnn 7
cnn7_df <- read.csv("1_Raw_CNN_DFs/cnn-5.csv")
cnn7_df$source <- "CNN"
cnn7_df <- cnn7_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn7_df <- cnn7_df  %>% select (-date_obj, -month, -year)
cnn7_df <- cnn7_df %>% 
  relocate(source)
cnn7_df <- cnn7_df %>% rename("transcript" = "text")
write_csv(cnn7_df, "cnn7_df_meta_data.csv")

# cnn 8
cnn8_df <- read.csv("1_Raw_CNN_DFs/cnn-6.csv")
cnn8_df$source <- "CNN"
cnn8_df <- cnn8_df %>%
  mutate(
    date_obj = make_date(year, month, date),
    date = format(date_obj, "%B %d %Y")
  )
cnn8_df <- cnn8_df  %>% select (-date_obj, -month, -year)
cnn8_df <- cnn8_df %>% 
  relocate(source)
cnn8_df <- cnn8_df %>% rename("transcript" = "text")
write_csv(cnn8_df, "cnn8_df_meta_data.csv")


#combine all cnn
cnn2_df <- read.csv("cnn2_df.csv")
cnn8_df <- read.csv("cnn8_df.csv")
cnn2_8_df <- rbind(cnn2_df, cnn8_df)
write_csv(cnn2_8_df, "cnn2_8_df.csv")

cnn6_df <- read.csv("cnn6_df.csv")
cnn7_df <- read.csv("cnn7_df.csv")
cnn6_7_df <- rbind(cnn6_df, cnn7_df)
write_csv(cnn6_7_df, "cnn6_7_df.csv")

#load all before combine
cnn1_3_df <- read.csv("cnn1_3_df.csv")
cnn4_5_df <- read.csv("cnn4_5_df.csv")
cnn6_7_df <- read.csv("cnn6_7_df.csv")
cnn2_8_df <- read.csv("cnn2_8_df.csv")

#combine all to master
master_cnn_df <- rbind(cnn1_3_df, cnn4_5_df, cnn6_7_df, cnn2_8_df)
write_csv(master_cnn_df, "master_cnn_df.csv")

#combine both masters
master_msnbc <- read.csv("master_msnbc_df.csv")
master_df <- rbind(master_msnbc, master_cnn_df)
write_csv(master_df, "master_df.csv")

master_df <- read.csv("master_df.csv")


#fox
fox <- read.csv("foxnews-transcript.csv")


#remove unnecessary columns 
fox <- fox %>% select (-imageUrl, -title, -description, -category, -isBreaking, -isLive, -duration)

# Assuming your column is named 'link'
fox <- fox %>%
  mutate(url = paste0("https://www.foxnews.com", url))

write_csv(fox, "fox_df.csv")

fox <- read.csv("fox_df.csv")

fox <- fox %>% rename("source" = "html_file")
fox$source <- "foxnews"
fox <- fox %>%
  mutate(date = as_date(publicationDate))
  
fox <- fox %>% select (-publicationDate, -lastPublishedDate, -url)

fox <- fox %>%
relocate(source)

write_csv(fox, "fox_df.csv")
