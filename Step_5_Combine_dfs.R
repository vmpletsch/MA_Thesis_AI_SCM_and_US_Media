#combine dfs

library(dplyr)
library(lubridate)
library(tidyverse)

df_1 <- read.csv("unnested_df_1.csv")
df_2 <- read.csv("unnested_df_2.csv")
df_3 <- read.csv("unnested_df_3.csv")
df_4 <- read.csv("unnested_df_4.csv")
df_5 <- read.csv("unnested_df_5.csv")
df_6 <- read.csv("unnested_df_6.csv")
df_7 <- read.csv("unnested_df_7.csv")
df_8 <- read.csv("unnested_df_8.csv")

cnn_BERT_df <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8)


cnn_BERT_df <- cnn_BERT_df %>%
  mutate(date = mdy(date)) %>%
  arrange(date)



write_csv(cnn_BERT_df , "cnn_df.csv")
