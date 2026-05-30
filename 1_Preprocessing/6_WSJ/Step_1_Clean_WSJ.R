# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# load WSJ
WSJ_final <- read.csv("8_WSJ/WSJ_Raw.csv")


# add missing dates

WSJ_final$date_published[80] <- "June 3, 2024"
WSJ_final$date_published[129] <- "February 5, 2024"
WSJ_final$date_published[168] <- "November 4, 2024"
WSJ_final$date_published[966] <- "August 17, 2023"
WSJ_final$date_published[1032] <- "October 25, 2024"
WSJ_final$date_published[1098] <- "October 12, 2023"
WSJ_final$date_published[1375] <- "December 15, 2023"
WSJ_final$date_published[1618] <- "July 6, 2024"
WSJ_final$date_published[1676] <- "August 28, 2023"
WSJ_final$date_published[1961] <- "August 25, 2023"
WSJ_final$date_published[2678] <- "September 11, 2023"
WSJ_final$date_published[2735] <- "June 29, 2024"
WSJ_final$date_published[2789] <- "January 27, 2023"
WSJ_final$date_published[3069] <- "January 2, 2024"
WSJ_final$date_published[3833] <- "April 13, 2023"
WSJ_final$date_published[3432] <- "June 3, 2024"
WSJ_final$date_published[3857] <- "February 26, 2020"
WSJ_final$date_published[4783] <- "June 12, 2018"
WSJ_final$date_published[4940] <- "April 18, 2018"
WSJ_final$date_published[6169] <- "November 12, 2019"
WSJ_final$date_published[6273] <- "June 13, 2020"
WSJ_final$date_published[7580] <- "November 7, 2020"
WSJ_final$date_published[7933] <- "January 9, 2020, "
WSJ_final$date_published[7949] <- "December 6, 2022"
WSJ_final$date_published[8175] <- "February 14, 2020"
WSJ_final$date_published[8191] <- "July 16, 2022"
WSJ_final$date_published[9513] <- "August 14, 2020"
WSJ_final$date_published[9588] <- "December 21, 2019"
WSJ_final$date_published[9897] <- "January 10, 2020"

write.csv(WSJ_final, "8_WSJ/WSJ_Raw.csv")

WSJ_final <- read_csv("8_WSJ/WSJ_Raw.csv")

WSJ_final$date_published[953] <- "August 07, 2024"
WSJ_final$date_published[3208] <- "March 03, 2023"
WSJ_final$date_published[2029] <- "August 30, 2024"
WSJ_final$date_published[7130] <- "August 22, 2022"

WSJ_final <- WSJ_final %>%
filter(row_number() != 4991)

WSJ_final <- WSJ_final %>%
  filter(row_number() != 6440)

WSJ_final <- WSJ_final %>%
  filter(row_number() != 8958)
WSJ_final <- WSJ_final %>%
  filter(row_number() != 2752)
WSJ_final <- WSJ_final %>%
  filter(row_number() != 8955)
WSJ_final <- WSJ_final %>%
  filter(row_number() != 7353)
WSJ_final <- WSJ_final %>%
  filter(row_number() != 8954)

write.csv(WSJ_final, "8_WSJ/WSJ_Raw.csv")

WSJ_final <- read_csv("8_WSJ/WSJ_Raw.csv")

# remove columns
WSJ_final <- WSJ_final  %>% select (-...1, -...2, -author, -url)
WSJ_final <- WSJ_final  %>% rename("date" = "date_published")

# fix date
WSJ_final <- WSJ_final %>% mutate(date = mdy(date))

# add source and medium
WSJ_final$source <- "WSJ"
WSJ_final$medium <- "article"


# relocate source medium to front
WSJ_final <- WSJ_final %>% 
  relocate(date)

WSJ_final <- WSJ_final %>% 
  relocate(medium)

WSJ_final <- WSJ_final %>% 
  relocate(source)

#count AI mentions and save in column

ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"

WSJ_final <- WSJ_final %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
WSJ_final <- WSJ_final %>%
  filter(AI_mentions != 0)

WSJ_final <- unique(WSJ_final)

write_csv(WSJ_final, "master_df_WSJ.csv")
