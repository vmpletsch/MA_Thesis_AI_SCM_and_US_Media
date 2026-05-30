# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# load nydf1
nyt_df1 <- read.csv("6_NYT/nyt_raw.csv")
nyt_df1 <- nyt_df1 %>% select (-author, -url)
nyt_df1 <- nyt_df1 %>% rename("date" = "date_published")

# load nydf2
nyt_df2 <- read.csv("6_NYT/nyt_articles.csv")
nyt_df2 <- nyt_df2 %>% select (-author, -url)
nyt_df2 <- nyt_df2 %>% rename("date" = "date_published")

# load nydf3
nyt_df3 <- read.csv("6_NYT/nyt_sup.csv")
nyt_df3 <- nyt_df3 %>% select (-publish_date)
nyt_df3 <- nyt_df3 %>% rename("text" = "full_article")
nyt_df3 <- nyt_df3 %>% relocate (title)

NYT <- rbind(nyt_df1, nyt_df2, nyt_df3)
write.csv(NYT, "6_NYT/NYT_raw_data.csv")

NYT <- read.csv("6_NYT/NYT_raw_data.csv")

NYT$date[9700] <- "2014-08-14"
NYT$date[9891] <- "2015-07-11"
NYT$date[10339] <- "2014-08-18"
NYT$date[10425] <- "2014-06-27"
NYT$date[10443] <- "2014-06-19"
NYT$date[10517] <- "2014-09-22"
NYT$date[10541] <- "2015-12-01"
NYT$date[10585] <- "2017-02-24"
NYT$date[10733] <- "2015-10-08"
NYT$date[10766] <- "2015-10-19"
NYT$date[11112] <- "2014-10-09"
NYT$date[11116] <- "2014-09-20"
NYT$date[11134] <- "2014-06-24"
NYT$date[11150] <- "2014-09-19"
NYT$date[11161] <- "2015-11-30"
NYT$date[11192] <- "2015-02-26"
NYT$date[11700] <- "2015-04-28"
NYT$date[11861] <- "2017-05-23"
NYT$date[11871] <- "2014-11-13"
NYT$date[11916] <- "2015-03-29"
NYT$date[12142] <- "2015-01-20"
NYT$date[12284] <- "2015-01-20"
NYT$date[12469] <- "2015-11-04"
NYT$date[12892] <- "2015-01-14"
NYT$date[12897] <- "2015-07-09"
NYT$date[12904] <- "2015-03-30"
NYT$date[12927] <- "2015-08-21"
NYT$date[12931] <- "2015-08-19"
NYT$date[10150] <- "2024-10-08"

NYT <- NYT %>%
  filter(row_number() != 11274)
NYT <- NYT %>%
  filter(row_number() != 2458)
NYT <- NYT %>%
  filter(row_number() != 76)

NYT$date[11311] <- "2000-05-26"
NYT$date[12498] <- "2000-06-29"
NYT$date[2275] <- "2024-09-26"
NYT$date[12642] <- "2015-10-28"
NYT$date[910] <- "2021-10-18"
NYT$date[216] <- "2015-05-15"
NYT$date[10668] <- "2015-05-15"
NYT$date[11490] <- "2014-10-27"
NYT$date[330] <- "2016-05-12"
NYT$date[12931] <- "2015-06-11"
NYT$date[1726] <- "2023-03-27"
NYT$date[331] <- "2016-03-15"
NYT$date[11287] <- "2015-07-22"
NYT$date[10522] <- "2015-11-09"
NYT$date[344] <- "2016-04-18"
NYT$date[294] <- "2016-07-18"
NYT$date[10391] <- "2015-06-08"
NYT$date[10721] <- "2015-12-15"
NYT$date[251] <- "2015-08-21"
NYT$date[11055] <- "2015-04-01"
NYT$date[11874] <- "2014-12-01"
NYT$date[10556] <- "2015-04-07"
NYT$date[11156] <- "2015-12-07"
NYT$date[10558] <- "2015-02-11"
NYT$date[11195] <- "2016-01-04"
NYT$date[1722] <- "2023-04-04"
NYT$date[11094] <- "2015-03-23"
NYT$date[11757] <- "2015-05-04"
NYT$date[7995] <- "2024-11-05"
NYT$date[8028] <- "2022-07-16"
NYT$date[7825] <- "2024-08-08"
NYT$date[9785] <- "2024-03-04"
NYT$date[11301] <- "2019-07-04"
NYT$date[10976] <- "2000-07-17"
NYT$date[127] <- "2000-06-26"
NYT$date[125] <- "2000-11-07"

write.csv(NYT, "6_NYT/NYT_raw_data.csv")

NYT <- read.csv("6_NYT/NYT_raw_data.csv")

NYT$date[126] <- "2001-03-05"
NYT$date[128] <- "2000-11-07"
NYT$date[129] <- "2001-09-02"
NYT$date[130] <- "2001-06-30"
NYT$date[131] <- "2001-09-13"
NYT$date[132] <- "2001-10-15"
NYT$date[133] <- "2002-07-07"
NYT$date[134] <- "2003-11-11"
NYT$date[135] <- "2002-08-08"
NYT$date[136] <- "2005-04-13"
NYT$date[137] <- "2005-04-24"
NYT$date[138] <- "2006-11-24"
NYT$date[139] <- "2006-06-24"
NYT$date[140] <- "2007-07-23"
NYT$date[141] <- "2006-05-26"
NYT$date[142] <- "2007-09-23"
NYT$date[143] <- "2007-03-25"
NYT$date[144] <- "2005-10-14"
NYT$date[145] <- "2007-07-20"
NYT$date[146] <- "2004-11-22"
NYT$date[147] <- "2007-07-23"
NYT$date[148] <- "2008-02-18"
NYT$date[149] <- "2008-03-16"
NYT$date[150] <- "2008-11-24"
NYT$date[152] <- "2006-08-03"
NYT$date[153] <- "2006-11-12"
NYT$date[154] <- "2001-12-23"
NYT$date[155] <- "2009-12-07"
NYT$date[156] <- "2010-06-27"
NYT$date[157] <- "2010-10-04"
NYT$date[158] <- "2009-04-27"
NYT$date[159] <- "2010-06-24"
NYT$date[160] <- "2010-07-04"

write.csv(NYT, "6_NYT/NYT_raw_data.csv")


NYT <- read.csv("6_NYT/NYT_raw_data.csv")

NYT <- NYT %>%
  filter(row_number() != 126)
NYT <- NYT %>%
  filter(row_number() != 150)

NYT <- NYT  %>% select (-X, -Unnamed..0.1, -Unnamed..0)

NYT <- unique(NYT)

NYT <- NYT  %>% 
  distinct(title, date, .keep_all = TRUE)

NYT <- NYT  %>% 
distinct(text, date, .keep_all = TRUE)

NYT <- NYT  %>% 
mutate(
  title_clean = str_to_lower(title),
  title_clean = str_replace_all(title_clean, "[[:punct:]]", ""),
  title_clean = str_squish(title_clean)
) %>%
  distinct(title_clean, date, .keep_all = TRUE)

NYT <- NYT  %>%   
  select(-title)

NYT <- NYT  %>% 
  distinct(title_clean, date, .keep_all = TRUE)

NYT <- NYT  %>% 
  distinct(text, .keep_all = TRUE)

NYT <- NYT  %>% 
  distinct(date, text, .keep_all = TRUE)

NYT <- NYT  %>% 
  mutate(
    text_clean = str_to_lower(text),
    text_clean = str_replace_all(text_clean, "[[:punct:]]", ""),
    text_clean = str_squish(text_clean)
  ) %>%
  distinct(text_clean, date, .keep_all = TRUE)

NYT <- NYT  %>% 
  distinct(text_clean, .keep_all = TRUE)

write.csv(NYT, "6_NYT/NYT_raw_data.csv")


NYT <- read.csv("6_NYT/NYT_raw_data.csv")

NYT <- NYT  %>%   
  select(-text_clean)

# add source and medium
NYT$source <- "NYT"
NYT$medium <- "article"

NYT <- NYT  %>%  rename("title" = "title_clean")
NYT <- NYT  %>% select(-Unnamed..0)

# relocate source medium to front
NYT <- NYT %>% 
  relocate(date)

NYT <- NYT %>% 
  relocate(medium)

NYT <- NYT %>% 
  relocate(source)

#count AI mentions and save in column

ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"

NYT <- NYT %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )
# remove all 0 mentions of AI
NYT <- NYT %>%
  filter(AI_mentions != 0)

# save 
write_csv(NYT, "master_df_NYT.csv")


NYT <- read.csv("master_df_NYT.csv")

NYT <- NYT %>%
mutate(
  date = as.Date(date),       
  year = year(date)           
)

NYT <- NYT %>%
  filter(year != 2026)

NYT <- NYT %>%
  filter(year != 2025)


write_csv(NYT, "master_df_NYT.csv")

NYT <- read.csv("master_df_NYT.csv")

NYT <- NYT %>%
  filter(row_number() != 3926)
NYT <- NYT %>%
  filter(row_number() != 3885)
NYT <- NYT %>%
  filter(row_number() != 8425)

write_csv(NYT, "master_df_NYT.csv")
NYT <- read.csv("master_df_NYT.csv")
