# Load necessary library
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)

# MSNBC df 1
msnbc_df <- read.csv("5_MSNBC/msnbc-2010--2021.csv")
msnbc_df <- msnbc_df  %>% select (-show_name, -headline, -guests, -url, -uid, -duration, -path, -time, -timezone, -wordcount, -subhead,-summary, -year, -month, -date)
msnbc_df <- msnbc_df %>% rename("source" = "channel.name",
                                "date" = "air_date", 
                                "title" = "program.name")
msnbc_df <- msnbc_df %>%
  mutate(
    date = parse_date_time(.data$date, orders = c("ymd", "mdy", "ymd HMS")),
    date = as.Date(date)
  )

# MSNBC df 2
msnbc2_df <- read.csv("5_MSNBC/msnbc--2003--2014.csv")
msnbc2_df <- msnbc2_df  %>% select (-Show, -Author, -Location.s., -Dateline, -Section, -Index.Terms, -Record.Number, -Length, -Estimated.printed.pages)
msnbc2_df <- msnbc2_df %>% rename("title" = "cite",
                                "text" = "Content", 
                                "date" = "Date",
                                "source" = "Source")
msnbc2_df <- msnbc2_df %>%
  mutate(
    date = parse_date_time(.data$date, orders = c("ymd", "mdy", "ymd HMS")),
    date = as.Date(date)
  )

# MSNBC df 3
msnbc3_df <- read.csv("5_MSNBC/supp_msnbc_raw.csv")
msnbc3_df <- msnbc3_df %>% rename( "text" = "body", 
                                  "date" = "publish_date")
msnbc3_df$source <- "msnbc"

msnbc3_df <- msnbc3_df %>%
  mutate(
    date = parse_date_time(.data$date, orders = c("ymd", "mdy", "ymd HMS")),
    date = as.Date(date)
  )

msnbc3_df$date[2] <- as.Date("2023-05-19") 
msnbc3_df$date[3] <- as.Date("2023-05-19") 
msnbc3_df$date[4] <- as.Date("2023-03-30") 
msnbc3_df$date[5] <- as.Date("2023-03-14") 
msnbc3_df$date[6] <- as.Date("2023-03-02") 
msnbc3_df$date[7] <- as.Date("2023-01-19") 
msnbc3_df$date[8] <- as.Date("2022-12-29") 
msnbc3_df$date[9] <- as.Date("2022-12-09") 

msnbc3_df$date[13] <- as.Date("2021-1-23") 
msnbc3_df$date[15] <- as.Date("2021-06-28") 
msnbc3_df$date[16] <- as.Date("2021-06-23") 
msnbc3_df$date[17] <- as.Date("2021-06-17") 
msnbc3_df$date[18] <- as.Date("2021-04-30") 
msnbc3_df$date[19] <- as.Date("2021-04-28") 
msnbc3_df$date[20] <- as.Date("2021-02-22") 
msnbc3_df$date[21] <- as.Date("2020-12-15") 
msnbc3_df$date[22] <- as.Date("2020-04-23") 
msnbc3_df$date[23] <- as.Date("2020-04-16")

msnbc3_df$date[24] <- as.Date("2020-04-10") 
msnbc3_df$date[25] <- as.Date("2019-12-20") 
msnbc3_df$date[26] <- as.Date("2019-11-20")
msnbc3_df$date[27] <- as.Date("2019-10-15") 
msnbc3_df$date[30] <- as.Date("2019-08-27") 
msnbc3_df$date[31] <- as.Date("2019-08-15") 
msnbc3_df$date[32] <- as.Date("2019-07-31") 

msnbc3_df$date[79] <- as.Date("2023-03-30") 
msnbc3_df$date[80] <- as.Date("2023-03-23") 
msnbc3_df$date[81] <- as.Date("2023-03-13")
msnbc3_df$date[82] <- as.Date("2023-03-01") 
msnbc3_df$date[83] <- as.Date("2023-03-01") 
msnbc3_df$date[84] <- as.Date("2023-02-23") 
msnbc3_df$date[85] <- as.Date("2023-02-19") 
msnbc3_df$date[86] <- as.Date("2023-02-09") 
msnbc3_df$date[87] <- as.Date("2023-02-06") 
msnbc3_df$date[88] <- as.Date("2023-02-03")
msnbc3_df$date[89] <- as.Date("2023-02-23") 
msnbc3_df$date[90] <- as.Date("2023-01-17") 
msnbc3_df$date[91] <- as.Date("2019-12-07") 
msnbc3_df$date[92] <- as.Date("2019-12-05") 
msnbc3_df$date[93] <- as.Date("2019-12-02") 
msnbc3_df$date[94] <- as.Date("2019-12-01") 
msnbc3_df$date[95] <- as.Date("2019-11-15") 
msnbc3_df$date[96] <- as.Date("2019-11-09") 
msnbc3_df$date[97] <- as.Date("2019-11-02") 
msnbc3_df$date[98] <- as.Date("2019-10-26") 
msnbc3_df$date[99] <- as.Date("2019-10-23") 

msnbc3_df$date[34] <- as.Date("2019-06-13") 
msnbc3_df$date[37] <- as.Date("2019-04-05")
msnbc3_df$date[38] <- as.Date("2019-02-21") 
msnbc3_df$date[39] <- as.Date("2019-02-18") 
msnbc3_df$date[40] <- as.Date("2019-02-18") 
msnbc3_df$date[41] <- as.Date("2019-01-08") 
msnbc3_df$date[42] <- as.Date("2018-11-01") 
msnbc3_df$date[43] <- as.Date("2018-10-15") 
msnbc3_df$date[44] <- as.Date("2018-09-06") 
msnbc3_df$date[45] <- as.Date("2018-05-30") 
msnbc3_df$date[46] <- as.Date("2018-05-20") 
msnbc3_df$date[47] <- as.Date("2018-05-10") 

msnbc3_df$date[92] <- as.Date("2019-12-05") 
msnbc3_df$date[93] <- as.Date("2019-12-02") 
msnbc3_df$date[94] <- as.Date("2019-12-01") 
msnbc3_df$date[95] <- as.Date("2019-11-15") 
msnbc3_df$date[96] <- as.Date("2019-11-09") 
msnbc3_df$date[97] <- as.Date("2019-11-02") 
msnbc3_df$date[98] <- as.Date("2019-10-26") 
msnbc3_df$date[99] <- as.Date("2019-10-23") 

msnbc3_df$date[112] <- as.Date("2022-04-26") 
msnbc3_df$date[113] <- as.Date("2022-04-13") 

msnbc3_df$date[114] <- as.Date("2022-03-17") 
msnbc3_df$date[115] <- as.Date("2022-03-02") 
msnbc3_df$date[116] <- as.Date("2022-03-02") 

msnbc3_df$date[118] <- as.Date("2022-02-18") 
msnbc3_df$date[119] <- as.Date("2022-02-17") 
msnbc3_df$date[120] <- as.Date("2022-02-16") 
msnbc3_df$date[121] <- as.Date("2022-02-10") 
msnbc3_df$date[122] <- as.Date("2022-02-09") 
msnbc3_df$date[123] <- as.Date("2022-02-04") 
msnbc3_df$date[124] <- as.Date("2022-02-01") 

msnbc3_df$date[125] <- as.Date("2022-01-20") 
msnbc3_df$date[126] <- as.Date("2022-01-14") 
msnbc3_df$date[127] <- as.Date("2022-01-13") 
msnbc3_df$date[128] <- as.Date("2022-01-06") 
msnbc3_df$date[129] <- as.Date("2022-01-05") 

msnbc3_df$date[130] <- as.Date("2021-12-28") 
msnbc3_df$date[131] <- as.Date("2021-12-23") 
msnbc3_df$date[132] <- as.Date("2021-12-22") 
msnbc3_df$date[133] <- as.Date("2021-12-14") 
msnbc3_df$date[134] <- as.Date("2021-12-07") 
msnbc3_df$date[135] <- as.Date("2021-12-02") 
msnbc3_df$date[136] <- as.Date("2021-12-01") 

msnbc3_df$date[137] <- as.Date("2021-11-23") 
msnbc3_df$date[138] <- as.Date("2021-11-17") 
msnbc3_df$date[139] <- as.Date("2021-11-04") 
msnbc3_df$date[140] <- as.Date("2021-11-02") 

msnbc3_df$date[141] <- as.Date("2021-10-27") 
msnbc3_df$date[142] <- as.Date("2021-10-12") 


msnbc3_df$date[48] <- as.Date("2018-03-21") 
msnbc3_df$date[49] <- as.Date("2017-12-28") 
msnbc3_df$date[50] <- as.Date("2017-09-06") 
msnbc3_df$date[51] <- as.Date("2017-09-04") 

msnbc3_df$date[53] <- as.Date("2017-02-02") 
msnbc3_df$date[54] <- as.Date("2016-12-08") 
msnbc3_df$date[55] <- as.Date("2016-12-06") 
msnbc3_df$date[56] <- as.Date("2016-12-05") 
msnbc3_df$date[58] <- as.Date("2016-09-08") 
msnbc3_df$date[59] <- as.Date("2016-08-26") 

msnbc3_df$date[61] <- as.Date("2016-03-24") 

msnbc3_df$date[65] <- as.Date("2013-05-13") 

msnbc3_df$date[69] <- as.Date("2009-07-29") 
msnbc3_df$date[70] <- as.Date("2009-06-23") 
msnbc3_df$date[71] <- as.Date("2006-06-20") 
msnbc3_df$date[72] <- as.Date("2004-09-21") 

msnbc3_df$date[74] <- as.Date("2023-05-18") 
msnbc3_df$date[75] <- as.Date("2023-04-19") 

msnbc3_df$date[102] <- as.Date("2022-09-05") 
msnbc3_df$date[103] <- as.Date("2022-07-14")

msnbc3_df$date[105] <- as.Date("2022-06-27")
msnbc3_df$date[106] <- as.Date("2022-06-26")

msnbc3_df$date[145] <- as.Date("2021-09-27")
msnbc3_df$date[146] <- as.Date("2021-09-23")
msnbc3_df$date[147] <- as.Date("2021-09-21")
msnbc3_df$date[148] <- as.Date("2021-09-14")
msnbc3_df$date[149] <- as.Date("2021-09-09")
msnbc3_df$date[150] <- as.Date("2021-09-03")
msnbc3_df$date[151] <- as.Date("2021-09-02")
msnbc3_df$date[152] <- as.Date("2021-08-24")

msnbc3_df$date[157] <- as.Date("2021-07-28")
msnbc3_df$date[158] <- as.Date("2021-07-27")
msnbc3_df$date[159] <- as.Date("2021-07-26")
msnbc3_df$date[160] <- as.Date("2021-07-23")
msnbc3_df$date[161] <- as.Date("2021-07-21")

msnbc3_df$date[163] <- as.Date("2021-06-22")
msnbc3_df$date[164] <- as.Date("2021-06-18")
msnbc3_df$date[165] <- as.Date("2021-06-18")
msnbc3_df$date[166] <- as.Date("2021-06-16")

msnbc3_df$date[168] <- as.Date("2021-05-26")
msnbc3_df$date[169] <- as.Date("2021-05-26")


msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 28)

msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 55)

msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 60)

msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 63)

msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 95)

msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 101)

msnbc3_df <- msnbc3_df %>%
  filter(row_number() != 110)


# msnbc df 4
msnbc4_df <- read.csv("5_MSNBC/supp_msnbc_2.csv")
msnbc4_df <- msnbc4_df %>% rename("text" = "body",
                                "date" = "publish_date")
msnbc4_df$source <- "msnbc"


msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1399)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1405)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1420)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1446)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1456)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1480)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1475)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1481)

msnbc4_df <- msnbc4_df %>%
  filter(row_number() != 1497)


msnbc_df <- rbind(msnbc_df, msnbc2_df, msnbc3_df, msnbc4_df)

# add medium
msnbc_df$medium <- "broadcast"

msnbc_df <- msnbc_df %>%
  filter(row_number() != 27972)

# relocate source medium to front
MSNBC_df <- msnbc_df  %>% 
  relocate(date)

MSNBC_df <- MSNBC_df %>% 
  relocate(medium)

MSNBC_df <- MSNBC_df %>% 
  relocate(source)

#count AI mentions and save in column

ai_pattern <- "artificial intelligence|(?<!\\w)AI(?!\\w)|A\\.I\\.?\\b"

MSNBC_df <- MSNBC_df %>%
  mutate(
    AI_mentions = str_count(text, regex(ai_pattern, ignore_case = TRUE))
  )

# remove all 0 mentions of AI
MSNBC_df <- MSNBC_df %>%
  filter(AI_mentions != 0)

MSNBC_df <- unique(MSNBC_df)

write_csv(MSNBC_df, "master_df_MSNBC.csv")







