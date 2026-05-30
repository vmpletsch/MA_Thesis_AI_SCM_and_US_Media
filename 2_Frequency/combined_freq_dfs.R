
# load latest version of freq_total, _freq_yearly_total, freq_yearly_total_rel

freq_total <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_total.csv")
freq_yearly_rel <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")
freq_yearly <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total.csv")

des_table <- read.csv("4_Results/1_Frequency/master_freq_total.csv")




# combine yearly freq

nyt_freq <- read.csv("nyt_freq.csv")
wsj_freq <- read.csv("wsj_yearly_counts.csv")

wsj_freq <- wsj_freq %>% select(-Total_25_Years)
wsj_freq <- wsj_freq %>% rename("WSJ_count" = "Article_Count")
wsj_freq <- wsj_freq %>% rename("year" = "Year")

nyt_freq <- nyt_freq %>% rename("NYT_count" = "n")
nyt_freq <- nyt_freq %>%
  filter(row_number() != 26)


master_freq <- left_join(nyt_freq, wsj_freq, by = "year")
 
write_csv(master_freq, "master_freq.csv")



# combine total freq


nyt_freq <- read.csv("master_freq_total.csv")
wsj_freq <- read.csv("8_WSJ/wsj_freq.csv")

master_freq <- rbind(nyt_freq, wsj_freq)

write.csv(master_freq, "master_freq_total.csv", row.names = FALSE)


# Add NBC + CNN + Fox + MSNBC to total freq 
# load total frequency
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")

total_freq <- total_freq %>%
  add_row(source = "NBC", total = NA)
total_freq <- total_freq %>%
  add_row(source = "CNN", total = NA)
total_freq <- total_freq %>%
  add_row(source = "Fox", total = NA)
total_freq <- total_freq %>%
  add_row(source = "MSNBC", total = NA)

write_csv(total_freq, "Results/Frequency/master_freq_total.csv")


# Add NBC + CNN + Fox + MSNBC to yearly freq 
# load yearly frequency 
yearly_freq <- read.csv("Results/Frequency/master_freq_yearly.csv")

yearly_freq$NBC_Count <- "NA"
yearly_freq$CNN_Count <- "NA"
yearly_freq$Fox_Count <- "NA"
yearly_freq$MSNBC_Count <- "NA"

write_csv(yearly_freq, "Results/Frequency/master_freq_yearly.csv")


# add in NBC MSNBC data to yearly freq
# load yearly frequency 
yearly_freq <- read.csv("Results/Frequency/master_freq_yearly.csv")
NBC_yearly <- read.csv("NBC_total_article_count.csv")
NBC_yearly <- NBC_yearly %>% rename ("NBC_Count" = "total_articles")
NBC_yearly <- NBC_yearly %>% rename ("year" = "Year")

NBC_yearly <- NBC_yearly %>%
  mutate(year = as.integer(year))

NBC_yearly <- NBC_yearly %>%
  add_row(year = 2000, NBC_Count = NA, .before = 1)
NBC_yearly <- NBC_yearly %>%
  add_row(year = 2001, NBC_Count = NA, .before = 2)
NBC_yearly <- NBC_yearly %>%
  add_row(year = 2002, NBC_Count = NA, .before = 3)
NBC_yearly <- NBC_yearly %>%
  add_row(year = 2004, NBC_Count = NA, .before = 5)

NBC_yearly <- NBC_yearly %>% select(-source)

NBC_yearly <- NBC_yearly %>%
  mutate(NBC_total = sum(NBC_Count, na.rm = TRUE))
NBC_yearly <- NBC_yearly %>% select(-year, -NBC_Count)
NBC_yearly <- NBC_yearly %>%
slice(-(2:25))
NBC_yearly$source <- "NBC"
NBC_yearly <- NBC_yearly %>% rename ("total" = "NBC_total")
NBC_yearly <- NBC_yearly %>% 
  relocate(source)

yearly_freq <- yearly_freq %>%
  select(-NBC_Count) %>%
  left_join(NBC_yearly, by = "year")

write_csv(yearly_freq, "Results/Frequency/master_freq_yearly.csv")


# add in NBC data to total freq
# load total frequency 
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- rbind(total_freq, NBC_yearly)
write_csv(total_freq, "Results/Frequency/master_freq_total.csv")


# add in Fox data to yearly freq

yearly_freq <- read.csv("Results/Frequency/master_freq_yearly.csv")
FOX_yearly <- read.csv("Fox_freq_totals.csv")
FOX_yearly <- FOX_yearly %>% rename ("Fox_Count" = "total_records")
FOX_yearly <- FOX_yearly %>% rename ("year" = "Year")

FOX_yearly <- FOX_yearly %>%
  mutate(year = as.integer(year))

FOX_yearly <- FOX_yearly %>%
  add_row(year = 2000, Fox_Count = NA, .before = 1)
FOX_yearly <- FOX_yearly %>%
  add_row(year = 2001, Fox_Count = NA, .before = 2)
FOX_yearly <- FOX_yearly %>%
  add_row(year = 2002, Fox_Count = NA, .before = 3)

FOX_yearly <- FOX_yearly %>% select(-source)

yearly_freq <- yearly_freq %>%
  select(-Fox_Count) %>%
  left_join(FOX_yearly, by = "year")

write_csv(yearly_freq, "Results/Frequency/master_freq_yearly.csv")

FOX_yearly <- FOX_yearly %>%
  mutate(total = sum(Fox_Count, na.rm = TRUE))
FOX_yearly <- FOX_yearly %>% select(-year, -Fox_Count)
FOX_yearly <- FOX_yearly %>%
  slice(-(2:25))
FOX_yearly$source <- "FOX"
FOX_yearly <- FOX_yearly %>% 
  relocate(source)

# add in FOX data to total freq
# load total frequency 
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- rbind(total_freq, FOX_yearly)
write_csv(total_freq, "Results/Frequency/master_freq_total.csv")



# add in MSNBC data to yearly freq

yearly_freq <- read.csv("Results/Frequency/master_freq_yearly.csv")
MSNBC_yearly <- read.csv("MSNBC_total_count.csv")
MSNBC_yearly <- MSNBC_yearly %>% rename ("MSNBC" = "total_records")
MSNBC_yearly <- MSNBC_yearly %>% rename ("year" = "Year")

MSNBC_yearly <- MSNBC_yearly %>%
  mutate(year = as.integer(year))

MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2000, MSNBC = NA, .before = 1)
MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2001, MSNBC = NA, .before = 2)
MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2002, MSNBC = NA, .before = 3)
MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2004, MSNBC = NA, .before = 5)
MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2007, MSNBC = NA, .before = 8)
MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2010, MSNBC = NA, .before = 10)
MSNBC_yearly <- MSNBC_yearly %>%
  add_row(year = 2009, MSNBC = NA, .before = 10)

MSNBC_yearly <- MSNBC_yearly %>% select(-source)

yearly_freq <- yearly_freq %>%
  select(-MSNBC_Count) %>%
  left_join(MSNBC_yearly, by = "year")

yearly_freq <- yearly_freq %>% rename("MSNBC_Count" = "MSNBC")

write_csv(yearly_freq, "Results/Frequency/master_freq_yearly.csv")

MSNBC_yearly <- MSNBC_yearly %>%
  mutate(total = sum(MSNBC, na.rm = TRUE))
MSNBC_yearly <- MSNBC_yearly %>% select(-year, -MSNBC)
MSNBC_yearly <- MSNBC_yearly %>% 
  slice(-(2:25))
MSNBC_yearly$source <- "MSNBC"
MSNBC_yearly <- MSNBC_yearly %>% 
  relocate(source)

# add in FOX data to total freq
# load total frequency 
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- rbind(total_freq, MSNBC_yearly)
write_csv(total_freq, "Results/Frequency/master_freq_total.csv")



# add in CNN data to yearly freq
yearly_freq <- read.csv("Results/Frequency/master_freq_yearly.csv")
CNN_yearly <- read.csv("cnn_freq.csv")
CNN_yearly <- CNN_yearly %>% rename ("CNN_Count" = "total")

CNN_yearly <- CNN_yearly %>%
  mutate(year = as.integer(year))

CNN_yearly <- CNN_yearly %>%
  filter(row_number() != 26)

yearly_freq <- yearly_freq %>%
  select(-CNN_Count) %>%
  left_join(CNN_yearly, by = "year")

write_csv(yearly_freq, "Results/Frequency/master_freq_yearly.csv")

CNN_yearly <- CNN_yearly %>%
  mutate(total = sum(CNN_Count, na.rm = TRUE))
CNN_yearly <- CNN_yearly %>% select(-CNN_Count, -year)
CNN_yearly <- CNN_yearly %>%
  slice(-(2:25))
CNN_yearly$source <- "CNN"
CNN_yearly <- CNN_yearly %>% 
  relocate(source)

# add in FOX data to total freq
# load total frequency 
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- rbind(total_freq, CNN_yearly)
write_csv(total_freq, "Results/Frequency/master_freq_total.csv")


# change column names
yearly_freq <- read.csv("Results/Frequency/master_freq_yearly.csv")

total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- total_freq %>% rename("Raw_total" = "total")




# CNN relevant total 
CNN_total_freq_rel <- read.csv("master_df_CNN.csv")
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")

CNN_total_freq_rel <- CNN_total_freq_rel %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

CNN_total_freq_rel <- CNN_total_freq_rel %>%
filter(year != 2025)

CNN_total_freq_rel <- CNN_total_freq_rel %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  ungroup()

CNN_total_freq_rel <- CNN_total_freq_rel %>%
  mutate(total = sum(total, na.rm = TRUE))

CNN_total_freq_rel <- CNN_total_freq_rel %>%
  rename("Relevant_total" = "total")

CNN_total_freq_rel$source <- "CNN" 

CNN_total_freq_rel <- CNN_total_freq_rel %>% select(-year)
CNN_total_freq_rel <- CNN_total_freq_rel %>%
  slice(-(2:25))
CNN_total_freq_rel <- CNN_total_freq_rel %>%
  relocate(source)

total_freq <- left_join(total_freq, CNN_total_freq_rel, by = "source")
total_freq <- total_freq %>%
  rows_update(CNN_total_freq_rel, by = "source")

write_csv(total_freq, "Results/Frequency/master_freq_total.csv")


#MSNBC relevant total 
MSNBC_total_freq_rel <- read.csv("master_df_MSNBC.csv")

MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  ungroup()

MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>%
  mutate(total = sum(total, na.rm = TRUE))

MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>% 
  rename("Relevant_total" = "total")

MSNBC_total_freq_rel$source <- "MSNBC" 

MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>% select(-year)
MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>%
  slice(-(2:19))
MSNBC_total_freq_rel <- MSNBC_total_freq_rel %>%
  relocate(source)
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- left_join(total_freq, MSNBC_total_freq_rel, by = "source")
total_freq <- total_freq %>%
  rows_update(MSNBC_total_freq_rel, by = "source")

write_csv(total_freq, "Results/Frequency/master_freq_total.csv")

# Fox relevant total
FOX_total_freq_rel <- read.csv("9_Final_DFs/master_df_FOX.csv")
total_freq <- read.csv("9_Final_DFs/master_freq_total.csv")

FOX_total_freq_rel <- FOX_total_freq_rel %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

FOX_total_freq_rel <- FOX_total_freq_rel %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  ungroup()

FOX_total_freq_rel <- FOX_total_freq_rel %>%
  mutate(total = sum(total, na.rm = TRUE))

FOX_total_freq_rel$source <- "FOX" 

FOX_total_freq_rel <- FOX_total_freq_rel %>% select(-year)
FOX_total_freq_rel <- FOX_total_freq_rel %>% rename("Relevant_total" = "total")
FOX_total_freq_rel <- FOX_total_freq_rel %>%
  slice(-(2:19))
FOX_total_freq_rel <- FOX_total_freq_rel %>%
  relocate(Relevant_total)
FOX_total_freq_rel <- FOX_total_freq_rel %>%
  relocate(source)

total_freq <- total_freq %>%
  rows_update(FOX_total_freq_rel, by = "source")

write_csv(total_freq, "Results/Frequency/master_freq_total.csv")

#NBC relevant total
NBC_total_freq_rel <- read.csv("master_df_NBC.csv")

NBC_total_freq_rel <- NBC_total_freq_rel %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

NBC_total_freq_rel <- NBC_total_freq_rel %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  ungroup()

NBC_total_freq_rel <- NBC_total_freq_rel %>%
  mutate(total = sum(total, na.rm = TRUE))

NBC_total_freq_rel <- NBC_total_freq_rel %>% 
  rename("Relevant_total" = "total")

NBC_total_freq_rel$source <- "NBC" 

NBC_total_freq_rel <- NBC_total_freq_rel %>% select(-year)
NBC_total_freq_rel <- NBC_total_freq_rel %>%
  slice(-(2:22))
NBC_total_freq_rel <- NBC_total_freq_rel %>%
  relocate(source)
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- total_freq %>%
  rows_update(NBC_total_freq_rel, by = "source")

write_csv(total_freq, "Results/Frequency/master_freq_total.csv")

#NYT relevant total
NYT_total_freq_rel <- read.csv("9_Final_DFs/master_df_NYT.csv")

NYT_total_freq_rel <- NYT_total_freq_rel %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

NYT_total_freq_rel <- NYT_total_freq_rel %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  ungroup()

NYT_total_freq_rel <- NYT_total_freq_rel %>%
  mutate(total = sum(total, na.rm = TRUE))

NYT_total_freq_rel <- NYT_total_freq_rel %>%
  rename("Relevant_total" = "total")

NYT_total_freq_rel$source <- "nyt" 

NYT_total_freq_rel <- NYT_total_freq_rel %>% select(-year)
NYT_total_freq_rel <- NYT_total_freq_rel %>%
  slice(-(2:25))
NYT_total_freq_rel <- NYT_total_freq_rel %>%
  relocate(source)
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- total_freq %>%
  rows_update(NYT_total_freq_rel, by = "source")

write_csv(total_freq, "9_Final_DFs/master_freq_total.csv")

#WSJ relevant total
WSJ_total_freq_rel <- read.csv("master_df_WSJ.csv")

WSJ_total_freq_rel <- WSJ_total_freq_rel %>%
  mutate(
    date = as.Date(date),       
    year = year(date)           
  )

WSJ_total_freq_rel <- WSJ_total_freq_rel %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  ungroup()

WSJ_total_freq_rel <- WSJ_total_freq_rel %>%
  mutate(total = sum(total, na.rm = TRUE))

WSJ_total_freq_rel <- WSJ_total_freq_rel %>%
  rename("Relevant_total" = "total")

WSJ_total_freq_rel$source <- "wsj" 

WSJ_total_freq_rel <- WSJ_total_freq_rel %>% select(-year)
WSJ_total_freq_rel <- WSJ_total_freq_rel %>%
  slice(-(2:25))
WSJ_total_freq_rel <- WSJ_total_freq_rel %>%
  relocate(source)
total_freq <- read.csv("Results/Frequency/master_freq_total.csv")
total_freq <- total_freq %>%
  rows_update(WSJ_total_freq_rel, by = "source")

write_csv(total_freq, "Results/Frequency/master_freq_total.csv")




