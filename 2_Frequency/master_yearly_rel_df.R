# load dfs 

yearly_freq_rel <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")
yearly_freq <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total.csv")
freq_total <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_total.csv")
master_df <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
Des_df <- read.csv("4_Results/0_Descriptive_Table/Descriptive_df.csv")


# redo CNN yearly_rel
CNN <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
CNN <- CNN %>%
  filter(source == "CNN")
unique(CNN$source)
#update CNN relevant_total 
 CNN <- CNN %>%
   mutate(year = year(date))
 CNN <- CNN %>%
   group_by(year) %>%
   mutate(CNN_rel = n()) %>%
   ungroup()
 CNN <- CNN %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source)
 CNN <- CNN %>% select(-id, -X.1, -political_orientation)
 CNN <- CNN   %>% 
   distinct(year, .keep_all = TRUE)
 # yearly_freq
 yearly_freq_rel <- yearly_freq_rel %>%
   left_join(CNN, by = "year")
 yearly_freq_rel <- yearly_freq_rel %>% select(-CNN_rel.x, -X)
 write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
#freq_total
CNN <- CNN %>%
   mutate(total = sum(CNN_rel, na.rm = TRUE))
CNN$source <- "CNN"
CNN <- CNN %>% select(-year, -CNN_rel)
CNN <- CNN %>% relocate(source)
CNN <- CNN %>% slice(-(2:25))
CNN <- CNN %>% rename("Relevant_total" = "total")
CNN$total <- "2076293"
CNN <- CNN %>% select(source, total, Relevant_total)
freq_total <- rbind(CNN,freq_total)
freq_total <- freq_total[-2, ]
freq_total <- freq_total[-7, ]
write.csv(freq_total, "2_Final_DFs/1_Frequency_dfs/master_freq_total.csv", row.names = FALSE)


# redo NYT yearly_rel
NYT <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
NYT <- NYT %>%
  filter(source == "NYT")
unique(NYT$source)
# update NYT relevant_total 
NYT <- NYT %>%
  mutate(year = year(date))
NYT <- NYT %>%
  group_by(year) %>%
  mutate(NYT_rel = n()) %>%
  ungroup()
NYT <- NYT %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source, -id, -X.1, -political_orientation)
NYT <- NYT %>%
  arrange(year)
NYT <- NYT  %>% 
  distinct(year, .keep_all = TRUE)
# yearly_freq
yearly_freq_rel <- yearly_freq_rel %>%
  left_join(NYT, by = "year")
yearly_freq_rel <- yearly_freq_rel %>% select(-NYT_rel.x)
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
#freq_total
NYT <- NYT %>%
  mutate(total = sum(NYT_rel, na.rm = TRUE))
NYT$source <- "NYT"
NYT <- NYT %>% select(-year, -NYT_rel)
NYT <- NYT %>% slice(-(2:25))
NYT <- NYT %>% rename("Relevant_total" = "total")
NYT$total <- "2076293"
NYT <- NYT %>% select(source, total, Relevant_total)
freq_total <- rbind(NYT,freq_total)
freq_total <- freq_total[-3, ]
freq_total[2, "total"] <- "334016"
write.csv(freq_total, "2_Final_DFs/1_Frequency_dfs/master_freq_total.csv", row.names = FALSE)
 
 
 
 
 
# redo WSJ yearly_rel
WSJ <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
WSJ <- WSJ %>%
  filter(source == "WSJ")
unique(WSJ$source) 
# update WSJ relevant_total 
WSJ <- WSJ %>%
  mutate(year = year(date))
WSJ <- WSJ %>%
  group_by(year) %>%
  mutate(WSJ_rel = n()) %>%
  ungroup()
WSJ <- WSJ %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source, -id, -X.1, -political_orientation)
WSJ <- WSJ %>% arrange(year)
WSJ <- WSJ   %>% 
  distinct(year, .keep_all = TRUE)
# yearly_freq
yearly_freq_rel <- yearly_freq_rel %>%
  left_join(wsj, by = "year")
yearly_freq_rel <- yearly_freq_rel %>% select(-WSJ_rel.x)
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
#freq_total
WSJ <- WSJ %>%
  mutate(total = sum(WSJ_rel, na.rm = TRUE))
WSJ$source <- "WSJ"
WSJ <- WSJ %>% select(-year, -WSJ_rel)
WSJ <- WSJ %>% slice(-(2:25))
WSJ <- WSJ %>% rename("Relevant_total" = "total")
WSJ$total <- "1325079"
WSJ <- WSJ %>% select(source, total, Relevant_total)
freq_total <- rbind(WSJ, freq_total)
freq_total <- freq_total[-4, ]
write.csv(freq_total, "2_Final_DFs/1_Frequency_dfs/master_freq_total.csv", row.names = FALSE)

 
 
# redo NBCyearly_rel
NBC <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
NBC <- NBC %>%
  filter(source == "NBC")
unique(NBC$source) 
# update NBC relevant_total 
NBC <- NBC %>%
  mutate(year = year(date))
NBC <- NBC %>%
  group_by(year) %>%
  mutate(NBC_rel = n()) %>%
  ungroup()
NBC <- NBC %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source, -id, -X.1, -political_orientation)
NBC <- NBC   %>% 
  distinct(year, .keep_all = TRUE)
NBC <- NBC %>% arrange(year)
NBC <- NBC %>%
  add_row(year = 2000, NBC_rel = NA, .before = 1)
NBC <- NBC %>%
  add_row(year = 2001, NBC_rel = NA, .before = 2)
NBC <- NBC %>%
  add_row(year = 2002, NBC_rel = NA, .before = 3)
# yearly_freq
yearly_freq_rel <- yearly_freq_rel %>%
  left_join(NBC, by = "year")
yearly_freq_rel <- yearly_freq_rel %>% select(-NBC_rel.x)
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
#freq_total
NBC <- NBC %>%
  mutate(total = sum(NBC_rel, na.rm = TRUE))
NBC$source <- "NBC"
NBC <- NBC %>% select(-year, -NBC_rel)
NBC <- NBC %>% slice(-(2:25))
NBC <- NBC %>% rename("Relevant_total" = "total")
NBC$total <- "1291479"
NBC <- NBC %>% select(source, total, Relevant_total)
freq_total <- rbind(NBC, freq_total)
freq_total <- freq_total[-5, ]
write.csv(freq_total, "2_Final_DFs/1_Frequency_dfs/master_freq_total.csv", row.names = FALSE) 
 



# redo CNN yearly_rel
MSNBC <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
MSNBC <- MSNBC %>%
  filter(source == "MSNBC")
unique(MSNBC$source)
# update MSNBC relevant_total 
MSNBC <- MSNBC %>%
  mutate(year = year(date))
MSNBC <- MSNBC %>%
  group_by(year) %>%
  mutate(MSNBC_rel = n()) %>%
  ungroup()
freq_total[6, "Relevant_total"] <- 236
freq_total[2, "Relevant_total"] <- 9497
# yearly_freq
yearly_freq_rel <- yearly_freq_rel %>%
  left_join(MSNBC, by = "year")
freq_total[6, "Relevant_total"] <- 236
yearly_freq_rel[6, "Relevant_total"] <- 236

write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
#freq_total
MSNBC <- MSNBC %>%
  mutate(total = sum(MSNBC_rel, na.rm = TRUE))
MSNBC$source <- "MSNBC"
MSNBC <- MSNBC %>% select(-year, -MSNBC_rel)
MSNBC <- MSNBC %>% relocate(source)
MSNBC <- MSNBC %>% slice(-(2:25))
MSNBC <- MSNBC %>% rename("Relevant_total" = "total")
MSNBC$total <- "2076293"
MSNBC <- MSNBC %>% select(source, total, Relevant_total)

yearly_freq_rel[1, "MSNBC_rel.y"] <- 0
yearly_freq_rel[2, "MSNBC_rel.y"] <- 0
yearly_freq_rel[3, "MSNBC_rel.y"] <- 0
yearly_freq_rel[6, "MSNBC_rel.y"] <- 3
yearly_freq_rel[9, "MSNBC_rel.y"] <- 0
yearly_freq_rel[11, "MSNBC_rel.y"] <- 2
yearly_freq_rel[12, "MSNBC_rel.y"] <- 0
yearly_freq_rel[13, "MSNBC_rel.y"] <- 1
yearly_freq_rel[15, "MSNBC_rel.y"] <- 2
yearly_freq_rel[17, "MSNBC_rel.y"] <- 29
yearly_freq_rel[18, "MSNBC_rel.y"] <- 14
yearly_freq_rel[19, "MSNBC_rel.y"] <- 23
yearly_freq_rel[20, "MSNBC_rel.y"] <- 56
yearly_freq_rel[21, "MSNBC_rel.y"] <- 14
yearly_freq_rel[23, "MSNBC_rel.y"] <- 17
yearly_freq_rel[25, "MSNBC_rel.y"] <- 0

write.csv(freq_total, "2_Final_DFs/1_Frequency_dfs/master_freq_total.csv", row.names = FALSE) 
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)


# redo FOX yearly_rel
FOX <- read.csv("2_Final_DFs/0_master_dfs/master_all_df.csv")
FOX <- FOX %>%
  filter(source == "FOX")
unique(FOX$source)
# update FOX relevant_total 
FOX <- FOX %>%
  mutate(year = year(date))
FOX <- FOX %>%
  group_by(year) %>%
  mutate(FOX_rel = n()) %>%
  ungroup()
FOX <- FOX %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source, -id, -X.1, -political_orientation)
FOX <- FOX %>%
  add_row(year = 2000, FOX_rel = NA, .before = 17)
FOX <- FOX %>%
  add_row(year = 2004, FOX_rel = NA, .before = 7)
FOX <- FOX %>%
  add_row(year = 2007, FOX_rel = NA, .before = 20)
FOX <- FOX %>%
  add_row(year = 2009, FOX_rel = NA, .before = 23)
FOX <- FOX %>%
  add_row(year = 2010, FOX_rel = NA, .before = 24)
FOX <- FOX %>%
  add_row(year = 2011, FOX_rel = NA, .before = 25)
FOX <- FOX %>%
  add_row(year = 2013, FOX_rel = NA, .before = 11)
FOX <- FOX %>% arrange(year)
FOX <- FOX  %>% 
  distinct(year, .keep_all = TRUE)
# yearly_freq
yearly_freq_rel <- yearly_freq_rel %>%
  left_join(FOX, by = "year")
yearly_freq_rel <- yearly_freq_rel %>% select(-FOX_rel.x)
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
#freq_total
FOX <- FOX %>%
  mutate(total = sum(FOX_rel, na.rm = TRUE))
FOX$source <- "FOX"
FOX <- FOX %>% select(-year, -FOX_rel)
FOX <- FOX %>% relocate(source)
FOX <- FOX %>% slice(-(2:25))
FOX <- FOX %>% rename("Relevant_total" = "total")
FOX$Relevant_total <- "1871"
freq_total[5, "Relevant_total"] <- 1871
write.csv(freq_total, "2_Final_DFs/1_Frequency_dfs/master_freq_total.csv", row.names = FALSE) 



write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)
yearly_freq_rel[1, "FOX_rel.y"] <- 0
yearly_freq_rel[5, "FOX_rel.y"] <- 0
yearly_freq_rel[8, "FOX_rel.y"] <- 0
yearly_freq_rel[10, "FOX_rel.y"] <- 0
yearly_freq_rel[11, "FOX_rel.y"] <- 0
yearly_freq_rel[12, "FOX_rel.y"] <- 0
yearly_freq_rel[14, "FOX_rel.y"] <- 0
yearly_freq_rel[23, "FOX_rel.y"] <- 102
yearly_freq_rel[24, "FOX_rel.y"] <- 662

write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv", row.names = FALSE)





####### ALL OLD CODE AND DFs ####### DO NOT RUN ###### USE AS TEMPLATE ######

# cnn
CNN <- read.csv("2_Final_DFs/99_master_dfs_old/master_df_CNN.csv")
CNN <- CNN %>%
  mutate(year = year(date))
CNN <- CNN %>%
  group_by(year) %>%
  mutate(CNN_rel = n()) %>%
  ungroup()
CNN <- CNN %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source)
CNN <- CNN   %>% 
  distinct(year, .keep_all = TRUE)

yearly_freq_rel <- yearly_freq_rel %>%
  left_join(CNN, by = "year")

write.csv(CNN, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")

# fox
FOX <- read.csv("2_Final_DFs/99_master_dfs_old/master_df_FOX.csv")
yearly_freq_rel <- read.csv("2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")
FOX <- FOX %>%
  mutate(year = year(date))
FOX <- FOX %>%
  group_by(year) %>%
  mutate(FOX_rel = n()) %>%
  ungroup()
FOX <- FOX %>% select(-AI_mentions, -title, -text, -date, -X, -medium, -source)
FOX <- FOX %>%
  distinct(year, .keep_all = TRUE)
FOX <- FOX %>%
  add_row(year = 2000, FOX_rel = NA, .before = 17)
FOX <- FOX %>%
  add_row(year = 2004, FOX_rel = NA, .before = 7)
FOX <- FOX %>%
  add_row(year = 2007, FOX_rel = NA, .before = 20)
FOX <- FOX %>%
  add_row(year = 2009, FOX_rel = NA, .before = 23)
FOX <- FOX %>%
  add_row(year = 2010, FOX_rel = NA, .before = 24)
FOX <- FOX %>%
  add_row(year = 2011, FOX_rel = NA, .before = 25)
FOX <- FOX %>%
  add_row(year = 2013, FOX_rel = NA, .before = 11)

FOX <- FOX %>%
arrange(year)

yearly_freq_rel <- yearly_freq_rel %>%
  select(-X)

yearly_freq_rel <- yearly_freq_rel %>%
  left_join(FOX, by = "year")

yearly_freq_rel <- yearly_freq_rel %>%
  select(-FOX_rel.x, -FOX_rel.y)
  
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")




MSNBC <- read.csv("2_Final_DFs/99_master_dfs_old/master_df_MSNBC.csv")
MSNBC <- MSNBC %>%
  mutate(year = year(date))
MSNBC <- MSNBC %>%
  group_by(year) %>%
  mutate(MSNBC_rel = n()) %>%
  ungroup()
MSNBC <- MSNBC %>% select(-AI_mentions, -title, -text, -date, -medium, -source)
MSNBC <- MSNBC %>%
  distinct(year, .keep_all = TRUE)

MSNBC <- MSNBC %>%
  add_row(year = 2000, MSNBC_rel = NA, .before = 1)
MSNBC <- MSNBC %>%
  add_row(year = 2001, MSNBC_rel = NA, .before = 2)
MSNBC <- MSNBC %>%
  add_row(year = 2002, MSNBC_rel = NA, .before = 3)
MSNBC <- MSNBC %>%
  add_row(year = 2008, MSNBC_rel = NA, .before = 8)
MSNBC <- MSNBC %>%
  add_row(year = 2011, MSNBC_rel = NA, .before = 10)
MSNBC <- MSNBC %>%
  add_row(year = 2024, MSNBC_rel = NA, .before = 10)

MSNBC <- MSNBC %>%
  arrange(year)

yearly_freq_rel <- yearly_freq_rel %>%
  left_join(MSNBC, by = "year")

yearly_freq_rel <- yearly_freq_rel %>%
select(-CNN_rel.x, -NBC_rel, -Fox_rel, -MSNBC_rel.x, -NYT_rel, -WSJ_rel) 


NBC <- read.csv("2_Final_DFs/99_master_dfs_old/master_df_NBC.csv")
NBC <- NBC %>%
  mutate(year = year(date))
NBC <- NBC %>%
  group_by(year) %>%
  mutate(NBC_rel = n()) %>%
  ungroup()
NBC <- NBC %>% select(-AI_mentions, -title, -text, -date, -medium, -source)
NBC <- NBC %>%
  distinct(year, .keep_all = TRUE)

NBC <- NBC %>%
  add_row(year = 2000, NBC_rel = NA, .before = 1)
NBC <- NBC %>%
  add_row(year = 2001, NBC_rel = NA, .before = 2)
NBC <- NBC %>%
  add_row(year = 2002, NBC_rel = NA, .before = 3)

NBC <- NBC %>%
  arrange(year)
NBC <- NBC %>% select(-X)

yearly_freq_rel <- yearly_freq_rel %>%
  left_join(NBC, by = "year")


NYT<- read.csv("2_Final_DFs/99_master_dfs_old/master_df_NYT.csv")
NYT <- NYT %>%
  mutate(year = year(date))
NYT <- NYT %>%
  group_by(year) %>%
  mutate(NYT_rel = n()) %>%
  ungroup()
NYT <- NYT %>% select(-AI_mentions, -title, -text, -date, -medium, -source)
NYT <- NYT %>%
  distinct(year, .keep_all = TRUE)

NYT <- NYT %>%
  arrange(year)

NYT <- NYT %>% select(-X)

yearly_freq_rel <- yearly_freq_rel %>%
  left_join(NYT, by = "year")
yearly_freq_rel <- yearly_freq_rel %>% select(-X, -NYT_rel.x)

write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")

WSJ <- read.csv("2_Final_DFs/99_master_dfs_old/master_df_WSJ.csv")
WSJ <- WSJ %>%
  mutate(year = year(date))
WSJ <- WSJ %>%
  group_by(year) %>%
  mutate(WSJ_rel = n()) %>%
  ungroup()
WSJ <- WSJ %>% select(-AI_mentions, -title, -text, -date, -medium, -source)
WSJ <- WSJ %>%
  distinct(year, .keep_all = TRUE)

WSJ <- WSJ %>%
  arrange(year)

WSJ <- WSJ %>% select(-X)

yearly_freq_rel <- yearly_freq_rel %>%
  left_join(WSJ, by = "year")

yearly_freq_rel <- yearly_freq_rel %>%
rename(FOX_rel = FOX_rel.x)

yearly_freq_rel <- yearly_freq_rel %>%
  rename(CNN_rel = CNN_rel.y)

yearly_freq_rel <- yearly_freq_rel %>%
  rename(MSNBC_rel = MSNBC_rel.y)

yearly_freq_rel <- yearly_freq_rel %>% select(-FOX_rel.y)

write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")

yearly_freq_rel <- yearly_freq_rel %>%
rename(NYT_rel = NYT_rel.y)
write.csv(yearly_freq_rel, "2_Final_DFs/1_Frequency_dfs/master_freq_yearly_total_rel.csv")
