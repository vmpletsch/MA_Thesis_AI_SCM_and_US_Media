library(quanteda)

# 1. Define your dictionary based on the article's themes
ai_frames_dict <- dictionary(list(
  politics = c("regulat*", "governance", "china", "policy", "surveillance"),
  economics = c("business", "finance", "labor", "jobs", "market", "startup"),
  science = c("research", "medical", "health*", "algorithm", "develop*"),
  culture = c("entertainment", "game*", "robot*", "fiction", "film")
))

# 2. Apply it to your corpus of transcripts
# Assuming 'my_corpus' is your transcript dataset
my_dfm <- dfm(tokens(my_corpus))
frame_counts <- dfm_lookup(my_dfm, dictionary = ai_frames_dict)

# 3. Convert to a data frame to see which frame dominates each transcript
frame_data <- convert(frame_counts, to = "data.frame")
