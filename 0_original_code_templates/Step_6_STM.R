library(stm)
library(quanteda)
library(tidyverse)

# 1. Clean data: Extract 'year' from 'date' and drop rows where it is missing, as it's required for the prevalence covariate
cnn_df$year <- as.numeric(substr(cnn_df$date, 1, 4))
cnn_df <- cnn_df %>% filter(!is.na(year) & !is.na(context_clip))

# 2. Create a corpus
processed <- textProcessor(cnn_df$context_clip, metadata = cnn_df)

# 3. Prepare documents (removes rare words to speed up 25-year analysis)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 5)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Double check the counts now (they should be identical)
length(out$documents)
nrow(out$meta)

stm_model <- stm(documents = out$documents, 
                 vocab = out$vocab, 
                 K = 20, 
                 prevalence =~ s(year), # The s() is for a non-linear spline
                 data = out$meta,
                 init.type = "Spectral")




prep <- estimateEffect(1:20 ~ s(year, 5), 
                       stmobj = stm_model, 
                       meta = meta, 
                       uncertainty = "Global")

# Summary of topic 1's evolution
summary(prep, topics = 1)





plot(prep, "year", 
     method = "continuous", 
     topics = c(1, 5, 10), 
     model = stm_model, 
     printlegend = TRUE, 
     xlab = "Year", 
     main = "Topic Evolution (2000-2025)")



