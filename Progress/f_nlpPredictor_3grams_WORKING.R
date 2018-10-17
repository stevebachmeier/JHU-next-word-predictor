# ==============================================================================
# ANALYSIS
# ==============================================================================
library(data.table)
library(stringr)

# ---- LOAD FILES ----
# all_1grams_nostopwords <- data.table(read.csv(file="data/all_1grams_noStopwords.txt",
#                                               header=T, sep=",", stringsAsFactors=F))
all_1grams <- data.table(read.csv(file="data/all_1grams.txt", 
                                  header=T, sep=",", stringsAsFactors=F))
all_2grams <- data.table(read.csv(file="data/all_2grams.txt", 
                                  header=T, sep=",", stringsAsFactors=F))
all_3grams <- data.table(read.csv(file="data/all_3grams.txt", 
                                  header=T, sep=",", stringsAsFactors=F))
all_4grams <- data.table(read.csv(file="data/all_4grams.txt", 
                                  header=T, sep=",", stringsAsFactors=F))

# ---- STUPID BACKOFF ----
nlpPredictor <- function(input) {
  # input="Hey! How long will this" # profiling
  # input="what time is" # merging bug
  # input="Don't do that! This way we" # check for sub-grepping
  # input="What the hell do you want to do at the end of "
  # input="I like how the same people are in almost all of Adam Sandler's"
  # input="Hey dude! Guess what, I am going "
  alpha <- 0.4 # Stupid backoff factor
  
  ngrams_matched <- data.table(
    ngram=character(), pred=character(), freq=numeric(), S0=numeric())
  ngrams_matched_bo1 <- data.table(
    ngram=character(), pred=character(), freq=numeric(), S_bo1=numeric())
  ngrams_matched_bo2 <- data.table(
    ngram=character(), pred=character(), freq=numeric(), S_bo2=numeric())
  ngrams_matched_bo3 <- data.table(
    ngram=character(), pred=character(), freq=numeric(), S_bo3=numeric())
  
  preds <- data.table(pred=all_1grams$ngram, S_total=0)
  
  # Initial search
  source("f_cleanWords.R")
  input_tokens <- cleanWords(input) %>% strsplit(" ")
  search_tokens <- tail(input_tokens[[1]], 3)
  search_string <- paste0("*", paste(search_tokens, collapse="_"), "_")
  n <- length(search_tokens)
  
  if (n == 3) {
    ngrams_matched <- all_4grams[grep(search_string, all_4grams$ngram, fixed=T),]
  } else if (n == 2) {
    ngrams_matched <- all_3grams[grep(search_string, all_3grams$ngram, fixed=T),]
  } else if (n == 1) {
    ngrams_matched <- all_2grams[grep(search_string, all_2grams$ngram, fixed=T),]
  } else {
    ngrams_matched <- all_1grams
  }
  
  ngrams_matched$pred <- str_remove(ngrams_matched$ngram, 
                                    pattern=paste0("\\", search_string))
  ngrams_matched$S0 <- alpha^0 * ngrams_matched$freq / sum(ngrams_matched$freq)
  
  preds <- merge(x=preds, y=ngrams_matched[,3:4], by="pred", all.x=T, sort=F)
  preds$S0[is.na(preds$S0)] <- 0
  preds$S_total <- preds$S_total + preds$S0
  
  # Stupid backoff #1
  search_tokens_bo1 <- search_tokens[-1]
  search_string_bo1 <- paste0("*", paste(search_tokens_bo1, collapse="_"), "_")
  n_bo1 <- length(search_tokens_bo1)
  
  if ((n_bo1 != n) & (n_bo1 == 2)) {
    ngrams_matched_bo1 <- all_3grams[grep(search_string_bo1, all_3grams$ngram, fixed=T),]
  } else if ((n_bo1 != n) & (n_bo1 == 1)) {
    ngrams_matched_bo1 <- all_2grams[grep(search_string_bo1, all_2grams$ngram, fixed=T),]
  } else if (n_bo1 != n) {
    ngrams_matched_bo1 <- all_1grams
  }
  
  ngrams_matched_bo1$pred <- str_remove(ngrams_matched_bo1$ngram,
                                        pattern=paste0("\\", search_string_bo1))
  ngrams_matched_bo1$S_bo1 <- alpha^1 * (ngrams_matched_bo1$freq / sum(ngrams_matched_bo1$freq))
  
  if (nrow(ngrams_matched_bo1) != 0) {
    preds <- merge(x=preds, y=ngrams_matched_bo1[,3:4], by="pred", all.x=T, sort=F)
    preds$S_bo1[is.na(preds$S_bo1)] <- 0
    preds$S_total <- preds$S_total + preds$S_bo1
  }
  
  # Stupid backoff #2
  search_tokens_bo2 <- search_tokens_bo1[-1]
  search_string_bo2 <- paste0("*", paste(search_tokens_bo2, collapse="_"), "_")
  n_bo2 <- length(search_tokens_bo2)
  
  if ((n_bo2 != n_bo1) & (n_bo2 == 1)) {
    ngrams_matched_bo2 <- all_2grams[grep(search_string_bo2, all_2grams$ngram, fixed=T),]
  } else if (n_bo2 != n_bo1) {
    ngrams_matched_bo2 <- all_1grams
  }
  
  ngrams_matched_bo2$pred <- str_remove(ngrams_matched_bo2$ngram,
                                        pattern=paste0("\\", search_string_bo2))
  ngrams_matched_bo2$S_bo2 <- alpha^2 * (ngrams_matched_bo2$freq / sum(ngrams_matched_bo2$freq))
  
  if (nrow(ngrams_matched_bo2) != 0) {
    preds <- merge(x=preds, y=ngrams_matched_bo2[,3:4], by="pred", all.x=T, sort=F)
    preds$S_bo2[is.na(preds$S_bo2)] <- 0
    preds$S_total <- preds$S_total + preds$S_bo2
  }
  
  # Stupid backoff #3
  if (n_bo2 == 1) {
    ngrams_matched_bo3 <- all_1grams
  }
  
  ngrams_matched_bo3$pred <- ngrams_matched_bo3$ngram
  ngrams_matched_bo3$S_bo3 <- alpha^3 * (ngrams_matched_bo3$freq / sum(ngrams_matched_bo3$freq))
  
  if (nrow(ngrams_matched_bo3) != 0) {
    preds <- merge(x=preds, y=ngrams_matched_bo3[,3:4], by="pred", all.x=T, sort=F)
    preds$S_bo3[is.na(preds$S_bo3)] <- 0
    preds$S_total <- preds$S_total + preds$S_bo3
  }

  # ---- RESULTS ----
  preds <- preds[order(-S_total)]
  
  # benchmark.R
  c(preds[1:3,1])$pred # For benchmark.R
  
  # printout
  # cat(paste(input, ":", "\n",
  #           "  [1] ", preds[1]$pred, "(S=",round(preds[1]$S_total, 2),")", "\n",
  #           "  [2] ", preds[2]$pred, "(S=",round(preds[2]$S_total, 2),")", "\n",
  #           "  [3] ", preds[3]$pred, "(S=",round(preds[3]$S_total, 2),")"))
  
  # preds table
  # preds
}
