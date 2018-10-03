# ==============================================================================
# LIBRARIES
# ==============================================================================
library(tm)
library(qdap)
library(quanteda)
library(dplyr)
library(data.table)
library(stringr)

# ==============================================================================
# LOAD DATA
# ==============================================================================

# ---- RAW DATA ----
# Twitter
con <- file("data/final/en_US/en_US.twitter.txt", open="rb")
twitter_raw <- readLines(con, skipNul=T)
close(con)

# News
con <- file("data/final/en_US/en_US.news.txt", open="rb")
news_raw <- readLines(con, skipNul=T)
close(con)

# Blogs
con <- file("data/final/en_US/en_US.blogs.txt", open="rb")
blogs_raw <- readLines(con, skipNul=T, encoding="UTF-8")
close(con)

# ---- SAMPLE DATA ----
sample_percentage <- 0.1
set.seed(101)
twitter_sample <- sample(x=twitter_raw, replace=F,
                         size=round(length(twitter_raw)*sample_percentage))
news_sample <- sample(x=news_raw, replace=F,
                      size=round(length(news_raw)*sample_percentage))
blogs_sample <- sample(x=blogs_raw, replace=F,
                       size=round(length(blogs_raw)*sample_percentage))

# all_sample <- c(twitter_sample, news_sample, blogs_sample)

# all_sample <- c("What should I eat? Should I do?",
#                  "Should I go there? Where can I go?",
#                  "What should I say to her?",
#                  "Where should I put it?")

all_sample <- c("I eat apples.", "We eat apples!", "You eat apples.", "They eat bananas?")

# ==============================================================================
# CLEAN TEXT
# ==============================================================================
cleanWords <- function(x) {
  tolower(x=x) %>% # lowercase
    iconv(from="UTF-8", to="ASCII", sub="'") %>% # convert to ASCII
    replace_contraction(sent.cap=F) %>% # separate word contractions
    replace_abbreviation() %>% # replace abbreviations with words
    sub(pattern="[ .?!][Uu][ .?!]", replacement=" you ") %>% # replace "u" with "you"
    removeNumbers() %>% 
    gsub(pattern="[.?!/-]", replacement=" ") %>% # remove some symbols
    replace_symbol() %>% # replace symbols with their words
    removePunctuation(preserve_intra_word_contractions=T, preserve_intra_word_dashes=T) %>%
    stripWhitespace()
}

all_sample_clean <- cleanWords(all_sample)

# ==============================================================================
# EXPLORATORY DATA ANALYSIS
# ==============================================================================

# ---- TOKENIZE ----
all_tokens <- tokens(x=all_sample_clean)

# ---- N-GRAMS ----
all_1grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=1, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_1grams) <- c("ngram","freq")

all_1grams_noStopwords <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=1, verbose=F, remove=tm::stopwords("english")), n=Inf)), keep.rownames=T)
colnames(all_1grams_noStopwords) <- c("ngram","freq")

all_2grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=2, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_2grams) <- c("ngram","freq")

all_3grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=3, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_3grams) <- c("ngram","freq")

all_4grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=4, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_4grams) <- c("ngram","freq")

# ---- FREQUENCY PLOTS ----
# 1-gram
plot.new()
dev.new()
par(mfrow=c(1,2), mar=c(6.2,6,5.5,0))

barplot(height=all_1grams$freq[1:20], names.arg=all_1grams$ngram[1:20], 
        horiz=F, las=2, main="With stopwords", cex.names=0.8, cex.axis=0.8, cex.main=0.8)

barplot(height=all_1grams_noStopwords$freq[1:20], names.arg=all_1grams_noStopwords$ngram[1:20], horiz=F, 
        las=2, main="No stopwords", cex.names = 0.8, cex.axis=0.8, cex.main=0.8)

mtext("Top 20 unigrams", side=1, line=-2, outer=T, cex=1)
mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# 2-gram
dev.new()
par(mfrow=c(1,1), mar=c(4,4,4,0))
barplot(height=all_2grams$freq[1:20], names.arg=all_2grams$ngram[1:20], 
        horiz=F, las=2, ylab="Frequency", cex.axis=0.8, cex.names=0.8)
mtext("Top 20 2-grams", side=3, line=-2, outer=T, cex=1)

# 3-grams
dev.new()
par(mfrow=c(1,1), mar=c(6,4,4,0))
barplot(height=all_3grams$freq[1:20], names.arg=all_3grams$ngram[1:20], 
        horiz=F, las=2, ylab="Frequency", cex.axis=0.8, cex.names=0.8)
mtext("Top 20 3-grams", side=3, line=-2, outer=T, cex=1)

# 4-grams
dev.new()
par(mfrow=c(1,1), mar=c(8,4,4,0))
barplot(height=all_4grams$freq[1:20], names.arg=all_4grams$ngram[1:20], 
        horiz=F, las=2, ylab="Frequency", cex.axis=0.8, cex.names=0.8)
mtext("Top 20 4-grams", side=3, line=-2, outer=T, cex=1)

# ==============================================================================
# ANALYSIS
# ==============================================================================
# 
# nlpPredictor <- function(x) {
#   input <- x
#   input_tokens <- cleanWords(input) %>% strsplit(" ")
#   search_tokens <- tail(input_tokens[[1]], 3)
#   search_string <- paste0(paste(search_tokens, collapse="_"), "_")
# 
#   if (length(search_tokens) == 3) {
#     ngrams_matched <- all_4grams[grep(search_string, all_4grams$ngram)[1:3],]
#   } else if (length(search_tokens) == 2) {
#     ngrams_matched <- all_3grams[grep(search_string, all_3grams$ngram)[1:3],]
#   } else {
#     ngrams_matched <- all_2grams[grep(search_string, all_2grams$ngram)[1:3],]
#   }
# 
#   pred <- matrix(data=NA, nrow=1, ncol=3)
# 
#   for (i in 1:3) {
#     pred[i] <- strsplit(ngrams_matched$ngram, "_")[[i]][length(strsplit(ngrams_matched$ngram, "_")[[i]])]
#   }
# 
#   paste0(input, " (1) ", pred[1], " / (2) ", pred[2], " / (3) ", pred[3])
# 
# }

alpha <- 0.4 # Stupid backoff factor
input <- "I eat "
input_tokens <- cleanWords(input) %>% strsplit(" ")

# Initial search
search_tokens <- tail(input_tokens[[1]], 3)
search_string <- paste0("^", paste(search_tokens, collapse="_"), "_")

ngrams_matched <- data.table(ngram=character(), pred=character(), freq=numeric(), S=numeric())
if (length(search_tokens) == 3) {
  ngrams_matched <- all_4grams[grep(search_string, all_4grams$ngram),]
} else if (length(search_tokens) == 2) {
  ngrams_matched <- all_3grams[grep(search_string, all_3grams$ngram),]
} else {
  ngrams_matched <- all_2grams[grep(search_string, all_2grams$ngram),]
}
ngrams_matched$pred <- str_remove(ngrams_matched$ngram, pattern=search_string)
ngrams_matched$S <- ngrams_matched$freq / sum(ngrams_matched$freq)
pred1 <- data.table(pred=all_1grams$ngram, S=0)
pred1[pred1$pred %in% ngrams_matched$pred,]$S <- ngrams_matched$S

# Stupid backoff
search_tokens2 <- search_tokens[-1]
search_string2 <- paste0("^", paste(search_tokens2, collapse="_"), "_")
ngrams_matched2 <- data.table(ngram=character(), pred=character(), freq=numeric(), S=numeric())
if (length(search_tokens2) == 3) {
  ngrams_matched2 <- all_4grams[grep(search_string2, all_4grams$ngram),]
} else if (length(search_tokens2) == 2) {
  ngrams_matched2 <- all_3grams[grep(search_string2, all_3grams$ngram),]
} else {
  ngrams_matched2 <- all_2grams[grep(search_string2, all_2grams$ngram),]
}
ngrams_matched2$pred <- str_remove(ngrams_matched2$ngram, pattern=search_string2)
ngrams_matched2$S <- alpha * (ngrams_matched2$freq / sum(ngrams_matched2$freq))
pred2 <- data.table(pred=all_1grams$ngram, S=0)
pred2[pred2$pred %in% ngrams_matched2$pred,]$S <- ngrams_matched2$S

# Stupid backoff
search_tokens3 <- search_tokens2[-1]
search_string3 <- paste0("^", paste(search_tokens3, collapse="_"), "_")
ngrams_matched3 <- data.table(ngram=character(), pred=character(), freq=numeric(), S=numeric())
if (length(search_tokens3) == 3) {
  ngrams_matched3 <- all_4grams[grep(search_string3, all_4grams$ngram),]
} else if (length(search_tokens3) == 2) {
  ngrams_matched3 <- all_3grams[grep(search_string3, all_3grams$ngram),]
} else {
  ngrams_matched3 <- all_2grams[grep(search_string3, all_2grams$ngram),]
}
ngrams_matched3$pred <- str_remove(ngrams_matched3$ngram, pattern=search_string3)
ngrams_matched3$S <- alpha^2 * (ngrams_matched3$freq / sum(ngrams_matched3$freq))
pred3 <- data.table(pred=all_1grams$ngram, S=0)
pred3[pred3$pred %in% ngrams_matched3$pred,]$S <- ngrams_matched3$S

# Stupid backoff
search_tokens4 <- search_tokens3[-1]
search_string4 <- paste0("^", paste(search_tokens4, collapse="_"), "_")
ngrams_matched4 <- data.table(ngram=character(), pred=character(), freq=numeric(), S=numeric())
if (length(search_tokens4) == 3) {
  ngrams_matched4 <- all_4grams[grep(search_string4, all_4grams$ngram),]
} else if (length(search_tokens4) == 2) {
  ngrams_matched4 <- all_3grams[grep(search_string4, all_3grams$ngram),]
} else if (length(search_tokens4) == 1) {
  ngrams_matched4 <- all_2grams[grep(search_string4, all_2grams$ngram),]
} else {
  ngrams_matched4 <- all_1grams
}
ngrams_matched4$pred <- ngrams_matched4$ngram
ngrams_matched4$S <- alpha^3 * (ngrams_matched4$freq / sum(ngrams_matched4$freq))
pred4 <- ngrams_matched4[,3:4]

pred <- cbind(pred1, pred2$S, pred3$S, pred4$S)
pred <- data.table(pred=pred$pred, S=pred$S + pred$V2 + pred$V3 + pred$V4)
pred <- pred[order(-S)]

paste0(input, " [1] ", pred[1]$pred, " (", round(pred[1]$S, 2), ") / [2] ", pred[2]$pred, " (", round(pred[2]$S, 2), ") / [3] ", pred[3]$pred, " (", round(pred[3]$S, 2), ")")

