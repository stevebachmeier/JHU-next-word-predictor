# ==============================================================================
# LIBRARIES
# ==============================================================================
library(tm)
library(qdap)
library(quanteda)
library(dplyr)

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
set.seed(101)
twitter_sample <- sample(x=twitter_raw, size=round(length(twitter_raw)*0.01), 
                            replace=F)
news_sample <- sample(x=news_raw, size=round(length(news_raw)*0.01), 
                                          replace=F)
blogs_sample <- sample(x=blogs_raw, size=round(length(blogs_raw)*0.01), 
                                          replace=F)
all_sample <- c(twitter_sample, news_sample, blogs_sample)
# test_sample <- sample(c(twitter_sample, blogs_sample, news_sample), 10, replace=F)

# ==============================================================================
# CLEAN TEXT
# ==============================================================================
cleanWords <- function(x) {
  tolower(x=x) %>%
    iconv(from="UTF-8", to="ASCII", sub="'") %>%
    replace_contraction(sent.cap=F) %>%
    replace_abbreviation() %>%
    sub(pattern="[ .?!][Uu][ .?!]", replacement=" you ") %>%
    removeNumbers() %>%
    gsub(pattern="[.?!/-]", replacement=" ") %>%
    replace_symbol() %>%
    removePunctuation(preserve_intra_word_contractions=T, preserve_intra_word_dashes=T) %>%
    stripWhitespace()
}

# twitter_sample_clean <- cleanWords(twitter_sample)
# news_sample_clean <- cleanWords(news_sample)
# blogs_sample_clean <- cleanWords(blogs_sample)
all_sample_clean <- cleanWords(all_sample)

# ==============================================================================
# EXPLORATORY DATA ANALYSIS
# ==============================================================================

# ---- WORD FREQUENCIES ----
# Full word count (incuding stopwords)
# twitter_freq_wStopwords <- data.frame(freq_terms(text.var=twitter_sample_clean, 
#                                            top=Inf, at.least=1, extend=T))
# news_freq_wStopwords <- data.frame(freq_terms(text.var=news_sample_clean, 
#                                         top=Inf, at.least=1, extend=T))
# blogs_freq_wStopwords <- data.frame(freq_terms(text.var=blogs_sample_clean, 
#                                          top=Inf, at.least=1, extend=T))
all_freq_wStopwords <- data.frame(freq_terms(text.var=all_sample_clean, 
                                               top=Inf, at.least=1, extend=T))

# Top, including stopwords
# twitter_freq20_wStopwords <- data.frame(freq_terms(text.var=twitter_sample_clean, 
#                                              top=20, at.least=1, extend=T))
# news_freq20_wStopwords <- data.frame(freq_terms(text.var=news_sample_clean, top=20, 
#                                           at.least=1, extend=T))
# blogs_freq20_wStopwords <- data.frame(freq_terms(text.var=blogs_sample_clean, top=20,
#                                            at.least=1, extend=T))
all_freq20_wStopwords <- data.frame(freq_terms(text.var=all_sample_clean, top=20,
                                                 at.least=1, extend=T))

# Top, excluding stopwords
# twitter_freq20 <- data.frame(freq_terms(text.var=twitter_sample_clean, top=20, at.least=1, 
#                                         stopwords=tm::stopwords("english"), extend=T))
# news_freq20 <- data.frame(freq_terms(text.var=news_sample_clean, top=20, at.least=1, 
#                                      stopwords=tm::stopwords("english"), extend=T))
# blogs_freq20 <- data.frame(freq_terms(text.var=blogs_sample_clean, top=20, at.least=1, 
#                                       stopwords=tm::stopwords("english"), extend=T))
all_freq20 <- data.frame(freq_terms(text.var=all_sample_clean, top=20, at.least=1, 
                                      stopwords=tm::stopwords("english"), extend=T))

# ---- FREQUENCY PLOTS ----
# Top, with and without stopwords
dev.new()
plot.new()
par(mfrow=c(1,2), mar=c(6.2,6,5.5,0))

barplot(height=all_freq20_wStopwords$FREQ, names.arg=all_freq20_wStopwords$WORD, 
        horiz=F, las=2, main="With stopwords", cex.names=0.8, cex.axis=0.8, cex.main=0.8)

barplot(height=all_freq20$FREQ, names.arg=all_freq20$WORD, horiz=F, 
        las=2, main="No stopwords", cex.names = 0.8, cex.axis=0.8, cex.main=0.8)
mtext("Top 20 unigrams", side=1, line=-2, outer=T, cex=1)
mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

dev.off()
# Top (all sources), with stopwords
# dev.new()
# par(mfrow=c(2,2), mar=c(6.2,6,5.5,0))
# barplot(height=twitter_freq20_wStopwords$FREQ, names.arg=twitter_freq20_wStopwords$WORD, 
#         horiz=F, las=2, main="Twitter")
# barplot(height=news_freq20_wStopwords$FREQ, names.arg=news_freq20_wStopwords$WORD, 
#         horiz=F, las=2, main="News")
# barplot(height=blogs_freq20_wStopwords$FREQ, names.arg=blogs_freq20_wStopwords$WORD, 
#         horiz=F, las=2, main="Blogs")
# barplot(height=all_freq20_wStopwords$FREQ, names.arg=all_freq20_wStopwords$WORD, 
#         horiz=F, las=2, main="Combined")
# mtext("Top 20 words (including stopwords)", side=3, line=-2, outer=T, cex=1)
# mtext("Words", side=1, line=-2, outer=T, cex=0.8)
# mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# Top (all), excluding stopwords
# dev.new()
# par(mfrow=c(2,2), mar=c(6.2,6,5.5,0))
# barplot(height=twitter_freq20$FREQ, names.arg=twitter_freq20$WORD, horiz=F, 
#         las=2, main="Twitter")
# barplot(height=news_freq20$FREQ, names.arg=news_freq20$WORD, horiz=F, 
#         las=2, main="News")
# barplot(height=blogs_freq20$FREQ, names.arg=blogs_freq20$WORD, horiz=F, 
#         las=2, main="Blogs")
# barplot(height=all_freq20$FREQ, names.arg=all_freq20$WORD, horiz=F, 
#         las=2, main="Combined")
# mtext("Top 20 words (excluding stopwords)", side=3, line=-2, outer=T, cex=1)
# mtext("Words", side=1, line=-2, outer=T, cex=0.8)
# mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# ---- TOKENIZE ----
# test_tokens <- tokens(x=test_sample_clean)
twitter_tokens <- tokens(x=twitter_sample_clean)
news_tokens <- tokens(x=news_sample_clean)
blogs_tokens <- tokens(x=blogs_sample_clean)
all_tokens <- tokens(x=all_sample_clean)

# ---- N-GRAMS ----

# Twitter 
twitter_2grams <- data.frame(topfeatures(dfm(twitter_tokens, ngrams=2, verbose=F), n=Inf))
twitter_2grams$names <- rownames(twitter_2grams)
colnames(twitter_2grams)[1] <- "ngrams2"
rownames(twitter_2grams) <- NULL
twitter_2grams <- select(twitter_2grams, names, ngrams2)

twitter_3grams <- data.frame(topfeatures(dfm(twitter_tokens, ngrams=3, verbose=F), n=Inf))
twitter_3grams$names <- rownames(twitter_3grams)
colnames(twitter_3grams)[1] <- "ngrams3"
rownames(twitter_3grams) <- NULL
twitter_3grams <- select(twitter_3grams, names, ngrams3)

twitter_4grams <- data.frame(topfeatures(dfm(twitter_tokens, ngrams=4, verbose=F), n=Inf))
twitter_4grams$names <- rownames(twitter_4grams)
colnames(twitter_4grams)[1] <- "ngrams4"
rownames(twitter_4grams) <- NULL
twitter_4grams <- select(twitter_4grams, names, ngrams4)

# twitter_5grams <- data.frame(topfeatures(dfm(twitter_tokens, ngrams=5, verbose=F), n=Inf))
# twitter_5grams$names <- rownames(twitter_5grams)
# colnames(twitter_5grams)[1] <- "ngrams5"
# rownames(twitter_5grams) <- NULL
# twitter_5grams <- select(twitter_5grams, names, ngrams5)

# News 
news_2grams <- data.frame(topfeatures(dfm(news_tokens, ngrams=2, verbose=F), n=Inf))
news_2grams$names <- rownames(news_2grams)
colnames(news_2grams)[1] <- "ngrams2"
rownames(news_2grams) <- NULL
news_2grams <- select(news_2grams, names, ngrams2)

news_3grams <- data.frame(topfeatures(dfm(news_tokens, ngrams=3, verbose=F), n=Inf))
news_3grams$names <- rownames(news_3grams)
colnames(news_3grams)[1] <- "ngrams3"
rownames(news_3grams) <- NULL
news_3grams <- select(news_3grams, names, ngrams3)

news_4grams <- data.frame(topfeatures(dfm(news_tokens, ngrams=4, verbose=F), n=Inf))
news_4grams$names <- rownames(news_4grams)
colnames(news_4grams)[1] <- "ngrams4"
rownames(news_4grams) <- NULL
news_4grams <- select(news_4grams, names, ngrams4)

# news_5grams <- data.frame(topfeatures(dfm(news_tokens, ngrams=5, verbose=F), n=Inf))
# news_5grams$names <- rownames(news_5grams)
# colnames(news_5grams)[1] <- "ngrams5"
# rownames(news_5grams) <- NULL
# news_5grams <- select(news_5grams, names, ngrams5)

# Blogs 
blogs_2grams <- data.frame(topfeatures(dfm(blogs_tokens, ngrams=2, verbose=F), n=Inf))
blogs_2grams$names <- rownames(blogs_2grams)
colnames(blogs_2grams)[1] <- "ngrams2"
rownames(blogs_2grams) <- NULL
blogs_2grams <- select(blogs_2grams, names, ngrams2)

blogs_3grams <- data.frame(topfeatures(dfm(blogs_tokens, ngrams=3, verbose=F), n=Inf))
blogs_3grams$names <- rownames(blogs_3grams)
colnames(blogs_3grams)[1] <- "ngrams3"
rownames(blogs_3grams) <- NULL
blogs_3grams <- select(blogs_3grams, names, ngrams3)

blogs_4grams <- data.frame(topfeatures(dfm(blogs_tokens, ngrams=4, verbose=F), n=Inf))
blogs_4grams$names <- rownames(blogs_4grams)
colnames(blogs_4grams)[1] <- "ngrams4"
rownames(blogs_4grams) <- NULL
blogs_4grams <- select(blogs_4grams, names, ngrams4)

# blogs_5grams <- data.frame(topfeatures(dfm(blogs_tokens, ngrams=5, verbose=F), n=Inf))
# blogs_5grams$names <- rownames(blogs_5grams)
# colnames(blogs_5grams)[1] <- "ngrams5"
# rownames(blogs_5grams) <- NULL
# blogs_5grams <- select(blogs_5grams, names, ngrams5)

# Combined 
all_2grams <- data.frame(topfeatures(dfm(all_tokens, ngrams=2, verbose=F), n=Inf))
all_2grams$names <- rownames(all_2grams)
colnames(all_2grams)[1] <- "ngrams2"
rownames(all_2grams) <- NULL
all_2grams <- select(all_2grams, names, ngrams2)

all_3grams <- data.frame(topfeatures(dfm(all_tokens, ngrams=3, verbose=F), n=Inf))
all_3grams$names <- rownames(all_3grams)
colnames(all_3grams)[1] <- "ngrams3"
rownames(all_3grams) <- NULL
all_3grams <- select(all_3grams, names, ngrams3)

all_4grams <- data.frame(topfeatures(dfm(all_tokens, ngrams=4, verbose=F), n=Inf))
all_4grams$names <- rownames(all_4grams)
colnames(all_4grams)[1] <- "ngrams4"
rownames(all_4grams) <- NULL
all_4grams <- select(all_4grams, names, ngrams4)

# all_5grams <- data.frame(topfeatures(dfm(all_tokens, ngrams=5, verbose=F), n=Inf))
# all_5grams$names <- rownames(all_5grams)
# colnames(all_5grams)[1] <- "ngrams5"
# rownames(all_5grams) <- NULL
# all_5grams <- select(all_5grams, names, ngrams5)

# ---- N-GRAM PLOTS ----
# 2-gram
dev.new()
par(mfrow=c(2,2), mar=c(5,4,5,0))
barplot(height=twitter_2grams$ngrams2[1:20], names.arg=twitter_2grams$names[1:20], 
        horiz=F, las=2, main="Twitter")
barplot(height=news_2grams$ngrams2[1:20], names.arg=news_2grams$names[1:20], 
        horiz=F, las=2, main="News")
barplot(height=blogs_2grams$ngrams2[1:20], names.arg=blogs_2grams$names[1:20], 
        horiz=F, las=2, main="Blogs")
barplot(height=all_2grams$ngrams2[1:20], names.arg=all_2grams$names[1:20], 
        horiz=F, las=2, main="Combined")
mtext("Top 20 2-grams", side=3, line=-2, outer=T, cex=1)
mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# 3-grams
dev.new()
par(mfrow=c(2,2), mar=c(8,4,5,0))
barplot(height=twitter_3grams$ngrams3[1:20], names.arg=twitter_3grams$names[1:20], 
        horiz=F, las=2, main="Twitter")
barplot(height=news_3grams$ngrams3[1:20], names.arg=news_3grams$names[1:20], 
        horiz=F, las=2, main="News")
barplot(height=blogs_3grams$ngrams3[1:20], names.arg=blogs_3grams$names[1:20], 
        horiz=F, las=2, main="Blogs")
barplot(height=all_3grams$ngrams3[1:20], names.arg=all_3grams$names[1:20], 
        horiz=F, las=2, main="Combined")
mtext("Top 20 3-grams", side=3, line=-2, outer=T, cex=1)
mtext("3-grams", side=1, line=-2, outer=T, cex=0.8)
mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# 4-grams
dev.new()
par(mfrow=c(2,2), mar=c(11,4,5,0))
barplot(height=twitter_4grams$ngrams4[1:20], names.arg=twitter_4grams$names[1:20], 
        horiz=F, las=2, main="Twitter")
barplot(height=news_4grams$ngrams4[1:20], names.arg=news_4grams$names[1:20], 
        horiz=F, las=2, main="News")
barplot(height=blogs_4grams$ngrams4[1:20], names.arg=blogs_4grams$names[1:20], 
        horiz=F, las=2, main="Blogs")
barplot(height=all_4grams$ngrams4[1:20], names.arg=all_4grams$names[1:20], 
        horiz=F, las=2, main="Combined")
mtext("Top 20 4-grams", side=3, line=-2, outer=T, cex=1)
mtext("4-grams", side=1, line=-2, outer=T, cex=0.8)
mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# 5-grams
# dev.new()
# par(mfrow=c(2,2), mar=c(14,4,5,0))
# barplot(height=twitter_5grams$ngrams5[1:20], names.arg=twitter_5grams$names[1:20], 
#         horiz=F, las=2, main="Twitter")
# barplot(height=news_5grams$ngrams5[1:20], names.arg=news_5grams$names[1:20], 
#         horiz=F, las=2, main="News")
# barplot(height=blogs_5grams$ngrams5[1:20], names.arg=blogs_5grams$names[1:20], 
#         horiz=F, las=2, main="Blogs")
# barplot(height=all_5grams$ngrams5[1:20], names.arg=all_5grams$names[1:20], 
#         horiz=F, las=2, main="Combined")
# mtext("Top 20 5-grams", side=3, line=-2, outer=T, cex=1)
# mtext("5-grams", side=1, line=-2, outer=T, cex=0.8)
# mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

dev.off()

# ==============================================================================
# ANALYSIS
# 1. Clean input
# 2. Consider last n (3, 2, or 1) words of inupt.
# 3. Search through n+1 gram for exact match
# 4. Offer top three n+1 words as predictions
#
# Note: it may be better to somehow weight the likelihood of the prediction if
# consider all calculated ngrams instead of just the n+1 gram. ie "I have a"
# might have a better chance of an accurte prediction if we search 4grams for 
# "I have a", 3grams for "have a", and 2grams for "a".
# ==============================================================================

nlpPredictor <- function(input) {
  input <- input
  input_tokens <- cleanWords(input) %>% strsplit(" ")
  search_tokens <- tail(input_tokens[[1]], 3)
  search_string <- paste0(paste(search_tokens, collapse="_"), "_")
  
  if (length(search_tokens) == 3) {
    ngrams_matched <- all_4grams[grep(search_string, all_4grams$names)[1:3],]
  } else if (length(search_tokens) == 2) {
    ngrams_matched <- all_3grams[grep(search_string, all_3grams$names)[1:3],]
  } else {
    ngrams_matched <- all_2grams[grep(search_string, all_2grams$names)[1:3],]
  }
  
  for (i in 1:3) {
    pred[i] <- strsplit(ngrams_matched$names, "_")[[i]][length(strsplit(ngrams_matched$names, "_")[[i]])]
  }
  paste0(input, " (1) ", pred[1], " / (2) ", pred[2], " / (3) ", pred[3])
}

