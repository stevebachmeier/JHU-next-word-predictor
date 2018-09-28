# ==============================================================================
# LIBRARIES
# ==============================================================================
library(tm)
library(qdap)
library(quanteda)
library(dplyr)
library(data.table)

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

all_sample_clean <- cleanWords(all_sample)

# ==============================================================================
# EXPLORATORY DATA ANALYSIS
# ==============================================================================

# ---- WORD FREQUENCIES ----
# Full word count (incuding stopwords)
all_freq_wStopwords <- data.table(freq_terms(text.var=all_sample_clean, 
                                                top=Inf, at.least=1, extend=T))

# Top, including stopwords
all_freq20_wStopwords <- data.table(freq_terms(text.var=all_sample_clean, top=20,
                                                 at.least=1, extend=T))

# Top, excluding stopwords
all_freq20 <- data.table(freq_terms(text.var=all_sample_clean, top=20, at.least=1, 
                                      stopwords=tm::stopwords("english"), extend=T))

# ---- FREQUENCY PLOTS ----
# Top, with and without stopwords
plot.new()
dev.new()
par(mfrow=c(1,2), mar=c(6.2,6,5.5,0))

barplot(height=all_freq20_wStopwords$FREQ, names.arg=all_freq20_wStopwords$WORD, 
        horiz=F, las=2, main="With stopwords", cex.names=0.8, cex.axis=0.8, cex.main=0.8)

barplot(height=all_freq20$FREQ, names.arg=all_freq20$WORD, horiz=F, 
        las=2, main="No stopwords", cex.names = 0.8, cex.axis=0.8, cex.main=0.8)

mtext("Top 20 unigrams", side=1, line=-2, outer=T, cex=1)
mtext("Frequency", side=2, line=-2, outer=T, cex=0.8)

# ---- TOKENIZE ----
all_tokens <- tokens(x=all_sample_clean)

# ---- N-GRAMS ----
all_2grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=2, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_2grams) <- c("ngram","freq")

all_3grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=3, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_3grams) <- c("ngram","freq")

all_4grams <- data.table(data.frame(topfeatures(dfm(all_tokens, ngrams=4, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_4grams) <- c("ngram","freq")

# ---- N-GRAM PLOTS ----
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

dev.off()

# ==============================================================================
# ANALYSIS
# 1. Clean input
# 2. Consider last n (3, 2, or 1) words of inupt.
# 3. Search through n+1 gram for exact match
# 4. Offer top three n+1 words as predictions
# ==============================================================================

nlpPredictor <- function(x) {
  input <- x
  input_tokens <- cleanWords(input) %>% strsplit(" ")
  search_tokens <- tail(input_tokens[[1]], 3)
  search_string <- paste0(paste(search_tokens, collapse="_"), "_")

  if (length(search_tokens) == 3) {
    ngrams_matched <- all_4grams[grep(search_string, all_4grams$ngram)[1:3],]
  } else if (length(search_tokens) == 2) {
    ngrams_matched <- all_3grams[grep(search_string, all_3grams$ngram)[1:3],]
  } else {
    ngrams_matched <- all_2grams[grep(search_string, all_2grams$ngram)[1:3],]
  }

  pred <- matrix(data=NA, nrow=1, ncol=3)

  for (i in 1:3) {
    pred[i] <- strsplit(ngrams_matched$ngram, "_")[[i]][length(strsplit(ngrams_matched$ngram, "_")[[i]])]
  }

  paste0(input, " (1) ", pred[1], " / (2) ", pred[2], " / (3) ", pred[3])

}
