library(quanteda)
library(data.table)

# ==============================================================================
# NLP PREP
# ==============================================================================

# ---- LOAD FILES ----
con <- file("data/all_sample_clean.txt", open="rb")
all_sample_clean <- readLines(con, skipNul=T)
close(con)

# ---- TOKENIZE ----
all_tokens <- tokens(x=all_sample_clean)

# ---- N-GRAMS ----
min_freq <- 3

all_1grams <- data.table(data.frame(topfeatures(
  dfm(all_tokens, ngrams=1, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_1grams) <- c("ngram","freq")
all_1grams <- all_1grams[freq>=min_freq]
write.table(x=all_1grams, file="data/all_1grams.txt",
            sep=",", row.names=F, col.names=T, quote=F)

# all_1grams_noStopwords <- data.table(data.frame(topfeatures(
#   dfm(all_tokens, ngrams=1, verbose=F, remove=tm::stopwords("english")), 
#   n=Inf)), keep.rownames=T)
# colnames(all_1grams_noStopwords) <- c("ngram","freq")
# all_1grams_noStopwords <- all_1grams_noStopwords[freq>=min_freq]
# write.table(x=all_1grams_noStopwords, file="data/all_1grams_noStopwords.txt",
#             sep=",", row.names=F, col.names=T, quote=F)

all_2grams <- data.table(data.frame(topfeatures(
  dfm(all_tokens, ngrams=2, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_2grams) <- c("ngram","freq")
all_2grams <- all_2grams[freq>=min_freq]
all_2grams$ngram <- paste0("*",all_2grams$ngram)
write.table(x=all_2grams, file="data/all_2grams.txt",
            sep=",", row.names=F, col.names=T, quote=F)

all_3grams <- data.table(data.frame(topfeatures(
  dfm(all_tokens, ngrams=3, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_3grams) <- c("ngram","freq")
all_3grams <- all_3grams[freq>=min_freq]
all_3grams$ngram <- paste0("*",all_3grams$ngram)
write.table(x=all_3grams, file="data/all_3grams.txt",
            sep=",", row.names=F, col.names=T, quote=F)

all_4grams <- data.table(data.frame(topfeatures(
  dfm(all_tokens, ngrams=4, verbose=F), n=Inf)), keep.rownames=T)
colnames(all_4grams) <- c("ngram","freq")
all_4grams <- all_4grams[freq>=min_freq]
all_4grams$ngram <- paste0("*",all_4grams$ngram)
write.table(x=all_4grams, file="data/all_4grams.txt",
            sep=",", row.names=F, col.names=T, quote=F)
