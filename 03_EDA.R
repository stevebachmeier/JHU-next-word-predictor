# ==============================================================================
# EXPLORATORY DATA ANALYSIS
# ==============================================================================

# ---- FREQUENCY PLOTS ----
# 1-gram
plot.new()
dev.new()
par(mfrow=c(1,2), mar=c(6.2,6,5.5,0))

barplot(height=all_1grams$freq[1:20], names.arg=all_1grams$ngram[1:20], horiz=F,
        las=2, main="With stopwords", cex.names=0.8, cex.axis=0.8, cex.main=0.8)

barplot(height=all_1grams_noStopwords$freq[1:20],
        names.arg=all_1grams_noStopwords$ngram[1:20], horiz=F, las=2,
        main="No stopwords", cex.names = 0.8, cex.axis=0.8, cex.main=0.8)

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

# 5-grams
plot.new()
dev.new()
par(mfrow=c(1,1), mar=c(10,4,4,0))
barplot(height=all_5grams$freq[1:20], names.arg=all_5grams$ngram[1:20],
        horiz=F, las=2, ylab="Frequency", cex.axis=0.8, cex.names=0.8)
mtext("Top 20 5-grams", side=3, line=-2, outer=T, cex=1)
