library(dplyr)

# ---------------------------------- WEEK 1 ----------------------------------- 

# Quiz
# 1. The en_US.blogs.txt file is how many megabytes?
file.info("data/final/en_US/en_US.blogs.txt")$size/1024/1024

# 2. The en_US.twitter.txt has how many lines of text?
length(twitter_raw)

# 3. What is the lengt of the longest line seen in any of the three en_US data sets?
paste0("twitter: ", max(nchar(twitter_raw)))
paste0("news: ", max(nchar(news_raw)))
paste0("blogs: ", max(nchar(blogs_raw)))

# 4. In the en_US twitter data set, if you divide the number of lines where the word
#    "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase)
#    occurs, about what do you get?
sum(str_count(string=twitter_raw, pattern="love")) / sum(str_count(string=twitter_raw, pattern="hate"))

# 5. The one tweet in the en_US twitter data set that matches the word "biostats" says what?
twitter_raw[grep(pattern="biostats", x=twitter_raw)]

# 6. How many tweets have the exact characters "A computer once beat me at chess,
#    but it was no match for me at kickboxing". (I.e. the line matches those
#    characters exactly.)
length(grep(pattern="A computer once beat me at chess, but it was no match for me at kickboxing", x=twitter_raw))

# ---------------------------------- WEEK 2 -----------------------------------

# 1. Some words are more frequent than others - what are the distributions of word frequencies?
# See plots in analysis.R

# 2. What are the frequencies of 2-grams and 3-grams in the dataset?
# See EDA section in analysis.R 

# 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# Twitter
twitter_total_words <- sum(twitter_freq_wStopwords$FREQ)
paste0("Total unique words, twitter: ", nrow(twitter_freq_wStopwords))
paste0("Total words, twitter: ", twitter_total_words)
x <- 0
i <- 1
while (x<0.5) {
  x <- sum(twitter_freq_wStopwords$FREQ[1:i]) / twitter_total_words
  i <- i + 1
}
paste0("Unique frequency-sorted words for 50% twitter coverage: ", i-1)

x <- 0
i <- 1
while (x<0.9) {
  x <- sum(twitter_freq_wStopwords$FREQ[1:i]) / twitter_total_words
  i <- i + 1
}
paste0("Unique frequency-sorted words for 90% twitter coverage: ", i-1)

# News
news_total_words <- sum(news_freq_wStopwords$FREQ)
paste0("Total unique words, news: ", nrow(news_freq_wStopwords))
paste0("Total words, news: ", news_total_words)
x <- 0
i <- 1
while (x<0.5) {
  x <- sum(news_freq_wStopwords$FREQ[1:i]) / news_total_words
  i <- i + 1
}
paste0("Unique frequency-sorted words for 50% news coverage: ", i-1)

x <- 0
i <- 1
while (x<0.9) {
  x <- sum(news_freq_wStopwords$FREQ[1:i]) / news_total_words
  i <- i + 1
}
paste0("Unique frequency-sorted words for 90% news coverage: ", i-1)

# Blogs
blogs_total_words <- sum(blogs_freq_wStopwords$FREQ)
paste0("Total unique words, blogs: ", nrow(blogs_freq_wStopwords))
paste0("Total words, blogs: ", blogs_total_words)
x <- 0
i <- 1
while (x<0.5) {
  x <- sum(blogs_freq_wStopwords$FREQ[1:i]) / blogs_total_words
  i <- i + 1
}
paste0("Unique frequency-sorted words for 50% blogs coverage: ", i-1)

x <- 0
i <- 1
while (x<0.9) {
  x <- sum(blogs_freq_wStopwords$FREQ[1:i]) / blogs_total_words
  i <- i + 1
}
paste0("Unique frequency-sorted words for 90% blogs coverage: ", i-1)

# 4. How do you evaluate how many of the words come from foreign languages?

# You'd have to compare word-by-word to the english language. There are also
# packages to help with this (textcat and cld2), but they don't seem to work all
# that well.

# 5. Can you think of a way to increase the coverage -- identifying words that 
# may not be in the corpora or using a smaller number of words in the dictionary 
# to cover the same number of phrases?

# I'm not sure here, but I think what it's getting at is removing stopwords.
# That would obviously make the corpora smaller and then the high-frequency
# words would actually have meaning.




