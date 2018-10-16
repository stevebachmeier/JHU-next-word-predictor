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
sample_percentage <- 0.1
set.seed(101)
twitter_sample <- sample(x=twitter_raw, replace=F,
                         size=round(length(twitter_raw)*sample_percentage))
news_sample <- sample(x=news_raw, replace=F,
                      size=round(length(news_raw)*sample_percentage))
blogs_sample <- sample(x=blogs_raw, replace=F,
                       size=round(length(blogs_raw)*sample_percentage))

all_sample <- c(twitter_sample, news_sample, blogs_sample)

# all_sample <- c("What should I eat? Should I do?",
#                 "Should I go there? Where can I go?",
#                 "What should I say to her?",
#                 "Where should I put it?")

# all_sample <- c("I eat apples.",
#                 "We eat apples!",
#                 "You eat apples.",
#                 "They eat bananas?")

# ==============================================================================
# CLEAN DATA
# ==============================================================================

source("f_cleanWords.R")
all_sample_clean <- cleanWords(all_sample)
write.table(x=all_sample_clean, file="data/all_sample_clean.txt",
            sep=",", row.names=F, col.names=FALSE, quote=F)

# all_sample_clean_no_profanity <- cleanWords_no_profanity(all_sample)
# write.table(x=all_sample_clean_no_profanity, file="data/all_sample_clean_no_profanity.txt",
#             sep=",", row.names=F, col.names=FALSE, quote=F)