library(dplyr)
library(qdap)
library(tm)

# ==============================================================================
# CLEAN TEXT
# ==============================================================================

# ---- LOAD PROFANITY LIST ----
# stopwords_profanity <- scan(file="data/profanity_custom.txt", what="character")

cleanWords <- function(x) {
  tolower(x=x) %>% # lowercase
    iconv(from="UTF-8", to="ASCII", sub="'") %>% # convert to ASCII
    gsub(pattern="[ .?!][Uu][ .?!]", replacement=" you ") %>% # replace "u" with "you"
    gsub(pattern="[ .?!][Uu][']", replacement=" you'") %>%
    gsub(pattern="[ .?!][Cc][ .?!]", replacement=" see ") %>% # replace "c" with "see"
    replace_contraction(sent.cap=F) %>% # separate word contractions
    replace_abbreviation() %>% # replace abbreviations with words
    removeNumbers() %>% 
    gsub(pattern="[.?!/-]", replacement=" ") %>% # remove some symbols
    replace_symbol() %>% # replace symbols with their words
    removePunctuation(preserve_intra_word_contractions=T, preserve_intra_word_dashes=T) %>%
    stripWhitespace()
}
