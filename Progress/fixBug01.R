library(data.table)
library(dplyr)

ngrams <- data.table(ngram=c("number_is_three", "number_is_five", "number_is_one"), 
                                  freq=c(100, 50, 25), pred=c("three", "five", "one"))
ngrams$S <- ngrams$freq / sum(ngrams$freq)
preds <- data.table(pred=c("four","two","seven","five","nine","three","eight","six","one","ten"), 
                         S=c(0,0,0,0,0,0,0,0,0,0))

# preds2 <- preds
# preds2[preds2$pred %in% ngrams$pred,]$S <- ngrams$S

# GOOD
preds3 <- merge(x=preds, y=ngrams[,3:4], by="pred", all.x=T, sort=F)
preds3[,S.x:=NULL]
colnames(preds3)[2] <- c("S")
preds3$S[is.na(preds3$S)] <- 0

