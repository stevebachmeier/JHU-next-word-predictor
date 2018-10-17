library(proftools)
library(Rgraphviz)

# ==============================================================================
# PROFILING
# ==============================================================================
source("f_nlpPredictor_4grams_WORKING.R")

input <- "Hey! How long will this"

Rprof(tmp <- tempfile())
nlpPredictor(input)
Rprof()
summaryRprof(tmp)

plot.new()
dev.new()
plotProfileCallGraph(readProfileData(tmp), score = "total")