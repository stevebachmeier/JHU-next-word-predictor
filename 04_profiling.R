# ==============================================================================
# PROFILING
# ==============================================================================
input <- "Hey! How long will this"

Rprof(tmp <- tempfile())
nlpPredictor(input)
Rprof()
summaryRprof(tmp)

library(proftools)
library(Rgraphviz)
dev.new()
plotProfileCallGraph(readProfileData(tmp), score = "total")