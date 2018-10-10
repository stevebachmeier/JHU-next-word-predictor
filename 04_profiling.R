library(proftools)
library(Rgraphviz)

# ==============================================================================
# PROFILING
# ==============================================================================
input <- "Hey! How long will this"

Rprof(tmp <- tempfile())
nlpPredictor(input)
Rprof()
summaryRprof(tmp)

plot.new()
dev.new()
plotProfileCallGraph(readProfileData(tmp), score = "total")