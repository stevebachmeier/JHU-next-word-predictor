# ==============================================================================
# PROFILING
# ==============================================================================

Rprof(tmp <- tempfile())
nlpPredictor("Hey dawg what should i ")
#nlpPredictor("Hey! How long will this")
Rprof()
summaryRprof(tmp)

library(proftools)
library(Rgraphviz)
dev.new()
plotProfileCallGraph(readProfileData(tmp), score = "total")