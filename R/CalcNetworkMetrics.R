# Script for calculating network distances and betweenness, etc.

library(igraph)

gbQNM <- read.csv("data/GB/GB_QNM_adjacencyMat.csv")

adjMatQNM <- as.matrix(gbQNM[,-1])
adjMatQNM[is.na(adjMatQNM)] <- 0 # Fill in NAs with zero

nodes <- gsub("[^A-Za-z0-9]","",as.character(gbQNM$X))
dimnames(adjMatQNM) <- list(nodes, nodes)

graphFCM <- graph_from_adjacency_matrix(adjmatrix = adjMatQNM,
                                        mode = "directed",
                                        weighted = TRUE,
                                        diag = FALSE)

as_ids(V(graphFCM))

# Get betweenness for pressed and focal nodes  
betweenness(graphFCM, v = V(graphFCM)[5])

# Try to get shortest path from BT to Groundfish
shortest_paths(graph = graphFCM, from = 5, to = 14, mode = "all", output = "both")

# Try to get shortest path from BT to Seafood
shortest_paths(graph = graphFCM, from = 5, to = 31, mode = "all", output = "vpath")
