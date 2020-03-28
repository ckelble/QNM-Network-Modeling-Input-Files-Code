#Fuzzy Cognizant Mapping using FCMapper
#Georeges Bank

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(FCMapper); library(here)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Load adjaceny matrix developed using mental modeler
GB <- as.matrix(read.csv(file = here('GB_QNM_adjacencyMat.csv'), row.names = 1))

GB[is.na(GB)] <- 0
concept.names <- row.names(GB)

#Diagnostics
check.matrix(GB)
matrix.indices(GB)
GBConceptIndices <- concept.indices(GB, concept.names)

#Run scenarios
NoChangesResults <- nochanges.scenario(GB, concept.names, 50)

graph.fcm(GB, concept.sizes = NoChangesResults$Equilibrium_value,
          concept.names)

#Climate scenario
GB.CC <- changes.scenario(GB, concept.names, iter=50,
                          set.concepts = c("Bottom Temperature"),
                          set.values = 1)
# Fishing scenario
GB.GF <- changes.scenario(GB, concept.names, iter=50,
                          set.concepts = c("Commercial Groundfish Fishery"),
                          set.values = 1)

# Climate and fishing scenario
GB.CF <- changes.scenario(GB, concept.names, iter=50,
                          set.concepts = c("Commercial Groundfish Fishery", "Bottom Temperature"),
                          set.values = 1)
summary(GB.CF)

#Compare scenarios
CC.compare <- comp.scenarios(NoChangesResults, GB.CC)
GF.compare <- comp.scenarios(NoChangesResults, GB.GF)
CF.compare <- comp.scenarios(NoChangesResults, GB.CF)
comp.maps(concept.names, concept.names)

FCMresults <- list(CC=CC.compare, GF=GF.compare, CF=CF.compare)
save(FCMresults, file = here("FCMresults.RData"))
