#Fuzzy Cognizant Mapping using FCMapper
#Georeges Bank
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "C:/Users/Sean.Lucey/Desktop/QNM/IEA_QNM"
  out.dir  <- "C:/Users/Sean.Lucey/Desktop/QNM/IEA_QNM"
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rpath_code/data"
  out.dir  <- "/home/slucey/slucey/Rpath_code/outputs"
}
data.dir <- "C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB"
out.dir  <- "C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput"
#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(FCMapper)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Load adjaceny matrix developed using mental modeler
GB <- as.matrix(read.csv(file = file.path(data.dir, 'GB_QNM_adjacencyMat.csv'),
                         row.names = 1))
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
CC.compare <- comp.scenarios(GB.CC, NoChangesResults)
GF.compare <- comp.scenarios(GB.GF, NoChangesResults)
CF.compare <- comp.scenarios(GB.CF, NoChangesResults)
comp.maps(concept.names, concept.names)

FCMresults <- list(CC=CC.compare, GF=GF.compare, CF=CF.compare)
#save(FCMresults, file="C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput/FCMresults.RData")
