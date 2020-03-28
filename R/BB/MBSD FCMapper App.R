#########################################
###  MBSD FCMAPPER Application        ###
#########################################

###Import CSV File and transform into Matrix

filename<-"MBSD_AdjMatrix.csv"

DataFrame = read.csv(filename, sep=",", row.names=1)
AdjMatrix <- as.matrix(DataFrame)
AdjMatrix[is.na(AdjMatrix)] <- 0
concept.names<-row.names(AdjMatrix)

###Check Matrix for use in FCMAPPER

check.matrix(AdjMatrix)

###Get Matrix Indices and Descriptions for the entire matrix and each concept/component

matrix.indices(AdjMatrix)
 
concept.indices(AdjMatrix, concept.names)

###Calculate the base concept sizes for the matrix with a no changes scenario
nochanges.scenario(AdjMatrix, concept.names, 100)
NoChangesResults = nochanges.scenario(AdjMatrix, concept.names, 100)

##Graph the NoChanges Scenario

graph.fcm(AdjMatrix,concept.sizes=NoChangesResults$Equilibrium_value,concept.names)



Scenario1<-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Temperature"),set.values=c(1))
summary(Scenario1)

Scenario2<-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Fishing"),set.values=c(0))
summary(Scenario2)

Scenario3<-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Fishing", "Temperature"),set.values=c(1))
summary(Scenario3)

Scenario4<-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Sediment","Nutrients"),set.values=c(0))
summary(Scenario4)


MaxAll_Full1<-comp.scenarios(NoChangesResults,Scenario1)
MaxAll_Full2<-comp.scenarios(NoChangesResults,Scenario2)
MaxAll_Full3<-comp.scenarios(NoChangesResults,Scenario3)
MaxAll_Full4<-comp.scenarios(NoChangesResults,Scenario4)

MaxTemp <-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Temperature"),set.values=c(1))
summary(MaxTemp)

## Compare Scenarios using the equilibrium concept values of two scenarios
MaxTemp_Full<-comp.scenarios(NoChangesResults,MaxTemp)

############
MaxFishing <-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Fishing"),set.values=c(1))
summary(MaxFishing)

MaxFishing_Full<-comp.scenarios(NoChangesResults,MaxFishing)

########################
MaxAll <-changes.scenario(AdjMatrix,concept.names,iter=25,set.concepts=c("Temperature","Fishing"),set.values=c(1))
summary(MaxAll)

MaxAll_Full<-comp.scenarios(NoChangesResults,MaxAll)


###comp.maps()

