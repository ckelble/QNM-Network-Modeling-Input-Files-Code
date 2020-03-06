

########################################################################
# All code was originally obtained from  Melbourne-Thomas et al. 2012. #
########################################################################
# Adapted from Jon Reum's instructions for IEA QNM Workshop
# by Robert Wildermuth, 9/27/2018

# The code was bundled into the R package "QPress", and you are are more than
# welcome to download that package and step through these exercises. I recently
# been having issues getting that package to load, so for now, I'm sticking with 
# the source code. 


# Let's get started! 
 library(QPress); library(here)

# 1. Set your workding directory to the "For class" folder, on my computer it's: 
#setwd("/Users/jonathanreum/Desktop/QNM_WG/For class")
#setwd("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB")
#Using package here to make more transferable


# 2. Load "Community.R" this code sets up the function that will be simulating the weighted community matricies 
source(here("Community.R"))

# 3. Load "dia.R". This contains code for reading in digraphs made using the Dia program 
filename <- "GeorgesBank_DiaReDo.dia"
source(here("dia.R"))

# 4. Load in the Georges Bank QNM
#SML - having an issue with Dia so this is the original code that made the dia from
#mental modeler
MM2Qpress <- function(data){
  
  mental.sheet <- as.data.table(data)
  names(mental.sheet)[1] <- 'Box'
  n <- nrow(mental.sheet)
  box.names <- names(mental.sheet)[which(names(mental.sheet) != 'Box')]
  model <- c()
  for(i in 1:n){
    pos <- which(mental.sheet[i, .SD, .SDcol = box.names] > 0)
    if(length(pos) > 0){
      pos.interaction <- paste(mental.sheet[i, Box], '->', 
                               names(mental.sheet[i, pos + 1, with = F]), 
                               sep = '')
      model <- append(model, pos.interaction)
    }
    
    neg <- which(mental.sheet[i, .SD, .SDcol = box.names] < 0)
    if(length(neg) > 0){
      neg.interaction <- paste(mental.sheet[i, Box], '-*', 
                               names(mental.sheet[i, neg + 1, with = F]), 
                               sep = '')
      model <- append(model, neg.interaction)
    }
  }
  return(model)
}
#function to create qnm models for qpress from signed digraphs
make.qnm<-function(modmat){
  q<-MM2Qpress(modmat)
  qnm<-dput(q)
  qnm.mod<-parse.digraph(qnm)
  qnm.model<-enforce.limitation(qnm.mod)
}

# Read in Georges Bank Mental Modeler adjacency matrix
# SML - Changes made to adjaceny matrix from original
# Removed link from bottom temperature to stratification
# Reversed signs of bottom temperature to habitat nodes (now negative)


adjGB <- read.csv(here("GB_QNM_adjacencyMat.csv"))

# need to change the column and row names to remove spaces and periods
nodeNames <- gsub(".", "", names(adjGB), fixed = TRUE)
names(adjGB) <- nodeNames
adjGB$X <- nodeNames[-1]




qpressGB <- make.qnm(adjGB)

write.dia(qpressGB, here("GB_2.dia"))

edges <- model.dia("GB_2.dia")

## Examine unweighted adjacency matrix
A <- adjacency.matrix(edges, labels=TRUE)

# Take a peak at the adjacency matrix
A

#Visualize using:
adjacency.image(edges)

# 5. Build the function that will sample link weights 

s <- community.sampler(edges)

# 6.  Do we have validation criteria? Do we want to filter out matricies that are able to reproduce a known behavior?
#     For now, we don't, so leave perturb and monitor as NA. This builds a function 

press.val <- press.validate(edges,
                         perturb=NA,
                         monitor=NA )

# 7. Build the function to define the perturbation scenario

#impact <- press.impact(edges, perturb=c("CommercialGroundfishFishery"=1))
#impact  <- press.impact(edges, perturb=c("BottomTemperature"=1))
#impact  <- press.impact(edges, perturb=c("BottomTemperature"=1,"CommercialGroundfishFishery"=1))

# 8. Simulate response of the community! 

#use 10000 simulations
for(scene in 1:3){
  if(scene == 1) impact <- press.impact(edges, perturb=c("CommercialGroundfishFishery"=1))
  if(scene == 2) impact <- press.impact(edges, perturb=c("BottomTemperature"=1))
  if(scene == 3) impact <- press.impact(edges, perturb=c("BottomTemperature"=1,"CommercialGroundfishFishery"=1))

  n.sims <- 10000  #should take about 10 seconds, if longer, there might have build a digraph that nearly always leads to an unstable community. If so, consider including more negative feedbacks, or setting the A diagonal to -1 
  results <- 0
  i <- 0

  gfWeights <- matrix(NA, nrow = n.sims, ncol = ncol(A))

  while(i < n.sims) {

    ## Randomly choose edges to retain
    #z <- s$select(runif(1))
    ## Sample community matrix
    W <- s$community()
    
    # test simulated weighted edges for Groundfish
    gfWeights[i, ] <- W[13,]
  
    ## Check press condition and stability
    if(!(press.val(W) && stable.community(W))) next
  
    ## Monitor impact post press
    imp <- impact(W)
    
    results <- results + outer(signum(imp, epsilon=1.0E-5),-1:1,'==')  #signum classifies the predicted response to -1, 0, 1. Values less abs(epsilon) are rounded down to zero. 
    i <- i+1
  }

  ## Print results
  rownames(results) <- levels(edges$From)
  colnames(results) <- c('-','0','+')


  # look at weights of GF Fishery on Groundfish
  summary(gfWeights[,5])

## Plot outcomes
library(RColorBrewer)
pal <- brewer.pal(n=5,"RdBu")[4:2]
opar <- par(mar=c(5,10,1,1)+0.1)
prop <- results/rowSums(results)
r <- colSums(t(prop)*(-1:1))
barplot(t(prop[order(r),]),
		horiz=T,cex.names=0.8,cex.axis=0.8,las=2,
		border=F,col=pal,xlab="Proportion")
par(opar)


  if(scene == 1) GFresults <- results
  if(scene == 2) BTresults <- results
  if(scene == 3) BTandGFres <- results
}

QNMresults <- list(GFresults=GFresults, BTresults=BTresults, BTandGFres=BTandGFres)

save(QNMresults, file = here("QNMresults.RData"))
