

########################################################################
# All code was originally obtained from  Melbourne-Thomas et al. 2012. #
########################################################################

# The code was bundled into the R package "QPress", and you are are more than
# welcome to download that package and step through these exercises. I recently
# been having issues getting that package to load, so you can step through the code in
# this exercise


# Let's get started! 
 

# 1. Set your workding directory to the "For class" folder, on my computer it's: 
setwd("/Users/jonathanreum/Desktop/QNM_WG/For class")

# 2. Load "Community.R" this code sets up the function that will be simulating the weighted community matricies 
source("Community.R")

# 3. Load "dia.R". This contains code for reading in digraphs made using the Dia program 
source("dia.R")

###########################################################
# Take a look at the Blue King Crab QNM. 
#     A. Download and install the freeware program Dia 
#     B. Open up the file "BKC_CrabClim_v2.6.dia"
#     C. Links terminating in an arrow indicate a positive effect of a node (origin) on another (arrow terminal)
#        Links terminating in dot indicate a negative effect.  
#        Links that are solid are "certain" to occur, those that are dashed are "uncertain", but if they do occur their sign is known
#     D. We can add new nodes, movde links around here, etc.. The saved .dia file is then analyzed in R
###############################################################


# 4. Load in the Blue King Crab QNM
#edges <- model.dia("BKC_CrabClim_v2.6.dia")
edgesLA <- model.dia("MBSD_jr_v1.dia")

## Examine unweighted adjacency matrix
#A <- adjacency.matrix(edges, labels=TRUE)

A <- adjacency.matrix(edgesLA, labels=TRUE)

# Take a peak at the adjacency matrix

#Visualize using:
adjacency.image(edgesLA)

# Notice the off diagonals are all 0. That is, we have no self-limitation or negative density dependence. 
# If we have specific ideas about which nodes exhibit self limitation, we can edit the digraph in Dia to add negative self effects 
# Alternatively, we might make the simplifyig assumption that all variables in the system have some 
# self-limitation, or are partially contolled by variables outside the system. In that case,
# we can just add a negative one on the diagonal of A using the below function (e.g., see Raymond et al. 2011, J. App. Ecol.)

## Function to add in -1 on the diagonal generate the community matrix


edgesNeg <- enforce.limitation(edgesLA)

A <- adjacency.matrix(edgesNeg,labels=TRUE)

adjacency.image(edgesNeg)


# Build the function that will sample link weights 
s <- community.sampler(edgesNeg)


# Do we have validation criteria? Do we want to filter out matricies that are able to reproduce a known behavior?
# For now, we don't, so leave perturb and monitor as NA. This builds a function 
press.val <- press.validate(edgesNeg,
                         perturb=NA,
                         monitor=NA )



# Build the function to define the perturbation scenario
impact <- press.impact(edgesNeg, perturb=c("NW"=1))
#impact <- press.impact(edgesNeg, perturb=c("Climate"=1))
#impact <- press.impact(edgesNeg, perturb=c("OA"=1))
#impact <- press.impact(edgesNeg, perturb=c("OA"=1,"Trawlfishery"=1))

# Simulate response of the community! 

#First, do we ever get stable draws from this web? Say out of 10,000


unstab<-0
for (i in 1:10000){
  W <- s$community()
  unstab<- unstab + !stable.community(W)
}

unstab


#use 10000 simulations
n.sims <- 10000  #should take about 10 seconds, if longer, there might have build a digraph that nearly always leads to an unstable community. If so, consider including more negative feedbacks, or setting the A diagonal to -1 
results <- 0
i <- 0



while(i < n.sims) {

  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()

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
results

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







#########################
# Load up Kelbel's FCM  
#########################


A_mat<-as.matrix(read.csv("MBSD_AdjMatrix.csv", row.names=1))




sAdj <- community.samplerAdj(A_mat)



impact <- press.impact(edgesNeg, perturb=c("Trawlfishery"=1))




sAdj$community()



#use 10000 simulations
n.sims <- 10000  #should take about 10 seconds, if longer, there might have build a digraph that nearly always leads to an unstable community. If so, consider including more negative feedbacks, or setting the A diagonal to -1 
results <- 0
i <- 0

while(i < n.sims) {

  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()

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
results

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





#####

















