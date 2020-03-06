

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
 library(QPress)

# 1. Set your workding directory to the "For class" folder, on my computer it's: 
#setwd("/Users/jonathanreum/Desktop/QNM_WG/For class")
setwd("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB")

# 2. Load "Community.R" this code sets up the function that will be simulating the weighted community matricies 
source("Community.R")

# 3. Load "dia.R". This contains code for reading in digraphs made using the Dia program 
filename <- "GeorgesBank_DiaReDo.dia"
source("dia.R")

# 4. Load in the Georges Bank QNM
edges <- model.dia("GeorgesBank_DiaReDo.dia")

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
#impact <- press.impact(edges, perturb=c("BottomTemperature"=1))
impact <- press.impact(edges, perturb=c("BottomTemperature"=1,"CommercialGroundfishFishery"=1))

# 8. Simulate response of the community! 

#use 10000 simulations
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
results

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


#GFresults <- results
#BTresults <- results
#BTandGFres <- results

QNMresults <- list(GFresults=GFresults, BTresults=BTresults, BTandGFres=BTandGFres)

#save(QNMresults, file="C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput/QNMresults.RData")
