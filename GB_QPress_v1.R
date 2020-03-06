

########################################################################
# All code was originally obtained from  Melbourne-Thomas et al. 2012. #
########################################################################
# Adapted from Jon Reum's instructions for IEA QNM Workshop
# by Robert Wildermuth, 8/29/2018

#Load the library
library(QPress)


setwd("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB")


###########################################################
# Take a look at the Blue King Crab QNM. 
#     A. Download and install the freeware program Dia (https://sourceforge.net/projects/dia-installer/)
#     B. Open up the file "BKC_CrabClim_v2.6.dia"
#     C. Links terminating in an arrow indicate a positive effect of a node (origin) on another (arrow terminal)
#        Links terminating in a dot indicate a negative effect.  
#        Links that are solid are "certain" to occur; dashed are "uncertain", but if they do occur their sign is known.
#     D. We can add new nodes, move links around here, etc.. The saved .dia file is loaded into R and analyzed.
###############################################################


# 4. Load in the Blue King Crab QNM
edges <- model.dia("GeorgesBank_DiaReDo.dia")

## Examine unweighted adjacency matrix
A <- adjacency.matrix(edges, labels=TRUE)

# Take a peak at the adjacency matrix
A

#Visualize A using:
adjacency.image(edges)

# 1. Build a set of stable matricies, 

sim<- system.simulate(n.sims=1000, edges=edges, 
  sampler = community.sampler(edges), validators = NULL) #We could add additional valudation criteria to filter out matricies that don't repordice a known system behavior 

#The sim object contains the inverse community matrcies, their corresponding edge weights, and a few other things.... 

#Look at the proportion of stable matricies when drawing edge weights from uniform distributions

sim$stable / sim$total #

# 2. Interactively expore how the nodes respond to different press scenarios

impact.barplot(sim)

# Look at how the community responds when nodes are pressed one at a time.
imptable<- impact.table(sim)

#Which node perturbations have similar community outcomes?

imp_dist<- dist(t(imptable))
plot(hclust(imp_dist), main="Perturbation similarity (Euclidean distance)", hang=-1)

imp_dist<- dist(imptable)
plot(hclust(imp_dist), main="Node similarity across perturbations (Euclidean distance)", hang=-1)


#######################################################################
#  To explore which links are important predictors                  #
#  of the sign response of a node see the "tree.R" script.            #
#######################################################################











