
library(reshape2)
library(ggplot2)

dat1<-read.csv("results/PI/BKC.csv")
dat1$sys<-  "bkc"
#Remove press nodes
dat1 <- dat1[!dat1$node%in%c("PCfishery", "Halfishery","Trawlfishery","Warming","OA","Hatchery","RKCfishery"),]

dat2<-read.csv("results/GB/GB.csv")
dat2 <- dat2[dat2$model %in% c("FCM", "QNM"), ]
dat2$sys<-  "gb"
# bring in results from changing link sign on QNM and FCM models
dat2$response <- NA
load(file="results/GB/FCMresults.RData")
load(file="results/GB/QNMresults.RData")
FCMresults[[1]]$node <- gsub("[^[:alnum:]]", "", FCMresults[[1]]$Concept)
test1 <- merge(dat2[dat2$model == "FCM" & dat2$press == "warming", ], FCMresults[[1]], by = "node")
dat2[dat2$model == "FCM" & dat2$press == "warming", "response"] <- test1$Percent_change

FCMresults[[2]]$node <- gsub("[^[:alnum:]]", "", FCMresults[[2]]$Concept)
test1 <- merge(dat2[dat2$model == "FCM" & dat2$press == "trawl", ], FCMresults[[2]], by = "node")
dat2[dat2$model == "FCM" & dat2$press == "trawl", "response"] <- test1$Percent_change

FCMresults[[3]]$node <- gsub("[^[:alnum:]]", "", FCMresults[[3]]$Concept)
test1 <- merge(dat2[dat2$model == "FCM" & dat2$press == "tw", ], FCMresults[[3]], by = "node")
dat2[dat2$model == "FCM" & dat2$press == "tw", "response"] <- test1$Percent_change

test3 <- as.data.frame(QNMresults$GFresults) 
test3$node <- row.names(QNMresults$GFresults)
test2 <- merge(dat2[dat2$model == "QNM" & dat2$press == "trawl", ], test3, by = "node")
dat2[dat2$model == "QNM" & dat2$press == "trawl", "response"] <- test2$`+`/10000

test3 <- as.data.frame(QNMresults$BTresults) 
test3$node <- row.names(QNMresults$BTresults)
test2 <- merge(dat2[dat2$model == "QNM" & dat2$press == "warming", ], test3, by = "node")
dat2[dat2$model == "QNM" & dat2$press == "warming", "response"] <- test2$`+`/10000

test3 <- as.data.frame(QNMresults$BTandGFres) 
test3$node <- row.names(QNMresults$BTandGFres)
test2 <- merge(dat2[dat2$model == "QNM" & dat2$press == "tw", ], test3, by = "node")
dat2[dat2$model == "QNM" & dat2$press == "tw", "response"] <- test2$`+`/10000

# Need to rebuild table to include SocCultVals and to bring in the state probabilities from updated BBN results
updatedGB <- read.csv("results/GB/GBupdatedresults_20190919.csv")
newBBN <- data.frame(node = rep(updatedGB$Concept, 3), 
                   model = rep("BBN", 3*31), 
                   press = rep(c("warming", "trawl", "tw"), each = 31), 
                   sys = rep("gb", 3*31))
# Make GB responses relative to uninfluenced posterior baseline
newBBN$response <- c((updatedGB$CC - updatedGB$Posterior)/updatedGB$Posterior, 
                     (updatedGB$GF - updatedGB$Posterior)/updatedGB$Posterior, 
                     (updatedGB$CF - updatedGB$Posterior)/updatedGB$Posterior)
dat2 <- rbind(dat2, newBBN)

#load("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput/FCMresults.RData")
#load("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput/QNMresults.RData")
addSCV <- data.frame(node = rep("SocialCulturalValues", 6),
                     model = rep(c("FCM", "QNM"), each = 3),
                     press = rep(c("warming", "trawl", "tw"), 2), 
                     sys = rep("gb", 6))
addSCV$response <- c(FCMresults$CC[FCMresults$CC$Concept == "Social & Cultural Values", "Percent_change"],
                     FCMresults$GF[FCMresults$GF$Concept == "Social & Cultural Values", "Percent_change"],
                     FCMresults$CF[FCMresults$CF$Concept == "Social & Cultural Values", "Percent_change"],
                     QNMresults$BTresults["SocialCulturalValues", "+"]/10000, 
                     QNMresults$GFresults["SocialCulturalValues", "+"]/10000,
                     QNMresults$BTandGFres["SocialCulturalValues", "+"]/10000)
dat2 <- rbind(dat2, addSCV)
dat2$res2 <- dat2$response

dat3<-read.csv("results/BB/MBSD.csv")
dat3$sys<- "mb"
# Fix some of the names for MBSD
levels(dat3$node) <- c("Birds", "Community Infrastructure", "Cultural", "Erosion", "Farm Land", "Farming", 
                       "Fish", "Fishing", "Flooding", "Habitable Land", "Health and Security", "Housing", 
                       "Hunting", "Invasive Species", "Jobs", "Nutrients", "Navigable Waterways", "Oil and Gas", 
                       "Oil Spills", "Open Water", "Other Animals", "Oysters", "Population Displacement", "Protected Species", 
                       "Recreation", "Subsistance", "SAV", "Sea-level Rise", "Sediment", "Shellfish", 
                       "Storm Surge", "Temperature", "Tourism", "Tropical Storms", "Wetlands")

#BBN: Percent Sign agreement 
dat1$res2[dat1$model=="BBN"]<- (dat1$response[dat1$model=="BBN"] - .5)*2
dat3$res2[dat3$model=="BBN"]<- (dat3$response[dat3$model=="BBN"] - .5)*2

dat<-rbind(dat1,dat2,dat3)

# read in and merge with coloring scheme
cols <- read.csv("data/ComponentGroupings.csv")
cols$System <- tolower(cols$System)

dat <- merge(dat, cols, by.x = c("node", "sys"), by.y = c("Component", "System"), all.x = TRUE)

dat[is.na(dat$sys), ]

#highlight BKC 
#dat$col<-"nonBKC"
#dat$col[dat$node%in%c("BKCA","BKCB","BKCJ","BKCL")]<-"BKC"


#QNM: percent Sign agreement 
dat$res2[dat$model=="QNM"]<- (dat$response[dat$model=="QNM"] - .5)*2


#NEED TO DO: MAKE Relative within run 
dat$res2[dat$model=="FCM"]<- dat$response[dat$model=="FCM"]/100

#Scale FCM relative to abs relative change 
sys<-unique(dat$sys)
press<- unique(dat$press)

for (i in 1:3){

	for (j in 1:3){

		top<- max(abs(dat$response[dat$model=="FCM" & dat$sys==sys[i] & dat$press==press[j]]))

		tscaled<-dat$response[dat$model=="FCM" & dat$sys==sys[i] & dat$press==press[j]]/top

		dat$res2[dat$model=="FCM" & dat$sys==sys[i] & dat$press==press[j]]<- tscaled
	}
}


dat$col<- as.factor(sign(dat$res2))

# RW: need to hard code neutral responses (0 = 10000) for unaffected QNM components
dat[dat$node %in% c("AirTemperature", "BottomSalinity", "DetritusBacteria", "GelatinousZooplankton", "MidAtlanticGroundfish", 
                    "Precipitation",  "RecreationalGroundfishFishery", "SourceWaterProportions", "Stratification", "SurfaceSalinity",              
                    "SurfaceTemperature", "TidalForcing", "Winds") & dat$model == "QNM",
    "col"] <- 0
dat[dat$node %in% c("Benthos", "CopepodsMicronekton", "HabitatPelagic", "PrimaryProduction") & dat$model == "QNM" & dat$press %in% c("trawl", "warming"),
    "col"] <- 0
dat[dat$node %in% c("Benthos", "CopepodsMicronekton", "HabitatPelagic", "PrimaryProduction") & dat$model == "QNM" & dat$press %in% c("tw"),
    "res2"] <- (1/10000)*2
dat[dat$node %in% c("Benthos", "CopepodsMicronekton", "HabitatPelagic", "PrimaryProduction") & dat$model == "QNM" & dat$press %in% c("tw"),
    "col"] <- 1
dat[dat$node %in% c("BottomTemperature", "HabitatNearshore") & dat$model == "QNM" & dat$press %in% c("trawl"),
    "col"] <- 0

# Also for BB QNM model:
dat[dat$node %in% c("Oil and Gas", "Oil Spills") & dat$model == "QNM", "col"] <- 0
dat[dat$node %in% c("Invasive Species", "Navigable Waterways", "Sea-level Rise", 
                    "Storm Surge", "Temperature", "Tropical Storms") & dat$model == "QNM" & dat$press %in% c("trawl"), "col"] <- 0

dat$model <- factor(dat$model,levels(dat$model)[c(3, 1, 2)])

dat$press <- factor(dat$press,levels(dat$press)[c(1, 3, 2)])


col<-c("blue", "grey", "redorange")
names(col)<- c(-1,0,1)

# edited from: https://community.rstudio.com/t/how-to-automatically-add-text-annotations-or-tags-outside-of-faceted-plots/13700/6
tag_facet2 <-  function(p, open=c("(",""), close = c(")","."),
                        tag_fun_top = function(i) letters[i],
                        tag_fun_right = utils::as.roman,
                        x = c(0.1,0), y = c(0.5, 1),
                        hjust = c(0,0), vjust = c(0.5,1), 
                        fontface = c(2,2), ...){
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  
  tags_top <- paste0(tag_fun_top(unique(lay$COL)))
  tags_right <- paste0(tag_fun_right(unique(lay$ROW)))
  
  tl <- lapply(tags_top, grid::textGrob, x=x[1], y=y[1],
               hjust=hjust[1], vjust=vjust[1], gp=grid::gpar(fontface=fontface[1], ...))
  rl <- lapply(tags_right, grid::textGrob, x=x[2], y=y[2],
               hjust=hjust[2], vjust=vjust[2], gp=grid::gpar(fontface=fontface[2],...))
  
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  l <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=l)
  
  # wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
  # g <- gtable::gtable_add_cols(g, wm, pos = max(l))
  # t <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  # g <- gtable::gtable_add_grob(g, grobs = rl, t=t, l=max(l) + 1)
  g <- gtable::gtable_add_cols(g, unit(1,"mm"), pos = max(l))
  
  grid::grid.newpage()
  grid::grid.draw(g)
}

# "BBN" "FCM" "QNM"
test1 <- subset(dat, sys=="mb")
test1$node <- factor(test1$node, levels = unique(test1$node[order(test1$Group)]))
test1$HEX <- as.character(test1$HEX)
test1$Color <- as.character(test1$Color)

test2 <- unique(test1[order(test1$Group), c("node", "Focal", "HEX")])

p <- ggplot(test1) + 	geom_point(aes(x=model, y=node, cex=abs(res2), colour=col)) + 
				facet_wrap(sys~press) + theme_bw() + 
				scale_color_manual(values=c("blue", "grey", "orangered")) + 
				theme(axis.line = element_line(colour = "black"),
    	panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()) + 
				theme(strip.text.x = element_blank()) + 
				geom_vline(xintercept= 2.5, lwd=.5, lty=2, col="grey") +
    theme(axis.text.y = element_text(colour = test2$HEX,
                                     face = ifelse(test2$Focal == "Y", "bold.italic", "plain"))) + 
    labs(x = "Model", y = "Node", subtitle = "") 
    # annotation_custom(grob = textGrob(label = c("Trawl", "Warming", "TW"), hjust = 0, gp = gpar(cex = 1.5)),
    #                   ymin = length(unique(test1$node))+1.5,      # Vertical position of the textGrob
    #                   ymax = length(unique(test1$node))+1.5,
    #                   xmin = 0.25,         # Note: The grobs are positioned outside the plot area
    #                   xmax = 0.25) 
  # annotation_custom(grob = textGrob(label = "Warming", hjust = 0, gp = gpar(cex = 1.5)),
  #                   ymin = length(unique(test1$node))+1,      # Vertical position of the textGrob
  #                   ymax = length(unique(test1$node))+1,
  #                   xmin = 4,         # Note: The grobs are positioned outside the plot area
  #                   xmax = 4) + 
  # annotation_custom(grob = textGrob(label = "TW", hjust = 0, gp = gpar(cex = 1.5)),
  #                   ymin = length(unique(test1$node))+1,      # Vertical position of the textGrob
  #                   ymax = length(unique(test1$node))+1,
  #                   xmin = 8,         # Note: The grobs are positioned outside the plot area
  #                   xmax = 8)



tag_facet2(p, tag_fun_top = function(i) c("Trawl", "Warming", "TW")[i])

####Make scatter plots of outcomes. 


datwide<-dcast(dat, node + press +sys~ model, value.var="res2")

# RW: need to hard code neutral responses (0 = 10000) for unaffected QNM components
datwide[datwide$node %in% c("AirTemperature", "BottomSalinity", "DetritusBacteria", "GelatinousZooplankton", "MidAtlanticGroundfish", 
                            "Precipitation",  "RecreationalGroundfishFishery", "SourceWaterProportions", "Stratification", "SurfaceSalinity",              
                            "SurfaceTemperature", "TidalForcing", "Winds"),
        "QNM"] <- 0
datwide[datwide$node %in% c("Benthos", "CopepodsMicronekton", "HabitatPelagic", "PrimaryProduction") & datwide$press %in% c("trawl", "warming"),
        "QNM"] <- 0
datwide[datwide$node %in% c("BottomTemperature", "HabitatNearshore") & datwide$press %in% c("trawl"),
        "QNM"] <- 0

# Also for BB QNM model:
datwide[datwide$node %in% c("Oil and Gas", "Oil Spills"),  "QNM"] <- 0
datwide[datwide$node %in% c("Invasive Species", "Navigable Waterways", "Sea-level Rise", 
                            "Storm Surge", "Temperature", "Tropical Storms") & datwide$press %in% c("trawl"), "QNM"] <- 0

# Save 'datwide' for reference
write.csv(datwide, file = "results/plottingVals.csv")

ggplot(datwide) + 
geom_rect(aes(xmin=-1, xmax=0, ymin =-1, ymax = 0), 
  fill = "darkolivegreen2", alpha = 1)+

geom_rect(aes(xmin=0, xmax=1, ymin =0, ymax = 1), 
  fill = "darkolivegreen2", alpha = 1)+

geom_rect(aes(xmin=-1, xmax=0, ymin =0, ymax = 1), 
  fill = "grey80", alpha = 1)+

geom_rect(aes(xmin=0, xmax=1, ymin =-1, ymax = 0), 
  fill = "grey80", alpha = 1)+
geom_point(aes(x=BBN, y=FCM, cex=.8, col="grey")) + 
	facet_grid(sys~press, scale="fixed") + 
	theme_bw() +
  	theme(axis.line = element_line(colour = "black"),
    	panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()) + 
  	 scale_x_continuous(name="BBN: Sign agreement",limits=c(-1,1)) + 
  	 scale_y_continuous(name="FCM: Relative change",limits=c(-1,1)) +
  	 scale_color_manual(values = c("grey60")) +
  	 geom_abline(intercept = 0, slope = 1, lty=1)+
  	 geom_vline(xintercept= 0, lwd=.5, lty=2) + 
	 geom_hline(yintercept= 0, lwd=.5, lty=2) + 
   geom_point(aes(BBN, FCM))
	


ggplot(datwide) + 
geom_rect(aes(xmin=-1, xmax=0, ymin =-1, ymax = 0), 
  fill = "darkolivegreen2", alpha = 1)+

geom_rect(aes(xmin=0, xmax=1, ymin =0, ymax = 1), 
  fill = "darkolivegreen2", alpha = 1)+

geom_rect(aes(xmin=-1, xmax=0, ymin =0, ymax = 1), 
  fill = "grey80", alpha = 1)+

geom_rect(aes(xmin=0, xmax=1, ymin =-1, ymax = 0), 
  fill = "grey80", alpha = 1)+
geom_point(aes(x=BBN, y=QNM, cex=.8, col="grey")) + 
	facet_grid(sys~press, scale="fixed") + 
	theme_bw() +
  	theme(axis.line = element_line(colour = "black"),
    	panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()) + 
  	 scale_x_continuous(name="BBN: Sign agreement",limits=c(-1,1)) + 
  	 scale_y_continuous(name="QNM: Sign agreement",limits=c(-1,1)) +
  	 scale_color_manual(values = c("grey60")) +
  	 geom_abline(intercept = 0, slope = 1, lty=1)+
  	 geom_vline(xintercept= 0, lwd=.5, lty=2) + 
	 geom_hline(yintercept= 0, lwd=.5, lty=2) + 
   geom_point(aes(BBN, QNM))


ggplot(datwide) + 

geom_rect(aes(xmin=-1, xmax=0, ymin =-1, ymax = 0), 
  fill = "darkolivegreen2", alpha = 1)+

geom_rect(aes(xmin=0, xmax=1, ymin =0, ymax = 1), 
  fill = "darkolivegreen2", alpha = 1)+

geom_rect(aes(xmin=-1, xmax=0, ymin =0, ymax = 1), 
  fill = "grey80", alpha = 1)+

geom_rect(aes(xmin=0, xmax=1, ymin =-1, ymax = 0), 
  fill = "grey80", alpha = 1)+

  geom_point(aes(x=FCM, y=QNM, cex=.8, col="grey")) + 

	facet_grid(sys~press, scale="fixed") + 
  
	theme_bw() +
  	theme(axis.line = element_line(colour = "black"),
    	panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()) + 
  	 scale_x_continuous(name="FCM: Relative change",limits=c(-1,1)) + 
  	 scale_y_continuous(name="QNM: Sign agreement",limits=c(-1,1)) +
  	 scale_color_manual(values = c("grey60")) +
  	 geom_abline(intercept = 0, slope = 1, lty=1)+
  	 geom_vline(xintercept= 0, lwd=.5, lty=2) + 
	 geom_hline(yintercept= 0, lwd=.5, lty=2) + 
   geom_point(aes(FCM, QNM))
  



###Get aggregate metrics of similarity  

#Make computations easier by making dataframe nested lists:  

datl<- split(datwide, datwide$press)

funS<-function(x){
	split(x, x$sys)
}

datll<-lapply(datl, FUN=funS)

#Functions for metrics we want to compute  

getSA<-function(x1,x2){
  sum(sign(x1)==sign(x2))/length(x1)
}

getMets<-function(x) {

  y<- cbind(x$QNM, x$BBN, x$FCM)

  df<-data.frame(var1=c("QNM","BBN","FCM"), var2=c("BBN","FCM", "QNM"))

  #Correlations
  df$cor<-NA
  df$cor[1]<- cor(y[,1],y[,2])
  df$cor[2]<- cor(y[,2],y[,3])
  df$cor[3]<- cor(y[,3],y[,1])
  
  
 #Sign aggrement 
  df$SA<-NA
  df$SA[1]<- getSA(y[,1],y[,2])
  df$SA[2]<- getSA(y[,2],y[,3])
  df$SA[3]<- getSA(y[,3],y[,1])
  

 #Magnitude 
  df$mag<-NA
  df$mag[1]<- mean(abs(abs(y[,1]) -abs(y[,2])))
  df$mag[2]<- NA
  df$mag[3]<- NA


 #MagnitudeCV
  df$magCV<-NA
  df$magCV[1]<- sd(abs(abs(y[,1]) -abs(y[,2])))/df$mag[1]
  df$magCV[2]<- NA
  df$magCV[3]<- NA

	
	return(df)
}


#Get table 
res<-melt(lapply(datll, FUN=function(x) lapply(x, FUN="getMets")))

t1<-dcast(res,  L1 + var1 + var2 ~ variable + L2,function(x) round(mean(x),2))

#write.csv(t1, file="comm_metrics_20200327.csv")





