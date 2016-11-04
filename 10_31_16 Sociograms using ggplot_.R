# To install ggnet use:
devtools::install_github("briatte/ggnet")

# Load libraries
library(ggplot2)
library(ggnet)
library(network)
library(sna)

###############################################################################################

# Sociograms: Top vs Bottom ranked for each NA metric

###############################################################################################

#**********************************************************************************************
# clusterCoef (Top= HAW)
#**********************************************************************************************

# HAW edge list
HAW_net <- network(HAW[,c(4:5)], directed = T)

png('HAW.png', width = 800, height = 600)
ggnet2(HAW_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "red4", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

#*******************************************************************************
# clusterCoef (Bottom = ADEL)
#**********************************************************************************************

# ADEL edge list
ADEL_net <- network(ADEL[,c(4:5)], directed = T)

png('ADEL.png', width = 1200, height = 800)
ggnet2(ADEL_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "steelblue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

#**********************************************************************************************
# degCent (Top= COLL)
#**********************************************************************************************

# COLL edge list
COLL_net <- network(COLL[,c(4:5)], directed = T)

png('COLL.png', width = 800, height = 600)
ggnet2(COLL_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "steelblue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

#**********************************************************************************************
# degCent (Bottom= RICH)
#**********************************************************************************************


#**********************************************************************************************
# netDensity (Top= PORT)
#**********************************************************************************************

PORT_net <- network(PORT[,c(4:5)], directed = T)

png('PORT_circle.png', width = 800, height = 600)
ggnet2(PORT_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "steelblue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

#**********************************************************************************************
# netDensity (Bottom= NMFC)
#**********************************************************************************************

NMFC_net <- network(NMFC[,c(4:5)], directed = T)

png('NMFC_circle.png', width = 800, height = 600)
ggnet2(NMFC_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "black", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

#**********************************************************************************************
# entropy (Top= NMFC)
#**********************************************************************************************


#**********************************************************************************************
# entropy (Bottom= PORT)
#**********************************************************************************************




# High clusterCoef = HAW04_TF

HAW04_TF_net <- network(HAW04_TF[,c(4:5)], directed = T)

png('HAW04_TF_net.png', width = 800, height = 600)
ggnet2(HAW04_TF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "orange", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# High clusterCoef = GEEL04_SDM

GEEL04_SDM_net <- network(GEEL04_SDM[,c(4:5)], directed = T)

png('GEEL04_SDM_net.png', width = 800, height = 600)
ggnet2(GEEL04_SDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "navy", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# Low clusterCoef = RICH03_G

RICH03_G_net <- network(RICH03_G[,c(4:5)], directed = T)

png('RICH03_G_net.png', width = 800, height = 600)
ggnet2(RICH03_G_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "grey", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# Low clusterCoef = GWS01_B

GWS01_B_net <- network(GWS01_B[,c(4:5)], directed = T)

png('GWS01_B_net.png', width = 800, height = 600)
ggnet2(GWS01_B_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "orange", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# Low network density = NMFC03_B

NMFC03_B_net <- network(NMFC03_B[,c(4:5)], directed = T)

png('NMFC03_B_net.png', width = 800, height = 600)
ggnet2(NMFC03_B_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# High network density = STK01_TAM

STK01_TAM_net <- network(STK01_TAM[,c(4:5)], directed = T)

png('STK01_TAM_net.png', width = 800, height = 600)
ggnet2(STK01_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

####################################################################################

# 11_03_16 Plots


####################################################################################
#CLUSTER COEF
####################################################################################

# Lowest cluster coefficient = RICH15_TF

RICH15_TF_net <- network(RICH15_TF[,c(4:5)], directed = T)

png('RICH15_TF_net.png', width = 800, height = 600)
ggnet2(RICH15_TF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Lowest cluster coefficient = WCE16_B

WCE16_B_net <- network(WCE16_B[,c(4:5)], directed = T)

png('WCE16_B_net.png', width = 800, height = 600)
ggnet2(WCE16_B_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 3rd Lowest cluster coefficient = FRE05_G

FRE05_G_net <- network(FRE05_G[,c(4:5)], directed = T)

png('FRE05_G_net.png', width = 800, height = 600)
ggnet2(FRE05_G_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# Highest cluster coefficient = HAW10_SF

HAW10_SF_net <- network(HAW10_SF[,c(4:5)], directed = T)

png('HAW10_SF_net.png', width = 800, height = 600)
ggnet2(HAW10_SF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Highest cluster coefficent = COLL17_SDM

COLL17_SDM_net <- network(COLL17_SDM[,c(4:5)], directed = T)

png('COLL17_SDM_net.png', width = 800, height = 600)
ggnet2(COLL17_SDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 3rd Highest cluster coefficient = HAW04_TF

HAW04_TF_net <- network(HAW04_TF[,c(4:5)], directed = T)

png('HAW04_TF_net.png', width = 800, height = 600)
ggnet2(HAW04_TF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 4th Highest cluster coefficient = HAW04_TDM

HAW04_TDM_net <- network(HAW04_TDM[,c(4:5)], directed = T)

png('HAW04_TDM_net.png', width = 800, height = 600)
ggnet2(HAW04_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# low cluster coefficient = 
GEEL09_TAM_net <- network(GEEL09_TAM[,c(4:5)], directed = T)

png('GEEL09_TAM_net.png', width = 800, height = 600)
ggnet2(GEEL09_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#56B4E9", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()


####################################################################################
#DEGREE CENTRALITY
####################################################################################

# Lowest degree centrality = GEEL21_SAM

GEEL21_SAM_net <- network(GEEL21_SAM[,c(4:5)], directed = T)

png('GEEL21_SAM_net.png', width = 800, height = 600)
ggnet2(GEEL21_SAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#D55E00", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Lowest degree centrality = CARL13_SF

CARL13_SF_net <- network(CARL13_SF[,c(4:5)], directed = T)

png('CARL13_SF_net.png', width = 800, height = 600)
ggnet2(CARL13_SF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# Highest degree centrality = GWS06_TDM

GWS06_TDM_net <- network(GWS06_TDM[,c(4:5)], directed = T)

png('GWS06_TDM_net.png', width = 800, height = 600)
ggnet2(GWS06_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#D55E00", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Highest degree centrality = WB01_TDM

WB01_TDM_net <- network(WB01_TDM[,c(4:5)], directed = T)

png('WB01_TDM_net.png', width = 800, height = 600)
ggnet2(WB01_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 3RD Highest degree centrality = STK20_TD

STK20_TD_net <- network(STK20_TD[,c(4:5)], directed = T)

png('STK20_TD_net.png', width = 800, height = 600)
ggnet2(STK20_TD_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()


####################################################################################
#NETWORK DENSITY
####################################################################################

# Lowest network density = NMFC03_B

NMFC03_B_net <- network(NMFC03_B[,c(4:5)], directed = T)

png('NMFC03_B_net.png', width = 800, height = 600)
ggnet2(NMFC03_B_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#CC79A7", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Lowest network density = PORT01_G

PORT01_G_net <- network(PORT01_G[,c(4:5)], directed = T)

png('PORT01_G_net.png', width = 800, height = 600)
ggnet2(PORT01_G_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 3rd Lowest network density = GCFC02_TF

GCFC02_TF_net <- network(GCFC02_TF[,c(4:5)], directed = T)

png('GCFC02_TF_net.png', width = 800, height = 600)
ggnet2(GCFC02_TF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 4th Lowest network density = HAW01_B

HAW01_B_net <- network(HAW01_B[,c(4:5)], directed = T)

png('HAW01_B_net.png', width = 800, height = 600)
ggnet2(HAW01_B_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# Highest network density = STK01_TAM

STK01_TAM_net <- network(STK01_TAM[,c(4:5)], directed = T)

png('STK01_TAM_net.png', width = 800, height = 600)
ggnet2(STK01_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#CC79A7", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Highest network density = BL01_SDM

BL01_SDM_net <- network(BL01_SDM[,c(4:5)], directed = T)

png('BL01_SDM_net.png', width = 800, height = 600)
ggnet2(BL01_SDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 3rd Highest network density = COLL01_TAM

COLL01_TAM_net <- network(COLL01_TAM[,c(4:5)], directed = T)

png('COLL01_TAM_net.png', width = 800, height = 600)
ggnet2(COLL01_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()


####################################################################################
#ENTROPY
####################################################################################

# Lowest entropy = STK01_TAM

STK01_TAM_net <- network(STK01_TAM[,c(4:5)], directed = T)

png('STK01_TAM_net.png', width = 800, height = 600)
ggnet2(STK01_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#009E73", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Lowest entropy = FRE10_TDM

FRE10_TDM_net <- network(FRE10_TDM[,c(4:5)], directed = T)

png('FRE10_TDM_net.png', width = 800, height = 600)
ggnet2(FRE10_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 2nd Lowest entropy = FRE10_TDM

FRE10_TDM_net <- network(FRE10_TDM[,c(4:5)], directed = T)

png('FRE10_TDM_net.png', width = 800, height = 600)
ggnet2(FRE10_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()

# 3rd Lowest entropy = MELB14_TAM

MELB14_TAM_net <- network( MELB14_TAM[,c(4:5)], directed = T)

png(' MELB14_TAM_net.png', width = 800, height = 600)
ggnet2( MELB14_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "blue", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
dev.off()


# Highest entropy = BL20_G

BL20_G_net <- network(BL20_G[,c(4:5)], directed = T)

png(' BL20_G_net.png', width = 800, height = 600)
ggnet2( BL20_G_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
        node.color = "#009E73", size = "degree", edge.alpha = 0.5,
        legend.position = "none")
dev.off()

# 2nd Highest entropy = RICH15_TF

RICH15_TF_net <- network(RICH15_TF[,c(4:5)], directed = T)

png(' RICH15_TF_net.png', width = 800, height = 600)
ggnet2( RICH15_TF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
        node.color = "blue", size = "degree", edge.alpha = 0.5,
        legend.position = "none")
dev.off()

# 3rd Highest entropy = HAW23_TF

HAW23_TF_net <- network(HAW23_TF[,c(4:5)], directed = T)

png(' HAW23_TF_net.png', width = 800, height = 600)
ggnet2( RICH15_TF_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
        node.color = "blue", size = "degree", edge.alpha = 0.5,
        legend.position = "none")
dev.off()


WB01_TDM_net <- network(WB01_TDM[,c(4:5)], directed = T)

png(' WB01_TDM_net.png', width = 800, height = 600)
WB01_TDM_net<-ggnet2( WB01_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
        node.color = "#009E73", size = "degree", edge.alpha = 0.5,
        legend.position = "none")
WB01_TDM_net <- WB01_TDM_net + ggtitle("Entropy") + theme(plot.title = element_text(size = 40, face = "bold")) 


plot(WB01_TDM_nett )
dev.off()


####################################################################################
#FINAL PLOTS 
####################################################################################

#Cluster coefficient
##low (GEEL09_TAM)
GEEL09_TAM_net <- network(GEEL09_TAM[,c(4:5)], directed = T)

png('GEEL09_TAM_net.png', width = 800, height = 600)
GEEL09_TAM_net <- ggnet2(GEEL09_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#56B4E9", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
GEEL09_TAM_net <- GEEL09_TAM_net + ggtitle("Cluster coefficient") + theme(plot.title = element_text(size = 30, face = "bold", margin = margin(t = 0, b = 40)), plot.margin=unit(c(2,0.5,2,2),"cm")) 
GEEL09_TAM_net <- GEEL09_TAM_net+ labs(x = NULL, y = "TEMP") + theme(axis.title.y = element_text(size = 100, colour = "#FFFFFF", margin = margin(20,0,0,0)))

plot(GEEL09_TAM_net)
dev.off()

##high (FRE19_G)
FRE19_G_net <- network(FRE19_G[,c(4:5)], directed = T)

png('FRE19_G_net.png', width = 800, height = 600)
FRE19_G_net <-ggnet2(FRE19_G_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#56B4E9", size = "degree", edge.alpha = 0.5,
       legend.position = "none")+ theme (plot.margin=unit(c(2,0.5,2,2),"cm"))

dev.off()

#Degree centrlaity
##low (GEEL21_SAM)
GEEL21_SAM_net <- network(GEEL21_SAM[,c(4:5)], directed = T)

png('GEEL21_SAM_net.png', width = 800, height = 600)
GEEL21_SAM_net <- ggnet2(GEEL21_SAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#D55E00", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
GEEL21_SAM_net <- GEEL21_SAM_net + ggtitle("Degree centrality") + theme(plot.title = element_text(size = 30, face = "bold", margin = margin(t = 0, b = 40)), plot.margin=unit(c(2,0.5,2,2),"cm")) 
GEEL21_SAM_net <- GEEL21_SAM_net+ labs(x = NULL, y = "TEMP") + theme(axis.title.y = element_text(size = 100, colour = "#FFFFFF", margin = margin(20,0,0,0)))

plot(GEEL21_SAM_net)
dev.off()

##high (GWS06_TDM)
GWS06_TDM_net <- network(GWS06_TDM[,c(4:5)], directed = T)

png('GWS06_TDM_net.png', width = 800, height = 600)
GWS06_TDM_net <- ggnet2(GWS06_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#D55E00", size = "degree", edge.alpha = 0.5,
       legend.position = "none")+ theme (plot.margin=unit(c(2,0.5,2,2),"cm"))

dev.off()

#Network density
##low (NMFC03_B) 
NMFC03_B_net <- network(NMFC03_B[,c(4:5)], directed = T)

png('NMFC03_B_net.png', width = 800, height = 600)
NMFC03_B_net <- ggnet2(NMFC03_B_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#CC79A7", size = "degree", edge.alpha = 0.5,
       legend.position = "none")
NMFC03_B_net <- NMFC03_B_net + ggtitle("Network density") + theme(plot.title = element_text(size = 30, face = "bold", margin = margin(t = 0, b = 40)), plot.margin=unit(c(2,0.5,2,2),"cm")) 
NMFC03_B_net <- NMFC03_B_net+ labs(x = NULL, y = "TEMP") + theme(axis.title.y = element_text(size = 100, colour = "#FFFFFF", margin = margin(20,0,0,0)))


plot(NMFC03_B_net)
dev.off()

##high (STK01_TAM)* THIS COULD BE BETTER
STK01_TAM_net <- network(STK01_TAM[,c(4:5)], directed = T)

png('STK01_TAM_net.png', width = 800, height = 600)
STK01_TAM_net <- ggnet2(STK01_TAM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
       node.color = "#CC79A7", size = "degree", edge.alpha = 0.5,
       legend.position = "none") + theme (plot.margin=unit(c(2,0.5,2,2),"cm"))

plot(STK01_TAM_net)
dev.off()

#Entropy 
##low (WB01_TDM)
WB01_TDM_net <- network(WB01_TDM[,c(4:5)], directed = T)

png(' WB01_TDM_net.png', width = 800, height = 600)
WB01_TDM_net<-ggnet2( WB01_TDM_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
                      node.color = "#009E73", size = "degree", edge.alpha = 0.5,
                      legend.position = "none")
WB01_TDM_net <- WB01_TDM_net + ggtitle("Entropy") + theme(plot.title = element_text(size = 30, face = "bold", margin = margin(t = 0, b = 40)), plot.margin=unit(c(2,0.5,2,2),"cm")) 
WB01_TDM_net <- WB01_TDM_net+ labs(x = NULL, y = "TEMP") + theme(axis.title.y = element_text(size = 100, colour = "#FFFFFF", margin = margin(20,0,0,0)))

plot(WB01_TDM_net)
dev.off()

##high (BL20_G)
BL20_G_net <- network(BL20_G[,c(4:5)], directed = T)

png(' BL20_G_net.png', width = 800, height = 600)
BL20_G_net <-ggnet2( BL20_G_net, arrow.size = 8, arrow.gap = 0.015, arrow.type = "open", mode = "fruchtermanreingold",
        node.color = "#009E73", size = "degree", edge.alpha = 0.5,
        legend.position = "none") + theme (plot.margin=unit(c(2,0.5,2,2),"cm"))

plot(BL20_G_net)
dev.off()

# Save four-panel plot to PNG
png("plot_sociograms_ALL.png", width = 1600, height = 800)
grid.arrange(GEEL09_TAM_net,GEEL21_SAM_net,NMFC03_B_net,WB01_TDM_net,
             FRE19_G_net, GWS06_TDM_net,STK01_TAM_net,BL20_G_net,nrow=2, ncol=4, widths=c(10,10,10,10))
dev.off()
