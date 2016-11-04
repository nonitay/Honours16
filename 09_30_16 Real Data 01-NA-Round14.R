#####
#09-30-16- Real data 14
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#BRISBANE

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Goal_F"
BL14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
BL14_Gg2 <- data.frame(BL14_G)
BL14_Gg2 <- BL14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_Gg2$player1
player2vector <- BL14_Gg2$player2
BL14_Gg3 <- BL14_Gg2
BL14_Gg3$p1inp2vec <- is.element(BL14_Gg3$player1, player2vector)
BL14_Gg3$p2inp1vec <- is.element(BL14_Gg3$player2, player1vector)

addPlayer1 <- BL14_Gg3[ which(BL14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_Gg3[ which(BL14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_Gg2 <- rbind(BL14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
BL14_Gft <- ftable(BL14_Gg2$player1, BL14_Gg2$player2)
BL14_Gft2 <- as.matrix(BL14_Gft)
numRows <- nrow(BL14_Gft2)
numCols <- ncol(BL14_Gft2)
BL14_Gft3 <- BL14_Gft2[c(2:numRows) , c(2:numCols)]
BL14_GTable <- graph.adjacency(BL14_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(BL14_GTable, vertex.label = V(BL14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
BL14_G.clusterCoef <- transitivity(BL14_GTable, type="global") #cluster coefficient
BL14_G.degreeCent <- centralization.degree(BL14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_Gftn <- as.network.matrix(BL14_Gft)
BL14_G.netDensity <- network.density(BL14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_G.entropy <- entropy(BL14_Gft) #entropy

BL14_G.netMx <- cbind(BL14_G.netMx, BL14_G.clusterCoef, BL14_G.degreeCent$centralization,
                      BL14_G.netDensity, BL14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Behind_F"
BL14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
BL14_Bg2 <- data.frame(BL14_B)
BL14_Bg2 <- BL14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_Bg2$player1
player2vector <- BL14_Bg2$player2
BL14_Bg3 <- BL14_Bg2
BL14_Bg3$p1inp2vec <- is.element(BL14_Bg3$player1, player2vector)
BL14_Bg3$p2inp1vec <- is.element(BL14_Bg3$player2, player1vector)

addPlayer1 <- BL14_Bg3[ which(BL14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_Bg3[ which(BL14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_Bg2 <- rbind(BL14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
BL14_Bft <- ftable(BL14_Bg2$player1, BL14_Bg2$player2)
BL14_Bft2 <- as.matrix(BL14_Bft)
numRows <- nrow(BL14_Bft2)
numCols <- ncol(BL14_Bft2)
BL14_Bft3 <- BL14_Bft2[c(2:numRows) , c(2:numCols)]
BL14_BTable <- graph.adjacency(BL14_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(BL14_BTable, vertex.label = V(BL14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
BL14_B.clusterCoef <- transitivity(BL14_BTable, type="global") #cluster coefficient
BL14_B.degreeCent <- centralization.degree(BL14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_Bftn <- as.network.matrix(BL14_Bft)
BL14_B.netDensity <- network.density(BL14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_B.entropy <- entropy(BL14_Bft) #entropy

BL14_B.netMx <- cbind(BL14_B.netMx, BL14_B.clusterCoef, BL14_B.degreeCent$centralization,
                      BL14_B.netDensity, BL14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Stoppage_F"
BL14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
BL14_SFg2 <- data.frame(BL14_SF)
BL14_SFg2 <- BL14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_SFg2$player1
player2vector <- BL14_SFg2$player2
BL14_SFg3 <- BL14_SFg2
BL14_SFg3$p1inp2vec <- is.element(BL14_SFg3$player1, player2vector)
BL14_SFg3$p2inp1vec <- is.element(BL14_SFg3$player2, player1vector)

addPlayer1 <- BL14_SFg3[ which(BL14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_SFg3[ which(BL14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_SFg2 <- rbind(BL14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
BL14_SFft <- ftable(BL14_SFg2$player1, BL14_SFg2$player2)
BL14_SFft2 <- as.matrix(BL14_SFft)
numRows <- nrow(BL14_SFft2)
numCols <- ncol(BL14_SFft2)
BL14_SFft3 <- BL14_SFft2[c(2:numRows) , c(2:numCols)]
BL14_SFTable <- graph.adjacency(BL14_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(BL14_SFTable, vertex.label = V(BL14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
BL14_SF.clusterCoef <- transitivity(BL14_SFTable, type="global") #cluster coefficient
BL14_SF.degreeCent <- centralization.degree(BL14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_SFftn <- as.network.matrix(BL14_SFft)
BL14_SF.netDensity <- network.density(BL14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_SF.entropy <- entropy(BL14_SFft) #entropy

BL14_SF.netMx <- cbind(BL14_SF.netMx, BL14_SF.clusterCoef, BL14_SF.degreeCent$centralization,
                       BL14_SF.netDensity, BL14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Turnover_F"
BL14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
BL14_TFg2 <- data.frame(BL14_TF)
BL14_TFg2 <- BL14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_TFg2$player1
player2vector <- BL14_TFg2$player2
BL14_TFg3 <- BL14_TFg2
BL14_TFg3$p1inp2vec <- is.element(BL14_TFg3$player1, player2vector)
BL14_TFg3$p2inp1vec <- is.element(BL14_TFg3$player2, player1vector)

addPlayer1 <- BL14_TFg3[ which(BL14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_TFg3[ which(BL14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_TFg2 <- rbind(BL14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
BL14_TFft <- ftable(BL14_TFg2$player1, BL14_TFg2$player2)
BL14_TFft2 <- as.matrix(BL14_TFft)
numRows <- nrow(BL14_TFft2)
numCols <- ncol(BL14_TFft2)
BL14_TFft3 <- BL14_TFft2[c(2:numRows) , c(2:numCols)]
BL14_TFTable <- graph.adjacency(BL14_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(BL14_TFTable, vertex.label = V(BL14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
BL14_TF.clusterCoef <- transitivity(BL14_TFTable, type="global") #cluster coefficient
BL14_TF.degreeCent <- centralization.degree(BL14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_TFftn <- as.network.matrix(BL14_TFft)
BL14_TF.netDensity <- network.density(BL14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_TF.entropy <- entropy(BL14_TFft) #entropy

BL14_TF.netMx <- cbind(BL14_TF.netMx, BL14_TF.clusterCoef, BL14_TF.degreeCent$centralization,
                       BL14_TF.netDensity, BL14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
BL14_SAMg2 <- data.frame(BL14_SAM)
BL14_SAMg2 <- BL14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_SAMg2$player1
player2vector <- BL14_SAMg2$player2
BL14_SAMg3 <- BL14_SAMg2
BL14_SAMg3$p1inp2vec <- is.element(BL14_SAMg3$player1, player2vector)
BL14_SAMg3$p2inp1vec <- is.element(BL14_SAMg3$player2, player1vector)

addPlayer1 <- BL14_SAMg3[ which(BL14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_SAMg3[ which(BL14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_SAMg2 <- rbind(BL14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
BL14_SAMft <- ftable(BL14_SAMg2$player1, BL14_SAMg2$player2)
BL14_SAMft2 <- as.matrix(BL14_SAMft)
numRows <- nrow(BL14_SAMft2)
numCols <- ncol(BL14_SAMft2)
BL14_SAMft3 <- BL14_SAMft2[c(2:numRows) , c(2:numCols)]
BL14_SAMTable <- graph.adjacency(BL14_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(BL14_SAMTable, vertex.label = V(BL14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
BL14_SAM.clusterCoef <- transitivity(BL14_SAMTable, type="global") #cluster coefficient
BL14_SAM.degreeCent <- centralization.degree(BL14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_SAMftn <- as.network.matrix(BL14_SAMft)
BL14_SAM.netDensity <- network.density(BL14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_SAM.entropy <- entropy(BL14_SAMft) #entropy

BL14_SAM.netMx <- cbind(BL14_SAM.netMx, BL14_SAM.clusterCoef, BL14_SAM.degreeCent$centralization,
                        BL14_SAM.netDensity, BL14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Turnover_AM"
BL14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
BL14_TAMg2 <- data.frame(BL14_TAM)
BL14_TAMg2 <- BL14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_TAMg2$player1
player2vector <- BL14_TAMg2$player2
BL14_TAMg3 <- BL14_TAMg2
BL14_TAMg3$p1inp2vec <- is.element(BL14_TAMg3$player1, player2vector)
BL14_TAMg3$p2inp1vec <- is.element(BL14_TAMg3$player2, player1vector)

addPlayer1 <- BL14_TAMg3[ which(BL14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_TAMg3[ which(BL14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_TAMg2 <- rbind(BL14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
BL14_TAMft <- ftable(BL14_TAMg2$player1, BL14_TAMg2$player2)
BL14_TAMft2 <- as.matrix(BL14_TAMft)
numRows <- nrow(BL14_TAMft2)
numCols <- ncol(BL14_TAMft2)
BL14_TAMft3 <- BL14_TAMft2[c(2:numRows) , c(2:numCols)]
BL14_TAMTable <- graph.adjacency(BL14_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(BL14_TAMTable, vertex.label = V(BL14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
BL14_TAM.clusterCoef <- transitivity(BL14_TAMTable, type="global") #cluster coefficient
BL14_TAM.degreeCent <- centralization.degree(BL14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_TAMftn <- as.network.matrix(BL14_TAMft)
BL14_TAM.netDensity <- network.density(BL14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_TAM.entropy <- entropy(BL14_TAMft) #entropy

BL14_TAM.netMx <- cbind(BL14_TAM.netMx, BL14_TAM.clusterCoef, BL14_TAM.degreeCent$centralization,
                        BL14_TAM.netDensity, BL14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
BL14_SDMg2 <- data.frame(BL14_SDM)
BL14_SDMg2 <- BL14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_SDMg2$player1
player2vector <- BL14_SDMg2$player2
BL14_SDMg3 <- BL14_SDMg2
BL14_SDMg3$p1inp2vec <- is.element(BL14_SDMg3$player1, player2vector)
BL14_SDMg3$p2inp1vec <- is.element(BL14_SDMg3$player2, player1vector)

addPlayer1 <- BL14_SDMg3[ which(BL14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_SDMg3[ which(BL14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_SDMg2 <- rbind(BL14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
BL14_SDMft <- ftable(BL14_SDMg2$player1, BL14_SDMg2$player2)
BL14_SDMft2 <- as.matrix(BL14_SDMft)
numRows <- nrow(BL14_SDMft2)
numCols <- ncol(BL14_SDMft2)
BL14_SDMft3 <- BL14_SDMft2[c(2:numRows) , c(2:numCols)]
BL14_SDMTable <- graph.adjacency(BL14_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(BL14_SDMTable, vertex.label = V(BL14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
BL14_SDM.clusterCoef <- transitivity(BL14_SDMTable, type="global") #cluster coefficient
BL14_SDM.degreeCent <- centralization.degree(BL14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_SDMftn <- as.network.matrix(BL14_SDMft)
BL14_SDM.netDensity <- network.density(BL14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_SDM.entropy <- entropy(BL14_SDMft) #entropy

BL14_SDM.netMx <- cbind(BL14_SDM.netMx, BL14_SDM.clusterCoef, BL14_SDM.degreeCent$centralization,
                        BL14_SDM.netDensity, BL14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "BL"
KIoutcome = "Turnover_DM"
BL14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
BL14_TDMg2 <- data.frame(BL14_TDM)
BL14_TDMg2 <- BL14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_TDMg2$player1
player2vector <- BL14_TDMg2$player2
BL14_TDMg3 <- BL14_TDMg2
BL14_TDMg3$p1inp2vec <- is.element(BL14_TDMg3$player1, player2vector)
BL14_TDMg3$p2inp1vec <- is.element(BL14_TDMg3$player2, player1vector)

addPlayer1 <- BL14_TDMg3[ which(BL14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_TDMg3[ which(BL14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_TDMg2 <- rbind(BL14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
BL14_TDMft <- ftable(BL14_TDMg2$player1, BL14_TDMg2$player2)
BL14_TDMft2 <- as.matrix(BL14_TDMft)
numRows <- nrow(BL14_TDMft2)
numCols <- ncol(BL14_TDMft2)
BL14_TDMft3 <- BL14_TDMft2[c(2:numRows) , c(2:numCols)]
BL14_TDMTable <- graph.adjacency(BL14_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(BL14_TDMTable, vertex.label = V(BL14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
BL14_TDM.clusterCoef <- transitivity(BL14_TDMTable, type="global") #cluster coefficient
BL14_TDM.degreeCent <- centralization.degree(BL14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_TDMftn <- as.network.matrix(BL14_TDMft)
BL14_TDM.netDensity <- network.density(BL14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_TDM.entropy <- entropy(BL14_TDMft) #entropy

BL14_TDM.netMx <- cbind(BL14_TDM.netMx, BL14_TDM.clusterCoef, BL14_TDM.degreeCent$centralization,
                        BL14_TDM.netDensity, BL14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Stoppage_D"
BL14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
BL14_SDg2 <- data.frame(BL14_SD)
BL14_SDg2 <- BL14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_SDg2$player1
player2vector <- BL14_SDg2$player2
BL14_SDg3 <- BL14_SDg2
BL14_SDg3$p1inp2vec <- is.element(BL14_SDg3$player1, player2vector)
BL14_SDg3$p2inp1vec <- is.element(BL14_SDg3$player2, player1vector)

addPlayer1 <- BL14_SDg3[ which(BL14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_SDg3[ which(BL14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_SDg2 <- rbind(BL14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
BL14_SDft <- ftable(BL14_SDg2$player1, BL14_SDg2$player2)
BL14_SDft2 <- as.matrix(BL14_SDft)
numRows <- nrow(BL14_SDft2)
numCols <- ncol(BL14_SDft2)
BL14_SDft3 <- BL14_SDft2[c(2:numRows) , c(2:numCols)]
BL14_SDTable <- graph.adjacency(BL14_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(BL14_SDTable, vertex.label = V(BL14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
BL14_SD.clusterCoef <- transitivity(BL14_SDTable, type="global") #cluster coefficient
BL14_SD.degreeCent <- centralization.degree(BL14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_SDftn <- as.network.matrix(BL14_SDft)
BL14_SD.netDensity <- network.density(BL14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_SD.entropy <- entropy(BL14_SDft) #entropy

BL14_SD.netMx <- cbind(BL14_SD.netMx, BL14_SD.clusterCoef, BL14_SD.degreeCent$centralization,
                       BL14_SD.netDensity, BL14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "Turnover_D"
BL14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
BL14_TDg2 <- data.frame(BL14_TD)
BL14_TDg2 <- BL14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_TDg2$player1
player2vector <- BL14_TDg2$player2
BL14_TDg3 <- BL14_TDg2
BL14_TDg3$p1inp2vec <- is.element(BL14_TDg3$player1, player2vector)
BL14_TDg3$p2inp1vec <- is.element(BL14_TDg3$player2, player1vector)

addPlayer1 <- BL14_TDg3[ which(BL14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_TDg3[ which(BL14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_TDg2 <- rbind(BL14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
BL14_TDft <- ftable(BL14_TDg2$player1, BL14_TDg2$player2)
BL14_TDft2 <- as.matrix(BL14_TDft)
numRows <- nrow(BL14_TDft2)
numCols <- ncol(BL14_TDft2)
BL14_TDft3 <- BL14_TDft2[c(2:numRows) , c(2:numCols)]
BL14_TDTable <- graph.adjacency(BL14_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(BL14_TDTable, vertex.label = V(BL14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
BL14_TD.clusterCoef <- transitivity(BL14_TDTable, type="global") #cluster coefficient
BL14_TD.degreeCent <- centralization.degree(BL14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_TDftn <- as.network.matrix(BL14_TDft)
BL14_TD.netDensity <- network.density(BL14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_TD.entropy <- entropy(BL14_TDft) #entropy

BL14_TD.netMx <- cbind(BL14_TD.netMx, BL14_TD.clusterCoef, BL14_TD.degreeCent$centralization,
                       BL14_TD.netDensity, BL14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
BL14_QTg2 <- data.frame(BL14_QT)
BL14_QTg2 <- BL14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL14_QTg2$player1
player2vector <- BL14_QTg2$player2
BL14_QTg3 <- BL14_QTg2
BL14_QTg3$p1inp2vec <- is.element(BL14_QTg3$player1, player2vector)
BL14_QTg3$p2inp1vec <- is.element(BL14_QTg3$player2, player1vector)

addPlayer1 <- BL14_QTg3[ which(BL14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL14_QTg3[ which(BL14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL14_QTg2 <- rbind(BL14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
BL14_QTft <- ftable(BL14_QTg2$player1, BL14_QTg2$player2)
BL14_QTft2 <- as.matrix(BL14_QTft)
numRows <- nrow(BL14_QTft2)
numCols <- ncol(BL14_QTft2)
BL14_QTft3 <- BL14_QTft2[c(2:numRows) , c(2:numCols)]
BL14_QTTable <- graph.adjacency(BL14_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(BL14_QTTable, vertex.label = V(BL14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
BL14_QT.clusterCoef <- transitivity(BL14_QTTable, type="global") #cluster coefficient
BL14_QT.degreeCent <- centralization.degree(BL14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL14_QTftn <- as.network.matrix(BL14_QTft)
BL14_QT.netDensity <- network.density(BL14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL14_QT.entropy <- entropy(BL14_QTft) #entropy

BL14_QT.netMx <- cbind(BL14_QT.netMx, BL14_QT.clusterCoef, BL14_QT.degreeCent$centralization,
                       BL14_QT.netDensity, BL14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL14_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "Goal_F"
CARL14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
CARL14_Gg2 <- data.frame(CARL14_G)
CARL14_Gg2 <- CARL14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_Gg2$player1
player2vector <- CARL14_Gg2$player2
CARL14_Gg3 <- CARL14_Gg2
CARL14_Gg3$p1inp2vec <- is.element(CARL14_Gg3$player1, player2vector)
CARL14_Gg3$p2inp1vec <- is.element(CARL14_Gg3$player2, player1vector)

addPlayer1 <- CARL14_Gg3[ which(CARL14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_Gg3[ which(CARL14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_Gg2 <- rbind(CARL14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
CARL14_Gft <- ftable(CARL14_Gg2$player1, CARL14_Gg2$player2)
CARL14_Gft2 <- as.matrix(CARL14_Gft)
numRows <- nrow(CARL14_Gft2)
numCols <- ncol(CARL14_Gft2)
CARL14_Gft3 <- CARL14_Gft2[c(2:numRows) , c(2:numCols)]
CARL14_GTable <- graph.adjacency(CARL14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(CARL14_GTable, vertex.label = V(CARL14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
CARL14_G.clusterCoef <- transitivity(CARL14_GTable, type="global") #cluster coefficient
CARL14_G.degreeCent <- centralization.degree(CARL14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_Gftn <- as.network.matrix(CARL14_Gft)
CARL14_G.netDensity <- network.density(CARL14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_G.entropy <- entropy(CARL14_Gft) #entropy

CARL14_G.netMx <- cbind(CARL14_G.netMx, CARL14_G.clusterCoef, CARL14_G.degreeCent$centralization,
                        CARL14_G.netDensity, CARL14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "Behind_F"
CARL14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
CARL14_Bg2 <- data.frame(CARL14_B)
CARL14_Bg2 <- CARL14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_Bg2$player1
player2vector <- CARL14_Bg2$player2
CARL14_Bg3 <- CARL14_Bg2
CARL14_Bg3$p1inp2vec <- is.element(CARL14_Bg3$player1, player2vector)
CARL14_Bg3$p2inp1vec <- is.element(CARL14_Bg3$player2, player1vector)

addPlayer1 <- CARL14_Bg3[ which(CARL14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_Bg3[ which(CARL14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_Bg2 <- rbind(CARL14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
CARL14_Bft <- ftable(CARL14_Bg2$player1, CARL14_Bg2$player2)
CARL14_Bft2 <- as.matrix(CARL14_Bft)
numRows <- nrow(CARL14_Bft2)
numCols <- ncol(CARL14_Bft2)
CARL14_Bft3 <- CARL14_Bft2[c(2:numRows) , c(2:numCols)]
CARL14_BTable <- graph.adjacency(CARL14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(CARL14_BTable, vertex.label = V(CARL14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
CARL14_B.clusterCoef <- transitivity(CARL14_BTable, type="global") #cluster coefficient
CARL14_B.degreeCent <- centralization.degree(CARL14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_Bftn <- as.network.matrix(CARL14_Bft)
CARL14_B.netDensity <- network.density(CARL14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_B.entropy <- entropy(CARL14_Bft) #entropy

CARL14_B.netMx <- cbind(CARL14_B.netMx, CARL14_B.clusterCoef, CARL14_B.degreeCent$centralization,
                        CARL14_B.netDensity, CARL14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
CARL14_SFg2 <- data.frame(CARL14_SF)
CARL14_SFg2 <- CARL14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_SFg2$player1
player2vector <- CARL14_SFg2$player2
CARL14_SFg3 <- CARL14_SFg2
CARL14_SFg3$p1inp2vec <- is.element(CARL14_SFg3$player1, player2vector)
CARL14_SFg3$p2inp1vec <- is.element(CARL14_SFg3$player2, player1vector)

addPlayer1 <- CARL14_SFg3[ which(CARL14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_SFg3[ which(CARL14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_SFg2 <- rbind(CARL14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
CARL14_SFft <- ftable(CARL14_SFg2$player1, CARL14_SFg2$player2)
CARL14_SFft2 <- as.matrix(CARL14_SFft)
numRows <- nrow(CARL14_SFft2)
numCols <- ncol(CARL14_SFft2)
CARL14_SFft3 <- CARL14_SFft2[c(2:numRows) , c(2:numCols)]
CARL14_SFTable <- graph.adjacency(CARL14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(CARL14_SFTable, vertex.label = V(CARL14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
CARL14_SF.clusterCoef <- transitivity(CARL14_SFTable, type="global") #cluster coefficient
CARL14_SF.degreeCent <- centralization.degree(CARL14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_SFftn <- as.network.matrix(CARL14_SFft)
CARL14_SF.netDensity <- network.density(CARL14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_SF.entropy <- entropy(CARL14_SFft) #entropy

CARL14_SF.netMx <- cbind(CARL14_SF.netMx, CARL14_SF.clusterCoef, CARL14_SF.degreeCent$centralization,
                         CARL14_SF.netDensity, CARL14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
CARL14_TFg2 <- data.frame(CARL14_TF)
CARL14_TFg2 <- CARL14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_TFg2$player1
player2vector <- CARL14_TFg2$player2
CARL14_TFg3 <- CARL14_TFg2
CARL14_TFg3$p1inp2vec <- is.element(CARL14_TFg3$player1, player2vector)
CARL14_TFg3$p2inp1vec <- is.element(CARL14_TFg3$player2, player1vector)

addPlayer1 <- CARL14_TFg3[ which(CARL14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_TFg3[ which(CARL14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_TFg2 <- rbind(CARL14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
CARL14_TFft <- ftable(CARL14_TFg2$player1, CARL14_TFg2$player2)
CARL14_TFft2 <- as.matrix(CARL14_TFft)
numRows <- nrow(CARL14_TFft2)
numCols <- ncol(CARL14_TFft2)
CARL14_TFft3 <- CARL14_TFft2[c(2:numRows) , c(2:numCols)]
CARL14_TFTable <- graph.adjacency(CARL14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(CARL14_TFTable, vertex.label = V(CARL14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
CARL14_TF.clusterCoef <- transitivity(CARL14_TFTable, type="global") #cluster coefficient
CARL14_TF.degreeCent <- centralization.degree(CARL14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_TFftn <- as.network.matrix(CARL14_TFft)
CARL14_TF.netDensity <- network.density(CARL14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_TF.entropy <- entropy(CARL14_TFft) #entropy

CARL14_TF.netMx <- cbind(CARL14_TF.netMx, CARL14_TF.clusterCoef, CARL14_TF.degreeCent$centralization,
                         CARL14_TF.netDensity, CARL14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
CARL14_SAMg2 <- data.frame(CARL14_SAM)
CARL14_SAMg2 <- CARL14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_SAMg2$player1
player2vector <- CARL14_SAMg2$player2
CARL14_SAMg3 <- CARL14_SAMg2
CARL14_SAMg3$p1inp2vec <- is.element(CARL14_SAMg3$player1, player2vector)
CARL14_SAMg3$p2inp1vec <- is.element(CARL14_SAMg3$player2, player1vector)

addPlayer1 <- CARL14_SAMg3[ which(CARL14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_SAMg3[ which(CARL14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_SAMg2 <- rbind(CARL14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
CARL14_SAMft <- ftable(CARL14_SAMg2$player1, CARL14_SAMg2$player2)
CARL14_SAMft2 <- as.matrix(CARL14_SAMft)
numRows <- nrow(CARL14_SAMft2)
numCols <- ncol(CARL14_SAMft2)
CARL14_SAMft3 <- CARL14_SAMft2[c(2:numRows) , c(2:numCols)]
CARL14_SAMTable <- graph.adjacency(CARL14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(CARL14_SAMTable, vertex.label = V(CARL14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
CARL14_SAM.clusterCoef <- transitivity(CARL14_SAMTable, type="global") #cluster coefficient
CARL14_SAM.degreeCent <- centralization.degree(CARL14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_SAMftn <- as.network.matrix(CARL14_SAMft)
CARL14_SAM.netDensity <- network.density(CARL14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_SAM.entropy <- entropy(CARL14_SAMft) #entropy

CARL14_SAM.netMx <- cbind(CARL14_SAM.netMx, CARL14_SAM.clusterCoef, CARL14_SAM.degreeCent$centralization,
                          CARL14_SAM.netDensity, CARL14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
CARL14_TAMg2 <- data.frame(CARL14_TAM)
CARL14_TAMg2 <- CARL14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_TAMg2$player1
player2vector <- CARL14_TAMg2$player2
CARL14_TAMg3 <- CARL14_TAMg2
CARL14_TAMg3$p1inp2vec <- is.element(CARL14_TAMg3$player1, player2vector)
CARL14_TAMg3$p2inp1vec <- is.element(CARL14_TAMg3$player2, player1vector)

addPlayer1 <- CARL14_TAMg3[ which(CARL14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL14_TAMg3[ which(CARL14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_TAMg2 <- rbind(CARL14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
CARL14_TAMft <- ftable(CARL14_TAMg2$player1, CARL14_TAMg2$player2)
CARL14_TAMft2 <- as.matrix(CARL14_TAMft)
numRows <- nrow(CARL14_TAMft2)
numCols <- ncol(CARL14_TAMft2)
CARL14_TAMft3 <- CARL14_TAMft2[c(2:numRows) , c(2:numCols)]
CARL14_TAMTable <- graph.adjacency(CARL14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(CARL14_TAMTable, vertex.label = V(CARL14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
CARL14_TAM.clusterCoef <- transitivity(CARL14_TAMTable, type="global") #cluster coefficient
CARL14_TAM.degreeCent <- centralization.degree(CARL14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_TAMftn <- as.network.matrix(CARL14_TAMft)
CARL14_TAM.netDensity <- network.density(CARL14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_TAM.entropy <- entropy(CARL14_TAMft) #entropy

CARL14_TAM.netMx <- cbind(CARL14_TAM.netMx, CARL14_TAM.clusterCoef, CARL14_TAM.degreeCent$centralization,
                          CARL14_TAM.netDensity, CARL14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
CARL14_SDMg2 <- data.frame(CARL14_SDM)
CARL14_SDMg2 <- CARL14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_SDMg2$player1
player2vector <- CARL14_SDMg2$player2
CARL14_SDMg3 <- CARL14_SDMg2
CARL14_SDMg3$p1inp2vec <- is.element(CARL14_SDMg3$player1, player2vector)
CARL14_SDMg3$p2inp1vec <- is.element(CARL14_SDMg3$player2, player1vector)

addPlayer1 <- CARL14_SDMg3[ which(CARL14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_SDMg3[ which(CARL14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_SDMg2 <- rbind(CARL14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
CARL14_SDMft <- ftable(CARL14_SDMg2$player1, CARL14_SDMg2$player2)
CARL14_SDMft2 <- as.matrix(CARL14_SDMft)
numRows <- nrow(CARL14_SDMft2)
numCols <- ncol(CARL14_SDMft2)
CARL14_SDMft3 <- CARL14_SDMft2[c(2:numRows) , c(2:numCols)]
CARL14_SDMTable <- graph.adjacency(CARL14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(CARL14_SDMTable, vertex.label = V(CARL14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
CARL14_SDM.clusterCoef <- transitivity(CARL14_SDMTable, type="global") #cluster coefficient
CARL14_SDM.degreeCent <- centralization.degree(CARL14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_SDMftn <- as.network.matrix(CARL14_SDMft)
CARL14_SDM.netDensity <- network.density(CARL14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_SDM.entropy <- entropy(CARL14_SDMft) #entropy

CARL14_SDM.netMx <- cbind(CARL14_SDM.netMx, CARL14_SDM.clusterCoef, CARL14_SDM.degreeCent$centralization,
                          CARL14_SDM.netDensity, CARL14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
CARL14_TDMg2 <- data.frame(CARL14_TDM)
CARL14_TDMg2 <- CARL14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_TDMg2$player1
player2vector <- CARL14_TDMg2$player2
CARL14_TDMg3 <- CARL14_TDMg2
CARL14_TDMg3$p1inp2vec <- is.element(CARL14_TDMg3$player1, player2vector)
CARL14_TDMg3$p2inp1vec <- is.element(CARL14_TDMg3$player2, player1vector)

addPlayer1 <- CARL14_TDMg3[ which(CARL14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_TDMg3[ which(CARL14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_TDMg2 <- rbind(CARL14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
CARL14_TDMft <- ftable(CARL14_TDMg2$player1, CARL14_TDMg2$player2)
CARL14_TDMft2 <- as.matrix(CARL14_TDMft)
numRows <- nrow(CARL14_TDMft2)
numCols <- ncol(CARL14_TDMft2)
CARL14_TDMft3 <- CARL14_TDMft2[c(2:numRows) , c(2:numCols)]
CARL14_TDMTable <- graph.adjacency(CARL14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(CARL14_TDMTable, vertex.label = V(CARL14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
CARL14_TDM.clusterCoef <- transitivity(CARL14_TDMTable, type="global") #cluster coefficient
CARL14_TDM.degreeCent <- centralization.degree(CARL14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_TDMftn <- as.network.matrix(CARL14_TDMft)
CARL14_TDM.netDensity <- network.density(CARL14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_TDM.entropy <- entropy(CARL14_TDMft) #entropy

CARL14_TDM.netMx <- cbind(CARL14_TDM.netMx, CARL14_TDM.clusterCoef, CARL14_TDM.degreeCent$centralization,
                          CARL14_TDM.netDensity, CARL14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
CARL14_SDg2 <- data.frame(CARL14_SD)
CARL14_SDg2 <- CARL14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_SDg2$player1
player2vector <- CARL14_SDg2$player2
CARL14_SDg3 <- CARL14_SDg2
CARL14_SDg3$p1inp2vec <- is.element(CARL14_SDg3$player1, player2vector)
CARL14_SDg3$p2inp1vec <- is.element(CARL14_SDg3$player2, player1vector)

addPlayer1 <- CARL14_SDg3[ which(CARL14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_SDg3[ which(CARL14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_SDg2 <- rbind(CARL14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
CARL14_SDft <- ftable(CARL14_SDg2$player1, CARL14_SDg2$player2)
CARL14_SDft2 <- as.matrix(CARL14_SDft)
numRows <- nrow(CARL14_SDft2)
numCols <- ncol(CARL14_SDft2)
CARL14_SDft3 <- CARL14_SDft2[c(2:numRows) , c(2:numCols)]
CARL14_SDTable <- graph.adjacency(CARL14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(CARL14_SDTable, vertex.label = V(CARL14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
CARL14_SD.clusterCoef <- transitivity(CARL14_SDTable, type="global") #cluster coefficient
CARL14_SD.degreeCent <- centralization.degree(CARL14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_SDftn <- as.network.matrix(CARL14_SDft)
CARL14_SD.netDensity <- network.density(CARL14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_SD.entropy <- entropy(CARL14_SDft) #entropy

CARL14_SD.netMx <- cbind(CARL14_SD.netMx, CARL14_SD.clusterCoef, CARL14_SD.degreeCent$centralization,
                         CARL14_SD.netDensity, CARL14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
CARL14_TDg2 <- data.frame(CARL14_TD)
CARL14_TDg2 <- CARL14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_TDg2$player1
player2vector <- CARL14_TDg2$player2
CARL14_TDg3 <- CARL14_TDg2
CARL14_TDg3$p1inp2vec <- is.element(CARL14_TDg3$player1, player2vector)
CARL14_TDg3$p2inp1vec <- is.element(CARL14_TDg3$player2, player1vector)

addPlayer1 <- CARL14_TDg3[ which(CARL14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_TDg3[ which(CARL14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_TDg2 <- rbind(CARL14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
CARL14_TDft <- ftable(CARL14_TDg2$player1, CARL14_TDg2$player2)
CARL14_TDft2 <- as.matrix(CARL14_TDft)
numRows <- nrow(CARL14_TDft2)
numCols <- ncol(CARL14_TDft2)
CARL14_TDft3 <- CARL14_TDft2[c(2:numRows) , c(2:numCols)]
CARL14_TDTable <- graph.adjacency(CARL14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(CARL14_TDTable, vertex.label = V(CARL14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
CARL14_TD.clusterCoef <- transitivity(CARL14_TDTable, type="global") #cluster coefficient
CARL14_TD.degreeCent <- centralization.degree(CARL14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_TDftn <- as.network.matrix(CARL14_TDft)
CARL14_TD.netDensity <- network.density(CARL14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_TD.entropy <- entropy(CARL14_TDft) #entropy

CARL14_TD.netMx <- cbind(CARL14_TD.netMx, CARL14_TD.clusterCoef, CARL14_TD.degreeCent$centralization,
                         CARL14_TD.netDensity, CARL14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
CARL14_QTg2 <- data.frame(CARL14_QT)
CARL14_QTg2 <- CARL14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL14_QTg2$player1
player2vector <- CARL14_QTg2$player2
CARL14_QTg3 <- CARL14_QTg2
CARL14_QTg3$p1inp2vec <- is.element(CARL14_QTg3$player1, player2vector)
CARL14_QTg3$p2inp1vec <- is.element(CARL14_QTg3$player2, player1vector)

addPlayer1 <- CARL14_QTg3[ which(CARL14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL14_QTg3[ which(CARL14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL14_QTg2 <- rbind(CARL14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
CARL14_QTft <- ftable(CARL14_QTg2$player1, CARL14_QTg2$player2)
CARL14_QTft2 <- as.matrix(CARL14_QTft)
numRows <- nrow(CARL14_QTft2)
numCols <- ncol(CARL14_QTft2)
CARL14_QTft3 <- CARL14_QTft2[c(2:numRows) , c(2:numCols)]
CARL14_QTTable <- graph.adjacency(CARL14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(CARL14_QTTable, vertex.label = V(CARL14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
CARL14_QT.clusterCoef <- transitivity(CARL14_QTTable, type="global") #cluster coefficient
CARL14_QT.degreeCent <- centralization.degree(CARL14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL14_QTftn <- as.network.matrix(CARL14_QTft)
CARL14_QT.netDensity <- network.density(CARL14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL14_QT.entropy <- entropy(CARL14_QTft) #entropy

CARL14_QT.netMx <- cbind(CARL14_QT.netMx, CARL14_QT.clusterCoef, CARL14_QT.degreeCent$centralization,
                         CARL14_QT.netDensity, CARL14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL14_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "Goal_F"
COLL14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
COLL14_Gg2 <- data.frame(COLL14_G)
COLL14_Gg2 <- COLL14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_Gg2$player1
player2vector <- COLL14_Gg2$player2
COLL14_Gg3 <- COLL14_Gg2
COLL14_Gg3$p1inp2vec <- is.element(COLL14_Gg3$player1, player2vector)
COLL14_Gg3$p2inp1vec <- is.element(COLL14_Gg3$player2, player1vector)

addPlayer1 <- COLL14_Gg3[ which(COLL14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_Gg3[ which(COLL14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_Gg2 <- rbind(COLL14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
COLL14_Gft <- ftable(COLL14_Gg2$player1, COLL14_Gg2$player2)
COLL14_Gft2 <- as.matrix(COLL14_Gft)
numRows <- nrow(COLL14_Gft2)
numCols <- ncol(COLL14_Gft2)
COLL14_Gft3 <- COLL14_Gft2[c(2:numRows) , c(2:numCols)]
COLL14_GTable <- graph.adjacency(COLL14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(COLL14_GTable, vertex.label = V(COLL14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
COLL14_G.clusterCoef <- transitivity(COLL14_GTable, type="global") #cluster coefficient
COLL14_G.degreeCent <- centralization.degree(COLL14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_Gftn <- as.network.matrix(COLL14_Gft)
COLL14_G.netDensity <- network.density(COLL14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_G.entropy <- entropy(COLL14_Gft) #entropy

COLL14_G.netMx <- cbind(COLL14_G.netMx, COLL14_G.clusterCoef, COLL14_G.degreeCent$centralization,
                        COLL14_G.netDensity, COLL14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "COLL"
KIoutcome = "Behind_F"
COLL14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
COLL14_Bg2 <- data.frame(COLL14_B)
COLL14_Bg2 <- COLL14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_Bg2$player1
player2vector <- COLL14_Bg2$player2
COLL14_Bg3 <- COLL14_Bg2
COLL14_Bg3$p1inp2vec <- is.element(COLL14_Bg3$player1, player2vector)
COLL14_Bg3$p2inp1vec <- is.element(COLL14_Bg3$player2, player1vector)

addPlayer1 <- COLL14_Bg3[ which(COLL14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_Bg3[ which(COLL14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_Bg2 <- rbind(COLL14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
COLL14_Bft <- ftable(COLL14_Bg2$player1, COLL14_Bg2$player2)
COLL14_Bft2 <- as.matrix(COLL14_Bft)
numRows <- nrow(COLL14_Bft2)
numCols <- ncol(COLL14_Bft2)
COLL14_Bft3 <- COLL14_Bft2[c(2:numRows) , c(2:numCols)]
COLL14_BTable <- graph.adjacency(COLL14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(COLL14_BTable, vertex.label = V(COLL14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
COLL14_B.clusterCoef <- transitivity(COLL14_BTable, type="global") #cluster coefficient
COLL14_B.degreeCent <- centralization.degree(COLL14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_Bftn <- as.network.matrix(COLL14_Bft)
COLL14_B.netDensity <- network.density(COLL14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_B.entropy <- entropy(COLL14_Bft) #entropy

COLL14_B.netMx <- cbind(COLL14_B.netMx, COLL14_B.clusterCoef, COLL14_B.degreeCent$centralization,
                        COLL14_B.netDensity, COLL14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
COLL14_SFg2 <- data.frame(COLL14_SF)
COLL14_SFg2 <- COLL14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_SFg2$player1
player2vector <- COLL14_SFg2$player2
COLL14_SFg3 <- COLL14_SFg2
COLL14_SFg3$p1inp2vec <- is.element(COLL14_SFg3$player1, player2vector)
COLL14_SFg3$p2inp1vec <- is.element(COLL14_SFg3$player2, player1vector)

addPlayer1 <- COLL14_SFg3[ which(COLL14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_SFg3[ which(COLL14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_SFg2 <- rbind(COLL14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
COLL14_SFft <- ftable(COLL14_SFg2$player1, COLL14_SFg2$player2)
COLL14_SFft2 <- as.matrix(COLL14_SFft)
numRows <- nrow(COLL14_SFft2)
numCols <- ncol(COLL14_SFft2)
COLL14_SFft3 <- COLL14_SFft2[c(2:numRows) , c(2:numCols)]
COLL14_SFTable <- graph.adjacency(COLL14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(COLL14_SFTable, vertex.label = V(COLL14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
COLL14_SF.clusterCoef <- transitivity(COLL14_SFTable, type="global") #cluster coefficient
COLL14_SF.degreeCent <- centralization.degree(COLL14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_SFftn <- as.network.matrix(COLL14_SFft)
COLL14_SF.netDensity <- network.density(COLL14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_SF.entropy <- entropy(COLL14_SFft) #entropy

COLL14_SF.netMx <- cbind(COLL14_SF.netMx, COLL14_SF.clusterCoef, COLL14_SF.degreeCent$centralization,
                         COLL14_SF.netDensity, COLL14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
COLL14_TFg2 <- data.frame(COLL14_TF)
COLL14_TFg2 <- COLL14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_TFg2$player1
player2vector <- COLL14_TFg2$player2
COLL14_TFg3 <- COLL14_TFg2
COLL14_TFg3$p1inp2vec <- is.element(COLL14_TFg3$player1, player2vector)
COLL14_TFg3$p2inp1vec <- is.element(COLL14_TFg3$player2, player1vector)

addPlayer1 <- COLL14_TFg3[ which(COLL14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_TFg3[ which(COLL14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_TFg2 <- rbind(COLL14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
COLL14_TFft <- ftable(COLL14_TFg2$player1, COLL14_TFg2$player2)
COLL14_TFft2 <- as.matrix(COLL14_TFft)
numRows <- nrow(COLL14_TFft2)
numCols <- ncol(COLL14_TFft2)
COLL14_TFft3 <- COLL14_TFft2[c(2:numRows) , c(2:numCols)]
COLL14_TFTable <- graph.adjacency(COLL14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(COLL14_TFTable, vertex.label = V(COLL14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
COLL14_TF.clusterCoef <- transitivity(COLL14_TFTable, type="global") #cluster coefficient
COLL14_TF.degreeCent <- centralization.degree(COLL14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_TFftn <- as.network.matrix(COLL14_TFft)
COLL14_TF.netDensity <- network.density(COLL14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_TF.entropy <- entropy(COLL14_TFft) #entropy

COLL14_TF.netMx <- cbind(COLL14_TF.netMx, COLL14_TF.clusterCoef, COLL14_TF.degreeCent$centralization,
                         COLL14_TF.netDensity, COLL14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
COLL14_SAMg2 <- data.frame(COLL14_SAM)
COLL14_SAMg2 <- COLL14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_SAMg2$player1
player2vector <- COLL14_SAMg2$player2
COLL14_SAMg3 <- COLL14_SAMg2
COLL14_SAMg3$p1inp2vec <- is.element(COLL14_SAMg3$player1, player2vector)
COLL14_SAMg3$p2inp1vec <- is.element(COLL14_SAMg3$player2, player1vector)

addPlayer1 <- COLL14_SAMg3[ which(COLL14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL14_SAMg3[ which(COLL14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_SAMg2 <- rbind(COLL14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
COLL14_SAMft <- ftable(COLL14_SAMg2$player1, COLL14_SAMg2$player2)
COLL14_SAMft2 <- as.matrix(COLL14_SAMft)
numRows <- nrow(COLL14_SAMft2)
numCols <- ncol(COLL14_SAMft2)
COLL14_SAMft3 <- COLL14_SAMft2[c(2:numRows) , c(2:numCols)]
COLL14_SAMTable <- graph.adjacency(COLL14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(COLL14_SAMTable, vertex.label = V(COLL14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
COLL14_SAM.clusterCoef <- transitivity(COLL14_SAMTable, type="global") #cluster coefficient
COLL14_SAM.degreeCent <- centralization.degree(COLL14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_SAMftn <- as.network.matrix(COLL14_SAMft)
COLL14_SAM.netDensity <- network.density(COLL14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_SAM.entropy <- entropy(COLL14_SAMft) #entropy

COLL14_SAM.netMx <- cbind(COLL14_SAM.netMx, COLL14_SAM.clusterCoef, COLL14_SAM.degreeCent$centralization,
                          COLL14_SAM.netDensity, COLL14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
COLL14_TAMg2 <- data.frame(COLL14_TAM)
COLL14_TAMg2 <- COLL14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_TAMg2$player1
player2vector <- COLL14_TAMg2$player2
COLL14_TAMg3 <- COLL14_TAMg2
COLL14_TAMg3$p1inp2vec <- is.element(COLL14_TAMg3$player1, player2vector)
COLL14_TAMg3$p2inp1vec <- is.element(COLL14_TAMg3$player2, player1vector)

addPlayer1 <- COLL14_TAMg3[ which(COLL14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_TAMg3[ which(COLL14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_TAMg2 <- rbind(COLL14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
COLL14_TAMft <- ftable(COLL14_TAMg2$player1, COLL14_TAMg2$player2)
COLL14_TAMft2 <- as.matrix(COLL14_TAMft)
numRows <- nrow(COLL14_TAMft2)
numCols <- ncol(COLL14_TAMft2)
COLL14_TAMft3 <- COLL14_TAMft2[c(2:numRows) , c(2:numCols)]
COLL14_TAMTable <- graph.adjacency(COLL14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(COLL14_TAMTable, vertex.label = V(COLL14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
COLL14_TAM.clusterCoef <- transitivity(COLL14_TAMTable, type="global") #cluster coefficient
COLL14_TAM.degreeCent <- centralization.degree(COLL14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_TAMftn <- as.network.matrix(COLL14_TAMft)
COLL14_TAM.netDensity <- network.density(COLL14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_TAM.entropy <- entropy(COLL14_TAMft) #entropy

COLL14_TAM.netMx <- cbind(COLL14_TAM.netMx, COLL14_TAM.clusterCoef, COLL14_TAM.degreeCent$centralization,
                          COLL14_TAM.netDensity, COLL14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
COLL14_SDMg2 <- data.frame(COLL14_SDM)
COLL14_SDMg2 <- COLL14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_SDMg2$player1
player2vector <- COLL14_SDMg2$player2
COLL14_SDMg3 <- COLL14_SDMg2
COLL14_SDMg3$p1inp2vec <- is.element(COLL14_SDMg3$player1, player2vector)
COLL14_SDMg3$p2inp1vec <- is.element(COLL14_SDMg3$player2, player1vector)

addPlayer1 <- COLL14_SDMg3[ which(COLL14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_SDMg3[ which(COLL14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_SDMg2 <- rbind(COLL14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
COLL14_SDMft <- ftable(COLL14_SDMg2$player1, COLL14_SDMg2$player2)
COLL14_SDMft2 <- as.matrix(COLL14_SDMft)
numRows <- nrow(COLL14_SDMft2)
numCols <- ncol(COLL14_SDMft2)
COLL14_SDMft3 <- COLL14_SDMft2[c(2:numRows) , c(2:numCols)]
COLL14_SDMTable <- graph.adjacency(COLL14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(COLL14_SDMTable, vertex.label = V(COLL14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
COLL14_SDM.clusterCoef <- transitivity(COLL14_SDMTable, type="global") #cluster coefficient
COLL14_SDM.degreeCent <- centralization.degree(COLL14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_SDMftn <- as.network.matrix(COLL14_SDMft)
COLL14_SDM.netDensity <- network.density(COLL14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_SDM.entropy <- entropy(COLL14_SDMft) #entropy

COLL14_SDM.netMx <- cbind(COLL14_SDM.netMx, COLL14_SDM.clusterCoef, COLL14_SDM.degreeCent$centralization,
                          COLL14_SDM.netDensity, COLL14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
COLL14_TDMg2 <- data.frame(COLL14_TDM)
COLL14_TDMg2 <- COLL14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_TDMg2$player1
player2vector <- COLL14_TDMg2$player2
COLL14_TDMg3 <- COLL14_TDMg2
COLL14_TDMg3$p1inp2vec <- is.element(COLL14_TDMg3$player1, player2vector)
COLL14_TDMg3$p2inp1vec <- is.element(COLL14_TDMg3$player2, player1vector)

addPlayer1 <- COLL14_TDMg3[ which(COLL14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_TDMg3[ which(COLL14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_TDMg2 <- rbind(COLL14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
COLL14_TDMft <- ftable(COLL14_TDMg2$player1, COLL14_TDMg2$player2)
COLL14_TDMft2 <- as.matrix(COLL14_TDMft)
numRows <- nrow(COLL14_TDMft2)
numCols <- ncol(COLL14_TDMft2)
COLL14_TDMft3 <- COLL14_TDMft2[c(2:numRows) , c(2:numCols)]
COLL14_TDMTable <- graph.adjacency(COLL14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(COLL14_TDMTable, vertex.label = V(COLL14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
COLL14_TDM.clusterCoef <- transitivity(COLL14_TDMTable, type="global") #cluster coefficient
COLL14_TDM.degreeCent <- centralization.degree(COLL14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_TDMftn <- as.network.matrix(COLL14_TDMft)
COLL14_TDM.netDensity <- network.density(COLL14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_TDM.entropy <- entropy(COLL14_TDMft) #entropy

COLL14_TDM.netMx <- cbind(COLL14_TDM.netMx, COLL14_TDM.clusterCoef, COLL14_TDM.degreeCent$centralization,
                          COLL14_TDM.netDensity, COLL14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
COLL14_SDg2 <- data.frame(COLL14_SD)
COLL14_SDg2 <- COLL14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_SDg2$player1
player2vector <- COLL14_SDg2$player2
COLL14_SDg3 <- COLL14_SDg2
COLL14_SDg3$p1inp2vec <- is.element(COLL14_SDg3$player1, player2vector)
COLL14_SDg3$p2inp1vec <- is.element(COLL14_SDg3$player2, player1vector)

addPlayer1 <- COLL14_SDg3[ which(COLL14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_SDg3[ which(COLL14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_SDg2 <- rbind(COLL14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
COLL14_SDft <- ftable(COLL14_SDg2$player1, COLL14_SDg2$player2)
COLL14_SDft2 <- as.matrix(COLL14_SDft)
numRows <- nrow(COLL14_SDft2)
numCols <- ncol(COLL14_SDft2)
COLL14_SDft3 <- COLL14_SDft2[c(2:numRows) , c(2:numCols)]
COLL14_SDTable <- graph.adjacency(COLL14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(COLL14_SDTable, vertex.label = V(COLL14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
COLL14_SD.clusterCoef <- transitivity(COLL14_SDTable, type="global") #cluster coefficient
COLL14_SD.degreeCent <- centralization.degree(COLL14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_SDftn <- as.network.matrix(COLL14_SDft)
COLL14_SD.netDensity <- network.density(COLL14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_SD.entropy <- entropy(COLL14_SDft) #entropy

COLL14_SD.netMx <- cbind(COLL14_SD.netMx, COLL14_SD.clusterCoef, COLL14_SD.degreeCent$centralization,
                         COLL14_SD.netDensity, COLL14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
COLL14_TDg2 <- data.frame(COLL14_TD)
COLL14_TDg2 <- COLL14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_TDg2$player1
player2vector <- COLL14_TDg2$player2
COLL14_TDg3 <- COLL14_TDg2
COLL14_TDg3$p1inp2vec <- is.element(COLL14_TDg3$player1, player2vector)
COLL14_TDg3$p2inp1vec <- is.element(COLL14_TDg3$player2, player1vector)

addPlayer1 <- COLL14_TDg3[ which(COLL14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_TDg3[ which(COLL14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_TDg2 <- rbind(COLL14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
COLL14_TDft <- ftable(COLL14_TDg2$player1, COLL14_TDg2$player2)
COLL14_TDft2 <- as.matrix(COLL14_TDft)
numRows <- nrow(COLL14_TDft2)
numCols <- ncol(COLL14_TDft2)
COLL14_TDft3 <- COLL14_TDft2[c(2:numRows) , c(2:numCols)]
COLL14_TDTable <- graph.adjacency(COLL14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(COLL14_TDTable, vertex.label = V(COLL14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
COLL14_TD.clusterCoef <- transitivity(COLL14_TDTable, type="global") #cluster coefficient
COLL14_TD.degreeCent <- centralization.degree(COLL14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_TDftn <- as.network.matrix(COLL14_TDft)
COLL14_TD.netDensity <- network.density(COLL14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_TD.entropy <- entropy(COLL14_TDft) #entropy

COLL14_TD.netMx <- cbind(COLL14_TD.netMx, COLL14_TD.clusterCoef, COLL14_TD.degreeCent$centralization,
                         COLL14_TD.netDensity, COLL14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
COLL14_QTg2 <- data.frame(COLL14_QT)
COLL14_QTg2 <- COLL14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL14_QTg2$player1
player2vector <- COLL14_QTg2$player2
COLL14_QTg3 <- COLL14_QTg2
COLL14_QTg3$p1inp2vec <- is.element(COLL14_QTg3$player1, player2vector)
COLL14_QTg3$p2inp1vec <- is.element(COLL14_QTg3$player2, player1vector)

addPlayer1 <- COLL14_QTg3[ which(COLL14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL14_QTg3[ which(COLL14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL14_QTg2 <- rbind(COLL14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
COLL14_QTft <- ftable(COLL14_QTg2$player1, COLL14_QTg2$player2)
COLL14_QTft2 <- as.matrix(COLL14_QTft)
numRows <- nrow(COLL14_QTft2)
numCols <- ncol(COLL14_QTft2)
COLL14_QTft3 <- COLL14_QTft2[c(2:numRows) , c(2:numCols)]
COLL14_QTTable <- graph.adjacency(COLL14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(COLL14_QTTable, vertex.label = V(COLL14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
COLL14_QT.clusterCoef <- transitivity(COLL14_QTTable, type="global") #cluster coefficient
COLL14_QT.degreeCent <- centralization.degree(COLL14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL14_QTftn <- as.network.matrix(COLL14_QTft)
COLL14_QT.netDensity <- network.density(COLL14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL14_QT.entropy <- entropy(COLL14_QTft) #entropy

COLL14_QT.netMx <- cbind(COLL14_QT.netMx, COLL14_QT.clusterCoef, COLL14_QT.degreeCent$centralization,
                         COLL14_QT.netDensity, COLL14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL14_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "ESS"
KIoutcome = "Goal_F"
ESS14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
ESS14_Gg2 <- data.frame(ESS14_G)
ESS14_Gg2 <- ESS14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_Gg2$player1
player2vector <- ESS14_Gg2$player2
ESS14_Gg3 <- ESS14_Gg2
ESS14_Gg3$p1inp2vec <- is.element(ESS14_Gg3$player1, player2vector)
ESS14_Gg3$p2inp1vec <- is.element(ESS14_Gg3$player2, player1vector)

addPlayer1 <- ESS14_Gg3[ which(ESS14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_Gg3[ which(ESS14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_Gg2 <- rbind(ESS14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
ESS14_Gft <- ftable(ESS14_Gg2$player1, ESS14_Gg2$player2)
ESS14_Gft2 <- as.matrix(ESS14_Gft)
numRows <- nrow(ESS14_Gft2)
numCols <- ncol(ESS14_Gft2)
ESS14_Gft3 <- ESS14_Gft2[c(2:numRows) , c(2:numCols)]
ESS14_GTable <- graph.adjacency(ESS14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(ESS14_GTable, vertex.label = V(ESS14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
ESS14_G.clusterCoef <- transitivity(ESS14_GTable, type="global") #cluster coefficient
ESS14_G.degreeCent <- centralization.degree(ESS14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_Gftn <- as.network.matrix(ESS14_Gft)
ESS14_G.netDensity <- network.density(ESS14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_G.entropy <- entropy(ESS14_Gft) #entropy

ESS14_G.netMx <- cbind(ESS14_G.netMx, ESS14_G.clusterCoef, ESS14_G.degreeCent$centralization,
                       ESS14_G.netDensity, ESS14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "ESS"
KIoutcome = "Behind_F"
ESS14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
ESS14_Bg2 <- data.frame(ESS14_B)
ESS14_Bg2 <- ESS14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_Bg2$player1
player2vector <- ESS14_Bg2$player2
ESS14_Bg3 <- ESS14_Bg2
ESS14_Bg3$p1inp2vec <- is.element(ESS14_Bg3$player1, player2vector)
ESS14_Bg3$p2inp1vec <- is.element(ESS14_Bg3$player2, player1vector)

addPlayer1 <- ESS14_Bg3[ which(ESS14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_Bg3[ which(ESS14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_Bg2 <- rbind(ESS14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
ESS14_Bft <- ftable(ESS14_Bg2$player1, ESS14_Bg2$player2)
ESS14_Bft2 <- as.matrix(ESS14_Bft)
numRows <- nrow(ESS14_Bft2)
numCols <- ncol(ESS14_Bft2)
ESS14_Bft3 <- ESS14_Bft2[c(2:numRows) , c(2:numCols)]
ESS14_BTable <- graph.adjacency(ESS14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(ESS14_BTable, vertex.label = V(ESS14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
ESS14_B.clusterCoef <- transitivity(ESS14_BTable, type="global") #cluster coefficient
ESS14_B.degreeCent <- centralization.degree(ESS14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_Bftn <- as.network.matrix(ESS14_Bft)
ESS14_B.netDensity <- network.density(ESS14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_B.entropy <- entropy(ESS14_Bft) #entropy

ESS14_B.netMx <- cbind(ESS14_B.netMx, ESS14_B.clusterCoef, ESS14_B.degreeCent$centralization,
                       ESS14_B.netDensity, ESS14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************

round = 14
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
ESS14_SFg2 <- data.frame(ESS14_SF)
ESS14_SFg2 <- ESS14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_SFg2$player1
player2vector <- ESS14_SFg2$player2
ESS14_SFg3 <- ESS14_SFg2
ESS14_SFg3$p1inp2vec <- is.element(ESS14_SFg3$player1, player2vector)
ESS14_SFg3$p2inp1vec <- is.element(ESS14_SFg3$player2, player1vector)

addPlayer1 <- ESS14_SFg3[ which(ESS14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_SFg3[ which(ESS14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_SFg2 <- rbind(ESS14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
ESS14_SFft <- ftable(ESS14_SFg2$player1, ESS14_SFg2$player2)
ESS14_SFft2 <- as.matrix(ESS14_SFft)
numRows <- nrow(ESS14_SFft2)
numCols <- ncol(ESS14_SFft2)
ESS14_SFft3 <- ESS14_SFft2[c(2:numRows) , c(2:numCols)]
ESS14_SFTable <- graph.adjacency(ESS14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(ESS14_SFTable, vertex.label = V(ESS14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
ESS14_SF.clusterCoef <- transitivity(ESS14_SFTable, type="global") #cluster coefficient
ESS14_SF.degreeCent <- centralization.degree(ESS14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_SFftn <- as.network.matrix(ESS14_SFft)
ESS14_SF.netDensity <- network.density(ESS14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_SF.entropy <- entropy(ESS14_SFft) #entropy

ESS14_SF.netMx <- cbind(ESS14_SF.netMx, ESS14_SF.clusterCoef, ESS14_SF.degreeCent$centralization,
                        ESS14_SF.netDensity, ESS14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
ESS14_TFg2 <- data.frame(ESS14_TF)
ESS14_TFg2 <- ESS14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_TFg2$player1
player2vector <- ESS14_TFg2$player2
ESS14_TFg3 <- ESS14_TFg2
ESS14_TFg3$p1inp2vec <- is.element(ESS14_TFg3$player1, player2vector)
ESS14_TFg3$p2inp1vec <- is.element(ESS14_TFg3$player2, player1vector)

addPlayer1 <- ESS14_TFg3[ which(ESS14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_TFg3[ which(ESS14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_TFg2 <- rbind(ESS14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
ESS14_TFft <- ftable(ESS14_TFg2$player1, ESS14_TFg2$player2)
ESS14_TFft2 <- as.matrix(ESS14_TFft)
numRows <- nrow(ESS14_TFft2)
numCols <- ncol(ESS14_TFft2)
ESS14_TFft3 <- ESS14_TFft2[c(2:numRows) , c(2:numCols)]
ESS14_TFTable <- graph.adjacency(ESS14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(ESS14_TFTable, vertex.label = V(ESS14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
ESS14_TF.clusterCoef <- transitivity(ESS14_TFTable, type="global") #cluster coefficient
ESS14_TF.degreeCent <- centralization.degree(ESS14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_TFftn <- as.network.matrix(ESS14_TFft)
ESS14_TF.netDensity <- network.density(ESS14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_TF.entropy <- entropy(ESS14_TFft) #entropy

ESS14_TF.netMx <- cbind(ESS14_TF.netMx, ESS14_TF.clusterCoef, ESS14_TF.degreeCent$centralization,
                        ESS14_TF.netDensity, ESS14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
ESS14_SAMg2 <- data.frame(ESS14_SAM)
ESS14_SAMg2 <- ESS14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_SAMg2$player1
player2vector <- ESS14_SAMg2$player2
ESS14_SAMg3 <- ESS14_SAMg2
ESS14_SAMg3$p1inp2vec <- is.element(ESS14_SAMg3$player1, player2vector)
ESS14_SAMg3$p2inp1vec <- is.element(ESS14_SAMg3$player2, player1vector)

addPlayer1 <- ESS14_SAMg3[ which(ESS14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_SAMg3[ which(ESS14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_SAMg2 <- rbind(ESS14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
ESS14_SAMft <- ftable(ESS14_SAMg2$player1, ESS14_SAMg2$player2)
ESS14_SAMft2 <- as.matrix(ESS14_SAMft)
numRows <- nrow(ESS14_SAMft2)
numCols <- ncol(ESS14_SAMft2)
ESS14_SAMft3 <- ESS14_SAMft2[c(2:numRows) , c(2:numCols)]
ESS14_SAMTable <- graph.adjacency(ESS14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(ESS14_SAMTable, vertex.label = V(ESS14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
ESS14_SAM.clusterCoef <- transitivity(ESS14_SAMTable, type="global") #cluster coefficient
ESS14_SAM.degreeCent <- centralization.degree(ESS14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_SAMftn <- as.network.matrix(ESS14_SAMft)
ESS14_SAM.netDensity <- network.density(ESS14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_SAM.entropy <- entropy(ESS14_SAMft) #entropy

ESS14_SAM.netMx <- cbind(ESS14_SAM.netMx, ESS14_SAM.clusterCoef, ESS14_SAM.degreeCent$centralization,
                         ESS14_SAM.netDensity, ESS14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
ESS14_TAMg2 <- data.frame(ESS14_TAM)
ESS14_TAMg2 <- ESS14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_TAMg2$player1
player2vector <- ESS14_TAMg2$player2
ESS14_TAMg3 <- ESS14_TAMg2
ESS14_TAMg3$p1inp2vec <- is.element(ESS14_TAMg3$player1, player2vector)
ESS14_TAMg3$p2inp1vec <- is.element(ESS14_TAMg3$player2, player1vector)

addPlayer1 <- ESS14_TAMg3[ which(ESS14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_TAMg3[ which(ESS14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_TAMg2 <- rbind(ESS14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
ESS14_TAMft <- ftable(ESS14_TAMg2$player1, ESS14_TAMg2$player2)
ESS14_TAMft2 <- as.matrix(ESS14_TAMft)
numRows <- nrow(ESS14_TAMft2)
numCols <- ncol(ESS14_TAMft2)
ESS14_TAMft3 <- ESS14_TAMft2[c(2:numRows) , c(2:numCols)]
ESS14_TAMTable <- graph.adjacency(ESS14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(ESS14_TAMTable, vertex.label = V(ESS14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
ESS14_TAM.clusterCoef <- transitivity(ESS14_TAMTable, type="global") #cluster coefficient
ESS14_TAM.degreeCent <- centralization.degree(ESS14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_TAMftn <- as.network.matrix(ESS14_TAMft)
ESS14_TAM.netDensity <- network.density(ESS14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_TAM.entropy <- entropy(ESS14_TAMft) #entropy

ESS14_TAM.netMx <- cbind(ESS14_TAM.netMx, ESS14_TAM.clusterCoef, ESS14_TAM.degreeCent$centralization,
                         ESS14_TAM.netDensity, ESS14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
ESS14_SDMg2 <- data.frame(ESS14_SDM)
ESS14_SDMg2 <- ESS14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_SDMg2$player1
player2vector <- ESS14_SDMg2$player2
ESS14_SDMg3 <- ESS14_SDMg2
ESS14_SDMg3$p1inp2vec <- is.element(ESS14_SDMg3$player1, player2vector)
ESS14_SDMg3$p2inp1vec <- is.element(ESS14_SDMg3$player2, player1vector)

addPlayer1 <- ESS14_SDMg3[ which(ESS14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_SDMg3[ which(ESS14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_SDMg2 <- rbind(ESS14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
ESS14_SDMft <- ftable(ESS14_SDMg2$player1, ESS14_SDMg2$player2)
ESS14_SDMft2 <- as.matrix(ESS14_SDMft)
numRows <- nrow(ESS14_SDMft2)
numCols <- ncol(ESS14_SDMft2)
ESS14_SDMft3 <- ESS14_SDMft2[c(2:numRows) , c(2:numCols)]
ESS14_SDMTable <- graph.adjacency(ESS14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(ESS14_SDMTable, vertex.label = V(ESS14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
ESS14_SDM.clusterCoef <- transitivity(ESS14_SDMTable, type="global") #cluster coefficient
ESS14_SDM.degreeCent <- centralization.degree(ESS14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_SDMftn <- as.network.matrix(ESS14_SDMft)
ESS14_SDM.netDensity <- network.density(ESS14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_SDM.entropy <- entropy(ESS14_SDMft) #entropy

ESS14_SDM.netMx <- cbind(ESS14_SDM.netMx, ESS14_SDM.clusterCoef, ESS14_SDM.degreeCent$centralization,
                         ESS14_SDM.netDensity, ESS14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
ESS14_TDMg2 <- data.frame(ESS14_TDM)
ESS14_TDMg2 <- ESS14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_TDMg2$player1
player2vector <- ESS14_TDMg2$player2
ESS14_TDMg3 <- ESS14_TDMg2
ESS14_TDMg3$p1inp2vec <- is.element(ESS14_TDMg3$player1, player2vector)
ESS14_TDMg3$p2inp1vec <- is.element(ESS14_TDMg3$player2, player1vector)

addPlayer1 <- ESS14_TDMg3[ which(ESS14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_TDMg3[ which(ESS14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_TDMg2 <- rbind(ESS14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
ESS14_TDMft <- ftable(ESS14_TDMg2$player1, ESS14_TDMg2$player2)
ESS14_TDMft2 <- as.matrix(ESS14_TDMft)
numRows <- nrow(ESS14_TDMft2)
numCols <- ncol(ESS14_TDMft2)
ESS14_TDMft3 <- ESS14_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS14_TDMTable <- graph.adjacency(ESS14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(ESS14_TDMTable, vertex.label = V(ESS14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
ESS14_TDM.clusterCoef <- transitivity(ESS14_TDMTable, type="global") #cluster coefficient
ESS14_TDM.degreeCent <- centralization.degree(ESS14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_TDMftn <- as.network.matrix(ESS14_TDMft)
ESS14_TDM.netDensity <- network.density(ESS14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_TDM.entropy <- entropy(ESS14_TDMft) #entropy

ESS14_TDM.netMx <- cbind(ESS14_TDM.netMx, ESS14_TDM.clusterCoef, ESS14_TDM.degreeCent$centralization,
                         ESS14_TDM.netDensity, ESS14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************

round = 14
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
ESS14_SDg2 <- data.frame(ESS14_SD)
ESS14_SDg2 <- ESS14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_SDg2$player1
player2vector <- ESS14_SDg2$player2
ESS14_SDg3 <- ESS14_SDg2
ESS14_SDg3$p1inp2vec <- is.element(ESS14_SDg3$player1, player2vector)
ESS14_SDg3$p2inp1vec <- is.element(ESS14_SDg3$player2, player1vector)

addPlayer1 <- ESS14_SDg3[ which(ESS14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_SDg3[ which(ESS14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_SDg2 <- rbind(ESS14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
ESS14_SDft <- ftable(ESS14_SDg2$player1, ESS14_SDg2$player2)
ESS14_SDft2 <- as.matrix(ESS14_SDft)
numRows <- nrow(ESS14_SDft2)
numCols <- ncol(ESS14_SDft2)
ESS14_SDft3 <- ESS14_SDft2[c(2:numRows) , c(2:numCols)]
ESS14_SDTable <- graph.adjacency(ESS14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(ESS14_SDTable, vertex.label = V(ESS14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
ESS14_SD.clusterCoef <- transitivity(ESS14_SDTable, type="global") #cluster coefficient
ESS14_SD.degreeCent <- centralization.degree(ESS14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_SDftn <- as.network.matrix(ESS14_SDft)
ESS14_SD.netDensity <- network.density(ESS14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_SD.entropy <- entropy(ESS14_SDft) #entropy

ESS14_SD.netMx <- cbind(ESS14_SD.netMx, ESS14_SD.clusterCoef, ESS14_SD.degreeCent$centralization,
                        ESS14_SD.netDensity, ESS14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
ESS14_TDg2 <- data.frame(ESS14_TD)
ESS14_TDg2 <- ESS14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_TDg2$player1
player2vector <- ESS14_TDg2$player2
ESS14_TDg3 <- ESS14_TDg2
ESS14_TDg3$p1inp2vec <- is.element(ESS14_TDg3$player1, player2vector)
ESS14_TDg3$p2inp1vec <- is.element(ESS14_TDg3$player2, player1vector)

addPlayer1 <- ESS14_TDg3[ which(ESS14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_TDg3[ which(ESS14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_TDg2 <- rbind(ESS14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
ESS14_TDft <- ftable(ESS14_TDg2$player1, ESS14_TDg2$player2)
ESS14_TDft2 <- as.matrix(ESS14_TDft)
numRows <- nrow(ESS14_TDft2)
numCols <- ncol(ESS14_TDft2)
ESS14_TDft3 <- ESS14_TDft2[c(2:numRows) , c(2:numCols)]
ESS14_TDTable <- graph.adjacency(ESS14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(ESS14_TDTable, vertex.label = V(ESS14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
ESS14_TD.clusterCoef <- transitivity(ESS14_TDTable, type="global") #cluster coefficient
ESS14_TD.degreeCent <- centralization.degree(ESS14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_TDftn <- as.network.matrix(ESS14_TDft)
ESS14_TD.netDensity <- network.density(ESS14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_TD.entropy <- entropy(ESS14_TDft) #entropy

ESS14_TD.netMx <- cbind(ESS14_TD.netMx, ESS14_TD.clusterCoef, ESS14_TD.degreeCent$centralization,
                        ESS14_TD.netDensity, ESS14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
ESS14_QTg2 <- data.frame(ESS14_QT)
ESS14_QTg2 <- ESS14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS14_QTg2$player1
player2vector <- ESS14_QTg2$player2
ESS14_QTg3 <- ESS14_QTg2
ESS14_QTg3$p1inp2vec <- is.element(ESS14_QTg3$player1, player2vector)
ESS14_QTg3$p2inp1vec <- is.element(ESS14_QTg3$player2, player1vector)

addPlayer1 <- ESS14_QTg3[ which(ESS14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS14_QTg3[ which(ESS14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS14_QTg2 <- rbind(ESS14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
ESS14_QTft <- ftable(ESS14_QTg2$player1, ESS14_QTg2$player2)
ESS14_QTft2 <- as.matrix(ESS14_QTft)
numRows <- nrow(ESS14_QTft2)
numCols <- ncol(ESS14_QTft2)
ESS14_QTft3 <- ESS14_QTft2[c(2:numRows) , c(2:numCols)]
ESS14_QTTable <- graph.adjacency(ESS14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(ESS14_QTTable, vertex.label = V(ESS14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
ESS14_QT.clusterCoef <- transitivity(ESS14_QTTable, type="global") #cluster coefficient
ESS14_QT.degreeCent <- centralization.degree(ESS14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS14_QTftn <- as.network.matrix(ESS14_QTft)
ESS14_QT.netDensity <- network.density(ESS14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS14_QT.entropy <- entropy(ESS14_QTft) #entropy

ESS14_QT.netMx <- cbind(ESS14_QT.netMx, ESS14_QT.clusterCoef, ESS14_QT.degreeCent$centralization,
                        ESS14_QT.netDensity, ESS14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS14_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "FRE"
KIoutcome = "Goal_F"
FRE14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
FRE14_Gg2 <- data.frame(FRE14_G)
FRE14_Gg2 <- FRE14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_Gg2$player1
player2vector <- FRE14_Gg2$player2
FRE14_Gg3 <- FRE14_Gg2
FRE14_Gg3$p1inp2vec <- is.element(FRE14_Gg3$player1, player2vector)
FRE14_Gg3$p2inp1vec <- is.element(FRE14_Gg3$player2, player1vector)

addPlayer1 <- FRE14_Gg3[ which(FRE14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_Gg3[ which(FRE14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_Gg2 <- rbind(FRE14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
FRE14_Gft <- ftable(FRE14_Gg2$player1, FRE14_Gg2$player2)
FRE14_Gft2 <- as.matrix(FRE14_Gft)
numRows <- nrow(FRE14_Gft2)
numCols <- ncol(FRE14_Gft2)
FRE14_Gft3 <- FRE14_Gft2[c(2:numRows) , c(2:numCols)]
FRE14_GTable <- graph.adjacency(FRE14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(FRE14_GTable, vertex.label = V(FRE14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
FRE14_G.clusterCoef <- transitivity(FRE14_GTable, type="global") #cluster coefficient
FRE14_G.degreeCent <- centralization.degree(FRE14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_Gftn <- as.network.matrix(FRE14_Gft)
FRE14_G.netDensity <- network.density(FRE14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_G.entropy <- entropy(FRE14_Gft) #entropy

FRE14_G.netMx <- cbind(FRE14_G.netMx, FRE14_G.clusterCoef, FRE14_G.degreeCent$centralization,
                       FRE14_G.netDensity, FRE14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "FRE"
KIoutcome = "Behind_F"
FRE14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
FRE14_Bg2 <- data.frame(FRE14_B)
FRE14_Bg2 <- FRE14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_Bg2$player1
player2vector <- FRE14_Bg2$player2
FRE14_Bg3 <- FRE14_Bg2
FRE14_Bg3$p1inp2vec <- is.element(FRE14_Bg3$player1, player2vector)
FRE14_Bg3$p2inp1vec <- is.element(FRE14_Bg3$player2, player1vector)

addPlayer1 <- FRE14_Bg3[ which(FRE14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_Bg3[ which(FRE14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_Bg2 <- rbind(FRE14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
FRE14_Bft <- ftable(FRE14_Bg2$player1, FRE14_Bg2$player2)
FRE14_Bft2 <- as.matrix(FRE14_Bft)
numRows <- nrow(FRE14_Bft2)
numCols <- ncol(FRE14_Bft2)
FRE14_Bft3 <- FRE14_Bft2[c(2:numRows) , c(2:numCols)]
FRE14_BTable <- graph.adjacency(FRE14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(FRE14_BTable, vertex.label = V(FRE14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
FRE14_B.clusterCoef <- transitivity(FRE14_BTable, type="global") #cluster coefficient
FRE14_B.degreeCent <- centralization.degree(FRE14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_Bftn <- as.network.matrix(FRE14_Bft)
FRE14_B.netDensity <- network.density(FRE14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_B.entropy <- entropy(FRE14_Bft) #entropy

FRE14_B.netMx <- cbind(FRE14_B.netMx, FRE14_B.clusterCoef, FRE14_B.degreeCent$centralization,
                       FRE14_B.netDensity, FRE14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
FRE14_SFg2 <- data.frame(FRE14_SF)
FRE14_SFg2 <- FRE14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_SFg2$player1
player2vector <- FRE14_SFg2$player2
FRE14_SFg3 <- FRE14_SFg2
FRE14_SFg3$p1inp2vec <- is.element(FRE14_SFg3$player1, player2vector)
FRE14_SFg3$p2inp1vec <- is.element(FRE14_SFg3$player2, player1vector)

addPlayer1 <- FRE14_SFg3[ which(FRE14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_SFg3[ which(FRE14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_SFg2 <- rbind(FRE14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
FRE14_SFft <- ftable(FRE14_SFg2$player1, FRE14_SFg2$player2)
FRE14_SFft2 <- as.matrix(FRE14_SFft)
numRows <- nrow(FRE14_SFft2)
numCols <- ncol(FRE14_SFft2)
FRE14_SFft3 <- FRE14_SFft2[c(2:numRows) , c(2:numCols)]
FRE14_SFTable <- graph.adjacency(FRE14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(FRE14_SFTable, vertex.label = V(FRE14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
FRE14_SF.clusterCoef <- transitivity(FRE14_SFTable, type="global") #cluster coefficient
FRE14_SF.degreeCent <- centralization.degree(FRE14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_SFftn <- as.network.matrix(FRE14_SFft)
FRE14_SF.netDensity <- network.density(FRE14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_SF.entropy <- entropy(FRE14_SFft) #entropy

FRE14_SF.netMx <- cbind(FRE14_SF.netMx, FRE14_SF.clusterCoef, FRE14_SF.degreeCent$centralization,
                        FRE14_SF.netDensity, FRE14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
FRE14_TFg2 <- data.frame(FRE14_TF)
FRE14_TFg2 <- FRE14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_TFg2$player1
player2vector <- FRE14_TFg2$player2
FRE14_TFg3 <- FRE14_TFg2
FRE14_TFg3$p1inp2vec <- is.element(FRE14_TFg3$player1, player2vector)
FRE14_TFg3$p2inp1vec <- is.element(FRE14_TFg3$player2, player1vector)

addPlayer1 <- FRE14_TFg3[ which(FRE14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_TFg3[ which(FRE14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_TFg2 <- rbind(FRE14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
FRE14_TFft <- ftable(FRE14_TFg2$player1, FRE14_TFg2$player2)
FRE14_TFft2 <- as.matrix(FRE14_TFft)
numRows <- nrow(FRE14_TFft2)
numCols <- ncol(FRE14_TFft2)
FRE14_TFft3 <- FRE14_TFft2[c(2:numRows) , c(2:numCols)]
FRE14_TFTable <- graph.adjacency(FRE14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(FRE14_TFTable, vertex.label = V(FRE14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
FRE14_TF.clusterCoef <- transitivity(FRE14_TFTable, type="global") #cluster coefficient
FRE14_TF.degreeCent <- centralization.degree(FRE14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_TFftn <- as.network.matrix(FRE14_TFft)
FRE14_TF.netDensity <- network.density(FRE14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_TF.entropy <- entropy(FRE14_TFft) #entropy

FRE14_TF.netMx <- cbind(FRE14_TF.netMx, FRE14_TF.clusterCoef, FRE14_TF.degreeCent$centralization,
                        FRE14_TF.netDensity, FRE14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
FRE14_SAMg2 <- data.frame(FRE14_SAM)
FRE14_SAMg2 <- FRE14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_SAMg2$player1
player2vector <- FRE14_SAMg2$player2
FRE14_SAMg3 <- FRE14_SAMg2
FRE14_SAMg3$p1inp2vec <- is.element(FRE14_SAMg3$player1, player2vector)
FRE14_SAMg3$p2inp1vec <- is.element(FRE14_SAMg3$player2, player1vector)

addPlayer1 <- FRE14_SAMg3[ which(FRE14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_SAMg3[ which(FRE14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_SAMg2 <- rbind(FRE14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
FRE14_SAMft <- ftable(FRE14_SAMg2$player1, FRE14_SAMg2$player2)
FRE14_SAMft2 <- as.matrix(FRE14_SAMft)
numRows <- nrow(FRE14_SAMft2)
numCols <- ncol(FRE14_SAMft2)
FRE14_SAMft3 <- FRE14_SAMft2[c(2:numRows) , c(2:numCols)]
FRE14_SAMTable <- graph.adjacency(FRE14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(FRE14_SAMTable, vertex.label = V(FRE14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
FRE14_SAM.clusterCoef <- transitivity(FRE14_SAMTable, type="global") #cluster coefficient
FRE14_SAM.degreeCent <- centralization.degree(FRE14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_SAMftn <- as.network.matrix(FRE14_SAMft)
FRE14_SAM.netDensity <- network.density(FRE14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_SAM.entropy <- entropy(FRE14_SAMft) #entropy

FRE14_SAM.netMx <- cbind(FRE14_SAM.netMx, FRE14_SAM.clusterCoef, FRE14_SAM.degreeCent$centralization,
                         FRE14_SAM.netDensity, FRE14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
FRE14_TAMg2 <- data.frame(FRE14_TAM)
FRE14_TAMg2 <- FRE14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_TAMg2$player1
player2vector <- FRE14_TAMg2$player2
FRE14_TAMg3 <- FRE14_TAMg2
FRE14_TAMg3$p1inp2vec <- is.element(FRE14_TAMg3$player1, player2vector)
FRE14_TAMg3$p2inp1vec <- is.element(FRE14_TAMg3$player2, player1vector)

addPlayer1 <- FRE14_TAMg3[ which(FRE14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_TAMg3[ which(FRE14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_TAMg2 <- rbind(FRE14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
FRE14_TAMft <- ftable(FRE14_TAMg2$player1, FRE14_TAMg2$player2)
FRE14_TAMft2 <- as.matrix(FRE14_TAMft)
numRows <- nrow(FRE14_TAMft2)
numCols <- ncol(FRE14_TAMft2)
FRE14_TAMft3 <- FRE14_TAMft2[c(2:numRows) , c(2:numCols)]
FRE14_TAMTable <- graph.adjacency(FRE14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(FRE14_TAMTable, vertex.label = V(FRE14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
FRE14_TAM.clusterCoef <- transitivity(FRE14_TAMTable, type="global") #cluster coefficient
FRE14_TAM.degreeCent <- centralization.degree(FRE14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_TAMftn <- as.network.matrix(FRE14_TAMft)
FRE14_TAM.netDensity <- network.density(FRE14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_TAM.entropy <- entropy(FRE14_TAMft) #entropy

FRE14_TAM.netMx <- cbind(FRE14_TAM.netMx, FRE14_TAM.clusterCoef, FRE14_TAM.degreeCent$centralization,
                         FRE14_TAM.netDensity, FRE14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
FRE14_SDMg2 <- data.frame(FRE14_SDM)
FRE14_SDMg2 <- FRE14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_SDMg2$player1
player2vector <- FRE14_SDMg2$player2
FRE14_SDMg3 <- FRE14_SDMg2
FRE14_SDMg3$p1inp2vec <- is.element(FRE14_SDMg3$player1, player2vector)
FRE14_SDMg3$p2inp1vec <- is.element(FRE14_SDMg3$player2, player1vector)

addPlayer1 <- FRE14_SDMg3[ which(FRE14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_SDMg3[ which(FRE14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_SDMg2 <- rbind(FRE14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
FRE14_SDMft <- ftable(FRE14_SDMg2$player1, FRE14_SDMg2$player2)
FRE14_SDMft2 <- as.matrix(FRE14_SDMft)
numRows <- nrow(FRE14_SDMft2)
numCols <- ncol(FRE14_SDMft2)
FRE14_SDMft3 <- FRE14_SDMft2[c(2:numRows) , c(2:numCols)]
FRE14_SDMTable <- graph.adjacency(FRE14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(FRE14_SDMTable, vertex.label = V(FRE14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
FRE14_SDM.clusterCoef <- transitivity(FRE14_SDMTable, type="global") #cluster coefficient
FRE14_SDM.degreeCent <- centralization.degree(FRE14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_SDMftn <- as.network.matrix(FRE14_SDMft)
FRE14_SDM.netDensity <- network.density(FRE14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_SDM.entropy <- entropy(FRE14_SDMft) #entropy

FRE14_SDM.netMx <- cbind(FRE14_SDM.netMx, FRE14_SDM.clusterCoef, FRE14_SDM.degreeCent$centralization,
                         FRE14_SDM.netDensity, FRE14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************
#NA

round = 14
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
FRE14_TDMg2 <- data.frame(FRE14_TDM)
FRE14_TDMg2 <- FRE14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_TDMg2$player1
player2vector <- FRE14_TDMg2$player2
FRE14_TDMg3 <- FRE14_TDMg2
FRE14_TDMg3$p1inp2vec <- is.element(FRE14_TDMg3$player1, player2vector)
FRE14_TDMg3$p2inp1vec <- is.element(FRE14_TDMg3$player2, player1vector)

addPlayer1 <- FRE14_TDMg3[ which(FRE14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_TDMg3[ which(FRE14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_TDMg2 <- rbind(FRE14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
FRE14_TDMft <- ftable(FRE14_TDMg2$player1, FRE14_TDMg2$player2)
FRE14_TDMft2 <- as.matrix(FRE14_TDMft)
numRows <- nrow(FRE14_TDMft2)
numCols <- ncol(FRE14_TDMft2)
FRE14_TDMft3 <- FRE14_TDMft2[c(2:numRows) , c(2:numCols)]
FRE14_TDMTable <- graph.adjacency(FRE14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(FRE14_TDMTable, vertex.label = V(FRE14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
FRE14_TDM.clusterCoef <- transitivity(FRE14_TDMTable, type="global") #cluster coefficient
FRE14_TDM.degreeCent <- centralization.degree(FRE14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_TDMftn <- as.network.matrix(FRE14_TDMft)
FRE14_TDM.netDensity <- network.density(FRE14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_TDM.entropy <- entropy(FRE14_TDMft) #entropy

FRE14_TDM.netMx <- cbind(FRE14_TDM.netMx, FRE14_TDM.clusterCoef, FRE14_TDM.degreeCent$centralization,
                         FRE14_TDM.netDensity, FRE14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************

round = 14
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
FRE14_SDg2 <- data.frame(FRE14_SD)
FRE14_SDg2 <- FRE14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_SDg2$player1
player2vector <- FRE14_SDg2$player2
FRE14_SDg3 <- FRE14_SDg2
FRE14_SDg3$p1inp2vec <- is.element(FRE14_SDg3$player1, player2vector)
FRE14_SDg3$p2inp1vec <- is.element(FRE14_SDg3$player2, player1vector)

addPlayer1 <- FRE14_SDg3[ which(FRE14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_SDg3[ which(FRE14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_SDg2 <- rbind(FRE14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
FRE14_SDft <- ftable(FRE14_SDg2$player1, FRE14_SDg2$player2)
FRE14_SDft2 <- as.matrix(FRE14_SDft)
numRows <- nrow(FRE14_SDft2)
numCols <- ncol(FRE14_SDft2)
FRE14_SDft3 <- FRE14_SDft2[c(2:numRows) , c(2:numCols)]
FRE14_SDTable <- graph.adjacency(FRE14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(FRE14_SDTable, vertex.label = V(FRE14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
FRE14_SD.clusterCoef <- transitivity(FRE14_SDTable, type="global") #cluster coefficient
FRE14_SD.degreeCent <- centralization.degree(FRE14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_SDftn <- as.network.matrix(FRE14_SDft)
FRE14_SD.netDensity <- network.density(FRE14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_SD.entropy <- entropy(FRE14_SDft) #entropy

FRE14_SD.netMx <- cbind(FRE14_SD.netMx, FRE14_SD.clusterCoef, FRE14_SD.degreeCent$centralization,
                        FRE14_SD.netDensity, FRE14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
FRE14_TDg2 <- data.frame(FRE14_TD)
FRE14_TDg2 <- FRE14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_TDg2$player1
player2vector <- FRE14_TDg2$player2
FRE14_TDg3 <- FRE14_TDg2
FRE14_TDg3$p1inp2vec <- is.element(FRE14_TDg3$player1, player2vector)
FRE14_TDg3$p2inp1vec <- is.element(FRE14_TDg3$player2, player1vector)

addPlayer1 <- FRE14_TDg3[ which(FRE14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_TDg3[ which(FRE14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_TDg2 <- rbind(FRE14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
FRE14_TDft <- ftable(FRE14_TDg2$player1, FRE14_TDg2$player2)
FRE14_TDft2 <- as.matrix(FRE14_TDft)
numRows <- nrow(FRE14_TDft2)
numCols <- ncol(FRE14_TDft2)
FRE14_TDft3 <- FRE14_TDft2[c(2:numRows) , c(2:numCols)]
FRE14_TDTable <- graph.adjacency(FRE14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(FRE14_TDTable, vertex.label = V(FRE14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
FRE14_TD.clusterCoef <- transitivity(FRE14_TDTable, type="global") #cluster coefficient
FRE14_TD.degreeCent <- centralization.degree(FRE14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_TDftn <- as.network.matrix(FRE14_TDft)
FRE14_TD.netDensity <- network.density(FRE14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_TD.entropy <- entropy(FRE14_TDft) #entropy

FRE14_TD.netMx <- cbind(FRE14_TD.netMx, FRE14_TD.clusterCoef, FRE14_TD.degreeCent$centralization,
                        FRE14_TD.netDensity, FRE14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
FRE14_QTg2 <- data.frame(FRE14_QT)
FRE14_QTg2 <- FRE14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE14_QTg2$player1
player2vector <- FRE14_QTg2$player2
FRE14_QTg3 <- FRE14_QTg2
FRE14_QTg3$p1inp2vec <- is.element(FRE14_QTg3$player1, player2vector)
FRE14_QTg3$p2inp1vec <- is.element(FRE14_QTg3$player2, player1vector)

addPlayer1 <- FRE14_QTg3[ which(FRE14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE14_QTg3[ which(FRE14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE14_QTg2 <- rbind(FRE14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
FRE14_QTft <- ftable(FRE14_QTg2$player1, FRE14_QTg2$player2)
FRE14_QTft2 <- as.matrix(FRE14_QTft)
numRows <- nrow(FRE14_QTft2)
numCols <- ncol(FRE14_QTft2)
FRE14_QTft3 <- FRE14_QTft2[c(2:numRows) , c(2:numCols)]
FRE14_QTTable <- graph.adjacency(FRE14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(FRE14_QTTable, vertex.label = V(FRE14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
FRE14_QT.clusterCoef <- transitivity(FRE14_QTTable, type="global") #cluster coefficient
FRE14_QT.degreeCent <- centralization.degree(FRE14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE14_QTftn <- as.network.matrix(FRE14_QTft)
FRE14_QT.netDensity <- network.density(FRE14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE14_QT.entropy <- entropy(FRE14_QTft) #entropy

FRE14_QT.netMx <- cbind(FRE14_QT.netMx, FRE14_QT.clusterCoef, FRE14_QT.degreeCent$centralization,
                        FRE14_QT.netDensity, FRE14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE14_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
GCFC14_Gg2 <- data.frame(GCFC14_G)
GCFC14_Gg2 <- GCFC14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_Gg2$player1
player2vector <- GCFC14_Gg2$player2
GCFC14_Gg3 <- GCFC14_Gg2
GCFC14_Gg3$p1inp2vec <- is.element(GCFC14_Gg3$player1, player2vector)
GCFC14_Gg3$p2inp1vec <- is.element(GCFC14_Gg3$player2, player1vector)

addPlayer1 <- GCFC14_Gg3[ which(GCFC14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_Gg3[ which(GCFC14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_Gg2 <- rbind(GCFC14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
GCFC14_Gft <- ftable(GCFC14_Gg2$player1, GCFC14_Gg2$player2)
GCFC14_Gft2 <- as.matrix(GCFC14_Gft)
numRows <- nrow(GCFC14_Gft2)
numCols <- ncol(GCFC14_Gft2)
GCFC14_Gft3 <- GCFC14_Gft2[c(2:numRows) , c(2:numCols)]
GCFC14_GTable <- graph.adjacency(GCFC14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(GCFC14_GTable, vertex.label = V(GCFC14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
GCFC14_G.clusterCoef <- transitivity(GCFC14_GTable, type="global") #cluster coefficient
GCFC14_G.degreeCent <- centralization.degree(GCFC14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_Gftn <- as.network.matrix(GCFC14_Gft)
GCFC14_G.netDensity <- network.density(GCFC14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_G.entropy <- entropy(GCFC14_Gft) #entropy

GCFC14_G.netMx <- cbind(GCFC14_G.netMx, GCFC14_G.clusterCoef, GCFC14_G.degreeCent$centralization,
                        GCFC14_G.netDensity, GCFC14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
GCFC14_Bg2 <- data.frame(GCFC14_B)
GCFC14_Bg2 <- GCFC14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_Bg2$player1
player2vector <- GCFC14_Bg2$player2
GCFC14_Bg3 <- GCFC14_Bg2
GCFC14_Bg3$p1inp2vec <- is.element(GCFC14_Bg3$player1, player2vector)
GCFC14_Bg3$p2inp1vec <- is.element(GCFC14_Bg3$player2, player1vector)

addPlayer1 <- GCFC14_Bg3[ which(GCFC14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC14_Bg3[ which(GCFC14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_Bg2 <- rbind(GCFC14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
GCFC14_Bft <- ftable(GCFC14_Bg2$player1, GCFC14_Bg2$player2)
GCFC14_Bft2 <- as.matrix(GCFC14_Bft)
numRows <- nrow(GCFC14_Bft2)
numCols <- ncol(GCFC14_Bft2)
GCFC14_Bft3 <- GCFC14_Bft2[c(2:numRows) , c(2:numCols)]
GCFC14_BTable <- graph.adjacency(GCFC14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(GCFC14_BTable, vertex.label = V(GCFC14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
GCFC14_B.clusterCoef <- transitivity(GCFC14_BTable, type="global") #cluster coefficient
GCFC14_B.degreeCent <- centralization.degree(GCFC14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_Bftn <- as.network.matrix(GCFC14_Bft)
GCFC14_B.netDensity <- network.density(GCFC14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_B.entropy <- entropy(GCFC14_Bft) #entropy

GCFC14_B.netMx <- cbind(GCFC14_B.netMx, GCFC14_B.clusterCoef, GCFC14_B.degreeCent$centralization,
                        GCFC14_B.netDensity, GCFC14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************

round = 14
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
GCFC14_SFg2 <- data.frame(GCFC14_SF)
GCFC14_SFg2 <- GCFC14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_SFg2$player1
player2vector <- GCFC14_SFg2$player2
GCFC14_SFg3 <- GCFC14_SFg2
GCFC14_SFg3$p1inp2vec <- is.element(GCFC14_SFg3$player1, player2vector)
GCFC14_SFg3$p2inp1vec <- is.element(GCFC14_SFg3$player2, player1vector)

addPlayer1 <- GCFC14_SFg3[ which(GCFC14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_SFg3[ which(GCFC14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_SFg2 <- rbind(GCFC14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
GCFC14_SFft <- ftable(GCFC14_SFg2$player1, GCFC14_SFg2$player2)
GCFC14_SFft2 <- as.matrix(GCFC14_SFft)
numRows <- nrow(GCFC14_SFft2)
numCols <- ncol(GCFC14_SFft2)
GCFC14_SFft3 <- GCFC14_SFft2[c(2:numRows) , c(2:numCols)]
GCFC14_SFTable <- graph.adjacency(GCFC14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(GCFC14_SFTable, vertex.label = V(GCFC14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
GCFC14_SF.clusterCoef <- transitivity(GCFC14_SFTable, type="global") #cluster coefficient
GCFC14_SF.degreeCent <- centralization.degree(GCFC14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_SFftn <- as.network.matrix(GCFC14_SFft)
GCFC14_SF.netDensity <- network.density(GCFC14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_SF.entropy <- entropy(GCFC14_SFft) #entropy

GCFC14_SF.netMx <- cbind(GCFC14_SF.netMx, GCFC14_SF.clusterCoef, GCFC14_SF.degreeCent$centralization,
                         GCFC14_SF.netDensity, GCFC14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
GCFC14_TFg2 <- data.frame(GCFC14_TF)
GCFC14_TFg2 <- GCFC14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_TFg2$player1
player2vector <- GCFC14_TFg2$player2
GCFC14_TFg3 <- GCFC14_TFg2
GCFC14_TFg3$p1inp2vec <- is.element(GCFC14_TFg3$player1, player2vector)
GCFC14_TFg3$p2inp1vec <- is.element(GCFC14_TFg3$player2, player1vector)

addPlayer1 <- GCFC14_TFg3[ which(GCFC14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_TFg3[ which(GCFC14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_TFg2 <- rbind(GCFC14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
GCFC14_TFft <- ftable(GCFC14_TFg2$player1, GCFC14_TFg2$player2)
GCFC14_TFft2 <- as.matrix(GCFC14_TFft)
numRows <- nrow(GCFC14_TFft2)
numCols <- ncol(GCFC14_TFft2)
GCFC14_TFft3 <- GCFC14_TFft2[c(2:numRows) , c(2:numCols)]
GCFC14_TFTable <- graph.adjacency(GCFC14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(GCFC14_TFTable, vertex.label = V(GCFC14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
GCFC14_TF.clusterCoef <- transitivity(GCFC14_TFTable, type="global") #cluster coefficient
GCFC14_TF.degreeCent <- centralization.degree(GCFC14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_TFftn <- as.network.matrix(GCFC14_TFft)
GCFC14_TF.netDensity <- network.density(GCFC14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_TF.entropy <- entropy(GCFC14_TFft) #entropy

GCFC14_TF.netMx <- cbind(GCFC14_TF.netMx, GCFC14_TF.clusterCoef, GCFC14_TF.degreeCent$centralization,
                         GCFC14_TF.netDensity, GCFC14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
GCFC14_SAMg2 <- data.frame(GCFC14_SAM)
GCFC14_SAMg2 <- GCFC14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_SAMg2$player1
player2vector <- GCFC14_SAMg2$player2
GCFC14_SAMg3 <- GCFC14_SAMg2
GCFC14_SAMg3$p1inp2vec <- is.element(GCFC14_SAMg3$player1, player2vector)
GCFC14_SAMg3$p2inp1vec <- is.element(GCFC14_SAMg3$player2, player1vector)

addPlayer1 <- GCFC14_SAMg3[ which(GCFC14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_SAMg3[ which(GCFC14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_SAMg2 <- rbind(GCFC14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
GCFC14_SAMft <- ftable(GCFC14_SAMg2$player1, GCFC14_SAMg2$player2)
GCFC14_SAMft2 <- as.matrix(GCFC14_SAMft)
numRows <- nrow(GCFC14_SAMft2)
numCols <- ncol(GCFC14_SAMft2)
GCFC14_SAMft3 <- GCFC14_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC14_SAMTable <- graph.adjacency(GCFC14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(GCFC14_SAMTable, vertex.label = V(GCFC14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
GCFC14_SAM.clusterCoef <- transitivity(GCFC14_SAMTable, type="global") #cluster coefficient
GCFC14_SAM.degreeCent <- centralization.degree(GCFC14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_SAMftn <- as.network.matrix(GCFC14_SAMft)
GCFC14_SAM.netDensity <- network.density(GCFC14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_SAM.entropy <- entropy(GCFC14_SAMft) #entropy

GCFC14_SAM.netMx <- cbind(GCFC14_SAM.netMx, GCFC14_SAM.clusterCoef, GCFC14_SAM.degreeCent$centralization,
                          GCFC14_SAM.netDensity, GCFC14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************
#NA

round = 14
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
GCFC14_TAMg2 <- data.frame(GCFC14_TAM)
GCFC14_TAMg2 <- GCFC14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_TAMg2$player1
player2vector <- GCFC14_TAMg2$player2
GCFC14_TAMg3 <- GCFC14_TAMg2
GCFC14_TAMg3$p1inp2vec <- is.element(GCFC14_TAMg3$player1, player2vector)
GCFC14_TAMg3$p2inp1vec <- is.element(GCFC14_TAMg3$player2, player1vector)

addPlayer1 <- GCFC14_TAMg3[ which(GCFC14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_TAMg3[ which(GCFC14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_TAMg2 <- rbind(GCFC14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
GCFC14_TAMft <- ftable(GCFC14_TAMg2$player1, GCFC14_TAMg2$player2)
GCFC14_TAMft2 <- as.matrix(GCFC14_TAMft)
numRows <- nrow(GCFC14_TAMft2)
numCols <- ncol(GCFC14_TAMft2)
GCFC14_TAMft3 <- GCFC14_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC14_TAMTable <- graph.adjacency(GCFC14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(GCFC14_TAMTable, vertex.label = V(GCFC14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
GCFC14_TAM.clusterCoef <- transitivity(GCFC14_TAMTable, type="global") #cluster coefficient
GCFC14_TAM.degreeCent <- centralization.degree(GCFC14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_TAMftn <- as.network.matrix(GCFC14_TAMft)
GCFC14_TAM.netDensity <- network.density(GCFC14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_TAM.entropy <- entropy(GCFC14_TAMft) #entropy

GCFC14_TAM.netMx <- cbind(GCFC14_TAM.netMx, GCFC14_TAM.clusterCoef, GCFC14_TAM.degreeCent$centralization,
                          GCFC14_TAM.netDensity, GCFC14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
GCFC14_SDMg2 <- data.frame(GCFC14_SDM)
GCFC14_SDMg2 <- GCFC14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_SDMg2$player1
player2vector <- GCFC14_SDMg2$player2
GCFC14_SDMg3 <- GCFC14_SDMg2
GCFC14_SDMg3$p1inp2vec <- is.element(GCFC14_SDMg3$player1, player2vector)
GCFC14_SDMg3$p2inp1vec <- is.element(GCFC14_SDMg3$player2, player1vector)

addPlayer1 <- GCFC14_SDMg3[ which(GCFC14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_SDMg3[ which(GCFC14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_SDMg2 <- rbind(GCFC14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
GCFC14_SDMft <- ftable(GCFC14_SDMg2$player1, GCFC14_SDMg2$player2)
GCFC14_SDMft2 <- as.matrix(GCFC14_SDMft)
numRows <- nrow(GCFC14_SDMft2)
numCols <- ncol(GCFC14_SDMft2)
GCFC14_SDMft3 <- GCFC14_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC14_SDMTable <- graph.adjacency(GCFC14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(GCFC14_SDMTable, vertex.label = V(GCFC14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
GCFC14_SDM.clusterCoef <- transitivity(GCFC14_SDMTable, type="global") #cluster coefficient
GCFC14_SDM.degreeCent <- centralization.degree(GCFC14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_SDMftn <- as.network.matrix(GCFC14_SDMft)
GCFC14_SDM.netDensity <- network.density(GCFC14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_SDM.entropy <- entropy(GCFC14_SDMft) #entropy

GCFC14_SDM.netMx <- cbind(GCFC14_SDM.netMx, GCFC14_SDM.clusterCoef, GCFC14_SDM.degreeCent$centralization,
                          GCFC14_SDM.netDensity, GCFC14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
GCFC14_TDMg2 <- data.frame(GCFC14_TDM)
GCFC14_TDMg2 <- GCFC14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_TDMg2$player1
player2vector <- GCFC14_TDMg2$player2
GCFC14_TDMg3 <- GCFC14_TDMg2
GCFC14_TDMg3$p1inp2vec <- is.element(GCFC14_TDMg3$player1, player2vector)
GCFC14_TDMg3$p2inp1vec <- is.element(GCFC14_TDMg3$player2, player1vector)

addPlayer1 <- GCFC14_TDMg3[ which(GCFC14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_TDMg3[ which(GCFC14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_TDMg2 <- rbind(GCFC14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
GCFC14_TDMft <- ftable(GCFC14_TDMg2$player1, GCFC14_TDMg2$player2)
GCFC14_TDMft2 <- as.matrix(GCFC14_TDMft)
numRows <- nrow(GCFC14_TDMft2)
numCols <- ncol(GCFC14_TDMft2)
GCFC14_TDMft3 <- GCFC14_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC14_TDMTable <- graph.adjacency(GCFC14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(GCFC14_TDMTable, vertex.label = V(GCFC14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
GCFC14_TDM.clusterCoef <- transitivity(GCFC14_TDMTable, type="global") #cluster coefficient
GCFC14_TDM.degreeCent <- centralization.degree(GCFC14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_TDMftn <- as.network.matrix(GCFC14_TDMft)
GCFC14_TDM.netDensity <- network.density(GCFC14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_TDM.entropy <- entropy(GCFC14_TDMft) #entropy

GCFC14_TDM.netMx <- cbind(GCFC14_TDM.netMx, GCFC14_TDM.clusterCoef, GCFC14_TDM.degreeCent$centralization,
                          GCFC14_TDM.netDensity, GCFC14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
GCFC14_SDg2 <- data.frame(GCFC14_SD)
GCFC14_SDg2 <- GCFC14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_SDg2$player1
player2vector <- GCFC14_SDg2$player2
GCFC14_SDg3 <- GCFC14_SDg2
GCFC14_SDg3$p1inp2vec <- is.element(GCFC14_SDg3$player1, player2vector)
GCFC14_SDg3$p2inp1vec <- is.element(GCFC14_SDg3$player2, player1vector)

addPlayer1 <- GCFC14_SDg3[ which(GCFC14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_SDg3[ which(GCFC14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_SDg2 <- rbind(GCFC14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
GCFC14_SDft <- ftable(GCFC14_SDg2$player1, GCFC14_SDg2$player2)
GCFC14_SDft2 <- as.matrix(GCFC14_SDft)
numRows <- nrow(GCFC14_SDft2)
numCols <- ncol(GCFC14_SDft2)
GCFC14_SDft3 <- GCFC14_SDft2[c(2:numRows) , c(2:numCols)]
GCFC14_SDTable <- graph.adjacency(GCFC14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(GCFC14_SDTable, vertex.label = V(GCFC14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
GCFC14_SD.clusterCoef <- transitivity(GCFC14_SDTable, type="global") #cluster coefficient
GCFC14_SD.degreeCent <- centralization.degree(GCFC14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_SDftn <- as.network.matrix(GCFC14_SDft)
GCFC14_SD.netDensity <- network.density(GCFC14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_SD.entropy <- entropy(GCFC14_SDft) #entropy

GCFC14_SD.netMx <- cbind(GCFC14_SD.netMx, GCFC14_SD.clusterCoef, GCFC14_SD.degreeCent$centralization,
                         GCFC14_SD.netDensity, GCFC14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
GCFC14_TDg2 <- data.frame(GCFC14_TD)
GCFC14_TDg2 <- GCFC14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_TDg2$player1
player2vector <- GCFC14_TDg2$player2
GCFC14_TDg3 <- GCFC14_TDg2
GCFC14_TDg3$p1inp2vec <- is.element(GCFC14_TDg3$player1, player2vector)
GCFC14_TDg3$p2inp1vec <- is.element(GCFC14_TDg3$player2, player1vector)

addPlayer1 <- GCFC14_TDg3[ which(GCFC14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_TDg3[ which(GCFC14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_TDg2 <- rbind(GCFC14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
GCFC14_TDft <- ftable(GCFC14_TDg2$player1, GCFC14_TDg2$player2)
GCFC14_TDft2 <- as.matrix(GCFC14_TDft)
numRows <- nrow(GCFC14_TDft2)
numCols <- ncol(GCFC14_TDft2)
GCFC14_TDft3 <- GCFC14_TDft2[c(2:numRows) , c(2:numCols)]
GCFC14_TDTable <- graph.adjacency(GCFC14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(GCFC14_TDTable, vertex.label = V(GCFC14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
GCFC14_TD.clusterCoef <- transitivity(GCFC14_TDTable, type="global") #cluster coefficient
GCFC14_TD.degreeCent <- centralization.degree(GCFC14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_TDftn <- as.network.matrix(GCFC14_TDft)
GCFC14_TD.netDensity <- network.density(GCFC14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_TD.entropy <- entropy(GCFC14_TDft) #entropy

GCFC14_TD.netMx <- cbind(GCFC14_TD.netMx, GCFC14_TD.clusterCoef, GCFC14_TD.degreeCent$centralization,
                         GCFC14_TD.netDensity, GCFC14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************

round = 14
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
GCFC14_QTg2 <- data.frame(GCFC14_QT)
GCFC14_QTg2 <- GCFC14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC14_QTg2$player1
player2vector <- GCFC14_QTg2$player2
GCFC14_QTg3 <- GCFC14_QTg2
GCFC14_QTg3$p1inp2vec <- is.element(GCFC14_QTg3$player1, player2vector)
GCFC14_QTg3$p2inp1vec <- is.element(GCFC14_QTg3$player2, player1vector)

addPlayer1 <- GCFC14_QTg3[ which(GCFC14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC14_QTg3[ which(GCFC14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC14_QTg2 <- rbind(GCFC14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
GCFC14_QTft <- ftable(GCFC14_QTg2$player1, GCFC14_QTg2$player2)
GCFC14_QTft2 <- as.matrix(GCFC14_QTft)
numRows <- nrow(GCFC14_QTft2)
numCols <- ncol(GCFC14_QTft2)
GCFC14_QTft3 <- GCFC14_QTft2[c(2:numRows) , c(2:numCols)]
GCFC14_QTTable <- graph.adjacency(GCFC14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(GCFC14_QTTable, vertex.label = V(GCFC14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
GCFC14_QT.clusterCoef <- transitivity(GCFC14_QTTable, type="global") #cluster coefficient
GCFC14_QT.degreeCent <- centralization.degree(GCFC14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC14_QTftn <- as.network.matrix(GCFC14_QTft)
GCFC14_QT.netDensity <- network.density(GCFC14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC14_QT.entropy <- entropy(GCFC14_QTft) #entropy

GCFC14_QT.netMx <- cbind(GCFC14_QT.netMx, GCFC14_QT.clusterCoef, GCFC14_QT.degreeCent$centralization,
                         GCFC14_QT.netDensity, GCFC14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC14_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "GWS"
KIoutcome = "Goal_F"
GWS14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
GWS14_Gg2 <- data.frame(GWS14_G)
GWS14_Gg2 <- GWS14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_Gg2$player1
player2vector <- GWS14_Gg2$player2
GWS14_Gg3 <- GWS14_Gg2
GWS14_Gg3$p1inp2vec <- is.element(GWS14_Gg3$player1, player2vector)
GWS14_Gg3$p2inp1vec <- is.element(GWS14_Gg3$player2, player1vector)

addPlayer1 <- GWS14_Gg3[ which(GWS14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_Gg3[ which(GWS14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_Gg2 <- rbind(GWS14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
GWS14_Gft <- ftable(GWS14_Gg2$player1, GWS14_Gg2$player2)
GWS14_Gft2 <- as.matrix(GWS14_Gft)
numRows <- nrow(GWS14_Gft2)
numCols <- ncol(GWS14_Gft2)
GWS14_Gft3 <- GWS14_Gft2[c(1:numRows) , c(1:numCols)]
GWS14_GTable <- graph.adjacency(GWS14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(GWS14_GTable, vertex.label = V(GWS14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
GWS14_G.clusterCoef <- transitivity(GWS14_GTable, type="global") #cluster coefficient
GWS14_G.degreeCent <- centralization.degree(GWS14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_Gftn <- as.network.matrix(GWS14_Gft)
GWS14_G.netDensity <- network.density(GWS14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_G.entropy <- entropy(GWS14_Gft) #entropy

GWS14_G.netMx <- cbind(GWS14_G.netMx, GWS14_G.clusterCoef, GWS14_G.degreeCent$centralization,
                       GWS14_G.netDensity, GWS14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "GWS"
KIoutcome = "Behind_F"
GWS14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
GWS14_Bg2 <- data.frame(GWS14_B)
GWS14_Bg2 <- GWS14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_Bg2$player1
player2vector <- GWS14_Bg2$player2
GWS14_Bg3 <- GWS14_Bg2
GWS14_Bg3$p1inp2vec <- is.element(GWS14_Bg3$player1, player2vector)
GWS14_Bg3$p2inp1vec <- is.element(GWS14_Bg3$player2, player1vector)

addPlayer1 <- GWS14_Bg3[ which(GWS14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_Bg3[ which(GWS14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_Bg2 <- rbind(GWS14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
GWS14_Bft <- ftable(GWS14_Bg2$player1, GWS14_Bg2$player2)
GWS14_Bft2 <- as.matrix(GWS14_Bft)
numRows <- nrow(GWS14_Bft2)
numCols <- ncol(GWS14_Bft2)
GWS14_Bft3 <- GWS14_Bft2[c(2:numRows) , c(2:numCols)]
GWS14_BTable <- graph.adjacency(GWS14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(GWS14_BTable, vertex.label = V(GWS14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
GWS14_B.clusterCoef <- transitivity(GWS14_BTable, type="global") #cluster coefficient
GWS14_B.degreeCent <- centralization.degree(GWS14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_Bftn <- as.network.matrix(GWS14_Bft)
GWS14_B.netDensity <- network.density(GWS14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_B.entropy <- entropy(GWS14_Bft) #entropy

GWS14_B.netMx <- cbind(GWS14_B.netMx, GWS14_B.clusterCoef, GWS14_B.degreeCent$centralization,
                       GWS14_B.netDensity, GWS14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
GWS14_SFg2 <- data.frame(GWS14_SF)
GWS14_SFg2 <- GWS14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_SFg2$player1
player2vector <- GWS14_SFg2$player2
GWS14_SFg3 <- GWS14_SFg2
GWS14_SFg3$p1inp2vec <- is.element(GWS14_SFg3$player1, player2vector)
GWS14_SFg3$p2inp1vec <- is.element(GWS14_SFg3$player2, player1vector)

addPlayer1 <- GWS14_SFg3[ which(GWS14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_SFg3[ which(GWS14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_SFg2 <- rbind(GWS14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
GWS14_SFft <- ftable(GWS14_SFg2$player1, GWS14_SFg2$player2)
GWS14_SFft2 <- as.matrix(GWS14_SFft)
numRows <- nrow(GWS14_SFft2)
numCols <- ncol(GWS14_SFft2)
GWS14_SFft3 <- GWS14_SFft2[c(2:numRows) , c(2:numCols)]
GWS14_SFTable <- graph.adjacency(GWS14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(GWS14_SFTable, vertex.label = V(GWS14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
GWS14_SF.clusterCoef <- transitivity(GWS14_SFTable, type="global") #cluster coefficient
GWS14_SF.degreeCent <- centralization.degree(GWS14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_SFftn <- as.network.matrix(GWS14_SFft)
GWS14_SF.netDensity <- network.density(GWS14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_SF.entropy <- entropy(GWS14_SFft) #entropy

GWS14_SF.netMx <- cbind(GWS14_SF.netMx, GWS14_SF.clusterCoef, GWS14_SF.degreeCent$centralization,
                        GWS14_SF.netDensity, GWS14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
GWS14_TFg2 <- data.frame(GWS14_TF)
GWS14_TFg2 <- GWS14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_TFg2$player1
player2vector <- GWS14_TFg2$player2
GWS14_TFg3 <- GWS14_TFg2
GWS14_TFg3$p1inp2vec <- is.element(GWS14_TFg3$player1, player2vector)
GWS14_TFg3$p2inp1vec <- is.element(GWS14_TFg3$player2, player1vector)

addPlayer1 <- GWS14_TFg3[ which(GWS14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_TFg3[ which(GWS14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_TFg2 <- rbind(GWS14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
GWS14_TFft <- ftable(GWS14_TFg2$player1, GWS14_TFg2$player2)
GWS14_TFft2 <- as.matrix(GWS14_TFft)
numRows <- nrow(GWS14_TFft2)
numCols <- ncol(GWS14_TFft2)
GWS14_TFft3 <- GWS14_TFft2[c(2:numRows) , c(2:numCols)]
GWS14_TFTable <- graph.adjacency(GWS14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(GWS14_TFTable, vertex.label = V(GWS14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
GWS14_TF.clusterCoef <- transitivity(GWS14_TFTable, type="global") #cluster coefficient
GWS14_TF.degreeCent <- centralization.degree(GWS14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_TFftn <- as.network.matrix(GWS14_TFft)
GWS14_TF.netDensity <- network.density(GWS14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_TF.entropy <- entropy(GWS14_TFft) #entropy

GWS14_TF.netMx <- cbind(GWS14_TF.netMx, GWS14_TF.clusterCoef, GWS14_TF.degreeCent$centralization,
                        GWS14_TF.netDensity, GWS14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
GWS14_SAMg2 <- data.frame(GWS14_SAM)
GWS14_SAMg2 <- GWS14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_SAMg2$player1
player2vector <- GWS14_SAMg2$player2
GWS14_SAMg3 <- GWS14_SAMg2
GWS14_SAMg3$p1inp2vec <- is.element(GWS14_SAMg3$player1, player2vector)
GWS14_SAMg3$p2inp1vec <- is.element(GWS14_SAMg3$player2, player1vector)

addPlayer1 <- GWS14_SAMg3[ which(GWS14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_SAMg3[ which(GWS14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_SAMg2 <- rbind(GWS14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
GWS14_SAMft <- ftable(GWS14_SAMg2$player1, GWS14_SAMg2$player2)
GWS14_SAMft2 <- as.matrix(GWS14_SAMft)
numRows <- nrow(GWS14_SAMft2)
numCols <- ncol(GWS14_SAMft2)
GWS14_SAMft3 <- GWS14_SAMft2[c(2:numRows) , c(2:numCols)]
GWS14_SAMTable <- graph.adjacency(GWS14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(GWS14_SAMTable, vertex.label = V(GWS14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
GWS14_SAM.clusterCoef <- transitivity(GWS14_SAMTable, type="global") #cluster coefficient
GWS14_SAM.degreeCent <- centralization.degree(GWS14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_SAMftn <- as.network.matrix(GWS14_SAMft)
GWS14_SAM.netDensity <- network.density(GWS14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_SAM.entropy <- entropy(GWS14_SAMft) #entropy

GWS14_SAM.netMx <- cbind(GWS14_SAM.netMx, GWS14_SAM.clusterCoef, GWS14_SAM.degreeCent$centralization,
                         GWS14_SAM.netDensity, GWS14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
GWS14_TAMg2 <- data.frame(GWS14_TAM)
GWS14_TAMg2 <- GWS14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_TAMg2$player1
player2vector <- GWS14_TAMg2$player2
GWS14_TAMg3 <- GWS14_TAMg2
GWS14_TAMg3$p1inp2vec <- is.element(GWS14_TAMg3$player1, player2vector)
GWS14_TAMg3$p2inp1vec <- is.element(GWS14_TAMg3$player2, player1vector)

addPlayer1 <- GWS14_TAMg3[ which(GWS14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS14_TAMg3[ which(GWS14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_TAMg2 <- rbind(GWS14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
GWS14_TAMft <- ftable(GWS14_TAMg2$player1, GWS14_TAMg2$player2)
GWS14_TAMft2 <- as.matrix(GWS14_TAMft)
numRows <- nrow(GWS14_TAMft2)
numCols <- ncol(GWS14_TAMft2)
GWS14_TAMft3 <- GWS14_TAMft2[c(2:numRows) , c(2:numCols)]
GWS14_TAMTable <- graph.adjacency(GWS14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(GWS14_TAMTable, vertex.label = V(GWS14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
GWS14_TAM.clusterCoef <- transitivity(GWS14_TAMTable, type="global") #cluster coefficient
GWS14_TAM.degreeCent <- centralization.degree(GWS14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_TAMftn <- as.network.matrix(GWS14_TAMft)
GWS14_TAM.netDensity <- network.density(GWS14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_TAM.entropy <- entropy(GWS14_TAMft) #entropy

GWS14_TAM.netMx <- cbind(GWS14_TAM.netMx, GWS14_TAM.clusterCoef, GWS14_TAM.degreeCent$centralization,
                         GWS14_TAM.netDensity, GWS14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
GWS14_SDMg2 <- data.frame(GWS14_SDM)
GWS14_SDMg2 <- GWS14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_SDMg2$player1
player2vector <- GWS14_SDMg2$player2
GWS14_SDMg3 <- GWS14_SDMg2
GWS14_SDMg3$p1inp2vec <- is.element(GWS14_SDMg3$player1, player2vector)
GWS14_SDMg3$p2inp1vec <- is.element(GWS14_SDMg3$player2, player1vector)

addPlayer1 <- GWS14_SDMg3[ which(GWS14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_SDMg3[ which(GWS14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_SDMg2 <- rbind(GWS14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
GWS14_SDMft <- ftable(GWS14_SDMg2$player1, GWS14_SDMg2$player2)
GWS14_SDMft2 <- as.matrix(GWS14_SDMft)
numRows <- nrow(GWS14_SDMft2)
numCols <- ncol(GWS14_SDMft2)
GWS14_SDMft3 <- GWS14_SDMft2[c(2:numRows) , c(2:numCols)]
GWS14_SDMTable <- graph.adjacency(GWS14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(GWS14_SDMTable, vertex.label = V(GWS14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
GWS14_SDM.clusterCoef <- transitivity(GWS14_SDMTable, type="global") #cluster coefficient
GWS14_SDM.degreeCent <- centralization.degree(GWS14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_SDMftn <- as.network.matrix(GWS14_SDMft)
GWS14_SDM.netDensity <- network.density(GWS14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_SDM.entropy <- entropy(GWS14_SDMft) #entropy

GWS14_SDM.netMx <- cbind(GWS14_SDM.netMx, GWS14_SDM.clusterCoef, GWS14_SDM.degreeCent$centralization,
                         GWS14_SDM.netDensity, GWS14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
GWS14_TDMg2 <- data.frame(GWS14_TDM)
GWS14_TDMg2 <- GWS14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_TDMg2$player1
player2vector <- GWS14_TDMg2$player2
GWS14_TDMg3 <- GWS14_TDMg2
GWS14_TDMg3$p1inp2vec <- is.element(GWS14_TDMg3$player1, player2vector)
GWS14_TDMg3$p2inp1vec <- is.element(GWS14_TDMg3$player2, player1vector)

addPlayer1 <- GWS14_TDMg3[ which(GWS14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_TDMg3[ which(GWS14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_TDMg2 <- rbind(GWS14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
GWS14_TDMft <- ftable(GWS14_TDMg2$player1, GWS14_TDMg2$player2)
GWS14_TDMft2 <- as.matrix(GWS14_TDMft)
numRows <- nrow(GWS14_TDMft2)
numCols <- ncol(GWS14_TDMft2)
GWS14_TDMft3 <- GWS14_TDMft2[c(2:numRows) , c(2:numCols)]
GWS14_TDMTable <- graph.adjacency(GWS14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(GWS14_TDMTable, vertex.label = V(GWS14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
GWS14_TDM.clusterCoef <- transitivity(GWS14_TDMTable, type="global") #cluster coefficient
GWS14_TDM.degreeCent <- centralization.degree(GWS14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_TDMftn <- as.network.matrix(GWS14_TDMft)
GWS14_TDM.netDensity <- network.density(GWS14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_TDM.entropy <- entropy(GWS14_TDMft) #entropy

GWS14_TDM.netMx <- cbind(GWS14_TDM.netMx, GWS14_TDM.clusterCoef, GWS14_TDM.degreeCent$centralization,
                         GWS14_TDM.netDensity, GWS14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************

round = 14
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
GWS14_SDg2 <- data.frame(GWS14_SD)
GWS14_SDg2 <- GWS14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_SDg2$player1
player2vector <- GWS14_SDg2$player2
GWS14_SDg3 <- GWS14_SDg2
GWS14_SDg3$p1inp2vec <- is.element(GWS14_SDg3$player1, player2vector)
GWS14_SDg3$p2inp1vec <- is.element(GWS14_SDg3$player2, player1vector)

addPlayer1 <- GWS14_SDg3[ which(GWS14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_SDg3[ which(GWS14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_SDg2 <- rbind(GWS14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
GWS14_SDft <- ftable(GWS14_SDg2$player1, GWS14_SDg2$player2)
GWS14_SDft2 <- as.matrix(GWS14_SDft)
numRows <- nrow(GWS14_SDft2)
numCols <- ncol(GWS14_SDft2)
GWS14_SDft3 <- GWS14_SDft2[c(2:numRows) , c(2:numCols)]
GWS14_SDTable <- graph.adjacency(GWS14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(GWS14_SDTable, vertex.label = V(GWS14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
GWS14_SD.clusterCoef <- transitivity(GWS14_SDTable, type="global") #cluster coefficient
GWS14_SD.degreeCent <- centralization.degree(GWS14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_SDftn <- as.network.matrix(GWS14_SDft)
GWS14_SD.netDensity <- network.density(GWS14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_SD.entropy <- entropy(GWS14_SDft) #entropy

GWS14_SD.netMx <- cbind(GWS14_SD.netMx, GWS14_SD.clusterCoef, GWS14_SD.degreeCent$centralization,
                        GWS14_SD.netDensity, GWS14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
GWS14_TDg2 <- data.frame(GWS14_TD)
GWS14_TDg2 <- GWS14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_TDg2$player1
player2vector <- GWS14_TDg2$player2
GWS14_TDg3 <- GWS14_TDg2
GWS14_TDg3$p1inp2vec <- is.element(GWS14_TDg3$player1, player2vector)
GWS14_TDg3$p2inp1vec <- is.element(GWS14_TDg3$player2, player1vector)

addPlayer1 <- GWS14_TDg3[ which(GWS14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_TDg3[ which(GWS14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_TDg2 <- rbind(GWS14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
GWS14_TDft <- ftable(GWS14_TDg2$player1, GWS14_TDg2$player2)
GWS14_TDft2 <- as.matrix(GWS14_TDft)
numRows <- nrow(GWS14_TDft2)
numCols <- ncol(GWS14_TDft2)
GWS14_TDft3 <- GWS14_TDft2[c(2:numRows) , c(2:numCols)]
GWS14_TDTable <- graph.adjacency(GWS14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(GWS14_TDTable, vertex.label = V(GWS14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
GWS14_TD.clusterCoef <- transitivity(GWS14_TDTable, type="global") #cluster coefficient
GWS14_TD.degreeCent <- centralization.degree(GWS14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_TDftn <- as.network.matrix(GWS14_TDft)
GWS14_TD.netDensity <- network.density(GWS14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_TD.entropy <- entropy(GWS14_TDft) #entropy

GWS14_TD.netMx <- cbind(GWS14_TD.netMx, GWS14_TD.clusterCoef, GWS14_TD.degreeCent$centralization,
                        GWS14_TD.netDensity, GWS14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
GWS14_QTg2 <- data.frame(GWS14_QT)
GWS14_QTg2 <- GWS14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS14_QTg2$player1
player2vector <- GWS14_QTg2$player2
GWS14_QTg3 <- GWS14_QTg2
GWS14_QTg3$p1inp2vec <- is.element(GWS14_QTg3$player1, player2vector)
GWS14_QTg3$p2inp1vec <- is.element(GWS14_QTg3$player2, player1vector)

addPlayer1 <- GWS14_QTg3[ which(GWS14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS14_QTg3[ which(GWS14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS14_QTg2 <- rbind(GWS14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
GWS14_QTft <- ftable(GWS14_QTg2$player1, GWS14_QTg2$player2)
GWS14_QTft2 <- as.matrix(GWS14_QTft)
numRows <- nrow(GWS14_QTft2)
numCols <- ncol(GWS14_QTft2)
GWS14_QTft3 <- GWS14_QTft2[c(2:numRows) , c(2:numCols)]
GWS14_QTTable <- graph.adjacency(GWS14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(GWS14_QTTable, vertex.label = V(GWS14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
GWS14_QT.clusterCoef <- transitivity(GWS14_QTTable, type="global") #cluster coefficient
GWS14_QT.degreeCent <- centralization.degree(GWS14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS14_QTftn <- as.network.matrix(GWS14_QTft)
GWS14_QT.netDensity <- network.density(GWS14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS14_QT.entropy <- entropy(GWS14_QTft) #entropy

GWS14_QT.netMx <- cbind(GWS14_QT.netMx, GWS14_QT.clusterCoef, GWS14_QT.degreeCent$centralization,
                        GWS14_QT.netDensity, GWS14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS14_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "HAW"
KIoutcome = "Goal_F"
HAW14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
HAW14_Gg2 <- data.frame(HAW14_G)
HAW14_Gg2 <- HAW14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_Gg2$player1
player2vector <- HAW14_Gg2$player2
HAW14_Gg3 <- HAW14_Gg2
HAW14_Gg3$p1inp2vec <- is.element(HAW14_Gg3$player1, player2vector)
HAW14_Gg3$p2inp1vec <- is.element(HAW14_Gg3$player2, player1vector)

addPlayer1 <- HAW14_Gg3[ which(HAW14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_Gg3[ which(HAW14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_Gg2 <- rbind(HAW14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
HAW14_Gft <- ftable(HAW14_Gg2$player1, HAW14_Gg2$player2)
HAW14_Gft2 <- as.matrix(HAW14_Gft)
numRows <- nrow(HAW14_Gft2)
numCols <- ncol(HAW14_Gft2)
HAW14_Gft3 <- HAW14_Gft2[c(2:numRows) , c(2:numCols)]
HAW14_GTable <- graph.adjacency(HAW14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(HAW14_GTable, vertex.label = V(HAW14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
HAW14_G.clusterCoef <- transitivity(HAW14_GTable, type="global") #cluster coefficient
HAW14_G.degreeCent <- centralization.degree(HAW14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_Gftn <- as.network.matrix(HAW14_Gft)
HAW14_G.netDensity <- network.density(HAW14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_G.entropy <- entropy(HAW14_Gft) #entropy

HAW14_G.netMx <- cbind(HAW14_G.netMx, HAW14_G.clusterCoef, HAW14_G.degreeCent$centralization,
                       HAW14_G.netDensity, HAW14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "HAW"
KIoutcome = "Behind_F"
HAW14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
HAW14_Bg2 <- data.frame(HAW14_B)
HAW14_Bg2 <- HAW14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_Bg2$player1
player2vector <- HAW14_Bg2$player2
HAW14_Bg3 <- HAW14_Bg2
HAW14_Bg3$p1inp2vec <- is.element(HAW14_Bg3$player1, player2vector)
HAW14_Bg3$p2inp1vec <- is.element(HAW14_Bg3$player2, player1vector)


addPlayer1 <- HAW14_Bg3[ which(HAW14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_Bg3[ which(HAW14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_Bg2 <- rbind(HAW14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
HAW14_Bft <- ftable(HAW14_Bg2$player1, HAW14_Bg2$player2)
HAW14_Bft2 <- as.matrix(HAW14_Bft)
numRows <- nrow(HAW14_Bft2)
numCols <- ncol(HAW14_Bft2)
HAW14_Bft3 <- HAW14_Bft2[c(2:numRows) , c(2:numCols)]
HAW14_BTable <- graph.adjacency(HAW14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(HAW14_BTable, vertex.label = V(HAW14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
HAW14_B.clusterCoef <- transitivity(HAW14_BTable, type="global") #cluster coefficient
HAW14_B.degreeCent <- centralization.degree(HAW14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_Bftn <- as.network.matrix(HAW14_Bft)
HAW14_B.netDensity <- network.density(HAW14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_B.entropy <- entropy(HAW14_Bft) #entropy

HAW14_B.netMx <- cbind(HAW14_B.netMx, HAW14_B.clusterCoef, HAW14_B.degreeCent$centralization,
                       HAW14_B.netDensity, HAW14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
HAW14_SFg2 <- data.frame(HAW14_SF)
HAW14_SFg2 <- HAW14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_SFg2$player1
player2vector <- HAW14_SFg2$player2
HAW14_SFg3 <- HAW14_SFg2
HAW14_SFg3$p1inp2vec <- is.element(HAW14_SFg3$player1, player2vector)
HAW14_SFg3$p2inp1vec <- is.element(HAW14_SFg3$player2, player1vector)

addPlayer1 <- HAW14_SFg3[ which(HAW14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_SFg3[ which(HAW14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_SFg2 <- rbind(HAW14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
HAW14_SFft <- ftable(HAW14_SFg2$player1, HAW14_SFg2$player2)
HAW14_SFft2 <- as.matrix(HAW14_SFft)
numRows <- nrow(HAW14_SFft2)
numCols <- ncol(HAW14_SFft2)
HAW14_SFft3 <- HAW14_SFft2[c(2:numRows) , c(2:numCols)]
HAW14_SFTable <- graph.adjacency(HAW14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(HAW14_SFTable, vertex.label = V(HAW14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
HAW14_SF.clusterCoef <- transitivity(HAW14_SFTable, type="global") #cluster coefficient
HAW14_SF.degreeCent <- centralization.degree(HAW14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_SFftn <- as.network.matrix(HAW14_SFft)
HAW14_SF.netDensity <- network.density(HAW14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_SF.entropy <- entropy(HAW14_SFft) #entropy

HAW14_SF.netMx <- cbind(HAW14_SF.netMx, HAW14_SF.clusterCoef, HAW14_SF.degreeCent$centralization,
                        HAW14_SF.netDensity, HAW14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
HAW14_TFg2 <- data.frame(HAW14_TF)
HAW14_TFg2 <- HAW14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_TFg2$player1
player2vector <- HAW14_TFg2$player2
HAW14_TFg3 <- HAW14_TFg2
HAW14_TFg3$p1inp2vec <- is.element(HAW14_TFg3$player1, player2vector)
HAW14_TFg3$p2inp1vec <- is.element(HAW14_TFg3$player2, player1vector)

addPlayer1 <- HAW14_TFg3[ which(HAW14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_TFg3[ which(HAW14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_TFg2 <- rbind(HAW14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
HAW14_TFft <- ftable(HAW14_TFg2$player1, HAW14_TFg2$player2)
HAW14_TFft2 <- as.matrix(HAW14_TFft)
numRows <- nrow(HAW14_TFft2)
numCols <- ncol(HAW14_TFft2)
HAW14_TFft3 <- HAW14_TFft2[c(2:numRows) , c(2:numCols)]
HAW14_TFTable <- graph.adjacency(HAW14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(HAW14_TFTable, vertex.label = V(HAW14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
HAW14_TF.clusterCoef <- transitivity(HAW14_TFTable, type="global") #cluster coefficient
HAW14_TF.degreeCent <- centralization.degree(HAW14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_TFftn <- as.network.matrix(HAW14_TFft)
HAW14_TF.netDensity <- network.density(HAW14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_TF.entropy <- entropy(HAW14_TFft) #entropy

HAW14_TF.netMx <- cbind(HAW14_TF.netMx, HAW14_TF.clusterCoef, HAW14_TF.degreeCent$centralization,
                        HAW14_TF.netDensity, HAW14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
HAW14_SAMg2 <- data.frame(HAW14_SAM)
HAW14_SAMg2 <- HAW14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_SAMg2$player1
player2vector <- HAW14_SAMg2$player2
HAW14_SAMg3 <- HAW14_SAMg2
HAW14_SAMg3$p1inp2vec <- is.element(HAW14_SAMg3$player1, player2vector)
HAW14_SAMg3$p2inp1vec <- is.element(HAW14_SAMg3$player2, player1vector)

addPlayer1 <- HAW14_SAMg3[ which(HAW14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_SAMg3[ which(HAW14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_SAMg2 <- rbind(HAW14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
HAW14_SAMft <- ftable(HAW14_SAMg2$player1, HAW14_SAMg2$player2)
HAW14_SAMft2 <- as.matrix(HAW14_SAMft)
numRows <- nrow(HAW14_SAMft2)
numCols <- ncol(HAW14_SAMft2)
HAW14_SAMft3 <- HAW14_SAMft2[c(2:numRows) , c(2:numCols)]
HAW14_SAMTable <- graph.adjacency(HAW14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(HAW14_SAMTable, vertex.label = V(HAW14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
HAW14_SAM.clusterCoef <- transitivity(HAW14_SAMTable, type="global") #cluster coefficient
HAW14_SAM.degreeCent <- centralization.degree(HAW14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_SAMftn <- as.network.matrix(HAW14_SAMft)
HAW14_SAM.netDensity <- network.density(HAW14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_SAM.entropy <- entropy(HAW14_SAMft) #entropy

HAW14_SAM.netMx <- cbind(HAW14_SAM.netMx, HAW14_SAM.clusterCoef, HAW14_SAM.degreeCent$centralization,
                         HAW14_SAM.netDensity, HAW14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
HAW14_TAMg2 <- data.frame(HAW14_TAM)
HAW14_TAMg2 <- HAW14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_TAMg2$player1
player2vector <- HAW14_TAMg2$player2
HAW14_TAMg3 <- HAW14_TAMg2
HAW14_TAMg3$p1inp2vec <- is.element(HAW14_TAMg3$player1, player2vector)
HAW14_TAMg3$p2inp1vec <- is.element(HAW14_TAMg3$player2, player1vector)

addPlayer1 <- HAW14_TAMg3[ which(HAW14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_TAMg3[ which(HAW14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_TAMg2 <- rbind(HAW14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
HAW14_TAMft <- ftable(HAW14_TAMg2$player1, HAW14_TAMg2$player2)
HAW14_TAMft2 <- as.matrix(HAW14_TAMft)
numRows <- nrow(HAW14_TAMft2)
numCols <- ncol(HAW14_TAMft2)
HAW14_TAMft3 <- HAW14_TAMft2[c(2:numRows) , c(2:numCols)]
HAW14_TAMTable <- graph.adjacency(HAW14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(HAW14_TAMTable, vertex.label = V(HAW14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
HAW14_TAM.clusterCoef <- transitivity(HAW14_TAMTable, type="global") #cluster coefficient
HAW14_TAM.degreeCent <- centralization.degree(HAW14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_TAMftn <- as.network.matrix(HAW14_TAMft)
HAW14_TAM.netDensity <- network.density(HAW14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_TAM.entropy <- entropy(HAW14_TAMft) #entropy

HAW14_TAM.netMx <- cbind(HAW14_TAM.netMx, HAW14_TAM.clusterCoef, HAW14_TAM.degreeCent$centralization,
                         HAW14_TAM.netDensity, HAW14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
HAW14_SDMg2 <- data.frame(HAW14_SDM)
HAW14_SDMg2 <- HAW14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_SDMg2$player1
player2vector <- HAW14_SDMg2$player2
HAW14_SDMg3 <- HAW14_SDMg2
HAW14_SDMg3$p1inp2vec <- is.element(HAW14_SDMg3$player1, player2vector)
HAW14_SDMg3$p2inp1vec <- is.element(HAW14_SDMg3$player2, player1vector)

addPlayer1 <- HAW14_SDMg3[ which(HAW14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_SDMg3[ which(HAW14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_SDMg2 <- rbind(HAW14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
HAW14_SDMft <- ftable(HAW14_SDMg2$player1, HAW14_SDMg2$player2)
HAW14_SDMft2 <- as.matrix(HAW14_SDMft)
numRows <- nrow(HAW14_SDMft2)
numCols <- ncol(HAW14_SDMft2)
HAW14_SDMft3 <- HAW14_SDMft2[c(2:numRows) , c(2:numCols)]
HAW14_SDMTable <- graph.adjacency(HAW14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(HAW14_SDMTable, vertex.label = V(HAW14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
HAW14_SDM.clusterCoef <- transitivity(HAW14_SDMTable, type="global") #cluster coefficient
HAW14_SDM.degreeCent <- centralization.degree(HAW14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_SDMftn <- as.network.matrix(HAW14_SDMft)
HAW14_SDM.netDensity <- network.density(HAW14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_SDM.entropy <- entropy(HAW14_SDMft) #entropy

HAW14_SDM.netMx <- cbind(HAW14_SDM.netMx, HAW14_SDM.clusterCoef, HAW14_SDM.degreeCent$centralization,
                         HAW14_SDM.netDensity, HAW14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
HAW14_TDMg2 <- data.frame(HAW14_TDM)
HAW14_TDMg2 <- HAW14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_TDMg2$player1
player2vector <- HAW14_TDMg2$player2
HAW14_TDMg3 <- HAW14_TDMg2
HAW14_TDMg3$p1inp2vec <- is.element(HAW14_TDMg3$player1, player2vector)
HAW14_TDMg3$p2inp1vec <- is.element(HAW14_TDMg3$player2, player1vector)

addPlayer1 <- HAW14_TDMg3[ which(HAW14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_TDMg3[ which(HAW14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_TDMg2 <- rbind(HAW14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
HAW14_TDMft <- ftable(HAW14_TDMg2$player1, HAW14_TDMg2$player2)
HAW14_TDMft2 <- as.matrix(HAW14_TDMft)
numRows <- nrow(HAW14_TDMft2)
numCols <- ncol(HAW14_TDMft2)
HAW14_TDMft3 <- HAW14_TDMft2[c(2:numRows) , c(2:numCols)]
HAW14_TDMTable <- graph.adjacency(HAW14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(HAW14_TDMTable, vertex.label = V(HAW14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
HAW14_TDM.clusterCoef <- transitivity(HAW14_TDMTable, type="global") #cluster coefficient
HAW14_TDM.degreeCent <- centralization.degree(HAW14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_TDMftn <- as.network.matrix(HAW14_TDMft)
HAW14_TDM.netDensity <- network.density(HAW14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_TDM.entropy <- entropy(HAW14_TDMft) #entropy

HAW14_TDM.netMx <- cbind(HAW14_TDM.netMx, HAW14_TDM.clusterCoef, HAW14_TDM.degreeCent$centralization,
                         HAW14_TDM.netDensity, HAW14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
HAW14_SDg2 <- data.frame(HAW14_SD)
HAW14_SDg2 <- HAW14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_SDg2$player1
player2vector <- HAW14_SDg2$player2
HAW14_SDg3 <- HAW14_SDg2
HAW14_SDg3$p1inp2vec <- is.element(HAW14_SDg3$player1, player2vector)
HAW14_SDg3$p2inp1vec <- is.element(HAW14_SDg3$player2, player1vector)

addPlayer1 <- HAW14_SDg3[ which(HAW14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_SDg3[ which(HAW14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_SDg2 <- rbind(HAW14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
HAW14_SDft <- ftable(HAW14_SDg2$player1, HAW14_SDg2$player2)
HAW14_SDft2 <- as.matrix(HAW14_SDft)
numRows <- nrow(HAW14_SDft2)
numCols <- ncol(HAW14_SDft2)
HAW14_SDft3 <- HAW14_SDft2[c(2:numRows) , c(2:numCols)]
HAW14_SDTable <- graph.adjacency(HAW14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(HAW14_SDTable, vertex.label = V(HAW14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
HAW14_SD.clusterCoef <- transitivity(HAW14_SDTable, type="global") #cluster coefficient
HAW14_SD.degreeCent <- centralization.degree(HAW14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_SDftn <- as.network.matrix(HAW14_SDft)
HAW14_SD.netDensity <- network.density(HAW14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_SD.entropy <- entropy(HAW14_SDft) #entropy

HAW14_SD.netMx <- cbind(HAW14_SD.netMx, HAW14_SD.clusterCoef, HAW14_SD.degreeCent$centralization,
                        HAW14_SD.netDensity, HAW14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************

round = 14
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
HAW14_TDg2 <- data.frame(HAW14_TD)
HAW14_TDg2 <- HAW14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_TDg2$player1
player2vector <- HAW14_TDg2$player2
HAW14_TDg3 <- HAW14_TDg2
HAW14_TDg3$p1inp2vec <- is.element(HAW14_TDg3$player1, player2vector)
HAW14_TDg3$p2inp1vec <- is.element(HAW14_TDg3$player2, player1vector)

addPlayer1 <- HAW14_TDg3[ which(HAW14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_TDg3[ which(HAW14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_TDg2 <- rbind(HAW14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
HAW14_TDft <- ftable(HAW14_TDg2$player1, HAW14_TDg2$player2)
HAW14_TDft2 <- as.matrix(HAW14_TDft)
numRows <- nrow(HAW14_TDft2)
numCols <- ncol(HAW14_TDft2)
HAW14_TDft3 <- HAW14_TDft2[c(2:numRows) , c(2:numCols)]
HAW14_TDTable <- graph.adjacency(HAW14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(HAW14_TDTable, vertex.label = V(HAW14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
HAW14_TD.clusterCoef <- transitivity(HAW14_TDTable, type="global") #cluster coefficient
HAW14_TD.degreeCent <- centralization.degree(HAW14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_TDftn <- as.network.matrix(HAW14_TDft)
HAW14_TD.netDensity <- network.density(HAW14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_TD.entropy <- entropy(HAW14_TDft) #entropy

HAW14_TD.netMx <- cbind(HAW14_TD.netMx, HAW14_TD.clusterCoef, HAW14_TD.degreeCent$centralization,
                        HAW14_TD.netDensity, HAW14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
HAW14_QTg2 <- data.frame(HAW14_QT)
HAW14_QTg2 <- HAW14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW14_QTg2$player1
player2vector <- HAW14_QTg2$player2
HAW14_QTg3 <- HAW14_QTg2
HAW14_QTg3$p1inp2vec <- is.element(HAW14_QTg3$player1, player2vector)
HAW14_QTg3$p2inp1vec <- is.element(HAW14_QTg3$player2, player1vector)

addPlayer1 <- HAW14_QTg3[ which(HAW14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW14_QTg3[ which(HAW14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW14_QTg2 <- rbind(HAW14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
HAW14_QTft <- ftable(HAW14_QTg2$player1, HAW14_QTg2$player2)
HAW14_QTft2 <- as.matrix(HAW14_QTft)
numRows <- nrow(HAW14_QTft2)
numCols <- ncol(HAW14_QTft2)
HAW14_QTft3 <- HAW14_QTft2[c(2:numRows) , c(2:numCols)]
HAW14_QTTable <- graph.adjacency(HAW14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(HAW14_QTTable, vertex.label = V(HAW14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
HAW14_QT.clusterCoef <- transitivity(HAW14_QTTable, type="global") #cluster coefficient
HAW14_QT.degreeCent <- centralization.degree(HAW14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW14_QTftn <- as.network.matrix(HAW14_QTft)
HAW14_QT.netDensity <- network.density(HAW14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW14_QT.entropy <- entropy(HAW14_QTft) #entropy

HAW14_QT.netMx <- cbind(HAW14_QT.netMx, HAW14_QT.clusterCoef, HAW14_QT.degreeCent$centralization,
                        HAW14_QT.netDensity, HAW14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW14_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "MELB"
KIoutcome = "Goal_F"
MELB14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
MELB14_Gg2 <- data.frame(MELB14_G)
MELB14_Gg2 <- MELB14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_Gg2$player1
player2vector <- MELB14_Gg2$player2
MELB14_Gg3 <- MELB14_Gg2
MELB14_Gg3$p1inp2vec <- is.element(MELB14_Gg3$player1, player2vector)
MELB14_Gg3$p2inp1vec <- is.element(MELB14_Gg3$player2, player1vector)

addPlayer1 <- MELB14_Gg3[ which(MELB14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB14_Gg2 <- rbind(MELB14_Gg2, addPlayer1)

#ROUND 14, Goal graph using weighted edges
MELB14_Gft <- ftable(MELB14_Gg2$player1, MELB14_Gg2$player2)
MELB14_Gft2 <- as.matrix(MELB14_Gft)
numRows <- nrow(MELB14_Gft2)
numCols <- ncol(MELB14_Gft2)
MELB14_Gft3 <- MELB14_Gft2[c(2:numRows) , c(1:numCols)]
MELB14_GTable <- graph.adjacency(MELB14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(MELB14_GTable, vertex.label = V(MELB14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
MELB14_G.clusterCoef <- transitivity(MELB14_GTable, type="global") #cluster coefficient
MELB14_G.degreeCent <- centralization.degree(MELB14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_Gftn <- as.network.matrix(MELB14_Gft)
MELB14_G.netDensity <- network.density(MELB14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_G.entropy <- entropy(MELB14_Gft) #entropy

MELB14_G.netMx <- cbind(MELB14_G.netMx, MELB14_G.clusterCoef, MELB14_G.degreeCent$centralization,
                        MELB14_G.netDensity, MELB14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "MELB"
KIoutcome = "Behind_F"
MELB14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
MELB14_Bg2 <- data.frame(MELB14_B)
MELB14_Bg2 <- MELB14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_Bg2$player1
player2vector <- MELB14_Bg2$player2
MELB14_Bg3 <- MELB14_Bg2
MELB14_Bg3$p1inp2vec <- is.element(MELB14_Bg3$player1, player2vector)
MELB14_Bg3$p2inp1vec <- is.element(MELB14_Bg3$player2, player1vector)

addPlayer1 <- MELB14_Bg3[ which(MELB14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_Bg3[ which(MELB14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_Bg2 <- rbind(MELB14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
MELB14_Bft <- ftable(MELB14_Bg2$player1, MELB14_Bg2$player2)
MELB14_Bft2 <- as.matrix(MELB14_Bft)
numRows <- nrow(MELB14_Bft2)
numCols <- ncol(MELB14_Bft2)
MELB14_Bft3 <- MELB14_Bft2[c(2:numRows) , c(2:numCols)]
MELB14_BTable <- graph.adjacency(MELB14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(MELB14_BTable, vertex.label = V(MELB14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
MELB14_B.clusterCoef <- transitivity(MELB14_BTable, type="global") #cluster coefficient
MELB14_B.degreeCent <- centralization.degree(MELB14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_Bftn <- as.network.matrix(MELB14_Bft)
MELB14_B.netDensity <- network.density(MELB14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_B.entropy <- entropy(MELB14_Bft) #entropy

MELB14_B.netMx <- cbind(MELB14_B.netMx, MELB14_B.clusterCoef, MELB14_B.degreeCent$centralization,
                        MELB14_B.netDensity, MELB14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
MELB14_SFg2 <- data.frame(MELB14_SF)
MELB14_SFg2 <- MELB14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_SFg2$player1
player2vector <- MELB14_SFg2$player2
MELB14_SFg3 <- MELB14_SFg2
MELB14_SFg3$p1inp2vec <- is.element(MELB14_SFg3$player1, player2vector)
MELB14_SFg3$p2inp1vec <- is.element(MELB14_SFg3$player2, player1vector)

addPlayer1 <- MELB14_SFg3[ which(MELB14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_SFg3[ which(MELB14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_SFg2 <- rbind(MELB14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
MELB14_SFft <- ftable(MELB14_SFg2$player1, MELB14_SFg2$player2)
MELB14_SFft2 <- as.matrix(MELB14_SFft)
numRows <- nrow(MELB14_SFft2)
numCols <- ncol(MELB14_SFft2)
MELB14_SFft3 <- MELB14_SFft2[c(2:numRows) , c(2:numCols)]
MELB14_SFTable <- graph.adjacency(MELB14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(MELB14_SFTable, vertex.label = V(MELB14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
MELB14_SF.clusterCoef <- transitivity(MELB14_SFTable, type="global") #cluster coefficient
MELB14_SF.degreeCent <- centralization.degree(MELB14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_SFftn <- as.network.matrix(MELB14_SFft)
MELB14_SF.netDensity <- network.density(MELB14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_SF.entropy <- entropy(MELB14_SFft) #entropy

MELB14_SF.netMx <- cbind(MELB14_SF.netMx, MELB14_SF.clusterCoef, MELB14_SF.degreeCent$centralization,
                         MELB14_SF.netDensity, MELB14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************

round = 14
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
MELB14_TFg2 <- data.frame(MELB14_TF)
MELB14_TFg2 <- MELB14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_TFg2$player1
player2vector <- MELB14_TFg2$player2
MELB14_TFg3 <- MELB14_TFg2
MELB14_TFg3$p1inp2vec <- is.element(MELB14_TFg3$player1, player2vector)
MELB14_TFg3$p2inp1vec <- is.element(MELB14_TFg3$player2, player1vector)

addPlayer1 <- MELB14_TFg3[ which(MELB14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_TFg3[ which(MELB14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_TFg2 <- rbind(MELB14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
MELB14_TFft <- ftable(MELB14_TFg2$player1, MELB14_TFg2$player2)
MELB14_TFft2 <- as.matrix(MELB14_TFft)
numRows <- nrow(MELB14_TFft2)
numCols <- ncol(MELB14_TFft2)
MELB14_TFft3 <- MELB14_TFft2[c(2:numRows) , c(2:numCols)]
MELB14_TFTable <- graph.adjacency(MELB14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(MELB14_TFTable, vertex.label = V(MELB14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
MELB14_TF.clusterCoef <- transitivity(MELB14_TFTable, type="global") #cluster coefficient
MELB14_TF.degreeCent <- centralization.degree(MELB14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_TFftn <- as.network.matrix(MELB14_TFft)
MELB14_TF.netDensity <- network.density(MELB14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_TF.entropy <- entropy(MELB14_TFft) #entropy

MELB14_TF.netMx <- cbind(MELB14_TF.netMx, MELB14_TF.clusterCoef, MELB14_TF.degreeCent$centralization,
                         MELB14_TF.netDensity, MELB14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
MELB14_SAMg2 <- data.frame(MELB14_SAM)
MELB14_SAMg2 <- MELB14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_SAMg2$player1
player2vector <- MELB14_SAMg2$player2
MELB14_SAMg3 <- MELB14_SAMg2
MELB14_SAMg3$p1inp2vec <- is.element(MELB14_SAMg3$player1, player2vector)
MELB14_SAMg3$p2inp1vec <- is.element(MELB14_SAMg3$player2, player1vector)

addPlayer1 <- MELB14_SAMg3[ which(MELB14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_SAMg3[ which(MELB14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_SAMg2 <- rbind(MELB14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
MELB14_SAMft <- ftable(MELB14_SAMg2$player1, MELB14_SAMg2$player2)
MELB14_SAMft2 <- as.matrix(MELB14_SAMft)
numRows <- nrow(MELB14_SAMft2)
numCols <- ncol(MELB14_SAMft2)
MELB14_SAMft3 <- MELB14_SAMft2[c(2:numRows) , c(2:numCols)]
MELB14_SAMTable <- graph.adjacency(MELB14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(MELB14_SAMTable, vertex.label = V(MELB14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
MELB14_SAM.clusterCoef <- transitivity(MELB14_SAMTable, type="global") #cluster coefficient
MELB14_SAM.degreeCent <- centralization.degree(MELB14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_SAMftn <- as.network.matrix(MELB14_SAMft)
MELB14_SAM.netDensity <- network.density(MELB14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_SAM.entropy <- entropy(MELB14_SAMft) #entropy

MELB14_SAM.netMx <- cbind(MELB14_SAM.netMx, MELB14_SAM.clusterCoef, MELB14_SAM.degreeCent$centralization,
                          MELB14_SAM.netDensity, MELB14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
MELB14_TAMg2 <- data.frame(MELB14_TAM)
MELB14_TAMg2 <- MELB14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_TAMg2$player1
player2vector <- MELB14_TAMg2$player2
MELB14_TAMg3 <- MELB14_TAMg2
MELB14_TAMg3$p1inp2vec <- is.element(MELB14_TAMg3$player1, player2vector)
MELB14_TAMg3$p2inp1vec <- is.element(MELB14_TAMg3$player2, player1vector)

addPlayer1 <- MELB14_TAMg3[ which(MELB14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB14_TAMg3[ which(MELB14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_TAMg2 <- rbind(MELB14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
MELB14_TAMft <- ftable(MELB14_TAMg2$player1, MELB14_TAMg2$player2)
MELB14_TAMft2 <- as.matrix(MELB14_TAMft)
numRows <- nrow(MELB14_TAMft2)
numCols <- ncol(MELB14_TAMft2)
MELB14_TAMft3 <- MELB14_TAMft2[c(2:numRows) , c(2:numCols)]
MELB14_TAMTable <- graph.adjacency(MELB14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(MELB14_TAMTable, vertex.label = V(MELB14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
MELB14_TAM.clusterCoef <- transitivity(MELB14_TAMTable, type="global") #cluster coefficient
MELB14_TAM.degreeCent <- centralization.degree(MELB14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_TAMftn <- as.network.matrix(MELB14_TAMft)
MELB14_TAM.netDensity <- network.density(MELB14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_TAM.entropy <- entropy(MELB14_TAMft) #entropy

MELB14_TAM.netMx <- cbind(MELB14_TAM.netMx, MELB14_TAM.clusterCoef, MELB14_TAM.degreeCent$centralization,
                          MELB14_TAM.netDensity, MELB14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
MELB14_SDMg2 <- data.frame(MELB14_SDM)
MELB14_SDMg2 <- MELB14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_SDMg2$player1
player2vector <- MELB14_SDMg2$player2
MELB14_SDMg3 <- MELB14_SDMg2
MELB14_SDMg3$p1inp2vec <- is.element(MELB14_SDMg3$player1, player2vector)
MELB14_SDMg3$p2inp1vec <- is.element(MELB14_SDMg3$player2, player1vector)

addPlayer1 <- MELB14_SDMg3[ which(MELB14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_SDMg3[ which(MELB14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_SDMg2 <- rbind(MELB14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
MELB14_SDMft <- ftable(MELB14_SDMg2$player1, MELB14_SDMg2$player2)
MELB14_SDMft2 <- as.matrix(MELB14_SDMft)
numRows <- nrow(MELB14_SDMft2)
numCols <- ncol(MELB14_SDMft2)
MELB14_SDMft3 <- MELB14_SDMft2[c(2:numRows) , c(2:numCols)]
MELB14_SDMTable <- graph.adjacency(MELB14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(MELB14_SDMTable, vertex.label = V(MELB14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
MELB14_SDM.clusterCoef <- transitivity(MELB14_SDMTable, type="global") #cluster coefficient
MELB14_SDM.degreeCent <- centralization.degree(MELB14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_SDMftn <- as.network.matrix(MELB14_SDMft)
MELB14_SDM.netDensity <- network.density(MELB14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_SDM.entropy <- entropy(MELB14_SDMft) #entropy

MELB14_SDM.netMx <- cbind(MELB14_SDM.netMx, MELB14_SDM.clusterCoef, MELB14_SDM.degreeCent$centralization,
                          MELB14_SDM.netDensity, MELB14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
MELB14_TDMg2 <- data.frame(MELB14_TDM)
MELB14_TDMg2 <- MELB14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_TDMg2$player1
player2vector <- MELB14_TDMg2$player2
MELB14_TDMg3 <- MELB14_TDMg2
MELB14_TDMg3$p1inp2vec <- is.element(MELB14_TDMg3$player1, player2vector)
MELB14_TDMg3$p2inp1vec <- is.element(MELB14_TDMg3$player2, player1vector)

addPlayer1 <- MELB14_TDMg3[ which(MELB14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_TDMg3[ which(MELB14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_TDMg2 <- rbind(MELB14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
MELB14_TDMft <- ftable(MELB14_TDMg2$player1, MELB14_TDMg2$player2)
MELB14_TDMft2 <- as.matrix(MELB14_TDMft)
numRows <- nrow(MELB14_TDMft2)
numCols <- ncol(MELB14_TDMft2)
MELB14_TDMft3 <- MELB14_TDMft2[c(2:numRows) , c(2:numCols)]
MELB14_TDMTable <- graph.adjacency(MELB14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(MELB14_TDMTable, vertex.label = V(MELB14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
MELB14_TDM.clusterCoef <- transitivity(MELB14_TDMTable, type="global") #cluster coefficient
MELB14_TDM.degreeCent <- centralization.degree(MELB14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_TDMftn <- as.network.matrix(MELB14_TDMft)
MELB14_TDM.netDensity <- network.density(MELB14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_TDM.entropy <- entropy(MELB14_TDMft) #entropy

MELB14_TDM.netMx <- cbind(MELB14_TDM.netMx, MELB14_TDM.clusterCoef, MELB14_TDM.degreeCent$centralization,
                          MELB14_TDM.netDensity, MELB14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
MELB14_SDg2 <- data.frame(MELB14_SD)
MELB14_SDg2 <- MELB14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_SDg2$player1
player2vector <- MELB14_SDg2$player2
MELB14_SDg3 <- MELB14_SDg2
MELB14_SDg3$p1inp2vec <- is.element(MELB14_SDg3$player1, player2vector)
MELB14_SDg3$p2inp1vec <- is.element(MELB14_SDg3$player2, player1vector)

addPlayer1 <- MELB14_SDg3[ which(MELB14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_SDg3[ which(MELB14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_SDg2 <- rbind(MELB14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
MELB14_SDft <- ftable(MELB14_SDg2$player1, MELB14_SDg2$player2)
MELB14_SDft2 <- as.matrix(MELB14_SDft)
numRows <- nrow(MELB14_SDft2)
numCols <- ncol(MELB14_SDft2)
MELB14_SDft3 <- MELB14_SDft2[c(2:numRows) , c(2:numCols)]
MELB14_SDTable <- graph.adjacency(MELB14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(MELB14_SDTable, vertex.label = V(MELB14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
MELB14_SD.clusterCoef <- transitivity(MELB14_SDTable, type="global") #cluster coefficient
MELB14_SD.degreeCent <- centralization.degree(MELB14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_SDftn <- as.network.matrix(MELB14_SDft)
MELB14_SD.netDensity <- network.density(MELB14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_SD.entropy <- entropy(MELB14_SDft) #entropy

MELB14_SD.netMx <- cbind(MELB14_SD.netMx, MELB14_SD.clusterCoef, MELB14_SD.degreeCent$centralization,
                         MELB14_SD.netDensity, MELB14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
MELB14_TDg2 <- data.frame(MELB14_TD)
MELB14_TDg2 <- MELB14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_TDg2$player1
player2vector <- MELB14_TDg2$player2
MELB14_TDg3 <- MELB14_TDg2
MELB14_TDg3$p1inp2vec <- is.element(MELB14_TDg3$player1, player2vector)
MELB14_TDg3$p2inp1vec <- is.element(MELB14_TDg3$player2, player1vector)

addPlayer1 <- MELB14_TDg3[ which(MELB14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_TDg3[ which(MELB14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_TDg2 <- rbind(MELB14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
MELB14_TDft <- ftable(MELB14_TDg2$player1, MELB14_TDg2$player2)
MELB14_TDft2 <- as.matrix(MELB14_TDft)
numRows <- nrow(MELB14_TDft2)
numCols <- ncol(MELB14_TDft2)
MELB14_TDft3 <- MELB14_TDft2[c(2:numRows) , c(2:numCols)]
MELB14_TDTable <- graph.adjacency(MELB14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(MELB14_TDTable, vertex.label = V(MELB14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
MELB14_TD.clusterCoef <- transitivity(MELB14_TDTable, type="global") #cluster coefficient
MELB14_TD.degreeCent <- centralization.degree(MELB14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_TDftn <- as.network.matrix(MELB14_TDft)
MELB14_TD.netDensity <- network.density(MELB14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_TD.entropy <- entropy(MELB14_TDft) #entropy

MELB14_TD.netMx <- cbind(MELB14_TD.netMx, MELB14_TD.clusterCoef, MELB14_TD.degreeCent$centralization,
                         MELB14_TD.netDensity, MELB14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************

round = 14
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
MELB14_QTg2 <- data.frame(MELB14_QT)
MELB14_QTg2 <- MELB14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB14_QTg2$player1
player2vector <- MELB14_QTg2$player2
MELB14_QTg3 <- MELB14_QTg2
MELB14_QTg3$p1inp2vec <- is.element(MELB14_QTg3$player1, player2vector)
MELB14_QTg3$p2inp1vec <- is.element(MELB14_QTg3$player2, player1vector)

addPlayer1 <- MELB14_QTg3[ which(MELB14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB14_QTg3[ which(MELB14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB14_QTg2 <- rbind(MELB14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
MELB14_QTft <- ftable(MELB14_QTg2$player1, MELB14_QTg2$player2)
MELB14_QTft2 <- as.matrix(MELB14_QTft)
numRows <- nrow(MELB14_QTft2)
numCols <- ncol(MELB14_QTft2)
MELB14_QTft3 <- MELB14_QTft2[c(2:numRows) , c(2:numCols)]
MELB14_QTTable <- graph.adjacency(MELB14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(MELB14_QTTable, vertex.label = V(MELB14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
MELB14_QT.clusterCoef <- transitivity(MELB14_QTTable, type="global") #cluster coefficient
MELB14_QT.degreeCent <- centralization.degree(MELB14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB14_QTftn <- as.network.matrix(MELB14_QTft)
MELB14_QT.netDensity <- network.density(MELB14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB14_QT.entropy <- entropy(MELB14_QTft) #entropy

MELB14_QT.netMx <- cbind(MELB14_QT.netMx, MELB14_QT.clusterCoef, MELB14_QT.degreeCent$centralization,
                         MELB14_QT.netDensity, MELB14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB14_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
NMFC14_Gg2 <- data.frame(NMFC14_G)
NMFC14_Gg2 <- NMFC14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_Gg2$player1
player2vector <- NMFC14_Gg2$player2
NMFC14_Gg3 <- NMFC14_Gg2
NMFC14_Gg3$p1inp2vec <- is.element(NMFC14_Gg3$player1, player2vector)
NMFC14_Gg3$p2inp1vec <- is.element(NMFC14_Gg3$player2, player1vector)

addPlayer1 <- NMFC14_Gg3[ which(NMFC14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_Gg3[ which(NMFC14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_Gg2 <- rbind(NMFC14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
NMFC14_Gft <- ftable(NMFC14_Gg2$player1, NMFC14_Gg2$player2)
NMFC14_Gft2 <- as.matrix(NMFC14_Gft)
numRows <- nrow(NMFC14_Gft2)
numCols <- ncol(NMFC14_Gft2)
NMFC14_Gft3 <- NMFC14_Gft2[c(2:numRows) , c(2:numCols)]
NMFC14_GTable <- graph.adjacency(NMFC14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(NMFC14_GTable, vertex.label = V(NMFC14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
NMFC14_G.clusterCoef <- transitivity(NMFC14_GTable, type="global") #cluster coefficient
NMFC14_G.degreeCent <- centralization.degree(NMFC14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_Gftn <- as.network.matrix(NMFC14_Gft)
NMFC14_G.netDensity <- network.density(NMFC14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_G.entropy <- entropy(NMFC14_Gft) #entropy

NMFC14_G.netMx <- cbind(NMFC14_G.netMx, NMFC14_G.clusterCoef, NMFC14_G.degreeCent$centralization,
                        NMFC14_G.netDensity, NMFC14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
NMFC14_Bg2 <- data.frame(NMFC14_B)
NMFC14_Bg2 <- NMFC14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_Bg2$player1
player2vector <- NMFC14_Bg2$player2
NMFC14_Bg3 <- NMFC14_Bg2
NMFC14_Bg3$p1inp2vec <- is.element(NMFC14_Bg3$player1, player2vector)
NMFC14_Bg3$p2inp1vec <- is.element(NMFC14_Bg3$player2, player1vector)

addPlayer1 <- NMFC14_Bg3[ which(NMFC14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_Bg3[ which(NMFC14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_Bg2 <- rbind(NMFC14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
NMFC14_Bft <- ftable(NMFC14_Bg2$player1, NMFC14_Bg2$player2)
NMFC14_Bft2 <- as.matrix(NMFC14_Bft)
numRows <- nrow(NMFC14_Bft2)
numCols <- ncol(NMFC14_Bft2)
NMFC14_Bft3 <- NMFC14_Bft2[c(2:numRows) , c(2:numCols)]
NMFC14_BTable <- graph.adjacency(NMFC14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(NMFC14_BTable, vertex.label = V(NMFC14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
NMFC14_B.clusterCoef <- transitivity(NMFC14_BTable, type="global") #cluster coefficient
NMFC14_B.degreeCent <- centralization.degree(NMFC14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_Bftn <- as.network.matrix(NMFC14_Bft)
NMFC14_B.netDensity <- network.density(NMFC14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_B.entropy <- entropy(NMFC14_Bft) #entropy

NMFC14_B.netMx <- cbind(NMFC14_B.netMx, NMFC14_B.clusterCoef, NMFC14_B.degreeCent$centralization,
                        NMFC14_B.netDensity, NMFC14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
NMFC14_SFg2 <- data.frame(NMFC14_SF)
NMFC14_SFg2 <- NMFC14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_SFg2$player1
player2vector <- NMFC14_SFg2$player2
NMFC14_SFg3 <- NMFC14_SFg2
NMFC14_SFg3$p1inp2vec <- is.element(NMFC14_SFg3$player1, player2vector)
NMFC14_SFg3$p2inp1vec <- is.element(NMFC14_SFg3$player2, player1vector)

addPlayer1 <- NMFC14_SFg3[ which(NMFC14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_SFg3[ which(NMFC14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_SFg2 <- rbind(NMFC14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
NMFC14_SFft <- ftable(NMFC14_SFg2$player1, NMFC14_SFg2$player2)
NMFC14_SFft2 <- as.matrix(NMFC14_SFft)
numRows <- nrow(NMFC14_SFft2)
numCols <- ncol(NMFC14_SFft2)
NMFC14_SFft3 <- NMFC14_SFft2[c(2:numRows) , c(2:numCols)]
NMFC14_SFTable <- graph.adjacency(NMFC14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(NMFC14_SFTable, vertex.label = V(NMFC14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
NMFC14_SF.clusterCoef <- transitivity(NMFC14_SFTable, type="global") #cluster coefficient
NMFC14_SF.degreeCent <- centralization.degree(NMFC14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_SFftn <- as.network.matrix(NMFC14_SFft)
NMFC14_SF.netDensity <- network.density(NMFC14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_SF.entropy <- entropy(NMFC14_SFft) #entropy

NMFC14_SF.netMx <- cbind(NMFC14_SF.netMx, NMFC14_SF.clusterCoef, NMFC14_SF.degreeCent$centralization,
                         NMFC14_SF.netDensity, NMFC14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
NMFC14_TFg2 <- data.frame(NMFC14_TF)
NMFC14_TFg2 <- NMFC14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_TFg2$player1
player2vector <- NMFC14_TFg2$player2
NMFC14_TFg3 <- NMFC14_TFg2
NMFC14_TFg3$p1inp2vec <- is.element(NMFC14_TFg3$player1, player2vector)
NMFC14_TFg3$p2inp1vec <- is.element(NMFC14_TFg3$player2, player1vector)

addPlayer1 <- NMFC14_TFg3[ which(NMFC14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_TFg3[ which(NMFC14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_TFg2 <- rbind(NMFC14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
NMFC14_TFft <- ftable(NMFC14_TFg2$player1, NMFC14_TFg2$player2)
NMFC14_TFft2 <- as.matrix(NMFC14_TFft)
numRows <- nrow(NMFC14_TFft2)
numCols <- ncol(NMFC14_TFft2)
NMFC14_TFft3 <- NMFC14_TFft2[c(2:numRows) , c(2:numCols)]
NMFC14_TFTable <- graph.adjacency(NMFC14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(NMFC14_TFTable, vertex.label = V(NMFC14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
NMFC14_TF.clusterCoef <- transitivity(NMFC14_TFTable, type="global") #cluster coefficient
NMFC14_TF.degreeCent <- centralization.degree(NMFC14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_TFftn <- as.network.matrix(NMFC14_TFft)
NMFC14_TF.netDensity <- network.density(NMFC14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_TF.entropy <- entropy(NMFC14_TFft) #entropy

NMFC14_TF.netMx <- cbind(NMFC14_TF.netMx, NMFC14_TF.clusterCoef, NMFC14_TF.degreeCent$centralization,
                         NMFC14_TF.netDensity, NMFC14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
NMFC14_SAMg2 <- data.frame(NMFC14_SAM)
NMFC14_SAMg2 <- NMFC14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_SAMg2$player1
player2vector <- NMFC14_SAMg2$player2
NMFC14_SAMg3 <- NMFC14_SAMg2
NMFC14_SAMg3$p1inp2vec <- is.element(NMFC14_SAMg3$player1, player2vector)
NMFC14_SAMg3$p2inp1vec <- is.element(NMFC14_SAMg3$player2, player1vector)

addPlayer1 <- NMFC14_SAMg3[ which(NMFC14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_SAMg3[ which(NMFC14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_SAMg2 <- rbind(NMFC14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
NMFC14_SAMft <- ftable(NMFC14_SAMg2$player1, NMFC14_SAMg2$player2)
NMFC14_SAMft2 <- as.matrix(NMFC14_SAMft)
numRows <- nrow(NMFC14_SAMft2)
numCols <- ncol(NMFC14_SAMft2)
NMFC14_SAMft3 <- NMFC14_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC14_SAMTable <- graph.adjacency(NMFC14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(NMFC14_SAMTable, vertex.label = V(NMFC14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
NMFC14_SAM.clusterCoef <- transitivity(NMFC14_SAMTable, type="global") #cluster coefficient
NMFC14_SAM.degreeCent <- centralization.degree(NMFC14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_SAMftn <- as.network.matrix(NMFC14_SAMft)
NMFC14_SAM.netDensity <- network.density(NMFC14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_SAM.entropy <- entropy(NMFC14_SAMft) #entropy

NMFC14_SAM.netMx <- cbind(NMFC14_SAM.netMx, NMFC14_SAM.clusterCoef, NMFC14_SAM.degreeCent$centralization,
                          NMFC14_SAM.netDensity, NMFC14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
NMFC14_TAMg2 <- data.frame(NMFC14_TAM)
NMFC14_TAMg2 <- NMFC14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_TAMg2$player1
player2vector <- NMFC14_TAMg2$player2
NMFC14_TAMg3 <- NMFC14_TAMg2
NMFC14_TAMg3$p1inp2vec <- is.element(NMFC14_TAMg3$player1, player2vector)
NMFC14_TAMg3$p2inp1vec <- is.element(NMFC14_TAMg3$player2, player1vector)

addPlayer1 <- NMFC14_TAMg3[ which(NMFC14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_TAMg3[ which(NMFC14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_TAMg2 <- rbind(NMFC14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
NMFC14_TAMft <- ftable(NMFC14_TAMg2$player1, NMFC14_TAMg2$player2)
NMFC14_TAMft2 <- as.matrix(NMFC14_TAMft)
numRows <- nrow(NMFC14_TAMft2)
numCols <- ncol(NMFC14_TAMft2)
NMFC14_TAMft3 <- NMFC14_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC14_TAMTable <- graph.adjacency(NMFC14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(NMFC14_TAMTable, vertex.label = V(NMFC14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
NMFC14_TAM.clusterCoef <- transitivity(NMFC14_TAMTable, type="global") #cluster coefficient
NMFC14_TAM.degreeCent <- centralization.degree(NMFC14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_TAMftn <- as.network.matrix(NMFC14_TAMft)
NMFC14_TAM.netDensity <- network.density(NMFC14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_TAM.entropy <- entropy(NMFC14_TAMft) #entropy

NMFC14_TAM.netMx <- cbind(NMFC14_TAM.netMx, NMFC14_TAM.clusterCoef, NMFC14_TAM.degreeCent$centralization,
                          NMFC14_TAM.netDensity, NMFC14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
NMFC14_SDMg2 <- data.frame(NMFC14_SDM)
NMFC14_SDMg2 <- NMFC14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_SDMg2$player1
player2vector <- NMFC14_SDMg2$player2
NMFC14_SDMg3 <- NMFC14_SDMg2
NMFC14_SDMg3$p1inp2vec <- is.element(NMFC14_SDMg3$player1, player2vector)
NMFC14_SDMg3$p2inp1vec <- is.element(NMFC14_SDMg3$player2, player1vector)

addPlayer1 <- NMFC14_SDMg3[ which(NMFC14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_SDMg3[ which(NMFC14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_SDMg2 <- rbind(NMFC14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
NMFC14_SDMft <- ftable(NMFC14_SDMg2$player1, NMFC14_SDMg2$player2)
NMFC14_SDMft2 <- as.matrix(NMFC14_SDMft)
numRows <- nrow(NMFC14_SDMft2)
numCols <- ncol(NMFC14_SDMft2)
NMFC14_SDMft3 <- NMFC14_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC14_SDMTable <- graph.adjacency(NMFC14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(NMFC14_SDMTable, vertex.label = V(NMFC14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
NMFC14_SDM.clusterCoef <- transitivity(NMFC14_SDMTable, type="global") #cluster coefficient
NMFC14_SDM.degreeCent <- centralization.degree(NMFC14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_SDMftn <- as.network.matrix(NMFC14_SDMft)
NMFC14_SDM.netDensity <- network.density(NMFC14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_SDM.entropy <- entropy(NMFC14_SDMft) #entropy

NMFC14_SDM.netMx <- cbind(NMFC14_SDM.netMx, NMFC14_SDM.clusterCoef, NMFC14_SDM.degreeCent$centralization,
                          NMFC14_SDM.netDensity, NMFC14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
NMFC14_TDMg2 <- data.frame(NMFC14_TDM)
NMFC14_TDMg2 <- NMFC14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_TDMg2$player1
player2vector <- NMFC14_TDMg2$player2
NMFC14_TDMg3 <- NMFC14_TDMg2
NMFC14_TDMg3$p1inp2vec <- is.element(NMFC14_TDMg3$player1, player2vector)
NMFC14_TDMg3$p2inp1vec <- is.element(NMFC14_TDMg3$player2, player1vector)

addPlayer1 <- NMFC14_TDMg3[ which(NMFC14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_TDMg3[ which(NMFC14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_TDMg2 <- rbind(NMFC14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
NMFC14_TDMft <- ftable(NMFC14_TDMg2$player1, NMFC14_TDMg2$player2)
NMFC14_TDMft2 <- as.matrix(NMFC14_TDMft)
numRows <- nrow(NMFC14_TDMft2)
numCols <- ncol(NMFC14_TDMft2)
NMFC14_TDMft3 <- NMFC14_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC14_TDMTable <- graph.adjacency(NMFC14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(NMFC14_TDMTable, vertex.label = V(NMFC14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
NMFC14_TDM.clusterCoef <- transitivity(NMFC14_TDMTable, type="global") #cluster coefficient
NMFC14_TDM.degreeCent <- centralization.degree(NMFC14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_TDMftn <- as.network.matrix(NMFC14_TDMft)
NMFC14_TDM.netDensity <- network.density(NMFC14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_TDM.entropy <- entropy(NMFC14_TDMft) #entropy

NMFC14_TDM.netMx <- cbind(NMFC14_TDM.netMx, NMFC14_TDM.clusterCoef, NMFC14_TDM.degreeCent$centralization,
                          NMFC14_TDM.netDensity, NMFC14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
NMFC14_SDg2 <- data.frame(NMFC14_SD)
NMFC14_SDg2 <- NMFC14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_SDg2$player1
player2vector <- NMFC14_SDg2$player2
NMFC14_SDg3 <- NMFC14_SDg2
NMFC14_SDg3$p1inp2vec <- is.element(NMFC14_SDg3$player1, player2vector)
NMFC14_SDg3$p2inp1vec <- is.element(NMFC14_SDg3$player2, player1vector)

addPlayer1 <- NMFC14_SDg3[ which(NMFC14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_SDg3[ which(NMFC14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_SDg2 <- rbind(NMFC14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
NMFC14_SDft <- ftable(NMFC14_SDg2$player1, NMFC14_SDg2$player2)
NMFC14_SDft2 <- as.matrix(NMFC14_SDft)
numRows <- nrow(NMFC14_SDft2)
numCols <- ncol(NMFC14_SDft2)
NMFC14_SDft3 <- NMFC14_SDft2[c(2:numRows) , c(2:numCols)]
NMFC14_SDTable <- graph.adjacency(NMFC14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(NMFC14_SDTable, vertex.label = V(NMFC14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
NMFC14_SD.clusterCoef <- transitivity(NMFC14_SDTable, type="global") #cluster coefficient
NMFC14_SD.degreeCent <- centralization.degree(NMFC14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_SDftn <- as.network.matrix(NMFC14_SDft)
NMFC14_SD.netDensity <- network.density(NMFC14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_SD.entropy <- entropy(NMFC14_SDft) #entropy

NMFC14_SD.netMx <- cbind(NMFC14_SD.netMx, NMFC14_SD.clusterCoef, NMFC14_SD.degreeCent$centralization,
                         NMFC14_SD.netDensity, NMFC14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************

round = 14
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
NMFC14_TDg2 <- data.frame(NMFC14_TD)
NMFC14_TDg2 <- NMFC14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_TDg2$player1
player2vector <- NMFC14_TDg2$player2
NMFC14_TDg3 <- NMFC14_TDg2
NMFC14_TDg3$p1inp2vec <- is.element(NMFC14_TDg3$player1, player2vector)
NMFC14_TDg3$p2inp1vec <- is.element(NMFC14_TDg3$player2, player1vector)

addPlayer1 <- NMFC14_TDg3[ which(NMFC14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_TDg3[ which(NMFC14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_TDg2 <- rbind(NMFC14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
NMFC14_TDft <- ftable(NMFC14_TDg2$player1, NMFC14_TDg2$player2)
NMFC14_TDft2 <- as.matrix(NMFC14_TDft)
numRows <- nrow(NMFC14_TDft2)
numCols <- ncol(NMFC14_TDft2)
NMFC14_TDft3 <- NMFC14_TDft2[c(2:numRows) , c(2:numCols)]
NMFC14_TDTable <- graph.adjacency(NMFC14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(NMFC14_TDTable, vertex.label = V(NMFC14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
NMFC14_TD.clusterCoef <- transitivity(NMFC14_TDTable, type="global") #cluster coefficient
NMFC14_TD.degreeCent <- centralization.degree(NMFC14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_TDftn <- as.network.matrix(NMFC14_TDft)
NMFC14_TD.netDensity <- network.density(NMFC14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_TD.entropy <- entropy(NMFC14_TDft) #entropy

NMFC14_TD.netMx <- cbind(NMFC14_TD.netMx, NMFC14_TD.clusterCoef, NMFC14_TD.degreeCent$centralization,
                         NMFC14_TD.netDensity, NMFC14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
NMFC14_QTg2 <- data.frame(NMFC14_QT)
NMFC14_QTg2 <- NMFC14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC14_QTg2$player1
player2vector <- NMFC14_QTg2$player2
NMFC14_QTg3 <- NMFC14_QTg2
NMFC14_QTg3$p1inp2vec <- is.element(NMFC14_QTg3$player1, player2vector)
NMFC14_QTg3$p2inp1vec <- is.element(NMFC14_QTg3$player2, player1vector)

addPlayer1 <- NMFC14_QTg3[ which(NMFC14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC14_QTg3[ which(NMFC14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC14_QTg2 <- rbind(NMFC14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
NMFC14_QTft <- ftable(NMFC14_QTg2$player1, NMFC14_QTg2$player2)
NMFC14_QTft2 <- as.matrix(NMFC14_QTft)
numRows <- nrow(NMFC14_QTft2)
numCols <- ncol(NMFC14_QTft2)
NMFC14_QTft3 <- NMFC14_QTft2[c(2:numRows) , c(2:numCols)]
NMFC14_QTTable <- graph.adjacency(NMFC14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(NMFC14_QTTable, vertex.label = V(NMFC14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
NMFC14_QT.clusterCoef <- transitivity(NMFC14_QTTable, type="global") #cluster coefficient
NMFC14_QT.degreeCent <- centralization.degree(NMFC14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC14_QTftn <- as.network.matrix(NMFC14_QTft)
NMFC14_QT.netDensity <- network.density(NMFC14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC14_QT.entropy <- entropy(NMFC14_QTft) #entropy

NMFC14_QT.netMx <- cbind(NMFC14_QT.netMx, NMFC14_QT.clusterCoef, NMFC14_QT.degreeCent$centralization,
                         NMFC14_QT.netDensity, NMFC14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC14_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "PORT"
KIoutcome = "Goal_F"
PORT14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
PORT14_Gg2 <- data.frame(PORT14_G)
PORT14_Gg2 <- PORT14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_Gg2$player1
player2vector <- PORT14_Gg2$player2
PORT14_Gg3 <- PORT14_Gg2
PORT14_Gg3$p1inp2vec <- is.element(PORT14_Gg3$player1, player2vector)
PORT14_Gg3$p2inp1vec <- is.element(PORT14_Gg3$player2, player1vector)

addPlayer1 <- PORT14_Gg3[ which(PORT14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_Gg3[ which(PORT14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_Gg2 <- rbind(PORT14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
PORT14_Gft <- ftable(PORT14_Gg2$player1, PORT14_Gg2$player2)
PORT14_Gft2 <- as.matrix(PORT14_Gft)
numRows <- nrow(PORT14_Gft2)
numCols <- ncol(PORT14_Gft2)
PORT14_Gft3 <- PORT14_Gft2[c(2:numRows) , c(2:numCols)]
PORT14_GTable <- graph.adjacency(PORT14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(PORT14_GTable, vertex.label = V(PORT14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
PORT14_G.clusterCoef <- transitivity(PORT14_GTable, type="global") #cluster coefficient
PORT14_G.degreeCent <- centralization.degree(PORT14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_Gftn <- as.network.matrix(PORT14_Gft)
PORT14_G.netDensity <- network.density(PORT14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_G.entropy <- entropy(PORT14_Gft) #entropy

PORT14_G.netMx <- cbind(PORT14_G.netMx, PORT14_G.clusterCoef, PORT14_G.degreeCent$centralization,
                        PORT14_G.netDensity, PORT14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "PORT"
KIoutcome = "Behind_F"
PORT14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
PORT14_Bg2 <- data.frame(PORT14_B)
PORT14_Bg2 <- PORT14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_Bg2$player1
player2vector <- PORT14_Bg2$player2
PORT14_Bg3 <- PORT14_Bg2
PORT14_Bg3$p1inp2vec <- is.element(PORT14_Bg3$player1, player2vector)
PORT14_Bg3$p2inp1vec <- is.element(PORT14_Bg3$player2, player1vector)

addPlayer1 <- PORT14_Bg3[ which(PORT14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_Bg3[ which(PORT14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_Bg2 <- rbind(PORT14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
PORT14_Bft <- ftable(PORT14_Bg2$player1, PORT14_Bg2$player2)
PORT14_Bft2 <- as.matrix(PORT14_Bft)
numRows <- nrow(PORT14_Bft2)
numCols <- ncol(PORT14_Bft2)
PORT14_Bft3 <- PORT14_Bft2[c(2:numRows) , c(2:numCols)]
PORT14_BTable <- graph.adjacency(PORT14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(PORT14_BTable, vertex.label = V(PORT14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
PORT14_B.clusterCoef <- transitivity(PORT14_BTable, type="global") #cluster coefficient
PORT14_B.degreeCent <- centralization.degree(PORT14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_Bftn <- as.network.matrix(PORT14_Bft)
PORT14_B.netDensity <- network.density(PORT14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_B.entropy <- entropy(PORT14_Bft) #entropy

PORT14_B.netMx <- cbind(PORT14_B.netMx, PORT14_B.clusterCoef, PORT14_B.degreeCent$centralization,
                        PORT14_B.netDensity, PORT14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************

round = 14
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
PORT14_SFg2 <- data.frame(PORT14_SF)
PORT14_SFg2 <- PORT14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_SFg2$player1
player2vector <- PORT14_SFg2$player2
PORT14_SFg3 <- PORT14_SFg2
PORT14_SFg3$p1inp2vec <- is.element(PORT14_SFg3$player1, player2vector)
PORT14_SFg3$p2inp1vec <- is.element(PORT14_SFg3$player2, player1vector)

addPlayer1 <- PORT14_SFg3[ which(PORT14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_SFg3[ which(PORT14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_SFg2 <- rbind(PORT14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
PORT14_SFft <- ftable(PORT14_SFg2$player1, PORT14_SFg2$player2)
PORT14_SFft2 <- as.matrix(PORT14_SFft)
numRows <- nrow(PORT14_SFft2)
numCols <- ncol(PORT14_SFft2)
PORT14_SFft3 <- PORT14_SFft2[c(2:numRows) , c(2:numCols)]
PORT14_SFTable <- graph.adjacency(PORT14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(PORT14_SFTable, vertex.label = V(PORT14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
PORT14_SF.clusterCoef <- transitivity(PORT14_SFTable, type="global") #cluster coefficient
PORT14_SF.degreeCent <- centralization.degree(PORT14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_SFftn <- as.network.matrix(PORT14_SFft)
PORT14_SF.netDensity <- network.density(PORT14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_SF.entropy <- entropy(PORT14_SFft) #entropy

PORT14_SF.netMx <- cbind(PORT14_SF.netMx, PORT14_SF.clusterCoef, PORT14_SF.degreeCent$centralization,
                         PORT14_SF.netDensity, PORT14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
PORT14_TFg2 <- data.frame(PORT14_TF)
PORT14_TFg2 <- PORT14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_TFg2$player1
player2vector <- PORT14_TFg2$player2
PORT14_TFg3 <- PORT14_TFg2
PORT14_TFg3$p1inp2vec <- is.element(PORT14_TFg3$player1, player2vector)
PORT14_TFg3$p2inp1vec <- is.element(PORT14_TFg3$player2, player1vector)

addPlayer1 <- PORT14_TFg3[ which(PORT14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT14_TFg2 <- rbind(PORT14_TFg2, addPlayer1)

#ROUND 14, FWD Turnover graph using weighted edges
PORT14_TFft <- ftable(PORT14_TFg2$player1, PORT14_TFg2$player2)
PORT14_TFft2 <- as.matrix(PORT14_TFft)
numRows <- nrow(PORT14_TFft2)
numCols <- ncol(PORT14_TFft2)
PORT14_TFft3 <- PORT14_TFft2[c(2:numRows) , c(1:numCols)]
PORT14_TFTable <- graph.adjacency(PORT14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(PORT14_TFTable, vertex.label = V(PORT14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
PORT14_TF.clusterCoef <- transitivity(PORT14_TFTable, type="global") #cluster coefficient
PORT14_TF.degreeCent <- centralization.degree(PORT14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_TFftn <- as.network.matrix(PORT14_TFft)
PORT14_TF.netDensity <- network.density(PORT14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_TF.entropy <- entropy(PORT14_TFft) #entropy

PORT14_TF.netMx <- cbind(PORT14_TF.netMx, PORT14_TF.clusterCoef, PORT14_TF.degreeCent$centralization,
                         PORT14_TF.netDensity, PORT14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
PORT14_SAMg2 <- data.frame(PORT14_SAM)
PORT14_SAMg2 <- PORT14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_SAMg2$player1
player2vector <- PORT14_SAMg2$player2
PORT14_SAMg3 <- PORT14_SAMg2
PORT14_SAMg3$p1inp2vec <- is.element(PORT14_SAMg3$player1, player2vector)
PORT14_SAMg3$p2inp1vec <- is.element(PORT14_SAMg3$player2, player1vector)

addPlayer1 <- PORT14_SAMg3[ which(PORT14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_SAMg3[ which(PORT14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_SAMg2 <- rbind(PORT14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
PORT14_SAMft <- ftable(PORT14_SAMg2$player1, PORT14_SAMg2$player2)
PORT14_SAMft2 <- as.matrix(PORT14_SAMft)
numRows <- nrow(PORT14_SAMft2)
numCols <- ncol(PORT14_SAMft2)
PORT14_SAMft3 <- PORT14_SAMft2[c(2:numRows) , c(2:numCols)]
PORT14_SAMTable <- graph.adjacency(PORT14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(PORT14_SAMTable, vertex.label = V(PORT14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
PORT14_SAM.clusterCoef <- transitivity(PORT14_SAMTable, type="global") #cluster coefficient
PORT14_SAM.degreeCent <- centralization.degree(PORT14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_SAMftn <- as.network.matrix(PORT14_SAMft)
PORT14_SAM.netDensity <- network.density(PORT14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_SAM.entropy <- entropy(PORT14_SAMft) #entropy

PORT14_SAM.netMx <- cbind(PORT14_SAM.netMx, PORT14_SAM.clusterCoef, PORT14_SAM.degreeCent$centralization,
                          PORT14_SAM.netDensity, PORT14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
PORT14_TAMg2 <- data.frame(PORT14_TAM)
PORT14_TAMg2 <- PORT14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_TAMg2$player1
player2vector <- PORT14_TAMg2$player2
PORT14_TAMg3 <- PORT14_TAMg2
PORT14_TAMg3$p1inp2vec <- is.element(PORT14_TAMg3$player1, player2vector)
PORT14_TAMg3$p2inp1vec <- is.element(PORT14_TAMg3$player2, player1vector)

addPlayer1 <- PORT14_TAMg3[ which(PORT14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_TAMg3[ which(PORT14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_TAMg2 <- rbind(PORT14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
PORT14_TAMft <- ftable(PORT14_TAMg2$player1, PORT14_TAMg2$player2)
PORT14_TAMft2 <- as.matrix(PORT14_TAMft)
numRows <- nrow(PORT14_TAMft2)
numCols <- ncol(PORT14_TAMft2)
PORT14_TAMft3 <- PORT14_TAMft2[c(2:numRows) , c(2:numCols)]
PORT14_TAMTable <- graph.adjacency(PORT14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(PORT14_TAMTable, vertex.label = V(PORT14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
PORT14_TAM.clusterCoef <- transitivity(PORT14_TAMTable, type="global") #cluster coefficient
PORT14_TAM.degreeCent <- centralization.degree(PORT14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_TAMftn <- as.network.matrix(PORT14_TAMft)
PORT14_TAM.netDensity <- network.density(PORT14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_TAM.entropy <- entropy(PORT14_TAMft) #entropy

PORT14_TAM.netMx <- cbind(PORT14_TAM.netMx, PORT14_TAM.clusterCoef, PORT14_TAM.degreeCent$centralization,
                          PORT14_TAM.netDensity, PORT14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
PORT14_SDMg2 <- data.frame(PORT14_SDM)
PORT14_SDMg2 <- PORT14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_SDMg2$player1
player2vector <- PORT14_SDMg2$player2
PORT14_SDMg3 <- PORT14_SDMg2
PORT14_SDMg3$p1inp2vec <- is.element(PORT14_SDMg3$player1, player2vector)
PORT14_SDMg3$p2inp1vec <- is.element(PORT14_SDMg3$player2, player1vector)

addPlayer1 <- PORT14_SDMg3[ which(PORT14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_SDMg3[ which(PORT14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_SDMg2 <- rbind(PORT14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
PORT14_SDMft <- ftable(PORT14_SDMg2$player1, PORT14_SDMg2$player2)
PORT14_SDMft2 <- as.matrix(PORT14_SDMft)
numRows <- nrow(PORT14_SDMft2)
numCols <- ncol(PORT14_SDMft2)
PORT14_SDMft3 <- PORT14_SDMft2[c(2:numRows) , c(2:numCols)]
PORT14_SDMTable <- graph.adjacency(PORT14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(PORT14_SDMTable, vertex.label = V(PORT14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
PORT14_SDM.clusterCoef <- transitivity(PORT14_SDMTable, type="global") #cluster coefficient
PORT14_SDM.degreeCent <- centralization.degree(PORT14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_SDMftn <- as.network.matrix(PORT14_SDMft)
PORT14_SDM.netDensity <- network.density(PORT14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_SDM.entropy <- entropy(PORT14_SDMft) #entropy

PORT14_SDM.netMx <- cbind(PORT14_SDM.netMx, PORT14_SDM.clusterCoef, PORT14_SDM.degreeCent$centralization,
                          PORT14_SDM.netDensity, PORT14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
PORT14_TDMg2 <- data.frame(PORT14_TDM)
PORT14_TDMg2 <- PORT14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_TDMg2$player1
player2vector <- PORT14_TDMg2$player2
PORT14_TDMg3 <- PORT14_TDMg2
PORT14_TDMg3$p1inp2vec <- is.element(PORT14_TDMg3$player1, player2vector)
PORT14_TDMg3$p2inp1vec <- is.element(PORT14_TDMg3$player2, player1vector)

addPlayer1 <- PORT14_TDMg3[ which(PORT14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_TDMg3[ which(PORT14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_TDMg2 <- rbind(PORT14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
PORT14_TDMft <- ftable(PORT14_TDMg2$player1, PORT14_TDMg2$player2)
PORT14_TDMft2 <- as.matrix(PORT14_TDMft)
numRows <- nrow(PORT14_TDMft2)
numCols <- ncol(PORT14_TDMft2)
PORT14_TDMft3 <- PORT14_TDMft2[c(2:numRows) , c(2:numCols)]
PORT14_TDMTable <- graph.adjacency(PORT14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(PORT14_TDMTable, vertex.label = V(PORT14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
PORT14_TDM.clusterCoef <- transitivity(PORT14_TDMTable, type="global") #cluster coefficient
PORT14_TDM.degreeCent <- centralization.degree(PORT14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_TDMftn <- as.network.matrix(PORT14_TDMft)
PORT14_TDM.netDensity <- network.density(PORT14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_TDM.entropy <- entropy(PORT14_TDMft) #entropy

PORT14_TDM.netMx <- cbind(PORT14_TDM.netMx, PORT14_TDM.clusterCoef, PORT14_TDM.degreeCent$centralization,
                          PORT14_TDM.netDensity, PORT14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
PORT14_SDg2 <- data.frame(PORT14_SD)
PORT14_SDg2 <- PORT14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_SDg2$player1
player2vector <- PORT14_SDg2$player2
PORT14_SDg3 <- PORT14_SDg2
PORT14_SDg3$p1inp2vec <- is.element(PORT14_SDg3$player1, player2vector)
PORT14_SDg3$p2inp1vec <- is.element(PORT14_SDg3$player2, player1vector)

addPlayer1 <- PORT14_SDg3[ which(PORT14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_SDg3[ which(PORT14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_SDg2 <- rbind(PORT14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
PORT14_SDft <- ftable(PORT14_SDg2$player1, PORT14_SDg2$player2)
PORT14_SDft2 <- as.matrix(PORT14_SDft)
numRows <- nrow(PORT14_SDft2)
numCols <- ncol(PORT14_SDft2)
PORT14_SDft3 <- PORT14_SDft2[c(2:numRows) , c(2:numCols)]
PORT14_SDTable <- graph.adjacency(PORT14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(PORT14_SDTable, vertex.label = V(PORT14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
PORT14_SD.clusterCoef <- transitivity(PORT14_SDTable, type="global") #cluster coefficient
PORT14_SD.degreeCent <- centralization.degree(PORT14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_SDftn <- as.network.matrix(PORT14_SDft)
PORT14_SD.netDensity <- network.density(PORT14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_SD.entropy <- entropy(PORT14_SDft) #entropy

PORT14_SD.netMx <- cbind(PORT14_SD.netMx, PORT14_SD.clusterCoef, PORT14_SD.degreeCent$centralization,
                         PORT14_SD.netDensity, PORT14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
PORT14_TDg2 <- data.frame(PORT14_TD)
PORT14_TDg2 <- PORT14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_TDg2$player1
player2vector <- PORT14_TDg2$player2
PORT14_TDg3 <- PORT14_TDg2
PORT14_TDg3$p1inp2vec <- is.element(PORT14_TDg3$player1, player2vector)
PORT14_TDg3$p2inp1vec <- is.element(PORT14_TDg3$player2, player1vector)

addPlayer1 <- PORT14_TDg3[ which(PORT14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_TDg3[ which(PORT14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_TDg2 <- rbind(PORT14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
PORT14_TDft <- ftable(PORT14_TDg2$player1, PORT14_TDg2$player2)
PORT14_TDft2 <- as.matrix(PORT14_TDft)
numRows <- nrow(PORT14_TDft2)
numCols <- ncol(PORT14_TDft2)
PORT14_TDft3 <- PORT14_TDft2[c(2:numRows) , c(2:numCols)]
PORT14_TDTable <- graph.adjacency(PORT14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(PORT14_TDTable, vertex.label = V(PORT14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
PORT14_TD.clusterCoef <- transitivity(PORT14_TDTable, type="global") #cluster coefficient
PORT14_TD.degreeCent <- centralization.degree(PORT14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_TDftn <- as.network.matrix(PORT14_TDft)
PORT14_TD.netDensity <- network.density(PORT14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_TD.entropy <- entropy(PORT14_TDft) #entropy

PORT14_TD.netMx <- cbind(PORT14_TD.netMx, PORT14_TD.clusterCoef, PORT14_TD.degreeCent$centralization,
                         PORT14_TD.netDensity, PORT14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
PORT14_QTg2 <- data.frame(PORT14_QT)
PORT14_QTg2 <- PORT14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT14_QTg2$player1
player2vector <- PORT14_QTg2$player2
PORT14_QTg3 <- PORT14_QTg2
PORT14_QTg3$p1inp2vec <- is.element(PORT14_QTg3$player1, player2vector)
PORT14_QTg3$p2inp1vec <- is.element(PORT14_QTg3$player2, player1vector)

addPlayer1 <- PORT14_QTg3[ which(PORT14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT14_QTg3[ which(PORT14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT14_QTg2 <- rbind(PORT14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
PORT14_QTft <- ftable(PORT14_QTg2$player1, PORT14_QTg2$player2)
PORT14_QTft2 <- as.matrix(PORT14_QTft)
numRows <- nrow(PORT14_QTft2)
numCols <- ncol(PORT14_QTft2)
PORT14_QTft3 <- PORT14_QTft2[c(2:numRows) , c(2:numCols)]
PORT14_QTTable <- graph.adjacency(PORT14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(PORT14_QTTable, vertex.label = V(PORT14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
PORT14_QT.clusterCoef <- transitivity(PORT14_QTTable, type="global") #cluster coefficient
PORT14_QT.degreeCent <- centralization.degree(PORT14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT14_QTftn <- as.network.matrix(PORT14_QTft)
PORT14_QT.netDensity <- network.density(PORT14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT14_QT.entropy <- entropy(PORT14_QTft) #entropy

PORT14_QT.netMx <- cbind(PORT14_QT.netMx, PORT14_QT.clusterCoef, PORT14_QT.degreeCent$centralization,
                         PORT14_QT.netDensity, PORT14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT14_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "RICH"
KIoutcome = "Goal_F"
RICH14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
RICH14_Gg2 <- data.frame(RICH14_G)
RICH14_Gg2 <- RICH14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_Gg2$player1
player2vector <- RICH14_Gg2$player2
RICH14_Gg3 <- RICH14_Gg2
RICH14_Gg3$p1inp2vec <- is.element(RICH14_Gg3$player1, player2vector)
RICH14_Gg3$p2inp1vec <- is.element(RICH14_Gg3$player2, player1vector)

addPlayer1 <- RICH14_Gg3[ which(RICH14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_Gg3[ which(RICH14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_Gg2 <- rbind(RICH14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
RICH14_Gft <- ftable(RICH14_Gg2$player1, RICH14_Gg2$player2)
RICH14_Gft2 <- as.matrix(RICH14_Gft)
numRows <- nrow(RICH14_Gft2)
numCols <- ncol(RICH14_Gft2)
RICH14_Gft3 <- RICH14_Gft2[c(2:numRows) , c(2:numCols)]
RICH14_GTable <- graph.adjacency(RICH14_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(RICH14_GTable, vertex.label = V(RICH14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
RICH14_G.clusterCoef <- transitivity(RICH14_GTable, type="global") #cluster coefficient
RICH14_G.degreeCent <- centralization.degree(RICH14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_Gftn <- as.network.matrix(RICH14_Gft)
RICH14_G.netDensity <- network.density(RICH14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_G.entropy <- entropy(RICH14_Gft) #entropy

RICH14_G.netMx <- cbind(RICH14_G.netMx, RICH14_G.clusterCoef, RICH14_G.degreeCent$centralization,
                        RICH14_G.netDensity, RICH14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************

round = 14
teamName = "RICH"
KIoutcome = "Behind_F"
RICH14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
RICH14_Bg2 <- data.frame(RICH14_B)
RICH14_Bg2 <- RICH14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_Bg2$player1
player2vector <- RICH14_Bg2$player2
RICH14_Bg3 <- RICH14_Bg2
RICH14_Bg3$p1inp2vec <- is.element(RICH14_Bg3$player1, player2vector)
RICH14_Bg3$p2inp1vec <- is.element(RICH14_Bg3$player2, player1vector)

addPlayer1 <- RICH14_Bg3[ which(RICH14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH14_Bg3[ which(RICH14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_Bg2 <- rbind(RICH14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
RICH14_Bft <- ftable(RICH14_Bg2$player1, RICH14_Bg2$player2)
RICH14_Bft2 <- as.matrix(RICH14_Bft)
numRows <- nrow(RICH14_Bft2)
numCols <- ncol(RICH14_Bft2)
RICH14_Bft3 <- RICH14_Bft2[c(2:numRows) , c(2:numCols)]
RICH14_BTable <- graph.adjacency(RICH14_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(RICH14_BTable, vertex.label = V(RICH14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
RICH14_B.clusterCoef <- transitivity(RICH14_BTable, type="global") #cluster coefficient
RICH14_B.degreeCent <- centralization.degree(RICH14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_Bftn <- as.network.matrix(RICH14_Bft)
RICH14_B.netDensity <- network.density(RICH14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_B.entropy <- entropy(RICH14_Bft) #entropy

RICH14_B.netMx <- cbind(RICH14_B.netMx, RICH14_B.clusterCoef, RICH14_B.degreeCent$centralization,
                        RICH14_B.netDensity, RICH14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
RICH14_SFg2 <- data.frame(RICH14_SF)
RICH14_SFg2 <- RICH14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_SFg2$player1
player2vector <- RICH14_SFg2$player2
RICH14_SFg3 <- RICH14_SFg2
RICH14_SFg3$p1inp2vec <- is.element(RICH14_SFg3$player1, player2vector)
RICH14_SFg3$p2inp1vec <- is.element(RICH14_SFg3$player2, player1vector)

addPlayer1 <- RICH14_SFg3[ which(RICH14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_SFg3[ which(RICH14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_SFg2 <- rbind(RICH14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
RICH14_SFft <- ftable(RICH14_SFg2$player1, RICH14_SFg2$player2)
RICH14_SFft2 <- as.matrix(RICH14_SFft)
numRows <- nrow(RICH14_SFft2)
numCols <- ncol(RICH14_SFft2)
RICH14_SFft3 <- RICH14_SFft2[c(2:numRows) , c(2:numCols)]
RICH14_SFTable <- graph.adjacency(RICH14_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(RICH14_SFTable, vertex.label = V(RICH14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
RICH14_SF.clusterCoef <- transitivity(RICH14_SFTable, type="global") #cluster coefficient
RICH14_SF.degreeCent <- centralization.degree(RICH14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_SFftn <- as.network.matrix(RICH14_SFft)
RICH14_SF.netDensity <- network.density(RICH14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_SF.entropy <- entropy(RICH14_SFft) #entropy

RICH14_SF.netMx <- cbind(RICH14_SF.netMx, RICH14_SF.clusterCoef, RICH14_SF.degreeCent$centralization,
                         RICH14_SF.netDensity, RICH14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************

round = 14
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
RICH14_TFg2 <- data.frame(RICH14_TF)
RICH14_TFg2 <- RICH14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_TFg2$player1
player2vector <- RICH14_TFg2$player2
RICH14_TFg3 <- RICH14_TFg2
RICH14_TFg3$p1inp2vec <- is.element(RICH14_TFg3$player1, player2vector)
RICH14_TFg3$p2inp1vec <- is.element(RICH14_TFg3$player2, player1vector)

addPlayer1 <- RICH14_TFg3[ which(RICH14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH14_TFg3[ which(RICH14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_TFg2 <- rbind(RICH14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
RICH14_TFft <- ftable(RICH14_TFg2$player1, RICH14_TFg2$player2)
RICH14_TFft2 <- as.matrix(RICH14_TFft)
numRows <- nrow(RICH14_TFft2)
numCols <- ncol(RICH14_TFft2)
RICH14_TFft3 <- RICH14_TFft2[c(2:numRows) , c(2:numCols)]
RICH14_TFTable <- graph.adjacency(RICH14_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(RICH14_TFTable, vertex.label = V(RICH14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
RICH14_TF.clusterCoef <- transitivity(RICH14_TFTable, type="global") #cluster coefficient
RICH14_TF.degreeCent <- centralization.degree(RICH14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_TFftn <- as.network.matrix(RICH14_TFft)
RICH14_TF.netDensity <- network.density(RICH14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_TF.entropy <- entropy(RICH14_TFft) #entropy

RICH14_TF.netMx <- cbind(RICH14_TF.netMx, RICH14_TF.clusterCoef, RICH14_TF.degreeCent$centralization,
                         RICH14_TF.netDensity, RICH14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
RICH14_SAMg2 <- data.frame(RICH14_SAM)
RICH14_SAMg2 <- RICH14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_SAMg2$player1
player2vector <- RICH14_SAMg2$player2
RICH14_SAMg3 <- RICH14_SAMg2
RICH14_SAMg3$p1inp2vec <- is.element(RICH14_SAMg3$player1, player2vector)
RICH14_SAMg3$p2inp1vec <- is.element(RICH14_SAMg3$player2, player1vector)

addPlayer1 <- RICH14_SAMg3[ which(RICH14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_SAMg3[ which(RICH14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_SAMg2 <- rbind(RICH14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
RICH14_SAMft <- ftable(RICH14_SAMg2$player1, RICH14_SAMg2$player2)
RICH14_SAMft2 <- as.matrix(RICH14_SAMft)
numRows <- nrow(RICH14_SAMft2)
numCols <- ncol(RICH14_SAMft2)
RICH14_SAMft3 <- RICH14_SAMft2[c(2:numRows) , c(2:numCols)]
RICH14_SAMTable <- graph.adjacency(RICH14_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(RICH14_SAMTable, vertex.label = V(RICH14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
RICH14_SAM.clusterCoef <- transitivity(RICH14_SAMTable, type="global") #cluster coefficient
RICH14_SAM.degreeCent <- centralization.degree(RICH14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_SAMftn <- as.network.matrix(RICH14_SAMft)
RICH14_SAM.netDensity <- network.density(RICH14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_SAM.entropy <- entropy(RICH14_SAMft) #entropy

RICH14_SAM.netMx <- cbind(RICH14_SAM.netMx, RICH14_SAM.clusterCoef, RICH14_SAM.degreeCent$centralization,
                          RICH14_SAM.netDensity, RICH14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
RICH14_TAMg2 <- data.frame(RICH14_TAM)
RICH14_TAMg2 <- RICH14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_TAMg2$player1
player2vector <- RICH14_TAMg2$player2
RICH14_TAMg3 <- RICH14_TAMg2
RICH14_TAMg3$p1inp2vec <- is.element(RICH14_TAMg3$player1, player2vector)
RICH14_TAMg3$p2inp1vec <- is.element(RICH14_TAMg3$player2, player1vector)

addPlayer1 <- RICH14_TAMg3[ which(RICH14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_TAMg3[ which(RICH14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_TAMg2 <- rbind(RICH14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
RICH14_TAMft <- ftable(RICH14_TAMg2$player1, RICH14_TAMg2$player2)
RICH14_TAMft2 <- as.matrix(RICH14_TAMft)
numRows <- nrow(RICH14_TAMft2)
numCols <- ncol(RICH14_TAMft2)
RICH14_TAMft3 <- RICH14_TAMft2[c(2:numRows) , c(2:numCols)]
RICH14_TAMTable <- graph.adjacency(RICH14_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(RICH14_TAMTable, vertex.label = V(RICH14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
RICH14_TAM.clusterCoef <- transitivity(RICH14_TAMTable, type="global") #cluster coefficient
RICH14_TAM.degreeCent <- centralization.degree(RICH14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_TAMftn <- as.network.matrix(RICH14_TAMft)
RICH14_TAM.netDensity <- network.density(RICH14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_TAM.entropy <- entropy(RICH14_TAMft) #entropy

RICH14_TAM.netMx <- cbind(RICH14_TAM.netMx, RICH14_TAM.clusterCoef, RICH14_TAM.degreeCent$centralization,
                          RICH14_TAM.netDensity, RICH14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
RICH14_SDMg2 <- data.frame(RICH14_SDM)
RICH14_SDMg2 <- RICH14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_SDMg2$player1
player2vector <- RICH14_SDMg2$player2
RICH14_SDMg3 <- RICH14_SDMg2
RICH14_SDMg3$p1inp2vec <- is.element(RICH14_SDMg3$player1, player2vector)
RICH14_SDMg3$p2inp1vec <- is.element(RICH14_SDMg3$player2, player1vector)

addPlayer1 <- RICH14_SDMg3[ which(RICH14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_SDMg3[ which(RICH14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_SDMg2 <- rbind(RICH14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
RICH14_SDMft <- ftable(RICH14_SDMg2$player1, RICH14_SDMg2$player2)
RICH14_SDMft2 <- as.matrix(RICH14_SDMft)
numRows <- nrow(RICH14_SDMft2)
numCols <- ncol(RICH14_SDMft2)
RICH14_SDMft3 <- RICH14_SDMft2[c(2:numRows) , c(2:numCols)]
RICH14_SDMTable <- graph.adjacency(RICH14_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(RICH14_SDMTable, vertex.label = V(RICH14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
RICH14_SDM.clusterCoef <- transitivity(RICH14_SDMTable, type="global") #cluster coefficient
RICH14_SDM.degreeCent <- centralization.degree(RICH14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_SDMftn <- as.network.matrix(RICH14_SDMft)
RICH14_SDM.netDensity <- network.density(RICH14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_SDM.entropy <- entropy(RICH14_SDMft) #entropy

RICH14_SDM.netMx <- cbind(RICH14_SDM.netMx, RICH14_SDM.clusterCoef, RICH14_SDM.degreeCent$centralization,
                          RICH14_SDM.netDensity, RICH14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
RICH14_TDMg2 <- data.frame(RICH14_TDM)
RICH14_TDMg2 <- RICH14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_TDMg2$player1
player2vector <- RICH14_TDMg2$player2
RICH14_TDMg3 <- RICH14_TDMg2
RICH14_TDMg3$p1inp2vec <- is.element(RICH14_TDMg3$player1, player2vector)
RICH14_TDMg3$p2inp1vec <- is.element(RICH14_TDMg3$player2, player1vector)

addPlayer1 <- RICH14_TDMg3[ which(RICH14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_TDMg3[ which(RICH14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_TDMg2 <- rbind(RICH14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
RICH14_TDMft <- ftable(RICH14_TDMg2$player1, RICH14_TDMg2$player2)
RICH14_TDMft2 <- as.matrix(RICH14_TDMft)
numRows <- nrow(RICH14_TDMft2)
numCols <- ncol(RICH14_TDMft2)
RICH14_TDMft3 <- RICH14_TDMft2[c(2:numRows) , c(2:numCols)]
RICH14_TDMTable <- graph.adjacency(RICH14_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(RICH14_TDMTable, vertex.label = V(RICH14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
RICH14_TDM.clusterCoef <- transitivity(RICH14_TDMTable, type="global") #cluster coefficient
RICH14_TDM.degreeCent <- centralization.degree(RICH14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_TDMftn <- as.network.matrix(RICH14_TDMft)
RICH14_TDM.netDensity <- network.density(RICH14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_TDM.entropy <- entropy(RICH14_TDMft) #entropy

RICH14_TDM.netMx <- cbind(RICH14_TDM.netMx, RICH14_TDM.clusterCoef, RICH14_TDM.degreeCent$centralization,
                          RICH14_TDM.netDensity, RICH14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************

round = 14
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
RICH14_SDg2 <- data.frame(RICH14_SD)
RICH14_SDg2 <- RICH14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_SDg2$player1
player2vector <- RICH14_SDg2$player2
RICH14_SDg3 <- RICH14_SDg2
RICH14_SDg3$p1inp2vec <- is.element(RICH14_SDg3$player1, player2vector)
RICH14_SDg3$p2inp1vec <- is.element(RICH14_SDg3$player2, player1vector)

addPlayer1 <- RICH14_SDg3[ which(RICH14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_SDg3[ which(RICH14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_SDg2 <- rbind(RICH14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
RICH14_SDft <- ftable(RICH14_SDg2$player1, RICH14_SDg2$player2)
RICH14_SDft2 <- as.matrix(RICH14_SDft)
numRows <- nrow(RICH14_SDft2)
numCols <- ncol(RICH14_SDft2)
RICH14_SDft3 <- RICH14_SDft2[c(2:numRows) , c(2:numCols)]
RICH14_SDTable <- graph.adjacency(RICH14_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(RICH14_SDTable, vertex.label = V(RICH14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
RICH14_SD.clusterCoef <- transitivity(RICH14_SDTable, type="global") #cluster coefficient
RICH14_SD.degreeCent <- centralization.degree(RICH14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_SDftn <- as.network.matrix(RICH14_SDft)
RICH14_SD.netDensity <- network.density(RICH14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_SD.entropy <- entropy(RICH14_SDft) #entropy

RICH14_SD.netMx <- cbind(RICH14_SD.netMx, RICH14_SD.clusterCoef, RICH14_SD.degreeCent$centralization,
                         RICH14_SD.netDensity, RICH14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
RICH14_TDg2 <- data.frame(RICH14_TD)
RICH14_TDg2 <- RICH14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_TDg2$player1
player2vector <- RICH14_TDg2$player2
RICH14_TDg3 <- RICH14_TDg2
RICH14_TDg3$p1inp2vec <- is.element(RICH14_TDg3$player1, player2vector)
RICH14_TDg3$p2inp1vec <- is.element(RICH14_TDg3$player2, player1vector)

addPlayer1 <- RICH14_TDg3[ which(RICH14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_TDg3[ which(RICH14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_TDg2 <- rbind(RICH14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
RICH14_TDft <- ftable(RICH14_TDg2$player1, RICH14_TDg2$player2)
RICH14_TDft2 <- as.matrix(RICH14_TDft)
numRows <- nrow(RICH14_TDft2)
numCols <- ncol(RICH14_TDft2)
RICH14_TDft3 <- RICH14_TDft2[c(2:numRows) , c(2:numCols)]
RICH14_TDTable <- graph.adjacency(RICH14_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(RICH14_TDTable, vertex.label = V(RICH14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
RICH14_TD.clusterCoef <- transitivity(RICH14_TDTable, type="global") #cluster coefficient
RICH14_TD.degreeCent <- centralization.degree(RICH14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_TDftn <- as.network.matrix(RICH14_TDft)
RICH14_TD.netDensity <- network.density(RICH14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_TD.entropy <- entropy(RICH14_TDft) #entropy

RICH14_TD.netMx <- cbind(RICH14_TD.netMx, RICH14_TD.clusterCoef, RICH14_TD.degreeCent$centralization,
                         RICH14_TD.netDensity, RICH14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
RICH14_QTg2 <- data.frame(RICH14_QT)
RICH14_QTg2 <- RICH14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH14_QTg2$player1
player2vector <- RICH14_QTg2$player2
RICH14_QTg3 <- RICH14_QTg2
RICH14_QTg3$p1inp2vec <- is.element(RICH14_QTg3$player1, player2vector)
RICH14_QTg3$p2inp1vec <- is.element(RICH14_QTg3$player2, player1vector)

addPlayer1 <- RICH14_QTg3[ which(RICH14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH14_QTg3[ which(RICH14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH14_QTg2 <- rbind(RICH14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
RICH14_QTft <- ftable(RICH14_QTg2$player1, RICH14_QTg2$player2)
RICH14_QTft2 <- as.matrix(RICH14_QTft)
numRows <- nrow(RICH14_QTft2)
numCols <- ncol(RICH14_QTft2)
RICH14_QTft3 <- RICH14_QTft2[c(2:numRows) , c(2:numCols)]
RICH14_QTTable <- graph.adjacency(RICH14_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(RICH14_QTTable, vertex.label = V(RICH14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
RICH14_QT.clusterCoef <- transitivity(RICH14_QTTable, type="global") #cluster coefficient
RICH14_QT.degreeCent <- centralization.degree(RICH14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH14_QTftn <- as.network.matrix(RICH14_QTft)
RICH14_QT.netDensity <- network.density(RICH14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH14_QT.entropy <- entropy(RICH14_QTft) #entropy

RICH14_QT.netMx <- cbind(RICH14_QT.netMx, RICH14_QT.clusterCoef, RICH14_QT.degreeCent$centralization,
                         RICH14_QT.netDensity, RICH14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH14_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "STK"
KIoutcome = "Goal_F"
STK14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
STK14_Gg2 <- data.frame(STK14_G)
STK14_Gg2 <- STK14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_Gg2$player1
player2vector <- STK14_Gg2$player2
STK14_Gg3 <- STK14_Gg2
STK14_Gg3$p1inp2vec <- is.element(STK14_Gg3$player1, player2vector)
STK14_Gg3$p2inp1vec <- is.element(STK14_Gg3$player2, player1vector)

addPlayer1 <- STK14_Gg3[ which(STK14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_Gg3[ which(STK14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_Gg2 <- rbind(STK14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
STK14_Gft <- ftable(STK14_Gg2$player1, STK14_Gg2$player2)
STK14_Gft2 <- as.matrix(STK14_Gft)
numRows <- nrow(STK14_Gft2)
numCols <- ncol(STK14_Gft2)
STK14_Gft3 <- STK14_Gft2[c(2:numRows) , c(2:numCols)]
STK14_GTable <- graph.adjacency(STK14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(STK14_GTable, vertex.label = V(STK14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
STK14_G.clusterCoef <- transitivity(STK14_GTable, type="global") #cluster coefficient
STK14_G.degreeCent <- centralization.degree(STK14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_Gftn <- as.network.matrix(STK14_Gft)
STK14_G.netDensity <- network.density(STK14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_G.entropy <- entropy(STK14_Gft) #entropy

STK14_G.netMx <- cbind(STK14_G.netMx, STK14_G.clusterCoef, STK14_G.degreeCent$centralization,
                       STK14_G.netDensity, STK14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Behind_F"
STK14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
STK14_Bg2 <- data.frame(STK14_B)
STK14_Bg2 <- STK14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_Bg2$player1
player2vector <- STK14_Bg2$player2
STK14_Bg3 <- STK14_Bg2
STK14_Bg3$p1inp2vec <- is.element(STK14_Bg3$player1, player2vector)
STK14_Bg3$p2inp1vec <- is.element(STK14_Bg3$player2, player1vector)

addPlayer1 <- STK14_Bg3[ which(STK14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_Bg3[ which(STK14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_Bg2 <- rbind(STK14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
STK14_Bft <- ftable(STK14_Bg2$player1, STK14_Bg2$player2)
STK14_Bft2 <- as.matrix(STK14_Bft)
numRows <- nrow(STK14_Bft2)
numCols <- ncol(STK14_Bft2)
STK14_Bft3 <- STK14_Bft2[c(2:numRows) , c(2:numCols)]
STK14_BTable <- graph.adjacency(STK14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(STK14_BTable, vertex.label = V(STK14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
STK14_B.clusterCoef <- transitivity(STK14_BTable, type="global") #cluster coefficient
STK14_B.degreeCent <- centralization.degree(STK14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_Bftn <- as.network.matrix(STK14_Bft)
STK14_B.netDensity <- network.density(STK14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_B.entropy <- entropy(STK14_Bft) #entropy

STK14_B.netMx <- cbind(STK14_B.netMx, STK14_B.clusterCoef, STK14_B.degreeCent$centralization,
                       STK14_B.netDensity, STK14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Stoppage_F"
STK14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
STK14_SFg2 <- data.frame(STK14_SF)
STK14_SFg2 <- STK14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_SFg2$player1
player2vector <- STK14_SFg2$player2
STK14_SFg3 <- STK14_SFg2
STK14_SFg3$p1inp2vec <- is.element(STK14_SFg3$player1, player2vector)
STK14_SFg3$p2inp1vec <- is.element(STK14_SFg3$player2, player1vector)

addPlayer1 <- STK14_SFg3[ which(STK14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_SFg3[ which(STK14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_SFg2 <- rbind(STK14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
STK14_SFft <- ftable(STK14_SFg2$player1, STK14_SFg2$player2)
STK14_SFft2 <- as.matrix(STK14_SFft)
numRows <- nrow(STK14_SFft2)
numCols <- ncol(STK14_SFft2)
STK14_SFft3 <- STK14_SFft2[c(2:numRows) , c(2:numCols)]
STK14_SFTable <- graph.adjacency(STK14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(STK14_SFTable, vertex.label = V(STK14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
STK14_SF.clusterCoef <- transitivity(STK14_SFTable, type="global") #cluster coefficient
STK14_SF.degreeCent <- centralization.degree(STK14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_SFftn <- as.network.matrix(STK14_SFft)
STK14_SF.netDensity <- network.density(STK14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_SF.entropy <- entropy(STK14_SFft) #entropy

STK14_SF.netMx <- cbind(STK14_SF.netMx, STK14_SF.clusterCoef, STK14_SF.degreeCent$centralization,
                        STK14_SF.netDensity, STK14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Turnover_F"
STK14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
STK14_TFg2 <- data.frame(STK14_TF)
STK14_TFg2 <- STK14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_TFg2$player1
player2vector <- STK14_TFg2$player2
STK14_TFg3 <- STK14_TFg2
STK14_TFg3$p1inp2vec <- is.element(STK14_TFg3$player1, player2vector)
STK14_TFg3$p2inp1vec <- is.element(STK14_TFg3$player2, player1vector)

addPlayer1 <- STK14_TFg3[ which(STK14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_TFg3[ which(STK14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_TFg2 <- rbind(STK14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
STK14_TFft <- ftable(STK14_TFg2$player1, STK14_TFg2$player2)
STK14_TFft2 <- as.matrix(STK14_TFft)
numRows <- nrow(STK14_TFft2)
numCols <- ncol(STK14_TFft2)
STK14_TFft3 <- STK14_TFft2[c(2:numRows) , c(2:numCols)]
STK14_TFTable <- graph.adjacency(STK14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(STK14_TFTable, vertex.label = V(STK14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
STK14_TF.clusterCoef <- transitivity(STK14_TFTable, type="global") #cluster coefficient
STK14_TF.degreeCent <- centralization.degree(STK14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_TFftn <- as.network.matrix(STK14_TFft)
STK14_TF.netDensity <- network.density(STK14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_TF.entropy <- entropy(STK14_TFft) #entropy

STK14_TF.netMx <- cbind(STK14_TF.netMx, STK14_TF.clusterCoef, STK14_TF.degreeCent$centralization,
                        STK14_TF.netDensity, STK14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
STK14_SAMg2 <- data.frame(STK14_SAM)
STK14_SAMg2 <- STK14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_SAMg2$player1
player2vector <- STK14_SAMg2$player2
STK14_SAMg3 <- STK14_SAMg2
STK14_SAMg3$p1inp2vec <- is.element(STK14_SAMg3$player1, player2vector)
STK14_SAMg3$p2inp1vec <- is.element(STK14_SAMg3$player2, player1vector)

addPlayer1 <- STK14_SAMg3[ which(STK14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_SAMg3[ which(STK14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_SAMg2 <- rbind(STK14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
STK14_SAMft <- ftable(STK14_SAMg2$player1, STK14_SAMg2$player2)
STK14_SAMft2 <- as.matrix(STK14_SAMft)
numRows <- nrow(STK14_SAMft2)
numCols <- ncol(STK14_SAMft2)
STK14_SAMft3 <- STK14_SAMft2[c(2:numRows) , c(2:numCols)]
STK14_SAMTable <- graph.adjacency(STK14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(STK14_SAMTable, vertex.label = V(STK14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
STK14_SAM.clusterCoef <- transitivity(STK14_SAMTable, type="global") #cluster coefficient
STK14_SAM.degreeCent <- centralization.degree(STK14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_SAMftn <- as.network.matrix(STK14_SAMft)
STK14_SAM.netDensity <- network.density(STK14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_SAM.entropy <- entropy(STK14_SAMft) #entropy

STK14_SAM.netMx <- cbind(STK14_SAM.netMx, STK14_SAM.clusterCoef, STK14_SAM.degreeCent$centralization,
                         STK14_SAM.netDensity, STK14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Turnover_AM"
STK14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
STK14_TAMg2 <- data.frame(STK14_TAM)
STK14_TAMg2 <- STK14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_TAMg2$player1
player2vector <- STK14_TAMg2$player2
STK14_TAMg3 <- STK14_TAMg2
STK14_TAMg3$p1inp2vec <- is.element(STK14_TAMg3$player1, player2vector)
STK14_TAMg3$p2inp1vec <- is.element(STK14_TAMg3$player2, player1vector)

addPlayer1 <- STK14_TAMg3[ which(STK14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_TAMg3[ which(STK14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_TAMg2 <- rbind(STK14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
STK14_TAMft <- ftable(STK14_TAMg2$player1, STK14_TAMg2$player2)
STK14_TAMft2 <- as.matrix(STK14_TAMft)
numRows <- nrow(STK14_TAMft2)
numCols <- ncol(STK14_TAMft2)
STK14_TAMft3 <- STK14_TAMft2[c(1:numRows) , c(1:numCols)]
STK14_TAMTable <- graph.adjacency(STK14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(STK14_TAMTable, vertex.label = V(STK14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
STK14_TAM.clusterCoef <- transitivity(STK14_TAMTable, type="global") #cluster coefficient
STK14_TAM.degreeCent <- centralization.degree(STK14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_TAMftn <- as.network.matrix(STK14_TAMft)
STK14_TAM.netDensity <- network.density(STK14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_TAM.entropy <- entropy(STK14_TAMft) #entropy

STK14_TAM.netMx <- cbind(STK14_TAM.netMx, STK14_TAM.clusterCoef, STK14_TAM.degreeCent$centralization,
                         STK14_TAM.netDensity, STK14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
STK14_SDMg2 <- data.frame(STK14_SDM)
STK14_SDMg2 <- STK14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_SDMg2$player1
player2vector <- STK14_SDMg2$player2
STK14_SDMg3 <- STK14_SDMg2
STK14_SDMg3$p1inp2vec <- is.element(STK14_SDMg3$player1, player2vector)
STK14_SDMg3$p2inp1vec <- is.element(STK14_SDMg3$player2, player1vector)

addPlayer1 <- STK14_SDMg3[ which(STK14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK14_SDMg3[ which(STK14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_SDMg2 <- rbind(STK14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
STK14_SDMft <- ftable(STK14_SDMg2$player1, STK14_SDMg2$player2)
STK14_SDMft2 <- as.matrix(STK14_SDMft)
numRows <- nrow(STK14_SDMft2)
numCols <- ncol(STK14_SDMft2)
STK14_SDMft3 <- STK14_SDMft2[c(2:numRows) , c(2:numCols)]
STK14_SDMTable <- graph.adjacency(STK14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(STK14_SDMTable, vertex.label = V(STK14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
STK14_SDM.clusterCoef <- transitivity(STK14_SDMTable, type="global") #cluster coefficient
STK14_SDM.degreeCent <- centralization.degree(STK14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_SDMftn <- as.network.matrix(STK14_SDMft)
STK14_SDM.netDensity <- network.density(STK14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_SDM.entropy <- entropy(STK14_SDMft) #entropy

STK14_SDM.netMx <- cbind(STK14_SDM.netMx, STK14_SDM.clusterCoef, STK14_SDM.degreeCent$centralization,
                         STK14_SDM.netDensity, STK14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Turnover_DM"
STK14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
STK14_TDMg2 <- data.frame(STK14_TDM)
STK14_TDMg2 <- STK14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_TDMg2$player1
player2vector <- STK14_TDMg2$player2
STK14_TDMg3 <- STK14_TDMg2
STK14_TDMg3$p1inp2vec <- is.element(STK14_TDMg3$player1, player2vector)
STK14_TDMg3$p2inp1vec <- is.element(STK14_TDMg3$player2, player1vector)

addPlayer1 <- STK14_TDMg3[ which(STK14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_TDMg3[ which(STK14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_TDMg2 <- rbind(STK14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
STK14_TDMft <- ftable(STK14_TDMg2$player1, STK14_TDMg2$player2)
STK14_TDMft2 <- as.matrix(STK14_TDMft)
numRows <- nrow(STK14_TDMft2)
numCols <- ncol(STK14_TDMft2)
STK14_TDMft3 <- STK14_TDMft2[c(2:numRows) , c(2:numCols)]
STK14_TDMTable <- graph.adjacency(STK14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(STK14_TDMTable, vertex.label = V(STK14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
STK14_TDM.clusterCoef <- transitivity(STK14_TDMTable, type="global") #cluster coefficient
STK14_TDM.degreeCent <- centralization.degree(STK14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_TDMftn <- as.network.matrix(STK14_TDMft)
STK14_TDM.netDensity <- network.density(STK14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_TDM.entropy <- entropy(STK14_TDMft) #entropy

STK14_TDM.netMx <- cbind(STK14_TDM.netMx, STK14_TDM.clusterCoef, STK14_TDM.degreeCent$centralization,
                         STK14_TDM.netDensity, STK14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Stoppage_D"
STK14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
STK14_SDg2 <- data.frame(STK14_SD)
STK14_SDg2 <- STK14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_SDg2$player1
player2vector <- STK14_SDg2$player2
STK14_SDg3 <- STK14_SDg2
STK14_SDg3$p1inp2vec <- is.element(STK14_SDg3$player1, player2vector)
STK14_SDg3$p2inp1vec <- is.element(STK14_SDg3$player2, player1vector)

addPlayer1 <- STK14_SDg3[ which(STK14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_SDg3[ which(STK14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_SDg2 <- rbind(STK14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
STK14_SDft <- ftable(STK14_SDg2$player1, STK14_SDg2$player2)
STK14_SDft2 <- as.matrix(STK14_SDft)
numRows <- nrow(STK14_SDft2)
numCols <- ncol(STK14_SDft2)
STK14_SDft3 <- STK14_SDft2[c(2:numRows) , c(2:numCols)]
STK14_SDTable <- graph.adjacency(STK14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(STK14_SDTable, vertex.label = V(STK14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
STK14_SD.clusterCoef <- transitivity(STK14_SDTable, type="global") #cluster coefficient
STK14_SD.degreeCent <- centralization.degree(STK14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_SDftn <- as.network.matrix(STK14_SDft)
STK14_SD.netDensity <- network.density(STK14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_SD.entropy <- entropy(STK14_SDft) #entropy

STK14_SD.netMx <- cbind(STK14_SD.netMx, STK14_SD.clusterCoef, STK14_SD.degreeCent$centralization,
                        STK14_SD.netDensity, STK14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "Turnover_D"
STK14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
STK14_TDg2 <- data.frame(STK14_TD)
STK14_TDg2 <- STK14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_TDg2$player1
player2vector <- STK14_TDg2$player2
STK14_TDg3 <- STK14_TDg2
STK14_TDg3$p1inp2vec <- is.element(STK14_TDg3$player1, player2vector)
STK14_TDg3$p2inp1vec <- is.element(STK14_TDg3$player2, player1vector)

addPlayer1 <- STK14_TDg3[ which(STK14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_TDg3[ which(STK14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_TDg2 <- rbind(STK14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
STK14_TDft <- ftable(STK14_TDg2$player1, STK14_TDg2$player2)
STK14_TDft2 <- as.matrix(STK14_TDft)
numRows <- nrow(STK14_TDft2)
numCols <- ncol(STK14_TDft2)
STK14_TDft3 <- STK14_TDft2[c(2:numRows) , c(2:numCols)]
STK14_TDTable <- graph.adjacency(STK14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(STK14_TDTable, vertex.label = V(STK14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
STK14_TD.clusterCoef <- transitivity(STK14_TDTable, type="global") #cluster coefficient
STK14_TD.degreeCent <- centralization.degree(STK14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_TDftn <- as.network.matrix(STK14_TDft)
STK14_TD.netDensity <- network.density(STK14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_TD.entropy <- entropy(STK14_TDft) #entropy

STK14_TD.netMx <- cbind(STK14_TD.netMx, STK14_TD.clusterCoef, STK14_TD.degreeCent$centralization,
                        STK14_TD.netDensity, STK14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
STK14_QTg2 <- data.frame(STK14_QT)
STK14_QTg2 <- STK14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK14_QTg2$player1
player2vector <- STK14_QTg2$player2
STK14_QTg3 <- STK14_QTg2
STK14_QTg3$p1inp2vec <- is.element(STK14_QTg3$player1, player2vector)
STK14_QTg3$p2inp1vec <- is.element(STK14_QTg3$player2, player1vector)

addPlayer1 <- STK14_QTg3[ which(STK14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK14_QTg3[ which(STK14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK14_QTg2 <- rbind(STK14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
STK14_QTft <- ftable(STK14_QTg2$player1, STK14_QTg2$player2)
STK14_QTft2 <- as.matrix(STK14_QTft)
numRows <- nrow(STK14_QTft2)
numCols <- ncol(STK14_QTft2)
STK14_QTft3 <- STK14_QTft2[c(2:numRows) , c(2:numCols)]
STK14_QTTable <- graph.adjacency(STK14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(STK14_QTTable, vertex.label = V(STK14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
STK14_QT.clusterCoef <- transitivity(STK14_QTTable, type="global") #cluster coefficient
STK14_QT.degreeCent <- centralization.degree(STK14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK14_QTftn <- as.network.matrix(STK14_QTft)
STK14_QT.netDensity <- network.density(STK14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK14_QT.entropy <- entropy(STK14_QTft) #entropy

STK14_QT.netMx <- cbind(STK14_QT.netMx, STK14_QT.clusterCoef, STK14_QT.degreeCent$centralization,
                        STK14_QT.netDensity, STK14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK14_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "SYD"
KIoutcome = "Goal_F"
SYD14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
SYD14_Gg2 <- data.frame(SYD14_G)
SYD14_Gg2 <- SYD14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_Gg2$player1
player2vector <- SYD14_Gg2$player2
SYD14_Gg3 <- SYD14_Gg2
SYD14_Gg3$p1inp2vec <- is.element(SYD14_Gg3$player1, player2vector)
SYD14_Gg3$p2inp1vec <- is.element(SYD14_Gg3$player2, player1vector)

addPlayer1 <- SYD14_Gg3[ which(SYD14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_Gg3[ which(SYD14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_Gg2 <- rbind(SYD14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
SYD14_Gft <- ftable(SYD14_Gg2$player1, SYD14_Gg2$player2)
SYD14_Gft2 <- as.matrix(SYD14_Gft)
numRows <- nrow(SYD14_Gft2)
numCols <- ncol(SYD14_Gft2)
SYD14_Gft3 <- SYD14_Gft2[c(2:numRows) , c(2:numCols)]
SYD14_GTable <- graph.adjacency(SYD14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(SYD14_GTable, vertex.label = V(SYD14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
SYD14_G.clusterCoef <- transitivity(SYD14_GTable, type="global") #cluster coefficient
SYD14_G.degreeCent <- centralization.degree(SYD14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_Gftn <- as.network.matrix(SYD14_Gft)
SYD14_G.netDensity <- network.density(SYD14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_G.entropy <- entropy(SYD14_Gft) #entropy

SYD14_G.netMx <- cbind(SYD14_G.netMx, SYD14_G.clusterCoef, SYD14_G.degreeCent$centralization,
                       SYD14_G.netDensity, SYD14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "Behind_F"
SYD14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
SYD14_Bg2 <- data.frame(SYD14_B)
SYD14_Bg2 <- SYD14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_Bg2$player1
player2vector <- SYD14_Bg2$player2
SYD14_Bg3 <- SYD14_Bg2
SYD14_Bg3$p1inp2vec <- is.element(SYD14_Bg3$player1, player2vector)
SYD14_Bg3$p2inp1vec <- is.element(SYD14_Bg3$player2, player1vector)

addPlayer1 <- SYD14_Bg3[ which(SYD14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_Bg3[ which(SYD14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_Bg2 <- rbind(SYD14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
SYD14_Bft <- ftable(SYD14_Bg2$player1, SYD14_Bg2$player2)
SYD14_Bft2 <- as.matrix(SYD14_Bft)
numRows <- nrow(SYD14_Bft2)
numCols <- ncol(SYD14_Bft2)
SYD14_Bft3 <- SYD14_Bft2[c(2:numRows) , c(2:numCols)]
SYD14_BTable <- graph.adjacency(SYD14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(SYD14_BTable, vertex.label = V(SYD14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
SYD14_B.clusterCoef <- transitivity(SYD14_BTable, type="global") #cluster coefficient
SYD14_B.degreeCent <- centralization.degree(SYD14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_Bftn <- as.network.matrix(SYD14_Bft)
SYD14_B.netDensity <- network.density(SYD14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_B.entropy <- entropy(SYD14_Bft) #entropy

SYD14_B.netMx <- cbind(SYD14_B.netMx, SYD14_B.clusterCoef, SYD14_B.degreeCent$centralization,
                       SYD14_B.netDensity, SYD14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
SYD14_SFg2 <- data.frame(SYD14_SF)
SYD14_SFg2 <- SYD14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_SFg2$player1
player2vector <- SYD14_SFg2$player2
SYD14_SFg3 <- SYD14_SFg2
SYD14_SFg3$p1inp2vec <- is.element(SYD14_SFg3$player1, player2vector)
SYD14_SFg3$p2inp1vec <- is.element(SYD14_SFg3$player2, player1vector)

addPlayer1 <- SYD14_SFg3[ which(SYD14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_SFg3[ which(SYD14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_SFg2 <- rbind(SYD14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
SYD14_SFft <- ftable(SYD14_SFg2$player1, SYD14_SFg2$player2)
SYD14_SFft2 <- as.matrix(SYD14_SFft)
numRows <- nrow(SYD14_SFft2)
numCols <- ncol(SYD14_SFft2)
SYD14_SFft3 <- SYD14_SFft2[c(2:numRows) , c(2:numCols)]
SYD14_SFTable <- graph.adjacency(SYD14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(SYD14_SFTable, vertex.label = V(SYD14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
SYD14_SF.clusterCoef <- transitivity(SYD14_SFTable, type="global") #cluster coefficient
SYD14_SF.degreeCent <- centralization.degree(SYD14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_SFftn <- as.network.matrix(SYD14_SFft)
SYD14_SF.netDensity <- network.density(SYD14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_SF.entropy <- entropy(SYD14_SFft) #entropy

SYD14_SF.netMx <- cbind(SYD14_SF.netMx, SYD14_SF.clusterCoef, SYD14_SF.degreeCent$centralization,
                        SYD14_SF.netDensity, SYD14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************

round = 14
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
SYD14_TFg2 <- data.frame(SYD14_TF)
SYD14_TFg2 <- SYD14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_TFg2$player1
player2vector <- SYD14_TFg2$player2
SYD14_TFg3 <- SYD14_TFg2
SYD14_TFg3$p1inp2vec <- is.element(SYD14_TFg3$player1, player2vector)
SYD14_TFg3$p2inp1vec <- is.element(SYD14_TFg3$player2, player1vector)

addPlayer1 <- SYD14_TFg3[ which(SYD14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_TFg3[ which(SYD14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_TFg2 <- rbind(SYD14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
SYD14_TFft <- ftable(SYD14_TFg2$player1, SYD14_TFg2$player2)
SYD14_TFft2 <- as.matrix(SYD14_TFft)
numRows <- nrow(SYD14_TFft2)
numCols <- ncol(SYD14_TFft2)
SYD14_TFft3 <- SYD14_TFft2[c(2:numRows) , c(2:numCols)]
SYD14_TFTable <- graph.adjacency(SYD14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(SYD14_TFTable, vertex.label = V(SYD14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
SYD14_TF.clusterCoef <- transitivity(SYD14_TFTable, type="global") #cluster coefficient
SYD14_TF.degreeCent <- centralization.degree(SYD14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_TFftn <- as.network.matrix(SYD14_TFft)
SYD14_TF.netDensity <- network.density(SYD14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_TF.entropy <- entropy(SYD14_TFft) #entropy

SYD14_TF.netMx <- cbind(SYD14_TF.netMx, SYD14_TF.clusterCoef, SYD14_TF.degreeCent$centralization,
                        SYD14_TF.netDensity, SYD14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************

round = 14
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
SYD14_SAMg2 <- data.frame(SYD14_SAM)
SYD14_SAMg2 <- SYD14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_SAMg2$player1
player2vector <- SYD14_SAMg2$player2
SYD14_SAMg3 <- SYD14_SAMg2
SYD14_SAMg3$p1inp2vec <- is.element(SYD14_SAMg3$player1, player2vector)
SYD14_SAMg3$p2inp1vec <- is.element(SYD14_SAMg3$player2, player1vector)

addPlayer1 <- SYD14_SAMg3[ which(SYD14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_SAMg3[ which(SYD14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_SAMg2 <- rbind(SYD14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
SYD14_SAMft <- ftable(SYD14_SAMg2$player1, SYD14_SAMg2$player2)
SYD14_SAMft2 <- as.matrix(SYD14_SAMft)
numRows <- nrow(SYD14_SAMft2)
numCols <- ncol(SYD14_SAMft2)
SYD14_SAMft3 <- SYD14_SAMft2[c(2:numRows) , c(2:numCols)]
SYD14_SAMTable <- graph.adjacency(SYD14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(SYD14_SAMTable, vertex.label = V(SYD14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
SYD14_SAM.clusterCoef <- transitivity(SYD14_SAMTable, type="global") #cluster coefficient
SYD14_SAM.degreeCent <- centralization.degree(SYD14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_SAMftn <- as.network.matrix(SYD14_SAMft)
SYD14_SAM.netDensity <- network.density(SYD14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_SAM.entropy <- entropy(SYD14_SAMft) #entropy

SYD14_SAM.netMx <- cbind(SYD14_SAM.netMx, SYD14_SAM.clusterCoef, SYD14_SAM.degreeCent$centralization,
                         SYD14_SAM.netDensity, SYD14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
SYD14_TAMg2 <- data.frame(SYD14_TAM)
SYD14_TAMg2 <- SYD14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_TAMg2$player1
player2vector <- SYD14_TAMg2$player2
SYD14_TAMg3 <- SYD14_TAMg2
SYD14_TAMg3$p1inp2vec <- is.element(SYD14_TAMg3$player1, player2vector)
SYD14_TAMg3$p2inp1vec <- is.element(SYD14_TAMg3$player2, player1vector)

addPlayer1 <- SYD14_TAMg3[ which(SYD14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_TAMg3[ which(SYD14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_TAMg2 <- rbind(SYD14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
SYD14_TAMft <- ftable(SYD14_TAMg2$player1, SYD14_TAMg2$player2)
SYD14_TAMft2 <- as.matrix(SYD14_TAMft)
numRows <- nrow(SYD14_TAMft2)
numCols <- ncol(SYD14_TAMft2)
SYD14_TAMft3 <- SYD14_TAMft2[c(2:numRows) , c(2:numCols)]
SYD14_TAMTable <- graph.adjacency(SYD14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(SYD14_TAMTable, vertex.label = V(SYD14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
SYD14_TAM.clusterCoef <- transitivity(SYD14_TAMTable, type="global") #cluster coefficient
SYD14_TAM.degreeCent <- centralization.degree(SYD14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_TAMftn <- as.network.matrix(SYD14_TAMft)
SYD14_TAM.netDensity <- network.density(SYD14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_TAM.entropy <- entropy(SYD14_TAMft) #entropy

SYD14_TAM.netMx <- cbind(SYD14_TAM.netMx, SYD14_TAM.clusterCoef, SYD14_TAM.degreeCent$centralization,
                         SYD14_TAM.netDensity, SYD14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
SYD14_SDMg2 <- data.frame(SYD14_SDM)
SYD14_SDMg2 <- SYD14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_SDMg2$player1
player2vector <- SYD14_SDMg2$player2
SYD14_SDMg3 <- SYD14_SDMg2
SYD14_SDMg3$p1inp2vec <- is.element(SYD14_SDMg3$player1, player2vector)
SYD14_SDMg3$p2inp1vec <- is.element(SYD14_SDMg3$player2, player1vector)

addPlayer1 <- SYD14_SDMg3[ which(SYD14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_SDMg3[ which(SYD14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_SDMg2 <- rbind(SYD14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
SYD14_SDMft <- ftable(SYD14_SDMg2$player1, SYD14_SDMg2$player2)
SYD14_SDMft2 <- as.matrix(SYD14_SDMft)
numRows <- nrow(SYD14_SDMft2)
numCols <- ncol(SYD14_SDMft2)
SYD14_SDMft3 <- SYD14_SDMft2[c(2:numRows) , c(2:numCols)]
SYD14_SDMTable <- graph.adjacency(SYD14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(SYD14_SDMTable, vertex.label = V(SYD14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
SYD14_SDM.clusterCoef <- transitivity(SYD14_SDMTable, type="global") #cluster coefficient
SYD14_SDM.degreeCent <- centralization.degree(SYD14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_SDMftn <- as.network.matrix(SYD14_SDMft)
SYD14_SDM.netDensity <- network.density(SYD14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_SDM.entropy <- entropy(SYD14_SDMft) #entropy

SYD14_SDM.netMx <- cbind(SYD14_SDM.netMx, SYD14_SDM.clusterCoef, SYD14_SDM.degreeCent$centralization,
                         SYD14_SDM.netDensity, SYD14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
SYD14_TDMg2 <- data.frame(SYD14_TDM)
SYD14_TDMg2 <- SYD14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_TDMg2$player1
player2vector <- SYD14_TDMg2$player2
SYD14_TDMg3 <- SYD14_TDMg2
SYD14_TDMg3$p1inp2vec <- is.element(SYD14_TDMg3$player1, player2vector)
SYD14_TDMg3$p2inp1vec <- is.element(SYD14_TDMg3$player2, player1vector)

addPlayer1 <- SYD14_TDMg3[ which(SYD14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_TDMg3[ which(SYD14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_TDMg2 <- rbind(SYD14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
SYD14_TDMft <- ftable(SYD14_TDMg2$player1, SYD14_TDMg2$player2)
SYD14_TDMft2 <- as.matrix(SYD14_TDMft)
numRows <- nrow(SYD14_TDMft2)
numCols <- ncol(SYD14_TDMft2)
SYD14_TDMft3 <- SYD14_TDMft2[c(2:numRows) , c(2:numCols)]
SYD14_TDMTable <- graph.adjacency(SYD14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(SYD14_TDMTable, vertex.label = V(SYD14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
SYD14_TDM.clusterCoef <- transitivity(SYD14_TDMTable, type="global") #cluster coefficient
SYD14_TDM.degreeCent <- centralization.degree(SYD14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_TDMftn <- as.network.matrix(SYD14_TDMft)
SYD14_TDM.netDensity <- network.density(SYD14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_TDM.entropy <- entropy(SYD14_TDMft) #entropy

SYD14_TDM.netMx <- cbind(SYD14_TDM.netMx, SYD14_TDM.clusterCoef, SYD14_TDM.degreeCent$centralization,
                         SYD14_TDM.netDensity, SYD14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
SYD14_SDg2 <- data.frame(SYD14_SD)
SYD14_SDg2 <- SYD14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_SDg2$player1
player2vector <- SYD14_SDg2$player2
SYD14_SDg3 <- SYD14_SDg2
SYD14_SDg3$p1inp2vec <- is.element(SYD14_SDg3$player1, player2vector)
SYD14_SDg3$p2inp1vec <- is.element(SYD14_SDg3$player2, player1vector)

addPlayer1 <- SYD14_SDg3[ which(SYD14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_SDg3[ which(SYD14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_SDg2 <- rbind(SYD14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
SYD14_SDft <- ftable(SYD14_SDg2$player1, SYD14_SDg2$player2)
SYD14_SDft2 <- as.matrix(SYD14_SDft)
numRows <- nrow(SYD14_SDft2)
numCols <- ncol(SYD14_SDft2)
SYD14_SDft3 <- SYD14_SDft2[c(2:numRows) , c(2:numCols)]
SYD14_SDTable <- graph.adjacency(SYD14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(SYD14_SDTable, vertex.label = V(SYD14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
SYD14_SD.clusterCoef <- transitivity(SYD14_SDTable, type="global") #cluster coefficient
SYD14_SD.degreeCent <- centralization.degree(SYD14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_SDftn <- as.network.matrix(SYD14_SDft)
SYD14_SD.netDensity <- network.density(SYD14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_SD.entropy <- entropy(SYD14_SDft) #entropy

SYD14_SD.netMx <- cbind(SYD14_SD.netMx, SYD14_SD.clusterCoef, SYD14_SD.degreeCent$centralization,
                        SYD14_SD.netDensity, SYD14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
SYD14_TDg2 <- data.frame(SYD14_TD)
SYD14_TDg2 <- SYD14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_TDg2$player1
player2vector <- SYD14_TDg2$player2
SYD14_TDg3 <- SYD14_TDg2
SYD14_TDg3$p1inp2vec <- is.element(SYD14_TDg3$player1, player2vector)
SYD14_TDg3$p2inp1vec <- is.element(SYD14_TDg3$player2, player1vector)

addPlayer1 <- SYD14_TDg3[ which(SYD14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_TDg3[ which(SYD14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_TDg2 <- rbind(SYD14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
SYD14_TDft <- ftable(SYD14_TDg2$player1, SYD14_TDg2$player2)
SYD14_TDft2 <- as.matrix(SYD14_TDft)
numRows <- nrow(SYD14_TDft2)
numCols <- ncol(SYD14_TDft2)
SYD14_TDft3 <- SYD14_TDft2[c(2:numRows) , c(2:numCols)]
SYD14_TDTable <- graph.adjacency(SYD14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(SYD14_TDTable, vertex.label = V(SYD14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
SYD14_TD.clusterCoef <- transitivity(SYD14_TDTable, type="global") #cluster coefficient
SYD14_TD.degreeCent <- centralization.degree(SYD14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_TDftn <- as.network.matrix(SYD14_TDft)
SYD14_TD.netDensity <- network.density(SYD14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_TD.entropy <- entropy(SYD14_TDft) #entropy

SYD14_TD.netMx <- cbind(SYD14_TD.netMx, SYD14_TD.clusterCoef, SYD14_TD.degreeCent$centralization,
                        SYD14_TD.netDensity, SYD14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
SYD14_QTg2 <- data.frame(SYD14_QT)
SYD14_QTg2 <- SYD14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD14_QTg2$player1
player2vector <- SYD14_QTg2$player2
SYD14_QTg3 <- SYD14_QTg2
SYD14_QTg3$p1inp2vec <- is.element(SYD14_QTg3$player1, player2vector)
SYD14_QTg3$p2inp1vec <- is.element(SYD14_QTg3$player2, player1vector)

addPlayer1 <- SYD14_QTg3[ which(SYD14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD14_QTg3[ which(SYD14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD14_QTg2 <- rbind(SYD14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
SYD14_QTft <- ftable(SYD14_QTg2$player1, SYD14_QTg2$player2)
SYD14_QTft2 <- as.matrix(SYD14_QTft)
numRows <- nrow(SYD14_QTft2)
numCols <- ncol(SYD14_QTft2)
SYD14_QTft3 <- SYD14_QTft2[c(2:numRows) , c(2:numCols)]
SYD14_QTTable <- graph.adjacency(SYD14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(SYD14_QTTable, vertex.label = V(SYD14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
SYD14_QT.clusterCoef <- transitivity(SYD14_QTTable, type="global") #cluster coefficient
SYD14_QT.degreeCent <- centralization.degree(SYD14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD14_QTftn <- as.network.matrix(SYD14_QTft)
SYD14_QT.netDensity <- network.density(SYD14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD14_QT.entropy <- entropy(SYD14_QTft) #entropy

SYD14_QT.netMx <- cbind(SYD14_QT.netMx, SYD14_QT.clusterCoef, SYD14_QT.degreeCent$centralization,
                        SYD14_QT.netDensity, SYD14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD14_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "WB"
KIoutcome = "Goal_F"
WB14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
WB14_Gg2 <- data.frame(WB14_G)
WB14_Gg2 <- WB14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_Gg2$player1
player2vector <- WB14_Gg2$player2
WB14_Gg3 <- WB14_Gg2
WB14_Gg3$p1inp2vec <- is.element(WB14_Gg3$player1, player2vector)
WB14_Gg3$p2inp1vec <- is.element(WB14_Gg3$player2, player1vector)

addPlayer1 <- WB14_Gg3[ which(WB14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_Gg3[ which(WB14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_Gg2 <- rbind(WB14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
WB14_Gft <- ftable(WB14_Gg2$player1, WB14_Gg2$player2)
WB14_Gft2 <- as.matrix(WB14_Gft)
numRows <- nrow(WB14_Gft2)
numCols <- ncol(WB14_Gft2)
WB14_Gft3 <- WB14_Gft2[c(2:numRows) , c(2:numCols)]
WB14_GTable <- graph.adjacency(WB14_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(WB14_GTable, vertex.label = V(WB14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
WB14_G.clusterCoef <- transitivity(WB14_GTable, type="global") #cluster coefficient
WB14_G.degreeCent <- centralization.degree(WB14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_Gftn <- as.network.matrix(WB14_Gft)
WB14_G.netDensity <- network.density(WB14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_G.entropy <- entropy(WB14_Gft) #entropy

WB14_G.netMx <- cbind(WB14_G.netMx, WB14_G.clusterCoef, WB14_G.degreeCent$centralization,
                      WB14_G.netDensity, WB14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "WB"
KIoutcome = "Behind_F"
WB14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
WB14_Bg2 <- data.frame(WB14_B)
WB14_Bg2 <- WB14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_Bg2$player1
player2vector <- WB14_Bg2$player2
WB14_Bg3 <- WB14_Bg2
WB14_Bg3$p1inp2vec <- is.element(WB14_Bg3$player1, player2vector)
WB14_Bg3$p2inp1vec <- is.element(WB14_Bg3$player2, player1vector)

addPlayer1 <- WB14_Bg3[ which(WB14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_Bg3[ which(WB14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_Bg2 <- rbind(WB14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
WB14_Bft <- ftable(WB14_Bg2$player1, WB14_Bg2$player2)
WB14_Bft2 <- as.matrix(WB14_Bft)
numRows <- nrow(WB14_Bft2)
numCols <- ncol(WB14_Bft2)
WB14_Bft3 <- WB14_Bft2[c(2:numRows) , c(2:numCols)]
WB14_BTable <- graph.adjacency(WB14_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(WB14_BTable, vertex.label = V(WB14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
WB14_B.clusterCoef <- transitivity(WB14_BTable, type="global") #cluster coefficient
WB14_B.degreeCent <- centralization.degree(WB14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_Bftn <- as.network.matrix(WB14_Bft)
WB14_B.netDensity <- network.density(WB14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_B.entropy <- entropy(WB14_Bft) #entropy

WB14_B.netMx <- cbind(WB14_B.netMx, WB14_B.clusterCoef, WB14_B.degreeCent$centralization,
                      WB14_B.netDensity, WB14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************

round = 14
teamName = "WB"
KIoutcome = "Stoppage_F"
WB14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
WB14_SFg2 <- data.frame(WB14_SF)
WB14_SFg2 <- WB14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_SFg2$player1
player2vector <- WB14_SFg2$player2
WB14_SFg3 <- WB14_SFg2
WB14_SFg3$p1inp2vec <- is.element(WB14_SFg3$player1, player2vector)
WB14_SFg3$p2inp1vec <- is.element(WB14_SFg3$player2, player1vector)

addPlayer1 <- WB14_SFg3[ which(WB14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB14_SFg3[ which(WB14_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_SFg2 <- rbind(WB14_SFg2, addPlayers)

#ROUND 14, FWD Stoppage graph using weighted edges
WB14_SFft <- ftable(WB14_SFg2$player1, WB14_SFg2$player2)
WB14_SFft2 <- as.matrix(WB14_SFft)
numRows <- nrow(WB14_SFft2)
numCols <- ncol(WB14_SFft2)
WB14_SFft3 <- WB14_SFft2[c(2:numRows) , c(2:numCols)]
WB14_SFTable <- graph.adjacency(WB14_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(WB14_SFTable, vertex.label = V(WB14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
WB14_SF.clusterCoef <- transitivity(WB14_SFTable, type="global") #cluster coefficient
WB14_SF.degreeCent <- centralization.degree(WB14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_SFftn <- as.network.matrix(WB14_SFft)
WB14_SF.netDensity <- network.density(WB14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_SF.entropy <- entropy(WB14_SFft) #entropy

WB14_SF.netMx <- cbind(WB14_SF.netMx, WB14_SF.clusterCoef, WB14_SF.degreeCent$centralization,
                       WB14_SF.netDensity, WB14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************

round = 14
teamName = "WB"
KIoutcome = "Turnover_F"
WB14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
WB14_TFg2 <- data.frame(WB14_TF)
WB14_TFg2 <- WB14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_TFg2$player1
player2vector <- WB14_TFg2$player2
WB14_TFg3 <- WB14_TFg2
WB14_TFg3$p1inp2vec <- is.element(WB14_TFg3$player1, player2vector)
WB14_TFg3$p2inp1vec <- is.element(WB14_TFg3$player2, player1vector)

addPlayer1 <- WB14_TFg3[ which(WB14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_TFg3[ which(WB14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_TFg2 <- rbind(WB14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
WB14_TFft <- ftable(WB14_TFg2$player1, WB14_TFg2$player2)
WB14_TFft2 <- as.matrix(WB14_TFft)
numRows <- nrow(WB14_TFft2)
numCols <- ncol(WB14_TFft2)
WB14_TFft3 <- WB14_TFft2[c(2:numRows) , c(2:numCols)]
WB14_TFTable <- graph.adjacency(WB14_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(WB14_TFTable, vertex.label = V(WB14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
WB14_TF.clusterCoef <- transitivity(WB14_TFTable, type="global") #cluster coefficient
WB14_TF.degreeCent <- centralization.degree(WB14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_TFftn <- as.network.matrix(WB14_TFft)
WB14_TF.netDensity <- network.density(WB14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_TF.entropy <- entropy(WB14_TFft) #entropy

WB14_TF.netMx <- cbind(WB14_TF.netMx, WB14_TF.clusterCoef, WB14_TF.degreeCent$centralization,
                       WB14_TF.netDensity, WB14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
WB14_SAMg2 <- data.frame(WB14_SAM)
WB14_SAMg2 <- WB14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_SAMg2$player1
player2vector <- WB14_SAMg2$player2
WB14_SAMg3 <- WB14_SAMg2
WB14_SAMg3$p1inp2vec <- is.element(WB14_SAMg3$player1, player2vector)
WB14_SAMg3$p2inp1vec <- is.element(WB14_SAMg3$player2, player1vector)

addPlayer1 <- WB14_SAMg3[ which(WB14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_SAMg3[ which(WB14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_SAMg2 <- rbind(WB14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
WB14_SAMft <- ftable(WB14_SAMg2$player1, WB14_SAMg2$player2)
WB14_SAMft2 <- as.matrix(WB14_SAMft)
numRows <- nrow(WB14_SAMft2)
numCols <- ncol(WB14_SAMft2)
WB14_SAMft3 <- WB14_SAMft2[c(2:numRows) , c(2:numCols)]
WB14_SAMTable <- graph.adjacency(WB14_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(WB14_SAMTable, vertex.label = V(WB14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
WB14_SAM.clusterCoef <- transitivity(WB14_SAMTable, type="global") #cluster coefficient
WB14_SAM.degreeCent <- centralization.degree(WB14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_SAMftn <- as.network.matrix(WB14_SAMft)
WB14_SAM.netDensity <- network.density(WB14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_SAM.entropy <- entropy(WB14_SAMft) #entropy

WB14_SAM.netMx <- cbind(WB14_SAM.netMx, WB14_SAM.clusterCoef, WB14_SAM.degreeCent$centralization,
                        WB14_SAM.netDensity, WB14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "WB"
KIoutcome = "Turnover_AM"
WB14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
WB14_TAMg2 <- data.frame(WB14_TAM)
WB14_TAMg2 <- WB14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_TAMg2$player1
player2vector <- WB14_TAMg2$player2
WB14_TAMg3 <- WB14_TAMg2
WB14_TAMg3$p1inp2vec <- is.element(WB14_TAMg3$player1, player2vector)
WB14_TAMg3$p2inp1vec <- is.element(WB14_TAMg3$player2, player1vector)

addPlayer1 <- WB14_TAMg3[ which(WB14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB14_TAMg3[ which(WB14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_TAMg2 <- rbind(WB14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
WB14_TAMft <- ftable(WB14_TAMg2$player1, WB14_TAMg2$player2)
WB14_TAMft2 <- as.matrix(WB14_TAMft)
numRows <- nrow(WB14_TAMft2)
numCols <- ncol(WB14_TAMft2)
WB14_TAMft3 <- WB14_TAMft2[c(2:numRows) , c(2:numCols)]
WB14_TAMTable <- graph.adjacency(WB14_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(WB14_TAMTable, vertex.label = V(WB14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
WB14_TAM.clusterCoef <- transitivity(WB14_TAMTable, type="global") #cluster coefficient
WB14_TAM.degreeCent <- centralization.degree(WB14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_TAMftn <- as.network.matrix(WB14_TAMft)
WB14_TAM.netDensity <- network.density(WB14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_TAM.entropy <- entropy(WB14_TAMft) #entropy

WB14_TAM.netMx <- cbind(WB14_TAM.netMx, WB14_TAM.clusterCoef, WB14_TAM.degreeCent$centralization,
                        WB14_TAM.netDensity, WB14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************

round = 14
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
WB14_SDMg2 <- data.frame(WB14_SDM)
WB14_SDMg2 <- WB14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_SDMg2$player1
player2vector <- WB14_SDMg2$player2
WB14_SDMg3 <- WB14_SDMg2
WB14_SDMg3$p1inp2vec <- is.element(WB14_SDMg3$player1, player2vector)
WB14_SDMg3$p2inp1vec <- is.element(WB14_SDMg3$player2, player1vector)

addPlayer1 <- WB14_SDMg3[ which(WB14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_SDMg3[ which(WB14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_SDMg2 <- rbind(WB14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
WB14_SDMft <- ftable(WB14_SDMg2$player1, WB14_SDMg2$player2)
WB14_SDMft2 <- as.matrix(WB14_SDMft)
numRows <- nrow(WB14_SDMft2)
numCols <- ncol(WB14_SDMft2)
WB14_SDMft3 <- WB14_SDMft2[c(2:numRows) , c(2:numCols)]
WB14_SDMTable <- graph.adjacency(WB14_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(WB14_SDMTable, vertex.label = V(WB14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
WB14_SDM.clusterCoef <- transitivity(WB14_SDMTable, type="global") #cluster coefficient
WB14_SDM.degreeCent <- centralization.degree(WB14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_SDMftn <- as.network.matrix(WB14_SDMft)
WB14_SDM.netDensity <- network.density(WB14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_SDM.entropy <- entropy(WB14_SDMft) #entropy

WB14_SDM.netMx <- cbind(WB14_SDM.netMx, WB14_SDM.clusterCoef, WB14_SDM.degreeCent$centralization,
                        WB14_SDM.netDensity, WB14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************

round = 14
teamName = "WB"
KIoutcome = "Turnover_DM"
WB14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
WB14_TDMg2 <- data.frame(WB14_TDM)
WB14_TDMg2 <- WB14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_TDMg2$player1
player2vector <- WB14_TDMg2$player2
WB14_TDMg3 <- WB14_TDMg2
WB14_TDMg3$p1inp2vec <- is.element(WB14_TDMg3$player1, player2vector)
WB14_TDMg3$p2inp1vec <- is.element(WB14_TDMg3$player2, player1vector)

addPlayer1 <- WB14_TDMg3[ which(WB14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_TDMg3[ which(WB14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_TDMg2 <- rbind(WB14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
WB14_TDMft <- ftable(WB14_TDMg2$player1, WB14_TDMg2$player2)
WB14_TDMft2 <- as.matrix(WB14_TDMft)
numRows <- nrow(WB14_TDMft2)
numCols <- ncol(WB14_TDMft2)
WB14_TDMft3 <- WB14_TDMft2[c(2:numRows) , c(2:numCols)]
WB14_TDMTable <- graph.adjacency(WB14_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(WB14_TDMTable, vertex.label = V(WB14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
WB14_TDM.clusterCoef <- transitivity(WB14_TDMTable, type="global") #cluster coefficient
WB14_TDM.degreeCent <- centralization.degree(WB14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_TDMftn <- as.network.matrix(WB14_TDMft)
WB14_TDM.netDensity <- network.density(WB14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_TDM.entropy <- entropy(WB14_TDMft) #entropy

WB14_TDM.netMx <- cbind(WB14_TDM.netMx, WB14_TDM.clusterCoef, WB14_TDM.degreeCent$centralization,
                        WB14_TDM.netDensity, WB14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "WB"
KIoutcome = "Stoppage_D"
WB14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
WB14_SDg2 <- data.frame(WB14_SD)
WB14_SDg2 <- WB14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_SDg2$player1
player2vector <- WB14_SDg2$player2
WB14_SDg3 <- WB14_SDg2
WB14_SDg3$p1inp2vec <- is.element(WB14_SDg3$player1, player2vector)
WB14_SDg3$p2inp1vec <- is.element(WB14_SDg3$player2, player1vector)

addPlayer1 <- WB14_SDg3[ which(WB14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_SDg3[ which(WB14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_SDg2 <- rbind(WB14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
WB14_SDft <- ftable(WB14_SDg2$player1, WB14_SDg2$player2)
WB14_SDft2 <- as.matrix(WB14_SDft)
numRows <- nrow(WB14_SDft2)
numCols <- ncol(WB14_SDft2)
WB14_SDft3 <- WB14_SDft2[c(2:numRows) , c(2:numCols)]
WB14_SDTable <- graph.adjacency(WB14_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(WB14_SDTable, vertex.label = V(WB14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
WB14_SD.clusterCoef <- transitivity(WB14_SDTable, type="global") #cluster coefficient
WB14_SD.degreeCent <- centralization.degree(WB14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_SDftn <- as.network.matrix(WB14_SDft)
WB14_SD.netDensity <- network.density(WB14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_SD.entropy <- entropy(WB14_SDft) #entropy

WB14_SD.netMx <- cbind(WB14_SD.netMx, WB14_SD.clusterCoef, WB14_SD.degreeCent$centralization,
                       WB14_SD.netDensity, WB14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "WB"
KIoutcome = "Turnover_D"
WB14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
WB14_TDg2 <- data.frame(WB14_TD)
WB14_TDg2 <- WB14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_TDg2$player1
player2vector <- WB14_TDg2$player2
WB14_TDg3 <- WB14_TDg2
WB14_TDg3$p1inp2vec <- is.element(WB14_TDg3$player1, player2vector)
WB14_TDg3$p2inp1vec <- is.element(WB14_TDg3$player2, player1vector)

addPlayer1 <- WB14_TDg3[ which(WB14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_TDg3[ which(WB14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_TDg2 <- rbind(WB14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
WB14_TDft <- ftable(WB14_TDg2$player1, WB14_TDg2$player2)
WB14_TDft2 <- as.matrix(WB14_TDft)
numRows <- nrow(WB14_TDft2)
numCols <- ncol(WB14_TDft2)
WB14_TDft3 <- WB14_TDft2[c(2:numRows) , c(2:numCols)]
WB14_TDTable <- graph.adjacency(WB14_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(WB14_TDTable, vertex.label = V(WB14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
WB14_TD.clusterCoef <- transitivity(WB14_TDTable, type="global") #cluster coefficient
WB14_TD.degreeCent <- centralization.degree(WB14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_TDftn <- as.network.matrix(WB14_TDft)
WB14_TD.netDensity <- network.density(WB14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_TD.entropy <- entropy(WB14_TDft) #entropy

WB14_TD.netMx <- cbind(WB14_TD.netMx, WB14_TD.clusterCoef, WB14_TD.degreeCent$centralization,
                       WB14_TD.netDensity, WB14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
WB14_QTg2 <- data.frame(WB14_QT)
WB14_QTg2 <- WB14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB14_QTg2$player1
player2vector <- WB14_QTg2$player2
WB14_QTg3 <- WB14_QTg2
WB14_QTg3$p1inp2vec <- is.element(WB14_QTg3$player1, player2vector)
WB14_QTg3$p2inp1vec <- is.element(WB14_QTg3$player2, player1vector)

addPlayer1 <- WB14_QTg3[ which(WB14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB14_QTg3[ which(WB14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB14_QTg2 <- rbind(WB14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
WB14_QTft <- ftable(WB14_QTg2$player1, WB14_QTg2$player2)
WB14_QTft2 <- as.matrix(WB14_QTft)
numRows <- nrow(WB14_QTft2)
numCols <- ncol(WB14_QTft2)
WB14_QTft3 <- WB14_QTft2[c(2:numRows) , c(2:numCols)]
WB14_QTTable <- graph.adjacency(WB14_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(WB14_QTTable, vertex.label = V(WB14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
WB14_QT.clusterCoef <- transitivity(WB14_QTTable, type="global") #cluster coefficient
WB14_QT.degreeCent <- centralization.degree(WB14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB14_QTftn <- as.network.matrix(WB14_QTft)
WB14_QT.netDensity <- network.density(WB14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB14_QT.entropy <- entropy(WB14_QTft) #entropy

WB14_QT.netMx <- cbind(WB14_QT.netMx, WB14_QT.clusterCoef, WB14_QT.degreeCent$centralization,
                       WB14_QT.netDensity, WB14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB14_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 14
##

#ROUND 14, Goal***************************************************************

round = 14
teamName = "WCE"
KIoutcome = "Goal_F"
WCE14_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Goal with weighted edges
WCE14_Gg2 <- data.frame(WCE14_G)
WCE14_Gg2 <- WCE14_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_Gg2$player1
player2vector <- WCE14_Gg2$player2
WCE14_Gg3 <- WCE14_Gg2
WCE14_Gg3$p1inp2vec <- is.element(WCE14_Gg3$player1, player2vector)
WCE14_Gg3$p2inp1vec <- is.element(WCE14_Gg3$player2, player1vector)

addPlayer1 <- WCE14_Gg3[ which(WCE14_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE14_Gg3[ which(WCE14_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_Gg2 <- rbind(WCE14_Gg2, addPlayers)

#ROUND 14, Goal graph using weighted edges
WCE14_Gft <- ftable(WCE14_Gg2$player1, WCE14_Gg2$player2)
WCE14_Gft2 <- as.matrix(WCE14_Gft)
numRows <- nrow(WCE14_Gft2)
numCols <- ncol(WCE14_Gft2)
WCE14_Gft3 <- WCE14_Gft2[c(2:numRows) , c(2:numCols)]
WCE14_GTable <- graph.adjacency(WCE14_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Goal graph=weighted
plot.igraph(WCE14_GTable, vertex.label = V(WCE14_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Goal calulation of network metrics
#igraph
WCE14_G.clusterCoef <- transitivity(WCE14_GTable, type="global") #cluster coefficient
WCE14_G.degreeCent <- centralization.degree(WCE14_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_Gftn <- as.network.matrix(WCE14_Gft)
WCE14_G.netDensity <- network.density(WCE14_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_G.entropy <- entropy(WCE14_Gft) #entropy

WCE14_G.netMx <- cbind(WCE14_G.netMx, WCE14_G.clusterCoef, WCE14_G.degreeCent$centralization,
                       WCE14_G.netDensity, WCE14_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_G.netMx) <- varnames

#ROUND 14, Behind***************************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Behind_F"
WCE14_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, Behind with weighted edges
WCE14_Bg2 <- data.frame(WCE14_B)
WCE14_Bg2 <- WCE14_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_Bg2$player1
player2vector <- WCE14_Bg2$player2
WCE14_Bg3 <- WCE14_Bg2
WCE14_Bg3$p1inp2vec <- is.element(WCE14_Bg3$player1, player2vector)
WCE14_Bg3$p2inp1vec <- is.element(WCE14_Bg3$player2, player1vector)

addPlayer1 <- WCE14_Bg3[ which(WCE14_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_Bg3[ which(WCE14_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_Bg2 <- rbind(WCE14_Bg2, addPlayers)

#ROUND 14, Behind graph using weighted edges
WCE14_Bft <- ftable(WCE14_Bg2$player1, WCE14_Bg2$player2)
WCE14_Bft2 <- as.matrix(WCE14_Bft)
numRows <- nrow(WCE14_Bft2)
numCols <- ncol(WCE14_Bft2)
WCE14_Bft3 <- WCE14_Bft2[c(2:numRows) , c(2:numCols)]
WCE14_BTable <- graph.adjacency(WCE14_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 14, Behind graph=weighted
plot.igraph(WCE14_BTable, vertex.label = V(WCE14_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, Behind calulation of network metrics
#igraph
WCE14_B.clusterCoef <- transitivity(WCE14_BTable, type="global") #cluster coefficient
WCE14_B.degreeCent <- centralization.degree(WCE14_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_Bftn <- as.network.matrix(WCE14_Bft)
WCE14_B.netDensity <- network.density(WCE14_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_B.entropy <- entropy(WCE14_Bft) #entropy

WCE14_B.netMx <- cbind(WCE14_B.netMx, WCE14_B.clusterCoef, WCE14_B.degreeCent$centralization,
                       WCE14_B.netDensity, WCE14_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_B.netMx) <- varnames

#ROUND 14, FWD Stoppage**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE14_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Stoppage with weighted edges
WCE14_SFg2 <- data.frame(WCE14_SF)
WCE14_SFg2 <- WCE14_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_SFg2$player1
player2vector <- WCE14_SFg2$player2
WCE14_SFg3 <- WCE14_SFg2
WCE14_SFg3$p1inp2vec <- is.element(WCE14_SFg3$player1, player2vector)
WCE14_SFg3$p2inp1vec <- is.element(WCE14_SFg3$player2, player1vector)

addPlayer1 <- WCE14_SFg3[ which(WCE14_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE14_SFg2 <- rbind(WCE14_SFg2, addPlayer1)

#ROUND 14, FWD Stoppage graph using weighted edges
WCE14_SFft <- ftable(WCE14_SFg2$player1, WCE14_SFg2$player2)
WCE14_SFft2 <- as.matrix(WCE14_SFft)
numRows <- nrow(WCE14_SFft2)
numCols <- ncol(WCE14_SFft2)
WCE14_SFft3 <- WCE14_SFft2[c(2:numRows) , c(1:numCols)]
WCE14_SFTable <- graph.adjacency(WCE14_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Stoppage graph=weighted
plot.igraph(WCE14_SFTable, vertex.label = V(WCE14_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Stoppage calulation of network metrics
#igraph
WCE14_SF.clusterCoef <- transitivity(WCE14_SFTable, type="global") #cluster coefficient
WCE14_SF.degreeCent <- centralization.degree(WCE14_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_SFftn <- as.network.matrix(WCE14_SFft)
WCE14_SF.netDensity <- network.density(WCE14_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_SF.entropy <- entropy(WCE14_SFft) #entropy

WCE14_SF.netMx <- cbind(WCE14_SF.netMx, WCE14_SF.clusterCoef, WCE14_SF.degreeCent$centralization,
                        WCE14_SF.netDensity, WCE14_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_SF.netMx) <- varnames

#ROUND 14, FWD Turnover**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE14_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, FWD Turnover with weighted edges
WCE14_TFg2 <- data.frame(WCE14_TF)
WCE14_TFg2 <- WCE14_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_TFg2$player1
player2vector <- WCE14_TFg2$player2
WCE14_TFg3 <- WCE14_TFg2
WCE14_TFg3$p1inp2vec <- is.element(WCE14_TFg3$player1, player2vector)
WCE14_TFg3$p2inp1vec <- is.element(WCE14_TFg3$player2, player1vector)

addPlayer1 <- WCE14_TFg3[ which(WCE14_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_TFg3[ which(WCE14_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_TFg2 <- rbind(WCE14_TFg2, addPlayers)

#ROUND 14, FWD Turnover graph using weighted edges
WCE14_TFft <- ftable(WCE14_TFg2$player1, WCE14_TFg2$player2)
WCE14_TFft2 <- as.matrix(WCE14_TFft)
numRows <- nrow(WCE14_TFft2)
numCols <- ncol(WCE14_TFft2)
WCE14_TFft3 <- WCE14_TFft2[c(2:numRows) , c(2:numCols)]
WCE14_TFTable <- graph.adjacency(WCE14_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, FWD Turnover graph=weighted
plot.igraph(WCE14_TFTable, vertex.label = V(WCE14_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, FWD Turnover calulation of network metrics
#igraph
WCE14_TF.clusterCoef <- transitivity(WCE14_TFTable, type="global") #cluster coefficient
WCE14_TF.degreeCent <- centralization.degree(WCE14_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_TFftn <- as.network.matrix(WCE14_TFft)
WCE14_TF.netDensity <- network.density(WCE14_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_TF.entropy <- entropy(WCE14_TFft) #entropy

WCE14_TF.netMx <- cbind(WCE14_TF.netMx, WCE14_TF.clusterCoef, WCE14_TF.degreeCent$centralization,
                        WCE14_TF.netDensity, WCE14_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_TF.netMx) <- varnames

#ROUND 14, AM Stoppage**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE14_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Stoppage with weighted edges
WCE14_SAMg2 <- data.frame(WCE14_SAM)
WCE14_SAMg2 <- WCE14_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_SAMg2$player1
player2vector <- WCE14_SAMg2$player2
WCE14_SAMg3 <- WCE14_SAMg2
WCE14_SAMg3$p1inp2vec <- is.element(WCE14_SAMg3$player1, player2vector)
WCE14_SAMg3$p2inp1vec <- is.element(WCE14_SAMg3$player2, player1vector)

addPlayer1 <- WCE14_SAMg3[ which(WCE14_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_SAMg3[ which(WCE14_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_SAMg2 <- rbind(WCE14_SAMg2, addPlayers)

#ROUND 14, AM Stoppage graph using weighted edges
WCE14_SAMft <- ftable(WCE14_SAMg2$player1, WCE14_SAMg2$player2)
WCE14_SAMft2 <- as.matrix(WCE14_SAMft)
numRows <- nrow(WCE14_SAMft2)
numCols <- ncol(WCE14_SAMft2)
WCE14_SAMft3 <- WCE14_SAMft2[c(2:numRows) , c(2:numCols)]
WCE14_SAMTable <- graph.adjacency(WCE14_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Stoppage graph=weighted
plot.igraph(WCE14_SAMTable, vertex.label = V(WCE14_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Stoppage calulation of network metrics
#igraph
WCE14_SAM.clusterCoef <- transitivity(WCE14_SAMTable, type="global") #cluster coefficient
WCE14_SAM.degreeCent <- centralization.degree(WCE14_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_SAMftn <- as.network.matrix(WCE14_SAMft)
WCE14_SAM.netDensity <- network.density(WCE14_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_SAM.entropy <- entropy(WCE14_SAMft) #entropy

WCE14_SAM.netMx <- cbind(WCE14_SAM.netMx, WCE14_SAM.clusterCoef, WCE14_SAM.degreeCent$centralization,
                         WCE14_SAM.netDensity, WCE14_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_SAM.netMx) <- varnames

#ROUND 14, AM Turnover**********************************************************

round = 14
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE14_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, AM Turnover with weighted edges
WCE14_TAMg2 <- data.frame(WCE14_TAM)
WCE14_TAMg2 <- WCE14_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_TAMg2$player1
player2vector <- WCE14_TAMg2$player2
WCE14_TAMg3 <- WCE14_TAMg2
WCE14_TAMg3$p1inp2vec <- is.element(WCE14_TAMg3$player1, player2vector)
WCE14_TAMg3$p2inp1vec <- is.element(WCE14_TAMg3$player2, player1vector)

addPlayer1 <- WCE14_TAMg3[ which(WCE14_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_TAMg3[ which(WCE14_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_TAMg2 <- rbind(WCE14_TAMg2, addPlayers)

#ROUND 14, AM Turnover graph using weighted edges
WCE14_TAMft <- ftable(WCE14_TAMg2$player1, WCE14_TAMg2$player2)
WCE14_TAMft2 <- as.matrix(WCE14_TAMft)
numRows <- nrow(WCE14_TAMft2)
numCols <- ncol(WCE14_TAMft2)
WCE14_TAMft3 <- WCE14_TAMft2[c(2:numRows) , c(2:numCols)]
WCE14_TAMTable <- graph.adjacency(WCE14_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, AM Turnover graph=weighted
plot.igraph(WCE14_TAMTable, vertex.label = V(WCE14_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, AM Turnover calulation of network metrics
#igraph
WCE14_TAM.clusterCoef <- transitivity(WCE14_TAMTable, type="global") #cluster coefficient
WCE14_TAM.degreeCent <- centralization.degree(WCE14_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_TAMftn <- as.network.matrix(WCE14_TAMft)
WCE14_TAM.netDensity <- network.density(WCE14_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_TAM.entropy <- entropy(WCE14_TAMft) #entropy

WCE14_TAM.netMx <- cbind(WCE14_TAM.netMx, WCE14_TAM.clusterCoef, WCE14_TAM.degreeCent$centralization,
                         WCE14_TAM.netDensity, WCE14_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_TAM.netMx) <- varnames

#ROUND 14, DM Stoppage**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE14_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Stoppage with weighted edges
WCE14_SDMg2 <- data.frame(WCE14_SDM)
WCE14_SDMg2 <- WCE14_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_SDMg2$player1
player2vector <- WCE14_SDMg2$player2
WCE14_SDMg3 <- WCE14_SDMg2
WCE14_SDMg3$p1inp2vec <- is.element(WCE14_SDMg3$player1, player2vector)
WCE14_SDMg3$p2inp1vec <- is.element(WCE14_SDMg3$player2, player1vector)

addPlayer1 <- WCE14_SDMg3[ which(WCE14_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_SDMg3[ which(WCE14_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_SDMg2 <- rbind(WCE14_SDMg2, addPlayers)

#ROUND 14, DM Stoppage graph using weighted edges
WCE14_SDMft <- ftable(WCE14_SDMg2$player1, WCE14_SDMg2$player2)
WCE14_SDMft2 <- as.matrix(WCE14_SDMft)
numRows <- nrow(WCE14_SDMft2)
numCols <- ncol(WCE14_SDMft2)
WCE14_SDMft3 <- WCE14_SDMft2[c(2:numRows) , c(2:numCols)]
WCE14_SDMTable <- graph.adjacency(WCE14_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Stoppage graph=weighted
plot.igraph(WCE14_SDMTable, vertex.label = V(WCE14_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Stoppage calulation of network metrics
#igraph
WCE14_SDM.clusterCoef <- transitivity(WCE14_SDMTable, type="global") #cluster coefficient
WCE14_SDM.degreeCent <- centralization.degree(WCE14_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_SDMftn <- as.network.matrix(WCE14_SDMft)
WCE14_SDM.netDensity <- network.density(WCE14_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_SDM.entropy <- entropy(WCE14_SDMft) #entropy

WCE14_SDM.netMx <- cbind(WCE14_SDM.netMx, WCE14_SDM.clusterCoef, WCE14_SDM.degreeCent$centralization,
                         WCE14_SDM.netDensity, WCE14_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_SDM.netMx) <- varnames

#ROUND 14, DM Turnover**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE14_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, DM Turnover with weighted edges
WCE14_TDMg2 <- data.frame(WCE14_TDM)
WCE14_TDMg2 <- WCE14_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_TDMg2$player1
player2vector <- WCE14_TDMg2$player2
WCE14_TDMg3 <- WCE14_TDMg2
WCE14_TDMg3$p1inp2vec <- is.element(WCE14_TDMg3$player1, player2vector)
WCE14_TDMg3$p2inp1vec <- is.element(WCE14_TDMg3$player2, player1vector)

addPlayer1 <- WCE14_TDMg3[ which(WCE14_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_TDMg3[ which(WCE14_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_TDMg2 <- rbind(WCE14_TDMg2, addPlayers)

#ROUND 14, DM Turnover graph using weighted edges
WCE14_TDMft <- ftable(WCE14_TDMg2$player1, WCE14_TDMg2$player2)
WCE14_TDMft2 <- as.matrix(WCE14_TDMft)
numRows <- nrow(WCE14_TDMft2)
numCols <- ncol(WCE14_TDMft2)
WCE14_TDMft3 <- WCE14_TDMft2[c(2:numRows) , c(2:numCols)]
WCE14_TDMTable <- graph.adjacency(WCE14_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 14, DM Turnover graph=weighted
plot.igraph(WCE14_TDMTable, vertex.label = V(WCE14_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, DM Turnover calulation of network metrics
#igraph
WCE14_TDM.clusterCoef <- transitivity(WCE14_TDMTable, type="global") #cluster coefficient
WCE14_TDM.degreeCent <- centralization.degree(WCE14_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_TDMftn <- as.network.matrix(WCE14_TDMft)
WCE14_TDM.netDensity <- network.density(WCE14_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_TDM.entropy <- entropy(WCE14_TDMft) #entropy

WCE14_TDM.netMx <- cbind(WCE14_TDM.netMx, WCE14_TDM.clusterCoef, WCE14_TDM.degreeCent$centralization,
                         WCE14_TDM.netDensity, WCE14_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_TDM.netMx) <- varnames

#ROUND 14, D Stoppage**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE14_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Stoppage with weighted edges
WCE14_SDg2 <- data.frame(WCE14_SD)
WCE14_SDg2 <- WCE14_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_SDg2$player1
player2vector <- WCE14_SDg2$player2
WCE14_SDg3 <- WCE14_SDg2
WCE14_SDg3$p1inp2vec <- is.element(WCE14_SDg3$player1, player2vector)
WCE14_SDg3$p2inp1vec <- is.element(WCE14_SDg3$player2, player1vector)

addPlayer1 <- WCE14_SDg3[ which(WCE14_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_SDg3[ which(WCE14_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_SDg2 <- rbind(WCE14_SDg2, addPlayers)

#ROUND 14, D Stoppage graph using weighted edges
WCE14_SDft <- ftable(WCE14_SDg2$player1, WCE14_SDg2$player2)
WCE14_SDft2 <- as.matrix(WCE14_SDft)
numRows <- nrow(WCE14_SDft2)
numCols <- ncol(WCE14_SDft2)
WCE14_SDft3 <- WCE14_SDft2[c(2:numRows) , c(2:numCols)]
WCE14_SDTable <- graph.adjacency(WCE14_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Stoppage graph=weighted
plot.igraph(WCE14_SDTable, vertex.label = V(WCE14_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Stoppage calulation of network metrics
#igraph
WCE14_SD.clusterCoef <- transitivity(WCE14_SDTable, type="global") #cluster coefficient
WCE14_SD.degreeCent <- centralization.degree(WCE14_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_SDftn <- as.network.matrix(WCE14_SDft)
WCE14_SD.netDensity <- network.density(WCE14_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_SD.entropy <- entropy(WCE14_SDft) #entropy

WCE14_SD.netMx <- cbind(WCE14_SD.netMx, WCE14_SD.clusterCoef, WCE14_SD.degreeCent$centralization,
                        WCE14_SD.netDensity, WCE14_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_SD.netMx) <- varnames

#ROUND 14, D Turnover**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE14_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, D Turnover with weighted edges
WCE14_TDg2 <- data.frame(WCE14_TD)
WCE14_TDg2 <- WCE14_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_TDg2$player1
player2vector <- WCE14_TDg2$player2
WCE14_TDg3 <- WCE14_TDg2
WCE14_TDg3$p1inp2vec <- is.element(WCE14_TDg3$player1, player2vector)
WCE14_TDg3$p2inp1vec <- is.element(WCE14_TDg3$player2, player1vector)

addPlayer1 <- WCE14_TDg3[ which(WCE14_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_TDg3[ which(WCE14_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_TDg2 <- rbind(WCE14_TDg2, addPlayers)

#ROUND 14, D Turnover graph using weighted edges
WCE14_TDft <- ftable(WCE14_TDg2$player1, WCE14_TDg2$player2)
WCE14_TDft2 <- as.matrix(WCE14_TDft)
numRows <- nrow(WCE14_TDft2)
numCols <- ncol(WCE14_TDft2)
WCE14_TDft3 <- WCE14_TDft2[c(2:numRows) , c(2:numCols)]
WCE14_TDTable <- graph.adjacency(WCE14_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, D Turnover graph=weighted
plot.igraph(WCE14_TDTable, vertex.label = V(WCE14_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, D Turnover calulation of network metrics
#igraph
WCE14_TD.clusterCoef <- transitivity(WCE14_TDTable, type="global") #cluster coefficient
WCE14_TD.degreeCent <- centralization.degree(WCE14_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_TDftn <- as.network.matrix(WCE14_TDft)
WCE14_TD.netDensity <- network.density(WCE14_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_TD.entropy <- entropy(WCE14_TDft) #entropy

WCE14_TD.netMx <- cbind(WCE14_TD.netMx, WCE14_TD.clusterCoef, WCE14_TD.degreeCent$centralization,
                        WCE14_TD.netDensity, WCE14_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_TD.netMx) <- varnames

#ROUND 14, End of Qtr**********************************************************
#NA

round = 14
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE14_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 14, End of Qtr with weighted edges
WCE14_QTg2 <- data.frame(WCE14_QT)
WCE14_QTg2 <- WCE14_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE14_QTg2$player1
player2vector <- WCE14_QTg2$player2
WCE14_QTg3 <- WCE14_QTg2
WCE14_QTg3$p1inp2vec <- is.element(WCE14_QTg3$player1, player2vector)
WCE14_QTg3$p2inp1vec <- is.element(WCE14_QTg3$player2, player1vector)

addPlayer1 <- WCE14_QTg3[ which(WCE14_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE14_QTg3[ which(WCE14_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE14_QTg2 <- rbind(WCE14_QTg2, addPlayers)

#ROUND 14, End of Qtr graph using weighted edges
WCE14_QTft <- ftable(WCE14_QTg2$player1, WCE14_QTg2$player2)
WCE14_QTft2 <- as.matrix(WCE14_QTft)
numRows <- nrow(WCE14_QTft2)
numCols <- ncol(WCE14_QTft2)
WCE14_QTft3 <- WCE14_QTft2[c(2:numRows) , c(2:numCols)]
WCE14_QTTable <- graph.adjacency(WCE14_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 14, End of Qtr graph=weighted
plot.igraph(WCE14_QTTable, vertex.label = V(WCE14_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE14_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 14, End of Qtr calulation of network metrics
#igraph
WCE14_QT.clusterCoef <- transitivity(WCE14_QTTable, type="global") #cluster coefficient
WCE14_QT.degreeCent <- centralization.degree(WCE14_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE14_QTftn <- as.network.matrix(WCE14_QTft)
WCE14_QT.netDensity <- network.density(WCE14_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE14_QT.entropy <- entropy(WCE14_QTft) #entropy

WCE14_QT.netMx <- cbind(WCE14_QT.netMx, WCE14_QT.clusterCoef, WCE14_QT.degreeCent$centralization,
                        WCE14_QT.netDensity, WCE14_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE14_QT.netMx) <- varnames
