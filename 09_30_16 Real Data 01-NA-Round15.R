#####
#09-30-16- Real data 15
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
ADEL15_Gg2 <- data.frame(ADEL15_G)
ADEL15_Gg2 <- ADEL15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_Gg2$player1
player2vector <- ADEL15_Gg2$player2
ADEL15_Gg3 <- ADEL15_Gg2
ADEL15_Gg3$p1inp2vec <- is.element(ADEL15_Gg3$player1, player2vector)
ADEL15_Gg3$p2inp1vec <- is.element(ADEL15_Gg3$player2, player1vector)

addPlayer1 <- ADEL15_Gg3[ which(ADEL15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_Gg3[ which(ADEL15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_Gg2 <- rbind(ADEL15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
ADEL15_Gft <- ftable(ADEL15_Gg2$player1, ADEL15_Gg2$player2)
ADEL15_Gft2 <- as.matrix(ADEL15_Gft)
numRows <- nrow(ADEL15_Gft2)
numCols <- ncol(ADEL15_Gft2)
ADEL15_Gft3 <- ADEL15_Gft2[c(2:numRows) , c(2:numCols)]
ADEL15_GTable <- graph.adjacency(ADEL15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(ADEL15_GTable, vertex.label = V(ADEL15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
ADEL15_G.clusterCoef <- transitivity(ADEL15_GTable, type="global") #cluster coefficient
ADEL15_G.degreeCent <- centralization.degree(ADEL15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_Gftn <- as.network.matrix(ADEL15_Gft)
ADEL15_G.netDensity <- network.density(ADEL15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_G.entropy <- entropy(ADEL15_Gft) #entropy

ADEL15_G.netMx <- cbind(ADEL15_G.netMx, ADEL15_G.clusterCoef, ADEL15_G.degreeCent$centralization,
                        ADEL15_G.netDensity, ADEL15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
ADEL15_Bg2 <- data.frame(ADEL15_B)
ADEL15_Bg2 <- ADEL15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_Bg2$player1
player2vector <- ADEL15_Bg2$player2
ADEL15_Bg3 <- ADEL15_Bg2
ADEL15_Bg3$p1inp2vec <- is.element(ADEL15_Bg3$player1, player2vector)
ADEL15_Bg3$p2inp1vec <- is.element(ADEL15_Bg3$player2, player1vector)

addPlayer1 <- ADEL15_Bg3[ which(ADEL15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_Bg3[ which(ADEL15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_Bg2 <- rbind(ADEL15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
ADEL15_Bft <- ftable(ADEL15_Bg2$player1, ADEL15_Bg2$player2)
ADEL15_Bft2 <- as.matrix(ADEL15_Bft)
numRows <- nrow(ADEL15_Bft2)
numCols <- ncol(ADEL15_Bft2)
ADEL15_Bft3 <- ADEL15_Bft2[c(2:numRows) , c(2:numCols)]
ADEL15_BTable <- graph.adjacency(ADEL15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(ADEL15_BTable, vertex.label = V(ADEL15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
ADEL15_B.clusterCoef <- transitivity(ADEL15_BTable, type="global") #cluster coefficient
ADEL15_B.degreeCent <- centralization.degree(ADEL15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_Bftn <- as.network.matrix(ADEL15_Bft)
ADEL15_B.netDensity <- network.density(ADEL15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_B.entropy <- entropy(ADEL15_Bft) #entropy

ADEL15_B.netMx <- cbind(ADEL15_B.netMx, ADEL15_B.clusterCoef, ADEL15_B.degreeCent$centralization,
                        ADEL15_B.netDensity, ADEL15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
ADEL15_SFg2 <- data.frame(ADEL15_SF)
ADEL15_SFg2 <- ADEL15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_SFg2$player1
player2vector <- ADEL15_SFg2$player2
ADEL15_SFg3 <- ADEL15_SFg2
ADEL15_SFg3$p1inp2vec <- is.element(ADEL15_SFg3$player1, player2vector)
ADEL15_SFg3$p2inp1vec <- is.element(ADEL15_SFg3$player2, player1vector)

addPlayer1 <- ADEL15_SFg3[ which(ADEL15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_SFg3[ which(ADEL15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_SFg2 <- rbind(ADEL15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
ADEL15_SFft <- ftable(ADEL15_SFg2$player1, ADEL15_SFg2$player2)
ADEL15_SFft2 <- as.matrix(ADEL15_SFft)
numRows <- nrow(ADEL15_SFft2)
numCols <- ncol(ADEL15_SFft2)
ADEL15_SFft3 <- ADEL15_SFft2[c(2:numRows) , c(2:numCols)]
ADEL15_SFTable <- graph.adjacency(ADEL15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(ADEL15_SFTable, vertex.label = V(ADEL15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
ADEL15_SF.clusterCoef <- transitivity(ADEL15_SFTable, type="global") #cluster coefficient
ADEL15_SF.degreeCent <- centralization.degree(ADEL15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_SFftn <- as.network.matrix(ADEL15_SFft)
ADEL15_SF.netDensity <- network.density(ADEL15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_SF.entropy <- entropy(ADEL15_SFft) #entropy

ADEL15_SF.netMx <- cbind(ADEL15_SF.netMx, ADEL15_SF.clusterCoef, ADEL15_SF.degreeCent$centralization,
                         ADEL15_SF.netDensity, ADEL15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
ADEL15_TFg2 <- data.frame(ADEL15_TF)
ADEL15_TFg2 <- ADEL15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_TFg2$player1
player2vector <- ADEL15_TFg2$player2
ADEL15_TFg3 <- ADEL15_TFg2
ADEL15_TFg3$p1inp2vec <- is.element(ADEL15_TFg3$player1, player2vector)
ADEL15_TFg3$p2inp1vec <- is.element(ADEL15_TFg3$player2, player1vector)

addPlayer1 <- ADEL15_TFg3[ which(ADEL15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_TFg3[ which(ADEL15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_TFg2 <- rbind(ADEL15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
ADEL15_TFft <- ftable(ADEL15_TFg2$player1, ADEL15_TFg2$player2)
ADEL15_TFft2 <- as.matrix(ADEL15_TFft)
numRows <- nrow(ADEL15_TFft2)
numCols <- ncol(ADEL15_TFft2)
ADEL15_TFft3 <- ADEL15_TFft2[c(2:numRows) , c(2:numCols)]
ADEL15_TFTable <- graph.adjacency(ADEL15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(ADEL15_TFTable, vertex.label = V(ADEL15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
ADEL15_TF.clusterCoef <- transitivity(ADEL15_TFTable, type="global") #cluster coefficient
ADEL15_TF.degreeCent <- centralization.degree(ADEL15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_TFftn <- as.network.matrix(ADEL15_TFft)
ADEL15_TF.netDensity <- network.density(ADEL15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_TF.entropy <- entropy(ADEL15_TFft) #entropy

ADEL15_TF.netMx <- cbind(ADEL15_TF.netMx, ADEL15_TF.clusterCoef, ADEL15_TF.degreeCent$centralization,
                         ADEL15_TF.netDensity, ADEL15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
ADEL15_SAMg2 <- data.frame(ADEL15_SAM)
ADEL15_SAMg2 <- ADEL15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_SAMg2$player1
player2vector <- ADEL15_SAMg2$player2
ADEL15_SAMg3 <- ADEL15_SAMg2
ADEL15_SAMg3$p1inp2vec <- is.element(ADEL15_SAMg3$player1, player2vector)
ADEL15_SAMg3$p2inp1vec <- is.element(ADEL15_SAMg3$player2, player1vector)

addPlayer1 <- ADEL15_SAMg3[ which(ADEL15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_SAMg3[ which(ADEL15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_SAMg2 <- rbind(ADEL15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
ADEL15_SAMft <- ftable(ADEL15_SAMg2$player1, ADEL15_SAMg2$player2)
ADEL15_SAMft2 <- as.matrix(ADEL15_SAMft)
numRows <- nrow(ADEL15_SAMft2)
numCols <- ncol(ADEL15_SAMft2)
ADEL15_SAMft3 <- ADEL15_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL15_SAMTable <- graph.adjacency(ADEL15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(ADEL15_SAMTable, vertex.label = V(ADEL15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
ADEL15_SAM.clusterCoef <- transitivity(ADEL15_SAMTable, type="global") #cluster coefficient
ADEL15_SAM.degreeCent <- centralization.degree(ADEL15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_SAMftn <- as.network.matrix(ADEL15_SAMft)
ADEL15_SAM.netDensity <- network.density(ADEL15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_SAM.entropy <- entropy(ADEL15_SAMft) #entropy

ADEL15_SAM.netMx <- cbind(ADEL15_SAM.netMx, ADEL15_SAM.clusterCoef, ADEL15_SAM.degreeCent$centralization,
                          ADEL15_SAM.netDensity, ADEL15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************
#NA

round = 15
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
ADEL15_TAMg2 <- data.frame(ADEL15_TAM)
ADEL15_TAMg2 <- ADEL15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_TAMg2$player1
player2vector <- ADEL15_TAMg2$player2
ADEL15_TAMg3 <- ADEL15_TAMg2
ADEL15_TAMg3$p1inp2vec <- is.element(ADEL15_TAMg3$player1, player2vector)
ADEL15_TAMg3$p2inp1vec <- is.element(ADEL15_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL15_TAMg3[ which(ADEL15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL15_TAMg2 <- rbind(ADEL15_TAMg2, addPlayer2)

#ROUND 15, AM Turnover graph using weighted edges
ADEL15_TAMft <- ftable(ADEL15_TAMg2$player1, ADEL15_TAMg2$player2)
ADEL15_TAMft2 <- as.matrix(ADEL15_TAMft)
numRows <- nrow(ADEL15_TAMft2)
numCols <- ncol(ADEL15_TAMft2)
ADEL15_TAMft3 <- ADEL15_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL15_TAMTable <- graph.adjacency(ADEL15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(ADEL15_TAMTable, vertex.label = V(ADEL15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
ADEL15_TAM.clusterCoef <- transitivity(ADEL15_TAMTable, type="global") #cluster coefficient
ADEL15_TAM.degreeCent <- centralization.degree(ADEL15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_TAMftn <- as.network.matrix(ADEL15_TAMft)
ADEL15_TAM.netDensity <- network.density(ADEL15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_TAM.entropy <- entropy(ADEL15_TAMft) #entropy

ADEL15_TAM.netMx <- cbind(ADEL15_TAM.netMx, ADEL15_TAM.clusterCoef, ADEL15_TAM.degreeCent$centralization,
                          ADEL15_TAM.netDensity, ADEL15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
ADEL15_SDMg2 <- data.frame(ADEL15_SDM)
ADEL15_SDMg2 <- ADEL15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_SDMg2$player1
player2vector <- ADEL15_SDMg2$player2
ADEL15_SDMg3 <- ADEL15_SDMg2
ADEL15_SDMg3$p1inp2vec <- is.element(ADEL15_SDMg3$player1, player2vector)
ADEL15_SDMg3$p2inp1vec <- is.element(ADEL15_SDMg3$player2, player1vector)

addPlayer1 <- ADEL15_SDMg3[ which(ADEL15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_SDMg3[ which(ADEL15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_SDMg2 <- rbind(ADEL15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
ADEL15_SDMft <- ftable(ADEL15_SDMg2$player1, ADEL15_SDMg2$player2)
ADEL15_SDMft2 <- as.matrix(ADEL15_SDMft)
numRows <- nrow(ADEL15_SDMft2)
numCols <- ncol(ADEL15_SDMft2)
ADEL15_SDMft3 <- ADEL15_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL15_SDMTable <- graph.adjacency(ADEL15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(ADEL15_SDMTable, vertex.label = V(ADEL15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
ADEL15_SDM.clusterCoef <- transitivity(ADEL15_SDMTable, type="global") #cluster coefficient
ADEL15_SDM.degreeCent <- centralization.degree(ADEL15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_SDMftn <- as.network.matrix(ADEL15_SDMft)
ADEL15_SDM.netDensity <- network.density(ADEL15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_SDM.entropy <- entropy(ADEL15_SDMft) #entropy

ADEL15_SDM.netMx <- cbind(ADEL15_SDM.netMx, ADEL15_SDM.clusterCoef, ADEL15_SDM.degreeCent$centralization,
                          ADEL15_SDM.netDensity, ADEL15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
ADEL15_TDMg2 <- data.frame(ADEL15_TDM)
ADEL15_TDMg2 <- ADEL15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_TDMg2$player1
player2vector <- ADEL15_TDMg2$player2
ADEL15_TDMg3 <- ADEL15_TDMg2
ADEL15_TDMg3$p1inp2vec <- is.element(ADEL15_TDMg3$player1, player2vector)
ADEL15_TDMg3$p2inp1vec <- is.element(ADEL15_TDMg3$player2, player1vector)

addPlayer1 <- ADEL15_TDMg3[ which(ADEL15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_TDMg3[ which(ADEL15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_TDMg2 <- rbind(ADEL15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
ADEL15_TDMft <- ftable(ADEL15_TDMg2$player1, ADEL15_TDMg2$player2)
ADEL15_TDMft2 <- as.matrix(ADEL15_TDMft)
numRows <- nrow(ADEL15_TDMft2)
numCols <- ncol(ADEL15_TDMft2)
ADEL15_TDMft3 <- ADEL15_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL15_TDMTable <- graph.adjacency(ADEL15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(ADEL15_TDMTable, vertex.label = V(ADEL15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
ADEL15_TDM.clusterCoef <- transitivity(ADEL15_TDMTable, type="global") #cluster coefficient
ADEL15_TDM.degreeCent <- centralization.degree(ADEL15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_TDMftn <- as.network.matrix(ADEL15_TDMft)
ADEL15_TDM.netDensity <- network.density(ADEL15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_TDM.entropy <- entropy(ADEL15_TDMft) #entropy

ADEL15_TDM.netMx <- cbind(ADEL15_TDM.netMx, ADEL15_TDM.clusterCoef, ADEL15_TDM.degreeCent$centralization,
                          ADEL15_TDM.netDensity, ADEL15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
ADEL15_SDg2 <- data.frame(ADEL15_SD)
ADEL15_SDg2 <- ADEL15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_SDg2$player1
player2vector <- ADEL15_SDg2$player2
ADEL15_SDg3 <- ADEL15_SDg2
ADEL15_SDg3$p1inp2vec <- is.element(ADEL15_SDg3$player1, player2vector)
ADEL15_SDg3$p2inp1vec <- is.element(ADEL15_SDg3$player2, player1vector)

addPlayer1 <- ADEL15_SDg3[ which(ADEL15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_SDg3[ which(ADEL15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_SDg2 <- rbind(ADEL15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
ADEL15_SDft <- ftable(ADEL15_SDg2$player1, ADEL15_SDg2$player2)
ADEL15_SDft2 <- as.matrix(ADEL15_SDft)
numRows <- nrow(ADEL15_SDft2)
numCols <- ncol(ADEL15_SDft2)
ADEL15_SDft3 <- ADEL15_SDft2[c(2:numRows) , c(2:numCols)]
ADEL15_SDTable <- graph.adjacency(ADEL15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(ADEL15_SDTable, vertex.label = V(ADEL15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
ADEL15_SD.clusterCoef <- transitivity(ADEL15_SDTable, type="global") #cluster coefficient
ADEL15_SD.degreeCent <- centralization.degree(ADEL15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_SDftn <- as.network.matrix(ADEL15_SDft)
ADEL15_SD.netDensity <- network.density(ADEL15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_SD.entropy <- entropy(ADEL15_SDft) #entropy

ADEL15_SD.netMx <- cbind(ADEL15_SD.netMx, ADEL15_SD.clusterCoef, ADEL15_SD.degreeCent$centralization,
                         ADEL15_SD.netDensity, ADEL15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************

round = 15
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
ADEL15_TDg2 <- data.frame(ADEL15_TD)
ADEL15_TDg2 <- ADEL15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_TDg2$player1
player2vector <- ADEL15_TDg2$player2
ADEL15_TDg3 <- ADEL15_TDg2
ADEL15_TDg3$p1inp2vec <- is.element(ADEL15_TDg3$player1, player2vector)
ADEL15_TDg3$p2inp1vec <- is.element(ADEL15_TDg3$player2, player1vector)

addPlayer1 <- ADEL15_TDg3[ which(ADEL15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_TDg3[ which(ADEL15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_TDg2 <- rbind(ADEL15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
ADEL15_TDft <- ftable(ADEL15_TDg2$player1, ADEL15_TDg2$player2)
ADEL15_TDft2 <- as.matrix(ADEL15_TDft)
numRows <- nrow(ADEL15_TDft2)
numCols <- ncol(ADEL15_TDft2)
ADEL15_TDft3 <- ADEL15_TDft2[c(2:numRows) , c(2:numCols)]
ADEL15_TDTable <- graph.adjacency(ADEL15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(ADEL15_TDTable, vertex.label = V(ADEL15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
ADEL15_TD.clusterCoef <- transitivity(ADEL15_TDTable, type="global") #cluster coefficient
ADEL15_TD.degreeCent <- centralization.degree(ADEL15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_TDftn <- as.network.matrix(ADEL15_TDft)
ADEL15_TD.netDensity <- network.density(ADEL15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_TD.entropy <- entropy(ADEL15_TDft) #entropy

ADEL15_TD.netMx <- cbind(ADEL15_TD.netMx, ADEL15_TD.clusterCoef, ADEL15_TD.degreeCent$centralization,
                         ADEL15_TD.netDensity, ADEL15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
ADEL15_QTg2 <- data.frame(ADEL15_QT)
ADEL15_QTg2 <- ADEL15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL15_QTg2$player1
player2vector <- ADEL15_QTg2$player2
ADEL15_QTg3 <- ADEL15_QTg2
ADEL15_QTg3$p1inp2vec <- is.element(ADEL15_QTg3$player1, player2vector)
ADEL15_QTg3$p2inp1vec <- is.element(ADEL15_QTg3$player2, player1vector)

addPlayer1 <- ADEL15_QTg3[ which(ADEL15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL15_QTg3[ which(ADEL15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL15_QTg2 <- rbind(ADEL15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
ADEL15_QTft <- ftable(ADEL15_QTg2$player1, ADEL15_QTg2$player2)
ADEL15_QTft2 <- as.matrix(ADEL15_QTft)
numRows <- nrow(ADEL15_QTft2)
numCols <- ncol(ADEL15_QTft2)
ADEL15_QTft3 <- ADEL15_QTft2[c(2:numRows) , c(2:numCols)]
ADEL15_QTTable <- graph.adjacency(ADEL15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(ADEL15_QTTable, vertex.label = V(ADEL15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
ADEL15_QT.clusterCoef <- transitivity(ADEL15_QTTable, type="global") #cluster coefficient
ADEL15_QT.degreeCent <- centralization.degree(ADEL15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL15_QTftn <- as.network.matrix(ADEL15_QTft)
ADEL15_QT.netDensity <- network.density(ADEL15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL15_QT.entropy <- entropy(ADEL15_QTft) #entropy

ADEL15_QT.netMx <- cbind(ADEL15_QT.netMx, ADEL15_QT.clusterCoef, ADEL15_QT.degreeCent$centralization,
                         ADEL15_QT.netDensity, ADEL15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL15_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "BL"
KIoutcome = "Goal_F"
BL15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
BL15_Gg2 <- data.frame(BL15_G)
BL15_Gg2 <- BL15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_Gg2$player1
player2vector <- BL15_Gg2$player2
BL15_Gg3 <- BL15_Gg2
BL15_Gg3$p1inp2vec <- is.element(BL15_Gg3$player1, player2vector)
BL15_Gg3$p2inp1vec <- is.element(BL15_Gg3$player2, player1vector)

addPlayer1 <- BL15_Gg3[ which(BL15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL15_Gg3[ which(BL15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_Gg2 <- rbind(BL15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
BL15_Gft <- ftable(BL15_Gg2$player1, BL15_Gg2$player2)
BL15_Gft2 <- as.matrix(BL15_Gft)
numRows <- nrow(BL15_Gft2)
numCols <- ncol(BL15_Gft2)
BL15_Gft3 <- BL15_Gft2[c(2:numRows) , c(2:numCols)]
BL15_GTable <- graph.adjacency(BL15_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(BL15_GTable, vertex.label = V(BL15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
BL15_G.clusterCoef <- transitivity(BL15_GTable, type="global") #cluster coefficient
BL15_G.degreeCent <- centralization.degree(BL15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_Gftn <- as.network.matrix(BL15_Gft)
BL15_G.netDensity <- network.density(BL15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_G.entropy <- entropy(BL15_Gft) #entropy

BL15_G.netMx <- cbind(BL15_G.netMx, BL15_G.clusterCoef, BL15_G.degreeCent$centralization,
                      BL15_G.netDensity, BL15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************

round = 15
teamName = "BL"
KIoutcome = "Behind_F"
BL15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
BL15_Bg2 <- data.frame(BL15_B)
BL15_Bg2 <- BL15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_Bg2$player1
player2vector <- BL15_Bg2$player2
BL15_Bg3 <- BL15_Bg2
BL15_Bg3$p1inp2vec <- is.element(BL15_Bg3$player1, player2vector)
BL15_Bg3$p2inp1vec <- is.element(BL15_Bg3$player2, player1vector)

addPlayer1 <- BL15_Bg3[ which(BL15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_Bg3[ which(BL15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_Bg2 <- rbind(BL15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
BL15_Bft <- ftable(BL15_Bg2$player1, BL15_Bg2$player2)
BL15_Bft2 <- as.matrix(BL15_Bft)
numRows <- nrow(BL15_Bft2)
numCols <- ncol(BL15_Bft2)
BL15_Bft3 <- BL15_Bft2[c(2:numRows) , c(2:numCols)]
BL15_BTable <- graph.adjacency(BL15_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(BL15_BTable, vertex.label = V(BL15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
BL15_B.clusterCoef <- transitivity(BL15_BTable, type="global") #cluster coefficient
BL15_B.degreeCent <- centralization.degree(BL15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_Bftn <- as.network.matrix(BL15_Bft)
BL15_B.netDensity <- network.density(BL15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_B.entropy <- entropy(BL15_Bft) #entropy

BL15_B.netMx <- cbind(BL15_B.netMx, BL15_B.clusterCoef, BL15_B.degreeCent$centralization,
                      BL15_B.netDensity, BL15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "BL"
KIoutcome = "Stoppage_F"
BL15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
BL15_SFg2 <- data.frame(BL15_SF)
BL15_SFg2 <- BL15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_SFg2$player1
player2vector <- BL15_SFg2$player2
BL15_SFg3 <- BL15_SFg2
BL15_SFg3$p1inp2vec <- is.element(BL15_SFg3$player1, player2vector)
BL15_SFg3$p2inp1vec <- is.element(BL15_SFg3$player2, player1vector)

addPlayer1 <- BL15_SFg3[ which(BL15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_SFg3[ which(BL15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_SFg2 <- rbind(BL15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
BL15_SFft <- ftable(BL15_SFg2$player1, BL15_SFg2$player2)
BL15_SFft2 <- as.matrix(BL15_SFft)
numRows <- nrow(BL15_SFft2)
numCols <- ncol(BL15_SFft2)
BL15_SFft3 <- BL15_SFft2[c(2:numRows) , c(2:numCols)]
BL15_SFTable <- graph.adjacency(BL15_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(BL15_SFTable, vertex.label = V(BL15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
BL15_SF.clusterCoef <- transitivity(BL15_SFTable, type="global") #cluster coefficient
BL15_SF.degreeCent <- centralization.degree(BL15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_SFftn <- as.network.matrix(BL15_SFft)
BL15_SF.netDensity <- network.density(BL15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_SF.entropy <- entropy(BL15_SFft) #entropy

BL15_SF.netMx <- cbind(BL15_SF.netMx, BL15_SF.clusterCoef, BL15_SF.degreeCent$centralization,
                       BL15_SF.netDensity, BL15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "BL"
KIoutcome = "Turnover_F"
BL15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
BL15_TFg2 <- data.frame(BL15_TF)
BL15_TFg2 <- BL15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_TFg2$player1
player2vector <- BL15_TFg2$player2
BL15_TFg3 <- BL15_TFg2
BL15_TFg3$p1inp2vec <- is.element(BL15_TFg3$player1, player2vector)
BL15_TFg3$p2inp1vec <- is.element(BL15_TFg3$player2, player1vector)

addPlayer1 <- BL15_TFg3[ which(BL15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_TFg3[ which(BL15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_TFg2 <- rbind(BL15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
BL15_TFft <- ftable(BL15_TFg2$player1, BL15_TFg2$player2)
BL15_TFft2 <- as.matrix(BL15_TFft)
numRows <- nrow(BL15_TFft2)
numCols <- ncol(BL15_TFft2)
BL15_TFft3 <- BL15_TFft2[c(2:numRows) , c(2:numCols)]
BL15_TFTable <- graph.adjacency(BL15_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(BL15_TFTable, vertex.label = V(BL15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
BL15_TF.clusterCoef <- transitivity(BL15_TFTable, type="global") #cluster coefficient
BL15_TF.degreeCent <- centralization.degree(BL15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_TFftn <- as.network.matrix(BL15_TFft)
BL15_TF.netDensity <- network.density(BL15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_TF.entropy <- entropy(BL15_TFft) #entropy

BL15_TF.netMx <- cbind(BL15_TF.netMx, BL15_TF.clusterCoef, BL15_TF.degreeCent$centralization,
                       BL15_TF.netDensity, BL15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
BL15_SAMg2 <- data.frame(BL15_SAM)
BL15_SAMg2 <- BL15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_SAMg2$player1
player2vector <- BL15_SAMg2$player2
BL15_SAMg3 <- BL15_SAMg2
BL15_SAMg3$p1inp2vec <- is.element(BL15_SAMg3$player1, player2vector)
BL15_SAMg3$p2inp1vec <- is.element(BL15_SAMg3$player2, player1vector)

addPlayer1 <- BL15_SAMg3[ which(BL15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_SAMg3[ which(BL15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_SAMg2 <- rbind(BL15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
BL15_SAMft <- ftable(BL15_SAMg2$player1, BL15_SAMg2$player2)
BL15_SAMft2 <- as.matrix(BL15_SAMft)
numRows <- nrow(BL15_SAMft2)
numCols <- ncol(BL15_SAMft2)
BL15_SAMft3 <- BL15_SAMft2[c(2:numRows) , c(2:numCols)]
BL15_SAMTable <- graph.adjacency(BL15_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(BL15_SAMTable, vertex.label = V(BL15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
BL15_SAM.clusterCoef <- transitivity(BL15_SAMTable, type="global") #cluster coefficient
BL15_SAM.degreeCent <- centralization.degree(BL15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_SAMftn <- as.network.matrix(BL15_SAMft)
BL15_SAM.netDensity <- network.density(BL15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_SAM.entropy <- entropy(BL15_SAMft) #entropy

BL15_SAM.netMx <- cbind(BL15_SAM.netMx, BL15_SAM.clusterCoef, BL15_SAM.degreeCent$centralization,
                        BL15_SAM.netDensity, BL15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************
#NA

round = 15
teamName = "BL"
KIoutcome = "Turnover_AM"
BL15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
BL15_TAMg2 <- data.frame(BL15_TAM)
BL15_TAMg2 <- BL15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_TAMg2$player1
player2vector <- BL15_TAMg2$player2
BL15_TAMg3 <- BL15_TAMg2
BL15_TAMg3$p1inp2vec <- is.element(BL15_TAMg3$player1, player2vector)
BL15_TAMg3$p2inp1vec <- is.element(BL15_TAMg3$player2, player1vector)

addPlayer1 <- BL15_TAMg3[ which(BL15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_TAMg3[ which(BL15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_TAMg2 <- rbind(BL15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
BL15_TAMft <- ftable(BL15_TAMg2$player1, BL15_TAMg2$player2)
BL15_TAMft2 <- as.matrix(BL15_TAMft)
numRows <- nrow(BL15_TAMft2)
numCols <- ncol(BL15_TAMft2)
BL15_TAMft3 <- BL15_TAMft2[c(2:numRows) , c(2:numCols)]
BL15_TAMTable <- graph.adjacency(BL15_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(BL15_TAMTable, vertex.label = V(BL15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
BL15_TAM.clusterCoef <- transitivity(BL15_TAMTable, type="global") #cluster coefficient
BL15_TAM.degreeCent <- centralization.degree(BL15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_TAMftn <- as.network.matrix(BL15_TAMft)
BL15_TAM.netDensity <- network.density(BL15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_TAM.entropy <- entropy(BL15_TAMft) #entropy

BL15_TAM.netMx <- cbind(BL15_TAM.netMx, BL15_TAM.clusterCoef, BL15_TAM.degreeCent$centralization,
                        BL15_TAM.netDensity, BL15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
BL15_SDMg2 <- data.frame(BL15_SDM)
BL15_SDMg2 <- BL15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_SDMg2$player1
player2vector <- BL15_SDMg2$player2
BL15_SDMg3 <- BL15_SDMg2
BL15_SDMg3$p1inp2vec <- is.element(BL15_SDMg3$player1, player2vector)
BL15_SDMg3$p2inp1vec <- is.element(BL15_SDMg3$player2, player1vector)

addPlayer1 <- BL15_SDMg3[ which(BL15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_SDMg3[ which(BL15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_SDMg2 <- rbind(BL15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
BL15_SDMft <- ftable(BL15_SDMg2$player1, BL15_SDMg2$player2)
BL15_SDMft2 <- as.matrix(BL15_SDMft)
numRows <- nrow(BL15_SDMft2)
numCols <- ncol(BL15_SDMft2)
BL15_SDMft3 <- BL15_SDMft2[c(2:numRows) , c(2:numCols)]
BL15_SDMTable <- graph.adjacency(BL15_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(BL15_SDMTable, vertex.label = V(BL15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
BL15_SDM.clusterCoef <- transitivity(BL15_SDMTable, type="global") #cluster coefficient
BL15_SDM.degreeCent <- centralization.degree(BL15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_SDMftn <- as.network.matrix(BL15_SDMft)
BL15_SDM.netDensity <- network.density(BL15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_SDM.entropy <- entropy(BL15_SDMft) #entropy

BL15_SDM.netMx <- cbind(BL15_SDM.netMx, BL15_SDM.clusterCoef, BL15_SDM.degreeCent$centralization,
                        BL15_SDM.netDensity, BL15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "BL"
KIoutcome = "Turnover_DM"
BL15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
BL15_TDMg2 <- data.frame(BL15_TDM)
BL15_TDMg2 <- BL15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_TDMg2$player1
player2vector <- BL15_TDMg2$player2
BL15_TDMg3 <- BL15_TDMg2
BL15_TDMg3$p1inp2vec <- is.element(BL15_TDMg3$player1, player2vector)
BL15_TDMg3$p2inp1vec <- is.element(BL15_TDMg3$player2, player1vector)

addPlayer1 <- BL15_TDMg3[ which(BL15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_TDMg3[ which(BL15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_TDMg2 <- rbind(BL15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
BL15_TDMft <- ftable(BL15_TDMg2$player1, BL15_TDMg2$player2)
BL15_TDMft2 <- as.matrix(BL15_TDMft)
numRows <- nrow(BL15_TDMft2)
numCols <- ncol(BL15_TDMft2)
BL15_TDMft3 <- BL15_TDMft2[c(2:numRows) , c(2:numCols)]
BL15_TDMTable <- graph.adjacency(BL15_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(BL15_TDMTable, vertex.label = V(BL15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
BL15_TDM.clusterCoef <- transitivity(BL15_TDMTable, type="global") #cluster coefficient
BL15_TDM.degreeCent <- centralization.degree(BL15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_TDMftn <- as.network.matrix(BL15_TDMft)
BL15_TDM.netDensity <- network.density(BL15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_TDM.entropy <- entropy(BL15_TDMft) #entropy

BL15_TDM.netMx <- cbind(BL15_TDM.netMx, BL15_TDM.clusterCoef, BL15_TDM.degreeCent$centralization,
                        BL15_TDM.netDensity, BL15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************

round = 15
teamName = "BL"
KIoutcome = "Stoppage_D"
BL15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
BL15_SDg2 <- data.frame(BL15_SD)
BL15_SDg2 <- BL15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_SDg2$player1
player2vector <- BL15_SDg2$player2
BL15_SDg3 <- BL15_SDg2
BL15_SDg3$p1inp2vec <- is.element(BL15_SDg3$player1, player2vector)
BL15_SDg3$p2inp1vec <- is.element(BL15_SDg3$player2, player1vector)

addPlayer1 <- BL15_SDg3[ which(BL15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_SDg3[ which(BL15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_SDg2 <- rbind(BL15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
BL15_SDft <- ftable(BL15_SDg2$player1, BL15_SDg2$player2)
BL15_SDft2 <- as.matrix(BL15_SDft)
numRows <- nrow(BL15_SDft2)
numCols <- ncol(BL15_SDft2)
BL15_SDft3 <- BL15_SDft2[c(2:numRows) , c(2:numCols)]
BL15_SDTable <- graph.adjacency(BL15_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(BL15_SDTable, vertex.label = V(BL15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
BL15_SD.clusterCoef <- transitivity(BL15_SDTable, type="global") #cluster coefficient
BL15_SD.degreeCent <- centralization.degree(BL15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_SDftn <- as.network.matrix(BL15_SDft)
BL15_SD.netDensity <- network.density(BL15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_SD.entropy <- entropy(BL15_SDft) #entropy

BL15_SD.netMx <- cbind(BL15_SD.netMx, BL15_SD.clusterCoef, BL15_SD.degreeCent$centralization,
                       BL15_SD.netDensity, BL15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "BL"
KIoutcome = "Turnover_D"
BL15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
BL15_TDg2 <- data.frame(BL15_TD)
BL15_TDg2 <- BL15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_TDg2$player1
player2vector <- BL15_TDg2$player2
BL15_TDg3 <- BL15_TDg2
BL15_TDg3$p1inp2vec <- is.element(BL15_TDg3$player1, player2vector)
BL15_TDg3$p2inp1vec <- is.element(BL15_TDg3$player2, player1vector)

addPlayer1 <- BL15_TDg3[ which(BL15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_TDg3[ which(BL15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_TDg2 <- rbind(BL15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
BL15_TDft <- ftable(BL15_TDg2$player1, BL15_TDg2$player2)
BL15_TDft2 <- as.matrix(BL15_TDft)
numRows <- nrow(BL15_TDft2)
numCols <- ncol(BL15_TDft2)
BL15_TDft3 <- BL15_TDft2[c(2:numRows) , c(2:numCols)]
BL15_TDTable <- graph.adjacency(BL15_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(BL15_TDTable, vertex.label = V(BL15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
BL15_TD.clusterCoef <- transitivity(BL15_TDTable, type="global") #cluster coefficient
BL15_TD.degreeCent <- centralization.degree(BL15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_TDftn <- as.network.matrix(BL15_TDft)
BL15_TD.netDensity <- network.density(BL15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_TD.entropy <- entropy(BL15_TDft) #entropy

BL15_TD.netMx <- cbind(BL15_TD.netMx, BL15_TD.clusterCoef, BL15_TD.degreeCent$centralization,
                       BL15_TD.netDensity, BL15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
BL15_QTg2 <- data.frame(BL15_QT)
BL15_QTg2 <- BL15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL15_QTg2$player1
player2vector <- BL15_QTg2$player2
BL15_QTg3 <- BL15_QTg2
BL15_QTg3$p1inp2vec <- is.element(BL15_QTg3$player1, player2vector)
BL15_QTg3$p2inp1vec <- is.element(BL15_QTg3$player2, player1vector)

addPlayer1 <- BL15_QTg3[ which(BL15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL15_QTg3[ which(BL15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL15_QTg2 <- rbind(BL15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
BL15_QTft <- ftable(BL15_QTg2$player1, BL15_QTg2$player2)
BL15_QTft2 <- as.matrix(BL15_QTft)
numRows <- nrow(BL15_QTft2)
numCols <- ncol(BL15_QTft2)
BL15_QTft3 <- BL15_QTft2[c(2:numRows) , c(2:numCols)]
BL15_QTTable <- graph.adjacency(BL15_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(BL15_QTTable, vertex.label = V(BL15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
BL15_QT.clusterCoef <- transitivity(BL15_QTTable, type="global") #cluster coefficient
BL15_QT.degreeCent <- centralization.degree(BL15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL15_QTftn <- as.network.matrix(BL15_QTft)
BL15_QT.netDensity <- network.density(BL15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL15_QT.entropy <- entropy(BL15_QTft) #entropy

BL15_QT.netMx <- cbind(BL15_QT.netMx, BL15_QT.clusterCoef, BL15_QT.degreeCent$centralization,
                       BL15_QT.netDensity, BL15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL15_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "CARL"
KIoutcome = "Goal_F"
CARL15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
CARL15_Gg2 <- data.frame(CARL15_G)
CARL15_Gg2 <- CARL15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_Gg2$player1
player2vector <- CARL15_Gg2$player2
CARL15_Gg3 <- CARL15_Gg2
CARL15_Gg3$p1inp2vec <- is.element(CARL15_Gg3$player1, player2vector)
CARL15_Gg3$p2inp1vec <- is.element(CARL15_Gg3$player2, player1vector)

addPlayer1 <- CARL15_Gg3[ which(CARL15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_Gg3[ which(CARL15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_Gg2 <- rbind(CARL15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
CARL15_Gft <- ftable(CARL15_Gg2$player1, CARL15_Gg2$player2)
CARL15_Gft2 <- as.matrix(CARL15_Gft)
numRows <- nrow(CARL15_Gft2)
numCols <- ncol(CARL15_Gft2)
CARL15_Gft3 <- CARL15_Gft2[c(2:numRows) , c(2:numCols)]
CARL15_GTable <- graph.adjacency(CARL15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(CARL15_GTable, vertex.label = V(CARL15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
CARL15_G.clusterCoef <- transitivity(CARL15_GTable, type="global") #cluster coefficient
CARL15_G.degreeCent <- centralization.degree(CARL15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_Gftn <- as.network.matrix(CARL15_Gft)
CARL15_G.netDensity <- network.density(CARL15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_G.entropy <- entropy(CARL15_Gft) #entropy

CARL15_G.netMx <- cbind(CARL15_G.netMx, CARL15_G.clusterCoef, CARL15_G.degreeCent$centralization,
                        CARL15_G.netDensity, CARL15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "Behind_F"
CARL15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
CARL15_Bg2 <- data.frame(CARL15_B)
CARL15_Bg2 <- CARL15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_Bg2$player1
player2vector <- CARL15_Bg2$player2
CARL15_Bg3 <- CARL15_Bg2
CARL15_Bg3$p1inp2vec <- is.element(CARL15_Bg3$player1, player2vector)
CARL15_Bg3$p2inp1vec <- is.element(CARL15_Bg3$player2, player1vector)

addPlayer1 <- CARL15_Bg3[ which(CARL15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_Bg3[ which(CARL15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_Bg2 <- rbind(CARL15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
CARL15_Bft <- ftable(CARL15_Bg2$player1, CARL15_Bg2$player2)
CARL15_Bft2 <- as.matrix(CARL15_Bft)
numRows <- nrow(CARL15_Bft2)
numCols <- ncol(CARL15_Bft2)
CARL15_Bft3 <- CARL15_Bft2[c(2:numRows) , c(2:numCols)]
CARL15_BTable <- graph.adjacency(CARL15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(CARL15_BTable, vertex.label = V(CARL15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
CARL15_B.clusterCoef <- transitivity(CARL15_BTable, type="global") #cluster coefficient
CARL15_B.degreeCent <- centralization.degree(CARL15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_Bftn <- as.network.matrix(CARL15_Bft)
CARL15_B.netDensity <- network.density(CARL15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_B.entropy <- entropy(CARL15_Bft) #entropy

CARL15_B.netMx <- cbind(CARL15_B.netMx, CARL15_B.clusterCoef, CARL15_B.degreeCent$centralization,
                        CARL15_B.netDensity, CARL15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************

round = 15
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
CARL15_SFg2 <- data.frame(CARL15_SF)
CARL15_SFg2 <- CARL15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_SFg2$player1
player2vector <- CARL15_SFg2$player2
CARL15_SFg3 <- CARL15_SFg2
CARL15_SFg3$p1inp2vec <- is.element(CARL15_SFg3$player1, player2vector)
CARL15_SFg3$p2inp1vec <- is.element(CARL15_SFg3$player2, player1vector)

addPlayer1 <- CARL15_SFg3[ which(CARL15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_SFg3[ which(CARL15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_SFg2 <- rbind(CARL15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
CARL15_SFft <- ftable(CARL15_SFg2$player1, CARL15_SFg2$player2)
CARL15_SFft2 <- as.matrix(CARL15_SFft)
numRows <- nrow(CARL15_SFft2)
numCols <- ncol(CARL15_SFft2)
CARL15_SFft3 <- CARL15_SFft2[c(2:numRows) , c(2:numCols)]
CARL15_SFTable <- graph.adjacency(CARL15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(CARL15_SFTable, vertex.label = V(CARL15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
CARL15_SF.clusterCoef <- transitivity(CARL15_SFTable, type="global") #cluster coefficient
CARL15_SF.degreeCent <- centralization.degree(CARL15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_SFftn <- as.network.matrix(CARL15_SFft)
CARL15_SF.netDensity <- network.density(CARL15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_SF.entropy <- entropy(CARL15_SFft) #entropy

CARL15_SF.netMx <- cbind(CARL15_SF.netMx, CARL15_SF.clusterCoef, CARL15_SF.degreeCent$centralization,
                         CARL15_SF.netDensity, CARL15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
CARL15_TFg2 <- data.frame(CARL15_TF)
CARL15_TFg2 <- CARL15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_TFg2$player1
player2vector <- CARL15_TFg2$player2
CARL15_TFg3 <- CARL15_TFg2
CARL15_TFg3$p1inp2vec <- is.element(CARL15_TFg3$player1, player2vector)
CARL15_TFg3$p2inp1vec <- is.element(CARL15_TFg3$player2, player1vector)

addPlayer1 <- CARL15_TFg3[ which(CARL15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_TFg3[ which(CARL15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_TFg2 <- rbind(CARL15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
CARL15_TFft <- ftable(CARL15_TFg2$player1, CARL15_TFg2$player2)
CARL15_TFft2 <- as.matrix(CARL15_TFft)
numRows <- nrow(CARL15_TFft2)
numCols <- ncol(CARL15_TFft2)
CARL15_TFft3 <- CARL15_TFft2[c(2:numRows) , c(2:numCols)]
CARL15_TFTable <- graph.adjacency(CARL15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(CARL15_TFTable, vertex.label = V(CARL15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
CARL15_TF.clusterCoef <- transitivity(CARL15_TFTable, type="global") #cluster coefficient
CARL15_TF.degreeCent <- centralization.degree(CARL15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_TFftn <- as.network.matrix(CARL15_TFft)
CARL15_TF.netDensity <- network.density(CARL15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_TF.entropy <- entropy(CARL15_TFft) #entropy

CARL15_TF.netMx <- cbind(CARL15_TF.netMx, CARL15_TF.clusterCoef, CARL15_TF.degreeCent$centralization,
                         CARL15_TF.netDensity, CARL15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
CARL15_SAMg2 <- data.frame(CARL15_SAM)
CARL15_SAMg2 <- CARL15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_SAMg2$player1
player2vector <- CARL15_SAMg2$player2
CARL15_SAMg3 <- CARL15_SAMg2
CARL15_SAMg3$p1inp2vec <- is.element(CARL15_SAMg3$player1, player2vector)
CARL15_SAMg3$p2inp1vec <- is.element(CARL15_SAMg3$player2, player1vector)

addPlayer1 <- CARL15_SAMg3[ which(CARL15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_SAMg3[ which(CARL15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_SAMg2 <- rbind(CARL15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
CARL15_SAMft <- ftable(CARL15_SAMg2$player1, CARL15_SAMg2$player2)
CARL15_SAMft2 <- as.matrix(CARL15_SAMft)
numRows <- nrow(CARL15_SAMft2)
numCols <- ncol(CARL15_SAMft2)
CARL15_SAMft3 <- CARL15_SAMft2[c(2:numRows) , c(2:numCols)]
CARL15_SAMTable <- graph.adjacency(CARL15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(CARL15_SAMTable, vertex.label = V(CARL15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
CARL15_SAM.clusterCoef <- transitivity(CARL15_SAMTable, type="global") #cluster coefficient
CARL15_SAM.degreeCent <- centralization.degree(CARL15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_SAMftn <- as.network.matrix(CARL15_SAMft)
CARL15_SAM.netDensity <- network.density(CARL15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_SAM.entropy <- entropy(CARL15_SAMft) #entropy

CARL15_SAM.netMx <- cbind(CARL15_SAM.netMx, CARL15_SAM.clusterCoef, CARL15_SAM.degreeCent$centralization,
                          CARL15_SAM.netDensity, CARL15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
CARL15_TAMg2 <- data.frame(CARL15_TAM)
CARL15_TAMg2 <- CARL15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_TAMg2$player1
player2vector <- CARL15_TAMg2$player2
CARL15_TAMg3 <- CARL15_TAMg2
CARL15_TAMg3$p1inp2vec <- is.element(CARL15_TAMg3$player1, player2vector)
CARL15_TAMg3$p2inp1vec <- is.element(CARL15_TAMg3$player2, player1vector)

addPlayer1 <- CARL15_TAMg3[ which(CARL15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_TAMg3[ which(CARL15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_TAMg2 <- rbind(CARL15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
CARL15_TAMft <- ftable(CARL15_TAMg2$player1, CARL15_TAMg2$player2)
CARL15_TAMft2 <- as.matrix(CARL15_TAMft)
numRows <- nrow(CARL15_TAMft2)
numCols <- ncol(CARL15_TAMft2)
CARL15_TAMft3 <- CARL15_TAMft2[c(2:numRows) , c(2:numCols)]
CARL15_TAMTable <- graph.adjacency(CARL15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(CARL15_TAMTable, vertex.label = V(CARL15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
CARL15_TAM.clusterCoef <- transitivity(CARL15_TAMTable, type="global") #cluster coefficient
CARL15_TAM.degreeCent <- centralization.degree(CARL15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_TAMftn <- as.network.matrix(CARL15_TAMft)
CARL15_TAM.netDensity <- network.density(CARL15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_TAM.entropy <- entropy(CARL15_TAMft) #entropy

CARL15_TAM.netMx <- cbind(CARL15_TAM.netMx, CARL15_TAM.clusterCoef, CARL15_TAM.degreeCent$centralization,
                          CARL15_TAM.netDensity, CARL15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
CARL15_SDMg2 <- data.frame(CARL15_SDM)
CARL15_SDMg2 <- CARL15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_SDMg2$player1
player2vector <- CARL15_SDMg2$player2
CARL15_SDMg3 <- CARL15_SDMg2
CARL15_SDMg3$p1inp2vec <- is.element(CARL15_SDMg3$player1, player2vector)
CARL15_SDMg3$p2inp1vec <- is.element(CARL15_SDMg3$player2, player1vector)

addPlayer1 <- CARL15_SDMg3[ which(CARL15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_SDMg3[ which(CARL15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_SDMg2 <- rbind(CARL15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
CARL15_SDMft <- ftable(CARL15_SDMg2$player1, CARL15_SDMg2$player2)
CARL15_SDMft2 <- as.matrix(CARL15_SDMft)
numRows <- nrow(CARL15_SDMft2)
numCols <- ncol(CARL15_SDMft2)
CARL15_SDMft3 <- CARL15_SDMft2[c(2:numRows) , c(2:numCols)]
CARL15_SDMTable <- graph.adjacency(CARL15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(CARL15_SDMTable, vertex.label = V(CARL15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
CARL15_SDM.clusterCoef <- transitivity(CARL15_SDMTable, type="global") #cluster coefficient
CARL15_SDM.degreeCent <- centralization.degree(CARL15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_SDMftn <- as.network.matrix(CARL15_SDMft)
CARL15_SDM.netDensity <- network.density(CARL15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_SDM.entropy <- entropy(CARL15_SDMft) #entropy

CARL15_SDM.netMx <- cbind(CARL15_SDM.netMx, CARL15_SDM.clusterCoef, CARL15_SDM.degreeCent$centralization,
                          CARL15_SDM.netDensity, CARL15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
CARL15_TDMg2 <- data.frame(CARL15_TDM)
CARL15_TDMg2 <- CARL15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_TDMg2$player1
player2vector <- CARL15_TDMg2$player2
CARL15_TDMg3 <- CARL15_TDMg2
CARL15_TDMg3$p1inp2vec <- is.element(CARL15_TDMg3$player1, player2vector)
CARL15_TDMg3$p2inp1vec <- is.element(CARL15_TDMg3$player2, player1vector)

addPlayer1 <- CARL15_TDMg3[ which(CARL15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_TDMg3[ which(CARL15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_TDMg2 <- rbind(CARL15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
CARL15_TDMft <- ftable(CARL15_TDMg2$player1, CARL15_TDMg2$player2)
CARL15_TDMft2 <- as.matrix(CARL15_TDMft)
numRows <- nrow(CARL15_TDMft2)
numCols <- ncol(CARL15_TDMft2)
CARL15_TDMft3 <- CARL15_TDMft2[c(2:numRows) , c(2:numCols)]
CARL15_TDMTable <- graph.adjacency(CARL15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(CARL15_TDMTable, vertex.label = V(CARL15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
CARL15_TDM.clusterCoef <- transitivity(CARL15_TDMTable, type="global") #cluster coefficient
CARL15_TDM.degreeCent <- centralization.degree(CARL15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_TDMftn <- as.network.matrix(CARL15_TDMft)
CARL15_TDM.netDensity <- network.density(CARL15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_TDM.entropy <- entropy(CARL15_TDMft) #entropy

CARL15_TDM.netMx <- cbind(CARL15_TDM.netMx, CARL15_TDM.clusterCoef, CARL15_TDM.degreeCent$centralization,
                          CARL15_TDM.netDensity, CARL15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
CARL15_SDg2 <- data.frame(CARL15_SD)
CARL15_SDg2 <- CARL15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_SDg2$player1
player2vector <- CARL15_SDg2$player2
CARL15_SDg3 <- CARL15_SDg2
CARL15_SDg3$p1inp2vec <- is.element(CARL15_SDg3$player1, player2vector)
CARL15_SDg3$p2inp1vec <- is.element(CARL15_SDg3$player2, player1vector)

addPlayer1 <- CARL15_SDg3[ which(CARL15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_SDg3[ which(CARL15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_SDg2 <- rbind(CARL15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
CARL15_SDft <- ftable(CARL15_SDg2$player1, CARL15_SDg2$player2)
CARL15_SDft2 <- as.matrix(CARL15_SDft)
numRows <- nrow(CARL15_SDft2)
numCols <- ncol(CARL15_SDft2)
CARL15_SDft3 <- CARL15_SDft2[c(2:numRows) , c(2:numCols)]
CARL15_SDTable <- graph.adjacency(CARL15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(CARL15_SDTable, vertex.label = V(CARL15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
CARL15_SD.clusterCoef <- transitivity(CARL15_SDTable, type="global") #cluster coefficient
CARL15_SD.degreeCent <- centralization.degree(CARL15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_SDftn <- as.network.matrix(CARL15_SDft)
CARL15_SD.netDensity <- network.density(CARL15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_SD.entropy <- entropy(CARL15_SDft) #entropy

CARL15_SD.netMx <- cbind(CARL15_SD.netMx, CARL15_SD.clusterCoef, CARL15_SD.degreeCent$centralization,
                         CARL15_SD.netDensity, CARL15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
CARL15_TDg2 <- data.frame(CARL15_TD)
CARL15_TDg2 <- CARL15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_TDg2$player1
player2vector <- CARL15_TDg2$player2
CARL15_TDg3 <- CARL15_TDg2
CARL15_TDg3$p1inp2vec <- is.element(CARL15_TDg3$player1, player2vector)
CARL15_TDg3$p2inp1vec <- is.element(CARL15_TDg3$player2, player1vector)

addPlayer1 <- CARL15_TDg3[ which(CARL15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_TDg3[ which(CARL15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_TDg2 <- rbind(CARL15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
CARL15_TDft <- ftable(CARL15_TDg2$player1, CARL15_TDg2$player2)
CARL15_TDft2 <- as.matrix(CARL15_TDft)
numRows <- nrow(CARL15_TDft2)
numCols <- ncol(CARL15_TDft2)
CARL15_TDft3 <- CARL15_TDft2[c(2:numRows) , c(2:numCols)]
CARL15_TDTable <- graph.adjacency(CARL15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(CARL15_TDTable, vertex.label = V(CARL15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
CARL15_TD.clusterCoef <- transitivity(CARL15_TDTable, type="global") #cluster coefficient
CARL15_TD.degreeCent <- centralization.degree(CARL15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_TDftn <- as.network.matrix(CARL15_TDft)
CARL15_TD.netDensity <- network.density(CARL15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_TD.entropy <- entropy(CARL15_TDft) #entropy

CARL15_TD.netMx <- cbind(CARL15_TD.netMx, CARL15_TD.clusterCoef, CARL15_TD.degreeCent$centralization,
                         CARL15_TD.netDensity, CARL15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
CARL15_QTg2 <- data.frame(CARL15_QT)
CARL15_QTg2 <- CARL15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL15_QTg2$player1
player2vector <- CARL15_QTg2$player2
CARL15_QTg3 <- CARL15_QTg2
CARL15_QTg3$p1inp2vec <- is.element(CARL15_QTg3$player1, player2vector)
CARL15_QTg3$p2inp1vec <- is.element(CARL15_QTg3$player2, player1vector)

addPlayer1 <- CARL15_QTg3[ which(CARL15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL15_QTg3[ which(CARL15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL15_QTg2 <- rbind(CARL15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
CARL15_QTft <- ftable(CARL15_QTg2$player1, CARL15_QTg2$player2)
CARL15_QTft2 <- as.matrix(CARL15_QTft)
numRows <- nrow(CARL15_QTft2)
numCols <- ncol(CARL15_QTft2)
CARL15_QTft3 <- CARL15_QTft2[c(2:numRows) , c(2:numCols)]
CARL15_QTTable <- graph.adjacency(CARL15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(CARL15_QTTable, vertex.label = V(CARL15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
CARL15_QT.clusterCoef <- transitivity(CARL15_QTTable, type="global") #cluster coefficient
CARL15_QT.degreeCent <- centralization.degree(CARL15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL15_QTftn <- as.network.matrix(CARL15_QTft)
CARL15_QT.netDensity <- network.density(CARL15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL15_QT.entropy <- entropy(CARL15_QTft) #entropy

CARL15_QT.netMx <- cbind(CARL15_QT.netMx, CARL15_QT.clusterCoef, CARL15_QT.degreeCent$centralization,
                         CARL15_QT.netDensity, CARL15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL15_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Goal_F"
COLL15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
COLL15_Gg2 <- data.frame(COLL15_G)
COLL15_Gg2 <- COLL15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_Gg2$player1
player2vector <- COLL15_Gg2$player2
COLL15_Gg3 <- COLL15_Gg2
COLL15_Gg3$p1inp2vec <- is.element(COLL15_Gg3$player1, player2vector)
COLL15_Gg3$p2inp1vec <- is.element(COLL15_Gg3$player2, player1vector)

addPlayer1 <- COLL15_Gg3[ which(COLL15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_Gg3[ which(COLL15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_Gg2 <- rbind(COLL15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
COLL15_Gft <- ftable(COLL15_Gg2$player1, COLL15_Gg2$player2)
COLL15_Gft2 <- as.matrix(COLL15_Gft)
numRows <- nrow(COLL15_Gft2)
numCols <- ncol(COLL15_Gft2)
COLL15_Gft3 <- COLL15_Gft2[c(2:numRows) , c(2:numCols)]
COLL15_GTable <- graph.adjacency(COLL15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(COLL15_GTable, vertex.label = V(COLL15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
COLL15_G.clusterCoef <- transitivity(COLL15_GTable, type="global") #cluster coefficient
COLL15_G.degreeCent <- centralization.degree(COLL15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_Gftn <- as.network.matrix(COLL15_Gft)
COLL15_G.netDensity <- network.density(COLL15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_G.entropy <- entropy(COLL15_Gft) #entropy

COLL15_G.netMx <- cbind(COLL15_G.netMx, COLL15_G.clusterCoef, COLL15_G.degreeCent$centralization,
                        COLL15_G.netDensity, COLL15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Behind_F"
COLL15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
COLL15_Bg2 <- data.frame(COLL15_B)
COLL15_Bg2 <- COLL15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_Bg2$player1
player2vector <- COLL15_Bg2$player2
COLL15_Bg3 <- COLL15_Bg2
COLL15_Bg3$p1inp2vec <- is.element(COLL15_Bg3$player1, player2vector)
COLL15_Bg3$p2inp1vec <- is.element(COLL15_Bg3$player2, player1vector)

addPlayer1 <- COLL15_Bg3[ which(COLL15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_Bg3[ which(COLL15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_Bg2 <- rbind(COLL15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
COLL15_Bft <- ftable(COLL15_Bg2$player1, COLL15_Bg2$player2)
COLL15_Bft2 <- as.matrix(COLL15_Bft)
numRows <- nrow(COLL15_Bft2)
numCols <- ncol(COLL15_Bft2)
COLL15_Bft3 <- COLL15_Bft2[c(2:numRows) , c(2:numCols)]
COLL15_BTable <- graph.adjacency(COLL15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(COLL15_BTable, vertex.label = V(COLL15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
COLL15_B.clusterCoef <- transitivity(COLL15_BTable, type="global") #cluster coefficient
COLL15_B.degreeCent <- centralization.degree(COLL15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_Bftn <- as.network.matrix(COLL15_Bft)
COLL15_B.netDensity <- network.density(COLL15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_B.entropy <- entropy(COLL15_Bft) #entropy

COLL15_B.netMx <- cbind(COLL15_B.netMx, COLL15_B.clusterCoef, COLL15_B.degreeCent$centralization,
                        COLL15_B.netDensity, COLL15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
COLL15_SFg2 <- data.frame(COLL15_SF)
COLL15_SFg2 <- COLL15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_SFg2$player1
player2vector <- COLL15_SFg2$player2
COLL15_SFg3 <- COLL15_SFg2
COLL15_SFg3$p1inp2vec <- is.element(COLL15_SFg3$player1, player2vector)
COLL15_SFg3$p2inp1vec <- is.element(COLL15_SFg3$player2, player1vector)

addPlayer1 <- COLL15_SFg3[ which(COLL15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_SFg3[ which(COLL15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_SFg2 <- rbind(COLL15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
COLL15_SFft <- ftable(COLL15_SFg2$player1, COLL15_SFg2$player2)
COLL15_SFft2 <- as.matrix(COLL15_SFft)
numRows <- nrow(COLL15_SFft2)
numCols <- ncol(COLL15_SFft2)
COLL15_SFft3 <- COLL15_SFft2[c(2:numRows) , c(2:numCols)]
COLL15_SFTable <- graph.adjacency(COLL15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(COLL15_SFTable, vertex.label = V(COLL15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
COLL15_SF.clusterCoef <- transitivity(COLL15_SFTable, type="global") #cluster coefficient
COLL15_SF.degreeCent <- centralization.degree(COLL15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_SFftn <- as.network.matrix(COLL15_SFft)
COLL15_SF.netDensity <- network.density(COLL15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_SF.entropy <- entropy(COLL15_SFft) #entropy

COLL15_SF.netMx <- cbind(COLL15_SF.netMx, COLL15_SF.clusterCoef, COLL15_SF.degreeCent$centralization,
                         COLL15_SF.netDensity, COLL15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
COLL15_TFg2 <- data.frame(COLL15_TF)
COLL15_TFg2 <- COLL15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_TFg2$player1
player2vector <- COLL15_TFg2$player2
COLL15_TFg3 <- COLL15_TFg2
COLL15_TFg3$p1inp2vec <- is.element(COLL15_TFg3$player1, player2vector)
COLL15_TFg3$p2inp1vec <- is.element(COLL15_TFg3$player2, player1vector)

addPlayer1 <- COLL15_TFg3[ which(COLL15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_TFg3[ which(COLL15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_TFg2 <- rbind(COLL15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
COLL15_TFft <- ftable(COLL15_TFg2$player1, COLL15_TFg2$player2)
COLL15_TFft2 <- as.matrix(COLL15_TFft)
numRows <- nrow(COLL15_TFft2)
numCols <- ncol(COLL15_TFft2)
COLL15_TFft3 <- COLL15_TFft2[c(2:numRows) , c(2:numCols)]
COLL15_TFTable <- graph.adjacency(COLL15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(COLL15_TFTable, vertex.label = V(COLL15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
COLL15_TF.clusterCoef <- transitivity(COLL15_TFTable, type="global") #cluster coefficient
COLL15_TF.degreeCent <- centralization.degree(COLL15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_TFftn <- as.network.matrix(COLL15_TFft)
COLL15_TF.netDensity <- network.density(COLL15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_TF.entropy <- entropy(COLL15_TFft) #entropy

COLL15_TF.netMx <- cbind(COLL15_TF.netMx, COLL15_TF.clusterCoef, COLL15_TF.degreeCent$centralization,
                         COLL15_TF.netDensity, COLL15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
COLL15_SAMg2 <- data.frame(COLL15_SAM)
COLL15_SAMg2 <- COLL15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_SAMg2$player1
player2vector <- COLL15_SAMg2$player2
COLL15_SAMg3 <- COLL15_SAMg2
COLL15_SAMg3$p1inp2vec <- is.element(COLL15_SAMg3$player1, player2vector)
COLL15_SAMg3$p2inp1vec <- is.element(COLL15_SAMg3$player2, player1vector)

addPlayer1 <- COLL15_SAMg3[ which(COLL15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_SAMg3[ which(COLL15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_SAMg2 <- rbind(COLL15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
COLL15_SAMft <- ftable(COLL15_SAMg2$player1, COLL15_SAMg2$player2)
COLL15_SAMft2 <- as.matrix(COLL15_SAMft)
numRows <- nrow(COLL15_SAMft2)
numCols <- ncol(COLL15_SAMft2)
COLL15_SAMft3 <- COLL15_SAMft2[c(2:numRows) , c(2:numCols)]
COLL15_SAMTable <- graph.adjacency(COLL15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(COLL15_SAMTable, vertex.label = V(COLL15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
COLL15_SAM.clusterCoef <- transitivity(COLL15_SAMTable, type="global") #cluster coefficient
COLL15_SAM.degreeCent <- centralization.degree(COLL15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_SAMftn <- as.network.matrix(COLL15_SAMft)
COLL15_SAM.netDensity <- network.density(COLL15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_SAM.entropy <- entropy(COLL15_SAMft) #entropy

COLL15_SAM.netMx <- cbind(COLL15_SAM.netMx, COLL15_SAM.clusterCoef, COLL15_SAM.degreeCent$centralization,
                          COLL15_SAM.netDensity, COLL15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
COLL15_TAMg2 <- data.frame(COLL15_TAM)
COLL15_TAMg2 <- COLL15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_TAMg2$player1
player2vector <- COLL15_TAMg2$player2
COLL15_TAMg3 <- COLL15_TAMg2
COLL15_TAMg3$p1inp2vec <- is.element(COLL15_TAMg3$player1, player2vector)
COLL15_TAMg3$p2inp1vec <- is.element(COLL15_TAMg3$player2, player1vector)

addPlayer1 <- COLL15_TAMg3[ which(COLL15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_TAMg3[ which(COLL15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_TAMg2 <- rbind(COLL15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
COLL15_TAMft <- ftable(COLL15_TAMg2$player1, COLL15_TAMg2$player2)
COLL15_TAMft2 <- as.matrix(COLL15_TAMft)
numRows <- nrow(COLL15_TAMft2)
numCols <- ncol(COLL15_TAMft2)
COLL15_TAMft3 <- COLL15_TAMft2[c(2:numRows) , c(2:numCols)]
COLL15_TAMTable <- graph.adjacency(COLL15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(COLL15_TAMTable, vertex.label = V(COLL15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
COLL15_TAM.clusterCoef <- transitivity(COLL15_TAMTable, type="global") #cluster coefficient
COLL15_TAM.degreeCent <- centralization.degree(COLL15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_TAMftn <- as.network.matrix(COLL15_TAMft)
COLL15_TAM.netDensity <- network.density(COLL15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_TAM.entropy <- entropy(COLL15_TAMft) #entropy

COLL15_TAM.netMx <- cbind(COLL15_TAM.netMx, COLL15_TAM.clusterCoef, COLL15_TAM.degreeCent$centralization,
                          COLL15_TAM.netDensity, COLL15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
COLL15_SDMg2 <- data.frame(COLL15_SDM)
COLL15_SDMg2 <- COLL15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_SDMg2$player1
player2vector <- COLL15_SDMg2$player2
COLL15_SDMg3 <- COLL15_SDMg2
COLL15_SDMg3$p1inp2vec <- is.element(COLL15_SDMg3$player1, player2vector)
COLL15_SDMg3$p2inp1vec <- is.element(COLL15_SDMg3$player2, player1vector)

addPlayer1 <- COLL15_SDMg3[ which(COLL15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_SDMg3[ which(COLL15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_SDMg2 <- rbind(COLL15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
COLL15_SDMft <- ftable(COLL15_SDMg2$player1, COLL15_SDMg2$player2)
COLL15_SDMft2 <- as.matrix(COLL15_SDMft)
numRows <- nrow(COLL15_SDMft2)
numCols <- ncol(COLL15_SDMft2)
COLL15_SDMft3 <- COLL15_SDMft2[c(2:numRows) , c(2:numCols)]
COLL15_SDMTable <- graph.adjacency(COLL15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(COLL15_SDMTable, vertex.label = V(COLL15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
COLL15_SDM.clusterCoef <- transitivity(COLL15_SDMTable, type="global") #cluster coefficient
COLL15_SDM.degreeCent <- centralization.degree(COLL15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_SDMftn <- as.network.matrix(COLL15_SDMft)
COLL15_SDM.netDensity <- network.density(COLL15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_SDM.entropy <- entropy(COLL15_SDMft) #entropy

COLL15_SDM.netMx <- cbind(COLL15_SDM.netMx, COLL15_SDM.clusterCoef, COLL15_SDM.degreeCent$centralization,
                          COLL15_SDM.netDensity, COLL15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
COLL15_TDMg2 <- data.frame(COLL15_TDM)
COLL15_TDMg2 <- COLL15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_TDMg2$player1
player2vector <- COLL15_TDMg2$player2
COLL15_TDMg3 <- COLL15_TDMg2
COLL15_TDMg3$p1inp2vec <- is.element(COLL15_TDMg3$player1, player2vector)
COLL15_TDMg3$p2inp1vec <- is.element(COLL15_TDMg3$player2, player1vector)

addPlayer1 <- COLL15_TDMg3[ which(COLL15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_TDMg3[ which(COLL15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_TDMg2 <- rbind(COLL15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
COLL15_TDMft <- ftable(COLL15_TDMg2$player1, COLL15_TDMg2$player2)
COLL15_TDMft2 <- as.matrix(COLL15_TDMft)
numRows <- nrow(COLL15_TDMft2)
numCols <- ncol(COLL15_TDMft2)
COLL15_TDMft3 <- COLL15_TDMft2[c(2:numRows) , c(2:numCols)]
COLL15_TDMTable <- graph.adjacency(COLL15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(COLL15_TDMTable, vertex.label = V(COLL15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
COLL15_TDM.clusterCoef <- transitivity(COLL15_TDMTable, type="global") #cluster coefficient
COLL15_TDM.degreeCent <- centralization.degree(COLL15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_TDMftn <- as.network.matrix(COLL15_TDMft)
COLL15_TDM.netDensity <- network.density(COLL15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_TDM.entropy <- entropy(COLL15_TDMft) #entropy

COLL15_TDM.netMx <- cbind(COLL15_TDM.netMx, COLL15_TDM.clusterCoef, COLL15_TDM.degreeCent$centralization,
                          COLL15_TDM.netDensity, COLL15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
COLL15_SDg2 <- data.frame(COLL15_SD)
COLL15_SDg2 <- COLL15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_SDg2$player1
player2vector <- COLL15_SDg2$player2
COLL15_SDg3 <- COLL15_SDg2
COLL15_SDg3$p1inp2vec <- is.element(COLL15_SDg3$player1, player2vector)
COLL15_SDg3$p2inp1vec <- is.element(COLL15_SDg3$player2, player1vector)

addPlayer1 <- COLL15_SDg3[ which(COLL15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_SDg3[ which(COLL15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_SDg2 <- rbind(COLL15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
COLL15_SDft <- ftable(COLL15_SDg2$player1, COLL15_SDg2$player2)
COLL15_SDft2 <- as.matrix(COLL15_SDft)
numRows <- nrow(COLL15_SDft2)
numCols <- ncol(COLL15_SDft2)
COLL15_SDft3 <- COLL15_SDft2[c(2:numRows) , c(2:numCols)]
COLL15_SDTable <- graph.adjacency(COLL15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(COLL15_SDTable, vertex.label = V(COLL15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
COLL15_SD.clusterCoef <- transitivity(COLL15_SDTable, type="global") #cluster coefficient
COLL15_SD.degreeCent <- centralization.degree(COLL15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_SDftn <- as.network.matrix(COLL15_SDft)
COLL15_SD.netDensity <- network.density(COLL15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_SD.entropy <- entropy(COLL15_SDft) #entropy

COLL15_SD.netMx <- cbind(COLL15_SD.netMx, COLL15_SD.clusterCoef, COLL15_SD.degreeCent$centralization,
                         COLL15_SD.netDensity, COLL15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
COLL15_TDg2 <- data.frame(COLL15_TD)
COLL15_TDg2 <- COLL15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_TDg2$player1
player2vector <- COLL15_TDg2$player2
COLL15_TDg3 <- COLL15_TDg2
COLL15_TDg3$p1inp2vec <- is.element(COLL15_TDg3$player1, player2vector)
COLL15_TDg3$p2inp1vec <- is.element(COLL15_TDg3$player2, player1vector)

addPlayer1 <- COLL15_TDg3[ which(COLL15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_TDg3[ which(COLL15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_TDg2 <- rbind(COLL15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
COLL15_TDft <- ftable(COLL15_TDg2$player1, COLL15_TDg2$player2)
COLL15_TDft2 <- as.matrix(COLL15_TDft)
numRows <- nrow(COLL15_TDft2)
numCols <- ncol(COLL15_TDft2)
COLL15_TDft3 <- COLL15_TDft2[c(2:numRows) , c(2:numCols)]
COLL15_TDTable <- graph.adjacency(COLL15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(COLL15_TDTable, vertex.label = V(COLL15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
COLL15_TD.clusterCoef <- transitivity(COLL15_TDTable, type="global") #cluster coefficient
COLL15_TD.degreeCent <- centralization.degree(COLL15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_TDftn <- as.network.matrix(COLL15_TDft)
COLL15_TD.netDensity <- network.density(COLL15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_TD.entropy <- entropy(COLL15_TDft) #entropy

COLL15_TD.netMx <- cbind(COLL15_TD.netMx, COLL15_TD.clusterCoef, COLL15_TD.degreeCent$centralization,
                         COLL15_TD.netDensity, COLL15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
COLL15_QTg2 <- data.frame(COLL15_QT)
COLL15_QTg2 <- COLL15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL15_QTg2$player1
player2vector <- COLL15_QTg2$player2
COLL15_QTg3 <- COLL15_QTg2
COLL15_QTg3$p1inp2vec <- is.element(COLL15_QTg3$player1, player2vector)
COLL15_QTg3$p2inp1vec <- is.element(COLL15_QTg3$player2, player1vector)

addPlayer1 <- COLL15_QTg3[ which(COLL15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL15_QTg3[ which(COLL15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL15_QTg2 <- rbind(COLL15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
COLL15_QTft <- ftable(COLL15_QTg2$player1, COLL15_QTg2$player2)
COLL15_QTft2 <- as.matrix(COLL15_QTft)
numRows <- nrow(COLL15_QTft2)
numCols <- ncol(COLL15_QTft2)
COLL15_QTft3 <- COLL15_QTft2[c(2:numRows) , c(2:numCols)]
COLL15_QTTable <- graph.adjacency(COLL15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(COLL15_QTTable, vertex.label = V(COLL15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
COLL15_QT.clusterCoef <- transitivity(COLL15_QTTable, type="global") #cluster coefficient
COLL15_QT.degreeCent <- centralization.degree(COLL15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL15_QTftn <- as.network.matrix(COLL15_QTft)
COLL15_QT.netDensity <- network.density(COLL15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL15_QT.entropy <- entropy(COLL15_QTft) #entropy

COLL15_QT.netMx <- cbind(COLL15_QT.netMx, COLL15_QT.clusterCoef, COLL15_QT.degreeCent$centralization,
                         COLL15_QT.netDensity, COLL15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL15_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "Goal_F"
ESS15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
ESS15_Gg2 <- data.frame(ESS15_G)
ESS15_Gg2 <- ESS15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_Gg2$player1
player2vector <- ESS15_Gg2$player2
ESS15_Gg3 <- ESS15_Gg2
ESS15_Gg3$p1inp2vec <- is.element(ESS15_Gg3$player1, player2vector)
ESS15_Gg3$p2inp1vec <- is.element(ESS15_Gg3$player2, player1vector)

addPlayer1 <- ESS15_Gg3[ which(ESS15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_Gg3[ which(ESS15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_Gg2 <- rbind(ESS15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
ESS15_Gft <- ftable(ESS15_Gg2$player1, ESS15_Gg2$player2)
ESS15_Gft2 <- as.matrix(ESS15_Gft)
numRows <- nrow(ESS15_Gft2)
numCols <- ncol(ESS15_Gft2)
ESS15_Gft3 <- ESS15_Gft2[c(2:numRows) , c(2:numCols)]
ESS15_GTable <- graph.adjacency(ESS15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(ESS15_GTable, vertex.label = V(ESS15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
ESS15_G.clusterCoef <- transitivity(ESS15_GTable, type="global") #cluster coefficient
ESS15_G.degreeCent <- centralization.degree(ESS15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_Gftn <- as.network.matrix(ESS15_Gft)
ESS15_G.netDensity <- network.density(ESS15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_G.entropy <- entropy(ESS15_Gft) #entropy

ESS15_G.netMx <- cbind(ESS15_G.netMx, ESS15_G.clusterCoef, ESS15_G.degreeCent$centralization,
                       ESS15_G.netDensity, ESS15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "Behind_F"
ESS15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
ESS15_Bg2 <- data.frame(ESS15_B)
ESS15_Bg2 <- ESS15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_Bg2$player1
player2vector <- ESS15_Bg2$player2
ESS15_Bg3 <- ESS15_Bg2
ESS15_Bg3$p1inp2vec <- is.element(ESS15_Bg3$player1, player2vector)
ESS15_Bg3$p2inp1vec <- is.element(ESS15_Bg3$player2, player1vector)

addPlayer1 <- ESS15_Bg3[ which(ESS15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_Bg3[ which(ESS15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_Bg2 <- rbind(ESS15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
ESS15_Bft <- ftable(ESS15_Bg2$player1, ESS15_Bg2$player2)
ESS15_Bft2 <- as.matrix(ESS15_Bft)
numRows <- nrow(ESS15_Bft2)
numCols <- ncol(ESS15_Bft2)
ESS15_Bft3 <- ESS15_Bft2[c(2:numRows) , c(2:numCols)]
ESS15_BTable <- graph.adjacency(ESS15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(ESS15_BTable, vertex.label = V(ESS15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
ESS15_B.clusterCoef <- transitivity(ESS15_BTable, type="global") #cluster coefficient
ESS15_B.degreeCent <- centralization.degree(ESS15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_Bftn <- as.network.matrix(ESS15_Bft)
ESS15_B.netDensity <- network.density(ESS15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_B.entropy <- entropy(ESS15_Bft) #entropy

ESS15_B.netMx <- cbind(ESS15_B.netMx, ESS15_B.clusterCoef, ESS15_B.degreeCent$centralization,
                       ESS15_B.netDensity, ESS15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
ESS15_SFg2 <- data.frame(ESS15_SF)
ESS15_SFg2 <- ESS15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_SFg2$player1
player2vector <- ESS15_SFg2$player2
ESS15_SFg3 <- ESS15_SFg2
ESS15_SFg3$p1inp2vec <- is.element(ESS15_SFg3$player1, player2vector)
ESS15_SFg3$p2inp1vec <- is.element(ESS15_SFg3$player2, player1vector)

addPlayer1 <- ESS15_SFg3[ which(ESS15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_SFg3[ which(ESS15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_SFg2 <- rbind(ESS15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
ESS15_SFft <- ftable(ESS15_SFg2$player1, ESS15_SFg2$player2)
ESS15_SFft2 <- as.matrix(ESS15_SFft)
numRows <- nrow(ESS15_SFft2)
numCols <- ncol(ESS15_SFft2)
ESS15_SFft3 <- ESS15_SFft2[c(2:numRows) , c(2:numCols)]
ESS15_SFTable <- graph.adjacency(ESS15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(ESS15_SFTable, vertex.label = V(ESS15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
ESS15_SF.clusterCoef <- transitivity(ESS15_SFTable, type="global") #cluster coefficient
ESS15_SF.degreeCent <- centralization.degree(ESS15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_SFftn <- as.network.matrix(ESS15_SFft)
ESS15_SF.netDensity <- network.density(ESS15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_SF.entropy <- entropy(ESS15_SFft) #entropy

ESS15_SF.netMx <- cbind(ESS15_SF.netMx, ESS15_SF.clusterCoef, ESS15_SF.degreeCent$centralization,
                        ESS15_SF.netDensity, ESS15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
ESS15_TFg2 <- data.frame(ESS15_TF)
ESS15_TFg2 <- ESS15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_TFg2$player1
player2vector <- ESS15_TFg2$player2
ESS15_TFg3 <- ESS15_TFg2
ESS15_TFg3$p1inp2vec <- is.element(ESS15_TFg3$player1, player2vector)
ESS15_TFg3$p2inp1vec <- is.element(ESS15_TFg3$player2, player1vector)

addPlayer1 <- ESS15_TFg3[ which(ESS15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_TFg3[ which(ESS15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_TFg2 <- rbind(ESS15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
ESS15_TFft <- ftable(ESS15_TFg2$player1, ESS15_TFg2$player2)
ESS15_TFft2 <- as.matrix(ESS15_TFft)
numRows <- nrow(ESS15_TFft2)
numCols <- ncol(ESS15_TFft2)
ESS15_TFft3 <- ESS15_TFft2[c(2:numRows) , c(2:numCols)]
ESS15_TFTable <- graph.adjacency(ESS15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(ESS15_TFTable, vertex.label = V(ESS15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
ESS15_TF.clusterCoef <- transitivity(ESS15_TFTable, type="global") #cluster coefficient
ESS15_TF.degreeCent <- centralization.degree(ESS15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_TFftn <- as.network.matrix(ESS15_TFft)
ESS15_TF.netDensity <- network.density(ESS15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_TF.entropy <- entropy(ESS15_TFft) #entropy

ESS15_TF.netMx <- cbind(ESS15_TF.netMx, ESS15_TF.clusterCoef, ESS15_TF.degreeCent$centralization,
                        ESS15_TF.netDensity, ESS15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
ESS15_SAMg2 <- data.frame(ESS15_SAM)
ESS15_SAMg2 <- ESS15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_SAMg2$player1
player2vector <- ESS15_SAMg2$player2
ESS15_SAMg3 <- ESS15_SAMg2
ESS15_SAMg3$p1inp2vec <- is.element(ESS15_SAMg3$player1, player2vector)
ESS15_SAMg3$p2inp1vec <- is.element(ESS15_SAMg3$player2, player1vector)

addPlayer1 <- ESS15_SAMg3[ which(ESS15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_SAMg3[ which(ESS15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_SAMg2 <- rbind(ESS15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
ESS15_SAMft <- ftable(ESS15_SAMg2$player1, ESS15_SAMg2$player2)
ESS15_SAMft2 <- as.matrix(ESS15_SAMft)
numRows <- nrow(ESS15_SAMft2)
numCols <- ncol(ESS15_SAMft2)
ESS15_SAMft3 <- ESS15_SAMft2[c(2:numRows) , c(2:numCols)]
ESS15_SAMTable <- graph.adjacency(ESS15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(ESS15_SAMTable, vertex.label = V(ESS15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
ESS15_SAM.clusterCoef <- transitivity(ESS15_SAMTable, type="global") #cluster coefficient
ESS15_SAM.degreeCent <- centralization.degree(ESS15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_SAMftn <- as.network.matrix(ESS15_SAMft)
ESS15_SAM.netDensity <- network.density(ESS15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_SAM.entropy <- entropy(ESS15_SAMft) #entropy

ESS15_SAM.netMx <- cbind(ESS15_SAM.netMx, ESS15_SAM.clusterCoef, ESS15_SAM.degreeCent$centralization,
                         ESS15_SAM.netDensity, ESS15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
ESS15_TAMg2 <- data.frame(ESS15_TAM)
ESS15_TAMg2 <- ESS15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_TAMg2$player1
player2vector <- ESS15_TAMg2$player2
ESS15_TAMg3 <- ESS15_TAMg2
ESS15_TAMg3$p1inp2vec <- is.element(ESS15_TAMg3$player1, player2vector)
ESS15_TAMg3$p2inp1vec <- is.element(ESS15_TAMg3$player2, player1vector)

addPlayer1 <- ESS15_TAMg3[ which(ESS15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_TAMg3[ which(ESS15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_TAMg2 <- rbind(ESS15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
ESS15_TAMft <- ftable(ESS15_TAMg2$player1, ESS15_TAMg2$player2)
ESS15_TAMft2 <- as.matrix(ESS15_TAMft)
numRows <- nrow(ESS15_TAMft2)
numCols <- ncol(ESS15_TAMft2)
ESS15_TAMft3 <- ESS15_TAMft2[c(2:numRows) , c(2:numCols)]
ESS15_TAMTable <- graph.adjacency(ESS15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(ESS15_TAMTable, vertex.label = V(ESS15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
ESS15_TAM.clusterCoef <- transitivity(ESS15_TAMTable, type="global") #cluster coefficient
ESS15_TAM.degreeCent <- centralization.degree(ESS15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_TAMftn <- as.network.matrix(ESS15_TAMft)
ESS15_TAM.netDensity <- network.density(ESS15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_TAM.entropy <- entropy(ESS15_TAMft) #entropy

ESS15_TAM.netMx <- cbind(ESS15_TAM.netMx, ESS15_TAM.clusterCoef, ESS15_TAM.degreeCent$centralization,
                         ESS15_TAM.netDensity, ESS15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
ESS15_SDMg2 <- data.frame(ESS15_SDM)
ESS15_SDMg2 <- ESS15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_SDMg2$player1
player2vector <- ESS15_SDMg2$player2
ESS15_SDMg3 <- ESS15_SDMg2
ESS15_SDMg3$p1inp2vec <- is.element(ESS15_SDMg3$player1, player2vector)
ESS15_SDMg3$p2inp1vec <- is.element(ESS15_SDMg3$player2, player1vector)

addPlayer1 <- ESS15_SDMg3[ which(ESS15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_SDMg3[ which(ESS15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_SDMg2 <- rbind(ESS15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
ESS15_SDMft <- ftable(ESS15_SDMg2$player1, ESS15_SDMg2$player2)
ESS15_SDMft2 <- as.matrix(ESS15_SDMft)
numRows <- nrow(ESS15_SDMft2)
numCols <- ncol(ESS15_SDMft2)
ESS15_SDMft3 <- ESS15_SDMft2[c(2:numRows) , c(2:numCols)]
ESS15_SDMTable <- graph.adjacency(ESS15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(ESS15_SDMTable, vertex.label = V(ESS15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
ESS15_SDM.clusterCoef <- transitivity(ESS15_SDMTable, type="global") #cluster coefficient
ESS15_SDM.degreeCent <- centralization.degree(ESS15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_SDMftn <- as.network.matrix(ESS15_SDMft)
ESS15_SDM.netDensity <- network.density(ESS15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_SDM.entropy <- entropy(ESS15_SDMft) #entropy

ESS15_SDM.netMx <- cbind(ESS15_SDM.netMx, ESS15_SDM.clusterCoef, ESS15_SDM.degreeCent$centralization,
                         ESS15_SDM.netDensity, ESS15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
ESS15_TDMg2 <- data.frame(ESS15_TDM)
ESS15_TDMg2 <- ESS15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_TDMg2$player1
player2vector <- ESS15_TDMg2$player2
ESS15_TDMg3 <- ESS15_TDMg2
ESS15_TDMg3$p1inp2vec <- is.element(ESS15_TDMg3$player1, player2vector)
ESS15_TDMg3$p2inp1vec <- is.element(ESS15_TDMg3$player2, player1vector)

addPlayer1 <- ESS15_TDMg3[ which(ESS15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_TDMg3[ which(ESS15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_TDMg2 <- rbind(ESS15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
ESS15_TDMft <- ftable(ESS15_TDMg2$player1, ESS15_TDMg2$player2)
ESS15_TDMft2 <- as.matrix(ESS15_TDMft)
numRows <- nrow(ESS15_TDMft2)
numCols <- ncol(ESS15_TDMft2)
ESS15_TDMft3 <- ESS15_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS15_TDMTable <- graph.adjacency(ESS15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(ESS15_TDMTable, vertex.label = V(ESS15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
ESS15_TDM.clusterCoef <- transitivity(ESS15_TDMTable, type="global") #cluster coefficient
ESS15_TDM.degreeCent <- centralization.degree(ESS15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_TDMftn <- as.network.matrix(ESS15_TDMft)
ESS15_TDM.netDensity <- network.density(ESS15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_TDM.entropy <- entropy(ESS15_TDMft) #entropy

ESS15_TDM.netMx <- cbind(ESS15_TDM.netMx, ESS15_TDM.clusterCoef, ESS15_TDM.degreeCent$centralization,
                         ESS15_TDM.netDensity, ESS15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
ESS15_SDg2 <- data.frame(ESS15_SD)
ESS15_SDg2 <- ESS15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_SDg2$player1
player2vector <- ESS15_SDg2$player2
ESS15_SDg3 <- ESS15_SDg2
ESS15_SDg3$p1inp2vec <- is.element(ESS15_SDg3$player1, player2vector)
ESS15_SDg3$p2inp1vec <- is.element(ESS15_SDg3$player2, player1vector)

addPlayer1 <- ESS15_SDg3[ which(ESS15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_SDg3[ which(ESS15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_SDg2 <- rbind(ESS15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
ESS15_SDft <- ftable(ESS15_SDg2$player1, ESS15_SDg2$player2)
ESS15_SDft2 <- as.matrix(ESS15_SDft)
numRows <- nrow(ESS15_SDft2)
numCols <- ncol(ESS15_SDft2)
ESS15_SDft3 <- ESS15_SDft2[c(2:numRows) , c(2:numCols)]
ESS15_SDTable <- graph.adjacency(ESS15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(ESS15_SDTable, vertex.label = V(ESS15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
ESS15_SD.clusterCoef <- transitivity(ESS15_SDTable, type="global") #cluster coefficient
ESS15_SD.degreeCent <- centralization.degree(ESS15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_SDftn <- as.network.matrix(ESS15_SDft)
ESS15_SD.netDensity <- network.density(ESS15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_SD.entropy <- entropy(ESS15_SDft) #entropy

ESS15_SD.netMx <- cbind(ESS15_SD.netMx, ESS15_SD.clusterCoef, ESS15_SD.degreeCent$centralization,
                        ESS15_SD.netDensity, ESS15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************

round = 15
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
ESS15_TDg2 <- data.frame(ESS15_TD)
ESS15_TDg2 <- ESS15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_TDg2$player1
player2vector <- ESS15_TDg2$player2
ESS15_TDg3 <- ESS15_TDg2
ESS15_TDg3$p1inp2vec <- is.element(ESS15_TDg3$player1, player2vector)
ESS15_TDg3$p2inp1vec <- is.element(ESS15_TDg3$player2, player1vector)

addPlayer1 <- ESS15_TDg3[ which(ESS15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_TDg3[ which(ESS15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_TDg2 <- rbind(ESS15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
ESS15_TDft <- ftable(ESS15_TDg2$player1, ESS15_TDg2$player2)
ESS15_TDft2 <- as.matrix(ESS15_TDft)
numRows <- nrow(ESS15_TDft2)
numCols <- ncol(ESS15_TDft2)
ESS15_TDft3 <- ESS15_TDft2[c(2:numRows) , c(2:numCols)]
ESS15_TDTable <- graph.adjacency(ESS15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(ESS15_TDTable, vertex.label = V(ESS15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
ESS15_TD.clusterCoef <- transitivity(ESS15_TDTable, type="global") #cluster coefficient
ESS15_TD.degreeCent <- centralization.degree(ESS15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_TDftn <- as.network.matrix(ESS15_TDft)
ESS15_TD.netDensity <- network.density(ESS15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_TD.entropy <- entropy(ESS15_TDft) #entropy

ESS15_TD.netMx <- cbind(ESS15_TD.netMx, ESS15_TD.clusterCoef, ESS15_TD.degreeCent$centralization,
                        ESS15_TD.netDensity, ESS15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
ESS15_QTg2 <- data.frame(ESS15_QT)
ESS15_QTg2 <- ESS15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS15_QTg2$player1
player2vector <- ESS15_QTg2$player2
ESS15_QTg3 <- ESS15_QTg2
ESS15_QTg3$p1inp2vec <- is.element(ESS15_QTg3$player1, player2vector)
ESS15_QTg3$p2inp1vec <- is.element(ESS15_QTg3$player2, player1vector)

addPlayer1 <- ESS15_QTg3[ which(ESS15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS15_QTg3[ which(ESS15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS15_QTg2 <- rbind(ESS15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
ESS15_QTft <- ftable(ESS15_QTg2$player1, ESS15_QTg2$player2)
ESS15_QTft2 <- as.matrix(ESS15_QTft)
numRows <- nrow(ESS15_QTft2)
numCols <- ncol(ESS15_QTft2)
ESS15_QTft3 <- ESS15_QTft2[c(2:numRows) , c(2:numCols)]
ESS15_QTTable <- graph.adjacency(ESS15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(ESS15_QTTable, vertex.label = V(ESS15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
ESS15_QT.clusterCoef <- transitivity(ESS15_QTTable, type="global") #cluster coefficient
ESS15_QT.degreeCent <- centralization.degree(ESS15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS15_QTftn <- as.network.matrix(ESS15_QTft)
ESS15_QT.netDensity <- network.density(ESS15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS15_QT.entropy <- entropy(ESS15_QTft) #entropy

ESS15_QT.netMx <- cbind(ESS15_QT.netMx, ESS15_QT.clusterCoef, ESS15_QT.degreeCent$centralization,
                        ESS15_QT.netDensity, ESS15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS15_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "FRE"
KIoutcome = "Goal_F"
FRE15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
FRE15_Gg2 <- data.frame(FRE15_G)
FRE15_Gg2 <- FRE15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_Gg2$player1
player2vector <- FRE15_Gg2$player2
FRE15_Gg3 <- FRE15_Gg2
FRE15_Gg3$p1inp2vec <- is.element(FRE15_Gg3$player1, player2vector)
FRE15_Gg3$p2inp1vec <- is.element(FRE15_Gg3$player2, player1vector)

addPlayer1 <- FRE15_Gg3[ which(FRE15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_Gg3[ which(FRE15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_Gg2 <- rbind(FRE15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
FRE15_Gft <- ftable(FRE15_Gg2$player1, FRE15_Gg2$player2)
FRE15_Gft2 <- as.matrix(FRE15_Gft)
numRows <- nrow(FRE15_Gft2)
numCols <- ncol(FRE15_Gft2)
FRE15_Gft3 <- FRE15_Gft2[c(2:numRows) , c(2:numCols)]
FRE15_GTable <- graph.adjacency(FRE15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(FRE15_GTable, vertex.label = V(FRE15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
FRE15_G.clusterCoef <- transitivity(FRE15_GTable, type="global") #cluster coefficient
FRE15_G.degreeCent <- centralization.degree(FRE15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_Gftn <- as.network.matrix(FRE15_Gft)
FRE15_G.netDensity <- network.density(FRE15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_G.entropy <- entropy(FRE15_Gft) #entropy

FRE15_G.netMx <- cbind(FRE15_G.netMx, FRE15_G.clusterCoef, FRE15_G.degreeCent$centralization,
                       FRE15_G.netDensity, FRE15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "FRE"
KIoutcome = "Behind_F"
FRE15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
FRE15_Bg2 <- data.frame(FRE15_B)
FRE15_Bg2 <- FRE15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_Bg2$player1
player2vector <- FRE15_Bg2$player2
FRE15_Bg3 <- FRE15_Bg2
FRE15_Bg3$p1inp2vec <- is.element(FRE15_Bg3$player1, player2vector)
FRE15_Bg3$p2inp1vec <- is.element(FRE15_Bg3$player2, player1vector)

addPlayer1 <- FRE15_Bg3[ which(FRE15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_Bg3[ which(FRE15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_Bg2 <- rbind(FRE15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
FRE15_Bft <- ftable(FRE15_Bg2$player1, FRE15_Bg2$player2)
FRE15_Bft2 <- as.matrix(FRE15_Bft)
numRows <- nrow(FRE15_Bft2)
numCols <- ncol(FRE15_Bft2)
FRE15_Bft3 <- FRE15_Bft2[c(2:numRows) , c(2:numCols)]
FRE15_BTable <- graph.adjacency(FRE15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(FRE15_BTable, vertex.label = V(FRE15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
FRE15_B.clusterCoef <- transitivity(FRE15_BTable, type="global") #cluster coefficient
FRE15_B.degreeCent <- centralization.degree(FRE15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_Bftn <- as.network.matrix(FRE15_Bft)
FRE15_B.netDensity <- network.density(FRE15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_B.entropy <- entropy(FRE15_Bft) #entropy

FRE15_B.netMx <- cbind(FRE15_B.netMx, FRE15_B.clusterCoef, FRE15_B.degreeCent$centralization,
                       FRE15_B.netDensity, FRE15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
FRE15_SFg2 <- data.frame(FRE15_SF)
FRE15_SFg2 <- FRE15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_SFg2$player1
player2vector <- FRE15_SFg2$player2
FRE15_SFg3 <- FRE15_SFg2
FRE15_SFg3$p1inp2vec <- is.element(FRE15_SFg3$player1, player2vector)
FRE15_SFg3$p2inp1vec <- is.element(FRE15_SFg3$player2, player1vector)

addPlayer1 <- FRE15_SFg3[ which(FRE15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_SFg3[ which(FRE15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_SFg2 <- rbind(FRE15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
FRE15_SFft <- ftable(FRE15_SFg2$player1, FRE15_SFg2$player2)
FRE15_SFft2 <- as.matrix(FRE15_SFft)
numRows <- nrow(FRE15_SFft2)
numCols <- ncol(FRE15_SFft2)
FRE15_SFft3 <- FRE15_SFft2[c(2:numRows) , c(2:numCols)]
FRE15_SFTable <- graph.adjacency(FRE15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(FRE15_SFTable, vertex.label = V(FRE15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
FRE15_SF.clusterCoef <- transitivity(FRE15_SFTable, type="global") #cluster coefficient
FRE15_SF.degreeCent <- centralization.degree(FRE15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_SFftn <- as.network.matrix(FRE15_SFft)
FRE15_SF.netDensity <- network.density(FRE15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_SF.entropy <- entropy(FRE15_SFft) #entropy

FRE15_SF.netMx <- cbind(FRE15_SF.netMx, FRE15_SF.clusterCoef, FRE15_SF.degreeCent$centralization,
                        FRE15_SF.netDensity, FRE15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
FRE15_TFg2 <- data.frame(FRE15_TF)
FRE15_TFg2 <- FRE15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_TFg2$player1
player2vector <- FRE15_TFg2$player2
FRE15_TFg3 <- FRE15_TFg2
FRE15_TFg3$p1inp2vec <- is.element(FRE15_TFg3$player1, player2vector)
FRE15_TFg3$p2inp1vec <- is.element(FRE15_TFg3$player2, player1vector)

addPlayer1 <- FRE15_TFg3[ which(FRE15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_TFg3[ which(FRE15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_TFg2 <- rbind(FRE15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
FRE15_TFft <- ftable(FRE15_TFg2$player1, FRE15_TFg2$player2)
FRE15_TFft2 <- as.matrix(FRE15_TFft)
numRows <- nrow(FRE15_TFft2)
numCols <- ncol(FRE15_TFft2)
FRE15_TFft3 <- FRE15_TFft2[c(2:numRows) , c(2:numCols)]
FRE15_TFTable <- graph.adjacency(FRE15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(FRE15_TFTable, vertex.label = V(FRE15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
FRE15_TF.clusterCoef <- transitivity(FRE15_TFTable, type="global") #cluster coefficient
FRE15_TF.degreeCent <- centralization.degree(FRE15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_TFftn <- as.network.matrix(FRE15_TFft)
FRE15_TF.netDensity <- network.density(FRE15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_TF.entropy <- entropy(FRE15_TFft) #entropy

FRE15_TF.netMx <- cbind(FRE15_TF.netMx, FRE15_TF.clusterCoef, FRE15_TF.degreeCent$centralization,
                        FRE15_TF.netDensity, FRE15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
FRE15_SAMg2 <- data.frame(FRE15_SAM)
FRE15_SAMg2 <- FRE15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_SAMg2$player1
player2vector <- FRE15_SAMg2$player2
FRE15_SAMg3 <- FRE15_SAMg2
FRE15_SAMg3$p1inp2vec <- is.element(FRE15_SAMg3$player1, player2vector)
FRE15_SAMg3$p2inp1vec <- is.element(FRE15_SAMg3$player2, player1vector)

addPlayer1 <- FRE15_SAMg3[ which(FRE15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_SAMg3[ which(FRE15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_SAMg2 <- rbind(FRE15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
FRE15_SAMft <- ftable(FRE15_SAMg2$player1, FRE15_SAMg2$player2)
FRE15_SAMft2 <- as.matrix(FRE15_SAMft)
numRows <- nrow(FRE15_SAMft2)
numCols <- ncol(FRE15_SAMft2)
FRE15_SAMft3 <- FRE15_SAMft2[c(2:numRows) , c(2:numCols)]
FRE15_SAMTable <- graph.adjacency(FRE15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(FRE15_SAMTable, vertex.label = V(FRE15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
FRE15_SAM.clusterCoef <- transitivity(FRE15_SAMTable, type="global") #cluster coefficient
FRE15_SAM.degreeCent <- centralization.degree(FRE15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_SAMftn <- as.network.matrix(FRE15_SAMft)
FRE15_SAM.netDensity <- network.density(FRE15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_SAM.entropy <- entropy(FRE15_SAMft) #entropy

FRE15_SAM.netMx <- cbind(FRE15_SAM.netMx, FRE15_SAM.clusterCoef, FRE15_SAM.degreeCent$centralization,
                         FRE15_SAM.netDensity, FRE15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
FRE15_TAMg2 <- data.frame(FRE15_TAM)
FRE15_TAMg2 <- FRE15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_TAMg2$player1
player2vector <- FRE15_TAMg2$player2
FRE15_TAMg3 <- FRE15_TAMg2
FRE15_TAMg3$p1inp2vec <- is.element(FRE15_TAMg3$player1, player2vector)
FRE15_TAMg3$p2inp1vec <- is.element(FRE15_TAMg3$player2, player1vector)

addPlayer1 <- FRE15_TAMg3[ which(FRE15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_TAMg3[ which(FRE15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_TAMg2 <- rbind(FRE15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
FRE15_TAMft <- ftable(FRE15_TAMg2$player1, FRE15_TAMg2$player2)
FRE15_TAMft2 <- as.matrix(FRE15_TAMft)
numRows <- nrow(FRE15_TAMft2)
numCols <- ncol(FRE15_TAMft2)
FRE15_TAMft3 <- FRE15_TAMft2[c(2:numRows) , c(2:numCols)]
FRE15_TAMTable <- graph.adjacency(FRE15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(FRE15_TAMTable, vertex.label = V(FRE15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
FRE15_TAM.clusterCoef <- transitivity(FRE15_TAMTable, type="global") #cluster coefficient
FRE15_TAM.degreeCent <- centralization.degree(FRE15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_TAMftn <- as.network.matrix(FRE15_TAMft)
FRE15_TAM.netDensity <- network.density(FRE15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_TAM.entropy <- entropy(FRE15_TAMft) #entropy

FRE15_TAM.netMx <- cbind(FRE15_TAM.netMx, FRE15_TAM.clusterCoef, FRE15_TAM.degreeCent$centralization,
                         FRE15_TAM.netDensity, FRE15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
FRE15_SDMg2 <- data.frame(FRE15_SDM)
FRE15_SDMg2 <- FRE15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_SDMg2$player1
player2vector <- FRE15_SDMg2$player2
FRE15_SDMg3 <- FRE15_SDMg2
FRE15_SDMg3$p1inp2vec <- is.element(FRE15_SDMg3$player1, player2vector)
FRE15_SDMg3$p2inp1vec <- is.element(FRE15_SDMg3$player2, player1vector)

addPlayer1 <- FRE15_SDMg3[ which(FRE15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_SDMg3[ which(FRE15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_SDMg2 <- rbind(FRE15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
FRE15_SDMft <- ftable(FRE15_SDMg2$player1, FRE15_SDMg2$player2)
FRE15_SDMft2 <- as.matrix(FRE15_SDMft)
numRows <- nrow(FRE15_SDMft2)
numCols <- ncol(FRE15_SDMft2)
FRE15_SDMft3 <- FRE15_SDMft2[c(2:numRows) , c(2:numCols)]
FRE15_SDMTable <- graph.adjacency(FRE15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(FRE15_SDMTable, vertex.label = V(FRE15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
FRE15_SDM.clusterCoef <- transitivity(FRE15_SDMTable, type="global") #cluster coefficient
FRE15_SDM.degreeCent <- centralization.degree(FRE15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_SDMftn <- as.network.matrix(FRE15_SDMft)
FRE15_SDM.netDensity <- network.density(FRE15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_SDM.entropy <- entropy(FRE15_SDMft) #entropy

FRE15_SDM.netMx <- cbind(FRE15_SDM.netMx, FRE15_SDM.clusterCoef, FRE15_SDM.degreeCent$centralization,
                         FRE15_SDM.netDensity, FRE15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
FRE15_TDMg2 <- data.frame(FRE15_TDM)
FRE15_TDMg2 <- FRE15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_TDMg2$player1
player2vector <- FRE15_TDMg2$player2
FRE15_TDMg3 <- FRE15_TDMg2
FRE15_TDMg3$p1inp2vec <- is.element(FRE15_TDMg3$player1, player2vector)
FRE15_TDMg3$p2inp1vec <- is.element(FRE15_TDMg3$player2, player1vector)

addPlayer1 <- FRE15_TDMg3[ which(FRE15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_TDMg3[ which(FRE15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_TDMg2 <- rbind(FRE15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
FRE15_TDMft <- ftable(FRE15_TDMg2$player1, FRE15_TDMg2$player2)
FRE15_TDMft2 <- as.matrix(FRE15_TDMft)
numRows <- nrow(FRE15_TDMft2)
numCols <- ncol(FRE15_TDMft2)
FRE15_TDMft3 <- FRE15_TDMft2[c(2:numRows) , c(2:numCols)]
FRE15_TDMTable <- graph.adjacency(FRE15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(FRE15_TDMTable, vertex.label = V(FRE15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
FRE15_TDM.clusterCoef <- transitivity(FRE15_TDMTable, type="global") #cluster coefficient
FRE15_TDM.degreeCent <- centralization.degree(FRE15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_TDMftn <- as.network.matrix(FRE15_TDMft)
FRE15_TDM.netDensity <- network.density(FRE15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_TDM.entropy <- entropy(FRE15_TDMft) #entropy

FRE15_TDM.netMx <- cbind(FRE15_TDM.netMx, FRE15_TDM.clusterCoef, FRE15_TDM.degreeCent$centralization,
                         FRE15_TDM.netDensity, FRE15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
FRE15_SDg2 <- data.frame(FRE15_SD)
FRE15_SDg2 <- FRE15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_SDg2$player1
player2vector <- FRE15_SDg2$player2
FRE15_SDg3 <- FRE15_SDg2
FRE15_SDg3$p1inp2vec <- is.element(FRE15_SDg3$player1, player2vector)
FRE15_SDg3$p2inp1vec <- is.element(FRE15_SDg3$player2, player1vector)

addPlayer1 <- FRE15_SDg3[ which(FRE15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_SDg3[ which(FRE15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_SDg2 <- rbind(FRE15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
FRE15_SDft <- ftable(FRE15_SDg2$player1, FRE15_SDg2$player2)
FRE15_SDft2 <- as.matrix(FRE15_SDft)
numRows <- nrow(FRE15_SDft2)
numCols <- ncol(FRE15_SDft2)
FRE15_SDft3 <- FRE15_SDft2[c(2:numRows) , c(2:numCols)]
FRE15_SDTable <- graph.adjacency(FRE15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(FRE15_SDTable, vertex.label = V(FRE15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
FRE15_SD.clusterCoef <- transitivity(FRE15_SDTable, type="global") #cluster coefficient
FRE15_SD.degreeCent <- centralization.degree(FRE15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_SDftn <- as.network.matrix(FRE15_SDft)
FRE15_SD.netDensity <- network.density(FRE15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_SD.entropy <- entropy(FRE15_SDft) #entropy

FRE15_SD.netMx <- cbind(FRE15_SD.netMx, FRE15_SD.clusterCoef, FRE15_SD.degreeCent$centralization,
                        FRE15_SD.netDensity, FRE15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************

round = 15
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
FRE15_TDg2 <- data.frame(FRE15_TD)
FRE15_TDg2 <- FRE15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_TDg2$player1
player2vector <- FRE15_TDg2$player2
FRE15_TDg3 <- FRE15_TDg2
FRE15_TDg3$p1inp2vec <- is.element(FRE15_TDg3$player1, player2vector)
FRE15_TDg3$p2inp1vec <- is.element(FRE15_TDg3$player2, player1vector)

addPlayer1 <- FRE15_TDg3[ which(FRE15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_TDg3[ which(FRE15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_TDg2 <- rbind(FRE15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
FRE15_TDft <- ftable(FRE15_TDg2$player1, FRE15_TDg2$player2)
FRE15_TDft2 <- as.matrix(FRE15_TDft)
numRows <- nrow(FRE15_TDft2)
numCols <- ncol(FRE15_TDft2)
FRE15_TDft3 <- FRE15_TDft2[c(2:numRows) , c(2:numCols)]
FRE15_TDTable <- graph.adjacency(FRE15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(FRE15_TDTable, vertex.label = V(FRE15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
FRE15_TD.clusterCoef <- transitivity(FRE15_TDTable, type="global") #cluster coefficient
FRE15_TD.degreeCent <- centralization.degree(FRE15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_TDftn <- as.network.matrix(FRE15_TDft)
FRE15_TD.netDensity <- network.density(FRE15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_TD.entropy <- entropy(FRE15_TDft) #entropy

FRE15_TD.netMx <- cbind(FRE15_TD.netMx, FRE15_TD.clusterCoef, FRE15_TD.degreeCent$centralization,
                        FRE15_TD.netDensity, FRE15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
FRE15_QTg2 <- data.frame(FRE15_QT)
FRE15_QTg2 <- FRE15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE15_QTg2$player1
player2vector <- FRE15_QTg2$player2
FRE15_QTg3 <- FRE15_QTg2
FRE15_QTg3$p1inp2vec <- is.element(FRE15_QTg3$player1, player2vector)
FRE15_QTg3$p2inp1vec <- is.element(FRE15_QTg3$player2, player1vector)

addPlayer1 <- FRE15_QTg3[ which(FRE15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE15_QTg3[ which(FRE15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE15_QTg2 <- rbind(FRE15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
FRE15_QTft <- ftable(FRE15_QTg2$player1, FRE15_QTg2$player2)
FRE15_QTft2 <- as.matrix(FRE15_QTft)
numRows <- nrow(FRE15_QTft2)
numCols <- ncol(FRE15_QTft2)
FRE15_QTft3 <- FRE15_QTft2[c(2:numRows) , c(2:numCols)]
FRE15_QTTable <- graph.adjacency(FRE15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(FRE15_QTTable, vertex.label = V(FRE15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
FRE15_QT.clusterCoef <- transitivity(FRE15_QTTable, type="global") #cluster coefficient
FRE15_QT.degreeCent <- centralization.degree(FRE15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE15_QTftn <- as.network.matrix(FRE15_QTft)
FRE15_QT.netDensity <- network.density(FRE15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE15_QT.entropy <- entropy(FRE15_QTft) #entropy

FRE15_QT.netMx <- cbind(FRE15_QT.netMx, FRE15_QT.clusterCoef, FRE15_QT.degreeCent$centralization,
                        FRE15_QT.netDensity, FRE15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE15_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
GCFC15_Gg2 <- data.frame(GCFC15_G)
GCFC15_Gg2 <- GCFC15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_Gg2$player1
player2vector <- GCFC15_Gg2$player2
GCFC15_Gg3 <- GCFC15_Gg2
GCFC15_Gg3$p1inp2vec <- is.element(GCFC15_Gg3$player1, player2vector)
GCFC15_Gg3$p2inp1vec <- is.element(GCFC15_Gg3$player2, player1vector)

addPlayer1 <- GCFC15_Gg3[ which(GCFC15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_Gg3[ which(GCFC15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_Gg2 <- rbind(GCFC15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
GCFC15_Gft <- ftable(GCFC15_Gg2$player1, GCFC15_Gg2$player2)
GCFC15_Gft2 <- as.matrix(GCFC15_Gft)
numRows <- nrow(GCFC15_Gft2)
numCols <- ncol(GCFC15_Gft2)
GCFC15_Gft3 <- GCFC15_Gft2[c(2:numRows) , c(2:numCols)]
GCFC15_GTable <- graph.adjacency(GCFC15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(GCFC15_GTable, vertex.label = V(GCFC15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
GCFC15_G.clusterCoef <- transitivity(GCFC15_GTable, type="global") #cluster coefficient
GCFC15_G.degreeCent <- centralization.degree(GCFC15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_Gftn <- as.network.matrix(GCFC15_Gft)
GCFC15_G.netDensity <- network.density(GCFC15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_G.entropy <- entropy(GCFC15_Gft) #entropy

GCFC15_G.netMx <- cbind(GCFC15_G.netMx, GCFC15_G.clusterCoef, GCFC15_G.degreeCent$centralization,
                        GCFC15_G.netDensity, GCFC15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
GCFC15_Bg2 <- data.frame(GCFC15_B)
GCFC15_Bg2 <- GCFC15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_Bg2$player1
player2vector <- GCFC15_Bg2$player2
GCFC15_Bg3 <- GCFC15_Bg2
GCFC15_Bg3$p1inp2vec <- is.element(GCFC15_Bg3$player1, player2vector)
GCFC15_Bg3$p2inp1vec <- is.element(GCFC15_Bg3$player2, player1vector)

addPlayer1 <- GCFC15_Bg3[ which(GCFC15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_Bg3[ which(GCFC15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_Bg2 <- rbind(GCFC15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
GCFC15_Bft <- ftable(GCFC15_Bg2$player1, GCFC15_Bg2$player2)
GCFC15_Bft2 <- as.matrix(GCFC15_Bft)
numRows <- nrow(GCFC15_Bft2)
numCols <- ncol(GCFC15_Bft2)
GCFC15_Bft3 <- GCFC15_Bft2[c(2:numRows) , c(2:numCols)]
GCFC15_BTable <- graph.adjacency(GCFC15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(GCFC15_BTable, vertex.label = V(GCFC15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
GCFC15_B.clusterCoef <- transitivity(GCFC15_BTable, type="global") #cluster coefficient
GCFC15_B.degreeCent <- centralization.degree(GCFC15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_Bftn <- as.network.matrix(GCFC15_Bft)
GCFC15_B.netDensity <- network.density(GCFC15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_B.entropy <- entropy(GCFC15_Bft) #entropy

GCFC15_B.netMx <- cbind(GCFC15_B.netMx, GCFC15_B.clusterCoef, GCFC15_B.degreeCent$centralization,
                        GCFC15_B.netDensity, GCFC15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
GCFC15_SFg2 <- data.frame(GCFC15_SF)
GCFC15_SFg2 <- GCFC15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_SFg2$player1
player2vector <- GCFC15_SFg2$player2
GCFC15_SFg3 <- GCFC15_SFg2
GCFC15_SFg3$p1inp2vec <- is.element(GCFC15_SFg3$player1, player2vector)
GCFC15_SFg3$p2inp1vec <- is.element(GCFC15_SFg3$player2, player1vector)

addPlayer1 <- GCFC15_SFg3[ which(GCFC15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC15_SFg2 <- rbind(GCFC15_SFg2, addPlayer1)

#ROUND 15, FWD Stoppage graph using weighted edges
GCFC15_SFft <- ftable(GCFC15_SFg2$player1, GCFC15_SFg2$player2)
GCFC15_SFft2 <- as.matrix(GCFC15_SFft)
numRows <- nrow(GCFC15_SFft2)
numCols <- ncol(GCFC15_SFft2)
GCFC15_SFft3 <- GCFC15_SFft2[c(2:numRows) , c(1:numCols)]
GCFC15_SFTable <- graph.adjacency(GCFC15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(GCFC15_SFTable, vertex.label = V(GCFC15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
GCFC15_SF.clusterCoef <- transitivity(GCFC15_SFTable, type="global") #cluster coefficient
GCFC15_SF.degreeCent <- centralization.degree(GCFC15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_SFftn <- as.network.matrix(GCFC15_SFft)
GCFC15_SF.netDensity <- network.density(GCFC15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_SF.entropy <- entropy(GCFC15_SFft) #entropy

GCFC15_SF.netMx <- cbind(GCFC15_SF.netMx, GCFC15_SF.clusterCoef, GCFC15_SF.degreeCent$centralization,
                         GCFC15_SF.netDensity, GCFC15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
GCFC15_TFg2 <- data.frame(GCFC15_TF)
GCFC15_TFg2 <- GCFC15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_TFg2$player1
player2vector <- GCFC15_TFg2$player2
GCFC15_TFg3 <- GCFC15_TFg2
GCFC15_TFg3$p1inp2vec <- is.element(GCFC15_TFg3$player1, player2vector)
GCFC15_TFg3$p2inp1vec <- is.element(GCFC15_TFg3$player2, player1vector)

addPlayer1 <- GCFC15_TFg3[ which(GCFC15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC15_TFg3[ which(GCFC15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_TFg2 <- rbind(GCFC15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
GCFC15_TFft <- ftable(GCFC15_TFg2$player1, GCFC15_TFg2$player2)
GCFC15_TFft2 <- as.matrix(GCFC15_TFft)
numRows <- nrow(GCFC15_TFft2)
numCols <- ncol(GCFC15_TFft2)
GCFC15_TFft3 <- GCFC15_TFft2[c(2:numRows) , c(2:numCols)]
GCFC15_TFTable <- graph.adjacency(GCFC15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(GCFC15_TFTable, vertex.label = V(GCFC15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
GCFC15_TF.clusterCoef <- transitivity(GCFC15_TFTable, type="global") #cluster coefficient
GCFC15_TF.degreeCent <- centralization.degree(GCFC15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_TFftn <- as.network.matrix(GCFC15_TFft)
GCFC15_TF.netDensity <- network.density(GCFC15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_TF.entropy <- entropy(GCFC15_TFft) #entropy

GCFC15_TF.netMx <- cbind(GCFC15_TF.netMx, GCFC15_TF.clusterCoef, GCFC15_TF.degreeCent$centralization,
                         GCFC15_TF.netDensity, GCFC15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
GCFC15_SAMg2 <- data.frame(GCFC15_SAM)
GCFC15_SAMg2 <- GCFC15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_SAMg2$player1
player2vector <- GCFC15_SAMg2$player2
GCFC15_SAMg3 <- GCFC15_SAMg2
GCFC15_SAMg3$p1inp2vec <- is.element(GCFC15_SAMg3$player1, player2vector)
GCFC15_SAMg3$p2inp1vec <- is.element(GCFC15_SAMg3$player2, player1vector)

addPlayer1 <- GCFC15_SAMg3[ which(GCFC15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_SAMg3[ which(GCFC15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_SAMg2 <- rbind(GCFC15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
GCFC15_SAMft <- ftable(GCFC15_SAMg2$player1, GCFC15_SAMg2$player2)
GCFC15_SAMft2 <- as.matrix(GCFC15_SAMft)
numRows <- nrow(GCFC15_SAMft2)
numCols <- ncol(GCFC15_SAMft2)
GCFC15_SAMft3 <- GCFC15_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC15_SAMTable <- graph.adjacency(GCFC15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(GCFC15_SAMTable, vertex.label = V(GCFC15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
GCFC15_SAM.clusterCoef <- transitivity(GCFC15_SAMTable, type="global") #cluster coefficient
GCFC15_SAM.degreeCent <- centralization.degree(GCFC15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_SAMftn <- as.network.matrix(GCFC15_SAMft)
GCFC15_SAM.netDensity <- network.density(GCFC15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_SAM.entropy <- entropy(GCFC15_SAMft) #entropy

GCFC15_SAM.netMx <- cbind(GCFC15_SAM.netMx, GCFC15_SAM.clusterCoef, GCFC15_SAM.degreeCent$centralization,
                          GCFC15_SAM.netDensity, GCFC15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
GCFC15_TAMg2 <- data.frame(GCFC15_TAM)
GCFC15_TAMg2 <- GCFC15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_TAMg2$player1
player2vector <- GCFC15_TAMg2$player2
GCFC15_TAMg3 <- GCFC15_TAMg2
GCFC15_TAMg3$p1inp2vec <- is.element(GCFC15_TAMg3$player1, player2vector)
GCFC15_TAMg3$p2inp1vec <- is.element(GCFC15_TAMg3$player2, player1vector)

addPlayer1 <- GCFC15_TAMg3[ which(GCFC15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_TAMg3[ which(GCFC15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_TAMg2 <- rbind(GCFC15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
GCFC15_TAMft <- ftable(GCFC15_TAMg2$player1, GCFC15_TAMg2$player2)
GCFC15_TAMft2 <- as.matrix(GCFC15_TAMft)
numRows <- nrow(GCFC15_TAMft2)
numCols <- ncol(GCFC15_TAMft2)
GCFC15_TAMft3 <- GCFC15_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC15_TAMTable <- graph.adjacency(GCFC15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(GCFC15_TAMTable, vertex.label = V(GCFC15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
GCFC15_TAM.clusterCoef <- transitivity(GCFC15_TAMTable, type="global") #cluster coefficient
GCFC15_TAM.degreeCent <- centralization.degree(GCFC15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_TAMftn <- as.network.matrix(GCFC15_TAMft)
GCFC15_TAM.netDensity <- network.density(GCFC15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_TAM.entropy <- entropy(GCFC15_TAMft) #entropy

GCFC15_TAM.netMx <- cbind(GCFC15_TAM.netMx, GCFC15_TAM.clusterCoef, GCFC15_TAM.degreeCent$centralization,
                          GCFC15_TAM.netDensity, GCFC15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
GCFC15_SDMg2 <- data.frame(GCFC15_SDM)
GCFC15_SDMg2 <- GCFC15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_SDMg2$player1
player2vector <- GCFC15_SDMg2$player2
GCFC15_SDMg3 <- GCFC15_SDMg2
GCFC15_SDMg3$p1inp2vec <- is.element(GCFC15_SDMg3$player1, player2vector)
GCFC15_SDMg3$p2inp1vec <- is.element(GCFC15_SDMg3$player2, player1vector)

addPlayer1 <- GCFC15_SDMg3[ which(GCFC15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_SDMg3[ which(GCFC15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_SDMg2 <- rbind(GCFC15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
GCFC15_SDMft <- ftable(GCFC15_SDMg2$player1, GCFC15_SDMg2$player2)
GCFC15_SDMft2 <- as.matrix(GCFC15_SDMft)
numRows <- nrow(GCFC15_SDMft2)
numCols <- ncol(GCFC15_SDMft2)
GCFC15_SDMft3 <- GCFC15_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC15_SDMTable <- graph.adjacency(GCFC15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(GCFC15_SDMTable, vertex.label = V(GCFC15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
GCFC15_SDM.clusterCoef <- transitivity(GCFC15_SDMTable, type="global") #cluster coefficient
GCFC15_SDM.degreeCent <- centralization.degree(GCFC15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_SDMftn <- as.network.matrix(GCFC15_SDMft)
GCFC15_SDM.netDensity <- network.density(GCFC15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_SDM.entropy <- entropy(GCFC15_SDMft) #entropy

GCFC15_SDM.netMx <- cbind(GCFC15_SDM.netMx, GCFC15_SDM.clusterCoef, GCFC15_SDM.degreeCent$centralization,
                          GCFC15_SDM.netDensity, GCFC15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
GCFC15_TDMg2 <- data.frame(GCFC15_TDM)
GCFC15_TDMg2 <- GCFC15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_TDMg2$player1
player2vector <- GCFC15_TDMg2$player2
GCFC15_TDMg3 <- GCFC15_TDMg2
GCFC15_TDMg3$p1inp2vec <- is.element(GCFC15_TDMg3$player1, player2vector)
GCFC15_TDMg3$p2inp1vec <- is.element(GCFC15_TDMg3$player2, player1vector)

addPlayer1 <- GCFC15_TDMg3[ which(GCFC15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_TDMg3[ which(GCFC15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_TDMg2 <- rbind(GCFC15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
GCFC15_TDMft <- ftable(GCFC15_TDMg2$player1, GCFC15_TDMg2$player2)
GCFC15_TDMft2 <- as.matrix(GCFC15_TDMft)
numRows <- nrow(GCFC15_TDMft2)
numCols <- ncol(GCFC15_TDMft2)
GCFC15_TDMft3 <- GCFC15_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC15_TDMTable <- graph.adjacency(GCFC15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(GCFC15_TDMTable, vertex.label = V(GCFC15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
GCFC15_TDM.clusterCoef <- transitivity(GCFC15_TDMTable, type="global") #cluster coefficient
GCFC15_TDM.degreeCent <- centralization.degree(GCFC15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_TDMftn <- as.network.matrix(GCFC15_TDMft)
GCFC15_TDM.netDensity <- network.density(GCFC15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_TDM.entropy <- entropy(GCFC15_TDMft) #entropy

GCFC15_TDM.netMx <- cbind(GCFC15_TDM.netMx, GCFC15_TDM.clusterCoef, GCFC15_TDM.degreeCent$centralization,
                          GCFC15_TDM.netDensity, GCFC15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
GCFC15_SDg2 <- data.frame(GCFC15_SD)
GCFC15_SDg2 <- GCFC15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_SDg2$player1
player2vector <- GCFC15_SDg2$player2
GCFC15_SDg3 <- GCFC15_SDg2
GCFC15_SDg3$p1inp2vec <- is.element(GCFC15_SDg3$player1, player2vector)
GCFC15_SDg3$p2inp1vec <- is.element(GCFC15_SDg3$player2, player1vector)

addPlayer1 <- GCFC15_SDg3[ which(GCFC15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_SDg3[ which(GCFC15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_SDg2 <- rbind(GCFC15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
GCFC15_SDft <- ftable(GCFC15_SDg2$player1, GCFC15_SDg2$player2)
GCFC15_SDft2 <- as.matrix(GCFC15_SDft)
numRows <- nrow(GCFC15_SDft2)
numCols <- ncol(GCFC15_SDft2)
GCFC15_SDft3 <- GCFC15_SDft2[c(2:numRows) , c(2:numCols)]
GCFC15_SDTable <- graph.adjacency(GCFC15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(GCFC15_SDTable, vertex.label = V(GCFC15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
GCFC15_SD.clusterCoef <- transitivity(GCFC15_SDTable, type="global") #cluster coefficient
GCFC15_SD.degreeCent <- centralization.degree(GCFC15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_SDftn <- as.network.matrix(GCFC15_SDft)
GCFC15_SD.netDensity <- network.density(GCFC15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_SD.entropy <- entropy(GCFC15_SDft) #entropy

GCFC15_SD.netMx <- cbind(GCFC15_SD.netMx, GCFC15_SD.clusterCoef, GCFC15_SD.degreeCent$centralization,
                         GCFC15_SD.netDensity, GCFC15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
GCFC15_TDg2 <- data.frame(GCFC15_TD)
GCFC15_TDg2 <- GCFC15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_TDg2$player1
player2vector <- GCFC15_TDg2$player2
GCFC15_TDg3 <- GCFC15_TDg2
GCFC15_TDg3$p1inp2vec <- is.element(GCFC15_TDg3$player1, player2vector)
GCFC15_TDg3$p2inp1vec <- is.element(GCFC15_TDg3$player2, player1vector)

addPlayer1 <- GCFC15_TDg3[ which(GCFC15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_TDg3[ which(GCFC15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_TDg2 <- rbind(GCFC15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
GCFC15_TDft <- ftable(GCFC15_TDg2$player1, GCFC15_TDg2$player2)
GCFC15_TDft2 <- as.matrix(GCFC15_TDft)
numRows <- nrow(GCFC15_TDft2)
numCols <- ncol(GCFC15_TDft2)
GCFC15_TDft3 <- GCFC15_TDft2[c(2:numRows) , c(2:numCols)]
GCFC15_TDTable <- graph.adjacency(GCFC15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(GCFC15_TDTable, vertex.label = V(GCFC15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
GCFC15_TD.clusterCoef <- transitivity(GCFC15_TDTable, type="global") #cluster coefficient
GCFC15_TD.degreeCent <- centralization.degree(GCFC15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_TDftn <- as.network.matrix(GCFC15_TDft)
GCFC15_TD.netDensity <- network.density(GCFC15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_TD.entropy <- entropy(GCFC15_TDft) #entropy

GCFC15_TD.netMx <- cbind(GCFC15_TD.netMx, GCFC15_TD.clusterCoef, GCFC15_TD.degreeCent$centralization,
                         GCFC15_TD.netDensity, GCFC15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
GCFC15_QTg2 <- data.frame(GCFC15_QT)
GCFC15_QTg2 <- GCFC15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC15_QTg2$player1
player2vector <- GCFC15_QTg2$player2
GCFC15_QTg3 <- GCFC15_QTg2
GCFC15_QTg3$p1inp2vec <- is.element(GCFC15_QTg3$player1, player2vector)
GCFC15_QTg3$p2inp1vec <- is.element(GCFC15_QTg3$player2, player1vector)

addPlayer1 <- GCFC15_QTg3[ which(GCFC15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC15_QTg3[ which(GCFC15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC15_QTg2 <- rbind(GCFC15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
GCFC15_QTft <- ftable(GCFC15_QTg2$player1, GCFC15_QTg2$player2)
GCFC15_QTft2 <- as.matrix(GCFC15_QTft)
numRows <- nrow(GCFC15_QTft2)
numCols <- ncol(GCFC15_QTft2)
GCFC15_QTft3 <- GCFC15_QTft2[c(2:numRows) , c(2:numCols)]
GCFC15_QTTable <- graph.adjacency(GCFC15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(GCFC15_QTTable, vertex.label = V(GCFC15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
GCFC15_QT.clusterCoef <- transitivity(GCFC15_QTTable, type="global") #cluster coefficient
GCFC15_QT.degreeCent <- centralization.degree(GCFC15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC15_QTftn <- as.network.matrix(GCFC15_QTft)
GCFC15_QT.netDensity <- network.density(GCFC15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC15_QT.entropy <- entropy(GCFC15_QTft) #entropy

GCFC15_QT.netMx <- cbind(GCFC15_QT.netMx, GCFC15_QT.clusterCoef, GCFC15_QT.degreeCent$centralization,
                         GCFC15_QT.netDensity, GCFC15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC15_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
GEEL15_Gg2 <- data.frame(GEEL15_G)
GEEL15_Gg2 <- GEEL15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_Gg2$player1
player2vector <- GEEL15_Gg2$player2
GEEL15_Gg3 <- GEEL15_Gg2
GEEL15_Gg3$p1inp2vec <- is.element(GEEL15_Gg3$player1, player2vector)
GEEL15_Gg3$p2inp1vec <- is.element(GEEL15_Gg3$player2, player1vector)

addPlayer1 <- GEEL15_Gg3[ which(GEEL15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_Gg3[ which(GEEL15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_Gg2 <- rbind(GEEL15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
GEEL15_Gft <- ftable(GEEL15_Gg2$player1, GEEL15_Gg2$player2)
GEEL15_Gft2 <- as.matrix(GEEL15_Gft)
numRows <- nrow(GEEL15_Gft2)
numCols <- ncol(GEEL15_Gft2)
GEEL15_Gft3 <- GEEL15_Gft2[c(2:numRows) , c(2:numCols)]
GEEL15_GTable <- graph.adjacency(GEEL15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(GEEL15_GTable, vertex.label = V(GEEL15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
GEEL15_G.clusterCoef <- transitivity(GEEL15_GTable, type="global") #cluster coefficient
GEEL15_G.degreeCent <- centralization.degree(GEEL15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_Gftn <- as.network.matrix(GEEL15_Gft)
GEEL15_G.netDensity <- network.density(GEEL15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_G.entropy <- entropy(GEEL15_Gft) #entropy

GEEL15_G.netMx <- cbind(GEEL15_G.netMx, GEEL15_G.clusterCoef, GEEL15_G.degreeCent$centralization,
                        GEEL15_G.netDensity, GEEL15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************

round = 15
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
GEEL15_Bg2 <- data.frame(GEEL15_B)
GEEL15_Bg2 <- GEEL15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_Bg2$player1
player2vector <- GEEL15_Bg2$player2
GEEL15_Bg3 <- GEEL15_Bg2
GEEL15_Bg3$p1inp2vec <- is.element(GEEL15_Bg3$player1, player2vector)
GEEL15_Bg3$p2inp1vec <- is.element(GEEL15_Bg3$player2, player1vector)

addPlayer1 <- GEEL15_Bg3[ which(GEEL15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_Bg3[ which(GEEL15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_Bg2 <- rbind(GEEL15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
GEEL15_Bft <- ftable(GEEL15_Bg2$player1, GEEL15_Bg2$player2)
GEEL15_Bft2 <- as.matrix(GEEL15_Bft)
numRows <- nrow(GEEL15_Bft2)
numCols <- ncol(GEEL15_Bft2)
GEEL15_Bft3 <- GEEL15_Bft2[c(2:numRows) , c(2:numCols)]
GEEL15_BTable <- graph.adjacency(GEEL15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(GEEL15_BTable, vertex.label = V(GEEL15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
GEEL15_B.clusterCoef <- transitivity(GEEL15_BTable, type="global") #cluster coefficient
GEEL15_B.degreeCent <- centralization.degree(GEEL15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_Bftn <- as.network.matrix(GEEL15_Bft)
GEEL15_B.netDensity <- network.density(GEEL15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_B.entropy <- entropy(GEEL15_Bft) #entropy

GEEL15_B.netMx <- cbind(GEEL15_B.netMx, GEEL15_B.clusterCoef, GEEL15_B.degreeCent$centralization,
                        GEEL15_B.netDensity, GEEL15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
GEEL15_SFg2 <- data.frame(GEEL15_SF)
GEEL15_SFg2 <- GEEL15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_SFg2$player1
player2vector <- GEEL15_SFg2$player2
GEEL15_SFg3 <- GEEL15_SFg2
GEEL15_SFg3$p1inp2vec <- is.element(GEEL15_SFg3$player1, player2vector)
GEEL15_SFg3$p2inp1vec <- is.element(GEEL15_SFg3$player2, player1vector)

addPlayer1 <- GEEL15_SFg3[ which(GEEL15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_SFg3[ which(GEEL15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_SFg2 <- rbind(GEEL15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
GEEL15_SFft <- ftable(GEEL15_SFg2$player1, GEEL15_SFg2$player2)
GEEL15_SFft2 <- as.matrix(GEEL15_SFft)
numRows <- nrow(GEEL15_SFft2)
numCols <- ncol(GEEL15_SFft2)
GEEL15_SFft3 <- GEEL15_SFft2[c(2:numRows) , c(2:numCols)]
GEEL15_SFTable <- graph.adjacency(GEEL15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(GEEL15_SFTable, vertex.label = V(GEEL15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
GEEL15_SF.clusterCoef <- transitivity(GEEL15_SFTable, type="global") #cluster coefficient
GEEL15_SF.degreeCent <- centralization.degree(GEEL15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_SFftn <- as.network.matrix(GEEL15_SFft)
GEEL15_SF.netDensity <- network.density(GEEL15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_SF.entropy <- entropy(GEEL15_SFft) #entropy

GEEL15_SF.netMx <- cbind(GEEL15_SF.netMx, GEEL15_SF.clusterCoef, GEEL15_SF.degreeCent$centralization,
                         GEEL15_SF.netDensity, GEEL15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
GEEL15_TFg2 <- data.frame(GEEL15_TF)
GEEL15_TFg2 <- GEEL15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_TFg2$player1
player2vector <- GEEL15_TFg2$player2
GEEL15_TFg3 <- GEEL15_TFg2
GEEL15_TFg3$p1inp2vec <- is.element(GEEL15_TFg3$player1, player2vector)
GEEL15_TFg3$p2inp1vec <- is.element(GEEL15_TFg3$player2, player1vector)

addPlayer1 <- GEEL15_TFg3[ which(GEEL15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_TFg3[ which(GEEL15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_TFg2 <- rbind(GEEL15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
GEEL15_TFft <- ftable(GEEL15_TFg2$player1, GEEL15_TFg2$player2)
GEEL15_TFft2 <- as.matrix(GEEL15_TFft)
numRows <- nrow(GEEL15_TFft2)
numCols <- ncol(GEEL15_TFft2)
GEEL15_TFft3 <- GEEL15_TFft2[c(2:numRows) , c(2:numCols)]
GEEL15_TFTable <- graph.adjacency(GEEL15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(GEEL15_TFTable, vertex.label = V(GEEL15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
GEEL15_TF.clusterCoef <- transitivity(GEEL15_TFTable, type="global") #cluster coefficient
GEEL15_TF.degreeCent <- centralization.degree(GEEL15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_TFftn <- as.network.matrix(GEEL15_TFft)
GEEL15_TF.netDensity <- network.density(GEEL15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_TF.entropy <- entropy(GEEL15_TFft) #entropy

GEEL15_TF.netMx <- cbind(GEEL15_TF.netMx, GEEL15_TF.clusterCoef, GEEL15_TF.degreeCent$centralization,
                         GEEL15_TF.netDensity, GEEL15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
GEEL15_SAMg2 <- data.frame(GEEL15_SAM)
GEEL15_SAMg2 <- GEEL15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_SAMg2$player1
player2vector <- GEEL15_SAMg2$player2
GEEL15_SAMg3 <- GEEL15_SAMg2
GEEL15_SAMg3$p1inp2vec <- is.element(GEEL15_SAMg3$player1, player2vector)
GEEL15_SAMg3$p2inp1vec <- is.element(GEEL15_SAMg3$player2, player1vector)

addPlayer1 <- GEEL15_SAMg3[ which(GEEL15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_SAMg3[ which(GEEL15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_SAMg2 <- rbind(GEEL15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
GEEL15_SAMft <- ftable(GEEL15_SAMg2$player1, GEEL15_SAMg2$player2)
GEEL15_SAMft2 <- as.matrix(GEEL15_SAMft)
numRows <- nrow(GEEL15_SAMft2)
numCols <- ncol(GEEL15_SAMft2)
GEEL15_SAMft3 <- GEEL15_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL15_SAMTable <- graph.adjacency(GEEL15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(GEEL15_SAMTable, vertex.label = V(GEEL15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
GEEL15_SAM.clusterCoef <- transitivity(GEEL15_SAMTable, type="global") #cluster coefficient
GEEL15_SAM.degreeCent <- centralization.degree(GEEL15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_SAMftn <- as.network.matrix(GEEL15_SAMft)
GEEL15_SAM.netDensity <- network.density(GEEL15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_SAM.entropy <- entropy(GEEL15_SAMft) #entropy

GEEL15_SAM.netMx <- cbind(GEEL15_SAM.netMx, GEEL15_SAM.clusterCoef, GEEL15_SAM.degreeCent$centralization,
                          GEEL15_SAM.netDensity, GEEL15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
GEEL15_TAMg2 <- data.frame(GEEL15_TAM)
GEEL15_TAMg2 <- GEEL15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_TAMg2$player1
player2vector <- GEEL15_TAMg2$player2
GEEL15_TAMg3 <- GEEL15_TAMg2
GEEL15_TAMg3$p1inp2vec <- is.element(GEEL15_TAMg3$player1, player2vector)
GEEL15_TAMg3$p2inp1vec <- is.element(GEEL15_TAMg3$player2, player1vector)

addPlayer1 <- GEEL15_TAMg3[ which(GEEL15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_TAMg3[ which(GEEL15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_TAMg2 <- rbind(GEEL15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
GEEL15_TAMft <- ftable(GEEL15_TAMg2$player1, GEEL15_TAMg2$player2)
GEEL15_TAMft2 <- as.matrix(GEEL15_TAMft)
numRows <- nrow(GEEL15_TAMft2)
numCols <- ncol(GEEL15_TAMft2)
GEEL15_TAMft3 <- GEEL15_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL15_TAMTable <- graph.adjacency(GEEL15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(GEEL15_TAMTable, vertex.label = V(GEEL15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
GEEL15_TAM.clusterCoef <- transitivity(GEEL15_TAMTable, type="global") #cluster coefficient
GEEL15_TAM.degreeCent <- centralization.degree(GEEL15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_TAMftn <- as.network.matrix(GEEL15_TAMft)
GEEL15_TAM.netDensity <- network.density(GEEL15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_TAM.entropy <- entropy(GEEL15_TAMft) #entropy

GEEL15_TAM.netMx <- cbind(GEEL15_TAM.netMx, GEEL15_TAM.clusterCoef, GEEL15_TAM.degreeCent$centralization,
                          GEEL15_TAM.netDensity, GEEL15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
GEEL15_SDMg2 <- data.frame(GEEL15_SDM)
GEEL15_SDMg2 <- GEEL15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_SDMg2$player1
player2vector <- GEEL15_SDMg2$player2
GEEL15_SDMg3 <- GEEL15_SDMg2
GEEL15_SDMg3$p1inp2vec <- is.element(GEEL15_SDMg3$player1, player2vector)
GEEL15_SDMg3$p2inp1vec <- is.element(GEEL15_SDMg3$player2, player1vector)

addPlayer1 <- GEEL15_SDMg3[ which(GEEL15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_SDMg3[ which(GEEL15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_SDMg2 <- rbind(GEEL15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
GEEL15_SDMft <- ftable(GEEL15_SDMg2$player1, GEEL15_SDMg2$player2)
GEEL15_SDMft2 <- as.matrix(GEEL15_SDMft)
numRows <- nrow(GEEL15_SDMft2)
numCols <- ncol(GEEL15_SDMft2)
GEEL15_SDMft3 <- GEEL15_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL15_SDMTable <- graph.adjacency(GEEL15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(GEEL15_SDMTable, vertex.label = V(GEEL15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
GEEL15_SDM.clusterCoef <- transitivity(GEEL15_SDMTable, type="global") #cluster coefficient
GEEL15_SDM.degreeCent <- centralization.degree(GEEL15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_SDMftn <- as.network.matrix(GEEL15_SDMft)
GEEL15_SDM.netDensity <- network.density(GEEL15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_SDM.entropy <- entropy(GEEL15_SDMft) #entropy

GEEL15_SDM.netMx <- cbind(GEEL15_SDM.netMx, GEEL15_SDM.clusterCoef, GEEL15_SDM.degreeCent$centralization,
                          GEEL15_SDM.netDensity, GEEL15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
GEEL15_TDMg2 <- data.frame(GEEL15_TDM)
GEEL15_TDMg2 <- GEEL15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_TDMg2$player1
player2vector <- GEEL15_TDMg2$player2
GEEL15_TDMg3 <- GEEL15_TDMg2
GEEL15_TDMg3$p1inp2vec <- is.element(GEEL15_TDMg3$player1, player2vector)
GEEL15_TDMg3$p2inp1vec <- is.element(GEEL15_TDMg3$player2, player1vector)

addPlayer1 <- GEEL15_TDMg3[ which(GEEL15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_TDMg3[ which(GEEL15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_TDMg2 <- rbind(GEEL15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
GEEL15_TDMft <- ftable(GEEL15_TDMg2$player1, GEEL15_TDMg2$player2)
GEEL15_TDMft2 <- as.matrix(GEEL15_TDMft)
numRows <- nrow(GEEL15_TDMft2)
numCols <- ncol(GEEL15_TDMft2)
GEEL15_TDMft3 <- GEEL15_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL15_TDMTable <- graph.adjacency(GEEL15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(GEEL15_TDMTable, vertex.label = V(GEEL15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
GEEL15_TDM.clusterCoef <- transitivity(GEEL15_TDMTable, type="global") #cluster coefficient
GEEL15_TDM.degreeCent <- centralization.degree(GEEL15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_TDMftn <- as.network.matrix(GEEL15_TDMft)
GEEL15_TDM.netDensity <- network.density(GEEL15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_TDM.entropy <- entropy(GEEL15_TDMft) #entropy

GEEL15_TDM.netMx <- cbind(GEEL15_TDM.netMx, GEEL15_TDM.clusterCoef, GEEL15_TDM.degreeCent$centralization,
                          GEEL15_TDM.netDensity, GEEL15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
GEEL15_SDg2 <- data.frame(GEEL15_SD)
GEEL15_SDg2 <- GEEL15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_SDg2$player1
player2vector <- GEEL15_SDg2$player2
GEEL15_SDg3 <- GEEL15_SDg2
GEEL15_SDg3$p1inp2vec <- is.element(GEEL15_SDg3$player1, player2vector)
GEEL15_SDg3$p2inp1vec <- is.element(GEEL15_SDg3$player2, player1vector)

addPlayer1 <- GEEL15_SDg3[ which(GEEL15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_SDg3[ which(GEEL15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_SDg2 <- rbind(GEEL15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
GEEL15_SDft <- ftable(GEEL15_SDg2$player1, GEEL15_SDg2$player2)
GEEL15_SDft2 <- as.matrix(GEEL15_SDft)
numRows <- nrow(GEEL15_SDft2)
numCols <- ncol(GEEL15_SDft2)
GEEL15_SDft3 <- GEEL15_SDft2[c(2:numRows) , c(2:numCols)]
GEEL15_SDTable <- graph.adjacency(GEEL15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(GEEL15_SDTable, vertex.label = V(GEEL15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
GEEL15_SD.clusterCoef <- transitivity(GEEL15_SDTable, type="global") #cluster coefficient
GEEL15_SD.degreeCent <- centralization.degree(GEEL15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_SDftn <- as.network.matrix(GEEL15_SDft)
GEEL15_SD.netDensity <- network.density(GEEL15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_SD.entropy <- entropy(GEEL15_SDft) #entropy

GEEL15_SD.netMx <- cbind(GEEL15_SD.netMx, GEEL15_SD.clusterCoef, GEEL15_SD.degreeCent$centralization,
                         GEEL15_SD.netDensity, GEEL15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
GEEL15_TDg2 <- data.frame(GEEL15_TD)
GEEL15_TDg2 <- GEEL15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_TDg2$player1
player2vector <- GEEL15_TDg2$player2
GEEL15_TDg3 <- GEEL15_TDg2
GEEL15_TDg3$p1inp2vec <- is.element(GEEL15_TDg3$player1, player2vector)
GEEL15_TDg3$p2inp1vec <- is.element(GEEL15_TDg3$player2, player1vector)

addPlayer1 <- GEEL15_TDg3[ which(GEEL15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_TDg3[ which(GEEL15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_TDg2 <- rbind(GEEL15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
GEEL15_TDft <- ftable(GEEL15_TDg2$player1, GEEL15_TDg2$player2)
GEEL15_TDft2 <- as.matrix(GEEL15_TDft)
numRows <- nrow(GEEL15_TDft2)
numCols <- ncol(GEEL15_TDft2)
GEEL15_TDft3 <- GEEL15_TDft2[c(2:numRows) , c(2:numCols)]
GEEL15_TDTable <- graph.adjacency(GEEL15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(GEEL15_TDTable, vertex.label = V(GEEL15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
GEEL15_TD.clusterCoef <- transitivity(GEEL15_TDTable, type="global") #cluster coefficient
GEEL15_TD.degreeCent <- centralization.degree(GEEL15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_TDftn <- as.network.matrix(GEEL15_TDft)
GEEL15_TD.netDensity <- network.density(GEEL15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_TD.entropy <- entropy(GEEL15_TDft) #entropy

GEEL15_TD.netMx <- cbind(GEEL15_TD.netMx, GEEL15_TD.clusterCoef, GEEL15_TD.degreeCent$centralization,
                         GEEL15_TD.netDensity, GEEL15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
GEEL15_QTg2 <- data.frame(GEEL15_QT)
GEEL15_QTg2 <- GEEL15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL15_QTg2$player1
player2vector <- GEEL15_QTg2$player2
GEEL15_QTg3 <- GEEL15_QTg2
GEEL15_QTg3$p1inp2vec <- is.element(GEEL15_QTg3$player1, player2vector)
GEEL15_QTg3$p2inp1vec <- is.element(GEEL15_QTg3$player2, player1vector)

addPlayer1 <- GEEL15_QTg3[ which(GEEL15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL15_QTg3[ which(GEEL15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL15_QTg2 <- rbind(GEEL15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
GEEL15_QTft <- ftable(GEEL15_QTg2$player1, GEEL15_QTg2$player2)
GEEL15_QTft2 <- as.matrix(GEEL15_QTft)
numRows <- nrow(GEEL15_QTft2)
numCols <- ncol(GEEL15_QTft2)
GEEL15_QTft3 <- GEEL15_QTft2[c(2:numRows) , c(2:numCols)]
GEEL15_QTTable <- graph.adjacency(GEEL15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(GEEL15_QTTable, vertex.label = V(GEEL15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
GEEL15_QT.clusterCoef <- transitivity(GEEL15_QTTable, type="global") #cluster coefficient
GEEL15_QT.degreeCent <- centralization.degree(GEEL15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL15_QTftn <- as.network.matrix(GEEL15_QTft)
GEEL15_QT.netDensity <- network.density(GEEL15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL15_QT.entropy <- entropy(GEEL15_QTft) #entropy

GEEL15_QT.netMx <- cbind(GEEL15_QT.netMx, GEEL15_QT.clusterCoef, GEEL15_QT.degreeCent$centralization,
                         GEEL15_QT.netDensity, GEEL15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL15_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "Goal_F"
GWS15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
GWS15_Gg2 <- data.frame(GWS15_G)
GWS15_Gg2 <- GWS15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_Gg2$player1
player2vector <- GWS15_Gg2$player2
GWS15_Gg3 <- GWS15_Gg2
GWS15_Gg3$p1inp2vec <- is.element(GWS15_Gg3$player1, player2vector)
GWS15_Gg3$p2inp1vec <- is.element(GWS15_Gg3$player2, player1vector)

addPlayer1 <- GWS15_Gg3[ which(GWS15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_Gg3[ which(GWS15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_Gg2 <- rbind(GWS15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
GWS15_Gft <- ftable(GWS15_Gg2$player1, GWS15_Gg2$player2)
GWS15_Gft2 <- as.matrix(GWS15_Gft)
numRows <- nrow(GWS15_Gft2)
numCols <- ncol(GWS15_Gft2)
GWS15_Gft3 <- GWS15_Gft2[c(1:numRows) , c(1:numCols)]
GWS15_GTable <- graph.adjacency(GWS15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(GWS15_GTable, vertex.label = V(GWS15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
GWS15_G.clusterCoef <- transitivity(GWS15_GTable, type="global") #cluster coefficient
GWS15_G.degreeCent <- centralization.degree(GWS15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_Gftn <- as.network.matrix(GWS15_Gft)
GWS15_G.netDensity <- network.density(GWS15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_G.entropy <- entropy(GWS15_Gft) #entropy

GWS15_G.netMx <- cbind(GWS15_G.netMx, GWS15_G.clusterCoef, GWS15_G.degreeCent$centralization,
                       GWS15_G.netDensity, GWS15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "Behind_F"
GWS15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
GWS15_Bg2 <- data.frame(GWS15_B)
GWS15_Bg2 <- GWS15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_Bg2$player1
player2vector <- GWS15_Bg2$player2
GWS15_Bg3 <- GWS15_Bg2
GWS15_Bg3$p1inp2vec <- is.element(GWS15_Bg3$player1, player2vector)
GWS15_Bg3$p2inp1vec <- is.element(GWS15_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS15_Bg3[ which(GWS15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS15_Bg2 <- rbind(GWS15_Bg2, addPlayer2)

#ROUND 15, Behind graph using weighted edges
GWS15_Bft <- ftable(GWS15_Bg2$player1, GWS15_Bg2$player2)
GWS15_Bft2 <- as.matrix(GWS15_Bft)
numRows <- nrow(GWS15_Bft2)
numCols <- ncol(GWS15_Bft2)
GWS15_Bft3 <- GWS15_Bft2[c(1:numRows) , c(2:numCols)]
GWS15_BTable <- graph.adjacency(GWS15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(GWS15_BTable, vertex.label = V(GWS15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
GWS15_B.clusterCoef <- transitivity(GWS15_BTable, type="global") #cluster coefficient
GWS15_B.degreeCent <- centralization.degree(GWS15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_Bftn <- as.network.matrix(GWS15_Bft)
GWS15_B.netDensity <- network.density(GWS15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_B.entropy <- entropy(GWS15_Bft) #entropy

GWS15_B.netMx <- cbind(GWS15_B.netMx, GWS15_B.clusterCoef, GWS15_B.degreeCent$centralization,
                       GWS15_B.netDensity, GWS15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
GWS15_SFg2 <- data.frame(GWS15_SF)
GWS15_SFg2 <- GWS15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_SFg2$player1
player2vector <- GWS15_SFg2$player2
GWS15_SFg3 <- GWS15_SFg2
GWS15_SFg3$p1inp2vec <- is.element(GWS15_SFg3$player1, player2vector)
GWS15_SFg3$p2inp1vec <- is.element(GWS15_SFg3$player2, player1vector)

addPlayer1 <- GWS15_SFg3[ which(GWS15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_SFg3[ which(GWS15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_SFg2 <- rbind(GWS15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
GWS15_SFft <- ftable(GWS15_SFg2$player1, GWS15_SFg2$player2)
GWS15_SFft2 <- as.matrix(GWS15_SFft)
numRows <- nrow(GWS15_SFft2)
numCols <- ncol(GWS15_SFft2)
GWS15_SFft3 <- GWS15_SFft2[c(2:numRows) , c(2:numCols)]
GWS15_SFTable <- graph.adjacency(GWS15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(GWS15_SFTable, vertex.label = V(GWS15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
GWS15_SF.clusterCoef <- transitivity(GWS15_SFTable, type="global") #cluster coefficient
GWS15_SF.degreeCent <- centralization.degree(GWS15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_SFftn <- as.network.matrix(GWS15_SFft)
GWS15_SF.netDensity <- network.density(GWS15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_SF.entropy <- entropy(GWS15_SFft) #entropy

GWS15_SF.netMx <- cbind(GWS15_SF.netMx, GWS15_SF.clusterCoef, GWS15_SF.degreeCent$centralization,
                        GWS15_SF.netDensity, GWS15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
GWS15_TFg2 <- data.frame(GWS15_TF)
GWS15_TFg2 <- GWS15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_TFg2$player1
player2vector <- GWS15_TFg2$player2
GWS15_TFg3 <- GWS15_TFg2
GWS15_TFg3$p1inp2vec <- is.element(GWS15_TFg3$player1, player2vector)
GWS15_TFg3$p2inp1vec <- is.element(GWS15_TFg3$player2, player1vector)

addPlayer1 <- GWS15_TFg3[ which(GWS15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_TFg3[ which(GWS15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_TFg2 <- rbind(GWS15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
GWS15_TFft <- ftable(GWS15_TFg2$player1, GWS15_TFg2$player2)
GWS15_TFft2 <- as.matrix(GWS15_TFft)
numRows <- nrow(GWS15_TFft2)
numCols <- ncol(GWS15_TFft2)
GWS15_TFft3 <- GWS15_TFft2[c(2:numRows) , c(2:numCols)]
GWS15_TFTable <- graph.adjacency(GWS15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(GWS15_TFTable, vertex.label = V(GWS15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
GWS15_TF.clusterCoef <- transitivity(GWS15_TFTable, type="global") #cluster coefficient
GWS15_TF.degreeCent <- centralization.degree(GWS15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_TFftn <- as.network.matrix(GWS15_TFft)
GWS15_TF.netDensity <- network.density(GWS15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_TF.entropy <- entropy(GWS15_TFft) #entropy

GWS15_TF.netMx <- cbind(GWS15_TF.netMx, GWS15_TF.clusterCoef, GWS15_TF.degreeCent$centralization,
                        GWS15_TF.netDensity, GWS15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
GWS15_SAMg2 <- data.frame(GWS15_SAM)
GWS15_SAMg2 <- GWS15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_SAMg2$player1
player2vector <- GWS15_SAMg2$player2
GWS15_SAMg3 <- GWS15_SAMg2
GWS15_SAMg3$p1inp2vec <- is.element(GWS15_SAMg3$player1, player2vector)
GWS15_SAMg3$p2inp1vec <- is.element(GWS15_SAMg3$player2, player1vector)

addPlayer1 <- GWS15_SAMg3[ which(GWS15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_SAMg3[ which(GWS15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_SAMg2 <- rbind(GWS15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
GWS15_SAMft <- ftable(GWS15_SAMg2$player1, GWS15_SAMg2$player2)
GWS15_SAMft2 <- as.matrix(GWS15_SAMft)
numRows <- nrow(GWS15_SAMft2)
numCols <- ncol(GWS15_SAMft2)
GWS15_SAMft3 <- GWS15_SAMft2[c(2:numRows) , c(2:numCols)]
GWS15_SAMTable <- graph.adjacency(GWS15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(GWS15_SAMTable, vertex.label = V(GWS15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
GWS15_SAM.clusterCoef <- transitivity(GWS15_SAMTable, type="global") #cluster coefficient
GWS15_SAM.degreeCent <- centralization.degree(GWS15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_SAMftn <- as.network.matrix(GWS15_SAMft)
GWS15_SAM.netDensity <- network.density(GWS15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_SAM.entropy <- entropy(GWS15_SAMft) #entropy

GWS15_SAM.netMx <- cbind(GWS15_SAM.netMx, GWS15_SAM.clusterCoef, GWS15_SAM.degreeCent$centralization,
                         GWS15_SAM.netDensity, GWS15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
GWS15_TAMg2 <- data.frame(GWS15_TAM)
GWS15_TAMg2 <- GWS15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_TAMg2$player1
player2vector <- GWS15_TAMg2$player2
GWS15_TAMg3 <- GWS15_TAMg2
GWS15_TAMg3$p1inp2vec <- is.element(GWS15_TAMg3$player1, player2vector)
GWS15_TAMg3$p2inp1vec <- is.element(GWS15_TAMg3$player2, player1vector)

addPlayer1 <- GWS15_TAMg3[ which(GWS15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_TAMg3[ which(GWS15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_TAMg2 <- rbind(GWS15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
GWS15_TAMft <- ftable(GWS15_TAMg2$player1, GWS15_TAMg2$player2)
GWS15_TAMft2 <- as.matrix(GWS15_TAMft)
numRows <- nrow(GWS15_TAMft2)
numCols <- ncol(GWS15_TAMft2)
GWS15_TAMft3 <- GWS15_TAMft2[c(2:numRows) , c(2:numCols)]
GWS15_TAMTable <- graph.adjacency(GWS15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(GWS15_TAMTable, vertex.label = V(GWS15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
GWS15_TAM.clusterCoef <- transitivity(GWS15_TAMTable, type="global") #cluster coefficient
GWS15_TAM.degreeCent <- centralization.degree(GWS15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_TAMftn <- as.network.matrix(GWS15_TAMft)
GWS15_TAM.netDensity <- network.density(GWS15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_TAM.entropy <- entropy(GWS15_TAMft) #entropy

GWS15_TAM.netMx <- cbind(GWS15_TAM.netMx, GWS15_TAM.clusterCoef, GWS15_TAM.degreeCent$centralization,
                         GWS15_TAM.netDensity, GWS15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
GWS15_SDMg2 <- data.frame(GWS15_SDM)
GWS15_SDMg2 <- GWS15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_SDMg2$player1
player2vector <- GWS15_SDMg2$player2
GWS15_SDMg3 <- GWS15_SDMg2
GWS15_SDMg3$p1inp2vec <- is.element(GWS15_SDMg3$player1, player2vector)
GWS15_SDMg3$p2inp1vec <- is.element(GWS15_SDMg3$player2, player1vector)

addPlayer1 <- GWS15_SDMg3[ which(GWS15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS15_SDMg3[ which(GWS15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_SDMg2 <- rbind(GWS15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
GWS15_SDMft <- ftable(GWS15_SDMg2$player1, GWS15_SDMg2$player2)
GWS15_SDMft2 <- as.matrix(GWS15_SDMft)
numRows <- nrow(GWS15_SDMft2)
numCols <- ncol(GWS15_SDMft2)
GWS15_SDMft3 <- GWS15_SDMft2[c(2:numRows) , c(2:numCols)]
GWS15_SDMTable <- graph.adjacency(GWS15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(GWS15_SDMTable, vertex.label = V(GWS15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
GWS15_SDM.clusterCoef <- transitivity(GWS15_SDMTable, type="global") #cluster coefficient
GWS15_SDM.degreeCent <- centralization.degree(GWS15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_SDMftn <- as.network.matrix(GWS15_SDMft)
GWS15_SDM.netDensity <- network.density(GWS15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_SDM.entropy <- entropy(GWS15_SDMft) #entropy

GWS15_SDM.netMx <- cbind(GWS15_SDM.netMx, GWS15_SDM.clusterCoef, GWS15_SDM.degreeCent$centralization,
                         GWS15_SDM.netDensity, GWS15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
GWS15_TDMg2 <- data.frame(GWS15_TDM)
GWS15_TDMg2 <- GWS15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_TDMg2$player1
player2vector <- GWS15_TDMg2$player2
GWS15_TDMg3 <- GWS15_TDMg2
GWS15_TDMg3$p1inp2vec <- is.element(GWS15_TDMg3$player1, player2vector)
GWS15_TDMg3$p2inp1vec <- is.element(GWS15_TDMg3$player2, player1vector)

addPlayer1 <- GWS15_TDMg3[ which(GWS15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_TDMg3[ which(GWS15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_TDMg2 <- rbind(GWS15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
GWS15_TDMft <- ftable(GWS15_TDMg2$player1, GWS15_TDMg2$player2)
GWS15_TDMft2 <- as.matrix(GWS15_TDMft)
numRows <- nrow(GWS15_TDMft2)
numCols <- ncol(GWS15_TDMft2)
GWS15_TDMft3 <- GWS15_TDMft2[c(2:numRows) , c(2:numCols)]
GWS15_TDMTable <- graph.adjacency(GWS15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(GWS15_TDMTable, vertex.label = V(GWS15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
GWS15_TDM.clusterCoef <- transitivity(GWS15_TDMTable, type="global") #cluster coefficient
GWS15_TDM.degreeCent <- centralization.degree(GWS15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_TDMftn <- as.network.matrix(GWS15_TDMft)
GWS15_TDM.netDensity <- network.density(GWS15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_TDM.entropy <- entropy(GWS15_TDMft) #entropy

GWS15_TDM.netMx <- cbind(GWS15_TDM.netMx, GWS15_TDM.clusterCoef, GWS15_TDM.degreeCent$centralization,
                         GWS15_TDM.netDensity, GWS15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
GWS15_SDg2 <- data.frame(GWS15_SD)
GWS15_SDg2 <- GWS15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_SDg2$player1
player2vector <- GWS15_SDg2$player2
GWS15_SDg3 <- GWS15_SDg2
GWS15_SDg3$p1inp2vec <- is.element(GWS15_SDg3$player1, player2vector)
GWS15_SDg3$p2inp1vec <- is.element(GWS15_SDg3$player2, player1vector)

addPlayer1 <- GWS15_SDg3[ which(GWS15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_SDg3[ which(GWS15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_SDg2 <- rbind(GWS15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
GWS15_SDft <- ftable(GWS15_SDg2$player1, GWS15_SDg2$player2)
GWS15_SDft2 <- as.matrix(GWS15_SDft)
numRows <- nrow(GWS15_SDft2)
numCols <- ncol(GWS15_SDft2)
GWS15_SDft3 <- GWS15_SDft2[c(2:numRows) , c(2:numCols)]
GWS15_SDTable <- graph.adjacency(GWS15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(GWS15_SDTable, vertex.label = V(GWS15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
GWS15_SD.clusterCoef <- transitivity(GWS15_SDTable, type="global") #cluster coefficient
GWS15_SD.degreeCent <- centralization.degree(GWS15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_SDftn <- as.network.matrix(GWS15_SDft)
GWS15_SD.netDensity <- network.density(GWS15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_SD.entropy <- entropy(GWS15_SDft) #entropy

GWS15_SD.netMx <- cbind(GWS15_SD.netMx, GWS15_SD.clusterCoef, GWS15_SD.degreeCent$centralization,
                        GWS15_SD.netDensity, GWS15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
GWS15_TDg2 <- data.frame(GWS15_TD)
GWS15_TDg2 <- GWS15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_TDg2$player1
player2vector <- GWS15_TDg2$player2
GWS15_TDg3 <- GWS15_TDg2
GWS15_TDg3$p1inp2vec <- is.element(GWS15_TDg3$player1, player2vector)
GWS15_TDg3$p2inp1vec <- is.element(GWS15_TDg3$player2, player1vector)

addPlayer1 <- GWS15_TDg3[ which(GWS15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_TDg3[ which(GWS15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_TDg2 <- rbind(GWS15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
GWS15_TDft <- ftable(GWS15_TDg2$player1, GWS15_TDg2$player2)
GWS15_TDft2 <- as.matrix(GWS15_TDft)
numRows <- nrow(GWS15_TDft2)
numCols <- ncol(GWS15_TDft2)
GWS15_TDft3 <- GWS15_TDft2[c(2:numRows) , c(2:numCols)]
GWS15_TDTable <- graph.adjacency(GWS15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(GWS15_TDTable, vertex.label = V(GWS15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
GWS15_TD.clusterCoef <- transitivity(GWS15_TDTable, type="global") #cluster coefficient
GWS15_TD.degreeCent <- centralization.degree(GWS15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_TDftn <- as.network.matrix(GWS15_TDft)
GWS15_TD.netDensity <- network.density(GWS15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_TD.entropy <- entropy(GWS15_TDft) #entropy

GWS15_TD.netMx <- cbind(GWS15_TD.netMx, GWS15_TD.clusterCoef, GWS15_TD.degreeCent$centralization,
                        GWS15_TD.netDensity, GWS15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
GWS15_QTg2 <- data.frame(GWS15_QT)
GWS15_QTg2 <- GWS15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS15_QTg2$player1
player2vector <- GWS15_QTg2$player2
GWS15_QTg3 <- GWS15_QTg2
GWS15_QTg3$p1inp2vec <- is.element(GWS15_QTg3$player1, player2vector)
GWS15_QTg3$p2inp1vec <- is.element(GWS15_QTg3$player2, player1vector)

addPlayer1 <- GWS15_QTg3[ which(GWS15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS15_QTg3[ which(GWS15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS15_QTg2 <- rbind(GWS15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
GWS15_QTft <- ftable(GWS15_QTg2$player1, GWS15_QTg2$player2)
GWS15_QTft2 <- as.matrix(GWS15_QTft)
numRows <- nrow(GWS15_QTft2)
numCols <- ncol(GWS15_QTft2)
GWS15_QTft3 <- GWS15_QTft2[c(2:numRows) , c(2:numCols)]
GWS15_QTTable <- graph.adjacency(GWS15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(GWS15_QTTable, vertex.label = V(GWS15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
GWS15_QT.clusterCoef <- transitivity(GWS15_QTTable, type="global") #cluster coefficient
GWS15_QT.degreeCent <- centralization.degree(GWS15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS15_QTftn <- as.network.matrix(GWS15_QTft)
GWS15_QT.netDensity <- network.density(GWS15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS15_QT.entropy <- entropy(GWS15_QTft) #entropy

GWS15_QT.netMx <- cbind(GWS15_QT.netMx, GWS15_QT.clusterCoef, GWS15_QT.degreeCent$centralization,
                        GWS15_QT.netDensity, GWS15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS15_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "Goal_F"
HAW15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
HAW15_Gg2 <- data.frame(HAW15_G)
HAW15_Gg2 <- HAW15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_Gg2$player1
player2vector <- HAW15_Gg2$player2
HAW15_Gg3 <- HAW15_Gg2
HAW15_Gg3$p1inp2vec <- is.element(HAW15_Gg3$player1, player2vector)
HAW15_Gg3$p2inp1vec <- is.element(HAW15_Gg3$player2, player1vector)

addPlayer1 <- HAW15_Gg3[ which(HAW15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_Gg3[ which(HAW15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_Gg2 <- rbind(HAW15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
HAW15_Gft <- ftable(HAW15_Gg2$player1, HAW15_Gg2$player2)
HAW15_Gft2 <- as.matrix(HAW15_Gft)
numRows <- nrow(HAW15_Gft2)
numCols <- ncol(HAW15_Gft2)
HAW15_Gft3 <- HAW15_Gft2[c(2:numRows) , c(2:numCols)]
HAW15_GTable <- graph.adjacency(HAW15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(HAW15_GTable, vertex.label = V(HAW15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
HAW15_G.clusterCoef <- transitivity(HAW15_GTable, type="global") #cluster coefficient
HAW15_G.degreeCent <- centralization.degree(HAW15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_Gftn <- as.network.matrix(HAW15_Gft)
HAW15_G.netDensity <- network.density(HAW15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_G.entropy <- entropy(HAW15_Gft) #entropy

HAW15_G.netMx <- cbind(HAW15_G.netMx, HAW15_G.clusterCoef, HAW15_G.degreeCent$centralization,
                       HAW15_G.netDensity, HAW15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "Behind_F"
HAW15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
HAW15_Bg2 <- data.frame(HAW15_B)
HAW15_Bg2 <- HAW15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_Bg2$player1
player2vector <- HAW15_Bg2$player2
HAW15_Bg3 <- HAW15_Bg2
HAW15_Bg3$p1inp2vec <- is.element(HAW15_Bg3$player1, player2vector)
HAW15_Bg3$p2inp1vec <- is.element(HAW15_Bg3$player2, player1vector)

addPlayer1 <- HAW15_Bg3[ which(HAW15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW15_Bg2 <- rbind(HAW15_Bg2, addPlayer1)

#ROUND 15, Behind graph using weighted edges
HAW15_Bft <- ftable(HAW15_Bg2$player1, HAW15_Bg2$player2)
HAW15_Bft2 <- as.matrix(HAW15_Bft)
numRows <- nrow(HAW15_Bft2)
numCols <- ncol(HAW15_Bft2)
HAW15_Bft3 <- HAW15_Bft2[c(2:numRows) , c(1:numCols)]
HAW15_BTable <- graph.adjacency(HAW15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(HAW15_BTable, vertex.label = V(HAW15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
HAW15_B.clusterCoef <- transitivity(HAW15_BTable, type="global") #cluster coefficient
HAW15_B.degreeCent <- centralization.degree(HAW15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_Bftn <- as.network.matrix(HAW15_Bft)
HAW15_B.netDensity <- network.density(HAW15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_B.entropy <- entropy(HAW15_Bft) #entropy

HAW15_B.netMx <- cbind(HAW15_B.netMx, HAW15_B.clusterCoef, HAW15_B.degreeCent$centralization,
                       HAW15_B.netDensity, HAW15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************

round = 15
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
HAW15_SFg2 <- data.frame(HAW15_SF)
HAW15_SFg2 <- HAW15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_SFg2$player1
player2vector <- HAW15_SFg2$player2
HAW15_SFg3 <- HAW15_SFg2
HAW15_SFg3$p1inp2vec <- is.element(HAW15_SFg3$player1, player2vector)
HAW15_SFg3$p2inp1vec <- is.element(HAW15_SFg3$player2, player1vector)

addPlayer1 <- HAW15_SFg3[ which(HAW15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_SFg3[ which(HAW15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_SFg2 <- rbind(HAW15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
HAW15_SFft <- ftable(HAW15_SFg2$player1, HAW15_SFg2$player2)
HAW15_SFft2 <- as.matrix(HAW15_SFft)
numRows <- nrow(HAW15_SFft2)
numCols <- ncol(HAW15_SFft2)
HAW15_SFft3 <- HAW15_SFft2[c(2:numRows) , c(2:numCols)]
HAW15_SFTable <- graph.adjacency(HAW15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(HAW15_SFTable, vertex.label = V(HAW15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
HAW15_SF.clusterCoef <- transitivity(HAW15_SFTable, type="global") #cluster coefficient
HAW15_SF.degreeCent <- centralization.degree(HAW15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_SFftn <- as.network.matrix(HAW15_SFft)
HAW15_SF.netDensity <- network.density(HAW15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_SF.entropy <- entropy(HAW15_SFft) #entropy

HAW15_SF.netMx <- cbind(HAW15_SF.netMx, HAW15_SF.clusterCoef, HAW15_SF.degreeCent$centralization,
                        HAW15_SF.netDensity, HAW15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
HAW15_TFg2 <- data.frame(HAW15_TF)
HAW15_TFg2 <- HAW15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_TFg2$player1
player2vector <- HAW15_TFg2$player2
HAW15_TFg3 <- HAW15_TFg2
HAW15_TFg3$p1inp2vec <- is.element(HAW15_TFg3$player1, player2vector)
HAW15_TFg3$p2inp1vec <- is.element(HAW15_TFg3$player2, player1vector)

addPlayer1 <- HAW15_TFg3[ which(HAW15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_TFg3[ which(HAW15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_TFg2 <- rbind(HAW15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
HAW15_TFft <- ftable(HAW15_TFg2$player1, HAW15_TFg2$player2)
HAW15_TFft2 <- as.matrix(HAW15_TFft)
numRows <- nrow(HAW15_TFft2)
numCols <- ncol(HAW15_TFft2)
HAW15_TFft3 <- HAW15_TFft2[c(2:numRows) , c(2:numCols)]
HAW15_TFTable <- graph.adjacency(HAW15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(HAW15_TFTable, vertex.label = V(HAW15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
HAW15_TF.clusterCoef <- transitivity(HAW15_TFTable, type="global") #cluster coefficient
HAW15_TF.degreeCent <- centralization.degree(HAW15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_TFftn <- as.network.matrix(HAW15_TFft)
HAW15_TF.netDensity <- network.density(HAW15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_TF.entropy <- entropy(HAW15_TFft) #entropy

HAW15_TF.netMx <- cbind(HAW15_TF.netMx, HAW15_TF.clusterCoef, HAW15_TF.degreeCent$centralization,
                        HAW15_TF.netDensity, HAW15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
HAW15_SAMg2 <- data.frame(HAW15_SAM)
HAW15_SAMg2 <- HAW15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_SAMg2$player1
player2vector <- HAW15_SAMg2$player2
HAW15_SAMg3 <- HAW15_SAMg2
HAW15_SAMg3$p1inp2vec <- is.element(HAW15_SAMg3$player1, player2vector)
HAW15_SAMg3$p2inp1vec <- is.element(HAW15_SAMg3$player2, player1vector)

addPlayer1 <- HAW15_SAMg3[ which(HAW15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_SAMg3[ which(HAW15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_SAMg2 <- rbind(HAW15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
HAW15_SAMft <- ftable(HAW15_SAMg2$player1, HAW15_SAMg2$player2)
HAW15_SAMft2 <- as.matrix(HAW15_SAMft)
numRows <- nrow(HAW15_SAMft2)
numCols <- ncol(HAW15_SAMft2)
HAW15_SAMft3 <- HAW15_SAMft2[c(2:numRows) , c(2:numCols)]
HAW15_SAMTable <- graph.adjacency(HAW15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(HAW15_SAMTable, vertex.label = V(HAW15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
HAW15_SAM.clusterCoef <- transitivity(HAW15_SAMTable, type="global") #cluster coefficient
HAW15_SAM.degreeCent <- centralization.degree(HAW15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_SAMftn <- as.network.matrix(HAW15_SAMft)
HAW15_SAM.netDensity <- network.density(HAW15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_SAM.entropy <- entropy(HAW15_SAMft) #entropy

HAW15_SAM.netMx <- cbind(HAW15_SAM.netMx, HAW15_SAM.clusterCoef, HAW15_SAM.degreeCent$centralization,
                         HAW15_SAM.netDensity, HAW15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
HAW15_TAMg2 <- data.frame(HAW15_TAM)
HAW15_TAMg2 <- HAW15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_TAMg2$player1
player2vector <- HAW15_TAMg2$player2
HAW15_TAMg3 <- HAW15_TAMg2
HAW15_TAMg3$p1inp2vec <- is.element(HAW15_TAMg3$player1, player2vector)
HAW15_TAMg3$p2inp1vec <- is.element(HAW15_TAMg3$player2, player1vector)

addPlayer1 <- HAW15_TAMg3[ which(HAW15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_TAMg3[ which(HAW15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_TAMg2 <- rbind(HAW15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
HAW15_TAMft <- ftable(HAW15_TAMg2$player1, HAW15_TAMg2$player2)
HAW15_TAMft2 <- as.matrix(HAW15_TAMft)
numRows <- nrow(HAW15_TAMft2)
numCols <- ncol(HAW15_TAMft2)
HAW15_TAMft3 <- HAW15_TAMft2[c(2:numRows) , c(2:numCols)]
HAW15_TAMTable <- graph.adjacency(HAW15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(HAW15_TAMTable, vertex.label = V(HAW15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
HAW15_TAM.clusterCoef <- transitivity(HAW15_TAMTable, type="global") #cluster coefficient
HAW15_TAM.degreeCent <- centralization.degree(HAW15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_TAMftn <- as.network.matrix(HAW15_TAMft)
HAW15_TAM.netDensity <- network.density(HAW15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_TAM.entropy <- entropy(HAW15_TAMft) #entropy

HAW15_TAM.netMx <- cbind(HAW15_TAM.netMx, HAW15_TAM.clusterCoef, HAW15_TAM.degreeCent$centralization,
                         HAW15_TAM.netDensity, HAW15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
HAW15_SDMg2 <- data.frame(HAW15_SDM)
HAW15_SDMg2 <- HAW15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_SDMg2$player1
player2vector <- HAW15_SDMg2$player2
HAW15_SDMg3 <- HAW15_SDMg2
HAW15_SDMg3$p1inp2vec <- is.element(HAW15_SDMg3$player1, player2vector)
HAW15_SDMg3$p2inp1vec <- is.element(HAW15_SDMg3$player2, player1vector)

addPlayer1 <- HAW15_SDMg3[ which(HAW15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_SDMg3[ which(HAW15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_SDMg2 <- rbind(HAW15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
HAW15_SDMft <- ftable(HAW15_SDMg2$player1, HAW15_SDMg2$player2)
HAW15_SDMft2 <- as.matrix(HAW15_SDMft)
numRows <- nrow(HAW15_SDMft2)
numCols <- ncol(HAW15_SDMft2)
HAW15_SDMft3 <- HAW15_SDMft2[c(2:numRows) , c(2:numCols)]
HAW15_SDMTable <- graph.adjacency(HAW15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(HAW15_SDMTable, vertex.label = V(HAW15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
HAW15_SDM.clusterCoef <- transitivity(HAW15_SDMTable, type="global") #cluster coefficient
HAW15_SDM.degreeCent <- centralization.degree(HAW15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_SDMftn <- as.network.matrix(HAW15_SDMft)
HAW15_SDM.netDensity <- network.density(HAW15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_SDM.entropy <- entropy(HAW15_SDMft) #entropy

HAW15_SDM.netMx <- cbind(HAW15_SDM.netMx, HAW15_SDM.clusterCoef, HAW15_SDM.degreeCent$centralization,
                         HAW15_SDM.netDensity, HAW15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
HAW15_TDMg2 <- data.frame(HAW15_TDM)
HAW15_TDMg2 <- HAW15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_TDMg2$player1
player2vector <- HAW15_TDMg2$player2
HAW15_TDMg3 <- HAW15_TDMg2
HAW15_TDMg3$p1inp2vec <- is.element(HAW15_TDMg3$player1, player2vector)
HAW15_TDMg3$p2inp1vec <- is.element(HAW15_TDMg3$player2, player1vector)

addPlayer1 <- HAW15_TDMg3[ which(HAW15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_TDMg3[ which(HAW15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_TDMg2 <- rbind(HAW15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
HAW15_TDMft <- ftable(HAW15_TDMg2$player1, HAW15_TDMg2$player2)
HAW15_TDMft2 <- as.matrix(HAW15_TDMft)
numRows <- nrow(HAW15_TDMft2)
numCols <- ncol(HAW15_TDMft2)
HAW15_TDMft3 <- HAW15_TDMft2[c(2:numRows) , c(2:numCols)]
HAW15_TDMTable <- graph.adjacency(HAW15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(HAW15_TDMTable, vertex.label = V(HAW15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
HAW15_TDM.clusterCoef <- transitivity(HAW15_TDMTable, type="global") #cluster coefficient
HAW15_TDM.degreeCent <- centralization.degree(HAW15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_TDMftn <- as.network.matrix(HAW15_TDMft)
HAW15_TDM.netDensity <- network.density(HAW15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_TDM.entropy <- entropy(HAW15_TDMft) #entropy

HAW15_TDM.netMx <- cbind(HAW15_TDM.netMx, HAW15_TDM.clusterCoef, HAW15_TDM.degreeCent$centralization,
                         HAW15_TDM.netDensity, HAW15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
HAW15_SDg2 <- data.frame(HAW15_SD)
HAW15_SDg2 <- HAW15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_SDg2$player1
player2vector <- HAW15_SDg2$player2
HAW15_SDg3 <- HAW15_SDg2
HAW15_SDg3$p1inp2vec <- is.element(HAW15_SDg3$player1, player2vector)
HAW15_SDg3$p2inp1vec <- is.element(HAW15_SDg3$player2, player1vector)

addPlayer1 <- HAW15_SDg3[ which(HAW15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_SDg3[ which(HAW15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_SDg2 <- rbind(HAW15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
HAW15_SDft <- ftable(HAW15_SDg2$player1, HAW15_SDg2$player2)
HAW15_SDft2 <- as.matrix(HAW15_SDft)
numRows <- nrow(HAW15_SDft2)
numCols <- ncol(HAW15_SDft2)
HAW15_SDft3 <- HAW15_SDft2[c(2:numRows) , c(2:numCols)]
HAW15_SDTable <- graph.adjacency(HAW15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(HAW15_SDTable, vertex.label = V(HAW15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
HAW15_SD.clusterCoef <- transitivity(HAW15_SDTable, type="global") #cluster coefficient
HAW15_SD.degreeCent <- centralization.degree(HAW15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_SDftn <- as.network.matrix(HAW15_SDft)
HAW15_SD.netDensity <- network.density(HAW15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_SD.entropy <- entropy(HAW15_SDft) #entropy

HAW15_SD.netMx <- cbind(HAW15_SD.netMx, HAW15_SD.clusterCoef, HAW15_SD.degreeCent$centralization,
                        HAW15_SD.netDensity, HAW15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
HAW15_TDg2 <- data.frame(HAW15_TD)
HAW15_TDg2 <- HAW15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_TDg2$player1
player2vector <- HAW15_TDg2$player2
HAW15_TDg3 <- HAW15_TDg2
HAW15_TDg3$p1inp2vec <- is.element(HAW15_TDg3$player1, player2vector)
HAW15_TDg3$p2inp1vec <- is.element(HAW15_TDg3$player2, player1vector)

addPlayer1 <- HAW15_TDg3[ which(HAW15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_TDg3[ which(HAW15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_TDg2 <- rbind(HAW15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
HAW15_TDft <- ftable(HAW15_TDg2$player1, HAW15_TDg2$player2)
HAW15_TDft2 <- as.matrix(HAW15_TDft)
numRows <- nrow(HAW15_TDft2)
numCols <- ncol(HAW15_TDft2)
HAW15_TDft3 <- HAW15_TDft2[c(2:numRows) , c(2:numCols)]
HAW15_TDTable <- graph.adjacency(HAW15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(HAW15_TDTable, vertex.label = V(HAW15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
HAW15_TD.clusterCoef <- transitivity(HAW15_TDTable, type="global") #cluster coefficient
HAW15_TD.degreeCent <- centralization.degree(HAW15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_TDftn <- as.network.matrix(HAW15_TDft)
HAW15_TD.netDensity <- network.density(HAW15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_TD.entropy <- entropy(HAW15_TDft) #entropy

HAW15_TD.netMx <- cbind(HAW15_TD.netMx, HAW15_TD.clusterCoef, HAW15_TD.degreeCent$centralization,
                        HAW15_TD.netDensity, HAW15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
HAW15_QTg2 <- data.frame(HAW15_QT)
HAW15_QTg2 <- HAW15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW15_QTg2$player1
player2vector <- HAW15_QTg2$player2
HAW15_QTg3 <- HAW15_QTg2
HAW15_QTg3$p1inp2vec <- is.element(HAW15_QTg3$player1, player2vector)
HAW15_QTg3$p2inp1vec <- is.element(HAW15_QTg3$player2, player1vector)

addPlayer1 <- HAW15_QTg3[ which(HAW15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW15_QTg3[ which(HAW15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW15_QTg2 <- rbind(HAW15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
HAW15_QTft <- ftable(HAW15_QTg2$player1, HAW15_QTg2$player2)
HAW15_QTft2 <- as.matrix(HAW15_QTft)
numRows <- nrow(HAW15_QTft2)
numCols <- ncol(HAW15_QTft2)
HAW15_QTft3 <- HAW15_QTft2[c(2:numRows) , c(2:numCols)]
HAW15_QTTable <- graph.adjacency(HAW15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(HAW15_QTTable, vertex.label = V(HAW15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
HAW15_QT.clusterCoef <- transitivity(HAW15_QTTable, type="global") #cluster coefficient
HAW15_QT.degreeCent <- centralization.degree(HAW15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW15_QTftn <- as.network.matrix(HAW15_QTft)
HAW15_QT.netDensity <- network.density(HAW15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW15_QT.entropy <- entropy(HAW15_QTft) #entropy

HAW15_QT.netMx <- cbind(HAW15_QT.netMx, HAW15_QT.clusterCoef, HAW15_QT.degreeCent$centralization,
                        HAW15_QT.netDensity, HAW15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW15_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "Goal_F"
MELB15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
MELB15_Gg2 <- data.frame(MELB15_G)
MELB15_Gg2 <- MELB15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_Gg2$player1
player2vector <- MELB15_Gg2$player2
MELB15_Gg3 <- MELB15_Gg2
MELB15_Gg3$p1inp2vec <- is.element(MELB15_Gg3$player1, player2vector)
MELB15_Gg3$p2inp1vec <- is.element(MELB15_Gg3$player2, player1vector)

addPlayer1 <- MELB15_Gg3[ which(MELB15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB15_Gg2 <- rbind(MELB15_Gg2, addPlayer1)

#ROUND 15, Goal graph using weighted edges
MELB15_Gft <- ftable(MELB15_Gg2$player1, MELB15_Gg2$player2)
MELB15_Gft2 <- as.matrix(MELB15_Gft)
numRows <- nrow(MELB15_Gft2)
numCols <- ncol(MELB15_Gft2)
MELB15_Gft3 <- MELB15_Gft2[c(2:numRows) , c(1:numCols)]
MELB15_GTable <- graph.adjacency(MELB15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(MELB15_GTable, vertex.label = V(MELB15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
MELB15_G.clusterCoef <- transitivity(MELB15_GTable, type="global") #cluster coefficient
MELB15_G.degreeCent <- centralization.degree(MELB15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_Gftn <- as.network.matrix(MELB15_Gft)
MELB15_G.netDensity <- network.density(MELB15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_G.entropy <- entropy(MELB15_Gft) #entropy

MELB15_G.netMx <- cbind(MELB15_G.netMx, MELB15_G.clusterCoef, MELB15_G.degreeCent$centralization,
                        MELB15_G.netDensity, MELB15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************

round = 15
teamName = "MELB"
KIoutcome = "Behind_F"
MELB15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
MELB15_Bg2 <- data.frame(MELB15_B)
MELB15_Bg2 <- MELB15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_Bg2$player1
player2vector <- MELB15_Bg2$player2
MELB15_Bg3 <- MELB15_Bg2
MELB15_Bg3$p1inp2vec <- is.element(MELB15_Bg3$player1, player2vector)
MELB15_Bg3$p2inp1vec <- is.element(MELB15_Bg3$player2, player1vector)

addPlayer1 <- MELB15_Bg3[ which(MELB15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB15_Bg3[ which(MELB15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_Bg2 <- rbind(MELB15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
MELB15_Bft <- ftable(MELB15_Bg2$player1, MELB15_Bg2$player2)
MELB15_Bft2 <- as.matrix(MELB15_Bft)
numRows <- nrow(MELB15_Bft2)
numCols <- ncol(MELB15_Bft2)
MELB15_Bft3 <- MELB15_Bft2[c(2:numRows) , c(2:numCols)]
MELB15_BTable <- graph.adjacency(MELB15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(MELB15_BTable, vertex.label = V(MELB15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
MELB15_B.clusterCoef <- transitivity(MELB15_BTable, type="global") #cluster coefficient
MELB15_B.degreeCent <- centralization.degree(MELB15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_Bftn <- as.network.matrix(MELB15_Bft)
MELB15_B.netDensity <- network.density(MELB15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_B.entropy <- entropy(MELB15_Bft) #entropy

MELB15_B.netMx <- cbind(MELB15_B.netMx, MELB15_B.clusterCoef, MELB15_B.degreeCent$centralization,
                        MELB15_B.netDensity, MELB15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
MELB15_SFg2 <- data.frame(MELB15_SF)
MELB15_SFg2 <- MELB15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_SFg2$player1
player2vector <- MELB15_SFg2$player2
MELB15_SFg3 <- MELB15_SFg2
MELB15_SFg3$p1inp2vec <- is.element(MELB15_SFg3$player1, player2vector)
MELB15_SFg3$p2inp1vec <- is.element(MELB15_SFg3$player2, player1vector)

addPlayer1 <- MELB15_SFg3[ which(MELB15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_SFg3[ which(MELB15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_SFg2 <- rbind(MELB15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
MELB15_SFft <- ftable(MELB15_SFg2$player1, MELB15_SFg2$player2)
MELB15_SFft2 <- as.matrix(MELB15_SFft)
numRows <- nrow(MELB15_SFft2)
numCols <- ncol(MELB15_SFft2)
MELB15_SFft3 <- MELB15_SFft2[c(2:numRows) , c(2:numCols)]
MELB15_SFTable <- graph.adjacency(MELB15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(MELB15_SFTable, vertex.label = V(MELB15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
MELB15_SF.clusterCoef <- transitivity(MELB15_SFTable, type="global") #cluster coefficient
MELB15_SF.degreeCent <- centralization.degree(MELB15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_SFftn <- as.network.matrix(MELB15_SFft)
MELB15_SF.netDensity <- network.density(MELB15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_SF.entropy <- entropy(MELB15_SFft) #entropy

MELB15_SF.netMx <- cbind(MELB15_SF.netMx, MELB15_SF.clusterCoef, MELB15_SF.degreeCent$centralization,
                         MELB15_SF.netDensity, MELB15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
MELB15_TFg2 <- data.frame(MELB15_TF)
MELB15_TFg2 <- MELB15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_TFg2$player1
player2vector <- MELB15_TFg2$player2
MELB15_TFg3 <- MELB15_TFg2
MELB15_TFg3$p1inp2vec <- is.element(MELB15_TFg3$player1, player2vector)
MELB15_TFg3$p2inp1vec <- is.element(MELB15_TFg3$player2, player1vector)

addPlayer1 <- MELB15_TFg3[ which(MELB15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_TFg3[ which(MELB15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_TFg2 <- rbind(MELB15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
MELB15_TFft <- ftable(MELB15_TFg2$player1, MELB15_TFg2$player2)
MELB15_TFft2 <- as.matrix(MELB15_TFft)
numRows <- nrow(MELB15_TFft2)
numCols <- ncol(MELB15_TFft2)
MELB15_TFft3 <- MELB15_TFft2[c(2:numRows) , c(2:numCols)]
MELB15_TFTable <- graph.adjacency(MELB15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(MELB15_TFTable, vertex.label = V(MELB15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
MELB15_TF.clusterCoef <- transitivity(MELB15_TFTable, type="global") #cluster coefficient
MELB15_TF.degreeCent <- centralization.degree(MELB15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_TFftn <- as.network.matrix(MELB15_TFft)
MELB15_TF.netDensity <- network.density(MELB15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_TF.entropy <- entropy(MELB15_TFft) #entropy

MELB15_TF.netMx <- cbind(MELB15_TF.netMx, MELB15_TF.clusterCoef, MELB15_TF.degreeCent$centralization,
                         MELB15_TF.netDensity, MELB15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
MELB15_SAMg2 <- data.frame(MELB15_SAM)
MELB15_SAMg2 <- MELB15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_SAMg2$player1
player2vector <- MELB15_SAMg2$player2
MELB15_SAMg3 <- MELB15_SAMg2
MELB15_SAMg3$p1inp2vec <- is.element(MELB15_SAMg3$player1, player2vector)
MELB15_SAMg3$p2inp1vec <- is.element(MELB15_SAMg3$player2, player1vector)

addPlayer1 <- MELB15_SAMg3[ which(MELB15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_SAMg3[ which(MELB15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_SAMg2 <- rbind(MELB15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
MELB15_SAMft <- ftable(MELB15_SAMg2$player1, MELB15_SAMg2$player2)
MELB15_SAMft2 <- as.matrix(MELB15_SAMft)
numRows <- nrow(MELB15_SAMft2)
numCols <- ncol(MELB15_SAMft2)
MELB15_SAMft3 <- MELB15_SAMft2[c(2:numRows) , c(2:numCols)]
MELB15_SAMTable <- graph.adjacency(MELB15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(MELB15_SAMTable, vertex.label = V(MELB15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
MELB15_SAM.clusterCoef <- transitivity(MELB15_SAMTable, type="global") #cluster coefficient
MELB15_SAM.degreeCent <- centralization.degree(MELB15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_SAMftn <- as.network.matrix(MELB15_SAMft)
MELB15_SAM.netDensity <- network.density(MELB15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_SAM.entropy <- entropy(MELB15_SAMft) #entropy

MELB15_SAM.netMx <- cbind(MELB15_SAM.netMx, MELB15_SAM.clusterCoef, MELB15_SAM.degreeCent$centralization,
                          MELB15_SAM.netDensity, MELB15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
MELB15_TAMg2 <- data.frame(MELB15_TAM)
MELB15_TAMg2 <- MELB15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_TAMg2$player1
player2vector <- MELB15_TAMg2$player2
MELB15_TAMg3 <- MELB15_TAMg2
MELB15_TAMg3$p1inp2vec <- is.element(MELB15_TAMg3$player1, player2vector)
MELB15_TAMg3$p2inp1vec <- is.element(MELB15_TAMg3$player2, player1vector)

addPlayer1 <- MELB15_TAMg3[ which(MELB15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_TAMg3[ which(MELB15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_TAMg2 <- rbind(MELB15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
MELB15_TAMft <- ftable(MELB15_TAMg2$player1, MELB15_TAMg2$player2)
MELB15_TAMft2 <- as.matrix(MELB15_TAMft)
numRows <- nrow(MELB15_TAMft2)
numCols <- ncol(MELB15_TAMft2)
MELB15_TAMft3 <- MELB15_TAMft2[c(2:numRows) , c(2:numCols)]
MELB15_TAMTable <- graph.adjacency(MELB15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(MELB15_TAMTable, vertex.label = V(MELB15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
MELB15_TAM.clusterCoef <- transitivity(MELB15_TAMTable, type="global") #cluster coefficient
MELB15_TAM.degreeCent <- centralization.degree(MELB15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_TAMftn <- as.network.matrix(MELB15_TAMft)
MELB15_TAM.netDensity <- network.density(MELB15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_TAM.entropy <- entropy(MELB15_TAMft) #entropy

MELB15_TAM.netMx <- cbind(MELB15_TAM.netMx, MELB15_TAM.clusterCoef, MELB15_TAM.degreeCent$centralization,
                          MELB15_TAM.netDensity, MELB15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
MELB15_SDMg2 <- data.frame(MELB15_SDM)
MELB15_SDMg2 <- MELB15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_SDMg2$player1
player2vector <- MELB15_SDMg2$player2
MELB15_SDMg3 <- MELB15_SDMg2
MELB15_SDMg3$p1inp2vec <- is.element(MELB15_SDMg3$player1, player2vector)
MELB15_SDMg3$p2inp1vec <- is.element(MELB15_SDMg3$player2, player1vector)

addPlayer1 <- MELB15_SDMg3[ which(MELB15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_SDMg3[ which(MELB15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_SDMg2 <- rbind(MELB15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
MELB15_SDMft <- ftable(MELB15_SDMg2$player1, MELB15_SDMg2$player2)
MELB15_SDMft2 <- as.matrix(MELB15_SDMft)
numRows <- nrow(MELB15_SDMft2)
numCols <- ncol(MELB15_SDMft2)
MELB15_SDMft3 <- MELB15_SDMft2[c(2:numRows) , c(2:numCols)]
MELB15_SDMTable <- graph.adjacency(MELB15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(MELB15_SDMTable, vertex.label = V(MELB15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
MELB15_SDM.clusterCoef <- transitivity(MELB15_SDMTable, type="global") #cluster coefficient
MELB15_SDM.degreeCent <- centralization.degree(MELB15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_SDMftn <- as.network.matrix(MELB15_SDMft)
MELB15_SDM.netDensity <- network.density(MELB15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_SDM.entropy <- entropy(MELB15_SDMft) #entropy

MELB15_SDM.netMx <- cbind(MELB15_SDM.netMx, MELB15_SDM.clusterCoef, MELB15_SDM.degreeCent$centralization,
                          MELB15_SDM.netDensity, MELB15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
MELB15_TDMg2 <- data.frame(MELB15_TDM)
MELB15_TDMg2 <- MELB15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_TDMg2$player1
player2vector <- MELB15_TDMg2$player2
MELB15_TDMg3 <- MELB15_TDMg2
MELB15_TDMg3$p1inp2vec <- is.element(MELB15_TDMg3$player1, player2vector)
MELB15_TDMg3$p2inp1vec <- is.element(MELB15_TDMg3$player2, player1vector)

addPlayer1 <- MELB15_TDMg3[ which(MELB15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_TDMg3[ which(MELB15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_TDMg2 <- rbind(MELB15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
MELB15_TDMft <- ftable(MELB15_TDMg2$player1, MELB15_TDMg2$player2)
MELB15_TDMft2 <- as.matrix(MELB15_TDMft)
numRows <- nrow(MELB15_TDMft2)
numCols <- ncol(MELB15_TDMft2)
MELB15_TDMft3 <- MELB15_TDMft2[c(2:numRows) , c(2:numCols)]
MELB15_TDMTable <- graph.adjacency(MELB15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(MELB15_TDMTable, vertex.label = V(MELB15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
MELB15_TDM.clusterCoef <- transitivity(MELB15_TDMTable, type="global") #cluster coefficient
MELB15_TDM.degreeCent <- centralization.degree(MELB15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_TDMftn <- as.network.matrix(MELB15_TDMft)
MELB15_TDM.netDensity <- network.density(MELB15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_TDM.entropy <- entropy(MELB15_TDMft) #entropy

MELB15_TDM.netMx <- cbind(MELB15_TDM.netMx, MELB15_TDM.clusterCoef, MELB15_TDM.degreeCent$centralization,
                          MELB15_TDM.netDensity, MELB15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
MELB15_SDg2 <- data.frame(MELB15_SD)
MELB15_SDg2 <- MELB15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_SDg2$player1
player2vector <- MELB15_SDg2$player2
MELB15_SDg3 <- MELB15_SDg2
MELB15_SDg3$p1inp2vec <- is.element(MELB15_SDg3$player1, player2vector)
MELB15_SDg3$p2inp1vec <- is.element(MELB15_SDg3$player2, player1vector)

addPlayer1 <- MELB15_SDg3[ which(MELB15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_SDg3[ which(MELB15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_SDg2 <- rbind(MELB15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
MELB15_SDft <- ftable(MELB15_SDg2$player1, MELB15_SDg2$player2)
MELB15_SDft2 <- as.matrix(MELB15_SDft)
numRows <- nrow(MELB15_SDft2)
numCols <- ncol(MELB15_SDft2)
MELB15_SDft3 <- MELB15_SDft2[c(2:numRows) , c(2:numCols)]
MELB15_SDTable <- graph.adjacency(MELB15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(MELB15_SDTable, vertex.label = V(MELB15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
MELB15_SD.clusterCoef <- transitivity(MELB15_SDTable, type="global") #cluster coefficient
MELB15_SD.degreeCent <- centralization.degree(MELB15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_SDftn <- as.network.matrix(MELB15_SDft)
MELB15_SD.netDensity <- network.density(MELB15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_SD.entropy <- entropy(MELB15_SDft) #entropy

MELB15_SD.netMx <- cbind(MELB15_SD.netMx, MELB15_SD.clusterCoef, MELB15_SD.degreeCent$centralization,
                         MELB15_SD.netDensity, MELB15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
MELB15_TDg2 <- data.frame(MELB15_TD)
MELB15_TDg2 <- MELB15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_TDg2$player1
player2vector <- MELB15_TDg2$player2
MELB15_TDg3 <- MELB15_TDg2
MELB15_TDg3$p1inp2vec <- is.element(MELB15_TDg3$player1, player2vector)
MELB15_TDg3$p2inp1vec <- is.element(MELB15_TDg3$player2, player1vector)

addPlayer1 <- MELB15_TDg3[ which(MELB15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_TDg3[ which(MELB15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_TDg2 <- rbind(MELB15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
MELB15_TDft <- ftable(MELB15_TDg2$player1, MELB15_TDg2$player2)
MELB15_TDft2 <- as.matrix(MELB15_TDft)
numRows <- nrow(MELB15_TDft2)
numCols <- ncol(MELB15_TDft2)
MELB15_TDft3 <- MELB15_TDft2[c(2:numRows) , c(2:numCols)]
MELB15_TDTable <- graph.adjacency(MELB15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(MELB15_TDTable, vertex.label = V(MELB15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
MELB15_TD.clusterCoef <- transitivity(MELB15_TDTable, type="global") #cluster coefficient
MELB15_TD.degreeCent <- centralization.degree(MELB15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_TDftn <- as.network.matrix(MELB15_TDft)
MELB15_TD.netDensity <- network.density(MELB15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_TD.entropy <- entropy(MELB15_TDft) #entropy

MELB15_TD.netMx <- cbind(MELB15_TD.netMx, MELB15_TD.clusterCoef, MELB15_TD.degreeCent$centralization,
                         MELB15_TD.netDensity, MELB15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
MELB15_QTg2 <- data.frame(MELB15_QT)
MELB15_QTg2 <- MELB15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB15_QTg2$player1
player2vector <- MELB15_QTg2$player2
MELB15_QTg3 <- MELB15_QTg2
MELB15_QTg3$p1inp2vec <- is.element(MELB15_QTg3$player1, player2vector)
MELB15_QTg3$p2inp1vec <- is.element(MELB15_QTg3$player2, player1vector)

addPlayer1 <- MELB15_QTg3[ which(MELB15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB15_QTg3[ which(MELB15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB15_QTg2 <- rbind(MELB15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
MELB15_QTft <- ftable(MELB15_QTg2$player1, MELB15_QTg2$player2)
MELB15_QTft2 <- as.matrix(MELB15_QTft)
numRows <- nrow(MELB15_QTft2)
numCols <- ncol(MELB15_QTft2)
MELB15_QTft3 <- MELB15_QTft2[c(2:numRows) , c(2:numCols)]
MELB15_QTTable <- graph.adjacency(MELB15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(MELB15_QTTable, vertex.label = V(MELB15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
MELB15_QT.clusterCoef <- transitivity(MELB15_QTTable, type="global") #cluster coefficient
MELB15_QT.degreeCent <- centralization.degree(MELB15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB15_QTftn <- as.network.matrix(MELB15_QTft)
MELB15_QT.netDensity <- network.density(MELB15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB15_QT.entropy <- entropy(MELB15_QTft) #entropy

MELB15_QT.netMx <- cbind(MELB15_QT.netMx, MELB15_QT.clusterCoef, MELB15_QT.degreeCent$centralization,
                         MELB15_QT.netDensity, MELB15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB15_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
NMFC15_Gg2 <- data.frame(NMFC15_G)
NMFC15_Gg2 <- NMFC15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_Gg2$player1
player2vector <- NMFC15_Gg2$player2
NMFC15_Gg3 <- NMFC15_Gg2
NMFC15_Gg3$p1inp2vec <- is.element(NMFC15_Gg3$player1, player2vector)
NMFC15_Gg3$p2inp1vec <- is.element(NMFC15_Gg3$player2, player1vector)

addPlayer1 <- NMFC15_Gg3[ which(NMFC15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_Gg3[ which(NMFC15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_Gg2 <- rbind(NMFC15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
NMFC15_Gft <- ftable(NMFC15_Gg2$player1, NMFC15_Gg2$player2)
NMFC15_Gft2 <- as.matrix(NMFC15_Gft)
numRows <- nrow(NMFC15_Gft2)
numCols <- ncol(NMFC15_Gft2)
NMFC15_Gft3 <- NMFC15_Gft2[c(2:numRows) , c(2:numCols)]
NMFC15_GTable <- graph.adjacency(NMFC15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(NMFC15_GTable, vertex.label = V(NMFC15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
NMFC15_G.clusterCoef <- transitivity(NMFC15_GTable, type="global") #cluster coefficient
NMFC15_G.degreeCent <- centralization.degree(NMFC15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_Gftn <- as.network.matrix(NMFC15_Gft)
NMFC15_G.netDensity <- network.density(NMFC15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_G.entropy <- entropy(NMFC15_Gft) #entropy

NMFC15_G.netMx <- cbind(NMFC15_G.netMx, NMFC15_G.clusterCoef, NMFC15_G.degreeCent$centralization,
                        NMFC15_G.netDensity, NMFC15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
NMFC15_Bg2 <- data.frame(NMFC15_B)
NMFC15_Bg2 <- NMFC15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_Bg2$player1
player2vector <- NMFC15_Bg2$player2
NMFC15_Bg3 <- NMFC15_Bg2
NMFC15_Bg3$p1inp2vec <- is.element(NMFC15_Bg3$player1, player2vector)
NMFC15_Bg3$p2inp1vec <- is.element(NMFC15_Bg3$player2, player1vector)

addPlayer1 <- NMFC15_Bg3[ which(NMFC15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_Bg3[ which(NMFC15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_Bg2 <- rbind(NMFC15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
NMFC15_Bft <- ftable(NMFC15_Bg2$player1, NMFC15_Bg2$player2)
NMFC15_Bft2 <- as.matrix(NMFC15_Bft)
numRows <- nrow(NMFC15_Bft2)
numCols <- ncol(NMFC15_Bft2)
NMFC15_Bft3 <- NMFC15_Bft2[c(2:numRows) , c(2:numCols)]
NMFC15_BTable <- graph.adjacency(NMFC15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(NMFC15_BTable, vertex.label = V(NMFC15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
NMFC15_B.clusterCoef <- transitivity(NMFC15_BTable, type="global") #cluster coefficient
NMFC15_B.degreeCent <- centralization.degree(NMFC15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_Bftn <- as.network.matrix(NMFC15_Bft)
NMFC15_B.netDensity <- network.density(NMFC15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_B.entropy <- entropy(NMFC15_Bft) #entropy

NMFC15_B.netMx <- cbind(NMFC15_B.netMx, NMFC15_B.clusterCoef, NMFC15_B.degreeCent$centralization,
                        NMFC15_B.netDensity, NMFC15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
NMFC15_SFg2 <- data.frame(NMFC15_SF)
NMFC15_SFg2 <- NMFC15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_SFg2$player1
player2vector <- NMFC15_SFg2$player2
NMFC15_SFg3 <- NMFC15_SFg2
NMFC15_SFg3$p1inp2vec <- is.element(NMFC15_SFg3$player1, player2vector)
NMFC15_SFg3$p2inp1vec <- is.element(NMFC15_SFg3$player2, player1vector)

addPlayer1 <- NMFC15_SFg3[ which(NMFC15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_SFg3[ which(NMFC15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_SFg2 <- rbind(NMFC15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
NMFC15_SFft <- ftable(NMFC15_SFg2$player1, NMFC15_SFg2$player2)
NMFC15_SFft2 <- as.matrix(NMFC15_SFft)
numRows <- nrow(NMFC15_SFft2)
numCols <- ncol(NMFC15_SFft2)
NMFC15_SFft3 <- NMFC15_SFft2[c(2:numRows) , c(2:numCols)]
NMFC15_SFTable <- graph.adjacency(NMFC15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(NMFC15_SFTable, vertex.label = V(NMFC15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
NMFC15_SF.clusterCoef <- transitivity(NMFC15_SFTable, type="global") #cluster coefficient
NMFC15_SF.degreeCent <- centralization.degree(NMFC15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_SFftn <- as.network.matrix(NMFC15_SFft)
NMFC15_SF.netDensity <- network.density(NMFC15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_SF.entropy <- entropy(NMFC15_SFft) #entropy

NMFC15_SF.netMx <- cbind(NMFC15_SF.netMx, NMFC15_SF.clusterCoef, NMFC15_SF.degreeCent$centralization,
                         NMFC15_SF.netDensity, NMFC15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
NMFC15_TFg2 <- data.frame(NMFC15_TF)
NMFC15_TFg2 <- NMFC15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_TFg2$player1
player2vector <- NMFC15_TFg2$player2
NMFC15_TFg3 <- NMFC15_TFg2
NMFC15_TFg3$p1inp2vec <- is.element(NMFC15_TFg3$player1, player2vector)
NMFC15_TFg3$p2inp1vec <- is.element(NMFC15_TFg3$player2, player1vector)

addPlayer1 <- NMFC15_TFg3[ which(NMFC15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_TFg3[ which(NMFC15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_TFg2 <- rbind(NMFC15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
NMFC15_TFft <- ftable(NMFC15_TFg2$player1, NMFC15_TFg2$player2)
NMFC15_TFft2 <- as.matrix(NMFC15_TFft)
numRows <- nrow(NMFC15_TFft2)
numCols <- ncol(NMFC15_TFft2)
NMFC15_TFft3 <- NMFC15_TFft2[c(2:numRows) , c(2:numCols)]
NMFC15_TFTable <- graph.adjacency(NMFC15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(NMFC15_TFTable, vertex.label = V(NMFC15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
NMFC15_TF.clusterCoef <- transitivity(NMFC15_TFTable, type="global") #cluster coefficient
NMFC15_TF.degreeCent <- centralization.degree(NMFC15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_TFftn <- as.network.matrix(NMFC15_TFft)
NMFC15_TF.netDensity <- network.density(NMFC15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_TF.entropy <- entropy(NMFC15_TFft) #entropy

NMFC15_TF.netMx <- cbind(NMFC15_TF.netMx, NMFC15_TF.clusterCoef, NMFC15_TF.degreeCent$centralization,
                         NMFC15_TF.netDensity, NMFC15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
NMFC15_SAMg2 <- data.frame(NMFC15_SAM)
NMFC15_SAMg2 <- NMFC15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_SAMg2$player1
player2vector <- NMFC15_SAMg2$player2
NMFC15_SAMg3 <- NMFC15_SAMg2
NMFC15_SAMg3$p1inp2vec <- is.element(NMFC15_SAMg3$player1, player2vector)
NMFC15_SAMg3$p2inp1vec <- is.element(NMFC15_SAMg3$player2, player1vector)

addPlayer1 <- NMFC15_SAMg3[ which(NMFC15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_SAMg3[ which(NMFC15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_SAMg2 <- rbind(NMFC15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
NMFC15_SAMft <- ftable(NMFC15_SAMg2$player1, NMFC15_SAMg2$player2)
NMFC15_SAMft2 <- as.matrix(NMFC15_SAMft)
numRows <- nrow(NMFC15_SAMft2)
numCols <- ncol(NMFC15_SAMft2)
NMFC15_SAMft3 <- NMFC15_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC15_SAMTable <- graph.adjacency(NMFC15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(NMFC15_SAMTable, vertex.label = V(NMFC15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
NMFC15_SAM.clusterCoef <- transitivity(NMFC15_SAMTable, type="global") #cluster coefficient
NMFC15_SAM.degreeCent <- centralization.degree(NMFC15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_SAMftn <- as.network.matrix(NMFC15_SAMft)
NMFC15_SAM.netDensity <- network.density(NMFC15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_SAM.entropy <- entropy(NMFC15_SAMft) #entropy

NMFC15_SAM.netMx <- cbind(NMFC15_SAM.netMx, NMFC15_SAM.clusterCoef, NMFC15_SAM.degreeCent$centralization,
                          NMFC15_SAM.netDensity, NMFC15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
NMFC15_TAMg2 <- data.frame(NMFC15_TAM)
NMFC15_TAMg2 <- NMFC15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_TAMg2$player1
player2vector <- NMFC15_TAMg2$player2
NMFC15_TAMg3 <- NMFC15_TAMg2
NMFC15_TAMg3$p1inp2vec <- is.element(NMFC15_TAMg3$player1, player2vector)
NMFC15_TAMg3$p2inp1vec <- is.element(NMFC15_TAMg3$player2, player1vector)

addPlayer1 <- NMFC15_TAMg3[ which(NMFC15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_TAMg3[ which(NMFC15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_TAMg2 <- rbind(NMFC15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
NMFC15_TAMft <- ftable(NMFC15_TAMg2$player1, NMFC15_TAMg2$player2)
NMFC15_TAMft2 <- as.matrix(NMFC15_TAMft)
numRows <- nrow(NMFC15_TAMft2)
numCols <- ncol(NMFC15_TAMft2)
NMFC15_TAMft3 <- NMFC15_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC15_TAMTable <- graph.adjacency(NMFC15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(NMFC15_TAMTable, vertex.label = V(NMFC15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
NMFC15_TAM.clusterCoef <- transitivity(NMFC15_TAMTable, type="global") #cluster coefficient
NMFC15_TAM.degreeCent <- centralization.degree(NMFC15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_TAMftn <- as.network.matrix(NMFC15_TAMft)
NMFC15_TAM.netDensity <- network.density(NMFC15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_TAM.entropy <- entropy(NMFC15_TAMft) #entropy

NMFC15_TAM.netMx <- cbind(NMFC15_TAM.netMx, NMFC15_TAM.clusterCoef, NMFC15_TAM.degreeCent$centralization,
                          NMFC15_TAM.netDensity, NMFC15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************

round = 15
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
NMFC15_SDMg2 <- data.frame(NMFC15_SDM)
NMFC15_SDMg2 <- NMFC15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_SDMg2$player1
player2vector <- NMFC15_SDMg2$player2
NMFC15_SDMg3 <- NMFC15_SDMg2
NMFC15_SDMg3$p1inp2vec <- is.element(NMFC15_SDMg3$player1, player2vector)
NMFC15_SDMg3$p2inp1vec <- is.element(NMFC15_SDMg3$player2, player1vector)

addPlayer1 <- NMFC15_SDMg3[ which(NMFC15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_SDMg3[ which(NMFC15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_SDMg2 <- rbind(NMFC15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
NMFC15_SDMft <- ftable(NMFC15_SDMg2$player1, NMFC15_SDMg2$player2)
NMFC15_SDMft2 <- as.matrix(NMFC15_SDMft)
numRows <- nrow(NMFC15_SDMft2)
numCols <- ncol(NMFC15_SDMft2)
NMFC15_SDMft3 <- NMFC15_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC15_SDMTable <- graph.adjacency(NMFC15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(NMFC15_SDMTable, vertex.label = V(NMFC15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
NMFC15_SDM.clusterCoef <- transitivity(NMFC15_SDMTable, type="global") #cluster coefficient
NMFC15_SDM.degreeCent <- centralization.degree(NMFC15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_SDMftn <- as.network.matrix(NMFC15_SDMft)
NMFC15_SDM.netDensity <- network.density(NMFC15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_SDM.entropy <- entropy(NMFC15_SDMft) #entropy

NMFC15_SDM.netMx <- cbind(NMFC15_SDM.netMx, NMFC15_SDM.clusterCoef, NMFC15_SDM.degreeCent$centralization,
                          NMFC15_SDM.netDensity, NMFC15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
NMFC15_TDMg2 <- data.frame(NMFC15_TDM)
NMFC15_TDMg2 <- NMFC15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_TDMg2$player1
player2vector <- NMFC15_TDMg2$player2
NMFC15_TDMg3 <- NMFC15_TDMg2
NMFC15_TDMg3$p1inp2vec <- is.element(NMFC15_TDMg3$player1, player2vector)
NMFC15_TDMg3$p2inp1vec <- is.element(NMFC15_TDMg3$player2, player1vector)

addPlayer1 <- NMFC15_TDMg3[ which(NMFC15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_TDMg3[ which(NMFC15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_TDMg2 <- rbind(NMFC15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
NMFC15_TDMft <- ftable(NMFC15_TDMg2$player1, NMFC15_TDMg2$player2)
NMFC15_TDMft2 <- as.matrix(NMFC15_TDMft)
numRows <- nrow(NMFC15_TDMft2)
numCols <- ncol(NMFC15_TDMft2)
NMFC15_TDMft3 <- NMFC15_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC15_TDMTable <- graph.adjacency(NMFC15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(NMFC15_TDMTable, vertex.label = V(NMFC15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
NMFC15_TDM.clusterCoef <- transitivity(NMFC15_TDMTable, type="global") #cluster coefficient
NMFC15_TDM.degreeCent <- centralization.degree(NMFC15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_TDMftn <- as.network.matrix(NMFC15_TDMft)
NMFC15_TDM.netDensity <- network.density(NMFC15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_TDM.entropy <- entropy(NMFC15_TDMft) #entropy

NMFC15_TDM.netMx <- cbind(NMFC15_TDM.netMx, NMFC15_TDM.clusterCoef, NMFC15_TDM.degreeCent$centralization,
                          NMFC15_TDM.netDensity, NMFC15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
NMFC15_SDg2 <- data.frame(NMFC15_SD)
NMFC15_SDg2 <- NMFC15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_SDg2$player1
player2vector <- NMFC15_SDg2$player2
NMFC15_SDg3 <- NMFC15_SDg2
NMFC15_SDg3$p1inp2vec <- is.element(NMFC15_SDg3$player1, player2vector)
NMFC15_SDg3$p2inp1vec <- is.element(NMFC15_SDg3$player2, player1vector)

addPlayer1 <- NMFC15_SDg3[ which(NMFC15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_SDg3[ which(NMFC15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_SDg2 <- rbind(NMFC15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
NMFC15_SDft <- ftable(NMFC15_SDg2$player1, NMFC15_SDg2$player2)
NMFC15_SDft2 <- as.matrix(NMFC15_SDft)
numRows <- nrow(NMFC15_SDft2)
numCols <- ncol(NMFC15_SDft2)
NMFC15_SDft3 <- NMFC15_SDft2[c(2:numRows) , c(2:numCols)]
NMFC15_SDTable <- graph.adjacency(NMFC15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(NMFC15_SDTable, vertex.label = V(NMFC15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
NMFC15_SD.clusterCoef <- transitivity(NMFC15_SDTable, type="global") #cluster coefficient
NMFC15_SD.degreeCent <- centralization.degree(NMFC15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_SDftn <- as.network.matrix(NMFC15_SDft)
NMFC15_SD.netDensity <- network.density(NMFC15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_SD.entropy <- entropy(NMFC15_SDft) #entropy

NMFC15_SD.netMx <- cbind(NMFC15_SD.netMx, NMFC15_SD.clusterCoef, NMFC15_SD.degreeCent$centralization,
                         NMFC15_SD.netDensity, NMFC15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
NMFC15_TDg2 <- data.frame(NMFC15_TD)
NMFC15_TDg2 <- NMFC15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_TDg2$player1
player2vector <- NMFC15_TDg2$player2
NMFC15_TDg3 <- NMFC15_TDg2
NMFC15_TDg3$p1inp2vec <- is.element(NMFC15_TDg3$player1, player2vector)
NMFC15_TDg3$p2inp1vec <- is.element(NMFC15_TDg3$player2, player1vector)

addPlayer1 <- NMFC15_TDg3[ which(NMFC15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_TDg3[ which(NMFC15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_TDg2 <- rbind(NMFC15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
NMFC15_TDft <- ftable(NMFC15_TDg2$player1, NMFC15_TDg2$player2)
NMFC15_TDft2 <- as.matrix(NMFC15_TDft)
numRows <- nrow(NMFC15_TDft2)
numCols <- ncol(NMFC15_TDft2)
NMFC15_TDft3 <- NMFC15_TDft2[c(2:numRows) , c(2:numCols)]
NMFC15_TDTable <- graph.adjacency(NMFC15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(NMFC15_TDTable, vertex.label = V(NMFC15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
NMFC15_TD.clusterCoef <- transitivity(NMFC15_TDTable, type="global") #cluster coefficient
NMFC15_TD.degreeCent <- centralization.degree(NMFC15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_TDftn <- as.network.matrix(NMFC15_TDft)
NMFC15_TD.netDensity <- network.density(NMFC15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_TD.entropy <- entropy(NMFC15_TDft) #entropy

NMFC15_TD.netMx <- cbind(NMFC15_TD.netMx, NMFC15_TD.clusterCoef, NMFC15_TD.degreeCent$centralization,
                         NMFC15_TD.netDensity, NMFC15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
NMFC15_QTg2 <- data.frame(NMFC15_QT)
NMFC15_QTg2 <- NMFC15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC15_QTg2$player1
player2vector <- NMFC15_QTg2$player2
NMFC15_QTg3 <- NMFC15_QTg2
NMFC15_QTg3$p1inp2vec <- is.element(NMFC15_QTg3$player1, player2vector)
NMFC15_QTg3$p2inp1vec <- is.element(NMFC15_QTg3$player2, player1vector)

addPlayer1 <- NMFC15_QTg3[ which(NMFC15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC15_QTg3[ which(NMFC15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC15_QTg2 <- rbind(NMFC15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
NMFC15_QTft <- ftable(NMFC15_QTg2$player1, NMFC15_QTg2$player2)
NMFC15_QTft2 <- as.matrix(NMFC15_QTft)
numRows <- nrow(NMFC15_QTft2)
numCols <- ncol(NMFC15_QTft2)
NMFC15_QTft3 <- NMFC15_QTft2[c(2:numRows) , c(2:numCols)]
NMFC15_QTTable <- graph.adjacency(NMFC15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(NMFC15_QTTable, vertex.label = V(NMFC15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
NMFC15_QT.clusterCoef <- transitivity(NMFC15_QTTable, type="global") #cluster coefficient
NMFC15_QT.degreeCent <- centralization.degree(NMFC15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC15_QTftn <- as.network.matrix(NMFC15_QTft)
NMFC15_QT.netDensity <- network.density(NMFC15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC15_QT.entropy <- entropy(NMFC15_QTft) #entropy

NMFC15_QT.netMx <- cbind(NMFC15_QT.netMx, NMFC15_QT.clusterCoef, NMFC15_QT.degreeCent$centralization,
                         NMFC15_QT.netDensity, NMFC15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC15_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Goal_F"
PORT15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
PORT15_Gg2 <- data.frame(PORT15_G)
PORT15_Gg2 <- PORT15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_Gg2$player1
player2vector <- PORT15_Gg2$player2
PORT15_Gg3 <- PORT15_Gg2
PORT15_Gg3$p1inp2vec <- is.element(PORT15_Gg3$player1, player2vector)
PORT15_Gg3$p2inp1vec <- is.element(PORT15_Gg3$player2, player1vector)

addPlayer1 <- PORT15_Gg3[ which(PORT15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT15_Gg2 <- rbind(PORT15_Gg2, addPlayer1)

#ROUND 15, Goal graph using weighted edges
PORT15_Gft <- ftable(PORT15_Gg2$player1, PORT15_Gg2$player2)
PORT15_Gft2 <- as.matrix(PORT15_Gft)
numRows <- nrow(PORT15_Gft2)
numCols <- ncol(PORT15_Gft2)
PORT15_Gft3 <- PORT15_Gft2[c(2:numRows) , c(1:numCols)]
PORT15_GTable <- graph.adjacency(PORT15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(PORT15_GTable, vertex.label = V(PORT15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
PORT15_G.clusterCoef <- transitivity(PORT15_GTable, type="global") #cluster coefficient
PORT15_G.degreeCent <- centralization.degree(PORT15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_Gftn <- as.network.matrix(PORT15_Gft)
PORT15_G.netDensity <- network.density(PORT15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_G.entropy <- entropy(PORT15_Gft) #entropy

PORT15_G.netMx <- cbind(PORT15_G.netMx, PORT15_G.clusterCoef, PORT15_G.degreeCent$centralization,
                        PORT15_G.netDensity, PORT15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Behind_F"
PORT15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
PORT15_Bg2 <- data.frame(PORT15_B)
PORT15_Bg2 <- PORT15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_Bg2$player1
player2vector <- PORT15_Bg2$player2
PORT15_Bg3 <- PORT15_Bg2
PORT15_Bg3$p1inp2vec <- is.element(PORT15_Bg3$player1, player2vector)
PORT15_Bg3$p2inp1vec <- is.element(PORT15_Bg3$player2, player1vector)

addPlayer1 <- PORT15_Bg3[ which(PORT15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_Bg3[ which(PORT15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_Bg2 <- rbind(PORT15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
PORT15_Bft <- ftable(PORT15_Bg2$player1, PORT15_Bg2$player2)
PORT15_Bft2 <- as.matrix(PORT15_Bft)
numRows <- nrow(PORT15_Bft2)
numCols <- ncol(PORT15_Bft2)
PORT15_Bft3 <- PORT15_Bft2[c(2:numRows) , c(2:numCols)]
PORT15_BTable <- graph.adjacency(PORT15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(PORT15_BTable, vertex.label = V(PORT15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
PORT15_B.clusterCoef <- transitivity(PORT15_BTable, type="global") #cluster coefficient
PORT15_B.degreeCent <- centralization.degree(PORT15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_Bftn <- as.network.matrix(PORT15_Bft)
PORT15_B.netDensity <- network.density(PORT15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_B.entropy <- entropy(PORT15_Bft) #entropy

PORT15_B.netMx <- cbind(PORT15_B.netMx, PORT15_B.clusterCoef, PORT15_B.degreeCent$centralization,
                        PORT15_B.netDensity, PORT15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
PORT15_SFg2 <- data.frame(PORT15_SF)
PORT15_SFg2 <- PORT15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_SFg2$player1
player2vector <- PORT15_SFg2$player2
PORT15_SFg3 <- PORT15_SFg2
PORT15_SFg3$p1inp2vec <- is.element(PORT15_SFg3$player1, player2vector)
PORT15_SFg3$p2inp1vec <- is.element(PORT15_SFg3$player2, player1vector)

addPlayer1 <- PORT15_SFg3[ which(PORT15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_SFg3[ which(PORT15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_SFg2 <- rbind(PORT15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
PORT15_SFft <- ftable(PORT15_SFg2$player1, PORT15_SFg2$player2)
PORT15_SFft2 <- as.matrix(PORT15_SFft)
numRows <- nrow(PORT15_SFft2)
numCols <- ncol(PORT15_SFft2)
PORT15_SFft3 <- PORT15_SFft2[c(2:numRows) , c(2:numCols)]
PORT15_SFTable <- graph.adjacency(PORT15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(PORT15_SFTable, vertex.label = V(PORT15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
PORT15_SF.clusterCoef <- transitivity(PORT15_SFTable, type="global") #cluster coefficient
PORT15_SF.degreeCent <- centralization.degree(PORT15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_SFftn <- as.network.matrix(PORT15_SFft)
PORT15_SF.netDensity <- network.density(PORT15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_SF.entropy <- entropy(PORT15_SFft) #entropy

PORT15_SF.netMx <- cbind(PORT15_SF.netMx, PORT15_SF.clusterCoef, PORT15_SF.degreeCent$centralization,
                         PORT15_SF.netDensity, PORT15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
PORT15_TFg2 <- data.frame(PORT15_TF)
PORT15_TFg2 <- PORT15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_TFg2$player1
player2vector <- PORT15_TFg2$player2
PORT15_TFg3 <- PORT15_TFg2
PORT15_TFg3$p1inp2vec <- is.element(PORT15_TFg3$player1, player2vector)
PORT15_TFg3$p2inp1vec <- is.element(PORT15_TFg3$player2, player1vector)

addPlayer1 <- PORT15_TFg3[ which(PORT15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT15_TFg2 <- rbind(PORT15_TFg2, addPlayer1)

#ROUND 15, FWD Turnover graph using weighted edges
PORT15_TFft <- ftable(PORT15_TFg2$player1, PORT15_TFg2$player2)
PORT15_TFft2 <- as.matrix(PORT15_TFft)
numRows <- nrow(PORT15_TFft2)
numCols <- ncol(PORT15_TFft2)
PORT15_TFft3 <- PORT15_TFft2[c(2:numRows) , c(1:numCols)]
PORT15_TFTable <- graph.adjacency(PORT15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(PORT15_TFTable, vertex.label = V(PORT15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
PORT15_TF.clusterCoef <- transitivity(PORT15_TFTable, type="global") #cluster coefficient
PORT15_TF.degreeCent <- centralization.degree(PORT15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_TFftn <- as.network.matrix(PORT15_TFft)
PORT15_TF.netDensity <- network.density(PORT15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_TF.entropy <- entropy(PORT15_TFft) #entropy

PORT15_TF.netMx <- cbind(PORT15_TF.netMx, PORT15_TF.clusterCoef, PORT15_TF.degreeCent$centralization,
                         PORT15_TF.netDensity, PORT15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
PORT15_SAMg2 <- data.frame(PORT15_SAM)
PORT15_SAMg2 <- PORT15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_SAMg2$player1
player2vector <- PORT15_SAMg2$player2
PORT15_SAMg3 <- PORT15_SAMg2
PORT15_SAMg3$p1inp2vec <- is.element(PORT15_SAMg3$player1, player2vector)
PORT15_SAMg3$p2inp1vec <- is.element(PORT15_SAMg3$player2, player1vector)

addPlayer1 <- PORT15_SAMg3[ which(PORT15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_SAMg3[ which(PORT15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_SAMg2 <- rbind(PORT15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
PORT15_SAMft <- ftable(PORT15_SAMg2$player1, PORT15_SAMg2$player2)
PORT15_SAMft2 <- as.matrix(PORT15_SAMft)
numRows <- nrow(PORT15_SAMft2)
numCols <- ncol(PORT15_SAMft2)
PORT15_SAMft3 <- PORT15_SAMft2[c(2:numRows) , c(2:numCols)]
PORT15_SAMTable <- graph.adjacency(PORT15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(PORT15_SAMTable, vertex.label = V(PORT15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
PORT15_SAM.clusterCoef <- transitivity(PORT15_SAMTable, type="global") #cluster coefficient
PORT15_SAM.degreeCent <- centralization.degree(PORT15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_SAMftn <- as.network.matrix(PORT15_SAMft)
PORT15_SAM.netDensity <- network.density(PORT15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_SAM.entropy <- entropy(PORT15_SAMft) #entropy

PORT15_SAM.netMx <- cbind(PORT15_SAM.netMx, PORT15_SAM.clusterCoef, PORT15_SAM.degreeCent$centralization,
                          PORT15_SAM.netDensity, PORT15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
PORT15_TAMg2 <- data.frame(PORT15_TAM)
PORT15_TAMg2 <- PORT15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_TAMg2$player1
player2vector <- PORT15_TAMg2$player2
PORT15_TAMg3 <- PORT15_TAMg2
PORT15_TAMg3$p1inp2vec <- is.element(PORT15_TAMg3$player1, player2vector)
PORT15_TAMg3$p2inp1vec <- is.element(PORT15_TAMg3$player2, player1vector)

addPlayer1 <- PORT15_TAMg3[ which(PORT15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT15_TAMg2 <- rbind(PORT15_TAMg2, addPlayer1)

#ROUND 15, AM Turnover graph using weighted edges
PORT15_TAMft <- ftable(PORT15_TAMg2$player1, PORT15_TAMg2$player2)
PORT15_TAMft2 <- as.matrix(PORT15_TAMft)
numRows <- nrow(PORT15_TAMft2)
numCols <- ncol(PORT15_TAMft2)
PORT15_TAMft3 <- PORT15_TAMft2[c(2:numRows) , c(1:numCols)]
PORT15_TAMTable <- graph.adjacency(PORT15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(PORT15_TAMTable, vertex.label = V(PORT15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
PORT15_TAM.clusterCoef <- transitivity(PORT15_TAMTable, type="global") #cluster coefficient
PORT15_TAM.degreeCent <- centralization.degree(PORT15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_TAMftn <- as.network.matrix(PORT15_TAMft)
PORT15_TAM.netDensity <- network.density(PORT15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_TAM.entropy <- entropy(PORT15_TAMft) #entropy

PORT15_TAM.netMx <- cbind(PORT15_TAM.netMx, PORT15_TAM.clusterCoef, PORT15_TAM.degreeCent$centralization,
                          PORT15_TAM.netDensity, PORT15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
PORT15_SDMg2 <- data.frame(PORT15_SDM)
PORT15_SDMg2 <- PORT15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_SDMg2$player1
player2vector <- PORT15_SDMg2$player2
PORT15_SDMg3 <- PORT15_SDMg2
PORT15_SDMg3$p1inp2vec <- is.element(PORT15_SDMg3$player1, player2vector)
PORT15_SDMg3$p2inp1vec <- is.element(PORT15_SDMg3$player2, player1vector)

addPlayer1 <- PORT15_SDMg3[ which(PORT15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_SDMg3[ which(PORT15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_SDMg2 <- rbind(PORT15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
PORT15_SDMft <- ftable(PORT15_SDMg2$player1, PORT15_SDMg2$player2)
PORT15_SDMft2 <- as.matrix(PORT15_SDMft)
numRows <- nrow(PORT15_SDMft2)
numCols <- ncol(PORT15_SDMft2)
PORT15_SDMft3 <- PORT15_SDMft2[c(2:numRows) , c(2:numCols)]
PORT15_SDMTable <- graph.adjacency(PORT15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(PORT15_SDMTable, vertex.label = V(PORT15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
PORT15_SDM.clusterCoef <- transitivity(PORT15_SDMTable, type="global") #cluster coefficient
PORT15_SDM.degreeCent <- centralization.degree(PORT15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_SDMftn <- as.network.matrix(PORT15_SDMft)
PORT15_SDM.netDensity <- network.density(PORT15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_SDM.entropy <- entropy(PORT15_SDMft) #entropy

PORT15_SDM.netMx <- cbind(PORT15_SDM.netMx, PORT15_SDM.clusterCoef, PORT15_SDM.degreeCent$centralization,
                          PORT15_SDM.netDensity, PORT15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
PORT15_TDMg2 <- data.frame(PORT15_TDM)
PORT15_TDMg2 <- PORT15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_TDMg2$player1
player2vector <- PORT15_TDMg2$player2
PORT15_TDMg3 <- PORT15_TDMg2
PORT15_TDMg3$p1inp2vec <- is.element(PORT15_TDMg3$player1, player2vector)
PORT15_TDMg3$p2inp1vec <- is.element(PORT15_TDMg3$player2, player1vector)

addPlayer1 <- PORT15_TDMg3[ which(PORT15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT15_TDMg3[ which(PORT15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_TDMg2 <- rbind(PORT15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
PORT15_TDMft <- ftable(PORT15_TDMg2$player1, PORT15_TDMg2$player2)
PORT15_TDMft2 <- as.matrix(PORT15_TDMft)
numRows <- nrow(PORT15_TDMft2)
numCols <- ncol(PORT15_TDMft2)
PORT15_TDMft3 <- PORT15_TDMft2[c(2:numRows) , c(2:numCols)]
PORT15_TDMTable <- graph.adjacency(PORT15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(PORT15_TDMTable, vertex.label = V(PORT15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
PORT15_TDM.clusterCoef <- transitivity(PORT15_TDMTable, type="global") #cluster coefficient
PORT15_TDM.degreeCent <- centralization.degree(PORT15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_TDMftn <- as.network.matrix(PORT15_TDMft)
PORT15_TDM.netDensity <- network.density(PORT15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_TDM.entropy <- entropy(PORT15_TDMft) #entropy

PORT15_TDM.netMx <- cbind(PORT15_TDM.netMx, PORT15_TDM.clusterCoef, PORT15_TDM.degreeCent$centralization,
                          PORT15_TDM.netDensity, PORT15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
PORT15_SDg2 <- data.frame(PORT15_SD)
PORT15_SDg2 <- PORT15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_SDg2$player1
player2vector <- PORT15_SDg2$player2
PORT15_SDg3 <- PORT15_SDg2
PORT15_SDg3$p1inp2vec <- is.element(PORT15_SDg3$player1, player2vector)
PORT15_SDg3$p2inp1vec <- is.element(PORT15_SDg3$player2, player1vector)

addPlayer1 <- PORT15_SDg3[ which(PORT15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_SDg3[ which(PORT15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_SDg2 <- rbind(PORT15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
PORT15_SDft <- ftable(PORT15_SDg2$player1, PORT15_SDg2$player2)
PORT15_SDft2 <- as.matrix(PORT15_SDft)
numRows <- nrow(PORT15_SDft2)
numCols <- ncol(PORT15_SDft2)
PORT15_SDft3 <- PORT15_SDft2[c(2:numRows) , c(2:numCols)]
PORT15_SDTable <- graph.adjacency(PORT15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(PORT15_SDTable, vertex.label = V(PORT15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
PORT15_SD.clusterCoef <- transitivity(PORT15_SDTable, type="global") #cluster coefficient
PORT15_SD.degreeCent <- centralization.degree(PORT15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_SDftn <- as.network.matrix(PORT15_SDft)
PORT15_SD.netDensity <- network.density(PORT15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_SD.entropy <- entropy(PORT15_SDft) #entropy

PORT15_SD.netMx <- cbind(PORT15_SD.netMx, PORT15_SD.clusterCoef, PORT15_SD.degreeCent$centralization,
                         PORT15_SD.netDensity, PORT15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
PORT15_TDg2 <- data.frame(PORT15_TD)
PORT15_TDg2 <- PORT15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_TDg2$player1
player2vector <- PORT15_TDg2$player2
PORT15_TDg3 <- PORT15_TDg2
PORT15_TDg3$p1inp2vec <- is.element(PORT15_TDg3$player1, player2vector)
PORT15_TDg3$p2inp1vec <- is.element(PORT15_TDg3$player2, player1vector)

addPlayer1 <- PORT15_TDg3[ which(PORT15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_TDg3[ which(PORT15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_TDg2 <- rbind(PORT15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
PORT15_TDft <- ftable(PORT15_TDg2$player1, PORT15_TDg2$player2)
PORT15_TDft2 <- as.matrix(PORT15_TDft)
numRows <- nrow(PORT15_TDft2)
numCols <- ncol(PORT15_TDft2)
PORT15_TDft3 <- PORT15_TDft2[c(2:numRows) , c(2:numCols)]
PORT15_TDTable <- graph.adjacency(PORT15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(PORT15_TDTable, vertex.label = V(PORT15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
PORT15_TD.clusterCoef <- transitivity(PORT15_TDTable, type="global") #cluster coefficient
PORT15_TD.degreeCent <- centralization.degree(PORT15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_TDftn <- as.network.matrix(PORT15_TDft)
PORT15_TD.netDensity <- network.density(PORT15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_TD.entropy <- entropy(PORT15_TDft) #entropy

PORT15_TD.netMx <- cbind(PORT15_TD.netMx, PORT15_TD.clusterCoef, PORT15_TD.degreeCent$centralization,
                         PORT15_TD.netDensity, PORT15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
PORT15_QTg2 <- data.frame(PORT15_QT)
PORT15_QTg2 <- PORT15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT15_QTg2$player1
player2vector <- PORT15_QTg2$player2
PORT15_QTg3 <- PORT15_QTg2
PORT15_QTg3$p1inp2vec <- is.element(PORT15_QTg3$player1, player2vector)
PORT15_QTg3$p2inp1vec <- is.element(PORT15_QTg3$player2, player1vector)

addPlayer1 <- PORT15_QTg3[ which(PORT15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT15_QTg3[ which(PORT15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT15_QTg2 <- rbind(PORT15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
PORT15_QTft <- ftable(PORT15_QTg2$player1, PORT15_QTg2$player2)
PORT15_QTft2 <- as.matrix(PORT15_QTft)
numRows <- nrow(PORT15_QTft2)
numCols <- ncol(PORT15_QTft2)
PORT15_QTft3 <- PORT15_QTft2[c(2:numRows) , c(2:numCols)]
PORT15_QTTable <- graph.adjacency(PORT15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(PORT15_QTTable, vertex.label = V(PORT15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
PORT15_QT.clusterCoef <- transitivity(PORT15_QTTable, type="global") #cluster coefficient
PORT15_QT.degreeCent <- centralization.degree(PORT15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT15_QTftn <- as.network.matrix(PORT15_QTft)
PORT15_QT.netDensity <- network.density(PORT15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT15_QT.entropy <- entropy(PORT15_QTft) #entropy

PORT15_QT.netMx <- cbind(PORT15_QT.netMx, PORT15_QT.clusterCoef, PORT15_QT.degreeCent$centralization,
                         PORT15_QT.netDensity, PORT15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT15_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "RICH"
KIoutcome = "Goal_F"
RICH15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
RICH15_Gg2 <- data.frame(RICH15_G)
RICH15_Gg2 <- RICH15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_Gg2$player1
player2vector <- RICH15_Gg2$player2
RICH15_Gg3 <- RICH15_Gg2
RICH15_Gg3$p1inp2vec <- is.element(RICH15_Gg3$player1, player2vector)
RICH15_Gg3$p2inp1vec <- is.element(RICH15_Gg3$player2, player1vector)

addPlayer1 <- RICH15_Gg3[ which(RICH15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_Gg3[ which(RICH15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_Gg2 <- rbind(RICH15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
RICH15_Gft <- ftable(RICH15_Gg2$player1, RICH15_Gg2$player2)
RICH15_Gft2 <- as.matrix(RICH15_Gft)
numRows <- nrow(RICH15_Gft2)
numCols <- ncol(RICH15_Gft2)
RICH15_Gft3 <- RICH15_Gft2[c(2:numRows) , c(2:numCols)]
RICH15_GTable <- graph.adjacency(RICH15_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(RICH15_GTable, vertex.label = V(RICH15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
RICH15_G.clusterCoef <- transitivity(RICH15_GTable, type="global") #cluster coefficient
RICH15_G.degreeCent <- centralization.degree(RICH15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_Gftn <- as.network.matrix(RICH15_Gft)
RICH15_G.netDensity <- network.density(RICH15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_G.entropy <- entropy(RICH15_Gft) #entropy

RICH15_G.netMx <- cbind(RICH15_G.netMx, RICH15_G.clusterCoef, RICH15_G.degreeCent$centralization,
                        RICH15_G.netDensity, RICH15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "RICH"
KIoutcome = "Behind_F"
RICH15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
RICH15_Bg2 <- data.frame(RICH15_B)
RICH15_Bg2 <- RICH15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_Bg2$player1
player2vector <- RICH15_Bg2$player2
RICH15_Bg3 <- RICH15_Bg2
RICH15_Bg3$p1inp2vec <- is.element(RICH15_Bg3$player1, player2vector)
RICH15_Bg3$p2inp1vec <- is.element(RICH15_Bg3$player2, player1vector)

addPlayer1 <- RICH15_Bg3[ which(RICH15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_Bg3[ which(RICH15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_Bg2 <- rbind(RICH15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
RICH15_Bft <- ftable(RICH15_Bg2$player1, RICH15_Bg2$player2)
RICH15_Bft2 <- as.matrix(RICH15_Bft)
numRows <- nrow(RICH15_Bft2)
numCols <- ncol(RICH15_Bft2)
RICH15_Bft3 <- RICH15_Bft2[c(2:numRows) , c(2:numCols)]
RICH15_BTable <- graph.adjacency(RICH15_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(RICH15_BTable, vertex.label = V(RICH15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
RICH15_B.clusterCoef <- transitivity(RICH15_BTable, type="global") #cluster coefficient
RICH15_B.degreeCent <- centralization.degree(RICH15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_Bftn <- as.network.matrix(RICH15_Bft)
RICH15_B.netDensity <- network.density(RICH15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_B.entropy <- entropy(RICH15_Bft) #entropy

RICH15_B.netMx <- cbind(RICH15_B.netMx, RICH15_B.clusterCoef, RICH15_B.degreeCent$centralization,
                        RICH15_B.netDensity, RICH15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************

round = 15
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
RICH15_SFg2 <- data.frame(RICH15_SF)
RICH15_SFg2 <- RICH15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_SFg2$player1
player2vector <- RICH15_SFg2$player2
RICH15_SFg3 <- RICH15_SFg2
RICH15_SFg3$p1inp2vec <- is.element(RICH15_SFg3$player1, player2vector)
RICH15_SFg3$p2inp1vec <- is.element(RICH15_SFg3$player2, player1vector)

addPlayer1 <- RICH15_SFg3[ which(RICH15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_SFg3[ which(RICH15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_SFg2 <- rbind(RICH15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
RICH15_SFft <- ftable(RICH15_SFg2$player1, RICH15_SFg2$player2)
RICH15_SFft2 <- as.matrix(RICH15_SFft)
numRows <- nrow(RICH15_SFft2)
numCols <- ncol(RICH15_SFft2)
RICH15_SFft3 <- RICH15_SFft2[c(2:numRows) , c(2:numCols)]
RICH15_SFTable <- graph.adjacency(RICH15_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(RICH15_SFTable, vertex.label = V(RICH15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
RICH15_SF.clusterCoef <- transitivity(RICH15_SFTable, type="global") #cluster coefficient
RICH15_SF.degreeCent <- centralization.degree(RICH15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_SFftn <- as.network.matrix(RICH15_SFft)
RICH15_SF.netDensity <- network.density(RICH15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_SF.entropy <- entropy(RICH15_SFft) #entropy

RICH15_SF.netMx <- cbind(RICH15_SF.netMx, RICH15_SF.clusterCoef, RICH15_SF.degreeCent$centralization,
                         RICH15_SF.netDensity, RICH15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
RICH15_TFg2 <- data.frame(RICH15_TF)
RICH15_TFg2 <- RICH15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_TFg2$player1
player2vector <- RICH15_TFg2$player2
RICH15_TFg3 <- RICH15_TFg2
RICH15_TFg3$p1inp2vec <- is.element(RICH15_TFg3$player1, player2vector)
RICH15_TFg3$p2inp1vec <- is.element(RICH15_TFg3$player2, player1vector)

addPlayer1 <- RICH15_TFg3[ which(RICH15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_TFg3[ which(RICH15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_TFg2 <- rbind(RICH15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
RICH15_TFft <- ftable(RICH15_TFg2$player1, RICH15_TFg2$player2)
RICH15_TFft2 <- as.matrix(RICH15_TFft)
numRows <- nrow(RICH15_TFft2)
numCols <- ncol(RICH15_TFft2)
RICH15_TFft3 <- RICH15_TFft2[c(2:numRows) , c(2:numCols)]
RICH15_TFTable <- graph.adjacency(RICH15_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(RICH15_TFTable, vertex.label = V(RICH15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
RICH15_TF.clusterCoef <- transitivity(RICH15_TFTable, type="global") #cluster coefficient
RICH15_TF.degreeCent <- centralization.degree(RICH15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_TFftn <- as.network.matrix(RICH15_TFft)
RICH15_TF.netDensity <- network.density(RICH15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_TF.entropy <- entropy(RICH15_TFft) #entropy

RICH15_TF.netMx <- cbind(RICH15_TF.netMx, RICH15_TF.clusterCoef, RICH15_TF.degreeCent$centralization,
                         RICH15_TF.netDensity, RICH15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
RICH15_SAMg2 <- data.frame(RICH15_SAM)
RICH15_SAMg2 <- RICH15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_SAMg2$player1
player2vector <- RICH15_SAMg2$player2
RICH15_SAMg3 <- RICH15_SAMg2
RICH15_SAMg3$p1inp2vec <- is.element(RICH15_SAMg3$player1, player2vector)
RICH15_SAMg3$p2inp1vec <- is.element(RICH15_SAMg3$player2, player1vector)

addPlayer1 <- RICH15_SAMg3[ which(RICH15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_SAMg3[ which(RICH15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_SAMg2 <- rbind(RICH15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
RICH15_SAMft <- ftable(RICH15_SAMg2$player1, RICH15_SAMg2$player2)
RICH15_SAMft2 <- as.matrix(RICH15_SAMft)
numRows <- nrow(RICH15_SAMft2)
numCols <- ncol(RICH15_SAMft2)
RICH15_SAMft3 <- RICH15_SAMft2[c(2:numRows) , c(2:numCols)]
RICH15_SAMTable <- graph.adjacency(RICH15_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(RICH15_SAMTable, vertex.label = V(RICH15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
RICH15_SAM.clusterCoef <- transitivity(RICH15_SAMTable, type="global") #cluster coefficient
RICH15_SAM.degreeCent <- centralization.degree(RICH15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_SAMftn <- as.network.matrix(RICH15_SAMft)
RICH15_SAM.netDensity <- network.density(RICH15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_SAM.entropy <- entropy(RICH15_SAMft) #entropy

RICH15_SAM.netMx <- cbind(RICH15_SAM.netMx, RICH15_SAM.clusterCoef, RICH15_SAM.degreeCent$centralization,
                          RICH15_SAM.netDensity, RICH15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
RICH15_TAMg2 <- data.frame(RICH15_TAM)
RICH15_TAMg2 <- RICH15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_TAMg2$player1
player2vector <- RICH15_TAMg2$player2
RICH15_TAMg3 <- RICH15_TAMg2
RICH15_TAMg3$p1inp2vec <- is.element(RICH15_TAMg3$player1, player2vector)
RICH15_TAMg3$p2inp1vec <- is.element(RICH15_TAMg3$player2, player1vector)

addPlayer1 <- RICH15_TAMg3[ which(RICH15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_TAMg3[ which(RICH15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_TAMg2 <- rbind(RICH15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
RICH15_TAMft <- ftable(RICH15_TAMg2$player1, RICH15_TAMg2$player2)
RICH15_TAMft2 <- as.matrix(RICH15_TAMft)
numRows <- nrow(RICH15_TAMft2)
numCols <- ncol(RICH15_TAMft2)
RICH15_TAMft3 <- RICH15_TAMft2[c(2:numRows) , c(2:numCols)]
RICH15_TAMTable <- graph.adjacency(RICH15_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(RICH15_TAMTable, vertex.label = V(RICH15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
RICH15_TAM.clusterCoef <- transitivity(RICH15_TAMTable, type="global") #cluster coefficient
RICH15_TAM.degreeCent <- centralization.degree(RICH15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_TAMftn <- as.network.matrix(RICH15_TAMft)
RICH15_TAM.netDensity <- network.density(RICH15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_TAM.entropy <- entropy(RICH15_TAMft) #entropy

RICH15_TAM.netMx <- cbind(RICH15_TAM.netMx, RICH15_TAM.clusterCoef, RICH15_TAM.degreeCent$centralization,
                          RICH15_TAM.netDensity, RICH15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
RICH15_SDMg2 <- data.frame(RICH15_SDM)
RICH15_SDMg2 <- RICH15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_SDMg2$player1
player2vector <- RICH15_SDMg2$player2
RICH15_SDMg3 <- RICH15_SDMg2
RICH15_SDMg3$p1inp2vec <- is.element(RICH15_SDMg3$player1, player2vector)
RICH15_SDMg3$p2inp1vec <- is.element(RICH15_SDMg3$player2, player1vector)

addPlayer1 <- RICH15_SDMg3[ which(RICH15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_SDMg3[ which(RICH15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_SDMg2 <- rbind(RICH15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
RICH15_SDMft <- ftable(RICH15_SDMg2$player1, RICH15_SDMg2$player2)
RICH15_SDMft2 <- as.matrix(RICH15_SDMft)
numRows <- nrow(RICH15_SDMft2)
numCols <- ncol(RICH15_SDMft2)
RICH15_SDMft3 <- RICH15_SDMft2[c(2:numRows) , c(2:numCols)]
RICH15_SDMTable <- graph.adjacency(RICH15_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(RICH15_SDMTable, vertex.label = V(RICH15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
RICH15_SDM.clusterCoef <- transitivity(RICH15_SDMTable, type="global") #cluster coefficient
RICH15_SDM.degreeCent <- centralization.degree(RICH15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_SDMftn <- as.network.matrix(RICH15_SDMft)
RICH15_SDM.netDensity <- network.density(RICH15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_SDM.entropy <- entropy(RICH15_SDMft) #entropy

RICH15_SDM.netMx <- cbind(RICH15_SDM.netMx, RICH15_SDM.clusterCoef, RICH15_SDM.degreeCent$centralization,
                          RICH15_SDM.netDensity, RICH15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
RICH15_TDMg2 <- data.frame(RICH15_TDM)
RICH15_TDMg2 <- RICH15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_TDMg2$player1
player2vector <- RICH15_TDMg2$player2
RICH15_TDMg3 <- RICH15_TDMg2
RICH15_TDMg3$p1inp2vec <- is.element(RICH15_TDMg3$player1, player2vector)
RICH15_TDMg3$p2inp1vec <- is.element(RICH15_TDMg3$player2, player1vector)

addPlayer1 <- RICH15_TDMg3[ which(RICH15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_TDMg3[ which(RICH15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_TDMg2 <- rbind(RICH15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
RICH15_TDMft <- ftable(RICH15_TDMg2$player1, RICH15_TDMg2$player2)
RICH15_TDMft2 <- as.matrix(RICH15_TDMft)
numRows <- nrow(RICH15_TDMft2)
numCols <- ncol(RICH15_TDMft2)
RICH15_TDMft3 <- RICH15_TDMft2[c(2:numRows) , c(2:numCols)]
RICH15_TDMTable <- graph.adjacency(RICH15_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(RICH15_TDMTable, vertex.label = V(RICH15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
RICH15_TDM.clusterCoef <- transitivity(RICH15_TDMTable, type="global") #cluster coefficient
RICH15_TDM.degreeCent <- centralization.degree(RICH15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_TDMftn <- as.network.matrix(RICH15_TDMft)
RICH15_TDM.netDensity <- network.density(RICH15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_TDM.entropy <- entropy(RICH15_TDMft) #entropy

RICH15_TDM.netMx <- cbind(RICH15_TDM.netMx, RICH15_TDM.clusterCoef, RICH15_TDM.degreeCent$centralization,
                          RICH15_TDM.netDensity, RICH15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
RICH15_SDg2 <- data.frame(RICH15_SD)
RICH15_SDg2 <- RICH15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_SDg2$player1
player2vector <- RICH15_SDg2$player2
RICH15_SDg3 <- RICH15_SDg2
RICH15_SDg3$p1inp2vec <- is.element(RICH15_SDg3$player1, player2vector)
RICH15_SDg3$p2inp1vec <- is.element(RICH15_SDg3$player2, player1vector)

addPlayer1 <- RICH15_SDg3[ which(RICH15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_SDg3[ which(RICH15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_SDg2 <- rbind(RICH15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
RICH15_SDft <- ftable(RICH15_SDg2$player1, RICH15_SDg2$player2)
RICH15_SDft2 <- as.matrix(RICH15_SDft)
numRows <- nrow(RICH15_SDft2)
numCols <- ncol(RICH15_SDft2)
RICH15_SDft3 <- RICH15_SDft2[c(2:numRows) , c(2:numCols)]
RICH15_SDTable <- graph.adjacency(RICH15_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(RICH15_SDTable, vertex.label = V(RICH15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
RICH15_SD.clusterCoef <- transitivity(RICH15_SDTable, type="global") #cluster coefficient
RICH15_SD.degreeCent <- centralization.degree(RICH15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_SDftn <- as.network.matrix(RICH15_SDft)
RICH15_SD.netDensity <- network.density(RICH15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_SD.entropy <- entropy(RICH15_SDft) #entropy

RICH15_SD.netMx <- cbind(RICH15_SD.netMx, RICH15_SD.clusterCoef, RICH15_SD.degreeCent$centralization,
                         RICH15_SD.netDensity, RICH15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
RICH15_TDg2 <- data.frame(RICH15_TD)
RICH15_TDg2 <- RICH15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_TDg2$player1
player2vector <- RICH15_TDg2$player2
RICH15_TDg3 <- RICH15_TDg2
RICH15_TDg3$p1inp2vec <- is.element(RICH15_TDg3$player1, player2vector)
RICH15_TDg3$p2inp1vec <- is.element(RICH15_TDg3$player2, player1vector)

addPlayer1 <- RICH15_TDg3[ which(RICH15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_TDg3[ which(RICH15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_TDg2 <- rbind(RICH15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
RICH15_TDft <- ftable(RICH15_TDg2$player1, RICH15_TDg2$player2)
RICH15_TDft2 <- as.matrix(RICH15_TDft)
numRows <- nrow(RICH15_TDft2)
numCols <- ncol(RICH15_TDft2)
RICH15_TDft3 <- RICH15_TDft2[c(2:numRows) , c(2:numCols)]
RICH15_TDTable <- graph.adjacency(RICH15_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(RICH15_TDTable, vertex.label = V(RICH15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
RICH15_TD.clusterCoef <- transitivity(RICH15_TDTable, type="global") #cluster coefficient
RICH15_TD.degreeCent <- centralization.degree(RICH15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_TDftn <- as.network.matrix(RICH15_TDft)
RICH15_TD.netDensity <- network.density(RICH15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_TD.entropy <- entropy(RICH15_TDft) #entropy

RICH15_TD.netMx <- cbind(RICH15_TD.netMx, RICH15_TD.clusterCoef, RICH15_TD.degreeCent$centralization,
                         RICH15_TD.netDensity, RICH15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
RICH15_QTg2 <- data.frame(RICH15_QT)
RICH15_QTg2 <- RICH15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH15_QTg2$player1
player2vector <- RICH15_QTg2$player2
RICH15_QTg3 <- RICH15_QTg2
RICH15_QTg3$p1inp2vec <- is.element(RICH15_QTg3$player1, player2vector)
RICH15_QTg3$p2inp1vec <- is.element(RICH15_QTg3$player2, player1vector)

addPlayer1 <- RICH15_QTg3[ which(RICH15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH15_QTg3[ which(RICH15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH15_QTg2 <- rbind(RICH15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
RICH15_QTft <- ftable(RICH15_QTg2$player1, RICH15_QTg2$player2)
RICH15_QTft2 <- as.matrix(RICH15_QTft)
numRows <- nrow(RICH15_QTft2)
numCols <- ncol(RICH15_QTft2)
RICH15_QTft3 <- RICH15_QTft2[c(2:numRows) , c(2:numCols)]
RICH15_QTTable <- graph.adjacency(RICH15_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(RICH15_QTTable, vertex.label = V(RICH15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
RICH15_QT.clusterCoef <- transitivity(RICH15_QTTable, type="global") #cluster coefficient
RICH15_QT.degreeCent <- centralization.degree(RICH15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH15_QTftn <- as.network.matrix(RICH15_QTft)
RICH15_QT.netDensity <- network.density(RICH15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH15_QT.entropy <- entropy(RICH15_QTft) #entropy

RICH15_QT.netMx <- cbind(RICH15_QT.netMx, RICH15_QT.clusterCoef, RICH15_QT.degreeCent$centralization,
                         RICH15_QT.netDensity, RICH15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH15_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "STK"
KIoutcome = "Goal_F"
STK15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
STK15_Gg2 <- data.frame(STK15_G)
STK15_Gg2 <- STK15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_Gg2$player1
player2vector <- STK15_Gg2$player2
STK15_Gg3 <- STK15_Gg2
STK15_Gg3$p1inp2vec <- is.element(STK15_Gg3$player1, player2vector)
STK15_Gg3$p2inp1vec <- is.element(STK15_Gg3$player2, player1vector)

addPlayer1 <- STK15_Gg3[ which(STK15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_Gg3[ which(STK15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_Gg2 <- rbind(STK15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
STK15_Gft <- ftable(STK15_Gg2$player1, STK15_Gg2$player2)
STK15_Gft2 <- as.matrix(STK15_Gft)
numRows <- nrow(STK15_Gft2)
numCols <- ncol(STK15_Gft2)
STK15_Gft3 <- STK15_Gft2[c(2:numRows) , c(2:numCols)]
STK15_GTable <- graph.adjacency(STK15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(STK15_GTable, vertex.label = V(STK15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
STK15_G.clusterCoef <- transitivity(STK15_GTable, type="global") #cluster coefficient
STK15_G.degreeCent <- centralization.degree(STK15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_Gftn <- as.network.matrix(STK15_Gft)
STK15_G.netDensity <- network.density(STK15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_G.entropy <- entropy(STK15_Gft) #entropy

STK15_G.netMx <- cbind(STK15_G.netMx, STK15_G.clusterCoef, STK15_G.degreeCent$centralization,
                       STK15_G.netDensity, STK15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************

round = 15
teamName = "STK"
KIoutcome = "Behind_F"
STK15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
STK15_Bg2 <- data.frame(STK15_B)
STK15_Bg2 <- STK15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_Bg2$player1
player2vector <- STK15_Bg2$player2
STK15_Bg3 <- STK15_Bg2
STK15_Bg3$p1inp2vec <- is.element(STK15_Bg3$player1, player2vector)
STK15_Bg3$p2inp1vec <- is.element(STK15_Bg3$player2, player1vector)

addPlayer1 <- STK15_Bg3[ which(STK15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_Bg3[ which(STK15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_Bg2 <- rbind(STK15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
STK15_Bft <- ftable(STK15_Bg2$player1, STK15_Bg2$player2)
STK15_Bft2 <- as.matrix(STK15_Bft)
numRows <- nrow(STK15_Bft2)
numCols <- ncol(STK15_Bft2)
STK15_Bft3 <- STK15_Bft2[c(2:numRows) , c(2:numCols)]
STK15_BTable <- graph.adjacency(STK15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(STK15_BTable, vertex.label = V(STK15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
STK15_B.clusterCoef <- transitivity(STK15_BTable, type="global") #cluster coefficient
STK15_B.degreeCent <- centralization.degree(STK15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_Bftn <- as.network.matrix(STK15_Bft)
STK15_B.netDensity <- network.density(STK15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_B.entropy <- entropy(STK15_Bft) #entropy

STK15_B.netMx <- cbind(STK15_B.netMx, STK15_B.clusterCoef, STK15_B.degreeCent$centralization,
                       STK15_B.netDensity, STK15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************

round = 15
teamName = "STK"
KIoutcome = "Stoppage_F"
STK15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
STK15_SFg2 <- data.frame(STK15_SF)
STK15_SFg2 <- STK15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_SFg2$player1
player2vector <- STK15_SFg2$player2
STK15_SFg3 <- STK15_SFg2
STK15_SFg3$p1inp2vec <- is.element(STK15_SFg3$player1, player2vector)
STK15_SFg3$p2inp1vec <- is.element(STK15_SFg3$player2, player1vector)

addPlayer1 <- STK15_SFg3[ which(STK15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_SFg3[ which(STK15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_SFg2 <- rbind(STK15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
STK15_SFft <- ftable(STK15_SFg2$player1, STK15_SFg2$player2)
STK15_SFft2 <- as.matrix(STK15_SFft)
numRows <- nrow(STK15_SFft2)
numCols <- ncol(STK15_SFft2)
STK15_SFft3 <- STK15_SFft2[c(2:numRows) , c(2:numCols)]
STK15_SFTable <- graph.adjacency(STK15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(STK15_SFTable, vertex.label = V(STK15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
STK15_SF.clusterCoef <- transitivity(STK15_SFTable, type="global") #cluster coefficient
STK15_SF.degreeCent <- centralization.degree(STK15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_SFftn <- as.network.matrix(STK15_SFft)
STK15_SF.netDensity <- network.density(STK15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_SF.entropy <- entropy(STK15_SFft) #entropy

STK15_SF.netMx <- cbind(STK15_SF.netMx, STK15_SF.clusterCoef, STK15_SF.degreeCent$centralization,
                        STK15_SF.netDensity, STK15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "STK"
KIoutcome = "Turnover_F"
STK15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
STK15_TFg2 <- data.frame(STK15_TF)
STK15_TFg2 <- STK15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_TFg2$player1
player2vector <- STK15_TFg2$player2
STK15_TFg3 <- STK15_TFg2
STK15_TFg3$p1inp2vec <- is.element(STK15_TFg3$player1, player2vector)
STK15_TFg3$p2inp1vec <- is.element(STK15_TFg3$player2, player1vector)

addPlayer1 <- STK15_TFg3[ which(STK15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK15_TFg3[ which(STK15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_TFg2 <- rbind(STK15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
STK15_TFft <- ftable(STK15_TFg2$player1, STK15_TFg2$player2)
STK15_TFft2 <- as.matrix(STK15_TFft)
numRows <- nrow(STK15_TFft2)
numCols <- ncol(STK15_TFft2)
STK15_TFft3 <- STK15_TFft2[c(2:numRows) , c(2:numCols)]
STK15_TFTable <- graph.adjacency(STK15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(STK15_TFTable, vertex.label = V(STK15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
STK15_TF.clusterCoef <- transitivity(STK15_TFTable, type="global") #cluster coefficient
STK15_TF.degreeCent <- centralization.degree(STK15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_TFftn <- as.network.matrix(STK15_TFft)
STK15_TF.netDensity <- network.density(STK15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_TF.entropy <- entropy(STK15_TFft) #entropy

STK15_TF.netMx <- cbind(STK15_TF.netMx, STK15_TF.clusterCoef, STK15_TF.degreeCent$centralization,
                        STK15_TF.netDensity, STK15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
STK15_SAMg2 <- data.frame(STK15_SAM)
STK15_SAMg2 <- STK15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_SAMg2$player1
player2vector <- STK15_SAMg2$player2
STK15_SAMg3 <- STK15_SAMg2
STK15_SAMg3$p1inp2vec <- is.element(STK15_SAMg3$player1, player2vector)
STK15_SAMg3$p2inp1vec <- is.element(STK15_SAMg3$player2, player1vector)

addPlayer1 <- STK15_SAMg3[ which(STK15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_SAMg3[ which(STK15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_SAMg2 <- rbind(STK15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
STK15_SAMft <- ftable(STK15_SAMg2$player1, STK15_SAMg2$player2)
STK15_SAMft2 <- as.matrix(STK15_SAMft)
numRows <- nrow(STK15_SAMft2)
numCols <- ncol(STK15_SAMft2)
STK15_SAMft3 <- STK15_SAMft2[c(2:numRows) , c(2:numCols)]
STK15_SAMTable <- graph.adjacency(STK15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(STK15_SAMTable, vertex.label = V(STK15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
STK15_SAM.clusterCoef <- transitivity(STK15_SAMTable, type="global") #cluster coefficient
STK15_SAM.degreeCent <- centralization.degree(STK15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_SAMftn <- as.network.matrix(STK15_SAMft)
STK15_SAM.netDensity <- network.density(STK15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_SAM.entropy <- entropy(STK15_SAMft) #entropy

STK15_SAM.netMx <- cbind(STK15_SAM.netMx, STK15_SAM.clusterCoef, STK15_SAM.degreeCent$centralization,
                         STK15_SAM.netDensity, STK15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "STK"
KIoutcome = "Turnover_AM"
STK15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
STK15_TAMg2 <- data.frame(STK15_TAM)
STK15_TAMg2 <- STK15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_TAMg2$player1
player2vector <- STK15_TAMg2$player2
STK15_TAMg3 <- STK15_TAMg2
STK15_TAMg3$p1inp2vec <- is.element(STK15_TAMg3$player1, player2vector)
STK15_TAMg3$p2inp1vec <- is.element(STK15_TAMg3$player2, player1vector)

addPlayer1 <- STK15_TAMg3[ which(STK15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_TAMg3[ which(STK15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_TAMg2 <- rbind(STK15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
STK15_TAMft <- ftable(STK15_TAMg2$player1, STK15_TAMg2$player2)
STK15_TAMft2 <- as.matrix(STK15_TAMft)
numRows <- nrow(STK15_TAMft2)
numCols <- ncol(STK15_TAMft2)
STK15_TAMft3 <- STK15_TAMft2[c(2:numRows) , c(2:numCols)]
STK15_TAMTable <- graph.adjacency(STK15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(STK15_TAMTable, vertex.label = V(STK15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
STK15_TAM.clusterCoef <- transitivity(STK15_TAMTable, type="global") #cluster coefficient
STK15_TAM.degreeCent <- centralization.degree(STK15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_TAMftn <- as.network.matrix(STK15_TAMft)
STK15_TAM.netDensity <- network.density(STK15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_TAM.entropy <- entropy(STK15_TAMft) #entropy

STK15_TAM.netMx <- cbind(STK15_TAM.netMx, STK15_TAM.clusterCoef, STK15_TAM.degreeCent$centralization,
                         STK15_TAM.netDensity, STK15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
STK15_SDMg2 <- data.frame(STK15_SDM)
STK15_SDMg2 <- STK15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_SDMg2$player1
player2vector <- STK15_SDMg2$player2
STK15_SDMg3 <- STK15_SDMg2
STK15_SDMg3$p1inp2vec <- is.element(STK15_SDMg3$player1, player2vector)
STK15_SDMg3$p2inp1vec <- is.element(STK15_SDMg3$player2, player1vector)

addPlayer1 <- STK15_SDMg3[ which(STK15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_SDMg3[ which(STK15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_SDMg2 <- rbind(STK15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
STK15_SDMft <- ftable(STK15_SDMg2$player1, STK15_SDMg2$player2)
STK15_SDMft2 <- as.matrix(STK15_SDMft)
numRows <- nrow(STK15_SDMft2)
numCols <- ncol(STK15_SDMft2)
STK15_SDMft3 <- STK15_SDMft2[c(2:numRows) , c(2:numCols)]
STK15_SDMTable <- graph.adjacency(STK15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(STK15_SDMTable, vertex.label = V(STK15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
STK15_SDM.clusterCoef <- transitivity(STK15_SDMTable, type="global") #cluster coefficient
STK15_SDM.degreeCent <- centralization.degree(STK15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_SDMftn <- as.network.matrix(STK15_SDMft)
STK15_SDM.netDensity <- network.density(STK15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_SDM.entropy <- entropy(STK15_SDMft) #entropy

STK15_SDM.netMx <- cbind(STK15_SDM.netMx, STK15_SDM.clusterCoef, STK15_SDM.degreeCent$centralization,
                         STK15_SDM.netDensity, STK15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "STK"
KIoutcome = "Turnover_DM"
STK15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
STK15_TDMg2 <- data.frame(STK15_TDM)
STK15_TDMg2 <- STK15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_TDMg2$player1
player2vector <- STK15_TDMg2$player2
STK15_TDMg3 <- STK15_TDMg2
STK15_TDMg3$p1inp2vec <- is.element(STK15_TDMg3$player1, player2vector)
STK15_TDMg3$p2inp1vec <- is.element(STK15_TDMg3$player2, player1vector)

addPlayer1 <- STK15_TDMg3[ which(STK15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_TDMg3[ which(STK15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_TDMg2 <- rbind(STK15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
STK15_TDMft <- ftable(STK15_TDMg2$player1, STK15_TDMg2$player2)
STK15_TDMft2 <- as.matrix(STK15_TDMft)
numRows <- nrow(STK15_TDMft2)
numCols <- ncol(STK15_TDMft2)
STK15_TDMft3 <- STK15_TDMft2[c(2:numRows) , c(2:numCols)]
STK15_TDMTable <- graph.adjacency(STK15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(STK15_TDMTable, vertex.label = V(STK15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
STK15_TDM.clusterCoef <- transitivity(STK15_TDMTable, type="global") #cluster coefficient
STK15_TDM.degreeCent <- centralization.degree(STK15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_TDMftn <- as.network.matrix(STK15_TDMft)
STK15_TDM.netDensity <- network.density(STK15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_TDM.entropy <- entropy(STK15_TDMft) #entropy

STK15_TDM.netMx <- cbind(STK15_TDM.netMx, STK15_TDM.clusterCoef, STK15_TDM.degreeCent$centralization,
                         STK15_TDM.netDensity, STK15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "STK"
KIoutcome = "Stoppage_D"
STK15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
STK15_SDg2 <- data.frame(STK15_SD)
STK15_SDg2 <- STK15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_SDg2$player1
player2vector <- STK15_SDg2$player2
STK15_SDg3 <- STK15_SDg2
STK15_SDg3$p1inp2vec <- is.element(STK15_SDg3$player1, player2vector)
STK15_SDg3$p2inp1vec <- is.element(STK15_SDg3$player2, player1vector)

addPlayer1 <- STK15_SDg3[ which(STK15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_SDg3[ which(STK15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_SDg2 <- rbind(STK15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
STK15_SDft <- ftable(STK15_SDg2$player1, STK15_SDg2$player2)
STK15_SDft2 <- as.matrix(STK15_SDft)
numRows <- nrow(STK15_SDft2)
numCols <- ncol(STK15_SDft2)
STK15_SDft3 <- STK15_SDft2[c(2:numRows) , c(2:numCols)]
STK15_SDTable <- graph.adjacency(STK15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(STK15_SDTable, vertex.label = V(STK15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
STK15_SD.clusterCoef <- transitivity(STK15_SDTable, type="global") #cluster coefficient
STK15_SD.degreeCent <- centralization.degree(STK15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_SDftn <- as.network.matrix(STK15_SDft)
STK15_SD.netDensity <- network.density(STK15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_SD.entropy <- entropy(STK15_SDft) #entropy

STK15_SD.netMx <- cbind(STK15_SD.netMx, STK15_SD.clusterCoef, STK15_SD.degreeCent$centralization,
                        STK15_SD.netDensity, STK15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "STK"
KIoutcome = "Turnover_D"
STK15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
STK15_TDg2 <- data.frame(STK15_TD)
STK15_TDg2 <- STK15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_TDg2$player1
player2vector <- STK15_TDg2$player2
STK15_TDg3 <- STK15_TDg2
STK15_TDg3$p1inp2vec <- is.element(STK15_TDg3$player1, player2vector)
STK15_TDg3$p2inp1vec <- is.element(STK15_TDg3$player2, player1vector)

addPlayer1 <- STK15_TDg3[ which(STK15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_TDg3[ which(STK15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_TDg2 <- rbind(STK15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
STK15_TDft <- ftable(STK15_TDg2$player1, STK15_TDg2$player2)
STK15_TDft2 <- as.matrix(STK15_TDft)
numRows <- nrow(STK15_TDft2)
numCols <- ncol(STK15_TDft2)
STK15_TDft3 <- STK15_TDft2[c(2:numRows) , c(2:numCols)]
STK15_TDTable <- graph.adjacency(STK15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(STK15_TDTable, vertex.label = V(STK15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
STK15_TD.clusterCoef <- transitivity(STK15_TDTable, type="global") #cluster coefficient
STK15_TD.degreeCent <- centralization.degree(STK15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_TDftn <- as.network.matrix(STK15_TDft)
STK15_TD.netDensity <- network.density(STK15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_TD.entropy <- entropy(STK15_TDft) #entropy

STK15_TD.netMx <- cbind(STK15_TD.netMx, STK15_TD.clusterCoef, STK15_TD.degreeCent$centralization,
                        STK15_TD.netDensity, STK15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
STK15_QTg2 <- data.frame(STK15_QT)
STK15_QTg2 <- STK15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK15_QTg2$player1
player2vector <- STK15_QTg2$player2
STK15_QTg3 <- STK15_QTg2
STK15_QTg3$p1inp2vec <- is.element(STK15_QTg3$player1, player2vector)
STK15_QTg3$p2inp1vec <- is.element(STK15_QTg3$player2, player1vector)

addPlayer1 <- STK15_QTg3[ which(STK15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK15_QTg3[ which(STK15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK15_QTg2 <- rbind(STK15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
STK15_QTft <- ftable(STK15_QTg2$player1, STK15_QTg2$player2)
STK15_QTft2 <- as.matrix(STK15_QTft)
numRows <- nrow(STK15_QTft2)
numCols <- ncol(STK15_QTft2)
STK15_QTft3 <- STK15_QTft2[c(2:numRows) , c(2:numCols)]
STK15_QTTable <- graph.adjacency(STK15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(STK15_QTTable, vertex.label = V(STK15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
STK15_QT.clusterCoef <- transitivity(STK15_QTTable, type="global") #cluster coefficient
STK15_QT.degreeCent <- centralization.degree(STK15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK15_QTftn <- as.network.matrix(STK15_QTft)
STK15_QT.netDensity <- network.density(STK15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK15_QT.entropy <- entropy(STK15_QTft) #entropy

STK15_QT.netMx <- cbind(STK15_QT.netMx, STK15_QT.clusterCoef, STK15_QT.degreeCent$centralization,
                        STK15_QT.netDensity, STK15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK15_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Goal_F"
SYD15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
SYD15_Gg2 <- data.frame(SYD15_G)
SYD15_Gg2 <- SYD15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_Gg2$player1
player2vector <- SYD15_Gg2$player2
SYD15_Gg3 <- SYD15_Gg2
SYD15_Gg3$p1inp2vec <- is.element(SYD15_Gg3$player1, player2vector)
SYD15_Gg3$p2inp1vec <- is.element(SYD15_Gg3$player2, player1vector)

addPlayer1 <- SYD15_Gg3[ which(SYD15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_Gg3[ which(SYD15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_Gg2 <- rbind(SYD15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
SYD15_Gft <- ftable(SYD15_Gg2$player1, SYD15_Gg2$player2)
SYD15_Gft2 <- as.matrix(SYD15_Gft)
numRows <- nrow(SYD15_Gft2)
numCols <- ncol(SYD15_Gft2)
SYD15_Gft3 <- SYD15_Gft2[c(2:numRows) , c(2:numCols)]
SYD15_GTable <- graph.adjacency(SYD15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(SYD15_GTable, vertex.label = V(SYD15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
SYD15_G.clusterCoef <- transitivity(SYD15_GTable, type="global") #cluster coefficient
SYD15_G.degreeCent <- centralization.degree(SYD15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_Gftn <- as.network.matrix(SYD15_Gft)
SYD15_G.netDensity <- network.density(SYD15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_G.entropy <- entropy(SYD15_Gft) #entropy

SYD15_G.netMx <- cbind(SYD15_G.netMx, SYD15_G.clusterCoef, SYD15_G.degreeCent$centralization,
                       SYD15_G.netDensity, SYD15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Behind_F"
SYD15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
SYD15_Bg2 <- data.frame(SYD15_B)
SYD15_Bg2 <- SYD15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_Bg2$player1
player2vector <- SYD15_Bg2$player2
SYD15_Bg3 <- SYD15_Bg2
SYD15_Bg3$p1inp2vec <- is.element(SYD15_Bg3$player1, player2vector)
SYD15_Bg3$p2inp1vec <- is.element(SYD15_Bg3$player2, player1vector)

addPlayer1 <- SYD15_Bg3[ which(SYD15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_Bg3[ which(SYD15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_Bg2 <- rbind(SYD15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
SYD15_Bft <- ftable(SYD15_Bg2$player1, SYD15_Bg2$player2)
SYD15_Bft2 <- as.matrix(SYD15_Bft)
numRows <- nrow(SYD15_Bft2)
numCols <- ncol(SYD15_Bft2)
SYD15_Bft3 <- SYD15_Bft2[c(2:numRows) , c(2:numCols)]
SYD15_BTable <- graph.adjacency(SYD15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(SYD15_BTable, vertex.label = V(SYD15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
SYD15_B.clusterCoef <- transitivity(SYD15_BTable, type="global") #cluster coefficient
SYD15_B.degreeCent <- centralization.degree(SYD15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_Bftn <- as.network.matrix(SYD15_Bft)
SYD15_B.netDensity <- network.density(SYD15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_B.entropy <- entropy(SYD15_Bft) #entropy

SYD15_B.netMx <- cbind(SYD15_B.netMx, SYD15_B.clusterCoef, SYD15_B.degreeCent$centralization,
                       SYD15_B.netDensity, SYD15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
SYD15_SFg2 <- data.frame(SYD15_SF)
SYD15_SFg2 <- SYD15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_SFg2$player1
player2vector <- SYD15_SFg2$player2
SYD15_SFg3 <- SYD15_SFg2
SYD15_SFg3$p1inp2vec <- is.element(SYD15_SFg3$player1, player2vector)
SYD15_SFg3$p2inp1vec <- is.element(SYD15_SFg3$player2, player1vector)

addPlayer1 <- SYD15_SFg3[ which(SYD15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_SFg3[ which(SYD15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_SFg2 <- rbind(SYD15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
SYD15_SFft <- ftable(SYD15_SFg2$player1, SYD15_SFg2$player2)
SYD15_SFft2 <- as.matrix(SYD15_SFft)
numRows <- nrow(SYD15_SFft2)
numCols <- ncol(SYD15_SFft2)
SYD15_SFft3 <- SYD15_SFft2[c(2:numRows) , c(2:numCols)]
SYD15_SFTable <- graph.adjacency(SYD15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(SYD15_SFTable, vertex.label = V(SYD15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
SYD15_SF.clusterCoef <- transitivity(SYD15_SFTable, type="global") #cluster coefficient
SYD15_SF.degreeCent <- centralization.degree(SYD15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_SFftn <- as.network.matrix(SYD15_SFft)
SYD15_SF.netDensity <- network.density(SYD15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_SF.entropy <- entropy(SYD15_SFft) #entropy

SYD15_SF.netMx <- cbind(SYD15_SF.netMx, SYD15_SF.clusterCoef, SYD15_SF.degreeCent$centralization,
                        SYD15_SF.netDensity, SYD15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
SYD15_TFg2 <- data.frame(SYD15_TF)
SYD15_TFg2 <- SYD15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_TFg2$player1
player2vector <- SYD15_TFg2$player2
SYD15_TFg3 <- SYD15_TFg2
SYD15_TFg3$p1inp2vec <- is.element(SYD15_TFg3$player1, player2vector)
SYD15_TFg3$p2inp1vec <- is.element(SYD15_TFg3$player2, player1vector)

addPlayer1 <- SYD15_TFg3[ which(SYD15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_TFg3[ which(SYD15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_TFg2 <- rbind(SYD15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
SYD15_TFft <- ftable(SYD15_TFg2$player1, SYD15_TFg2$player2)
SYD15_TFft2 <- as.matrix(SYD15_TFft)
numRows <- nrow(SYD15_TFft2)
numCols <- ncol(SYD15_TFft2)
SYD15_TFft3 <- SYD15_TFft2[c(2:numRows) , c(2:numCols)]
SYD15_TFTable <- graph.adjacency(SYD15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(SYD15_TFTable, vertex.label = V(SYD15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
SYD15_TF.clusterCoef <- transitivity(SYD15_TFTable, type="global") #cluster coefficient
SYD15_TF.degreeCent <- centralization.degree(SYD15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_TFftn <- as.network.matrix(SYD15_TFft)
SYD15_TF.netDensity <- network.density(SYD15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_TF.entropy <- entropy(SYD15_TFft) #entropy

SYD15_TF.netMx <- cbind(SYD15_TF.netMx, SYD15_TF.clusterCoef, SYD15_TF.degreeCent$centralization,
                        SYD15_TF.netDensity, SYD15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
SYD15_SAMg2 <- data.frame(SYD15_SAM)
SYD15_SAMg2 <- SYD15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_SAMg2$player1
player2vector <- SYD15_SAMg2$player2
SYD15_SAMg3 <- SYD15_SAMg2
SYD15_SAMg3$p1inp2vec <- is.element(SYD15_SAMg3$player1, player2vector)
SYD15_SAMg3$p2inp1vec <- is.element(SYD15_SAMg3$player2, player1vector)

addPlayer1 <- SYD15_SAMg3[ which(SYD15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_SAMg3[ which(SYD15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_SAMg2 <- rbind(SYD15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
SYD15_SAMft <- ftable(SYD15_SAMg2$player1, SYD15_SAMg2$player2)
SYD15_SAMft2 <- as.matrix(SYD15_SAMft)
numRows <- nrow(SYD15_SAMft2)
numCols <- ncol(SYD15_SAMft2)
SYD15_SAMft3 <- SYD15_SAMft2[c(2:numRows) , c(2:numCols)]
SYD15_SAMTable <- graph.adjacency(SYD15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(SYD15_SAMTable, vertex.label = V(SYD15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
SYD15_SAM.clusterCoef <- transitivity(SYD15_SAMTable, type="global") #cluster coefficient
SYD15_SAM.degreeCent <- centralization.degree(SYD15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_SAMftn <- as.network.matrix(SYD15_SAMft)
SYD15_SAM.netDensity <- network.density(SYD15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_SAM.entropy <- entropy(SYD15_SAMft) #entropy

SYD15_SAM.netMx <- cbind(SYD15_SAM.netMx, SYD15_SAM.clusterCoef, SYD15_SAM.degreeCent$centralization,
                         SYD15_SAM.netDensity, SYD15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
SYD15_TAMg2 <- data.frame(SYD15_TAM)
SYD15_TAMg2 <- SYD15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_TAMg2$player1
player2vector <- SYD15_TAMg2$player2
SYD15_TAMg3 <- SYD15_TAMg2
SYD15_TAMg3$p1inp2vec <- is.element(SYD15_TAMg3$player1, player2vector)
SYD15_TAMg3$p2inp1vec <- is.element(SYD15_TAMg3$player2, player1vector)

addPlayer1 <- SYD15_TAMg3[ which(SYD15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD15_TAMg3[ which(SYD15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_TAMg2 <- rbind(SYD15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
SYD15_TAMft <- ftable(SYD15_TAMg2$player1, SYD15_TAMg2$player2)
SYD15_TAMft2 <- as.matrix(SYD15_TAMft)
numRows <- nrow(SYD15_TAMft2)
numCols <- ncol(SYD15_TAMft2)
SYD15_TAMft3 <- SYD15_TAMft2[c(2:numRows) , c(2:numCols)]
SYD15_TAMTable <- graph.adjacency(SYD15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(SYD15_TAMTable, vertex.label = V(SYD15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
SYD15_TAM.clusterCoef <- transitivity(SYD15_TAMTable, type="global") #cluster coefficient
SYD15_TAM.degreeCent <- centralization.degree(SYD15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_TAMftn <- as.network.matrix(SYD15_TAMft)
SYD15_TAM.netDensity <- network.density(SYD15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_TAM.entropy <- entropy(SYD15_TAMft) #entropy

SYD15_TAM.netMx <- cbind(SYD15_TAM.netMx, SYD15_TAM.clusterCoef, SYD15_TAM.degreeCent$centralization,
                         SYD15_TAM.netDensity, SYD15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
SYD15_SDMg2 <- data.frame(SYD15_SDM)
SYD15_SDMg2 <- SYD15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_SDMg2$player1
player2vector <- SYD15_SDMg2$player2
SYD15_SDMg3 <- SYD15_SDMg2
SYD15_SDMg3$p1inp2vec <- is.element(SYD15_SDMg3$player1, player2vector)
SYD15_SDMg3$p2inp1vec <- is.element(SYD15_SDMg3$player2, player1vector)

addPlayer1 <- SYD15_SDMg3[ which(SYD15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_SDMg3[ which(SYD15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_SDMg2 <- rbind(SYD15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
SYD15_SDMft <- ftable(SYD15_SDMg2$player1, SYD15_SDMg2$player2)
SYD15_SDMft2 <- as.matrix(SYD15_SDMft)
numRows <- nrow(SYD15_SDMft2)
numCols <- ncol(SYD15_SDMft2)
SYD15_SDMft3 <- SYD15_SDMft2[c(2:numRows) , c(2:numCols)]
SYD15_SDMTable <- graph.adjacency(SYD15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(SYD15_SDMTable, vertex.label = V(SYD15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
SYD15_SDM.clusterCoef <- transitivity(SYD15_SDMTable, type="global") #cluster coefficient
SYD15_SDM.degreeCent <- centralization.degree(SYD15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_SDMftn <- as.network.matrix(SYD15_SDMft)
SYD15_SDM.netDensity <- network.density(SYD15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_SDM.entropy <- entropy(SYD15_SDMft) #entropy

SYD15_SDM.netMx <- cbind(SYD15_SDM.netMx, SYD15_SDM.clusterCoef, SYD15_SDM.degreeCent$centralization,
                         SYD15_SDM.netDensity, SYD15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
SYD15_TDMg2 <- data.frame(SYD15_TDM)
SYD15_TDMg2 <- SYD15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_TDMg2$player1
player2vector <- SYD15_TDMg2$player2
SYD15_TDMg3 <- SYD15_TDMg2
SYD15_TDMg3$p1inp2vec <- is.element(SYD15_TDMg3$player1, player2vector)
SYD15_TDMg3$p2inp1vec <- is.element(SYD15_TDMg3$player2, player1vector)

addPlayer1 <- SYD15_TDMg3[ which(SYD15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_TDMg3[ which(SYD15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_TDMg2 <- rbind(SYD15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
SYD15_TDMft <- ftable(SYD15_TDMg2$player1, SYD15_TDMg2$player2)
SYD15_TDMft2 <- as.matrix(SYD15_TDMft)
numRows <- nrow(SYD15_TDMft2)
numCols <- ncol(SYD15_TDMft2)
SYD15_TDMft3 <- SYD15_TDMft2[c(2:numRows) , c(2:numCols)]
SYD15_TDMTable <- graph.adjacency(SYD15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(SYD15_TDMTable, vertex.label = V(SYD15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
SYD15_TDM.clusterCoef <- transitivity(SYD15_TDMTable, type="global") #cluster coefficient
SYD15_TDM.degreeCent <- centralization.degree(SYD15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_TDMftn <- as.network.matrix(SYD15_TDMft)
SYD15_TDM.netDensity <- network.density(SYD15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_TDM.entropy <- entropy(SYD15_TDMft) #entropy

SYD15_TDM.netMx <- cbind(SYD15_TDM.netMx, SYD15_TDM.clusterCoef, SYD15_TDM.degreeCent$centralization,
                         SYD15_TDM.netDensity, SYD15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
SYD15_SDg2 <- data.frame(SYD15_SD)
SYD15_SDg2 <- SYD15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_SDg2$player1
player2vector <- SYD15_SDg2$player2
SYD15_SDg3 <- SYD15_SDg2
SYD15_SDg3$p1inp2vec <- is.element(SYD15_SDg3$player1, player2vector)
SYD15_SDg3$p2inp1vec <- is.element(SYD15_SDg3$player2, player1vector)

addPlayer1 <- SYD15_SDg3[ which(SYD15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_SDg3[ which(SYD15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_SDg2 <- rbind(SYD15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
SYD15_SDft <- ftable(SYD15_SDg2$player1, SYD15_SDg2$player2)
SYD15_SDft2 <- as.matrix(SYD15_SDft)
numRows <- nrow(SYD15_SDft2)
numCols <- ncol(SYD15_SDft2)
SYD15_SDft3 <- SYD15_SDft2[c(2:numRows) , c(2:numCols)]
SYD15_SDTable <- graph.adjacency(SYD15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(SYD15_SDTable, vertex.label = V(SYD15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
SYD15_SD.clusterCoef <- transitivity(SYD15_SDTable, type="global") #cluster coefficient
SYD15_SD.degreeCent <- centralization.degree(SYD15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_SDftn <- as.network.matrix(SYD15_SDft)
SYD15_SD.netDensity <- network.density(SYD15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_SD.entropy <- entropy(SYD15_SDft) #entropy

SYD15_SD.netMx <- cbind(SYD15_SD.netMx, SYD15_SD.clusterCoef, SYD15_SD.degreeCent$centralization,
                        SYD15_SD.netDensity, SYD15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
SYD15_TDg2 <- data.frame(SYD15_TD)
SYD15_TDg2 <- SYD15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_TDg2$player1
player2vector <- SYD15_TDg2$player2
SYD15_TDg3 <- SYD15_TDg2
SYD15_TDg3$p1inp2vec <- is.element(SYD15_TDg3$player1, player2vector)
SYD15_TDg3$p2inp1vec <- is.element(SYD15_TDg3$player2, player1vector)

addPlayer1 <- SYD15_TDg3[ which(SYD15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_TDg3[ which(SYD15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_TDg2 <- rbind(SYD15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
SYD15_TDft <- ftable(SYD15_TDg2$player1, SYD15_TDg2$player2)
SYD15_TDft2 <- as.matrix(SYD15_TDft)
numRows <- nrow(SYD15_TDft2)
numCols <- ncol(SYD15_TDft2)
SYD15_TDft3 <- SYD15_TDft2[c(2:numRows) , c(2:numCols)]
SYD15_TDTable <- graph.adjacency(SYD15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(SYD15_TDTable, vertex.label = V(SYD15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
SYD15_TD.clusterCoef <- transitivity(SYD15_TDTable, type="global") #cluster coefficient
SYD15_TD.degreeCent <- centralization.degree(SYD15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_TDftn <- as.network.matrix(SYD15_TDft)
SYD15_TD.netDensity <- network.density(SYD15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_TD.entropy <- entropy(SYD15_TDft) #entropy

SYD15_TD.netMx <- cbind(SYD15_TD.netMx, SYD15_TD.clusterCoef, SYD15_TD.degreeCent$centralization,
                        SYD15_TD.netDensity, SYD15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
SYD15_QTg2 <- data.frame(SYD15_QT)
SYD15_QTg2 <- SYD15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD15_QTg2$player1
player2vector <- SYD15_QTg2$player2
SYD15_QTg3 <- SYD15_QTg2
SYD15_QTg3$p1inp2vec <- is.element(SYD15_QTg3$player1, player2vector)
SYD15_QTg3$p2inp1vec <- is.element(SYD15_QTg3$player2, player1vector)

addPlayer1 <- SYD15_QTg3[ which(SYD15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD15_QTg3[ which(SYD15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD15_QTg2 <- rbind(SYD15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
SYD15_QTft <- ftable(SYD15_QTg2$player1, SYD15_QTg2$player2)
SYD15_QTft2 <- as.matrix(SYD15_QTft)
numRows <- nrow(SYD15_QTft2)
numCols <- ncol(SYD15_QTft2)
SYD15_QTft3 <- SYD15_QTft2[c(2:numRows) , c(2:numCols)]
SYD15_QTTable <- graph.adjacency(SYD15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(SYD15_QTTable, vertex.label = V(SYD15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
SYD15_QT.clusterCoef <- transitivity(SYD15_QTTable, type="global") #cluster coefficient
SYD15_QT.degreeCent <- centralization.degree(SYD15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD15_QTftn <- as.network.matrix(SYD15_QTft)
SYD15_QT.netDensity <- network.density(SYD15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD15_QT.entropy <- entropy(SYD15_QTft) #entropy

SYD15_QT.netMx <- cbind(SYD15_QT.netMx, SYD15_QT.clusterCoef, SYD15_QT.degreeCent$centralization,
                        SYD15_QT.netDensity, SYD15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD15_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "WB"
KIoutcome = "Goal_F"
WB15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
WB15_Gg2 <- data.frame(WB15_G)
WB15_Gg2 <- WB15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_Gg2$player1
player2vector <- WB15_Gg2$player2
WB15_Gg3 <- WB15_Gg2
WB15_Gg3$p1inp2vec <- is.element(WB15_Gg3$player1, player2vector)
WB15_Gg3$p2inp1vec <- is.element(WB15_Gg3$player2, player1vector)

addPlayer1 <- WB15_Gg3[ which(WB15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB15_Gg3[ which(WB15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_Gg2 <- rbind(WB15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
WB15_Gft <- ftable(WB15_Gg2$player1, WB15_Gg2$player2)
WB15_Gft2 <- as.matrix(WB15_Gft)
numRows <- nrow(WB15_Gft2)
numCols <- ncol(WB15_Gft2)
WB15_Gft3 <- WB15_Gft2[c(2:numRows) , c(2:numCols)]
WB15_GTable <- graph.adjacency(WB15_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(WB15_GTable, vertex.label = V(WB15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
WB15_G.clusterCoef <- transitivity(WB15_GTable, type="global") #cluster coefficient
WB15_G.degreeCent <- centralization.degree(WB15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_Gftn <- as.network.matrix(WB15_Gft)
WB15_G.netDensity <- network.density(WB15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_G.entropy <- entropy(WB15_Gft) #entropy

WB15_G.netMx <- cbind(WB15_G.netMx, WB15_G.clusterCoef, WB15_G.degreeCent$centralization,
                      WB15_G.netDensity, WB15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Behind_F"
WB15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
WB15_Bg2 <- data.frame(WB15_B)
WB15_Bg2 <- WB15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_Bg2$player1
player2vector <- WB15_Bg2$player2
WB15_Bg3 <- WB15_Bg2
WB15_Bg3$p1inp2vec <- is.element(WB15_Bg3$player1, player2vector)
WB15_Bg3$p2inp1vec <- is.element(WB15_Bg3$player2, player1vector)

addPlayer1 <- WB15_Bg3[ which(WB15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_Bg3[ which(WB15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_Bg2 <- rbind(WB15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
WB15_Bft <- ftable(WB15_Bg2$player1, WB15_Bg2$player2)
WB15_Bft2 <- as.matrix(WB15_Bft)
numRows <- nrow(WB15_Bft2)
numCols <- ncol(WB15_Bft2)
WB15_Bft3 <- WB15_Bft2[c(2:numRows) , c(2:numCols)]
WB15_BTable <- graph.adjacency(WB15_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(WB15_BTable, vertex.label = V(WB15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
WB15_B.clusterCoef <- transitivity(WB15_BTable, type="global") #cluster coefficient
WB15_B.degreeCent <- centralization.degree(WB15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_Bftn <- as.network.matrix(WB15_Bft)
WB15_B.netDensity <- network.density(WB15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_B.entropy <- entropy(WB15_Bft) #entropy

WB15_B.netMx <- cbind(WB15_B.netMx, WB15_B.clusterCoef, WB15_B.degreeCent$centralization,
                      WB15_B.netDensity, WB15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Stoppage_F"
WB15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
WB15_SFg2 <- data.frame(WB15_SF)
WB15_SFg2 <- WB15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_SFg2$player1
player2vector <- WB15_SFg2$player2
WB15_SFg3 <- WB15_SFg2
WB15_SFg3$p1inp2vec <- is.element(WB15_SFg3$player1, player2vector)
WB15_SFg3$p2inp1vec <- is.element(WB15_SFg3$player2, player1vector)

addPlayer1 <- WB15_SFg3[ which(WB15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_SFg3[ which(WB15_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_SFg2 <- rbind(WB15_SFg2, addPlayers)

#ROUND 15, FWD Stoppage graph using weighted edges
WB15_SFft <- ftable(WB15_SFg2$player1, WB15_SFg2$player2)
WB15_SFft2 <- as.matrix(WB15_SFft)
numRows <- nrow(WB15_SFft2)
numCols <- ncol(WB15_SFft2)
WB15_SFft3 <- WB15_SFft2[c(2:numRows) , c(2:numCols)]
WB15_SFTable <- graph.adjacency(WB15_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(WB15_SFTable, vertex.label = V(WB15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
WB15_SF.clusterCoef <- transitivity(WB15_SFTable, type="global") #cluster coefficient
WB15_SF.degreeCent <- centralization.degree(WB15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_SFftn <- as.network.matrix(WB15_SFft)
WB15_SF.netDensity <- network.density(WB15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_SF.entropy <- entropy(WB15_SFft) #entropy

WB15_SF.netMx <- cbind(WB15_SF.netMx, WB15_SF.clusterCoef, WB15_SF.degreeCent$centralization,
                       WB15_SF.netDensity, WB15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************

round = 15
teamName = "WB"
KIoutcome = "Turnover_F"
WB15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
WB15_TFg2 <- data.frame(WB15_TF)
WB15_TFg2 <- WB15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_TFg2$player1
player2vector <- WB15_TFg2$player2
WB15_TFg3 <- WB15_TFg2
WB15_TFg3$p1inp2vec <- is.element(WB15_TFg3$player1, player2vector)
WB15_TFg3$p2inp1vec <- is.element(WB15_TFg3$player2, player1vector)

addPlayer1 <- WB15_TFg3[ which(WB15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_TFg3[ which(WB15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_TFg2 <- rbind(WB15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
WB15_TFft <- ftable(WB15_TFg2$player1, WB15_TFg2$player2)
WB15_TFft2 <- as.matrix(WB15_TFft)
numRows <- nrow(WB15_TFft2)
numCols <- ncol(WB15_TFft2)
WB15_TFft3 <- WB15_TFft2[c(2:numRows) , c(2:numCols)]
WB15_TFTable <- graph.adjacency(WB15_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(WB15_TFTable, vertex.label = V(WB15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
WB15_TF.clusterCoef <- transitivity(WB15_TFTable, type="global") #cluster coefficient
WB15_TF.degreeCent <- centralization.degree(WB15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_TFftn <- as.network.matrix(WB15_TFft)
WB15_TF.netDensity <- network.density(WB15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_TF.entropy <- entropy(WB15_TFft) #entropy

WB15_TF.netMx <- cbind(WB15_TF.netMx, WB15_TF.clusterCoef, WB15_TF.degreeCent$centralization,
                       WB15_TF.netDensity, WB15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************

round = 15
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
WB15_SAMg2 <- data.frame(WB15_SAM)
WB15_SAMg2 <- WB15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_SAMg2$player1
player2vector <- WB15_SAMg2$player2
WB15_SAMg3 <- WB15_SAMg2
WB15_SAMg3$p1inp2vec <- is.element(WB15_SAMg3$player1, player2vector)
WB15_SAMg3$p2inp1vec <- is.element(WB15_SAMg3$player2, player1vector)

addPlayer1 <- WB15_SAMg3[ which(WB15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_SAMg3[ which(WB15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_SAMg2 <- rbind(WB15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
WB15_SAMft <- ftable(WB15_SAMg2$player1, WB15_SAMg2$player2)
WB15_SAMft2 <- as.matrix(WB15_SAMft)
numRows <- nrow(WB15_SAMft2)
numCols <- ncol(WB15_SAMft2)
WB15_SAMft3 <- WB15_SAMft2[c(2:numRows) , c(2:numCols)]
WB15_SAMTable <- graph.adjacency(WB15_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(WB15_SAMTable, vertex.label = V(WB15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
WB15_SAM.clusterCoef <- transitivity(WB15_SAMTable, type="global") #cluster coefficient
WB15_SAM.degreeCent <- centralization.degree(WB15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_SAMftn <- as.network.matrix(WB15_SAMft)
WB15_SAM.netDensity <- network.density(WB15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_SAM.entropy <- entropy(WB15_SAMft) #entropy

WB15_SAM.netMx <- cbind(WB15_SAM.netMx, WB15_SAM.clusterCoef, WB15_SAM.degreeCent$centralization,
                        WB15_SAM.netDensity, WB15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Turnover_AM"
WB15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
WB15_TAMg2 <- data.frame(WB15_TAM)
WB15_TAMg2 <- WB15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_TAMg2$player1
player2vector <- WB15_TAMg2$player2
WB15_TAMg3 <- WB15_TAMg2
WB15_TAMg3$p1inp2vec <- is.element(WB15_TAMg3$player1, player2vector)
WB15_TAMg3$p2inp1vec <- is.element(WB15_TAMg3$player2, player1vector)

addPlayer1 <- WB15_TAMg3[ which(WB15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_TAMg3[ which(WB15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_TAMg2 <- rbind(WB15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
WB15_TAMft <- ftable(WB15_TAMg2$player1, WB15_TAMg2$player2)
WB15_TAMft2 <- as.matrix(WB15_TAMft)
numRows <- nrow(WB15_TAMft2)
numCols <- ncol(WB15_TAMft2)
WB15_TAMft3 <- WB15_TAMft2[c(2:numRows) , c(2:numCols)]
WB15_TAMTable <- graph.adjacency(WB15_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(WB15_TAMTable, vertex.label = V(WB15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
WB15_TAM.clusterCoef <- transitivity(WB15_TAMTable, type="global") #cluster coefficient
WB15_TAM.degreeCent <- centralization.degree(WB15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_TAMftn <- as.network.matrix(WB15_TAMft)
WB15_TAM.netDensity <- network.density(WB15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_TAM.entropy <- entropy(WB15_TAMft) #entropy

WB15_TAM.netMx <- cbind(WB15_TAM.netMx, WB15_TAM.clusterCoef, WB15_TAM.degreeCent$centralization,
                        WB15_TAM.netDensity, WB15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
WB15_SDMg2 <- data.frame(WB15_SDM)
WB15_SDMg2 <- WB15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_SDMg2$player1
player2vector <- WB15_SDMg2$player2
WB15_SDMg3 <- WB15_SDMg2
WB15_SDMg3$p1inp2vec <- is.element(WB15_SDMg3$player1, player2vector)
WB15_SDMg3$p2inp1vec <- is.element(WB15_SDMg3$player2, player1vector)

addPlayer1 <- WB15_SDMg3[ which(WB15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_SDMg3[ which(WB15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_SDMg2 <- rbind(WB15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
WB15_SDMft <- ftable(WB15_SDMg2$player1, WB15_SDMg2$player2)
WB15_SDMft2 <- as.matrix(WB15_SDMft)
numRows <- nrow(WB15_SDMft2)
numCols <- ncol(WB15_SDMft2)
WB15_SDMft3 <- WB15_SDMft2[c(2:numRows) , c(2:numCols)]
WB15_SDMTable <- graph.adjacency(WB15_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(WB15_SDMTable, vertex.label = V(WB15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
WB15_SDM.clusterCoef <- transitivity(WB15_SDMTable, type="global") #cluster coefficient
WB15_SDM.degreeCent <- centralization.degree(WB15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_SDMftn <- as.network.matrix(WB15_SDMft)
WB15_SDM.netDensity <- network.density(WB15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_SDM.entropy <- entropy(WB15_SDMft) #entropy

WB15_SDM.netMx <- cbind(WB15_SDM.netMx, WB15_SDM.clusterCoef, WB15_SDM.degreeCent$centralization,
                        WB15_SDM.netDensity, WB15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Turnover_DM"
WB15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
WB15_TDMg2 <- data.frame(WB15_TDM)
WB15_TDMg2 <- WB15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_TDMg2$player1
player2vector <- WB15_TDMg2$player2
WB15_TDMg3 <- WB15_TDMg2
WB15_TDMg3$p1inp2vec <- is.element(WB15_TDMg3$player1, player2vector)
WB15_TDMg3$p2inp1vec <- is.element(WB15_TDMg3$player2, player1vector)

addPlayer1 <- WB15_TDMg3[ which(WB15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_TDMg3[ which(WB15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_TDMg2 <- rbind(WB15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
WB15_TDMft <- ftable(WB15_TDMg2$player1, WB15_TDMg2$player2)
WB15_TDMft2 <- as.matrix(WB15_TDMft)
numRows <- nrow(WB15_TDMft2)
numCols <- ncol(WB15_TDMft2)
WB15_TDMft3 <- WB15_TDMft2[c(2:numRows) , c(2:numCols)]
WB15_TDMTable <- graph.adjacency(WB15_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(WB15_TDMTable, vertex.label = V(WB15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
WB15_TDM.clusterCoef <- transitivity(WB15_TDMTable, type="global") #cluster coefficient
WB15_TDM.degreeCent <- centralization.degree(WB15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_TDMftn <- as.network.matrix(WB15_TDMft)
WB15_TDM.netDensity <- network.density(WB15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_TDM.entropy <- entropy(WB15_TDMft) #entropy

WB15_TDM.netMx <- cbind(WB15_TDM.netMx, WB15_TDM.clusterCoef, WB15_TDM.degreeCent$centralization,
                        WB15_TDM.netDensity, WB15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Stoppage_D"
WB15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
WB15_SDg2 <- data.frame(WB15_SD)
WB15_SDg2 <- WB15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_SDg2$player1
player2vector <- WB15_SDg2$player2
WB15_SDg3 <- WB15_SDg2
WB15_SDg3$p1inp2vec <- is.element(WB15_SDg3$player1, player2vector)
WB15_SDg3$p2inp1vec <- is.element(WB15_SDg3$player2, player1vector)

addPlayer1 <- WB15_SDg3[ which(WB15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_SDg3[ which(WB15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_SDg2 <- rbind(WB15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
WB15_SDft <- ftable(WB15_SDg2$player1, WB15_SDg2$player2)
WB15_SDft2 <- as.matrix(WB15_SDft)
numRows <- nrow(WB15_SDft2)
numCols <- ncol(WB15_SDft2)
WB15_SDft3 <- WB15_SDft2[c(2:numRows) , c(2:numCols)]
WB15_SDTable <- graph.adjacency(WB15_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(WB15_SDTable, vertex.label = V(WB15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
WB15_SD.clusterCoef <- transitivity(WB15_SDTable, type="global") #cluster coefficient
WB15_SD.degreeCent <- centralization.degree(WB15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_SDftn <- as.network.matrix(WB15_SDft)
WB15_SD.netDensity <- network.density(WB15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_SD.entropy <- entropy(WB15_SDft) #entropy

WB15_SD.netMx <- cbind(WB15_SD.netMx, WB15_SD.clusterCoef, WB15_SD.degreeCent$centralization,
                       WB15_SD.netDensity, WB15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "Turnover_D"
WB15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
WB15_TDg2 <- data.frame(WB15_TD)
WB15_TDg2 <- WB15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_TDg2$player1
player2vector <- WB15_TDg2$player2
WB15_TDg3 <- WB15_TDg2
WB15_TDg3$p1inp2vec <- is.element(WB15_TDg3$player1, player2vector)
WB15_TDg3$p2inp1vec <- is.element(WB15_TDg3$player2, player1vector)

addPlayer1 <- WB15_TDg3[ which(WB15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_TDg3[ which(WB15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_TDg2 <- rbind(WB15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
WB15_TDft <- ftable(WB15_TDg2$player1, WB15_TDg2$player2)
WB15_TDft2 <- as.matrix(WB15_TDft)
numRows <- nrow(WB15_TDft2)
numCols <- ncol(WB15_TDft2)
WB15_TDft3 <- WB15_TDft2[c(2:numRows) , c(2:numCols)]
WB15_TDTable <- graph.adjacency(WB15_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(WB15_TDTable, vertex.label = V(WB15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
WB15_TD.clusterCoef <- transitivity(WB15_TDTable, type="global") #cluster coefficient
WB15_TD.degreeCent <- centralization.degree(WB15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_TDftn <- as.network.matrix(WB15_TDft)
WB15_TD.netDensity <- network.density(WB15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_TD.entropy <- entropy(WB15_TDft) #entropy

WB15_TD.netMx <- cbind(WB15_TD.netMx, WB15_TD.clusterCoef, WB15_TD.degreeCent$centralization,
                       WB15_TD.netDensity, WB15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
WB15_QTg2 <- data.frame(WB15_QT)
WB15_QTg2 <- WB15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB15_QTg2$player1
player2vector <- WB15_QTg2$player2
WB15_QTg3 <- WB15_QTg2
WB15_QTg3$p1inp2vec <- is.element(WB15_QTg3$player1, player2vector)
WB15_QTg3$p2inp1vec <- is.element(WB15_QTg3$player2, player1vector)

addPlayer1 <- WB15_QTg3[ which(WB15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB15_QTg3[ which(WB15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB15_QTg2 <- rbind(WB15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
WB15_QTft <- ftable(WB15_QTg2$player1, WB15_QTg2$player2)
WB15_QTft2 <- as.matrix(WB15_QTft)
numRows <- nrow(WB15_QTft2)
numCols <- ncol(WB15_QTft2)
WB15_QTft3 <- WB15_QTft2[c(2:numRows) , c(2:numCols)]
WB15_QTTable <- graph.adjacency(WB15_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(WB15_QTTable, vertex.label = V(WB15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
WB15_QT.clusterCoef <- transitivity(WB15_QTTable, type="global") #cluster coefficient
WB15_QT.degreeCent <- centralization.degree(WB15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB15_QTftn <- as.network.matrix(WB15_QTft)
WB15_QT.netDensity <- network.density(WB15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB15_QT.entropy <- entropy(WB15_QTft) #entropy

WB15_QT.netMx <- cbind(WB15_QT.netMx, WB15_QT.clusterCoef, WB15_QT.degreeCent$centralization,
                       WB15_QT.netDensity, WB15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB15_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 15
##

#ROUND 15, Goal***************************************************************

round = 15
teamName = "WCE"
KIoutcome = "Goal_F"
WCE15_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Goal with weighted edges
WCE15_Gg2 <- data.frame(WCE15_G)
WCE15_Gg2 <- WCE15_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_Gg2$player1
player2vector <- WCE15_Gg2$player2
WCE15_Gg3 <- WCE15_Gg2
WCE15_Gg3$p1inp2vec <- is.element(WCE15_Gg3$player1, player2vector)
WCE15_Gg3$p2inp1vec <- is.element(WCE15_Gg3$player2, player1vector)

addPlayer1 <- WCE15_Gg3[ which(WCE15_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_Gg3[ which(WCE15_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_Gg2 <- rbind(WCE15_Gg2, addPlayers)

#ROUND 15, Goal graph using weighted edges
WCE15_Gft <- ftable(WCE15_Gg2$player1, WCE15_Gg2$player2)
WCE15_Gft2 <- as.matrix(WCE15_Gft)
numRows <- nrow(WCE15_Gft2)
numCols <- ncol(WCE15_Gft2)
WCE15_Gft3 <- WCE15_Gft2[c(2:numRows) , c(2:numCols)]
WCE15_GTable <- graph.adjacency(WCE15_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Goal graph=weighted
plot.igraph(WCE15_GTable, vertex.label = V(WCE15_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Goal calulation of network metrics
#igraph
WCE15_G.clusterCoef <- transitivity(WCE15_GTable, type="global") #cluster coefficient
WCE15_G.degreeCent <- centralization.degree(WCE15_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_Gftn <- as.network.matrix(WCE15_Gft)
WCE15_G.netDensity <- network.density(WCE15_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_G.entropy <- entropy(WCE15_Gft) #entropy

WCE15_G.netMx <- cbind(WCE15_G.netMx, WCE15_G.clusterCoef, WCE15_G.degreeCent$centralization,
                       WCE15_G.netDensity, WCE15_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_G.netMx) <- varnames

#ROUND 15, Behind***************************************************************

round = 15
teamName = "WCE"
KIoutcome = "Behind_F"
WCE15_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, Behind with weighted edges
WCE15_Bg2 <- data.frame(WCE15_B)
WCE15_Bg2 <- WCE15_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_Bg2$player1
player2vector <- WCE15_Bg2$player2
WCE15_Bg3 <- WCE15_Bg2
WCE15_Bg3$p1inp2vec <- is.element(WCE15_Bg3$player1, player2vector)
WCE15_Bg3$p2inp1vec <- is.element(WCE15_Bg3$player2, player1vector)

addPlayer1 <- WCE15_Bg3[ which(WCE15_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_Bg3[ which(WCE15_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_Bg2 <- rbind(WCE15_Bg2, addPlayers)

#ROUND 15, Behind graph using weighted edges
WCE15_Bft <- ftable(WCE15_Bg2$player1, WCE15_Bg2$player2)
WCE15_Bft2 <- as.matrix(WCE15_Bft)
numRows <- nrow(WCE15_Bft2)
numCols <- ncol(WCE15_Bft2)
WCE15_Bft3 <- WCE15_Bft2[c(2:numRows) , c(2:numCols)]
WCE15_BTable <- graph.adjacency(WCE15_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 15, Behind graph=weighted
plot.igraph(WCE15_BTable, vertex.label = V(WCE15_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, Behind calulation of network metrics
#igraph
WCE15_B.clusterCoef <- transitivity(WCE15_BTable, type="global") #cluster coefficient
WCE15_B.degreeCent <- centralization.degree(WCE15_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_Bftn <- as.network.matrix(WCE15_Bft)
WCE15_B.netDensity <- network.density(WCE15_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_B.entropy <- entropy(WCE15_Bft) #entropy

WCE15_B.netMx <- cbind(WCE15_B.netMx, WCE15_B.clusterCoef, WCE15_B.degreeCent$centralization,
                       WCE15_B.netDensity, WCE15_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_B.netMx) <- varnames

#ROUND 15, FWD Stoppage**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE15_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Stoppage with weighted edges
WCE15_SFg2 <- data.frame(WCE15_SF)
WCE15_SFg2 <- WCE15_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_SFg2$player1
player2vector <- WCE15_SFg2$player2
WCE15_SFg3 <- WCE15_SFg2
WCE15_SFg3$p1inp2vec <- is.element(WCE15_SFg3$player1, player2vector)
WCE15_SFg3$p2inp1vec <- is.element(WCE15_SFg3$player2, player1vector)

addPlayer1 <- WCE15_SFg3[ which(WCE15_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE15_SFg2 <- rbind(WCE15_SFg2, addPlayer1)

#ROUND 15, FWD Stoppage graph using weighted edges
WCE15_SFft <- ftable(WCE15_SFg2$player1, WCE15_SFg2$player2)
WCE15_SFft2 <- as.matrix(WCE15_SFft)
numRows <- nrow(WCE15_SFft2)
numCols <- ncol(WCE15_SFft2)
WCE15_SFft3 <- WCE15_SFft2[c(2:numRows) , c(1:numCols)]
WCE15_SFTable <- graph.adjacency(WCE15_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Stoppage graph=weighted
plot.igraph(WCE15_SFTable, vertex.label = V(WCE15_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Stoppage calulation of network metrics
#igraph
WCE15_SF.clusterCoef <- transitivity(WCE15_SFTable, type="global") #cluster coefficient
WCE15_SF.degreeCent <- centralization.degree(WCE15_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_SFftn <- as.network.matrix(WCE15_SFft)
WCE15_SF.netDensity <- network.density(WCE15_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_SF.entropy <- entropy(WCE15_SFft) #entropy

WCE15_SF.netMx <- cbind(WCE15_SF.netMx, WCE15_SF.clusterCoef, WCE15_SF.degreeCent$centralization,
                        WCE15_SF.netDensity, WCE15_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_SF.netMx) <- varnames

#ROUND 15, FWD Turnover**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE15_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, FWD Turnover with weighted edges
WCE15_TFg2 <- data.frame(WCE15_TF)
WCE15_TFg2 <- WCE15_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_TFg2$player1
player2vector <- WCE15_TFg2$player2
WCE15_TFg3 <- WCE15_TFg2
WCE15_TFg3$p1inp2vec <- is.element(WCE15_TFg3$player1, player2vector)
WCE15_TFg3$p2inp1vec <- is.element(WCE15_TFg3$player2, player1vector)

addPlayer1 <- WCE15_TFg3[ which(WCE15_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_TFg3[ which(WCE15_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_TFg2 <- rbind(WCE15_TFg2, addPlayers)

#ROUND 15, FWD Turnover graph using weighted edges
WCE15_TFft <- ftable(WCE15_TFg2$player1, WCE15_TFg2$player2)
WCE15_TFft2 <- as.matrix(WCE15_TFft)
numRows <- nrow(WCE15_TFft2)
numCols <- ncol(WCE15_TFft2)
WCE15_TFft3 <- WCE15_TFft2[c(2:numRows) , c(2:numCols)]
WCE15_TFTable <- graph.adjacency(WCE15_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, FWD Turnover graph=weighted
plot.igraph(WCE15_TFTable, vertex.label = V(WCE15_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, FWD Turnover calulation of network metrics
#igraph
WCE15_TF.clusterCoef <- transitivity(WCE15_TFTable, type="global") #cluster coefficient
WCE15_TF.degreeCent <- centralization.degree(WCE15_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_TFftn <- as.network.matrix(WCE15_TFft)
WCE15_TF.netDensity <- network.density(WCE15_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_TF.entropy <- entropy(WCE15_TFft) #entropy

WCE15_TF.netMx <- cbind(WCE15_TF.netMx, WCE15_TF.clusterCoef, WCE15_TF.degreeCent$centralization,
                        WCE15_TF.netDensity, WCE15_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_TF.netMx) <- varnames

#ROUND 15, AM Stoppage**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE15_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Stoppage with weighted edges
WCE15_SAMg2 <- data.frame(WCE15_SAM)
WCE15_SAMg2 <- WCE15_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_SAMg2$player1
player2vector <- WCE15_SAMg2$player2
WCE15_SAMg3 <- WCE15_SAMg2
WCE15_SAMg3$p1inp2vec <- is.element(WCE15_SAMg3$player1, player2vector)
WCE15_SAMg3$p2inp1vec <- is.element(WCE15_SAMg3$player2, player1vector)

addPlayer1 <- WCE15_SAMg3[ which(WCE15_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_SAMg3[ which(WCE15_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_SAMg2 <- rbind(WCE15_SAMg2, addPlayers)

#ROUND 15, AM Stoppage graph using weighted edges
WCE15_SAMft <- ftable(WCE15_SAMg2$player1, WCE15_SAMg2$player2)
WCE15_SAMft2 <- as.matrix(WCE15_SAMft)
numRows <- nrow(WCE15_SAMft2)
numCols <- ncol(WCE15_SAMft2)
WCE15_SAMft3 <- WCE15_SAMft2[c(2:numRows) , c(2:numCols)]
WCE15_SAMTable <- graph.adjacency(WCE15_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Stoppage graph=weighted
plot.igraph(WCE15_SAMTable, vertex.label = V(WCE15_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Stoppage calulation of network metrics
#igraph
WCE15_SAM.clusterCoef <- transitivity(WCE15_SAMTable, type="global") #cluster coefficient
WCE15_SAM.degreeCent <- centralization.degree(WCE15_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_SAMftn <- as.network.matrix(WCE15_SAMft)
WCE15_SAM.netDensity <- network.density(WCE15_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_SAM.entropy <- entropy(WCE15_SAMft) #entropy

WCE15_SAM.netMx <- cbind(WCE15_SAM.netMx, WCE15_SAM.clusterCoef, WCE15_SAM.degreeCent$centralization,
                         WCE15_SAM.netDensity, WCE15_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_SAM.netMx) <- varnames

#ROUND 15, AM Turnover**********************************************************

round = 15
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE15_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, AM Turnover with weighted edges
WCE15_TAMg2 <- data.frame(WCE15_TAM)
WCE15_TAMg2 <- WCE15_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_TAMg2$player1
player2vector <- WCE15_TAMg2$player2
WCE15_TAMg3 <- WCE15_TAMg2
WCE15_TAMg3$p1inp2vec <- is.element(WCE15_TAMg3$player1, player2vector)
WCE15_TAMg3$p2inp1vec <- is.element(WCE15_TAMg3$player2, player1vector)

addPlayer1 <- WCE15_TAMg3[ which(WCE15_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE15_TAMg3[ which(WCE15_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_TAMg2 <- rbind(WCE15_TAMg2, addPlayers)

#ROUND 15, AM Turnover graph using weighted edges
WCE15_TAMft <- ftable(WCE15_TAMg2$player1, WCE15_TAMg2$player2)
WCE15_TAMft2 <- as.matrix(WCE15_TAMft)
numRows <- nrow(WCE15_TAMft2)
numCols <- ncol(WCE15_TAMft2)
WCE15_TAMft3 <- WCE15_TAMft2[c(2:numRows) , c(2:numCols)]
WCE15_TAMTable <- graph.adjacency(WCE15_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, AM Turnover graph=weighted
plot.igraph(WCE15_TAMTable, vertex.label = V(WCE15_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, AM Turnover calulation of network metrics
#igraph
WCE15_TAM.clusterCoef <- transitivity(WCE15_TAMTable, type="global") #cluster coefficient
WCE15_TAM.degreeCent <- centralization.degree(WCE15_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_TAMftn <- as.network.matrix(WCE15_TAMft)
WCE15_TAM.netDensity <- network.density(WCE15_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_TAM.entropy <- entropy(WCE15_TAMft) #entropy

WCE15_TAM.netMx <- cbind(WCE15_TAM.netMx, WCE15_TAM.clusterCoef, WCE15_TAM.degreeCent$centralization,
                         WCE15_TAM.netDensity, WCE15_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_TAM.netMx) <- varnames

#ROUND 15, DM Stoppage**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE15_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Stoppage with weighted edges
WCE15_SDMg2 <- data.frame(WCE15_SDM)
WCE15_SDMg2 <- WCE15_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_SDMg2$player1
player2vector <- WCE15_SDMg2$player2
WCE15_SDMg3 <- WCE15_SDMg2
WCE15_SDMg3$p1inp2vec <- is.element(WCE15_SDMg3$player1, player2vector)
WCE15_SDMg3$p2inp1vec <- is.element(WCE15_SDMg3$player2, player1vector)

addPlayer1 <- WCE15_SDMg3[ which(WCE15_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_SDMg3[ which(WCE15_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_SDMg2 <- rbind(WCE15_SDMg2, addPlayers)

#ROUND 15, DM Stoppage graph using weighted edges
WCE15_SDMft <- ftable(WCE15_SDMg2$player1, WCE15_SDMg2$player2)
WCE15_SDMft2 <- as.matrix(WCE15_SDMft)
numRows <- nrow(WCE15_SDMft2)
numCols <- ncol(WCE15_SDMft2)
WCE15_SDMft3 <- WCE15_SDMft2[c(2:numRows) , c(2:numCols)]
WCE15_SDMTable <- graph.adjacency(WCE15_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Stoppage graph=weighted
plot.igraph(WCE15_SDMTable, vertex.label = V(WCE15_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Stoppage calulation of network metrics
#igraph
WCE15_SDM.clusterCoef <- transitivity(WCE15_SDMTable, type="global") #cluster coefficient
WCE15_SDM.degreeCent <- centralization.degree(WCE15_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_SDMftn <- as.network.matrix(WCE15_SDMft)
WCE15_SDM.netDensity <- network.density(WCE15_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_SDM.entropy <- entropy(WCE15_SDMft) #entropy

WCE15_SDM.netMx <- cbind(WCE15_SDM.netMx, WCE15_SDM.clusterCoef, WCE15_SDM.degreeCent$centralization,
                         WCE15_SDM.netDensity, WCE15_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_SDM.netMx) <- varnames

#ROUND 15, DM Turnover**********************************************************

round = 15
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE15_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, DM Turnover with weighted edges
WCE15_TDMg2 <- data.frame(WCE15_TDM)
WCE15_TDMg2 <- WCE15_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_TDMg2$player1
player2vector <- WCE15_TDMg2$player2
WCE15_TDMg3 <- WCE15_TDMg2
WCE15_TDMg3$p1inp2vec <- is.element(WCE15_TDMg3$player1, player2vector)
WCE15_TDMg3$p2inp1vec <- is.element(WCE15_TDMg3$player2, player1vector)

addPlayer1 <- WCE15_TDMg3[ which(WCE15_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_TDMg3[ which(WCE15_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_TDMg2 <- rbind(WCE15_TDMg2, addPlayers)

#ROUND 15, DM Turnover graph using weighted edges
WCE15_TDMft <- ftable(WCE15_TDMg2$player1, WCE15_TDMg2$player2)
WCE15_TDMft2 <- as.matrix(WCE15_TDMft)
numRows <- nrow(WCE15_TDMft2)
numCols <- ncol(WCE15_TDMft2)
WCE15_TDMft3 <- WCE15_TDMft2[c(2:numRows) , c(2:numCols)]
WCE15_TDMTable <- graph.adjacency(WCE15_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 15, DM Turnover graph=weighted
plot.igraph(WCE15_TDMTable, vertex.label = V(WCE15_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, DM Turnover calulation of network metrics
#igraph
WCE15_TDM.clusterCoef <- transitivity(WCE15_TDMTable, type="global") #cluster coefficient
WCE15_TDM.degreeCent <- centralization.degree(WCE15_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_TDMftn <- as.network.matrix(WCE15_TDMft)
WCE15_TDM.netDensity <- network.density(WCE15_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_TDM.entropy <- entropy(WCE15_TDMft) #entropy

WCE15_TDM.netMx <- cbind(WCE15_TDM.netMx, WCE15_TDM.clusterCoef, WCE15_TDM.degreeCent$centralization,
                         WCE15_TDM.netDensity, WCE15_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_TDM.netMx) <- varnames

#ROUND 15, D Stoppage**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE15_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Stoppage with weighted edges
WCE15_SDg2 <- data.frame(WCE15_SD)
WCE15_SDg2 <- WCE15_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_SDg2$player1
player2vector <- WCE15_SDg2$player2
WCE15_SDg3 <- WCE15_SDg2
WCE15_SDg3$p1inp2vec <- is.element(WCE15_SDg3$player1, player2vector)
WCE15_SDg3$p2inp1vec <- is.element(WCE15_SDg3$player2, player1vector)

addPlayer1 <- WCE15_SDg3[ which(WCE15_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_SDg3[ which(WCE15_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_SDg2 <- rbind(WCE15_SDg2, addPlayers)

#ROUND 15, D Stoppage graph using weighted edges
WCE15_SDft <- ftable(WCE15_SDg2$player1, WCE15_SDg2$player2)
WCE15_SDft2 <- as.matrix(WCE15_SDft)
numRows <- nrow(WCE15_SDft2)
numCols <- ncol(WCE15_SDft2)
WCE15_SDft3 <- WCE15_SDft2[c(2:numRows) , c(2:numCols)]
WCE15_SDTable <- graph.adjacency(WCE15_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Stoppage graph=weighted
plot.igraph(WCE15_SDTable, vertex.label = V(WCE15_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Stoppage calulation of network metrics
#igraph
WCE15_SD.clusterCoef <- transitivity(WCE15_SDTable, type="global") #cluster coefficient
WCE15_SD.degreeCent <- centralization.degree(WCE15_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_SDftn <- as.network.matrix(WCE15_SDft)
WCE15_SD.netDensity <- network.density(WCE15_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_SD.entropy <- entropy(WCE15_SDft) #entropy

WCE15_SD.netMx <- cbind(WCE15_SD.netMx, WCE15_SD.clusterCoef, WCE15_SD.degreeCent$centralization,
                        WCE15_SD.netDensity, WCE15_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_SD.netMx) <- varnames

#ROUND 15, D Turnover**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE15_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, D Turnover with weighted edges
WCE15_TDg2 <- data.frame(WCE15_TD)
WCE15_TDg2 <- WCE15_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_TDg2$player1
player2vector <- WCE15_TDg2$player2
WCE15_TDg3 <- WCE15_TDg2
WCE15_TDg3$p1inp2vec <- is.element(WCE15_TDg3$player1, player2vector)
WCE15_TDg3$p2inp1vec <- is.element(WCE15_TDg3$player2, player1vector)

addPlayer1 <- WCE15_TDg3[ which(WCE15_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_TDg3[ which(WCE15_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_TDg2 <- rbind(WCE15_TDg2, addPlayers)

#ROUND 15, D Turnover graph using weighted edges
WCE15_TDft <- ftable(WCE15_TDg2$player1, WCE15_TDg2$player2)
WCE15_TDft2 <- as.matrix(WCE15_TDft)
numRows <- nrow(WCE15_TDft2)
numCols <- ncol(WCE15_TDft2)
WCE15_TDft3 <- WCE15_TDft2[c(2:numRows) , c(2:numCols)]
WCE15_TDTable <- graph.adjacency(WCE15_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, D Turnover graph=weighted
plot.igraph(WCE15_TDTable, vertex.label = V(WCE15_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, D Turnover calulation of network metrics
#igraph
WCE15_TD.clusterCoef <- transitivity(WCE15_TDTable, type="global") #cluster coefficient
WCE15_TD.degreeCent <- centralization.degree(WCE15_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_TDftn <- as.network.matrix(WCE15_TDft)
WCE15_TD.netDensity <- network.density(WCE15_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_TD.entropy <- entropy(WCE15_TDft) #entropy

WCE15_TD.netMx <- cbind(WCE15_TD.netMx, WCE15_TD.clusterCoef, WCE15_TD.degreeCent$centralization,
                        WCE15_TD.netDensity, WCE15_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_TD.netMx) <- varnames

#ROUND 15, End of Qtr**********************************************************
#NA

round = 15
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE15_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 15, End of Qtr with weighted edges
WCE15_QTg2 <- data.frame(WCE15_QT)
WCE15_QTg2 <- WCE15_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE15_QTg2$player1
player2vector <- WCE15_QTg2$player2
WCE15_QTg3 <- WCE15_QTg2
WCE15_QTg3$p1inp2vec <- is.element(WCE15_QTg3$player1, player2vector)
WCE15_QTg3$p2inp1vec <- is.element(WCE15_QTg3$player2, player1vector)

addPlayer1 <- WCE15_QTg3[ which(WCE15_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE15_QTg3[ which(WCE15_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE15_QTg2 <- rbind(WCE15_QTg2, addPlayers)

#ROUND 15, End of Qtr graph using weighted edges
WCE15_QTft <- ftable(WCE15_QTg2$player1, WCE15_QTg2$player2)
WCE15_QTft2 <- as.matrix(WCE15_QTft)
numRows <- nrow(WCE15_QTft2)
numCols <- ncol(WCE15_QTft2)
WCE15_QTft3 <- WCE15_QTft2[c(2:numRows) , c(2:numCols)]
WCE15_QTTable <- graph.adjacency(WCE15_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 15, End of Qtr graph=weighted
plot.igraph(WCE15_QTTable, vertex.label = V(WCE15_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE15_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 15, End of Qtr calulation of network metrics
#igraph
WCE15_QT.clusterCoef <- transitivity(WCE15_QTTable, type="global") #cluster coefficient
WCE15_QT.degreeCent <- centralization.degree(WCE15_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE15_QTftn <- as.network.matrix(WCE15_QTft)
WCE15_QT.netDensity <- network.density(WCE15_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE15_QT.entropy <- entropy(WCE15_QTft) #entropy

WCE15_QT.netMx <- cbind(WCE15_QT.netMx, WCE15_QT.clusterCoef, WCE15_QT.degreeCent$centralization,
                        WCE15_QT.netDensity, WCE15_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE15_QT.netMx) <- varnames
