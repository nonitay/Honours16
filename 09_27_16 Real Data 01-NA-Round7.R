#####
#09-26-16- Real data 01
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
ADEL07_Gg2 <- data.frame(ADEL07_G)
ADEL07_Gg2 <- ADEL07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_Gg2$player1
player2vector <- ADEL07_Gg2$player2
ADEL07_Gg3 <- ADEL07_Gg2
ADEL07_Gg3$p1inp2vec <- is.element(ADEL07_Gg3$player1, player2vector)
ADEL07_Gg3$p2inp1vec <- is.element(ADEL07_Gg3$player2, player1vector)

addPlayer1 <- ADEL07_Gg3[ which(ADEL07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_Gg3[ which(ADEL07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_Gg2 <- rbind(ADEL07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
ADEL07_Gft <- ftable(ADEL07_Gg2$player1, ADEL07_Gg2$player2)
ADEL07_Gft2 <- as.matrix(ADEL07_Gft)
numRows <- nrow(ADEL07_Gft2)
numCols <- ncol(ADEL07_Gft2)
ADEL07_Gft3 <- ADEL07_Gft2[c(2:numRows) , c(2:numCols)]
ADEL07_GTable <- graph.adjacency(ADEL07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(ADEL07_GTable, vertex.label = V(ADEL07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
ADEL07_G.clusterCoef <- transitivity(ADEL07_GTable, type="global") #cluster coefficient
ADEL07_G.degreeCent <- centralization.degree(ADEL07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_Gftn <- as.network.matrix(ADEL07_Gft)
ADEL07_G.netDensity <- network.density(ADEL07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_G.entropy <- entropy(ADEL07_Gft) #entropy

ADEL07_G.netMx <- cbind(ADEL07_G.netMx, ADEL07_G.clusterCoef, ADEL07_G.degreeCent$centralization,
                        ADEL07_G.netDensity, ADEL07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************

round = 7
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
ADEL07_Bg2 <- data.frame(ADEL07_B)
ADEL07_Bg2 <- ADEL07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_Bg2$player1
player2vector <- ADEL07_Bg2$player2
ADEL07_Bg3 <- ADEL07_Bg2
ADEL07_Bg3$p1inp2vec <- is.element(ADEL07_Bg3$player1, player2vector)
ADEL07_Bg3$p2inp1vec <- is.element(ADEL07_Bg3$player2, player1vector)

addPlayer1 <- ADEL07_Bg3[ which(ADEL07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_Bg3[ which(ADEL07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_Bg2 <- rbind(ADEL07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
ADEL07_Bft <- ftable(ADEL07_Bg2$player1, ADEL07_Bg2$player2)
ADEL07_Bft2 <- as.matrix(ADEL07_Bft)
numRows <- nrow(ADEL07_Bft2)
numCols <- ncol(ADEL07_Bft2)
ADEL07_Bft3 <- ADEL07_Bft2[c(2:numRows) , c(2:numCols)]
ADEL07_BTable <- graph.adjacency(ADEL07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(ADEL07_BTable, vertex.label = V(ADEL07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
ADEL07_B.clusterCoef <- transitivity(ADEL07_BTable, type="global") #cluster coefficient
ADEL07_B.degreeCent <- centralization.degree(ADEL07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_Bftn <- as.network.matrix(ADEL07_Bft)
ADEL07_B.netDensity <- network.density(ADEL07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_B.entropy <- entropy(ADEL07_Bft) #entropy

ADEL07_B.netMx <- cbind(ADEL07_B.netMx, ADEL07_B.clusterCoef, ADEL07_B.degreeCent$centralization,
                        ADEL07_B.netDensity, ADEL07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************

round = 7
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
ADEL07_SFg2 <- data.frame(ADEL07_SF)
ADEL07_SFg2 <- ADEL07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_SFg2$player1
player2vector <- ADEL07_SFg2$player2
ADEL07_SFg3 <- ADEL07_SFg2
ADEL07_SFg3$p1inp2vec <- is.element(ADEL07_SFg3$player1, player2vector)
ADEL07_SFg3$p2inp1vec <- is.element(ADEL07_SFg3$player2, player1vector)

addPlayer1 <- ADEL07_SFg3[ which(ADEL07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_SFg3[ which(ADEL07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_SFg2 <- rbind(ADEL07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
ADEL07_SFft <- ftable(ADEL07_SFg2$player1, ADEL07_SFg2$player2)
ADEL07_SFft2 <- as.matrix(ADEL07_SFft)
numRows <- nrow(ADEL07_SFft2)
numCols <- ncol(ADEL07_SFft2)
ADEL07_SFft3 <- ADEL07_SFft2[c(2:numRows) , c(2:numCols)]
ADEL07_SFTable <- graph.adjacency(ADEL07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(ADEL07_SFTable, vertex.label = V(ADEL07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
ADEL07_SF.clusterCoef <- transitivity(ADEL07_SFTable, type="global") #cluster coefficient
ADEL07_SF.degreeCent <- centralization.degree(ADEL07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_SFftn <- as.network.matrix(ADEL07_SFft)
ADEL07_SF.netDensity <- network.density(ADEL07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_SF.entropy <- entropy(ADEL07_SFft) #entropy

ADEL07_SF.netMx <- cbind(ADEL07_SF.netMx, ADEL07_SF.clusterCoef, ADEL07_SF.degreeCent$centralization,
                         ADEL07_SF.netDensity, ADEL07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
ADEL07_TFg2 <- data.frame(ADEL07_TF)
ADEL07_TFg2 <- ADEL07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_TFg2$player1
player2vector <- ADEL07_TFg2$player2
ADEL07_TFg3 <- ADEL07_TFg2
ADEL07_TFg3$p1inp2vec <- is.element(ADEL07_TFg3$player1, player2vector)
ADEL07_TFg3$p2inp1vec <- is.element(ADEL07_TFg3$player2, player1vector)

addPlayer1 <- ADEL07_TFg3[ which(ADEL07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_TFg3[ which(ADEL07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_TFg2 <- rbind(ADEL07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
ADEL07_TFft <- ftable(ADEL07_TFg2$player1, ADEL07_TFg2$player2)
ADEL07_TFft2 <- as.matrix(ADEL07_TFft)
numRows <- nrow(ADEL07_TFft2)
numCols <- ncol(ADEL07_TFft2)
ADEL07_TFft3 <- ADEL07_TFft2[c(2:numRows) , c(2:numCols)]
ADEL07_TFTable <- graph.adjacency(ADEL07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(ADEL07_TFTable, vertex.label = V(ADEL07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
ADEL07_TF.clusterCoef <- transitivity(ADEL07_TFTable, type="global") #cluster coefficient
ADEL07_TF.degreeCent <- centralization.degree(ADEL07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_TFftn <- as.network.matrix(ADEL07_TFft)
ADEL07_TF.netDensity <- network.density(ADEL07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_TF.entropy <- entropy(ADEL07_TFft) #entropy

ADEL07_TF.netMx <- cbind(ADEL07_TF.netMx, ADEL07_TF.clusterCoef, ADEL07_TF.degreeCent$centralization,
                         ADEL07_TF.netDensity, ADEL07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
ADEL07_SAMg2 <- data.frame(ADEL07_SAM)
ADEL07_SAMg2 <- ADEL07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_SAMg2$player1
player2vector <- ADEL07_SAMg2$player2
ADEL07_SAMg3 <- ADEL07_SAMg2
ADEL07_SAMg3$p1inp2vec <- is.element(ADEL07_SAMg3$player1, player2vector)
ADEL07_SAMg3$p2inp1vec <- is.element(ADEL07_SAMg3$player2, player1vector)

addPlayer1 <- ADEL07_SAMg3[ which(ADEL07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_SAMg3[ which(ADEL07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_SAMg2 <- rbind(ADEL07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
ADEL07_SAMft <- ftable(ADEL07_SAMg2$player1, ADEL07_SAMg2$player2)
ADEL07_SAMft2 <- as.matrix(ADEL07_SAMft)
numRows <- nrow(ADEL07_SAMft2)
numCols <- ncol(ADEL07_SAMft2)
ADEL07_SAMft3 <- ADEL07_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL07_SAMTable <- graph.adjacency(ADEL07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(ADEL07_SAMTable, vertex.label = V(ADEL07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
ADEL07_SAM.clusterCoef <- transitivity(ADEL07_SAMTable, type="global") #cluster coefficient
ADEL07_SAM.degreeCent <- centralization.degree(ADEL07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_SAMftn <- as.network.matrix(ADEL07_SAMft)
ADEL07_SAM.netDensity <- network.density(ADEL07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_SAM.entropy <- entropy(ADEL07_SAMft) #entropy

ADEL07_SAM.netMx <- cbind(ADEL07_SAM.netMx, ADEL07_SAM.clusterCoef, ADEL07_SAM.degreeCent$centralization,
                          ADEL07_SAM.netDensity, ADEL07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************
#NA

round = 7
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
ADEL07_TAMg2 <- data.frame(ADEL07_TAM)
ADEL07_TAMg2 <- ADEL07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_TAMg2$player1
player2vector <- ADEL07_TAMg2$player2
ADEL07_TAMg3 <- ADEL07_TAMg2
ADEL07_TAMg3$p1inp2vec <- is.element(ADEL07_TAMg3$player1, player2vector)
ADEL07_TAMg3$p2inp1vec <- is.element(ADEL07_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL07_TAMg3[ which(ADEL07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL07_TAMg2 <- rbind(ADEL07_TAMg2, addPlayer2)

#ROUND 7, AM Turnover graph using weighted edges
ADEL07_TAMft <- ftable(ADEL07_TAMg2$player1, ADEL07_TAMg2$player2)
ADEL07_TAMft2 <- as.matrix(ADEL07_TAMft)
numRows <- nrow(ADEL07_TAMft2)
numCols <- ncol(ADEL07_TAMft2)
ADEL07_TAMft3 <- ADEL07_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL07_TAMTable <- graph.adjacency(ADEL07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(ADEL07_TAMTable, vertex.label = V(ADEL07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
ADEL07_TAM.clusterCoef <- transitivity(ADEL07_TAMTable, type="global") #cluster coefficient
ADEL07_TAM.degreeCent <- centralization.degree(ADEL07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_TAMftn <- as.network.matrix(ADEL07_TAMft)
ADEL07_TAM.netDensity <- network.density(ADEL07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_TAM.entropy <- entropy(ADEL07_TAMft) #entropy

ADEL07_TAM.netMx <- cbind(ADEL07_TAM.netMx, ADEL07_TAM.clusterCoef, ADEL07_TAM.degreeCent$centralization,
                          ADEL07_TAM.netDensity, ADEL07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
ADEL07_SDMg2 <- data.frame(ADEL07_SDM)
ADEL07_SDMg2 <- ADEL07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_SDMg2$player1
player2vector <- ADEL07_SDMg2$player2
ADEL07_SDMg3 <- ADEL07_SDMg2
ADEL07_SDMg3$p1inp2vec <- is.element(ADEL07_SDMg3$player1, player2vector)
ADEL07_SDMg3$p2inp1vec <- is.element(ADEL07_SDMg3$player2, player1vector)

addPlayer1 <- ADEL07_SDMg3[ which(ADEL07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_SDMg3[ which(ADEL07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_SDMg2 <- rbind(ADEL07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
ADEL07_SDMft <- ftable(ADEL07_SDMg2$player1, ADEL07_SDMg2$player2)
ADEL07_SDMft2 <- as.matrix(ADEL07_SDMft)
numRows <- nrow(ADEL07_SDMft2)
numCols <- ncol(ADEL07_SDMft2)
ADEL07_SDMft3 <- ADEL07_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL07_SDMTable <- graph.adjacency(ADEL07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(ADEL07_SDMTable, vertex.label = V(ADEL07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
ADEL07_SDM.clusterCoef <- transitivity(ADEL07_SDMTable, type="global") #cluster coefficient
ADEL07_SDM.degreeCent <- centralization.degree(ADEL07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_SDMftn <- as.network.matrix(ADEL07_SDMft)
ADEL07_SDM.netDensity <- network.density(ADEL07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_SDM.entropy <- entropy(ADEL07_SDMft) #entropy

ADEL07_SDM.netMx <- cbind(ADEL07_SDM.netMx, ADEL07_SDM.clusterCoef, ADEL07_SDM.degreeCent$centralization,
                          ADEL07_SDM.netDensity, ADEL07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
ADEL07_TDMg2 <- data.frame(ADEL07_TDM)
ADEL07_TDMg2 <- ADEL07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_TDMg2$player1
player2vector <- ADEL07_TDMg2$player2
ADEL07_TDMg3 <- ADEL07_TDMg2
ADEL07_TDMg3$p1inp2vec <- is.element(ADEL07_TDMg3$player1, player2vector)
ADEL07_TDMg3$p2inp1vec <- is.element(ADEL07_TDMg3$player2, player1vector)

addPlayer1 <- ADEL07_TDMg3[ which(ADEL07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_TDMg3[ which(ADEL07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_TDMg2 <- rbind(ADEL07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
ADEL07_TDMft <- ftable(ADEL07_TDMg2$player1, ADEL07_TDMg2$player2)
ADEL07_TDMft2 <- as.matrix(ADEL07_TDMft)
numRows <- nrow(ADEL07_TDMft2)
numCols <- ncol(ADEL07_TDMft2)
ADEL07_TDMft3 <- ADEL07_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL07_TDMTable <- graph.adjacency(ADEL07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(ADEL07_TDMTable, vertex.label = V(ADEL07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
ADEL07_TDM.clusterCoef <- transitivity(ADEL07_TDMTable, type="global") #cluster coefficient
ADEL07_TDM.degreeCent <- centralization.degree(ADEL07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_TDMftn <- as.network.matrix(ADEL07_TDMft)
ADEL07_TDM.netDensity <- network.density(ADEL07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_TDM.entropy <- entropy(ADEL07_TDMft) #entropy

ADEL07_TDM.netMx <- cbind(ADEL07_TDM.netMx, ADEL07_TDM.clusterCoef, ADEL07_TDM.degreeCent$centralization,
                          ADEL07_TDM.netDensity, ADEL07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************

round = 7
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
ADEL07_SDg2 <- data.frame(ADEL07_SD)
ADEL07_SDg2 <- ADEL07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_SDg2$player1
player2vector <- ADEL07_SDg2$player2
ADEL07_SDg3 <- ADEL07_SDg2
ADEL07_SDg3$p1inp2vec <- is.element(ADEL07_SDg3$player1, player2vector)
ADEL07_SDg3$p2inp1vec <- is.element(ADEL07_SDg3$player2, player1vector)

addPlayer1 <- ADEL07_SDg3[ which(ADEL07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL07_SDg3[ which(ADEL07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_SDg2 <- rbind(ADEL07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
ADEL07_SDft <- ftable(ADEL07_SDg2$player1, ADEL07_SDg2$player2)
ADEL07_SDft2 <- as.matrix(ADEL07_SDft)
numRows <- nrow(ADEL07_SDft2)
numCols <- ncol(ADEL07_SDft2)
ADEL07_SDft3 <- ADEL07_SDft2[c(2:numRows) , c(2:numCols)]
ADEL07_SDTable <- graph.adjacency(ADEL07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(ADEL07_SDTable, vertex.label = V(ADEL07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
ADEL07_SD.clusterCoef <- transitivity(ADEL07_SDTable, type="global") #cluster coefficient
ADEL07_SD.degreeCent <- centralization.degree(ADEL07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_SDftn <- as.network.matrix(ADEL07_SDft)
ADEL07_SD.netDensity <- network.density(ADEL07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_SD.entropy <- entropy(ADEL07_SDft) #entropy

ADEL07_SD.netMx <- cbind(ADEL07_SD.netMx, ADEL07_SD.clusterCoef, ADEL07_SD.degreeCent$centralization,
                         ADEL07_SD.netDensity, ADEL07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
ADEL07_TDg2 <- data.frame(ADEL07_TD)
ADEL07_TDg2 <- ADEL07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_TDg2$player1
player2vector <- ADEL07_TDg2$player2
ADEL07_TDg3 <- ADEL07_TDg2
ADEL07_TDg3$p1inp2vec <- is.element(ADEL07_TDg3$player1, player2vector)
ADEL07_TDg3$p2inp1vec <- is.element(ADEL07_TDg3$player2, player1vector)

addPlayer1 <- ADEL07_TDg3[ which(ADEL07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_TDg3[ which(ADEL07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_TDg2 <- rbind(ADEL07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
ADEL07_TDft <- ftable(ADEL07_TDg2$player1, ADEL07_TDg2$player2)
ADEL07_TDft2 <- as.matrix(ADEL07_TDft)
numRows <- nrow(ADEL07_TDft2)
numCols <- ncol(ADEL07_TDft2)
ADEL07_TDft3 <- ADEL07_TDft2[c(2:numRows) , c(2:numCols)]
ADEL07_TDTable <- graph.adjacency(ADEL07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(ADEL07_TDTable, vertex.label = V(ADEL07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
ADEL07_TD.clusterCoef <- transitivity(ADEL07_TDTable, type="global") #cluster coefficient
ADEL07_TD.degreeCent <- centralization.degree(ADEL07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_TDftn <- as.network.matrix(ADEL07_TDft)
ADEL07_TD.netDensity <- network.density(ADEL07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_TD.entropy <- entropy(ADEL07_TDft) #entropy

ADEL07_TD.netMx <- cbind(ADEL07_TD.netMx, ADEL07_TD.clusterCoef, ADEL07_TD.degreeCent$centralization,
                         ADEL07_TD.netDensity, ADEL07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
ADEL07_QTg2 <- data.frame(ADEL07_QT)
ADEL07_QTg2 <- ADEL07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL07_QTg2$player1
player2vector <- ADEL07_QTg2$player2
ADEL07_QTg3 <- ADEL07_QTg2
ADEL07_QTg3$p1inp2vec <- is.element(ADEL07_QTg3$player1, player2vector)
ADEL07_QTg3$p2inp1vec <- is.element(ADEL07_QTg3$player2, player1vector)

addPlayer1 <- ADEL07_QTg3[ which(ADEL07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL07_QTg3[ which(ADEL07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL07_QTg2 <- rbind(ADEL07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
ADEL07_QTft <- ftable(ADEL07_QTg2$player1, ADEL07_QTg2$player2)
ADEL07_QTft2 <- as.matrix(ADEL07_QTft)
numRows <- nrow(ADEL07_QTft2)
numCols <- ncol(ADEL07_QTft2)
ADEL07_QTft3 <- ADEL07_QTft2[c(2:numRows) , c(2:numCols)]
ADEL07_QTTable <- graph.adjacency(ADEL07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(ADEL07_QTTable, vertex.label = V(ADEL07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
ADEL07_QT.clusterCoef <- transitivity(ADEL07_QTTable, type="global") #cluster coefficient
ADEL07_QT.degreeCent <- centralization.degree(ADEL07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL07_QTftn <- as.network.matrix(ADEL07_QTft)
ADEL07_QT.netDensity <- network.density(ADEL07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL07_QT.entropy <- entropy(ADEL07_QTft) #entropy

ADEL07_QT.netMx <- cbind(ADEL07_QT.netMx, ADEL07_QT.clusterCoef, ADEL07_QT.degreeCent$centralization,
                         ADEL07_QT.netDensity, ADEL07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL07_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "BL"
KIoutcome = "Goal_F"
BL07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
BL07_Gg2 <- data.frame(BL07_G)
BL07_Gg2 <- BL07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_Gg2$player1
player2vector <- BL07_Gg2$player2
BL07_Gg3 <- BL07_Gg2
BL07_Gg3$p1inp2vec <- is.element(BL07_Gg3$player1, player2vector)
BL07_Gg3$p2inp1vec <- is.element(BL07_Gg3$player2, player1vector)

addPlayer1 <- BL07_Gg3[ which(BL07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_Gg3[ which(BL07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_Gg2 <- rbind(BL07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
BL07_Gft <- ftable(BL07_Gg2$player1, BL07_Gg2$player2)
BL07_Gft2 <- as.matrix(BL07_Gft)
numRows <- nrow(BL07_Gft2)
numCols <- ncol(BL07_Gft2)
BL07_Gft3 <- BL07_Gft2[c(2:numRows) , c(2:numCols)]
BL07_GTable <- graph.adjacency(BL07_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(BL07_GTable, vertex.label = V(BL07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
BL07_G.clusterCoef <- transitivity(BL07_GTable, type="global") #cluster coefficient
BL07_G.degreeCent <- centralization.degree(BL07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_Gftn <- as.network.matrix(BL07_Gft)
BL07_G.netDensity <- network.density(BL07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_G.entropy <- entropy(BL07_Gft) #entropy

BL07_G.netMx <- cbind(BL07_G.netMx, BL07_G.clusterCoef, BL07_G.degreeCent$centralization,
                      BL07_G.netDensity, BL07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "BL"
KIoutcome = "Behind_F"
BL07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
BL07_Bg2 <- data.frame(BL07_B)
BL07_Bg2 <- BL07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_Bg2$player1
player2vector <- BL07_Bg2$player2
BL07_Bg3 <- BL07_Bg2
BL07_Bg3$p1inp2vec <- is.element(BL07_Bg3$player1, player2vector)
BL07_Bg3$p2inp1vec <- is.element(BL07_Bg3$player2, player1vector)

addPlayer1 <- BL07_Bg3[ which(BL07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_Bg3[ which(BL07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_Bg2 <- rbind(BL07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
BL07_Bft <- ftable(BL07_Bg2$player1, BL07_Bg2$player2)
BL07_Bft2 <- as.matrix(BL07_Bft)
numRows <- nrow(BL07_Bft2)
numCols <- ncol(BL07_Bft2)
BL07_Bft3 <- BL07_Bft2[c(2:numRows) , c(2:numCols)]
BL07_BTable <- graph.adjacency(BL07_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(BL07_BTable, vertex.label = V(BL07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
BL07_B.clusterCoef <- transitivity(BL07_BTable, type="global") #cluster coefficient
BL07_B.degreeCent <- centralization.degree(BL07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_Bftn <- as.network.matrix(BL07_Bft)
BL07_B.netDensity <- network.density(BL07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_B.entropy <- entropy(BL07_Bft) #entropy

BL07_B.netMx <- cbind(BL07_B.netMx, BL07_B.clusterCoef, BL07_B.degreeCent$centralization,
                      BL07_B.netDensity, BL07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "BL"
KIoutcome = "Stoppage_F"
BL07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
BL07_SFg2 <- data.frame(BL07_SF)
BL07_SFg2 <- BL07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_SFg2$player1
player2vector <- BL07_SFg2$player2
BL07_SFg3 <- BL07_SFg2
BL07_SFg3$p1inp2vec <- is.element(BL07_SFg3$player1, player2vector)
BL07_SFg3$p2inp1vec <- is.element(BL07_SFg3$player2, player1vector)

addPlayer1 <- BL07_SFg3[ which(BL07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_SFg3[ which(BL07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_SFg2 <- rbind(BL07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
BL07_SFft <- ftable(BL07_SFg2$player1, BL07_SFg2$player2)
BL07_SFft2 <- as.matrix(BL07_SFft)
numRows <- nrow(BL07_SFft2)
numCols <- ncol(BL07_SFft2)
BL07_SFft3 <- BL07_SFft2[c(2:numRows) , c(2:numCols)]
BL07_SFTable <- graph.adjacency(BL07_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(BL07_SFTable, vertex.label = V(BL07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
BL07_SF.clusterCoef <- transitivity(BL07_SFTable, type="global") #cluster coefficient
BL07_SF.degreeCent <- centralization.degree(BL07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_SFftn <- as.network.matrix(BL07_SFft)
BL07_SF.netDensity <- network.density(BL07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_SF.entropy <- entropy(BL07_SFft) #entropy

BL07_SF.netMx <- cbind(BL07_SF.netMx, BL07_SF.clusterCoef, BL07_SF.degreeCent$centralization,
                       BL07_SF.netDensity, BL07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "BL"
KIoutcome = "Turnover_F"
BL07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
BL07_TFg2 <- data.frame(BL07_TF)
BL07_TFg2 <- BL07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_TFg2$player1
player2vector <- BL07_TFg2$player2
BL07_TFg3 <- BL07_TFg2
BL07_TFg3$p1inp2vec <- is.element(BL07_TFg3$player1, player2vector)
BL07_TFg3$p2inp1vec <- is.element(BL07_TFg3$player2, player1vector)

addPlayer1 <- BL07_TFg3[ which(BL07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_TFg3[ which(BL07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_TFg2 <- rbind(BL07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
BL07_TFft <- ftable(BL07_TFg2$player1, BL07_TFg2$player2)
BL07_TFft2 <- as.matrix(BL07_TFft)
numRows <- nrow(BL07_TFft2)
numCols <- ncol(BL07_TFft2)
BL07_TFft3 <- BL07_TFft2[c(2:numRows) , c(2:numCols)]
BL07_TFTable <- graph.adjacency(BL07_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(BL07_TFTable, vertex.label = V(BL07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
BL07_TF.clusterCoef <- transitivity(BL07_TFTable, type="global") #cluster coefficient
BL07_TF.degreeCent <- centralization.degree(BL07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_TFftn <- as.network.matrix(BL07_TFft)
BL07_TF.netDensity <- network.density(BL07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_TF.entropy <- entropy(BL07_TFft) #entropy

BL07_TF.netMx <- cbind(BL07_TF.netMx, BL07_TF.clusterCoef, BL07_TF.degreeCent$centralization,
                       BL07_TF.netDensity, BL07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
BL07_SAMg2 <- data.frame(BL07_SAM)
BL07_SAMg2 <- BL07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_SAMg2$player1
player2vector <- BL07_SAMg2$player2
BL07_SAMg3 <- BL07_SAMg2
BL07_SAMg3$p1inp2vec <- is.element(BL07_SAMg3$player1, player2vector)
BL07_SAMg3$p2inp1vec <- is.element(BL07_SAMg3$player2, player1vector)

addPlayer1 <- BL07_SAMg3[ which(BL07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_SAMg3[ which(BL07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_SAMg2 <- rbind(BL07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
BL07_SAMft <- ftable(BL07_SAMg2$player1, BL07_SAMg2$player2)
BL07_SAMft2 <- as.matrix(BL07_SAMft)
numRows <- nrow(BL07_SAMft2)
numCols <- ncol(BL07_SAMft2)
BL07_SAMft3 <- BL07_SAMft2[c(2:numRows) , c(2:numCols)]
BL07_SAMTable <- graph.adjacency(BL07_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(BL07_SAMTable, vertex.label = V(BL07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
BL07_SAM.clusterCoef <- transitivity(BL07_SAMTable, type="global") #cluster coefficient
BL07_SAM.degreeCent <- centralization.degree(BL07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_SAMftn <- as.network.matrix(BL07_SAMft)
BL07_SAM.netDensity <- network.density(BL07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_SAM.entropy <- entropy(BL07_SAMft) #entropy

BL07_SAM.netMx <- cbind(BL07_SAM.netMx, BL07_SAM.clusterCoef, BL07_SAM.degreeCent$centralization,
                        BL07_SAM.netDensity, BL07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "BL"
KIoutcome = "Turnover_AM"
BL07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
BL07_TAMg2 <- data.frame(BL07_TAM)
BL07_TAMg2 <- BL07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_TAMg2$player1
player2vector <- BL07_TAMg2$player2
BL07_TAMg3 <- BL07_TAMg2
BL07_TAMg3$p1inp2vec <- is.element(BL07_TAMg3$player1, player2vector)
BL07_TAMg3$p2inp1vec <- is.element(BL07_TAMg3$player2, player1vector)

addPlayer1 <- BL07_TAMg3[ which(BL07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_TAMg3[ which(BL07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_TAMg2 <- rbind(BL07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
BL07_TAMft <- ftable(BL07_TAMg2$player1, BL07_TAMg2$player2)
BL07_TAMft2 <- as.matrix(BL07_TAMft)
numRows <- nrow(BL07_TAMft2)
numCols <- ncol(BL07_TAMft2)
BL07_TAMft3 <- BL07_TAMft2[c(2:numRows) , c(2:numCols)]
BL07_TAMTable <- graph.adjacency(BL07_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(BL07_TAMTable, vertex.label = V(BL07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
BL07_TAM.clusterCoef <- transitivity(BL07_TAMTable, type="global") #cluster coefficient
BL07_TAM.degreeCent <- centralization.degree(BL07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_TAMftn <- as.network.matrix(BL07_TAMft)
BL07_TAM.netDensity <- network.density(BL07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_TAM.entropy <- entropy(BL07_TAMft) #entropy

BL07_TAM.netMx <- cbind(BL07_TAM.netMx, BL07_TAM.clusterCoef, BL07_TAM.degreeCent$centralization,
                        BL07_TAM.netDensity, BL07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
BL07_SDMg2 <- data.frame(BL07_SDM)
BL07_SDMg2 <- BL07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_SDMg2$player1
player2vector <- BL07_SDMg2$player2
BL07_SDMg3 <- BL07_SDMg2
BL07_SDMg3$p1inp2vec <- is.element(BL07_SDMg3$player1, player2vector)
BL07_SDMg3$p2inp1vec <- is.element(BL07_SDMg3$player2, player1vector)

addPlayer1 <- BL07_SDMg3[ which(BL07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL07_SDMg3[ which(BL07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_SDMg2 <- rbind(BL07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
BL07_SDMft <- ftable(BL07_SDMg2$player1, BL07_SDMg2$player2)
BL07_SDMft2 <- as.matrix(BL07_SDMft)
numRows <- nrow(BL07_SDMft2)
numCols <- ncol(BL07_SDMft2)
BL07_SDMft3 <- BL07_SDMft2[c(2:numRows) , c(2:numCols)]
BL07_SDMTable <- graph.adjacency(BL07_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(BL07_SDMTable, vertex.label = V(BL07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
BL07_SDM.clusterCoef <- transitivity(BL07_SDMTable, type="global") #cluster coefficient
BL07_SDM.degreeCent <- centralization.degree(BL07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_SDMftn <- as.network.matrix(BL07_SDMft)
BL07_SDM.netDensity <- network.density(BL07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_SDM.entropy <- entropy(BL07_SDMft) #entropy

BL07_SDM.netMx <- cbind(BL07_SDM.netMx, BL07_SDM.clusterCoef, BL07_SDM.degreeCent$centralization,
                        BL07_SDM.netDensity, BL07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "BL"
KIoutcome = "Turnover_DM"
BL07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
BL07_TDMg2 <- data.frame(BL07_TDM)
BL07_TDMg2 <- BL07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_TDMg2$player1
player2vector <- BL07_TDMg2$player2
BL07_TDMg3 <- BL07_TDMg2
BL07_TDMg3$p1inp2vec <- is.element(BL07_TDMg3$player1, player2vector)
BL07_TDMg3$p2inp1vec <- is.element(BL07_TDMg3$player2, player1vector)

addPlayer1 <- BL07_TDMg3[ which(BL07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_TDMg3[ which(BL07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_TDMg2 <- rbind(BL07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
BL07_TDMft <- ftable(BL07_TDMg2$player1, BL07_TDMg2$player2)
BL07_TDMft2 <- as.matrix(BL07_TDMft)
numRows <- nrow(BL07_TDMft2)
numCols <- ncol(BL07_TDMft2)
BL07_TDMft3 <- BL07_TDMft2[c(2:numRows) , c(2:numCols)]
BL07_TDMTable <- graph.adjacency(BL07_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(BL07_TDMTable, vertex.label = V(BL07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
BL07_TDM.clusterCoef <- transitivity(BL07_TDMTable, type="global") #cluster coefficient
BL07_TDM.degreeCent <- centralization.degree(BL07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_TDMftn <- as.network.matrix(BL07_TDMft)
BL07_TDM.netDensity <- network.density(BL07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_TDM.entropy <- entropy(BL07_TDMft) #entropy

BL07_TDM.netMx <- cbind(BL07_TDM.netMx, BL07_TDM.clusterCoef, BL07_TDM.degreeCent$centralization,
                        BL07_TDM.netDensity, BL07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "BL"
KIoutcome = "Stoppage_D"
BL07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
BL07_SDg2 <- data.frame(BL07_SD)
BL07_SDg2 <- BL07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_SDg2$player1
player2vector <- BL07_SDg2$player2
BL07_SDg3 <- BL07_SDg2
BL07_SDg3$p1inp2vec <- is.element(BL07_SDg3$player1, player2vector)
BL07_SDg3$p2inp1vec <- is.element(BL07_SDg3$player2, player1vector)

addPlayer1 <- BL07_SDg3[ which(BL07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_SDg3[ which(BL07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_SDg2 <- rbind(BL07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
BL07_SDft <- ftable(BL07_SDg2$player1, BL07_SDg2$player2)
BL07_SDft2 <- as.matrix(BL07_SDft)
numRows <- nrow(BL07_SDft2)
numCols <- ncol(BL07_SDft2)
BL07_SDft3 <- BL07_SDft2[c(2:numRows) , c(2:numCols)]
BL07_SDTable <- graph.adjacency(BL07_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(BL07_SDTable, vertex.label = V(BL07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
BL07_SD.clusterCoef <- transitivity(BL07_SDTable, type="global") #cluster coefficient
BL07_SD.degreeCent <- centralization.degree(BL07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_SDftn <- as.network.matrix(BL07_SDft)
BL07_SD.netDensity <- network.density(BL07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_SD.entropy <- entropy(BL07_SDft) #entropy

BL07_SD.netMx <- cbind(BL07_SD.netMx, BL07_SD.clusterCoef, BL07_SD.degreeCent$centralization,
                       BL07_SD.netDensity, BL07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "BL"
KIoutcome = "Turnover_D"
BL07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
BL07_TDg2 <- data.frame(BL07_TD)
BL07_TDg2 <- BL07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_TDg2$player1
player2vector <- BL07_TDg2$player2
BL07_TDg3 <- BL07_TDg2
BL07_TDg3$p1inp2vec <- is.element(BL07_TDg3$player1, player2vector)
BL07_TDg3$p2inp1vec <- is.element(BL07_TDg3$player2, player1vector)

addPlayer1 <- BL07_TDg3[ which(BL07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_TDg3[ which(BL07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_TDg2 <- rbind(BL07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
BL07_TDft <- ftable(BL07_TDg2$player1, BL07_TDg2$player2)
BL07_TDft2 <- as.matrix(BL07_TDft)
numRows <- nrow(BL07_TDft2)
numCols <- ncol(BL07_TDft2)
BL07_TDft3 <- BL07_TDft2[c(2:numRows) , c(2:numCols)]
BL07_TDTable <- graph.adjacency(BL07_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(BL07_TDTable, vertex.label = V(BL07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
BL07_TD.clusterCoef <- transitivity(BL07_TDTable, type="global") #cluster coefficient
BL07_TD.degreeCent <- centralization.degree(BL07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_TDftn <- as.network.matrix(BL07_TDft)
BL07_TD.netDensity <- network.density(BL07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_TD.entropy <- entropy(BL07_TDft) #entropy

BL07_TD.netMx <- cbind(BL07_TD.netMx, BL07_TD.clusterCoef, BL07_TD.degreeCent$centralization,
                       BL07_TD.netDensity, BL07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
BL07_QTg2 <- data.frame(BL07_QT)
BL07_QTg2 <- BL07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL07_QTg2$player1
player2vector <- BL07_QTg2$player2
BL07_QTg3 <- BL07_QTg2
BL07_QTg3$p1inp2vec <- is.element(BL07_QTg3$player1, player2vector)
BL07_QTg3$p2inp1vec <- is.element(BL07_QTg3$player2, player1vector)

addPlayer1 <- BL07_QTg3[ which(BL07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL07_QTg3[ which(BL07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL07_QTg2 <- rbind(BL07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
BL07_QTft <- ftable(BL07_QTg2$player1, BL07_QTg2$player2)
BL07_QTft2 <- as.matrix(BL07_QTft)
numRows <- nrow(BL07_QTft2)
numCols <- ncol(BL07_QTft2)
BL07_QTft3 <- BL07_QTft2[c(2:numRows) , c(2:numCols)]
BL07_QTTable <- graph.adjacency(BL07_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(BL07_QTTable, vertex.label = V(BL07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
BL07_QT.clusterCoef <- transitivity(BL07_QTTable, type="global") #cluster coefficient
BL07_QT.degreeCent <- centralization.degree(BL07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL07_QTftn <- as.network.matrix(BL07_QTft)
BL07_QT.netDensity <- network.density(BL07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL07_QT.entropy <- entropy(BL07_QTft) #entropy

BL07_QT.netMx <- cbind(BL07_QT.netMx, BL07_QT.clusterCoef, BL07_QT.degreeCent$centralization,
                       BL07_QT.netDensity, BL07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL07_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "CARL"
KIoutcome = "Goal_F"
CARL07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
CARL07_Gg2 <- data.frame(CARL07_G)
CARL07_Gg2 <- CARL07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_Gg2$player1
player2vector <- CARL07_Gg2$player2
CARL07_Gg3 <- CARL07_Gg2
CARL07_Gg3$p1inp2vec <- is.element(CARL07_Gg3$player1, player2vector)
CARL07_Gg3$p2inp1vec <- is.element(CARL07_Gg3$player2, player1vector)

addPlayer1 <- CARL07_Gg3[ which(CARL07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_Gg3[ which(CARL07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_Gg2 <- rbind(CARL07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
CARL07_Gft <- ftable(CARL07_Gg2$player1, CARL07_Gg2$player2)
CARL07_Gft2 <- as.matrix(CARL07_Gft)
numRows <- nrow(CARL07_Gft2)
numCols <- ncol(CARL07_Gft2)
CARL07_Gft3 <- CARL07_Gft2[c(2:numRows) , c(2:numCols)]
CARL07_GTable <- graph.adjacency(CARL07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(CARL07_GTable, vertex.label = V(CARL07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
CARL07_G.clusterCoef <- transitivity(CARL07_GTable, type="global") #cluster coefficient
CARL07_G.degreeCent <- centralization.degree(CARL07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_Gftn <- as.network.matrix(CARL07_Gft)
CARL07_G.netDensity <- network.density(CARL07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_G.entropy <- entropy(CARL07_Gft) #entropy

CARL07_G.netMx <- cbind(CARL07_G.netMx, CARL07_G.clusterCoef, CARL07_G.degreeCent$centralization,
                        CARL07_G.netDensity, CARL07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "CARL"
KIoutcome = "Behind_F"
CARL07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
CARL07_Bg2 <- data.frame(CARL07_B)
CARL07_Bg2 <- CARL07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_Bg2$player1
player2vector <- CARL07_Bg2$player2
CARL07_Bg3 <- CARL07_Bg2
CARL07_Bg3$p1inp2vec <- is.element(CARL07_Bg3$player1, player2vector)
CARL07_Bg3$p2inp1vec <- is.element(CARL07_Bg3$player2, player1vector)

addPlayer1 <- CARL07_Bg3[ which(CARL07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_Bg3[ which(CARL07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_Bg2 <- rbind(CARL07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
CARL07_Bft <- ftable(CARL07_Bg2$player1, CARL07_Bg2$player2)
CARL07_Bft2 <- as.matrix(CARL07_Bft)
numRows <- nrow(CARL07_Bft2)
numCols <- ncol(CARL07_Bft2)
CARL07_Bft3 <- CARL07_Bft2[c(2:numRows) , c(2:numCols)]
CARL07_BTable <- graph.adjacency(CARL07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(CARL07_BTable, vertex.label = V(CARL07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
CARL07_B.clusterCoef <- transitivity(CARL07_BTable, type="global") #cluster coefficient
CARL07_B.degreeCent <- centralization.degree(CARL07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_Bftn <- as.network.matrix(CARL07_Bft)
CARL07_B.netDensity <- network.density(CARL07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_B.entropy <- entropy(CARL07_Bft) #entropy

CARL07_B.netMx <- cbind(CARL07_B.netMx, CARL07_B.clusterCoef, CARL07_B.degreeCent$centralization,
                        CARL07_B.netDensity, CARL07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
CARL07_SFg2 <- data.frame(CARL07_SF)
CARL07_SFg2 <- CARL07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_SFg2$player1
player2vector <- CARL07_SFg2$player2
CARL07_SFg3 <- CARL07_SFg2
CARL07_SFg3$p1inp2vec <- is.element(CARL07_SFg3$player1, player2vector)
CARL07_SFg3$p2inp1vec <- is.element(CARL07_SFg3$player2, player1vector)

addPlayer1 <- CARL07_SFg3[ which(CARL07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_SFg3[ which(CARL07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_SFg2 <- rbind(CARL07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
CARL07_SFft <- ftable(CARL07_SFg2$player1, CARL07_SFg2$player2)
CARL07_SFft2 <- as.matrix(CARL07_SFft)
numRows <- nrow(CARL07_SFft2)
numCols <- ncol(CARL07_SFft2)
CARL07_SFft3 <- CARL07_SFft2[c(2:numRows) , c(2:numCols)]
CARL07_SFTable <- graph.adjacency(CARL07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(CARL07_SFTable, vertex.label = V(CARL07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
CARL07_SF.clusterCoef <- transitivity(CARL07_SFTable, type="global") #cluster coefficient
CARL07_SF.degreeCent <- centralization.degree(CARL07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_SFftn <- as.network.matrix(CARL07_SFft)
CARL07_SF.netDensity <- network.density(CARL07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_SF.entropy <- entropy(CARL07_SFft) #entropy

CARL07_SF.netMx <- cbind(CARL07_SF.netMx, CARL07_SF.clusterCoef, CARL07_SF.degreeCent$centralization,
                         CARL07_SF.netDensity, CARL07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
CARL07_TFg2 <- data.frame(CARL07_TF)
CARL07_TFg2 <- CARL07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_TFg2$player1
player2vector <- CARL07_TFg2$player2
CARL07_TFg3 <- CARL07_TFg2
CARL07_TFg3$p1inp2vec <- is.element(CARL07_TFg3$player1, player2vector)
CARL07_TFg3$p2inp1vec <- is.element(CARL07_TFg3$player2, player1vector)

addPlayer1 <- CARL07_TFg3[ which(CARL07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_TFg3[ which(CARL07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_TFg2 <- rbind(CARL07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
CARL07_TFft <- ftable(CARL07_TFg2$player1, CARL07_TFg2$player2)
CARL07_TFft2 <- as.matrix(CARL07_TFft)
numRows <- nrow(CARL07_TFft2)
numCols <- ncol(CARL07_TFft2)
CARL07_TFft3 <- CARL07_TFft2[c(2:numRows) , c(2:numCols)]
CARL07_TFTable <- graph.adjacency(CARL07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(CARL07_TFTable, vertex.label = V(CARL07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
CARL07_TF.clusterCoef <- transitivity(CARL07_TFTable, type="global") #cluster coefficient
CARL07_TF.degreeCent <- centralization.degree(CARL07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_TFftn <- as.network.matrix(CARL07_TFft)
CARL07_TF.netDensity <- network.density(CARL07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_TF.entropy <- entropy(CARL07_TFft) #entropy

CARL07_TF.netMx <- cbind(CARL07_TF.netMx, CARL07_TF.clusterCoef, CARL07_TF.degreeCent$centralization,
                         CARL07_TF.netDensity, CARL07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
CARL07_SAMg2 <- data.frame(CARL07_SAM)
CARL07_SAMg2 <- CARL07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_SAMg2$player1
player2vector <- CARL07_SAMg2$player2
CARL07_SAMg3 <- CARL07_SAMg2
CARL07_SAMg3$p1inp2vec <- is.element(CARL07_SAMg3$player1, player2vector)
CARL07_SAMg3$p2inp1vec <- is.element(CARL07_SAMg3$player2, player1vector)

addPlayer1 <- CARL07_SAMg3[ which(CARL07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_SAMg3[ which(CARL07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_SAMg2 <- rbind(CARL07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
CARL07_SAMft <- ftable(CARL07_SAMg2$player1, CARL07_SAMg2$player2)
CARL07_SAMft2 <- as.matrix(CARL07_SAMft)
numRows <- nrow(CARL07_SAMft2)
numCols <- ncol(CARL07_SAMft2)
CARL07_SAMft3 <- CARL07_SAMft2[c(2:numRows) , c(2:numCols)]
CARL07_SAMTable <- graph.adjacency(CARL07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(CARL07_SAMTable, vertex.label = V(CARL07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
CARL07_SAM.clusterCoef <- transitivity(CARL07_SAMTable, type="global") #cluster coefficient
CARL07_SAM.degreeCent <- centralization.degree(CARL07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_SAMftn <- as.network.matrix(CARL07_SAMft)
CARL07_SAM.netDensity <- network.density(CARL07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_SAM.entropy <- entropy(CARL07_SAMft) #entropy

CARL07_SAM.netMx <- cbind(CARL07_SAM.netMx, CARL07_SAM.clusterCoef, CARL07_SAM.degreeCent$centralization,
                          CARL07_SAM.netDensity, CARL07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
CARL07_TAMg2 <- data.frame(CARL07_TAM)
CARL07_TAMg2 <- CARL07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_TAMg2$player1
player2vector <- CARL07_TAMg2$player2
CARL07_TAMg3 <- CARL07_TAMg2
CARL07_TAMg3$p1inp2vec <- is.element(CARL07_TAMg3$player1, player2vector)
CARL07_TAMg3$p2inp1vec <- is.element(CARL07_TAMg3$player2, player1vector)

addPlayer1 <- CARL07_TAMg3[ which(CARL07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_TAMg3[ which(CARL07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_TAMg2 <- rbind(CARL07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
CARL07_TAMft <- ftable(CARL07_TAMg2$player1, CARL07_TAMg2$player2)
CARL07_TAMft2 <- as.matrix(CARL07_TAMft)
numRows <- nrow(CARL07_TAMft2)
numCols <- ncol(CARL07_TAMft2)
CARL07_TAMft3 <- CARL07_TAMft2[c(2:numRows) , c(2:numCols)]
CARL07_TAMTable <- graph.adjacency(CARL07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(CARL07_TAMTable, vertex.label = V(CARL07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
CARL07_TAM.clusterCoef <- transitivity(CARL07_TAMTable, type="global") #cluster coefficient
CARL07_TAM.degreeCent <- centralization.degree(CARL07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_TAMftn <- as.network.matrix(CARL07_TAMft)
CARL07_TAM.netDensity <- network.density(CARL07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_TAM.entropy <- entropy(CARL07_TAMft) #entropy

CARL07_TAM.netMx <- cbind(CARL07_TAM.netMx, CARL07_TAM.clusterCoef, CARL07_TAM.degreeCent$centralization,
                          CARL07_TAM.netDensity, CARL07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
CARL07_SDMg2 <- data.frame(CARL07_SDM)
CARL07_SDMg2 <- CARL07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_SDMg2$player1
player2vector <- CARL07_SDMg2$player2
CARL07_SDMg3 <- CARL07_SDMg2
CARL07_SDMg3$p1inp2vec <- is.element(CARL07_SDMg3$player1, player2vector)
CARL07_SDMg3$p2inp1vec <- is.element(CARL07_SDMg3$player2, player1vector)

addPlayer1 <- CARL07_SDMg3[ which(CARL07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_SDMg3[ which(CARL07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_SDMg2 <- rbind(CARL07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
CARL07_SDMft <- ftable(CARL07_SDMg2$player1, CARL07_SDMg2$player2)
CARL07_SDMft2 <- as.matrix(CARL07_SDMft)
numRows <- nrow(CARL07_SDMft2)
numCols <- ncol(CARL07_SDMft2)
CARL07_SDMft3 <- CARL07_SDMft2[c(2:numRows) , c(2:numCols)]
CARL07_SDMTable <- graph.adjacency(CARL07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(CARL07_SDMTable, vertex.label = V(CARL07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
CARL07_SDM.clusterCoef <- transitivity(CARL07_SDMTable, type="global") #cluster coefficient
CARL07_SDM.degreeCent <- centralization.degree(CARL07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_SDMftn <- as.network.matrix(CARL07_SDMft)
CARL07_SDM.netDensity <- network.density(CARL07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_SDM.entropy <- entropy(CARL07_SDMft) #entropy

CARL07_SDM.netMx <- cbind(CARL07_SDM.netMx, CARL07_SDM.clusterCoef, CARL07_SDM.degreeCent$centralization,
                          CARL07_SDM.netDensity, CARL07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
CARL07_TDMg2 <- data.frame(CARL07_TDM)
CARL07_TDMg2 <- CARL07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_TDMg2$player1
player2vector <- CARL07_TDMg2$player2
CARL07_TDMg3 <- CARL07_TDMg2
CARL07_TDMg3$p1inp2vec <- is.element(CARL07_TDMg3$player1, player2vector)
CARL07_TDMg3$p2inp1vec <- is.element(CARL07_TDMg3$player2, player1vector)

addPlayer1 <- CARL07_TDMg3[ which(CARL07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_TDMg3[ which(CARL07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_TDMg2 <- rbind(CARL07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
CARL07_TDMft <- ftable(CARL07_TDMg2$player1, CARL07_TDMg2$player2)
CARL07_TDMft2 <- as.matrix(CARL07_TDMft)
numRows <- nrow(CARL07_TDMft2)
numCols <- ncol(CARL07_TDMft2)
CARL07_TDMft3 <- CARL07_TDMft2[c(2:numRows) , c(2:numCols)]
CARL07_TDMTable <- graph.adjacency(CARL07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(CARL07_TDMTable, vertex.label = V(CARL07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
CARL07_TDM.clusterCoef <- transitivity(CARL07_TDMTable, type="global") #cluster coefficient
CARL07_TDM.degreeCent <- centralization.degree(CARL07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_TDMftn <- as.network.matrix(CARL07_TDMft)
CARL07_TDM.netDensity <- network.density(CARL07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_TDM.entropy <- entropy(CARL07_TDMft) #entropy

CARL07_TDM.netMx <- cbind(CARL07_TDM.netMx, CARL07_TDM.clusterCoef, CARL07_TDM.degreeCent$centralization,
                          CARL07_TDM.netDensity, CARL07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
CARL07_SDg2 <- data.frame(CARL07_SD)
CARL07_SDg2 <- CARL07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_SDg2$player1
player2vector <- CARL07_SDg2$player2
CARL07_SDg3 <- CARL07_SDg2
CARL07_SDg3$p1inp2vec <- is.element(CARL07_SDg3$player1, player2vector)
CARL07_SDg3$p2inp1vec <- is.element(CARL07_SDg3$player2, player1vector)

addPlayer1 <- CARL07_SDg3[ which(CARL07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_SDg3[ which(CARL07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_SDg2 <- rbind(CARL07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
CARL07_SDft <- ftable(CARL07_SDg2$player1, CARL07_SDg2$player2)
CARL07_SDft2 <- as.matrix(CARL07_SDft)
numRows <- nrow(CARL07_SDft2)
numCols <- ncol(CARL07_SDft2)
CARL07_SDft3 <- CARL07_SDft2[c(2:numRows) , c(2:numCols)]
CARL07_SDTable <- graph.adjacency(CARL07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(CARL07_SDTable, vertex.label = V(CARL07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
CARL07_SD.clusterCoef <- transitivity(CARL07_SDTable, type="global") #cluster coefficient
CARL07_SD.degreeCent <- centralization.degree(CARL07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_SDftn <- as.network.matrix(CARL07_SDft)
CARL07_SD.netDensity <- network.density(CARL07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_SD.entropy <- entropy(CARL07_SDft) #entropy

CARL07_SD.netMx <- cbind(CARL07_SD.netMx, CARL07_SD.clusterCoef, CARL07_SD.degreeCent$centralization,
                         CARL07_SD.netDensity, CARL07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************

round = 7
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
CARL07_TDg2 <- data.frame(CARL07_TD)
CARL07_TDg2 <- CARL07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_TDg2$player1
player2vector <- CARL07_TDg2$player2
CARL07_TDg3 <- CARL07_TDg2
CARL07_TDg3$p1inp2vec <- is.element(CARL07_TDg3$player1, player2vector)
CARL07_TDg3$p2inp1vec <- is.element(CARL07_TDg3$player2, player1vector)

addPlayer1 <- CARL07_TDg3[ which(CARL07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_TDg3[ which(CARL07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_TDg2 <- rbind(CARL07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
CARL07_TDft <- ftable(CARL07_TDg2$player1, CARL07_TDg2$player2)
CARL07_TDft2 <- as.matrix(CARL07_TDft)
numRows <- nrow(CARL07_TDft2)
numCols <- ncol(CARL07_TDft2)
CARL07_TDft3 <- CARL07_TDft2[c(2:numRows) , c(2:numCols)]
CARL07_TDTable <- graph.adjacency(CARL07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(CARL07_TDTable, vertex.label = V(CARL07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
CARL07_TD.clusterCoef <- transitivity(CARL07_TDTable, type="global") #cluster coefficient
CARL07_TD.degreeCent <- centralization.degree(CARL07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_TDftn <- as.network.matrix(CARL07_TDft)
CARL07_TD.netDensity <- network.density(CARL07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_TD.entropy <- entropy(CARL07_TDft) #entropy

CARL07_TD.netMx <- cbind(CARL07_TD.netMx, CARL07_TD.clusterCoef, CARL07_TD.degreeCent$centralization,
                         CARL07_TD.netDensity, CARL07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
CARL07_QTg2 <- data.frame(CARL07_QT)
CARL07_QTg2 <- CARL07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL07_QTg2$player1
player2vector <- CARL07_QTg2$player2
CARL07_QTg3 <- CARL07_QTg2
CARL07_QTg3$p1inp2vec <- is.element(CARL07_QTg3$player1, player2vector)
CARL07_QTg3$p2inp1vec <- is.element(CARL07_QTg3$player2, player1vector)

addPlayer1 <- CARL07_QTg3[ which(CARL07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL07_QTg3[ which(CARL07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL07_QTg2 <- rbind(CARL07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
CARL07_QTft <- ftable(CARL07_QTg2$player1, CARL07_QTg2$player2)
CARL07_QTft2 <- as.matrix(CARL07_QTft)
numRows <- nrow(CARL07_QTft2)
numCols <- ncol(CARL07_QTft2)
CARL07_QTft3 <- CARL07_QTft2[c(2:numRows) , c(2:numCols)]
CARL07_QTTable <- graph.adjacency(CARL07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(CARL07_QTTable, vertex.label = V(CARL07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
CARL07_QT.clusterCoef <- transitivity(CARL07_QTTable, type="global") #cluster coefficient
CARL07_QT.degreeCent <- centralization.degree(CARL07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL07_QTftn <- as.network.matrix(CARL07_QTft)
CARL07_QT.netDensity <- network.density(CARL07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL07_QT.entropy <- entropy(CARL07_QTft) #entropy

CARL07_QT.netMx <- cbind(CARL07_QT.netMx, CARL07_QT.clusterCoef, CARL07_QT.degreeCent$centralization,
                         CARL07_QT.netDensity, CARL07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL07_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "COLL"
KIoutcome = "Goal_F"
COLL07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
COLL07_Gg2 <- data.frame(COLL07_G)
COLL07_Gg2 <- COLL07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_Gg2$player1
player2vector <- COLL07_Gg2$player2
COLL07_Gg3 <- COLL07_Gg2
COLL07_Gg3$p1inp2vec <- is.element(COLL07_Gg3$player1, player2vector)
COLL07_Gg3$p2inp1vec <- is.element(COLL07_Gg3$player2, player1vector)

addPlayer1 <- COLL07_Gg3[ which(COLL07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_Gg3[ which(COLL07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_Gg2 <- rbind(COLL07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
COLL07_Gft <- ftable(COLL07_Gg2$player1, COLL07_Gg2$player2)
COLL07_Gft2 <- as.matrix(COLL07_Gft)
numRows <- nrow(COLL07_Gft2)
numCols <- ncol(COLL07_Gft2)
COLL07_Gft3 <- COLL07_Gft2[c(2:numRows) , c(2:numCols)]
COLL07_GTable <- graph.adjacency(COLL07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(COLL07_GTable, vertex.label = V(COLL07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
COLL07_G.clusterCoef <- transitivity(COLL07_GTable, type="global") #cluster coefficient
COLL07_G.degreeCent <- centralization.degree(COLL07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_Gftn <- as.network.matrix(COLL07_Gft)
COLL07_G.netDensity <- network.density(COLL07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_G.entropy <- entropy(COLL07_Gft) #entropy

COLL07_G.netMx <- cbind(COLL07_G.netMx, COLL07_G.clusterCoef, COLL07_G.degreeCent$centralization,
                        COLL07_G.netDensity, COLL07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Behind_F"
COLL07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
COLL07_Bg2 <- data.frame(COLL07_B)
COLL07_Bg2 <- COLL07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_Bg2$player1
player2vector <- COLL07_Bg2$player2
COLL07_Bg3 <- COLL07_Bg2
COLL07_Bg3$p1inp2vec <- is.element(COLL07_Bg3$player1, player2vector)
COLL07_Bg3$p2inp1vec <- is.element(COLL07_Bg3$player2, player1vector)

addPlayer1 <- COLL07_Bg3[ which(COLL07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_Bg3[ which(COLL07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_Bg2 <- rbind(COLL07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
COLL07_Bft <- ftable(COLL07_Bg2$player1, COLL07_Bg2$player2)
COLL07_Bft2 <- as.matrix(COLL07_Bft)
numRows <- nrow(COLL07_Bft2)
numCols <- ncol(COLL07_Bft2)
COLL07_Bft3 <- COLL07_Bft2[c(2:numRows) , c(2:numCols)]
COLL07_BTable <- graph.adjacency(COLL07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(COLL07_BTable, vertex.label = V(COLL07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
COLL07_B.clusterCoef <- transitivity(COLL07_BTable, type="global") #cluster coefficient
COLL07_B.degreeCent <- centralization.degree(COLL07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_Bftn <- as.network.matrix(COLL07_Bft)
COLL07_B.netDensity <- network.density(COLL07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_B.entropy <- entropy(COLL07_Bft) #entropy

COLL07_B.netMx <- cbind(COLL07_B.netMx, COLL07_B.clusterCoef, COLL07_B.degreeCent$centralization,
                        COLL07_B.netDensity, COLL07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
COLL07_SFg2 <- data.frame(COLL07_SF)
COLL07_SFg2 <- COLL07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_SFg2$player1
player2vector <- COLL07_SFg2$player2
COLL07_SFg3 <- COLL07_SFg2
COLL07_SFg3$p1inp2vec <- is.element(COLL07_SFg3$player1, player2vector)
COLL07_SFg3$p2inp1vec <- is.element(COLL07_SFg3$player2, player1vector)

addPlayer1 <- COLL07_SFg3[ which(COLL07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_SFg3[ which(COLL07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_SFg2 <- rbind(COLL07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
COLL07_SFft <- ftable(COLL07_SFg2$player1, COLL07_SFg2$player2)
COLL07_SFft2 <- as.matrix(COLL07_SFft)
numRows <- nrow(COLL07_SFft2)
numCols <- ncol(COLL07_SFft2)
COLL07_SFft3 <- COLL07_SFft2[c(2:numRows) , c(2:numCols)]
COLL07_SFTable <- graph.adjacency(COLL07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(COLL07_SFTable, vertex.label = V(COLL07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
COLL07_SF.clusterCoef <- transitivity(COLL07_SFTable, type="global") #cluster coefficient
COLL07_SF.degreeCent <- centralization.degree(COLL07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_SFftn <- as.network.matrix(COLL07_SFft)
COLL07_SF.netDensity <- network.density(COLL07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_SF.entropy <- entropy(COLL07_SFft) #entropy

COLL07_SF.netMx <- cbind(COLL07_SF.netMx, COLL07_SF.clusterCoef, COLL07_SF.degreeCent$centralization,
                         COLL07_SF.netDensity, COLL07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
COLL07_TFg2 <- data.frame(COLL07_TF)
COLL07_TFg2 <- COLL07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_TFg2$player1
player2vector <- COLL07_TFg2$player2
COLL07_TFg3 <- COLL07_TFg2
COLL07_TFg3$p1inp2vec <- is.element(COLL07_TFg3$player1, player2vector)
COLL07_TFg3$p2inp1vec <- is.element(COLL07_TFg3$player2, player1vector)

addPlayer1 <- COLL07_TFg3[ which(COLL07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_TFg3[ which(COLL07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_TFg2 <- rbind(COLL07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
COLL07_TFft <- ftable(COLL07_TFg2$player1, COLL07_TFg2$player2)
COLL07_TFft2 <- as.matrix(COLL07_TFft)
numRows <- nrow(COLL07_TFft2)
numCols <- ncol(COLL07_TFft2)
COLL07_TFft3 <- COLL07_TFft2[c(2:numRows) , c(2:numCols)]
COLL07_TFTable <- graph.adjacency(COLL07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(COLL07_TFTable, vertex.label = V(COLL07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
COLL07_TF.clusterCoef <- transitivity(COLL07_TFTable, type="global") #cluster coefficient
COLL07_TF.degreeCent <- centralization.degree(COLL07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_TFftn <- as.network.matrix(COLL07_TFft)
COLL07_TF.netDensity <- network.density(COLL07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_TF.entropy <- entropy(COLL07_TFft) #entropy

COLL07_TF.netMx <- cbind(COLL07_TF.netMx, COLL07_TF.clusterCoef, COLL07_TF.degreeCent$centralization,
                         COLL07_TF.netDensity, COLL07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
COLL07_SAMg2 <- data.frame(COLL07_SAM)
COLL07_SAMg2 <- COLL07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_SAMg2$player1
player2vector <- COLL07_SAMg2$player2
COLL07_SAMg3 <- COLL07_SAMg2
COLL07_SAMg3$p1inp2vec <- is.element(COLL07_SAMg3$player1, player2vector)
COLL07_SAMg3$p2inp1vec <- is.element(COLL07_SAMg3$player2, player1vector)

addPlayer1 <- COLL07_SAMg3[ which(COLL07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_SAMg3[ which(COLL07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_SAMg2 <- rbind(COLL07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
COLL07_SAMft <- ftable(COLL07_SAMg2$player1, COLL07_SAMg2$player2)
COLL07_SAMft2 <- as.matrix(COLL07_SAMft)
numRows <- nrow(COLL07_SAMft2)
numCols <- ncol(COLL07_SAMft2)
COLL07_SAMft3 <- COLL07_SAMft2[c(2:numRows) , c(2:numCols)]
COLL07_SAMTable <- graph.adjacency(COLL07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(COLL07_SAMTable, vertex.label = V(COLL07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
COLL07_SAM.clusterCoef <- transitivity(COLL07_SAMTable, type="global") #cluster coefficient
COLL07_SAM.degreeCent <- centralization.degree(COLL07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_SAMftn <- as.network.matrix(COLL07_SAMft)
COLL07_SAM.netDensity <- network.density(COLL07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_SAM.entropy <- entropy(COLL07_SAMft) #entropy

COLL07_SAM.netMx <- cbind(COLL07_SAM.netMx, COLL07_SAM.clusterCoef, COLL07_SAM.degreeCent$centralization,
                          COLL07_SAM.netDensity, COLL07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
COLL07_TAMg2 <- data.frame(COLL07_TAM)
COLL07_TAMg2 <- COLL07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_TAMg2$player1
player2vector <- COLL07_TAMg2$player2
COLL07_TAMg3 <- COLL07_TAMg2
COLL07_TAMg3$p1inp2vec <- is.element(COLL07_TAMg3$player1, player2vector)
COLL07_TAMg3$p2inp1vec <- is.element(COLL07_TAMg3$player2, player1vector)

addPlayer1 <- COLL07_TAMg3[ which(COLL07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_TAMg3[ which(COLL07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_TAMg2 <- rbind(COLL07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
COLL07_TAMft <- ftable(COLL07_TAMg2$player1, COLL07_TAMg2$player2)
COLL07_TAMft2 <- as.matrix(COLL07_TAMft)
numRows <- nrow(COLL07_TAMft2)
numCols <- ncol(COLL07_TAMft2)
COLL07_TAMft3 <- COLL07_TAMft2[c(2:numRows) , c(2:numCols)]
COLL07_TAMTable <- graph.adjacency(COLL07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(COLL07_TAMTable, vertex.label = V(COLL07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
COLL07_TAM.clusterCoef <- transitivity(COLL07_TAMTable, type="global") #cluster coefficient
COLL07_TAM.degreeCent <- centralization.degree(COLL07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_TAMftn <- as.network.matrix(COLL07_TAMft)
COLL07_TAM.netDensity <- network.density(COLL07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_TAM.entropy <- entropy(COLL07_TAMft) #entropy

COLL07_TAM.netMx <- cbind(COLL07_TAM.netMx, COLL07_TAM.clusterCoef, COLL07_TAM.degreeCent$centralization,
                          COLL07_TAM.netDensity, COLL07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
COLL07_SDMg2 <- data.frame(COLL07_SDM)
COLL07_SDMg2 <- COLL07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_SDMg2$player1
player2vector <- COLL07_SDMg2$player2
COLL07_SDMg3 <- COLL07_SDMg2
COLL07_SDMg3$p1inp2vec <- is.element(COLL07_SDMg3$player1, player2vector)
COLL07_SDMg3$p2inp1vec <- is.element(COLL07_SDMg3$player2, player1vector)

addPlayer1 <- COLL07_SDMg3[ which(COLL07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_SDMg3[ which(COLL07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_SDMg2 <- rbind(COLL07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
COLL07_SDMft <- ftable(COLL07_SDMg2$player1, COLL07_SDMg2$player2)
COLL07_SDMft2 <- as.matrix(COLL07_SDMft)
numRows <- nrow(COLL07_SDMft2)
numCols <- ncol(COLL07_SDMft2)
COLL07_SDMft3 <- COLL07_SDMft2[c(2:numRows) , c(2:numCols)]
COLL07_SDMTable <- graph.adjacency(COLL07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(COLL07_SDMTable, vertex.label = V(COLL07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
COLL07_SDM.clusterCoef <- transitivity(COLL07_SDMTable, type="global") #cluster coefficient
COLL07_SDM.degreeCent <- centralization.degree(COLL07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_SDMftn <- as.network.matrix(COLL07_SDMft)
COLL07_SDM.netDensity <- network.density(COLL07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_SDM.entropy <- entropy(COLL07_SDMft) #entropy

COLL07_SDM.netMx <- cbind(COLL07_SDM.netMx, COLL07_SDM.clusterCoef, COLL07_SDM.degreeCent$centralization,
                          COLL07_SDM.netDensity, COLL07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
COLL07_TDMg2 <- data.frame(COLL07_TDM)
COLL07_TDMg2 <- COLL07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_TDMg2$player1
player2vector <- COLL07_TDMg2$player2
COLL07_TDMg3 <- COLL07_TDMg2
COLL07_TDMg3$p1inp2vec <- is.element(COLL07_TDMg3$player1, player2vector)
COLL07_TDMg3$p2inp1vec <- is.element(COLL07_TDMg3$player2, player1vector)

addPlayer1 <- COLL07_TDMg3[ which(COLL07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_TDMg3[ which(COLL07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_TDMg2 <- rbind(COLL07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
COLL07_TDMft <- ftable(COLL07_TDMg2$player1, COLL07_TDMg2$player2)
COLL07_TDMft2 <- as.matrix(COLL07_TDMft)
numRows <- nrow(COLL07_TDMft2)
numCols <- ncol(COLL07_TDMft2)
COLL07_TDMft3 <- COLL07_TDMft2[c(2:numRows) , c(2:numCols)]
COLL07_TDMTable <- graph.adjacency(COLL07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(COLL07_TDMTable, vertex.label = V(COLL07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
COLL07_TDM.clusterCoef <- transitivity(COLL07_TDMTable, type="global") #cluster coefficient
COLL07_TDM.degreeCent <- centralization.degree(COLL07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_TDMftn <- as.network.matrix(COLL07_TDMft)
COLL07_TDM.netDensity <- network.density(COLL07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_TDM.entropy <- entropy(COLL07_TDMft) #entropy

COLL07_TDM.netMx <- cbind(COLL07_TDM.netMx, COLL07_TDM.clusterCoef, COLL07_TDM.degreeCent$centralization,
                          COLL07_TDM.netDensity, COLL07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
COLL07_SDg2 <- data.frame(COLL07_SD)
COLL07_SDg2 <- COLL07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_SDg2$player1
player2vector <- COLL07_SDg2$player2
COLL07_SDg3 <- COLL07_SDg2
COLL07_SDg3$p1inp2vec <- is.element(COLL07_SDg3$player1, player2vector)
COLL07_SDg3$p2inp1vec <- is.element(COLL07_SDg3$player2, player1vector)

addPlayer1 <- COLL07_SDg3[ which(COLL07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_SDg3[ which(COLL07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_SDg2 <- rbind(COLL07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
COLL07_SDft <- ftable(COLL07_SDg2$player1, COLL07_SDg2$player2)
COLL07_SDft2 <- as.matrix(COLL07_SDft)
numRows <- nrow(COLL07_SDft2)
numCols <- ncol(COLL07_SDft2)
COLL07_SDft3 <- COLL07_SDft2[c(2:numRows) , c(2:numCols)]
COLL07_SDTable <- graph.adjacency(COLL07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(COLL07_SDTable, vertex.label = V(COLL07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
COLL07_SD.clusterCoef <- transitivity(COLL07_SDTable, type="global") #cluster coefficient
COLL07_SD.degreeCent <- centralization.degree(COLL07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_SDftn <- as.network.matrix(COLL07_SDft)
COLL07_SD.netDensity <- network.density(COLL07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_SD.entropy <- entropy(COLL07_SDft) #entropy

COLL07_SD.netMx <- cbind(COLL07_SD.netMx, COLL07_SD.clusterCoef, COLL07_SD.degreeCent$centralization,
                         COLL07_SD.netDensity, COLL07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
COLL07_TDg2 <- data.frame(COLL07_TD)
COLL07_TDg2 <- COLL07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_TDg2$player1
player2vector <- COLL07_TDg2$player2
COLL07_TDg3 <- COLL07_TDg2
COLL07_TDg3$p1inp2vec <- is.element(COLL07_TDg3$player1, player2vector)
COLL07_TDg3$p2inp1vec <- is.element(COLL07_TDg3$player2, player1vector)

addPlayer1 <- COLL07_TDg3[ which(COLL07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_TDg3[ which(COLL07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_TDg2 <- rbind(COLL07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
COLL07_TDft <- ftable(COLL07_TDg2$player1, COLL07_TDg2$player2)
COLL07_TDft2 <- as.matrix(COLL07_TDft)
numRows <- nrow(COLL07_TDft2)
numCols <- ncol(COLL07_TDft2)
COLL07_TDft3 <- COLL07_TDft2[c(2:numRows) , c(2:numCols)]
COLL07_TDTable <- graph.adjacency(COLL07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(COLL07_TDTable, vertex.label = V(COLL07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
COLL07_TD.clusterCoef <- transitivity(COLL07_TDTable, type="global") #cluster coefficient
COLL07_TD.degreeCent <- centralization.degree(COLL07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_TDftn <- as.network.matrix(COLL07_TDft)
COLL07_TD.netDensity <- network.density(COLL07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_TD.entropy <- entropy(COLL07_TDft) #entropy

COLL07_TD.netMx <- cbind(COLL07_TD.netMx, COLL07_TD.clusterCoef, COLL07_TD.degreeCent$centralization,
                         COLL07_TD.netDensity, COLL07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
COLL07_QTg2 <- data.frame(COLL07_QT)
COLL07_QTg2 <- COLL07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL07_QTg2$player1
player2vector <- COLL07_QTg2$player2
COLL07_QTg3 <- COLL07_QTg2
COLL07_QTg3$p1inp2vec <- is.element(COLL07_QTg3$player1, player2vector)
COLL07_QTg3$p2inp1vec <- is.element(COLL07_QTg3$player2, player1vector)

addPlayer1 <- COLL07_QTg3[ which(COLL07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL07_QTg3[ which(COLL07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL07_QTg2 <- rbind(COLL07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
COLL07_QTft <- ftable(COLL07_QTg2$player1, COLL07_QTg2$player2)
COLL07_QTft2 <- as.matrix(COLL07_QTft)
numRows <- nrow(COLL07_QTft2)
numCols <- ncol(COLL07_QTft2)
COLL07_QTft3 <- COLL07_QTft2[c(2:numRows) , c(2:numCols)]
COLL07_QTTable <- graph.adjacency(COLL07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(COLL07_QTTable, vertex.label = V(COLL07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
COLL07_QT.clusterCoef <- transitivity(COLL07_QTTable, type="global") #cluster coefficient
COLL07_QT.degreeCent <- centralization.degree(COLL07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL07_QTftn <- as.network.matrix(COLL07_QTft)
COLL07_QT.netDensity <- network.density(COLL07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL07_QT.entropy <- entropy(COLL07_QTft) #entropy

COLL07_QT.netMx <- cbind(COLL07_QT.netMx, COLL07_QT.clusterCoef, COLL07_QT.degreeCent$centralization,
                         COLL07_QT.netDensity, COLL07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL07_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "ESS"
KIoutcome = "Goal_F"
ESS07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
ESS07_Gg2 <- data.frame(ESS07_G)
ESS07_Gg2 <- ESS07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_Gg2$player1
player2vector <- ESS07_Gg2$player2
ESS07_Gg3 <- ESS07_Gg2
ESS07_Gg3$p1inp2vec <- is.element(ESS07_Gg3$player1, player2vector)
ESS07_Gg3$p2inp1vec <- is.element(ESS07_Gg3$player2, player1vector)

addPlayer1 <- ESS07_Gg3[ which(ESS07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_Gg3[ which(ESS07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_Gg2 <- rbind(ESS07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
ESS07_Gft <- ftable(ESS07_Gg2$player1, ESS07_Gg2$player2)
ESS07_Gft2 <- as.matrix(ESS07_Gft)
numRows <- nrow(ESS07_Gft2)
numCols <- ncol(ESS07_Gft2)
ESS07_Gft3 <- ESS07_Gft2[c(2:numRows) , c(2:numCols)]
ESS07_GTable <- graph.adjacency(ESS07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(ESS07_GTable, vertex.label = V(ESS07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
ESS07_G.clusterCoef <- transitivity(ESS07_GTable, type="global") #cluster coefficient
ESS07_G.degreeCent <- centralization.degree(ESS07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_Gftn <- as.network.matrix(ESS07_Gft)
ESS07_G.netDensity <- network.density(ESS07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_G.entropy <- entropy(ESS07_Gft) #entropy

ESS07_G.netMx <- cbind(ESS07_G.netMx, ESS07_G.clusterCoef, ESS07_G.degreeCent$centralization,
                       ESS07_G.netDensity, ESS07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "ESS"
KIoutcome = "Behind_F"
ESS07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
ESS07_Bg2 <- data.frame(ESS07_B)
ESS07_Bg2 <- ESS07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_Bg2$player1
player2vector <- ESS07_Bg2$player2
ESS07_Bg3 <- ESS07_Bg2
ESS07_Bg3$p1inp2vec <- is.element(ESS07_Bg3$player1, player2vector)
ESS07_Bg3$p2inp1vec <- is.element(ESS07_Bg3$player2, player1vector)

addPlayer1 <- ESS07_Bg3[ which(ESS07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_Bg3[ which(ESS07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_Bg2 <- rbind(ESS07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
ESS07_Bft <- ftable(ESS07_Bg2$player1, ESS07_Bg2$player2)
ESS07_Bft2 <- as.matrix(ESS07_Bft)
numRows <- nrow(ESS07_Bft2)
numCols <- ncol(ESS07_Bft2)
ESS07_Bft3 <- ESS07_Bft2[c(2:numRows) , c(2:numCols)]
ESS07_BTable <- graph.adjacency(ESS07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(ESS07_BTable, vertex.label = V(ESS07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
ESS07_B.clusterCoef <- transitivity(ESS07_BTable, type="global") #cluster coefficient
ESS07_B.degreeCent <- centralization.degree(ESS07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_Bftn <- as.network.matrix(ESS07_Bft)
ESS07_B.netDensity <- network.density(ESS07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_B.entropy <- entropy(ESS07_Bft) #entropy

ESS07_B.netMx <- cbind(ESS07_B.netMx, ESS07_B.clusterCoef, ESS07_B.degreeCent$centralization,
                       ESS07_B.netDensity, ESS07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
ESS07_SFg2 <- data.frame(ESS07_SF)
ESS07_SFg2 <- ESS07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_SFg2$player1
player2vector <- ESS07_SFg2$player2
ESS07_SFg3 <- ESS07_SFg2
ESS07_SFg3$p1inp2vec <- is.element(ESS07_SFg3$player1, player2vector)
ESS07_SFg3$p2inp1vec <- is.element(ESS07_SFg3$player2, player1vector)

addPlayer1 <- ESS07_SFg3[ which(ESS07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_SFg3[ which(ESS07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_SFg2 <- rbind(ESS07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
ESS07_SFft <- ftable(ESS07_SFg2$player1, ESS07_SFg2$player2)
ESS07_SFft2 <- as.matrix(ESS07_SFft)
numRows <- nrow(ESS07_SFft2)
numCols <- ncol(ESS07_SFft2)
ESS07_SFft3 <- ESS07_SFft2[c(2:numRows) , c(2:numCols)]
ESS07_SFTable <- graph.adjacency(ESS07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(ESS07_SFTable, vertex.label = V(ESS07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
ESS07_SF.clusterCoef <- transitivity(ESS07_SFTable, type="global") #cluster coefficient
ESS07_SF.degreeCent <- centralization.degree(ESS07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_SFftn <- as.network.matrix(ESS07_SFft)
ESS07_SF.netDensity <- network.density(ESS07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_SF.entropy <- entropy(ESS07_SFft) #entropy

ESS07_SF.netMx <- cbind(ESS07_SF.netMx, ESS07_SF.clusterCoef, ESS07_SF.degreeCent$centralization,
                        ESS07_SF.netDensity, ESS07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
ESS07_TFg2 <- data.frame(ESS07_TF)
ESS07_TFg2 <- ESS07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_TFg2$player1
player2vector <- ESS07_TFg2$player2
ESS07_TFg3 <- ESS07_TFg2
ESS07_TFg3$p1inp2vec <- is.element(ESS07_TFg3$player1, player2vector)
ESS07_TFg3$p2inp1vec <- is.element(ESS07_TFg3$player2, player1vector)

addPlayer1 <- ESS07_TFg3[ which(ESS07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_TFg3[ which(ESS07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_TFg2 <- rbind(ESS07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
ESS07_TFft <- ftable(ESS07_TFg2$player1, ESS07_TFg2$player2)
ESS07_TFft2 <- as.matrix(ESS07_TFft)
numRows <- nrow(ESS07_TFft2)
numCols <- ncol(ESS07_TFft2)
ESS07_TFft3 <- ESS07_TFft2[c(2:numRows) , c(2:numCols)]
ESS07_TFTable <- graph.adjacency(ESS07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(ESS07_TFTable, vertex.label = V(ESS07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
ESS07_TF.clusterCoef <- transitivity(ESS07_TFTable, type="global") #cluster coefficient
ESS07_TF.degreeCent <- centralization.degree(ESS07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_TFftn <- as.network.matrix(ESS07_TFft)
ESS07_TF.netDensity <- network.density(ESS07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_TF.entropy <- entropy(ESS07_TFft) #entropy

ESS07_TF.netMx <- cbind(ESS07_TF.netMx, ESS07_TF.clusterCoef, ESS07_TF.degreeCent$centralization,
                        ESS07_TF.netDensity, ESS07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
ESS07_SAMg2 <- data.frame(ESS07_SAM)
ESS07_SAMg2 <- ESS07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_SAMg2$player1
player2vector <- ESS07_SAMg2$player2
ESS07_SAMg3 <- ESS07_SAMg2
ESS07_SAMg3$p1inp2vec <- is.element(ESS07_SAMg3$player1, player2vector)
ESS07_SAMg3$p2inp1vec <- is.element(ESS07_SAMg3$player2, player1vector)

addPlayer1 <- ESS07_SAMg3[ which(ESS07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_SAMg3[ which(ESS07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_SAMg2 <- rbind(ESS07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
ESS07_SAMft <- ftable(ESS07_SAMg2$player1, ESS07_SAMg2$player2)
ESS07_SAMft2 <- as.matrix(ESS07_SAMft)
numRows <- nrow(ESS07_SAMft2)
numCols <- ncol(ESS07_SAMft2)
ESS07_SAMft3 <- ESS07_SAMft2[c(2:numRows) , c(2:numCols)]
ESS07_SAMTable <- graph.adjacency(ESS07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(ESS07_SAMTable, vertex.label = V(ESS07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
ESS07_SAM.clusterCoef <- transitivity(ESS07_SAMTable, type="global") #cluster coefficient
ESS07_SAM.degreeCent <- centralization.degree(ESS07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_SAMftn <- as.network.matrix(ESS07_SAMft)
ESS07_SAM.netDensity <- network.density(ESS07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_SAM.entropy <- entropy(ESS07_SAMft) #entropy

ESS07_SAM.netMx <- cbind(ESS07_SAM.netMx, ESS07_SAM.clusterCoef, ESS07_SAM.degreeCent$centralization,
                         ESS07_SAM.netDensity, ESS07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
ESS07_TAMg2 <- data.frame(ESS07_TAM)
ESS07_TAMg2 <- ESS07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_TAMg2$player1
player2vector <- ESS07_TAMg2$player2
ESS07_TAMg3 <- ESS07_TAMg2
ESS07_TAMg3$p1inp2vec <- is.element(ESS07_TAMg3$player1, player2vector)
ESS07_TAMg3$p2inp1vec <- is.element(ESS07_TAMg3$player2, player1vector)

addPlayer1 <- ESS07_TAMg3[ which(ESS07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_TAMg3[ which(ESS07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_TAMg2 <- rbind(ESS07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
ESS07_TAMft <- ftable(ESS07_TAMg2$player1, ESS07_TAMg2$player2)
ESS07_TAMft2 <- as.matrix(ESS07_TAMft)
numRows <- nrow(ESS07_TAMft2)
numCols <- ncol(ESS07_TAMft2)
ESS07_TAMft3 <- ESS07_TAMft2[c(2:numRows) , c(2:numCols)]
ESS07_TAMTable <- graph.adjacency(ESS07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(ESS07_TAMTable, vertex.label = V(ESS07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
ESS07_TAM.clusterCoef <- transitivity(ESS07_TAMTable, type="global") #cluster coefficient
ESS07_TAM.degreeCent <- centralization.degree(ESS07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_TAMftn <- as.network.matrix(ESS07_TAMft)
ESS07_TAM.netDensity <- network.density(ESS07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_TAM.entropy <- entropy(ESS07_TAMft) #entropy

ESS07_TAM.netMx <- cbind(ESS07_TAM.netMx, ESS07_TAM.clusterCoef, ESS07_TAM.degreeCent$centralization,
                         ESS07_TAM.netDensity, ESS07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
ESS07_SDMg2 <- data.frame(ESS07_SDM)
ESS07_SDMg2 <- ESS07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_SDMg2$player1
player2vector <- ESS07_SDMg2$player2
ESS07_SDMg3 <- ESS07_SDMg2
ESS07_SDMg3$p1inp2vec <- is.element(ESS07_SDMg3$player1, player2vector)
ESS07_SDMg3$p2inp1vec <- is.element(ESS07_SDMg3$player2, player1vector)

addPlayer1 <- ESS07_SDMg3[ which(ESS07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_SDMg3[ which(ESS07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_SDMg2 <- rbind(ESS07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
ESS07_SDMft <- ftable(ESS07_SDMg2$player1, ESS07_SDMg2$player2)
ESS07_SDMft2 <- as.matrix(ESS07_SDMft)
numRows <- nrow(ESS07_SDMft2)
numCols <- ncol(ESS07_SDMft2)
ESS07_SDMft3 <- ESS07_SDMft2[c(2:numRows) , c(2:numCols)]
ESS07_SDMTable <- graph.adjacency(ESS07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(ESS07_SDMTable, vertex.label = V(ESS07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
ESS07_SDM.clusterCoef <- transitivity(ESS07_SDMTable, type="global") #cluster coefficient
ESS07_SDM.degreeCent <- centralization.degree(ESS07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_SDMftn <- as.network.matrix(ESS07_SDMft)
ESS07_SDM.netDensity <- network.density(ESS07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_SDM.entropy <- entropy(ESS07_SDMft) #entropy

ESS07_SDM.netMx <- cbind(ESS07_SDM.netMx, ESS07_SDM.clusterCoef, ESS07_SDM.degreeCent$centralization,
                         ESS07_SDM.netDensity, ESS07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
ESS07_TDMg2 <- data.frame(ESS07_TDM)
ESS07_TDMg2 <- ESS07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_TDMg2$player1
player2vector <- ESS07_TDMg2$player2
ESS07_TDMg3 <- ESS07_TDMg2
ESS07_TDMg3$p1inp2vec <- is.element(ESS07_TDMg3$player1, player2vector)
ESS07_TDMg3$p2inp1vec <- is.element(ESS07_TDMg3$player2, player1vector)

addPlayer1 <- ESS07_TDMg3[ which(ESS07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS07_TDMg3[ which(ESS07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_TDMg2 <- rbind(ESS07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
ESS07_TDMft <- ftable(ESS07_TDMg2$player1, ESS07_TDMg2$player2)
ESS07_TDMft2 <- as.matrix(ESS07_TDMft)
numRows <- nrow(ESS07_TDMft2)
numCols <- ncol(ESS07_TDMft2)
ESS07_TDMft3 <- ESS07_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS07_TDMTable <- graph.adjacency(ESS07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(ESS07_TDMTable, vertex.label = V(ESS07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
ESS07_TDM.clusterCoef <- transitivity(ESS07_TDMTable, type="global") #cluster coefficient
ESS07_TDM.degreeCent <- centralization.degree(ESS07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_TDMftn <- as.network.matrix(ESS07_TDMft)
ESS07_TDM.netDensity <- network.density(ESS07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_TDM.entropy <- entropy(ESS07_TDMft) #entropy

ESS07_TDM.netMx <- cbind(ESS07_TDM.netMx, ESS07_TDM.clusterCoef, ESS07_TDM.degreeCent$centralization,
                         ESS07_TDM.netDensity, ESS07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
ESS07_SDg2 <- data.frame(ESS07_SD)
ESS07_SDg2 <- ESS07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_SDg2$player1
player2vector <- ESS07_SDg2$player2
ESS07_SDg3 <- ESS07_SDg2
ESS07_SDg3$p1inp2vec <- is.element(ESS07_SDg3$player1, player2vector)
ESS07_SDg3$p2inp1vec <- is.element(ESS07_SDg3$player2, player1vector)

addPlayer1 <- ESS07_SDg3[ which(ESS07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_SDg3[ which(ESS07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_SDg2 <- rbind(ESS07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
ESS07_SDft <- ftable(ESS07_SDg2$player1, ESS07_SDg2$player2)
ESS07_SDft2 <- as.matrix(ESS07_SDft)
numRows <- nrow(ESS07_SDft2)
numCols <- ncol(ESS07_SDft2)
ESS07_SDft3 <- ESS07_SDft2[c(2:numRows) , c(2:numCols)]
ESS07_SDTable <- graph.adjacency(ESS07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(ESS07_SDTable, vertex.label = V(ESS07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
ESS07_SD.clusterCoef <- transitivity(ESS07_SDTable, type="global") #cluster coefficient
ESS07_SD.degreeCent <- centralization.degree(ESS07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_SDftn <- as.network.matrix(ESS07_SDft)
ESS07_SD.netDensity <- network.density(ESS07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_SD.entropy <- entropy(ESS07_SDft) #entropy

ESS07_SD.netMx <- cbind(ESS07_SD.netMx, ESS07_SD.clusterCoef, ESS07_SD.degreeCent$centralization,
                        ESS07_SD.netDensity, ESS07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
ESS07_TDg2 <- data.frame(ESS07_TD)
ESS07_TDg2 <- ESS07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_TDg2$player1
player2vector <- ESS07_TDg2$player2
ESS07_TDg3 <- ESS07_TDg2
ESS07_TDg3$p1inp2vec <- is.element(ESS07_TDg3$player1, player2vector)
ESS07_TDg3$p2inp1vec <- is.element(ESS07_TDg3$player2, player1vector)

addPlayer1 <- ESS07_TDg3[ which(ESS07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_TDg3[ which(ESS07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_TDg2 <- rbind(ESS07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
ESS07_TDft <- ftable(ESS07_TDg2$player1, ESS07_TDg2$player2)
ESS07_TDft2 <- as.matrix(ESS07_TDft)
numRows <- nrow(ESS07_TDft2)
numCols <- ncol(ESS07_TDft2)
ESS07_TDft3 <- ESS07_TDft2[c(2:numRows) , c(2:numCols)]
ESS07_TDTable <- graph.adjacency(ESS07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(ESS07_TDTable, vertex.label = V(ESS07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
ESS07_TD.clusterCoef <- transitivity(ESS07_TDTable, type="global") #cluster coefficient
ESS07_TD.degreeCent <- centralization.degree(ESS07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_TDftn <- as.network.matrix(ESS07_TDft)
ESS07_TD.netDensity <- network.density(ESS07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_TD.entropy <- entropy(ESS07_TDft) #entropy

ESS07_TD.netMx <- cbind(ESS07_TD.netMx, ESS07_TD.clusterCoef, ESS07_TD.degreeCent$centralization,
                        ESS07_TD.netDensity, ESS07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
ESS07_QTg2 <- data.frame(ESS07_QT)
ESS07_QTg2 <- ESS07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS07_QTg2$player1
player2vector <- ESS07_QTg2$player2
ESS07_QTg3 <- ESS07_QTg2
ESS07_QTg3$p1inp2vec <- is.element(ESS07_QTg3$player1, player2vector)
ESS07_QTg3$p2inp1vec <- is.element(ESS07_QTg3$player2, player1vector)

addPlayer1 <- ESS07_QTg3[ which(ESS07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS07_QTg3[ which(ESS07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS07_QTg2 <- rbind(ESS07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
ESS07_QTft <- ftable(ESS07_QTg2$player1, ESS07_QTg2$player2)
ESS07_QTft2 <- as.matrix(ESS07_QTft)
numRows <- nrow(ESS07_QTft2)
numCols <- ncol(ESS07_QTft2)
ESS07_QTft3 <- ESS07_QTft2[c(2:numRows) , c(2:numCols)]
ESS07_QTTable <- graph.adjacency(ESS07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(ESS07_QTTable, vertex.label = V(ESS07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
ESS07_QT.clusterCoef <- transitivity(ESS07_QTTable, type="global") #cluster coefficient
ESS07_QT.degreeCent <- centralization.degree(ESS07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS07_QTftn <- as.network.matrix(ESS07_QTft)
ESS07_QT.netDensity <- network.density(ESS07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS07_QT.entropy <- entropy(ESS07_QTft) #entropy

ESS07_QT.netMx <- cbind(ESS07_QT.netMx, ESS07_QT.clusterCoef, ESS07_QT.degreeCent$centralization,
                        ESS07_QT.netDensity, ESS07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS07_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Goal_F"
FRE07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
FRE07_Gg2 <- data.frame(FRE07_G)
FRE07_Gg2 <- FRE07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_Gg2$player1
player2vector <- FRE07_Gg2$player2
FRE07_Gg3 <- FRE07_Gg2
FRE07_Gg3$p1inp2vec <- is.element(FRE07_Gg3$player1, player2vector)
FRE07_Gg3$p2inp1vec <- is.element(FRE07_Gg3$player2, player1vector)

addPlayer1 <- FRE07_Gg3[ which(FRE07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_Gg3[ which(FRE07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_Gg2 <- rbind(FRE07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
FRE07_Gft <- ftable(FRE07_Gg2$player1, FRE07_Gg2$player2)
FRE07_Gft2 <- as.matrix(FRE07_Gft)
numRows <- nrow(FRE07_Gft2)
numCols <- ncol(FRE07_Gft2)
FRE07_Gft3 <- FRE07_Gft2[c(2:numRows) , c(2:numCols)]
FRE07_GTable <- graph.adjacency(FRE07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(FRE07_GTable, vertex.label = V(FRE07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
FRE07_G.clusterCoef <- transitivity(FRE07_GTable, type="global") #cluster coefficient
FRE07_G.degreeCent <- centralization.degree(FRE07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_Gftn <- as.network.matrix(FRE07_Gft)
FRE07_G.netDensity <- network.density(FRE07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_G.entropy <- entropy(FRE07_Gft) #entropy

FRE07_G.netMx <- cbind(FRE07_G.netMx, FRE07_G.clusterCoef, FRE07_G.degreeCent$centralization,
                       FRE07_G.netDensity, FRE07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************

round = 7
teamName = "FRE"
KIoutcome = "Behind_F"
FRE07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
FRE07_Bg2 <- data.frame(FRE07_B)
FRE07_Bg2 <- FRE07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_Bg2$player1
player2vector <- FRE07_Bg2$player2
FRE07_Bg3 <- FRE07_Bg2
FRE07_Bg3$p1inp2vec <- is.element(FRE07_Bg3$player1, player2vector)
FRE07_Bg3$p2inp1vec <- is.element(FRE07_Bg3$player2, player1vector)

addPlayer1 <- FRE07_Bg3[ which(FRE07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_Bg3[ which(FRE07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_Bg2 <- rbind(FRE07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
FRE07_Bft <- ftable(FRE07_Bg2$player1, FRE07_Bg2$player2)
FRE07_Bft2 <- as.matrix(FRE07_Bft)
numRows <- nrow(FRE07_Bft2)
numCols <- ncol(FRE07_Bft2)
FRE07_Bft3 <- FRE07_Bft2[c(2:numRows) , c(2:numCols)]
FRE07_BTable <- graph.adjacency(FRE07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(FRE07_BTable, vertex.label = V(FRE07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
FRE07_B.clusterCoef <- transitivity(FRE07_BTable, type="global") #cluster coefficient
FRE07_B.degreeCent <- centralization.degree(FRE07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_Bftn <- as.network.matrix(FRE07_Bft)
FRE07_B.netDensity <- network.density(FRE07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_B.entropy <- entropy(FRE07_Bft) #entropy

FRE07_B.netMx <- cbind(FRE07_B.netMx, FRE07_B.clusterCoef, FRE07_B.degreeCent$centralization,
                       FRE07_B.netDensity, FRE07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
FRE07_SFg2 <- data.frame(FRE07_SF)
FRE07_SFg2 <- FRE07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_SFg2$player1
player2vector <- FRE07_SFg2$player2
FRE07_SFg3 <- FRE07_SFg2
FRE07_SFg3$p1inp2vec <- is.element(FRE07_SFg3$player1, player2vector)
FRE07_SFg3$p2inp1vec <- is.element(FRE07_SFg3$player2, player1vector)

addPlayer1 <- FRE07_SFg3[ which(FRE07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_SFg3[ which(FRE07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_SFg2 <- rbind(FRE07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
FRE07_SFft <- ftable(FRE07_SFg2$player1, FRE07_SFg2$player2)
FRE07_SFft2 <- as.matrix(FRE07_SFft)
numRows <- nrow(FRE07_SFft2)
numCols <- ncol(FRE07_SFft2)
FRE07_SFft3 <- FRE07_SFft2[c(2:numRows) , c(2:numCols)]
FRE07_SFTable <- graph.adjacency(FRE07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(FRE07_SFTable, vertex.label = V(FRE07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
FRE07_SF.clusterCoef <- transitivity(FRE07_SFTable, type="global") #cluster coefficient
FRE07_SF.degreeCent <- centralization.degree(FRE07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_SFftn <- as.network.matrix(FRE07_SFft)
FRE07_SF.netDensity <- network.density(FRE07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_SF.entropy <- entropy(FRE07_SFft) #entropy

FRE07_SF.netMx <- cbind(FRE07_SF.netMx, FRE07_SF.clusterCoef, FRE07_SF.degreeCent$centralization,
                        FRE07_SF.netDensity, FRE07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
FRE07_TFg2 <- data.frame(FRE07_TF)
FRE07_TFg2 <- FRE07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_TFg2$player1
player2vector <- FRE07_TFg2$player2
FRE07_TFg3 <- FRE07_TFg2
FRE07_TFg3$p1inp2vec <- is.element(FRE07_TFg3$player1, player2vector)
FRE07_TFg3$p2inp1vec <- is.element(FRE07_TFg3$player2, player1vector)

addPlayer1 <- FRE07_TFg3[ which(FRE07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_TFg3[ which(FRE07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_TFg2 <- rbind(FRE07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
FRE07_TFft <- ftable(FRE07_TFg2$player1, FRE07_TFg2$player2)
FRE07_TFft2 <- as.matrix(FRE07_TFft)
numRows <- nrow(FRE07_TFft2)
numCols <- ncol(FRE07_TFft2)
FRE07_TFft3 <- FRE07_TFft2[c(2:numRows) , c(2:numCols)]
FRE07_TFTable <- graph.adjacency(FRE07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(FRE07_TFTable, vertex.label = V(FRE07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
FRE07_TF.clusterCoef <- transitivity(FRE07_TFTable, type="global") #cluster coefficient
FRE07_TF.degreeCent <- centralization.degree(FRE07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_TFftn <- as.network.matrix(FRE07_TFft)
FRE07_TF.netDensity <- network.density(FRE07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_TF.entropy <- entropy(FRE07_TFft) #entropy

FRE07_TF.netMx <- cbind(FRE07_TF.netMx, FRE07_TF.clusterCoef, FRE07_TF.degreeCent$centralization,
                        FRE07_TF.netDensity, FRE07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
FRE07_SAMg2 <- data.frame(FRE07_SAM)
FRE07_SAMg2 <- FRE07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_SAMg2$player1
player2vector <- FRE07_SAMg2$player2
FRE07_SAMg3 <- FRE07_SAMg2
FRE07_SAMg3$p1inp2vec <- is.element(FRE07_SAMg3$player1, player2vector)
FRE07_SAMg3$p2inp1vec <- is.element(FRE07_SAMg3$player2, player1vector)

addPlayer1 <- FRE07_SAMg3[ which(FRE07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_SAMg3[ which(FRE07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_SAMg2 <- rbind(FRE07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
FRE07_SAMft <- ftable(FRE07_SAMg2$player1, FRE07_SAMg2$player2)
FRE07_SAMft2 <- as.matrix(FRE07_SAMft)
numRows <- nrow(FRE07_SAMft2)
numCols <- ncol(FRE07_SAMft2)
FRE07_SAMft3 <- FRE07_SAMft2[c(2:numRows) , c(2:numCols)]
FRE07_SAMTable <- graph.adjacency(FRE07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(FRE07_SAMTable, vertex.label = V(FRE07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
FRE07_SAM.clusterCoef <- transitivity(FRE07_SAMTable, type="global") #cluster coefficient
FRE07_SAM.degreeCent <- centralization.degree(FRE07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_SAMftn <- as.network.matrix(FRE07_SAMft)
FRE07_SAM.netDensity <- network.density(FRE07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_SAM.entropy <- entropy(FRE07_SAMft) #entropy

FRE07_SAM.netMx <- cbind(FRE07_SAM.netMx, FRE07_SAM.clusterCoef, FRE07_SAM.degreeCent$centralization,
                         FRE07_SAM.netDensity, FRE07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
FRE07_TAMg2 <- data.frame(FRE07_TAM)
FRE07_TAMg2 <- FRE07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_TAMg2$player1
player2vector <- FRE07_TAMg2$player2
FRE07_TAMg3 <- FRE07_TAMg2
FRE07_TAMg3$p1inp2vec <- is.element(FRE07_TAMg3$player1, player2vector)
FRE07_TAMg3$p2inp1vec <- is.element(FRE07_TAMg3$player2, player1vector)

addPlayer1 <- FRE07_TAMg3[ which(FRE07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_TAMg3[ which(FRE07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_TAMg2 <- rbind(FRE07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
FRE07_TAMft <- ftable(FRE07_TAMg2$player1, FRE07_TAMg2$player2)
FRE07_TAMft2 <- as.matrix(FRE07_TAMft)
numRows <- nrow(FRE07_TAMft2)
numCols <- ncol(FRE07_TAMft2)
FRE07_TAMft3 <- FRE07_TAMft2[c(2:numRows) , c(2:numCols)]
FRE07_TAMTable <- graph.adjacency(FRE07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(FRE07_TAMTable, vertex.label = V(FRE07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
FRE07_TAM.clusterCoef <- transitivity(FRE07_TAMTable, type="global") #cluster coefficient
FRE07_TAM.degreeCent <- centralization.degree(FRE07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_TAMftn <- as.network.matrix(FRE07_TAMft)
FRE07_TAM.netDensity <- network.density(FRE07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_TAM.entropy <- entropy(FRE07_TAMft) #entropy

FRE07_TAM.netMx <- cbind(FRE07_TAM.netMx, FRE07_TAM.clusterCoef, FRE07_TAM.degreeCent$centralization,
                         FRE07_TAM.netDensity, FRE07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
FRE07_SDMg2 <- data.frame(FRE07_SDM)
FRE07_SDMg2 <- FRE07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_SDMg2$player1
player2vector <- FRE07_SDMg2$player2
FRE07_SDMg3 <- FRE07_SDMg2
FRE07_SDMg3$p1inp2vec <- is.element(FRE07_SDMg3$player1, player2vector)
FRE07_SDMg3$p2inp1vec <- is.element(FRE07_SDMg3$player2, player1vector)

addPlayer1 <- FRE07_SDMg3[ which(FRE07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_SDMg3[ which(FRE07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_SDMg2 <- rbind(FRE07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
FRE07_SDMft <- ftable(FRE07_SDMg2$player1, FRE07_SDMg2$player2)
FRE07_SDMft2 <- as.matrix(FRE07_SDMft)
numRows <- nrow(FRE07_SDMft2)
numCols <- ncol(FRE07_SDMft2)
FRE07_SDMft3 <- FRE07_SDMft2[c(2:numRows) , c(2:numCols)]
FRE07_SDMTable <- graph.adjacency(FRE07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(FRE07_SDMTable, vertex.label = V(FRE07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
FRE07_SDM.clusterCoef <- transitivity(FRE07_SDMTable, type="global") #cluster coefficient
FRE07_SDM.degreeCent <- centralization.degree(FRE07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_SDMftn <- as.network.matrix(FRE07_SDMft)
FRE07_SDM.netDensity <- network.density(FRE07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_SDM.entropy <- entropy(FRE07_SDMft) #entropy

FRE07_SDM.netMx <- cbind(FRE07_SDM.netMx, FRE07_SDM.clusterCoef, FRE07_SDM.degreeCent$centralization,
                         FRE07_SDM.netDensity, FRE07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
FRE07_TDMg2 <- data.frame(FRE07_TDM)
FRE07_TDMg2 <- FRE07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_TDMg2$player1
player2vector <- FRE07_TDMg2$player2
FRE07_TDMg3 <- FRE07_TDMg2
FRE07_TDMg3$p1inp2vec <- is.element(FRE07_TDMg3$player1, player2vector)
FRE07_TDMg3$p2inp1vec <- is.element(FRE07_TDMg3$player2, player1vector)

addPlayer1 <- FRE07_TDMg3[ which(FRE07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE07_TDMg3[ which(FRE07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_TDMg2 <- rbind(FRE07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
FRE07_TDMft <- ftable(FRE07_TDMg2$player1, FRE07_TDMg2$player2)
FRE07_TDMft2 <- as.matrix(FRE07_TDMft)
numRows <- nrow(FRE07_TDMft2)
numCols <- ncol(FRE07_TDMft2)
FRE07_TDMft3 <- FRE07_TDMft2[c(2:numRows) , c(2:numCols)]
FRE07_TDMTable <- graph.adjacency(FRE07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(FRE07_TDMTable, vertex.label = V(FRE07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
FRE07_TDM.clusterCoef <- transitivity(FRE07_TDMTable, type="global") #cluster coefficient
FRE07_TDM.degreeCent <- centralization.degree(FRE07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_TDMftn <- as.network.matrix(FRE07_TDMft)
FRE07_TDM.netDensity <- network.density(FRE07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_TDM.entropy <- entropy(FRE07_TDMft) #entropy

FRE07_TDM.netMx <- cbind(FRE07_TDM.netMx, FRE07_TDM.clusterCoef, FRE07_TDM.degreeCent$centralization,
                         FRE07_TDM.netDensity, FRE07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
FRE07_SDg2 <- data.frame(FRE07_SD)
FRE07_SDg2 <- FRE07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_SDg2$player1
player2vector <- FRE07_SDg2$player2
FRE07_SDg3 <- FRE07_SDg2
FRE07_SDg3$p1inp2vec <- is.element(FRE07_SDg3$player1, player2vector)
FRE07_SDg3$p2inp1vec <- is.element(FRE07_SDg3$player2, player1vector)

addPlayer1 <- FRE07_SDg3[ which(FRE07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_SDg3[ which(FRE07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_SDg2 <- rbind(FRE07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
FRE07_SDft <- ftable(FRE07_SDg2$player1, FRE07_SDg2$player2)
FRE07_SDft2 <- as.matrix(FRE07_SDft)
numRows <- nrow(FRE07_SDft2)
numCols <- ncol(FRE07_SDft2)
FRE07_SDft3 <- FRE07_SDft2[c(2:numRows) , c(2:numCols)]
FRE07_SDTable <- graph.adjacency(FRE07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(FRE07_SDTable, vertex.label = V(FRE07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
FRE07_SD.clusterCoef <- transitivity(FRE07_SDTable, type="global") #cluster coefficient
FRE07_SD.degreeCent <- centralization.degree(FRE07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_SDftn <- as.network.matrix(FRE07_SDft)
FRE07_SD.netDensity <- network.density(FRE07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_SD.entropy <- entropy(FRE07_SDft) #entropy

FRE07_SD.netMx <- cbind(FRE07_SD.netMx, FRE07_SD.clusterCoef, FRE07_SD.degreeCent$centralization,
                        FRE07_SD.netDensity, FRE07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
FRE07_TDg2 <- data.frame(FRE07_TD)
FRE07_TDg2 <- FRE07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_TDg2$player1
player2vector <- FRE07_TDg2$player2
FRE07_TDg3 <- FRE07_TDg2
FRE07_TDg3$p1inp2vec <- is.element(FRE07_TDg3$player1, player2vector)
FRE07_TDg3$p2inp1vec <- is.element(FRE07_TDg3$player2, player1vector)

addPlayer1 <- FRE07_TDg3[ which(FRE07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_TDg3[ which(FRE07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_TDg2 <- rbind(FRE07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
FRE07_TDft <- ftable(FRE07_TDg2$player1, FRE07_TDg2$player2)
FRE07_TDft2 <- as.matrix(FRE07_TDft)
numRows <- nrow(FRE07_TDft2)
numCols <- ncol(FRE07_TDft2)
FRE07_TDft3 <- FRE07_TDft2[c(2:numRows) , c(2:numCols)]
FRE07_TDTable <- graph.adjacency(FRE07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(FRE07_TDTable, vertex.label = V(FRE07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
FRE07_TD.clusterCoef <- transitivity(FRE07_TDTable, type="global") #cluster coefficient
FRE07_TD.degreeCent <- centralization.degree(FRE07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_TDftn <- as.network.matrix(FRE07_TDft)
FRE07_TD.netDensity <- network.density(FRE07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_TD.entropy <- entropy(FRE07_TDft) #entropy

FRE07_TD.netMx <- cbind(FRE07_TD.netMx, FRE07_TD.clusterCoef, FRE07_TD.degreeCent$centralization,
                        FRE07_TD.netDensity, FRE07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
FRE07_QTg2 <- data.frame(FRE07_QT)
FRE07_QTg2 <- FRE07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE07_QTg2$player1
player2vector <- FRE07_QTg2$player2
FRE07_QTg3 <- FRE07_QTg2
FRE07_QTg3$p1inp2vec <- is.element(FRE07_QTg3$player1, player2vector)
FRE07_QTg3$p2inp1vec <- is.element(FRE07_QTg3$player2, player1vector)

addPlayer1 <- FRE07_QTg3[ which(FRE07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE07_QTg3[ which(FRE07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE07_QTg2 <- rbind(FRE07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
FRE07_QTft <- ftable(FRE07_QTg2$player1, FRE07_QTg2$player2)
FRE07_QTft2 <- as.matrix(FRE07_QTft)
numRows <- nrow(FRE07_QTft2)
numCols <- ncol(FRE07_QTft2)
FRE07_QTft3 <- FRE07_QTft2[c(2:numRows) , c(2:numCols)]
FRE07_QTTable <- graph.adjacency(FRE07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(FRE07_QTTable, vertex.label = V(FRE07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
FRE07_QT.clusterCoef <- transitivity(FRE07_QTTable, type="global") #cluster coefficient
FRE07_QT.degreeCent <- centralization.degree(FRE07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE07_QTftn <- as.network.matrix(FRE07_QTft)
FRE07_QT.netDensity <- network.density(FRE07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE07_QT.entropy <- entropy(FRE07_QTft) #entropy

FRE07_QT.netMx <- cbind(FRE07_QT.netMx, FRE07_QT.clusterCoef, FRE07_QT.degreeCent$centralization,
                        FRE07_QT.netDensity, FRE07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE07_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
GCFC07_Gg2 <- data.frame(GCFC07_G)
GCFC07_Gg2 <- GCFC07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_Gg2$player1
player2vector <- GCFC07_Gg2$player2
GCFC07_Gg3 <- GCFC07_Gg2
GCFC07_Gg3$p1inp2vec <- is.element(GCFC07_Gg3$player1, player2vector)
GCFC07_Gg3$p2inp1vec <- is.element(GCFC07_Gg3$player2, player1vector)

addPlayer1 <- GCFC07_Gg3[ which(GCFC07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_Gg3[ which(GCFC07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_Gg2 <- rbind(GCFC07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
GCFC07_Gft <- ftable(GCFC07_Gg2$player1, GCFC07_Gg2$player2)
GCFC07_Gft2 <- as.matrix(GCFC07_Gft)
numRows <- nrow(GCFC07_Gft2)
numCols <- ncol(GCFC07_Gft2)
GCFC07_Gft3 <- GCFC07_Gft2[c(2:numRows) , c(2:numCols)]
GCFC07_GTable <- graph.adjacency(GCFC07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(GCFC07_GTable, vertex.label = V(GCFC07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
GCFC07_G.clusterCoef <- transitivity(GCFC07_GTable, type="global") #cluster coefficient
GCFC07_G.degreeCent <- centralization.degree(GCFC07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_Gftn <- as.network.matrix(GCFC07_Gft)
GCFC07_G.netDensity <- network.density(GCFC07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_G.entropy <- entropy(GCFC07_Gft) #entropy

GCFC07_G.netMx <- cbind(GCFC07_G.netMx, GCFC07_G.clusterCoef, GCFC07_G.degreeCent$centralization,
                        GCFC07_G.netDensity, GCFC07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
GCFC07_Bg2 <- data.frame(GCFC07_B)
GCFC07_Bg2 <- GCFC07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_Bg2$player1
player2vector <- GCFC07_Bg2$player2
GCFC07_Bg3 <- GCFC07_Bg2
GCFC07_Bg3$p1inp2vec <- is.element(GCFC07_Bg3$player1, player2vector)
GCFC07_Bg3$p2inp1vec <- is.element(GCFC07_Bg3$player2, player1vector)

addPlayer1 <- GCFC07_Bg3[ which(GCFC07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_Bg3[ which(GCFC07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_Bg2 <- rbind(GCFC07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
GCFC07_Bft <- ftable(GCFC07_Bg2$player1, GCFC07_Bg2$player2)
GCFC07_Bft2 <- as.matrix(GCFC07_Bft)
numRows <- nrow(GCFC07_Bft2)
numCols <- ncol(GCFC07_Bft2)
GCFC07_Bft3 <- GCFC07_Bft2[c(2:numRows) , c(2:numCols)]
GCFC07_BTable <- graph.adjacency(GCFC07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(GCFC07_BTable, vertex.label = V(GCFC07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
GCFC07_B.clusterCoef <- transitivity(GCFC07_BTable, type="global") #cluster coefficient
GCFC07_B.degreeCent <- centralization.degree(GCFC07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_Bftn <- as.network.matrix(GCFC07_Bft)
GCFC07_B.netDensity <- network.density(GCFC07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_B.entropy <- entropy(GCFC07_Bft) #entropy

GCFC07_B.netMx <- cbind(GCFC07_B.netMx, GCFC07_B.clusterCoef, GCFC07_B.degreeCent$centralization,
                        GCFC07_B.netDensity, GCFC07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
GCFC07_SFg2 <- data.frame(GCFC07_SF)
GCFC07_SFg2 <- GCFC07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_SFg2$player1
player2vector <- GCFC07_SFg2$player2
GCFC07_SFg3 <- GCFC07_SFg2
GCFC07_SFg3$p1inp2vec <- is.element(GCFC07_SFg3$player1, player2vector)
GCFC07_SFg3$p2inp1vec <- is.element(GCFC07_SFg3$player2, player1vector)

addPlayer1 <- GCFC07_SFg3[ which(GCFC07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC07_SFg2 <- rbind(GCFC07_SFg2, addPlayer1)

#ROUND 7, FWD Stoppage graph using weighted edges
GCFC07_SFft <- ftable(GCFC07_SFg2$player1, GCFC07_SFg2$player2)
GCFC07_SFft2 <- as.matrix(GCFC07_SFft)
numRows <- nrow(GCFC07_SFft2)
numCols <- ncol(GCFC07_SFft2)
GCFC07_SFft3 <- GCFC07_SFft2[c(2:numRows) , c(1:numCols)]
GCFC07_SFTable <- graph.adjacency(GCFC07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(GCFC07_SFTable, vertex.label = V(GCFC07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
GCFC07_SF.clusterCoef <- transitivity(GCFC07_SFTable, type="global") #cluster coefficient
GCFC07_SF.degreeCent <- centralization.degree(GCFC07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_SFftn <- as.network.matrix(GCFC07_SFft)
GCFC07_SF.netDensity <- network.density(GCFC07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_SF.entropy <- entropy(GCFC07_SFft) #entropy

GCFC07_SF.netMx <- cbind(GCFC07_SF.netMx, GCFC07_SF.clusterCoef, GCFC07_SF.degreeCent$centralization,
                         GCFC07_SF.netDensity, GCFC07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
GCFC07_TFg2 <- data.frame(GCFC07_TF)
GCFC07_TFg2 <- GCFC07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_TFg2$player1
player2vector <- GCFC07_TFg2$player2
GCFC07_TFg3 <- GCFC07_TFg2
GCFC07_TFg3$p1inp2vec <- is.element(GCFC07_TFg3$player1, player2vector)
GCFC07_TFg3$p2inp1vec <- is.element(GCFC07_TFg3$player2, player1vector)

addPlayer1 <- GCFC07_TFg3[ which(GCFC07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_TFg3[ which(GCFC07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_TFg2 <- rbind(GCFC07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
GCFC07_TFft <- ftable(GCFC07_TFg2$player1, GCFC07_TFg2$player2)
GCFC07_TFft2 <- as.matrix(GCFC07_TFft)
numRows <- nrow(GCFC07_TFft2)
numCols <- ncol(GCFC07_TFft2)
GCFC07_TFft3 <- GCFC07_TFft2[c(2:numRows) , c(2:numCols)]
GCFC07_TFTable <- graph.adjacency(GCFC07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(GCFC07_TFTable, vertex.label = V(GCFC07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
GCFC07_TF.clusterCoef <- transitivity(GCFC07_TFTable, type="global") #cluster coefficient
GCFC07_TF.degreeCent <- centralization.degree(GCFC07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_TFftn <- as.network.matrix(GCFC07_TFft)
GCFC07_TF.netDensity <- network.density(GCFC07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_TF.entropy <- entropy(GCFC07_TFft) #entropy

GCFC07_TF.netMx <- cbind(GCFC07_TF.netMx, GCFC07_TF.clusterCoef, GCFC07_TF.degreeCent$centralization,
                         GCFC07_TF.netDensity, GCFC07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
GCFC07_SAMg2 <- data.frame(GCFC07_SAM)
GCFC07_SAMg2 <- GCFC07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_SAMg2$player1
player2vector <- GCFC07_SAMg2$player2
GCFC07_SAMg3 <- GCFC07_SAMg2
GCFC07_SAMg3$p1inp2vec <- is.element(GCFC07_SAMg3$player1, player2vector)
GCFC07_SAMg3$p2inp1vec <- is.element(GCFC07_SAMg3$player2, player1vector)

addPlayer1 <- GCFC07_SAMg3[ which(GCFC07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_SAMg3[ which(GCFC07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_SAMg2 <- rbind(GCFC07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
GCFC07_SAMft <- ftable(GCFC07_SAMg2$player1, GCFC07_SAMg2$player2)
GCFC07_SAMft2 <- as.matrix(GCFC07_SAMft)
numRows <- nrow(GCFC07_SAMft2)
numCols <- ncol(GCFC07_SAMft2)
GCFC07_SAMft3 <- GCFC07_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC07_SAMTable <- graph.adjacency(GCFC07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(GCFC07_SAMTable, vertex.label = V(GCFC07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
GCFC07_SAM.clusterCoef <- transitivity(GCFC07_SAMTable, type="global") #cluster coefficient
GCFC07_SAM.degreeCent <- centralization.degree(GCFC07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_SAMftn <- as.network.matrix(GCFC07_SAMft)
GCFC07_SAM.netDensity <- network.density(GCFC07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_SAM.entropy <- entropy(GCFC07_SAMft) #entropy

GCFC07_SAM.netMx <- cbind(GCFC07_SAM.netMx, GCFC07_SAM.clusterCoef, GCFC07_SAM.degreeCent$centralization,
                          GCFC07_SAM.netDensity, GCFC07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
GCFC07_TAMg2 <- data.frame(GCFC07_TAM)
GCFC07_TAMg2 <- GCFC07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_TAMg2$player1
player2vector <- GCFC07_TAMg2$player2
GCFC07_TAMg3 <- GCFC07_TAMg2
GCFC07_TAMg3$p1inp2vec <- is.element(GCFC07_TAMg3$player1, player2vector)
GCFC07_TAMg3$p2inp1vec <- is.element(GCFC07_TAMg3$player2, player1vector)

addPlayer1 <- GCFC07_TAMg3[ which(GCFC07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC07_TAMg3[ which(GCFC07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_TAMg2 <- rbind(GCFC07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
GCFC07_TAMft <- ftable(GCFC07_TAMg2$player1, GCFC07_TAMg2$player2)
GCFC07_TAMft2 <- as.matrix(GCFC07_TAMft)
numRows <- nrow(GCFC07_TAMft2)
numCols <- ncol(GCFC07_TAMft2)
GCFC07_TAMft3 <- GCFC07_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC07_TAMTable <- graph.adjacency(GCFC07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(GCFC07_TAMTable, vertex.label = V(GCFC07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
GCFC07_TAM.clusterCoef <- transitivity(GCFC07_TAMTable, type="global") #cluster coefficient
GCFC07_TAM.degreeCent <- centralization.degree(GCFC07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_TAMftn <- as.network.matrix(GCFC07_TAMft)
GCFC07_TAM.netDensity <- network.density(GCFC07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_TAM.entropy <- entropy(GCFC07_TAMft) #entropy

GCFC07_TAM.netMx <- cbind(GCFC07_TAM.netMx, GCFC07_TAM.clusterCoef, GCFC07_TAM.degreeCent$centralization,
                          GCFC07_TAM.netDensity, GCFC07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
GCFC07_SDMg2 <- data.frame(GCFC07_SDM)
GCFC07_SDMg2 <- GCFC07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_SDMg2$player1
player2vector <- GCFC07_SDMg2$player2
GCFC07_SDMg3 <- GCFC07_SDMg2
GCFC07_SDMg3$p1inp2vec <- is.element(GCFC07_SDMg3$player1, player2vector)
GCFC07_SDMg3$p2inp1vec <- is.element(GCFC07_SDMg3$player2, player1vector)

addPlayer1 <- GCFC07_SDMg3[ which(GCFC07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_SDMg3[ which(GCFC07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_SDMg2 <- rbind(GCFC07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
GCFC07_SDMft <- ftable(GCFC07_SDMg2$player1, GCFC07_SDMg2$player2)
GCFC07_SDMft2 <- as.matrix(GCFC07_SDMft)
numRows <- nrow(GCFC07_SDMft2)
numCols <- ncol(GCFC07_SDMft2)
GCFC07_SDMft3 <- GCFC07_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC07_SDMTable <- graph.adjacency(GCFC07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(GCFC07_SDMTable, vertex.label = V(GCFC07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
GCFC07_SDM.clusterCoef <- transitivity(GCFC07_SDMTable, type="global") #cluster coefficient
GCFC07_SDM.degreeCent <- centralization.degree(GCFC07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_SDMftn <- as.network.matrix(GCFC07_SDMft)
GCFC07_SDM.netDensity <- network.density(GCFC07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_SDM.entropy <- entropy(GCFC07_SDMft) #entropy

GCFC07_SDM.netMx <- cbind(GCFC07_SDM.netMx, GCFC07_SDM.clusterCoef, GCFC07_SDM.degreeCent$centralization,
                          GCFC07_SDM.netDensity, GCFC07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
GCFC07_TDMg2 <- data.frame(GCFC07_TDM)
GCFC07_TDMg2 <- GCFC07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_TDMg2$player1
player2vector <- GCFC07_TDMg2$player2
GCFC07_TDMg3 <- GCFC07_TDMg2
GCFC07_TDMg3$p1inp2vec <- is.element(GCFC07_TDMg3$player1, player2vector)
GCFC07_TDMg3$p2inp1vec <- is.element(GCFC07_TDMg3$player2, player1vector)

addPlayer1 <- GCFC07_TDMg3[ which(GCFC07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC07_TDMg3[ which(GCFC07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_TDMg2 <- rbind(GCFC07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
GCFC07_TDMft <- ftable(GCFC07_TDMg2$player1, GCFC07_TDMg2$player2)
GCFC07_TDMft2 <- as.matrix(GCFC07_TDMft)
numRows <- nrow(GCFC07_TDMft2)
numCols <- ncol(GCFC07_TDMft2)
GCFC07_TDMft3 <- GCFC07_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC07_TDMTable <- graph.adjacency(GCFC07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(GCFC07_TDMTable, vertex.label = V(GCFC07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
GCFC07_TDM.clusterCoef <- transitivity(GCFC07_TDMTable, type="global") #cluster coefficient
GCFC07_TDM.degreeCent <- centralization.degree(GCFC07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_TDMftn <- as.network.matrix(GCFC07_TDMft)
GCFC07_TDM.netDensity <- network.density(GCFC07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_TDM.entropy <- entropy(GCFC07_TDMft) #entropy

GCFC07_TDM.netMx <- cbind(GCFC07_TDM.netMx, GCFC07_TDM.clusterCoef, GCFC07_TDM.degreeCent$centralization,
                          GCFC07_TDM.netDensity, GCFC07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
GCFC07_SDg2 <- data.frame(GCFC07_SD)
GCFC07_SDg2 <- GCFC07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_SDg2$player1
player2vector <- GCFC07_SDg2$player2
GCFC07_SDg3 <- GCFC07_SDg2
GCFC07_SDg3$p1inp2vec <- is.element(GCFC07_SDg3$player1, player2vector)
GCFC07_SDg3$p2inp1vec <- is.element(GCFC07_SDg3$player2, player1vector)

addPlayer1 <- GCFC07_SDg3[ which(GCFC07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_SDg3[ which(GCFC07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_SDg2 <- rbind(GCFC07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
GCFC07_SDft <- ftable(GCFC07_SDg2$player1, GCFC07_SDg2$player2)
GCFC07_SDft2 <- as.matrix(GCFC07_SDft)
numRows <- nrow(GCFC07_SDft2)
numCols <- ncol(GCFC07_SDft2)
GCFC07_SDft3 <- GCFC07_SDft2[c(2:numRows) , c(2:numCols)]
GCFC07_SDTable <- graph.adjacency(GCFC07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(GCFC07_SDTable, vertex.label = V(GCFC07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
GCFC07_SD.clusterCoef <- transitivity(GCFC07_SDTable, type="global") #cluster coefficient
GCFC07_SD.degreeCent <- centralization.degree(GCFC07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_SDftn <- as.network.matrix(GCFC07_SDft)
GCFC07_SD.netDensity <- network.density(GCFC07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_SD.entropy <- entropy(GCFC07_SDft) #entropy

GCFC07_SD.netMx <- cbind(GCFC07_SD.netMx, GCFC07_SD.clusterCoef, GCFC07_SD.degreeCent$centralization,
                         GCFC07_SD.netDensity, GCFC07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
GCFC07_TDg2 <- data.frame(GCFC07_TD)
GCFC07_TDg2 <- GCFC07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_TDg2$player1
player2vector <- GCFC07_TDg2$player2
GCFC07_TDg3 <- GCFC07_TDg2
GCFC07_TDg3$p1inp2vec <- is.element(GCFC07_TDg3$player1, player2vector)
GCFC07_TDg3$p2inp1vec <- is.element(GCFC07_TDg3$player2, player1vector)

addPlayer1 <- GCFC07_TDg3[ which(GCFC07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_TDg3[ which(GCFC07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_TDg2 <- rbind(GCFC07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
GCFC07_TDft <- ftable(GCFC07_TDg2$player1, GCFC07_TDg2$player2)
GCFC07_TDft2 <- as.matrix(GCFC07_TDft)
numRows <- nrow(GCFC07_TDft2)
numCols <- ncol(GCFC07_TDft2)
GCFC07_TDft3 <- GCFC07_TDft2[c(2:numRows) , c(2:numCols)]
GCFC07_TDTable <- graph.adjacency(GCFC07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(GCFC07_TDTable, vertex.label = V(GCFC07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
GCFC07_TD.clusterCoef <- transitivity(GCFC07_TDTable, type="global") #cluster coefficient
GCFC07_TD.degreeCent <- centralization.degree(GCFC07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_TDftn <- as.network.matrix(GCFC07_TDft)
GCFC07_TD.netDensity <- network.density(GCFC07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_TD.entropy <- entropy(GCFC07_TDft) #entropy

GCFC07_TD.netMx <- cbind(GCFC07_TD.netMx, GCFC07_TD.clusterCoef, GCFC07_TD.degreeCent$centralization,
                         GCFC07_TD.netDensity, GCFC07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
GCFC07_QTg2 <- data.frame(GCFC07_QT)
GCFC07_QTg2 <- GCFC07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC07_QTg2$player1
player2vector <- GCFC07_QTg2$player2
GCFC07_QTg3 <- GCFC07_QTg2
GCFC07_QTg3$p1inp2vec <- is.element(GCFC07_QTg3$player1, player2vector)
GCFC07_QTg3$p2inp1vec <- is.element(GCFC07_QTg3$player2, player1vector)

addPlayer1 <- GCFC07_QTg3[ which(GCFC07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC07_QTg3[ which(GCFC07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC07_QTg2 <- rbind(GCFC07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
GCFC07_QTft <- ftable(GCFC07_QTg2$player1, GCFC07_QTg2$player2)
GCFC07_QTft2 <- as.matrix(GCFC07_QTft)
numRows <- nrow(GCFC07_QTft2)
numCols <- ncol(GCFC07_QTft2)
GCFC07_QTft3 <- GCFC07_QTft2[c(2:numRows) , c(2:numCols)]
GCFC07_QTTable <- graph.adjacency(GCFC07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(GCFC07_QTTable, vertex.label = V(GCFC07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
GCFC07_QT.clusterCoef <- transitivity(GCFC07_QTTable, type="global") #cluster coefficient
GCFC07_QT.degreeCent <- centralization.degree(GCFC07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC07_QTftn <- as.network.matrix(GCFC07_QTft)
GCFC07_QT.netDensity <- network.density(GCFC07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC07_QT.entropy <- entropy(GCFC07_QTft) #entropy

GCFC07_QT.netMx <- cbind(GCFC07_QT.netMx, GCFC07_QT.clusterCoef, GCFC07_QT.degreeCent$centralization,
                         GCFC07_QT.netDensity, GCFC07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC07_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
GEEL07_Gg2 <- data.frame(GEEL07_G)
GEEL07_Gg2 <- GEEL07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_Gg2$player1
player2vector <- GEEL07_Gg2$player2
GEEL07_Gg3 <- GEEL07_Gg2
GEEL07_Gg3$p1inp2vec <- is.element(GEEL07_Gg3$player1, player2vector)
GEEL07_Gg3$p2inp1vec <- is.element(GEEL07_Gg3$player2, player1vector)

addPlayer1 <- GEEL07_Gg3[ which(GEEL07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_Gg3[ which(GEEL07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_Gg2 <- rbind(GEEL07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
GEEL07_Gft <- ftable(GEEL07_Gg2$player1, GEEL07_Gg2$player2)
GEEL07_Gft2 <- as.matrix(GEEL07_Gft)
numRows <- nrow(GEEL07_Gft2)
numCols <- ncol(GEEL07_Gft2)
GEEL07_Gft3 <- GEEL07_Gft2[c(2:numRows) , c(2:numCols)]
GEEL07_GTable <- graph.adjacency(GEEL07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(GEEL07_GTable, vertex.label = V(GEEL07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
GEEL07_G.clusterCoef <- transitivity(GEEL07_GTable, type="global") #cluster coefficient
GEEL07_G.degreeCent <- centralization.degree(GEEL07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_Gftn <- as.network.matrix(GEEL07_Gft)
GEEL07_G.netDensity <- network.density(GEEL07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_G.entropy <- entropy(GEEL07_Gft) #entropy

GEEL07_G.netMx <- cbind(GEEL07_G.netMx, GEEL07_G.clusterCoef, GEEL07_G.degreeCent$centralization,
                        GEEL07_G.netDensity, GEEL07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
GEEL07_Bg2 <- data.frame(GEEL07_B)
GEEL07_Bg2 <- GEEL07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_Bg2$player1
player2vector <- GEEL07_Bg2$player2
GEEL07_Bg3 <- GEEL07_Bg2
GEEL07_Bg3$p1inp2vec <- is.element(GEEL07_Bg3$player1, player2vector)
GEEL07_Bg3$p2inp1vec <- is.element(GEEL07_Bg3$player2, player1vector)

addPlayer1 <- GEEL07_Bg3[ which(GEEL07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_Bg3[ which(GEEL07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_Bg2 <- rbind(GEEL07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
GEEL07_Bft <- ftable(GEEL07_Bg2$player1, GEEL07_Bg2$player2)
GEEL07_Bft2 <- as.matrix(GEEL07_Bft)
numRows <- nrow(GEEL07_Bft2)
numCols <- ncol(GEEL07_Bft2)
GEEL07_Bft3 <- GEEL07_Bft2[c(2:numRows) , c(2:numCols)]
GEEL07_BTable <- graph.adjacency(GEEL07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(GEEL07_BTable, vertex.label = V(GEEL07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
GEEL07_B.clusterCoef <- transitivity(GEEL07_BTable, type="global") #cluster coefficient
GEEL07_B.degreeCent <- centralization.degree(GEEL07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_Bftn <- as.network.matrix(GEEL07_Bft)
GEEL07_B.netDensity <- network.density(GEEL07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_B.entropy <- entropy(GEEL07_Bft) #entropy

GEEL07_B.netMx <- cbind(GEEL07_B.netMx, GEEL07_B.clusterCoef, GEEL07_B.degreeCent$centralization,
                        GEEL07_B.netDensity, GEEL07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
GEEL07_SFg2 <- data.frame(GEEL07_SF)
GEEL07_SFg2 <- GEEL07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_SFg2$player1
player2vector <- GEEL07_SFg2$player2
GEEL07_SFg3 <- GEEL07_SFg2
GEEL07_SFg3$p1inp2vec <- is.element(GEEL07_SFg3$player1, player2vector)
GEEL07_SFg3$p2inp1vec <- is.element(GEEL07_SFg3$player2, player1vector)

addPlayer1 <- GEEL07_SFg3[ which(GEEL07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_SFg3[ which(GEEL07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_SFg2 <- rbind(GEEL07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
GEEL07_SFft <- ftable(GEEL07_SFg2$player1, GEEL07_SFg2$player2)
GEEL07_SFft2 <- as.matrix(GEEL07_SFft)
numRows <- nrow(GEEL07_SFft2)
numCols <- ncol(GEEL07_SFft2)
GEEL07_SFft3 <- GEEL07_SFft2[c(2:numRows) , c(2:numCols)]
GEEL07_SFTable <- graph.adjacency(GEEL07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(GEEL07_SFTable, vertex.label = V(GEEL07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
GEEL07_SF.clusterCoef <- transitivity(GEEL07_SFTable, type="global") #cluster coefficient
GEEL07_SF.degreeCent <- centralization.degree(GEEL07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_SFftn <- as.network.matrix(GEEL07_SFft)
GEEL07_SF.netDensity <- network.density(GEEL07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_SF.entropy <- entropy(GEEL07_SFft) #entropy

GEEL07_SF.netMx <- cbind(GEEL07_SF.netMx, GEEL07_SF.clusterCoef, GEEL07_SF.degreeCent$centralization,
                         GEEL07_SF.netDensity, GEEL07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
GEEL07_TFg2 <- data.frame(GEEL07_TF)
GEEL07_TFg2 <- GEEL07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_TFg2$player1
player2vector <- GEEL07_TFg2$player2
GEEL07_TFg3 <- GEEL07_TFg2
GEEL07_TFg3$p1inp2vec <- is.element(GEEL07_TFg3$player1, player2vector)
GEEL07_TFg3$p2inp1vec <- is.element(GEEL07_TFg3$player2, player1vector)

addPlayer1 <- GEEL07_TFg3[ which(GEEL07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_TFg3[ which(GEEL07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_TFg2 <- rbind(GEEL07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
GEEL07_TFft <- ftable(GEEL07_TFg2$player1, GEEL07_TFg2$player2)
GEEL07_TFft2 <- as.matrix(GEEL07_TFft)
numRows <- nrow(GEEL07_TFft2)
numCols <- ncol(GEEL07_TFft2)
GEEL07_TFft3 <- GEEL07_TFft2[c(2:numRows) , c(2:numCols)]
GEEL07_TFTable <- graph.adjacency(GEEL07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(GEEL07_TFTable, vertex.label = V(GEEL07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
GEEL07_TF.clusterCoef <- transitivity(GEEL07_TFTable, type="global") #cluster coefficient
GEEL07_TF.degreeCent <- centralization.degree(GEEL07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_TFftn <- as.network.matrix(GEEL07_TFft)
GEEL07_TF.netDensity <- network.density(GEEL07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_TF.entropy <- entropy(GEEL07_TFft) #entropy

GEEL07_TF.netMx <- cbind(GEEL07_TF.netMx, GEEL07_TF.clusterCoef, GEEL07_TF.degreeCent$centralization,
                         GEEL07_TF.netDensity, GEEL07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
GEEL07_SAMg2 <- data.frame(GEEL07_SAM)
GEEL07_SAMg2 <- GEEL07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_SAMg2$player1
player2vector <- GEEL07_SAMg2$player2
GEEL07_SAMg3 <- GEEL07_SAMg2
GEEL07_SAMg3$p1inp2vec <- is.element(GEEL07_SAMg3$player1, player2vector)
GEEL07_SAMg3$p2inp1vec <- is.element(GEEL07_SAMg3$player2, player1vector)

addPlayer1 <- GEEL07_SAMg3[ which(GEEL07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_SAMg3[ which(GEEL07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_SAMg2 <- rbind(GEEL07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
GEEL07_SAMft <- ftable(GEEL07_SAMg2$player1, GEEL07_SAMg2$player2)
GEEL07_SAMft2 <- as.matrix(GEEL07_SAMft)
numRows <- nrow(GEEL07_SAMft2)
numCols <- ncol(GEEL07_SAMft2)
GEEL07_SAMft3 <- GEEL07_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL07_SAMTable <- graph.adjacency(GEEL07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(GEEL07_SAMTable, vertex.label = V(GEEL07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
GEEL07_SAM.clusterCoef <- transitivity(GEEL07_SAMTable, type="global") #cluster coefficient
GEEL07_SAM.degreeCent <- centralization.degree(GEEL07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_SAMftn <- as.network.matrix(GEEL07_SAMft)
GEEL07_SAM.netDensity <- network.density(GEEL07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_SAM.entropy <- entropy(GEEL07_SAMft) #entropy

GEEL07_SAM.netMx <- cbind(GEEL07_SAM.netMx, GEEL07_SAM.clusterCoef, GEEL07_SAM.degreeCent$centralization,
                          GEEL07_SAM.netDensity, GEEL07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
GEEL07_TAMg2 <- data.frame(GEEL07_TAM)
GEEL07_TAMg2 <- GEEL07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_TAMg2$player1
player2vector <- GEEL07_TAMg2$player2
GEEL07_TAMg3 <- GEEL07_TAMg2
GEEL07_TAMg3$p1inp2vec <- is.element(GEEL07_TAMg3$player1, player2vector)
GEEL07_TAMg3$p2inp1vec <- is.element(GEEL07_TAMg3$player2, player1vector)

addPlayer1 <- GEEL07_TAMg3[ which(GEEL07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_TAMg3[ which(GEEL07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_TAMg2 <- rbind(GEEL07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
GEEL07_TAMft <- ftable(GEEL07_TAMg2$player1, GEEL07_TAMg2$player2)
GEEL07_TAMft2 <- as.matrix(GEEL07_TAMft)
numRows <- nrow(GEEL07_TAMft2)
numCols <- ncol(GEEL07_TAMft2)
GEEL07_TAMft3 <- GEEL07_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL07_TAMTable <- graph.adjacency(GEEL07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(GEEL07_TAMTable, vertex.label = V(GEEL07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
GEEL07_TAM.clusterCoef <- transitivity(GEEL07_TAMTable, type="global") #cluster coefficient
GEEL07_TAM.degreeCent <- centralization.degree(GEEL07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_TAMftn <- as.network.matrix(GEEL07_TAMft)
GEEL07_TAM.netDensity <- network.density(GEEL07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_TAM.entropy <- entropy(GEEL07_TAMft) #entropy

GEEL07_TAM.netMx <- cbind(GEEL07_TAM.netMx, GEEL07_TAM.clusterCoef, GEEL07_TAM.degreeCent$centralization,
                          GEEL07_TAM.netDensity, GEEL07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
GEEL07_SDMg2 <- data.frame(GEEL07_SDM)
GEEL07_SDMg2 <- GEEL07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_SDMg2$player1
player2vector <- GEEL07_SDMg2$player2
GEEL07_SDMg3 <- GEEL07_SDMg2
GEEL07_SDMg3$p1inp2vec <- is.element(GEEL07_SDMg3$player1, player2vector)
GEEL07_SDMg3$p2inp1vec <- is.element(GEEL07_SDMg3$player2, player1vector)

addPlayer1 <- GEEL07_SDMg3[ which(GEEL07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL07_SDMg3[ which(GEEL07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_SDMg2 <- rbind(GEEL07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
GEEL07_SDMft <- ftable(GEEL07_SDMg2$player1, GEEL07_SDMg2$player2)
GEEL07_SDMft2 <- as.matrix(GEEL07_SDMft)
numRows <- nrow(GEEL07_SDMft2)
numCols <- ncol(GEEL07_SDMft2)
GEEL07_SDMft3 <- GEEL07_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL07_SDMTable <- graph.adjacency(GEEL07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(GEEL07_SDMTable, vertex.label = V(GEEL07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
GEEL07_SDM.clusterCoef <- transitivity(GEEL07_SDMTable, type="global") #cluster coefficient
GEEL07_SDM.degreeCent <- centralization.degree(GEEL07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_SDMftn <- as.network.matrix(GEEL07_SDMft)
GEEL07_SDM.netDensity <- network.density(GEEL07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_SDM.entropy <- entropy(GEEL07_SDMft) #entropy

GEEL07_SDM.netMx <- cbind(GEEL07_SDM.netMx, GEEL07_SDM.clusterCoef, GEEL07_SDM.degreeCent$centralization,
                          GEEL07_SDM.netDensity, GEEL07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
GEEL07_TDMg2 <- data.frame(GEEL07_TDM)
GEEL07_TDMg2 <- GEEL07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_TDMg2$player1
player2vector <- GEEL07_TDMg2$player2
GEEL07_TDMg3 <- GEEL07_TDMg2
GEEL07_TDMg3$p1inp2vec <- is.element(GEEL07_TDMg3$player1, player2vector)
GEEL07_TDMg3$p2inp1vec <- is.element(GEEL07_TDMg3$player2, player1vector)

addPlayer1 <- GEEL07_TDMg3[ which(GEEL07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_TDMg3[ which(GEEL07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_TDMg2 <- rbind(GEEL07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
GEEL07_TDMft <- ftable(GEEL07_TDMg2$player1, GEEL07_TDMg2$player2)
GEEL07_TDMft2 <- as.matrix(GEEL07_TDMft)
numRows <- nrow(GEEL07_TDMft2)
numCols <- ncol(GEEL07_TDMft2)
GEEL07_TDMft3 <- GEEL07_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL07_TDMTable <- graph.adjacency(GEEL07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(GEEL07_TDMTable, vertex.label = V(GEEL07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
GEEL07_TDM.clusterCoef <- transitivity(GEEL07_TDMTable, type="global") #cluster coefficient
GEEL07_TDM.degreeCent <- centralization.degree(GEEL07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_TDMftn <- as.network.matrix(GEEL07_TDMft)
GEEL07_TDM.netDensity <- network.density(GEEL07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_TDM.entropy <- entropy(GEEL07_TDMft) #entropy

GEEL07_TDM.netMx <- cbind(GEEL07_TDM.netMx, GEEL07_TDM.clusterCoef, GEEL07_TDM.degreeCent$centralization,
                          GEEL07_TDM.netDensity, GEEL07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
GEEL07_SDg2 <- data.frame(GEEL07_SD)
GEEL07_SDg2 <- GEEL07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_SDg2$player1
player2vector <- GEEL07_SDg2$player2
GEEL07_SDg3 <- GEEL07_SDg2
GEEL07_SDg3$p1inp2vec <- is.element(GEEL07_SDg3$player1, player2vector)
GEEL07_SDg3$p2inp1vec <- is.element(GEEL07_SDg3$player2, player1vector)

addPlayer1 <- GEEL07_SDg3[ which(GEEL07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_SDg3[ which(GEEL07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_SDg2 <- rbind(GEEL07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
GEEL07_SDft <- ftable(GEEL07_SDg2$player1, GEEL07_SDg2$player2)
GEEL07_SDft2 <- as.matrix(GEEL07_SDft)
numRows <- nrow(GEEL07_SDft2)
numCols <- ncol(GEEL07_SDft2)
GEEL07_SDft3 <- GEEL07_SDft2[c(2:numRows) , c(2:numCols)]
GEEL07_SDTable <- graph.adjacency(GEEL07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(GEEL07_SDTable, vertex.label = V(GEEL07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
GEEL07_SD.clusterCoef <- transitivity(GEEL07_SDTable, type="global") #cluster coefficient
GEEL07_SD.degreeCent <- centralization.degree(GEEL07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_SDftn <- as.network.matrix(GEEL07_SDft)
GEEL07_SD.netDensity <- network.density(GEEL07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_SD.entropy <- entropy(GEEL07_SDft) #entropy

GEEL07_SD.netMx <- cbind(GEEL07_SD.netMx, GEEL07_SD.clusterCoef, GEEL07_SD.degreeCent$centralization,
                         GEEL07_SD.netDensity, GEEL07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
GEEL07_TDg2 <- data.frame(GEEL07_TD)
GEEL07_TDg2 <- GEEL07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_TDg2$player1
player2vector <- GEEL07_TDg2$player2
GEEL07_TDg3 <- GEEL07_TDg2
GEEL07_TDg3$p1inp2vec <- is.element(GEEL07_TDg3$player1, player2vector)
GEEL07_TDg3$p2inp1vec <- is.element(GEEL07_TDg3$player2, player1vector)

addPlayer1 <- GEEL07_TDg3[ which(GEEL07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_TDg3[ which(GEEL07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_TDg2 <- rbind(GEEL07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
GEEL07_TDft <- ftable(GEEL07_TDg2$player1, GEEL07_TDg2$player2)
GEEL07_TDft2 <- as.matrix(GEEL07_TDft)
numRows <- nrow(GEEL07_TDft2)
numCols <- ncol(GEEL07_TDft2)
GEEL07_TDft3 <- GEEL07_TDft2[c(2:numRows) , c(2:numCols)]
GEEL07_TDTable <- graph.adjacency(GEEL07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(GEEL07_TDTable, vertex.label = V(GEEL07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
GEEL07_TD.clusterCoef <- transitivity(GEEL07_TDTable, type="global") #cluster coefficient
GEEL07_TD.degreeCent <- centralization.degree(GEEL07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_TDftn <- as.network.matrix(GEEL07_TDft)
GEEL07_TD.netDensity <- network.density(GEEL07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_TD.entropy <- entropy(GEEL07_TDft) #entropy

GEEL07_TD.netMx <- cbind(GEEL07_TD.netMx, GEEL07_TD.clusterCoef, GEEL07_TD.degreeCent$centralization,
                         GEEL07_TD.netDensity, GEEL07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
GEEL07_QTg2 <- data.frame(GEEL07_QT)
GEEL07_QTg2 <- GEEL07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL07_QTg2$player1
player2vector <- GEEL07_QTg2$player2
GEEL07_QTg3 <- GEEL07_QTg2
GEEL07_QTg3$p1inp2vec <- is.element(GEEL07_QTg3$player1, player2vector)
GEEL07_QTg3$p2inp1vec <- is.element(GEEL07_QTg3$player2, player1vector)

addPlayer1 <- GEEL07_QTg3[ which(GEEL07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL07_QTg3[ which(GEEL07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL07_QTg2 <- rbind(GEEL07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
GEEL07_QTft <- ftable(GEEL07_QTg2$player1, GEEL07_QTg2$player2)
GEEL07_QTft2 <- as.matrix(GEEL07_QTft)
numRows <- nrow(GEEL07_QTft2)
numCols <- ncol(GEEL07_QTft2)
GEEL07_QTft3 <- GEEL07_QTft2[c(2:numRows) , c(2:numCols)]
GEEL07_QTTable <- graph.adjacency(GEEL07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(GEEL07_QTTable, vertex.label = V(GEEL07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
GEEL07_QT.clusterCoef <- transitivity(GEEL07_QTTable, type="global") #cluster coefficient
GEEL07_QT.degreeCent <- centralization.degree(GEEL07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL07_QTftn <- as.network.matrix(GEEL07_QTft)
GEEL07_QT.netDensity <- network.density(GEEL07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL07_QT.entropy <- entropy(GEEL07_QTft) #entropy

GEEL07_QT.netMx <- cbind(GEEL07_QT.netMx, GEEL07_QT.clusterCoef, GEEL07_QT.degreeCent$centralization,
                         GEEL07_QT.netDensity, GEEL07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL07_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Goal_F"
GWS07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
GWS07_Gg2 <- data.frame(GWS07_G)
GWS07_Gg2 <- GWS07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_Gg2$player1
player2vector <- GWS07_Gg2$player2
GWS07_Gg3 <- GWS07_Gg2
GWS07_Gg3$p1inp2vec <- is.element(GWS07_Gg3$player1, player2vector)
GWS07_Gg3$p2inp1vec <- is.element(GWS07_Gg3$player2, player1vector)

addPlayer1 <- GWS07_Gg3[ which(GWS07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_Gg3[ which(GWS07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_Gg2 <- rbind(GWS07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
GWS07_Gft <- ftable(GWS07_Gg2$player1, GWS07_Gg2$player2)
GWS07_Gft2 <- as.matrix(GWS07_Gft)
numRows <- nrow(GWS07_Gft2)
numCols <- ncol(GWS07_Gft2)
GWS07_Gft3 <- GWS07_Gft2[c(1:numRows) , c(1:numCols)]
GWS07_GTable <- graph.adjacency(GWS07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(GWS07_GTable, vertex.label = V(GWS07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
GWS07_G.clusterCoef <- transitivity(GWS07_GTable, type="global") #cluster coefficient
GWS07_G.degreeCent <- centralization.degree(GWS07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_Gftn <- as.network.matrix(GWS07_Gft)
GWS07_G.netDensity <- network.density(GWS07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_G.entropy <- entropy(GWS07_Gft) #entropy

GWS07_G.netMx <- cbind(GWS07_G.netMx, GWS07_G.clusterCoef, GWS07_G.degreeCent$centralization,
                       GWS07_G.netDensity, GWS07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Behind_F"
GWS07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
GWS07_Bg2 <- data.frame(GWS07_B)
GWS07_Bg2 <- GWS07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_Bg2$player1
player2vector <- GWS07_Bg2$player2
GWS07_Bg3 <- GWS07_Bg2
GWS07_Bg3$p1inp2vec <- is.element(GWS07_Bg3$player1, player2vector)
GWS07_Bg3$p2inp1vec <- is.element(GWS07_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS07_Bg3[ which(GWS07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS07_Bg2 <- rbind(GWS07_Bg2, addPlayer2)

#ROUND 7, Behind graph using weighted edges
GWS07_Bft <- ftable(GWS07_Bg2$player1, GWS07_Bg2$player2)
GWS07_Bft2 <- as.matrix(GWS07_Bft)
numRows <- nrow(GWS07_Bft2)
numCols <- ncol(GWS07_Bft2)
GWS07_Bft3 <- GWS07_Bft2[c(1:numRows) , c(2:numCols)]
GWS07_BTable <- graph.adjacency(GWS07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(GWS07_BTable, vertex.label = V(GWS07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
GWS07_B.clusterCoef <- transitivity(GWS07_BTable, type="global") #cluster coefficient
GWS07_B.degreeCent <- centralization.degree(GWS07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_Bftn <- as.network.matrix(GWS07_Bft)
GWS07_B.netDensity <- network.density(GWS07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_B.entropy <- entropy(GWS07_Bft) #entropy

GWS07_B.netMx <- cbind(GWS07_B.netMx, GWS07_B.clusterCoef, GWS07_B.degreeCent$centralization,
                       GWS07_B.netDensity, GWS07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
GWS07_SFg2 <- data.frame(GWS07_SF)
GWS07_SFg2 <- GWS07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_SFg2$player1
player2vector <- GWS07_SFg2$player2
GWS07_SFg3 <- GWS07_SFg2
GWS07_SFg3$p1inp2vec <- is.element(GWS07_SFg3$player1, player2vector)
GWS07_SFg3$p2inp1vec <- is.element(GWS07_SFg3$player2, player1vector)

addPlayer1 <- GWS07_SFg3[ which(GWS07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_SFg3[ which(GWS07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_SFg2 <- rbind(GWS07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
GWS07_SFft <- ftable(GWS07_SFg2$player1, GWS07_SFg2$player2)
GWS07_SFft2 <- as.matrix(GWS07_SFft)
numRows <- nrow(GWS07_SFft2)
numCols <- ncol(GWS07_SFft2)
GWS07_SFft3 <- GWS07_SFft2[c(2:numRows) , c(2:numCols)]
GWS07_SFTable <- graph.adjacency(GWS07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(GWS07_SFTable, vertex.label = V(GWS07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
GWS07_SF.clusterCoef <- transitivity(GWS07_SFTable, type="global") #cluster coefficient
GWS07_SF.degreeCent <- centralization.degree(GWS07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_SFftn <- as.network.matrix(GWS07_SFft)
GWS07_SF.netDensity <- network.density(GWS07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_SF.entropy <- entropy(GWS07_SFft) #entropy

GWS07_SF.netMx <- cbind(GWS07_SF.netMx, GWS07_SF.clusterCoef, GWS07_SF.degreeCent$centralization,
                        GWS07_SF.netDensity, GWS07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
GWS07_TFg2 <- data.frame(GWS07_TF)
GWS07_TFg2 <- GWS07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_TFg2$player1
player2vector <- GWS07_TFg2$player2
GWS07_TFg3 <- GWS07_TFg2
GWS07_TFg3$p1inp2vec <- is.element(GWS07_TFg3$player1, player2vector)
GWS07_TFg3$p2inp1vec <- is.element(GWS07_TFg3$player2, player1vector)

addPlayer1 <- GWS07_TFg3[ which(GWS07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_TFg3[ which(GWS07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_TFg2 <- rbind(GWS07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
GWS07_TFft <- ftable(GWS07_TFg2$player1, GWS07_TFg2$player2)
GWS07_TFft2 <- as.matrix(GWS07_TFft)
numRows <- nrow(GWS07_TFft2)
numCols <- ncol(GWS07_TFft2)
GWS07_TFft3 <- GWS07_TFft2[c(2:numRows) , c(2:numCols)]
GWS07_TFTable <- graph.adjacency(GWS07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(GWS07_TFTable, vertex.label = V(GWS07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
GWS07_TF.clusterCoef <- transitivity(GWS07_TFTable, type="global") #cluster coefficient
GWS07_TF.degreeCent <- centralization.degree(GWS07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_TFftn <- as.network.matrix(GWS07_TFft)
GWS07_TF.netDensity <- network.density(GWS07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_TF.entropy <- entropy(GWS07_TFft) #entropy

GWS07_TF.netMx <- cbind(GWS07_TF.netMx, GWS07_TF.clusterCoef, GWS07_TF.degreeCent$centralization,
                        GWS07_TF.netDensity, GWS07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
GWS07_SAMg2 <- data.frame(GWS07_SAM)
GWS07_SAMg2 <- GWS07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_SAMg2$player1
player2vector <- GWS07_SAMg2$player2
GWS07_SAMg3 <- GWS07_SAMg2
GWS07_SAMg3$p1inp2vec <- is.element(GWS07_SAMg3$player1, player2vector)
GWS07_SAMg3$p2inp1vec <- is.element(GWS07_SAMg3$player2, player1vector)

addPlayer1 <- GWS07_SAMg3[ which(GWS07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_SAMg3[ which(GWS07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_SAMg2 <- rbind(GWS07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
GWS07_SAMft <- ftable(GWS07_SAMg2$player1, GWS07_SAMg2$player2)
GWS07_SAMft2 <- as.matrix(GWS07_SAMft)
numRows <- nrow(GWS07_SAMft2)
numCols <- ncol(GWS07_SAMft2)
GWS07_SAMft3 <- GWS07_SAMft2[c(2:numRows) , c(2:numCols)]
GWS07_SAMTable <- graph.adjacency(GWS07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(GWS07_SAMTable, vertex.label = V(GWS07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
GWS07_SAM.clusterCoef <- transitivity(GWS07_SAMTable, type="global") #cluster coefficient
GWS07_SAM.degreeCent <- centralization.degree(GWS07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_SAMftn <- as.network.matrix(GWS07_SAMft)
GWS07_SAM.netDensity <- network.density(GWS07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_SAM.entropy <- entropy(GWS07_SAMft) #entropy

GWS07_SAM.netMx <- cbind(GWS07_SAM.netMx, GWS07_SAM.clusterCoef, GWS07_SAM.degreeCent$centralization,
                         GWS07_SAM.netDensity, GWS07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
GWS07_TAMg2 <- data.frame(GWS07_TAM)
GWS07_TAMg2 <- GWS07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_TAMg2$player1
player2vector <- GWS07_TAMg2$player2
GWS07_TAMg3 <- GWS07_TAMg2
GWS07_TAMg3$p1inp2vec <- is.element(GWS07_TAMg3$player1, player2vector)
GWS07_TAMg3$p2inp1vec <- is.element(GWS07_TAMg3$player2, player1vector)

addPlayer1 <- GWS07_TAMg3[ which(GWS07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_TAMg3[ which(GWS07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_TAMg2 <- rbind(GWS07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
GWS07_TAMft <- ftable(GWS07_TAMg2$player1, GWS07_TAMg2$player2)
GWS07_TAMft2 <- as.matrix(GWS07_TAMft)
numRows <- nrow(GWS07_TAMft2)
numCols <- ncol(GWS07_TAMft2)
GWS07_TAMft3 <- GWS07_TAMft2[c(2:numRows) , c(2:numCols)]
GWS07_TAMTable <- graph.adjacency(GWS07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(GWS07_TAMTable, vertex.label = V(GWS07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
GWS07_TAM.clusterCoef <- transitivity(GWS07_TAMTable, type="global") #cluster coefficient
GWS07_TAM.degreeCent <- centralization.degree(GWS07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_TAMftn <- as.network.matrix(GWS07_TAMft)
GWS07_TAM.netDensity <- network.density(GWS07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_TAM.entropy <- entropy(GWS07_TAMft) #entropy

GWS07_TAM.netMx <- cbind(GWS07_TAM.netMx, GWS07_TAM.clusterCoef, GWS07_TAM.degreeCent$centralization,
                         GWS07_TAM.netDensity, GWS07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
GWS07_SDMg2 <- data.frame(GWS07_SDM)
GWS07_SDMg2 <- GWS07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_SDMg2$player1
player2vector <- GWS07_SDMg2$player2
GWS07_SDMg3 <- GWS07_SDMg2
GWS07_SDMg3$p1inp2vec <- is.element(GWS07_SDMg3$player1, player2vector)
GWS07_SDMg3$p2inp1vec <- is.element(GWS07_SDMg3$player2, player1vector)

addPlayer1 <- GWS07_SDMg3[ which(GWS07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_SDMg3[ which(GWS07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_SDMg2 <- rbind(GWS07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
GWS07_SDMft <- ftable(GWS07_SDMg2$player1, GWS07_SDMg2$player2)
GWS07_SDMft2 <- as.matrix(GWS07_SDMft)
numRows <- nrow(GWS07_SDMft2)
numCols <- ncol(GWS07_SDMft2)
GWS07_SDMft3 <- GWS07_SDMft2[c(2:numRows) , c(2:numCols)]
GWS07_SDMTable <- graph.adjacency(GWS07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(GWS07_SDMTable, vertex.label = V(GWS07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
GWS07_SDM.clusterCoef <- transitivity(GWS07_SDMTable, type="global") #cluster coefficient
GWS07_SDM.degreeCent <- centralization.degree(GWS07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_SDMftn <- as.network.matrix(GWS07_SDMft)
GWS07_SDM.netDensity <- network.density(GWS07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_SDM.entropy <- entropy(GWS07_SDMft) #entropy

GWS07_SDM.netMx <- cbind(GWS07_SDM.netMx, GWS07_SDM.clusterCoef, GWS07_SDM.degreeCent$centralization,
                         GWS07_SDM.netDensity, GWS07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
GWS07_TDMg2 <- data.frame(GWS07_TDM)
GWS07_TDMg2 <- GWS07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_TDMg2$player1
player2vector <- GWS07_TDMg2$player2
GWS07_TDMg3 <- GWS07_TDMg2
GWS07_TDMg3$p1inp2vec <- is.element(GWS07_TDMg3$player1, player2vector)
GWS07_TDMg3$p2inp1vec <- is.element(GWS07_TDMg3$player2, player1vector)

addPlayer1 <- GWS07_TDMg3[ which(GWS07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_TDMg3[ which(GWS07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_TDMg2 <- rbind(GWS07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
GWS07_TDMft <- ftable(GWS07_TDMg2$player1, GWS07_TDMg2$player2)
GWS07_TDMft2 <- as.matrix(GWS07_TDMft)
numRows <- nrow(GWS07_TDMft2)
numCols <- ncol(GWS07_TDMft2)
GWS07_TDMft3 <- GWS07_TDMft2[c(2:numRows) , c(2:numCols)]
GWS07_TDMTable <- graph.adjacency(GWS07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(GWS07_TDMTable, vertex.label = V(GWS07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
GWS07_TDM.clusterCoef <- transitivity(GWS07_TDMTable, type="global") #cluster coefficient
GWS07_TDM.degreeCent <- centralization.degree(GWS07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_TDMftn <- as.network.matrix(GWS07_TDMft)
GWS07_TDM.netDensity <- network.density(GWS07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_TDM.entropy <- entropy(GWS07_TDMft) #entropy

GWS07_TDM.netMx <- cbind(GWS07_TDM.netMx, GWS07_TDM.clusterCoef, GWS07_TDM.degreeCent$centralization,
                         GWS07_TDM.netDensity, GWS07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
GWS07_SDg2 <- data.frame(GWS07_SD)
GWS07_SDg2 <- GWS07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_SDg2$player1
player2vector <- GWS07_SDg2$player2
GWS07_SDg3 <- GWS07_SDg2
GWS07_SDg3$p1inp2vec <- is.element(GWS07_SDg3$player1, player2vector)
GWS07_SDg3$p2inp1vec <- is.element(GWS07_SDg3$player2, player1vector)

addPlayer1 <- GWS07_SDg3[ which(GWS07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_SDg3[ which(GWS07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_SDg2 <- rbind(GWS07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
GWS07_SDft <- ftable(GWS07_SDg2$player1, GWS07_SDg2$player2)
GWS07_SDft2 <- as.matrix(GWS07_SDft)
numRows <- nrow(GWS07_SDft2)
numCols <- ncol(GWS07_SDft2)
GWS07_SDft3 <- GWS07_SDft2[c(2:numRows) , c(2:numCols)]
GWS07_SDTable <- graph.adjacency(GWS07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(GWS07_SDTable, vertex.label = V(GWS07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
GWS07_SD.clusterCoef <- transitivity(GWS07_SDTable, type="global") #cluster coefficient
GWS07_SD.degreeCent <- centralization.degree(GWS07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_SDftn <- as.network.matrix(GWS07_SDft)
GWS07_SD.netDensity <- network.density(GWS07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_SD.entropy <- entropy(GWS07_SDft) #entropy

GWS07_SD.netMx <- cbind(GWS07_SD.netMx, GWS07_SD.clusterCoef, GWS07_SD.degreeCent$centralization,
                        GWS07_SD.netDensity, GWS07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
GWS07_TDg2 <- data.frame(GWS07_TD)
GWS07_TDg2 <- GWS07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_TDg2$player1
player2vector <- GWS07_TDg2$player2
GWS07_TDg3 <- GWS07_TDg2
GWS07_TDg3$p1inp2vec <- is.element(GWS07_TDg3$player1, player2vector)
GWS07_TDg3$p2inp1vec <- is.element(GWS07_TDg3$player2, player1vector)

addPlayer1 <- GWS07_TDg3[ which(GWS07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_TDg3[ which(GWS07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_TDg2 <- rbind(GWS07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
GWS07_TDft <- ftable(GWS07_TDg2$player1, GWS07_TDg2$player2)
GWS07_TDft2 <- as.matrix(GWS07_TDft)
numRows <- nrow(GWS07_TDft2)
numCols <- ncol(GWS07_TDft2)
GWS07_TDft3 <- GWS07_TDft2[c(2:numRows) , c(2:numCols)]
GWS07_TDTable <- graph.adjacency(GWS07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(GWS07_TDTable, vertex.label = V(GWS07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
GWS07_TD.clusterCoef <- transitivity(GWS07_TDTable, type="global") #cluster coefficient
GWS07_TD.degreeCent <- centralization.degree(GWS07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_TDftn <- as.network.matrix(GWS07_TDft)
GWS07_TD.netDensity <- network.density(GWS07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_TD.entropy <- entropy(GWS07_TDft) #entropy

GWS07_TD.netMx <- cbind(GWS07_TD.netMx, GWS07_TD.clusterCoef, GWS07_TD.degreeCent$centralization,
                        GWS07_TD.netDensity, GWS07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
GWS07_QTg2 <- data.frame(GWS07_QT)
GWS07_QTg2 <- GWS07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS07_QTg2$player1
player2vector <- GWS07_QTg2$player2
GWS07_QTg3 <- GWS07_QTg2
GWS07_QTg3$p1inp2vec <- is.element(GWS07_QTg3$player1, player2vector)
GWS07_QTg3$p2inp1vec <- is.element(GWS07_QTg3$player2, player1vector)

addPlayer1 <- GWS07_QTg3[ which(GWS07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS07_QTg3[ which(GWS07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS07_QTg2 <- rbind(GWS07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
GWS07_QTft <- ftable(GWS07_QTg2$player1, GWS07_QTg2$player2)
GWS07_QTft2 <- as.matrix(GWS07_QTft)
numRows <- nrow(GWS07_QTft2)
numCols <- ncol(GWS07_QTft2)
GWS07_QTft3 <- GWS07_QTft2[c(2:numRows) , c(2:numCols)]
GWS07_QTTable <- graph.adjacency(GWS07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(GWS07_QTTable, vertex.label = V(GWS07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
GWS07_QT.clusterCoef <- transitivity(GWS07_QTTable, type="global") #cluster coefficient
GWS07_QT.degreeCent <- centralization.degree(GWS07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS07_QTftn <- as.network.matrix(GWS07_QTft)
GWS07_QT.netDensity <- network.density(GWS07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS07_QT.entropy <- entropy(GWS07_QTft) #entropy

GWS07_QT.netMx <- cbind(GWS07_QT.netMx, GWS07_QT.clusterCoef, GWS07_QT.degreeCent$centralization,
                        GWS07_QT.netDensity, GWS07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS07_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "Goal_F"
HAW07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
HAW07_Gg2 <- data.frame(HAW07_G)
HAW07_Gg2 <- HAW07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_Gg2$player1
player2vector <- HAW07_Gg2$player2
HAW07_Gg3 <- HAW07_Gg2
HAW07_Gg3$p1inp2vec <- is.element(HAW07_Gg3$player1, player2vector)
HAW07_Gg3$p2inp1vec <- is.element(HAW07_Gg3$player2, player1vector)

addPlayer1 <- HAW07_Gg3[ which(HAW07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_Gg3[ which(HAW07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_Gg2 <- rbind(HAW07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
HAW07_Gft <- ftable(HAW07_Gg2$player1, HAW07_Gg2$player2)
HAW07_Gft2 <- as.matrix(HAW07_Gft)
numRows <- nrow(HAW07_Gft2)
numCols <- ncol(HAW07_Gft2)
HAW07_Gft3 <- HAW07_Gft2[c(2:numRows) , c(2:numCols)]
HAW07_GTable <- graph.adjacency(HAW07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(HAW07_GTable, vertex.label = V(HAW07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
HAW07_G.clusterCoef <- transitivity(HAW07_GTable, type="global") #cluster coefficient
HAW07_G.degreeCent <- centralization.degree(HAW07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_Gftn <- as.network.matrix(HAW07_Gft)
HAW07_G.netDensity <- network.density(HAW07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_G.entropy <- entropy(HAW07_Gft) #entropy

HAW07_G.netMx <- cbind(HAW07_G.netMx, HAW07_G.clusterCoef, HAW07_G.degreeCent$centralization,
                       HAW07_G.netDensity, HAW07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************

round = 7
teamName = "HAW"
KIoutcome = "Behind_F"
HAW07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
HAW07_Bg2 <- data.frame(HAW07_B)
HAW07_Bg2 <- HAW07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_Bg2$player1
player2vector <- HAW07_Bg2$player2
HAW07_Bg3 <- HAW07_Bg2
HAW07_Bg3$p1inp2vec <- is.element(HAW07_Bg3$player1, player2vector)
HAW07_Bg3$p2inp1vec <- is.element(HAW07_Bg3$player2, player1vector)

addPlayer1 <- HAW07_Bg3[ which(HAW07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_Bg3[ which(HAW07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_Bg2 <- rbind(HAW07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
HAW07_Bft <- ftable(HAW07_Bg2$player1, HAW07_Bg2$player2)
HAW07_Bft2 <- as.matrix(HAW07_Bft)
numRows <- nrow(HAW07_Bft2)
numCols <- ncol(HAW07_Bft2)
HAW07_Bft3 <- HAW07_Bft2[c(2:numRows) , c(2:numCols)]
HAW07_BTable <- graph.adjacency(HAW07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(HAW07_BTable, vertex.label = V(HAW07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
HAW07_B.clusterCoef <- transitivity(HAW07_BTable, type="global") #cluster coefficient
HAW07_B.degreeCent <- centralization.degree(HAW07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_Bftn <- as.network.matrix(HAW07_Bft)
HAW07_B.netDensity <- network.density(HAW07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_B.entropy <- entropy(HAW07_Bft) #entropy

HAW07_B.netMx <- cbind(HAW07_B.netMx, HAW07_B.clusterCoef, HAW07_B.degreeCent$centralization,
                       HAW07_B.netDensity, HAW07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
HAW07_SFg2 <- data.frame(HAW07_SF)
HAW07_SFg2 <- HAW07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_SFg2$player1
player2vector <- HAW07_SFg2$player2
HAW07_SFg3 <- HAW07_SFg2
HAW07_SFg3$p1inp2vec <- is.element(HAW07_SFg3$player1, player2vector)
HAW07_SFg3$p2inp1vec <- is.element(HAW07_SFg3$player2, player1vector)

addPlayer1 <- HAW07_SFg3[ which(HAW07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_SFg3[ which(HAW07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_SFg2 <- rbind(HAW07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
HAW07_SFft <- ftable(HAW07_SFg2$player1, HAW07_SFg2$player2)
HAW07_SFft2 <- as.matrix(HAW07_SFft)
numRows <- nrow(HAW07_SFft2)
numCols <- ncol(HAW07_SFft2)
HAW07_SFft3 <- HAW07_SFft2[c(2:numRows) , c(2:numCols)]
HAW07_SFTable <- graph.adjacency(HAW07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(HAW07_SFTable, vertex.label = V(HAW07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
HAW07_SF.clusterCoef <- transitivity(HAW07_SFTable, type="global") #cluster coefficient
HAW07_SF.degreeCent <- centralization.degree(HAW07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_SFftn <- as.network.matrix(HAW07_SFft)
HAW07_SF.netDensity <- network.density(HAW07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_SF.entropy <- entropy(HAW07_SFft) #entropy

HAW07_SF.netMx <- cbind(HAW07_SF.netMx, HAW07_SF.clusterCoef, HAW07_SF.degreeCent$centralization,
                        HAW07_SF.netDensity, HAW07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
HAW07_TFg2 <- data.frame(HAW07_TF)
HAW07_TFg2 <- HAW07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_TFg2$player1
player2vector <- HAW07_TFg2$player2
HAW07_TFg3 <- HAW07_TFg2
HAW07_TFg3$p1inp2vec <- is.element(HAW07_TFg3$player1, player2vector)
HAW07_TFg3$p2inp1vec <- is.element(HAW07_TFg3$player2, player1vector)

addPlayer1 <- HAW07_TFg3[ which(HAW07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_TFg3[ which(HAW07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_TFg2 <- rbind(HAW07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
HAW07_TFft <- ftable(HAW07_TFg2$player1, HAW07_TFg2$player2)
HAW07_TFft2 <- as.matrix(HAW07_TFft)
numRows <- nrow(HAW07_TFft2)
numCols <- ncol(HAW07_TFft2)
HAW07_TFft3 <- HAW07_TFft2[c(2:numRows) , c(2:numCols)]
HAW07_TFTable <- graph.adjacency(HAW07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(HAW07_TFTable, vertex.label = V(HAW07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
HAW07_TF.clusterCoef <- transitivity(HAW07_TFTable, type="global") #cluster coefficient
HAW07_TF.degreeCent <- centralization.degree(HAW07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_TFftn <- as.network.matrix(HAW07_TFft)
HAW07_TF.netDensity <- network.density(HAW07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_TF.entropy <- entropy(HAW07_TFft) #entropy

HAW07_TF.netMx <- cbind(HAW07_TF.netMx, HAW07_TF.clusterCoef, HAW07_TF.degreeCent$centralization,
                        HAW07_TF.netDensity, HAW07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
HAW07_SAMg2 <- data.frame(HAW07_SAM)
HAW07_SAMg2 <- HAW07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_SAMg2$player1
player2vector <- HAW07_SAMg2$player2
HAW07_SAMg3 <- HAW07_SAMg2
HAW07_SAMg3$p1inp2vec <- is.element(HAW07_SAMg3$player1, player2vector)
HAW07_SAMg3$p2inp1vec <- is.element(HAW07_SAMg3$player2, player1vector)

addPlayer1 <- HAW07_SAMg3[ which(HAW07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_SAMg3[ which(HAW07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_SAMg2 <- rbind(HAW07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
HAW07_SAMft <- ftable(HAW07_SAMg2$player1, HAW07_SAMg2$player2)
HAW07_SAMft2 <- as.matrix(HAW07_SAMft)
numRows <- nrow(HAW07_SAMft2)
numCols <- ncol(HAW07_SAMft2)
HAW07_SAMft3 <- HAW07_SAMft2[c(2:numRows) , c(2:numCols)]
HAW07_SAMTable <- graph.adjacency(HAW07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(HAW07_SAMTable, vertex.label = V(HAW07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
HAW07_SAM.clusterCoef <- transitivity(HAW07_SAMTable, type="global") #cluster coefficient
HAW07_SAM.degreeCent <- centralization.degree(HAW07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_SAMftn <- as.network.matrix(HAW07_SAMft)
HAW07_SAM.netDensity <- network.density(HAW07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_SAM.entropy <- entropy(HAW07_SAMft) #entropy

HAW07_SAM.netMx <- cbind(HAW07_SAM.netMx, HAW07_SAM.clusterCoef, HAW07_SAM.degreeCent$centralization,
                         HAW07_SAM.netDensity, HAW07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
HAW07_TAMg2 <- data.frame(HAW07_TAM)
HAW07_TAMg2 <- HAW07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_TAMg2$player1
player2vector <- HAW07_TAMg2$player2
HAW07_TAMg3 <- HAW07_TAMg2
HAW07_TAMg3$p1inp2vec <- is.element(HAW07_TAMg3$player1, player2vector)
HAW07_TAMg3$p2inp1vec <- is.element(HAW07_TAMg3$player2, player1vector)

addPlayer1 <- HAW07_TAMg3[ which(HAW07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_TAMg3[ which(HAW07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_TAMg2 <- rbind(HAW07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
HAW07_TAMft <- ftable(HAW07_TAMg2$player1, HAW07_TAMg2$player2)
HAW07_TAMft2 <- as.matrix(HAW07_TAMft)
numRows <- nrow(HAW07_TAMft2)
numCols <- ncol(HAW07_TAMft2)
HAW07_TAMft3 <- HAW07_TAMft2[c(2:numRows) , c(2:numCols)]
HAW07_TAMTable <- graph.adjacency(HAW07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(HAW07_TAMTable, vertex.label = V(HAW07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
HAW07_TAM.clusterCoef <- transitivity(HAW07_TAMTable, type="global") #cluster coefficient
HAW07_TAM.degreeCent <- centralization.degree(HAW07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_TAMftn <- as.network.matrix(HAW07_TAMft)
HAW07_TAM.netDensity <- network.density(HAW07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_TAM.entropy <- entropy(HAW07_TAMft) #entropy

HAW07_TAM.netMx <- cbind(HAW07_TAM.netMx, HAW07_TAM.clusterCoef, HAW07_TAM.degreeCent$centralization,
                         HAW07_TAM.netDensity, HAW07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
HAW07_SDMg2 <- data.frame(HAW07_SDM)
HAW07_SDMg2 <- HAW07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_SDMg2$player1
player2vector <- HAW07_SDMg2$player2
HAW07_SDMg3 <- HAW07_SDMg2
HAW07_SDMg3$p1inp2vec <- is.element(HAW07_SDMg3$player1, player2vector)
HAW07_SDMg3$p2inp1vec <- is.element(HAW07_SDMg3$player2, player1vector)

addPlayer1 <- HAW07_SDMg3[ which(HAW07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_SDMg3[ which(HAW07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_SDMg2 <- rbind(HAW07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
HAW07_SDMft <- ftable(HAW07_SDMg2$player1, HAW07_SDMg2$player2)
HAW07_SDMft2 <- as.matrix(HAW07_SDMft)
numRows <- nrow(HAW07_SDMft2)
numCols <- ncol(HAW07_SDMft2)
HAW07_SDMft3 <- HAW07_SDMft2[c(2:numRows) , c(2:numCols)]
HAW07_SDMTable <- graph.adjacency(HAW07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(HAW07_SDMTable, vertex.label = V(HAW07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
HAW07_SDM.clusterCoef <- transitivity(HAW07_SDMTable, type="global") #cluster coefficient
HAW07_SDM.degreeCent <- centralization.degree(HAW07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_SDMftn <- as.network.matrix(HAW07_SDMft)
HAW07_SDM.netDensity <- network.density(HAW07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_SDM.entropy <- entropy(HAW07_SDMft) #entropy

HAW07_SDM.netMx <- cbind(HAW07_SDM.netMx, HAW07_SDM.clusterCoef, HAW07_SDM.degreeCent$centralization,
                         HAW07_SDM.netDensity, HAW07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
HAW07_TDMg2 <- data.frame(HAW07_TDM)
HAW07_TDMg2 <- HAW07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_TDMg2$player1
player2vector <- HAW07_TDMg2$player2
HAW07_TDMg3 <- HAW07_TDMg2
HAW07_TDMg3$p1inp2vec <- is.element(HAW07_TDMg3$player1, player2vector)
HAW07_TDMg3$p2inp1vec <- is.element(HAW07_TDMg3$player2, player1vector)

addPlayer1 <- HAW07_TDMg3[ which(HAW07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_TDMg3[ which(HAW07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_TDMg2 <- rbind(HAW07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
HAW07_TDMft <- ftable(HAW07_TDMg2$player1, HAW07_TDMg2$player2)
HAW07_TDMft2 <- as.matrix(HAW07_TDMft)
numRows <- nrow(HAW07_TDMft2)
numCols <- ncol(HAW07_TDMft2)
HAW07_TDMft3 <- HAW07_TDMft2[c(2:numRows) , c(2:numCols)]
HAW07_TDMTable <- graph.adjacency(HAW07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(HAW07_TDMTable, vertex.label = V(HAW07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
HAW07_TDM.clusterCoef <- transitivity(HAW07_TDMTable, type="global") #cluster coefficient
HAW07_TDM.degreeCent <- centralization.degree(HAW07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_TDMftn <- as.network.matrix(HAW07_TDMft)
HAW07_TDM.netDensity <- network.density(HAW07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_TDM.entropy <- entropy(HAW07_TDMft) #entropy

HAW07_TDM.netMx <- cbind(HAW07_TDM.netMx, HAW07_TDM.clusterCoef, HAW07_TDM.degreeCent$centralization,
                         HAW07_TDM.netDensity, HAW07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************

round = 7
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
HAW07_SDg2 <- data.frame(HAW07_SD)
HAW07_SDg2 <- HAW07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_SDg2$player1
player2vector <- HAW07_SDg2$player2
HAW07_SDg3 <- HAW07_SDg2
HAW07_SDg3$p1inp2vec <- is.element(HAW07_SDg3$player1, player2vector)
HAW07_SDg3$p2inp1vec <- is.element(HAW07_SDg3$player2, player1vector)

addPlayer1 <- HAW07_SDg3[ which(HAW07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_SDg3[ which(HAW07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_SDg2 <- rbind(HAW07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
HAW07_SDft <- ftable(HAW07_SDg2$player1, HAW07_SDg2$player2)
HAW07_SDft2 <- as.matrix(HAW07_SDft)
numRows <- nrow(HAW07_SDft2)
numCols <- ncol(HAW07_SDft2)
HAW07_SDft3 <- HAW07_SDft2[c(2:numRows) , c(2:numCols)]
HAW07_SDTable <- graph.adjacency(HAW07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(HAW07_SDTable, vertex.label = V(HAW07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
HAW07_SD.clusterCoef <- transitivity(HAW07_SDTable, type="global") #cluster coefficient
HAW07_SD.degreeCent <- centralization.degree(HAW07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_SDftn <- as.network.matrix(HAW07_SDft)
HAW07_SD.netDensity <- network.density(HAW07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_SD.entropy <- entropy(HAW07_SDft) #entropy

HAW07_SD.netMx <- cbind(HAW07_SD.netMx, HAW07_SD.clusterCoef, HAW07_SD.degreeCent$centralization,
                        HAW07_SD.netDensity, HAW07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************

round = 7
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
HAW07_TDg2 <- data.frame(HAW07_TD)
HAW07_TDg2 <- HAW07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_TDg2$player1
player2vector <- HAW07_TDg2$player2
HAW07_TDg3 <- HAW07_TDg2
HAW07_TDg3$p1inp2vec <- is.element(HAW07_TDg3$player1, player2vector)
HAW07_TDg3$p2inp1vec <- is.element(HAW07_TDg3$player2, player1vector)

addPlayer1 <- HAW07_TDg3[ which(HAW07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_TDg3[ which(HAW07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_TDg2 <- rbind(HAW07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
HAW07_TDft <- ftable(HAW07_TDg2$player1, HAW07_TDg2$player2)
HAW07_TDft2 <- as.matrix(HAW07_TDft)
numRows <- nrow(HAW07_TDft2)
numCols <- ncol(HAW07_TDft2)
HAW07_TDft3 <- HAW07_TDft2[c(2:numRows) , c(2:numCols)]
HAW07_TDTable <- graph.adjacency(HAW07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(HAW07_TDTable, vertex.label = V(HAW07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
HAW07_TD.clusterCoef <- transitivity(HAW07_TDTable, type="global") #cluster coefficient
HAW07_TD.degreeCent <- centralization.degree(HAW07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_TDftn <- as.network.matrix(HAW07_TDft)
HAW07_TD.netDensity <- network.density(HAW07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_TD.entropy <- entropy(HAW07_TDft) #entropy

HAW07_TD.netMx <- cbind(HAW07_TD.netMx, HAW07_TD.clusterCoef, HAW07_TD.degreeCent$centralization,
                        HAW07_TD.netDensity, HAW07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
HAW07_QTg2 <- data.frame(HAW07_QT)
HAW07_QTg2 <- HAW07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW07_QTg2$player1
player2vector <- HAW07_QTg2$player2
HAW07_QTg3 <- HAW07_QTg2
HAW07_QTg3$p1inp2vec <- is.element(HAW07_QTg3$player1, player2vector)
HAW07_QTg3$p2inp1vec <- is.element(HAW07_QTg3$player2, player1vector)

addPlayer1 <- HAW07_QTg3[ which(HAW07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW07_QTg3[ which(HAW07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW07_QTg2 <- rbind(HAW07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
HAW07_QTft <- ftable(HAW07_QTg2$player1, HAW07_QTg2$player2)
HAW07_QTft2 <- as.matrix(HAW07_QTft)
numRows <- nrow(HAW07_QTft2)
numCols <- ncol(HAW07_QTft2)
HAW07_QTft3 <- HAW07_QTft2[c(2:numRows) , c(2:numCols)]
HAW07_QTTable <- graph.adjacency(HAW07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(HAW07_QTTable, vertex.label = V(HAW07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
HAW07_QT.clusterCoef <- transitivity(HAW07_QTTable, type="global") #cluster coefficient
HAW07_QT.degreeCent <- centralization.degree(HAW07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW07_QTftn <- as.network.matrix(HAW07_QTft)
HAW07_QT.netDensity <- network.density(HAW07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW07_QT.entropy <- entropy(HAW07_QTft) #entropy

HAW07_QT.netMx <- cbind(HAW07_QT.netMx, HAW07_QT.clusterCoef, HAW07_QT.degreeCent$centralization,
                        HAW07_QT.netDensity, HAW07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW07_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Goal_F"
MELB07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
MELB07_Gg2 <- data.frame(MELB07_G)
MELB07_Gg2 <- MELB07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_Gg2$player1
player2vector <- MELB07_Gg2$player2
MELB07_Gg3 <- MELB07_Gg2
MELB07_Gg3$p1inp2vec <- is.element(MELB07_Gg3$player1, player2vector)
MELB07_Gg3$p2inp1vec <- is.element(MELB07_Gg3$player2, player1vector)

addPlayer1 <- MELB07_Gg3[ which(MELB07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB07_Gg2 <- rbind(MELB07_Gg2, addPlayer1)

#ROUND 7, Goal graph using weighted edges
MELB07_Gft <- ftable(MELB07_Gg2$player1, MELB07_Gg2$player2)
MELB07_Gft2 <- as.matrix(MELB07_Gft)
numRows <- nrow(MELB07_Gft2)
numCols <- ncol(MELB07_Gft2)
MELB07_Gft3 <- MELB07_Gft2[c(2:numRows) , c(1:numCols)]
MELB07_GTable <- graph.adjacency(MELB07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(MELB07_GTable, vertex.label = V(MELB07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
MELB07_G.clusterCoef <- transitivity(MELB07_GTable, type="global") #cluster coefficient
MELB07_G.degreeCent <- centralization.degree(MELB07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_Gftn <- as.network.matrix(MELB07_Gft)
MELB07_G.netDensity <- network.density(MELB07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_G.entropy <- entropy(MELB07_Gft) #entropy

MELB07_G.netMx <- cbind(MELB07_G.netMx, MELB07_G.clusterCoef, MELB07_G.degreeCent$centralization,
                        MELB07_G.netDensity, MELB07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************

round = 7
teamName = "MELB"
KIoutcome = "Behind_F"
MELB07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
MELB07_Bg2 <- data.frame(MELB07_B)
MELB07_Bg2 <- MELB07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_Bg2$player1
player2vector <- MELB07_Bg2$player2
MELB07_Bg3 <- MELB07_Bg2
MELB07_Bg3$p1inp2vec <- is.element(MELB07_Bg3$player1, player2vector)
MELB07_Bg3$p2inp1vec <- is.element(MELB07_Bg3$player2, player1vector)

addPlayer1 <- MELB07_Bg3[ which(MELB07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_Bg3[ which(MELB07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_Bg2 <- rbind(MELB07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
MELB07_Bft <- ftable(MELB07_Bg2$player1, MELB07_Bg2$player2)
MELB07_Bft2 <- as.matrix(MELB07_Bft)
numRows <- nrow(MELB07_Bft2)
numCols <- ncol(MELB07_Bft2)
MELB07_Bft3 <- MELB07_Bft2[c(2:numRows) , c(2:numCols)]
MELB07_BTable <- graph.adjacency(MELB07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(MELB07_BTable, vertex.label = V(MELB07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
MELB07_B.clusterCoef <- transitivity(MELB07_BTable, type="global") #cluster coefficient
MELB07_B.degreeCent <- centralization.degree(MELB07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_Bftn <- as.network.matrix(MELB07_Bft)
MELB07_B.netDensity <- network.density(MELB07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_B.entropy <- entropy(MELB07_Bft) #entropy

MELB07_B.netMx <- cbind(MELB07_B.netMx, MELB07_B.clusterCoef, MELB07_B.degreeCent$centralization,
                        MELB07_B.netDensity, MELB07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
MELB07_SFg2 <- data.frame(MELB07_SF)
MELB07_SFg2 <- MELB07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_SFg2$player1
player2vector <- MELB07_SFg2$player2
MELB07_SFg3 <- MELB07_SFg2
MELB07_SFg3$p1inp2vec <- is.element(MELB07_SFg3$player1, player2vector)
MELB07_SFg3$p2inp1vec <- is.element(MELB07_SFg3$player2, player1vector)

addPlayer1 <- MELB07_SFg3[ which(MELB07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_SFg3[ which(MELB07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_SFg2 <- rbind(MELB07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
MELB07_SFft <- ftable(MELB07_SFg2$player1, MELB07_SFg2$player2)
MELB07_SFft2 <- as.matrix(MELB07_SFft)
numRows <- nrow(MELB07_SFft2)
numCols <- ncol(MELB07_SFft2)
MELB07_SFft3 <- MELB07_SFft2[c(2:numRows) , c(2:numCols)]
MELB07_SFTable <- graph.adjacency(MELB07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(MELB07_SFTable, vertex.label = V(MELB07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
MELB07_SF.clusterCoef <- transitivity(MELB07_SFTable, type="global") #cluster coefficient
MELB07_SF.degreeCent <- centralization.degree(MELB07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_SFftn <- as.network.matrix(MELB07_SFft)
MELB07_SF.netDensity <- network.density(MELB07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_SF.entropy <- entropy(MELB07_SFft) #entropy

MELB07_SF.netMx <- cbind(MELB07_SF.netMx, MELB07_SF.clusterCoef, MELB07_SF.degreeCent$centralization,
                         MELB07_SF.netDensity, MELB07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
MELB07_TFg2 <- data.frame(MELB07_TF)
MELB07_TFg2 <- MELB07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_TFg2$player1
player2vector <- MELB07_TFg2$player2
MELB07_TFg3 <- MELB07_TFg2
MELB07_TFg3$p1inp2vec <- is.element(MELB07_TFg3$player1, player2vector)
MELB07_TFg3$p2inp1vec <- is.element(MELB07_TFg3$player2, player1vector)

addPlayer1 <- MELB07_TFg3[ which(MELB07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_TFg3[ which(MELB07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_TFg2 <- rbind(MELB07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
MELB07_TFft <- ftable(MELB07_TFg2$player1, MELB07_TFg2$player2)
MELB07_TFft2 <- as.matrix(MELB07_TFft)
numRows <- nrow(MELB07_TFft2)
numCols <- ncol(MELB07_TFft2)
MELB07_TFft3 <- MELB07_TFft2[c(2:numRows) , c(2:numCols)]
MELB07_TFTable <- graph.adjacency(MELB07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(MELB07_TFTable, vertex.label = V(MELB07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
MELB07_TF.clusterCoef <- transitivity(MELB07_TFTable, type="global") #cluster coefficient
MELB07_TF.degreeCent <- centralization.degree(MELB07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_TFftn <- as.network.matrix(MELB07_TFft)
MELB07_TF.netDensity <- network.density(MELB07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_TF.entropy <- entropy(MELB07_TFft) #entropy

MELB07_TF.netMx <- cbind(MELB07_TF.netMx, MELB07_TF.clusterCoef, MELB07_TF.degreeCent$centralization,
                         MELB07_TF.netDensity, MELB07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
MELB07_SAMg2 <- data.frame(MELB07_SAM)
MELB07_SAMg2 <- MELB07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_SAMg2$player1
player2vector <- MELB07_SAMg2$player2
MELB07_SAMg3 <- MELB07_SAMg2
MELB07_SAMg3$p1inp2vec <- is.element(MELB07_SAMg3$player1, player2vector)
MELB07_SAMg3$p2inp1vec <- is.element(MELB07_SAMg3$player2, player1vector)

addPlayer1 <- MELB07_SAMg3[ which(MELB07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_SAMg3[ which(MELB07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_SAMg2 <- rbind(MELB07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
MELB07_SAMft <- ftable(MELB07_SAMg2$player1, MELB07_SAMg2$player2)
MELB07_SAMft2 <- as.matrix(MELB07_SAMft)
numRows <- nrow(MELB07_SAMft2)
numCols <- ncol(MELB07_SAMft2)
MELB07_SAMft3 <- MELB07_SAMft2[c(2:numRows) , c(2:numCols)]
MELB07_SAMTable <- graph.adjacency(MELB07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(MELB07_SAMTable, vertex.label = V(MELB07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
MELB07_SAM.clusterCoef <- transitivity(MELB07_SAMTable, type="global") #cluster coefficient
MELB07_SAM.degreeCent <- centralization.degree(MELB07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_SAMftn <- as.network.matrix(MELB07_SAMft)
MELB07_SAM.netDensity <- network.density(MELB07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_SAM.entropy <- entropy(MELB07_SAMft) #entropy

MELB07_SAM.netMx <- cbind(MELB07_SAM.netMx, MELB07_SAM.clusterCoef, MELB07_SAM.degreeCent$centralization,
                          MELB07_SAM.netDensity, MELB07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
MELB07_TAMg2 <- data.frame(MELB07_TAM)
MELB07_TAMg2 <- MELB07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_TAMg2$player1
player2vector <- MELB07_TAMg2$player2
MELB07_TAMg3 <- MELB07_TAMg2
MELB07_TAMg3$p1inp2vec <- is.element(MELB07_TAMg3$player1, player2vector)
MELB07_TAMg3$p2inp1vec <- is.element(MELB07_TAMg3$player2, player1vector)

addPlayer1 <- MELB07_TAMg3[ which(MELB07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_TAMg3[ which(MELB07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_TAMg2 <- rbind(MELB07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
MELB07_TAMft <- ftable(MELB07_TAMg2$player1, MELB07_TAMg2$player2)
MELB07_TAMft2 <- as.matrix(MELB07_TAMft)
numRows <- nrow(MELB07_TAMft2)
numCols <- ncol(MELB07_TAMft2)
MELB07_TAMft3 <- MELB07_TAMft2[c(2:numRows) , c(2:numCols)]
MELB07_TAMTable <- graph.adjacency(MELB07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(MELB07_TAMTable, vertex.label = V(MELB07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
MELB07_TAM.clusterCoef <- transitivity(MELB07_TAMTable, type="global") #cluster coefficient
MELB07_TAM.degreeCent <- centralization.degree(MELB07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_TAMftn <- as.network.matrix(MELB07_TAMft)
MELB07_TAM.netDensity <- network.density(MELB07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_TAM.entropy <- entropy(MELB07_TAMft) #entropy

MELB07_TAM.netMx <- cbind(MELB07_TAM.netMx, MELB07_TAM.clusterCoef, MELB07_TAM.degreeCent$centralization,
                          MELB07_TAM.netDensity, MELB07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
MELB07_SDMg2 <- data.frame(MELB07_SDM)
MELB07_SDMg2 <- MELB07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_SDMg2$player1
player2vector <- MELB07_SDMg2$player2
MELB07_SDMg3 <- MELB07_SDMg2
MELB07_SDMg3$p1inp2vec <- is.element(MELB07_SDMg3$player1, player2vector)
MELB07_SDMg3$p2inp1vec <- is.element(MELB07_SDMg3$player2, player1vector)

addPlayer1 <- MELB07_SDMg3[ which(MELB07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_SDMg3[ which(MELB07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_SDMg2 <- rbind(MELB07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
MELB07_SDMft <- ftable(MELB07_SDMg2$player1, MELB07_SDMg2$player2)
MELB07_SDMft2 <- as.matrix(MELB07_SDMft)
numRows <- nrow(MELB07_SDMft2)
numCols <- ncol(MELB07_SDMft2)
MELB07_SDMft3 <- MELB07_SDMft2[c(2:numRows) , c(2:numCols)]
MELB07_SDMTable <- graph.adjacency(MELB07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(MELB07_SDMTable, vertex.label = V(MELB07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
MELB07_SDM.clusterCoef <- transitivity(MELB07_SDMTable, type="global") #cluster coefficient
MELB07_SDM.degreeCent <- centralization.degree(MELB07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_SDMftn <- as.network.matrix(MELB07_SDMft)
MELB07_SDM.netDensity <- network.density(MELB07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_SDM.entropy <- entropy(MELB07_SDMft) #entropy

MELB07_SDM.netMx <- cbind(MELB07_SDM.netMx, MELB07_SDM.clusterCoef, MELB07_SDM.degreeCent$centralization,
                          MELB07_SDM.netDensity, MELB07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
MELB07_TDMg2 <- data.frame(MELB07_TDM)
MELB07_TDMg2 <- MELB07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_TDMg2$player1
player2vector <- MELB07_TDMg2$player2
MELB07_TDMg3 <- MELB07_TDMg2
MELB07_TDMg3$p1inp2vec <- is.element(MELB07_TDMg3$player1, player2vector)
MELB07_TDMg3$p2inp1vec <- is.element(MELB07_TDMg3$player2, player1vector)

addPlayer1 <- MELB07_TDMg3[ which(MELB07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_TDMg3[ which(MELB07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_TDMg2 <- rbind(MELB07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
MELB07_TDMft <- ftable(MELB07_TDMg2$player1, MELB07_TDMg2$player2)
MELB07_TDMft2 <- as.matrix(MELB07_TDMft)
numRows <- nrow(MELB07_TDMft2)
numCols <- ncol(MELB07_TDMft2)
MELB07_TDMft3 <- MELB07_TDMft2[c(2:numRows) , c(2:numCols)]
MELB07_TDMTable <- graph.adjacency(MELB07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(MELB07_TDMTable, vertex.label = V(MELB07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
MELB07_TDM.clusterCoef <- transitivity(MELB07_TDMTable, type="global") #cluster coefficient
MELB07_TDM.degreeCent <- centralization.degree(MELB07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_TDMftn <- as.network.matrix(MELB07_TDMft)
MELB07_TDM.netDensity <- network.density(MELB07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_TDM.entropy <- entropy(MELB07_TDMft) #entropy

MELB07_TDM.netMx <- cbind(MELB07_TDM.netMx, MELB07_TDM.clusterCoef, MELB07_TDM.degreeCent$centralization,
                          MELB07_TDM.netDensity, MELB07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
MELB07_SDg2 <- data.frame(MELB07_SD)
MELB07_SDg2 <- MELB07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_SDg2$player1
player2vector <- MELB07_SDg2$player2
MELB07_SDg3 <- MELB07_SDg2
MELB07_SDg3$p1inp2vec <- is.element(MELB07_SDg3$player1, player2vector)
MELB07_SDg3$p2inp1vec <- is.element(MELB07_SDg3$player2, player1vector)

addPlayer1 <- MELB07_SDg3[ which(MELB07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_SDg3[ which(MELB07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_SDg2 <- rbind(MELB07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
MELB07_SDft <- ftable(MELB07_SDg2$player1, MELB07_SDg2$player2)
MELB07_SDft2 <- as.matrix(MELB07_SDft)
numRows <- nrow(MELB07_SDft2)
numCols <- ncol(MELB07_SDft2)
MELB07_SDft3 <- MELB07_SDft2[c(2:numRows) , c(2:numCols)]
MELB07_SDTable <- graph.adjacency(MELB07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(MELB07_SDTable, vertex.label = V(MELB07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
MELB07_SD.clusterCoef <- transitivity(MELB07_SDTable, type="global") #cluster coefficient
MELB07_SD.degreeCent <- centralization.degree(MELB07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_SDftn <- as.network.matrix(MELB07_SDft)
MELB07_SD.netDensity <- network.density(MELB07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_SD.entropy <- entropy(MELB07_SDft) #entropy

MELB07_SD.netMx <- cbind(MELB07_SD.netMx, MELB07_SD.clusterCoef, MELB07_SD.degreeCent$centralization,
                         MELB07_SD.netDensity, MELB07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
MELB07_TDg2 <- data.frame(MELB07_TD)
MELB07_TDg2 <- MELB07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_TDg2$player1
player2vector <- MELB07_TDg2$player2
MELB07_TDg3 <- MELB07_TDg2
MELB07_TDg3$p1inp2vec <- is.element(MELB07_TDg3$player1, player2vector)
MELB07_TDg3$p2inp1vec <- is.element(MELB07_TDg3$player2, player1vector)

addPlayer1 <- MELB07_TDg3[ which(MELB07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_TDg3[ which(MELB07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_TDg2 <- rbind(MELB07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
MELB07_TDft <- ftable(MELB07_TDg2$player1, MELB07_TDg2$player2)
MELB07_TDft2 <- as.matrix(MELB07_TDft)
numRows <- nrow(MELB07_TDft2)
numCols <- ncol(MELB07_TDft2)
MELB07_TDft3 <- MELB07_TDft2[c(2:numRows) , c(2:numCols)]
MELB07_TDTable <- graph.adjacency(MELB07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(MELB07_TDTable, vertex.label = V(MELB07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
MELB07_TD.clusterCoef <- transitivity(MELB07_TDTable, type="global") #cluster coefficient
MELB07_TD.degreeCent <- centralization.degree(MELB07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_TDftn <- as.network.matrix(MELB07_TDft)
MELB07_TD.netDensity <- network.density(MELB07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_TD.entropy <- entropy(MELB07_TDft) #entropy

MELB07_TD.netMx <- cbind(MELB07_TD.netMx, MELB07_TD.clusterCoef, MELB07_TD.degreeCent$centralization,
                         MELB07_TD.netDensity, MELB07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
MELB07_QTg2 <- data.frame(MELB07_QT)
MELB07_QTg2 <- MELB07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB07_QTg2$player1
player2vector <- MELB07_QTg2$player2
MELB07_QTg3 <- MELB07_QTg2
MELB07_QTg3$p1inp2vec <- is.element(MELB07_QTg3$player1, player2vector)
MELB07_QTg3$p2inp1vec <- is.element(MELB07_QTg3$player2, player1vector)

addPlayer1 <- MELB07_QTg3[ which(MELB07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB07_QTg3[ which(MELB07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB07_QTg2 <- rbind(MELB07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
MELB07_QTft <- ftable(MELB07_QTg2$player1, MELB07_QTg2$player2)
MELB07_QTft2 <- as.matrix(MELB07_QTft)
numRows <- nrow(MELB07_QTft2)
numCols <- ncol(MELB07_QTft2)
MELB07_QTft3 <- MELB07_QTft2[c(2:numRows) , c(2:numCols)]
MELB07_QTTable <- graph.adjacency(MELB07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(MELB07_QTTable, vertex.label = V(MELB07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
MELB07_QT.clusterCoef <- transitivity(MELB07_QTTable, type="global") #cluster coefficient
MELB07_QT.degreeCent <- centralization.degree(MELB07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB07_QTftn <- as.network.matrix(MELB07_QTft)
MELB07_QT.netDensity <- network.density(MELB07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB07_QT.entropy <- entropy(MELB07_QTft) #entropy

MELB07_QT.netMx <- cbind(MELB07_QT.netMx, MELB07_QT.clusterCoef, MELB07_QT.degreeCent$centralization,
                         MELB07_QT.netDensity, MELB07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB07_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
NMFC07_Gg2 <- data.frame(NMFC07_G)
NMFC07_Gg2 <- NMFC07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_Gg2$player1
player2vector <- NMFC07_Gg2$player2
NMFC07_Gg3 <- NMFC07_Gg2
NMFC07_Gg3$p1inp2vec <- is.element(NMFC07_Gg3$player1, player2vector)
NMFC07_Gg3$p2inp1vec <- is.element(NMFC07_Gg3$player2, player1vector)

addPlayer1 <- NMFC07_Gg3[ which(NMFC07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_Gg3[ which(NMFC07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_Gg2 <- rbind(NMFC07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
NMFC07_Gft <- ftable(NMFC07_Gg2$player1, NMFC07_Gg2$player2)
NMFC07_Gft2 <- as.matrix(NMFC07_Gft)
numRows <- nrow(NMFC07_Gft2)
numCols <- ncol(NMFC07_Gft2)
NMFC07_Gft3 <- NMFC07_Gft2[c(2:numRows) , c(2:numCols)]
NMFC07_GTable <- graph.adjacency(NMFC07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(NMFC07_GTable, vertex.label = V(NMFC07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
NMFC07_G.clusterCoef <- transitivity(NMFC07_GTable, type="global") #cluster coefficient
NMFC07_G.degreeCent <- centralization.degree(NMFC07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_Gftn <- as.network.matrix(NMFC07_Gft)
NMFC07_G.netDensity <- network.density(NMFC07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_G.entropy <- entropy(NMFC07_Gft) #entropy

NMFC07_G.netMx <- cbind(NMFC07_G.netMx, NMFC07_G.clusterCoef, NMFC07_G.degreeCent$centralization,
                        NMFC07_G.netDensity, NMFC07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
NMFC07_Bg2 <- data.frame(NMFC07_B)
NMFC07_Bg2 <- NMFC07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_Bg2$player1
player2vector <- NMFC07_Bg2$player2
NMFC07_Bg3 <- NMFC07_Bg2
NMFC07_Bg3$p1inp2vec <- is.element(NMFC07_Bg3$player1, player2vector)
NMFC07_Bg3$p2inp1vec <- is.element(NMFC07_Bg3$player2, player1vector)

addPlayer1 <- NMFC07_Bg3[ which(NMFC07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_Bg3[ which(NMFC07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_Bg2 <- rbind(NMFC07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
NMFC07_Bft <- ftable(NMFC07_Bg2$player1, NMFC07_Bg2$player2)
NMFC07_Bft2 <- as.matrix(NMFC07_Bft)
numRows <- nrow(NMFC07_Bft2)
numCols <- ncol(NMFC07_Bft2)
NMFC07_Bft3 <- NMFC07_Bft2[c(2:numRows) , c(2:numCols)]
NMFC07_BTable <- graph.adjacency(NMFC07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(NMFC07_BTable, vertex.label = V(NMFC07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
NMFC07_B.clusterCoef <- transitivity(NMFC07_BTable, type="global") #cluster coefficient
NMFC07_B.degreeCent <- centralization.degree(NMFC07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_Bftn <- as.network.matrix(NMFC07_Bft)
NMFC07_B.netDensity <- network.density(NMFC07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_B.entropy <- entropy(NMFC07_Bft) #entropy

NMFC07_B.netMx <- cbind(NMFC07_B.netMx, NMFC07_B.clusterCoef, NMFC07_B.degreeCent$centralization,
                        NMFC07_B.netDensity, NMFC07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
NMFC07_SFg2 <- data.frame(NMFC07_SF)
NMFC07_SFg2 <- NMFC07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_SFg2$player1
player2vector <- NMFC07_SFg2$player2
NMFC07_SFg3 <- NMFC07_SFg2
NMFC07_SFg3$p1inp2vec <- is.element(NMFC07_SFg3$player1, player2vector)
NMFC07_SFg3$p2inp1vec <- is.element(NMFC07_SFg3$player2, player1vector)

addPlayer1 <- NMFC07_SFg3[ which(NMFC07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_SFg3[ which(NMFC07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_SFg2 <- rbind(NMFC07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
NMFC07_SFft <- ftable(NMFC07_SFg2$player1, NMFC07_SFg2$player2)
NMFC07_SFft2 <- as.matrix(NMFC07_SFft)
numRows <- nrow(NMFC07_SFft2)
numCols <- ncol(NMFC07_SFft2)
NMFC07_SFft3 <- NMFC07_SFft2[c(2:numRows) , c(2:numCols)]
NMFC07_SFTable <- graph.adjacency(NMFC07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(NMFC07_SFTable, vertex.label = V(NMFC07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
NMFC07_SF.clusterCoef <- transitivity(NMFC07_SFTable, type="global") #cluster coefficient
NMFC07_SF.degreeCent <- centralization.degree(NMFC07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_SFftn <- as.network.matrix(NMFC07_SFft)
NMFC07_SF.netDensity <- network.density(NMFC07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_SF.entropy <- entropy(NMFC07_SFft) #entropy

NMFC07_SF.netMx <- cbind(NMFC07_SF.netMx, NMFC07_SF.clusterCoef, NMFC07_SF.degreeCent$centralization,
                         NMFC07_SF.netDensity, NMFC07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
NMFC07_TFg2 <- data.frame(NMFC07_TF)
NMFC07_TFg2 <- NMFC07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_TFg2$player1
player2vector <- NMFC07_TFg2$player2
NMFC07_TFg3 <- NMFC07_TFg2
NMFC07_TFg3$p1inp2vec <- is.element(NMFC07_TFg3$player1, player2vector)
NMFC07_TFg3$p2inp1vec <- is.element(NMFC07_TFg3$player2, player1vector)

addPlayer1 <- NMFC07_TFg3[ which(NMFC07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC07_TFg3[ which(NMFC07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_TFg2 <- rbind(NMFC07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
NMFC07_TFft <- ftable(NMFC07_TFg2$player1, NMFC07_TFg2$player2)
NMFC07_TFft2 <- as.matrix(NMFC07_TFft)
numRows <- nrow(NMFC07_TFft2)
numCols <- ncol(NMFC07_TFft2)
NMFC07_TFft3 <- NMFC07_TFft2[c(2:numRows) , c(2:numCols)]
NMFC07_TFTable <- graph.adjacency(NMFC07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(NMFC07_TFTable, vertex.label = V(NMFC07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
NMFC07_TF.clusterCoef <- transitivity(NMFC07_TFTable, type="global") #cluster coefficient
NMFC07_TF.degreeCent <- centralization.degree(NMFC07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_TFftn <- as.network.matrix(NMFC07_TFft)
NMFC07_TF.netDensity <- network.density(NMFC07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_TF.entropy <- entropy(NMFC07_TFft) #entropy

NMFC07_TF.netMx <- cbind(NMFC07_TF.netMx, NMFC07_TF.clusterCoef, NMFC07_TF.degreeCent$centralization,
                         NMFC07_TF.netDensity, NMFC07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
NMFC07_SAMg2 <- data.frame(NMFC07_SAM)
NMFC07_SAMg2 <- NMFC07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_SAMg2$player1
player2vector <- NMFC07_SAMg2$player2
NMFC07_SAMg3 <- NMFC07_SAMg2
NMFC07_SAMg3$p1inp2vec <- is.element(NMFC07_SAMg3$player1, player2vector)
NMFC07_SAMg3$p2inp1vec <- is.element(NMFC07_SAMg3$player2, player1vector)

addPlayer1 <- NMFC07_SAMg3[ which(NMFC07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_SAMg3[ which(NMFC07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_SAMg2 <- rbind(NMFC07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
NMFC07_SAMft <- ftable(NMFC07_SAMg2$player1, NMFC07_SAMg2$player2)
NMFC07_SAMft2 <- as.matrix(NMFC07_SAMft)
numRows <- nrow(NMFC07_SAMft2)
numCols <- ncol(NMFC07_SAMft2)
NMFC07_SAMft3 <- NMFC07_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC07_SAMTable <- graph.adjacency(NMFC07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(NMFC07_SAMTable, vertex.label = V(NMFC07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
NMFC07_SAM.clusterCoef <- transitivity(NMFC07_SAMTable, type="global") #cluster coefficient
NMFC07_SAM.degreeCent <- centralization.degree(NMFC07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_SAMftn <- as.network.matrix(NMFC07_SAMft)
NMFC07_SAM.netDensity <- network.density(NMFC07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_SAM.entropy <- entropy(NMFC07_SAMft) #entropy

NMFC07_SAM.netMx <- cbind(NMFC07_SAM.netMx, NMFC07_SAM.clusterCoef, NMFC07_SAM.degreeCent$centralization,
                          NMFC07_SAM.netDensity, NMFC07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
NMFC07_TAMg2 <- data.frame(NMFC07_TAM)
NMFC07_TAMg2 <- NMFC07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_TAMg2$player1
player2vector <- NMFC07_TAMg2$player2
NMFC07_TAMg3 <- NMFC07_TAMg2
NMFC07_TAMg3$p1inp2vec <- is.element(NMFC07_TAMg3$player1, player2vector)
NMFC07_TAMg3$p2inp1vec <- is.element(NMFC07_TAMg3$player2, player1vector)

addPlayer1 <- NMFC07_TAMg3[ which(NMFC07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_TAMg3[ which(NMFC07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_TAMg2 <- rbind(NMFC07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
NMFC07_TAMft <- ftable(NMFC07_TAMg2$player1, NMFC07_TAMg2$player2)
NMFC07_TAMft2 <- as.matrix(NMFC07_TAMft)
numRows <- nrow(NMFC07_TAMft2)
numCols <- ncol(NMFC07_TAMft2)
NMFC07_TAMft3 <- NMFC07_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC07_TAMTable <- graph.adjacency(NMFC07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(NMFC07_TAMTable, vertex.label = V(NMFC07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
NMFC07_TAM.clusterCoef <- transitivity(NMFC07_TAMTable, type="global") #cluster coefficient
NMFC07_TAM.degreeCent <- centralization.degree(NMFC07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_TAMftn <- as.network.matrix(NMFC07_TAMft)
NMFC07_TAM.netDensity <- network.density(NMFC07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_TAM.entropy <- entropy(NMFC07_TAMft) #entropy

NMFC07_TAM.netMx <- cbind(NMFC07_TAM.netMx, NMFC07_TAM.clusterCoef, NMFC07_TAM.degreeCent$centralization,
                          NMFC07_TAM.netDensity, NMFC07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
NMFC07_SDMg2 <- data.frame(NMFC07_SDM)
NMFC07_SDMg2 <- NMFC07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_SDMg2$player1
player2vector <- NMFC07_SDMg2$player2
NMFC07_SDMg3 <- NMFC07_SDMg2
NMFC07_SDMg3$p1inp2vec <- is.element(NMFC07_SDMg3$player1, player2vector)
NMFC07_SDMg3$p2inp1vec <- is.element(NMFC07_SDMg3$player2, player1vector)

addPlayer1 <- NMFC07_SDMg3[ which(NMFC07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_SDMg3[ which(NMFC07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_SDMg2 <- rbind(NMFC07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
NMFC07_SDMft <- ftable(NMFC07_SDMg2$player1, NMFC07_SDMg2$player2)
NMFC07_SDMft2 <- as.matrix(NMFC07_SDMft)
numRows <- nrow(NMFC07_SDMft2)
numCols <- ncol(NMFC07_SDMft2)
NMFC07_SDMft3 <- NMFC07_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC07_SDMTable <- graph.adjacency(NMFC07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(NMFC07_SDMTable, vertex.label = V(NMFC07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
NMFC07_SDM.clusterCoef <- transitivity(NMFC07_SDMTable, type="global") #cluster coefficient
NMFC07_SDM.degreeCent <- centralization.degree(NMFC07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_SDMftn <- as.network.matrix(NMFC07_SDMft)
NMFC07_SDM.netDensity <- network.density(NMFC07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_SDM.entropy <- entropy(NMFC07_SDMft) #entropy

NMFC07_SDM.netMx <- cbind(NMFC07_SDM.netMx, NMFC07_SDM.clusterCoef, NMFC07_SDM.degreeCent$centralization,
                          NMFC07_SDM.netDensity, NMFC07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************
#NA
round = 7
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
NMFC07_TDMg2 <- data.frame(NMFC07_TDM)
NMFC07_TDMg2 <- NMFC07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_TDMg2$player1
player2vector <- NMFC07_TDMg2$player2
NMFC07_TDMg3 <- NMFC07_TDMg2
NMFC07_TDMg3$p1inp2vec <- is.element(NMFC07_TDMg3$player1, player2vector)
NMFC07_TDMg3$p2inp1vec <- is.element(NMFC07_TDMg3$player2, player1vector)

addPlayer1 <- NMFC07_TDMg3[ which(NMFC07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_TDMg3[ which(NMFC07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_TDMg2 <- rbind(NMFC07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
NMFC07_TDMft <- ftable(NMFC07_TDMg2$player1, NMFC07_TDMg2$player2)
NMFC07_TDMft2 <- as.matrix(NMFC07_TDMft)
numRows <- nrow(NMFC07_TDMft2)
numCols <- ncol(NMFC07_TDMft2)
NMFC07_TDMft3 <- NMFC07_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC07_TDMTable <- graph.adjacency(NMFC07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(NMFC07_TDMTable, vertex.label = V(NMFC07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
NMFC07_TDM.clusterCoef <- transitivity(NMFC07_TDMTable, type="global") #cluster coefficient
NMFC07_TDM.degreeCent <- centralization.degree(NMFC07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_TDMftn <- as.network.matrix(NMFC07_TDMft)
NMFC07_TDM.netDensity <- network.density(NMFC07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_TDM.entropy <- entropy(NMFC07_TDMft) #entropy

NMFC07_TDM.netMx <- cbind(NMFC07_TDM.netMx, NMFC07_TDM.clusterCoef, NMFC07_TDM.degreeCent$centralization,
                          NMFC07_TDM.netDensity, NMFC07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
NMFC07_SDg2 <- data.frame(NMFC07_SD)
NMFC07_SDg2 <- NMFC07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_SDg2$player1
player2vector <- NMFC07_SDg2$player2
NMFC07_SDg3 <- NMFC07_SDg2
NMFC07_SDg3$p1inp2vec <- is.element(NMFC07_SDg3$player1, player2vector)
NMFC07_SDg3$p2inp1vec <- is.element(NMFC07_SDg3$player2, player1vector)

addPlayer1 <- NMFC07_SDg3[ which(NMFC07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_SDg3[ which(NMFC07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_SDg2 <- rbind(NMFC07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
NMFC07_SDft <- ftable(NMFC07_SDg2$player1, NMFC07_SDg2$player2)
NMFC07_SDft2 <- as.matrix(NMFC07_SDft)
numRows <- nrow(NMFC07_SDft2)
numCols <- ncol(NMFC07_SDft2)
NMFC07_SDft3 <- NMFC07_SDft2[c(2:numRows) , c(2:numCols)]
NMFC07_SDTable <- graph.adjacency(NMFC07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(NMFC07_SDTable, vertex.label = V(NMFC07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
NMFC07_SD.clusterCoef <- transitivity(NMFC07_SDTable, type="global") #cluster coefficient
NMFC07_SD.degreeCent <- centralization.degree(NMFC07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_SDftn <- as.network.matrix(NMFC07_SDft)
NMFC07_SD.netDensity <- network.density(NMFC07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_SD.entropy <- entropy(NMFC07_SDft) #entropy

NMFC07_SD.netMx <- cbind(NMFC07_SD.netMx, NMFC07_SD.clusterCoef, NMFC07_SD.degreeCent$centralization,
                         NMFC07_SD.netDensity, NMFC07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
NMFC07_TDg2 <- data.frame(NMFC07_TD)
NMFC07_TDg2 <- NMFC07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_TDg2$player1
player2vector <- NMFC07_TDg2$player2
NMFC07_TDg3 <- NMFC07_TDg2
NMFC07_TDg3$p1inp2vec <- is.element(NMFC07_TDg3$player1, player2vector)
NMFC07_TDg3$p2inp1vec <- is.element(NMFC07_TDg3$player2, player1vector)

addPlayer1 <- NMFC07_TDg3[ which(NMFC07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_TDg3[ which(NMFC07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_TDg2 <- rbind(NMFC07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
NMFC07_TDft <- ftable(NMFC07_TDg2$player1, NMFC07_TDg2$player2)
NMFC07_TDft2 <- as.matrix(NMFC07_TDft)
numRows <- nrow(NMFC07_TDft2)
numCols <- ncol(NMFC07_TDft2)
NMFC07_TDft3 <- NMFC07_TDft2[c(2:numRows) , c(2:numCols)]
NMFC07_TDTable <- graph.adjacency(NMFC07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(NMFC07_TDTable, vertex.label = V(NMFC07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
NMFC07_TD.clusterCoef <- transitivity(NMFC07_TDTable, type="global") #cluster coefficient
NMFC07_TD.degreeCent <- centralization.degree(NMFC07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_TDftn <- as.network.matrix(NMFC07_TDft)
NMFC07_TD.netDensity <- network.density(NMFC07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_TD.entropy <- entropy(NMFC07_TDft) #entropy

NMFC07_TD.netMx <- cbind(NMFC07_TD.netMx, NMFC07_TD.clusterCoef, NMFC07_TD.degreeCent$centralization,
                         NMFC07_TD.netDensity, NMFC07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
NMFC07_QTg2 <- data.frame(NMFC07_QT)
NMFC07_QTg2 <- NMFC07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC07_QTg2$player1
player2vector <- NMFC07_QTg2$player2
NMFC07_QTg3 <- NMFC07_QTg2
NMFC07_QTg3$p1inp2vec <- is.element(NMFC07_QTg3$player1, player2vector)
NMFC07_QTg3$p2inp1vec <- is.element(NMFC07_QTg3$player2, player1vector)

addPlayer1 <- NMFC07_QTg3[ which(NMFC07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC07_QTg3[ which(NMFC07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC07_QTg2 <- rbind(NMFC07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
NMFC07_QTft <- ftable(NMFC07_QTg2$player1, NMFC07_QTg2$player2)
NMFC07_QTft2 <- as.matrix(NMFC07_QTft)
numRows <- nrow(NMFC07_QTft2)
numCols <- ncol(NMFC07_QTft2)
NMFC07_QTft3 <- NMFC07_QTft2[c(2:numRows) , c(2:numCols)]
NMFC07_QTTable <- graph.adjacency(NMFC07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(NMFC07_QTTable, vertex.label = V(NMFC07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
NMFC07_QT.clusterCoef <- transitivity(NMFC07_QTTable, type="global") #cluster coefficient
NMFC07_QT.degreeCent <- centralization.degree(NMFC07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC07_QTftn <- as.network.matrix(NMFC07_QTft)
NMFC07_QT.netDensity <- network.density(NMFC07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC07_QT.entropy <- entropy(NMFC07_QTft) #entropy

NMFC07_QT.netMx <- cbind(NMFC07_QT.netMx, NMFC07_QT.clusterCoef, NMFC07_QT.degreeCent$centralization,
                         NMFC07_QT.netDensity, NMFC07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC07_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Goal_F"
PORT07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
PORT07_Gg2 <- data.frame(PORT07_G)
PORT07_Gg2 <- PORT07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_Gg2$player1
player2vector <- PORT07_Gg2$player2
PORT07_Gg3 <- PORT07_Gg2
PORT07_Gg3$p1inp2vec <- is.element(PORT07_Gg3$player1, player2vector)
PORT07_Gg3$p2inp1vec <- is.element(PORT07_Gg3$player2, player1vector)

addPlayer1 <- PORT07_Gg3[ which(PORT07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT07_Gg2 <- rbind(PORT07_Gg2, addPlayer1)

#ROUND 7, Goal graph using weighted edges
PORT07_Gft <- ftable(PORT07_Gg2$player1, PORT07_Gg2$player2)
PORT07_Gft2 <- as.matrix(PORT07_Gft)
numRows <- nrow(PORT07_Gft2)
numCols <- ncol(PORT07_Gft2)
PORT07_Gft3 <- PORT07_Gft2[c(2:numRows) , c(1:numCols)]
PORT07_GTable <- graph.adjacency(PORT07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(PORT07_GTable, vertex.label = V(PORT07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
PORT07_G.clusterCoef <- transitivity(PORT07_GTable, type="global") #cluster coefficient
PORT07_G.degreeCent <- centralization.degree(PORT07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_Gftn <- as.network.matrix(PORT07_Gft)
PORT07_G.netDensity <- network.density(PORT07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_G.entropy <- entropy(PORT07_Gft) #entropy

PORT07_G.netMx <- cbind(PORT07_G.netMx, PORT07_G.clusterCoef, PORT07_G.degreeCent$centralization,
                        PORT07_G.netDensity, PORT07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Behind_F"
PORT07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
PORT07_Bg2 <- data.frame(PORT07_B)
PORT07_Bg2 <- PORT07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_Bg2$player1
player2vector <- PORT07_Bg2$player2
PORT07_Bg3 <- PORT07_Bg2
PORT07_Bg3$p1inp2vec <- is.element(PORT07_Bg3$player1, player2vector)
PORT07_Bg3$p2inp1vec <- is.element(PORT07_Bg3$player2, player1vector)

addPlayer1 <- PORT07_Bg3[ which(PORT07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_Bg3[ which(PORT07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_Bg2 <- rbind(PORT07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
PORT07_Bft <- ftable(PORT07_Bg2$player1, PORT07_Bg2$player2)
PORT07_Bft2 <- as.matrix(PORT07_Bft)
numRows <- nrow(PORT07_Bft2)
numCols <- ncol(PORT07_Bft2)
PORT07_Bft3 <- PORT07_Bft2[c(2:numRows) , c(2:numCols)]
PORT07_BTable <- graph.adjacency(PORT07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(PORT07_BTable, vertex.label = V(PORT07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
PORT07_B.clusterCoef <- transitivity(PORT07_BTable, type="global") #cluster coefficient
PORT07_B.degreeCent <- centralization.degree(PORT07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_Bftn <- as.network.matrix(PORT07_Bft)
PORT07_B.netDensity <- network.density(PORT07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_B.entropy <- entropy(PORT07_Bft) #entropy

PORT07_B.netMx <- cbind(PORT07_B.netMx, PORT07_B.clusterCoef, PORT07_B.degreeCent$centralization,
                        PORT07_B.netDensity, PORT07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
PORT07_SFg2 <- data.frame(PORT07_SF)
PORT07_SFg2 <- PORT07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_SFg2$player1
player2vector <- PORT07_SFg2$player2
PORT07_SFg3 <- PORT07_SFg2
PORT07_SFg3$p1inp2vec <- is.element(PORT07_SFg3$player1, player2vector)
PORT07_SFg3$p2inp1vec <- is.element(PORT07_SFg3$player2, player1vector)

addPlayer1 <- PORT07_SFg3[ which(PORT07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_SFg3[ which(PORT07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_SFg2 <- rbind(PORT07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
PORT07_SFft <- ftable(PORT07_SFg2$player1, PORT07_SFg2$player2)
PORT07_SFft2 <- as.matrix(PORT07_SFft)
numRows <- nrow(PORT07_SFft2)
numCols <- ncol(PORT07_SFft2)
PORT07_SFft3 <- PORT07_SFft2[c(2:numRows) , c(2:numCols)]
PORT07_SFTable <- graph.adjacency(PORT07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(PORT07_SFTable, vertex.label = V(PORT07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
PORT07_SF.clusterCoef <- transitivity(PORT07_SFTable, type="global") #cluster coefficient
PORT07_SF.degreeCent <- centralization.degree(PORT07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_SFftn <- as.network.matrix(PORT07_SFft)
PORT07_SF.netDensity <- network.density(PORT07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_SF.entropy <- entropy(PORT07_SFft) #entropy

PORT07_SF.netMx <- cbind(PORT07_SF.netMx, PORT07_SF.clusterCoef, PORT07_SF.degreeCent$centralization,
                         PORT07_SF.netDensity, PORT07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
PORT07_TFg2 <- data.frame(PORT07_TF)
PORT07_TFg2 <- PORT07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_TFg2$player1
player2vector <- PORT07_TFg2$player2
PORT07_TFg3 <- PORT07_TFg2
PORT07_TFg3$p1inp2vec <- is.element(PORT07_TFg3$player1, player2vector)
PORT07_TFg3$p2inp1vec <- is.element(PORT07_TFg3$player2, player1vector)

addPlayer1 <- PORT07_TFg3[ which(PORT07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_TFg3[ which(PORT07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_TFg2 <- rbind(PORT07_TFg2, addPlayers)


#ROUND 7, FWD Turnover graph using weighted edges
PORT07_TFft <- ftable(PORT07_TFg2$player1, PORT07_TFg2$player2)
PORT07_TFft2 <- as.matrix(PORT07_TFft)
numRows <- nrow(PORT07_TFft2)
numCols <- ncol(PORT07_TFft2)
PORT07_TFft3 <- PORT07_TFft2[c(2:numRows) , c(2:numCols)]
PORT07_TFTable <- graph.adjacency(PORT07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(PORT07_TFTable, vertex.label = V(PORT07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
PORT07_TF.clusterCoef <- transitivity(PORT07_TFTable, type="global") #cluster coefficient
PORT07_TF.degreeCent <- centralization.degree(PORT07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_TFftn <- as.network.matrix(PORT07_TFft)
PORT07_TF.netDensity <- network.density(PORT07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_TF.entropy <- entropy(PORT07_TFft) #entropy

PORT07_TF.netMx <- cbind(PORT07_TF.netMx, PORT07_TF.clusterCoef, PORT07_TF.degreeCent$centralization,
                         PORT07_TF.netDensity, PORT07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
PORT07_SAMg2 <- data.frame(PORT07_SAM)
PORT07_SAMg2 <- PORT07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_SAMg2$player1
player2vector <- PORT07_SAMg2$player2
PORT07_SAMg3 <- PORT07_SAMg2
PORT07_SAMg3$p1inp2vec <- is.element(PORT07_SAMg3$player1, player2vector)
PORT07_SAMg3$p2inp1vec <- is.element(PORT07_SAMg3$player2, player1vector)

addPlayer1 <- PORT07_SAMg3[ which(PORT07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_SAMg3[ which(PORT07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_SAMg2 <- rbind(PORT07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
PORT07_SAMft <- ftable(PORT07_SAMg2$player1, PORT07_SAMg2$player2)
PORT07_SAMft2 <- as.matrix(PORT07_SAMft)
numRows <- nrow(PORT07_SAMft2)
numCols <- ncol(PORT07_SAMft2)
PORT07_SAMft3 <- PORT07_SAMft2[c(2:numRows) , c(2:numCols)]
PORT07_SAMTable <- graph.adjacency(PORT07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(PORT07_SAMTable, vertex.label = V(PORT07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
PORT07_SAM.clusterCoef <- transitivity(PORT07_SAMTable, type="global") #cluster coefficient
PORT07_SAM.degreeCent <- centralization.degree(PORT07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_SAMftn <- as.network.matrix(PORT07_SAMft)
PORT07_SAM.netDensity <- network.density(PORT07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_SAM.entropy <- entropy(PORT07_SAMft) #entropy

PORT07_SAM.netMx <- cbind(PORT07_SAM.netMx, PORT07_SAM.clusterCoef, PORT07_SAM.degreeCent$centralization,
                          PORT07_SAM.netDensity, PORT07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
PORT07_TAMg2 <- data.frame(PORT07_TAM)
PORT07_TAMg2 <- PORT07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_TAMg2$player1
player2vector <- PORT07_TAMg2$player2
PORT07_TAMg3 <- PORT07_TAMg2
PORT07_TAMg3$p1inp2vec <- is.element(PORT07_TAMg3$player1, player2vector)
PORT07_TAMg3$p2inp1vec <- is.element(PORT07_TAMg3$player2, player1vector)

addPlayer1 <- PORT07_TAMg3[ which(PORT07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_TAMg3[ which(PORT07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_TAMg2 <- rbind(PORT07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
PORT07_TAMft <- ftable(PORT07_TAMg2$player1, PORT07_TAMg2$player2)
PORT07_TAMft2 <- as.matrix(PORT07_TAMft)
numRows <- nrow(PORT07_TAMft2)
numCols <- ncol(PORT07_TAMft2)
PORT07_TAMft3 <- PORT07_TAMft2[c(2:numRows) , c(2:numCols)]
PORT07_TAMTable <- graph.adjacency(PORT07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(PORT07_TAMTable, vertex.label = V(PORT07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
PORT07_TAM.clusterCoef <- transitivity(PORT07_TAMTable, type="global") #cluster coefficient
PORT07_TAM.degreeCent <- centralization.degree(PORT07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_TAMftn <- as.network.matrix(PORT07_TAMft)
PORT07_TAM.netDensity <- network.density(PORT07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_TAM.entropy <- entropy(PORT07_TAMft) #entropy

PORT07_TAM.netMx <- cbind(PORT07_TAM.netMx, PORT07_TAM.clusterCoef, PORT07_TAM.degreeCent$centralization,
                          PORT07_TAM.netDensity, PORT07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
PORT07_SDMg2 <- data.frame(PORT07_SDM)
PORT07_SDMg2 <- PORT07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_SDMg2$player1
player2vector <- PORT07_SDMg2$player2
PORT07_SDMg3 <- PORT07_SDMg2
PORT07_SDMg3$p1inp2vec <- is.element(PORT07_SDMg3$player1, player2vector)
PORT07_SDMg3$p2inp1vec <- is.element(PORT07_SDMg3$player2, player1vector)

addPlayer1 <- PORT07_SDMg3[ which(PORT07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_SDMg3[ which(PORT07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_SDMg2 <- rbind(PORT07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
PORT07_SDMft <- ftable(PORT07_SDMg2$player1, PORT07_SDMg2$player2)
PORT07_SDMft2 <- as.matrix(PORT07_SDMft)
numRows <- nrow(PORT07_SDMft2)
numCols <- ncol(PORT07_SDMft2)
PORT07_SDMft3 <- PORT07_SDMft2[c(2:numRows) , c(2:numCols)]
PORT07_SDMTable <- graph.adjacency(PORT07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(PORT07_SDMTable, vertex.label = V(PORT07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
PORT07_SDM.clusterCoef <- transitivity(PORT07_SDMTable, type="global") #cluster coefficient
PORT07_SDM.degreeCent <- centralization.degree(PORT07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_SDMftn <- as.network.matrix(PORT07_SDMft)
PORT07_SDM.netDensity <- network.density(PORT07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_SDM.entropy <- entropy(PORT07_SDMft) #entropy

PORT07_SDM.netMx <- cbind(PORT07_SDM.netMx, PORT07_SDM.clusterCoef, PORT07_SDM.degreeCent$centralization,
                          PORT07_SDM.netDensity, PORT07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
PORT07_TDMg2 <- data.frame(PORT07_TDM)
PORT07_TDMg2 <- PORT07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_TDMg2$player1
player2vector <- PORT07_TDMg2$player2
PORT07_TDMg3 <- PORT07_TDMg2
PORT07_TDMg3$p1inp2vec <- is.element(PORT07_TDMg3$player1, player2vector)
PORT07_TDMg3$p2inp1vec <- is.element(PORT07_TDMg3$player2, player1vector)

addPlayer1 <- PORT07_TDMg3[ which(PORT07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_TDMg3[ which(PORT07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_TDMg2 <- rbind(PORT07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
PORT07_TDMft <- ftable(PORT07_TDMg2$player1, PORT07_TDMg2$player2)
PORT07_TDMft2 <- as.matrix(PORT07_TDMft)
numRows <- nrow(PORT07_TDMft2)
numCols <- ncol(PORT07_TDMft2)
PORT07_TDMft3 <- PORT07_TDMft2[c(2:numRows) , c(2:numCols)]
PORT07_TDMTable <- graph.adjacency(PORT07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(PORT07_TDMTable, vertex.label = V(PORT07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
PORT07_TDM.clusterCoef <- transitivity(PORT07_TDMTable, type="global") #cluster coefficient
PORT07_TDM.degreeCent <- centralization.degree(PORT07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_TDMftn <- as.network.matrix(PORT07_TDMft)
PORT07_TDM.netDensity <- network.density(PORT07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_TDM.entropy <- entropy(PORT07_TDMft) #entropy

PORT07_TDM.netMx <- cbind(PORT07_TDM.netMx, PORT07_TDM.clusterCoef, PORT07_TDM.degreeCent$centralization,
                          PORT07_TDM.netDensity, PORT07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
PORT07_SDg2 <- data.frame(PORT07_SD)
PORT07_SDg2 <- PORT07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_SDg2$player1
player2vector <- PORT07_SDg2$player2
PORT07_SDg3 <- PORT07_SDg2
PORT07_SDg3$p1inp2vec <- is.element(PORT07_SDg3$player1, player2vector)
PORT07_SDg3$p2inp1vec <- is.element(PORT07_SDg3$player2, player1vector)

addPlayer1 <- PORT07_SDg3[ which(PORT07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_SDg3[ which(PORT07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_SDg2 <- rbind(PORT07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
PORT07_SDft <- ftable(PORT07_SDg2$player1, PORT07_SDg2$player2)
PORT07_SDft2 <- as.matrix(PORT07_SDft)
numRows <- nrow(PORT07_SDft2)
numCols <- ncol(PORT07_SDft2)
PORT07_SDft3 <- PORT07_SDft2[c(2:numRows) , c(2:numCols)]
PORT07_SDTable <- graph.adjacency(PORT07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(PORT07_SDTable, vertex.label = V(PORT07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
PORT07_SD.clusterCoef <- transitivity(PORT07_SDTable, type="global") #cluster coefficient
PORT07_SD.degreeCent <- centralization.degree(PORT07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_SDftn <- as.network.matrix(PORT07_SDft)
PORT07_SD.netDensity <- network.density(PORT07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_SD.entropy <- entropy(PORT07_SDft) #entropy

PORT07_SD.netMx <- cbind(PORT07_SD.netMx, PORT07_SD.clusterCoef, PORT07_SD.degreeCent$centralization,
                         PORT07_SD.netDensity, PORT07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
PORT07_TDg2 <- data.frame(PORT07_TD)
PORT07_TDg2 <- PORT07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_TDg2$player1
player2vector <- PORT07_TDg2$player2
PORT07_TDg3 <- PORT07_TDg2
PORT07_TDg3$p1inp2vec <- is.element(PORT07_TDg3$player1, player2vector)
PORT07_TDg3$p2inp1vec <- is.element(PORT07_TDg3$player2, player1vector)

addPlayer1 <- PORT07_TDg3[ which(PORT07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_TDg3[ which(PORT07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_TDg2 <- rbind(PORT07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
PORT07_TDft <- ftable(PORT07_TDg2$player1, PORT07_TDg2$player2)
PORT07_TDft2 <- as.matrix(PORT07_TDft)
numRows <- nrow(PORT07_TDft2)
numCols <- ncol(PORT07_TDft2)
PORT07_TDft3 <- PORT07_TDft2[c(2:numRows) , c(2:numCols)]
PORT07_TDTable <- graph.adjacency(PORT07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(PORT07_TDTable, vertex.label = V(PORT07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
PORT07_TD.clusterCoef <- transitivity(PORT07_TDTable, type="global") #cluster coefficient
PORT07_TD.degreeCent <- centralization.degree(PORT07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_TDftn <- as.network.matrix(PORT07_TDft)
PORT07_TD.netDensity <- network.density(PORT07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_TD.entropy <- entropy(PORT07_TDft) #entropy

PORT07_TD.netMx <- cbind(PORT07_TD.netMx, PORT07_TD.clusterCoef, PORT07_TD.degreeCent$centralization,
                         PORT07_TD.netDensity, PORT07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
PORT07_QTg2 <- data.frame(PORT07_QT)
PORT07_QTg2 <- PORT07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT07_QTg2$player1
player2vector <- PORT07_QTg2$player2
PORT07_QTg3 <- PORT07_QTg2
PORT07_QTg3$p1inp2vec <- is.element(PORT07_QTg3$player1, player2vector)
PORT07_QTg3$p2inp1vec <- is.element(PORT07_QTg3$player2, player1vector)

addPlayer1 <- PORT07_QTg3[ which(PORT07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT07_QTg3[ which(PORT07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT07_QTg2 <- rbind(PORT07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
PORT07_QTft <- ftable(PORT07_QTg2$player1, PORT07_QTg2$player2)
PORT07_QTft2 <- as.matrix(PORT07_QTft)
numRows <- nrow(PORT07_QTft2)
numCols <- ncol(PORT07_QTft2)
PORT07_QTft3 <- PORT07_QTft2[c(2:numRows) , c(2:numCols)]
PORT07_QTTable <- graph.adjacency(PORT07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(PORT07_QTTable, vertex.label = V(PORT07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
PORT07_QT.clusterCoef <- transitivity(PORT07_QTTable, type="global") #cluster coefficient
PORT07_QT.degreeCent <- centralization.degree(PORT07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT07_QTftn <- as.network.matrix(PORT07_QTft)
PORT07_QT.netDensity <- network.density(PORT07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT07_QT.entropy <- entropy(PORT07_QTft) #entropy

PORT07_QT.netMx <- cbind(PORT07_QT.netMx, PORT07_QT.clusterCoef, PORT07_QT.degreeCent$centralization,
                         PORT07_QT.netDensity, PORT07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT07_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "RICH"
KIoutcome = "Goal_F"
RICH07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
RICH07_Gg2 <- data.frame(RICH07_G)
RICH07_Gg2 <- RICH07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_Gg2$player1
player2vector <- RICH07_Gg2$player2
RICH07_Gg3 <- RICH07_Gg2
RICH07_Gg3$p1inp2vec <- is.element(RICH07_Gg3$player1, player2vector)
RICH07_Gg3$p2inp1vec <- is.element(RICH07_Gg3$player2, player1vector)

addPlayer1 <- RICH07_Gg3[ which(RICH07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_Gg3[ which(RICH07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_Gg2 <- rbind(RICH07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
RICH07_Gft <- ftable(RICH07_Gg2$player1, RICH07_Gg2$player2)
RICH07_Gft2 <- as.matrix(RICH07_Gft)
numRows <- nrow(RICH07_Gft2)
numCols <- ncol(RICH07_Gft2)
RICH07_Gft3 <- RICH07_Gft2[c(2:numRows) , c(2:numCols)]
RICH07_GTable <- graph.adjacency(RICH07_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(RICH07_GTable, vertex.label = V(RICH07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
RICH07_G.clusterCoef <- transitivity(RICH07_GTable, type="global") #cluster coefficient
RICH07_G.degreeCent <- centralization.degree(RICH07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_Gftn <- as.network.matrix(RICH07_Gft)
RICH07_G.netDensity <- network.density(RICH07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_G.entropy <- entropy(RICH07_Gft) #entropy

RICH07_G.netMx <- cbind(RICH07_G.netMx, RICH07_G.clusterCoef, RICH07_G.degreeCent$centralization,
                        RICH07_G.netDensity, RICH07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "Behind_F"
RICH07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
RICH07_Bg2 <- data.frame(RICH07_B)
RICH07_Bg2 <- RICH07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_Bg2$player1
player2vector <- RICH07_Bg2$player2
RICH07_Bg3 <- RICH07_Bg2
RICH07_Bg3$p1inp2vec <- is.element(RICH07_Bg3$player1, player2vector)
RICH07_Bg3$p2inp1vec <- is.element(RICH07_Bg3$player2, player1vector)

addPlayer1 <- RICH07_Bg3[ which(RICH07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_Bg3[ which(RICH07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_Bg2 <- rbind(RICH07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
RICH07_Bft <- ftable(RICH07_Bg2$player1, RICH07_Bg2$player2)
RICH07_Bft2 <- as.matrix(RICH07_Bft)
numRows <- nrow(RICH07_Bft2)
numCols <- ncol(RICH07_Bft2)
RICH07_Bft3 <- RICH07_Bft2[c(2:numRows) , c(2:numCols)]
RICH07_BTable <- graph.adjacency(RICH07_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(RICH07_BTable, vertex.label = V(RICH07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
RICH07_B.clusterCoef <- transitivity(RICH07_BTable, type="global") #cluster coefficient
RICH07_B.degreeCent <- centralization.degree(RICH07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_Bftn <- as.network.matrix(RICH07_Bft)
RICH07_B.netDensity <- network.density(RICH07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_B.entropy <- entropy(RICH07_Bft) #entropy

RICH07_B.netMx <- cbind(RICH07_B.netMx, RICH07_B.clusterCoef, RICH07_B.degreeCent$centralization,
                        RICH07_B.netDensity, RICH07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
RICH07_SFg2 <- data.frame(RICH07_SF)
RICH07_SFg2 <- RICH07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_SFg2$player1
player2vector <- RICH07_SFg2$player2
RICH07_SFg3 <- RICH07_SFg2
RICH07_SFg3$p1inp2vec <- is.element(RICH07_SFg3$player1, player2vector)
RICH07_SFg3$p2inp1vec <- is.element(RICH07_SFg3$player2, player1vector)

addPlayer1 <- RICH07_SFg3[ which(RICH07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_SFg3[ which(RICH07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_SFg2 <- rbind(RICH07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
RICH07_SFft <- ftable(RICH07_SFg2$player1, RICH07_SFg2$player2)
RICH07_SFft2 <- as.matrix(RICH07_SFft)
numRows <- nrow(RICH07_SFft2)
numCols <- ncol(RICH07_SFft2)
RICH07_SFft3 <- RICH07_SFft2[c(2:numRows) , c(2:numCols)]
RICH07_SFTable <- graph.adjacency(RICH07_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(RICH07_SFTable, vertex.label = V(RICH07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
RICH07_SF.clusterCoef <- transitivity(RICH07_SFTable, type="global") #cluster coefficient
RICH07_SF.degreeCent <- centralization.degree(RICH07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_SFftn <- as.network.matrix(RICH07_SFft)
RICH07_SF.netDensity <- network.density(RICH07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_SF.entropy <- entropy(RICH07_SFft) #entropy

RICH07_SF.netMx <- cbind(RICH07_SF.netMx, RICH07_SF.clusterCoef, RICH07_SF.degreeCent$centralization,
                         RICH07_SF.netDensity, RICH07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
RICH07_TFg2 <- data.frame(RICH07_TF)
RICH07_TFg2 <- RICH07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_TFg2$player1
player2vector <- RICH07_TFg2$player2
RICH07_TFg3 <- RICH07_TFg2
RICH07_TFg3$p1inp2vec <- is.element(RICH07_TFg3$player1, player2vector)
RICH07_TFg3$p2inp1vec <- is.element(RICH07_TFg3$player2, player1vector)

addPlayer1 <- RICH07_TFg3[ which(RICH07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH07_TFg3[ which(RICH07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_TFg2 <- rbind(RICH07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
RICH07_TFft <- ftable(RICH07_TFg2$player1, RICH07_TFg2$player2)
RICH07_TFft2 <- as.matrix(RICH07_TFft)
numRows <- nrow(RICH07_TFft2)
numCols <- ncol(RICH07_TFft2)
RICH07_TFft3 <- RICH07_TFft2[c(2:numRows) , c(2:numCols)]
RICH07_TFTable <- graph.adjacency(RICH07_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(RICH07_TFTable, vertex.label = V(RICH07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
RICH07_TF.clusterCoef <- transitivity(RICH07_TFTable, type="global") #cluster coefficient
RICH07_TF.degreeCent <- centralization.degree(RICH07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_TFftn <- as.network.matrix(RICH07_TFft)
RICH07_TF.netDensity <- network.density(RICH07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_TF.entropy <- entropy(RICH07_TFft) #entropy

RICH07_TF.netMx <- cbind(RICH07_TF.netMx, RICH07_TF.clusterCoef, RICH07_TF.degreeCent$centralization,
                         RICH07_TF.netDensity, RICH07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
RICH07_SAMg2 <- data.frame(RICH07_SAM)
RICH07_SAMg2 <- RICH07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_SAMg2$player1
player2vector <- RICH07_SAMg2$player2
RICH07_SAMg3 <- RICH07_SAMg2
RICH07_SAMg3$p1inp2vec <- is.element(RICH07_SAMg3$player1, player2vector)
RICH07_SAMg3$p2inp1vec <- is.element(RICH07_SAMg3$player2, player1vector)

addPlayer1 <- RICH07_SAMg3[ which(RICH07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_SAMg3[ which(RICH07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_SAMg2 <- rbind(RICH07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
RICH07_SAMft <- ftable(RICH07_SAMg2$player1, RICH07_SAMg2$player2)
RICH07_SAMft2 <- as.matrix(RICH07_SAMft)
numRows <- nrow(RICH07_SAMft2)
numCols <- ncol(RICH07_SAMft2)
RICH07_SAMft3 <- RICH07_SAMft2[c(2:numRows) , c(2:numCols)]
RICH07_SAMTable <- graph.adjacency(RICH07_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(RICH07_SAMTable, vertex.label = V(RICH07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
RICH07_SAM.clusterCoef <- transitivity(RICH07_SAMTable, type="global") #cluster coefficient
RICH07_SAM.degreeCent <- centralization.degree(RICH07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_SAMftn <- as.network.matrix(RICH07_SAMft)
RICH07_SAM.netDensity <- network.density(RICH07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_SAM.entropy <- entropy(RICH07_SAMft) #entropy

RICH07_SAM.netMx <- cbind(RICH07_SAM.netMx, RICH07_SAM.clusterCoef, RICH07_SAM.degreeCent$centralization,
                          RICH07_SAM.netDensity, RICH07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
RICH07_TAMg2 <- data.frame(RICH07_TAM)
RICH07_TAMg2 <- RICH07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_TAMg2$player1
player2vector <- RICH07_TAMg2$player2
RICH07_TAMg3 <- RICH07_TAMg2
RICH07_TAMg3$p1inp2vec <- is.element(RICH07_TAMg3$player1, player2vector)
RICH07_TAMg3$p2inp1vec <- is.element(RICH07_TAMg3$player2, player1vector)

addPlayer1 <- RICH07_TAMg3[ which(RICH07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_TAMg3[ which(RICH07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_TAMg2 <- rbind(RICH07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
RICH07_TAMft <- ftable(RICH07_TAMg2$player1, RICH07_TAMg2$player2)
RICH07_TAMft2 <- as.matrix(RICH07_TAMft)
numRows <- nrow(RICH07_TAMft2)
numCols <- ncol(RICH07_TAMft2)
RICH07_TAMft3 <- RICH07_TAMft2[c(2:numRows) , c(2:numCols)]
RICH07_TAMTable <- graph.adjacency(RICH07_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(RICH07_TAMTable, vertex.label = V(RICH07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
RICH07_TAM.clusterCoef <- transitivity(RICH07_TAMTable, type="global") #cluster coefficient
RICH07_TAM.degreeCent <- centralization.degree(RICH07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_TAMftn <- as.network.matrix(RICH07_TAMft)
RICH07_TAM.netDensity <- network.density(RICH07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_TAM.entropy <- entropy(RICH07_TAMft) #entropy

RICH07_TAM.netMx <- cbind(RICH07_TAM.netMx, RICH07_TAM.clusterCoef, RICH07_TAM.degreeCent$centralization,
                          RICH07_TAM.netDensity, RICH07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
RICH07_SDMg2 <- data.frame(RICH07_SDM)
RICH07_SDMg2 <- RICH07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_SDMg2$player1
player2vector <- RICH07_SDMg2$player2
RICH07_SDMg3 <- RICH07_SDMg2
RICH07_SDMg3$p1inp2vec <- is.element(RICH07_SDMg3$player1, player2vector)
RICH07_SDMg3$p2inp1vec <- is.element(RICH07_SDMg3$player2, player1vector)

addPlayer1 <- RICH07_SDMg3[ which(RICH07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_SDMg3[ which(RICH07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_SDMg2 <- rbind(RICH07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
RICH07_SDMft <- ftable(RICH07_SDMg2$player1, RICH07_SDMg2$player2)
RICH07_SDMft2 <- as.matrix(RICH07_SDMft)
numRows <- nrow(RICH07_SDMft2)
numCols <- ncol(RICH07_SDMft2)
RICH07_SDMft3 <- RICH07_SDMft2[c(2:numRows) , c(2:numCols)]
RICH07_SDMTable <- graph.adjacency(RICH07_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(RICH07_SDMTable, vertex.label = V(RICH07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
RICH07_SDM.clusterCoef <- transitivity(RICH07_SDMTable, type="global") #cluster coefficient
RICH07_SDM.degreeCent <- centralization.degree(RICH07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_SDMftn <- as.network.matrix(RICH07_SDMft)
RICH07_SDM.netDensity <- network.density(RICH07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_SDM.entropy <- entropy(RICH07_SDMft) #entropy

RICH07_SDM.netMx <- cbind(RICH07_SDM.netMx, RICH07_SDM.clusterCoef, RICH07_SDM.degreeCent$centralization,
                          RICH07_SDM.netDensity, RICH07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
RICH07_TDMg2 <- data.frame(RICH07_TDM)
RICH07_TDMg2 <- RICH07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_TDMg2$player1
player2vector <- RICH07_TDMg2$player2
RICH07_TDMg3 <- RICH07_TDMg2
RICH07_TDMg3$p1inp2vec <- is.element(RICH07_TDMg3$player1, player2vector)
RICH07_TDMg3$p2inp1vec <- is.element(RICH07_TDMg3$player2, player1vector)

addPlayer1 <- RICH07_TDMg3[ which(RICH07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_TDMg3[ which(RICH07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_TDMg2 <- rbind(RICH07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
RICH07_TDMft <- ftable(RICH07_TDMg2$player1, RICH07_TDMg2$player2)
RICH07_TDMft2 <- as.matrix(RICH07_TDMft)
numRows <- nrow(RICH07_TDMft2)
numCols <- ncol(RICH07_TDMft2)
RICH07_TDMft3 <- RICH07_TDMft2[c(2:numRows) , c(2:numCols)]
RICH07_TDMTable <- graph.adjacency(RICH07_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(RICH07_TDMTable, vertex.label = V(RICH07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
RICH07_TDM.clusterCoef <- transitivity(RICH07_TDMTable, type="global") #cluster coefficient
RICH07_TDM.degreeCent <- centralization.degree(RICH07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_TDMftn <- as.network.matrix(RICH07_TDMft)
RICH07_TDM.netDensity <- network.density(RICH07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_TDM.entropy <- entropy(RICH07_TDMft) #entropy

RICH07_TDM.netMx <- cbind(RICH07_TDM.netMx, RICH07_TDM.clusterCoef, RICH07_TDM.degreeCent$centralization,
                          RICH07_TDM.netDensity, RICH07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
RICH07_SDg2 <- data.frame(RICH07_SD)
RICH07_SDg2 <- RICH07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_SDg2$player1
player2vector <- RICH07_SDg2$player2
RICH07_SDg3 <- RICH07_SDg2
RICH07_SDg3$p1inp2vec <- is.element(RICH07_SDg3$player1, player2vector)
RICH07_SDg3$p2inp1vec <- is.element(RICH07_SDg3$player2, player1vector)

addPlayer1 <- RICH07_SDg3[ which(RICH07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_SDg3[ which(RICH07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_SDg2 <- rbind(RICH07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
RICH07_SDft <- ftable(RICH07_SDg2$player1, RICH07_SDg2$player2)
RICH07_SDft2 <- as.matrix(RICH07_SDft)
numRows <- nrow(RICH07_SDft2)
numCols <- ncol(RICH07_SDft2)
RICH07_SDft3 <- RICH07_SDft2[c(2:numRows) , c(2:numCols)]
RICH07_SDTable <- graph.adjacency(RICH07_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(RICH07_SDTable, vertex.label = V(RICH07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
RICH07_SD.clusterCoef <- transitivity(RICH07_SDTable, type="global") #cluster coefficient
RICH07_SD.degreeCent <- centralization.degree(RICH07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_SDftn <- as.network.matrix(RICH07_SDft)
RICH07_SD.netDensity <- network.density(RICH07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_SD.entropy <- entropy(RICH07_SDft) #entropy

RICH07_SD.netMx <- cbind(RICH07_SD.netMx, RICH07_SD.clusterCoef, RICH07_SD.degreeCent$centralization,
                         RICH07_SD.netDensity, RICH07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
RICH07_TDg2 <- data.frame(RICH07_TD)
RICH07_TDg2 <- RICH07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_TDg2$player1
player2vector <- RICH07_TDg2$player2
RICH07_TDg3 <- RICH07_TDg2
RICH07_TDg3$p1inp2vec <- is.element(RICH07_TDg3$player1, player2vector)
RICH07_TDg3$p2inp1vec <- is.element(RICH07_TDg3$player2, player1vector)

addPlayer1 <- RICH07_TDg3[ which(RICH07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_TDg3[ which(RICH07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_TDg2 <- rbind(RICH07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
RICH07_TDft <- ftable(RICH07_TDg2$player1, RICH07_TDg2$player2)
RICH07_TDft2 <- as.matrix(RICH07_TDft)
numRows <- nrow(RICH07_TDft2)
numCols <- ncol(RICH07_TDft2)
RICH07_TDft3 <- RICH07_TDft2[c(2:numRows) , c(2:numCols)]
RICH07_TDTable <- graph.adjacency(RICH07_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(RICH07_TDTable, vertex.label = V(RICH07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
RICH07_TD.clusterCoef <- transitivity(RICH07_TDTable, type="global") #cluster coefficient
RICH07_TD.degreeCent <- centralization.degree(RICH07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_TDftn <- as.network.matrix(RICH07_TDft)
RICH07_TD.netDensity <- network.density(RICH07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_TD.entropy <- entropy(RICH07_TDft) #entropy

RICH07_TD.netMx <- cbind(RICH07_TD.netMx, RICH07_TD.clusterCoef, RICH07_TD.degreeCent$centralization,
                         RICH07_TD.netDensity, RICH07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
RICH07_QTg2 <- data.frame(RICH07_QT)
RICH07_QTg2 <- RICH07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH07_QTg2$player1
player2vector <- RICH07_QTg2$player2
RICH07_QTg3 <- RICH07_QTg2
RICH07_QTg3$p1inp2vec <- is.element(RICH07_QTg3$player1, player2vector)
RICH07_QTg3$p2inp1vec <- is.element(RICH07_QTg3$player2, player1vector)

addPlayer1 <- RICH07_QTg3[ which(RICH07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH07_QTg3[ which(RICH07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH07_QTg2 <- rbind(RICH07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
RICH07_QTft <- ftable(RICH07_QTg2$player1, RICH07_QTg2$player2)
RICH07_QTft2 <- as.matrix(RICH07_QTft)
numRows <- nrow(RICH07_QTft2)
numCols <- ncol(RICH07_QTft2)
RICH07_QTft3 <- RICH07_QTft2[c(2:numRows) , c(2:numCols)]
RICH07_QTTable <- graph.adjacency(RICH07_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(RICH07_QTTable, vertex.label = V(RICH07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
RICH07_QT.clusterCoef <- transitivity(RICH07_QTTable, type="global") #cluster coefficient
RICH07_QT.degreeCent <- centralization.degree(RICH07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH07_QTftn <- as.network.matrix(RICH07_QTft)
RICH07_QT.netDensity <- network.density(RICH07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH07_QT.entropy <- entropy(RICH07_QTft) #entropy

RICH07_QT.netMx <- cbind(RICH07_QT.netMx, RICH07_QT.clusterCoef, RICH07_QT.degreeCent$centralization,
                         RICH07_QT.netDensity, RICH07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH07_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "STK"
KIoutcome = "Goal_F"
STK07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
STK07_Gg2 <- data.frame(STK07_G)
STK07_Gg2 <- STK07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_Gg2$player1
player2vector <- STK07_Gg2$player2
STK07_Gg3 <- STK07_Gg2
STK07_Gg3$p1inp2vec <- is.element(STK07_Gg3$player1, player2vector)
STK07_Gg3$p2inp1vec <- is.element(STK07_Gg3$player2, player1vector)

addPlayer1 <- STK07_Gg3[ which(STK07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_Gg3[ which(STK07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_Gg2 <- rbind(STK07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
STK07_Gft <- ftable(STK07_Gg2$player1, STK07_Gg2$player2)
STK07_Gft2 <- as.matrix(STK07_Gft)
numRows <- nrow(STK07_Gft2)
numCols <- ncol(STK07_Gft2)
STK07_Gft3 <- STK07_Gft2[c(2:numRows) , c(2:numCols)]
STK07_GTable <- graph.adjacency(STK07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(STK07_GTable, vertex.label = V(STK07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
STK07_G.clusterCoef <- transitivity(STK07_GTable, type="global") #cluster coefficient
STK07_G.degreeCent <- centralization.degree(STK07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_Gftn <- as.network.matrix(STK07_Gft)
STK07_G.netDensity <- network.density(STK07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_G.entropy <- entropy(STK07_Gft) #entropy

STK07_G.netMx <- cbind(STK07_G.netMx, STK07_G.clusterCoef, STK07_G.degreeCent$centralization,
                       STK07_G.netDensity, STK07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************

round = 7
teamName = "STK"
KIoutcome = "Behind_F"
STK07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
STK07_Bg2 <- data.frame(STK07_B)
STK07_Bg2 <- STK07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_Bg2$player1
player2vector <- STK07_Bg2$player2
STK07_Bg3 <- STK07_Bg2
STK07_Bg3$p1inp2vec <- is.element(STK07_Bg3$player1, player2vector)
STK07_Bg3$p2inp1vec <- is.element(STK07_Bg3$player2, player1vector)

addPlayer1 <- STK07_Bg3[ which(STK07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_Bg3[ which(STK07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_Bg2 <- rbind(STK07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
STK07_Bft <- ftable(STK07_Bg2$player1, STK07_Bg2$player2)
STK07_Bft2 <- as.matrix(STK07_Bft)
numRows <- nrow(STK07_Bft2)
numCols <- ncol(STK07_Bft2)
STK07_Bft3 <- STK07_Bft2[c(2:numRows) , c(2:numCols)]
STK07_BTable <- graph.adjacency(STK07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(STK07_BTable, vertex.label = V(STK07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
STK07_B.clusterCoef <- transitivity(STK07_BTable, type="global") #cluster coefficient
STK07_B.degreeCent <- centralization.degree(STK07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_Bftn <- as.network.matrix(STK07_Bft)
STK07_B.netDensity <- network.density(STK07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_B.entropy <- entropy(STK07_Bft) #entropy

STK07_B.netMx <- cbind(STK07_B.netMx, STK07_B.clusterCoef, STK07_B.degreeCent$centralization,
                       STK07_B.netDensity, STK07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "STK"
KIoutcome = "Stoppage_F"
STK07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
STK07_SFg2 <- data.frame(STK07_SF)
STK07_SFg2 <- STK07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_SFg2$player1
player2vector <- STK07_SFg2$player2
STK07_SFg3 <- STK07_SFg2
STK07_SFg3$p1inp2vec <- is.element(STK07_SFg3$player1, player2vector)
STK07_SFg3$p2inp1vec <- is.element(STK07_SFg3$player2, player1vector)

addPlayer1 <- STK07_SFg3[ which(STK07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_SFg3[ which(STK07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_SFg2 <- rbind(STK07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
STK07_SFft <- ftable(STK07_SFg2$player1, STK07_SFg2$player2)
STK07_SFft2 <- as.matrix(STK07_SFft)
numRows <- nrow(STK07_SFft2)
numCols <- ncol(STK07_SFft2)
STK07_SFft3 <- STK07_SFft2[c(2:numRows) , c(2:numCols)]
STK07_SFTable <- graph.adjacency(STK07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(STK07_SFTable, vertex.label = V(STK07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
STK07_SF.clusterCoef <- transitivity(STK07_SFTable, type="global") #cluster coefficient
STK07_SF.degreeCent <- centralization.degree(STK07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_SFftn <- as.network.matrix(STK07_SFft)
STK07_SF.netDensity <- network.density(STK07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_SF.entropy <- entropy(STK07_SFft) #entropy

STK07_SF.netMx <- cbind(STK07_SF.netMx, STK07_SF.clusterCoef, STK07_SF.degreeCent$centralization,
                        STK07_SF.netDensity, STK07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "STK"
KIoutcome = "Turnover_F"
STK07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
STK07_TFg2 <- data.frame(STK07_TF)
STK07_TFg2 <- STK07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_TFg2$player1
player2vector <- STK07_TFg2$player2
STK07_TFg3 <- STK07_TFg2
STK07_TFg3$p1inp2vec <- is.element(STK07_TFg3$player1, player2vector)
STK07_TFg3$p2inp1vec <- is.element(STK07_TFg3$player2, player1vector)

addPlayer1 <- STK07_TFg3[ which(STK07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_TFg3[ which(STK07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_TFg2 <- rbind(STK07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
STK07_TFft <- ftable(STK07_TFg2$player1, STK07_TFg2$player2)
STK07_TFft2 <- as.matrix(STK07_TFft)
numRows <- nrow(STK07_TFft2)
numCols <- ncol(STK07_TFft2)
STK07_TFft3 <- STK07_TFft2[c(2:numRows) , c(2:numCols)]
STK07_TFTable <- graph.adjacency(STK07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(STK07_TFTable, vertex.label = V(STK07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
STK07_TF.clusterCoef <- transitivity(STK07_TFTable, type="global") #cluster coefficient
STK07_TF.degreeCent <- centralization.degree(STK07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_TFftn <- as.network.matrix(STK07_TFft)
STK07_TF.netDensity <- network.density(STK07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_TF.entropy <- entropy(STK07_TFft) #entropy

STK07_TF.netMx <- cbind(STK07_TF.netMx, STK07_TF.clusterCoef, STK07_TF.degreeCent$centralization,
                        STK07_TF.netDensity, STK07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
STK07_SAMg2 <- data.frame(STK07_SAM)
STK07_SAMg2 <- STK07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_SAMg2$player1
player2vector <- STK07_SAMg2$player2
STK07_SAMg3 <- STK07_SAMg2
STK07_SAMg3$p1inp2vec <- is.element(STK07_SAMg3$player1, player2vector)
STK07_SAMg3$p2inp1vec <- is.element(STK07_SAMg3$player2, player1vector)

addPlayer1 <- STK07_SAMg3[ which(STK07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_SAMg3[ which(STK07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_SAMg2 <- rbind(STK07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
STK07_SAMft <- ftable(STK07_SAMg2$player1, STK07_SAMg2$player2)
STK07_SAMft2 <- as.matrix(STK07_SAMft)
numRows <- nrow(STK07_SAMft2)
numCols <- ncol(STK07_SAMft2)
STK07_SAMft3 <- STK07_SAMft2[c(2:numRows) , c(2:numCols)]
STK07_SAMTable <- graph.adjacency(STK07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(STK07_SAMTable, vertex.label = V(STK07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
STK07_SAM.clusterCoef <- transitivity(STK07_SAMTable, type="global") #cluster coefficient
STK07_SAM.degreeCent <- centralization.degree(STK07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_SAMftn <- as.network.matrix(STK07_SAMft)
STK07_SAM.netDensity <- network.density(STK07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_SAM.entropy <- entropy(STK07_SAMft) #entropy

STK07_SAM.netMx <- cbind(STK07_SAM.netMx, STK07_SAM.clusterCoef, STK07_SAM.degreeCent$centralization,
                         STK07_SAM.netDensity, STK07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "STK"
KIoutcome = "Turnover_AM"
STK07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
STK07_TAMg2 <- data.frame(STK07_TAM)
STK07_TAMg2 <- STK07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_TAMg2$player1
player2vector <- STK07_TAMg2$player2
STK07_TAMg3 <- STK07_TAMg2
STK07_TAMg3$p1inp2vec <- is.element(STK07_TAMg3$player1, player2vector)
STK07_TAMg3$p2inp1vec <- is.element(STK07_TAMg3$player2, player1vector)

addPlayer1 <- STK07_TAMg3[ which(STK07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_TAMg3[ which(STK07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_TAMg2 <- rbind(STK07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
STK07_TAMft <- ftable(STK07_TAMg2$player1, STK07_TAMg2$player2)
STK07_TAMft2 <- as.matrix(STK07_TAMft)
numRows <- nrow(STK07_TAMft2)
numCols <- ncol(STK07_TAMft2)
STK07_TAMft3 <- STK07_TAMft2[c(2:numRows) , c(2:numCols)]
STK07_TAMTable <- graph.adjacency(STK07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(STK07_TAMTable, vertex.label = V(STK07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
STK07_TAM.clusterCoef <- transitivity(STK07_TAMTable, type="global") #cluster coefficient
STK07_TAM.degreeCent <- centralization.degree(STK07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_TAMftn <- as.network.matrix(STK07_TAMft)
STK07_TAM.netDensity <- network.density(STK07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_TAM.entropy <- entropy(STK07_TAMft) #entropy

STK07_TAM.netMx <- cbind(STK07_TAM.netMx, STK07_TAM.clusterCoef, STK07_TAM.degreeCent$centralization,
                         STK07_TAM.netDensity, STK07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************

round = 7
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
STK07_SDMg2 <- data.frame(STK07_SDM)
STK07_SDMg2 <- STK07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_SDMg2$player1
player2vector <- STK07_SDMg2$player2
STK07_SDMg3 <- STK07_SDMg2
STK07_SDMg3$p1inp2vec <- is.element(STK07_SDMg3$player1, player2vector)
STK07_SDMg3$p2inp1vec <- is.element(STK07_SDMg3$player2, player1vector)

addPlayer1 <- STK07_SDMg3[ which(STK07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_SDMg3[ which(STK07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_SDMg2 <- rbind(STK07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
STK07_SDMft <- ftable(STK07_SDMg2$player1, STK07_SDMg2$player2)
STK07_SDMft2 <- as.matrix(STK07_SDMft)
numRows <- nrow(STK07_SDMft2)
numCols <- ncol(STK07_SDMft2)
STK07_SDMft3 <- STK07_SDMft2[c(2:numRows) , c(2:numCols)]
STK07_SDMTable <- graph.adjacency(STK07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(STK07_SDMTable, vertex.label = V(STK07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
STK07_SDM.clusterCoef <- transitivity(STK07_SDMTable, type="global") #cluster coefficient
STK07_SDM.degreeCent <- centralization.degree(STK07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_SDMftn <- as.network.matrix(STK07_SDMft)
STK07_SDM.netDensity <- network.density(STK07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_SDM.entropy <- entropy(STK07_SDMft) #entropy

STK07_SDM.netMx <- cbind(STK07_SDM.netMx, STK07_SDM.clusterCoef, STK07_SDM.degreeCent$centralization,
                         STK07_SDM.netDensity, STK07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "STK"
KIoutcome = "Turnover_DM"
STK07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
STK07_TDMg2 <- data.frame(STK07_TDM)
STK07_TDMg2 <- STK07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_TDMg2$player1
player2vector <- STK07_TDMg2$player2
STK07_TDMg3 <- STK07_TDMg2
STK07_TDMg3$p1inp2vec <- is.element(STK07_TDMg3$player1, player2vector)
STK07_TDMg3$p2inp1vec <- is.element(STK07_TDMg3$player2, player1vector)

addPlayer1 <- STK07_TDMg3[ which(STK07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_TDMg3[ which(STK07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_TDMg2 <- rbind(STK07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
STK07_TDMft <- ftable(STK07_TDMg2$player1, STK07_TDMg2$player2)
STK07_TDMft2 <- as.matrix(STK07_TDMft)
numRows <- nrow(STK07_TDMft2)
numCols <- ncol(STK07_TDMft2)
STK07_TDMft3 <- STK07_TDMft2[c(2:numRows) , c(2:numCols)]
STK07_TDMTable <- graph.adjacency(STK07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(STK07_TDMTable, vertex.label = V(STK07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
STK07_TDM.clusterCoef <- transitivity(STK07_TDMTable, type="global") #cluster coefficient
STK07_TDM.degreeCent <- centralization.degree(STK07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_TDMftn <- as.network.matrix(STK07_TDMft)
STK07_TDM.netDensity <- network.density(STK07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_TDM.entropy <- entropy(STK07_TDMft) #entropy

STK07_TDM.netMx <- cbind(STK07_TDM.netMx, STK07_TDM.clusterCoef, STK07_TDM.degreeCent$centralization,
                         STK07_TDM.netDensity, STK07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************

round = 7
teamName = "STK"
KIoutcome = "Stoppage_D"
STK07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
STK07_SDg2 <- data.frame(STK07_SD)
STK07_SDg2 <- STK07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_SDg2$player1
player2vector <- STK07_SDg2$player2
STK07_SDg3 <- STK07_SDg2
STK07_SDg3$p1inp2vec <- is.element(STK07_SDg3$player1, player2vector)
STK07_SDg3$p2inp1vec <- is.element(STK07_SDg3$player2, player1vector)

addPlayer1 <- STK07_SDg3[ which(STK07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_SDg3[ which(STK07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_SDg2 <- rbind(STK07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
STK07_SDft <- ftable(STK07_SDg2$player1, STK07_SDg2$player2)
STK07_SDft2 <- as.matrix(STK07_SDft)
numRows <- nrow(STK07_SDft2)
numCols <- ncol(STK07_SDft2)
STK07_SDft3 <- STK07_SDft2[c(2:numRows) , c(2:numCols)]
STK07_SDTable <- graph.adjacency(STK07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(STK07_SDTable, vertex.label = V(STK07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
STK07_SD.clusterCoef <- transitivity(STK07_SDTable, type="global") #cluster coefficient
STK07_SD.degreeCent <- centralization.degree(STK07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_SDftn <- as.network.matrix(STK07_SDft)
STK07_SD.netDensity <- network.density(STK07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_SD.entropy <- entropy(STK07_SDft) #entropy

STK07_SD.netMx <- cbind(STK07_SD.netMx, STK07_SD.clusterCoef, STK07_SD.degreeCent$centralization,
                        STK07_SD.netDensity, STK07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "STK"
KIoutcome = "Turnover_D"
STK07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
STK07_TDg2 <- data.frame(STK07_TD)
STK07_TDg2 <- STK07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_TDg2$player1
player2vector <- STK07_TDg2$player2
STK07_TDg3 <- STK07_TDg2
STK07_TDg3$p1inp2vec <- is.element(STK07_TDg3$player1, player2vector)
STK07_TDg3$p2inp1vec <- is.element(STK07_TDg3$player2, player1vector)

addPlayer1 <- STK07_TDg3[ which(STK07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_TDg3[ which(STK07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_TDg2 <- rbind(STK07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
STK07_TDft <- ftable(STK07_TDg2$player1, STK07_TDg2$player2)
STK07_TDft2 <- as.matrix(STK07_TDft)
numRows <- nrow(STK07_TDft2)
numCols <- ncol(STK07_TDft2)
STK07_TDft3 <- STK07_TDft2[c(2:numRows) , c(2:numCols)]
STK07_TDTable <- graph.adjacency(STK07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(STK07_TDTable, vertex.label = V(STK07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
STK07_TD.clusterCoef <- transitivity(STK07_TDTable, type="global") #cluster coefficient
STK07_TD.degreeCent <- centralization.degree(STK07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_TDftn <- as.network.matrix(STK07_TDft)
STK07_TD.netDensity <- network.density(STK07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_TD.entropy <- entropy(STK07_TDft) #entropy

STK07_TD.netMx <- cbind(STK07_TD.netMx, STK07_TD.clusterCoef, STK07_TD.degreeCent$centralization,
                        STK07_TD.netDensity, STK07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
STK07_QTg2 <- data.frame(STK07_QT)
STK07_QTg2 <- STK07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK07_QTg2$player1
player2vector <- STK07_QTg2$player2
STK07_QTg3 <- STK07_QTg2
STK07_QTg3$p1inp2vec <- is.element(STK07_QTg3$player1, player2vector)
STK07_QTg3$p2inp1vec <- is.element(STK07_QTg3$player2, player1vector)

addPlayer1 <- STK07_QTg3[ which(STK07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK07_QTg3[ which(STK07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK07_QTg2 <- rbind(STK07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
STK07_QTft <- ftable(STK07_QTg2$player1, STK07_QTg2$player2)
STK07_QTft2 <- as.matrix(STK07_QTft)
numRows <- nrow(STK07_QTft2)
numCols <- ncol(STK07_QTft2)
STK07_QTft3 <- STK07_QTft2[c(2:numRows) , c(2:numCols)]
STK07_QTTable <- graph.adjacency(STK07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(STK07_QTTable, vertex.label = V(STK07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
STK07_QT.clusterCoef <- transitivity(STK07_QTTable, type="global") #cluster coefficient
STK07_QT.degreeCent <- centralization.degree(STK07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK07_QTftn <- as.network.matrix(STK07_QTft)
STK07_QT.netDensity <- network.density(STK07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK07_QT.entropy <- entropy(STK07_QTft) #entropy

STK07_QT.netMx <- cbind(STK07_QT.netMx, STK07_QT.clusterCoef, STK07_QT.degreeCent$centralization,
                        STK07_QT.netDensity, STK07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK07_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "SYD"
KIoutcome = "Goal_F"
SYD07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
SYD07_Gg2 <- data.frame(SYD07_G)
SYD07_Gg2 <- SYD07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_Gg2$player1
player2vector <- SYD07_Gg2$player2
SYD07_Gg3 <- SYD07_Gg2
SYD07_Gg3$p1inp2vec <- is.element(SYD07_Gg3$player1, player2vector)
SYD07_Gg3$p2inp1vec <- is.element(SYD07_Gg3$player2, player1vector)

addPlayer1 <- SYD07_Gg3[ which(SYD07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_Gg3[ which(SYD07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_Gg2 <- rbind(SYD07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
SYD07_Gft <- ftable(SYD07_Gg2$player1, SYD07_Gg2$player2)
SYD07_Gft2 <- as.matrix(SYD07_Gft)
numRows <- nrow(SYD07_Gft2)
numCols <- ncol(SYD07_Gft2)
SYD07_Gft3 <- SYD07_Gft2[c(2:numRows) , c(2:numCols)]
SYD07_GTable <- graph.adjacency(SYD07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(SYD07_GTable, vertex.label = V(SYD07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
SYD07_G.clusterCoef <- transitivity(SYD07_GTable, type="global") #cluster coefficient
SYD07_G.degreeCent <- centralization.degree(SYD07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_Gftn <- as.network.matrix(SYD07_Gft)
SYD07_G.netDensity <- network.density(SYD07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_G.entropy <- entropy(SYD07_Gft) #entropy

SYD07_G.netMx <- cbind(SYD07_G.netMx, SYD07_G.clusterCoef, SYD07_G.degreeCent$centralization,
                       SYD07_G.netDensity, SYD07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************

round = 7
teamName = "SYD"
KIoutcome = "Behind_F"
SYD07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
SYD07_Bg2 <- data.frame(SYD07_B)
SYD07_Bg2 <- SYD07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_Bg2$player1
player2vector <- SYD07_Bg2$player2
SYD07_Bg3 <- SYD07_Bg2
SYD07_Bg3$p1inp2vec <- is.element(SYD07_Bg3$player1, player2vector)
SYD07_Bg3$p2inp1vec <- is.element(SYD07_Bg3$player2, player1vector)

addPlayer1 <- SYD07_Bg3[ which(SYD07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_Bg3[ which(SYD07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_Bg2 <- rbind(SYD07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
SYD07_Bft <- ftable(SYD07_Bg2$player1, SYD07_Bg2$player2)
SYD07_Bft2 <- as.matrix(SYD07_Bft)
numRows <- nrow(SYD07_Bft2)
numCols <- ncol(SYD07_Bft2)
SYD07_Bft3 <- SYD07_Bft2[c(2:numRows) , c(2:numCols)]
SYD07_BTable <- graph.adjacency(SYD07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(SYD07_BTable, vertex.label = V(SYD07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
SYD07_B.clusterCoef <- transitivity(SYD07_BTable, type="global") #cluster coefficient
SYD07_B.degreeCent <- centralization.degree(SYD07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_Bftn <- as.network.matrix(SYD07_Bft)
SYD07_B.netDensity <- network.density(SYD07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_B.entropy <- entropy(SYD07_Bft) #entropy

SYD07_B.netMx <- cbind(SYD07_B.netMx, SYD07_B.clusterCoef, SYD07_B.degreeCent$centralization,
                       SYD07_B.netDensity, SYD07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************
#NA

round = 7
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
SYD07_SFg2 <- data.frame(SYD07_SF)
SYD07_SFg2 <- SYD07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_SFg2$player1
player2vector <- SYD07_SFg2$player2
SYD07_SFg3 <- SYD07_SFg2
SYD07_SFg3$p1inp2vec <- is.element(SYD07_SFg3$player1, player2vector)
SYD07_SFg3$p2inp1vec <- is.element(SYD07_SFg3$player2, player1vector)

addPlayer1 <- SYD07_SFg3[ which(SYD07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_SFg3[ which(SYD07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_SFg2 <- rbind(SYD07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
SYD07_SFft <- ftable(SYD07_SFg2$player1, SYD07_SFg2$player2)
SYD07_SFft2 <- as.matrix(SYD07_SFft)
numRows <- nrow(SYD07_SFft2)
numCols <- ncol(SYD07_SFft2)
SYD07_SFft3 <- SYD07_SFft2[c(2:numRows) , c(2:numCols)]
SYD07_SFTable <- graph.adjacency(SYD07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(SYD07_SFTable, vertex.label = V(SYD07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
SYD07_SF.clusterCoef <- transitivity(SYD07_SFTable, type="global") #cluster coefficient
SYD07_SF.degreeCent <- centralization.degree(SYD07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_SFftn <- as.network.matrix(SYD07_SFft)
SYD07_SF.netDensity <- network.density(SYD07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_SF.entropy <- entropy(SYD07_SFft) #entropy

SYD07_SF.netMx <- cbind(SYD07_SF.netMx, SYD07_SF.clusterCoef, SYD07_SF.degreeCent$centralization,
                        SYD07_SF.netDensity, SYD07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
SYD07_TFg2 <- data.frame(SYD07_TF)
SYD07_TFg2 <- SYD07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_TFg2$player1
player2vector <- SYD07_TFg2$player2
SYD07_TFg3 <- SYD07_TFg2
SYD07_TFg3$p1inp2vec <- is.element(SYD07_TFg3$player1, player2vector)
SYD07_TFg3$p2inp1vec <- is.element(SYD07_TFg3$player2, player1vector)

addPlayer1 <- SYD07_TFg3[ which(SYD07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_TFg3[ which(SYD07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_TFg2 <- rbind(SYD07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
SYD07_TFft <- ftable(SYD07_TFg2$player1, SYD07_TFg2$player2)
SYD07_TFft2 <- as.matrix(SYD07_TFft)
numRows <- nrow(SYD07_TFft2)
numCols <- ncol(SYD07_TFft2)
SYD07_TFft3 <- SYD07_TFft2[c(2:numRows) , c(2:numCols)]
SYD07_TFTable <- graph.adjacency(SYD07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(SYD07_TFTable, vertex.label = V(SYD07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
SYD07_TF.clusterCoef <- transitivity(SYD07_TFTable, type="global") #cluster coefficient
SYD07_TF.degreeCent <- centralization.degree(SYD07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_TFftn <- as.network.matrix(SYD07_TFft)
SYD07_TF.netDensity <- network.density(SYD07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_TF.entropy <- entropy(SYD07_TFft) #entropy

SYD07_TF.netMx <- cbind(SYD07_TF.netMx, SYD07_TF.clusterCoef, SYD07_TF.degreeCent$centralization,
                        SYD07_TF.netDensity, SYD07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
SYD07_SAMg2 <- data.frame(SYD07_SAM)
SYD07_SAMg2 <- SYD07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_SAMg2$player1
player2vector <- SYD07_SAMg2$player2
SYD07_SAMg3 <- SYD07_SAMg2
SYD07_SAMg3$p1inp2vec <- is.element(SYD07_SAMg3$player1, player2vector)
SYD07_SAMg3$p2inp1vec <- is.element(SYD07_SAMg3$player2, player1vector)

addPlayer1 <- SYD07_SAMg3[ which(SYD07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_SAMg3[ which(SYD07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_SAMg2 <- rbind(SYD07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
SYD07_SAMft <- ftable(SYD07_SAMg2$player1, SYD07_SAMg2$player2)
SYD07_SAMft2 <- as.matrix(SYD07_SAMft)
numRows <- nrow(SYD07_SAMft2)
numCols <- ncol(SYD07_SAMft2)
SYD07_SAMft3 <- SYD07_SAMft2[c(2:numRows) , c(2:numCols)]
SYD07_SAMTable <- graph.adjacency(SYD07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(SYD07_SAMTable, vertex.label = V(SYD07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
SYD07_SAM.clusterCoef <- transitivity(SYD07_SAMTable, type="global") #cluster coefficient
SYD07_SAM.degreeCent <- centralization.degree(SYD07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_SAMftn <- as.network.matrix(SYD07_SAMft)
SYD07_SAM.netDensity <- network.density(SYD07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_SAM.entropy <- entropy(SYD07_SAMft) #entropy

SYD07_SAM.netMx <- cbind(SYD07_SAM.netMx, SYD07_SAM.clusterCoef, SYD07_SAM.degreeCent$centralization,
                         SYD07_SAM.netDensity, SYD07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
SYD07_TAMg2 <- data.frame(SYD07_TAM)
SYD07_TAMg2 <- SYD07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_TAMg2$player1
player2vector <- SYD07_TAMg2$player2
SYD07_TAMg3 <- SYD07_TAMg2
SYD07_TAMg3$p1inp2vec <- is.element(SYD07_TAMg3$player1, player2vector)
SYD07_TAMg3$p2inp1vec <- is.element(SYD07_TAMg3$player2, player1vector)

addPlayer1 <- SYD07_TAMg3[ which(SYD07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_TAMg3[ which(SYD07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_TAMg2 <- rbind(SYD07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
SYD07_TAMft <- ftable(SYD07_TAMg2$player1, SYD07_TAMg2$player2)
SYD07_TAMft2 <- as.matrix(SYD07_TAMft)
numRows <- nrow(SYD07_TAMft2)
numCols <- ncol(SYD07_TAMft2)
SYD07_TAMft3 <- SYD07_TAMft2[c(2:numRows) , c(2:numCols)]
SYD07_TAMTable <- graph.adjacency(SYD07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(SYD07_TAMTable, vertex.label = V(SYD07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
SYD07_TAM.clusterCoef <- transitivity(SYD07_TAMTable, type="global") #cluster coefficient
SYD07_TAM.degreeCent <- centralization.degree(SYD07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_TAMftn <- as.network.matrix(SYD07_TAMft)
SYD07_TAM.netDensity <- network.density(SYD07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_TAM.entropy <- entropy(SYD07_TAMft) #entropy

SYD07_TAM.netMx <- cbind(SYD07_TAM.netMx, SYD07_TAM.clusterCoef, SYD07_TAM.degreeCent$centralization,
                         SYD07_TAM.netDensity, SYD07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
SYD07_SDMg2 <- data.frame(SYD07_SDM)
SYD07_SDMg2 <- SYD07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_SDMg2$player1
player2vector <- SYD07_SDMg2$player2
SYD07_SDMg3 <- SYD07_SDMg2
SYD07_SDMg3$p1inp2vec <- is.element(SYD07_SDMg3$player1, player2vector)
SYD07_SDMg3$p2inp1vec <- is.element(SYD07_SDMg3$player2, player1vector)

addPlayer1 <- SYD07_SDMg3[ which(SYD07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_SDMg3[ which(SYD07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_SDMg2 <- rbind(SYD07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
SYD07_SDMft <- ftable(SYD07_SDMg2$player1, SYD07_SDMg2$player2)
SYD07_SDMft2 <- as.matrix(SYD07_SDMft)
numRows <- nrow(SYD07_SDMft2)
numCols <- ncol(SYD07_SDMft2)
SYD07_SDMft3 <- SYD07_SDMft2[c(2:numRows) , c(2:numCols)]
SYD07_SDMTable <- graph.adjacency(SYD07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(SYD07_SDMTable, vertex.label = V(SYD07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
SYD07_SDM.clusterCoef <- transitivity(SYD07_SDMTable, type="global") #cluster coefficient
SYD07_SDM.degreeCent <- centralization.degree(SYD07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_SDMftn <- as.network.matrix(SYD07_SDMft)
SYD07_SDM.netDensity <- network.density(SYD07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_SDM.entropy <- entropy(SYD07_SDMft) #entropy

SYD07_SDM.netMx <- cbind(SYD07_SDM.netMx, SYD07_SDM.clusterCoef, SYD07_SDM.degreeCent$centralization,
                         SYD07_SDM.netDensity, SYD07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
SYD07_TDMg2 <- data.frame(SYD07_TDM)
SYD07_TDMg2 <- SYD07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_TDMg2$player1
player2vector <- SYD07_TDMg2$player2
SYD07_TDMg3 <- SYD07_TDMg2
SYD07_TDMg3$p1inp2vec <- is.element(SYD07_TDMg3$player1, player2vector)
SYD07_TDMg3$p2inp1vec <- is.element(SYD07_TDMg3$player2, player1vector)

addPlayer1 <- SYD07_TDMg3[ which(SYD07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_TDMg3[ which(SYD07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_TDMg2 <- rbind(SYD07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
SYD07_TDMft <- ftable(SYD07_TDMg2$player1, SYD07_TDMg2$player2)
SYD07_TDMft2 <- as.matrix(SYD07_TDMft)
numRows <- nrow(SYD07_TDMft2)
numCols <- ncol(SYD07_TDMft2)
SYD07_TDMft3 <- SYD07_TDMft2[c(2:numRows) , c(2:numCols)]
SYD07_TDMTable <- graph.adjacency(SYD07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(SYD07_TDMTable, vertex.label = V(SYD07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
SYD07_TDM.clusterCoef <- transitivity(SYD07_TDMTable, type="global") #cluster coefficient
SYD07_TDM.degreeCent <- centralization.degree(SYD07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_TDMftn <- as.network.matrix(SYD07_TDMft)
SYD07_TDM.netDensity <- network.density(SYD07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_TDM.entropy <- entropy(SYD07_TDMft) #entropy

SYD07_TDM.netMx <- cbind(SYD07_TDM.netMx, SYD07_TDM.clusterCoef, SYD07_TDM.degreeCent$centralization,
                         SYD07_TDM.netDensity, SYD07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
SYD07_SDg2 <- data.frame(SYD07_SD)
SYD07_SDg2 <- SYD07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_SDg2$player1
player2vector <- SYD07_SDg2$player2
SYD07_SDg3 <- SYD07_SDg2
SYD07_SDg3$p1inp2vec <- is.element(SYD07_SDg3$player1, player2vector)
SYD07_SDg3$p2inp1vec <- is.element(SYD07_SDg3$player2, player1vector)

addPlayer1 <- SYD07_SDg3[ which(SYD07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_SDg3[ which(SYD07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_SDg2 <- rbind(SYD07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
SYD07_SDft <- ftable(SYD07_SDg2$player1, SYD07_SDg2$player2)
SYD07_SDft2 <- as.matrix(SYD07_SDft)
numRows <- nrow(SYD07_SDft2)
numCols <- ncol(SYD07_SDft2)
SYD07_SDft3 <- SYD07_SDft2[c(2:numRows) , c(2:numCols)]
SYD07_SDTable <- graph.adjacency(SYD07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(SYD07_SDTable, vertex.label = V(SYD07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
SYD07_SD.clusterCoef <- transitivity(SYD07_SDTable, type="global") #cluster coefficient
SYD07_SD.degreeCent <- centralization.degree(SYD07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_SDftn <- as.network.matrix(SYD07_SDft)
SYD07_SD.netDensity <- network.density(SYD07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_SD.entropy <- entropy(SYD07_SDft) #entropy

SYD07_SD.netMx <- cbind(SYD07_SD.netMx, SYD07_SD.clusterCoef, SYD07_SD.degreeCent$centralization,
                        SYD07_SD.netDensity, SYD07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
SYD07_TDg2 <- data.frame(SYD07_TD)
SYD07_TDg2 <- SYD07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_TDg2$player1
player2vector <- SYD07_TDg2$player2
SYD07_TDg3 <- SYD07_TDg2
SYD07_TDg3$p1inp2vec <- is.element(SYD07_TDg3$player1, player2vector)
SYD07_TDg3$p2inp1vec <- is.element(SYD07_TDg3$player2, player1vector)

addPlayer1 <- SYD07_TDg3[ which(SYD07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_TDg3[ which(SYD07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_TDg2 <- rbind(SYD07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
SYD07_TDft <- ftable(SYD07_TDg2$player1, SYD07_TDg2$player2)
SYD07_TDft2 <- as.matrix(SYD07_TDft)
numRows <- nrow(SYD07_TDft2)
numCols <- ncol(SYD07_TDft2)
SYD07_TDft3 <- SYD07_TDft2[c(2:numRows) , c(2:numCols)]
SYD07_TDTable <- graph.adjacency(SYD07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(SYD07_TDTable, vertex.label = V(SYD07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
SYD07_TD.clusterCoef <- transitivity(SYD07_TDTable, type="global") #cluster coefficient
SYD07_TD.degreeCent <- centralization.degree(SYD07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_TDftn <- as.network.matrix(SYD07_TDft)
SYD07_TD.netDensity <- network.density(SYD07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_TD.entropy <- entropy(SYD07_TDft) #entropy

SYD07_TD.netMx <- cbind(SYD07_TD.netMx, SYD07_TD.clusterCoef, SYD07_TD.degreeCent$centralization,
                        SYD07_TD.netDensity, SYD07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
SYD07_QTg2 <- data.frame(SYD07_QT)
SYD07_QTg2 <- SYD07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD07_QTg2$player1
player2vector <- SYD07_QTg2$player2
SYD07_QTg3 <- SYD07_QTg2
SYD07_QTg3$p1inp2vec <- is.element(SYD07_QTg3$player1, player2vector)
SYD07_QTg3$p2inp1vec <- is.element(SYD07_QTg3$player2, player1vector)

addPlayer1 <- SYD07_QTg3[ which(SYD07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD07_QTg3[ which(SYD07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD07_QTg2 <- rbind(SYD07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
SYD07_QTft <- ftable(SYD07_QTg2$player1, SYD07_QTg2$player2)
SYD07_QTft2 <- as.matrix(SYD07_QTft)
numRows <- nrow(SYD07_QTft2)
numCols <- ncol(SYD07_QTft2)
SYD07_QTft3 <- SYD07_QTft2[c(2:numRows) , c(2:numCols)]
SYD07_QTTable <- graph.adjacency(SYD07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(SYD07_QTTable, vertex.label = V(SYD07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
SYD07_QT.clusterCoef <- transitivity(SYD07_QTTable, type="global") #cluster coefficient
SYD07_QT.degreeCent <- centralization.degree(SYD07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD07_QTftn <- as.network.matrix(SYD07_QTft)
SYD07_QT.netDensity <- network.density(SYD07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD07_QT.entropy <- entropy(SYD07_QTft) #entropy

SYD07_QT.netMx <- cbind(SYD07_QT.netMx, SYD07_QT.clusterCoef, SYD07_QT.degreeCent$centralization,
                        SYD07_QT.netDensity, SYD07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD07_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************

round = 7
teamName = "WB"
KIoutcome = "Goal_F"
WB07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
WB07_Gg2 <- data.frame(WB07_G)
WB07_Gg2 <- WB07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_Gg2$player1
player2vector <- WB07_Gg2$player2
WB07_Gg3 <- WB07_Gg2
WB07_Gg3$p1inp2vec <- is.element(WB07_Gg3$player1, player2vector)
WB07_Gg3$p2inp1vec <- is.element(WB07_Gg3$player2, player1vector)

addPlayer1 <- WB07_Gg3[ which(WB07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_Gg3[ which(WB07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_Gg2 <- rbind(WB07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
WB07_Gft <- ftable(WB07_Gg2$player1, WB07_Gg2$player2)
WB07_Gft2 <- as.matrix(WB07_Gft)
numRows <- nrow(WB07_Gft2)
numCols <- ncol(WB07_Gft2)
WB07_Gft3 <- WB07_Gft2[c(2:numRows) , c(2:numCols)]
WB07_GTable <- graph.adjacency(WB07_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(WB07_GTable, vertex.label = V(WB07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
WB07_G.clusterCoef <- transitivity(WB07_GTable, type="global") #cluster coefficient
WB07_G.degreeCent <- centralization.degree(WB07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_Gftn <- as.network.matrix(WB07_Gft)
WB07_G.netDensity <- network.density(WB07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_G.entropy <- entropy(WB07_Gft) #entropy

WB07_G.netMx <- cbind(WB07_G.netMx, WB07_G.clusterCoef, WB07_G.degreeCent$centralization,
                      WB07_G.netDensity, WB07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "WB"
KIoutcome = "Behind_F"
WB07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
WB07_Bg2 <- data.frame(WB07_B)
WB07_Bg2 <- WB07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_Bg2$player1
player2vector <- WB07_Bg2$player2
WB07_Bg3 <- WB07_Bg2
WB07_Bg3$p1inp2vec <- is.element(WB07_Bg3$player1, player2vector)
WB07_Bg3$p2inp1vec <- is.element(WB07_Bg3$player2, player1vector)

addPlayer1 <- WB07_Bg3[ which(WB07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_Bg3[ which(WB07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_Bg2 <- rbind(WB07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
WB07_Bft <- ftable(WB07_Bg2$player1, WB07_Bg2$player2)
WB07_Bft2 <- as.matrix(WB07_Bft)
numRows <- nrow(WB07_Bft2)
numCols <- ncol(WB07_Bft2)
WB07_Bft3 <- WB07_Bft2[c(2:numRows) , c(2:numCols)]
WB07_BTable <- graph.adjacency(WB07_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(WB07_BTable, vertex.label = V(WB07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
WB07_B.clusterCoef <- transitivity(WB07_BTable, type="global") #cluster coefficient
WB07_B.degreeCent <- centralization.degree(WB07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_Bftn <- as.network.matrix(WB07_Bft)
WB07_B.netDensity <- network.density(WB07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_B.entropy <- entropy(WB07_Bft) #entropy

WB07_B.netMx <- cbind(WB07_B.netMx, WB07_B.clusterCoef, WB07_B.degreeCent$centralization,
                      WB07_B.netDensity, WB07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************

round = 7
teamName = "WB"
KIoutcome = "Stoppage_F"
WB07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
WB07_SFg2 <- data.frame(WB07_SF)
WB07_SFg2 <- WB07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_SFg2$player1
player2vector <- WB07_SFg2$player2
WB07_SFg3 <- WB07_SFg2
WB07_SFg3$p1inp2vec <- is.element(WB07_SFg3$player1, player2vector)
WB07_SFg3$p2inp1vec <- is.element(WB07_SFg3$player2, player1vector)

addPlayer1 <- WB07_SFg3[ which(WB07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_SFg3[ which(WB07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_SFg2 <- rbind(WB07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
WB07_SFft <- ftable(WB07_SFg2$player1, WB07_SFg2$player2)
WB07_SFft2 <- as.matrix(WB07_SFft)
numRows <- nrow(WB07_SFft2)
numCols <- ncol(WB07_SFft2)
WB07_SFft3 <- WB07_SFft2[c(2:numRows) , c(2:numCols)]
WB07_SFTable <- graph.adjacency(WB07_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(WB07_SFTable, vertex.label = V(WB07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
WB07_SF.clusterCoef <- transitivity(WB07_SFTable, type="global") #cluster coefficient
WB07_SF.degreeCent <- centralization.degree(WB07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_SFftn <- as.network.matrix(WB07_SFft)
WB07_SF.netDensity <- network.density(WB07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_SF.entropy <- entropy(WB07_SFft) #entropy

WB07_SF.netMx <- cbind(WB07_SF.netMx, WB07_SF.clusterCoef, WB07_SF.degreeCent$centralization,
                       WB07_SF.netDensity, WB07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************

round = 7
teamName = "WB"
KIoutcome = "Turnover_F"
WB07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
WB07_TFg2 <- data.frame(WB07_TF)
WB07_TFg2 <- WB07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_TFg2$player1
player2vector <- WB07_TFg2$player2
WB07_TFg3 <- WB07_TFg2
WB07_TFg3$p1inp2vec <- is.element(WB07_TFg3$player1, player2vector)
WB07_TFg3$p2inp1vec <- is.element(WB07_TFg3$player2, player1vector)

addPlayer1 <- WB07_TFg3[ which(WB07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_TFg3[ which(WB07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_TFg2 <- rbind(WB07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
WB07_TFft <- ftable(WB07_TFg2$player1, WB07_TFg2$player2)
WB07_TFft2 <- as.matrix(WB07_TFft)
numRows <- nrow(WB07_TFft2)
numCols <- ncol(WB07_TFft2)
WB07_TFft3 <- WB07_TFft2[c(2:numRows) , c(2:numCols)]
WB07_TFTable <- graph.adjacency(WB07_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(WB07_TFTable, vertex.label = V(WB07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
WB07_TF.clusterCoef <- transitivity(WB07_TFTable, type="global") #cluster coefficient
WB07_TF.degreeCent <- centralization.degree(WB07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_TFftn <- as.network.matrix(WB07_TFft)
WB07_TF.netDensity <- network.density(WB07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_TF.entropy <- entropy(WB07_TFft) #entropy

WB07_TF.netMx <- cbind(WB07_TF.netMx, WB07_TF.clusterCoef, WB07_TF.degreeCent$centralization,
                       WB07_TF.netDensity, WB07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************
#NA

round = 7
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
WB07_SAMg2 <- data.frame(WB07_SAM)
WB07_SAMg2 <- WB07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_SAMg2$player1
player2vector <- WB07_SAMg2$player2
WB07_SAMg3 <- WB07_SAMg2
WB07_SAMg3$p1inp2vec <- is.element(WB07_SAMg3$player1, player2vector)
WB07_SAMg3$p2inp1vec <- is.element(WB07_SAMg3$player2, player1vector)

addPlayer1 <- WB07_SAMg3[ which(WB07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_SAMg3[ which(WB07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_SAMg2 <- rbind(WB07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
WB07_SAMft <- ftable(WB07_SAMg2$player1, WB07_SAMg2$player2)
WB07_SAMft2 <- as.matrix(WB07_SAMft)
numRows <- nrow(WB07_SAMft2)
numCols <- ncol(WB07_SAMft2)
WB07_SAMft3 <- WB07_SAMft2[c(2:numRows) , c(2:numCols)]
WB07_SAMTable <- graph.adjacency(WB07_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(WB07_SAMTable, vertex.label = V(WB07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
WB07_SAM.clusterCoef <- transitivity(WB07_SAMTable, type="global") #cluster coefficient
WB07_SAM.degreeCent <- centralization.degree(WB07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_SAMftn <- as.network.matrix(WB07_SAMft)
WB07_SAM.netDensity <- network.density(WB07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_SAM.entropy <- entropy(WB07_SAMft) #entropy

WB07_SAM.netMx <- cbind(WB07_SAM.netMx, WB07_SAM.clusterCoef, WB07_SAM.degreeCent$centralization,
                        WB07_SAM.netDensity, WB07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "WB"
KIoutcome = "Turnover_AM"
WB07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
WB07_TAMg2 <- data.frame(WB07_TAM)
WB07_TAMg2 <- WB07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_TAMg2$player1
player2vector <- WB07_TAMg2$player2
WB07_TAMg3 <- WB07_TAMg2
WB07_TAMg3$p1inp2vec <- is.element(WB07_TAMg3$player1, player2vector)
WB07_TAMg3$p2inp1vec <- is.element(WB07_TAMg3$player2, player1vector)

addPlayer1 <- WB07_TAMg3[ which(WB07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_TAMg3[ which(WB07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_TAMg2 <- rbind(WB07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
WB07_TAMft <- ftable(WB07_TAMg2$player1, WB07_TAMg2$player2)
WB07_TAMft2 <- as.matrix(WB07_TAMft)
numRows <- nrow(WB07_TAMft2)
numCols <- ncol(WB07_TAMft2)
WB07_TAMft3 <- WB07_TAMft2[c(2:numRows) , c(2:numCols)]
WB07_TAMTable <- graph.adjacency(WB07_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(WB07_TAMTable, vertex.label = V(WB07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
WB07_TAM.clusterCoef <- transitivity(WB07_TAMTable, type="global") #cluster coefficient
WB07_TAM.degreeCent <- centralization.degree(WB07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_TAMftn <- as.network.matrix(WB07_TAMft)
WB07_TAM.netDensity <- network.density(WB07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_TAM.entropy <- entropy(WB07_TAMft) #entropy

WB07_TAM.netMx <- cbind(WB07_TAM.netMx, WB07_TAM.clusterCoef, WB07_TAM.degreeCent$centralization,
                        WB07_TAM.netDensity, WB07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
WB07_SDMg2 <- data.frame(WB07_SDM)
WB07_SDMg2 <- WB07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_SDMg2$player1
player2vector <- WB07_SDMg2$player2
WB07_SDMg3 <- WB07_SDMg2
WB07_SDMg3$p1inp2vec <- is.element(WB07_SDMg3$player1, player2vector)
WB07_SDMg3$p2inp1vec <- is.element(WB07_SDMg3$player2, player1vector)

addPlayer1 <- WB07_SDMg3[ which(WB07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_SDMg3[ which(WB07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_SDMg2 <- rbind(WB07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
WB07_SDMft <- ftable(WB07_SDMg2$player1, WB07_SDMg2$player2)
WB07_SDMft2 <- as.matrix(WB07_SDMft)
numRows <- nrow(WB07_SDMft2)
numCols <- ncol(WB07_SDMft2)
WB07_SDMft3 <- WB07_SDMft2[c(2:numRows) , c(2:numCols)]
WB07_SDMTable <- graph.adjacency(WB07_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(WB07_SDMTable, vertex.label = V(WB07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
WB07_SDM.clusterCoef <- transitivity(WB07_SDMTable, type="global") #cluster coefficient
WB07_SDM.degreeCent <- centralization.degree(WB07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_SDMftn <- as.network.matrix(WB07_SDMft)
WB07_SDM.netDensity <- network.density(WB07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_SDM.entropy <- entropy(WB07_SDMft) #entropy

WB07_SDM.netMx <- cbind(WB07_SDM.netMx, WB07_SDM.clusterCoef, WB07_SDM.degreeCent$centralization,
                        WB07_SDM.netDensity, WB07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************

round = 7
teamName = "WB"
KIoutcome = "Turnover_DM"
WB07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
WB07_TDMg2 <- data.frame(WB07_TDM)
WB07_TDMg2 <- WB07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_TDMg2$player1
player2vector <- WB07_TDMg2$player2
WB07_TDMg3 <- WB07_TDMg2
WB07_TDMg3$p1inp2vec <- is.element(WB07_TDMg3$player1, player2vector)
WB07_TDMg3$p2inp1vec <- is.element(WB07_TDMg3$player2, player1vector)

addPlayer1 <- WB07_TDMg3[ which(WB07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_TDMg3[ which(WB07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_TDMg2 <- rbind(WB07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
WB07_TDMft <- ftable(WB07_TDMg2$player1, WB07_TDMg2$player2)
WB07_TDMft2 <- as.matrix(WB07_TDMft)
numRows <- nrow(WB07_TDMft2)
numCols <- ncol(WB07_TDMft2)
WB07_TDMft3 <- WB07_TDMft2[c(2:numRows) , c(2:numCols)]
WB07_TDMTable <- graph.adjacency(WB07_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(WB07_TDMTable, vertex.label = V(WB07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
WB07_TDM.clusterCoef <- transitivity(WB07_TDMTable, type="global") #cluster coefficient
WB07_TDM.degreeCent <- centralization.degree(WB07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_TDMftn <- as.network.matrix(WB07_TDMft)
WB07_TDM.netDensity <- network.density(WB07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_TDM.entropy <- entropy(WB07_TDMft) #entropy

WB07_TDM.netMx <- cbind(WB07_TDM.netMx, WB07_TDM.clusterCoef, WB07_TDM.degreeCent$centralization,
                        WB07_TDM.netDensity, WB07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "WB"
KIoutcome = "Stoppage_D"
WB07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
WB07_SDg2 <- data.frame(WB07_SD)
WB07_SDg2 <- WB07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_SDg2$player1
player2vector <- WB07_SDg2$player2
WB07_SDg3 <- WB07_SDg2
WB07_SDg3$p1inp2vec <- is.element(WB07_SDg3$player1, player2vector)
WB07_SDg3$p2inp1vec <- is.element(WB07_SDg3$player2, player1vector)

addPlayer1 <- WB07_SDg3[ which(WB07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_SDg3[ which(WB07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_SDg2 <- rbind(WB07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
WB07_SDft <- ftable(WB07_SDg2$player1, WB07_SDg2$player2)
WB07_SDft2 <- as.matrix(WB07_SDft)
numRows <- nrow(WB07_SDft2)
numCols <- ncol(WB07_SDft2)
WB07_SDft3 <- WB07_SDft2[c(2:numRows) , c(2:numCols)]
WB07_SDTable <- graph.adjacency(WB07_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(WB07_SDTable, vertex.label = V(WB07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
WB07_SD.clusterCoef <- transitivity(WB07_SDTable, type="global") #cluster coefficient
WB07_SD.degreeCent <- centralization.degree(WB07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_SDftn <- as.network.matrix(WB07_SDft)
WB07_SD.netDensity <- network.density(WB07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_SD.entropy <- entropy(WB07_SDft) #entropy

WB07_SD.netMx <- cbind(WB07_SD.netMx, WB07_SD.clusterCoef, WB07_SD.degreeCent$centralization,
                       WB07_SD.netDensity, WB07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "WB"
KIoutcome = "Turnover_D"
WB07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
WB07_TDg2 <- data.frame(WB07_TD)
WB07_TDg2 <- WB07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_TDg2$player1
player2vector <- WB07_TDg2$player2
WB07_TDg3 <- WB07_TDg2
WB07_TDg3$p1inp2vec <- is.element(WB07_TDg3$player1, player2vector)
WB07_TDg3$p2inp1vec <- is.element(WB07_TDg3$player2, player1vector)

addPlayer1 <- WB07_TDg3[ which(WB07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_TDg3[ which(WB07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_TDg2 <- rbind(WB07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
WB07_TDft <- ftable(WB07_TDg2$player1, WB07_TDg2$player2)
WB07_TDft2 <- as.matrix(WB07_TDft)
numRows <- nrow(WB07_TDft2)
numCols <- ncol(WB07_TDft2)
WB07_TDft3 <- WB07_TDft2[c(2:numRows) , c(2:numCols)]
WB07_TDTable <- graph.adjacency(WB07_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(WB07_TDTable, vertex.label = V(WB07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
WB07_TD.clusterCoef <- transitivity(WB07_TDTable, type="global") #cluster coefficient
WB07_TD.degreeCent <- centralization.degree(WB07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_TDftn <- as.network.matrix(WB07_TDft)
WB07_TD.netDensity <- network.density(WB07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_TD.entropy <- entropy(WB07_TDft) #entropy

WB07_TD.netMx <- cbind(WB07_TD.netMx, WB07_TD.clusterCoef, WB07_TD.degreeCent$centralization,
                       WB07_TD.netDensity, WB07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
WB07_QTg2 <- data.frame(WB07_QT)
WB07_QTg2 <- WB07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB07_QTg2$player1
player2vector <- WB07_QTg2$player2
WB07_QTg3 <- WB07_QTg2
WB07_QTg3$p1inp2vec <- is.element(WB07_QTg3$player1, player2vector)
WB07_QTg3$p2inp1vec <- is.element(WB07_QTg3$player2, player1vector)

addPlayer1 <- WB07_QTg3[ which(WB07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB07_QTg3[ which(WB07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB07_QTg2 <- rbind(WB07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
WB07_QTft <- ftable(WB07_QTg2$player1, WB07_QTg2$player2)
WB07_QTft2 <- as.matrix(WB07_QTft)
numRows <- nrow(WB07_QTft2)
numCols <- ncol(WB07_QTft2)
WB07_QTft3 <- WB07_QTft2[c(2:numRows) , c(2:numCols)]
WB07_QTTable <- graph.adjacency(WB07_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(WB07_QTTable, vertex.label = V(WB07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
WB07_QT.clusterCoef <- transitivity(WB07_QTTable, type="global") #cluster coefficient
WB07_QT.degreeCent <- centralization.degree(WB07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB07_QTftn <- as.network.matrix(WB07_QTft)
WB07_QT.netDensity <- network.density(WB07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB07_QT.entropy <- entropy(WB07_QTft) #entropy

WB07_QT.netMx <- cbind(WB07_QT.netMx, WB07_QT.clusterCoef, WB07_QT.degreeCent$centralization,
                       WB07_QT.netDensity, WB07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB07_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 7
##

#ROUND 7, Goal***************************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Goal_F"
WCE07_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Goal with weighted edges
WCE07_Gg2 <- data.frame(WCE07_G)
WCE07_Gg2 <- WCE07_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_Gg2$player1
player2vector <- WCE07_Gg2$player2
WCE07_Gg3 <- WCE07_Gg2
WCE07_Gg3$p1inp2vec <- is.element(WCE07_Gg3$player1, player2vector)
WCE07_Gg3$p2inp1vec <- is.element(WCE07_Gg3$player2, player1vector)

addPlayer1 <- WCE07_Gg3[ which(WCE07_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_Gg3[ which(WCE07_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_Gg2 <- rbind(WCE07_Gg2, addPlayers)

#ROUND 7, Goal graph using weighted edges
WCE07_Gft <- ftable(WCE07_Gg2$player1, WCE07_Gg2$player2)
WCE07_Gft2 <- as.matrix(WCE07_Gft)
numRows <- nrow(WCE07_Gft2)
numCols <- ncol(WCE07_Gft2)
WCE07_Gft3 <- WCE07_Gft2[c(2:numRows) , c(2:numCols)]
WCE07_GTable <- graph.adjacency(WCE07_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Goal graph=weighted
plot.igraph(WCE07_GTable, vertex.label = V(WCE07_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Goal calulation of network metrics
#igraph
WCE07_G.clusterCoef <- transitivity(WCE07_GTable, type="global") #cluster coefficient
WCE07_G.degreeCent <- centralization.degree(WCE07_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_Gftn <- as.network.matrix(WCE07_Gft)
WCE07_G.netDensity <- network.density(WCE07_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_G.entropy <- entropy(WCE07_Gft) #entropy

WCE07_G.netMx <- cbind(WCE07_G.netMx, WCE07_G.clusterCoef, WCE07_G.degreeCent$centralization,
                       WCE07_G.netDensity, WCE07_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_G.netMx) <- varnames

#ROUND 7, Behind***************************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Behind_F"
WCE07_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, Behind with weighted edges
WCE07_Bg2 <- data.frame(WCE07_B)
WCE07_Bg2 <- WCE07_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_Bg2$player1
player2vector <- WCE07_Bg2$player2
WCE07_Bg3 <- WCE07_Bg2
WCE07_Bg3$p1inp2vec <- is.element(WCE07_Bg3$player1, player2vector)
WCE07_Bg3$p2inp1vec <- is.element(WCE07_Bg3$player2, player1vector)

addPlayer1 <- WCE07_Bg3[ which(WCE07_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_Bg3[ which(WCE07_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_Bg2 <- rbind(WCE07_Bg2, addPlayers)

#ROUND 7, Behind graph using weighted edges
WCE07_Bft <- ftable(WCE07_Bg2$player1, WCE07_Bg2$player2)
WCE07_Bft2 <- as.matrix(WCE07_Bft)
numRows <- nrow(WCE07_Bft2)
numCols <- ncol(WCE07_Bft2)
WCE07_Bft3 <- WCE07_Bft2[c(2:numRows) , c(2:numCols)]
WCE07_BTable <- graph.adjacency(WCE07_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 7, Behind graph=weighted
plot.igraph(WCE07_BTable, vertex.label = V(WCE07_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, Behind calulation of network metrics
#igraph
WCE07_B.clusterCoef <- transitivity(WCE07_BTable, type="global") #cluster coefficient
WCE07_B.degreeCent <- centralization.degree(WCE07_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_Bftn <- as.network.matrix(WCE07_Bft)
WCE07_B.netDensity <- network.density(WCE07_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_B.entropy <- entropy(WCE07_Bft) #entropy

WCE07_B.netMx <- cbind(WCE07_B.netMx, WCE07_B.clusterCoef, WCE07_B.degreeCent$centralization,
                       WCE07_B.netDensity, WCE07_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_B.netMx) <- varnames

#ROUND 7, FWD Stoppage**********************************************************

round = 7
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE07_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Stoppage with weighted edges
WCE07_SFg2 <- data.frame(WCE07_SF)
WCE07_SFg2 <- WCE07_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_SFg2$player1
player2vector <- WCE07_SFg2$player2
WCE07_SFg3 <- WCE07_SFg2
WCE07_SFg3$p1inp2vec <- is.element(WCE07_SFg3$player1, player2vector)
WCE07_SFg3$p2inp1vec <- is.element(WCE07_SFg3$player2, player1vector)

addPlayer1 <- WCE07_SFg3[ which(WCE07_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_SFg3[ which(WCE07_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_SFg2 <- rbind(WCE07_SFg2, addPlayers)

#ROUND 7, FWD Stoppage graph using weighted edges
WCE07_SFft <- ftable(WCE07_SFg2$player1, WCE07_SFg2$player2)
WCE07_SFft2 <- as.matrix(WCE07_SFft)
numRows <- nrow(WCE07_SFft2)
numCols <- ncol(WCE07_SFft2)
WCE07_SFft3 <- WCE07_SFft2[c(2:numRows) , c(2:numCols)]
WCE07_SFTable <- graph.adjacency(WCE07_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Stoppage graph=weighted
plot.igraph(WCE07_SFTable, vertex.label = V(WCE07_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Stoppage calulation of network metrics
#igraph
WCE07_SF.clusterCoef <- transitivity(WCE07_SFTable, type="global") #cluster coefficient
WCE07_SF.degreeCent <- centralization.degree(WCE07_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_SFftn <- as.network.matrix(WCE07_SFft)
WCE07_SF.netDensity <- network.density(WCE07_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_SF.entropy <- entropy(WCE07_SFft) #entropy

WCE07_SF.netMx <- cbind(WCE07_SF.netMx, WCE07_SF.clusterCoef, WCE07_SF.degreeCent$centralization,
                        WCE07_SF.netDensity, WCE07_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_SF.netMx) <- varnames

#ROUND 7, FWD Turnover**********************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE07_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, FWD Turnover with weighted edges
WCE07_TFg2 <- data.frame(WCE07_TF)
WCE07_TFg2 <- WCE07_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_TFg2$player1
player2vector <- WCE07_TFg2$player2
WCE07_TFg3 <- WCE07_TFg2
WCE07_TFg3$p1inp2vec <- is.element(WCE07_TFg3$player1, player2vector)
WCE07_TFg3$p2inp1vec <- is.element(WCE07_TFg3$player2, player1vector)

addPlayer1 <- WCE07_TFg3[ which(WCE07_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_TFg3[ which(WCE07_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_TFg2 <- rbind(WCE07_TFg2, addPlayers)

#ROUND 7, FWD Turnover graph using weighted edges
WCE07_TFft <- ftable(WCE07_TFg2$player1, WCE07_TFg2$player2)
WCE07_TFft2 <- as.matrix(WCE07_TFft)
numRows <- nrow(WCE07_TFft2)
numCols <- ncol(WCE07_TFft2)
WCE07_TFft3 <- WCE07_TFft2[c(2:numRows) , c(2:numCols)]
WCE07_TFTable <- graph.adjacency(WCE07_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, FWD Turnover graph=weighted
plot.igraph(WCE07_TFTable, vertex.label = V(WCE07_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, FWD Turnover calulation of network metrics
#igraph
WCE07_TF.clusterCoef <- transitivity(WCE07_TFTable, type="global") #cluster coefficient
WCE07_TF.degreeCent <- centralization.degree(WCE07_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_TFftn <- as.network.matrix(WCE07_TFft)
WCE07_TF.netDensity <- network.density(WCE07_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_TF.entropy <- entropy(WCE07_TFft) #entropy

WCE07_TF.netMx <- cbind(WCE07_TF.netMx, WCE07_TF.clusterCoef, WCE07_TF.degreeCent$centralization,
                        WCE07_TF.netDensity, WCE07_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_TF.netMx) <- varnames

#ROUND 7, AM Stoppage**********************************************************

round = 7
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE07_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Stoppage with weighted edges
WCE07_SAMg2 <- data.frame(WCE07_SAM)
WCE07_SAMg2 <- WCE07_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_SAMg2$player1
player2vector <- WCE07_SAMg2$player2
WCE07_SAMg3 <- WCE07_SAMg2
WCE07_SAMg3$p1inp2vec <- is.element(WCE07_SAMg3$player1, player2vector)
WCE07_SAMg3$p2inp1vec <- is.element(WCE07_SAMg3$player2, player1vector)

addPlayer1 <- WCE07_SAMg3[ which(WCE07_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_SAMg3[ which(WCE07_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_SAMg2 <- rbind(WCE07_SAMg2, addPlayers)

#ROUND 7, AM Stoppage graph using weighted edges
WCE07_SAMft <- ftable(WCE07_SAMg2$player1, WCE07_SAMg2$player2)
WCE07_SAMft2 <- as.matrix(WCE07_SAMft)
numRows <- nrow(WCE07_SAMft2)
numCols <- ncol(WCE07_SAMft2)
WCE07_SAMft3 <- WCE07_SAMft2[c(2:numRows) , c(2:numCols)]
WCE07_SAMTable <- graph.adjacency(WCE07_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Stoppage graph=weighted
plot.igraph(WCE07_SAMTable, vertex.label = V(WCE07_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Stoppage calulation of network metrics
#igraph
WCE07_SAM.clusterCoef <- transitivity(WCE07_SAMTable, type="global") #cluster coefficient
WCE07_SAM.degreeCent <- centralization.degree(WCE07_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_SAMftn <- as.network.matrix(WCE07_SAMft)
WCE07_SAM.netDensity <- network.density(WCE07_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_SAM.entropy <- entropy(WCE07_SAMft) #entropy

WCE07_SAM.netMx <- cbind(WCE07_SAM.netMx, WCE07_SAM.clusterCoef, WCE07_SAM.degreeCent$centralization,
                         WCE07_SAM.netDensity, WCE07_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_SAM.netMx) <- varnames

#ROUND 7, AM Turnover**********************************************************

round = 7
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE07_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, AM Turnover with weighted edges
WCE07_TAMg2 <- data.frame(WCE07_TAM)
WCE07_TAMg2 <- WCE07_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_TAMg2$player1
player2vector <- WCE07_TAMg2$player2
WCE07_TAMg3 <- WCE07_TAMg2
WCE07_TAMg3$p1inp2vec <- is.element(WCE07_TAMg3$player1, player2vector)
WCE07_TAMg3$p2inp1vec <- is.element(WCE07_TAMg3$player2, player1vector)

addPlayer1 <- WCE07_TAMg3[ which(WCE07_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_TAMg3[ which(WCE07_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_TAMg2 <- rbind(WCE07_TAMg2, addPlayers)

#ROUND 7, AM Turnover graph using weighted edges
WCE07_TAMft <- ftable(WCE07_TAMg2$player1, WCE07_TAMg2$player2)
WCE07_TAMft2 <- as.matrix(WCE07_TAMft)
numRows <- nrow(WCE07_TAMft2)
numCols <- ncol(WCE07_TAMft2)
WCE07_TAMft3 <- WCE07_TAMft2[c(2:numRows) , c(2:numCols)]
WCE07_TAMTable <- graph.adjacency(WCE07_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, AM Turnover graph=weighted
plot.igraph(WCE07_TAMTable, vertex.label = V(WCE07_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, AM Turnover calulation of network metrics
#igraph
WCE07_TAM.clusterCoef <- transitivity(WCE07_TAMTable, type="global") #cluster coefficient
WCE07_TAM.degreeCent <- centralization.degree(WCE07_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_TAMftn <- as.network.matrix(WCE07_TAMft)
WCE07_TAM.netDensity <- network.density(WCE07_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_TAM.entropy <- entropy(WCE07_TAMft) #entropy

WCE07_TAM.netMx <- cbind(WCE07_TAM.netMx, WCE07_TAM.clusterCoef, WCE07_TAM.degreeCent$centralization,
                         WCE07_TAM.netDensity, WCE07_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_TAM.netMx) <- varnames

#ROUND 7, DM Stoppage**********************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE07_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Stoppage with weighted edges
WCE07_SDMg2 <- data.frame(WCE07_SDM)
WCE07_SDMg2 <- WCE07_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_SDMg2$player1
player2vector <- WCE07_SDMg2$player2
WCE07_SDMg3 <- WCE07_SDMg2
WCE07_SDMg3$p1inp2vec <- is.element(WCE07_SDMg3$player1, player2vector)
WCE07_SDMg3$p2inp1vec <- is.element(WCE07_SDMg3$player2, player1vector)

addPlayer1 <- WCE07_SDMg3[ which(WCE07_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_SDMg3[ which(WCE07_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_SDMg2 <- rbind(WCE07_SDMg2, addPlayers)

#ROUND 7, DM Stoppage graph using weighted edges
WCE07_SDMft <- ftable(WCE07_SDMg2$player1, WCE07_SDMg2$player2)
WCE07_SDMft2 <- as.matrix(WCE07_SDMft)
numRows <- nrow(WCE07_SDMft2)
numCols <- ncol(WCE07_SDMft2)
WCE07_SDMft3 <- WCE07_SDMft2[c(2:numRows) , c(2:numCols)]
WCE07_SDMTable <- graph.adjacency(WCE07_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Stoppage graph=weighted
plot.igraph(WCE07_SDMTable, vertex.label = V(WCE07_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Stoppage calulation of network metrics
#igraph
WCE07_SDM.clusterCoef <- transitivity(WCE07_SDMTable, type="global") #cluster coefficient
WCE07_SDM.degreeCent <- centralization.degree(WCE07_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_SDMftn <- as.network.matrix(WCE07_SDMft)
WCE07_SDM.netDensity <- network.density(WCE07_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_SDM.entropy <- entropy(WCE07_SDMft) #entropy

WCE07_SDM.netMx <- cbind(WCE07_SDM.netMx, WCE07_SDM.clusterCoef, WCE07_SDM.degreeCent$centralization,
                         WCE07_SDM.netDensity, WCE07_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_SDM.netMx) <- varnames

#ROUND 7, DM Turnover**********************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE07_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, DM Turnover with weighted edges
WCE07_TDMg2 <- data.frame(WCE07_TDM)
WCE07_TDMg2 <- WCE07_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_TDMg2$player1
player2vector <- WCE07_TDMg2$player2
WCE07_TDMg3 <- WCE07_TDMg2
WCE07_TDMg3$p1inp2vec <- is.element(WCE07_TDMg3$player1, player2vector)
WCE07_TDMg3$p2inp1vec <- is.element(WCE07_TDMg3$player2, player1vector)

addPlayer1 <- WCE07_TDMg3[ which(WCE07_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_TDMg3[ which(WCE07_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_TDMg2 <- rbind(WCE07_TDMg2, addPlayers)

#ROUND 7, DM Turnover graph using weighted edges
WCE07_TDMft <- ftable(WCE07_TDMg2$player1, WCE07_TDMg2$player2)
WCE07_TDMft2 <- as.matrix(WCE07_TDMft)
numRows <- nrow(WCE07_TDMft2)
numCols <- ncol(WCE07_TDMft2)
WCE07_TDMft3 <- WCE07_TDMft2[c(2:numRows) , c(2:numCols)]
WCE07_TDMTable <- graph.adjacency(WCE07_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 7, DM Turnover graph=weighted
plot.igraph(WCE07_TDMTable, vertex.label = V(WCE07_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, DM Turnover calulation of network metrics
#igraph
WCE07_TDM.clusterCoef <- transitivity(WCE07_TDMTable, type="global") #cluster coefficient
WCE07_TDM.degreeCent <- centralization.degree(WCE07_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_TDMftn <- as.network.matrix(WCE07_TDMft)
WCE07_TDM.netDensity <- network.density(WCE07_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_TDM.entropy <- entropy(WCE07_TDMft) #entropy

WCE07_TDM.netMx <- cbind(WCE07_TDM.netMx, WCE07_TDM.clusterCoef, WCE07_TDM.degreeCent$centralization,
                         WCE07_TDM.netDensity, WCE07_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_TDM.netMx) <- varnames

#ROUND 7, D Stoppage**********************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE07_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Stoppage with weighted edges
WCE07_SDg2 <- data.frame(WCE07_SD)
WCE07_SDg2 <- WCE07_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_SDg2$player1
player2vector <- WCE07_SDg2$player2
WCE07_SDg3 <- WCE07_SDg2
WCE07_SDg3$p1inp2vec <- is.element(WCE07_SDg3$player1, player2vector)
WCE07_SDg3$p2inp1vec <- is.element(WCE07_SDg3$player2, player1vector)

addPlayer1 <- WCE07_SDg3[ which(WCE07_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_SDg3[ which(WCE07_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_SDg2 <- rbind(WCE07_SDg2, addPlayers)

#ROUND 7, D Stoppage graph using weighted edges
WCE07_SDft <- ftable(WCE07_SDg2$player1, WCE07_SDg2$player2)
WCE07_SDft2 <- as.matrix(WCE07_SDft)
numRows <- nrow(WCE07_SDft2)
numCols <- ncol(WCE07_SDft2)
WCE07_SDft3 <- WCE07_SDft2[c(2:numRows) , c(2:numCols)]
WCE07_SDTable <- graph.adjacency(WCE07_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Stoppage graph=weighted
plot.igraph(WCE07_SDTable, vertex.label = V(WCE07_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Stoppage calulation of network metrics
#igraph
WCE07_SD.clusterCoef <- transitivity(WCE07_SDTable, type="global") #cluster coefficient
WCE07_SD.degreeCent <- centralization.degree(WCE07_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_SDftn <- as.network.matrix(WCE07_SDft)
WCE07_SD.netDensity <- network.density(WCE07_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_SD.entropy <- entropy(WCE07_SDft) #entropy

WCE07_SD.netMx <- cbind(WCE07_SD.netMx, WCE07_SD.clusterCoef, WCE07_SD.degreeCent$centralization,
                        WCE07_SD.netDensity, WCE07_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_SD.netMx) <- varnames

#ROUND 7, D Turnover**********************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE07_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, D Turnover with weighted edges
WCE07_TDg2 <- data.frame(WCE07_TD)
WCE07_TDg2 <- WCE07_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_TDg2$player1
player2vector <- WCE07_TDg2$player2
WCE07_TDg3 <- WCE07_TDg2
WCE07_TDg3$p1inp2vec <- is.element(WCE07_TDg3$player1, player2vector)
WCE07_TDg3$p2inp1vec <- is.element(WCE07_TDg3$player2, player1vector)

addPlayer1 <- WCE07_TDg3[ which(WCE07_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_TDg3[ which(WCE07_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_TDg2 <- rbind(WCE07_TDg2, addPlayers)

#ROUND 7, D Turnover graph using weighted edges
WCE07_TDft <- ftable(WCE07_TDg2$player1, WCE07_TDg2$player2)
WCE07_TDft2 <- as.matrix(WCE07_TDft)
numRows <- nrow(WCE07_TDft2)
numCols <- ncol(WCE07_TDft2)
WCE07_TDft3 <- WCE07_TDft2[c(2:numRows) , c(2:numCols)]
WCE07_TDTable <- graph.adjacency(WCE07_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, D Turnover graph=weighted
plot.igraph(WCE07_TDTable, vertex.label = V(WCE07_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, D Turnover calulation of network metrics
#igraph
WCE07_TD.clusterCoef <- transitivity(WCE07_TDTable, type="global") #cluster coefficient
WCE07_TD.degreeCent <- centralization.degree(WCE07_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_TDftn <- as.network.matrix(WCE07_TDft)
WCE07_TD.netDensity <- network.density(WCE07_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_TD.entropy <- entropy(WCE07_TDft) #entropy

WCE07_TD.netMx <- cbind(WCE07_TD.netMx, WCE07_TD.clusterCoef, WCE07_TD.degreeCent$centralization,
                        WCE07_TD.netDensity, WCE07_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_TD.netMx) <- varnames

#ROUND 7, End of Qtr**********************************************************
#NA

round = 7
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE07_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 7, End of Qtr with weighted edges
WCE07_QTg2 <- data.frame(WCE07_QT)
WCE07_QTg2 <- WCE07_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE07_QTg2$player1
player2vector <- WCE07_QTg2$player2
WCE07_QTg3 <- WCE07_QTg2
WCE07_QTg3$p1inp2vec <- is.element(WCE07_QTg3$player1, player2vector)
WCE07_QTg3$p2inp1vec <- is.element(WCE07_QTg3$player2, player1vector)

addPlayer1 <- WCE07_QTg3[ which(WCE07_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE07_QTg3[ which(WCE07_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE07_QTg2 <- rbind(WCE07_QTg2, addPlayers)

#ROUND 7, End of Qtr graph using weighted edges
WCE07_QTft <- ftable(WCE07_QTg2$player1, WCE07_QTg2$player2)
WCE07_QTft2 <- as.matrix(WCE07_QTft)
numRows <- nrow(WCE07_QTft2)
numCols <- ncol(WCE07_QTft2)
WCE07_QTft3 <- WCE07_QTft2[c(2:numRows) , c(2:numCols)]
WCE07_QTTable <- graph.adjacency(WCE07_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 7, End of Qtr graph=weighted
plot.igraph(WCE07_QTTable, vertex.label = V(WCE07_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE07_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 7, End of Qtr calulation of network metrics
#igraph
WCE07_QT.clusterCoef <- transitivity(WCE07_QTTable, type="global") #cluster coefficient
WCE07_QT.degreeCent <- centralization.degree(WCE07_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE07_QTftn <- as.network.matrix(WCE07_QTft)
WCE07_QT.netDensity <- network.density(WCE07_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE07_QT.entropy <- entropy(WCE07_QTft) #entropy

WCE07_QT.netMx <- cbind(WCE07_QT.netMx, WCE07_QT.clusterCoef, WCE07_QT.degreeCent$centralization,
                        WCE07_QT.netDensity, WCE07_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE07_QT.netMx) <- varnames
