#####
#09-27-16- Real data 09
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
ADEL09_Gg2 <- data.frame(ADEL09_G)
ADEL09_Gg2 <- ADEL09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_Gg2$player1
player2vector <- ADEL09_Gg2$player2
ADEL09_Gg3 <- ADEL09_Gg2
ADEL09_Gg3$p1inp2vec <- is.element(ADEL09_Gg3$player1, player2vector)
ADEL09_Gg3$p2inp1vec <- is.element(ADEL09_Gg3$player2, player1vector)

addPlayer1 <- ADEL09_Gg3[ which(ADEL09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_Gg3[ which(ADEL09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_Gg2 <- rbind(ADEL09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
ADEL09_Gft <- ftable(ADEL09_Gg2$player1, ADEL09_Gg2$player2)
ADEL09_Gft2 <- as.matrix(ADEL09_Gft)
numRows <- nrow(ADEL09_Gft2)
numCols <- ncol(ADEL09_Gft2)
ADEL09_Gft3 <- ADEL09_Gft2[c(2:numRows) , c(2:numCols)]
ADEL09_GTable <- graph.adjacency(ADEL09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(ADEL09_GTable, vertex.label = V(ADEL09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
ADEL09_G.clusterCoef <- transitivity(ADEL09_GTable, type="global") #cluster coefficient
ADEL09_G.degreeCent <- centralization.degree(ADEL09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_Gftn <- as.network.matrix(ADEL09_Gft)
ADEL09_G.netDensity <- network.density(ADEL09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_G.entropy <- entropy(ADEL09_Gft) #entropy

ADEL09_G.netMx <- cbind(ADEL09_G.netMx, ADEL09_G.clusterCoef, ADEL09_G.degreeCent$centralization,
                        ADEL09_G.netDensity, ADEL09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
ADEL09_Bg2 <- data.frame(ADEL09_B)
ADEL09_Bg2 <- ADEL09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_Bg2$player1
player2vector <- ADEL09_Bg2$player2
ADEL09_Bg3 <- ADEL09_Bg2
ADEL09_Bg3$p1inp2vec <- is.element(ADEL09_Bg3$player1, player2vector)
ADEL09_Bg3$p2inp1vec <- is.element(ADEL09_Bg3$player2, player1vector)

addPlayer1 <- ADEL09_Bg3[ which(ADEL09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_Bg3[ which(ADEL09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_Bg2 <- rbind(ADEL09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
ADEL09_Bft <- ftable(ADEL09_Bg2$player1, ADEL09_Bg2$player2)
ADEL09_Bft2 <- as.matrix(ADEL09_Bft)
numRows <- nrow(ADEL09_Bft2)
numCols <- ncol(ADEL09_Bft2)
ADEL09_Bft3 <- ADEL09_Bft2[c(2:numRows) , c(2:numCols)]
ADEL09_BTable <- graph.adjacency(ADEL09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(ADEL09_BTable, vertex.label = V(ADEL09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
ADEL09_B.clusterCoef <- transitivity(ADEL09_BTable, type="global") #cluster coefficient
ADEL09_B.degreeCent <- centralization.degree(ADEL09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_Bftn <- as.network.matrix(ADEL09_Bft)
ADEL09_B.netDensity <- network.density(ADEL09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_B.entropy <- entropy(ADEL09_Bft) #entropy

ADEL09_B.netMx <- cbind(ADEL09_B.netMx, ADEL09_B.clusterCoef, ADEL09_B.degreeCent$centralization,
                        ADEL09_B.netDensity, ADEL09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
ADEL09_SFg2 <- data.frame(ADEL09_SF)
ADEL09_SFg2 <- ADEL09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_SFg2$player1
player2vector <- ADEL09_SFg2$player2
ADEL09_SFg3 <- ADEL09_SFg2
ADEL09_SFg3$p1inp2vec <- is.element(ADEL09_SFg3$player1, player2vector)
ADEL09_SFg3$p2inp1vec <- is.element(ADEL09_SFg3$player2, player1vector)

addPlayer1 <- ADEL09_SFg3[ which(ADEL09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_SFg3[ which(ADEL09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_SFg2 <- rbind(ADEL09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
ADEL09_SFft <- ftable(ADEL09_SFg2$player1, ADEL09_SFg2$player2)
ADEL09_SFft2 <- as.matrix(ADEL09_SFft)
numRows <- nrow(ADEL09_SFft2)
numCols <- ncol(ADEL09_SFft2)
ADEL09_SFft3 <- ADEL09_SFft2[c(2:numRows) , c(2:numCols)]
ADEL09_SFTable <- graph.adjacency(ADEL09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(ADEL09_SFTable, vertex.label = V(ADEL09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
ADEL09_SF.clusterCoef <- transitivity(ADEL09_SFTable, type="global") #cluster coefficient
ADEL09_SF.degreeCent <- centralization.degree(ADEL09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_SFftn <- as.network.matrix(ADEL09_SFft)
ADEL09_SF.netDensity <- network.density(ADEL09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_SF.entropy <- entropy(ADEL09_SFft) #entropy

ADEL09_SF.netMx <- cbind(ADEL09_SF.netMx, ADEL09_SF.clusterCoef, ADEL09_SF.degreeCent$centralization,
                         ADEL09_SF.netDensity, ADEL09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
ADEL09_TFg2 <- data.frame(ADEL09_TF)
ADEL09_TFg2 <- ADEL09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_TFg2$player1
player2vector <- ADEL09_TFg2$player2
ADEL09_TFg3 <- ADEL09_TFg2
ADEL09_TFg3$p1inp2vec <- is.element(ADEL09_TFg3$player1, player2vector)
ADEL09_TFg3$p2inp1vec <- is.element(ADEL09_TFg3$player2, player1vector)

addPlayer1 <- ADEL09_TFg3[ which(ADEL09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_TFg3[ which(ADEL09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_TFg2 <- rbind(ADEL09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
ADEL09_TFft <- ftable(ADEL09_TFg2$player1, ADEL09_TFg2$player2)
ADEL09_TFft2 <- as.matrix(ADEL09_TFft)
numRows <- nrow(ADEL09_TFft2)
numCols <- ncol(ADEL09_TFft2)
ADEL09_TFft3 <- ADEL09_TFft2[c(2:numRows) , c(2:numCols)]
ADEL09_TFTable <- graph.adjacency(ADEL09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(ADEL09_TFTable, vertex.label = V(ADEL09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
ADEL09_TF.clusterCoef <- transitivity(ADEL09_TFTable, type="global") #cluster coefficient
ADEL09_TF.degreeCent <- centralization.degree(ADEL09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_TFftn <- as.network.matrix(ADEL09_TFft)
ADEL09_TF.netDensity <- network.density(ADEL09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_TF.entropy <- entropy(ADEL09_TFft) #entropy

ADEL09_TF.netMx <- cbind(ADEL09_TF.netMx, ADEL09_TF.clusterCoef, ADEL09_TF.degreeCent$centralization,
                         ADEL09_TF.netDensity, ADEL09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
ADEL09_SAMg2 <- data.frame(ADEL09_SAM)
ADEL09_SAMg2 <- ADEL09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_SAMg2$player1
player2vector <- ADEL09_SAMg2$player2
ADEL09_SAMg3 <- ADEL09_SAMg2
ADEL09_SAMg3$p1inp2vec <- is.element(ADEL09_SAMg3$player1, player2vector)
ADEL09_SAMg3$p2inp1vec <- is.element(ADEL09_SAMg3$player2, player1vector)

addPlayer1 <- ADEL09_SAMg3[ which(ADEL09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_SAMg3[ which(ADEL09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_SAMg2 <- rbind(ADEL09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
ADEL09_SAMft <- ftable(ADEL09_SAMg2$player1, ADEL09_SAMg2$player2)
ADEL09_SAMft2 <- as.matrix(ADEL09_SAMft)
numRows <- nrow(ADEL09_SAMft2)
numCols <- ncol(ADEL09_SAMft2)
ADEL09_SAMft3 <- ADEL09_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL09_SAMTable <- graph.adjacency(ADEL09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(ADEL09_SAMTable, vertex.label = V(ADEL09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
ADEL09_SAM.clusterCoef <- transitivity(ADEL09_SAMTable, type="global") #cluster coefficient
ADEL09_SAM.degreeCent <- centralization.degree(ADEL09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_SAMftn <- as.network.matrix(ADEL09_SAMft)
ADEL09_SAM.netDensity <- network.density(ADEL09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_SAM.entropy <- entropy(ADEL09_SAMft) #entropy

ADEL09_SAM.netMx <- cbind(ADEL09_SAM.netMx, ADEL09_SAM.clusterCoef, ADEL09_SAM.degreeCent$centralization,
                          ADEL09_SAM.netDensity, ADEL09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
ADEL09_TAMg2 <- data.frame(ADEL09_TAM)
ADEL09_TAMg2 <- ADEL09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_TAMg2$player1
player2vector <- ADEL09_TAMg2$player2
ADEL09_TAMg3 <- ADEL09_TAMg2
ADEL09_TAMg3$p1inp2vec <- is.element(ADEL09_TAMg3$player1, player2vector)
ADEL09_TAMg3$p2inp1vec <- is.element(ADEL09_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL09_TAMg3[ which(ADEL09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL09_TAMg2 <- rbind(ADEL09_TAMg2, addPlayer2)

#ROUND 9, AM Turnover graph using weighted edges
ADEL09_TAMft <- ftable(ADEL09_TAMg2$player1, ADEL09_TAMg2$player2)
ADEL09_TAMft2 <- as.matrix(ADEL09_TAMft)
numRows <- nrow(ADEL09_TAMft2)
numCols <- ncol(ADEL09_TAMft2)
ADEL09_TAMft3 <- ADEL09_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL09_TAMTable <- graph.adjacency(ADEL09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(ADEL09_TAMTable, vertex.label = V(ADEL09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
ADEL09_TAM.clusterCoef <- transitivity(ADEL09_TAMTable, type="global") #cluster coefficient
ADEL09_TAM.degreeCent <- centralization.degree(ADEL09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_TAMftn <- as.network.matrix(ADEL09_TAMft)
ADEL09_TAM.netDensity <- network.density(ADEL09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_TAM.entropy <- entropy(ADEL09_TAMft) #entropy

ADEL09_TAM.netMx <- cbind(ADEL09_TAM.netMx, ADEL09_TAM.clusterCoef, ADEL09_TAM.degreeCent$centralization,
                          ADEL09_TAM.netDensity, ADEL09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
ADEL09_SDMg2 <- data.frame(ADEL09_SDM)
ADEL09_SDMg2 <- ADEL09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_SDMg2$player1
player2vector <- ADEL09_SDMg2$player2
ADEL09_SDMg3 <- ADEL09_SDMg2
ADEL09_SDMg3$p1inp2vec <- is.element(ADEL09_SDMg3$player1, player2vector)
ADEL09_SDMg3$p2inp1vec <- is.element(ADEL09_SDMg3$player2, player1vector)

addPlayer1 <- ADEL09_SDMg3[ which(ADEL09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_SDMg3[ which(ADEL09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_SDMg2 <- rbind(ADEL09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
ADEL09_SDMft <- ftable(ADEL09_SDMg2$player1, ADEL09_SDMg2$player2)
ADEL09_SDMft2 <- as.matrix(ADEL09_SDMft)
numRows <- nrow(ADEL09_SDMft2)
numCols <- ncol(ADEL09_SDMft2)
ADEL09_SDMft3 <- ADEL09_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL09_SDMTable <- graph.adjacency(ADEL09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(ADEL09_SDMTable, vertex.label = V(ADEL09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
ADEL09_SDM.clusterCoef <- transitivity(ADEL09_SDMTable, type="global") #cluster coefficient
ADEL09_SDM.degreeCent <- centralization.degree(ADEL09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_SDMftn <- as.network.matrix(ADEL09_SDMft)
ADEL09_SDM.netDensity <- network.density(ADEL09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_SDM.entropy <- entropy(ADEL09_SDMft) #entropy

ADEL09_SDM.netMx <- cbind(ADEL09_SDM.netMx, ADEL09_SDM.clusterCoef, ADEL09_SDM.degreeCent$centralization,
                          ADEL09_SDM.netDensity, ADEL09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
ADEL09_TDMg2 <- data.frame(ADEL09_TDM)
ADEL09_TDMg2 <- ADEL09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_TDMg2$player1
player2vector <- ADEL09_TDMg2$player2
ADEL09_TDMg3 <- ADEL09_TDMg2
ADEL09_TDMg3$p1inp2vec <- is.element(ADEL09_TDMg3$player1, player2vector)
ADEL09_TDMg3$p2inp1vec <- is.element(ADEL09_TDMg3$player2, player1vector)

addPlayer1 <- ADEL09_TDMg3[ which(ADEL09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_TDMg3[ which(ADEL09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_TDMg2 <- rbind(ADEL09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
ADEL09_TDMft <- ftable(ADEL09_TDMg2$player1, ADEL09_TDMg2$player2)
ADEL09_TDMft2 <- as.matrix(ADEL09_TDMft)
numRows <- nrow(ADEL09_TDMft2)
numCols <- ncol(ADEL09_TDMft2)
ADEL09_TDMft3 <- ADEL09_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL09_TDMTable <- graph.adjacency(ADEL09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(ADEL09_TDMTable, vertex.label = V(ADEL09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
ADEL09_TDM.clusterCoef <- transitivity(ADEL09_TDMTable, type="global") #cluster coefficient
ADEL09_TDM.degreeCent <- centralization.degree(ADEL09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_TDMftn <- as.network.matrix(ADEL09_TDMft)
ADEL09_TDM.netDensity <- network.density(ADEL09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_TDM.entropy <- entropy(ADEL09_TDMft) #entropy

ADEL09_TDM.netMx <- cbind(ADEL09_TDM.netMx, ADEL09_TDM.clusterCoef, ADEL09_TDM.degreeCent$centralization,
                          ADEL09_TDM.netDensity, ADEL09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
ADEL09_SDg2 <- data.frame(ADEL09_SD)
ADEL09_SDg2 <- ADEL09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_SDg2$player1
player2vector <- ADEL09_SDg2$player2
ADEL09_SDg3 <- ADEL09_SDg2
ADEL09_SDg3$p1inp2vec <- is.element(ADEL09_SDg3$player1, player2vector)
ADEL09_SDg3$p2inp1vec <- is.element(ADEL09_SDg3$player2, player1vector)

addPlayer1 <- ADEL09_SDg3[ which(ADEL09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_SDg3[ which(ADEL09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_SDg2 <- rbind(ADEL09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
ADEL09_SDft <- ftable(ADEL09_SDg2$player1, ADEL09_SDg2$player2)
ADEL09_SDft2 <- as.matrix(ADEL09_SDft)
numRows <- nrow(ADEL09_SDft2)
numCols <- ncol(ADEL09_SDft2)
ADEL09_SDft3 <- ADEL09_SDft2[c(2:numRows) , c(2:numCols)]
ADEL09_SDTable <- graph.adjacency(ADEL09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(ADEL09_SDTable, vertex.label = V(ADEL09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
ADEL09_SD.clusterCoef <- transitivity(ADEL09_SDTable, type="global") #cluster coefficient
ADEL09_SD.degreeCent <- centralization.degree(ADEL09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_SDftn <- as.network.matrix(ADEL09_SDft)
ADEL09_SD.netDensity <- network.density(ADEL09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_SD.entropy <- entropy(ADEL09_SDft) #entropy

ADEL09_SD.netMx <- cbind(ADEL09_SD.netMx, ADEL09_SD.clusterCoef, ADEL09_SD.degreeCent$centralization,
                         ADEL09_SD.netDensity, ADEL09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
ADEL09_TDg2 <- data.frame(ADEL09_TD)
ADEL09_TDg2 <- ADEL09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_TDg2$player1
player2vector <- ADEL09_TDg2$player2
ADEL09_TDg3 <- ADEL09_TDg2
ADEL09_TDg3$p1inp2vec <- is.element(ADEL09_TDg3$player1, player2vector)
ADEL09_TDg3$p2inp1vec <- is.element(ADEL09_TDg3$player2, player1vector)

addPlayer1 <- ADEL09_TDg3[ which(ADEL09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_TDg3[ which(ADEL09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_TDg2 <- rbind(ADEL09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
ADEL09_TDft <- ftable(ADEL09_TDg2$player1, ADEL09_TDg2$player2)
ADEL09_TDft2 <- as.matrix(ADEL09_TDft)
numRows <- nrow(ADEL09_TDft2)
numCols <- ncol(ADEL09_TDft2)
ADEL09_TDft3 <- ADEL09_TDft2[c(2:numRows) , c(2:numCols)]
ADEL09_TDTable <- graph.adjacency(ADEL09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(ADEL09_TDTable, vertex.label = V(ADEL09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
ADEL09_TD.clusterCoef <- transitivity(ADEL09_TDTable, type="global") #cluster coefficient
ADEL09_TD.degreeCent <- centralization.degree(ADEL09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_TDftn <- as.network.matrix(ADEL09_TDft)
ADEL09_TD.netDensity <- network.density(ADEL09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_TD.entropy <- entropy(ADEL09_TDft) #entropy

ADEL09_TD.netMx <- cbind(ADEL09_TD.netMx, ADEL09_TD.clusterCoef, ADEL09_TD.degreeCent$centralization,
                         ADEL09_TD.netDensity, ADEL09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
ADEL09_QTg2 <- data.frame(ADEL09_QT)
ADEL09_QTg2 <- ADEL09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL09_QTg2$player1
player2vector <- ADEL09_QTg2$player2
ADEL09_QTg3 <- ADEL09_QTg2
ADEL09_QTg3$p1inp2vec <- is.element(ADEL09_QTg3$player1, player2vector)
ADEL09_QTg3$p2inp1vec <- is.element(ADEL09_QTg3$player2, player1vector)

addPlayer1 <- ADEL09_QTg3[ which(ADEL09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL09_QTg3[ which(ADEL09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL09_QTg2 <- rbind(ADEL09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
ADEL09_QTft <- ftable(ADEL09_QTg2$player1, ADEL09_QTg2$player2)
ADEL09_QTft2 <- as.matrix(ADEL09_QTft)
numRows <- nrow(ADEL09_QTft2)
numCols <- ncol(ADEL09_QTft2)
ADEL09_QTft3 <- ADEL09_QTft2[c(2:numRows) , c(2:numCols)]
ADEL09_QTTable <- graph.adjacency(ADEL09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(ADEL09_QTTable, vertex.label = V(ADEL09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
ADEL09_QT.clusterCoef <- transitivity(ADEL09_QTTable, type="global") #cluster coefficient
ADEL09_QT.degreeCent <- centralization.degree(ADEL09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL09_QTftn <- as.network.matrix(ADEL09_QTft)
ADEL09_QT.netDensity <- network.density(ADEL09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL09_QT.entropy <- entropy(ADEL09_QTft) #entropy

ADEL09_QT.netMx <- cbind(ADEL09_QT.netMx, ADEL09_QT.clusterCoef, ADEL09_QT.degreeCent$centralization,
                         ADEL09_QT.netDensity, ADEL09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL09_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "BL"
KIoutcome = "Goal_F"
BL09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
BL09_Gg2 <- data.frame(BL09_G)
BL09_Gg2 <- BL09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_Gg2$player1
player2vector <- BL09_Gg2$player2
BL09_Gg3 <- BL09_Gg2
BL09_Gg3$p1inp2vec <- is.element(BL09_Gg3$player1, player2vector)
BL09_Gg3$p2inp1vec <- is.element(BL09_Gg3$player2, player1vector)

addPlayer1 <- BL09_Gg3[ which(BL09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_Gg3[ which(BL09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_Gg2 <- rbind(BL09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
BL09_Gft <- ftable(BL09_Gg2$player1, BL09_Gg2$player2)
BL09_Gft2 <- as.matrix(BL09_Gft)
numRows <- nrow(BL09_Gft2)
numCols <- ncol(BL09_Gft2)
BL09_Gft3 <- BL09_Gft2[c(2:numRows) , c(2:numCols)]
BL09_GTable <- graph.adjacency(BL09_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(BL09_GTable, vertex.label = V(BL09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
BL09_G.clusterCoef <- transitivity(BL09_GTable, type="global") #cluster coefficient
BL09_G.degreeCent <- centralization.degree(BL09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_Gftn <- as.network.matrix(BL09_Gft)
BL09_G.netDensity <- network.density(BL09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_G.entropy <- entropy(BL09_Gft) #entropy

BL09_G.netMx <- cbind(BL09_G.netMx, BL09_G.clusterCoef, BL09_G.degreeCent$centralization,
                      BL09_G.netDensity, BL09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "BL"
KIoutcome = "Behind_F"
BL09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
BL09_Bg2 <- data.frame(BL09_B)
BL09_Bg2 <- BL09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_Bg2$player1
player2vector <- BL09_Bg2$player2
BL09_Bg3 <- BL09_Bg2
BL09_Bg3$p1inp2vec <- is.element(BL09_Bg3$player1, player2vector)
BL09_Bg3$p2inp1vec <- is.element(BL09_Bg3$player2, player1vector)

addPlayer1 <- BL09_Bg3[ which(BL09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_Bg3[ which(BL09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_Bg2 <- rbind(BL09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
BL09_Bft <- ftable(BL09_Bg2$player1, BL09_Bg2$player2)
BL09_Bft2 <- as.matrix(BL09_Bft)
numRows <- nrow(BL09_Bft2)
numCols <- ncol(BL09_Bft2)
BL09_Bft3 <- BL09_Bft2[c(2:numRows) , c(2:numCols)]
BL09_BTable <- graph.adjacency(BL09_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(BL09_BTable, vertex.label = V(BL09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
BL09_B.clusterCoef <- transitivity(BL09_BTable, type="global") #cluster coefficient
BL09_B.degreeCent <- centralization.degree(BL09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_Bftn <- as.network.matrix(BL09_Bft)
BL09_B.netDensity <- network.density(BL09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_B.entropy <- entropy(BL09_Bft) #entropy

BL09_B.netMx <- cbind(BL09_B.netMx, BL09_B.clusterCoef, BL09_B.degreeCent$centralization,
                      BL09_B.netDensity, BL09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "BL"
KIoutcome = "Stoppage_F"
BL09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
BL09_SFg2 <- data.frame(BL09_SF)
BL09_SFg2 <- BL09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_SFg2$player1
player2vector <- BL09_SFg2$player2
BL09_SFg3 <- BL09_SFg2
BL09_SFg3$p1inp2vec <- is.element(BL09_SFg3$player1, player2vector)
BL09_SFg3$p2inp1vec <- is.element(BL09_SFg3$player2, player1vector)

addPlayer1 <- BL09_SFg3[ which(BL09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_SFg3[ which(BL09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_SFg2 <- rbind(BL09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
BL09_SFft <- ftable(BL09_SFg2$player1, BL09_SFg2$player2)
BL09_SFft2 <- as.matrix(BL09_SFft)
numRows <- nrow(BL09_SFft2)
numCols <- ncol(BL09_SFft2)
BL09_SFft3 <- BL09_SFft2[c(2:numRows) , c(2:numCols)]
BL09_SFTable <- graph.adjacency(BL09_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(BL09_SFTable, vertex.label = V(BL09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
BL09_SF.clusterCoef <- transitivity(BL09_SFTable, type="global") #cluster coefficient
BL09_SF.degreeCent <- centralization.degree(BL09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_SFftn <- as.network.matrix(BL09_SFft)
BL09_SF.netDensity <- network.density(BL09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_SF.entropy <- entropy(BL09_SFft) #entropy

BL09_SF.netMx <- cbind(BL09_SF.netMx, BL09_SF.clusterCoef, BL09_SF.degreeCent$centralization,
                       BL09_SF.netDensity, BL09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "BL"
KIoutcome = "Turnover_F"
BL09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
BL09_TFg2 <- data.frame(BL09_TF)
BL09_TFg2 <- BL09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_TFg2$player1
player2vector <- BL09_TFg2$player2
BL09_TFg3 <- BL09_TFg2
BL09_TFg3$p1inp2vec <- is.element(BL09_TFg3$player1, player2vector)
BL09_TFg3$p2inp1vec <- is.element(BL09_TFg3$player2, player1vector)

addPlayer1 <- BL09_TFg3[ which(BL09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_TFg3[ which(BL09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_TFg2 <- rbind(BL09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
BL09_TFft <- ftable(BL09_TFg2$player1, BL09_TFg2$player2)
BL09_TFft2 <- as.matrix(BL09_TFft)
numRows <- nrow(BL09_TFft2)
numCols <- ncol(BL09_TFft2)
BL09_TFft3 <- BL09_TFft2[c(2:numRows) , c(2:numCols)]
BL09_TFTable <- graph.adjacency(BL09_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(BL09_TFTable, vertex.label = V(BL09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
BL09_TF.clusterCoef <- transitivity(BL09_TFTable, type="global") #cluster coefficient
BL09_TF.degreeCent <- centralization.degree(BL09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_TFftn <- as.network.matrix(BL09_TFft)
BL09_TF.netDensity <- network.density(BL09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_TF.entropy <- entropy(BL09_TFft) #entropy

BL09_TF.netMx <- cbind(BL09_TF.netMx, BL09_TF.clusterCoef, BL09_TF.degreeCent$centralization,
                       BL09_TF.netDensity, BL09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
BL09_SAMg2 <- data.frame(BL09_SAM)
BL09_SAMg2 <- BL09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_SAMg2$player1
player2vector <- BL09_SAMg2$player2
BL09_SAMg3 <- BL09_SAMg2
BL09_SAMg3$p1inp2vec <- is.element(BL09_SAMg3$player1, player2vector)
BL09_SAMg3$p2inp1vec <- is.element(BL09_SAMg3$player2, player1vector)

addPlayer1 <- BL09_SAMg3[ which(BL09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_SAMg3[ which(BL09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_SAMg2 <- rbind(BL09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
BL09_SAMft <- ftable(BL09_SAMg2$player1, BL09_SAMg2$player2)
BL09_SAMft2 <- as.matrix(BL09_SAMft)
numRows <- nrow(BL09_SAMft2)
numCols <- ncol(BL09_SAMft2)
BL09_SAMft3 <- BL09_SAMft2[c(2:numRows) , c(2:numCols)]
BL09_SAMTable <- graph.adjacency(BL09_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(BL09_SAMTable, vertex.label = V(BL09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
BL09_SAM.clusterCoef <- transitivity(BL09_SAMTable, type="global") #cluster coefficient
BL09_SAM.degreeCent <- centralization.degree(BL09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_SAMftn <- as.network.matrix(BL09_SAMft)
BL09_SAM.netDensity <- network.density(BL09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_SAM.entropy <- entropy(BL09_SAMft) #entropy

BL09_SAM.netMx <- cbind(BL09_SAM.netMx, BL09_SAM.clusterCoef, BL09_SAM.degreeCent$centralization,
                        BL09_SAM.netDensity, BL09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "BL"
KIoutcome = "Turnover_AM"
BL09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
BL09_TAMg2 <- data.frame(BL09_TAM)
BL09_TAMg2 <- BL09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_TAMg2$player1
player2vector <- BL09_TAMg2$player2
BL09_TAMg3 <- BL09_TAMg2
BL09_TAMg3$p1inp2vec <- is.element(BL09_TAMg3$player1, player2vector)
BL09_TAMg3$p2inp1vec <- is.element(BL09_TAMg3$player2, player1vector)

addPlayer1 <- BL09_TAMg3[ which(BL09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL09_TAMg3[ which(BL09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_TAMg2 <- rbind(BL09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
BL09_TAMft <- ftable(BL09_TAMg2$player1, BL09_TAMg2$player2)
BL09_TAMft2 <- as.matrix(BL09_TAMft)
numRows <- nrow(BL09_TAMft2)
numCols <- ncol(BL09_TAMft2)
BL09_TAMft3 <- BL09_TAMft2[c(2:numRows) , c(2:numCols)]
BL09_TAMTable <- graph.adjacency(BL09_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(BL09_TAMTable, vertex.label = V(BL09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
BL09_TAM.clusterCoef <- transitivity(BL09_TAMTable, type="global") #cluster coefficient
BL09_TAM.degreeCent <- centralization.degree(BL09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_TAMftn <- as.network.matrix(BL09_TAMft)
BL09_TAM.netDensity <- network.density(BL09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_TAM.entropy <- entropy(BL09_TAMft) #entropy

BL09_TAM.netMx <- cbind(BL09_TAM.netMx, BL09_TAM.clusterCoef, BL09_TAM.degreeCent$centralization,
                        BL09_TAM.netDensity, BL09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
BL09_SDMg2 <- data.frame(BL09_SDM)
BL09_SDMg2 <- BL09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_SDMg2$player1
player2vector <- BL09_SDMg2$player2
BL09_SDMg3 <- BL09_SDMg2
BL09_SDMg3$p1inp2vec <- is.element(BL09_SDMg3$player1, player2vector)
BL09_SDMg3$p2inp1vec <- is.element(BL09_SDMg3$player2, player1vector)

addPlayer1 <- BL09_SDMg3[ which(BL09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_SDMg3[ which(BL09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_SDMg2 <- rbind(BL09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
BL09_SDMft <- ftable(BL09_SDMg2$player1, BL09_SDMg2$player2)
BL09_SDMft2 <- as.matrix(BL09_SDMft)
numRows <- nrow(BL09_SDMft2)
numCols <- ncol(BL09_SDMft2)
BL09_SDMft3 <- BL09_SDMft2[c(2:numRows) , c(2:numCols)]
BL09_SDMTable <- graph.adjacency(BL09_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(BL09_SDMTable, vertex.label = V(BL09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
BL09_SDM.clusterCoef <- transitivity(BL09_SDMTable, type="global") #cluster coefficient
BL09_SDM.degreeCent <- centralization.degree(BL09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_SDMftn <- as.network.matrix(BL09_SDMft)
BL09_SDM.netDensity <- network.density(BL09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_SDM.entropy <- entropy(BL09_SDMft) #entropy

BL09_SDM.netMx <- cbind(BL09_SDM.netMx, BL09_SDM.clusterCoef, BL09_SDM.degreeCent$centralization,
                        BL09_SDM.netDensity, BL09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "BL"
KIoutcome = "Turnover_DM"
BL09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
BL09_TDMg2 <- data.frame(BL09_TDM)
BL09_TDMg2 <- BL09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_TDMg2$player1
player2vector <- BL09_TDMg2$player2
BL09_TDMg3 <- BL09_TDMg2
BL09_TDMg3$p1inp2vec <- is.element(BL09_TDMg3$player1, player2vector)
BL09_TDMg3$p2inp1vec <- is.element(BL09_TDMg3$player2, player1vector)

addPlayer1 <- BL09_TDMg3[ which(BL09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_TDMg3[ which(BL09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_TDMg2 <- rbind(BL09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
BL09_TDMft <- ftable(BL09_TDMg2$player1, BL09_TDMg2$player2)
BL09_TDMft2 <- as.matrix(BL09_TDMft)
numRows <- nrow(BL09_TDMft2)
numCols <- ncol(BL09_TDMft2)
BL09_TDMft3 <- BL09_TDMft2[c(2:numRows) , c(2:numCols)]
BL09_TDMTable <- graph.adjacency(BL09_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(BL09_TDMTable, vertex.label = V(BL09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
BL09_TDM.clusterCoef <- transitivity(BL09_TDMTable, type="global") #cluster coefficient
BL09_TDM.degreeCent <- centralization.degree(BL09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_TDMftn <- as.network.matrix(BL09_TDMft)
BL09_TDM.netDensity <- network.density(BL09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_TDM.entropy <- entropy(BL09_TDMft) #entropy

BL09_TDM.netMx <- cbind(BL09_TDM.netMx, BL09_TDM.clusterCoef, BL09_TDM.degreeCent$centralization,
                        BL09_TDM.netDensity, BL09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "BL"
KIoutcome = "Stoppage_D"
BL09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
BL09_SDg2 <- data.frame(BL09_SD)
BL09_SDg2 <- BL09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_SDg2$player1
player2vector <- BL09_SDg2$player2
BL09_SDg3 <- BL09_SDg2
BL09_SDg3$p1inp2vec <- is.element(BL09_SDg3$player1, player2vector)
BL09_SDg3$p2inp1vec <- is.element(BL09_SDg3$player2, player1vector)

addPlayer1 <- BL09_SDg3[ which(BL09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_SDg3[ which(BL09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_SDg2 <- rbind(BL09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
BL09_SDft <- ftable(BL09_SDg2$player1, BL09_SDg2$player2)
BL09_SDft2 <- as.matrix(BL09_SDft)
numRows <- nrow(BL09_SDft2)
numCols <- ncol(BL09_SDft2)
BL09_SDft3 <- BL09_SDft2[c(2:numRows) , c(2:numCols)]
BL09_SDTable <- graph.adjacency(BL09_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(BL09_SDTable, vertex.label = V(BL09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
BL09_SD.clusterCoef <- transitivity(BL09_SDTable, type="global") #cluster coefficient
BL09_SD.degreeCent <- centralization.degree(BL09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_SDftn <- as.network.matrix(BL09_SDft)
BL09_SD.netDensity <- network.density(BL09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_SD.entropy <- entropy(BL09_SDft) #entropy

BL09_SD.netMx <- cbind(BL09_SD.netMx, BL09_SD.clusterCoef, BL09_SD.degreeCent$centralization,
                       BL09_SD.netDensity, BL09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "BL"
KIoutcome = "Turnover_D"
BL09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
BL09_TDg2 <- data.frame(BL09_TD)
BL09_TDg2 <- BL09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_TDg2$player1
player2vector <- BL09_TDg2$player2
BL09_TDg3 <- BL09_TDg2
BL09_TDg3$p1inp2vec <- is.element(BL09_TDg3$player1, player2vector)
BL09_TDg3$p2inp1vec <- is.element(BL09_TDg3$player2, player1vector)

addPlayer1 <- BL09_TDg3[ which(BL09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_TDg3[ which(BL09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_TDg2 <- rbind(BL09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
BL09_TDft <- ftable(BL09_TDg2$player1, BL09_TDg2$player2)
BL09_TDft2 <- as.matrix(BL09_TDft)
numRows <- nrow(BL09_TDft2)
numCols <- ncol(BL09_TDft2)
BL09_TDft3 <- BL09_TDft2[c(2:numRows) , c(2:numCols)]
BL09_TDTable <- graph.adjacency(BL09_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(BL09_TDTable, vertex.label = V(BL09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
BL09_TD.clusterCoef <- transitivity(BL09_TDTable, type="global") #cluster coefficient
BL09_TD.degreeCent <- centralization.degree(BL09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_TDftn <- as.network.matrix(BL09_TDft)
BL09_TD.netDensity <- network.density(BL09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_TD.entropy <- entropy(BL09_TDft) #entropy

BL09_TD.netMx <- cbind(BL09_TD.netMx, BL09_TD.clusterCoef, BL09_TD.degreeCent$centralization,
                       BL09_TD.netDensity, BL09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
BL09_QTg2 <- data.frame(BL09_QT)
BL09_QTg2 <- BL09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL09_QTg2$player1
player2vector <- BL09_QTg2$player2
BL09_QTg3 <- BL09_QTg2
BL09_QTg3$p1inp2vec <- is.element(BL09_QTg3$player1, player2vector)
BL09_QTg3$p2inp1vec <- is.element(BL09_QTg3$player2, player1vector)

addPlayer1 <- BL09_QTg3[ which(BL09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL09_QTg3[ which(BL09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL09_QTg2 <- rbind(BL09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
BL09_QTft <- ftable(BL09_QTg2$player1, BL09_QTg2$player2)
BL09_QTft2 <- as.matrix(BL09_QTft)
numRows <- nrow(BL09_QTft2)
numCols <- ncol(BL09_QTft2)
BL09_QTft3 <- BL09_QTft2[c(2:numRows) , c(2:numCols)]
BL09_QTTable <- graph.adjacency(BL09_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(BL09_QTTable, vertex.label = V(BL09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
BL09_QT.clusterCoef <- transitivity(BL09_QTTable, type="global") #cluster coefficient
BL09_QT.degreeCent <- centralization.degree(BL09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL09_QTftn <- as.network.matrix(BL09_QTft)
BL09_QT.netDensity <- network.density(BL09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL09_QT.entropy <- entropy(BL09_QTft) #entropy

BL09_QT.netMx <- cbind(BL09_QT.netMx, BL09_QT.clusterCoef, BL09_QT.degreeCent$centralization,
                       BL09_QT.netDensity, BL09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL09_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "CARL"
KIoutcome = "Goal_F"
CARL09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
CARL09_Gg2 <- data.frame(CARL09_G)
CARL09_Gg2 <- CARL09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_Gg2$player1
player2vector <- CARL09_Gg2$player2
CARL09_Gg3 <- CARL09_Gg2
CARL09_Gg3$p1inp2vec <- is.element(CARL09_Gg3$player1, player2vector)
CARL09_Gg3$p2inp1vec <- is.element(CARL09_Gg3$player2, player1vector)

addPlayer1 <- CARL09_Gg3[ which(CARL09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_Gg3[ which(CARL09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_Gg2 <- rbind(CARL09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
CARL09_Gft <- ftable(CARL09_Gg2$player1, CARL09_Gg2$player2)
CARL09_Gft2 <- as.matrix(CARL09_Gft)
numRows <- nrow(CARL09_Gft2)
numCols <- ncol(CARL09_Gft2)
CARL09_Gft3 <- CARL09_Gft2[c(2:numRows) , c(2:numCols)]
CARL09_GTable <- graph.adjacency(CARL09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(CARL09_GTable, vertex.label = V(CARL09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
CARL09_G.clusterCoef <- transitivity(CARL09_GTable, type="global") #cluster coefficient
CARL09_G.degreeCent <- centralization.degree(CARL09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_Gftn <- as.network.matrix(CARL09_Gft)
CARL09_G.netDensity <- network.density(CARL09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_G.entropy <- entropy(CARL09_Gft) #entropy

CARL09_G.netMx <- cbind(CARL09_G.netMx, CARL09_G.clusterCoef, CARL09_G.degreeCent$centralization,
                        CARL09_G.netDensity, CARL09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "Behind_F"
CARL09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
CARL09_Bg2 <- data.frame(CARL09_B)
CARL09_Bg2 <- CARL09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_Bg2$player1
player2vector <- CARL09_Bg2$player2
CARL09_Bg3 <- CARL09_Bg2
CARL09_Bg3$p1inp2vec <- is.element(CARL09_Bg3$player1, player2vector)
CARL09_Bg3$p2inp1vec <- is.element(CARL09_Bg3$player2, player1vector)

addPlayer1 <- CARL09_Bg3[ which(CARL09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_Bg3[ which(CARL09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_Bg2 <- rbind(CARL09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
CARL09_Bft <- ftable(CARL09_Bg2$player1, CARL09_Bg2$player2)
CARL09_Bft2 <- as.matrix(CARL09_Bft)
numRows <- nrow(CARL09_Bft2)
numCols <- ncol(CARL09_Bft2)
CARL09_Bft3 <- CARL09_Bft2[c(2:numRows) , c(2:numCols)]
CARL09_BTable <- graph.adjacency(CARL09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(CARL09_BTable, vertex.label = V(CARL09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
CARL09_B.clusterCoef <- transitivity(CARL09_BTable, type="global") #cluster coefficient
CARL09_B.degreeCent <- centralization.degree(CARL09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_Bftn <- as.network.matrix(CARL09_Bft)
CARL09_B.netDensity <- network.density(CARL09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_B.entropy <- entropy(CARL09_Bft) #entropy

CARL09_B.netMx <- cbind(CARL09_B.netMx, CARL09_B.clusterCoef, CARL09_B.degreeCent$centralization,
                        CARL09_B.netDensity, CARL09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
CARL09_SFg2 <- data.frame(CARL09_SF)
CARL09_SFg2 <- CARL09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_SFg2$player1
player2vector <- CARL09_SFg2$player2
CARL09_SFg3 <- CARL09_SFg2
CARL09_SFg3$p1inp2vec <- is.element(CARL09_SFg3$player1, player2vector)
CARL09_SFg3$p2inp1vec <- is.element(CARL09_SFg3$player2, player1vector)

addPlayer1 <- CARL09_SFg3[ which(CARL09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_SFg3[ which(CARL09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_SFg2 <- rbind(CARL09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
CARL09_SFft <- ftable(CARL09_SFg2$player1, CARL09_SFg2$player2)
CARL09_SFft2 <- as.matrix(CARL09_SFft)
numRows <- nrow(CARL09_SFft2)
numCols <- ncol(CARL09_SFft2)
CARL09_SFft3 <- CARL09_SFft2[c(2:numRows) , c(2:numCols)]
CARL09_SFTable <- graph.adjacency(CARL09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(CARL09_SFTable, vertex.label = V(CARL09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
CARL09_SF.clusterCoef <- transitivity(CARL09_SFTable, type="global") #cluster coefficient
CARL09_SF.degreeCent <- centralization.degree(CARL09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_SFftn <- as.network.matrix(CARL09_SFft)
CARL09_SF.netDensity <- network.density(CARL09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_SF.entropy <- entropy(CARL09_SFft) #entropy

CARL09_SF.netMx <- cbind(CARL09_SF.netMx, CARL09_SF.clusterCoef, CARL09_SF.degreeCent$centralization,
                         CARL09_SF.netDensity, CARL09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
CARL09_TFg2 <- data.frame(CARL09_TF)
CARL09_TFg2 <- CARL09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_TFg2$player1
player2vector <- CARL09_TFg2$player2
CARL09_TFg3 <- CARL09_TFg2
CARL09_TFg3$p1inp2vec <- is.element(CARL09_TFg3$player1, player2vector)
CARL09_TFg3$p2inp1vec <- is.element(CARL09_TFg3$player2, player1vector)

addPlayer1 <- CARL09_TFg3[ which(CARL09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_TFg3[ which(CARL09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_TFg2 <- rbind(CARL09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
CARL09_TFft <- ftable(CARL09_TFg2$player1, CARL09_TFg2$player2)
CARL09_TFft2 <- as.matrix(CARL09_TFft)
numRows <- nrow(CARL09_TFft2)
numCols <- ncol(CARL09_TFft2)
CARL09_TFft3 <- CARL09_TFft2[c(2:numRows) , c(2:numCols)]
CARL09_TFTable <- graph.adjacency(CARL09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(CARL09_TFTable, vertex.label = V(CARL09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
CARL09_TF.clusterCoef <- transitivity(CARL09_TFTable, type="global") #cluster coefficient
CARL09_TF.degreeCent <- centralization.degree(CARL09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_TFftn <- as.network.matrix(CARL09_TFft)
CARL09_TF.netDensity <- network.density(CARL09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_TF.entropy <- entropy(CARL09_TFft) #entropy

CARL09_TF.netMx <- cbind(CARL09_TF.netMx, CARL09_TF.clusterCoef, CARL09_TF.degreeCent$centralization,
                         CARL09_TF.netDensity, CARL09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
CARL09_SAMg2 <- data.frame(CARL09_SAM)
CARL09_SAMg2 <- CARL09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_SAMg2$player1
player2vector <- CARL09_SAMg2$player2
CARL09_SAMg3 <- CARL09_SAMg2
CARL09_SAMg3$p1inp2vec <- is.element(CARL09_SAMg3$player1, player2vector)
CARL09_SAMg3$p2inp1vec <- is.element(CARL09_SAMg3$player2, player1vector)

addPlayer1 <- CARL09_SAMg3[ which(CARL09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_SAMg3[ which(CARL09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_SAMg2 <- rbind(CARL09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
CARL09_SAMft <- ftable(CARL09_SAMg2$player1, CARL09_SAMg2$player2)
CARL09_SAMft2 <- as.matrix(CARL09_SAMft)
numRows <- nrow(CARL09_SAMft2)
numCols <- ncol(CARL09_SAMft2)
CARL09_SAMft3 <- CARL09_SAMft2[c(2:numRows) , c(2:numCols)]
CARL09_SAMTable <- graph.adjacency(CARL09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(CARL09_SAMTable, vertex.label = V(CARL09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
CARL09_SAM.clusterCoef <- transitivity(CARL09_SAMTable, type="global") #cluster coefficient
CARL09_SAM.degreeCent <- centralization.degree(CARL09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_SAMftn <- as.network.matrix(CARL09_SAMft)
CARL09_SAM.netDensity <- network.density(CARL09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_SAM.entropy <- entropy(CARL09_SAMft) #entropy

CARL09_SAM.netMx <- cbind(CARL09_SAM.netMx, CARL09_SAM.clusterCoef, CARL09_SAM.degreeCent$centralization,
                          CARL09_SAM.netDensity, CARL09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
CARL09_TAMg2 <- data.frame(CARL09_TAM)
CARL09_TAMg2 <- CARL09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_TAMg2$player1
player2vector <- CARL09_TAMg2$player2
CARL09_TAMg3 <- CARL09_TAMg2
CARL09_TAMg3$p1inp2vec <- is.element(CARL09_TAMg3$player1, player2vector)
CARL09_TAMg3$p2inp1vec <- is.element(CARL09_TAMg3$player2, player1vector)

addPlayer1 <- CARL09_TAMg3[ which(CARL09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL09_TAMg3[ which(CARL09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_TAMg2 <- rbind(CARL09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
CARL09_TAMft <- ftable(CARL09_TAMg2$player1, CARL09_TAMg2$player2)
CARL09_TAMft2 <- as.matrix(CARL09_TAMft)
numRows <- nrow(CARL09_TAMft2)
numCols <- ncol(CARL09_TAMft2)
CARL09_TAMft3 <- CARL09_TAMft2[c(2:numRows) , c(2:numCols)]
CARL09_TAMTable <- graph.adjacency(CARL09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(CARL09_TAMTable, vertex.label = V(CARL09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
CARL09_TAM.clusterCoef <- transitivity(CARL09_TAMTable, type="global") #cluster coefficient
CARL09_TAM.degreeCent <- centralization.degree(CARL09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_TAMftn <- as.network.matrix(CARL09_TAMft)
CARL09_TAM.netDensity <- network.density(CARL09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_TAM.entropy <- entropy(CARL09_TAMft) #entropy

CARL09_TAM.netMx <- cbind(CARL09_TAM.netMx, CARL09_TAM.clusterCoef, CARL09_TAM.degreeCent$centralization,
                          CARL09_TAM.netDensity, CARL09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
CARL09_SDMg2 <- data.frame(CARL09_SDM)
CARL09_SDMg2 <- CARL09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_SDMg2$player1
player2vector <- CARL09_SDMg2$player2
CARL09_SDMg3 <- CARL09_SDMg2
CARL09_SDMg3$p1inp2vec <- is.element(CARL09_SDMg3$player1, player2vector)
CARL09_SDMg3$p2inp1vec <- is.element(CARL09_SDMg3$player2, player1vector)

addPlayer1 <- CARL09_SDMg3[ which(CARL09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_SDMg3[ which(CARL09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_SDMg2 <- rbind(CARL09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
CARL09_SDMft <- ftable(CARL09_SDMg2$player1, CARL09_SDMg2$player2)
CARL09_SDMft2 <- as.matrix(CARL09_SDMft)
numRows <- nrow(CARL09_SDMft2)
numCols <- ncol(CARL09_SDMft2)
CARL09_SDMft3 <- CARL09_SDMft2[c(2:numRows) , c(2:numCols)]
CARL09_SDMTable <- graph.adjacency(CARL09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(CARL09_SDMTable, vertex.label = V(CARL09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
CARL09_SDM.clusterCoef <- transitivity(CARL09_SDMTable, type="global") #cluster coefficient
CARL09_SDM.degreeCent <- centralization.degree(CARL09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_SDMftn <- as.network.matrix(CARL09_SDMft)
CARL09_SDM.netDensity <- network.density(CARL09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_SDM.entropy <- entropy(CARL09_SDMft) #entropy

CARL09_SDM.netMx <- cbind(CARL09_SDM.netMx, CARL09_SDM.clusterCoef, CARL09_SDM.degreeCent$centralization,
                          CARL09_SDM.netDensity, CARL09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
CARL09_TDMg2 <- data.frame(CARL09_TDM)
CARL09_TDMg2 <- CARL09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_TDMg2$player1
player2vector <- CARL09_TDMg2$player2
CARL09_TDMg3 <- CARL09_TDMg2
CARL09_TDMg3$p1inp2vec <- is.element(CARL09_TDMg3$player1, player2vector)
CARL09_TDMg3$p2inp1vec <- is.element(CARL09_TDMg3$player2, player1vector)

addPlayer1 <- CARL09_TDMg3[ which(CARL09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_TDMg3[ which(CARL09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_TDMg2 <- rbind(CARL09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
CARL09_TDMft <- ftable(CARL09_TDMg2$player1, CARL09_TDMg2$player2)
CARL09_TDMft2 <- as.matrix(CARL09_TDMft)
numRows <- nrow(CARL09_TDMft2)
numCols <- ncol(CARL09_TDMft2)
CARL09_TDMft3 <- CARL09_TDMft2[c(2:numRows) , c(2:numCols)]
CARL09_TDMTable <- graph.adjacency(CARL09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(CARL09_TDMTable, vertex.label = V(CARL09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
CARL09_TDM.clusterCoef <- transitivity(CARL09_TDMTable, type="global") #cluster coefficient
CARL09_TDM.degreeCent <- centralization.degree(CARL09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_TDMftn <- as.network.matrix(CARL09_TDMft)
CARL09_TDM.netDensity <- network.density(CARL09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_TDM.entropy <- entropy(CARL09_TDMft) #entropy

CARL09_TDM.netMx <- cbind(CARL09_TDM.netMx, CARL09_TDM.clusterCoef, CARL09_TDM.degreeCent$centralization,
                          CARL09_TDM.netDensity, CARL09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
CARL09_SDg2 <- data.frame(CARL09_SD)
CARL09_SDg2 <- CARL09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_SDg2$player1
player2vector <- CARL09_SDg2$player2
CARL09_SDg3 <- CARL09_SDg2
CARL09_SDg3$p1inp2vec <- is.element(CARL09_SDg3$player1, player2vector)
CARL09_SDg3$p2inp1vec <- is.element(CARL09_SDg3$player2, player1vector)

addPlayer1 <- CARL09_SDg3[ which(CARL09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_SDg3[ which(CARL09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_SDg2 <- rbind(CARL09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
CARL09_SDft <- ftable(CARL09_SDg2$player1, CARL09_SDg2$player2)
CARL09_SDft2 <- as.matrix(CARL09_SDft)
numRows <- nrow(CARL09_SDft2)
numCols <- ncol(CARL09_SDft2)
CARL09_SDft3 <- CARL09_SDft2[c(2:numRows) , c(2:numCols)]
CARL09_SDTable <- graph.adjacency(CARL09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(CARL09_SDTable, vertex.label = V(CARL09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
CARL09_SD.clusterCoef <- transitivity(CARL09_SDTable, type="global") #cluster coefficient
CARL09_SD.degreeCent <- centralization.degree(CARL09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_SDftn <- as.network.matrix(CARL09_SDft)
CARL09_SD.netDensity <- network.density(CARL09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_SD.entropy <- entropy(CARL09_SDft) #entropy

CARL09_SD.netMx <- cbind(CARL09_SD.netMx, CARL09_SD.clusterCoef, CARL09_SD.degreeCent$centralization,
                         CARL09_SD.netDensity, CARL09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
CARL09_TDg2 <- data.frame(CARL09_TD)
CARL09_TDg2 <- CARL09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_TDg2$player1
player2vector <- CARL09_TDg2$player2
CARL09_TDg3 <- CARL09_TDg2
CARL09_TDg3$p1inp2vec <- is.element(CARL09_TDg3$player1, player2vector)
CARL09_TDg3$p2inp1vec <- is.element(CARL09_TDg3$player2, player1vector)

addPlayer1 <- CARL09_TDg3[ which(CARL09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_TDg3[ which(CARL09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_TDg2 <- rbind(CARL09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
CARL09_TDft <- ftable(CARL09_TDg2$player1, CARL09_TDg2$player2)
CARL09_TDft2 <- as.matrix(CARL09_TDft)
numRows <- nrow(CARL09_TDft2)
numCols <- ncol(CARL09_TDft2)
CARL09_TDft3 <- CARL09_TDft2[c(2:numRows) , c(2:numCols)]
CARL09_TDTable <- graph.adjacency(CARL09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(CARL09_TDTable, vertex.label = V(CARL09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
CARL09_TD.clusterCoef <- transitivity(CARL09_TDTable, type="global") #cluster coefficient
CARL09_TD.degreeCent <- centralization.degree(CARL09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_TDftn <- as.network.matrix(CARL09_TDft)
CARL09_TD.netDensity <- network.density(CARL09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_TD.entropy <- entropy(CARL09_TDft) #entropy

CARL09_TD.netMx <- cbind(CARL09_TD.netMx, CARL09_TD.clusterCoef, CARL09_TD.degreeCent$centralization,
                         CARL09_TD.netDensity, CARL09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
CARL09_QTg2 <- data.frame(CARL09_QT)
CARL09_QTg2 <- CARL09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL09_QTg2$player1
player2vector <- CARL09_QTg2$player2
CARL09_QTg3 <- CARL09_QTg2
CARL09_QTg3$p1inp2vec <- is.element(CARL09_QTg3$player1, player2vector)
CARL09_QTg3$p2inp1vec <- is.element(CARL09_QTg3$player2, player1vector)

addPlayer1 <- CARL09_QTg3[ which(CARL09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL09_QTg3[ which(CARL09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL09_QTg2 <- rbind(CARL09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
CARL09_QTft <- ftable(CARL09_QTg2$player1, CARL09_QTg2$player2)
CARL09_QTft2 <- as.matrix(CARL09_QTft)
numRows <- nrow(CARL09_QTft2)
numCols <- ncol(CARL09_QTft2)
CARL09_QTft3 <- CARL09_QTft2[c(2:numRows) , c(2:numCols)]
CARL09_QTTable <- graph.adjacency(CARL09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(CARL09_QTTable, vertex.label = V(CARL09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
CARL09_QT.clusterCoef <- transitivity(CARL09_QTTable, type="global") #cluster coefficient
CARL09_QT.degreeCent <- centralization.degree(CARL09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL09_QTftn <- as.network.matrix(CARL09_QTft)
CARL09_QT.netDensity <- network.density(CARL09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL09_QT.entropy <- entropy(CARL09_QTft) #entropy

CARL09_QT.netMx <- cbind(CARL09_QT.netMx, CARL09_QT.clusterCoef, CARL09_QT.degreeCent$centralization,
                         CARL09_QT.netDensity, CARL09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL09_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "Goal_F"
COLL09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
COLL09_Gg2 <- data.frame(COLL09_G)
COLL09_Gg2 <- COLL09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_Gg2$player1
player2vector <- COLL09_Gg2$player2
COLL09_Gg3 <- COLL09_Gg2
COLL09_Gg3$p1inp2vec <- is.element(COLL09_Gg3$player1, player2vector)
COLL09_Gg3$p2inp1vec <- is.element(COLL09_Gg3$player2, player1vector)

addPlayer1 <- COLL09_Gg3[ which(COLL09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_Gg3[ which(COLL09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_Gg2 <- rbind(COLL09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
COLL09_Gft <- ftable(COLL09_Gg2$player1, COLL09_Gg2$player2)
COLL09_Gft2 <- as.matrix(COLL09_Gft)
numRows <- nrow(COLL09_Gft2)
numCols <- ncol(COLL09_Gft2)
COLL09_Gft3 <- COLL09_Gft2[c(2:numRows) , c(2:numCols)]
COLL09_GTable <- graph.adjacency(COLL09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(COLL09_GTable, vertex.label = V(COLL09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
COLL09_G.clusterCoef <- transitivity(COLL09_GTable, type="global") #cluster coefficient
COLL09_G.degreeCent <- centralization.degree(COLL09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_Gftn <- as.network.matrix(COLL09_Gft)
COLL09_G.netDensity <- network.density(COLL09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_G.entropy <- entropy(COLL09_Gft) #entropy

COLL09_G.netMx <- cbind(COLL09_G.netMx, COLL09_G.clusterCoef, COLL09_G.degreeCent$centralization,
                        COLL09_G.netDensity, COLL09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "Behind_F"
COLL09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
COLL09_Bg2 <- data.frame(COLL09_B)
COLL09_Bg2 <- COLL09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_Bg2$player1
player2vector <- COLL09_Bg2$player2
COLL09_Bg3 <- COLL09_Bg2
COLL09_Bg3$p1inp2vec <- is.element(COLL09_Bg3$player1, player2vector)
COLL09_Bg3$p2inp1vec <- is.element(COLL09_Bg3$player2, player1vector)

addPlayer1 <- COLL09_Bg3[ which(COLL09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_Bg3[ which(COLL09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_Bg2 <- rbind(COLL09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
COLL09_Bft <- ftable(COLL09_Bg2$player1, COLL09_Bg2$player2)
COLL09_Bft2 <- as.matrix(COLL09_Bft)
numRows <- nrow(COLL09_Bft2)
numCols <- ncol(COLL09_Bft2)
COLL09_Bft3 <- COLL09_Bft2[c(2:numRows) , c(2:numCols)]
COLL09_BTable <- graph.adjacency(COLL09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(COLL09_BTable, vertex.label = V(COLL09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
COLL09_B.clusterCoef <- transitivity(COLL09_BTable, type="global") #cluster coefficient
COLL09_B.degreeCent <- centralization.degree(COLL09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_Bftn <- as.network.matrix(COLL09_Bft)
COLL09_B.netDensity <- network.density(COLL09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_B.entropy <- entropy(COLL09_Bft) #entropy

COLL09_B.netMx <- cbind(COLL09_B.netMx, COLL09_B.clusterCoef, COLL09_B.degreeCent$centralization,
                        COLL09_B.netDensity, COLL09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
COLL09_SFg2 <- data.frame(COLL09_SF)
COLL09_SFg2 <- COLL09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_SFg2$player1
player2vector <- COLL09_SFg2$player2
COLL09_SFg3 <- COLL09_SFg2
COLL09_SFg3$p1inp2vec <- is.element(COLL09_SFg3$player1, player2vector)
COLL09_SFg3$p2inp1vec <- is.element(COLL09_SFg3$player2, player1vector)

addPlayer1 <- COLL09_SFg3[ which(COLL09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_SFg3[ which(COLL09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_SFg2 <- rbind(COLL09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
COLL09_SFft <- ftable(COLL09_SFg2$player1, COLL09_SFg2$player2)
COLL09_SFft2 <- as.matrix(COLL09_SFft)
numRows <- nrow(COLL09_SFft2)
numCols <- ncol(COLL09_SFft2)
COLL09_SFft3 <- COLL09_SFft2[c(2:numRows) , c(2:numCols)]
COLL09_SFTable <- graph.adjacency(COLL09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(COLL09_SFTable, vertex.label = V(COLL09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
COLL09_SF.clusterCoef <- transitivity(COLL09_SFTable, type="global") #cluster coefficient
COLL09_SF.degreeCent <- centralization.degree(COLL09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_SFftn <- as.network.matrix(COLL09_SFft)
COLL09_SF.netDensity <- network.density(COLL09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_SF.entropy <- entropy(COLL09_SFft) #entropy

COLL09_SF.netMx <- cbind(COLL09_SF.netMx, COLL09_SF.clusterCoef, COLL09_SF.degreeCent$centralization,
                         COLL09_SF.netDensity, COLL09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
COLL09_TFg2 <- data.frame(COLL09_TF)
COLL09_TFg2 <- COLL09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_TFg2$player1
player2vector <- COLL09_TFg2$player2
COLL09_TFg3 <- COLL09_TFg2
COLL09_TFg3$p1inp2vec <- is.element(COLL09_TFg3$player1, player2vector)
COLL09_TFg3$p2inp1vec <- is.element(COLL09_TFg3$player2, player1vector)

addPlayer1 <- COLL09_TFg3[ which(COLL09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_TFg3[ which(COLL09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_TFg2 <- rbind(COLL09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
COLL09_TFft <- ftable(COLL09_TFg2$player1, COLL09_TFg2$player2)
COLL09_TFft2 <- as.matrix(COLL09_TFft)
numRows <- nrow(COLL09_TFft2)
numCols <- ncol(COLL09_TFft2)
COLL09_TFft3 <- COLL09_TFft2[c(2:numRows) , c(2:numCols)]
COLL09_TFTable <- graph.adjacency(COLL09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(COLL09_TFTable, vertex.label = V(COLL09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
COLL09_TF.clusterCoef <- transitivity(COLL09_TFTable, type="global") #cluster coefficient
COLL09_TF.degreeCent <- centralization.degree(COLL09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_TFftn <- as.network.matrix(COLL09_TFft)
COLL09_TF.netDensity <- network.density(COLL09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_TF.entropy <- entropy(COLL09_TFft) #entropy

COLL09_TF.netMx <- cbind(COLL09_TF.netMx, COLL09_TF.clusterCoef, COLL09_TF.degreeCent$centralization,
                         COLL09_TF.netDensity, COLL09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
COLL09_SAMg2 <- data.frame(COLL09_SAM)
COLL09_SAMg2 <- COLL09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_SAMg2$player1
player2vector <- COLL09_SAMg2$player2
COLL09_SAMg3 <- COLL09_SAMg2
COLL09_SAMg3$p1inp2vec <- is.element(COLL09_SAMg3$player1, player2vector)
COLL09_SAMg3$p2inp1vec <- is.element(COLL09_SAMg3$player2, player1vector)

addPlayer1 <- COLL09_SAMg3[ which(COLL09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_SAMg3[ which(COLL09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_SAMg2 <- rbind(COLL09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
COLL09_SAMft <- ftable(COLL09_SAMg2$player1, COLL09_SAMg2$player2)
COLL09_SAMft2 <- as.matrix(COLL09_SAMft)
numRows <- nrow(COLL09_SAMft2)
numCols <- ncol(COLL09_SAMft2)
COLL09_SAMft3 <- COLL09_SAMft2[c(2:numRows) , c(2:numCols)]
COLL09_SAMTable <- graph.adjacency(COLL09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(COLL09_SAMTable, vertex.label = V(COLL09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
COLL09_SAM.clusterCoef <- transitivity(COLL09_SAMTable, type="global") #cluster coefficient
COLL09_SAM.degreeCent <- centralization.degree(COLL09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_SAMftn <- as.network.matrix(COLL09_SAMft)
COLL09_SAM.netDensity <- network.density(COLL09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_SAM.entropy <- entropy(COLL09_SAMft) #entropy

COLL09_SAM.netMx <- cbind(COLL09_SAM.netMx, COLL09_SAM.clusterCoef, COLL09_SAM.degreeCent$centralization,
                          COLL09_SAM.netDensity, COLL09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
COLL09_TAMg2 <- data.frame(COLL09_TAM)
COLL09_TAMg2 <- COLL09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_TAMg2$player1
player2vector <- COLL09_TAMg2$player2
COLL09_TAMg3 <- COLL09_TAMg2
COLL09_TAMg3$p1inp2vec <- is.element(COLL09_TAMg3$player1, player2vector)
COLL09_TAMg3$p2inp1vec <- is.element(COLL09_TAMg3$player2, player1vector)

addPlayer1 <- COLL09_TAMg3[ which(COLL09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_TAMg3[ which(COLL09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_TAMg2 <- rbind(COLL09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
COLL09_TAMft <- ftable(COLL09_TAMg2$player1, COLL09_TAMg2$player2)
COLL09_TAMft2 <- as.matrix(COLL09_TAMft)
numRows <- nrow(COLL09_TAMft2)
numCols <- ncol(COLL09_TAMft2)
COLL09_TAMft3 <- COLL09_TAMft2[c(2:numRows) , c(2:numCols)]
COLL09_TAMTable <- graph.adjacency(COLL09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(COLL09_TAMTable, vertex.label = V(COLL09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
COLL09_TAM.clusterCoef <- transitivity(COLL09_TAMTable, type="global") #cluster coefficient
COLL09_TAM.degreeCent <- centralization.degree(COLL09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_TAMftn <- as.network.matrix(COLL09_TAMft)
COLL09_TAM.netDensity <- network.density(COLL09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_TAM.entropy <- entropy(COLL09_TAMft) #entropy

COLL09_TAM.netMx <- cbind(COLL09_TAM.netMx, COLL09_TAM.clusterCoef, COLL09_TAM.degreeCent$centralization,
                          COLL09_TAM.netDensity, COLL09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
COLL09_SDMg2 <- data.frame(COLL09_SDM)
COLL09_SDMg2 <- COLL09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_SDMg2$player1
player2vector <- COLL09_SDMg2$player2
COLL09_SDMg3 <- COLL09_SDMg2
COLL09_SDMg3$p1inp2vec <- is.element(COLL09_SDMg3$player1, player2vector)
COLL09_SDMg3$p2inp1vec <- is.element(COLL09_SDMg3$player2, player1vector)

addPlayer1 <- COLL09_SDMg3[ which(COLL09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_SDMg3[ which(COLL09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_SDMg2 <- rbind(COLL09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
COLL09_SDMft <- ftable(COLL09_SDMg2$player1, COLL09_SDMg2$player2)
COLL09_SDMft2 <- as.matrix(COLL09_SDMft)
numRows <- nrow(COLL09_SDMft2)
numCols <- ncol(COLL09_SDMft2)
COLL09_SDMft3 <- COLL09_SDMft2[c(2:numRows) , c(2:numCols)]
COLL09_SDMTable <- graph.adjacency(COLL09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(COLL09_SDMTable, vertex.label = V(COLL09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
COLL09_SDM.clusterCoef <- transitivity(COLL09_SDMTable, type="global") #cluster coefficient
COLL09_SDM.degreeCent <- centralization.degree(COLL09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_SDMftn <- as.network.matrix(COLL09_SDMft)
COLL09_SDM.netDensity <- network.density(COLL09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_SDM.entropy <- entropy(COLL09_SDMft) #entropy

COLL09_SDM.netMx <- cbind(COLL09_SDM.netMx, COLL09_SDM.clusterCoef, COLL09_SDM.degreeCent$centralization,
                          COLL09_SDM.netDensity, COLL09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
COLL09_TDMg2 <- data.frame(COLL09_TDM)
COLL09_TDMg2 <- COLL09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_TDMg2$player1
player2vector <- COLL09_TDMg2$player2
COLL09_TDMg3 <- COLL09_TDMg2
COLL09_TDMg3$p1inp2vec <- is.element(COLL09_TDMg3$player1, player2vector)
COLL09_TDMg3$p2inp1vec <- is.element(COLL09_TDMg3$player2, player1vector)

addPlayer1 <- COLL09_TDMg3[ which(COLL09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_TDMg3[ which(COLL09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_TDMg2 <- rbind(COLL09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
COLL09_TDMft <- ftable(COLL09_TDMg2$player1, COLL09_TDMg2$player2)
COLL09_TDMft2 <- as.matrix(COLL09_TDMft)
numRows <- nrow(COLL09_TDMft2)
numCols <- ncol(COLL09_TDMft2)
COLL09_TDMft3 <- COLL09_TDMft2[c(2:numRows) , c(2:numCols)]
COLL09_TDMTable <- graph.adjacency(COLL09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(COLL09_TDMTable, vertex.label = V(COLL09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
COLL09_TDM.clusterCoef <- transitivity(COLL09_TDMTable, type="global") #cluster coefficient
COLL09_TDM.degreeCent <- centralization.degree(COLL09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_TDMftn <- as.network.matrix(COLL09_TDMft)
COLL09_TDM.netDensity <- network.density(COLL09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_TDM.entropy <- entropy(COLL09_TDMft) #entropy

COLL09_TDM.netMx <- cbind(COLL09_TDM.netMx, COLL09_TDM.clusterCoef, COLL09_TDM.degreeCent$centralization,
                          COLL09_TDM.netDensity, COLL09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
COLL09_SDg2 <- data.frame(COLL09_SD)
COLL09_SDg2 <- COLL09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_SDg2$player1
player2vector <- COLL09_SDg2$player2
COLL09_SDg3 <- COLL09_SDg2
COLL09_SDg3$p1inp2vec <- is.element(COLL09_SDg3$player1, player2vector)
COLL09_SDg3$p2inp1vec <- is.element(COLL09_SDg3$player2, player1vector)

addPlayer1 <- COLL09_SDg3[ which(COLL09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_SDg3[ which(COLL09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_SDg2 <- rbind(COLL09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
COLL09_SDft <- ftable(COLL09_SDg2$player1, COLL09_SDg2$player2)
COLL09_SDft2 <- as.matrix(COLL09_SDft)
numRows <- nrow(COLL09_SDft2)
numCols <- ncol(COLL09_SDft2)
COLL09_SDft3 <- COLL09_SDft2[c(2:numRows) , c(2:numCols)]
COLL09_SDTable <- graph.adjacency(COLL09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(COLL09_SDTable, vertex.label = V(COLL09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
COLL09_SD.clusterCoef <- transitivity(COLL09_SDTable, type="global") #cluster coefficient
COLL09_SD.degreeCent <- centralization.degree(COLL09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_SDftn <- as.network.matrix(COLL09_SDft)
COLL09_SD.netDensity <- network.density(COLL09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_SD.entropy <- entropy(COLL09_SDft) #entropy

COLL09_SD.netMx <- cbind(COLL09_SD.netMx, COLL09_SD.clusterCoef, COLL09_SD.degreeCent$centralization,
                         COLL09_SD.netDensity, COLL09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
COLL09_TDg2 <- data.frame(COLL09_TD)
COLL09_TDg2 <- COLL09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_TDg2$player1
player2vector <- COLL09_TDg2$player2
COLL09_TDg3 <- COLL09_TDg2
COLL09_TDg3$p1inp2vec <- is.element(COLL09_TDg3$player1, player2vector)
COLL09_TDg3$p2inp1vec <- is.element(COLL09_TDg3$player2, player1vector)

addPlayer1 <- COLL09_TDg3[ which(COLL09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_TDg3[ which(COLL09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_TDg2 <- rbind(COLL09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
COLL09_TDft <- ftable(COLL09_TDg2$player1, COLL09_TDg2$player2)
COLL09_TDft2 <- as.matrix(COLL09_TDft)
numRows <- nrow(COLL09_TDft2)
numCols <- ncol(COLL09_TDft2)
COLL09_TDft3 <- COLL09_TDft2[c(2:numRows) , c(2:numCols)]
COLL09_TDTable <- graph.adjacency(COLL09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(COLL09_TDTable, vertex.label = V(COLL09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
COLL09_TD.clusterCoef <- transitivity(COLL09_TDTable, type="global") #cluster coefficient
COLL09_TD.degreeCent <- centralization.degree(COLL09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_TDftn <- as.network.matrix(COLL09_TDft)
COLL09_TD.netDensity <- network.density(COLL09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_TD.entropy <- entropy(COLL09_TDft) #entropy

COLL09_TD.netMx <- cbind(COLL09_TD.netMx, COLL09_TD.clusterCoef, COLL09_TD.degreeCent$centralization,
                         COLL09_TD.netDensity, COLL09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
COLL09_QTg2 <- data.frame(COLL09_QT)
COLL09_QTg2 <- COLL09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL09_QTg2$player1
player2vector <- COLL09_QTg2$player2
COLL09_QTg3 <- COLL09_QTg2
COLL09_QTg3$p1inp2vec <- is.element(COLL09_QTg3$player1, player2vector)
COLL09_QTg3$p2inp1vec <- is.element(COLL09_QTg3$player2, player1vector)

addPlayer1 <- COLL09_QTg3[ which(COLL09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL09_QTg3[ which(COLL09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL09_QTg2 <- rbind(COLL09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
COLL09_QTft <- ftable(COLL09_QTg2$player1, COLL09_QTg2$player2)
COLL09_QTft2 <- as.matrix(COLL09_QTft)
numRows <- nrow(COLL09_QTft2)
numCols <- ncol(COLL09_QTft2)
COLL09_QTft3 <- COLL09_QTft2[c(2:numRows) , c(2:numCols)]
COLL09_QTTable <- graph.adjacency(COLL09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(COLL09_QTTable, vertex.label = V(COLL09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
COLL09_QT.clusterCoef <- transitivity(COLL09_QTTable, type="global") #cluster coefficient
COLL09_QT.degreeCent <- centralization.degree(COLL09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL09_QTftn <- as.network.matrix(COLL09_QTft)
COLL09_QT.netDensity <- network.density(COLL09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL09_QT.entropy <- entropy(COLL09_QTft) #entropy

COLL09_QT.netMx <- cbind(COLL09_QT.netMx, COLL09_QT.clusterCoef, COLL09_QT.degreeCent$centralization,
                         COLL09_QT.netDensity, COLL09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL09_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "ESS"
KIoutcome = "Goal_F"
ESS09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
ESS09_Gg2 <- data.frame(ESS09_G)
ESS09_Gg2 <- ESS09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_Gg2$player1
player2vector <- ESS09_Gg2$player2
ESS09_Gg3 <- ESS09_Gg2
ESS09_Gg3$p1inp2vec <- is.element(ESS09_Gg3$player1, player2vector)
ESS09_Gg3$p2inp1vec <- is.element(ESS09_Gg3$player2, player1vector)

addPlayer1 <- ESS09_Gg3[ which(ESS09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_Gg3[ which(ESS09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_Gg2 <- rbind(ESS09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
ESS09_Gft <- ftable(ESS09_Gg2$player1, ESS09_Gg2$player2)
ESS09_Gft2 <- as.matrix(ESS09_Gft)
numRows <- nrow(ESS09_Gft2)
numCols <- ncol(ESS09_Gft2)
ESS09_Gft3 <- ESS09_Gft2[c(2:numRows) , c(2:numCols)]
ESS09_GTable <- graph.adjacency(ESS09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(ESS09_GTable, vertex.label = V(ESS09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
ESS09_G.clusterCoef <- transitivity(ESS09_GTable, type="global") #cluster coefficient
ESS09_G.degreeCent <- centralization.degree(ESS09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_Gftn <- as.network.matrix(ESS09_Gft)
ESS09_G.netDensity <- network.density(ESS09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_G.entropy <- entropy(ESS09_Gft) #entropy

ESS09_G.netMx <- cbind(ESS09_G.netMx, ESS09_G.clusterCoef, ESS09_G.degreeCent$centralization,
                       ESS09_G.netDensity, ESS09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Behind_F"
ESS09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
ESS09_Bg2 <- data.frame(ESS09_B)
ESS09_Bg2 <- ESS09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_Bg2$player1
player2vector <- ESS09_Bg2$player2
ESS09_Bg3 <- ESS09_Bg2
ESS09_Bg3$p1inp2vec <- is.element(ESS09_Bg3$player1, player2vector)
ESS09_Bg3$p2inp1vec <- is.element(ESS09_Bg3$player2, player1vector)

addPlayer1 <- ESS09_Bg3[ which(ESS09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_Bg3[ which(ESS09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_Bg2 <- rbind(ESS09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
ESS09_Bft <- ftable(ESS09_Bg2$player1, ESS09_Bg2$player2)
ESS09_Bft2 <- as.matrix(ESS09_Bft)
numRows <- nrow(ESS09_Bft2)
numCols <- ncol(ESS09_Bft2)
ESS09_Bft3 <- ESS09_Bft2[c(2:numRows) , c(2:numCols)]
ESS09_BTable <- graph.adjacency(ESS09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(ESS09_BTable, vertex.label = V(ESS09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
ESS09_B.clusterCoef <- transitivity(ESS09_BTable, type="global") #cluster coefficient
ESS09_B.degreeCent <- centralization.degree(ESS09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_Bftn <- as.network.matrix(ESS09_Bft)
ESS09_B.netDensity <- network.density(ESS09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_B.entropy <- entropy(ESS09_Bft) #entropy

ESS09_B.netMx <- cbind(ESS09_B.netMx, ESS09_B.clusterCoef, ESS09_B.degreeCent$centralization,
                       ESS09_B.netDensity, ESS09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
ESS09_SFg2 <- data.frame(ESS09_SF)
ESS09_SFg2 <- ESS09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_SFg2$player1
player2vector <- ESS09_SFg2$player2
ESS09_SFg3 <- ESS09_SFg2
ESS09_SFg3$p1inp2vec <- is.element(ESS09_SFg3$player1, player2vector)
ESS09_SFg3$p2inp1vec <- is.element(ESS09_SFg3$player2, player1vector)

addPlayer1 <- ESS09_SFg3[ which(ESS09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_SFg3[ which(ESS09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_SFg2 <- rbind(ESS09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
ESS09_SFft <- ftable(ESS09_SFg2$player1, ESS09_SFg2$player2)
ESS09_SFft2 <- as.matrix(ESS09_SFft)
numRows <- nrow(ESS09_SFft2)
numCols <- ncol(ESS09_SFft2)
ESS09_SFft3 <- ESS09_SFft2[c(2:numRows) , c(2:numCols)]
ESS09_SFTable <- graph.adjacency(ESS09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(ESS09_SFTable, vertex.label = V(ESS09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
ESS09_SF.clusterCoef <- transitivity(ESS09_SFTable, type="global") #cluster coefficient
ESS09_SF.degreeCent <- centralization.degree(ESS09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_SFftn <- as.network.matrix(ESS09_SFft)
ESS09_SF.netDensity <- network.density(ESS09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_SF.entropy <- entropy(ESS09_SFft) #entropy

ESS09_SF.netMx <- cbind(ESS09_SF.netMx, ESS09_SF.clusterCoef, ESS09_SF.degreeCent$centralization,
                        ESS09_SF.netDensity, ESS09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
ESS09_TFg2 <- data.frame(ESS09_TF)
ESS09_TFg2 <- ESS09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_TFg2$player1
player2vector <- ESS09_TFg2$player2
ESS09_TFg3 <- ESS09_TFg2
ESS09_TFg3$p1inp2vec <- is.element(ESS09_TFg3$player1, player2vector)
ESS09_TFg3$p2inp1vec <- is.element(ESS09_TFg3$player2, player1vector)

addPlayer1 <- ESS09_TFg3[ which(ESS09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_TFg3[ which(ESS09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_TFg2 <- rbind(ESS09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
ESS09_TFft <- ftable(ESS09_TFg2$player1, ESS09_TFg2$player2)
ESS09_TFft2 <- as.matrix(ESS09_TFft)
numRows <- nrow(ESS09_TFft2)
numCols <- ncol(ESS09_TFft2)
ESS09_TFft3 <- ESS09_TFft2[c(2:numRows) , c(2:numCols)]
ESS09_TFTable <- graph.adjacency(ESS09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(ESS09_TFTable, vertex.label = V(ESS09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
ESS09_TF.clusterCoef <- transitivity(ESS09_TFTable, type="global") #cluster coefficient
ESS09_TF.degreeCent <- centralization.degree(ESS09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_TFftn <- as.network.matrix(ESS09_TFft)
ESS09_TF.netDensity <- network.density(ESS09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_TF.entropy <- entropy(ESS09_TFft) #entropy

ESS09_TF.netMx <- cbind(ESS09_TF.netMx, ESS09_TF.clusterCoef, ESS09_TF.degreeCent$centralization,
                        ESS09_TF.netDensity, ESS09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
ESS09_SAMg2 <- data.frame(ESS09_SAM)
ESS09_SAMg2 <- ESS09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_SAMg2$player1
player2vector <- ESS09_SAMg2$player2
ESS09_SAMg3 <- ESS09_SAMg2
ESS09_SAMg3$p1inp2vec <- is.element(ESS09_SAMg3$player1, player2vector)
ESS09_SAMg3$p2inp1vec <- is.element(ESS09_SAMg3$player2, player1vector)

addPlayer1 <- ESS09_SAMg3[ which(ESS09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_SAMg3[ which(ESS09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_SAMg2 <- rbind(ESS09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
ESS09_SAMft <- ftable(ESS09_SAMg2$player1, ESS09_SAMg2$player2)
ESS09_SAMft2 <- as.matrix(ESS09_SAMft)
numRows <- nrow(ESS09_SAMft2)
numCols <- ncol(ESS09_SAMft2)
ESS09_SAMft3 <- ESS09_SAMft2[c(2:numRows) , c(2:numCols)]
ESS09_SAMTable <- graph.adjacency(ESS09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(ESS09_SAMTable, vertex.label = V(ESS09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
ESS09_SAM.clusterCoef <- transitivity(ESS09_SAMTable, type="global") #cluster coefficient
ESS09_SAM.degreeCent <- centralization.degree(ESS09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_SAMftn <- as.network.matrix(ESS09_SAMft)
ESS09_SAM.netDensity <- network.density(ESS09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_SAM.entropy <- entropy(ESS09_SAMft) #entropy

ESS09_SAM.netMx <- cbind(ESS09_SAM.netMx, ESS09_SAM.clusterCoef, ESS09_SAM.degreeCent$centralization,
                         ESS09_SAM.netDensity, ESS09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
ESS09_TAMg2 <- data.frame(ESS09_TAM)
ESS09_TAMg2 <- ESS09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_TAMg2$player1
player2vector <- ESS09_TAMg2$player2
ESS09_TAMg3 <- ESS09_TAMg2
ESS09_TAMg3$p1inp2vec <- is.element(ESS09_TAMg3$player1, player2vector)
ESS09_TAMg3$p2inp1vec <- is.element(ESS09_TAMg3$player2, player1vector)

addPlayer1 <- ESS09_TAMg3[ which(ESS09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS09_TAMg3[ which(ESS09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_TAMg2 <- rbind(ESS09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
ESS09_TAMft <- ftable(ESS09_TAMg2$player1, ESS09_TAMg2$player2)
ESS09_TAMft2 <- as.matrix(ESS09_TAMft)
numRows <- nrow(ESS09_TAMft2)
numCols <- ncol(ESS09_TAMft2)
ESS09_TAMft3 <- ESS09_TAMft2[c(2:numRows) , c(2:numCols)]
ESS09_TAMTable <- graph.adjacency(ESS09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(ESS09_TAMTable, vertex.label = V(ESS09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
ESS09_TAM.clusterCoef <- transitivity(ESS09_TAMTable, type="global") #cluster coefficient
ESS09_TAM.degreeCent <- centralization.degree(ESS09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_TAMftn <- as.network.matrix(ESS09_TAMft)
ESS09_TAM.netDensity <- network.density(ESS09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_TAM.entropy <- entropy(ESS09_TAMft) #entropy

ESS09_TAM.netMx <- cbind(ESS09_TAM.netMx, ESS09_TAM.clusterCoef, ESS09_TAM.degreeCent$centralization,
                         ESS09_TAM.netDensity, ESS09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
ESS09_SDMg2 <- data.frame(ESS09_SDM)
ESS09_SDMg2 <- ESS09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_SDMg2$player1
player2vector <- ESS09_SDMg2$player2
ESS09_SDMg3 <- ESS09_SDMg2
ESS09_SDMg3$p1inp2vec <- is.element(ESS09_SDMg3$player1, player2vector)
ESS09_SDMg3$p2inp1vec <- is.element(ESS09_SDMg3$player2, player1vector)

addPlayer1 <- ESS09_SDMg3[ which(ESS09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_SDMg3[ which(ESS09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_SDMg2 <- rbind(ESS09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
ESS09_SDMft <- ftable(ESS09_SDMg2$player1, ESS09_SDMg2$player2)
ESS09_SDMft2 <- as.matrix(ESS09_SDMft)
numRows <- nrow(ESS09_SDMft2)
numCols <- ncol(ESS09_SDMft2)
ESS09_SDMft3 <- ESS09_SDMft2[c(2:numRows) , c(2:numCols)]
ESS09_SDMTable <- graph.adjacency(ESS09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(ESS09_SDMTable, vertex.label = V(ESS09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
ESS09_SDM.clusterCoef <- transitivity(ESS09_SDMTable, type="global") #cluster coefficient
ESS09_SDM.degreeCent <- centralization.degree(ESS09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_SDMftn <- as.network.matrix(ESS09_SDMft)
ESS09_SDM.netDensity <- network.density(ESS09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_SDM.entropy <- entropy(ESS09_SDMft) #entropy

ESS09_SDM.netMx <- cbind(ESS09_SDM.netMx, ESS09_SDM.clusterCoef, ESS09_SDM.degreeCent$centralization,
                         ESS09_SDM.netDensity, ESS09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
ESS09_TDMg2 <- data.frame(ESS09_TDM)
ESS09_TDMg2 <- ESS09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_TDMg2$player1
player2vector <- ESS09_TDMg2$player2
ESS09_TDMg3 <- ESS09_TDMg2
ESS09_TDMg3$p1inp2vec <- is.element(ESS09_TDMg3$player1, player2vector)
ESS09_TDMg3$p2inp1vec <- is.element(ESS09_TDMg3$player2, player1vector)

addPlayer1 <- ESS09_TDMg3[ which(ESS09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_TDMg3[ which(ESS09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_TDMg2 <- rbind(ESS09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
ESS09_TDMft <- ftable(ESS09_TDMg2$player1, ESS09_TDMg2$player2)
ESS09_TDMft2 <- as.matrix(ESS09_TDMft)
numRows <- nrow(ESS09_TDMft2)
numCols <- ncol(ESS09_TDMft2)
ESS09_TDMft3 <- ESS09_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS09_TDMTable <- graph.adjacency(ESS09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(ESS09_TDMTable, vertex.label = V(ESS09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
ESS09_TDM.clusterCoef <- transitivity(ESS09_TDMTable, type="global") #cluster coefficient
ESS09_TDM.degreeCent <- centralization.degree(ESS09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_TDMftn <- as.network.matrix(ESS09_TDMft)
ESS09_TDM.netDensity <- network.density(ESS09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_TDM.entropy <- entropy(ESS09_TDMft) #entropy

ESS09_TDM.netMx <- cbind(ESS09_TDM.netMx, ESS09_TDM.clusterCoef, ESS09_TDM.degreeCent$centralization,
                         ESS09_TDM.netDensity, ESS09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
ESS09_SDg2 <- data.frame(ESS09_SD)
ESS09_SDg2 <- ESS09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_SDg2$player1
player2vector <- ESS09_SDg2$player2
ESS09_SDg3 <- ESS09_SDg2
ESS09_SDg3$p1inp2vec <- is.element(ESS09_SDg3$player1, player2vector)
ESS09_SDg3$p2inp1vec <- is.element(ESS09_SDg3$player2, player1vector)

addPlayer1 <- ESS09_SDg3[ which(ESS09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_SDg3[ which(ESS09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_SDg2 <- rbind(ESS09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
ESS09_SDft <- ftable(ESS09_SDg2$player1, ESS09_SDg2$player2)
ESS09_SDft2 <- as.matrix(ESS09_SDft)
numRows <- nrow(ESS09_SDft2)
numCols <- ncol(ESS09_SDft2)
ESS09_SDft3 <- ESS09_SDft2[c(2:numRows) , c(2:numCols)]
ESS09_SDTable <- graph.adjacency(ESS09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(ESS09_SDTable, vertex.label = V(ESS09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
ESS09_SD.clusterCoef <- transitivity(ESS09_SDTable, type="global") #cluster coefficient
ESS09_SD.degreeCent <- centralization.degree(ESS09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_SDftn <- as.network.matrix(ESS09_SDft)
ESS09_SD.netDensity <- network.density(ESS09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_SD.entropy <- entropy(ESS09_SDft) #entropy

ESS09_SD.netMx <- cbind(ESS09_SD.netMx, ESS09_SD.clusterCoef, ESS09_SD.degreeCent$centralization,
                        ESS09_SD.netDensity, ESS09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
ESS09_TDg2 <- data.frame(ESS09_TD)
ESS09_TDg2 <- ESS09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_TDg2$player1
player2vector <- ESS09_TDg2$player2
ESS09_TDg3 <- ESS09_TDg2
ESS09_TDg3$p1inp2vec <- is.element(ESS09_TDg3$player1, player2vector)
ESS09_TDg3$p2inp1vec <- is.element(ESS09_TDg3$player2, player1vector)

addPlayer1 <- ESS09_TDg3[ which(ESS09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_TDg3[ which(ESS09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_TDg2 <- rbind(ESS09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
ESS09_TDft <- ftable(ESS09_TDg2$player1, ESS09_TDg2$player2)
ESS09_TDft2 <- as.matrix(ESS09_TDft)
numRows <- nrow(ESS09_TDft2)
numCols <- ncol(ESS09_TDft2)
ESS09_TDft3 <- ESS09_TDft2[c(2:numRows) , c(2:numCols)]
ESS09_TDTable <- graph.adjacency(ESS09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(ESS09_TDTable, vertex.label = V(ESS09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
ESS09_TD.clusterCoef <- transitivity(ESS09_TDTable, type="global") #cluster coefficient
ESS09_TD.degreeCent <- centralization.degree(ESS09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_TDftn <- as.network.matrix(ESS09_TDft)
ESS09_TD.netDensity <- network.density(ESS09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_TD.entropy <- entropy(ESS09_TDft) #entropy

ESS09_TD.netMx <- cbind(ESS09_TD.netMx, ESS09_TD.clusterCoef, ESS09_TD.degreeCent$centralization,
                        ESS09_TD.netDensity, ESS09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
ESS09_QTg2 <- data.frame(ESS09_QT)
ESS09_QTg2 <- ESS09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS09_QTg2$player1
player2vector <- ESS09_QTg2$player2
ESS09_QTg3 <- ESS09_QTg2
ESS09_QTg3$p1inp2vec <- is.element(ESS09_QTg3$player1, player2vector)
ESS09_QTg3$p2inp1vec <- is.element(ESS09_QTg3$player2, player1vector)

addPlayer1 <- ESS09_QTg3[ which(ESS09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS09_QTg3[ which(ESS09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS09_QTg2 <- rbind(ESS09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
ESS09_QTft <- ftable(ESS09_QTg2$player1, ESS09_QTg2$player2)
ESS09_QTft2 <- as.matrix(ESS09_QTft)
numRows <- nrow(ESS09_QTft2)
numCols <- ncol(ESS09_QTft2)
ESS09_QTft3 <- ESS09_QTft2[c(2:numRows) , c(2:numCols)]
ESS09_QTTable <- graph.adjacency(ESS09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(ESS09_QTTable, vertex.label = V(ESS09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
ESS09_QT.clusterCoef <- transitivity(ESS09_QTTable, type="global") #cluster coefficient
ESS09_QT.degreeCent <- centralization.degree(ESS09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS09_QTftn <- as.network.matrix(ESS09_QTft)
ESS09_QT.netDensity <- network.density(ESS09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS09_QT.entropy <- entropy(ESS09_QTft) #entropy

ESS09_QT.netMx <- cbind(ESS09_QT.netMx, ESS09_QT.clusterCoef, ESS09_QT.degreeCent$centralization,
                        ESS09_QT.netDensity, ESS09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS09_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "FRE"
KIoutcome = "Goal_F"
FRE09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
FRE09_Gg2 <- data.frame(FRE09_G)
FRE09_Gg2 <- FRE09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_Gg2$player1
player2vector <- FRE09_Gg2$player2
FRE09_Gg3 <- FRE09_Gg2
FRE09_Gg3$p1inp2vec <- is.element(FRE09_Gg3$player1, player2vector)
FRE09_Gg3$p2inp1vec <- is.element(FRE09_Gg3$player2, player1vector)

addPlayer1 <- FRE09_Gg3[ which(FRE09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_Gg3[ which(FRE09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_Gg2 <- rbind(FRE09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
FRE09_Gft <- ftable(FRE09_Gg2$player1, FRE09_Gg2$player2)
FRE09_Gft2 <- as.matrix(FRE09_Gft)
numRows <- nrow(FRE09_Gft2)
numCols <- ncol(FRE09_Gft2)
FRE09_Gft3 <- FRE09_Gft2[c(2:numRows) , c(2:numCols)]
FRE09_GTable <- graph.adjacency(FRE09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(FRE09_GTable, vertex.label = V(FRE09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
FRE09_G.clusterCoef <- transitivity(FRE09_GTable, type="global") #cluster coefficient
FRE09_G.degreeCent <- centralization.degree(FRE09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_Gftn <- as.network.matrix(FRE09_Gft)
FRE09_G.netDensity <- network.density(FRE09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_G.entropy <- entropy(FRE09_Gft) #entropy

FRE09_G.netMx <- cbind(FRE09_G.netMx, FRE09_G.clusterCoef, FRE09_G.degreeCent$centralization,
                       FRE09_G.netDensity, FRE09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "FRE"
KIoutcome = "Behind_F"
FRE09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
FRE09_Bg2 <- data.frame(FRE09_B)
FRE09_Bg2 <- FRE09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_Bg2$player1
player2vector <- FRE09_Bg2$player2
FRE09_Bg3 <- FRE09_Bg2
FRE09_Bg3$p1inp2vec <- is.element(FRE09_Bg3$player1, player2vector)
FRE09_Bg3$p2inp1vec <- is.element(FRE09_Bg3$player2, player1vector)

addPlayer1 <- FRE09_Bg3[ which(FRE09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_Bg3[ which(FRE09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_Bg2 <- rbind(FRE09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
FRE09_Bft <- ftable(FRE09_Bg2$player1, FRE09_Bg2$player2)
FRE09_Bft2 <- as.matrix(FRE09_Bft)
numRows <- nrow(FRE09_Bft2)
numCols <- ncol(FRE09_Bft2)
FRE09_Bft3 <- FRE09_Bft2[c(2:numRows) , c(2:numCols)]
FRE09_BTable <- graph.adjacency(FRE09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(FRE09_BTable, vertex.label = V(FRE09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
FRE09_B.clusterCoef <- transitivity(FRE09_BTable, type="global") #cluster coefficient
FRE09_B.degreeCent <- centralization.degree(FRE09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_Bftn <- as.network.matrix(FRE09_Bft)
FRE09_B.netDensity <- network.density(FRE09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_B.entropy <- entropy(FRE09_Bft) #entropy

FRE09_B.netMx <- cbind(FRE09_B.netMx, FRE09_B.clusterCoef, FRE09_B.degreeCent$centralization,
                       FRE09_B.netDensity, FRE09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
FRE09_SFg2 <- data.frame(FRE09_SF)
FRE09_SFg2 <- FRE09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_SFg2$player1
player2vector <- FRE09_SFg2$player2
FRE09_SFg3 <- FRE09_SFg2
FRE09_SFg3$p1inp2vec <- is.element(FRE09_SFg3$player1, player2vector)
FRE09_SFg3$p2inp1vec <- is.element(FRE09_SFg3$player2, player1vector)

addPlayer1 <- FRE09_SFg3[ which(FRE09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_SFg3[ which(FRE09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_SFg2 <- rbind(FRE09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
FRE09_SFft <- ftable(FRE09_SFg2$player1, FRE09_SFg2$player2)
FRE09_SFft2 <- as.matrix(FRE09_SFft)
numRows <- nrow(FRE09_SFft2)
numCols <- ncol(FRE09_SFft2)
FRE09_SFft3 <- FRE09_SFft2[c(2:numRows) , c(2:numCols)]
FRE09_SFTable <- graph.adjacency(FRE09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(FRE09_SFTable, vertex.label = V(FRE09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
FRE09_SF.clusterCoef <- transitivity(FRE09_SFTable, type="global") #cluster coefficient
FRE09_SF.degreeCent <- centralization.degree(FRE09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_SFftn <- as.network.matrix(FRE09_SFft)
FRE09_SF.netDensity <- network.density(FRE09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_SF.entropy <- entropy(FRE09_SFft) #entropy

FRE09_SF.netMx <- cbind(FRE09_SF.netMx, FRE09_SF.clusterCoef, FRE09_SF.degreeCent$centralization,
                        FRE09_SF.netDensity, FRE09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
FRE09_TFg2 <- data.frame(FRE09_TF)
FRE09_TFg2 <- FRE09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_TFg2$player1
player2vector <- FRE09_TFg2$player2
FRE09_TFg3 <- FRE09_TFg2
FRE09_TFg3$p1inp2vec <- is.element(FRE09_TFg3$player1, player2vector)
FRE09_TFg3$p2inp1vec <- is.element(FRE09_TFg3$player2, player1vector)

addPlayer1 <- FRE09_TFg3[ which(FRE09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_TFg3[ which(FRE09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_TFg2 <- rbind(FRE09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
FRE09_TFft <- ftable(FRE09_TFg2$player1, FRE09_TFg2$player2)
FRE09_TFft2 <- as.matrix(FRE09_TFft)
numRows <- nrow(FRE09_TFft2)
numCols <- ncol(FRE09_TFft2)
FRE09_TFft3 <- FRE09_TFft2[c(2:numRows) , c(2:numCols)]
FRE09_TFTable <- graph.adjacency(FRE09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(FRE09_TFTable, vertex.label = V(FRE09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
FRE09_TF.clusterCoef <- transitivity(FRE09_TFTable, type="global") #cluster coefficient
FRE09_TF.degreeCent <- centralization.degree(FRE09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_TFftn <- as.network.matrix(FRE09_TFft)
FRE09_TF.netDensity <- network.density(FRE09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_TF.entropy <- entropy(FRE09_TFft) #entropy

FRE09_TF.netMx <- cbind(FRE09_TF.netMx, FRE09_TF.clusterCoef, FRE09_TF.degreeCent$centralization,
                        FRE09_TF.netDensity, FRE09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
FRE09_SAMg2 <- data.frame(FRE09_SAM)
FRE09_SAMg2 <- FRE09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_SAMg2$player1
player2vector <- FRE09_SAMg2$player2
FRE09_SAMg3 <- FRE09_SAMg2
FRE09_SAMg3$p1inp2vec <- is.element(FRE09_SAMg3$player1, player2vector)
FRE09_SAMg3$p2inp1vec <- is.element(FRE09_SAMg3$player2, player1vector)

addPlayer1 <- FRE09_SAMg3[ which(FRE09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_SAMg3[ which(FRE09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_SAMg2 <- rbind(FRE09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
FRE09_SAMft <- ftable(FRE09_SAMg2$player1, FRE09_SAMg2$player2)
FRE09_SAMft2 <- as.matrix(FRE09_SAMft)
numRows <- nrow(FRE09_SAMft2)
numCols <- ncol(FRE09_SAMft2)
FRE09_SAMft3 <- FRE09_SAMft2[c(2:numRows) , c(2:numCols)]
FRE09_SAMTable <- graph.adjacency(FRE09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(FRE09_SAMTable, vertex.label = V(FRE09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
FRE09_SAM.clusterCoef <- transitivity(FRE09_SAMTable, type="global") #cluster coefficient
FRE09_SAM.degreeCent <- centralization.degree(FRE09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_SAMftn <- as.network.matrix(FRE09_SAMft)
FRE09_SAM.netDensity <- network.density(FRE09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_SAM.entropy <- entropy(FRE09_SAMft) #entropy

FRE09_SAM.netMx <- cbind(FRE09_SAM.netMx, FRE09_SAM.clusterCoef, FRE09_SAM.degreeCent$centralization,
                         FRE09_SAM.netDensity, FRE09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
FRE09_TAMg2 <- data.frame(FRE09_TAM)
FRE09_TAMg2 <- FRE09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_TAMg2$player1
player2vector <- FRE09_TAMg2$player2
FRE09_TAMg3 <- FRE09_TAMg2
FRE09_TAMg3$p1inp2vec <- is.element(FRE09_TAMg3$player1, player2vector)
FRE09_TAMg3$p2inp1vec <- is.element(FRE09_TAMg3$player2, player1vector)

addPlayer1 <- FRE09_TAMg3[ which(FRE09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_TAMg3[ which(FRE09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_TAMg2 <- rbind(FRE09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
FRE09_TAMft <- ftable(FRE09_TAMg2$player1, FRE09_TAMg2$player2)
FRE09_TAMft2 <- as.matrix(FRE09_TAMft)
numRows <- nrow(FRE09_TAMft2)
numCols <- ncol(FRE09_TAMft2)
FRE09_TAMft3 <- FRE09_TAMft2[c(2:numRows) , c(2:numCols)]
FRE09_TAMTable <- graph.adjacency(FRE09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(FRE09_TAMTable, vertex.label = V(FRE09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
FRE09_TAM.clusterCoef <- transitivity(FRE09_TAMTable, type="global") #cluster coefficient
FRE09_TAM.degreeCent <- centralization.degree(FRE09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_TAMftn <- as.network.matrix(FRE09_TAMft)
FRE09_TAM.netDensity <- network.density(FRE09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_TAM.entropy <- entropy(FRE09_TAMft) #entropy

FRE09_TAM.netMx <- cbind(FRE09_TAM.netMx, FRE09_TAM.clusterCoef, FRE09_TAM.degreeCent$centralization,
                         FRE09_TAM.netDensity, FRE09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
FRE09_SDMg2 <- data.frame(FRE09_SDM)
FRE09_SDMg2 <- FRE09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_SDMg2$player1
player2vector <- FRE09_SDMg2$player2
FRE09_SDMg3 <- FRE09_SDMg2
FRE09_SDMg3$p1inp2vec <- is.element(FRE09_SDMg3$player1, player2vector)
FRE09_SDMg3$p2inp1vec <- is.element(FRE09_SDMg3$player2, player1vector)

addPlayer1 <- FRE09_SDMg3[ which(FRE09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE09_SDMg3[ which(FRE09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_SDMg2 <- rbind(FRE09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
FRE09_SDMft <- ftable(FRE09_SDMg2$player1, FRE09_SDMg2$player2)
FRE09_SDMft2 <- as.matrix(FRE09_SDMft)
numRows <- nrow(FRE09_SDMft2)
numCols <- ncol(FRE09_SDMft2)
FRE09_SDMft3 <- FRE09_SDMft2[c(2:numRows) , c(2:numCols)]
FRE09_SDMTable <- graph.adjacency(FRE09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(FRE09_SDMTable, vertex.label = V(FRE09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
FRE09_SDM.clusterCoef <- transitivity(FRE09_SDMTable, type="global") #cluster coefficient
FRE09_SDM.degreeCent <- centralization.degree(FRE09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_SDMftn <- as.network.matrix(FRE09_SDMft)
FRE09_SDM.netDensity <- network.density(FRE09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_SDM.entropy <- entropy(FRE09_SDMft) #entropy

FRE09_SDM.netMx <- cbind(FRE09_SDM.netMx, FRE09_SDM.clusterCoef, FRE09_SDM.degreeCent$centralization,
                         FRE09_SDM.netDensity, FRE09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
FRE09_TDMg2 <- data.frame(FRE09_TDM)
FRE09_TDMg2 <- FRE09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_TDMg2$player1
player2vector <- FRE09_TDMg2$player2
FRE09_TDMg3 <- FRE09_TDMg2
FRE09_TDMg3$p1inp2vec <- is.element(FRE09_TDMg3$player1, player2vector)
FRE09_TDMg3$p2inp1vec <- is.element(FRE09_TDMg3$player2, player1vector)

addPlayer1 <- FRE09_TDMg3[ which(FRE09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_TDMg3[ which(FRE09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_TDMg2 <- rbind(FRE09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
FRE09_TDMft <- ftable(FRE09_TDMg2$player1, FRE09_TDMg2$player2)
FRE09_TDMft2 <- as.matrix(FRE09_TDMft)
numRows <- nrow(FRE09_TDMft2)
numCols <- ncol(FRE09_TDMft2)
FRE09_TDMft3 <- FRE09_TDMft2[c(2:numRows) , c(2:numCols)]
FRE09_TDMTable <- graph.adjacency(FRE09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(FRE09_TDMTable, vertex.label = V(FRE09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
FRE09_TDM.clusterCoef <- transitivity(FRE09_TDMTable, type="global") #cluster coefficient
FRE09_TDM.degreeCent <- centralization.degree(FRE09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_TDMftn <- as.network.matrix(FRE09_TDMft)
FRE09_TDM.netDensity <- network.density(FRE09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_TDM.entropy <- entropy(FRE09_TDMft) #entropy

FRE09_TDM.netMx <- cbind(FRE09_TDM.netMx, FRE09_TDM.clusterCoef, FRE09_TDM.degreeCent$centralization,
                         FRE09_TDM.netDensity, FRE09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************

round = 9
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
FRE09_SDg2 <- data.frame(FRE09_SD)
FRE09_SDg2 <- FRE09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_SDg2$player1
player2vector <- FRE09_SDg2$player2
FRE09_SDg3 <- FRE09_SDg2
FRE09_SDg3$p1inp2vec <- is.element(FRE09_SDg3$player1, player2vector)
FRE09_SDg3$p2inp1vec <- is.element(FRE09_SDg3$player2, player1vector)

addPlayer1 <- FRE09_SDg3[ which(FRE09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_SDg3[ which(FRE09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_SDg2 <- rbind(FRE09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
FRE09_SDft <- ftable(FRE09_SDg2$player1, FRE09_SDg2$player2)
FRE09_SDft2 <- as.matrix(FRE09_SDft)
numRows <- nrow(FRE09_SDft2)
numCols <- ncol(FRE09_SDft2)
FRE09_SDft3 <- FRE09_SDft2[c(2:numRows) , c(2:numCols)]
FRE09_SDTable <- graph.adjacency(FRE09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(FRE09_SDTable, vertex.label = V(FRE09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
FRE09_SD.clusterCoef <- transitivity(FRE09_SDTable, type="global") #cluster coefficient
FRE09_SD.degreeCent <- centralization.degree(FRE09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_SDftn <- as.network.matrix(FRE09_SDft)
FRE09_SD.netDensity <- network.density(FRE09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_SD.entropy <- entropy(FRE09_SDft) #entropy

FRE09_SD.netMx <- cbind(FRE09_SD.netMx, FRE09_SD.clusterCoef, FRE09_SD.degreeCent$centralization,
                        FRE09_SD.netDensity, FRE09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
FRE09_TDg2 <- data.frame(FRE09_TD)
FRE09_TDg2 <- FRE09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_TDg2$player1
player2vector <- FRE09_TDg2$player2
FRE09_TDg3 <- FRE09_TDg2
FRE09_TDg3$p1inp2vec <- is.element(FRE09_TDg3$player1, player2vector)
FRE09_TDg3$p2inp1vec <- is.element(FRE09_TDg3$player2, player1vector)

addPlayer1 <- FRE09_TDg3[ which(FRE09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_TDg3[ which(FRE09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_TDg2 <- rbind(FRE09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
FRE09_TDft <- ftable(FRE09_TDg2$player1, FRE09_TDg2$player2)
FRE09_TDft2 <- as.matrix(FRE09_TDft)
numRows <- nrow(FRE09_TDft2)
numCols <- ncol(FRE09_TDft2)
FRE09_TDft3 <- FRE09_TDft2[c(2:numRows) , c(2:numCols)]
FRE09_TDTable <- graph.adjacency(FRE09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(FRE09_TDTable, vertex.label = V(FRE09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
FRE09_TD.clusterCoef <- transitivity(FRE09_TDTable, type="global") #cluster coefficient
FRE09_TD.degreeCent <- centralization.degree(FRE09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_TDftn <- as.network.matrix(FRE09_TDft)
FRE09_TD.netDensity <- network.density(FRE09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_TD.entropy <- entropy(FRE09_TDft) #entropy

FRE09_TD.netMx <- cbind(FRE09_TD.netMx, FRE09_TD.clusterCoef, FRE09_TD.degreeCent$centralization,
                        FRE09_TD.netDensity, FRE09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
FRE09_QTg2 <- data.frame(FRE09_QT)
FRE09_QTg2 <- FRE09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE09_QTg2$player1
player2vector <- FRE09_QTg2$player2
FRE09_QTg3 <- FRE09_QTg2
FRE09_QTg3$p1inp2vec <- is.element(FRE09_QTg3$player1, player2vector)
FRE09_QTg3$p2inp1vec <- is.element(FRE09_QTg3$player2, player1vector)

addPlayer1 <- FRE09_QTg3[ which(FRE09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE09_QTg3[ which(FRE09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE09_QTg2 <- rbind(FRE09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
FRE09_QTft <- ftable(FRE09_QTg2$player1, FRE09_QTg2$player2)
FRE09_QTft2 <- as.matrix(FRE09_QTft)
numRows <- nrow(FRE09_QTft2)
numCols <- ncol(FRE09_QTft2)
FRE09_QTft3 <- FRE09_QTft2[c(2:numRows) , c(2:numCols)]
FRE09_QTTable <- graph.adjacency(FRE09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(FRE09_QTTable, vertex.label = V(FRE09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
FRE09_QT.clusterCoef <- transitivity(FRE09_QTTable, type="global") #cluster coefficient
FRE09_QT.degreeCent <- centralization.degree(FRE09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE09_QTftn <- as.network.matrix(FRE09_QTft)
FRE09_QT.netDensity <- network.density(FRE09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE09_QT.entropy <- entropy(FRE09_QTft) #entropy

FRE09_QT.netMx <- cbind(FRE09_QT.netMx, FRE09_QT.clusterCoef, FRE09_QT.degreeCent$centralization,
                        FRE09_QT.netDensity, FRE09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE09_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
GCFC09_Gg2 <- data.frame(GCFC09_G)
GCFC09_Gg2 <- GCFC09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_Gg2$player1
player2vector <- GCFC09_Gg2$player2
GCFC09_Gg3 <- GCFC09_Gg2
GCFC09_Gg3$p1inp2vec <- is.element(GCFC09_Gg3$player1, player2vector)
GCFC09_Gg3$p2inp1vec <- is.element(GCFC09_Gg3$player2, player1vector)

addPlayer1 <- GCFC09_Gg3[ which(GCFC09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_Gg3[ which(GCFC09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_Gg2 <- rbind(GCFC09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
GCFC09_Gft <- ftable(GCFC09_Gg2$player1, GCFC09_Gg2$player2)
GCFC09_Gft2 <- as.matrix(GCFC09_Gft)
numRows <- nrow(GCFC09_Gft2)
numCols <- ncol(GCFC09_Gft2)
GCFC09_Gft3 <- GCFC09_Gft2[c(2:numRows) , c(2:numCols)]
GCFC09_GTable <- graph.adjacency(GCFC09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(GCFC09_GTable, vertex.label = V(GCFC09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
GCFC09_G.clusterCoef <- transitivity(GCFC09_GTable, type="global") #cluster coefficient
GCFC09_G.degreeCent <- centralization.degree(GCFC09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_Gftn <- as.network.matrix(GCFC09_Gft)
GCFC09_G.netDensity <- network.density(GCFC09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_G.entropy <- entropy(GCFC09_Gft) #entropy

GCFC09_G.netMx <- cbind(GCFC09_G.netMx, GCFC09_G.clusterCoef, GCFC09_G.degreeCent$centralization,
                        GCFC09_G.netDensity, GCFC09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
GCFC09_Bg2 <- data.frame(GCFC09_B)
GCFC09_Bg2 <- GCFC09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_Bg2$player1
player2vector <- GCFC09_Bg2$player2
GCFC09_Bg3 <- GCFC09_Bg2
GCFC09_Bg3$p1inp2vec <- is.element(GCFC09_Bg3$player1, player2vector)
GCFC09_Bg3$p2inp1vec <- is.element(GCFC09_Bg3$player2, player1vector)

addPlayer1 <- GCFC09_Bg3[ which(GCFC09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_Bg3[ which(GCFC09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_Bg2 <- rbind(GCFC09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
GCFC09_Bft <- ftable(GCFC09_Bg2$player1, GCFC09_Bg2$player2)
GCFC09_Bft2 <- as.matrix(GCFC09_Bft)
numRows <- nrow(GCFC09_Bft2)
numCols <- ncol(GCFC09_Bft2)
GCFC09_Bft3 <- GCFC09_Bft2[c(2:numRows) , c(2:numCols)]
GCFC09_BTable <- graph.adjacency(GCFC09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(GCFC09_BTable, vertex.label = V(GCFC09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
GCFC09_B.clusterCoef <- transitivity(GCFC09_BTable, type="global") #cluster coefficient
GCFC09_B.degreeCent <- centralization.degree(GCFC09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_Bftn <- as.network.matrix(GCFC09_Bft)
GCFC09_B.netDensity <- network.density(GCFC09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_B.entropy <- entropy(GCFC09_Bft) #entropy

GCFC09_B.netMx <- cbind(GCFC09_B.netMx, GCFC09_B.clusterCoef, GCFC09_B.degreeCent$centralization,
                        GCFC09_B.netDensity, GCFC09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
GCFC09_SFg2 <- data.frame(GCFC09_SF)
GCFC09_SFg2 <- GCFC09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_SFg2$player1
player2vector <- GCFC09_SFg2$player2
GCFC09_SFg3 <- GCFC09_SFg2
GCFC09_SFg3$p1inp2vec <- is.element(GCFC09_SFg3$player1, player2vector)
GCFC09_SFg3$p2inp1vec <- is.element(GCFC09_SFg3$player2, player1vector)

addPlayer1 <- GCFC09_SFg3[ which(GCFC09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC09_SFg2 <- rbind(GCFC09_SFg2, addPlayer1)

#ROUND 9, FWD Stoppage graph using weighted edges
GCFC09_SFft <- ftable(GCFC09_SFg2$player1, GCFC09_SFg2$player2)
GCFC09_SFft2 <- as.matrix(GCFC09_SFft)
numRows <- nrow(GCFC09_SFft2)
numCols <- ncol(GCFC09_SFft2)
GCFC09_SFft3 <- GCFC09_SFft2[c(2:numRows) , c(1:numCols)]
GCFC09_SFTable <- graph.adjacency(GCFC09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(GCFC09_SFTable, vertex.label = V(GCFC09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
GCFC09_SF.clusterCoef <- transitivity(GCFC09_SFTable, type="global") #cluster coefficient
GCFC09_SF.degreeCent <- centralization.degree(GCFC09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_SFftn <- as.network.matrix(GCFC09_SFft)
GCFC09_SF.netDensity <- network.density(GCFC09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_SF.entropy <- entropy(GCFC09_SFft) #entropy

GCFC09_SF.netMx <- cbind(GCFC09_SF.netMx, GCFC09_SF.clusterCoef, GCFC09_SF.degreeCent$centralization,
                         GCFC09_SF.netDensity, GCFC09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
GCFC09_TFg2 <- data.frame(GCFC09_TF)
GCFC09_TFg2 <- GCFC09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_TFg2$player1
player2vector <- GCFC09_TFg2$player2
GCFC09_TFg3 <- GCFC09_TFg2
GCFC09_TFg3$p1inp2vec <- is.element(GCFC09_TFg3$player1, player2vector)
GCFC09_TFg3$p2inp1vec <- is.element(GCFC09_TFg3$player2, player1vector)

addPlayer1 <- GCFC09_TFg3[ which(GCFC09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_TFg3[ which(GCFC09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_TFg2 <- rbind(GCFC09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
GCFC09_TFft <- ftable(GCFC09_TFg2$player1, GCFC09_TFg2$player2)
GCFC09_TFft2 <- as.matrix(GCFC09_TFft)
numRows <- nrow(GCFC09_TFft2)
numCols <- ncol(GCFC09_TFft2)
GCFC09_TFft3 <- GCFC09_TFft2[c(2:numRows) , c(2:numCols)]
GCFC09_TFTable <- graph.adjacency(GCFC09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(GCFC09_TFTable, vertex.label = V(GCFC09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
GCFC09_TF.clusterCoef <- transitivity(GCFC09_TFTable, type="global") #cluster coefficient
GCFC09_TF.degreeCent <- centralization.degree(GCFC09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_TFftn <- as.network.matrix(GCFC09_TFft)
GCFC09_TF.netDensity <- network.density(GCFC09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_TF.entropy <- entropy(GCFC09_TFft) #entropy

GCFC09_TF.netMx <- cbind(GCFC09_TF.netMx, GCFC09_TF.clusterCoef, GCFC09_TF.degreeCent$centralization,
                         GCFC09_TF.netDensity, GCFC09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
GCFC09_SAMg2 <- data.frame(GCFC09_SAM)
GCFC09_SAMg2 <- GCFC09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_SAMg2$player1
player2vector <- GCFC09_SAMg2$player2
GCFC09_SAMg3 <- GCFC09_SAMg2
GCFC09_SAMg3$p1inp2vec <- is.element(GCFC09_SAMg3$player1, player2vector)
GCFC09_SAMg3$p2inp1vec <- is.element(GCFC09_SAMg3$player2, player1vector)

addPlayer1 <- GCFC09_SAMg3[ which(GCFC09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_SAMg3[ which(GCFC09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_SAMg2 <- rbind(GCFC09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
GCFC09_SAMft <- ftable(GCFC09_SAMg2$player1, GCFC09_SAMg2$player2)
GCFC09_SAMft2 <- as.matrix(GCFC09_SAMft)
numRows <- nrow(GCFC09_SAMft2)
numCols <- ncol(GCFC09_SAMft2)
GCFC09_SAMft3 <- GCFC09_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC09_SAMTable <- graph.adjacency(GCFC09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(GCFC09_SAMTable, vertex.label = V(GCFC09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
GCFC09_SAM.clusterCoef <- transitivity(GCFC09_SAMTable, type="global") #cluster coefficient
GCFC09_SAM.degreeCent <- centralization.degree(GCFC09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_SAMftn <- as.network.matrix(GCFC09_SAMft)
GCFC09_SAM.netDensity <- network.density(GCFC09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_SAM.entropy <- entropy(GCFC09_SAMft) #entropy

GCFC09_SAM.netMx <- cbind(GCFC09_SAM.netMx, GCFC09_SAM.clusterCoef, GCFC09_SAM.degreeCent$centralization,
                          GCFC09_SAM.netDensity, GCFC09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
GCFC09_TAMg2 <- data.frame(GCFC09_TAM)
GCFC09_TAMg2 <- GCFC09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_TAMg2$player1
player2vector <- GCFC09_TAMg2$player2
GCFC09_TAMg3 <- GCFC09_TAMg2
GCFC09_TAMg3$p1inp2vec <- is.element(GCFC09_TAMg3$player1, player2vector)
GCFC09_TAMg3$p2inp1vec <- is.element(GCFC09_TAMg3$player2, player1vector)

addPlayer1 <- GCFC09_TAMg3[ which(GCFC09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_TAMg3[ which(GCFC09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_TAMg2 <- rbind(GCFC09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
GCFC09_TAMft <- ftable(GCFC09_TAMg2$player1, GCFC09_TAMg2$player2)
GCFC09_TAMft2 <- as.matrix(GCFC09_TAMft)
numRows <- nrow(GCFC09_TAMft2)
numCols <- ncol(GCFC09_TAMft2)
GCFC09_TAMft3 <- GCFC09_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC09_TAMTable <- graph.adjacency(GCFC09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(GCFC09_TAMTable, vertex.label = V(GCFC09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
GCFC09_TAM.clusterCoef <- transitivity(GCFC09_TAMTable, type="global") #cluster coefficient
GCFC09_TAM.degreeCent <- centralization.degree(GCFC09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_TAMftn <- as.network.matrix(GCFC09_TAMft)
GCFC09_TAM.netDensity <- network.density(GCFC09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_TAM.entropy <- entropy(GCFC09_TAMft) #entropy

GCFC09_TAM.netMx <- cbind(GCFC09_TAM.netMx, GCFC09_TAM.clusterCoef, GCFC09_TAM.degreeCent$centralization,
                          GCFC09_TAM.netDensity, GCFC09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
GCFC09_SDMg2 <- data.frame(GCFC09_SDM)
GCFC09_SDMg2 <- GCFC09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_SDMg2$player1
player2vector <- GCFC09_SDMg2$player2
GCFC09_SDMg3 <- GCFC09_SDMg2
GCFC09_SDMg3$p1inp2vec <- is.element(GCFC09_SDMg3$player1, player2vector)
GCFC09_SDMg3$p2inp1vec <- is.element(GCFC09_SDMg3$player2, player1vector)

addPlayer1 <- GCFC09_SDMg3[ which(GCFC09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_SDMg3[ which(GCFC09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_SDMg2 <- rbind(GCFC09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
GCFC09_SDMft <- ftable(GCFC09_SDMg2$player1, GCFC09_SDMg2$player2)
GCFC09_SDMft2 <- as.matrix(GCFC09_SDMft)
numRows <- nrow(GCFC09_SDMft2)
numCols <- ncol(GCFC09_SDMft2)
GCFC09_SDMft3 <- GCFC09_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC09_SDMTable <- graph.adjacency(GCFC09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(GCFC09_SDMTable, vertex.label = V(GCFC09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
GCFC09_SDM.clusterCoef <- transitivity(GCFC09_SDMTable, type="global") #cluster coefficient
GCFC09_SDM.degreeCent <- centralization.degree(GCFC09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_SDMftn <- as.network.matrix(GCFC09_SDMft)
GCFC09_SDM.netDensity <- network.density(GCFC09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_SDM.entropy <- entropy(GCFC09_SDMft) #entropy

GCFC09_SDM.netMx <- cbind(GCFC09_SDM.netMx, GCFC09_SDM.clusterCoef, GCFC09_SDM.degreeCent$centralization,
                          GCFC09_SDM.netDensity, GCFC09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
GCFC09_TDMg2 <- data.frame(GCFC09_TDM)
GCFC09_TDMg2 <- GCFC09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_TDMg2$player1
player2vector <- GCFC09_TDMg2$player2
GCFC09_TDMg3 <- GCFC09_TDMg2
GCFC09_TDMg3$p1inp2vec <- is.element(GCFC09_TDMg3$player1, player2vector)
GCFC09_TDMg3$p2inp1vec <- is.element(GCFC09_TDMg3$player2, player1vector)

addPlayer1 <- GCFC09_TDMg3[ which(GCFC09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC09_TDMg3[ which(GCFC09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_TDMg2 <- rbind(GCFC09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
GCFC09_TDMft <- ftable(GCFC09_TDMg2$player1, GCFC09_TDMg2$player2)
GCFC09_TDMft2 <- as.matrix(GCFC09_TDMft)
numRows <- nrow(GCFC09_TDMft2)
numCols <- ncol(GCFC09_TDMft2)
GCFC09_TDMft3 <- GCFC09_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC09_TDMTable <- graph.adjacency(GCFC09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(GCFC09_TDMTable, vertex.label = V(GCFC09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
GCFC09_TDM.clusterCoef <- transitivity(GCFC09_TDMTable, type="global") #cluster coefficient
GCFC09_TDM.degreeCent <- centralization.degree(GCFC09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_TDMftn <- as.network.matrix(GCFC09_TDMft)
GCFC09_TDM.netDensity <- network.density(GCFC09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_TDM.entropy <- entropy(GCFC09_TDMft) #entropy

GCFC09_TDM.netMx <- cbind(GCFC09_TDM.netMx, GCFC09_TDM.clusterCoef, GCFC09_TDM.degreeCent$centralization,
                          GCFC09_TDM.netDensity, GCFC09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
GCFC09_SDg2 <- data.frame(GCFC09_SD)
GCFC09_SDg2 <- GCFC09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_SDg2$player1
player2vector <- GCFC09_SDg2$player2
GCFC09_SDg3 <- GCFC09_SDg2
GCFC09_SDg3$p1inp2vec <- is.element(GCFC09_SDg3$player1, player2vector)
GCFC09_SDg3$p2inp1vec <- is.element(GCFC09_SDg3$player2, player1vector)

addPlayer1 <- GCFC09_SDg3[ which(GCFC09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_SDg3[ which(GCFC09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_SDg2 <- rbind(GCFC09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
GCFC09_SDft <- ftable(GCFC09_SDg2$player1, GCFC09_SDg2$player2)
GCFC09_SDft2 <- as.matrix(GCFC09_SDft)
numRows <- nrow(GCFC09_SDft2)
numCols <- ncol(GCFC09_SDft2)
GCFC09_SDft3 <- GCFC09_SDft2[c(2:numRows) , c(2:numCols)]
GCFC09_SDTable <- graph.adjacency(GCFC09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(GCFC09_SDTable, vertex.label = V(GCFC09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
GCFC09_SD.clusterCoef <- transitivity(GCFC09_SDTable, type="global") #cluster coefficient
GCFC09_SD.degreeCent <- centralization.degree(GCFC09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_SDftn <- as.network.matrix(GCFC09_SDft)
GCFC09_SD.netDensity <- network.density(GCFC09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_SD.entropy <- entropy(GCFC09_SDft) #entropy

GCFC09_SD.netMx <- cbind(GCFC09_SD.netMx, GCFC09_SD.clusterCoef, GCFC09_SD.degreeCent$centralization,
                         GCFC09_SD.netDensity, GCFC09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
GCFC09_TDg2 <- data.frame(GCFC09_TD)
GCFC09_TDg2 <- GCFC09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_TDg2$player1
player2vector <- GCFC09_TDg2$player2
GCFC09_TDg3 <- GCFC09_TDg2
GCFC09_TDg3$p1inp2vec <- is.element(GCFC09_TDg3$player1, player2vector)
GCFC09_TDg3$p2inp1vec <- is.element(GCFC09_TDg3$player2, player1vector)

addPlayer1 <- GCFC09_TDg3[ which(GCFC09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_TDg3[ which(GCFC09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_TDg2 <- rbind(GCFC09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
GCFC09_TDft <- ftable(GCFC09_TDg2$player1, GCFC09_TDg2$player2)
GCFC09_TDft2 <- as.matrix(GCFC09_TDft)
numRows <- nrow(GCFC09_TDft2)
numCols <- ncol(GCFC09_TDft2)
GCFC09_TDft3 <- GCFC09_TDft2[c(2:numRows) , c(2:numCols)]
GCFC09_TDTable <- graph.adjacency(GCFC09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(GCFC09_TDTable, vertex.label = V(GCFC09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
GCFC09_TD.clusterCoef <- transitivity(GCFC09_TDTable, type="global") #cluster coefficient
GCFC09_TD.degreeCent <- centralization.degree(GCFC09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_TDftn <- as.network.matrix(GCFC09_TDft)
GCFC09_TD.netDensity <- network.density(GCFC09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_TD.entropy <- entropy(GCFC09_TDft) #entropy

GCFC09_TD.netMx <- cbind(GCFC09_TD.netMx, GCFC09_TD.clusterCoef, GCFC09_TD.degreeCent$centralization,
                         GCFC09_TD.netDensity, GCFC09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
GCFC09_QTg2 <- data.frame(GCFC09_QT)
GCFC09_QTg2 <- GCFC09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC09_QTg2$player1
player2vector <- GCFC09_QTg2$player2
GCFC09_QTg3 <- GCFC09_QTg2
GCFC09_QTg3$p1inp2vec <- is.element(GCFC09_QTg3$player1, player2vector)
GCFC09_QTg3$p2inp1vec <- is.element(GCFC09_QTg3$player2, player1vector)

addPlayer1 <- GCFC09_QTg3[ which(GCFC09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC09_QTg3[ which(GCFC09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC09_QTg2 <- rbind(GCFC09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
GCFC09_QTft <- ftable(GCFC09_QTg2$player1, GCFC09_QTg2$player2)
GCFC09_QTft2 <- as.matrix(GCFC09_QTft)
numRows <- nrow(GCFC09_QTft2)
numCols <- ncol(GCFC09_QTft2)
GCFC09_QTft3 <- GCFC09_QTft2[c(2:numRows) , c(2:numCols)]
GCFC09_QTTable <- graph.adjacency(GCFC09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(GCFC09_QTTable, vertex.label = V(GCFC09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
GCFC09_QT.clusterCoef <- transitivity(GCFC09_QTTable, type="global") #cluster coefficient
GCFC09_QT.degreeCent <- centralization.degree(GCFC09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC09_QTftn <- as.network.matrix(GCFC09_QTft)
GCFC09_QT.netDensity <- network.density(GCFC09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC09_QT.entropy <- entropy(GCFC09_QTft) #entropy

GCFC09_QT.netMx <- cbind(GCFC09_QT.netMx, GCFC09_QT.clusterCoef, GCFC09_QT.degreeCent$centralization,
                         GCFC09_QT.netDensity, GCFC09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC09_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
GEEL09_Gg2 <- data.frame(GEEL09_G)
GEEL09_Gg2 <- GEEL09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_Gg2$player1
player2vector <- GEEL09_Gg2$player2
GEEL09_Gg3 <- GEEL09_Gg2
GEEL09_Gg3$p1inp2vec <- is.element(GEEL09_Gg3$player1, player2vector)
GEEL09_Gg3$p2inp1vec <- is.element(GEEL09_Gg3$player2, player1vector)

addPlayer1 <- GEEL09_Gg3[ which(GEEL09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_Gg3[ which(GEEL09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_Gg2 <- rbind(GEEL09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
GEEL09_Gft <- ftable(GEEL09_Gg2$player1, GEEL09_Gg2$player2)
GEEL09_Gft2 <- as.matrix(GEEL09_Gft)
numRows <- nrow(GEEL09_Gft2)
numCols <- ncol(GEEL09_Gft2)
GEEL09_Gft3 <- GEEL09_Gft2[c(2:numRows) , c(2:numCols)]
GEEL09_GTable <- graph.adjacency(GEEL09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(GEEL09_GTable, vertex.label = V(GEEL09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
GEEL09_G.clusterCoef <- transitivity(GEEL09_GTable, type="global") #cluster coefficient
GEEL09_G.degreeCent <- centralization.degree(GEEL09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_Gftn <- as.network.matrix(GEEL09_Gft)
GEEL09_G.netDensity <- network.density(GEEL09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_G.entropy <- entropy(GEEL09_Gft) #entropy

GEEL09_G.netMx <- cbind(GEEL09_G.netMx, GEEL09_G.clusterCoef, GEEL09_G.degreeCent$centralization,
                        GEEL09_G.netDensity, GEEL09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
GEEL09_Bg2 <- data.frame(GEEL09_B)
GEEL09_Bg2 <- GEEL09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_Bg2$player1
player2vector <- GEEL09_Bg2$player2
GEEL09_Bg3 <- GEEL09_Bg2
GEEL09_Bg3$p1inp2vec <- is.element(GEEL09_Bg3$player1, player2vector)
GEEL09_Bg3$p2inp1vec <- is.element(GEEL09_Bg3$player2, player1vector)

addPlayer1 <- GEEL09_Bg3[ which(GEEL09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL09_Bg3[ which(GEEL09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_Bg2 <- rbind(GEEL09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
GEEL09_Bft <- ftable(GEEL09_Bg2$player1, GEEL09_Bg2$player2)
GEEL09_Bft2 <- as.matrix(GEEL09_Bft)
numRows <- nrow(GEEL09_Bft2)
numCols <- ncol(GEEL09_Bft2)
GEEL09_Bft3 <- GEEL09_Bft2[c(2:numRows) , c(2:numCols)]
GEEL09_BTable <- graph.adjacency(GEEL09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(GEEL09_BTable, vertex.label = V(GEEL09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
GEEL09_B.clusterCoef <- transitivity(GEEL09_BTable, type="global") #cluster coefficient
GEEL09_B.degreeCent <- centralization.degree(GEEL09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_Bftn <- as.network.matrix(GEEL09_Bft)
GEEL09_B.netDensity <- network.density(GEEL09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_B.entropy <- entropy(GEEL09_Bft) #entropy

GEEL09_B.netMx <- cbind(GEEL09_B.netMx, GEEL09_B.clusterCoef, GEEL09_B.degreeCent$centralization,
                        GEEL09_B.netDensity, GEEL09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
GEEL09_SFg2 <- data.frame(GEEL09_SF)
GEEL09_SFg2 <- GEEL09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_SFg2$player1
player2vector <- GEEL09_SFg2$player2
GEEL09_SFg3 <- GEEL09_SFg2
GEEL09_SFg3$p1inp2vec <- is.element(GEEL09_SFg3$player1, player2vector)
GEEL09_SFg3$p2inp1vec <- is.element(GEEL09_SFg3$player2, player1vector)

addPlayer1 <- GEEL09_SFg3[ which(GEEL09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_SFg3[ which(GEEL09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_SFg2 <- rbind(GEEL09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
GEEL09_SFft <- ftable(GEEL09_SFg2$player1, GEEL09_SFg2$player2)
GEEL09_SFft2 <- as.matrix(GEEL09_SFft)
numRows <- nrow(GEEL09_SFft2)
numCols <- ncol(GEEL09_SFft2)
GEEL09_SFft3 <- GEEL09_SFft2[c(2:numRows) , c(2:numCols)]
GEEL09_SFTable <- graph.adjacency(GEEL09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(GEEL09_SFTable, vertex.label = V(GEEL09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
GEEL09_SF.clusterCoef <- transitivity(GEEL09_SFTable, type="global") #cluster coefficient
GEEL09_SF.degreeCent <- centralization.degree(GEEL09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_SFftn <- as.network.matrix(GEEL09_SFft)
GEEL09_SF.netDensity <- network.density(GEEL09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_SF.entropy <- entropy(GEEL09_SFft) #entropy

GEEL09_SF.netMx <- cbind(GEEL09_SF.netMx, GEEL09_SF.clusterCoef, GEEL09_SF.degreeCent$centralization,
                         GEEL09_SF.netDensity, GEEL09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
GEEL09_TFg2 <- data.frame(GEEL09_TF)
GEEL09_TFg2 <- GEEL09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_TFg2$player1
player2vector <- GEEL09_TFg2$player2
GEEL09_TFg3 <- GEEL09_TFg2
GEEL09_TFg3$p1inp2vec <- is.element(GEEL09_TFg3$player1, player2vector)
GEEL09_TFg3$p2inp1vec <- is.element(GEEL09_TFg3$player2, player1vector)

addPlayer1 <- GEEL09_TFg3[ which(GEEL09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_TFg3[ which(GEEL09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_TFg2 <- rbind(GEEL09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
GEEL09_TFft <- ftable(GEEL09_TFg2$player1, GEEL09_TFg2$player2)
GEEL09_TFft2 <- as.matrix(GEEL09_TFft)
numRows <- nrow(GEEL09_TFft2)
numCols <- ncol(GEEL09_TFft2)
GEEL09_TFft3 <- GEEL09_TFft2[c(2:numRows) , c(2:numCols)]
GEEL09_TFTable <- graph.adjacency(GEEL09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(GEEL09_TFTable, vertex.label = V(GEEL09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
GEEL09_TF.clusterCoef <- transitivity(GEEL09_TFTable, type="global") #cluster coefficient
GEEL09_TF.degreeCent <- centralization.degree(GEEL09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_TFftn <- as.network.matrix(GEEL09_TFft)
GEEL09_TF.netDensity <- network.density(GEEL09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_TF.entropy <- entropy(GEEL09_TFft) #entropy

GEEL09_TF.netMx <- cbind(GEEL09_TF.netMx, GEEL09_TF.clusterCoef, GEEL09_TF.degreeCent$centralization,
                         GEEL09_TF.netDensity, GEEL09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
GEEL09_SAMg2 <- data.frame(GEEL09_SAM)
GEEL09_SAMg2 <- GEEL09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_SAMg2$player1
player2vector <- GEEL09_SAMg2$player2
GEEL09_SAMg3 <- GEEL09_SAMg2
GEEL09_SAMg3$p1inp2vec <- is.element(GEEL09_SAMg3$player1, player2vector)
GEEL09_SAMg3$p2inp1vec <- is.element(GEEL09_SAMg3$player2, player1vector)

addPlayer1 <- GEEL09_SAMg3[ which(GEEL09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_SAMg3[ which(GEEL09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_SAMg2 <- rbind(GEEL09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
GEEL09_SAMft <- ftable(GEEL09_SAMg2$player1, GEEL09_SAMg2$player2)
GEEL09_SAMft2 <- as.matrix(GEEL09_SAMft)
numRows <- nrow(GEEL09_SAMft2)
numCols <- ncol(GEEL09_SAMft2)
GEEL09_SAMft3 <- GEEL09_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL09_SAMTable <- graph.adjacency(GEEL09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(GEEL09_SAMTable, vertex.label = V(GEEL09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
GEEL09_SAM.clusterCoef <- transitivity(GEEL09_SAMTable, type="global") #cluster coefficient
GEEL09_SAM.degreeCent <- centralization.degree(GEEL09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_SAMftn <- as.network.matrix(GEEL09_SAMft)
GEEL09_SAM.netDensity <- network.density(GEEL09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_SAM.entropy <- entropy(GEEL09_SAMft) #entropy

GEEL09_SAM.netMx <- cbind(GEEL09_SAM.netMx, GEEL09_SAM.clusterCoef, GEEL09_SAM.degreeCent$centralization,
                          GEEL09_SAM.netDensity, GEEL09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
GEEL09_TAMg2 <- data.frame(GEEL09_TAM)
GEEL09_TAMg2 <- GEEL09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_TAMg2$player1
player2vector <- GEEL09_TAMg2$player2
GEEL09_TAMg3 <- GEEL09_TAMg2
GEEL09_TAMg3$p1inp2vec <- is.element(GEEL09_TAMg3$player1, player2vector)
GEEL09_TAMg3$p2inp1vec <- is.element(GEEL09_TAMg3$player2, player1vector)

addPlayer1 <- GEEL09_TAMg3[ which(GEEL09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_TAMg3[ which(GEEL09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_TAMg2 <- rbind(GEEL09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
GEEL09_TAMft <- ftable(GEEL09_TAMg2$player1, GEEL09_TAMg2$player2)
GEEL09_TAMft2 <- as.matrix(GEEL09_TAMft)
numRows <- nrow(GEEL09_TAMft2)
numCols <- ncol(GEEL09_TAMft2)
GEEL09_TAMft3 <- GEEL09_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL09_TAMTable <- graph.adjacency(GEEL09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(GEEL09_TAMTable, vertex.label = V(GEEL09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
GEEL09_TAM.clusterCoef <- transitivity(GEEL09_TAMTable, type="global") #cluster coefficient
GEEL09_TAM.degreeCent <- centralization.degree(GEEL09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_TAMftn <- as.network.matrix(GEEL09_TAMft)
GEEL09_TAM.netDensity <- network.density(GEEL09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_TAM.entropy <- entropy(GEEL09_TAMft) #entropy

GEEL09_TAM.netMx <- cbind(GEEL09_TAM.netMx, GEEL09_TAM.clusterCoef, GEEL09_TAM.degreeCent$centralization,
                          GEEL09_TAM.netDensity, GEEL09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
GEEL09_SDMg2 <- data.frame(GEEL09_SDM)
GEEL09_SDMg2 <- GEEL09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_SDMg2$player1
player2vector <- GEEL09_SDMg2$player2
GEEL09_SDMg3 <- GEEL09_SDMg2
GEEL09_SDMg3$p1inp2vec <- is.element(GEEL09_SDMg3$player1, player2vector)
GEEL09_SDMg3$p2inp1vec <- is.element(GEEL09_SDMg3$player2, player1vector)

addPlayer1 <- GEEL09_SDMg3[ which(GEEL09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_SDMg3[ which(GEEL09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_SDMg2 <- rbind(GEEL09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
GEEL09_SDMft <- ftable(GEEL09_SDMg2$player1, GEEL09_SDMg2$player2)
GEEL09_SDMft2 <- as.matrix(GEEL09_SDMft)
numRows <- nrow(GEEL09_SDMft2)
numCols <- ncol(GEEL09_SDMft2)
GEEL09_SDMft3 <- GEEL09_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL09_SDMTable <- graph.adjacency(GEEL09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(GEEL09_SDMTable, vertex.label = V(GEEL09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
GEEL09_SDM.clusterCoef <- transitivity(GEEL09_SDMTable, type="global") #cluster coefficient
GEEL09_SDM.degreeCent <- centralization.degree(GEEL09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_SDMftn <- as.network.matrix(GEEL09_SDMft)
GEEL09_SDM.netDensity <- network.density(GEEL09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_SDM.entropy <- entropy(GEEL09_SDMft) #entropy

GEEL09_SDM.netMx <- cbind(GEEL09_SDM.netMx, GEEL09_SDM.clusterCoef, GEEL09_SDM.degreeCent$centralization,
                          GEEL09_SDM.netDensity, GEEL09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
GEEL09_TDMg2 <- data.frame(GEEL09_TDM)
GEEL09_TDMg2 <- GEEL09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_TDMg2$player1
player2vector <- GEEL09_TDMg2$player2
GEEL09_TDMg3 <- GEEL09_TDMg2
GEEL09_TDMg3$p1inp2vec <- is.element(GEEL09_TDMg3$player1, player2vector)
GEEL09_TDMg3$p2inp1vec <- is.element(GEEL09_TDMg3$player2, player1vector)

addPlayer1 <- GEEL09_TDMg3[ which(GEEL09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_TDMg3[ which(GEEL09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_TDMg2 <- rbind(GEEL09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
GEEL09_TDMft <- ftable(GEEL09_TDMg2$player1, GEEL09_TDMg2$player2)
GEEL09_TDMft2 <- as.matrix(GEEL09_TDMft)
numRows <- nrow(GEEL09_TDMft2)
numCols <- ncol(GEEL09_TDMft2)
GEEL09_TDMft3 <- GEEL09_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL09_TDMTable <- graph.adjacency(GEEL09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(GEEL09_TDMTable, vertex.label = V(GEEL09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
GEEL09_TDM.clusterCoef <- transitivity(GEEL09_TDMTable, type="global") #cluster coefficient
GEEL09_TDM.degreeCent <- centralization.degree(GEEL09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_TDMftn <- as.network.matrix(GEEL09_TDMft)
GEEL09_TDM.netDensity <- network.density(GEEL09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_TDM.entropy <- entropy(GEEL09_TDMft) #entropy

GEEL09_TDM.netMx <- cbind(GEEL09_TDM.netMx, GEEL09_TDM.clusterCoef, GEEL09_TDM.degreeCent$centralization,
                          GEEL09_TDM.netDensity, GEEL09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************

round = 9
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
GEEL09_SDg2 <- data.frame(GEEL09_SD)
GEEL09_SDg2 <- GEEL09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_SDg2$player1
player2vector <- GEEL09_SDg2$player2
GEEL09_SDg3 <- GEEL09_SDg2
GEEL09_SDg3$p1inp2vec <- is.element(GEEL09_SDg3$player1, player2vector)
GEEL09_SDg3$p2inp1vec <- is.element(GEEL09_SDg3$player2, player1vector)

addPlayer1 <- GEEL09_SDg3[ which(GEEL09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_SDg3[ which(GEEL09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_SDg2 <- rbind(GEEL09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
GEEL09_SDft <- ftable(GEEL09_SDg2$player1, GEEL09_SDg2$player2)
GEEL09_SDft2 <- as.matrix(GEEL09_SDft)
numRows <- nrow(GEEL09_SDft2)
numCols <- ncol(GEEL09_SDft2)
GEEL09_SDft3 <- GEEL09_SDft2[c(2:numRows) , c(2:numCols)]
GEEL09_SDTable <- graph.adjacency(GEEL09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(GEEL09_SDTable, vertex.label = V(GEEL09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
GEEL09_SD.clusterCoef <- transitivity(GEEL09_SDTable, type="global") #cluster coefficient
GEEL09_SD.degreeCent <- centralization.degree(GEEL09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_SDftn <- as.network.matrix(GEEL09_SDft)
GEEL09_SD.netDensity <- network.density(GEEL09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_SD.entropy <- entropy(GEEL09_SDft) #entropy

GEEL09_SD.netMx <- cbind(GEEL09_SD.netMx, GEEL09_SD.clusterCoef, GEEL09_SD.degreeCent$centralization,
                         GEEL09_SD.netDensity, GEEL09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
GEEL09_TDg2 <- data.frame(GEEL09_TD)
GEEL09_TDg2 <- GEEL09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_TDg2$player1
player2vector <- GEEL09_TDg2$player2
GEEL09_TDg3 <- GEEL09_TDg2
GEEL09_TDg3$p1inp2vec <- is.element(GEEL09_TDg3$player1, player2vector)
GEEL09_TDg3$p2inp1vec <- is.element(GEEL09_TDg3$player2, player1vector)

addPlayer1 <- GEEL09_TDg3[ which(GEEL09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_TDg3[ which(GEEL09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_TDg2 <- rbind(GEEL09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
GEEL09_TDft <- ftable(GEEL09_TDg2$player1, GEEL09_TDg2$player2)
GEEL09_TDft2 <- as.matrix(GEEL09_TDft)
numRows <- nrow(GEEL09_TDft2)
numCols <- ncol(GEEL09_TDft2)
GEEL09_TDft3 <- GEEL09_TDft2[c(2:numRows) , c(2:numCols)]
GEEL09_TDTable <- graph.adjacency(GEEL09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(GEEL09_TDTable, vertex.label = V(GEEL09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
GEEL09_TD.clusterCoef <- transitivity(GEEL09_TDTable, type="global") #cluster coefficient
GEEL09_TD.degreeCent <- centralization.degree(GEEL09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_TDftn <- as.network.matrix(GEEL09_TDft)
GEEL09_TD.netDensity <- network.density(GEEL09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_TD.entropy <- entropy(GEEL09_TDft) #entropy

GEEL09_TD.netMx <- cbind(GEEL09_TD.netMx, GEEL09_TD.clusterCoef, GEEL09_TD.degreeCent$centralization,
                         GEEL09_TD.netDensity, GEEL09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
GEEL09_QTg2 <- data.frame(GEEL09_QT)
GEEL09_QTg2 <- GEEL09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL09_QTg2$player1
player2vector <- GEEL09_QTg2$player2
GEEL09_QTg3 <- GEEL09_QTg2
GEEL09_QTg3$p1inp2vec <- is.element(GEEL09_QTg3$player1, player2vector)
GEEL09_QTg3$p2inp1vec <- is.element(GEEL09_QTg3$player2, player1vector)

addPlayer1 <- GEEL09_QTg3[ which(GEEL09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL09_QTg3[ which(GEEL09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL09_QTg2 <- rbind(GEEL09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
GEEL09_QTft <- ftable(GEEL09_QTg2$player1, GEEL09_QTg2$player2)
GEEL09_QTft2 <- as.matrix(GEEL09_QTft)
numRows <- nrow(GEEL09_QTft2)
numCols <- ncol(GEEL09_QTft2)
GEEL09_QTft3 <- GEEL09_QTft2[c(2:numRows) , c(2:numCols)]
GEEL09_QTTable <- graph.adjacency(GEEL09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(GEEL09_QTTable, vertex.label = V(GEEL09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
GEEL09_QT.clusterCoef <- transitivity(GEEL09_QTTable, type="global") #cluster coefficient
GEEL09_QT.degreeCent <- centralization.degree(GEEL09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL09_QTftn <- as.network.matrix(GEEL09_QTft)
GEEL09_QT.netDensity <- network.density(GEEL09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL09_QT.entropy <- entropy(GEEL09_QTft) #entropy

GEEL09_QT.netMx <- cbind(GEEL09_QT.netMx, GEEL09_QT.clusterCoef, GEEL09_QT.degreeCent$centralization,
                         GEEL09_QT.netDensity, GEEL09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL09_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "GWS"
KIoutcome = "Goal_F"
GWS09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
GWS09_Gg2 <- data.frame(GWS09_G)
GWS09_Gg2 <- GWS09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_Gg2$player1
player2vector <- GWS09_Gg2$player2
GWS09_Gg3 <- GWS09_Gg2
GWS09_Gg3$p1inp2vec <- is.element(GWS09_Gg3$player1, player2vector)
GWS09_Gg3$p2inp1vec <- is.element(GWS09_Gg3$player2, player1vector)

addPlayer1 <- GWS09_Gg3[ which(GWS09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_Gg3[ which(GWS09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_Gg2 <- rbind(GWS09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
GWS09_Gft <- ftable(GWS09_Gg2$player1, GWS09_Gg2$player2)
GWS09_Gft2 <- as.matrix(GWS09_Gft)
numRows <- nrow(GWS09_Gft2)
numCols <- ncol(GWS09_Gft2)
GWS09_Gft3 <- GWS09_Gft2[c(1:numRows) , c(1:numCols)]
GWS09_GTable <- graph.adjacency(GWS09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(GWS09_GTable, vertex.label = V(GWS09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
GWS09_G.clusterCoef <- transitivity(GWS09_GTable, type="global") #cluster coefficient
GWS09_G.degreeCent <- centralization.degree(GWS09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_Gftn <- as.network.matrix(GWS09_Gft)
GWS09_G.netDensity <- network.density(GWS09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_G.entropy <- entropy(GWS09_Gft) #entropy

GWS09_G.netMx <- cbind(GWS09_G.netMx, GWS09_G.clusterCoef, GWS09_G.degreeCent$centralization,
                       GWS09_G.netDensity, GWS09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "GWS"
KIoutcome = "Behind_F"
GWS09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
GWS09_Bg2 <- data.frame(GWS09_B)
GWS09_Bg2 <- GWS09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_Bg2$player1
player2vector <- GWS09_Bg2$player2
GWS09_Bg3 <- GWS09_Bg2
GWS09_Bg3$p1inp2vec <- is.element(GWS09_Bg3$player1, player2vector)
GWS09_Bg3$p2inp1vec <- is.element(GWS09_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS09_Bg3[ which(GWS09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS09_Bg2 <- rbind(GWS09_Bg2, addPlayer2)

#ROUND 9, Behind graph using weighted edges
GWS09_Bft <- ftable(GWS09_Bg2$player1, GWS09_Bg2$player2)
GWS09_Bft2 <- as.matrix(GWS09_Bft)
numRows <- nrow(GWS09_Bft2)
numCols <- ncol(GWS09_Bft2)
GWS09_Bft3 <- GWS09_Bft2[c(1:numRows) , c(2:numCols)]
GWS09_BTable <- graph.adjacency(GWS09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(GWS09_BTable, vertex.label = V(GWS09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
GWS09_B.clusterCoef <- transitivity(GWS09_BTable, type="global") #cluster coefficient
GWS09_B.degreeCent <- centralization.degree(GWS09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_Bftn <- as.network.matrix(GWS09_Bft)
GWS09_B.netDensity <- network.density(GWS09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_B.entropy <- entropy(GWS09_Bft) #entropy

GWS09_B.netMx <- cbind(GWS09_B.netMx, GWS09_B.clusterCoef, GWS09_B.degreeCent$centralization,
                       GWS09_B.netDensity, GWS09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
GWS09_SFg2 <- data.frame(GWS09_SF)
GWS09_SFg2 <- GWS09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_SFg2$player1
player2vector <- GWS09_SFg2$player2
GWS09_SFg3 <- GWS09_SFg2
GWS09_SFg3$p1inp2vec <- is.element(GWS09_SFg3$player1, player2vector)
GWS09_SFg3$p2inp1vec <- is.element(GWS09_SFg3$player2, player1vector)

addPlayer1 <- GWS09_SFg3[ which(GWS09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_SFg3[ which(GWS09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_SFg2 <- rbind(GWS09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
GWS09_SFft <- ftable(GWS09_SFg2$player1, GWS09_SFg2$player2)
GWS09_SFft2 <- as.matrix(GWS09_SFft)
numRows <- nrow(GWS09_SFft2)
numCols <- ncol(GWS09_SFft2)
GWS09_SFft3 <- GWS09_SFft2[c(2:numRows) , c(2:numCols)]
GWS09_SFTable <- graph.adjacency(GWS09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(GWS09_SFTable, vertex.label = V(GWS09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
GWS09_SF.clusterCoef <- transitivity(GWS09_SFTable, type="global") #cluster coefficient
GWS09_SF.degreeCent <- centralization.degree(GWS09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_SFftn <- as.network.matrix(GWS09_SFft)
GWS09_SF.netDensity <- network.density(GWS09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_SF.entropy <- entropy(GWS09_SFft) #entropy

GWS09_SF.netMx <- cbind(GWS09_SF.netMx, GWS09_SF.clusterCoef, GWS09_SF.degreeCent$centralization,
                        GWS09_SF.netDensity, GWS09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
GWS09_TFg2 <- data.frame(GWS09_TF)
GWS09_TFg2 <- GWS09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_TFg2$player1
player2vector <- GWS09_TFg2$player2
GWS09_TFg3 <- GWS09_TFg2
GWS09_TFg3$p1inp2vec <- is.element(GWS09_TFg3$player1, player2vector)
GWS09_TFg3$p2inp1vec <- is.element(GWS09_TFg3$player2, player1vector)

addPlayer1 <- GWS09_TFg3[ which(GWS09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_TFg3[ which(GWS09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_TFg2 <- rbind(GWS09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
GWS09_TFft <- ftable(GWS09_TFg2$player1, GWS09_TFg2$player2)
GWS09_TFft2 <- as.matrix(GWS09_TFft)
numRows <- nrow(GWS09_TFft2)
numCols <- ncol(GWS09_TFft2)
GWS09_TFft3 <- GWS09_TFft2[c(2:numRows) , c(2:numCols)]
GWS09_TFTable <- graph.adjacency(GWS09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(GWS09_TFTable, vertex.label = V(GWS09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
GWS09_TF.clusterCoef <- transitivity(GWS09_TFTable, type="global") #cluster coefficient
GWS09_TF.degreeCent <- centralization.degree(GWS09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_TFftn <- as.network.matrix(GWS09_TFft)
GWS09_TF.netDensity <- network.density(GWS09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_TF.entropy <- entropy(GWS09_TFft) #entropy

GWS09_TF.netMx <- cbind(GWS09_TF.netMx, GWS09_TF.clusterCoef, GWS09_TF.degreeCent$centralization,
                        GWS09_TF.netDensity, GWS09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
GWS09_SAMg2 <- data.frame(GWS09_SAM)
GWS09_SAMg2 <- GWS09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_SAMg2$player1
player2vector <- GWS09_SAMg2$player2
GWS09_SAMg3 <- GWS09_SAMg2
GWS09_SAMg3$p1inp2vec <- is.element(GWS09_SAMg3$player1, player2vector)
GWS09_SAMg3$p2inp1vec <- is.element(GWS09_SAMg3$player2, player1vector)

addPlayer1 <- GWS09_SAMg3[ which(GWS09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_SAMg3[ which(GWS09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_SAMg2 <- rbind(GWS09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
GWS09_SAMft <- ftable(GWS09_SAMg2$player1, GWS09_SAMg2$player2)
GWS09_SAMft2 <- as.matrix(GWS09_SAMft)
numRows <- nrow(GWS09_SAMft2)
numCols <- ncol(GWS09_SAMft2)
GWS09_SAMft3 <- GWS09_SAMft2[c(2:numRows) , c(2:numCols)]
GWS09_SAMTable <- graph.adjacency(GWS09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(GWS09_SAMTable, vertex.label = V(GWS09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
GWS09_SAM.clusterCoef <- transitivity(GWS09_SAMTable, type="global") #cluster coefficient
GWS09_SAM.degreeCent <- centralization.degree(GWS09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_SAMftn <- as.network.matrix(GWS09_SAMft)
GWS09_SAM.netDensity <- network.density(GWS09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_SAM.entropy <- entropy(GWS09_SAMft) #entropy

GWS09_SAM.netMx <- cbind(GWS09_SAM.netMx, GWS09_SAM.clusterCoef, GWS09_SAM.degreeCent$centralization,
                         GWS09_SAM.netDensity, GWS09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
GWS09_TAMg2 <- data.frame(GWS09_TAM)
GWS09_TAMg2 <- GWS09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_TAMg2$player1
player2vector <- GWS09_TAMg2$player2
GWS09_TAMg3 <- GWS09_TAMg2
GWS09_TAMg3$p1inp2vec <- is.element(GWS09_TAMg3$player1, player2vector)
GWS09_TAMg3$p2inp1vec <- is.element(GWS09_TAMg3$player2, player1vector)

addPlayer1 <- GWS09_TAMg3[ which(GWS09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_TAMg3[ which(GWS09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_TAMg2 <- rbind(GWS09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
GWS09_TAMft <- ftable(GWS09_TAMg2$player1, GWS09_TAMg2$player2)
GWS09_TAMft2 <- as.matrix(GWS09_TAMft)
numRows <- nrow(GWS09_TAMft2)
numCols <- ncol(GWS09_TAMft2)
GWS09_TAMft3 <- GWS09_TAMft2[c(2:numRows) , c(2:numCols)]
GWS09_TAMTable <- graph.adjacency(GWS09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(GWS09_TAMTable, vertex.label = V(GWS09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
GWS09_TAM.clusterCoef <- transitivity(GWS09_TAMTable, type="global") #cluster coefficient
GWS09_TAM.degreeCent <- centralization.degree(GWS09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_TAMftn <- as.network.matrix(GWS09_TAMft)
GWS09_TAM.netDensity <- network.density(GWS09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_TAM.entropy <- entropy(GWS09_TAMft) #entropy

GWS09_TAM.netMx <- cbind(GWS09_TAM.netMx, GWS09_TAM.clusterCoef, GWS09_TAM.degreeCent$centralization,
                         GWS09_TAM.netDensity, GWS09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
GWS09_SDMg2 <- data.frame(GWS09_SDM)
GWS09_SDMg2 <- GWS09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_SDMg2$player1
player2vector <- GWS09_SDMg2$player2
GWS09_SDMg3 <- GWS09_SDMg2
GWS09_SDMg3$p1inp2vec <- is.element(GWS09_SDMg3$player1, player2vector)
GWS09_SDMg3$p2inp1vec <- is.element(GWS09_SDMg3$player2, player1vector)

addPlayer1 <- GWS09_SDMg3[ which(GWS09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_SDMg3[ which(GWS09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_SDMg2 <- rbind(GWS09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
GWS09_SDMft <- ftable(GWS09_SDMg2$player1, GWS09_SDMg2$player2)
GWS09_SDMft2 <- as.matrix(GWS09_SDMft)
numRows <- nrow(GWS09_SDMft2)
numCols <- ncol(GWS09_SDMft2)
GWS09_SDMft3 <- GWS09_SDMft2[c(2:numRows) , c(2:numCols)]
GWS09_SDMTable <- graph.adjacency(GWS09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(GWS09_SDMTable, vertex.label = V(GWS09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
GWS09_SDM.clusterCoef <- transitivity(GWS09_SDMTable, type="global") #cluster coefficient
GWS09_SDM.degreeCent <- centralization.degree(GWS09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_SDMftn <- as.network.matrix(GWS09_SDMft)
GWS09_SDM.netDensity <- network.density(GWS09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_SDM.entropy <- entropy(GWS09_SDMft) #entropy

GWS09_SDM.netMx <- cbind(GWS09_SDM.netMx, GWS09_SDM.clusterCoef, GWS09_SDM.degreeCent$centralization,
                         GWS09_SDM.netDensity, GWS09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
GWS09_TDMg2 <- data.frame(GWS09_TDM)
GWS09_TDMg2 <- GWS09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_TDMg2$player1
player2vector <- GWS09_TDMg2$player2
GWS09_TDMg3 <- GWS09_TDMg2
GWS09_TDMg3$p1inp2vec <- is.element(GWS09_TDMg3$player1, player2vector)
GWS09_TDMg3$p2inp1vec <- is.element(GWS09_TDMg3$player2, player1vector)

addPlayer1 <- GWS09_TDMg3[ which(GWS09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_TDMg3[ which(GWS09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_TDMg2 <- rbind(GWS09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
GWS09_TDMft <- ftable(GWS09_TDMg2$player1, GWS09_TDMg2$player2)
GWS09_TDMft2 <- as.matrix(GWS09_TDMft)
numRows <- nrow(GWS09_TDMft2)
numCols <- ncol(GWS09_TDMft2)
GWS09_TDMft3 <- GWS09_TDMft2[c(2:numRows) , c(2:numCols)]
GWS09_TDMTable <- graph.adjacency(GWS09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(GWS09_TDMTable, vertex.label = V(GWS09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
GWS09_TDM.clusterCoef <- transitivity(GWS09_TDMTable, type="global") #cluster coefficient
GWS09_TDM.degreeCent <- centralization.degree(GWS09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_TDMftn <- as.network.matrix(GWS09_TDMft)
GWS09_TDM.netDensity <- network.density(GWS09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_TDM.entropy <- entropy(GWS09_TDMft) #entropy

GWS09_TDM.netMx <- cbind(GWS09_TDM.netMx, GWS09_TDM.clusterCoef, GWS09_TDM.degreeCent$centralization,
                         GWS09_TDM.netDensity, GWS09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************

round = 9
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
GWS09_SDg2 <- data.frame(GWS09_SD)
GWS09_SDg2 <- GWS09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_SDg2$player1
player2vector <- GWS09_SDg2$player2
GWS09_SDg3 <- GWS09_SDg2
GWS09_SDg3$p1inp2vec <- is.element(GWS09_SDg3$player1, player2vector)
GWS09_SDg3$p2inp1vec <- is.element(GWS09_SDg3$player2, player1vector)

addPlayer1 <- GWS09_SDg3[ which(GWS09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_SDg3[ which(GWS09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_SDg2 <- rbind(GWS09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
GWS09_SDft <- ftable(GWS09_SDg2$player1, GWS09_SDg2$player2)
GWS09_SDft2 <- as.matrix(GWS09_SDft)
numRows <- nrow(GWS09_SDft2)
numCols <- ncol(GWS09_SDft2)
GWS09_SDft3 <- GWS09_SDft2[c(2:numRows) , c(2:numCols)]
GWS09_SDTable <- graph.adjacency(GWS09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(GWS09_SDTable, vertex.label = V(GWS09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
GWS09_SD.clusterCoef <- transitivity(GWS09_SDTable, type="global") #cluster coefficient
GWS09_SD.degreeCent <- centralization.degree(GWS09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_SDftn <- as.network.matrix(GWS09_SDft)
GWS09_SD.netDensity <- network.density(GWS09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_SD.entropy <- entropy(GWS09_SDft) #entropy

GWS09_SD.netMx <- cbind(GWS09_SD.netMx, GWS09_SD.clusterCoef, GWS09_SD.degreeCent$centralization,
                        GWS09_SD.netDensity, GWS09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
GWS09_TDg2 <- data.frame(GWS09_TD)
GWS09_TDg2 <- GWS09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_TDg2$player1
player2vector <- GWS09_TDg2$player2
GWS09_TDg3 <- GWS09_TDg2
GWS09_TDg3$p1inp2vec <- is.element(GWS09_TDg3$player1, player2vector)
GWS09_TDg3$p2inp1vec <- is.element(GWS09_TDg3$player2, player1vector)

addPlayer1 <- GWS09_TDg3[ which(GWS09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_TDg3[ which(GWS09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_TDg2 <- rbind(GWS09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
GWS09_TDft <- ftable(GWS09_TDg2$player1, GWS09_TDg2$player2)
GWS09_TDft2 <- as.matrix(GWS09_TDft)
numRows <- nrow(GWS09_TDft2)
numCols <- ncol(GWS09_TDft2)
GWS09_TDft3 <- GWS09_TDft2[c(2:numRows) , c(2:numCols)]
GWS09_TDTable <- graph.adjacency(GWS09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(GWS09_TDTable, vertex.label = V(GWS09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
GWS09_TD.clusterCoef <- transitivity(GWS09_TDTable, type="global") #cluster coefficient
GWS09_TD.degreeCent <- centralization.degree(GWS09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_TDftn <- as.network.matrix(GWS09_TDft)
GWS09_TD.netDensity <- network.density(GWS09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_TD.entropy <- entropy(GWS09_TDft) #entropy

GWS09_TD.netMx <- cbind(GWS09_TD.netMx, GWS09_TD.clusterCoef, GWS09_TD.degreeCent$centralization,
                        GWS09_TD.netDensity, GWS09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
GWS09_QTg2 <- data.frame(GWS09_QT)
GWS09_QTg2 <- GWS09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS09_QTg2$player1
player2vector <- GWS09_QTg2$player2
GWS09_QTg3 <- GWS09_QTg2
GWS09_QTg3$p1inp2vec <- is.element(GWS09_QTg3$player1, player2vector)
GWS09_QTg3$p2inp1vec <- is.element(GWS09_QTg3$player2, player1vector)

addPlayer1 <- GWS09_QTg3[ which(GWS09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS09_QTg3[ which(GWS09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS09_QTg2 <- rbind(GWS09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
GWS09_QTft <- ftable(GWS09_QTg2$player1, GWS09_QTg2$player2)
GWS09_QTft2 <- as.matrix(GWS09_QTft)
numRows <- nrow(GWS09_QTft2)
numCols <- ncol(GWS09_QTft2)
GWS09_QTft3 <- GWS09_QTft2[c(2:numRows) , c(2:numCols)]
GWS09_QTTable <- graph.adjacency(GWS09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(GWS09_QTTable, vertex.label = V(GWS09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
GWS09_QT.clusterCoef <- transitivity(GWS09_QTTable, type="global") #cluster coefficient
GWS09_QT.degreeCent <- centralization.degree(GWS09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS09_QTftn <- as.network.matrix(GWS09_QTft)
GWS09_QT.netDensity <- network.density(GWS09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS09_QT.entropy <- entropy(GWS09_QTft) #entropy

GWS09_QT.netMx <- cbind(GWS09_QT.netMx, GWS09_QT.clusterCoef, GWS09_QT.degreeCent$centralization,
                        GWS09_QT.netDensity, GWS09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS09_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "HAW"
KIoutcome = "Goal_F"
HAW09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
HAW09_Gg2 <- data.frame(HAW09_G)
HAW09_Gg2 <- HAW09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_Gg2$player1
player2vector <- HAW09_Gg2$player2
HAW09_Gg3 <- HAW09_Gg2
HAW09_Gg3$p1inp2vec <- is.element(HAW09_Gg3$player1, player2vector)
HAW09_Gg3$p2inp1vec <- is.element(HAW09_Gg3$player2, player1vector)

addPlayer1 <- HAW09_Gg3[ which(HAW09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_Gg3[ which(HAW09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_Gg2 <- rbind(HAW09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
HAW09_Gft <- ftable(HAW09_Gg2$player1, HAW09_Gg2$player2)
HAW09_Gft2 <- as.matrix(HAW09_Gft)
numRows <- nrow(HAW09_Gft2)
numCols <- ncol(HAW09_Gft2)
HAW09_Gft3 <- HAW09_Gft2[c(2:numRows) , c(2:numCols)]
HAW09_GTable <- graph.adjacency(HAW09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(HAW09_GTable, vertex.label = V(HAW09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
HAW09_G.clusterCoef <- transitivity(HAW09_GTable, type="global") #cluster coefficient
HAW09_G.degreeCent <- centralization.degree(HAW09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_Gftn <- as.network.matrix(HAW09_Gft)
HAW09_G.netDensity <- network.density(HAW09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_G.entropy <- entropy(HAW09_Gft) #entropy

HAW09_G.netMx <- cbind(HAW09_G.netMx, HAW09_G.clusterCoef, HAW09_G.degreeCent$centralization,
                       HAW09_G.netDensity, HAW09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Behind_F"
HAW09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
HAW09_Bg2 <- data.frame(HAW09_B)
HAW09_Bg2 <- HAW09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_Bg2$player1
player2vector <- HAW09_Bg2$player2
HAW09_Bg3 <- HAW09_Bg2
HAW09_Bg3$p1inp2vec <- is.element(HAW09_Bg3$player1, player2vector)
HAW09_Bg3$p2inp1vec <- is.element(HAW09_Bg3$player2, player1vector)

addPlayer1 <- HAW09_Bg3[ which(HAW09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW09_Bg2 <- rbind(HAW09_Bg2, addPlayer1)

#ROUND 9, Behind graph using weighted edges
HAW09_Bft <- ftable(HAW09_Bg2$player1, HAW09_Bg2$player2)
HAW09_Bft2 <- as.matrix(HAW09_Bft)
numRows <- nrow(HAW09_Bft2)
numCols <- ncol(HAW09_Bft2)
HAW09_Bft3 <- HAW09_Bft2[c(2:numRows) , c(1:numCols)]
HAW09_BTable <- graph.adjacency(HAW09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(HAW09_BTable, vertex.label = V(HAW09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
HAW09_B.clusterCoef <- transitivity(HAW09_BTable, type="global") #cluster coefficient
HAW09_B.degreeCent <- centralization.degree(HAW09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_Bftn <- as.network.matrix(HAW09_Bft)
HAW09_B.netDensity <- network.density(HAW09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_B.entropy <- entropy(HAW09_Bft) #entropy

HAW09_B.netMx <- cbind(HAW09_B.netMx, HAW09_B.clusterCoef, HAW09_B.degreeCent$centralization,
                       HAW09_B.netDensity, HAW09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
HAW09_SFg2 <- data.frame(HAW09_SF)
HAW09_SFg2 <- HAW09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_SFg2$player1
player2vector <- HAW09_SFg2$player2
HAW09_SFg3 <- HAW09_SFg2
HAW09_SFg3$p1inp2vec <- is.element(HAW09_SFg3$player1, player2vector)
HAW09_SFg3$p2inp1vec <- is.element(HAW09_SFg3$player2, player1vector)

addPlayer1 <- HAW09_SFg3[ which(HAW09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_SFg3[ which(HAW09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_SFg2 <- rbind(HAW09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
HAW09_SFft <- ftable(HAW09_SFg2$player1, HAW09_SFg2$player2)
HAW09_SFft2 <- as.matrix(HAW09_SFft)
numRows <- nrow(HAW09_SFft2)
numCols <- ncol(HAW09_SFft2)
HAW09_SFft3 <- HAW09_SFft2[c(2:numRows) , c(2:numCols)]
HAW09_SFTable <- graph.adjacency(HAW09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(HAW09_SFTable, vertex.label = V(HAW09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
HAW09_SF.clusterCoef <- transitivity(HAW09_SFTable, type="global") #cluster coefficient
HAW09_SF.degreeCent <- centralization.degree(HAW09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_SFftn <- as.network.matrix(HAW09_SFft)
HAW09_SF.netDensity <- network.density(HAW09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_SF.entropy <- entropy(HAW09_SFft) #entropy

HAW09_SF.netMx <- cbind(HAW09_SF.netMx, HAW09_SF.clusterCoef, HAW09_SF.degreeCent$centralization,
                        HAW09_SF.netDensity, HAW09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
HAW09_TFg2 <- data.frame(HAW09_TF)
HAW09_TFg2 <- HAW09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_TFg2$player1
player2vector <- HAW09_TFg2$player2
HAW09_TFg3 <- HAW09_TFg2
HAW09_TFg3$p1inp2vec <- is.element(HAW09_TFg3$player1, player2vector)
HAW09_TFg3$p2inp1vec <- is.element(HAW09_TFg3$player2, player1vector)

addPlayer1 <- HAW09_TFg3[ which(HAW09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_TFg3[ which(HAW09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_TFg2 <- rbind(HAW09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
HAW09_TFft <- ftable(HAW09_TFg2$player1, HAW09_TFg2$player2)
HAW09_TFft2 <- as.matrix(HAW09_TFft)
numRows <- nrow(HAW09_TFft2)
numCols <- ncol(HAW09_TFft2)
HAW09_TFft3 <- HAW09_TFft2[c(2:numRows) , c(2:numCols)]
HAW09_TFTable <- graph.adjacency(HAW09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(HAW09_TFTable, vertex.label = V(HAW09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
HAW09_TF.clusterCoef <- transitivity(HAW09_TFTable, type="global") #cluster coefficient
HAW09_TF.degreeCent <- centralization.degree(HAW09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_TFftn <- as.network.matrix(HAW09_TFft)
HAW09_TF.netDensity <- network.density(HAW09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_TF.entropy <- entropy(HAW09_TFft) #entropy

HAW09_TF.netMx <- cbind(HAW09_TF.netMx, HAW09_TF.clusterCoef, HAW09_TF.degreeCent$centralization,
                        HAW09_TF.netDensity, HAW09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
HAW09_SAMg2 <- data.frame(HAW09_SAM)
HAW09_SAMg2 <- HAW09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_SAMg2$player1
player2vector <- HAW09_SAMg2$player2
HAW09_SAMg3 <- HAW09_SAMg2
HAW09_SAMg3$p1inp2vec <- is.element(HAW09_SAMg3$player1, player2vector)
HAW09_SAMg3$p2inp1vec <- is.element(HAW09_SAMg3$player2, player1vector)

addPlayer1 <- HAW09_SAMg3[ which(HAW09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_SAMg3[ which(HAW09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_SAMg2 <- rbind(HAW09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
HAW09_SAMft <- ftable(HAW09_SAMg2$player1, HAW09_SAMg2$player2)
HAW09_SAMft2 <- as.matrix(HAW09_SAMft)
numRows <- nrow(HAW09_SAMft2)
numCols <- ncol(HAW09_SAMft2)
HAW09_SAMft3 <- HAW09_SAMft2[c(2:numRows) , c(2:numCols)]
HAW09_SAMTable <- graph.adjacency(HAW09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(HAW09_SAMTable, vertex.label = V(HAW09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
HAW09_SAM.clusterCoef <- transitivity(HAW09_SAMTable, type="global") #cluster coefficient
HAW09_SAM.degreeCent <- centralization.degree(HAW09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_SAMftn <- as.network.matrix(HAW09_SAMft)
HAW09_SAM.netDensity <- network.density(HAW09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_SAM.entropy <- entropy(HAW09_SAMft) #entropy

HAW09_SAM.netMx <- cbind(HAW09_SAM.netMx, HAW09_SAM.clusterCoef, HAW09_SAM.degreeCent$centralization,
                         HAW09_SAM.netDensity, HAW09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
HAW09_TAMg2 <- data.frame(HAW09_TAM)
HAW09_TAMg2 <- HAW09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_TAMg2$player1
player2vector <- HAW09_TAMg2$player2
HAW09_TAMg3 <- HAW09_TAMg2
HAW09_TAMg3$p1inp2vec <- is.element(HAW09_TAMg3$player1, player2vector)
HAW09_TAMg3$p2inp1vec <- is.element(HAW09_TAMg3$player2, player1vector)

addPlayer1 <- HAW09_TAMg3[ which(HAW09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_TAMg3[ which(HAW09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_TAMg2 <- rbind(HAW09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
HAW09_TAMft <- ftable(HAW09_TAMg2$player1, HAW09_TAMg2$player2)
HAW09_TAMft2 <- as.matrix(HAW09_TAMft)
numRows <- nrow(HAW09_TAMft2)
numCols <- ncol(HAW09_TAMft2)
HAW09_TAMft3 <- HAW09_TAMft2[c(2:numRows) , c(2:numCols)]
HAW09_TAMTable <- graph.adjacency(HAW09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(HAW09_TAMTable, vertex.label = V(HAW09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
HAW09_TAM.clusterCoef <- transitivity(HAW09_TAMTable, type="global") #cluster coefficient
HAW09_TAM.degreeCent <- centralization.degree(HAW09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_TAMftn <- as.network.matrix(HAW09_TAMft)
HAW09_TAM.netDensity <- network.density(HAW09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_TAM.entropy <- entropy(HAW09_TAMft) #entropy

HAW09_TAM.netMx <- cbind(HAW09_TAM.netMx, HAW09_TAM.clusterCoef, HAW09_TAM.degreeCent$centralization,
                         HAW09_TAM.netDensity, HAW09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
HAW09_SDMg2 <- data.frame(HAW09_SDM)
HAW09_SDMg2 <- HAW09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_SDMg2$player1
player2vector <- HAW09_SDMg2$player2
HAW09_SDMg3 <- HAW09_SDMg2
HAW09_SDMg3$p1inp2vec <- is.element(HAW09_SDMg3$player1, player2vector)
HAW09_SDMg3$p2inp1vec <- is.element(HAW09_SDMg3$player2, player1vector)

addPlayer1 <- HAW09_SDMg3[ which(HAW09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_SDMg3[ which(HAW09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_SDMg2 <- rbind(HAW09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
HAW09_SDMft <- ftable(HAW09_SDMg2$player1, HAW09_SDMg2$player2)
HAW09_SDMft2 <- as.matrix(HAW09_SDMft)
numRows <- nrow(HAW09_SDMft2)
numCols <- ncol(HAW09_SDMft2)
HAW09_SDMft3 <- HAW09_SDMft2[c(2:numRows) , c(2:numCols)]
HAW09_SDMTable <- graph.adjacency(HAW09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(HAW09_SDMTable, vertex.label = V(HAW09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
HAW09_SDM.clusterCoef <- transitivity(HAW09_SDMTable, type="global") #cluster coefficient
HAW09_SDM.degreeCent <- centralization.degree(HAW09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_SDMftn <- as.network.matrix(HAW09_SDMft)
HAW09_SDM.netDensity <- network.density(HAW09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_SDM.entropy <- entropy(HAW09_SDMft) #entropy

HAW09_SDM.netMx <- cbind(HAW09_SDM.netMx, HAW09_SDM.clusterCoef, HAW09_SDM.degreeCent$centralization,
                         HAW09_SDM.netDensity, HAW09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
HAW09_TDMg2 <- data.frame(HAW09_TDM)
HAW09_TDMg2 <- HAW09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_TDMg2$player1
player2vector <- HAW09_TDMg2$player2
HAW09_TDMg3 <- HAW09_TDMg2
HAW09_TDMg3$p1inp2vec <- is.element(HAW09_TDMg3$player1, player2vector)
HAW09_TDMg3$p2inp1vec <- is.element(HAW09_TDMg3$player2, player1vector)

addPlayer1 <- HAW09_TDMg3[ which(HAW09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_TDMg3[ which(HAW09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_TDMg2 <- rbind(HAW09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
HAW09_TDMft <- ftable(HAW09_TDMg2$player1, HAW09_TDMg2$player2)
HAW09_TDMft2 <- as.matrix(HAW09_TDMft)
numRows <- nrow(HAW09_TDMft2)
numCols <- ncol(HAW09_TDMft2)
HAW09_TDMft3 <- HAW09_TDMft2[c(2:numRows) , c(2:numCols)]
HAW09_TDMTable <- graph.adjacency(HAW09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(HAW09_TDMTable, vertex.label = V(HAW09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
HAW09_TDM.clusterCoef <- transitivity(HAW09_TDMTable, type="global") #cluster coefficient
HAW09_TDM.degreeCent <- centralization.degree(HAW09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_TDMftn <- as.network.matrix(HAW09_TDMft)
HAW09_TDM.netDensity <- network.density(HAW09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_TDM.entropy <- entropy(HAW09_TDMft) #entropy

HAW09_TDM.netMx <- cbind(HAW09_TDM.netMx, HAW09_TDM.clusterCoef, HAW09_TDM.degreeCent$centralization,
                         HAW09_TDM.netDensity, HAW09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
HAW09_SDg2 <- data.frame(HAW09_SD)
HAW09_SDg2 <- HAW09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_SDg2$player1
player2vector <- HAW09_SDg2$player2
HAW09_SDg3 <- HAW09_SDg2
HAW09_SDg3$p1inp2vec <- is.element(HAW09_SDg3$player1, player2vector)
HAW09_SDg3$p2inp1vec <- is.element(HAW09_SDg3$player2, player1vector)

addPlayer1 <- HAW09_SDg3[ which(HAW09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_SDg3[ which(HAW09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_SDg2 <- rbind(HAW09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
HAW09_SDft <- ftable(HAW09_SDg2$player1, HAW09_SDg2$player2)
HAW09_SDft2 <- as.matrix(HAW09_SDft)
numRows <- nrow(HAW09_SDft2)
numCols <- ncol(HAW09_SDft2)
HAW09_SDft3 <- HAW09_SDft2[c(2:numRows) , c(2:numCols)]
HAW09_SDTable <- graph.adjacency(HAW09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(HAW09_SDTable, vertex.label = V(HAW09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
HAW09_SD.clusterCoef <- transitivity(HAW09_SDTable, type="global") #cluster coefficient
HAW09_SD.degreeCent <- centralization.degree(HAW09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_SDftn <- as.network.matrix(HAW09_SDft)
HAW09_SD.netDensity <- network.density(HAW09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_SD.entropy <- entropy(HAW09_SDft) #entropy

HAW09_SD.netMx <- cbind(HAW09_SD.netMx, HAW09_SD.clusterCoef, HAW09_SD.degreeCent$centralization,
                        HAW09_SD.netDensity, HAW09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
HAW09_TDg2 <- data.frame(HAW09_TD)
HAW09_TDg2 <- HAW09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_TDg2$player1
player2vector <- HAW09_TDg2$player2
HAW09_TDg3 <- HAW09_TDg2
HAW09_TDg3$p1inp2vec <- is.element(HAW09_TDg3$player1, player2vector)
HAW09_TDg3$p2inp1vec <- is.element(HAW09_TDg3$player2, player1vector)

addPlayer1 <- HAW09_TDg3[ which(HAW09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_TDg3[ which(HAW09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_TDg2 <- rbind(HAW09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
HAW09_TDft <- ftable(HAW09_TDg2$player1, HAW09_TDg2$player2)
HAW09_TDft2 <- as.matrix(HAW09_TDft)
numRows <- nrow(HAW09_TDft2)
numCols <- ncol(HAW09_TDft2)
HAW09_TDft3 <- HAW09_TDft2[c(2:numRows) , c(2:numCols)]
HAW09_TDTable <- graph.adjacency(HAW09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(HAW09_TDTable, vertex.label = V(HAW09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
HAW09_TD.clusterCoef <- transitivity(HAW09_TDTable, type="global") #cluster coefficient
HAW09_TD.degreeCent <- centralization.degree(HAW09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_TDftn <- as.network.matrix(HAW09_TDft)
HAW09_TD.netDensity <- network.density(HAW09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_TD.entropy <- entropy(HAW09_TDft) #entropy

HAW09_TD.netMx <- cbind(HAW09_TD.netMx, HAW09_TD.clusterCoef, HAW09_TD.degreeCent$centralization,
                        HAW09_TD.netDensity, HAW09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
HAW09_QTg2 <- data.frame(HAW09_QT)
HAW09_QTg2 <- HAW09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW09_QTg2$player1
player2vector <- HAW09_QTg2$player2
HAW09_QTg3 <- HAW09_QTg2
HAW09_QTg3$p1inp2vec <- is.element(HAW09_QTg3$player1, player2vector)
HAW09_QTg3$p2inp1vec <- is.element(HAW09_QTg3$player2, player1vector)

addPlayer1 <- HAW09_QTg3[ which(HAW09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW09_QTg3[ which(HAW09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW09_QTg2 <- rbind(HAW09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
HAW09_QTft <- ftable(HAW09_QTg2$player1, HAW09_QTg2$player2)
HAW09_QTft2 <- as.matrix(HAW09_QTft)
numRows <- nrow(HAW09_QTft2)
numCols <- ncol(HAW09_QTft2)
HAW09_QTft3 <- HAW09_QTft2[c(2:numRows) , c(2:numCols)]
HAW09_QTTable <- graph.adjacency(HAW09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(HAW09_QTTable, vertex.label = V(HAW09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
HAW09_QT.clusterCoef <- transitivity(HAW09_QTTable, type="global") #cluster coefficient
HAW09_QT.degreeCent <- centralization.degree(HAW09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW09_QTftn <- as.network.matrix(HAW09_QTft)
HAW09_QT.netDensity <- network.density(HAW09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW09_QT.entropy <- entropy(HAW09_QTft) #entropy

HAW09_QT.netMx <- cbind(HAW09_QT.netMx, HAW09_QT.clusterCoef, HAW09_QT.degreeCent$centralization,
                        HAW09_QT.netDensity, HAW09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW09_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "MELB"
KIoutcome = "Goal_F"
MELB09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
MELB09_Gg2 <- data.frame(MELB09_G)
MELB09_Gg2 <- MELB09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_Gg2$player1
player2vector <- MELB09_Gg2$player2
MELB09_Gg3 <- MELB09_Gg2
MELB09_Gg3$p1inp2vec <- is.element(MELB09_Gg3$player1, player2vector)
MELB09_Gg3$p2inp1vec <- is.element(MELB09_Gg3$player2, player1vector)

addPlayer1 <- MELB09_Gg3[ which(MELB09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_Gg3[ which(MELB09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_Gg2 <- rbind(MELB09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
MELB09_Gft <- ftable(MELB09_Gg2$player1, MELB09_Gg2$player2)
MELB09_Gft2 <- as.matrix(MELB09_Gft)
numRows <- nrow(MELB09_Gft2)
numCols <- ncol(MELB09_Gft2)
MELB09_Gft3 <- MELB09_Gft2[c(2:numRows) , c(2:numCols)]
MELB09_GTable <- graph.adjacency(MELB09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(MELB09_GTable, vertex.label = V(MELB09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
MELB09_G.clusterCoef <- transitivity(MELB09_GTable, type="global") #cluster coefficient
MELB09_G.degreeCent <- centralization.degree(MELB09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_Gftn <- as.network.matrix(MELB09_Gft)
MELB09_G.netDensity <- network.density(MELB09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_G.entropy <- entropy(MELB09_Gft) #entropy

MELB09_G.netMx <- cbind(MELB09_G.netMx, MELB09_G.clusterCoef, MELB09_G.degreeCent$centralization,
                        MELB09_G.netDensity, MELB09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Behind_F"
MELB09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
MELB09_Bg2 <- data.frame(MELB09_B)
MELB09_Bg2 <- MELB09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_Bg2$player1
player2vector <- MELB09_Bg2$player2
MELB09_Bg3 <- MELB09_Bg2
MELB09_Bg3$p1inp2vec <- is.element(MELB09_Bg3$player1, player2vector)
MELB09_Bg3$p2inp1vec <- is.element(MELB09_Bg3$player2, player1vector)

addPlayer1 <- MELB09_Bg3[ which(MELB09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_Bg3[ which(MELB09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_Bg2 <- rbind(MELB09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
MELB09_Bft <- ftable(MELB09_Bg2$player1, MELB09_Bg2$player2)
MELB09_Bft2 <- as.matrix(MELB09_Bft)
numRows <- nrow(MELB09_Bft2)
numCols <- ncol(MELB09_Bft2)
MELB09_Bft3 <- MELB09_Bft2[c(2:numRows) , c(2:numCols)]
MELB09_BTable <- graph.adjacency(MELB09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(MELB09_BTable, vertex.label = V(MELB09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
MELB09_B.clusterCoef <- transitivity(MELB09_BTable, type="global") #cluster coefficient
MELB09_B.degreeCent <- centralization.degree(MELB09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_Bftn <- as.network.matrix(MELB09_Bft)
MELB09_B.netDensity <- network.density(MELB09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_B.entropy <- entropy(MELB09_Bft) #entropy

MELB09_B.netMx <- cbind(MELB09_B.netMx, MELB09_B.clusterCoef, MELB09_B.degreeCent$centralization,
                        MELB09_B.netDensity, MELB09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
MELB09_SFg2 <- data.frame(MELB09_SF)
MELB09_SFg2 <- MELB09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_SFg2$player1
player2vector <- MELB09_SFg2$player2
MELB09_SFg3 <- MELB09_SFg2
MELB09_SFg3$p1inp2vec <- is.element(MELB09_SFg3$player1, player2vector)
MELB09_SFg3$p2inp1vec <- is.element(MELB09_SFg3$player2, player1vector)

addPlayer1 <- MELB09_SFg3[ which(MELB09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_SFg3[ which(MELB09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_SFg2 <- rbind(MELB09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
MELB09_SFft <- ftable(MELB09_SFg2$player1, MELB09_SFg2$player2)
MELB09_SFft2 <- as.matrix(MELB09_SFft)
numRows <- nrow(MELB09_SFft2)
numCols <- ncol(MELB09_SFft2)
MELB09_SFft3 <- MELB09_SFft2[c(2:numRows) , c(2:numCols)]
MELB09_SFTable <- graph.adjacency(MELB09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(MELB09_SFTable, vertex.label = V(MELB09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
MELB09_SF.clusterCoef <- transitivity(MELB09_SFTable, type="global") #cluster coefficient
MELB09_SF.degreeCent <- centralization.degree(MELB09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_SFftn <- as.network.matrix(MELB09_SFft)
MELB09_SF.netDensity <- network.density(MELB09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_SF.entropy <- entropy(MELB09_SFft) #entropy

MELB09_SF.netMx <- cbind(MELB09_SF.netMx, MELB09_SF.clusterCoef, MELB09_SF.degreeCent$centralization,
                         MELB09_SF.netDensity, MELB09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
MELB09_TFg2 <- data.frame(MELB09_TF)
MELB09_TFg2 <- MELB09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_TFg2$player1
player2vector <- MELB09_TFg2$player2
MELB09_TFg3 <- MELB09_TFg2
MELB09_TFg3$p1inp2vec <- is.element(MELB09_TFg3$player1, player2vector)
MELB09_TFg3$p2inp1vec <- is.element(MELB09_TFg3$player2, player1vector)

addPlayer1 <- MELB09_TFg3[ which(MELB09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_TFg3[ which(MELB09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_TFg2 <- rbind(MELB09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
MELB09_TFft <- ftable(MELB09_TFg2$player1, MELB09_TFg2$player2)
MELB09_TFft2 <- as.matrix(MELB09_TFft)
numRows <- nrow(MELB09_TFft2)
numCols <- ncol(MELB09_TFft2)
MELB09_TFft3 <- MELB09_TFft2[c(2:numRows) , c(2:numCols)]
MELB09_TFTable <- graph.adjacency(MELB09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(MELB09_TFTable, vertex.label = V(MELB09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
MELB09_TF.clusterCoef <- transitivity(MELB09_TFTable, type="global") #cluster coefficient
MELB09_TF.degreeCent <- centralization.degree(MELB09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_TFftn <- as.network.matrix(MELB09_TFft)
MELB09_TF.netDensity <- network.density(MELB09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_TF.entropy <- entropy(MELB09_TFft) #entropy

MELB09_TF.netMx <- cbind(MELB09_TF.netMx, MELB09_TF.clusterCoef, MELB09_TF.degreeCent$centralization,
                         MELB09_TF.netDensity, MELB09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
MELB09_SAMg2 <- data.frame(MELB09_SAM)
MELB09_SAMg2 <- MELB09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_SAMg2$player1
player2vector <- MELB09_SAMg2$player2
MELB09_SAMg3 <- MELB09_SAMg2
MELB09_SAMg3$p1inp2vec <- is.element(MELB09_SAMg3$player1, player2vector)
MELB09_SAMg3$p2inp1vec <- is.element(MELB09_SAMg3$player2, player1vector)

addPlayer1 <- MELB09_SAMg3[ which(MELB09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_SAMg3[ which(MELB09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_SAMg2 <- rbind(MELB09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
MELB09_SAMft <- ftable(MELB09_SAMg2$player1, MELB09_SAMg2$player2)
MELB09_SAMft2 <- as.matrix(MELB09_SAMft)
numRows <- nrow(MELB09_SAMft2)
numCols <- ncol(MELB09_SAMft2)
MELB09_SAMft3 <- MELB09_SAMft2[c(2:numRows) , c(2:numCols)]
MELB09_SAMTable <- graph.adjacency(MELB09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(MELB09_SAMTable, vertex.label = V(MELB09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
MELB09_SAM.clusterCoef <- transitivity(MELB09_SAMTable, type="global") #cluster coefficient
MELB09_SAM.degreeCent <- centralization.degree(MELB09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_SAMftn <- as.network.matrix(MELB09_SAMft)
MELB09_SAM.netDensity <- network.density(MELB09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_SAM.entropy <- entropy(MELB09_SAMft) #entropy

MELB09_SAM.netMx <- cbind(MELB09_SAM.netMx, MELB09_SAM.clusterCoef, MELB09_SAM.degreeCent$centralization,
                          MELB09_SAM.netDensity, MELB09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
MELB09_TAMg2 <- data.frame(MELB09_TAM)
MELB09_TAMg2 <- MELB09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_TAMg2$player1
player2vector <- MELB09_TAMg2$player2
MELB09_TAMg3 <- MELB09_TAMg2
MELB09_TAMg3$p1inp2vec <- is.element(MELB09_TAMg3$player1, player2vector)
MELB09_TAMg3$p2inp1vec <- is.element(MELB09_TAMg3$player2, player1vector)

addPlayer1 <- MELB09_TAMg3[ which(MELB09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_TAMg3[ which(MELB09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_TAMg2 <- rbind(MELB09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
MELB09_TAMft <- ftable(MELB09_TAMg2$player1, MELB09_TAMg2$player2)
MELB09_TAMft2 <- as.matrix(MELB09_TAMft)
numRows <- nrow(MELB09_TAMft2)
numCols <- ncol(MELB09_TAMft2)
MELB09_TAMft3 <- MELB09_TAMft2[c(2:numRows) , c(2:numCols)]
MELB09_TAMTable <- graph.adjacency(MELB09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(MELB09_TAMTable, vertex.label = V(MELB09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
MELB09_TAM.clusterCoef <- transitivity(MELB09_TAMTable, type="global") #cluster coefficient
MELB09_TAM.degreeCent <- centralization.degree(MELB09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_TAMftn <- as.network.matrix(MELB09_TAMft)
MELB09_TAM.netDensity <- network.density(MELB09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_TAM.entropy <- entropy(MELB09_TAMft) #entropy

MELB09_TAM.netMx <- cbind(MELB09_TAM.netMx, MELB09_TAM.clusterCoef, MELB09_TAM.degreeCent$centralization,
                          MELB09_TAM.netDensity, MELB09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
MELB09_SDMg2 <- data.frame(MELB09_SDM)
MELB09_SDMg2 <- MELB09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_SDMg2$player1
player2vector <- MELB09_SDMg2$player2
MELB09_SDMg3 <- MELB09_SDMg2
MELB09_SDMg3$p1inp2vec <- is.element(MELB09_SDMg3$player1, player2vector)
MELB09_SDMg3$p2inp1vec <- is.element(MELB09_SDMg3$player2, player1vector)

addPlayer1 <- MELB09_SDMg3[ which(MELB09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_SDMg3[ which(MELB09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_SDMg2 <- rbind(MELB09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
MELB09_SDMft <- ftable(MELB09_SDMg2$player1, MELB09_SDMg2$player2)
MELB09_SDMft2 <- as.matrix(MELB09_SDMft)
numRows <- nrow(MELB09_SDMft2)
numCols <- ncol(MELB09_SDMft2)
MELB09_SDMft3 <- MELB09_SDMft2[c(2:numRows) , c(2:numCols)]
MELB09_SDMTable <- graph.adjacency(MELB09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(MELB09_SDMTable, vertex.label = V(MELB09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
MELB09_SDM.clusterCoef <- transitivity(MELB09_SDMTable, type="global") #cluster coefficient
MELB09_SDM.degreeCent <- centralization.degree(MELB09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_SDMftn <- as.network.matrix(MELB09_SDMft)
MELB09_SDM.netDensity <- network.density(MELB09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_SDM.entropy <- entropy(MELB09_SDMft) #entropy

MELB09_SDM.netMx <- cbind(MELB09_SDM.netMx, MELB09_SDM.clusterCoef, MELB09_SDM.degreeCent$centralization,
                          MELB09_SDM.netDensity, MELB09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
MELB09_TDMg2 <- data.frame(MELB09_TDM)
MELB09_TDMg2 <- MELB09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_TDMg2$player1
player2vector <- MELB09_TDMg2$player2
MELB09_TDMg3 <- MELB09_TDMg2
MELB09_TDMg3$p1inp2vec <- is.element(MELB09_TDMg3$player1, player2vector)
MELB09_TDMg3$p2inp1vec <- is.element(MELB09_TDMg3$player2, player1vector)

addPlayer1 <- MELB09_TDMg3[ which(MELB09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_TDMg3[ which(MELB09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_TDMg2 <- rbind(MELB09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
MELB09_TDMft <- ftable(MELB09_TDMg2$player1, MELB09_TDMg2$player2)
MELB09_TDMft2 <- as.matrix(MELB09_TDMft)
numRows <- nrow(MELB09_TDMft2)
numCols <- ncol(MELB09_TDMft2)
MELB09_TDMft3 <- MELB09_TDMft2[c(2:numRows) , c(2:numCols)]
MELB09_TDMTable <- graph.adjacency(MELB09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(MELB09_TDMTable, vertex.label = V(MELB09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
MELB09_TDM.clusterCoef <- transitivity(MELB09_TDMTable, type="global") #cluster coefficient
MELB09_TDM.degreeCent <- centralization.degree(MELB09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_TDMftn <- as.network.matrix(MELB09_TDMft)
MELB09_TDM.netDensity <- network.density(MELB09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_TDM.entropy <- entropy(MELB09_TDMft) #entropy

MELB09_TDM.netMx <- cbind(MELB09_TDM.netMx, MELB09_TDM.clusterCoef, MELB09_TDM.degreeCent$centralization,
                          MELB09_TDM.netDensity, MELB09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
MELB09_SDg2 <- data.frame(MELB09_SD)
MELB09_SDg2 <- MELB09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_SDg2$player1
player2vector <- MELB09_SDg2$player2
MELB09_SDg3 <- MELB09_SDg2
MELB09_SDg3$p1inp2vec <- is.element(MELB09_SDg3$player1, player2vector)
MELB09_SDg3$p2inp1vec <- is.element(MELB09_SDg3$player2, player1vector)

addPlayer1 <- MELB09_SDg3[ which(MELB09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_SDg3[ which(MELB09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_SDg2 <- rbind(MELB09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
MELB09_SDft <- ftable(MELB09_SDg2$player1, MELB09_SDg2$player2)
MELB09_SDft2 <- as.matrix(MELB09_SDft)
numRows <- nrow(MELB09_SDft2)
numCols <- ncol(MELB09_SDft2)
MELB09_SDft3 <- MELB09_SDft2[c(2:numRows) , c(2:numCols)]
MELB09_SDTable <- graph.adjacency(MELB09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(MELB09_SDTable, vertex.label = V(MELB09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
MELB09_SD.clusterCoef <- transitivity(MELB09_SDTable, type="global") #cluster coefficient
MELB09_SD.degreeCent <- centralization.degree(MELB09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_SDftn <- as.network.matrix(MELB09_SDft)
MELB09_SD.netDensity <- network.density(MELB09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_SD.entropy <- entropy(MELB09_SDft) #entropy

MELB09_SD.netMx <- cbind(MELB09_SD.netMx, MELB09_SD.clusterCoef, MELB09_SD.degreeCent$centralization,
                         MELB09_SD.netDensity, MELB09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
MELB09_TDg2 <- data.frame(MELB09_TD)
MELB09_TDg2 <- MELB09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_TDg2$player1
player2vector <- MELB09_TDg2$player2
MELB09_TDg3 <- MELB09_TDg2
MELB09_TDg3$p1inp2vec <- is.element(MELB09_TDg3$player1, player2vector)
MELB09_TDg3$p2inp1vec <- is.element(MELB09_TDg3$player2, player1vector)

addPlayer1 <- MELB09_TDg3[ which(MELB09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_TDg3[ which(MELB09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_TDg2 <- rbind(MELB09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
MELB09_TDft <- ftable(MELB09_TDg2$player1, MELB09_TDg2$player2)
MELB09_TDft2 <- as.matrix(MELB09_TDft)
numRows <- nrow(MELB09_TDft2)
numCols <- ncol(MELB09_TDft2)
MELB09_TDft3 <- MELB09_TDft2[c(2:numRows) , c(2:numCols)]
MELB09_TDTable <- graph.adjacency(MELB09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(MELB09_TDTable, vertex.label = V(MELB09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
MELB09_TD.clusterCoef <- transitivity(MELB09_TDTable, type="global") #cluster coefficient
MELB09_TD.degreeCent <- centralization.degree(MELB09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_TDftn <- as.network.matrix(MELB09_TDft)
MELB09_TD.netDensity <- network.density(MELB09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_TD.entropy <- entropy(MELB09_TDft) #entropy

MELB09_TD.netMx <- cbind(MELB09_TD.netMx, MELB09_TD.clusterCoef, MELB09_TD.degreeCent$centralization,
                         MELB09_TD.netDensity, MELB09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
MELB09_QTg2 <- data.frame(MELB09_QT)
MELB09_QTg2 <- MELB09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB09_QTg2$player1
player2vector <- MELB09_QTg2$player2
MELB09_QTg3 <- MELB09_QTg2
MELB09_QTg3$p1inp2vec <- is.element(MELB09_QTg3$player1, player2vector)
MELB09_QTg3$p2inp1vec <- is.element(MELB09_QTg3$player2, player1vector)

addPlayer1 <- MELB09_QTg3[ which(MELB09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB09_QTg3[ which(MELB09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB09_QTg2 <- rbind(MELB09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
MELB09_QTft <- ftable(MELB09_QTg2$player1, MELB09_QTg2$player2)
MELB09_QTft2 <- as.matrix(MELB09_QTft)
numRows <- nrow(MELB09_QTft2)
numCols <- ncol(MELB09_QTft2)
MELB09_QTft3 <- MELB09_QTft2[c(2:numRows) , c(2:numCols)]
MELB09_QTTable <- graph.adjacency(MELB09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(MELB09_QTTable, vertex.label = V(MELB09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
MELB09_QT.clusterCoef <- transitivity(MELB09_QTTable, type="global") #cluster coefficient
MELB09_QT.degreeCent <- centralization.degree(MELB09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB09_QTftn <- as.network.matrix(MELB09_QTft)
MELB09_QT.netDensity <- network.density(MELB09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB09_QT.entropy <- entropy(MELB09_QTft) #entropy

MELB09_QT.netMx <- cbind(MELB09_QT.netMx, MELB09_QT.clusterCoef, MELB09_QT.degreeCent$centralization,
                         MELB09_QT.netDensity, MELB09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB09_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
NMFC09_Gg2 <- data.frame(NMFC09_G)
NMFC09_Gg2 <- NMFC09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_Gg2$player1
player2vector <- NMFC09_Gg2$player2
NMFC09_Gg3 <- NMFC09_Gg2
NMFC09_Gg3$p1inp2vec <- is.element(NMFC09_Gg3$player1, player2vector)
NMFC09_Gg3$p2inp1vec <- is.element(NMFC09_Gg3$player2, player1vector)

addPlayer1 <- NMFC09_Gg3[ which(NMFC09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_Gg3[ which(NMFC09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_Gg2 <- rbind(NMFC09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
NMFC09_Gft <- ftable(NMFC09_Gg2$player1, NMFC09_Gg2$player2)
NMFC09_Gft2 <- as.matrix(NMFC09_Gft)
numRows <- nrow(NMFC09_Gft2)
numCols <- ncol(NMFC09_Gft2)
NMFC09_Gft3 <- NMFC09_Gft2[c(2:numRows) , c(2:numCols)]
NMFC09_GTable <- graph.adjacency(NMFC09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(NMFC09_GTable, vertex.label = V(NMFC09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
NMFC09_G.clusterCoef <- transitivity(NMFC09_GTable, type="global") #cluster coefficient
NMFC09_G.degreeCent <- centralization.degree(NMFC09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_Gftn <- as.network.matrix(NMFC09_Gft)
NMFC09_G.netDensity <- network.density(NMFC09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_G.entropy <- entropy(NMFC09_Gft) #entropy

NMFC09_G.netMx <- cbind(NMFC09_G.netMx, NMFC09_G.clusterCoef, NMFC09_G.degreeCent$centralization,
                        NMFC09_G.netDensity, NMFC09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************

round = 9
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
NMFC09_Bg2 <- data.frame(NMFC09_B)
NMFC09_Bg2 <- NMFC09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_Bg2$player1
player2vector <- NMFC09_Bg2$player2
NMFC09_Bg3 <- NMFC09_Bg2
NMFC09_Bg3$p1inp2vec <- is.element(NMFC09_Bg3$player1, player2vector)
NMFC09_Bg3$p2inp1vec <- is.element(NMFC09_Bg3$player2, player1vector)

addPlayer1 <- NMFC09_Bg3[ which(NMFC09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_Bg3[ which(NMFC09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_Bg2 <- rbind(NMFC09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
NMFC09_Bft <- ftable(NMFC09_Bg2$player1, NMFC09_Bg2$player2)
NMFC09_Bft2 <- as.matrix(NMFC09_Bft)
numRows <- nrow(NMFC09_Bft2)
numCols <- ncol(NMFC09_Bft2)
NMFC09_Bft3 <- NMFC09_Bft2[c(2:numRows) , c(2:numCols)]
NMFC09_BTable <- graph.adjacency(NMFC09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(NMFC09_BTable, vertex.label = V(NMFC09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
NMFC09_B.clusterCoef <- transitivity(NMFC09_BTable, type="global") #cluster coefficient
NMFC09_B.degreeCent <- centralization.degree(NMFC09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_Bftn <- as.network.matrix(NMFC09_Bft)
NMFC09_B.netDensity <- network.density(NMFC09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_B.entropy <- entropy(NMFC09_Bft) #entropy

NMFC09_B.netMx <- cbind(NMFC09_B.netMx, NMFC09_B.clusterCoef, NMFC09_B.degreeCent$centralization,
                        NMFC09_B.netDensity, NMFC09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************

round = 9
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
NMFC09_SFg2 <- data.frame(NMFC09_SF)
NMFC09_SFg2 <- NMFC09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_SFg2$player1
player2vector <- NMFC09_SFg2$player2
NMFC09_SFg3 <- NMFC09_SFg2
NMFC09_SFg3$p1inp2vec <- is.element(NMFC09_SFg3$player1, player2vector)
NMFC09_SFg3$p2inp1vec <- is.element(NMFC09_SFg3$player2, player1vector)

addPlayer1 <- NMFC09_SFg3[ which(NMFC09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC09_SFg3[ which(NMFC09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_SFg2 <- rbind(NMFC09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
NMFC09_SFft <- ftable(NMFC09_SFg2$player1, NMFC09_SFg2$player2)
NMFC09_SFft2 <- as.matrix(NMFC09_SFft)
numRows <- nrow(NMFC09_SFft2)
numCols <- ncol(NMFC09_SFft2)
NMFC09_SFft3 <- NMFC09_SFft2[c(2:numRows) , c(2:numCols)]
NMFC09_SFTable <- graph.adjacency(NMFC09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(NMFC09_SFTable, vertex.label = V(NMFC09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
NMFC09_SF.clusterCoef <- transitivity(NMFC09_SFTable, type="global") #cluster coefficient
NMFC09_SF.degreeCent <- centralization.degree(NMFC09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_SFftn <- as.network.matrix(NMFC09_SFft)
NMFC09_SF.netDensity <- network.density(NMFC09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_SF.entropy <- entropy(NMFC09_SFft) #entropy

NMFC09_SF.netMx <- cbind(NMFC09_SF.netMx, NMFC09_SF.clusterCoef, NMFC09_SF.degreeCent$centralization,
                         NMFC09_SF.netDensity, NMFC09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
NMFC09_TFg2 <- data.frame(NMFC09_TF)
NMFC09_TFg2 <- NMFC09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_TFg2$player1
player2vector <- NMFC09_TFg2$player2
NMFC09_TFg3 <- NMFC09_TFg2
NMFC09_TFg3$p1inp2vec <- is.element(NMFC09_TFg3$player1, player2vector)
NMFC09_TFg3$p2inp1vec <- is.element(NMFC09_TFg3$player2, player1vector)

addPlayer1 <- NMFC09_TFg3[ which(NMFC09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_TFg3[ which(NMFC09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_TFg2 <- rbind(NMFC09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
NMFC09_TFft <- ftable(NMFC09_TFg2$player1, NMFC09_TFg2$player2)
NMFC09_TFft2 <- as.matrix(NMFC09_TFft)
numRows <- nrow(NMFC09_TFft2)
numCols <- ncol(NMFC09_TFft2)
NMFC09_TFft3 <- NMFC09_TFft2[c(2:numRows) , c(2:numCols)]
NMFC09_TFTable <- graph.adjacency(NMFC09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(NMFC09_TFTable, vertex.label = V(NMFC09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
NMFC09_TF.clusterCoef <- transitivity(NMFC09_TFTable, type="global") #cluster coefficient
NMFC09_TF.degreeCent <- centralization.degree(NMFC09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_TFftn <- as.network.matrix(NMFC09_TFft)
NMFC09_TF.netDensity <- network.density(NMFC09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_TF.entropy <- entropy(NMFC09_TFft) #entropy

NMFC09_TF.netMx <- cbind(NMFC09_TF.netMx, NMFC09_TF.clusterCoef, NMFC09_TF.degreeCent$centralization,
                         NMFC09_TF.netDensity, NMFC09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
NMFC09_SAMg2 <- data.frame(NMFC09_SAM)
NMFC09_SAMg2 <- NMFC09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_SAMg2$player1
player2vector <- NMFC09_SAMg2$player2
NMFC09_SAMg3 <- NMFC09_SAMg2
NMFC09_SAMg3$p1inp2vec <- is.element(NMFC09_SAMg3$player1, player2vector)
NMFC09_SAMg3$p2inp1vec <- is.element(NMFC09_SAMg3$player2, player1vector)

addPlayer1 <- NMFC09_SAMg3[ which(NMFC09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_SAMg3[ which(NMFC09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_SAMg2 <- rbind(NMFC09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
NMFC09_SAMft <- ftable(NMFC09_SAMg2$player1, NMFC09_SAMg2$player2)
NMFC09_SAMft2 <- as.matrix(NMFC09_SAMft)
numRows <- nrow(NMFC09_SAMft2)
numCols <- ncol(NMFC09_SAMft2)
NMFC09_SAMft3 <- NMFC09_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC09_SAMTable <- graph.adjacency(NMFC09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(NMFC09_SAMTable, vertex.label = V(NMFC09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
NMFC09_SAM.clusterCoef <- transitivity(NMFC09_SAMTable, type="global") #cluster coefficient
NMFC09_SAM.degreeCent <- centralization.degree(NMFC09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_SAMftn <- as.network.matrix(NMFC09_SAMft)
NMFC09_SAM.netDensity <- network.density(NMFC09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_SAM.entropy <- entropy(NMFC09_SAMft) #entropy

NMFC09_SAM.netMx <- cbind(NMFC09_SAM.netMx, NMFC09_SAM.clusterCoef, NMFC09_SAM.degreeCent$centralization,
                          NMFC09_SAM.netDensity, NMFC09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
NMFC09_TAMg2 <- data.frame(NMFC09_TAM)
NMFC09_TAMg2 <- NMFC09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_TAMg2$player1
player2vector <- NMFC09_TAMg2$player2
NMFC09_TAMg3 <- NMFC09_TAMg2
NMFC09_TAMg3$p1inp2vec <- is.element(NMFC09_TAMg3$player1, player2vector)
NMFC09_TAMg3$p2inp1vec <- is.element(NMFC09_TAMg3$player2, player1vector)

addPlayer1 <- NMFC09_TAMg3[ which(NMFC09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_TAMg3[ which(NMFC09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_TAMg2 <- rbind(NMFC09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
NMFC09_TAMft <- ftable(NMFC09_TAMg2$player1, NMFC09_TAMg2$player2)
NMFC09_TAMft2 <- as.matrix(NMFC09_TAMft)
numRows <- nrow(NMFC09_TAMft2)
numCols <- ncol(NMFC09_TAMft2)
NMFC09_TAMft3 <- NMFC09_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC09_TAMTable <- graph.adjacency(NMFC09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(NMFC09_TAMTable, vertex.label = V(NMFC09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
NMFC09_TAM.clusterCoef <- transitivity(NMFC09_TAMTable, type="global") #cluster coefficient
NMFC09_TAM.degreeCent <- centralization.degree(NMFC09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_TAMftn <- as.network.matrix(NMFC09_TAMft)
NMFC09_TAM.netDensity <- network.density(NMFC09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_TAM.entropy <- entropy(NMFC09_TAMft) #entropy

NMFC09_TAM.netMx <- cbind(NMFC09_TAM.netMx, NMFC09_TAM.clusterCoef, NMFC09_TAM.degreeCent$centralization,
                          NMFC09_TAM.netDensity, NMFC09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
NMFC09_SDMg2 <- data.frame(NMFC09_SDM)
NMFC09_SDMg2 <- NMFC09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_SDMg2$player1
player2vector <- NMFC09_SDMg2$player2
NMFC09_SDMg3 <- NMFC09_SDMg2
NMFC09_SDMg3$p1inp2vec <- is.element(NMFC09_SDMg3$player1, player2vector)
NMFC09_SDMg3$p2inp1vec <- is.element(NMFC09_SDMg3$player2, player1vector)

addPlayer1 <- NMFC09_SDMg3[ which(NMFC09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_SDMg3[ which(NMFC09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_SDMg2 <- rbind(NMFC09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
NMFC09_SDMft <- ftable(NMFC09_SDMg2$player1, NMFC09_SDMg2$player2)
NMFC09_SDMft2 <- as.matrix(NMFC09_SDMft)
numRows <- nrow(NMFC09_SDMft2)
numCols <- ncol(NMFC09_SDMft2)
NMFC09_SDMft3 <- NMFC09_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC09_SDMTable <- graph.adjacency(NMFC09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(NMFC09_SDMTable, vertex.label = V(NMFC09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
NMFC09_SDM.clusterCoef <- transitivity(NMFC09_SDMTable, type="global") #cluster coefficient
NMFC09_SDM.degreeCent <- centralization.degree(NMFC09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_SDMftn <- as.network.matrix(NMFC09_SDMft)
NMFC09_SDM.netDensity <- network.density(NMFC09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_SDM.entropy <- entropy(NMFC09_SDMft) #entropy

NMFC09_SDM.netMx <- cbind(NMFC09_SDM.netMx, NMFC09_SDM.clusterCoef, NMFC09_SDM.degreeCent$centralization,
                          NMFC09_SDM.netDensity, NMFC09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
NMFC09_TDMg2 <- data.frame(NMFC09_TDM)
NMFC09_TDMg2 <- NMFC09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_TDMg2$player1
player2vector <- NMFC09_TDMg2$player2
NMFC09_TDMg3 <- NMFC09_TDMg2
NMFC09_TDMg3$p1inp2vec <- is.element(NMFC09_TDMg3$player1, player2vector)
NMFC09_TDMg3$p2inp1vec <- is.element(NMFC09_TDMg3$player2, player1vector)

addPlayer1 <- NMFC09_TDMg3[ which(NMFC09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_TDMg3[ which(NMFC09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_TDMg2 <- rbind(NMFC09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
NMFC09_TDMft <- ftable(NMFC09_TDMg2$player1, NMFC09_TDMg2$player2)
NMFC09_TDMft2 <- as.matrix(NMFC09_TDMft)
numRows <- nrow(NMFC09_TDMft2)
numCols <- ncol(NMFC09_TDMft2)
NMFC09_TDMft3 <- NMFC09_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC09_TDMTable <- graph.adjacency(NMFC09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(NMFC09_TDMTable, vertex.label = V(NMFC09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
NMFC09_TDM.clusterCoef <- transitivity(NMFC09_TDMTable, type="global") #cluster coefficient
NMFC09_TDM.degreeCent <- centralization.degree(NMFC09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_TDMftn <- as.network.matrix(NMFC09_TDMft)
NMFC09_TDM.netDensity <- network.density(NMFC09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_TDM.entropy <- entropy(NMFC09_TDMft) #entropy

NMFC09_TDM.netMx <- cbind(NMFC09_TDM.netMx, NMFC09_TDM.clusterCoef, NMFC09_TDM.degreeCent$centralization,
                          NMFC09_TDM.netDensity, NMFC09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
NMFC09_SDg2 <- data.frame(NMFC09_SD)
NMFC09_SDg2 <- NMFC09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_SDg2$player1
player2vector <- NMFC09_SDg2$player2
NMFC09_SDg3 <- NMFC09_SDg2
NMFC09_SDg3$p1inp2vec <- is.element(NMFC09_SDg3$player1, player2vector)
NMFC09_SDg3$p2inp1vec <- is.element(NMFC09_SDg3$player2, player1vector)

addPlayer1 <- NMFC09_SDg3[ which(NMFC09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_SDg3[ which(NMFC09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_SDg2 <- rbind(NMFC09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
NMFC09_SDft <- ftable(NMFC09_SDg2$player1, NMFC09_SDg2$player2)
NMFC09_SDft2 <- as.matrix(NMFC09_SDft)
numRows <- nrow(NMFC09_SDft2)
numCols <- ncol(NMFC09_SDft2)
NMFC09_SDft3 <- NMFC09_SDft2[c(2:numRows) , c(2:numCols)]
NMFC09_SDTable <- graph.adjacency(NMFC09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(NMFC09_SDTable, vertex.label = V(NMFC09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
NMFC09_SD.clusterCoef <- transitivity(NMFC09_SDTable, type="global") #cluster coefficient
NMFC09_SD.degreeCent <- centralization.degree(NMFC09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_SDftn <- as.network.matrix(NMFC09_SDft)
NMFC09_SD.netDensity <- network.density(NMFC09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_SD.entropy <- entropy(NMFC09_SDft) #entropy

NMFC09_SD.netMx <- cbind(NMFC09_SD.netMx, NMFC09_SD.clusterCoef, NMFC09_SD.degreeCent$centralization,
                         NMFC09_SD.netDensity, NMFC09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
NMFC09_TDg2 <- data.frame(NMFC09_TD)
NMFC09_TDg2 <- NMFC09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_TDg2$player1
player2vector <- NMFC09_TDg2$player2
NMFC09_TDg3 <- NMFC09_TDg2
NMFC09_TDg3$p1inp2vec <- is.element(NMFC09_TDg3$player1, player2vector)
NMFC09_TDg3$p2inp1vec <- is.element(NMFC09_TDg3$player2, player1vector)

addPlayer1 <- NMFC09_TDg3[ which(NMFC09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_TDg3[ which(NMFC09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_TDg2 <- rbind(NMFC09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
NMFC09_TDft <- ftable(NMFC09_TDg2$player1, NMFC09_TDg2$player2)
NMFC09_TDft2 <- as.matrix(NMFC09_TDft)
numRows <- nrow(NMFC09_TDft2)
numCols <- ncol(NMFC09_TDft2)
NMFC09_TDft3 <- NMFC09_TDft2[c(2:numRows) , c(2:numCols)]
NMFC09_TDTable <- graph.adjacency(NMFC09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(NMFC09_TDTable, vertex.label = V(NMFC09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
NMFC09_TD.clusterCoef <- transitivity(NMFC09_TDTable, type="global") #cluster coefficient
NMFC09_TD.degreeCent <- centralization.degree(NMFC09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_TDftn <- as.network.matrix(NMFC09_TDft)
NMFC09_TD.netDensity <- network.density(NMFC09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_TD.entropy <- entropy(NMFC09_TDft) #entropy

NMFC09_TD.netMx <- cbind(NMFC09_TD.netMx, NMFC09_TD.clusterCoef, NMFC09_TD.degreeCent$centralization,
                         NMFC09_TD.netDensity, NMFC09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
NMFC09_QTg2 <- data.frame(NMFC09_QT)
NMFC09_QTg2 <- NMFC09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC09_QTg2$player1
player2vector <- NMFC09_QTg2$player2
NMFC09_QTg3 <- NMFC09_QTg2
NMFC09_QTg3$p1inp2vec <- is.element(NMFC09_QTg3$player1, player2vector)
NMFC09_QTg3$p2inp1vec <- is.element(NMFC09_QTg3$player2, player1vector)

addPlayer1 <- NMFC09_QTg3[ which(NMFC09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC09_QTg3[ which(NMFC09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC09_QTg2 <- rbind(NMFC09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
NMFC09_QTft <- ftable(NMFC09_QTg2$player1, NMFC09_QTg2$player2)
NMFC09_QTft2 <- as.matrix(NMFC09_QTft)
numRows <- nrow(NMFC09_QTft2)
numCols <- ncol(NMFC09_QTft2)
NMFC09_QTft3 <- NMFC09_QTft2[c(2:numRows) , c(2:numCols)]
NMFC09_QTTable <- graph.adjacency(NMFC09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(NMFC09_QTTable, vertex.label = V(NMFC09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
NMFC09_QT.clusterCoef <- transitivity(NMFC09_QTTable, type="global") #cluster coefficient
NMFC09_QT.degreeCent <- centralization.degree(NMFC09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC09_QTftn <- as.network.matrix(NMFC09_QTft)
NMFC09_QT.netDensity <- network.density(NMFC09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC09_QT.entropy <- entropy(NMFC09_QTft) #entropy

NMFC09_QT.netMx <- cbind(NMFC09_QT.netMx, NMFC09_QT.clusterCoef, NMFC09_QT.degreeCent$centralization,
                         NMFC09_QT.netDensity, NMFC09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC09_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Goal_F"
PORT09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
PORT09_Gg2 <- data.frame(PORT09_G)
PORT09_Gg2 <- PORT09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_Gg2$player1
player2vector <- PORT09_Gg2$player2
PORT09_Gg3 <- PORT09_Gg2
PORT09_Gg3$p1inp2vec <- is.element(PORT09_Gg3$player1, player2vector)
PORT09_Gg3$p2inp1vec <- is.element(PORT09_Gg3$player2, player1vector)

addPlayer1 <- PORT09_Gg3[ which(PORT09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT09_Gg2 <- rbind(PORT09_Gg2, addPlayer1)

#ROUND 9, Goal graph using weighted edges
PORT09_Gft <- ftable(PORT09_Gg2$player1, PORT09_Gg2$player2)
PORT09_Gft2 <- as.matrix(PORT09_Gft)
numRows <- nrow(PORT09_Gft2)
numCols <- ncol(PORT09_Gft2)
PORT09_Gft3 <- PORT09_Gft2[c(2:numRows) , c(1:numCols)]
PORT09_GTable <- graph.adjacency(PORT09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(PORT09_GTable, vertex.label = V(PORT09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
PORT09_G.clusterCoef <- transitivity(PORT09_GTable, type="global") #cluster coefficient
PORT09_G.degreeCent <- centralization.degree(PORT09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_Gftn <- as.network.matrix(PORT09_Gft)
PORT09_G.netDensity <- network.density(PORT09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_G.entropy <- entropy(PORT09_Gft) #entropy

PORT09_G.netMx <- cbind(PORT09_G.netMx, PORT09_G.clusterCoef, PORT09_G.degreeCent$centralization,
                        PORT09_G.netDensity, PORT09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Behind_F"
PORT09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
PORT09_Bg2 <- data.frame(PORT09_B)
PORT09_Bg2 <- PORT09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_Bg2$player1
player2vector <- PORT09_Bg2$player2
PORT09_Bg3 <- PORT09_Bg2
PORT09_Bg3$p1inp2vec <- is.element(PORT09_Bg3$player1, player2vector)
PORT09_Bg3$p2inp1vec <- is.element(PORT09_Bg3$player2, player1vector)

addPlayer1 <- PORT09_Bg3[ which(PORT09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_Bg3[ which(PORT09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_Bg2 <- rbind(PORT09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
PORT09_Bft <- ftable(PORT09_Bg2$player1, PORT09_Bg2$player2)
PORT09_Bft2 <- as.matrix(PORT09_Bft)
numRows <- nrow(PORT09_Bft2)
numCols <- ncol(PORT09_Bft2)
PORT09_Bft3 <- PORT09_Bft2[c(2:numRows) , c(2:numCols)]
PORT09_BTable <- graph.adjacency(PORT09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(PORT09_BTable, vertex.label = V(PORT09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
PORT09_B.clusterCoef <- transitivity(PORT09_BTable, type="global") #cluster coefficient
PORT09_B.degreeCent <- centralization.degree(PORT09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_Bftn <- as.network.matrix(PORT09_Bft)
PORT09_B.netDensity <- network.density(PORT09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_B.entropy <- entropy(PORT09_Bft) #entropy

PORT09_B.netMx <- cbind(PORT09_B.netMx, PORT09_B.clusterCoef, PORT09_B.degreeCent$centralization,
                        PORT09_B.netDensity, PORT09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
PORT09_SFg2 <- data.frame(PORT09_SF)
PORT09_SFg2 <- PORT09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_SFg2$player1
player2vector <- PORT09_SFg2$player2
PORT09_SFg3 <- PORT09_SFg2
PORT09_SFg3$p1inp2vec <- is.element(PORT09_SFg3$player1, player2vector)
PORT09_SFg3$p2inp1vec <- is.element(PORT09_SFg3$player2, player1vector)

addPlayer1 <- PORT09_SFg3[ which(PORT09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_SFg3[ which(PORT09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_SFg2 <- rbind(PORT09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
PORT09_SFft <- ftable(PORT09_SFg2$player1, PORT09_SFg2$player2)
PORT09_SFft2 <- as.matrix(PORT09_SFft)
numRows <- nrow(PORT09_SFft2)
numCols <- ncol(PORT09_SFft2)
PORT09_SFft3 <- PORT09_SFft2[c(2:numRows) , c(2:numCols)]
PORT09_SFTable <- graph.adjacency(PORT09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(PORT09_SFTable, vertex.label = V(PORT09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
PORT09_SF.clusterCoef <- transitivity(PORT09_SFTable, type="global") #cluster coefficient
PORT09_SF.degreeCent <- centralization.degree(PORT09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_SFftn <- as.network.matrix(PORT09_SFft)
PORT09_SF.netDensity <- network.density(PORT09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_SF.entropy <- entropy(PORT09_SFft) #entropy

PORT09_SF.netMx <- cbind(PORT09_SF.netMx, PORT09_SF.clusterCoef, PORT09_SF.degreeCent$centralization,
                         PORT09_SF.netDensity, PORT09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
PORT09_TFg2 <- data.frame(PORT09_TF)
PORT09_TFg2 <- PORT09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_TFg2$player1
player2vector <- PORT09_TFg2$player2
PORT09_TFg3 <- PORT09_TFg2
PORT09_TFg3$p1inp2vec <- is.element(PORT09_TFg3$player1, player2vector)
PORT09_TFg3$p2inp1vec <- is.element(PORT09_TFg3$player2, player1vector)

addPlayer1 <- PORT09_TFg3[ which(PORT09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT09_TFg2 <- rbind(PORT09_TFg2, addPlayer1)

#ROUND 9, FWD Turnover graph using weighted edges
PORT09_TFft <- ftable(PORT09_TFg2$player1, PORT09_TFg2$player2)
PORT09_TFft2 <- as.matrix(PORT09_TFft)
numRows <- nrow(PORT09_TFft2)
numCols <- ncol(PORT09_TFft2)
PORT09_TFft3 <- PORT09_TFft2[c(2:numRows) , c(1:numCols)]
PORT09_TFTable <- graph.adjacency(PORT09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(PORT09_TFTable, vertex.label = V(PORT09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
PORT09_TF.clusterCoef <- transitivity(PORT09_TFTable, type="global") #cluster coefficient
PORT09_TF.degreeCent <- centralization.degree(PORT09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_TFftn <- as.network.matrix(PORT09_TFft)
PORT09_TF.netDensity <- network.density(PORT09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_TF.entropy <- entropy(PORT09_TFft) #entropy

PORT09_TF.netMx <- cbind(PORT09_TF.netMx, PORT09_TF.clusterCoef, PORT09_TF.degreeCent$centralization,
                         PORT09_TF.netDensity, PORT09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
PORT09_SAMg2 <- data.frame(PORT09_SAM)
PORT09_SAMg2 <- PORT09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_SAMg2$player1
player2vector <- PORT09_SAMg2$player2
PORT09_SAMg3 <- PORT09_SAMg2
PORT09_SAMg3$p1inp2vec <- is.element(PORT09_SAMg3$player1, player2vector)
PORT09_SAMg3$p2inp1vec <- is.element(PORT09_SAMg3$player2, player1vector)

addPlayer1 <- PORT09_SAMg3[ which(PORT09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_SAMg3[ which(PORT09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_SAMg2 <- rbind(PORT09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
PORT09_SAMft <- ftable(PORT09_SAMg2$player1, PORT09_SAMg2$player2)
PORT09_SAMft2 <- as.matrix(PORT09_SAMft)
numRows <- nrow(PORT09_SAMft2)
numCols <- ncol(PORT09_SAMft2)
PORT09_SAMft3 <- PORT09_SAMft2[c(2:numRows) , c(2:numCols)]
PORT09_SAMTable <- graph.adjacency(PORT09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(PORT09_SAMTable, vertex.label = V(PORT09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
PORT09_SAM.clusterCoef <- transitivity(PORT09_SAMTable, type="global") #cluster coefficient
PORT09_SAM.degreeCent <- centralization.degree(PORT09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_SAMftn <- as.network.matrix(PORT09_SAMft)
PORT09_SAM.netDensity <- network.density(PORT09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_SAM.entropy <- entropy(PORT09_SAMft) #entropy

PORT09_SAM.netMx <- cbind(PORT09_SAM.netMx, PORT09_SAM.clusterCoef, PORT09_SAM.degreeCent$centralization,
                          PORT09_SAM.netDensity, PORT09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
PORT09_TAMg2 <- data.frame(PORT09_TAM)
PORT09_TAMg2 <- PORT09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_TAMg2$player1
player2vector <- PORT09_TAMg2$player2
PORT09_TAMg3 <- PORT09_TAMg2
PORT09_TAMg3$p1inp2vec <- is.element(PORT09_TAMg3$player1, player2vector)
PORT09_TAMg3$p2inp1vec <- is.element(PORT09_TAMg3$player2, player1vector)

addPlayer1 <- PORT09_TAMg3[ which(PORT09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT09_TAMg2 <- rbind(PORT09_TAMg2, addPlayer1)

#ROUND 9, AM Turnover graph using weighted edges
PORT09_TAMft <- ftable(PORT09_TAMg2$player1, PORT09_TAMg2$player2)
PORT09_TAMft2 <- as.matrix(PORT09_TAMft)
numRows <- nrow(PORT09_TAMft2)
numCols <- ncol(PORT09_TAMft2)
PORT09_TAMft3 <- PORT09_TAMft2[c(2:numRows) , c(1:numCols)]
PORT09_TAMTable <- graph.adjacency(PORT09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(PORT09_TAMTable, vertex.label = V(PORT09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
PORT09_TAM.clusterCoef <- transitivity(PORT09_TAMTable, type="global") #cluster coefficient
PORT09_TAM.degreeCent <- centralization.degree(PORT09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_TAMftn <- as.network.matrix(PORT09_TAMft)
PORT09_TAM.netDensity <- network.density(PORT09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_TAM.entropy <- entropy(PORT09_TAMft) #entropy

PORT09_TAM.netMx <- cbind(PORT09_TAM.netMx, PORT09_TAM.clusterCoef, PORT09_TAM.degreeCent$centralization,
                          PORT09_TAM.netDensity, PORT09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
PORT09_SDMg2 <- data.frame(PORT09_SDM)
PORT09_SDMg2 <- PORT09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_SDMg2$player1
player2vector <- PORT09_SDMg2$player2
PORT09_SDMg3 <- PORT09_SDMg2
PORT09_SDMg3$p1inp2vec <- is.element(PORT09_SDMg3$player1, player2vector)
PORT09_SDMg3$p2inp1vec <- is.element(PORT09_SDMg3$player2, player1vector)

addPlayer1 <- PORT09_SDMg3[ which(PORT09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_SDMg3[ which(PORT09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_SDMg2 <- rbind(PORT09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
PORT09_SDMft <- ftable(PORT09_SDMg2$player1, PORT09_SDMg2$player2)
PORT09_SDMft2 <- as.matrix(PORT09_SDMft)
numRows <- nrow(PORT09_SDMft2)
numCols <- ncol(PORT09_SDMft2)
PORT09_SDMft3 <- PORT09_SDMft2[c(2:numRows) , c(2:numCols)]
PORT09_SDMTable <- graph.adjacency(PORT09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(PORT09_SDMTable, vertex.label = V(PORT09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
PORT09_SDM.clusterCoef <- transitivity(PORT09_SDMTable, type="global") #cluster coefficient
PORT09_SDM.degreeCent <- centralization.degree(PORT09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_SDMftn <- as.network.matrix(PORT09_SDMft)
PORT09_SDM.netDensity <- network.density(PORT09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_SDM.entropy <- entropy(PORT09_SDMft) #entropy

PORT09_SDM.netMx <- cbind(PORT09_SDM.netMx, PORT09_SDM.clusterCoef, PORT09_SDM.degreeCent$centralization,
                          PORT09_SDM.netDensity, PORT09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
PORT09_TDMg2 <- data.frame(PORT09_TDM)
PORT09_TDMg2 <- PORT09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_TDMg2$player1
player2vector <- PORT09_TDMg2$player2
PORT09_TDMg3 <- PORT09_TDMg2
PORT09_TDMg3$p1inp2vec <- is.element(PORT09_TDMg3$player1, player2vector)
PORT09_TDMg3$p2inp1vec <- is.element(PORT09_TDMg3$player2, player1vector)

addPlayer1 <- PORT09_TDMg3[ which(PORT09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_TDMg3[ which(PORT09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_TDMg2 <- rbind(PORT09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
PORT09_TDMft <- ftable(PORT09_TDMg2$player1, PORT09_TDMg2$player2)
PORT09_TDMft2 <- as.matrix(PORT09_TDMft)
numRows <- nrow(PORT09_TDMft2)
numCols <- ncol(PORT09_TDMft2)
PORT09_TDMft3 <- PORT09_TDMft2[c(2:numRows) , c(2:numCols)]
PORT09_TDMTable <- graph.adjacency(PORT09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(PORT09_TDMTable, vertex.label = V(PORT09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
PORT09_TDM.clusterCoef <- transitivity(PORT09_TDMTable, type="global") #cluster coefficient
PORT09_TDM.degreeCent <- centralization.degree(PORT09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_TDMftn <- as.network.matrix(PORT09_TDMft)
PORT09_TDM.netDensity <- network.density(PORT09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_TDM.entropy <- entropy(PORT09_TDMft) #entropy

PORT09_TDM.netMx <- cbind(PORT09_TDM.netMx, PORT09_TDM.clusterCoef, PORT09_TDM.degreeCent$centralization,
                          PORT09_TDM.netDensity, PORT09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
PORT09_SDg2 <- data.frame(PORT09_SD)
PORT09_SDg2 <- PORT09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_SDg2$player1
player2vector <- PORT09_SDg2$player2
PORT09_SDg3 <- PORT09_SDg2
PORT09_SDg3$p1inp2vec <- is.element(PORT09_SDg3$player1, player2vector)
PORT09_SDg3$p2inp1vec <- is.element(PORT09_SDg3$player2, player1vector)

addPlayer1 <- PORT09_SDg3[ which(PORT09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_SDg3[ which(PORT09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_SDg2 <- rbind(PORT09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
PORT09_SDft <- ftable(PORT09_SDg2$player1, PORT09_SDg2$player2)
PORT09_SDft2 <- as.matrix(PORT09_SDft)
numRows <- nrow(PORT09_SDft2)
numCols <- ncol(PORT09_SDft2)
PORT09_SDft3 <- PORT09_SDft2[c(2:numRows) , c(2:numCols)]
PORT09_SDTable <- graph.adjacency(PORT09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(PORT09_SDTable, vertex.label = V(PORT09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
PORT09_SD.clusterCoef <- transitivity(PORT09_SDTable, type="global") #cluster coefficient
PORT09_SD.degreeCent <- centralization.degree(PORT09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_SDftn <- as.network.matrix(PORT09_SDft)
PORT09_SD.netDensity <- network.density(PORT09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_SD.entropy <- entropy(PORT09_SDft) #entropy

PORT09_SD.netMx <- cbind(PORT09_SD.netMx, PORT09_SD.clusterCoef, PORT09_SD.degreeCent$centralization,
                         PORT09_SD.netDensity, PORT09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
PORT09_TDg2 <- data.frame(PORT09_TD)
PORT09_TDg2 <- PORT09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_TDg2$player1
player2vector <- PORT09_TDg2$player2
PORT09_TDg3 <- PORT09_TDg2
PORT09_TDg3$p1inp2vec <- is.element(PORT09_TDg3$player1, player2vector)
PORT09_TDg3$p2inp1vec <- is.element(PORT09_TDg3$player2, player1vector)

addPlayer1 <- PORT09_TDg3[ which(PORT09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_TDg3[ which(PORT09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_TDg2 <- rbind(PORT09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
PORT09_TDft <- ftable(PORT09_TDg2$player1, PORT09_TDg2$player2)
PORT09_TDft2 <- as.matrix(PORT09_TDft)
numRows <- nrow(PORT09_TDft2)
numCols <- ncol(PORT09_TDft2)
PORT09_TDft3 <- PORT09_TDft2[c(2:numRows) , c(2:numCols)]
PORT09_TDTable <- graph.adjacency(PORT09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(PORT09_TDTable, vertex.label = V(PORT09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
PORT09_TD.clusterCoef <- transitivity(PORT09_TDTable, type="global") #cluster coefficient
PORT09_TD.degreeCent <- centralization.degree(PORT09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_TDftn <- as.network.matrix(PORT09_TDft)
PORT09_TD.netDensity <- network.density(PORT09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_TD.entropy <- entropy(PORT09_TDft) #entropy

PORT09_TD.netMx <- cbind(PORT09_TD.netMx, PORT09_TD.clusterCoef, PORT09_TD.degreeCent$centralization,
                         PORT09_TD.netDensity, PORT09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
PORT09_QTg2 <- data.frame(PORT09_QT)
PORT09_QTg2 <- PORT09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT09_QTg2$player1
player2vector <- PORT09_QTg2$player2
PORT09_QTg3 <- PORT09_QTg2
PORT09_QTg3$p1inp2vec <- is.element(PORT09_QTg3$player1, player2vector)
PORT09_QTg3$p2inp1vec <- is.element(PORT09_QTg3$player2, player1vector)

addPlayer1 <- PORT09_QTg3[ which(PORT09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT09_QTg3[ which(PORT09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT09_QTg2 <- rbind(PORT09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
PORT09_QTft <- ftable(PORT09_QTg2$player1, PORT09_QTg2$player2)
PORT09_QTft2 <- as.matrix(PORT09_QTft)
numRows <- nrow(PORT09_QTft2)
numCols <- ncol(PORT09_QTft2)
PORT09_QTft3 <- PORT09_QTft2[c(2:numRows) , c(2:numCols)]
PORT09_QTTable <- graph.adjacency(PORT09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(PORT09_QTTable, vertex.label = V(PORT09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
PORT09_QT.clusterCoef <- transitivity(PORT09_QTTable, type="global") #cluster coefficient
PORT09_QT.degreeCent <- centralization.degree(PORT09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT09_QTftn <- as.network.matrix(PORT09_QTft)
PORT09_QT.netDensity <- network.density(PORT09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT09_QT.entropy <- entropy(PORT09_QTft) #entropy

PORT09_QT.netMx <- cbind(PORT09_QT.netMx, PORT09_QT.clusterCoef, PORT09_QT.degreeCent$centralization,
                         PORT09_QT.netDensity, PORT09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT09_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "RICH"
KIoutcome = "Goal_F"
RICH09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
RICH09_Gg2 <- data.frame(RICH09_G)
RICH09_Gg2 <- RICH09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_Gg2$player1
player2vector <- RICH09_Gg2$player2
RICH09_Gg3 <- RICH09_Gg2
RICH09_Gg3$p1inp2vec <- is.element(RICH09_Gg3$player1, player2vector)
RICH09_Gg3$p2inp1vec <- is.element(RICH09_Gg3$player2, player1vector)

addPlayer1 <- RICH09_Gg3[ which(RICH09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_Gg3[ which(RICH09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_Gg2 <- rbind(RICH09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
RICH09_Gft <- ftable(RICH09_Gg2$player1, RICH09_Gg2$player2)
RICH09_Gft2 <- as.matrix(RICH09_Gft)
numRows <- nrow(RICH09_Gft2)
numCols <- ncol(RICH09_Gft2)
RICH09_Gft3 <- RICH09_Gft2[c(2:numRows) , c(2:numCols)]
RICH09_GTable <- graph.adjacency(RICH09_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(RICH09_GTable, vertex.label = V(RICH09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
RICH09_G.clusterCoef <- transitivity(RICH09_GTable, type="global") #cluster coefficient
RICH09_G.degreeCent <- centralization.degree(RICH09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_Gftn <- as.network.matrix(RICH09_Gft)
RICH09_G.netDensity <- network.density(RICH09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_G.entropy <- entropy(RICH09_Gft) #entropy

RICH09_G.netMx <- cbind(RICH09_G.netMx, RICH09_G.clusterCoef, RICH09_G.degreeCent$centralization,
                        RICH09_G.netDensity, RICH09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************

round = 9
teamName = "RICH"
KIoutcome = "Behind_F"
RICH09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
RICH09_Bg2 <- data.frame(RICH09_B)
RICH09_Bg2 <- RICH09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_Bg2$player1
player2vector <- RICH09_Bg2$player2
RICH09_Bg3 <- RICH09_Bg2
RICH09_Bg3$p1inp2vec <- is.element(RICH09_Bg3$player1, player2vector)
RICH09_Bg3$p2inp1vec <- is.element(RICH09_Bg3$player2, player1vector)

addPlayer1 <- RICH09_Bg3[ which(RICH09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_Bg3[ which(RICH09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_Bg2 <- rbind(RICH09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
RICH09_Bft <- ftable(RICH09_Bg2$player1, RICH09_Bg2$player2)
RICH09_Bft2 <- as.matrix(RICH09_Bft)
numRows <- nrow(RICH09_Bft2)
numCols <- ncol(RICH09_Bft2)
RICH09_Bft3 <- RICH09_Bft2[c(2:numRows) , c(2:numCols)]
RICH09_BTable <- graph.adjacency(RICH09_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(RICH09_BTable, vertex.label = V(RICH09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
RICH09_B.clusterCoef <- transitivity(RICH09_BTable, type="global") #cluster coefficient
RICH09_B.degreeCent <- centralization.degree(RICH09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_Bftn <- as.network.matrix(RICH09_Bft)
RICH09_B.netDensity <- network.density(RICH09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_B.entropy <- entropy(RICH09_Bft) #entropy

RICH09_B.netMx <- cbind(RICH09_B.netMx, RICH09_B.clusterCoef, RICH09_B.degreeCent$centralization,
                        RICH09_B.netDensity, RICH09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
RICH09_SFg2 <- data.frame(RICH09_SF)
RICH09_SFg2 <- RICH09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_SFg2$player1
player2vector <- RICH09_SFg2$player2
RICH09_SFg3 <- RICH09_SFg2
RICH09_SFg3$p1inp2vec <- is.element(RICH09_SFg3$player1, player2vector)
RICH09_SFg3$p2inp1vec <- is.element(RICH09_SFg3$player2, player1vector)

addPlayer1 <- RICH09_SFg3[ which(RICH09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_SFg3[ which(RICH09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_SFg2 <- rbind(RICH09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
RICH09_SFft <- ftable(RICH09_SFg2$player1, RICH09_SFg2$player2)
RICH09_SFft2 <- as.matrix(RICH09_SFft)
numRows <- nrow(RICH09_SFft2)
numCols <- ncol(RICH09_SFft2)
RICH09_SFft3 <- RICH09_SFft2[c(2:numRows) , c(2:numCols)]
RICH09_SFTable <- graph.adjacency(RICH09_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(RICH09_SFTable, vertex.label = V(RICH09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
RICH09_SF.clusterCoef <- transitivity(RICH09_SFTable, type="global") #cluster coefficient
RICH09_SF.degreeCent <- centralization.degree(RICH09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_SFftn <- as.network.matrix(RICH09_SFft)
RICH09_SF.netDensity <- network.density(RICH09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_SF.entropy <- entropy(RICH09_SFft) #entropy

RICH09_SF.netMx <- cbind(RICH09_SF.netMx, RICH09_SF.clusterCoef, RICH09_SF.degreeCent$centralization,
                         RICH09_SF.netDensity, RICH09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
RICH09_TFg2 <- data.frame(RICH09_TF)
RICH09_TFg2 <- RICH09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_TFg2$player1
player2vector <- RICH09_TFg2$player2
RICH09_TFg3 <- RICH09_TFg2
RICH09_TFg3$p1inp2vec <- is.element(RICH09_TFg3$player1, player2vector)
RICH09_TFg3$p2inp1vec <- is.element(RICH09_TFg3$player2, player1vector)

addPlayer1 <- RICH09_TFg3[ which(RICH09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH09_TFg3[ which(RICH09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_TFg2 <- rbind(RICH09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
RICH09_TFft <- ftable(RICH09_TFg2$player1, RICH09_TFg2$player2)
RICH09_TFft2 <- as.matrix(RICH09_TFft)
numRows <- nrow(RICH09_TFft2)
numCols <- ncol(RICH09_TFft2)
RICH09_TFft3 <- RICH09_TFft2[c(2:numRows) , c(2:numCols)]
RICH09_TFTable <- graph.adjacency(RICH09_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(RICH09_TFTable, vertex.label = V(RICH09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
RICH09_TF.clusterCoef <- transitivity(RICH09_TFTable, type="global") #cluster coefficient
RICH09_TF.degreeCent <- centralization.degree(RICH09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_TFftn <- as.network.matrix(RICH09_TFft)
RICH09_TF.netDensity <- network.density(RICH09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_TF.entropy <- entropy(RICH09_TFft) #entropy

RICH09_TF.netMx <- cbind(RICH09_TF.netMx, RICH09_TF.clusterCoef, RICH09_TF.degreeCent$centralization,
                         RICH09_TF.netDensity, RICH09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
RICH09_SAMg2 <- data.frame(RICH09_SAM)
RICH09_SAMg2 <- RICH09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_SAMg2$player1
player2vector <- RICH09_SAMg2$player2
RICH09_SAMg3 <- RICH09_SAMg2
RICH09_SAMg3$p1inp2vec <- is.element(RICH09_SAMg3$player1, player2vector)
RICH09_SAMg3$p2inp1vec <- is.element(RICH09_SAMg3$player2, player1vector)

addPlayer1 <- RICH09_SAMg3[ which(RICH09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_SAMg3[ which(RICH09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_SAMg2 <- rbind(RICH09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
RICH09_SAMft <- ftable(RICH09_SAMg2$player1, RICH09_SAMg2$player2)
RICH09_SAMft2 <- as.matrix(RICH09_SAMft)
numRows <- nrow(RICH09_SAMft2)
numCols <- ncol(RICH09_SAMft2)
RICH09_SAMft3 <- RICH09_SAMft2[c(2:numRows) , c(2:numCols)]
RICH09_SAMTable <- graph.adjacency(RICH09_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(RICH09_SAMTable, vertex.label = V(RICH09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
RICH09_SAM.clusterCoef <- transitivity(RICH09_SAMTable, type="global") #cluster coefficient
RICH09_SAM.degreeCent <- centralization.degree(RICH09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_SAMftn <- as.network.matrix(RICH09_SAMft)
RICH09_SAM.netDensity <- network.density(RICH09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_SAM.entropy <- entropy(RICH09_SAMft) #entropy

RICH09_SAM.netMx <- cbind(RICH09_SAM.netMx, RICH09_SAM.clusterCoef, RICH09_SAM.degreeCent$centralization,
                          RICH09_SAM.netDensity, RICH09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
RICH09_TAMg2 <- data.frame(RICH09_TAM)
RICH09_TAMg2 <- RICH09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_TAMg2$player1
player2vector <- RICH09_TAMg2$player2
RICH09_TAMg3 <- RICH09_TAMg2
RICH09_TAMg3$p1inp2vec <- is.element(RICH09_TAMg3$player1, player2vector)
RICH09_TAMg3$p2inp1vec <- is.element(RICH09_TAMg3$player2, player1vector)

addPlayer1 <- RICH09_TAMg3[ which(RICH09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_TAMg3[ which(RICH09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_TAMg2 <- rbind(RICH09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
RICH09_TAMft <- ftable(RICH09_TAMg2$player1, RICH09_TAMg2$player2)
RICH09_TAMft2 <- as.matrix(RICH09_TAMft)
numRows <- nrow(RICH09_TAMft2)
numCols <- ncol(RICH09_TAMft2)
RICH09_TAMft3 <- RICH09_TAMft2[c(2:numRows) , c(2:numCols)]
RICH09_TAMTable <- graph.adjacency(RICH09_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(RICH09_TAMTable, vertex.label = V(RICH09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
RICH09_TAM.clusterCoef <- transitivity(RICH09_TAMTable, type="global") #cluster coefficient
RICH09_TAM.degreeCent <- centralization.degree(RICH09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_TAMftn <- as.network.matrix(RICH09_TAMft)
RICH09_TAM.netDensity <- network.density(RICH09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_TAM.entropy <- entropy(RICH09_TAMft) #entropy

RICH09_TAM.netMx <- cbind(RICH09_TAM.netMx, RICH09_TAM.clusterCoef, RICH09_TAM.degreeCent$centralization,
                          RICH09_TAM.netDensity, RICH09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
RICH09_SDMg2 <- data.frame(RICH09_SDM)
RICH09_SDMg2 <- RICH09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_SDMg2$player1
player2vector <- RICH09_SDMg2$player2
RICH09_SDMg3 <- RICH09_SDMg2
RICH09_SDMg3$p1inp2vec <- is.element(RICH09_SDMg3$player1, player2vector)
RICH09_SDMg3$p2inp1vec <- is.element(RICH09_SDMg3$player2, player1vector)

addPlayer1 <- RICH09_SDMg3[ which(RICH09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_SDMg3[ which(RICH09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_SDMg2 <- rbind(RICH09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
RICH09_SDMft <- ftable(RICH09_SDMg2$player1, RICH09_SDMg2$player2)
RICH09_SDMft2 <- as.matrix(RICH09_SDMft)
numRows <- nrow(RICH09_SDMft2)
numCols <- ncol(RICH09_SDMft2)
RICH09_SDMft3 <- RICH09_SDMft2[c(2:numRows) , c(2:numCols)]
RICH09_SDMTable <- graph.adjacency(RICH09_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(RICH09_SDMTable, vertex.label = V(RICH09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
RICH09_SDM.clusterCoef <- transitivity(RICH09_SDMTable, type="global") #cluster coefficient
RICH09_SDM.degreeCent <- centralization.degree(RICH09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_SDMftn <- as.network.matrix(RICH09_SDMft)
RICH09_SDM.netDensity <- network.density(RICH09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_SDM.entropy <- entropy(RICH09_SDMft) #entropy

RICH09_SDM.netMx <- cbind(RICH09_SDM.netMx, RICH09_SDM.clusterCoef, RICH09_SDM.degreeCent$centralization,
                          RICH09_SDM.netDensity, RICH09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
RICH09_TDMg2 <- data.frame(RICH09_TDM)
RICH09_TDMg2 <- RICH09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_TDMg2$player1
player2vector <- RICH09_TDMg2$player2
RICH09_TDMg3 <- RICH09_TDMg2
RICH09_TDMg3$p1inp2vec <- is.element(RICH09_TDMg3$player1, player2vector)
RICH09_TDMg3$p2inp1vec <- is.element(RICH09_TDMg3$player2, player1vector)

addPlayer1 <- RICH09_TDMg3[ which(RICH09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH09_TDMg3[ which(RICH09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_TDMg2 <- rbind(RICH09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
RICH09_TDMft <- ftable(RICH09_TDMg2$player1, RICH09_TDMg2$player2)
RICH09_TDMft2 <- as.matrix(RICH09_TDMft)
numRows <- nrow(RICH09_TDMft2)
numCols <- ncol(RICH09_TDMft2)
RICH09_TDMft3 <- RICH09_TDMft2[c(2:numRows) , c(2:numCols)]
RICH09_TDMTable <- graph.adjacency(RICH09_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(RICH09_TDMTable, vertex.label = V(RICH09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
RICH09_TDM.clusterCoef <- transitivity(RICH09_TDMTable, type="global") #cluster coefficient
RICH09_TDM.degreeCent <- centralization.degree(RICH09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_TDMftn <- as.network.matrix(RICH09_TDMft)
RICH09_TDM.netDensity <- network.density(RICH09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_TDM.entropy <- entropy(RICH09_TDMft) #entropy

RICH09_TDM.netMx <- cbind(RICH09_TDM.netMx, RICH09_TDM.clusterCoef, RICH09_TDM.degreeCent$centralization,
                          RICH09_TDM.netDensity, RICH09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
RICH09_SDg2 <- data.frame(RICH09_SD)
RICH09_SDg2 <- RICH09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_SDg2$player1
player2vector <- RICH09_SDg2$player2
RICH09_SDg3 <- RICH09_SDg2
RICH09_SDg3$p1inp2vec <- is.element(RICH09_SDg3$player1, player2vector)
RICH09_SDg3$p2inp1vec <- is.element(RICH09_SDg3$player2, player1vector)

addPlayer1 <- RICH09_SDg3[ which(RICH09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_SDg3[ which(RICH09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_SDg2 <- rbind(RICH09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
RICH09_SDft <- ftable(RICH09_SDg2$player1, RICH09_SDg2$player2)
RICH09_SDft2 <- as.matrix(RICH09_SDft)
numRows <- nrow(RICH09_SDft2)
numCols <- ncol(RICH09_SDft2)
RICH09_SDft3 <- RICH09_SDft2[c(2:numRows) , c(2:numCols)]
RICH09_SDTable <- graph.adjacency(RICH09_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(RICH09_SDTable, vertex.label = V(RICH09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
RICH09_SD.clusterCoef <- transitivity(RICH09_SDTable, type="global") #cluster coefficient
RICH09_SD.degreeCent <- centralization.degree(RICH09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_SDftn <- as.network.matrix(RICH09_SDft)
RICH09_SD.netDensity <- network.density(RICH09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_SD.entropy <- entropy(RICH09_SDft) #entropy

RICH09_SD.netMx <- cbind(RICH09_SD.netMx, RICH09_SD.clusterCoef, RICH09_SD.degreeCent$centralization,
                         RICH09_SD.netDensity, RICH09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
RICH09_TDg2 <- data.frame(RICH09_TD)
RICH09_TDg2 <- RICH09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_TDg2$player1
player2vector <- RICH09_TDg2$player2
RICH09_TDg3 <- RICH09_TDg2
RICH09_TDg3$p1inp2vec <- is.element(RICH09_TDg3$player1, player2vector)
RICH09_TDg3$p2inp1vec <- is.element(RICH09_TDg3$player2, player1vector)

addPlayer1 <- RICH09_TDg3[ which(RICH09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_TDg3[ which(RICH09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_TDg2 <- rbind(RICH09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
RICH09_TDft <- ftable(RICH09_TDg2$player1, RICH09_TDg2$player2)
RICH09_TDft2 <- as.matrix(RICH09_TDft)
numRows <- nrow(RICH09_TDft2)
numCols <- ncol(RICH09_TDft2)
RICH09_TDft3 <- RICH09_TDft2[c(2:numRows) , c(2:numCols)]
RICH09_TDTable <- graph.adjacency(RICH09_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(RICH09_TDTable, vertex.label = V(RICH09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
RICH09_TD.clusterCoef <- transitivity(RICH09_TDTable, type="global") #cluster coefficient
RICH09_TD.degreeCent <- centralization.degree(RICH09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_TDftn <- as.network.matrix(RICH09_TDft)
RICH09_TD.netDensity <- network.density(RICH09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_TD.entropy <- entropy(RICH09_TDft) #entropy

RICH09_TD.netMx <- cbind(RICH09_TD.netMx, RICH09_TD.clusterCoef, RICH09_TD.degreeCent$centralization,
                         RICH09_TD.netDensity, RICH09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
RICH09_QTg2 <- data.frame(RICH09_QT)
RICH09_QTg2 <- RICH09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH09_QTg2$player1
player2vector <- RICH09_QTg2$player2
RICH09_QTg3 <- RICH09_QTg2
RICH09_QTg3$p1inp2vec <- is.element(RICH09_QTg3$player1, player2vector)
RICH09_QTg3$p2inp1vec <- is.element(RICH09_QTg3$player2, player1vector)

addPlayer1 <- RICH09_QTg3[ which(RICH09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH09_QTg3[ which(RICH09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH09_QTg2 <- rbind(RICH09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
RICH09_QTft <- ftable(RICH09_QTg2$player1, RICH09_QTg2$player2)
RICH09_QTft2 <- as.matrix(RICH09_QTft)
numRows <- nrow(RICH09_QTft2)
numCols <- ncol(RICH09_QTft2)
RICH09_QTft3 <- RICH09_QTft2[c(2:numRows) , c(2:numCols)]
RICH09_QTTable <- graph.adjacency(RICH09_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(RICH09_QTTable, vertex.label = V(RICH09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
RICH09_QT.clusterCoef <- transitivity(RICH09_QTTable, type="global") #cluster coefficient
RICH09_QT.degreeCent <- centralization.degree(RICH09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH09_QTftn <- as.network.matrix(RICH09_QTft)
RICH09_QT.netDensity <- network.density(RICH09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH09_QT.entropy <- entropy(RICH09_QTft) #entropy

RICH09_QT.netMx <- cbind(RICH09_QT.netMx, RICH09_QT.clusterCoef, RICH09_QT.degreeCent$centralization,
                         RICH09_QT.netDensity, RICH09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH09_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************

round = 9
teamName = "STK"
KIoutcome = "Goal_F"
STK09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
STK09_Gg2 <- data.frame(STK09_G)
STK09_Gg2 <- STK09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_Gg2$player1
player2vector <- STK09_Gg2$player2
STK09_Gg3 <- STK09_Gg2
STK09_Gg3$p1inp2vec <- is.element(STK09_Gg3$player1, player2vector)
STK09_Gg3$p2inp1vec <- is.element(STK09_Gg3$player2, player1vector)

addPlayer1 <- STK09_Gg3[ which(STK09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK09_Gg3[ which(STK09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_Gg2 <- rbind(STK09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
STK09_Gft <- ftable(STK09_Gg2$player1, STK09_Gg2$player2)
STK09_Gft2 <- as.matrix(STK09_Gft)
numRows <- nrow(STK09_Gft2)
numCols <- ncol(STK09_Gft2)
STK09_Gft3 <- STK09_Gft2[c(2:numRows) , c(2:numCols)]
STK09_GTable <- graph.adjacency(STK09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(STK09_GTable, vertex.label = V(STK09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
STK09_G.clusterCoef <- transitivity(STK09_GTable, type="global") #cluster coefficient
STK09_G.degreeCent <- centralization.degree(STK09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_Gftn <- as.network.matrix(STK09_Gft)
STK09_G.netDensity <- network.density(STK09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_G.entropy <- entropy(STK09_Gft) #entropy

STK09_G.netMx <- cbind(STK09_G.netMx, STK09_G.clusterCoef, STK09_G.degreeCent$centralization,
                       STK09_G.netDensity, STK09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "Behind_F"
STK09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
STK09_Bg2 <- data.frame(STK09_B)
STK09_Bg2 <- STK09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_Bg2$player1
player2vector <- STK09_Bg2$player2
STK09_Bg3 <- STK09_Bg2
STK09_Bg3$p1inp2vec <- is.element(STK09_Bg3$player1, player2vector)
STK09_Bg3$p2inp1vec <- is.element(STK09_Bg3$player2, player1vector)

addPlayer1 <- STK09_Bg3[ which(STK09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_Bg3[ which(STK09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_Bg2 <- rbind(STK09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
STK09_Bft <- ftable(STK09_Bg2$player1, STK09_Bg2$player2)
STK09_Bft2 <- as.matrix(STK09_Bft)
numRows <- nrow(STK09_Bft2)
numCols <- ncol(STK09_Bft2)
STK09_Bft3 <- STK09_Bft2[c(2:numRows) , c(2:numCols)]
STK09_BTable <- graph.adjacency(STK09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(STK09_BTable, vertex.label = V(STK09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
STK09_B.clusterCoef <- transitivity(STK09_BTable, type="global") #cluster coefficient
STK09_B.degreeCent <- centralization.degree(STK09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_Bftn <- as.network.matrix(STK09_Bft)
STK09_B.netDensity <- network.density(STK09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_B.entropy <- entropy(STK09_Bft) #entropy

STK09_B.netMx <- cbind(STK09_B.netMx, STK09_B.clusterCoef, STK09_B.degreeCent$centralization,
                       STK09_B.netDensity, STK09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "Stoppage_F"
STK09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
STK09_SFg2 <- data.frame(STK09_SF)
STK09_SFg2 <- STK09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_SFg2$player1
player2vector <- STK09_SFg2$player2
STK09_SFg3 <- STK09_SFg2
STK09_SFg3$p1inp2vec <- is.element(STK09_SFg3$player1, player2vector)
STK09_SFg3$p2inp1vec <- is.element(STK09_SFg3$player2, player1vector)

addPlayer1 <- STK09_SFg3[ which(STK09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_SFg3[ which(STK09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_SFg2 <- rbind(STK09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
STK09_SFft <- ftable(STK09_SFg2$player1, STK09_SFg2$player2)
STK09_SFft2 <- as.matrix(STK09_SFft)
numRows <- nrow(STK09_SFft2)
numCols <- ncol(STK09_SFft2)
STK09_SFft3 <- STK09_SFft2[c(2:numRows) , c(2:numCols)]
STK09_SFTable <- graph.adjacency(STK09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(STK09_SFTable, vertex.label = V(STK09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
STK09_SF.clusterCoef <- transitivity(STK09_SFTable, type="global") #cluster coefficient
STK09_SF.degreeCent <- centralization.degree(STK09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_SFftn <- as.network.matrix(STK09_SFft)
STK09_SF.netDensity <- network.density(STK09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_SF.entropy <- entropy(STK09_SFft) #entropy

STK09_SF.netMx <- cbind(STK09_SF.netMx, STK09_SF.clusterCoef, STK09_SF.degreeCent$centralization,
                        STK09_SF.netDensity, STK09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "Turnover_F"
STK09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
STK09_TFg2 <- data.frame(STK09_TF)
STK09_TFg2 <- STK09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_TFg2$player1
player2vector <- STK09_TFg2$player2
STK09_TFg3 <- STK09_TFg2
STK09_TFg3$p1inp2vec <- is.element(STK09_TFg3$player1, player2vector)
STK09_TFg3$p2inp1vec <- is.element(STK09_TFg3$player2, player1vector)

addPlayer1 <- STK09_TFg3[ which(STK09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_TFg3[ which(STK09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_TFg2 <- rbind(STK09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
STK09_TFft <- ftable(STK09_TFg2$player1, STK09_TFg2$player2)
STK09_TFft2 <- as.matrix(STK09_TFft)
numRows <- nrow(STK09_TFft2)
numCols <- ncol(STK09_TFft2)
STK09_TFft3 <- STK09_TFft2[c(2:numRows) , c(2:numCols)]
STK09_TFTable <- graph.adjacency(STK09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(STK09_TFTable, vertex.label = V(STK09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
STK09_TF.clusterCoef <- transitivity(STK09_TFTable, type="global") #cluster coefficient
STK09_TF.degreeCent <- centralization.degree(STK09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_TFftn <- as.network.matrix(STK09_TFft)
STK09_TF.netDensity <- network.density(STK09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_TF.entropy <- entropy(STK09_TFft) #entropy

STK09_TF.netMx <- cbind(STK09_TF.netMx, STK09_TF.clusterCoef, STK09_TF.degreeCent$centralization,
                        STK09_TF.netDensity, STK09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
STK09_SAMg2 <- data.frame(STK09_SAM)
STK09_SAMg2 <- STK09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_SAMg2$player1
player2vector <- STK09_SAMg2$player2
STK09_SAMg3 <- STK09_SAMg2
STK09_SAMg3$p1inp2vec <- is.element(STK09_SAMg3$player1, player2vector)
STK09_SAMg3$p2inp1vec <- is.element(STK09_SAMg3$player2, player1vector)

addPlayer1 <- STK09_SAMg3[ which(STK09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_SAMg3[ which(STK09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_SAMg2 <- rbind(STK09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
STK09_SAMft <- ftable(STK09_SAMg2$player1, STK09_SAMg2$player2)
STK09_SAMft2 <- as.matrix(STK09_SAMft)
numRows <- nrow(STK09_SAMft2)
numCols <- ncol(STK09_SAMft2)
STK09_SAMft3 <- STK09_SAMft2[c(2:numRows) , c(2:numCols)]
STK09_SAMTable <- graph.adjacency(STK09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(STK09_SAMTable, vertex.label = V(STK09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
STK09_SAM.clusterCoef <- transitivity(STK09_SAMTable, type="global") #cluster coefficient
STK09_SAM.degreeCent <- centralization.degree(STK09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_SAMftn <- as.network.matrix(STK09_SAMft)
STK09_SAM.netDensity <- network.density(STK09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_SAM.entropy <- entropy(STK09_SAMft) #entropy

STK09_SAM.netMx <- cbind(STK09_SAM.netMx, STK09_SAM.clusterCoef, STK09_SAM.degreeCent$centralization,
                         STK09_SAM.netDensity, STK09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "STK"
KIoutcome = "Turnover_AM"
STK09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
STK09_TAMg2 <- data.frame(STK09_TAM)
STK09_TAMg2 <- STK09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_TAMg2$player1
player2vector <- STK09_TAMg2$player2
STK09_TAMg3 <- STK09_TAMg2
STK09_TAMg3$p1inp2vec <- is.element(STK09_TAMg3$player1, player2vector)
STK09_TAMg3$p2inp1vec <- is.element(STK09_TAMg3$player2, player1vector)

addPlayer1 <- STK09_TAMg3[ which(STK09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK09_TAMg3[ which(STK09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_TAMg2 <- rbind(STK09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
STK09_TAMft <- ftable(STK09_TAMg2$player1, STK09_TAMg2$player2)
STK09_TAMft2 <- as.matrix(STK09_TAMft)
numRows <- nrow(STK09_TAMft2)
numCols <- ncol(STK09_TAMft2)
STK09_TAMft3 <- STK09_TAMft2[c(2:numRows) , c(2:numCols)]
STK09_TAMTable <- graph.adjacency(STK09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(STK09_TAMTable, vertex.label = V(STK09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
STK09_TAM.clusterCoef <- transitivity(STK09_TAMTable, type="global") #cluster coefficient
STK09_TAM.degreeCent <- centralization.degree(STK09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_TAMftn <- as.network.matrix(STK09_TAMft)
STK09_TAM.netDensity <- network.density(STK09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_TAM.entropy <- entropy(STK09_TAMft) #entropy

STK09_TAM.netMx <- cbind(STK09_TAM.netMx, STK09_TAM.clusterCoef, STK09_TAM.degreeCent$centralization,
                         STK09_TAM.netDensity, STK09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
STK09_SDMg2 <- data.frame(STK09_SDM)
STK09_SDMg2 <- STK09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_SDMg2$player1
player2vector <- STK09_SDMg2$player2
STK09_SDMg3 <- STK09_SDMg2
STK09_SDMg3$p1inp2vec <- is.element(STK09_SDMg3$player1, player2vector)
STK09_SDMg3$p2inp1vec <- is.element(STK09_SDMg3$player2, player1vector)

addPlayer1 <- STK09_SDMg3[ which(STK09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_SDMg3[ which(STK09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_SDMg2 <- rbind(STK09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
STK09_SDMft <- ftable(STK09_SDMg2$player1, STK09_SDMg2$player2)
STK09_SDMft2 <- as.matrix(STK09_SDMft)
numRows <- nrow(STK09_SDMft2)
numCols <- ncol(STK09_SDMft2)
STK09_SDMft3 <- STK09_SDMft2[c(2:numRows) , c(2:numCols)]
STK09_SDMTable <- graph.adjacency(STK09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(STK09_SDMTable, vertex.label = V(STK09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
STK09_SDM.clusterCoef <- transitivity(STK09_SDMTable, type="global") #cluster coefficient
STK09_SDM.degreeCent <- centralization.degree(STK09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_SDMftn <- as.network.matrix(STK09_SDMft)
STK09_SDM.netDensity <- network.density(STK09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_SDM.entropy <- entropy(STK09_SDMft) #entropy

STK09_SDM.netMx <- cbind(STK09_SDM.netMx, STK09_SDM.clusterCoef, STK09_SDM.degreeCent$centralization,
                         STK09_SDM.netDensity, STK09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "STK"
KIoutcome = "Turnover_DM"
STK09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
STK09_TDMg2 <- data.frame(STK09_TDM)
STK09_TDMg2 <- STK09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_TDMg2$player1
player2vector <- STK09_TDMg2$player2
STK09_TDMg3 <- STK09_TDMg2
STK09_TDMg3$p1inp2vec <- is.element(STK09_TDMg3$player1, player2vector)
STK09_TDMg3$p2inp1vec <- is.element(STK09_TDMg3$player2, player1vector)

addPlayer1 <- STK09_TDMg3[ which(STK09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_TDMg3[ which(STK09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_TDMg2 <- rbind(STK09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
STK09_TDMft <- ftable(STK09_TDMg2$player1, STK09_TDMg2$player2)
STK09_TDMft2 <- as.matrix(STK09_TDMft)
numRows <- nrow(STK09_TDMft2)
numCols <- ncol(STK09_TDMft2)
STK09_TDMft3 <- STK09_TDMft2[c(2:numRows) , c(2:numCols)]
STK09_TDMTable <- graph.adjacency(STK09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(STK09_TDMTable, vertex.label = V(STK09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
STK09_TDM.clusterCoef <- transitivity(STK09_TDMTable, type="global") #cluster coefficient
STK09_TDM.degreeCent <- centralization.degree(STK09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_TDMftn <- as.network.matrix(STK09_TDMft)
STK09_TDM.netDensity <- network.density(STK09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_TDM.entropy <- entropy(STK09_TDMft) #entropy

STK09_TDM.netMx <- cbind(STK09_TDM.netMx, STK09_TDM.clusterCoef, STK09_TDM.degreeCent$centralization,
                         STK09_TDM.netDensity, STK09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "Stoppage_D"
STK09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
STK09_SDg2 <- data.frame(STK09_SD)
STK09_SDg2 <- STK09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_SDg2$player1
player2vector <- STK09_SDg2$player2
STK09_SDg3 <- STK09_SDg2
STK09_SDg3$p1inp2vec <- is.element(STK09_SDg3$player1, player2vector)
STK09_SDg3$p2inp1vec <- is.element(STK09_SDg3$player2, player1vector)

addPlayer1 <- STK09_SDg3[ which(STK09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_SDg3[ which(STK09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_SDg2 <- rbind(STK09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
STK09_SDft <- ftable(STK09_SDg2$player1, STK09_SDg2$player2)
STK09_SDft2 <- as.matrix(STK09_SDft)
numRows <- nrow(STK09_SDft2)
numCols <- ncol(STK09_SDft2)
STK09_SDft3 <- STK09_SDft2[c(2:numRows) , c(2:numCols)]
STK09_SDTable <- graph.adjacency(STK09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(STK09_SDTable, vertex.label = V(STK09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
STK09_SD.clusterCoef <- transitivity(STK09_SDTable, type="global") #cluster coefficient
STK09_SD.degreeCent <- centralization.degree(STK09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_SDftn <- as.network.matrix(STK09_SDft)
STK09_SD.netDensity <- network.density(STK09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_SD.entropy <- entropy(STK09_SDft) #entropy

STK09_SD.netMx <- cbind(STK09_SD.netMx, STK09_SD.clusterCoef, STK09_SD.degreeCent$centralization,
                        STK09_SD.netDensity, STK09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************

round = 9
teamName = "STK"
KIoutcome = "Turnover_D"
STK09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
STK09_TDg2 <- data.frame(STK09_TD)
STK09_TDg2 <- STK09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_TDg2$player1
player2vector <- STK09_TDg2$player2
STK09_TDg3 <- STK09_TDg2
STK09_TDg3$p1inp2vec <- is.element(STK09_TDg3$player1, player2vector)
STK09_TDg3$p2inp1vec <- is.element(STK09_TDg3$player2, player1vector)

addPlayer1 <- STK09_TDg3[ which(STK09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_TDg3[ which(STK09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_TDg2 <- rbind(STK09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
STK09_TDft <- ftable(STK09_TDg2$player1, STK09_TDg2$player2)
STK09_TDft2 <- as.matrix(STK09_TDft)
numRows <- nrow(STK09_TDft2)
numCols <- ncol(STK09_TDft2)
STK09_TDft3 <- STK09_TDft2[c(2:numRows) , c(2:numCols)]
STK09_TDTable <- graph.adjacency(STK09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(STK09_TDTable, vertex.label = V(STK09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
STK09_TD.clusterCoef <- transitivity(STK09_TDTable, type="global") #cluster coefficient
STK09_TD.degreeCent <- centralization.degree(STK09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_TDftn <- as.network.matrix(STK09_TDft)
STK09_TD.netDensity <- network.density(STK09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_TD.entropy <- entropy(STK09_TDft) #entropy

STK09_TD.netMx <- cbind(STK09_TD.netMx, STK09_TD.clusterCoef, STK09_TD.degreeCent$centralization,
                        STK09_TD.netDensity, STK09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
STK09_QTg2 <- data.frame(STK09_QT)
STK09_QTg2 <- STK09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK09_QTg2$player1
player2vector <- STK09_QTg2$player2
STK09_QTg3 <- STK09_QTg2
STK09_QTg3$p1inp2vec <- is.element(STK09_QTg3$player1, player2vector)
STK09_QTg3$p2inp1vec <- is.element(STK09_QTg3$player2, player1vector)

addPlayer1 <- STK09_QTg3[ which(STK09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK09_QTg3[ which(STK09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK09_QTg2 <- rbind(STK09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
STK09_QTft <- ftable(STK09_QTg2$player1, STK09_QTg2$player2)
STK09_QTft2 <- as.matrix(STK09_QTft)
numRows <- nrow(STK09_QTft2)
numCols <- ncol(STK09_QTft2)
STK09_QTft3 <- STK09_QTft2[c(2:numRows) , c(2:numCols)]
STK09_QTTable <- graph.adjacency(STK09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(STK09_QTTable, vertex.label = V(STK09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
STK09_QT.clusterCoef <- transitivity(STK09_QTTable, type="global") #cluster coefficient
STK09_QT.degreeCent <- centralization.degree(STK09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK09_QTftn <- as.network.matrix(STK09_QTft)
STK09_QT.netDensity <- network.density(STK09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK09_QT.entropy <- entropy(STK09_QTft) #entropy

STK09_QT.netMx <- cbind(STK09_QT.netMx, STK09_QT.clusterCoef, STK09_QT.degreeCent$centralization,
                        STK09_QT.netDensity, STK09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK09_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "SYD"
KIoutcome = "Goal_F"
SYD09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
SYD09_Gg2 <- data.frame(SYD09_G)
SYD09_Gg2 <- SYD09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_Gg2$player1
player2vector <- SYD09_Gg2$player2
SYD09_Gg3 <- SYD09_Gg2
SYD09_Gg3$p1inp2vec <- is.element(SYD09_Gg3$player1, player2vector)
SYD09_Gg3$p2inp1vec <- is.element(SYD09_Gg3$player2, player1vector)

addPlayer1 <- SYD09_Gg3[ which(SYD09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_Gg3[ which(SYD09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_Gg2 <- rbind(SYD09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
SYD09_Gft <- ftable(SYD09_Gg2$player1, SYD09_Gg2$player2)
SYD09_Gft2 <- as.matrix(SYD09_Gft)
numRows <- nrow(SYD09_Gft2)
numCols <- ncol(SYD09_Gft2)
SYD09_Gft3 <- SYD09_Gft2[c(2:numRows) , c(2:numCols)]
SYD09_GTable <- graph.adjacency(SYD09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(SYD09_GTable, vertex.label = V(SYD09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
SYD09_G.clusterCoef <- transitivity(SYD09_GTable, type="global") #cluster coefficient
SYD09_G.degreeCent <- centralization.degree(SYD09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_Gftn <- as.network.matrix(SYD09_Gft)
SYD09_G.netDensity <- network.density(SYD09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_G.entropy <- entropy(SYD09_Gft) #entropy

SYD09_G.netMx <- cbind(SYD09_G.netMx, SYD09_G.clusterCoef, SYD09_G.degreeCent$centralization,
                       SYD09_G.netDensity, SYD09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "SYD"
KIoutcome = "Behind_F"
SYD09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
SYD09_Bg2 <- data.frame(SYD09_B)
SYD09_Bg2 <- SYD09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_Bg2$player1
player2vector <- SYD09_Bg2$player2
SYD09_Bg3 <- SYD09_Bg2
SYD09_Bg3$p1inp2vec <- is.element(SYD09_Bg3$player1, player2vector)
SYD09_Bg3$p2inp1vec <- is.element(SYD09_Bg3$player2, player1vector)

addPlayer1 <- SYD09_Bg3[ which(SYD09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_Bg3[ which(SYD09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_Bg2 <- rbind(SYD09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
SYD09_Bft <- ftable(SYD09_Bg2$player1, SYD09_Bg2$player2)
SYD09_Bft2 <- as.matrix(SYD09_Bft)
numRows <- nrow(SYD09_Bft2)
numCols <- ncol(SYD09_Bft2)
SYD09_Bft3 <- SYD09_Bft2[c(2:numRows) , c(2:numCols)]
SYD09_BTable <- graph.adjacency(SYD09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(SYD09_BTable, vertex.label = V(SYD09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
SYD09_B.clusterCoef <- transitivity(SYD09_BTable, type="global") #cluster coefficient
SYD09_B.degreeCent <- centralization.degree(SYD09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_Bftn <- as.network.matrix(SYD09_Bft)
SYD09_B.netDensity <- network.density(SYD09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_B.entropy <- entropy(SYD09_Bft) #entropy

SYD09_B.netMx <- cbind(SYD09_B.netMx, SYD09_B.clusterCoef, SYD09_B.degreeCent$centralization,
                       SYD09_B.netDensity, SYD09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************

round = 9
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
SYD09_SFg2 <- data.frame(SYD09_SF)
SYD09_SFg2 <- SYD09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_SFg2$player1
player2vector <- SYD09_SFg2$player2
SYD09_SFg3 <- SYD09_SFg2
SYD09_SFg3$p1inp2vec <- is.element(SYD09_SFg3$player1, player2vector)
SYD09_SFg3$p2inp1vec <- is.element(SYD09_SFg3$player2, player1vector)

addPlayer1 <- SYD09_SFg3[ which(SYD09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_SFg3[ which(SYD09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_SFg2 <- rbind(SYD09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
SYD09_SFft <- ftable(SYD09_SFg2$player1, SYD09_SFg2$player2)
SYD09_SFft2 <- as.matrix(SYD09_SFft)
numRows <- nrow(SYD09_SFft2)
numCols <- ncol(SYD09_SFft2)
SYD09_SFft3 <- SYD09_SFft2[c(2:numRows) , c(2:numCols)]
SYD09_SFTable <- graph.adjacency(SYD09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(SYD09_SFTable, vertex.label = V(SYD09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
SYD09_SF.clusterCoef <- transitivity(SYD09_SFTable, type="global") #cluster coefficient
SYD09_SF.degreeCent <- centralization.degree(SYD09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_SFftn <- as.network.matrix(SYD09_SFft)
SYD09_SF.netDensity <- network.density(SYD09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_SF.entropy <- entropy(SYD09_SFft) #entropy

SYD09_SF.netMx <- cbind(SYD09_SF.netMx, SYD09_SF.clusterCoef, SYD09_SF.degreeCent$centralization,
                        SYD09_SF.netDensity, SYD09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************

round = 9
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
SYD09_TFg2 <- data.frame(SYD09_TF)
SYD09_TFg2 <- SYD09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_TFg2$player1
player2vector <- SYD09_TFg2$player2
SYD09_TFg3 <- SYD09_TFg2
SYD09_TFg3$p1inp2vec <- is.element(SYD09_TFg3$player1, player2vector)
SYD09_TFg3$p2inp1vec <- is.element(SYD09_TFg3$player2, player1vector)

addPlayer1 <- SYD09_TFg3[ which(SYD09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_TFg3[ which(SYD09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_TFg2 <- rbind(SYD09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
SYD09_TFft <- ftable(SYD09_TFg2$player1, SYD09_TFg2$player2)
SYD09_TFft2 <- as.matrix(SYD09_TFft)
numRows <- nrow(SYD09_TFft2)
numCols <- ncol(SYD09_TFft2)
SYD09_TFft3 <- SYD09_TFft2[c(2:numRows) , c(2:numCols)]
SYD09_TFTable <- graph.adjacency(SYD09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(SYD09_TFTable, vertex.label = V(SYD09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
SYD09_TF.clusterCoef <- transitivity(SYD09_TFTable, type="global") #cluster coefficient
SYD09_TF.degreeCent <- centralization.degree(SYD09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_TFftn <- as.network.matrix(SYD09_TFft)
SYD09_TF.netDensity <- network.density(SYD09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_TF.entropy <- entropy(SYD09_TFft) #entropy

SYD09_TF.netMx <- cbind(SYD09_TF.netMx, SYD09_TF.clusterCoef, SYD09_TF.degreeCent$centralization,
                        SYD09_TF.netDensity, SYD09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
SYD09_SAMg2 <- data.frame(SYD09_SAM)
SYD09_SAMg2 <- SYD09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_SAMg2$player1
player2vector <- SYD09_SAMg2$player2
SYD09_SAMg3 <- SYD09_SAMg2
SYD09_SAMg3$p1inp2vec <- is.element(SYD09_SAMg3$player1, player2vector)
SYD09_SAMg3$p2inp1vec <- is.element(SYD09_SAMg3$player2, player1vector)

addPlayer1 <- SYD09_SAMg3[ which(SYD09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_SAMg3[ which(SYD09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_SAMg2 <- rbind(SYD09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
SYD09_SAMft <- ftable(SYD09_SAMg2$player1, SYD09_SAMg2$player2)
SYD09_SAMft2 <- as.matrix(SYD09_SAMft)
numRows <- nrow(SYD09_SAMft2)
numCols <- ncol(SYD09_SAMft2)
SYD09_SAMft3 <- SYD09_SAMft2[c(2:numRows) , c(2:numCols)]
SYD09_SAMTable <- graph.adjacency(SYD09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(SYD09_SAMTable, vertex.label = V(SYD09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
SYD09_SAM.clusterCoef <- transitivity(SYD09_SAMTable, type="global") #cluster coefficient
SYD09_SAM.degreeCent <- centralization.degree(SYD09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_SAMftn <- as.network.matrix(SYD09_SAMft)
SYD09_SAM.netDensity <- network.density(SYD09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_SAM.entropy <- entropy(SYD09_SAMft) #entropy

SYD09_SAM.netMx <- cbind(SYD09_SAM.netMx, SYD09_SAM.clusterCoef, SYD09_SAM.degreeCent$centralization,
                         SYD09_SAM.netDensity, SYD09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************

round = 9
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
SYD09_TAMg2 <- data.frame(SYD09_TAM)
SYD09_TAMg2 <- SYD09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_TAMg2$player1
player2vector <- SYD09_TAMg2$player2
SYD09_TAMg3 <- SYD09_TAMg2
SYD09_TAMg3$p1inp2vec <- is.element(SYD09_TAMg3$player1, player2vector)
SYD09_TAMg3$p2inp1vec <- is.element(SYD09_TAMg3$player2, player1vector)

addPlayer1 <- SYD09_TAMg3[ which(SYD09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD09_TAMg3[ which(SYD09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_TAMg2 <- rbind(SYD09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
SYD09_TAMft <- ftable(SYD09_TAMg2$player1, SYD09_TAMg2$player2)
SYD09_TAMft2 <- as.matrix(SYD09_TAMft)
numRows <- nrow(SYD09_TAMft2)
numCols <- ncol(SYD09_TAMft2)
SYD09_TAMft3 <- SYD09_TAMft2[c(2:numRows) , c(2:numCols)]
SYD09_TAMTable <- graph.adjacency(SYD09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(SYD09_TAMTable, vertex.label = V(SYD09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
SYD09_TAM.clusterCoef <- transitivity(SYD09_TAMTable, type="global") #cluster coefficient
SYD09_TAM.degreeCent <- centralization.degree(SYD09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_TAMftn <- as.network.matrix(SYD09_TAMft)
SYD09_TAM.netDensity <- network.density(SYD09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_TAM.entropy <- entropy(SYD09_TAMft) #entropy

SYD09_TAM.netMx <- cbind(SYD09_TAM.netMx, SYD09_TAM.clusterCoef, SYD09_TAM.degreeCent$centralization,
                         SYD09_TAM.netDensity, SYD09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
SYD09_SDMg2 <- data.frame(SYD09_SDM)
SYD09_SDMg2 <- SYD09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_SDMg2$player1
player2vector <- SYD09_SDMg2$player2
SYD09_SDMg3 <- SYD09_SDMg2
SYD09_SDMg3$p1inp2vec <- is.element(SYD09_SDMg3$player1, player2vector)
SYD09_SDMg3$p2inp1vec <- is.element(SYD09_SDMg3$player2, player1vector)

addPlayer1 <- SYD09_SDMg3[ which(SYD09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_SDMg3[ which(SYD09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_SDMg2 <- rbind(SYD09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
SYD09_SDMft <- ftable(SYD09_SDMg2$player1, SYD09_SDMg2$player2)
SYD09_SDMft2 <- as.matrix(SYD09_SDMft)
numRows <- nrow(SYD09_SDMft2)
numCols <- ncol(SYD09_SDMft2)
SYD09_SDMft3 <- SYD09_SDMft2[c(2:numRows) , c(2:numCols)]
SYD09_SDMTable <- graph.adjacency(SYD09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(SYD09_SDMTable, vertex.label = V(SYD09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
SYD09_SDM.clusterCoef <- transitivity(SYD09_SDMTable, type="global") #cluster coefficient
SYD09_SDM.degreeCent <- centralization.degree(SYD09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_SDMftn <- as.network.matrix(SYD09_SDMft)
SYD09_SDM.netDensity <- network.density(SYD09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_SDM.entropy <- entropy(SYD09_SDMft) #entropy

SYD09_SDM.netMx <- cbind(SYD09_SDM.netMx, SYD09_SDM.clusterCoef, SYD09_SDM.degreeCent$centralization,
                         SYD09_SDM.netDensity, SYD09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************
#NA

round = 9
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
SYD09_TDMg2 <- data.frame(SYD09_TDM)
SYD09_TDMg2 <- SYD09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_TDMg2$player1
player2vector <- SYD09_TDMg2$player2
SYD09_TDMg3 <- SYD09_TDMg2
SYD09_TDMg3$p1inp2vec <- is.element(SYD09_TDMg3$player1, player2vector)
SYD09_TDMg3$p2inp1vec <- is.element(SYD09_TDMg3$player2, player1vector)

addPlayer1 <- SYD09_TDMg3[ which(SYD09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_TDMg3[ which(SYD09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_TDMg2 <- rbind(SYD09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
SYD09_TDMft <- ftable(SYD09_TDMg2$player1, SYD09_TDMg2$player2)
SYD09_TDMft2 <- as.matrix(SYD09_TDMft)
numRows <- nrow(SYD09_TDMft2)
numCols <- ncol(SYD09_TDMft2)
SYD09_TDMft3 <- SYD09_TDMft2[c(2:numRows) , c(2:numCols)]
SYD09_TDMTable <- graph.adjacency(SYD09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(SYD09_TDMTable, vertex.label = V(SYD09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
SYD09_TDM.clusterCoef <- transitivity(SYD09_TDMTable, type="global") #cluster coefficient
SYD09_TDM.degreeCent <- centralization.degree(SYD09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_TDMftn <- as.network.matrix(SYD09_TDMft)
SYD09_TDM.netDensity <- network.density(SYD09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_TDM.entropy <- entropy(SYD09_TDMft) #entropy

SYD09_TDM.netMx <- cbind(SYD09_TDM.netMx, SYD09_TDM.clusterCoef, SYD09_TDM.degreeCent$centralization,
                         SYD09_TDM.netDensity, SYD09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************

round = 9
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
SYD09_SDg2 <- data.frame(SYD09_SD)
SYD09_SDg2 <- SYD09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_SDg2$player1
player2vector <- SYD09_SDg2$player2
SYD09_SDg3 <- SYD09_SDg2
SYD09_SDg3$p1inp2vec <- is.element(SYD09_SDg3$player1, player2vector)
SYD09_SDg3$p2inp1vec <- is.element(SYD09_SDg3$player2, player1vector)

addPlayer1 <- SYD09_SDg3[ which(SYD09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_SDg3[ which(SYD09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_SDg2 <- rbind(SYD09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
SYD09_SDft <- ftable(SYD09_SDg2$player1, SYD09_SDg2$player2)
SYD09_SDft2 <- as.matrix(SYD09_SDft)
numRows <- nrow(SYD09_SDft2)
numCols <- ncol(SYD09_SDft2)
SYD09_SDft3 <- SYD09_SDft2[c(2:numRows) , c(2:numCols)]
SYD09_SDTable <- graph.adjacency(SYD09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(SYD09_SDTable, vertex.label = V(SYD09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
SYD09_SD.clusterCoef <- transitivity(SYD09_SDTable, type="global") #cluster coefficient
SYD09_SD.degreeCent <- centralization.degree(SYD09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_SDftn <- as.network.matrix(SYD09_SDft)
SYD09_SD.netDensity <- network.density(SYD09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_SD.entropy <- entropy(SYD09_SDft) #entropy

SYD09_SD.netMx <- cbind(SYD09_SD.netMx, SYD09_SD.clusterCoef, SYD09_SD.degreeCent$centralization,
                        SYD09_SD.netDensity, SYD09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
SYD09_TDg2 <- data.frame(SYD09_TD)
SYD09_TDg2 <- SYD09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_TDg2$player1
player2vector <- SYD09_TDg2$player2
SYD09_TDg3 <- SYD09_TDg2
SYD09_TDg3$p1inp2vec <- is.element(SYD09_TDg3$player1, player2vector)
SYD09_TDg3$p2inp1vec <- is.element(SYD09_TDg3$player2, player1vector)

addPlayer1 <- SYD09_TDg3[ which(SYD09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_TDg3[ which(SYD09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_TDg2 <- rbind(SYD09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
SYD09_TDft <- ftable(SYD09_TDg2$player1, SYD09_TDg2$player2)
SYD09_TDft2 <- as.matrix(SYD09_TDft)
numRows <- nrow(SYD09_TDft2)
numCols <- ncol(SYD09_TDft2)
SYD09_TDft3 <- SYD09_TDft2[c(2:numRows) , c(2:numCols)]
SYD09_TDTable <- graph.adjacency(SYD09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(SYD09_TDTable, vertex.label = V(SYD09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
SYD09_TD.clusterCoef <- transitivity(SYD09_TDTable, type="global") #cluster coefficient
SYD09_TD.degreeCent <- centralization.degree(SYD09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_TDftn <- as.network.matrix(SYD09_TDft)
SYD09_TD.netDensity <- network.density(SYD09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_TD.entropy <- entropy(SYD09_TDft) #entropy

SYD09_TD.netMx <- cbind(SYD09_TD.netMx, SYD09_TD.clusterCoef, SYD09_TD.degreeCent$centralization,
                        SYD09_TD.netDensity, SYD09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
SYD09_QTg2 <- data.frame(SYD09_QT)
SYD09_QTg2 <- SYD09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD09_QTg2$player1
player2vector <- SYD09_QTg2$player2
SYD09_QTg3 <- SYD09_QTg2
SYD09_QTg3$p1inp2vec <- is.element(SYD09_QTg3$player1, player2vector)
SYD09_QTg3$p2inp1vec <- is.element(SYD09_QTg3$player2, player1vector)

addPlayer1 <- SYD09_QTg3[ which(SYD09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD09_QTg3[ which(SYD09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD09_QTg2 <- rbind(SYD09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
SYD09_QTft <- ftable(SYD09_QTg2$player1, SYD09_QTg2$player2)
SYD09_QTft2 <- as.matrix(SYD09_QTft)
numRows <- nrow(SYD09_QTft2)
numCols <- ncol(SYD09_QTft2)
SYD09_QTft3 <- SYD09_QTft2[c(2:numRows) , c(2:numCols)]
SYD09_QTTable <- graph.adjacency(SYD09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(SYD09_QTTable, vertex.label = V(SYD09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
SYD09_QT.clusterCoef <- transitivity(SYD09_QTTable, type="global") #cluster coefficient
SYD09_QT.degreeCent <- centralization.degree(SYD09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD09_QTftn <- as.network.matrix(SYD09_QTft)
SYD09_QT.netDensity <- network.density(SYD09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD09_QT.entropy <- entropy(SYD09_QTft) #entropy

SYD09_QT.netMx <- cbind(SYD09_QT.netMx, SYD09_QT.clusterCoef, SYD09_QT.degreeCent$centralization,
                        SYD09_QT.netDensity, SYD09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD09_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS
#NA
##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Goal_F"
WB09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
WB09_Gg2 <- data.frame(WB09_G)
WB09_Gg2 <- WB09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_Gg2$player1
player2vector <- WB09_Gg2$player2
WB09_Gg3 <- WB09_Gg2
WB09_Gg3$p1inp2vec <- is.element(WB09_Gg3$player1, player2vector)
WB09_Gg3$p2inp1vec <- is.element(WB09_Gg3$player2, player1vector)

addPlayer1 <- WB09_Gg3[ which(WB09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_Gg3[ which(WB09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_Gg2 <- rbind(WB09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
WB09_Gft <- ftable(WB09_Gg2$player1, WB09_Gg2$player2)
WB09_Gft2 <- as.matrix(WB09_Gft)
numRows <- nrow(WB09_Gft2)
numCols <- ncol(WB09_Gft2)
WB09_Gft3 <- WB09_Gft2[c(2:numRows) , c(2:numCols)]
WB09_GTable <- graph.adjacency(WB09_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(WB09_GTable, vertex.label = V(WB09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
WB09_G.clusterCoef <- transitivity(WB09_GTable, type="global") #cluster coefficient
WB09_G.degreeCent <- centralization.degree(WB09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_Gftn <- as.network.matrix(WB09_Gft)
WB09_G.netDensity <- network.density(WB09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_G.entropy <- entropy(WB09_Gft) #entropy

WB09_G.netMx <- cbind(WB09_G.netMx, WB09_G.clusterCoef, WB09_G.degreeCent$centralization,
                      WB09_G.netDensity, WB09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Behind_F"
WB09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
WB09_Bg2 <- data.frame(WB09_B)
WB09_Bg2 <- WB09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_Bg2$player1
player2vector <- WB09_Bg2$player2
WB09_Bg3 <- WB09_Bg2
WB09_Bg3$p1inp2vec <- is.element(WB09_Bg3$player1, player2vector)
WB09_Bg3$p2inp1vec <- is.element(WB09_Bg3$player2, player1vector)

addPlayer1 <- WB09_Bg3[ which(WB09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_Bg3[ which(WB09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_Bg2 <- rbind(WB09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
WB09_Bft <- ftable(WB09_Bg2$player1, WB09_Bg2$player2)
WB09_Bft2 <- as.matrix(WB09_Bft)
numRows <- nrow(WB09_Bft2)
numCols <- ncol(WB09_Bft2)
WB09_Bft3 <- WB09_Bft2[c(2:numRows) , c(2:numCols)]
WB09_BTable <- graph.adjacency(WB09_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(WB09_BTable, vertex.label = V(WB09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
WB09_B.clusterCoef <- transitivity(WB09_BTable, type="global") #cluster coefficient
WB09_B.degreeCent <- centralization.degree(WB09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_Bftn <- as.network.matrix(WB09_Bft)
WB09_B.netDensity <- network.density(WB09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_B.entropy <- entropy(WB09_Bft) #entropy

WB09_B.netMx <- cbind(WB09_B.netMx, WB09_B.clusterCoef, WB09_B.degreeCent$centralization,
                      WB09_B.netDensity, WB09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Stoppage_F"
WB09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
WB09_SFg2 <- data.frame(WB09_SF)
WB09_SFg2 <- WB09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_SFg2$player1
player2vector <- WB09_SFg2$player2
WB09_SFg3 <- WB09_SFg2
WB09_SFg3$p1inp2vec <- is.element(WB09_SFg3$player1, player2vector)
WB09_SFg3$p2inp1vec <- is.element(WB09_SFg3$player2, player1vector)

addPlayer1 <- WB09_SFg3[ which(WB09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_SFg3[ which(WB09_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_SFg2 <- rbind(WB09_SFg2, addPlayers)

#ROUND 9, FWD Stoppage graph using weighted edges
WB09_SFft <- ftable(WB09_SFg2$player1, WB09_SFg2$player2)
WB09_SFft2 <- as.matrix(WB09_SFft)
numRows <- nrow(WB09_SFft2)
numCols <- ncol(WB09_SFft2)
WB09_SFft3 <- WB09_SFft2[c(2:numRows) , c(2:numCols)]
WB09_SFTable <- graph.adjacency(WB09_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(WB09_SFTable, vertex.label = V(WB09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
WB09_SF.clusterCoef <- transitivity(WB09_SFTable, type="global") #cluster coefficient
WB09_SF.degreeCent <- centralization.degree(WB09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_SFftn <- as.network.matrix(WB09_SFft)
WB09_SF.netDensity <- network.density(WB09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_SF.entropy <- entropy(WB09_SFft) #entropy

WB09_SF.netMx <- cbind(WB09_SF.netMx, WB09_SF.clusterCoef, WB09_SF.degreeCent$centralization,
                       WB09_SF.netDensity, WB09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Turnover_F"
WB09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
WB09_TFg2 <- data.frame(WB09_TF)
WB09_TFg2 <- WB09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_TFg2$player1
player2vector <- WB09_TFg2$player2
WB09_TFg3 <- WB09_TFg2
WB09_TFg3$p1inp2vec <- is.element(WB09_TFg3$player1, player2vector)
WB09_TFg3$p2inp1vec <- is.element(WB09_TFg3$player2, player1vector)

addPlayer1 <- WB09_TFg3[ which(WB09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_TFg3[ which(WB09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_TFg2 <- rbind(WB09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
WB09_TFft <- ftable(WB09_TFg2$player1, WB09_TFg2$player2)
WB09_TFft2 <- as.matrix(WB09_TFft)
numRows <- nrow(WB09_TFft2)
numCols <- ncol(WB09_TFft2)
WB09_TFft3 <- WB09_TFft2[c(2:numRows) , c(2:numCols)]
WB09_TFTable <- graph.adjacency(WB09_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(WB09_TFTable, vertex.label = V(WB09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
WB09_TF.clusterCoef <- transitivity(WB09_TFTable, type="global") #cluster coefficient
WB09_TF.degreeCent <- centralization.degree(WB09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_TFftn <- as.network.matrix(WB09_TFft)
WB09_TF.netDensity <- network.density(WB09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_TF.entropy <- entropy(WB09_TFft) #entropy

WB09_TF.netMx <- cbind(WB09_TF.netMx, WB09_TF.clusterCoef, WB09_TF.degreeCent$centralization,
                       WB09_TF.netDensity, WB09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
WB09_SAMg2 <- data.frame(WB09_SAM)
WB09_SAMg2 <- WB09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_SAMg2$player1
player2vector <- WB09_SAMg2$player2
WB09_SAMg3 <- WB09_SAMg2
WB09_SAMg3$p1inp2vec <- is.element(WB09_SAMg3$player1, player2vector)
WB09_SAMg3$p2inp1vec <- is.element(WB09_SAMg3$player2, player1vector)

addPlayer1 <- WB09_SAMg3[ which(WB09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_SAMg3[ which(WB09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_SAMg2 <- rbind(WB09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
WB09_SAMft <- ftable(WB09_SAMg2$player1, WB09_SAMg2$player2)
WB09_SAMft2 <- as.matrix(WB09_SAMft)
numRows <- nrow(WB09_SAMft2)
numCols <- ncol(WB09_SAMft2)
WB09_SAMft3 <- WB09_SAMft2[c(2:numRows) , c(2:numCols)]
WB09_SAMTable <- graph.adjacency(WB09_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(WB09_SAMTable, vertex.label = V(WB09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
WB09_SAM.clusterCoef <- transitivity(WB09_SAMTable, type="global") #cluster coefficient
WB09_SAM.degreeCent <- centralization.degree(WB09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_SAMftn <- as.network.matrix(WB09_SAMft)
WB09_SAM.netDensity <- network.density(WB09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_SAM.entropy <- entropy(WB09_SAMft) #entropy

WB09_SAM.netMx <- cbind(WB09_SAM.netMx, WB09_SAM.clusterCoef, WB09_SAM.degreeCent$centralization,
                        WB09_SAM.netDensity, WB09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Turnover_AM"
WB09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
WB09_TAMg2 <- data.frame(WB09_TAM)
WB09_TAMg2 <- WB09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_TAMg2$player1
player2vector <- WB09_TAMg2$player2
WB09_TAMg3 <- WB09_TAMg2
WB09_TAMg3$p1inp2vec <- is.element(WB09_TAMg3$player1, player2vector)
WB09_TAMg3$p2inp1vec <- is.element(WB09_TAMg3$player2, player1vector)

addPlayer1 <- WB09_TAMg3[ which(WB09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_TAMg3[ which(WB09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_TAMg2 <- rbind(WB09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
WB09_TAMft <- ftable(WB09_TAMg2$player1, WB09_TAMg2$player2)
WB09_TAMft2 <- as.matrix(WB09_TAMft)
numRows <- nrow(WB09_TAMft2)
numCols <- ncol(WB09_TAMft2)
WB09_TAMft3 <- WB09_TAMft2[c(2:numRows) , c(2:numCols)]
WB09_TAMTable <- graph.adjacency(WB09_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(WB09_TAMTable, vertex.label = V(WB09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
WB09_TAM.clusterCoef <- transitivity(WB09_TAMTable, type="global") #cluster coefficient
WB09_TAM.degreeCent <- centralization.degree(WB09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_TAMftn <- as.network.matrix(WB09_TAMft)
WB09_TAM.netDensity <- network.density(WB09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_TAM.entropy <- entropy(WB09_TAMft) #entropy

WB09_TAM.netMx <- cbind(WB09_TAM.netMx, WB09_TAM.clusterCoef, WB09_TAM.degreeCent$centralization,
                        WB09_TAM.netDensity, WB09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
WB09_SDMg2 <- data.frame(WB09_SDM)
WB09_SDMg2 <- WB09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_SDMg2$player1
player2vector <- WB09_SDMg2$player2
WB09_SDMg3 <- WB09_SDMg2
WB09_SDMg3$p1inp2vec <- is.element(WB09_SDMg3$player1, player2vector)
WB09_SDMg3$p2inp1vec <- is.element(WB09_SDMg3$player2, player1vector)

addPlayer1 <- WB09_SDMg3[ which(WB09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_SDMg3[ which(WB09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_SDMg2 <- rbind(WB09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
WB09_SDMft <- ftable(WB09_SDMg2$player1, WB09_SDMg2$player2)
WB09_SDMft2 <- as.matrix(WB09_SDMft)
numRows <- nrow(WB09_SDMft2)
numCols <- ncol(WB09_SDMft2)
WB09_SDMft3 <- WB09_SDMft2[c(2:numRows) , c(2:numCols)]
WB09_SDMTable <- graph.adjacency(WB09_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(WB09_SDMTable, vertex.label = V(WB09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
WB09_SDM.clusterCoef <- transitivity(WB09_SDMTable, type="global") #cluster coefficient
WB09_SDM.degreeCent <- centralization.degree(WB09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_SDMftn <- as.network.matrix(WB09_SDMft)
WB09_SDM.netDensity <- network.density(WB09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_SDM.entropy <- entropy(WB09_SDMft) #entropy

WB09_SDM.netMx <- cbind(WB09_SDM.netMx, WB09_SDM.clusterCoef, WB09_SDM.degreeCent$centralization,
                        WB09_SDM.netDensity, WB09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Turnover_DM"
WB09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
WB09_TDMg2 <- data.frame(WB09_TDM)
WB09_TDMg2 <- WB09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_TDMg2$player1
player2vector <- WB09_TDMg2$player2
WB09_TDMg3 <- WB09_TDMg2
WB09_TDMg3$p1inp2vec <- is.element(WB09_TDMg3$player1, player2vector)
WB09_TDMg3$p2inp1vec <- is.element(WB09_TDMg3$player2, player1vector)

addPlayer1 <- WB09_TDMg3[ which(WB09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_TDMg3[ which(WB09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_TDMg2 <- rbind(WB09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
WB09_TDMft <- ftable(WB09_TDMg2$player1, WB09_TDMg2$player2)
WB09_TDMft2 <- as.matrix(WB09_TDMft)
numRows <- nrow(WB09_TDMft2)
numCols <- ncol(WB09_TDMft2)
WB09_TDMft3 <- WB09_TDMft2[c(2:numRows) , c(2:numCols)]
WB09_TDMTable <- graph.adjacency(WB09_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(WB09_TDMTable, vertex.label = V(WB09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
WB09_TDM.clusterCoef <- transitivity(WB09_TDMTable, type="global") #cluster coefficient
WB09_TDM.degreeCent <- centralization.degree(WB09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_TDMftn <- as.network.matrix(WB09_TDMft)
WB09_TDM.netDensity <- network.density(WB09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_TDM.entropy <- entropy(WB09_TDMft) #entropy

WB09_TDM.netMx <- cbind(WB09_TDM.netMx, WB09_TDM.clusterCoef, WB09_TDM.degreeCent$centralization,
                        WB09_TDM.netDensity, WB09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Stoppage_D"
WB09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
WB09_SDg2 <- data.frame(WB09_SD)
WB09_SDg2 <- WB09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_SDg2$player1
player2vector <- WB09_SDg2$player2
WB09_SDg3 <- WB09_SDg2
WB09_SDg3$p1inp2vec <- is.element(WB09_SDg3$player1, player2vector)
WB09_SDg3$p2inp1vec <- is.element(WB09_SDg3$player2, player1vector)

addPlayer1 <- WB09_SDg3[ which(WB09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_SDg3[ which(WB09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_SDg2 <- rbind(WB09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
WB09_SDft <- ftable(WB09_SDg2$player1, WB09_SDg2$player2)
WB09_SDft2 <- as.matrix(WB09_SDft)
numRows <- nrow(WB09_SDft2)
numCols <- ncol(WB09_SDft2)
WB09_SDft3 <- WB09_SDft2[c(2:numRows) , c(2:numCols)]
WB09_SDTable <- graph.adjacency(WB09_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(WB09_SDTable, vertex.label = V(WB09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
WB09_SD.clusterCoef <- transitivity(WB09_SDTable, type="global") #cluster coefficient
WB09_SD.degreeCent <- centralization.degree(WB09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_SDftn <- as.network.matrix(WB09_SDft)
WB09_SD.netDensity <- network.density(WB09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_SD.entropy <- entropy(WB09_SDft) #entropy

WB09_SD.netMx <- cbind(WB09_SD.netMx, WB09_SD.clusterCoef, WB09_SD.degreeCent$centralization,
                       WB09_SD.netDensity, WB09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "Turnover_D"
WB09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
WB09_TDg2 <- data.frame(WB09_TD)
WB09_TDg2 <- WB09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_TDg2$player1
player2vector <- WB09_TDg2$player2
WB09_TDg3 <- WB09_TDg2
WB09_TDg3$p1inp2vec <- is.element(WB09_TDg3$player1, player2vector)
WB09_TDg3$p2inp1vec <- is.element(WB09_TDg3$player2, player1vector)

addPlayer1 <- WB09_TDg3[ which(WB09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_TDg3[ which(WB09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_TDg2 <- rbind(WB09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
WB09_TDft <- ftable(WB09_TDg2$player1, WB09_TDg2$player2)
WB09_TDft2 <- as.matrix(WB09_TDft)
numRows <- nrow(WB09_TDft2)
numCols <- ncol(WB09_TDft2)
WB09_TDft3 <- WB09_TDft2[c(2:numRows) , c(2:numCols)]
WB09_TDTable <- graph.adjacency(WB09_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(WB09_TDTable, vertex.label = V(WB09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
WB09_TD.clusterCoef <- transitivity(WB09_TDTable, type="global") #cluster coefficient
WB09_TD.degreeCent <- centralization.degree(WB09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_TDftn <- as.network.matrix(WB09_TDft)
WB09_TD.netDensity <- network.density(WB09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_TD.entropy <- entropy(WB09_TDft) #entropy

WB09_TD.netMx <- cbind(WB09_TD.netMx, WB09_TD.clusterCoef, WB09_TD.degreeCent$centralization,
                       WB09_TD.netDensity, WB09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
WB09_QTg2 <- data.frame(WB09_QT)
WB09_QTg2 <- WB09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB09_QTg2$player1
player2vector <- WB09_QTg2$player2
WB09_QTg3 <- WB09_QTg2
WB09_QTg3$p1inp2vec <- is.element(WB09_QTg3$player1, player2vector)
WB09_QTg3$p2inp1vec <- is.element(WB09_QTg3$player2, player1vector)

addPlayer1 <- WB09_QTg3[ which(WB09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB09_QTg3[ which(WB09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB09_QTg2 <- rbind(WB09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
WB09_QTft <- ftable(WB09_QTg2$player1, WB09_QTg2$player2)
WB09_QTft2 <- as.matrix(WB09_QTft)
numRows <- nrow(WB09_QTft2)
numCols <- ncol(WB09_QTft2)
WB09_QTft3 <- WB09_QTft2[c(2:numRows) , c(2:numCols)]
WB09_QTTable <- graph.adjacency(WB09_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(WB09_QTTable, vertex.label = V(WB09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
WB09_QT.clusterCoef <- transitivity(WB09_QTTable, type="global") #cluster coefficient
WB09_QT.degreeCent <- centralization.degree(WB09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB09_QTftn <- as.network.matrix(WB09_QTft)
WB09_QT.netDensity <- network.density(WB09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB09_QT.entropy <- entropy(WB09_QTft) #entropy

WB09_QT.netMx <- cbind(WB09_QT.netMx, WB09_QT.clusterCoef, WB09_QT.degreeCent$centralization,
                       WB09_QT.netDensity, WB09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB09_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 9
##

#ROUND 9, Goal***************************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "Goal_F"
WCE09_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Goal with weighted edges
WCE09_Gg2 <- data.frame(WCE09_G)
WCE09_Gg2 <- WCE09_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_Gg2$player1
player2vector <- WCE09_Gg2$player2
WCE09_Gg3 <- WCE09_Gg2
WCE09_Gg3$p1inp2vec <- is.element(WCE09_Gg3$player1, player2vector)
WCE09_Gg3$p2inp1vec <- is.element(WCE09_Gg3$player2, player1vector)

addPlayer1 <- WCE09_Gg3[ which(WCE09_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_Gg3[ which(WCE09_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_Gg2 <- rbind(WCE09_Gg2, addPlayers)

#ROUND 9, Goal graph using weighted edges
WCE09_Gft <- ftable(WCE09_Gg2$player1, WCE09_Gg2$player2)
WCE09_Gft2 <- as.matrix(WCE09_Gft)
numRows <- nrow(WCE09_Gft2)
numCols <- ncol(WCE09_Gft2)
WCE09_Gft3 <- WCE09_Gft2[c(2:numRows) , c(2:numCols)]
WCE09_GTable <- graph.adjacency(WCE09_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Goal graph=weighted
plot.igraph(WCE09_GTable, vertex.label = V(WCE09_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Goal calulation of network metrics
#igraph
WCE09_G.clusterCoef <- transitivity(WCE09_GTable, type="global") #cluster coefficient
WCE09_G.degreeCent <- centralization.degree(WCE09_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_Gftn <- as.network.matrix(WCE09_Gft)
WCE09_G.netDensity <- network.density(WCE09_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_G.entropy <- entropy(WCE09_Gft) #entropy

WCE09_G.netMx <- cbind(WCE09_G.netMx, WCE09_G.clusterCoef, WCE09_G.degreeCent$centralization,
                       WCE09_G.netDensity, WCE09_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_G.netMx) <- varnames

#ROUND 9, Behind***************************************************************

round = 9
teamName = "WCE"
KIoutcome = "Behind_F"
WCE09_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, Behind with weighted edges
WCE09_Bg2 <- data.frame(WCE09_B)
WCE09_Bg2 <- WCE09_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_Bg2$player1
player2vector <- WCE09_Bg2$player2
WCE09_Bg3 <- WCE09_Bg2
WCE09_Bg3$p1inp2vec <- is.element(WCE09_Bg3$player1, player2vector)
WCE09_Bg3$p2inp1vec <- is.element(WCE09_Bg3$player2, player1vector)

addPlayer1 <- WCE09_Bg3[ which(WCE09_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE09_Bg3[ which(WCE09_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_Bg2 <- rbind(WCE09_Bg2, addPlayers)

#ROUND 9, Behind graph using weighted edges
WCE09_Bft <- ftable(WCE09_Bg2$player1, WCE09_Bg2$player2)
WCE09_Bft2 <- as.matrix(WCE09_Bft)
numRows <- nrow(WCE09_Bft2)
numCols <- ncol(WCE09_Bft2)
WCE09_Bft3 <- WCE09_Bft2[c(2:numRows) , c(2:numCols)]
WCE09_BTable <- graph.adjacency(WCE09_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 9, Behind graph=weighted
plot.igraph(WCE09_BTable, vertex.label = V(WCE09_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, Behind calulation of network metrics
#igraph
WCE09_B.clusterCoef <- transitivity(WCE09_BTable, type="global") #cluster coefficient
WCE09_B.degreeCent <- centralization.degree(WCE09_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_Bftn <- as.network.matrix(WCE09_Bft)
WCE09_B.netDensity <- network.density(WCE09_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_B.entropy <- entropy(WCE09_Bft) #entropy

WCE09_B.netMx <- cbind(WCE09_B.netMx, WCE09_B.clusterCoef, WCE09_B.degreeCent$centralization,
                       WCE09_B.netDensity, WCE09_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_B.netMx) <- varnames

#ROUND 9, FWD Stoppage**********************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE09_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Stoppage with weighted edges
WCE09_SFg2 <- data.frame(WCE09_SF)
WCE09_SFg2 <- WCE09_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_SFg2$player1
player2vector <- WCE09_SFg2$player2
WCE09_SFg3 <- WCE09_SFg2
WCE09_SFg3$p1inp2vec <- is.element(WCE09_SFg3$player1, player2vector)
WCE09_SFg3$p2inp1vec <- is.element(WCE09_SFg3$player2, player1vector)

addPlayer1 <- WCE09_SFg3[ which(WCE09_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE09_SFg2 <- rbind(WCE09_SFg2, addPlayer1)

#ROUND 9, FWD Stoppage graph using weighted edges
WCE09_SFft <- ftable(WCE09_SFg2$player1, WCE09_SFg2$player2)
WCE09_SFft2 <- as.matrix(WCE09_SFft)
numRows <- nrow(WCE09_SFft2)
numCols <- ncol(WCE09_SFft2)
WCE09_SFft3 <- WCE09_SFft2[c(2:numRows) , c(1:numCols)]
WCE09_SFTable <- graph.adjacency(WCE09_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Stoppage graph=weighted
plot.igraph(WCE09_SFTable, vertex.label = V(WCE09_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Stoppage calulation of network metrics
#igraph
WCE09_SF.clusterCoef <- transitivity(WCE09_SFTable, type="global") #cluster coefficient
WCE09_SF.degreeCent <- centralization.degree(WCE09_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_SFftn <- as.network.matrix(WCE09_SFft)
WCE09_SF.netDensity <- network.density(WCE09_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_SF.entropy <- entropy(WCE09_SFft) #entropy

WCE09_SF.netMx <- cbind(WCE09_SF.netMx, WCE09_SF.clusterCoef, WCE09_SF.degreeCent$centralization,
                        WCE09_SF.netDensity, WCE09_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_SF.netMx) <- varnames

#ROUND 9, FWD Turnover**********************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE09_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, FWD Turnover with weighted edges
WCE09_TFg2 <- data.frame(WCE09_TF)
WCE09_TFg2 <- WCE09_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_TFg2$player1
player2vector <- WCE09_TFg2$player2
WCE09_TFg3 <- WCE09_TFg2
WCE09_TFg3$p1inp2vec <- is.element(WCE09_TFg3$player1, player2vector)
WCE09_TFg3$p2inp1vec <- is.element(WCE09_TFg3$player2, player1vector)

addPlayer1 <- WCE09_TFg3[ which(WCE09_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_TFg3[ which(WCE09_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_TFg2 <- rbind(WCE09_TFg2, addPlayers)

#ROUND 9, FWD Turnover graph using weighted edges
WCE09_TFft <- ftable(WCE09_TFg2$player1, WCE09_TFg2$player2)
WCE09_TFft2 <- as.matrix(WCE09_TFft)
numRows <- nrow(WCE09_TFft2)
numCols <- ncol(WCE09_TFft2)
WCE09_TFft3 <- WCE09_TFft2[c(2:numRows) , c(2:numCols)]
WCE09_TFTable <- graph.adjacency(WCE09_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, FWD Turnover graph=weighted
plot.igraph(WCE09_TFTable, vertex.label = V(WCE09_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, FWD Turnover calulation of network metrics
#igraph
WCE09_TF.clusterCoef <- transitivity(WCE09_TFTable, type="global") #cluster coefficient
WCE09_TF.degreeCent <- centralization.degree(WCE09_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_TFftn <- as.network.matrix(WCE09_TFft)
WCE09_TF.netDensity <- network.density(WCE09_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_TF.entropy <- entropy(WCE09_TFft) #entropy

WCE09_TF.netMx <- cbind(WCE09_TF.netMx, WCE09_TF.clusterCoef, WCE09_TF.degreeCent$centralization,
                        WCE09_TF.netDensity, WCE09_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_TF.netMx) <- varnames

#ROUND 9, AM Stoppage**********************************************************

round = 9
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE09_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Stoppage with weighted edges
WCE09_SAMg2 <- data.frame(WCE09_SAM)
WCE09_SAMg2 <- WCE09_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_SAMg2$player1
player2vector <- WCE09_SAMg2$player2
WCE09_SAMg3 <- WCE09_SAMg2
WCE09_SAMg3$p1inp2vec <- is.element(WCE09_SAMg3$player1, player2vector)
WCE09_SAMg3$p2inp1vec <- is.element(WCE09_SAMg3$player2, player1vector)

addPlayer1 <- WCE09_SAMg3[ which(WCE09_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_SAMg3[ which(WCE09_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_SAMg2 <- rbind(WCE09_SAMg2, addPlayers)

#ROUND 9, AM Stoppage graph using weighted edges
WCE09_SAMft <- ftable(WCE09_SAMg2$player1, WCE09_SAMg2$player2)
WCE09_SAMft2 <- as.matrix(WCE09_SAMft)
numRows <- nrow(WCE09_SAMft2)
numCols <- ncol(WCE09_SAMft2)
WCE09_SAMft3 <- WCE09_SAMft2[c(2:numRows) , c(2:numCols)]
WCE09_SAMTable <- graph.adjacency(WCE09_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Stoppage graph=weighted
plot.igraph(WCE09_SAMTable, vertex.label = V(WCE09_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Stoppage calulation of network metrics
#igraph
WCE09_SAM.clusterCoef <- transitivity(WCE09_SAMTable, type="global") #cluster coefficient
WCE09_SAM.degreeCent <- centralization.degree(WCE09_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_SAMftn <- as.network.matrix(WCE09_SAMft)
WCE09_SAM.netDensity <- network.density(WCE09_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_SAM.entropy <- entropy(WCE09_SAMft) #entropy

WCE09_SAM.netMx <- cbind(WCE09_SAM.netMx, WCE09_SAM.clusterCoef, WCE09_SAM.degreeCent$centralization,
                         WCE09_SAM.netDensity, WCE09_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_SAM.netMx) <- varnames

#ROUND 9, AM Turnover**********************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE09_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, AM Turnover with weighted edges
WCE09_TAMg2 <- data.frame(WCE09_TAM)
WCE09_TAMg2 <- WCE09_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_TAMg2$player1
player2vector <- WCE09_TAMg2$player2
WCE09_TAMg3 <- WCE09_TAMg2
WCE09_TAMg3$p1inp2vec <- is.element(WCE09_TAMg3$player1, player2vector)
WCE09_TAMg3$p2inp1vec <- is.element(WCE09_TAMg3$player2, player1vector)

addPlayer1 <- WCE09_TAMg3[ which(WCE09_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE09_TAMg3[ which(WCE09_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_TAMg2 <- rbind(WCE09_TAMg2, addPlayers)

#ROUND 9, AM Turnover graph using weighted edges
WCE09_TAMft <- ftable(WCE09_TAMg2$player1, WCE09_TAMg2$player2)
WCE09_TAMft2 <- as.matrix(WCE09_TAMft)
numRows <- nrow(WCE09_TAMft2)
numCols <- ncol(WCE09_TAMft2)
WCE09_TAMft3 <- WCE09_TAMft2[c(2:numRows) , c(2:numCols)]
WCE09_TAMTable <- graph.adjacency(WCE09_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, AM Turnover graph=weighted
plot.igraph(WCE09_TAMTable, vertex.label = V(WCE09_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, AM Turnover calulation of network metrics
#igraph
WCE09_TAM.clusterCoef <- transitivity(WCE09_TAMTable, type="global") #cluster coefficient
WCE09_TAM.degreeCent <- centralization.degree(WCE09_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_TAMftn <- as.network.matrix(WCE09_TAMft)
WCE09_TAM.netDensity <- network.density(WCE09_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_TAM.entropy <- entropy(WCE09_TAMft) #entropy

WCE09_TAM.netMx <- cbind(WCE09_TAM.netMx, WCE09_TAM.clusterCoef, WCE09_TAM.degreeCent$centralization,
                         WCE09_TAM.netDensity, WCE09_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_TAM.netMx) <- varnames

#ROUND 9, DM Stoppage**********************************************************

round = 9
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE09_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Stoppage with weighted edges
WCE09_SDMg2 <- data.frame(WCE09_SDM)
WCE09_SDMg2 <- WCE09_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_SDMg2$player1
player2vector <- WCE09_SDMg2$player2
WCE09_SDMg3 <- WCE09_SDMg2
WCE09_SDMg3$p1inp2vec <- is.element(WCE09_SDMg3$player1, player2vector)
WCE09_SDMg3$p2inp1vec <- is.element(WCE09_SDMg3$player2, player1vector)

addPlayer1 <- WCE09_SDMg3[ which(WCE09_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_SDMg3[ which(WCE09_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_SDMg2 <- rbind(WCE09_SDMg2, addPlayers)

#ROUND 9, DM Stoppage graph using weighted edges
WCE09_SDMft <- ftable(WCE09_SDMg2$player1, WCE09_SDMg2$player2)
WCE09_SDMft2 <- as.matrix(WCE09_SDMft)
numRows <- nrow(WCE09_SDMft2)
numCols <- ncol(WCE09_SDMft2)
WCE09_SDMft3 <- WCE09_SDMft2[c(2:numRows) , c(2:numCols)]
WCE09_SDMTable <- graph.adjacency(WCE09_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Stoppage graph=weighted
plot.igraph(WCE09_SDMTable, vertex.label = V(WCE09_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Stoppage calulation of network metrics
#igraph
WCE09_SDM.clusterCoef <- transitivity(WCE09_SDMTable, type="global") #cluster coefficient
WCE09_SDM.degreeCent <- centralization.degree(WCE09_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_SDMftn <- as.network.matrix(WCE09_SDMft)
WCE09_SDM.netDensity <- network.density(WCE09_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_SDM.entropy <- entropy(WCE09_SDMft) #entropy

WCE09_SDM.netMx <- cbind(WCE09_SDM.netMx, WCE09_SDM.clusterCoef, WCE09_SDM.degreeCent$centralization,
                         WCE09_SDM.netDensity, WCE09_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_SDM.netMx) <- varnames

#ROUND 9, DM Turnover**********************************************************

round = 9
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE09_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, DM Turnover with weighted edges
WCE09_TDMg2 <- data.frame(WCE09_TDM)
WCE09_TDMg2 <- WCE09_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_TDMg2$player1
player2vector <- WCE09_TDMg2$player2
WCE09_TDMg3 <- WCE09_TDMg2
WCE09_TDMg3$p1inp2vec <- is.element(WCE09_TDMg3$player1, player2vector)
WCE09_TDMg3$p2inp1vec <- is.element(WCE09_TDMg3$player2, player1vector)

addPlayer1 <- WCE09_TDMg3[ which(WCE09_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE09_TDMg3[ which(WCE09_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_TDMg2 <- rbind(WCE09_TDMg2, addPlayers)

#ROUND 9, DM Turnover graph using weighted edges
WCE09_TDMft <- ftable(WCE09_TDMg2$player1, WCE09_TDMg2$player2)
WCE09_TDMft2 <- as.matrix(WCE09_TDMft)
numRows <- nrow(WCE09_TDMft2)
numCols <- ncol(WCE09_TDMft2)
WCE09_TDMft3 <- WCE09_TDMft2[c(2:numRows) , c(2:numCols)]
WCE09_TDMTable <- graph.adjacency(WCE09_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 9, DM Turnover graph=weighted
plot.igraph(WCE09_TDMTable, vertex.label = V(WCE09_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, DM Turnover calulation of network metrics
#igraph
WCE09_TDM.clusterCoef <- transitivity(WCE09_TDMTable, type="global") #cluster coefficient
WCE09_TDM.degreeCent <- centralization.degree(WCE09_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_TDMftn <- as.network.matrix(WCE09_TDMft)
WCE09_TDM.netDensity <- network.density(WCE09_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_TDM.entropy <- entropy(WCE09_TDMft) #entropy

WCE09_TDM.netMx <- cbind(WCE09_TDM.netMx, WCE09_TDM.clusterCoef, WCE09_TDM.degreeCent$centralization,
                         WCE09_TDM.netDensity, WCE09_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_TDM.netMx) <- varnames

#ROUND 9, D Stoppage**********************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE09_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Stoppage with weighted edges
WCE09_SDg2 <- data.frame(WCE09_SD)
WCE09_SDg2 <- WCE09_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_SDg2$player1
player2vector <- WCE09_SDg2$player2
WCE09_SDg3 <- WCE09_SDg2
WCE09_SDg3$p1inp2vec <- is.element(WCE09_SDg3$player1, player2vector)
WCE09_SDg3$p2inp1vec <- is.element(WCE09_SDg3$player2, player1vector)

addPlayer1 <- WCE09_SDg3[ which(WCE09_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_SDg3[ which(WCE09_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_SDg2 <- rbind(WCE09_SDg2, addPlayers)

#ROUND 9, D Stoppage graph using weighted edges
WCE09_SDft <- ftable(WCE09_SDg2$player1, WCE09_SDg2$player2)
WCE09_SDft2 <- as.matrix(WCE09_SDft)
numRows <- nrow(WCE09_SDft2)
numCols <- ncol(WCE09_SDft2)
WCE09_SDft3 <- WCE09_SDft2[c(2:numRows) , c(2:numCols)]
WCE09_SDTable <- graph.adjacency(WCE09_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Stoppage graph=weighted
plot.igraph(WCE09_SDTable, vertex.label = V(WCE09_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Stoppage calulation of network metrics
#igraph
WCE09_SD.clusterCoef <- transitivity(WCE09_SDTable, type="global") #cluster coefficient
WCE09_SD.degreeCent <- centralization.degree(WCE09_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_SDftn <- as.network.matrix(WCE09_SDft)
WCE09_SD.netDensity <- network.density(WCE09_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_SD.entropy <- entropy(WCE09_SDft) #entropy

WCE09_SD.netMx <- cbind(WCE09_SD.netMx, WCE09_SD.clusterCoef, WCE09_SD.degreeCent$centralization,
                        WCE09_SD.netDensity, WCE09_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_SD.netMx) <- varnames

#ROUND 9, D Turnover**********************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE09_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, D Turnover with weighted edges
WCE09_TDg2 <- data.frame(WCE09_TD)
WCE09_TDg2 <- WCE09_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_TDg2$player1
player2vector <- WCE09_TDg2$player2
WCE09_TDg3 <- WCE09_TDg2
WCE09_TDg3$p1inp2vec <- is.element(WCE09_TDg3$player1, player2vector)
WCE09_TDg3$p2inp1vec <- is.element(WCE09_TDg3$player2, player1vector)

addPlayer1 <- WCE09_TDg3[ which(WCE09_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_TDg3[ which(WCE09_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_TDg2 <- rbind(WCE09_TDg2, addPlayers)

#ROUND 9, D Turnover graph using weighted edges
WCE09_TDft <- ftable(WCE09_TDg2$player1, WCE09_TDg2$player2)
WCE09_TDft2 <- as.matrix(WCE09_TDft)
numRows <- nrow(WCE09_TDft2)
numCols <- ncol(WCE09_TDft2)
WCE09_TDft3 <- WCE09_TDft2[c(2:numRows) , c(2:numCols)]
WCE09_TDTable <- graph.adjacency(WCE09_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, D Turnover graph=weighted
plot.igraph(WCE09_TDTable, vertex.label = V(WCE09_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, D Turnover calulation of network metrics
#igraph
WCE09_TD.clusterCoef <- transitivity(WCE09_TDTable, type="global") #cluster coefficient
WCE09_TD.degreeCent <- centralization.degree(WCE09_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_TDftn <- as.network.matrix(WCE09_TDft)
WCE09_TD.netDensity <- network.density(WCE09_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_TD.entropy <- entropy(WCE09_TDft) #entropy

WCE09_TD.netMx <- cbind(WCE09_TD.netMx, WCE09_TD.clusterCoef, WCE09_TD.degreeCent$centralization,
                        WCE09_TD.netDensity, WCE09_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_TD.netMx) <- varnames

#ROUND 9, End of Qtr**********************************************************
#NA

round = 9
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE09_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 9, End of Qtr with weighted edges
WCE09_QTg2 <- data.frame(WCE09_QT)
WCE09_QTg2 <- WCE09_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE09_QTg2$player1
player2vector <- WCE09_QTg2$player2
WCE09_QTg3 <- WCE09_QTg2
WCE09_QTg3$p1inp2vec <- is.element(WCE09_QTg3$player1, player2vector)
WCE09_QTg3$p2inp1vec <- is.element(WCE09_QTg3$player2, player1vector)

addPlayer1 <- WCE09_QTg3[ which(WCE09_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE09_QTg3[ which(WCE09_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE09_QTg2 <- rbind(WCE09_QTg2, addPlayers)

#ROUND 9, End of Qtr graph using weighted edges
WCE09_QTft <- ftable(WCE09_QTg2$player1, WCE09_QTg2$player2)
WCE09_QTft2 <- as.matrix(WCE09_QTft)
numRows <- nrow(WCE09_QTft2)
numCols <- ncol(WCE09_QTft2)
WCE09_QTft3 <- WCE09_QTft2[c(2:numRows) , c(2:numCols)]
WCE09_QTTable <- graph.adjacency(WCE09_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 9, End of Qtr graph=weighted
plot.igraph(WCE09_QTTable, vertex.label = V(WCE09_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE09_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 9, End of Qtr calulation of network metrics
#igraph
WCE09_QT.clusterCoef <- transitivity(WCE09_QTTable, type="global") #cluster coefficient
WCE09_QT.degreeCent <- centralization.degree(WCE09_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE09_QTftn <- as.network.matrix(WCE09_QTft)
WCE09_QT.netDensity <- network.density(WCE09_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE09_QT.entropy <- entropy(WCE09_QTft) #entropy

WCE09_QT.netMx <- cbind(WCE09_QT.netMx, WCE09_QT.clusterCoef, WCE09_QT.degreeCent$centralization,
                        WCE09_QT.netDensity, WCE09_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE09_QT.netMx) <- varnames
