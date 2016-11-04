#####
#10-03-16- Real data 22
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************

round = 22
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
ADEL22_Gg2 <- data.frame(ADEL22_G)
ADEL22_Gg2 <- ADEL22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_Gg2$player1
player2vector <- ADEL22_Gg2$player2
ADEL22_Gg3 <- ADEL22_Gg2
ADEL22_Gg3$p1inp2vec <- is.element(ADEL22_Gg3$player1, player2vector)
ADEL22_Gg3$p2inp1vec <- is.element(ADEL22_Gg3$player2, player1vector)

addPlayer1 <- ADEL22_Gg3[ which(ADEL22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_Gg3[ which(ADEL22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_Gg2 <- rbind(ADEL22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
ADEL22_Gft <- ftable(ADEL22_Gg2$player1, ADEL22_Gg2$player2)
ADEL22_Gft2 <- as.matrix(ADEL22_Gft)
numRows <- nrow(ADEL22_Gft2)
numCols <- ncol(ADEL22_Gft2)
ADEL22_Gft3 <- ADEL22_Gft2[c(2:numRows) , c(2:numCols)]
ADEL22_GTable <- graph.adjacency(ADEL22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(ADEL22_GTable, vertex.label = V(ADEL22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
ADEL22_G.clusterCoef <- transitivity(ADEL22_GTable, type="global") #cluster coefficient
ADEL22_G.degreeCent <- centralization.degree(ADEL22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_Gftn <- as.network.matrix(ADEL22_Gft)
ADEL22_G.netDensity <- network.density(ADEL22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_G.entropy <- entropy(ADEL22_Gft) #entropy

ADEL22_G.netMx <- cbind(ADEL22_G.netMx, ADEL22_G.clusterCoef, ADEL22_G.degreeCent$centralization,
                        ADEL22_G.netDensity, ADEL22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************

round = 22
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
ADEL22_Bg2 <- data.frame(ADEL22_B)
ADEL22_Bg2 <- ADEL22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_Bg2$player1
player2vector <- ADEL22_Bg2$player2
ADEL22_Bg3 <- ADEL22_Bg2
ADEL22_Bg3$p1inp2vec <- is.element(ADEL22_Bg3$player1, player2vector)
ADEL22_Bg3$p2inp1vec <- is.element(ADEL22_Bg3$player2, player1vector)

addPlayer1 <- ADEL22_Bg3[ which(ADEL22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_Bg3[ which(ADEL22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_Bg2 <- rbind(ADEL22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
ADEL22_Bft <- ftable(ADEL22_Bg2$player1, ADEL22_Bg2$player2)
ADEL22_Bft2 <- as.matrix(ADEL22_Bft)
numRows <- nrow(ADEL22_Bft2)
numCols <- ncol(ADEL22_Bft2)
ADEL22_Bft3 <- ADEL22_Bft2[c(2:numRows) , c(2:numCols)]
ADEL22_BTable <- graph.adjacency(ADEL22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(ADEL22_BTable, vertex.label = V(ADEL22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
ADEL22_B.clusterCoef <- transitivity(ADEL22_BTable, type="global") #cluster coefficient
ADEL22_B.degreeCent <- centralization.degree(ADEL22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_Bftn <- as.network.matrix(ADEL22_Bft)
ADEL22_B.netDensity <- network.density(ADEL22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_B.entropy <- entropy(ADEL22_Bft) #entropy

ADEL22_B.netMx <- cbind(ADEL22_B.netMx, ADEL22_B.clusterCoef, ADEL22_B.degreeCent$centralization,
                        ADEL22_B.netDensity, ADEL22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
ADEL22_SFg2 <- data.frame(ADEL22_SF)
ADEL22_SFg2 <- ADEL22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_SFg2$player1
player2vector <- ADEL22_SFg2$player2
ADEL22_SFg3 <- ADEL22_SFg2
ADEL22_SFg3$p1inp2vec <- is.element(ADEL22_SFg3$player1, player2vector)
ADEL22_SFg3$p2inp1vec <- is.element(ADEL22_SFg3$player2, player1vector)

addPlayer1 <- ADEL22_SFg3[ which(ADEL22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_SFg3[ which(ADEL22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_SFg2 <- rbind(ADEL22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
ADEL22_SFft <- ftable(ADEL22_SFg2$player1, ADEL22_SFg2$player2)
ADEL22_SFft2 <- as.matrix(ADEL22_SFft)
numRows <- nrow(ADEL22_SFft2)
numCols <- ncol(ADEL22_SFft2)
ADEL22_SFft3 <- ADEL22_SFft2[c(2:numRows) , c(2:numCols)]
ADEL22_SFTable <- graph.adjacency(ADEL22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(ADEL22_SFTable, vertex.label = V(ADEL22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
ADEL22_SF.clusterCoef <- transitivity(ADEL22_SFTable, type="global") #cluster coefficient
ADEL22_SF.degreeCent <- centralization.degree(ADEL22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_SFftn <- as.network.matrix(ADEL22_SFft)
ADEL22_SF.netDensity <- network.density(ADEL22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_SF.entropy <- entropy(ADEL22_SFft) #entropy

ADEL22_SF.netMx <- cbind(ADEL22_SF.netMx, ADEL22_SF.clusterCoef, ADEL22_SF.degreeCent$centralization,
                         ADEL22_SF.netDensity, ADEL22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
ADEL22_TFg2 <- data.frame(ADEL22_TF)
ADEL22_TFg2 <- ADEL22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_TFg2$player1
player2vector <- ADEL22_TFg2$player2
ADEL22_TFg3 <- ADEL22_TFg2
ADEL22_TFg3$p1inp2vec <- is.element(ADEL22_TFg3$player1, player2vector)
ADEL22_TFg3$p2inp1vec <- is.element(ADEL22_TFg3$player2, player1vector)

addPlayer1 <- ADEL22_TFg3[ which(ADEL22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_TFg3[ which(ADEL22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_TFg2 <- rbind(ADEL22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
ADEL22_TFft <- ftable(ADEL22_TFg2$player1, ADEL22_TFg2$player2)
ADEL22_TFft2 <- as.matrix(ADEL22_TFft)
numRows <- nrow(ADEL22_TFft2)
numCols <- ncol(ADEL22_TFft2)
ADEL22_TFft3 <- ADEL22_TFft2[c(2:numRows) , c(2:numCols)]
ADEL22_TFTable <- graph.adjacency(ADEL22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(ADEL22_TFTable, vertex.label = V(ADEL22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
ADEL22_TF.clusterCoef <- transitivity(ADEL22_TFTable, type="global") #cluster coefficient
ADEL22_TF.degreeCent <- centralization.degree(ADEL22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_TFftn <- as.network.matrix(ADEL22_TFft)
ADEL22_TF.netDensity <- network.density(ADEL22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_TF.entropy <- entropy(ADEL22_TFft) #entropy

ADEL22_TF.netMx <- cbind(ADEL22_TF.netMx, ADEL22_TF.clusterCoef, ADEL22_TF.degreeCent$centralization,
                         ADEL22_TF.netDensity, ADEL22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
ADEL22_SAMg2 <- data.frame(ADEL22_SAM)
ADEL22_SAMg2 <- ADEL22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_SAMg2$player1
player2vector <- ADEL22_SAMg2$player2
ADEL22_SAMg3 <- ADEL22_SAMg2
ADEL22_SAMg3$p1inp2vec <- is.element(ADEL22_SAMg3$player1, player2vector)
ADEL22_SAMg3$p2inp1vec <- is.element(ADEL22_SAMg3$player2, player1vector)

addPlayer1 <- ADEL22_SAMg3[ which(ADEL22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_SAMg3[ which(ADEL22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_SAMg2 <- rbind(ADEL22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
ADEL22_SAMft <- ftable(ADEL22_SAMg2$player1, ADEL22_SAMg2$player2)
ADEL22_SAMft2 <- as.matrix(ADEL22_SAMft)
numRows <- nrow(ADEL22_SAMft2)
numCols <- ncol(ADEL22_SAMft2)
ADEL22_SAMft3 <- ADEL22_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL22_SAMTable <- graph.adjacency(ADEL22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(ADEL22_SAMTable, vertex.label = V(ADEL22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
ADEL22_SAM.clusterCoef <- transitivity(ADEL22_SAMTable, type="global") #cluster coefficient
ADEL22_SAM.degreeCent <- centralization.degree(ADEL22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_SAMftn <- as.network.matrix(ADEL22_SAMft)
ADEL22_SAM.netDensity <- network.density(ADEL22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_SAM.entropy <- entropy(ADEL22_SAMft) #entropy

ADEL22_SAM.netMx <- cbind(ADEL22_SAM.netMx, ADEL22_SAM.clusterCoef, ADEL22_SAM.degreeCent$centralization,
                          ADEL22_SAM.netDensity, ADEL22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
ADEL22_TAMg2 <- data.frame(ADEL22_TAM)
ADEL22_TAMg2 <- ADEL22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_TAMg2$player1
player2vector <- ADEL22_TAMg2$player2
ADEL22_TAMg3 <- ADEL22_TAMg2
ADEL22_TAMg3$p1inp2vec <- is.element(ADEL22_TAMg3$player1, player2vector)
ADEL22_TAMg3$p2inp1vec <- is.element(ADEL22_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL22_TAMg3[ which(ADEL22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL22_TAMg2 <- rbind(ADEL22_TAMg2, addPlayer2)

#ROUND 22, AM Turnover graph using weighted edges
ADEL22_TAMft <- ftable(ADEL22_TAMg2$player1, ADEL22_TAMg2$player2)
ADEL22_TAMft2 <- as.matrix(ADEL22_TAMft)
numRows <- nrow(ADEL22_TAMft2)
numCols <- ncol(ADEL22_TAMft2)
ADEL22_TAMft3 <- ADEL22_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL22_TAMTable <- graph.adjacency(ADEL22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(ADEL22_TAMTable, vertex.label = V(ADEL22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
ADEL22_TAM.clusterCoef <- transitivity(ADEL22_TAMTable, type="global") #cluster coefficient
ADEL22_TAM.degreeCent <- centralization.degree(ADEL22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_TAMftn <- as.network.matrix(ADEL22_TAMft)
ADEL22_TAM.netDensity <- network.density(ADEL22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_TAM.entropy <- entropy(ADEL22_TAMft) #entropy

ADEL22_TAM.netMx <- cbind(ADEL22_TAM.netMx, ADEL22_TAM.clusterCoef, ADEL22_TAM.degreeCent$centralization,
                          ADEL22_TAM.netDensity, ADEL22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
ADEL22_SDMg2 <- data.frame(ADEL22_SDM)
ADEL22_SDMg2 <- ADEL22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_SDMg2$player1
player2vector <- ADEL22_SDMg2$player2
ADEL22_SDMg3 <- ADEL22_SDMg2
ADEL22_SDMg3$p1inp2vec <- is.element(ADEL22_SDMg3$player1, player2vector)
ADEL22_SDMg3$p2inp1vec <- is.element(ADEL22_SDMg3$player2, player1vector)

addPlayer1 <- ADEL22_SDMg3[ which(ADEL22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_SDMg3[ which(ADEL22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_SDMg2 <- rbind(ADEL22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
ADEL22_SDMft <- ftable(ADEL22_SDMg2$player1, ADEL22_SDMg2$player2)
ADEL22_SDMft2 <- as.matrix(ADEL22_SDMft)
numRows <- nrow(ADEL22_SDMft2)
numCols <- ncol(ADEL22_SDMft2)
ADEL22_SDMft3 <- ADEL22_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL22_SDMTable <- graph.adjacency(ADEL22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(ADEL22_SDMTable, vertex.label = V(ADEL22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
ADEL22_SDM.clusterCoef <- transitivity(ADEL22_SDMTable, type="global") #cluster coefficient
ADEL22_SDM.degreeCent <- centralization.degree(ADEL22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_SDMftn <- as.network.matrix(ADEL22_SDMft)
ADEL22_SDM.netDensity <- network.density(ADEL22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_SDM.entropy <- entropy(ADEL22_SDMft) #entropy

ADEL22_SDM.netMx <- cbind(ADEL22_SDM.netMx, ADEL22_SDM.clusterCoef, ADEL22_SDM.degreeCent$centralization,
                          ADEL22_SDM.netDensity, ADEL22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
ADEL22_TDMg2 <- data.frame(ADEL22_TDM)
ADEL22_TDMg2 <- ADEL22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_TDMg2$player1
player2vector <- ADEL22_TDMg2$player2
ADEL22_TDMg3 <- ADEL22_TDMg2
ADEL22_TDMg3$p1inp2vec <- is.element(ADEL22_TDMg3$player1, player2vector)
ADEL22_TDMg3$p2inp1vec <- is.element(ADEL22_TDMg3$player2, player1vector)

addPlayer1 <- ADEL22_TDMg3[ which(ADEL22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL22_TDMg3[ which(ADEL22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_TDMg2 <- rbind(ADEL22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
ADEL22_TDMft <- ftable(ADEL22_TDMg2$player1, ADEL22_TDMg2$player2)
ADEL22_TDMft2 <- as.matrix(ADEL22_TDMft)
numRows <- nrow(ADEL22_TDMft2)
numCols <- ncol(ADEL22_TDMft2)
ADEL22_TDMft3 <- ADEL22_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL22_TDMTable <- graph.adjacency(ADEL22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(ADEL22_TDMTable, vertex.label = V(ADEL22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
ADEL22_TDM.clusterCoef <- transitivity(ADEL22_TDMTable, type="global") #cluster coefficient
ADEL22_TDM.degreeCent <- centralization.degree(ADEL22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_TDMftn <- as.network.matrix(ADEL22_TDMft)
ADEL22_TDM.netDensity <- network.density(ADEL22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_TDM.entropy <- entropy(ADEL22_TDMft) #entropy

ADEL22_TDM.netMx <- cbind(ADEL22_TDM.netMx, ADEL22_TDM.clusterCoef, ADEL22_TDM.degreeCent$centralization,
                          ADEL22_TDM.netDensity, ADEL22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
ADEL22_SDg2 <- data.frame(ADEL22_SD)
ADEL22_SDg2 <- ADEL22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_SDg2$player1
player2vector <- ADEL22_SDg2$player2
ADEL22_SDg3 <- ADEL22_SDg2
ADEL22_SDg3$p1inp2vec <- is.element(ADEL22_SDg3$player1, player2vector)
ADEL22_SDg3$p2inp1vec <- is.element(ADEL22_SDg3$player2, player1vector)

addPlayer1 <- ADEL22_SDg3[ which(ADEL22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_SDg3[ which(ADEL22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_SDg2 <- rbind(ADEL22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
ADEL22_SDft <- ftable(ADEL22_SDg2$player1, ADEL22_SDg2$player2)
ADEL22_SDft2 <- as.matrix(ADEL22_SDft)
numRows <- nrow(ADEL22_SDft2)
numCols <- ncol(ADEL22_SDft2)
ADEL22_SDft3 <- ADEL22_SDft2[c(2:numRows) , c(2:numCols)]
ADEL22_SDTable <- graph.adjacency(ADEL22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(ADEL22_SDTable, vertex.label = V(ADEL22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
ADEL22_SD.clusterCoef <- transitivity(ADEL22_SDTable, type="global") #cluster coefficient
ADEL22_SD.degreeCent <- centralization.degree(ADEL22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_SDftn <- as.network.matrix(ADEL22_SDft)
ADEL22_SD.netDensity <- network.density(ADEL22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_SD.entropy <- entropy(ADEL22_SDft) #entropy

ADEL22_SD.netMx <- cbind(ADEL22_SD.netMx, ADEL22_SD.clusterCoef, ADEL22_SD.degreeCent$centralization,
                         ADEL22_SD.netDensity, ADEL22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
ADEL22_TDg2 <- data.frame(ADEL22_TD)
ADEL22_TDg2 <- ADEL22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_TDg2$player1
player2vector <- ADEL22_TDg2$player2
ADEL22_TDg3 <- ADEL22_TDg2
ADEL22_TDg3$p1inp2vec <- is.element(ADEL22_TDg3$player1, player2vector)
ADEL22_TDg3$p2inp1vec <- is.element(ADEL22_TDg3$player2, player1vector)

addPlayer1 <- ADEL22_TDg3[ which(ADEL22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_TDg3[ which(ADEL22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_TDg2 <- rbind(ADEL22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
ADEL22_TDft <- ftable(ADEL22_TDg2$player1, ADEL22_TDg2$player2)
ADEL22_TDft2 <- as.matrix(ADEL22_TDft)
numRows <- nrow(ADEL22_TDft2)
numCols <- ncol(ADEL22_TDft2)
ADEL22_TDft3 <- ADEL22_TDft2[c(2:numRows) , c(2:numCols)]
ADEL22_TDTable <- graph.adjacency(ADEL22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(ADEL22_TDTable, vertex.label = V(ADEL22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
ADEL22_TD.clusterCoef <- transitivity(ADEL22_TDTable, type="global") #cluster coefficient
ADEL22_TD.degreeCent <- centralization.degree(ADEL22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_TDftn <- as.network.matrix(ADEL22_TDft)
ADEL22_TD.netDensity <- network.density(ADEL22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_TD.entropy <- entropy(ADEL22_TDft) #entropy

ADEL22_TD.netMx <- cbind(ADEL22_TD.netMx, ADEL22_TD.clusterCoef, ADEL22_TD.degreeCent$centralization,
                         ADEL22_TD.netDensity, ADEL22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
ADEL22_QTg2 <- data.frame(ADEL22_QT)
ADEL22_QTg2 <- ADEL22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL22_QTg2$player1
player2vector <- ADEL22_QTg2$player2
ADEL22_QTg3 <- ADEL22_QTg2
ADEL22_QTg3$p1inp2vec <- is.element(ADEL22_QTg3$player1, player2vector)
ADEL22_QTg3$p2inp1vec <- is.element(ADEL22_QTg3$player2, player1vector)

addPlayer1 <- ADEL22_QTg3[ which(ADEL22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL22_QTg3[ which(ADEL22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL22_QTg2 <- rbind(ADEL22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
ADEL22_QTft <- ftable(ADEL22_QTg2$player1, ADEL22_QTg2$player2)
ADEL22_QTft2 <- as.matrix(ADEL22_QTft)
numRows <- nrow(ADEL22_QTft2)
numCols <- ncol(ADEL22_QTft2)
ADEL22_QTft3 <- ADEL22_QTft2[c(2:numRows) , c(2:numCols)]
ADEL22_QTTable <- graph.adjacency(ADEL22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(ADEL22_QTTable, vertex.label = V(ADEL22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
ADEL22_QT.clusterCoef <- transitivity(ADEL22_QTTable, type="global") #cluster coefficient
ADEL22_QT.degreeCent <- centralization.degree(ADEL22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL22_QTftn <- as.network.matrix(ADEL22_QTft)
ADEL22_QT.netDensity <- network.density(ADEL22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL22_QT.entropy <- entropy(ADEL22_QTft) #entropy

ADEL22_QT.netMx <- cbind(ADEL22_QT.netMx, ADEL22_QT.clusterCoef, ADEL22_QT.degreeCent$centralization,
                         ADEL22_QT.netDensity, ADEL22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL22_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************

round = 22
teamName = "BL"
KIoutcome = "Goal_F"
BL22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
BL22_Gg2 <- data.frame(BL22_G)
BL22_Gg2 <- BL22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_Gg2$player1
player2vector <- BL22_Gg2$player2
BL22_Gg3 <- BL22_Gg2
BL22_Gg3$p1inp2vec <- is.element(BL22_Gg3$player1, player2vector)
BL22_Gg3$p2inp1vec <- is.element(BL22_Gg3$player2, player1vector)

addPlayer1 <- BL22_Gg3[ which(BL22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_Gg3[ which(BL22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_Gg2 <- rbind(BL22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
BL22_Gft <- ftable(BL22_Gg2$player1, BL22_Gg2$player2)
BL22_Gft2 <- as.matrix(BL22_Gft)
numRows <- nrow(BL22_Gft2)
numCols <- ncol(BL22_Gft2)
BL22_Gft3 <- BL22_Gft2[c(2:numRows) , c(2:numCols)]
BL22_GTable <- graph.adjacency(BL22_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(BL22_GTable, vertex.label = V(BL22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
BL22_G.clusterCoef <- transitivity(BL22_GTable, type="global") #cluster coefficient
BL22_G.degreeCent <- centralization.degree(BL22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_Gftn <- as.network.matrix(BL22_Gft)
BL22_G.netDensity <- network.density(BL22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_G.entropy <- entropy(BL22_Gft) #entropy

BL22_G.netMx <- cbind(BL22_G.netMx, BL22_G.clusterCoef, BL22_G.degreeCent$centralization,
                      BL22_G.netDensity, BL22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "BL"
KIoutcome = "Behind_F"
BL22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
BL22_Bg2 <- data.frame(BL22_B)
BL22_Bg2 <- BL22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_Bg2$player1
player2vector <- BL22_Bg2$player2
BL22_Bg3 <- BL22_Bg2
BL22_Bg3$p1inp2vec <- is.element(BL22_Bg3$player1, player2vector)
BL22_Bg3$p2inp1vec <- is.element(BL22_Bg3$player2, player1vector)

addPlayer1 <- BL22_Bg3[ which(BL22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_Bg3[ which(BL22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_Bg2 <- rbind(BL22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
BL22_Bft <- ftable(BL22_Bg2$player1, BL22_Bg2$player2)
BL22_Bft2 <- as.matrix(BL22_Bft)
numRows <- nrow(BL22_Bft2)
numCols <- ncol(BL22_Bft2)
BL22_Bft3 <- BL22_Bft2[c(2:numRows) , c(2:numCols)]
BL22_BTable <- graph.adjacency(BL22_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(BL22_BTable, vertex.label = V(BL22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
BL22_B.clusterCoef <- transitivity(BL22_BTable, type="global") #cluster coefficient
BL22_B.degreeCent <- centralization.degree(BL22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_Bftn <- as.network.matrix(BL22_Bft)
BL22_B.netDensity <- network.density(BL22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_B.entropy <- entropy(BL22_Bft) #entropy

BL22_B.netMx <- cbind(BL22_B.netMx, BL22_B.clusterCoef, BL22_B.degreeCent$centralization,
                      BL22_B.netDensity, BL22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "BL"
KIoutcome = "Stoppage_F"
BL22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
BL22_SFg2 <- data.frame(BL22_SF)
BL22_SFg2 <- BL22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_SFg2$player1
player2vector <- BL22_SFg2$player2
BL22_SFg3 <- BL22_SFg2
BL22_SFg3$p1inp2vec <- is.element(BL22_SFg3$player1, player2vector)
BL22_SFg3$p2inp1vec <- is.element(BL22_SFg3$player2, player1vector)

addPlayer1 <- BL22_SFg3[ which(BL22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_SFg3[ which(BL22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_SFg2 <- rbind(BL22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
BL22_SFft <- ftable(BL22_SFg2$player1, BL22_SFg2$player2)
BL22_SFft2 <- as.matrix(BL22_SFft)
numRows <- nrow(BL22_SFft2)
numCols <- ncol(BL22_SFft2)
BL22_SFft3 <- BL22_SFft2[c(2:numRows) , c(2:numCols)]
BL22_SFTable <- graph.adjacency(BL22_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(BL22_SFTable, vertex.label = V(BL22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
BL22_SF.clusterCoef <- transitivity(BL22_SFTable, type="global") #cluster coefficient
BL22_SF.degreeCent <- centralization.degree(BL22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_SFftn <- as.network.matrix(BL22_SFft)
BL22_SF.netDensity <- network.density(BL22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_SF.entropy <- entropy(BL22_SFft) #entropy

BL22_SF.netMx <- cbind(BL22_SF.netMx, BL22_SF.clusterCoef, BL22_SF.degreeCent$centralization,
                       BL22_SF.netDensity, BL22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "BL"
KIoutcome = "Turnover_F"
BL22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
BL22_TFg2 <- data.frame(BL22_TF)
BL22_TFg2 <- BL22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_TFg2$player1
player2vector <- BL22_TFg2$player2
BL22_TFg3 <- BL22_TFg2
BL22_TFg3$p1inp2vec <- is.element(BL22_TFg3$player1, player2vector)
BL22_TFg3$p2inp1vec <- is.element(BL22_TFg3$player2, player1vector)

addPlayer1 <- BL22_TFg3[ which(BL22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL22_TFg3[ which(BL22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_TFg2 <- rbind(BL22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
BL22_TFft <- ftable(BL22_TFg2$player1, BL22_TFg2$player2)
BL22_TFft2 <- as.matrix(BL22_TFft)
numRows <- nrow(BL22_TFft2)
numCols <- ncol(BL22_TFft2)
BL22_TFft3 <- BL22_TFft2[c(2:numRows) , c(2:numCols)]
BL22_TFTable <- graph.adjacency(BL22_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(BL22_TFTable, vertex.label = V(BL22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
BL22_TF.clusterCoef <- transitivity(BL22_TFTable, type="global") #cluster coefficient
BL22_TF.degreeCent <- centralization.degree(BL22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_TFftn <- as.network.matrix(BL22_TFft)
BL22_TF.netDensity <- network.density(BL22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_TF.entropy <- entropy(BL22_TFft) #entropy

BL22_TF.netMx <- cbind(BL22_TF.netMx, BL22_TF.clusterCoef, BL22_TF.degreeCent$centralization,
                       BL22_TF.netDensity, BL22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
BL22_SAMg2 <- data.frame(BL22_SAM)
BL22_SAMg2 <- BL22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_SAMg2$player1
player2vector <- BL22_SAMg2$player2
BL22_SAMg3 <- BL22_SAMg2
BL22_SAMg3$p1inp2vec <- is.element(BL22_SAMg3$player1, player2vector)
BL22_SAMg3$p2inp1vec <- is.element(BL22_SAMg3$player2, player1vector)

addPlayer1 <- BL22_SAMg3[ which(BL22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_SAMg3[ which(BL22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_SAMg2 <- rbind(BL22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
BL22_SAMft <- ftable(BL22_SAMg2$player1, BL22_SAMg2$player2)
BL22_SAMft2 <- as.matrix(BL22_SAMft)
numRows <- nrow(BL22_SAMft2)
numCols <- ncol(BL22_SAMft2)
BL22_SAMft3 <- BL22_SAMft2[c(2:numRows) , c(2:numCols)]
BL22_SAMTable <- graph.adjacency(BL22_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(BL22_SAMTable, vertex.label = V(BL22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
BL22_SAM.clusterCoef <- transitivity(BL22_SAMTable, type="global") #cluster coefficient
BL22_SAM.degreeCent <- centralization.degree(BL22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_SAMftn <- as.network.matrix(BL22_SAMft)
BL22_SAM.netDensity <- network.density(BL22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_SAM.entropy <- entropy(BL22_SAMft) #entropy

BL22_SAM.netMx <- cbind(BL22_SAM.netMx, BL22_SAM.clusterCoef, BL22_SAM.degreeCent$centralization,
                        BL22_SAM.netDensity, BL22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "BL"
KIoutcome = "Turnover_AM"
BL22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
BL22_TAMg2 <- data.frame(BL22_TAM)
BL22_TAMg2 <- BL22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_TAMg2$player1
player2vector <- BL22_TAMg2$player2
BL22_TAMg3 <- BL22_TAMg2
BL22_TAMg3$p1inp2vec <- is.element(BL22_TAMg3$player1, player2vector)
BL22_TAMg3$p2inp1vec <- is.element(BL22_TAMg3$player2, player1vector)

addPlayer1 <- BL22_TAMg3[ which(BL22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_TAMg3[ which(BL22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_TAMg2 <- rbind(BL22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
BL22_TAMft <- ftable(BL22_TAMg2$player1, BL22_TAMg2$player2)
BL22_TAMft2 <- as.matrix(BL22_TAMft)
numRows <- nrow(BL22_TAMft2)
numCols <- ncol(BL22_TAMft2)
BL22_TAMft3 <- BL22_TAMft2[c(2:numRows) , c(2:numCols)]
BL22_TAMTable <- graph.adjacency(BL22_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(BL22_TAMTable, vertex.label = V(BL22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
BL22_TAM.clusterCoef <- transitivity(BL22_TAMTable, type="global") #cluster coefficient
BL22_TAM.degreeCent <- centralization.degree(BL22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_TAMftn <- as.network.matrix(BL22_TAMft)
BL22_TAM.netDensity <- network.density(BL22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_TAM.entropy <- entropy(BL22_TAMft) #entropy

BL22_TAM.netMx <- cbind(BL22_TAM.netMx, BL22_TAM.clusterCoef, BL22_TAM.degreeCent$centralization,
                        BL22_TAM.netDensity, BL22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
BL22_SDMg2 <- data.frame(BL22_SDM)
BL22_SDMg2 <- BL22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_SDMg2$player1
player2vector <- BL22_SDMg2$player2
BL22_SDMg3 <- BL22_SDMg2
BL22_SDMg3$p1inp2vec <- is.element(BL22_SDMg3$player1, player2vector)
BL22_SDMg3$p2inp1vec <- is.element(BL22_SDMg3$player2, player1vector)

addPlayer1 <- BL22_SDMg3[ which(BL22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_SDMg3[ which(BL22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_SDMg2 <- rbind(BL22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
BL22_SDMft <- ftable(BL22_SDMg2$player1, BL22_SDMg2$player2)
BL22_SDMft2 <- as.matrix(BL22_SDMft)
numRows <- nrow(BL22_SDMft2)
numCols <- ncol(BL22_SDMft2)
BL22_SDMft3 <- BL22_SDMft2[c(2:numRows) , c(2:numCols)]
BL22_SDMTable <- graph.adjacency(BL22_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(BL22_SDMTable, vertex.label = V(BL22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
BL22_SDM.clusterCoef <- transitivity(BL22_SDMTable, type="global") #cluster coefficient
BL22_SDM.degreeCent <- centralization.degree(BL22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_SDMftn <- as.network.matrix(BL22_SDMft)
BL22_SDM.netDensity <- network.density(BL22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_SDM.entropy <- entropy(BL22_SDMft) #entropy

BL22_SDM.netMx <- cbind(BL22_SDM.netMx, BL22_SDM.clusterCoef, BL22_SDM.degreeCent$centralization,
                        BL22_SDM.netDensity, BL22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************
#NA

round = 22
teamName = "BL"
KIoutcome = "Turnover_DM"
BL22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
BL22_TDMg2 <- data.frame(BL22_TDM)
BL22_TDMg2 <- BL22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_TDMg2$player1
player2vector <- BL22_TDMg2$player2
BL22_TDMg3 <- BL22_TDMg2
BL22_TDMg3$p1inp2vec <- is.element(BL22_TDMg3$player1, player2vector)
BL22_TDMg3$p2inp1vec <- is.element(BL22_TDMg3$player2, player1vector)

addPlayer1 <- BL22_TDMg3[ which(BL22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_TDMg3[ which(BL22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_TDMg2 <- rbind(BL22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
BL22_TDMft <- ftable(BL22_TDMg2$player1, BL22_TDMg2$player2)
BL22_TDMft2 <- as.matrix(BL22_TDMft)
numRows <- nrow(BL22_TDMft2)
numCols <- ncol(BL22_TDMft2)
BL22_TDMft3 <- BL22_TDMft2[c(2:numRows) , c(2:numCols)]
BL22_TDMTable <- graph.adjacency(BL22_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(BL22_TDMTable, vertex.label = V(BL22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
BL22_TDM.clusterCoef <- transitivity(BL22_TDMTable, type="global") #cluster coefficient
BL22_TDM.degreeCent <- centralization.degree(BL22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_TDMftn <- as.network.matrix(BL22_TDMft)
BL22_TDM.netDensity <- network.density(BL22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_TDM.entropy <- entropy(BL22_TDMft) #entropy

BL22_TDM.netMx <- cbind(BL22_TDM.netMx, BL22_TDM.clusterCoef, BL22_TDM.degreeCent$centralization,
                        BL22_TDM.netDensity, BL22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************

round = 22
teamName = "BL"
KIoutcome = "Stoppage_D"
BL22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
BL22_SDg2 <- data.frame(BL22_SD)
BL22_SDg2 <- BL22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_SDg2$player1
player2vector <- BL22_SDg2$player2
BL22_SDg3 <- BL22_SDg2
BL22_SDg3$p1inp2vec <- is.element(BL22_SDg3$player1, player2vector)
BL22_SDg3$p2inp1vec <- is.element(BL22_SDg3$player2, player1vector)

addPlayer1 <- BL22_SDg3[ which(BL22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_SDg3[ which(BL22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_SDg2 <- rbind(BL22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
BL22_SDft <- ftable(BL22_SDg2$player1, BL22_SDg2$player2)
BL22_SDft2 <- as.matrix(BL22_SDft)
numRows <- nrow(BL22_SDft2)
numCols <- ncol(BL22_SDft2)
BL22_SDft3 <- BL22_SDft2[c(2:numRows) , c(2:numCols)]
BL22_SDTable <- graph.adjacency(BL22_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(BL22_SDTable, vertex.label = V(BL22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
BL22_SD.clusterCoef <- transitivity(BL22_SDTable, type="global") #cluster coefficient
BL22_SD.degreeCent <- centralization.degree(BL22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_SDftn <- as.network.matrix(BL22_SDft)
BL22_SD.netDensity <- network.density(BL22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_SD.entropy <- entropy(BL22_SDft) #entropy

BL22_SD.netMx <- cbind(BL22_SD.netMx, BL22_SD.clusterCoef, BL22_SD.degreeCent$centralization,
                       BL22_SD.netDensity, BL22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "BL"
KIoutcome = "Turnover_D"
BL22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
BL22_TDg2 <- data.frame(BL22_TD)
BL22_TDg2 <- BL22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_TDg2$player1
player2vector <- BL22_TDg2$player2
BL22_TDg3 <- BL22_TDg2
BL22_TDg3$p1inp2vec <- is.element(BL22_TDg3$player1, player2vector)
BL22_TDg3$p2inp1vec <- is.element(BL22_TDg3$player2, player1vector)

addPlayer1 <- BL22_TDg3[ which(BL22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_TDg3[ which(BL22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_TDg2 <- rbind(BL22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
BL22_TDft <- ftable(BL22_TDg2$player1, BL22_TDg2$player2)
BL22_TDft2 <- as.matrix(BL22_TDft)
numRows <- nrow(BL22_TDft2)
numCols <- ncol(BL22_TDft2)
BL22_TDft3 <- BL22_TDft2[c(2:numRows) , c(2:numCols)]
BL22_TDTable <- graph.adjacency(BL22_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(BL22_TDTable, vertex.label = V(BL22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
BL22_TD.clusterCoef <- transitivity(BL22_TDTable, type="global") #cluster coefficient
BL22_TD.degreeCent <- centralization.degree(BL22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_TDftn <- as.network.matrix(BL22_TDft)
BL22_TD.netDensity <- network.density(BL22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_TD.entropy <- entropy(BL22_TDft) #entropy

BL22_TD.netMx <- cbind(BL22_TD.netMx, BL22_TD.clusterCoef, BL22_TD.degreeCent$centralization,
                       BL22_TD.netDensity, BL22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
BL22_QTg2 <- data.frame(BL22_QT)
BL22_QTg2 <- BL22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL22_QTg2$player1
player2vector <- BL22_QTg2$player2
BL22_QTg3 <- BL22_QTg2
BL22_QTg3$p1inp2vec <- is.element(BL22_QTg3$player1, player2vector)
BL22_QTg3$p2inp1vec <- is.element(BL22_QTg3$player2, player1vector)

addPlayer1 <- BL22_QTg3[ which(BL22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL22_QTg3[ which(BL22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL22_QTg2 <- rbind(BL22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
BL22_QTft <- ftable(BL22_QTg2$player1, BL22_QTg2$player2)
BL22_QTft2 <- as.matrix(BL22_QTft)
numRows <- nrow(BL22_QTft2)
numCols <- ncol(BL22_QTft2)
BL22_QTft3 <- BL22_QTft2[c(2:numRows) , c(2:numCols)]
BL22_QTTable <- graph.adjacency(BL22_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(BL22_QTTable, vertex.label = V(BL22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
BL22_QT.clusterCoef <- transitivity(BL22_QTTable, type="global") #cluster coefficient
BL22_QT.degreeCent <- centralization.degree(BL22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL22_QTftn <- as.network.matrix(BL22_QTft)
BL22_QT.netDensity <- network.density(BL22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL22_QT.entropy <- entropy(BL22_QTft) #entropy

BL22_QT.netMx <- cbind(BL22_QT.netMx, BL22_QT.clusterCoef, BL22_QT.degreeCent$centralization,
                       BL22_QT.netDensity, BL22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL22_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "CARL"
KIoutcome = "Goal_F"
CARL22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
CARL22_Gg2 <- data.frame(CARL22_G)
CARL22_Gg2 <- CARL22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_Gg2$player1
player2vector <- CARL22_Gg2$player2
CARL22_Gg3 <- CARL22_Gg2
CARL22_Gg3$p1inp2vec <- is.element(CARL22_Gg3$player1, player2vector)
CARL22_Gg3$p2inp1vec <- is.element(CARL22_Gg3$player2, player1vector)

addPlayer1 <- CARL22_Gg3[ which(CARL22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_Gg3[ which(CARL22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_Gg2 <- rbind(CARL22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
CARL22_Gft <- ftable(CARL22_Gg2$player1, CARL22_Gg2$player2)
CARL22_Gft2 <- as.matrix(CARL22_Gft)
numRows <- nrow(CARL22_Gft2)
numCols <- ncol(CARL22_Gft2)
CARL22_Gft3 <- CARL22_Gft2[c(2:numRows) , c(2:numCols)]
CARL22_GTable <- graph.adjacency(CARL22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(CARL22_GTable, vertex.label = V(CARL22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
CARL22_G.clusterCoef <- transitivity(CARL22_GTable, type="global") #cluster coefficient
CARL22_G.degreeCent <- centralization.degree(CARL22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_Gftn <- as.network.matrix(CARL22_Gft)
CARL22_G.netDensity <- network.density(CARL22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_G.entropy <- entropy(CARL22_Gft) #entropy

CARL22_G.netMx <- cbind(CARL22_G.netMx, CARL22_G.clusterCoef, CARL22_G.degreeCent$centralization,
                        CARL22_G.netDensity, CARL22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************

round = 22
teamName = "CARL"
KIoutcome = "Behind_F"
CARL22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
CARL22_Bg2 <- data.frame(CARL22_B)
CARL22_Bg2 <- CARL22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_Bg2$player1
player2vector <- CARL22_Bg2$player2
CARL22_Bg3 <- CARL22_Bg2
CARL22_Bg3$p1inp2vec <- is.element(CARL22_Bg3$player1, player2vector)
CARL22_Bg3$p2inp1vec <- is.element(CARL22_Bg3$player2, player1vector)

addPlayer1 <- CARL22_Bg3[ which(CARL22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_Bg3[ which(CARL22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_Bg2 <- rbind(CARL22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
CARL22_Bft <- ftable(CARL22_Bg2$player1, CARL22_Bg2$player2)
CARL22_Bft2 <- as.matrix(CARL22_Bft)
numRows <- nrow(CARL22_Bft2)
numCols <- ncol(CARL22_Bft2)
CARL22_Bft3 <- CARL22_Bft2[c(2:numRows) , c(2:numCols)]
CARL22_BTable <- graph.adjacency(CARL22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(CARL22_BTable, vertex.label = V(CARL22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
CARL22_B.clusterCoef <- transitivity(CARL22_BTable, type="global") #cluster coefficient
CARL22_B.degreeCent <- centralization.degree(CARL22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_Bftn <- as.network.matrix(CARL22_Bft)
CARL22_B.netDensity <- network.density(CARL22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_B.entropy <- entropy(CARL22_Bft) #entropy

CARL22_B.netMx <- cbind(CARL22_B.netMx, CARL22_B.clusterCoef, CARL22_B.degreeCent$centralization,
                        CARL22_B.netDensity, CARL22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
CARL22_SFg2 <- data.frame(CARL22_SF)
CARL22_SFg2 <- CARL22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_SFg2$player1
player2vector <- CARL22_SFg2$player2
CARL22_SFg3 <- CARL22_SFg2
CARL22_SFg3$p1inp2vec <- is.element(CARL22_SFg3$player1, player2vector)
CARL22_SFg3$p2inp1vec <- is.element(CARL22_SFg3$player2, player1vector)

addPlayer1 <- CARL22_SFg3[ which(CARL22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_SFg3[ which(CARL22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_SFg2 <- rbind(CARL22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
CARL22_SFft <- ftable(CARL22_SFg2$player1, CARL22_SFg2$player2)
CARL22_SFft2 <- as.matrix(CARL22_SFft)
numRows <- nrow(CARL22_SFft2)
numCols <- ncol(CARL22_SFft2)
CARL22_SFft3 <- CARL22_SFft2[c(2:numRows) , c(2:numCols)]
CARL22_SFTable <- graph.adjacency(CARL22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(CARL22_SFTable, vertex.label = V(CARL22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
CARL22_SF.clusterCoef <- transitivity(CARL22_SFTable, type="global") #cluster coefficient
CARL22_SF.degreeCent <- centralization.degree(CARL22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_SFftn <- as.network.matrix(CARL22_SFft)
CARL22_SF.netDensity <- network.density(CARL22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_SF.entropy <- entropy(CARL22_SFft) #entropy

CARL22_SF.netMx <- cbind(CARL22_SF.netMx, CARL22_SF.clusterCoef, CARL22_SF.degreeCent$centralization,
                         CARL22_SF.netDensity, CARL22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
CARL22_TFg2 <- data.frame(CARL22_TF)
CARL22_TFg2 <- CARL22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_TFg2$player1
player2vector <- CARL22_TFg2$player2
CARL22_TFg3 <- CARL22_TFg2
CARL22_TFg3$p1inp2vec <- is.element(CARL22_TFg3$player1, player2vector)
CARL22_TFg3$p2inp1vec <- is.element(CARL22_TFg3$player2, player1vector)

addPlayer1 <- CARL22_TFg3[ which(CARL22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL22_TFg3[ which(CARL22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_TFg2 <- rbind(CARL22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
CARL22_TFft <- ftable(CARL22_TFg2$player1, CARL22_TFg2$player2)
CARL22_TFft2 <- as.matrix(CARL22_TFft)
numRows <- nrow(CARL22_TFft2)
numCols <- ncol(CARL22_TFft2)
CARL22_TFft3 <- CARL22_TFft2[c(2:numRows) , c(2:numCols)]
CARL22_TFTable <- graph.adjacency(CARL22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(CARL22_TFTable, vertex.label = V(CARL22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
CARL22_TF.clusterCoef <- transitivity(CARL22_TFTable, type="global") #cluster coefficient
CARL22_TF.degreeCent <- centralization.degree(CARL22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_TFftn <- as.network.matrix(CARL22_TFft)
CARL22_TF.netDensity <- network.density(CARL22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_TF.entropy <- entropy(CARL22_TFft) #entropy

CARL22_TF.netMx <- cbind(CARL22_TF.netMx, CARL22_TF.clusterCoef, CARL22_TF.degreeCent$centralization,
                         CARL22_TF.netDensity, CARL22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
CARL22_SAMg2 <- data.frame(CARL22_SAM)
CARL22_SAMg2 <- CARL22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_SAMg2$player1
player2vector <- CARL22_SAMg2$player2
CARL22_SAMg3 <- CARL22_SAMg2
CARL22_SAMg3$p1inp2vec <- is.element(CARL22_SAMg3$player1, player2vector)
CARL22_SAMg3$p2inp1vec <- is.element(CARL22_SAMg3$player2, player1vector)

addPlayer1 <- CARL22_SAMg3[ which(CARL22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_SAMg3[ which(CARL22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_SAMg2 <- rbind(CARL22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
CARL22_SAMft <- ftable(CARL22_SAMg2$player1, CARL22_SAMg2$player2)
CARL22_SAMft2 <- as.matrix(CARL22_SAMft)
numRows <- nrow(CARL22_SAMft2)
numCols <- ncol(CARL22_SAMft2)
CARL22_SAMft3 <- CARL22_SAMft2[c(2:numRows) , c(2:numCols)]
CARL22_SAMTable <- graph.adjacency(CARL22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(CARL22_SAMTable, vertex.label = V(CARL22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
CARL22_SAM.clusterCoef <- transitivity(CARL22_SAMTable, type="global") #cluster coefficient
CARL22_SAM.degreeCent <- centralization.degree(CARL22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_SAMftn <- as.network.matrix(CARL22_SAMft)
CARL22_SAM.netDensity <- network.density(CARL22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_SAM.entropy <- entropy(CARL22_SAMft) #entropy

CARL22_SAM.netMx <- cbind(CARL22_SAM.netMx, CARL22_SAM.clusterCoef, CARL22_SAM.degreeCent$centralization,
                          CARL22_SAM.netDensity, CARL22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
CARL22_TAMg2 <- data.frame(CARL22_TAM)
CARL22_TAMg2 <- CARL22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_TAMg2$player1
player2vector <- CARL22_TAMg2$player2
CARL22_TAMg3 <- CARL22_TAMg2
CARL22_TAMg3$p1inp2vec <- is.element(CARL22_TAMg3$player1, player2vector)
CARL22_TAMg3$p2inp1vec <- is.element(CARL22_TAMg3$player2, player1vector)

addPlayer1 <- CARL22_TAMg3[ which(CARL22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_TAMg3[ which(CARL22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_TAMg2 <- rbind(CARL22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
CARL22_TAMft <- ftable(CARL22_TAMg2$player1, CARL22_TAMg2$player2)
CARL22_TAMft2 <- as.matrix(CARL22_TAMft)
numRows <- nrow(CARL22_TAMft2)
numCols <- ncol(CARL22_TAMft2)
CARL22_TAMft3 <- CARL22_TAMft2[c(2:numRows) , c(2:numCols)]
CARL22_TAMTable <- graph.adjacency(CARL22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(CARL22_TAMTable, vertex.label = V(CARL22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
CARL22_TAM.clusterCoef <- transitivity(CARL22_TAMTable, type="global") #cluster coefficient
CARL22_TAM.degreeCent <- centralization.degree(CARL22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_TAMftn <- as.network.matrix(CARL22_TAMft)
CARL22_TAM.netDensity <- network.density(CARL22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_TAM.entropy <- entropy(CARL22_TAMft) #entropy

CARL22_TAM.netMx <- cbind(CARL22_TAM.netMx, CARL22_TAM.clusterCoef, CARL22_TAM.degreeCent$centralization,
                          CARL22_TAM.netDensity, CARL22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
CARL22_SDMg2 <- data.frame(CARL22_SDM)
CARL22_SDMg2 <- CARL22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_SDMg2$player1
player2vector <- CARL22_SDMg2$player2
CARL22_SDMg3 <- CARL22_SDMg2
CARL22_SDMg3$p1inp2vec <- is.element(CARL22_SDMg3$player1, player2vector)
CARL22_SDMg3$p2inp1vec <- is.element(CARL22_SDMg3$player2, player1vector)

addPlayer1 <- CARL22_SDMg3[ which(CARL22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_SDMg3[ which(CARL22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_SDMg2 <- rbind(CARL22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
CARL22_SDMft <- ftable(CARL22_SDMg2$player1, CARL22_SDMg2$player2)
CARL22_SDMft2 <- as.matrix(CARL22_SDMft)
numRows <- nrow(CARL22_SDMft2)
numCols <- ncol(CARL22_SDMft2)
CARL22_SDMft3 <- CARL22_SDMft2[c(2:numRows) , c(2:numCols)]
CARL22_SDMTable <- graph.adjacency(CARL22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(CARL22_SDMTable, vertex.label = V(CARL22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
CARL22_SDM.clusterCoef <- transitivity(CARL22_SDMTable, type="global") #cluster coefficient
CARL22_SDM.degreeCent <- centralization.degree(CARL22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_SDMftn <- as.network.matrix(CARL22_SDMft)
CARL22_SDM.netDensity <- network.density(CARL22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_SDM.entropy <- entropy(CARL22_SDMft) #entropy

CARL22_SDM.netMx <- cbind(CARL22_SDM.netMx, CARL22_SDM.clusterCoef, CARL22_SDM.degreeCent$centralization,
                          CARL22_SDM.netDensity, CARL22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************
#NA

round = 22
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
CARL22_TDMg2 <- data.frame(CARL22_TDM)
CARL22_TDMg2 <- CARL22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_TDMg2$player1
player2vector <- CARL22_TDMg2$player2
CARL22_TDMg3 <- CARL22_TDMg2
CARL22_TDMg3$p1inp2vec <- is.element(CARL22_TDMg3$player1, player2vector)
CARL22_TDMg3$p2inp1vec <- is.element(CARL22_TDMg3$player2, player1vector)

addPlayer1 <- CARL22_TDMg3[ which(CARL22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_TDMg3[ which(CARL22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_TDMg2 <- rbind(CARL22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
CARL22_TDMft <- ftable(CARL22_TDMg2$player1, CARL22_TDMg2$player2)
CARL22_TDMft2 <- as.matrix(CARL22_TDMft)
numRows <- nrow(CARL22_TDMft2)
numCols <- ncol(CARL22_TDMft2)
CARL22_TDMft3 <- CARL22_TDMft2[c(2:numRows) , c(2:numCols)]
CARL22_TDMTable <- graph.adjacency(CARL22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(CARL22_TDMTable, vertex.label = V(CARL22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
CARL22_TDM.clusterCoef <- transitivity(CARL22_TDMTable, type="global") #cluster coefficient
CARL22_TDM.degreeCent <- centralization.degree(CARL22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_TDMftn <- as.network.matrix(CARL22_TDMft)
CARL22_TDM.netDensity <- network.density(CARL22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_TDM.entropy <- entropy(CARL22_TDMft) #entropy

CARL22_TDM.netMx <- cbind(CARL22_TDM.netMx, CARL22_TDM.clusterCoef, CARL22_TDM.degreeCent$centralization,
                          CARL22_TDM.netDensity, CARL22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
CARL22_SDg2 <- data.frame(CARL22_SD)
CARL22_SDg2 <- CARL22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_SDg2$player1
player2vector <- CARL22_SDg2$player2
CARL22_SDg3 <- CARL22_SDg2
CARL22_SDg3$p1inp2vec <- is.element(CARL22_SDg3$player1, player2vector)
CARL22_SDg3$p2inp1vec <- is.element(CARL22_SDg3$player2, player1vector)

addPlayer1 <- CARL22_SDg3[ which(CARL22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_SDg3[ which(CARL22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_SDg2 <- rbind(CARL22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
CARL22_SDft <- ftable(CARL22_SDg2$player1, CARL22_SDg2$player2)
CARL22_SDft2 <- as.matrix(CARL22_SDft)
numRows <- nrow(CARL22_SDft2)
numCols <- ncol(CARL22_SDft2)
CARL22_SDft3 <- CARL22_SDft2[c(2:numRows) , c(2:numCols)]
CARL22_SDTable <- graph.adjacency(CARL22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(CARL22_SDTable, vertex.label = V(CARL22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
CARL22_SD.clusterCoef <- transitivity(CARL22_SDTable, type="global") #cluster coefficient
CARL22_SD.degreeCent <- centralization.degree(CARL22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_SDftn <- as.network.matrix(CARL22_SDft)
CARL22_SD.netDensity <- network.density(CARL22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_SD.entropy <- entropy(CARL22_SDft) #entropy

CARL22_SD.netMx <- cbind(CARL22_SD.netMx, CARL22_SD.clusterCoef, CARL22_SD.degreeCent$centralization,
                         CARL22_SD.netDensity, CARL22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
CARL22_TDg2 <- data.frame(CARL22_TD)
CARL22_TDg2 <- CARL22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_TDg2$player1
player2vector <- CARL22_TDg2$player2
CARL22_TDg3 <- CARL22_TDg2
CARL22_TDg3$p1inp2vec <- is.element(CARL22_TDg3$player1, player2vector)
CARL22_TDg3$p2inp1vec <- is.element(CARL22_TDg3$player2, player1vector)

addPlayer1 <- CARL22_TDg3[ which(CARL22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_TDg3[ which(CARL22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_TDg2 <- rbind(CARL22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
CARL22_TDft <- ftable(CARL22_TDg2$player1, CARL22_TDg2$player2)
CARL22_TDft2 <- as.matrix(CARL22_TDft)
numRows <- nrow(CARL22_TDft2)
numCols <- ncol(CARL22_TDft2)
CARL22_TDft3 <- CARL22_TDft2[c(2:numRows) , c(2:numCols)]
CARL22_TDTable <- graph.adjacency(CARL22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(CARL22_TDTable, vertex.label = V(CARL22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
CARL22_TD.clusterCoef <- transitivity(CARL22_TDTable, type="global") #cluster coefficient
CARL22_TD.degreeCent <- centralization.degree(CARL22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_TDftn <- as.network.matrix(CARL22_TDft)
CARL22_TD.netDensity <- network.density(CARL22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_TD.entropy <- entropy(CARL22_TDft) #entropy

CARL22_TD.netMx <- cbind(CARL22_TD.netMx, CARL22_TD.clusterCoef, CARL22_TD.degreeCent$centralization,
                         CARL22_TD.netDensity, CARL22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
CARL22_QTg2 <- data.frame(CARL22_QT)
CARL22_QTg2 <- CARL22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL22_QTg2$player1
player2vector <- CARL22_QTg2$player2
CARL22_QTg3 <- CARL22_QTg2
CARL22_QTg3$p1inp2vec <- is.element(CARL22_QTg3$player1, player2vector)
CARL22_QTg3$p2inp1vec <- is.element(CARL22_QTg3$player2, player1vector)

addPlayer1 <- CARL22_QTg3[ which(CARL22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL22_QTg3[ which(CARL22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL22_QTg2 <- rbind(CARL22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
CARL22_QTft <- ftable(CARL22_QTg2$player1, CARL22_QTg2$player2)
CARL22_QTft2 <- as.matrix(CARL22_QTft)
numRows <- nrow(CARL22_QTft2)
numCols <- ncol(CARL22_QTft2)
CARL22_QTft3 <- CARL22_QTft2[c(2:numRows) , c(2:numCols)]
CARL22_QTTable <- graph.adjacency(CARL22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(CARL22_QTTable, vertex.label = V(CARL22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
CARL22_QT.clusterCoef <- transitivity(CARL22_QTTable, type="global") #cluster coefficient
CARL22_QT.degreeCent <- centralization.degree(CARL22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL22_QTftn <- as.network.matrix(CARL22_QTft)
CARL22_QT.netDensity <- network.density(CARL22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL22_QT.entropy <- entropy(CARL22_QTft) #entropy

CARL22_QT.netMx <- cbind(CARL22_QT.netMx, CARL22_QT.clusterCoef, CARL22_QT.degreeCent$centralization,
                         CARL22_QT.netDensity, CARL22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL22_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Goal_F"
COLL22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
COLL22_Gg2 <- data.frame(COLL22_G)
COLL22_Gg2 <- COLL22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_Gg2$player1
player2vector <- COLL22_Gg2$player2
COLL22_Gg3 <- COLL22_Gg2
COLL22_Gg3$p1inp2vec <- is.element(COLL22_Gg3$player1, player2vector)
COLL22_Gg3$p2inp1vec <- is.element(COLL22_Gg3$player2, player1vector)

addPlayer1 <- COLL22_Gg3[ which(COLL22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_Gg3[ which(COLL22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_Gg2 <- rbind(COLL22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
COLL22_Gft <- ftable(COLL22_Gg2$player1, COLL22_Gg2$player2)
COLL22_Gft2 <- as.matrix(COLL22_Gft)
numRows <- nrow(COLL22_Gft2)
numCols <- ncol(COLL22_Gft2)
COLL22_Gft3 <- COLL22_Gft2[c(2:numRows) , c(2:numCols)]
COLL22_GTable <- graph.adjacency(COLL22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(COLL22_GTable, vertex.label = V(COLL22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
COLL22_G.clusterCoef <- transitivity(COLL22_GTable, type="global") #cluster coefficient
COLL22_G.degreeCent <- centralization.degree(COLL22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_Gftn <- as.network.matrix(COLL22_Gft)
COLL22_G.netDensity <- network.density(COLL22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_G.entropy <- entropy(COLL22_Gft) #entropy

COLL22_G.netMx <- cbind(COLL22_G.netMx, COLL22_G.clusterCoef, COLL22_G.degreeCent$centralization,
                        COLL22_G.netDensity, COLL22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Behind_F"
COLL22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
COLL22_Bg2 <- data.frame(COLL22_B)
COLL22_Bg2 <- COLL22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_Bg2$player1
player2vector <- COLL22_Bg2$player2
COLL22_Bg3 <- COLL22_Bg2
COLL22_Bg3$p1inp2vec <- is.element(COLL22_Bg3$player1, player2vector)
COLL22_Bg3$p2inp1vec <- is.element(COLL22_Bg3$player2, player1vector)

addPlayer1 <- COLL22_Bg3[ which(COLL22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_Bg3[ which(COLL22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_Bg2 <- rbind(COLL22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
COLL22_Bft <- ftable(COLL22_Bg2$player1, COLL22_Bg2$player2)
COLL22_Bft2 <- as.matrix(COLL22_Bft)
numRows <- nrow(COLL22_Bft2)
numCols <- ncol(COLL22_Bft2)
COLL22_Bft3 <- COLL22_Bft2[c(2:numRows) , c(2:numCols)]
COLL22_BTable <- graph.adjacency(COLL22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(COLL22_BTable, vertex.label = V(COLL22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
COLL22_B.clusterCoef <- transitivity(COLL22_BTable, type="global") #cluster coefficient
COLL22_B.degreeCent <- centralization.degree(COLL22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_Bftn <- as.network.matrix(COLL22_Bft)
COLL22_B.netDensity <- network.density(COLL22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_B.entropy <- entropy(COLL22_Bft) #entropy

COLL22_B.netMx <- cbind(COLL22_B.netMx, COLL22_B.clusterCoef, COLL22_B.degreeCent$centralization,
                        COLL22_B.netDensity, COLL22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
COLL22_SFg2 <- data.frame(COLL22_SF)
COLL22_SFg2 <- COLL22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_SFg2$player1
player2vector <- COLL22_SFg2$player2
COLL22_SFg3 <- COLL22_SFg2
COLL22_SFg3$p1inp2vec <- is.element(COLL22_SFg3$player1, player2vector)
COLL22_SFg3$p2inp1vec <- is.element(COLL22_SFg3$player2, player1vector)

addPlayer1 <- COLL22_SFg3[ which(COLL22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_SFg3[ which(COLL22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_SFg2 <- rbind(COLL22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
COLL22_SFft <- ftable(COLL22_SFg2$player1, COLL22_SFg2$player2)
COLL22_SFft2 <- as.matrix(COLL22_SFft)
numRows <- nrow(COLL22_SFft2)
numCols <- ncol(COLL22_SFft2)
COLL22_SFft3 <- COLL22_SFft2[c(2:numRows) , c(2:numCols)]
COLL22_SFTable <- graph.adjacency(COLL22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(COLL22_SFTable, vertex.label = V(COLL22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
COLL22_SF.clusterCoef <- transitivity(COLL22_SFTable, type="global") #cluster coefficient
COLL22_SF.degreeCent <- centralization.degree(COLL22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_SFftn <- as.network.matrix(COLL22_SFft)
COLL22_SF.netDensity <- network.density(COLL22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_SF.entropy <- entropy(COLL22_SFft) #entropy

COLL22_SF.netMx <- cbind(COLL22_SF.netMx, COLL22_SF.clusterCoef, COLL22_SF.degreeCent$centralization,
                         COLL22_SF.netDensity, COLL22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
COLL22_TFg2 <- data.frame(COLL22_TF)
COLL22_TFg2 <- COLL22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_TFg2$player1
player2vector <- COLL22_TFg2$player2
COLL22_TFg3 <- COLL22_TFg2
COLL22_TFg3$p1inp2vec <- is.element(COLL22_TFg3$player1, player2vector)
COLL22_TFg3$p2inp1vec <- is.element(COLL22_TFg3$player2, player1vector)

addPlayer1 <- COLL22_TFg3[ which(COLL22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_TFg3[ which(COLL22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_TFg2 <- rbind(COLL22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
COLL22_TFft <- ftable(COLL22_TFg2$player1, COLL22_TFg2$player2)
COLL22_TFft2 <- as.matrix(COLL22_TFft)
numRows <- nrow(COLL22_TFft2)
numCols <- ncol(COLL22_TFft2)
COLL22_TFft3 <- COLL22_TFft2[c(2:numRows) , c(2:numCols)]
COLL22_TFTable <- graph.adjacency(COLL22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(COLL22_TFTable, vertex.label = V(COLL22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
COLL22_TF.clusterCoef <- transitivity(COLL22_TFTable, type="global") #cluster coefficient
COLL22_TF.degreeCent <- centralization.degree(COLL22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_TFftn <- as.network.matrix(COLL22_TFft)
COLL22_TF.netDensity <- network.density(COLL22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_TF.entropy <- entropy(COLL22_TFft) #entropy

COLL22_TF.netMx <- cbind(COLL22_TF.netMx, COLL22_TF.clusterCoef, COLL22_TF.degreeCent$centralization,
                         COLL22_TF.netDensity, COLL22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
COLL22_SAMg2 <- data.frame(COLL22_SAM)
COLL22_SAMg2 <- COLL22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_SAMg2$player1
player2vector <- COLL22_SAMg2$player2
COLL22_SAMg3 <- COLL22_SAMg2
COLL22_SAMg3$p1inp2vec <- is.element(COLL22_SAMg3$player1, player2vector)
COLL22_SAMg3$p2inp1vec <- is.element(COLL22_SAMg3$player2, player1vector)

addPlayer1 <- COLL22_SAMg3[ which(COLL22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_SAMg3[ which(COLL22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_SAMg2 <- rbind(COLL22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
COLL22_SAMft <- ftable(COLL22_SAMg2$player1, COLL22_SAMg2$player2)
COLL22_SAMft2 <- as.matrix(COLL22_SAMft)
numRows <- nrow(COLL22_SAMft2)
numCols <- ncol(COLL22_SAMft2)
COLL22_SAMft3 <- COLL22_SAMft2[c(2:numRows) , c(2:numCols)]
COLL22_SAMTable <- graph.adjacency(COLL22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(COLL22_SAMTable, vertex.label = V(COLL22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
COLL22_SAM.clusterCoef <- transitivity(COLL22_SAMTable, type="global") #cluster coefficient
COLL22_SAM.degreeCent <- centralization.degree(COLL22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_SAMftn <- as.network.matrix(COLL22_SAMft)
COLL22_SAM.netDensity <- network.density(COLL22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_SAM.entropy <- entropy(COLL22_SAMft) #entropy

COLL22_SAM.netMx <- cbind(COLL22_SAM.netMx, COLL22_SAM.clusterCoef, COLL22_SAM.degreeCent$centralization,
                          COLL22_SAM.netDensity, COLL22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
COLL22_TAMg2 <- data.frame(COLL22_TAM)
COLL22_TAMg2 <- COLL22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_TAMg2$player1
player2vector <- COLL22_TAMg2$player2
COLL22_TAMg3 <- COLL22_TAMg2
COLL22_TAMg3$p1inp2vec <- is.element(COLL22_TAMg3$player1, player2vector)
COLL22_TAMg3$p2inp1vec <- is.element(COLL22_TAMg3$player2, player1vector)

addPlayer1 <- COLL22_TAMg3[ which(COLL22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_TAMg3[ which(COLL22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_TAMg2 <- rbind(COLL22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
COLL22_TAMft <- ftable(COLL22_TAMg2$player1, COLL22_TAMg2$player2)
COLL22_TAMft2 <- as.matrix(COLL22_TAMft)
numRows <- nrow(COLL22_TAMft2)
numCols <- ncol(COLL22_TAMft2)
COLL22_TAMft3 <- COLL22_TAMft2[c(2:numRows) , c(2:numCols)]
COLL22_TAMTable <- graph.adjacency(COLL22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(COLL22_TAMTable, vertex.label = V(COLL22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
COLL22_TAM.clusterCoef <- transitivity(COLL22_TAMTable, type="global") #cluster coefficient
COLL22_TAM.degreeCent <- centralization.degree(COLL22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_TAMftn <- as.network.matrix(COLL22_TAMft)
COLL22_TAM.netDensity <- network.density(COLL22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_TAM.entropy <- entropy(COLL22_TAMft) #entropy

COLL22_TAM.netMx <- cbind(COLL22_TAM.netMx, COLL22_TAM.clusterCoef, COLL22_TAM.degreeCent$centralization,
                          COLL22_TAM.netDensity, COLL22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
COLL22_SDMg2 <- data.frame(COLL22_SDM)
COLL22_SDMg2 <- COLL22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_SDMg2$player1
player2vector <- COLL22_SDMg2$player2
COLL22_SDMg3 <- COLL22_SDMg2
COLL22_SDMg3$p1inp2vec <- is.element(COLL22_SDMg3$player1, player2vector)
COLL22_SDMg3$p2inp1vec <- is.element(COLL22_SDMg3$player2, player1vector)

addPlayer1 <- COLL22_SDMg3[ which(COLL22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_SDMg3[ which(COLL22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_SDMg2 <- rbind(COLL22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
COLL22_SDMft <- ftable(COLL22_SDMg2$player1, COLL22_SDMg2$player2)
COLL22_SDMft2 <- as.matrix(COLL22_SDMft)
numRows <- nrow(COLL22_SDMft2)
numCols <- ncol(COLL22_SDMft2)
COLL22_SDMft3 <- COLL22_SDMft2[c(2:numRows) , c(2:numCols)]
COLL22_SDMTable <- graph.adjacency(COLL22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(COLL22_SDMTable, vertex.label = V(COLL22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
COLL22_SDM.clusterCoef <- transitivity(COLL22_SDMTable, type="global") #cluster coefficient
COLL22_SDM.degreeCent <- centralization.degree(COLL22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_SDMftn <- as.network.matrix(COLL22_SDMft)
COLL22_SDM.netDensity <- network.density(COLL22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_SDM.entropy <- entropy(COLL22_SDMft) #entropy

COLL22_SDM.netMx <- cbind(COLL22_SDM.netMx, COLL22_SDM.clusterCoef, COLL22_SDM.degreeCent$centralization,
                          COLL22_SDM.netDensity, COLL22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
COLL22_TDMg2 <- data.frame(COLL22_TDM)
COLL22_TDMg2 <- COLL22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_TDMg2$player1
player2vector <- COLL22_TDMg2$player2
COLL22_TDMg3 <- COLL22_TDMg2
COLL22_TDMg3$p1inp2vec <- is.element(COLL22_TDMg3$player1, player2vector)
COLL22_TDMg3$p2inp1vec <- is.element(COLL22_TDMg3$player2, player1vector)

addPlayer1 <- COLL22_TDMg3[ which(COLL22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_TDMg3[ which(COLL22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_TDMg2 <- rbind(COLL22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
COLL22_TDMft <- ftable(COLL22_TDMg2$player1, COLL22_TDMg2$player2)
COLL22_TDMft2 <- as.matrix(COLL22_TDMft)
numRows <- nrow(COLL22_TDMft2)
numCols <- ncol(COLL22_TDMft2)
COLL22_TDMft3 <- COLL22_TDMft2[c(2:numRows) , c(2:numCols)]
COLL22_TDMTable <- graph.adjacency(COLL22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(COLL22_TDMTable, vertex.label = V(COLL22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
COLL22_TDM.clusterCoef <- transitivity(COLL22_TDMTable, type="global") #cluster coefficient
COLL22_TDM.degreeCent <- centralization.degree(COLL22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_TDMftn <- as.network.matrix(COLL22_TDMft)
COLL22_TDM.netDensity <- network.density(COLL22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_TDM.entropy <- entropy(COLL22_TDMft) #entropy

COLL22_TDM.netMx <- cbind(COLL22_TDM.netMx, COLL22_TDM.clusterCoef, COLL22_TDM.degreeCent$centralization,
                          COLL22_TDM.netDensity, COLL22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
COLL22_SDg2 <- data.frame(COLL22_SD)
COLL22_SDg2 <- COLL22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_SDg2$player1
player2vector <- COLL22_SDg2$player2
COLL22_SDg3 <- COLL22_SDg2
COLL22_SDg3$p1inp2vec <- is.element(COLL22_SDg3$player1, player2vector)
COLL22_SDg3$p2inp1vec <- is.element(COLL22_SDg3$player2, player1vector)

addPlayer1 <- COLL22_SDg3[ which(COLL22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_SDg3[ which(COLL22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_SDg2 <- rbind(COLL22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
COLL22_SDft <- ftable(COLL22_SDg2$player1, COLL22_SDg2$player2)
COLL22_SDft2 <- as.matrix(COLL22_SDft)
numRows <- nrow(COLL22_SDft2)
numCols <- ncol(COLL22_SDft2)
COLL22_SDft3 <- COLL22_SDft2[c(2:numRows) , c(2:numCols)]
COLL22_SDTable <- graph.adjacency(COLL22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(COLL22_SDTable, vertex.label = V(COLL22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
COLL22_SD.clusterCoef <- transitivity(COLL22_SDTable, type="global") #cluster coefficient
COLL22_SD.degreeCent <- centralization.degree(COLL22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_SDftn <- as.network.matrix(COLL22_SDft)
COLL22_SD.netDensity <- network.density(COLL22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_SD.entropy <- entropy(COLL22_SDft) #entropy

COLL22_SD.netMx <- cbind(COLL22_SD.netMx, COLL22_SD.clusterCoef, COLL22_SD.degreeCent$centralization,
                         COLL22_SD.netDensity, COLL22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
COLL22_TDg2 <- data.frame(COLL22_TD)
COLL22_TDg2 <- COLL22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_TDg2$player1
player2vector <- COLL22_TDg2$player2
COLL22_TDg3 <- COLL22_TDg2
COLL22_TDg3$p1inp2vec <- is.element(COLL22_TDg3$player1, player2vector)
COLL22_TDg3$p2inp1vec <- is.element(COLL22_TDg3$player2, player1vector)

addPlayer1 <- COLL22_TDg3[ which(COLL22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_TDg3[ which(COLL22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_TDg2 <- rbind(COLL22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
COLL22_TDft <- ftable(COLL22_TDg2$player1, COLL22_TDg2$player2)
COLL22_TDft2 <- as.matrix(COLL22_TDft)
numRows <- nrow(COLL22_TDft2)
numCols <- ncol(COLL22_TDft2)
COLL22_TDft3 <- COLL22_TDft2[c(2:numRows) , c(2:numCols)]
COLL22_TDTable <- graph.adjacency(COLL22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(COLL22_TDTable, vertex.label = V(COLL22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
COLL22_TD.clusterCoef <- transitivity(COLL22_TDTable, type="global") #cluster coefficient
COLL22_TD.degreeCent <- centralization.degree(COLL22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_TDftn <- as.network.matrix(COLL22_TDft)
COLL22_TD.netDensity <- network.density(COLL22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_TD.entropy <- entropy(COLL22_TDft) #entropy

COLL22_TD.netMx <- cbind(COLL22_TD.netMx, COLL22_TD.clusterCoef, COLL22_TD.degreeCent$centralization,
                         COLL22_TD.netDensity, COLL22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
COLL22_QTg2 <- data.frame(COLL22_QT)
COLL22_QTg2 <- COLL22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL22_QTg2$player1
player2vector <- COLL22_QTg2$player2
COLL22_QTg3 <- COLL22_QTg2
COLL22_QTg3$p1inp2vec <- is.element(COLL22_QTg3$player1, player2vector)
COLL22_QTg3$p2inp1vec <- is.element(COLL22_QTg3$player2, player1vector)

addPlayer1 <- COLL22_QTg3[ which(COLL22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL22_QTg3[ which(COLL22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL22_QTg2 <- rbind(COLL22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
COLL22_QTft <- ftable(COLL22_QTg2$player1, COLL22_QTg2$player2)
COLL22_QTft2 <- as.matrix(COLL22_QTft)
numRows <- nrow(COLL22_QTft2)
numCols <- ncol(COLL22_QTft2)
COLL22_QTft3 <- COLL22_QTft2[c(2:numRows) , c(2:numCols)]
COLL22_QTTable <- graph.adjacency(COLL22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(COLL22_QTTable, vertex.label = V(COLL22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
COLL22_QT.clusterCoef <- transitivity(COLL22_QTTable, type="global") #cluster coefficient
COLL22_QT.degreeCent <- centralization.degree(COLL22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL22_QTftn <- as.network.matrix(COLL22_QTft)
COLL22_QT.netDensity <- network.density(COLL22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL22_QT.entropy <- entropy(COLL22_QTft) #entropy

COLL22_QT.netMx <- cbind(COLL22_QT.netMx, COLL22_QT.clusterCoef, COLL22_QT.degreeCent$centralization,
                         COLL22_QT.netDensity, COLL22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL22_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "ESS"
KIoutcome = "Goal_F"
ESS22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
ESS22_Gg2 <- data.frame(ESS22_G)
ESS22_Gg2 <- ESS22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_Gg2$player1
player2vector <- ESS22_Gg2$player2
ESS22_Gg3 <- ESS22_Gg2
ESS22_Gg3$p1inp2vec <- is.element(ESS22_Gg3$player1, player2vector)
ESS22_Gg3$p2inp1vec <- is.element(ESS22_Gg3$player2, player1vector)

addPlayer1 <- ESS22_Gg3[ which(ESS22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_Gg3[ which(ESS22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_Gg2 <- rbind(ESS22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
ESS22_Gft <- ftable(ESS22_Gg2$player1, ESS22_Gg2$player2)
ESS22_Gft2 <- as.matrix(ESS22_Gft)
numRows <- nrow(ESS22_Gft2)
numCols <- ncol(ESS22_Gft2)
ESS22_Gft3 <- ESS22_Gft2[c(2:numRows) , c(2:numCols)]
ESS22_GTable <- graph.adjacency(ESS22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(ESS22_GTable, vertex.label = V(ESS22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
ESS22_G.clusterCoef <- transitivity(ESS22_GTable, type="global") #cluster coefficient
ESS22_G.degreeCent <- centralization.degree(ESS22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_Gftn <- as.network.matrix(ESS22_Gft)
ESS22_G.netDensity <- network.density(ESS22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_G.entropy <- entropy(ESS22_Gft) #entropy

ESS22_G.netMx <- cbind(ESS22_G.netMx, ESS22_G.clusterCoef, ESS22_G.degreeCent$centralization,
                       ESS22_G.netDensity, ESS22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************

round = 22
teamName = "ESS"
KIoutcome = "Behind_F"
ESS22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
ESS22_Bg2 <- data.frame(ESS22_B)
ESS22_Bg2 <- ESS22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_Bg2$player1
player2vector <- ESS22_Bg2$player2
ESS22_Bg3 <- ESS22_Bg2
ESS22_Bg3$p1inp2vec <- is.element(ESS22_Bg3$player1, player2vector)
ESS22_Bg3$p2inp1vec <- is.element(ESS22_Bg3$player2, player1vector)

addPlayer1 <- ESS22_Bg3[ which(ESS22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS22_Bg3[ which(ESS22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_Bg2 <- rbind(ESS22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
ESS22_Bft <- ftable(ESS22_Bg2$player1, ESS22_Bg2$player2)
ESS22_Bft2 <- as.matrix(ESS22_Bft)
numRows <- nrow(ESS22_Bft2)
numCols <- ncol(ESS22_Bft2)
ESS22_Bft3 <- ESS22_Bft2[c(2:numRows) , c(2:numCols)]
ESS22_BTable <- graph.adjacency(ESS22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(ESS22_BTable, vertex.label = V(ESS22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
ESS22_B.clusterCoef <- transitivity(ESS22_BTable, type="global") #cluster coefficient
ESS22_B.degreeCent <- centralization.degree(ESS22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_Bftn <- as.network.matrix(ESS22_Bft)
ESS22_B.netDensity <- network.density(ESS22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_B.entropy <- entropy(ESS22_Bft) #entropy

ESS22_B.netMx <- cbind(ESS22_B.netMx, ESS22_B.clusterCoef, ESS22_B.degreeCent$centralization,
                       ESS22_B.netDensity, ESS22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
ESS22_SFg2 <- data.frame(ESS22_SF)
ESS22_SFg2 <- ESS22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_SFg2$player1
player2vector <- ESS22_SFg2$player2
ESS22_SFg3 <- ESS22_SFg2
ESS22_SFg3$p1inp2vec <- is.element(ESS22_SFg3$player1, player2vector)
ESS22_SFg3$p2inp1vec <- is.element(ESS22_SFg3$player2, player1vector)

addPlayer1 <- ESS22_SFg3[ which(ESS22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_SFg3[ which(ESS22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_SFg2 <- rbind(ESS22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
ESS22_SFft <- ftable(ESS22_SFg2$player1, ESS22_SFg2$player2)
ESS22_SFft2 <- as.matrix(ESS22_SFft)
numRows <- nrow(ESS22_SFft2)
numCols <- ncol(ESS22_SFft2)
ESS22_SFft3 <- ESS22_SFft2[c(2:numRows) , c(2:numCols)]
ESS22_SFTable <- graph.adjacency(ESS22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(ESS22_SFTable, vertex.label = V(ESS22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
ESS22_SF.clusterCoef <- transitivity(ESS22_SFTable, type="global") #cluster coefficient
ESS22_SF.degreeCent <- centralization.degree(ESS22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_SFftn <- as.network.matrix(ESS22_SFft)
ESS22_SF.netDensity <- network.density(ESS22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_SF.entropy <- entropy(ESS22_SFft) #entropy

ESS22_SF.netMx <- cbind(ESS22_SF.netMx, ESS22_SF.clusterCoef, ESS22_SF.degreeCent$centralization,
                        ESS22_SF.netDensity, ESS22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
ESS22_TFg2 <- data.frame(ESS22_TF)
ESS22_TFg2 <- ESS22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_TFg2$player1
player2vector <- ESS22_TFg2$player2
ESS22_TFg3 <- ESS22_TFg2
ESS22_TFg3$p1inp2vec <- is.element(ESS22_TFg3$player1, player2vector)
ESS22_TFg3$p2inp1vec <- is.element(ESS22_TFg3$player2, player1vector)

addPlayer1 <- ESS22_TFg3[ which(ESS22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_TFg3[ which(ESS22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_TFg2 <- rbind(ESS22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
ESS22_TFft <- ftable(ESS22_TFg2$player1, ESS22_TFg2$player2)
ESS22_TFft2 <- as.matrix(ESS22_TFft)
numRows <- nrow(ESS22_TFft2)
numCols <- ncol(ESS22_TFft2)
ESS22_TFft3 <- ESS22_TFft2[c(2:numRows) , c(2:numCols)]
ESS22_TFTable <- graph.adjacency(ESS22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(ESS22_TFTable, vertex.label = V(ESS22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
ESS22_TF.clusterCoef <- transitivity(ESS22_TFTable, type="global") #cluster coefficient
ESS22_TF.degreeCent <- centralization.degree(ESS22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_TFftn <- as.network.matrix(ESS22_TFft)
ESS22_TF.netDensity <- network.density(ESS22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_TF.entropy <- entropy(ESS22_TFft) #entropy

ESS22_TF.netMx <- cbind(ESS22_TF.netMx, ESS22_TF.clusterCoef, ESS22_TF.degreeCent$centralization,
                        ESS22_TF.netDensity, ESS22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
ESS22_SAMg2 <- data.frame(ESS22_SAM)
ESS22_SAMg2 <- ESS22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_SAMg2$player1
player2vector <- ESS22_SAMg2$player2
ESS22_SAMg3 <- ESS22_SAMg2
ESS22_SAMg3$p1inp2vec <- is.element(ESS22_SAMg3$player1, player2vector)
ESS22_SAMg3$p2inp1vec <- is.element(ESS22_SAMg3$player2, player1vector)

addPlayer1 <- ESS22_SAMg3[ which(ESS22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS22_SAMg3[ which(ESS22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_SAMg2 <- rbind(ESS22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
ESS22_SAMft <- ftable(ESS22_SAMg2$player1, ESS22_SAMg2$player2)
ESS22_SAMft2 <- as.matrix(ESS22_SAMft)
numRows <- nrow(ESS22_SAMft2)
numCols <- ncol(ESS22_SAMft2)
ESS22_SAMft3 <- ESS22_SAMft2[c(2:numRows) , c(2:numCols)]
ESS22_SAMTable <- graph.adjacency(ESS22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(ESS22_SAMTable, vertex.label = V(ESS22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
ESS22_SAM.clusterCoef <- transitivity(ESS22_SAMTable, type="global") #cluster coefficient
ESS22_SAM.degreeCent <- centralization.degree(ESS22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_SAMftn <- as.network.matrix(ESS22_SAMft)
ESS22_SAM.netDensity <- network.density(ESS22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_SAM.entropy <- entropy(ESS22_SAMft) #entropy

ESS22_SAM.netMx <- cbind(ESS22_SAM.netMx, ESS22_SAM.clusterCoef, ESS22_SAM.degreeCent$centralization,
                         ESS22_SAM.netDensity, ESS22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
ESS22_TAMg2 <- data.frame(ESS22_TAM)
ESS22_TAMg2 <- ESS22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_TAMg2$player1
player2vector <- ESS22_TAMg2$player2
ESS22_TAMg3 <- ESS22_TAMg2
ESS22_TAMg3$p1inp2vec <- is.element(ESS22_TAMg3$player1, player2vector)
ESS22_TAMg3$p2inp1vec <- is.element(ESS22_TAMg3$player2, player1vector)

addPlayer1 <- ESS22_TAMg3[ which(ESS22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_TAMg3[ which(ESS22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_TAMg2 <- rbind(ESS22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
ESS22_TAMft <- ftable(ESS22_TAMg2$player1, ESS22_TAMg2$player2)
ESS22_TAMft2 <- as.matrix(ESS22_TAMft)
numRows <- nrow(ESS22_TAMft2)
numCols <- ncol(ESS22_TAMft2)
ESS22_TAMft3 <- ESS22_TAMft2[c(2:numRows) , c(2:numCols)]
ESS22_TAMTable <- graph.adjacency(ESS22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(ESS22_TAMTable, vertex.label = V(ESS22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
ESS22_TAM.clusterCoef <- transitivity(ESS22_TAMTable, type="global") #cluster coefficient
ESS22_TAM.degreeCent <- centralization.degree(ESS22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_TAMftn <- as.network.matrix(ESS22_TAMft)
ESS22_TAM.netDensity <- network.density(ESS22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_TAM.entropy <- entropy(ESS22_TAMft) #entropy

ESS22_TAM.netMx <- cbind(ESS22_TAM.netMx, ESS22_TAM.clusterCoef, ESS22_TAM.degreeCent$centralization,
                         ESS22_TAM.netDensity, ESS22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
ESS22_SDMg2 <- data.frame(ESS22_SDM)
ESS22_SDMg2 <- ESS22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_SDMg2$player1
player2vector <- ESS22_SDMg2$player2
ESS22_SDMg3 <- ESS22_SDMg2
ESS22_SDMg3$p1inp2vec <- is.element(ESS22_SDMg3$player1, player2vector)
ESS22_SDMg3$p2inp1vec <- is.element(ESS22_SDMg3$player2, player1vector)

addPlayer1 <- ESS22_SDMg3[ which(ESS22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_SDMg3[ which(ESS22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_SDMg2 <- rbind(ESS22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
ESS22_SDMft <- ftable(ESS22_SDMg2$player1, ESS22_SDMg2$player2)
ESS22_SDMft2 <- as.matrix(ESS22_SDMft)
numRows <- nrow(ESS22_SDMft2)
numCols <- ncol(ESS22_SDMft2)
ESS22_SDMft3 <- ESS22_SDMft2[c(2:numRows) , c(2:numCols)]
ESS22_SDMTable <- graph.adjacency(ESS22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(ESS22_SDMTable, vertex.label = V(ESS22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
ESS22_SDM.clusterCoef <- transitivity(ESS22_SDMTable, type="global") #cluster coefficient
ESS22_SDM.degreeCent <- centralization.degree(ESS22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_SDMftn <- as.network.matrix(ESS22_SDMft)
ESS22_SDM.netDensity <- network.density(ESS22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_SDM.entropy <- entropy(ESS22_SDMft) #entropy

ESS22_SDM.netMx <- cbind(ESS22_SDM.netMx, ESS22_SDM.clusterCoef, ESS22_SDM.degreeCent$centralization,
                         ESS22_SDM.netDensity, ESS22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
ESS22_TDMg2 <- data.frame(ESS22_TDM)
ESS22_TDMg2 <- ESS22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_TDMg2$player1
player2vector <- ESS22_TDMg2$player2
ESS22_TDMg3 <- ESS22_TDMg2
ESS22_TDMg3$p1inp2vec <- is.element(ESS22_TDMg3$player1, player2vector)
ESS22_TDMg3$p2inp1vec <- is.element(ESS22_TDMg3$player2, player1vector)

addPlayer1 <- ESS22_TDMg3[ which(ESS22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_TDMg3[ which(ESS22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_TDMg2 <- rbind(ESS22_TDMg2, addPlayers)


#ROUND 22, DM Turnover graph using weighted edges
ESS22_TDMft <- ftable(ESS22_TDMg2$player1, ESS22_TDMg2$player2)
ESS22_TDMft2 <- as.matrix(ESS22_TDMft)
numRows <- nrow(ESS22_TDMft2)
numCols <- ncol(ESS22_TDMft2)
ESS22_TDMft3 <- ESS22_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS22_TDMTable <- graph.adjacency(ESS22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(ESS22_TDMTable, vertex.label = V(ESS22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
ESS22_TDM.clusterCoef <- transitivity(ESS22_TDMTable, type="global") #cluster coefficient
ESS22_TDM.degreeCent <- centralization.degree(ESS22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_TDMftn <- as.network.matrix(ESS22_TDMft)
ESS22_TDM.netDensity <- network.density(ESS22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_TDM.entropy <- entropy(ESS22_TDMft) #entropy

ESS22_TDM.netMx <- cbind(ESS22_TDM.netMx, ESS22_TDM.clusterCoef, ESS22_TDM.degreeCent$centralization,
                         ESS22_TDM.netDensity, ESS22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
ESS22_SDg2 <- data.frame(ESS22_SD)
ESS22_SDg2 <- ESS22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_SDg2$player1
player2vector <- ESS22_SDg2$player2
ESS22_SDg3 <- ESS22_SDg2
ESS22_SDg3$p1inp2vec <- is.element(ESS22_SDg3$player1, player2vector)
ESS22_SDg3$p2inp1vec <- is.element(ESS22_SDg3$player2, player1vector)

addPlayer1 <- ESS22_SDg3[ which(ESS22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_SDg3[ which(ESS22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_SDg2 <- rbind(ESS22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
ESS22_SDft <- ftable(ESS22_SDg2$player1, ESS22_SDg2$player2)
ESS22_SDft2 <- as.matrix(ESS22_SDft)
numRows <- nrow(ESS22_SDft2)
numCols <- ncol(ESS22_SDft2)
ESS22_SDft3 <- ESS22_SDft2[c(2:numRows) , c(2:numCols)]
ESS22_SDTable <- graph.adjacency(ESS22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(ESS22_SDTable, vertex.label = V(ESS22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
ESS22_SD.clusterCoef <- transitivity(ESS22_SDTable, type="global") #cluster coefficient
ESS22_SD.degreeCent <- centralization.degree(ESS22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_SDftn <- as.network.matrix(ESS22_SDft)
ESS22_SD.netDensity <- network.density(ESS22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_SD.entropy <- entropy(ESS22_SDft) #entropy

ESS22_SD.netMx <- cbind(ESS22_SD.netMx, ESS22_SD.clusterCoef, ESS22_SD.degreeCent$centralization,
                        ESS22_SD.netDensity, ESS22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************

round = 22
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
ESS22_TDg2 <- data.frame(ESS22_TD)
ESS22_TDg2 <- ESS22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_TDg2$player1
player2vector <- ESS22_TDg2$player2
ESS22_TDg3 <- ESS22_TDg2
ESS22_TDg3$p1inp2vec <- is.element(ESS22_TDg3$player1, player2vector)
ESS22_TDg3$p2inp1vec <- is.element(ESS22_TDg3$player2, player1vector)

addPlayer1 <- ESS22_TDg3[ which(ESS22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_TDg3[ which(ESS22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_TDg2 <- rbind(ESS22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
ESS22_TDft <- ftable(ESS22_TDg2$player1, ESS22_TDg2$player2)
ESS22_TDft2 <- as.matrix(ESS22_TDft)
numRows <- nrow(ESS22_TDft2)
numCols <- ncol(ESS22_TDft2)
ESS22_TDft3 <- ESS22_TDft2[c(2:numRows) , c(2:numCols)]
ESS22_TDTable <- graph.adjacency(ESS22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(ESS22_TDTable, vertex.label = V(ESS22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
ESS22_TD.clusterCoef <- transitivity(ESS22_TDTable, type="global") #cluster coefficient
ESS22_TD.degreeCent <- centralization.degree(ESS22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_TDftn <- as.network.matrix(ESS22_TDft)
ESS22_TD.netDensity <- network.density(ESS22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_TD.entropy <- entropy(ESS22_TDft) #entropy

ESS22_TD.netMx <- cbind(ESS22_TD.netMx, ESS22_TD.clusterCoef, ESS22_TD.degreeCent$centralization,
                        ESS22_TD.netDensity, ESS22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
ESS22_QTg2 <- data.frame(ESS22_QT)
ESS22_QTg2 <- ESS22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS22_QTg2$player1
player2vector <- ESS22_QTg2$player2
ESS22_QTg3 <- ESS22_QTg2
ESS22_QTg3$p1inp2vec <- is.element(ESS22_QTg3$player1, player2vector)
ESS22_QTg3$p2inp1vec <- is.element(ESS22_QTg3$player2, player1vector)

addPlayer1 <- ESS22_QTg3[ which(ESS22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS22_QTg3[ which(ESS22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS22_QTg2 <- rbind(ESS22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
ESS22_QTft <- ftable(ESS22_QTg2$player1, ESS22_QTg2$player2)
ESS22_QTft2 <- as.matrix(ESS22_QTft)
numRows <- nrow(ESS22_QTft2)
numCols <- ncol(ESS22_QTft2)
ESS22_QTft3 <- ESS22_QTft2[c(2:numRows) , c(2:numCols)]
ESS22_QTTable <- graph.adjacency(ESS22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(ESS22_QTTable, vertex.label = V(ESS22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
ESS22_QT.clusterCoef <- transitivity(ESS22_QTTable, type="global") #cluster coefficient
ESS22_QT.degreeCent <- centralization.degree(ESS22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS22_QTftn <- as.network.matrix(ESS22_QTft)
ESS22_QT.netDensity <- network.density(ESS22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS22_QT.entropy <- entropy(ESS22_QTft) #entropy

ESS22_QT.netMx <- cbind(ESS22_QT.netMx, ESS22_QT.clusterCoef, ESS22_QT.degreeCent$centralization,
                        ESS22_QT.netDensity, ESS22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS22_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************

round = 22
teamName = "FRE"
KIoutcome = "Goal_F"
FRE22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
FRE22_Gg2 <- data.frame(FRE22_G)
FRE22_Gg2 <- FRE22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_Gg2$player1
player2vector <- FRE22_Gg2$player2
FRE22_Gg3 <- FRE22_Gg2
FRE22_Gg3$p1inp2vec <- is.element(FRE22_Gg3$player1, player2vector)
FRE22_Gg3$p2inp1vec <- is.element(FRE22_Gg3$player2, player1vector)

addPlayer1 <- FRE22_Gg3[ which(FRE22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_Gg3[ which(FRE22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_Gg2 <- rbind(FRE22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
FRE22_Gft <- ftable(FRE22_Gg2$player1, FRE22_Gg2$player2)
FRE22_Gft2 <- as.matrix(FRE22_Gft)
numRows <- nrow(FRE22_Gft2)
numCols <- ncol(FRE22_Gft2)
FRE22_Gft3 <- FRE22_Gft2[c(2:numRows) , c(2:numCols)]
FRE22_GTable <- graph.adjacency(FRE22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(FRE22_GTable, vertex.label = V(FRE22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
FRE22_G.clusterCoef <- transitivity(FRE22_GTable, type="global") #cluster coefficient
FRE22_G.degreeCent <- centralization.degree(FRE22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_Gftn <- as.network.matrix(FRE22_Gft)
FRE22_G.netDensity <- network.density(FRE22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_G.entropy <- entropy(FRE22_Gft) #entropy

FRE22_G.netMx <- cbind(FRE22_G.netMx, FRE22_G.clusterCoef, FRE22_G.degreeCent$centralization,
                       FRE22_G.netDensity, FRE22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "Behind_F"
FRE22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
FRE22_Bg2 <- data.frame(FRE22_B)
FRE22_Bg2 <- FRE22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_Bg2$player1
player2vector <- FRE22_Bg2$player2
FRE22_Bg3 <- FRE22_Bg2
FRE22_Bg3$p1inp2vec <- is.element(FRE22_Bg3$player1, player2vector)
FRE22_Bg3$p2inp1vec <- is.element(FRE22_Bg3$player2, player1vector)

addPlayer1 <- FRE22_Bg3[ which(FRE22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_Bg3[ which(FRE22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_Bg2 <- rbind(FRE22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
FRE22_Bft <- ftable(FRE22_Bg2$player1, FRE22_Bg2$player2)
FRE22_Bft2 <- as.matrix(FRE22_Bft)
numRows <- nrow(FRE22_Bft2)
numCols <- ncol(FRE22_Bft2)
FRE22_Bft3 <- FRE22_Bft2[c(2:numRows) , c(2:numCols)]
FRE22_BTable <- graph.adjacency(FRE22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(FRE22_BTable, vertex.label = V(FRE22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
FRE22_B.clusterCoef <- transitivity(FRE22_BTable, type="global") #cluster coefficient
FRE22_B.degreeCent <- centralization.degree(FRE22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_Bftn <- as.network.matrix(FRE22_Bft)
FRE22_B.netDensity <- network.density(FRE22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_B.entropy <- entropy(FRE22_Bft) #entropy

FRE22_B.netMx <- cbind(FRE22_B.netMx, FRE22_B.clusterCoef, FRE22_B.degreeCent$centralization,
                       FRE22_B.netDensity, FRE22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************

round = 22
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
FRE22_SFg2 <- data.frame(FRE22_SF)
FRE22_SFg2 <- FRE22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_SFg2$player1
player2vector <- FRE22_SFg2$player2
FRE22_SFg3 <- FRE22_SFg2
FRE22_SFg3$p1inp2vec <- is.element(FRE22_SFg3$player1, player2vector)
FRE22_SFg3$p2inp1vec <- is.element(FRE22_SFg3$player2, player1vector)

addPlayer1 <- FRE22_SFg3[ which(FRE22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_SFg3[ which(FRE22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_SFg2 <- rbind(FRE22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
FRE22_SFft <- ftable(FRE22_SFg2$player1, FRE22_SFg2$player2)
FRE22_SFft2 <- as.matrix(FRE22_SFft)
numRows <- nrow(FRE22_SFft2)
numCols <- ncol(FRE22_SFft2)
FRE22_SFft3 <- FRE22_SFft2[c(2:numRows) , c(2:numCols)]
FRE22_SFTable <- graph.adjacency(FRE22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(FRE22_SFTable, vertex.label = V(FRE22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
FRE22_SF.clusterCoef <- transitivity(FRE22_SFTable, type="global") #cluster coefficient
FRE22_SF.degreeCent <- centralization.degree(FRE22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_SFftn <- as.network.matrix(FRE22_SFft)
FRE22_SF.netDensity <- network.density(FRE22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_SF.entropy <- entropy(FRE22_SFft) #entropy

FRE22_SF.netMx <- cbind(FRE22_SF.netMx, FRE22_SF.clusterCoef, FRE22_SF.degreeCent$centralization,
                        FRE22_SF.netDensity, FRE22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
FRE22_TFg2 <- data.frame(FRE22_TF)
FRE22_TFg2 <- FRE22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_TFg2$player1
player2vector <- FRE22_TFg2$player2
FRE22_TFg3 <- FRE22_TFg2
FRE22_TFg3$p1inp2vec <- is.element(FRE22_TFg3$player1, player2vector)
FRE22_TFg3$p2inp1vec <- is.element(FRE22_TFg3$player2, player1vector)

addPlayer1 <- FRE22_TFg3[ which(FRE22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE22_TFg3[ which(FRE22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_TFg2 <- rbind(FRE22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
FRE22_TFft <- ftable(FRE22_TFg2$player1, FRE22_TFg2$player2)
FRE22_TFft2 <- as.matrix(FRE22_TFft)
numRows <- nrow(FRE22_TFft2)
numCols <- ncol(FRE22_TFft2)
FRE22_TFft3 <- FRE22_TFft2[c(2:numRows) , c(2:numCols)]
FRE22_TFTable <- graph.adjacency(FRE22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(FRE22_TFTable, vertex.label = V(FRE22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
FRE22_TF.clusterCoef <- transitivity(FRE22_TFTable, type="global") #cluster coefficient
FRE22_TF.degreeCent <- centralization.degree(FRE22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_TFftn <- as.network.matrix(FRE22_TFft)
FRE22_TF.netDensity <- network.density(FRE22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_TF.entropy <- entropy(FRE22_TFft) #entropy

FRE22_TF.netMx <- cbind(FRE22_TF.netMx, FRE22_TF.clusterCoef, FRE22_TF.degreeCent$centralization,
                        FRE22_TF.netDensity, FRE22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
FRE22_SAMg2 <- data.frame(FRE22_SAM)
FRE22_SAMg2 <- FRE22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_SAMg2$player1
player2vector <- FRE22_SAMg2$player2
FRE22_SAMg3 <- FRE22_SAMg2
FRE22_SAMg3$p1inp2vec <- is.element(FRE22_SAMg3$player1, player2vector)
FRE22_SAMg3$p2inp1vec <- is.element(FRE22_SAMg3$player2, player1vector)

addPlayer1 <- FRE22_SAMg3[ which(FRE22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_SAMg3[ which(FRE22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_SAMg2 <- rbind(FRE22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
FRE22_SAMft <- ftable(FRE22_SAMg2$player1, FRE22_SAMg2$player2)
FRE22_SAMft2 <- as.matrix(FRE22_SAMft)
numRows <- nrow(FRE22_SAMft2)
numCols <- ncol(FRE22_SAMft2)
FRE22_SAMft3 <- FRE22_SAMft2[c(2:numRows) , c(2:numCols)]
FRE22_SAMTable <- graph.adjacency(FRE22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(FRE22_SAMTable, vertex.label = V(FRE22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
FRE22_SAM.clusterCoef <- transitivity(FRE22_SAMTable, type="global") #cluster coefficient
FRE22_SAM.degreeCent <- centralization.degree(FRE22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_SAMftn <- as.network.matrix(FRE22_SAMft)
FRE22_SAM.netDensity <- network.density(FRE22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_SAM.entropy <- entropy(FRE22_SAMft) #entropy

FRE22_SAM.netMx <- cbind(FRE22_SAM.netMx, FRE22_SAM.clusterCoef, FRE22_SAM.degreeCent$centralization,
                         FRE22_SAM.netDensity, FRE22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
FRE22_TAMg2 <- data.frame(FRE22_TAM)
FRE22_TAMg2 <- FRE22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_TAMg2$player1
player2vector <- FRE22_TAMg2$player2
FRE22_TAMg3 <- FRE22_TAMg2
FRE22_TAMg3$p1inp2vec <- is.element(FRE22_TAMg3$player1, player2vector)
FRE22_TAMg3$p2inp1vec <- is.element(FRE22_TAMg3$player2, player1vector)

addPlayer1 <- FRE22_TAMg3[ which(FRE22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_TAMg3[ which(FRE22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_TAMg2 <- rbind(FRE22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
FRE22_TAMft <- ftable(FRE22_TAMg2$player1, FRE22_TAMg2$player2)
FRE22_TAMft2 <- as.matrix(FRE22_TAMft)
numRows <- nrow(FRE22_TAMft2)
numCols <- ncol(FRE22_TAMft2)
FRE22_TAMft3 <- FRE22_TAMft2[c(2:numRows) , c(2:numCols)]
FRE22_TAMTable <- graph.adjacency(FRE22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(FRE22_TAMTable, vertex.label = V(FRE22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
FRE22_TAM.clusterCoef <- transitivity(FRE22_TAMTable, type="global") #cluster coefficient
FRE22_TAM.degreeCent <- centralization.degree(FRE22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_TAMftn <- as.network.matrix(FRE22_TAMft)
FRE22_TAM.netDensity <- network.density(FRE22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_TAM.entropy <- entropy(FRE22_TAMft) #entropy

FRE22_TAM.netMx <- cbind(FRE22_TAM.netMx, FRE22_TAM.clusterCoef, FRE22_TAM.degreeCent$centralization,
                         FRE22_TAM.netDensity, FRE22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
FRE22_SDMg2 <- data.frame(FRE22_SDM)
FRE22_SDMg2 <- FRE22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_SDMg2$player1
player2vector <- FRE22_SDMg2$player2
FRE22_SDMg3 <- FRE22_SDMg2
FRE22_SDMg3$p1inp2vec <- is.element(FRE22_SDMg3$player1, player2vector)
FRE22_SDMg3$p2inp1vec <- is.element(FRE22_SDMg3$player2, player1vector)

addPlayer1 <- FRE22_SDMg3[ which(FRE22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_SDMg3[ which(FRE22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_SDMg2 <- rbind(FRE22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
FRE22_SDMft <- ftable(FRE22_SDMg2$player1, FRE22_SDMg2$player2)
FRE22_SDMft2 <- as.matrix(FRE22_SDMft)
numRows <- nrow(FRE22_SDMft2)
numCols <- ncol(FRE22_SDMft2)
FRE22_SDMft3 <- FRE22_SDMft2[c(2:numRows) , c(2:numCols)]
FRE22_SDMTable <- graph.adjacency(FRE22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(FRE22_SDMTable, vertex.label = V(FRE22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
FRE22_SDM.clusterCoef <- transitivity(FRE22_SDMTable, type="global") #cluster coefficient
FRE22_SDM.degreeCent <- centralization.degree(FRE22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_SDMftn <- as.network.matrix(FRE22_SDMft)
FRE22_SDM.netDensity <- network.density(FRE22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_SDM.entropy <- entropy(FRE22_SDMft) #entropy

FRE22_SDM.netMx <- cbind(FRE22_SDM.netMx, FRE22_SDM.clusterCoef, FRE22_SDM.degreeCent$centralization,
                         FRE22_SDM.netDensity, FRE22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
FRE22_TDMg2 <- data.frame(FRE22_TDM)
FRE22_TDMg2 <- FRE22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_TDMg2$player1
player2vector <- FRE22_TDMg2$player2
FRE22_TDMg3 <- FRE22_TDMg2
FRE22_TDMg3$p1inp2vec <- is.element(FRE22_TDMg3$player1, player2vector)
FRE22_TDMg3$p2inp1vec <- is.element(FRE22_TDMg3$player2, player1vector)

addPlayer1 <- FRE22_TDMg3[ which(FRE22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_TDMg3[ which(FRE22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_TDMg2 <- rbind(FRE22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
FRE22_TDMft <- ftable(FRE22_TDMg2$player1, FRE22_TDMg2$player2)
FRE22_TDMft2 <- as.matrix(FRE22_TDMft)
numRows <- nrow(FRE22_TDMft2)
numCols <- ncol(FRE22_TDMft2)
FRE22_TDMft3 <- FRE22_TDMft2[c(2:numRows) , c(2:numCols)]
FRE22_TDMTable <- graph.adjacency(FRE22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(FRE22_TDMTable, vertex.label = V(FRE22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
FRE22_TDM.clusterCoef <- transitivity(FRE22_TDMTable, type="global") #cluster coefficient
FRE22_TDM.degreeCent <- centralization.degree(FRE22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_TDMftn <- as.network.matrix(FRE22_TDMft)
FRE22_TDM.netDensity <- network.density(FRE22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_TDM.entropy <- entropy(FRE22_TDMft) #entropy

FRE22_TDM.netMx <- cbind(FRE22_TDM.netMx, FRE22_TDM.clusterCoef, FRE22_TDM.degreeCent$centralization,
                         FRE22_TDM.netDensity, FRE22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
FRE22_SDg2 <- data.frame(FRE22_SD)
FRE22_SDg2 <- FRE22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_SDg2$player1
player2vector <- FRE22_SDg2$player2
FRE22_SDg3 <- FRE22_SDg2
FRE22_SDg3$p1inp2vec <- is.element(FRE22_SDg3$player1, player2vector)
FRE22_SDg3$p2inp1vec <- is.element(FRE22_SDg3$player2, player1vector)

addPlayer1 <- FRE22_SDg3[ which(FRE22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_SDg3[ which(FRE22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_SDg2 <- rbind(FRE22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
FRE22_SDft <- ftable(FRE22_SDg2$player1, FRE22_SDg2$player2)
FRE22_SDft2 <- as.matrix(FRE22_SDft)
numRows <- nrow(FRE22_SDft2)
numCols <- ncol(FRE22_SDft2)
FRE22_SDft3 <- FRE22_SDft2[c(2:numRows) , c(2:numCols)]
FRE22_SDTable <- graph.adjacency(FRE22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(FRE22_SDTable, vertex.label = V(FRE22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
FRE22_SD.clusterCoef <- transitivity(FRE22_SDTable, type="global") #cluster coefficient
FRE22_SD.degreeCent <- centralization.degree(FRE22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_SDftn <- as.network.matrix(FRE22_SDft)
FRE22_SD.netDensity <- network.density(FRE22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_SD.entropy <- entropy(FRE22_SDft) #entropy

FRE22_SD.netMx <- cbind(FRE22_SD.netMx, FRE22_SD.clusterCoef, FRE22_SD.degreeCent$centralization,
                        FRE22_SD.netDensity, FRE22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
FRE22_TDg2 <- data.frame(FRE22_TD)
FRE22_TDg2 <- FRE22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_TDg2$player1
player2vector <- FRE22_TDg2$player2
FRE22_TDg3 <- FRE22_TDg2
FRE22_TDg3$p1inp2vec <- is.element(FRE22_TDg3$player1, player2vector)
FRE22_TDg3$p2inp1vec <- is.element(FRE22_TDg3$player2, player1vector)

addPlayer1 <- FRE22_TDg3[ which(FRE22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_TDg3[ which(FRE22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_TDg2 <- rbind(FRE22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
FRE22_TDft <- ftable(FRE22_TDg2$player1, FRE22_TDg2$player2)
FRE22_TDft2 <- as.matrix(FRE22_TDft)
numRows <- nrow(FRE22_TDft2)
numCols <- ncol(FRE22_TDft2)
FRE22_TDft3 <- FRE22_TDft2[c(2:numRows) , c(2:numCols)]
FRE22_TDTable <- graph.adjacency(FRE22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(FRE22_TDTable, vertex.label = V(FRE22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
FRE22_TD.clusterCoef <- transitivity(FRE22_TDTable, type="global") #cluster coefficient
FRE22_TD.degreeCent <- centralization.degree(FRE22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_TDftn <- as.network.matrix(FRE22_TDft)
FRE22_TD.netDensity <- network.density(FRE22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_TD.entropy <- entropy(FRE22_TDft) #entropy

FRE22_TD.netMx <- cbind(FRE22_TD.netMx, FRE22_TD.clusterCoef, FRE22_TD.degreeCent$centralization,
                        FRE22_TD.netDensity, FRE22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
FRE22_QTg2 <- data.frame(FRE22_QT)
FRE22_QTg2 <- FRE22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE22_QTg2$player1
player2vector <- FRE22_QTg2$player2
FRE22_QTg3 <- FRE22_QTg2
FRE22_QTg3$p1inp2vec <- is.element(FRE22_QTg3$player1, player2vector)
FRE22_QTg3$p2inp1vec <- is.element(FRE22_QTg3$player2, player1vector)

addPlayer1 <- FRE22_QTg3[ which(FRE22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE22_QTg3[ which(FRE22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE22_QTg2 <- rbind(FRE22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
FRE22_QTft <- ftable(FRE22_QTg2$player1, FRE22_QTg2$player2)
FRE22_QTft2 <- as.matrix(FRE22_QTft)
numRows <- nrow(FRE22_QTft2)
numCols <- ncol(FRE22_QTft2)
FRE22_QTft3 <- FRE22_QTft2[c(2:numRows) , c(2:numCols)]
FRE22_QTTable <- graph.adjacency(FRE22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(FRE22_QTTable, vertex.label = V(FRE22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
FRE22_QT.clusterCoef <- transitivity(FRE22_QTTable, type="global") #cluster coefficient
FRE22_QT.degreeCent <- centralization.degree(FRE22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE22_QTftn <- as.network.matrix(FRE22_QTft)
FRE22_QT.netDensity <- network.density(FRE22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE22_QT.entropy <- entropy(FRE22_QTft) #entropy

FRE22_QT.netMx <- cbind(FRE22_QT.netMx, FRE22_QT.clusterCoef, FRE22_QT.degreeCent$centralization,
                        FRE22_QT.netDensity, FRE22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE22_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
GCFC22_Gg2 <- data.frame(GCFC22_G)
GCFC22_Gg2 <- GCFC22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_Gg2$player1
player2vector <- GCFC22_Gg2$player2
GCFC22_Gg3 <- GCFC22_Gg2
GCFC22_Gg3$p1inp2vec <- is.element(GCFC22_Gg3$player1, player2vector)
GCFC22_Gg3$p2inp1vec <- is.element(GCFC22_Gg3$player2, player1vector)

addPlayer1 <- GCFC22_Gg3[ which(GCFC22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_Gg3[ which(GCFC22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_Gg2 <- rbind(GCFC22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
GCFC22_Gft <- ftable(GCFC22_Gg2$player1, GCFC22_Gg2$player2)
GCFC22_Gft2 <- as.matrix(GCFC22_Gft)
numRows <- nrow(GCFC22_Gft2)
numCols <- ncol(GCFC22_Gft2)
GCFC22_Gft3 <- GCFC22_Gft2[c(2:numRows) , c(2:numCols)]
GCFC22_GTable <- graph.adjacency(GCFC22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(GCFC22_GTable, vertex.label = V(GCFC22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
GCFC22_G.clusterCoef <- transitivity(GCFC22_GTable, type="global") #cluster coefficient
GCFC22_G.degreeCent <- centralization.degree(GCFC22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_Gftn <- as.network.matrix(GCFC22_Gft)
GCFC22_G.netDensity <- network.density(GCFC22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_G.entropy <- entropy(GCFC22_Gft) #entropy

GCFC22_G.netMx <- cbind(GCFC22_G.netMx, GCFC22_G.clusterCoef, GCFC22_G.degreeCent$centralization,
                        GCFC22_G.netDensity, GCFC22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
GCFC22_Bg2 <- data.frame(GCFC22_B)
GCFC22_Bg2 <- GCFC22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_Bg2$player1
player2vector <- GCFC22_Bg2$player2
GCFC22_Bg3 <- GCFC22_Bg2
GCFC22_Bg3$p1inp2vec <- is.element(GCFC22_Bg3$player1, player2vector)
GCFC22_Bg3$p2inp1vec <- is.element(GCFC22_Bg3$player2, player1vector)

addPlayer1 <- GCFC22_Bg3[ which(GCFC22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_Bg3[ which(GCFC22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_Bg2 <- rbind(GCFC22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
GCFC22_Bft <- ftable(GCFC22_Bg2$player1, GCFC22_Bg2$player2)
GCFC22_Bft2 <- as.matrix(GCFC22_Bft)
numRows <- nrow(GCFC22_Bft2)
numCols <- ncol(GCFC22_Bft2)
GCFC22_Bft3 <- GCFC22_Bft2[c(2:numRows) , c(2:numCols)]
GCFC22_BTable <- graph.adjacency(GCFC22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(GCFC22_BTable, vertex.label = V(GCFC22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
GCFC22_B.clusterCoef <- transitivity(GCFC22_BTable, type="global") #cluster coefficient
GCFC22_B.degreeCent <- centralization.degree(GCFC22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_Bftn <- as.network.matrix(GCFC22_Bft)
GCFC22_B.netDensity <- network.density(GCFC22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_B.entropy <- entropy(GCFC22_Bft) #entropy

GCFC22_B.netMx <- cbind(GCFC22_B.netMx, GCFC22_B.clusterCoef, GCFC22_B.degreeCent$centralization,
                        GCFC22_B.netDensity, GCFC22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************

round = 22
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
GCFC22_SFg2 <- data.frame(GCFC22_SF)
GCFC22_SFg2 <- GCFC22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_SFg2$player1
player2vector <- GCFC22_SFg2$player2
GCFC22_SFg3 <- GCFC22_SFg2
GCFC22_SFg3$p1inp2vec <- is.element(GCFC22_SFg3$player1, player2vector)
GCFC22_SFg3$p2inp1vec <- is.element(GCFC22_SFg3$player2, player1vector)

addPlayer1 <- GCFC22_SFg3[ which(GCFC22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC22_SFg3[ which(GCFC22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_SFg2 <- rbind(GCFC22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
GCFC22_SFft <- ftable(GCFC22_SFg2$player1, GCFC22_SFg2$player2)
GCFC22_SFft2 <- as.matrix(GCFC22_SFft)
numRows <- nrow(GCFC22_SFft2)
numCols <- ncol(GCFC22_SFft2)
GCFC22_SFft3 <- GCFC22_SFft2[c(2:numRows) , c(2:numCols)]
GCFC22_SFTable <- graph.adjacency(GCFC22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(GCFC22_SFTable, vertex.label = V(GCFC22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
GCFC22_SF.clusterCoef <- transitivity(GCFC22_SFTable, type="global") #cluster coefficient
GCFC22_SF.degreeCent <- centralization.degree(GCFC22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_SFftn <- as.network.matrix(GCFC22_SFft)
GCFC22_SF.netDensity <- network.density(GCFC22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_SF.entropy <- entropy(GCFC22_SFft) #entropy

GCFC22_SF.netMx <- cbind(GCFC22_SF.netMx, GCFC22_SF.clusterCoef, GCFC22_SF.degreeCent$centralization,
                         GCFC22_SF.netDensity, GCFC22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
GCFC22_TFg2 <- data.frame(GCFC22_TF)
GCFC22_TFg2 <- GCFC22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_TFg2$player1
player2vector <- GCFC22_TFg2$player2
GCFC22_TFg3 <- GCFC22_TFg2
GCFC22_TFg3$p1inp2vec <- is.element(GCFC22_TFg3$player1, player2vector)
GCFC22_TFg3$p2inp1vec <- is.element(GCFC22_TFg3$player2, player1vector)

addPlayer1 <- GCFC22_TFg3[ which(GCFC22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_TFg3[ which(GCFC22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_TFg2 <- rbind(GCFC22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
GCFC22_TFft <- ftable(GCFC22_TFg2$player1, GCFC22_TFg2$player2)
GCFC22_TFft2 <- as.matrix(GCFC22_TFft)
numRows <- nrow(GCFC22_TFft2)
numCols <- ncol(GCFC22_TFft2)
GCFC22_TFft3 <- GCFC22_TFft2[c(2:numRows) , c(2:numCols)]
GCFC22_TFTable <- graph.adjacency(GCFC22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(GCFC22_TFTable, vertex.label = V(GCFC22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
GCFC22_TF.clusterCoef <- transitivity(GCFC22_TFTable, type="global") #cluster coefficient
GCFC22_TF.degreeCent <- centralization.degree(GCFC22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_TFftn <- as.network.matrix(GCFC22_TFft)
GCFC22_TF.netDensity <- network.density(GCFC22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_TF.entropy <- entropy(GCFC22_TFft) #entropy

GCFC22_TF.netMx <- cbind(GCFC22_TF.netMx, GCFC22_TF.clusterCoef, GCFC22_TF.degreeCent$centralization,
                         GCFC22_TF.netDensity, GCFC22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
GCFC22_SAMg2 <- data.frame(GCFC22_SAM)
GCFC22_SAMg2 <- GCFC22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_SAMg2$player1
player2vector <- GCFC22_SAMg2$player2
GCFC22_SAMg3 <- GCFC22_SAMg2
GCFC22_SAMg3$p1inp2vec <- is.element(GCFC22_SAMg3$player1, player2vector)
GCFC22_SAMg3$p2inp1vec <- is.element(GCFC22_SAMg3$player2, player1vector)

addPlayer1 <- GCFC22_SAMg3[ which(GCFC22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_SAMg3[ which(GCFC22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_SAMg2 <- rbind(GCFC22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
GCFC22_SAMft <- ftable(GCFC22_SAMg2$player1, GCFC22_SAMg2$player2)
GCFC22_SAMft2 <- as.matrix(GCFC22_SAMft)
numRows <- nrow(GCFC22_SAMft2)
numCols <- ncol(GCFC22_SAMft2)
GCFC22_SAMft3 <- GCFC22_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC22_SAMTable <- graph.adjacency(GCFC22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(GCFC22_SAMTable, vertex.label = V(GCFC22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
GCFC22_SAM.clusterCoef <- transitivity(GCFC22_SAMTable, type="global") #cluster coefficient
GCFC22_SAM.degreeCent <- centralization.degree(GCFC22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_SAMftn <- as.network.matrix(GCFC22_SAMft)
GCFC22_SAM.netDensity <- network.density(GCFC22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_SAM.entropy <- entropy(GCFC22_SAMft) #entropy

GCFC22_SAM.netMx <- cbind(GCFC22_SAM.netMx, GCFC22_SAM.clusterCoef, GCFC22_SAM.degreeCent$centralization,
                          GCFC22_SAM.netDensity, GCFC22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
GCFC22_TAMg2 <- data.frame(GCFC22_TAM)
GCFC22_TAMg2 <- GCFC22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_TAMg2$player1
player2vector <- GCFC22_TAMg2$player2
GCFC22_TAMg3 <- GCFC22_TAMg2
GCFC22_TAMg3$p1inp2vec <- is.element(GCFC22_TAMg3$player1, player2vector)
GCFC22_TAMg3$p2inp1vec <- is.element(GCFC22_TAMg3$player2, player1vector)

addPlayer1 <- GCFC22_TAMg3[ which(GCFC22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_TAMg3[ which(GCFC22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_TAMg2 <- rbind(GCFC22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
GCFC22_TAMft <- ftable(GCFC22_TAMg2$player1, GCFC22_TAMg2$player2)
GCFC22_TAMft2 <- as.matrix(GCFC22_TAMft)
numRows <- nrow(GCFC22_TAMft2)
numCols <- ncol(GCFC22_TAMft2)
GCFC22_TAMft3 <- GCFC22_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC22_TAMTable <- graph.adjacency(GCFC22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(GCFC22_TAMTable, vertex.label = V(GCFC22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
GCFC22_TAM.clusterCoef <- transitivity(GCFC22_TAMTable, type="global") #cluster coefficient
GCFC22_TAM.degreeCent <- centralization.degree(GCFC22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_TAMftn <- as.network.matrix(GCFC22_TAMft)
GCFC22_TAM.netDensity <- network.density(GCFC22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_TAM.entropy <- entropy(GCFC22_TAMft) #entropy

GCFC22_TAM.netMx <- cbind(GCFC22_TAM.netMx, GCFC22_TAM.clusterCoef, GCFC22_TAM.degreeCent$centralization,
                          GCFC22_TAM.netDensity, GCFC22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
GCFC22_SDMg2 <- data.frame(GCFC22_SDM)
GCFC22_SDMg2 <- GCFC22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_SDMg2$player1
player2vector <- GCFC22_SDMg2$player2
GCFC22_SDMg3 <- GCFC22_SDMg2
GCFC22_SDMg3$p1inp2vec <- is.element(GCFC22_SDMg3$player1, player2vector)
GCFC22_SDMg3$p2inp1vec <- is.element(GCFC22_SDMg3$player2, player1vector)

addPlayer1 <- GCFC22_SDMg3[ which(GCFC22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_SDMg3[ which(GCFC22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_SDMg2 <- rbind(GCFC22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
GCFC22_SDMft <- ftable(GCFC22_SDMg2$player1, GCFC22_SDMg2$player2)
GCFC22_SDMft2 <- as.matrix(GCFC22_SDMft)
numRows <- nrow(GCFC22_SDMft2)
numCols <- ncol(GCFC22_SDMft2)
GCFC22_SDMft3 <- GCFC22_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC22_SDMTable <- graph.adjacency(GCFC22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(GCFC22_SDMTable, vertex.label = V(GCFC22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
GCFC22_SDM.clusterCoef <- transitivity(GCFC22_SDMTable, type="global") #cluster coefficient
GCFC22_SDM.degreeCent <- centralization.degree(GCFC22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_SDMftn <- as.network.matrix(GCFC22_SDMft)
GCFC22_SDM.netDensity <- network.density(GCFC22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_SDM.entropy <- entropy(GCFC22_SDMft) #entropy

GCFC22_SDM.netMx <- cbind(GCFC22_SDM.netMx, GCFC22_SDM.clusterCoef, GCFC22_SDM.degreeCent$centralization,
                          GCFC22_SDM.netDensity, GCFC22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
GCFC22_TDMg2 <- data.frame(GCFC22_TDM)
GCFC22_TDMg2 <- GCFC22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_TDMg2$player1
player2vector <- GCFC22_TDMg2$player2
GCFC22_TDMg3 <- GCFC22_TDMg2
GCFC22_TDMg3$p1inp2vec <- is.element(GCFC22_TDMg3$player1, player2vector)
GCFC22_TDMg3$p2inp1vec <- is.element(GCFC22_TDMg3$player2, player1vector)

addPlayer1 <- GCFC22_TDMg3[ which(GCFC22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_TDMg3[ which(GCFC22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_TDMg2 <- rbind(GCFC22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
GCFC22_TDMft <- ftable(GCFC22_TDMg2$player1, GCFC22_TDMg2$player2)
GCFC22_TDMft2 <- as.matrix(GCFC22_TDMft)
numRows <- nrow(GCFC22_TDMft2)
numCols <- ncol(GCFC22_TDMft2)
GCFC22_TDMft3 <- GCFC22_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC22_TDMTable <- graph.adjacency(GCFC22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(GCFC22_TDMTable, vertex.label = V(GCFC22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
GCFC22_TDM.clusterCoef <- transitivity(GCFC22_TDMTable, type="global") #cluster coefficient
GCFC22_TDM.degreeCent <- centralization.degree(GCFC22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_TDMftn <- as.network.matrix(GCFC22_TDMft)
GCFC22_TDM.netDensity <- network.density(GCFC22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_TDM.entropy <- entropy(GCFC22_TDMft) #entropy

GCFC22_TDM.netMx <- cbind(GCFC22_TDM.netMx, GCFC22_TDM.clusterCoef, GCFC22_TDM.degreeCent$centralization,
                          GCFC22_TDM.netDensity, GCFC22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
GCFC22_SDg2 <- data.frame(GCFC22_SD)
GCFC22_SDg2 <- GCFC22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_SDg2$player1
player2vector <- GCFC22_SDg2$player2
GCFC22_SDg3 <- GCFC22_SDg2
GCFC22_SDg3$p1inp2vec <- is.element(GCFC22_SDg3$player1, player2vector)
GCFC22_SDg3$p2inp1vec <- is.element(GCFC22_SDg3$player2, player1vector)

addPlayer1 <- GCFC22_SDg3[ which(GCFC22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_SDg3[ which(GCFC22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_SDg2 <- rbind(GCFC22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
GCFC22_SDft <- ftable(GCFC22_SDg2$player1, GCFC22_SDg2$player2)
GCFC22_SDft2 <- as.matrix(GCFC22_SDft)
numRows <- nrow(GCFC22_SDft2)
numCols <- ncol(GCFC22_SDft2)
GCFC22_SDft3 <- GCFC22_SDft2[c(2:numRows) , c(2:numCols)]
GCFC22_SDTable <- graph.adjacency(GCFC22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(GCFC22_SDTable, vertex.label = V(GCFC22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
GCFC22_SD.clusterCoef <- transitivity(GCFC22_SDTable, type="global") #cluster coefficient
GCFC22_SD.degreeCent <- centralization.degree(GCFC22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_SDftn <- as.network.matrix(GCFC22_SDft)
GCFC22_SD.netDensity <- network.density(GCFC22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_SD.entropy <- entropy(GCFC22_SDft) #entropy

GCFC22_SD.netMx <- cbind(GCFC22_SD.netMx, GCFC22_SD.clusterCoef, GCFC22_SD.degreeCent$centralization,
                         GCFC22_SD.netDensity, GCFC22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
GCFC22_TDg2 <- data.frame(GCFC22_TD)
GCFC22_TDg2 <- GCFC22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_TDg2$player1
player2vector <- GCFC22_TDg2$player2
GCFC22_TDg3 <- GCFC22_TDg2
GCFC22_TDg3$p1inp2vec <- is.element(GCFC22_TDg3$player1, player2vector)
GCFC22_TDg3$p2inp1vec <- is.element(GCFC22_TDg3$player2, player1vector)

addPlayer1 <- GCFC22_TDg3[ which(GCFC22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_TDg3[ which(GCFC22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_TDg2 <- rbind(GCFC22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
GCFC22_TDft <- ftable(GCFC22_TDg2$player1, GCFC22_TDg2$player2)
GCFC22_TDft2 <- as.matrix(GCFC22_TDft)
numRows <- nrow(GCFC22_TDft2)
numCols <- ncol(GCFC22_TDft2)
GCFC22_TDft3 <- GCFC22_TDft2[c(2:numRows) , c(2:numCols)]
GCFC22_TDTable <- graph.adjacency(GCFC22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(GCFC22_TDTable, vertex.label = V(GCFC22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
GCFC22_TD.clusterCoef <- transitivity(GCFC22_TDTable, type="global") #cluster coefficient
GCFC22_TD.degreeCent <- centralization.degree(GCFC22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_TDftn <- as.network.matrix(GCFC22_TDft)
GCFC22_TD.netDensity <- network.density(GCFC22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_TD.entropy <- entropy(GCFC22_TDft) #entropy

GCFC22_TD.netMx <- cbind(GCFC22_TD.netMx, GCFC22_TD.clusterCoef, GCFC22_TD.degreeCent$centralization,
                         GCFC22_TD.netDensity, GCFC22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
GCFC22_QTg2 <- data.frame(GCFC22_QT)
GCFC22_QTg2 <- GCFC22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC22_QTg2$player1
player2vector <- GCFC22_QTg2$player2
GCFC22_QTg3 <- GCFC22_QTg2
GCFC22_QTg3$p1inp2vec <- is.element(GCFC22_QTg3$player1, player2vector)
GCFC22_QTg3$p2inp1vec <- is.element(GCFC22_QTg3$player2, player1vector)

addPlayer1 <- GCFC22_QTg3[ which(GCFC22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC22_QTg3[ which(GCFC22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC22_QTg2 <- rbind(GCFC22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
GCFC22_QTft <- ftable(GCFC22_QTg2$player1, GCFC22_QTg2$player2)
GCFC22_QTft2 <- as.matrix(GCFC22_QTft)
numRows <- nrow(GCFC22_QTft2)
numCols <- ncol(GCFC22_QTft2)
GCFC22_QTft3 <- GCFC22_QTft2[c(2:numRows) , c(2:numCols)]
GCFC22_QTTable <- graph.adjacency(GCFC22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(GCFC22_QTTable, vertex.label = V(GCFC22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
GCFC22_QT.clusterCoef <- transitivity(GCFC22_QTTable, type="global") #cluster coefficient
GCFC22_QT.degreeCent <- centralization.degree(GCFC22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC22_QTftn <- as.network.matrix(GCFC22_QTft)
GCFC22_QT.netDensity <- network.density(GCFC22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC22_QT.entropy <- entropy(GCFC22_QTft) #entropy

GCFC22_QT.netMx <- cbind(GCFC22_QT.netMx, GCFC22_QT.clusterCoef, GCFC22_QT.degreeCent$centralization,
                         GCFC22_QT.netDensity, GCFC22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC22_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
GEEL22_Gg2 <- data.frame(GEEL22_G)
GEEL22_Gg2 <- GEEL22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_Gg2$player1
player2vector <- GEEL22_Gg2$player2
GEEL22_Gg3 <- GEEL22_Gg2
GEEL22_Gg3$p1inp2vec <- is.element(GEEL22_Gg3$player1, player2vector)
GEEL22_Gg3$p2inp1vec <- is.element(GEEL22_Gg3$player2, player1vector)

addPlayer1 <- GEEL22_Gg3[ which(GEEL22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_Gg3[ which(GEEL22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_Gg2 <- rbind(GEEL22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
GEEL22_Gft <- ftable(GEEL22_Gg2$player1, GEEL22_Gg2$player2)
GEEL22_Gft2 <- as.matrix(GEEL22_Gft)
numRows <- nrow(GEEL22_Gft2)
numCols <- ncol(GEEL22_Gft2)
GEEL22_Gft3 <- GEEL22_Gft2[c(2:numRows) , c(2:numCols)]
GEEL22_GTable <- graph.adjacency(GEEL22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(GEEL22_GTable, vertex.label = V(GEEL22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
GEEL22_G.clusterCoef <- transitivity(GEEL22_GTable, type="global") #cluster coefficient
GEEL22_G.degreeCent <- centralization.degree(GEEL22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_Gftn <- as.network.matrix(GEEL22_Gft)
GEEL22_G.netDensity <- network.density(GEEL22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_G.entropy <- entropy(GEEL22_Gft) #entropy

GEEL22_G.netMx <- cbind(GEEL22_G.netMx, GEEL22_G.clusterCoef, GEEL22_G.degreeCent$centralization,
                        GEEL22_G.netDensity, GEEL22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
GEEL22_Bg2 <- data.frame(GEEL22_B)
GEEL22_Bg2 <- GEEL22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_Bg2$player1
player2vector <- GEEL22_Bg2$player2
GEEL22_Bg3 <- GEEL22_Bg2
GEEL22_Bg3$p1inp2vec <- is.element(GEEL22_Bg3$player1, player2vector)
GEEL22_Bg3$p2inp1vec <- is.element(GEEL22_Bg3$player2, player1vector)

addPlayer1 <- GEEL22_Bg3[ which(GEEL22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_Bg3[ which(GEEL22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_Bg2 <- rbind(GEEL22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
GEEL22_Bft <- ftable(GEEL22_Bg2$player1, GEEL22_Bg2$player2)
GEEL22_Bft2 <- as.matrix(GEEL22_Bft)
numRows <- nrow(GEEL22_Bft2)
numCols <- ncol(GEEL22_Bft2)
GEEL22_Bft3 <- GEEL22_Bft2[c(2:numRows) , c(2:numCols)]
GEEL22_BTable <- graph.adjacency(GEEL22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(GEEL22_BTable, vertex.label = V(GEEL22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
GEEL22_B.clusterCoef <- transitivity(GEEL22_BTable, type="global") #cluster coefficient
GEEL22_B.degreeCent <- centralization.degree(GEEL22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_Bftn <- as.network.matrix(GEEL22_Bft)
GEEL22_B.netDensity <- network.density(GEEL22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_B.entropy <- entropy(GEEL22_Bft) #entropy

GEEL22_B.netMx <- cbind(GEEL22_B.netMx, GEEL22_B.clusterCoef, GEEL22_B.degreeCent$centralization,
                        GEEL22_B.netDensity, GEEL22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************

round = 22
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
GEEL22_SFg2 <- data.frame(GEEL22_SF)
GEEL22_SFg2 <- GEEL22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_SFg2$player1
player2vector <- GEEL22_SFg2$player2
GEEL22_SFg3 <- GEEL22_SFg2
GEEL22_SFg3$p1inp2vec <- is.element(GEEL22_SFg3$player1, player2vector)
GEEL22_SFg3$p2inp1vec <- is.element(GEEL22_SFg3$player2, player1vector)

addPlayer1 <- GEEL22_SFg3[ which(GEEL22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_SFg3[ which(GEEL22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_SFg2 <- rbind(GEEL22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
GEEL22_SFft <- ftable(GEEL22_SFg2$player1, GEEL22_SFg2$player2)
GEEL22_SFft2 <- as.matrix(GEEL22_SFft)
numRows <- nrow(GEEL22_SFft2)
numCols <- ncol(GEEL22_SFft2)
GEEL22_SFft3 <- GEEL22_SFft2[c(2:numRows) , c(2:numCols)]
GEEL22_SFTable <- graph.adjacency(GEEL22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(GEEL22_SFTable, vertex.label = V(GEEL22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
GEEL22_SF.clusterCoef <- transitivity(GEEL22_SFTable, type="global") #cluster coefficient
GEEL22_SF.degreeCent <- centralization.degree(GEEL22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_SFftn <- as.network.matrix(GEEL22_SFft)
GEEL22_SF.netDensity <- network.density(GEEL22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_SF.entropy <- entropy(GEEL22_SFft) #entropy

GEEL22_SF.netMx <- cbind(GEEL22_SF.netMx, GEEL22_SF.clusterCoef, GEEL22_SF.degreeCent$centralization,
                         GEEL22_SF.netDensity, GEEL22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
GEEL22_TFg2 <- data.frame(GEEL22_TF)
GEEL22_TFg2 <- GEEL22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_TFg2$player1
player2vector <- GEEL22_TFg2$player2
GEEL22_TFg3 <- GEEL22_TFg2
GEEL22_TFg3$p1inp2vec <- is.element(GEEL22_TFg3$player1, player2vector)
GEEL22_TFg3$p2inp1vec <- is.element(GEEL22_TFg3$player2, player1vector)

addPlayer1 <- GEEL22_TFg3[ which(GEEL22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_TFg3[ which(GEEL22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_TFg2 <- rbind(GEEL22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
GEEL22_TFft <- ftable(GEEL22_TFg2$player1, GEEL22_TFg2$player2)
GEEL22_TFft2 <- as.matrix(GEEL22_TFft)
numRows <- nrow(GEEL22_TFft2)
numCols <- ncol(GEEL22_TFft2)
GEEL22_TFft3 <- GEEL22_TFft2[c(2:numRows) , c(2:numCols)]
GEEL22_TFTable <- graph.adjacency(GEEL22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(GEEL22_TFTable, vertex.label = V(GEEL22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
GEEL22_TF.clusterCoef <- transitivity(GEEL22_TFTable, type="global") #cluster coefficient
GEEL22_TF.degreeCent <- centralization.degree(GEEL22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_TFftn <- as.network.matrix(GEEL22_TFft)
GEEL22_TF.netDensity <- network.density(GEEL22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_TF.entropy <- entropy(GEEL22_TFft) #entropy

GEEL22_TF.netMx <- cbind(GEEL22_TF.netMx, GEEL22_TF.clusterCoef, GEEL22_TF.degreeCent$centralization,
                         GEEL22_TF.netDensity, GEEL22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
GEEL22_SAMg2 <- data.frame(GEEL22_SAM)
GEEL22_SAMg2 <- GEEL22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_SAMg2$player1
player2vector <- GEEL22_SAMg2$player2
GEEL22_SAMg3 <- GEEL22_SAMg2
GEEL22_SAMg3$p1inp2vec <- is.element(GEEL22_SAMg3$player1, player2vector)
GEEL22_SAMg3$p2inp1vec <- is.element(GEEL22_SAMg3$player2, player1vector)

addPlayer1 <- GEEL22_SAMg3[ which(GEEL22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_SAMg3[ which(GEEL22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_SAMg2 <- rbind(GEEL22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
GEEL22_SAMft <- ftable(GEEL22_SAMg2$player1, GEEL22_SAMg2$player2)
GEEL22_SAMft2 <- as.matrix(GEEL22_SAMft)
numRows <- nrow(GEEL22_SAMft2)
numCols <- ncol(GEEL22_SAMft2)
GEEL22_SAMft3 <- GEEL22_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL22_SAMTable <- graph.adjacency(GEEL22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(GEEL22_SAMTable, vertex.label = V(GEEL22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
GEEL22_SAM.clusterCoef <- transitivity(GEEL22_SAMTable, type="global") #cluster coefficient
GEEL22_SAM.degreeCent <- centralization.degree(GEEL22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_SAMftn <- as.network.matrix(GEEL22_SAMft)
GEEL22_SAM.netDensity <- network.density(GEEL22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_SAM.entropy <- entropy(GEEL22_SAMft) #entropy

GEEL22_SAM.netMx <- cbind(GEEL22_SAM.netMx, GEEL22_SAM.clusterCoef, GEEL22_SAM.degreeCent$centralization,
                          GEEL22_SAM.netDensity, GEEL22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
GEEL22_TAMg2 <- data.frame(GEEL22_TAM)
GEEL22_TAMg2 <- GEEL22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_TAMg2$player1
player2vector <- GEEL22_TAMg2$player2
GEEL22_TAMg3 <- GEEL22_TAMg2
GEEL22_TAMg3$p1inp2vec <- is.element(GEEL22_TAMg3$player1, player2vector)
GEEL22_TAMg3$p2inp1vec <- is.element(GEEL22_TAMg3$player2, player1vector)

addPlayer1 <- GEEL22_TAMg3[ which(GEEL22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_TAMg3[ which(GEEL22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_TAMg2 <- rbind(GEEL22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
GEEL22_TAMft <- ftable(GEEL22_TAMg2$player1, GEEL22_TAMg2$player2)
GEEL22_TAMft2 <- as.matrix(GEEL22_TAMft)
numRows <- nrow(GEEL22_TAMft2)
numCols <- ncol(GEEL22_TAMft2)
GEEL22_TAMft3 <- GEEL22_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL22_TAMTable <- graph.adjacency(GEEL22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(GEEL22_TAMTable, vertex.label = V(GEEL22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
GEEL22_TAM.clusterCoef <- transitivity(GEEL22_TAMTable, type="global") #cluster coefficient
GEEL22_TAM.degreeCent <- centralization.degree(GEEL22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_TAMftn <- as.network.matrix(GEEL22_TAMft)
GEEL22_TAM.netDensity <- network.density(GEEL22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_TAM.entropy <- entropy(GEEL22_TAMft) #entropy

GEEL22_TAM.netMx <- cbind(GEEL22_TAM.netMx, GEEL22_TAM.clusterCoef, GEEL22_TAM.degreeCent$centralization,
                          GEEL22_TAM.netDensity, GEEL22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
GEEL22_SDMg2 <- data.frame(GEEL22_SDM)
GEEL22_SDMg2 <- GEEL22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_SDMg2$player1
player2vector <- GEEL22_SDMg2$player2
GEEL22_SDMg3 <- GEEL22_SDMg2
GEEL22_SDMg3$p1inp2vec <- is.element(GEEL22_SDMg3$player1, player2vector)
GEEL22_SDMg3$p2inp1vec <- is.element(GEEL22_SDMg3$player2, player1vector)

addPlayer1 <- GEEL22_SDMg3[ which(GEEL22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL22_SDMg3[ which(GEEL22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_SDMg2 <- rbind(GEEL22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
GEEL22_SDMft <- ftable(GEEL22_SDMg2$player1, GEEL22_SDMg2$player2)
GEEL22_SDMft2 <- as.matrix(GEEL22_SDMft)
numRows <- nrow(GEEL22_SDMft2)
numCols <- ncol(GEEL22_SDMft2)
GEEL22_SDMft3 <- GEEL22_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL22_SDMTable <- graph.adjacency(GEEL22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(GEEL22_SDMTable, vertex.label = V(GEEL22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
GEEL22_SDM.clusterCoef <- transitivity(GEEL22_SDMTable, type="global") #cluster coefficient
GEEL22_SDM.degreeCent <- centralization.degree(GEEL22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_SDMftn <- as.network.matrix(GEEL22_SDMft)
GEEL22_SDM.netDensity <- network.density(GEEL22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_SDM.entropy <- entropy(GEEL22_SDMft) #entropy

GEEL22_SDM.netMx <- cbind(GEEL22_SDM.netMx, GEEL22_SDM.clusterCoef, GEEL22_SDM.degreeCent$centralization,
                          GEEL22_SDM.netDensity, GEEL22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
GEEL22_TDMg2 <- data.frame(GEEL22_TDM)
GEEL22_TDMg2 <- GEEL22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_TDMg2$player1
player2vector <- GEEL22_TDMg2$player2
GEEL22_TDMg3 <- GEEL22_TDMg2
GEEL22_TDMg3$p1inp2vec <- is.element(GEEL22_TDMg3$player1, player2vector)
GEEL22_TDMg3$p2inp1vec <- is.element(GEEL22_TDMg3$player2, player1vector)

addPlayer1 <- GEEL22_TDMg3[ which(GEEL22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_TDMg3[ which(GEEL22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_TDMg2 <- rbind(GEEL22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
GEEL22_TDMft <- ftable(GEEL22_TDMg2$player1, GEEL22_TDMg2$player2)
GEEL22_TDMft2 <- as.matrix(GEEL22_TDMft)
numRows <- nrow(GEEL22_TDMft2)
numCols <- ncol(GEEL22_TDMft2)
GEEL22_TDMft3 <- GEEL22_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL22_TDMTable <- graph.adjacency(GEEL22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(GEEL22_TDMTable, vertex.label = V(GEEL22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
GEEL22_TDM.clusterCoef <- transitivity(GEEL22_TDMTable, type="global") #cluster coefficient
GEEL22_TDM.degreeCent <- centralization.degree(GEEL22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_TDMftn <- as.network.matrix(GEEL22_TDMft)
GEEL22_TDM.netDensity <- network.density(GEEL22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_TDM.entropy <- entropy(GEEL22_TDMft) #entropy

GEEL22_TDM.netMx <- cbind(GEEL22_TDM.netMx, GEEL22_TDM.clusterCoef, GEEL22_TDM.degreeCent$centralization,
                          GEEL22_TDM.netDensity, GEEL22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
GEEL22_SDg2 <- data.frame(GEEL22_SD)
GEEL22_SDg2 <- GEEL22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_SDg2$player1
player2vector <- GEEL22_SDg2$player2
GEEL22_SDg3 <- GEEL22_SDg2
GEEL22_SDg3$p1inp2vec <- is.element(GEEL22_SDg3$player1, player2vector)
GEEL22_SDg3$p2inp1vec <- is.element(GEEL22_SDg3$player2, player1vector)

addPlayer1 <- GEEL22_SDg3[ which(GEEL22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_SDg3[ which(GEEL22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_SDg2 <- rbind(GEEL22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
GEEL22_SDft <- ftable(GEEL22_SDg2$player1, GEEL22_SDg2$player2)
GEEL22_SDft2 <- as.matrix(GEEL22_SDft)
numRows <- nrow(GEEL22_SDft2)
numCols <- ncol(GEEL22_SDft2)
GEEL22_SDft3 <- GEEL22_SDft2[c(2:numRows) , c(2:numCols)]
GEEL22_SDTable <- graph.adjacency(GEEL22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(GEEL22_SDTable, vertex.label = V(GEEL22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
GEEL22_SD.clusterCoef <- transitivity(GEEL22_SDTable, type="global") #cluster coefficient
GEEL22_SD.degreeCent <- centralization.degree(GEEL22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_SDftn <- as.network.matrix(GEEL22_SDft)
GEEL22_SD.netDensity <- network.density(GEEL22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_SD.entropy <- entropy(GEEL22_SDft) #entropy

GEEL22_SD.netMx <- cbind(GEEL22_SD.netMx, GEEL22_SD.clusterCoef, GEEL22_SD.degreeCent$centralization,
                         GEEL22_SD.netDensity, GEEL22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************

round = 22
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
GEEL22_TDg2 <- data.frame(GEEL22_TD)
GEEL22_TDg2 <- GEEL22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_TDg2$player1
player2vector <- GEEL22_TDg2$player2
GEEL22_TDg3 <- GEEL22_TDg2
GEEL22_TDg3$p1inp2vec <- is.element(GEEL22_TDg3$player1, player2vector)
GEEL22_TDg3$p2inp1vec <- is.element(GEEL22_TDg3$player2, player1vector)

addPlayer1 <- GEEL22_TDg3[ which(GEEL22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL22_TDg3[ which(GEEL22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_TDg2 <- rbind(GEEL22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
GEEL22_TDft <- ftable(GEEL22_TDg2$player1, GEEL22_TDg2$player2)
GEEL22_TDft2 <- as.matrix(GEEL22_TDft)
numRows <- nrow(GEEL22_TDft2)
numCols <- ncol(GEEL22_TDft2)
GEEL22_TDft3 <- GEEL22_TDft2[c(2:numRows) , c(2:numCols)]
GEEL22_TDTable <- graph.adjacency(GEEL22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(GEEL22_TDTable, vertex.label = V(GEEL22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
GEEL22_TD.clusterCoef <- transitivity(GEEL22_TDTable, type="global") #cluster coefficient
GEEL22_TD.degreeCent <- centralization.degree(GEEL22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_TDftn <- as.network.matrix(GEEL22_TDft)
GEEL22_TD.netDensity <- network.density(GEEL22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_TD.entropy <- entropy(GEEL22_TDft) #entropy

GEEL22_TD.netMx <- cbind(GEEL22_TD.netMx, GEEL22_TD.clusterCoef, GEEL22_TD.degreeCent$centralization,
                         GEEL22_TD.netDensity, GEEL22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
GEEL22_QTg2 <- data.frame(GEEL22_QT)
GEEL22_QTg2 <- GEEL22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL22_QTg2$player1
player2vector <- GEEL22_QTg2$player2
GEEL22_QTg3 <- GEEL22_QTg2
GEEL22_QTg3$p1inp2vec <- is.element(GEEL22_QTg3$player1, player2vector)
GEEL22_QTg3$p2inp1vec <- is.element(GEEL22_QTg3$player2, player1vector)

addPlayer1 <- GEEL22_QTg3[ which(GEEL22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL22_QTg3[ which(GEEL22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL22_QTg2 <- rbind(GEEL22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
GEEL22_QTft <- ftable(GEEL22_QTg2$player1, GEEL22_QTg2$player2)
GEEL22_QTft2 <- as.matrix(GEEL22_QTft)
numRows <- nrow(GEEL22_QTft2)
numCols <- ncol(GEEL22_QTft2)
GEEL22_QTft3 <- GEEL22_QTft2[c(2:numRows) , c(2:numCols)]
GEEL22_QTTable <- graph.adjacency(GEEL22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(GEEL22_QTTable, vertex.label = V(GEEL22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
GEEL22_QT.clusterCoef <- transitivity(GEEL22_QTTable, type="global") #cluster coefficient
GEEL22_QT.degreeCent <- centralization.degree(GEEL22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL22_QTftn <- as.network.matrix(GEEL22_QTft)
GEEL22_QT.netDensity <- network.density(GEEL22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL22_QT.entropy <- entropy(GEEL22_QTft) #entropy

GEEL22_QT.netMx <- cbind(GEEL22_QT.netMx, GEEL22_QT.clusterCoef, GEEL22_QT.degreeCent$centralization,
                         GEEL22_QT.netDensity, GEEL22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL22_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Goal_F"
GWS22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
GWS22_Gg2 <- data.frame(GWS22_G)
GWS22_Gg2 <- GWS22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_Gg2$player1
player2vector <- GWS22_Gg2$player2
GWS22_Gg3 <- GWS22_Gg2
GWS22_Gg3$p1inp2vec <- is.element(GWS22_Gg3$player1, player2vector)
GWS22_Gg3$p2inp1vec <- is.element(GWS22_Gg3$player2, player1vector)

addPlayer1 <- GWS22_Gg3[ which(GWS22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_Gg3[ which(GWS22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_Gg2 <- rbind(GWS22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
GWS22_Gft <- ftable(GWS22_Gg2$player1, GWS22_Gg2$player2)
GWS22_Gft2 <- as.matrix(GWS22_Gft)
numRows <- nrow(GWS22_Gft2)
numCols <- ncol(GWS22_Gft2)
GWS22_Gft3 <- GWS22_Gft2[c(1:numRows) , c(1:numCols)]
GWS22_GTable <- graph.adjacency(GWS22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(GWS22_GTable, vertex.label = V(GWS22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
GWS22_G.clusterCoef <- transitivity(GWS22_GTable, type="global") #cluster coefficient
GWS22_G.degreeCent <- centralization.degree(GWS22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_Gftn <- as.network.matrix(GWS22_Gft)
GWS22_G.netDensity <- network.density(GWS22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_G.entropy <- entropy(GWS22_Gft) #entropy

GWS22_G.netMx <- cbind(GWS22_G.netMx, GWS22_G.clusterCoef, GWS22_G.degreeCent$centralization,
                       GWS22_G.netDensity, GWS22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Behind_F"
GWS22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
GWS22_Bg2 <- data.frame(GWS22_B)
GWS22_Bg2 <- GWS22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_Bg2$player1
player2vector <- GWS22_Bg2$player2
GWS22_Bg3 <- GWS22_Bg2
GWS22_Bg3$p1inp2vec <- is.element(GWS22_Bg3$player1, player2vector)
GWS22_Bg3$p2inp1vec <- is.element(GWS22_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS22_Bg3[ which(GWS22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS22_Bg2 <- rbind(GWS22_Bg2, addPlayer2)

#ROUND 22, Behind graph using weighted edges
GWS22_Bft <- ftable(GWS22_Bg2$player1, GWS22_Bg2$player2)
GWS22_Bft2 <- as.matrix(GWS22_Bft)
numRows <- nrow(GWS22_Bft2)
numCols <- ncol(GWS22_Bft2)
GWS22_Bft3 <- GWS22_Bft2[c(1:numRows) , c(2:numCols)]
GWS22_BTable <- graph.adjacency(GWS22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(GWS22_BTable, vertex.label = V(GWS22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
GWS22_B.clusterCoef <- transitivity(GWS22_BTable, type="global") #cluster coefficient
GWS22_B.degreeCent <- centralization.degree(GWS22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_Bftn <- as.network.matrix(GWS22_Bft)
GWS22_B.netDensity <- network.density(GWS22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_B.entropy <- entropy(GWS22_Bft) #entropy

GWS22_B.netMx <- cbind(GWS22_B.netMx, GWS22_B.clusterCoef, GWS22_B.degreeCent$centralization,
                       GWS22_B.netDensity, GWS22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
GWS22_SFg2 <- data.frame(GWS22_SF)
GWS22_SFg2 <- GWS22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_SFg2$player1
player2vector <- GWS22_SFg2$player2
GWS22_SFg3 <- GWS22_SFg2
GWS22_SFg3$p1inp2vec <- is.element(GWS22_SFg3$player1, player2vector)
GWS22_SFg3$p2inp1vec <- is.element(GWS22_SFg3$player2, player1vector)

addPlayer1 <- GWS22_SFg3[ which(GWS22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_SFg3[ which(GWS22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_SFg2 <- rbind(GWS22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
GWS22_SFft <- ftable(GWS22_SFg2$player1, GWS22_SFg2$player2)
GWS22_SFft2 <- as.matrix(GWS22_SFft)
numRows <- nrow(GWS22_SFft2)
numCols <- ncol(GWS22_SFft2)
GWS22_SFft3 <- GWS22_SFft2[c(2:numRows) , c(2:numCols)]
GWS22_SFTable <- graph.adjacency(GWS22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(GWS22_SFTable, vertex.label = V(GWS22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
GWS22_SF.clusterCoef <- transitivity(GWS22_SFTable, type="global") #cluster coefficient
GWS22_SF.degreeCent <- centralization.degree(GWS22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_SFftn <- as.network.matrix(GWS22_SFft)
GWS22_SF.netDensity <- network.density(GWS22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_SF.entropy <- entropy(GWS22_SFft) #entropy

GWS22_SF.netMx <- cbind(GWS22_SF.netMx, GWS22_SF.clusterCoef, GWS22_SF.degreeCent$centralization,
                        GWS22_SF.netDensity, GWS22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
GWS22_TFg2 <- data.frame(GWS22_TF)
GWS22_TFg2 <- GWS22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_TFg2$player1
player2vector <- GWS22_TFg2$player2
GWS22_TFg3 <- GWS22_TFg2
GWS22_TFg3$p1inp2vec <- is.element(GWS22_TFg3$player1, player2vector)
GWS22_TFg3$p2inp1vec <- is.element(GWS22_TFg3$player2, player1vector)

addPlayer1 <- GWS22_TFg3[ which(GWS22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_TFg3[ which(GWS22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_TFg2 <- rbind(GWS22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
GWS22_TFft <- ftable(GWS22_TFg2$player1, GWS22_TFg2$player2)
GWS22_TFft2 <- as.matrix(GWS22_TFft)
numRows <- nrow(GWS22_TFft2)
numCols <- ncol(GWS22_TFft2)
GWS22_TFft3 <- GWS22_TFft2[c(2:numRows) , c(2:numCols)]
GWS22_TFTable <- graph.adjacency(GWS22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(GWS22_TFTable, vertex.label = V(GWS22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
GWS22_TF.clusterCoef <- transitivity(GWS22_TFTable, type="global") #cluster coefficient
GWS22_TF.degreeCent <- centralization.degree(GWS22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_TFftn <- as.network.matrix(GWS22_TFft)
GWS22_TF.netDensity <- network.density(GWS22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_TF.entropy <- entropy(GWS22_TFft) #entropy

GWS22_TF.netMx <- cbind(GWS22_TF.netMx, GWS22_TF.clusterCoef, GWS22_TF.degreeCent$centralization,
                        GWS22_TF.netDensity, GWS22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
GWS22_SAMg2 <- data.frame(GWS22_SAM)
GWS22_SAMg2 <- GWS22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_SAMg2$player1
player2vector <- GWS22_SAMg2$player2
GWS22_SAMg3 <- GWS22_SAMg2
GWS22_SAMg3$p1inp2vec <- is.element(GWS22_SAMg3$player1, player2vector)
GWS22_SAMg3$p2inp1vec <- is.element(GWS22_SAMg3$player2, player1vector)

addPlayer1 <- GWS22_SAMg3[ which(GWS22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_SAMg3[ which(GWS22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_SAMg2 <- rbind(GWS22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
GWS22_SAMft <- ftable(GWS22_SAMg2$player1, GWS22_SAMg2$player2)
GWS22_SAMft2 <- as.matrix(GWS22_SAMft)
numRows <- nrow(GWS22_SAMft2)
numCols <- ncol(GWS22_SAMft2)
GWS22_SAMft3 <- GWS22_SAMft2[c(2:numRows) , c(2:numCols)]
GWS22_SAMTable <- graph.adjacency(GWS22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(GWS22_SAMTable, vertex.label = V(GWS22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
GWS22_SAM.clusterCoef <- transitivity(GWS22_SAMTable, type="global") #cluster coefficient
GWS22_SAM.degreeCent <- centralization.degree(GWS22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_SAMftn <- as.network.matrix(GWS22_SAMft)
GWS22_SAM.netDensity <- network.density(GWS22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_SAM.entropy <- entropy(GWS22_SAMft) #entropy

GWS22_SAM.netMx <- cbind(GWS22_SAM.netMx, GWS22_SAM.clusterCoef, GWS22_SAM.degreeCent$centralization,
                         GWS22_SAM.netDensity, GWS22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
GWS22_TAMg2 <- data.frame(GWS22_TAM)
GWS22_TAMg2 <- GWS22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_TAMg2$player1
player2vector <- GWS22_TAMg2$player2
GWS22_TAMg3 <- GWS22_TAMg2
GWS22_TAMg3$p1inp2vec <- is.element(GWS22_TAMg3$player1, player2vector)
GWS22_TAMg3$p2inp1vec <- is.element(GWS22_TAMg3$player2, player1vector)

addPlayer1 <- GWS22_TAMg3[ which(GWS22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_TAMg3[ which(GWS22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_TAMg2 <- rbind(GWS22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
GWS22_TAMft <- ftable(GWS22_TAMg2$player1, GWS22_TAMg2$player2)
GWS22_TAMft2 <- as.matrix(GWS22_TAMft)
numRows <- nrow(GWS22_TAMft2)
numCols <- ncol(GWS22_TAMft2)
GWS22_TAMft3 <- GWS22_TAMft2[c(2:numRows) , c(2:numCols)]
GWS22_TAMTable <- graph.adjacency(GWS22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(GWS22_TAMTable, vertex.label = V(GWS22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
GWS22_TAM.clusterCoef <- transitivity(GWS22_TAMTable, type="global") #cluster coefficient
GWS22_TAM.degreeCent <- centralization.degree(GWS22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_TAMftn <- as.network.matrix(GWS22_TAMft)
GWS22_TAM.netDensity <- network.density(GWS22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_TAM.entropy <- entropy(GWS22_TAMft) #entropy

GWS22_TAM.netMx <- cbind(GWS22_TAM.netMx, GWS22_TAM.clusterCoef, GWS22_TAM.degreeCent$centralization,
                         GWS22_TAM.netDensity, GWS22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
GWS22_SDMg2 <- data.frame(GWS22_SDM)
GWS22_SDMg2 <- GWS22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_SDMg2$player1
player2vector <- GWS22_SDMg2$player2
GWS22_SDMg3 <- GWS22_SDMg2
GWS22_SDMg3$p1inp2vec <- is.element(GWS22_SDMg3$player1, player2vector)
GWS22_SDMg3$p2inp1vec <- is.element(GWS22_SDMg3$player2, player1vector)

addPlayer1 <- GWS22_SDMg3[ which(GWS22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_SDMg3[ which(GWS22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_SDMg2 <- rbind(GWS22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
GWS22_SDMft <- ftable(GWS22_SDMg2$player1, GWS22_SDMg2$player2)
GWS22_SDMft2 <- as.matrix(GWS22_SDMft)
numRows <- nrow(GWS22_SDMft2)
numCols <- ncol(GWS22_SDMft2)
GWS22_SDMft3 <- GWS22_SDMft2[c(2:numRows) , c(2:numCols)]
GWS22_SDMTable <- graph.adjacency(GWS22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(GWS22_SDMTable, vertex.label = V(GWS22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
GWS22_SDM.clusterCoef <- transitivity(GWS22_SDMTable, type="global") #cluster coefficient
GWS22_SDM.degreeCent <- centralization.degree(GWS22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_SDMftn <- as.network.matrix(GWS22_SDMft)
GWS22_SDM.netDensity <- network.density(GWS22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_SDM.entropy <- entropy(GWS22_SDMft) #entropy

GWS22_SDM.netMx <- cbind(GWS22_SDM.netMx, GWS22_SDM.clusterCoef, GWS22_SDM.degreeCent$centralization,
                         GWS22_SDM.netDensity, GWS22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
GWS22_TDMg2 <- data.frame(GWS22_TDM)
GWS22_TDMg2 <- GWS22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_TDMg2$player1
player2vector <- GWS22_TDMg2$player2
GWS22_TDMg3 <- GWS22_TDMg2
GWS22_TDMg3$p1inp2vec <- is.element(GWS22_TDMg3$player1, player2vector)
GWS22_TDMg3$p2inp1vec <- is.element(GWS22_TDMg3$player2, player1vector)

addPlayer1 <- GWS22_TDMg3[ which(GWS22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_TDMg3[ which(GWS22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_TDMg2 <- rbind(GWS22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
GWS22_TDMft <- ftable(GWS22_TDMg2$player1, GWS22_TDMg2$player2)
GWS22_TDMft2 <- as.matrix(GWS22_TDMft)
numRows <- nrow(GWS22_TDMft2)
numCols <- ncol(GWS22_TDMft2)
GWS22_TDMft3 <- GWS22_TDMft2[c(2:numRows) , c(2:numCols)]
GWS22_TDMTable <- graph.adjacency(GWS22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(GWS22_TDMTable, vertex.label = V(GWS22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
GWS22_TDM.clusterCoef <- transitivity(GWS22_TDMTable, type="global") #cluster coefficient
GWS22_TDM.degreeCent <- centralization.degree(GWS22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_TDMftn <- as.network.matrix(GWS22_TDMft)
GWS22_TDM.netDensity <- network.density(GWS22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_TDM.entropy <- entropy(GWS22_TDMft) #entropy

GWS22_TDM.netMx <- cbind(GWS22_TDM.netMx, GWS22_TDM.clusterCoef, GWS22_TDM.degreeCent$centralization,
                         GWS22_TDM.netDensity, GWS22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
GWS22_SDg2 <- data.frame(GWS22_SD)
GWS22_SDg2 <- GWS22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_SDg2$player1
player2vector <- GWS22_SDg2$player2
GWS22_SDg3 <- GWS22_SDg2
GWS22_SDg3$p1inp2vec <- is.element(GWS22_SDg3$player1, player2vector)
GWS22_SDg3$p2inp1vec <- is.element(GWS22_SDg3$player2, player1vector)

addPlayer1 <- GWS22_SDg3[ which(GWS22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_SDg3[ which(GWS22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_SDg2 <- rbind(GWS22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
GWS22_SDft <- ftable(GWS22_SDg2$player1, GWS22_SDg2$player2)
GWS22_SDft2 <- as.matrix(GWS22_SDft)
numRows <- nrow(GWS22_SDft2)
numCols <- ncol(GWS22_SDft2)
GWS22_SDft3 <- GWS22_SDft2[c(2:numRows) , c(2:numCols)]
GWS22_SDTable <- graph.adjacency(GWS22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(GWS22_SDTable, vertex.label = V(GWS22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
GWS22_SD.clusterCoef <- transitivity(GWS22_SDTable, type="global") #cluster coefficient
GWS22_SD.degreeCent <- centralization.degree(GWS22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_SDftn <- as.network.matrix(GWS22_SDft)
GWS22_SD.netDensity <- network.density(GWS22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_SD.entropy <- entropy(GWS22_SDft) #entropy

GWS22_SD.netMx <- cbind(GWS22_SD.netMx, GWS22_SD.clusterCoef, GWS22_SD.degreeCent$centralization,
                        GWS22_SD.netDensity, GWS22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************

round = 22
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
GWS22_TDg2 <- data.frame(GWS22_TD)
GWS22_TDg2 <- GWS22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_TDg2$player1
player2vector <- GWS22_TDg2$player2
GWS22_TDg3 <- GWS22_TDg2
GWS22_TDg3$p1inp2vec <- is.element(GWS22_TDg3$player1, player2vector)
GWS22_TDg3$p2inp1vec <- is.element(GWS22_TDg3$player2, player1vector)

addPlayer1 <- GWS22_TDg3[ which(GWS22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_TDg3[ which(GWS22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_TDg2 <- rbind(GWS22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
GWS22_TDft <- ftable(GWS22_TDg2$player1, GWS22_TDg2$player2)
GWS22_TDft2 <- as.matrix(GWS22_TDft)
numRows <- nrow(GWS22_TDft2)
numCols <- ncol(GWS22_TDft2)
GWS22_TDft3 <- GWS22_TDft2[c(2:numRows) , c(2:numCols)]
GWS22_TDTable <- graph.adjacency(GWS22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(GWS22_TDTable, vertex.label = V(GWS22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
GWS22_TD.clusterCoef <- transitivity(GWS22_TDTable, type="global") #cluster coefficient
GWS22_TD.degreeCent <- centralization.degree(GWS22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_TDftn <- as.network.matrix(GWS22_TDft)
GWS22_TD.netDensity <- network.density(GWS22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_TD.entropy <- entropy(GWS22_TDft) #entropy

GWS22_TD.netMx <- cbind(GWS22_TD.netMx, GWS22_TD.clusterCoef, GWS22_TD.degreeCent$centralization,
                        GWS22_TD.netDensity, GWS22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
GWS22_QTg2 <- data.frame(GWS22_QT)
GWS22_QTg2 <- GWS22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS22_QTg2$player1
player2vector <- GWS22_QTg2$player2
GWS22_QTg3 <- GWS22_QTg2
GWS22_QTg3$p1inp2vec <- is.element(GWS22_QTg3$player1, player2vector)
GWS22_QTg3$p2inp1vec <- is.element(GWS22_QTg3$player2, player1vector)

addPlayer1 <- GWS22_QTg3[ which(GWS22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS22_QTg3[ which(GWS22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS22_QTg2 <- rbind(GWS22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
GWS22_QTft <- ftable(GWS22_QTg2$player1, GWS22_QTg2$player2)
GWS22_QTft2 <- as.matrix(GWS22_QTft)
numRows <- nrow(GWS22_QTft2)
numCols <- ncol(GWS22_QTft2)
GWS22_QTft3 <- GWS22_QTft2[c(2:numRows) , c(2:numCols)]
GWS22_QTTable <- graph.adjacency(GWS22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(GWS22_QTTable, vertex.label = V(GWS22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
GWS22_QT.clusterCoef <- transitivity(GWS22_QTTable, type="global") #cluster coefficient
GWS22_QT.degreeCent <- centralization.degree(GWS22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS22_QTftn <- as.network.matrix(GWS22_QTft)
GWS22_QT.netDensity <- network.density(GWS22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS22_QT.entropy <- entropy(GWS22_QTft) #entropy

GWS22_QT.netMx <- cbind(GWS22_QT.netMx, GWS22_QT.clusterCoef, GWS22_QT.degreeCent$centralization,
                        GWS22_QT.netDensity, GWS22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS22_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************

round = 22
teamName = "HAW"
KIoutcome = "Goal_F"
HAW22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
HAW22_Gg2 <- data.frame(HAW22_G)
HAW22_Gg2 <- HAW22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_Gg2$player1
player2vector <- HAW22_Gg2$player2
HAW22_Gg3 <- HAW22_Gg2
HAW22_Gg3$p1inp2vec <- is.element(HAW22_Gg3$player1, player2vector)
HAW22_Gg3$p2inp1vec <- is.element(HAW22_Gg3$player2, player1vector)

addPlayer1 <- HAW22_Gg3[ which(HAW22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_Gg3[ which(HAW22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_Gg2 <- rbind(HAW22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
HAW22_Gft <- ftable(HAW22_Gg2$player1, HAW22_Gg2$player2)
HAW22_Gft2 <- as.matrix(HAW22_Gft)
numRows <- nrow(HAW22_Gft2)
numCols <- ncol(HAW22_Gft2)
HAW22_Gft3 <- HAW22_Gft2[c(2:numRows) , c(2:numCols)]
HAW22_GTable <- graph.adjacency(HAW22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(HAW22_GTable, vertex.label = V(HAW22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
HAW22_G.clusterCoef <- transitivity(HAW22_GTable, type="global") #cluster coefficient
HAW22_G.degreeCent <- centralization.degree(HAW22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_Gftn <- as.network.matrix(HAW22_Gft)
HAW22_G.netDensity <- network.density(HAW22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_G.entropy <- entropy(HAW22_Gft) #entropy

HAW22_G.netMx <- cbind(HAW22_G.netMx, HAW22_G.clusterCoef, HAW22_G.degreeCent$centralization,
                       HAW22_G.netDensity, HAW22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Behind_F"
HAW22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
HAW22_Bg2 <- data.frame(HAW22_B)
HAW22_Bg2 <- HAW22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_Bg2$player1
player2vector <- HAW22_Bg2$player2
HAW22_Bg3 <- HAW22_Bg2
HAW22_Bg3$p1inp2vec <- is.element(HAW22_Bg3$player1, player2vector)
HAW22_Bg3$p2inp1vec <- is.element(HAW22_Bg3$player2, player1vector)

addPlayer1 <- HAW22_Bg3[ which(HAW22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW22_Bg2 <- rbind(HAW22_Bg2, addPlayer1)

#ROUND 22, Behind graph using weighted edges
HAW22_Bft <- ftable(HAW22_Bg2$player1, HAW22_Bg2$player2)
HAW22_Bft2 <- as.matrix(HAW22_Bft)
numRows <- nrow(HAW22_Bft2)
numCols <- ncol(HAW22_Bft2)
HAW22_Bft3 <- HAW22_Bft2[c(2:numRows) , c(1:numCols)]
HAW22_BTable <- graph.adjacency(HAW22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(HAW22_BTable, vertex.label = V(HAW22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
HAW22_B.clusterCoef <- transitivity(HAW22_BTable, type="global") #cluster coefficient
HAW22_B.degreeCent <- centralization.degree(HAW22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_Bftn <- as.network.matrix(HAW22_Bft)
HAW22_B.netDensity <- network.density(HAW22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_B.entropy <- entropy(HAW22_Bft) #entropy

HAW22_B.netMx <- cbind(HAW22_B.netMx, HAW22_B.clusterCoef, HAW22_B.degreeCent$centralization,
                       HAW22_B.netDensity, HAW22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
HAW22_SFg2 <- data.frame(HAW22_SF)
HAW22_SFg2 <- HAW22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_SFg2$player1
player2vector <- HAW22_SFg2$player2
HAW22_SFg3 <- HAW22_SFg2
HAW22_SFg3$p1inp2vec <- is.element(HAW22_SFg3$player1, player2vector)
HAW22_SFg3$p2inp1vec <- is.element(HAW22_SFg3$player2, player1vector)

addPlayer1 <- HAW22_SFg3[ which(HAW22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_SFg3[ which(HAW22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_SFg2 <- rbind(HAW22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
HAW22_SFft <- ftable(HAW22_SFg2$player1, HAW22_SFg2$player2)
HAW22_SFft2 <- as.matrix(HAW22_SFft)
numRows <- nrow(HAW22_SFft2)
numCols <- ncol(HAW22_SFft2)
HAW22_SFft3 <- HAW22_SFft2[c(2:numRows) , c(2:numCols)]
HAW22_SFTable <- graph.adjacency(HAW22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(HAW22_SFTable, vertex.label = V(HAW22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
HAW22_SF.clusterCoef <- transitivity(HAW22_SFTable, type="global") #cluster coefficient
HAW22_SF.degreeCent <- centralization.degree(HAW22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_SFftn <- as.network.matrix(HAW22_SFft)
HAW22_SF.netDensity <- network.density(HAW22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_SF.entropy <- entropy(HAW22_SFft) #entropy

HAW22_SF.netMx <- cbind(HAW22_SF.netMx, HAW22_SF.clusterCoef, HAW22_SF.degreeCent$centralization,
                        HAW22_SF.netDensity, HAW22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
HAW22_TFg2 <- data.frame(HAW22_TF)
HAW22_TFg2 <- HAW22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_TFg2$player1
player2vector <- HAW22_TFg2$player2
HAW22_TFg3 <- HAW22_TFg2
HAW22_TFg3$p1inp2vec <- is.element(HAW22_TFg3$player1, player2vector)
HAW22_TFg3$p2inp1vec <- is.element(HAW22_TFg3$player2, player1vector)

addPlayer1 <- HAW22_TFg3[ which(HAW22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_TFg3[ which(HAW22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_TFg2 <- rbind(HAW22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
HAW22_TFft <- ftable(HAW22_TFg2$player1, HAW22_TFg2$player2)
HAW22_TFft2 <- as.matrix(HAW22_TFft)
numRows <- nrow(HAW22_TFft2)
numCols <- ncol(HAW22_TFft2)
HAW22_TFft3 <- HAW22_TFft2[c(2:numRows) , c(2:numCols)]
HAW22_TFTable <- graph.adjacency(HAW22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(HAW22_TFTable, vertex.label = V(HAW22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
HAW22_TF.clusterCoef <- transitivity(HAW22_TFTable, type="global") #cluster coefficient
HAW22_TF.degreeCent <- centralization.degree(HAW22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_TFftn <- as.network.matrix(HAW22_TFft)
HAW22_TF.netDensity <- network.density(HAW22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_TF.entropy <- entropy(HAW22_TFft) #entropy

HAW22_TF.netMx <- cbind(HAW22_TF.netMx, HAW22_TF.clusterCoef, HAW22_TF.degreeCent$centralization,
                        HAW22_TF.netDensity, HAW22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
HAW22_SAMg2 <- data.frame(HAW22_SAM)
HAW22_SAMg2 <- HAW22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_SAMg2$player1
player2vector <- HAW22_SAMg2$player2
HAW22_SAMg3 <- HAW22_SAMg2
HAW22_SAMg3$p1inp2vec <- is.element(HAW22_SAMg3$player1, player2vector)
HAW22_SAMg3$p2inp1vec <- is.element(HAW22_SAMg3$player2, player1vector)

addPlayer1 <- HAW22_SAMg3[ which(HAW22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_SAMg3[ which(HAW22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_SAMg2 <- rbind(HAW22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
HAW22_SAMft <- ftable(HAW22_SAMg2$player1, HAW22_SAMg2$player2)
HAW22_SAMft2 <- as.matrix(HAW22_SAMft)
numRows <- nrow(HAW22_SAMft2)
numCols <- ncol(HAW22_SAMft2)
HAW22_SAMft3 <- HAW22_SAMft2[c(2:numRows) , c(2:numCols)]
HAW22_SAMTable <- graph.adjacency(HAW22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(HAW22_SAMTable, vertex.label = V(HAW22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
HAW22_SAM.clusterCoef <- transitivity(HAW22_SAMTable, type="global") #cluster coefficient
HAW22_SAM.degreeCent <- centralization.degree(HAW22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_SAMftn <- as.network.matrix(HAW22_SAMft)
HAW22_SAM.netDensity <- network.density(HAW22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_SAM.entropy <- entropy(HAW22_SAMft) #entropy

HAW22_SAM.netMx <- cbind(HAW22_SAM.netMx, HAW22_SAM.clusterCoef, HAW22_SAM.degreeCent$centralization,
                         HAW22_SAM.netDensity, HAW22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
HAW22_TAMg2 <- data.frame(HAW22_TAM)
HAW22_TAMg2 <- HAW22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_TAMg2$player1
player2vector <- HAW22_TAMg2$player2
HAW22_TAMg3 <- HAW22_TAMg2
HAW22_TAMg3$p1inp2vec <- is.element(HAW22_TAMg3$player1, player2vector)
HAW22_TAMg3$p2inp1vec <- is.element(HAW22_TAMg3$player2, player1vector)

addPlayer1 <- HAW22_TAMg3[ which(HAW22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_TAMg3[ which(HAW22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_TAMg2 <- rbind(HAW22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
HAW22_TAMft <- ftable(HAW22_TAMg2$player1, HAW22_TAMg2$player2)
HAW22_TAMft2 <- as.matrix(HAW22_TAMft)
numRows <- nrow(HAW22_TAMft2)
numCols <- ncol(HAW22_TAMft2)
HAW22_TAMft3 <- HAW22_TAMft2[c(2:numRows) , c(2:numCols)]
HAW22_TAMTable <- graph.adjacency(HAW22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(HAW22_TAMTable, vertex.label = V(HAW22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
HAW22_TAM.clusterCoef <- transitivity(HAW22_TAMTable, type="global") #cluster coefficient
HAW22_TAM.degreeCent <- centralization.degree(HAW22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_TAMftn <- as.network.matrix(HAW22_TAMft)
HAW22_TAM.netDensity <- network.density(HAW22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_TAM.entropy <- entropy(HAW22_TAMft) #entropy

HAW22_TAM.netMx <- cbind(HAW22_TAM.netMx, HAW22_TAM.clusterCoef, HAW22_TAM.degreeCent$centralization,
                         HAW22_TAM.netDensity, HAW22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
HAW22_SDMg2 <- data.frame(HAW22_SDM)
HAW22_SDMg2 <- HAW22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_SDMg2$player1
player2vector <- HAW22_SDMg2$player2
HAW22_SDMg3 <- HAW22_SDMg2
HAW22_SDMg3$p1inp2vec <- is.element(HAW22_SDMg3$player1, player2vector)
HAW22_SDMg3$p2inp1vec <- is.element(HAW22_SDMg3$player2, player1vector)

addPlayer1 <- HAW22_SDMg3[ which(HAW22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_SDMg3[ which(HAW22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_SDMg2 <- rbind(HAW22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
HAW22_SDMft <- ftable(HAW22_SDMg2$player1, HAW22_SDMg2$player2)
HAW22_SDMft2 <- as.matrix(HAW22_SDMft)
numRows <- nrow(HAW22_SDMft2)
numCols <- ncol(HAW22_SDMft2)
HAW22_SDMft3 <- HAW22_SDMft2[c(2:numRows) , c(2:numCols)]
HAW22_SDMTable <- graph.adjacency(HAW22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(HAW22_SDMTable, vertex.label = V(HAW22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
HAW22_SDM.clusterCoef <- transitivity(HAW22_SDMTable, type="global") #cluster coefficient
HAW22_SDM.degreeCent <- centralization.degree(HAW22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_SDMftn <- as.network.matrix(HAW22_SDMft)
HAW22_SDM.netDensity <- network.density(HAW22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_SDM.entropy <- entropy(HAW22_SDMft) #entropy

HAW22_SDM.netMx <- cbind(HAW22_SDM.netMx, HAW22_SDM.clusterCoef, HAW22_SDM.degreeCent$centralization,
                         HAW22_SDM.netDensity, HAW22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
HAW22_TDMg2 <- data.frame(HAW22_TDM)
HAW22_TDMg2 <- HAW22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_TDMg2$player1
player2vector <- HAW22_TDMg2$player2
HAW22_TDMg3 <- HAW22_TDMg2
HAW22_TDMg3$p1inp2vec <- is.element(HAW22_TDMg3$player1, player2vector)
HAW22_TDMg3$p2inp1vec <- is.element(HAW22_TDMg3$player2, player1vector)

addPlayer1 <- HAW22_TDMg3[ which(HAW22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_TDMg3[ which(HAW22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_TDMg2 <- rbind(HAW22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
HAW22_TDMft <- ftable(HAW22_TDMg2$player1, HAW22_TDMg2$player2)
HAW22_TDMft2 <- as.matrix(HAW22_TDMft)
numRows <- nrow(HAW22_TDMft2)
numCols <- ncol(HAW22_TDMft2)
HAW22_TDMft3 <- HAW22_TDMft2[c(2:numRows) , c(2:numCols)]
HAW22_TDMTable <- graph.adjacency(HAW22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(HAW22_TDMTable, vertex.label = V(HAW22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
HAW22_TDM.clusterCoef <- transitivity(HAW22_TDMTable, type="global") #cluster coefficient
HAW22_TDM.degreeCent <- centralization.degree(HAW22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_TDMftn <- as.network.matrix(HAW22_TDMft)
HAW22_TDM.netDensity <- network.density(HAW22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_TDM.entropy <- entropy(HAW22_TDMft) #entropy

HAW22_TDM.netMx <- cbind(HAW22_TDM.netMx, HAW22_TDM.clusterCoef, HAW22_TDM.degreeCent$centralization,
                         HAW22_TDM.netDensity, HAW22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
HAW22_SDg2 <- data.frame(HAW22_SD)
HAW22_SDg2 <- HAW22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_SDg2$player1
player2vector <- HAW22_SDg2$player2
HAW22_SDg3 <- HAW22_SDg2
HAW22_SDg3$p1inp2vec <- is.element(HAW22_SDg3$player1, player2vector)
HAW22_SDg3$p2inp1vec <- is.element(HAW22_SDg3$player2, player1vector)

addPlayer1 <- HAW22_SDg3[ which(HAW22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_SDg3[ which(HAW22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_SDg2 <- rbind(HAW22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
HAW22_SDft <- ftable(HAW22_SDg2$player1, HAW22_SDg2$player2)
HAW22_SDft2 <- as.matrix(HAW22_SDft)
numRows <- nrow(HAW22_SDft2)
numCols <- ncol(HAW22_SDft2)
HAW22_SDft3 <- HAW22_SDft2[c(2:numRows) , c(2:numCols)]
HAW22_SDTable <- graph.adjacency(HAW22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(HAW22_SDTable, vertex.label = V(HAW22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
HAW22_SD.clusterCoef <- transitivity(HAW22_SDTable, type="global") #cluster coefficient
HAW22_SD.degreeCent <- centralization.degree(HAW22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_SDftn <- as.network.matrix(HAW22_SDft)
HAW22_SD.netDensity <- network.density(HAW22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_SD.entropy <- entropy(HAW22_SDft) #entropy

HAW22_SD.netMx <- cbind(HAW22_SD.netMx, HAW22_SD.clusterCoef, HAW22_SD.degreeCent$centralization,
                        HAW22_SD.netDensity, HAW22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
HAW22_TDg2 <- data.frame(HAW22_TD)
HAW22_TDg2 <- HAW22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_TDg2$player1
player2vector <- HAW22_TDg2$player2
HAW22_TDg3 <- HAW22_TDg2
HAW22_TDg3$p1inp2vec <- is.element(HAW22_TDg3$player1, player2vector)
HAW22_TDg3$p2inp1vec <- is.element(HAW22_TDg3$player2, player1vector)

addPlayer1 <- HAW22_TDg3[ which(HAW22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_TDg3[ which(HAW22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_TDg2 <- rbind(HAW22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
HAW22_TDft <- ftable(HAW22_TDg2$player1, HAW22_TDg2$player2)
HAW22_TDft2 <- as.matrix(HAW22_TDft)
numRows <- nrow(HAW22_TDft2)
numCols <- ncol(HAW22_TDft2)
HAW22_TDft3 <- HAW22_TDft2[c(2:numRows) , c(2:numCols)]
HAW22_TDTable <- graph.adjacency(HAW22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(HAW22_TDTable, vertex.label = V(HAW22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
HAW22_TD.clusterCoef <- transitivity(HAW22_TDTable, type="global") #cluster coefficient
HAW22_TD.degreeCent <- centralization.degree(HAW22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_TDftn <- as.network.matrix(HAW22_TDft)
HAW22_TD.netDensity <- network.density(HAW22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_TD.entropy <- entropy(HAW22_TDft) #entropy

HAW22_TD.netMx <- cbind(HAW22_TD.netMx, HAW22_TD.clusterCoef, HAW22_TD.degreeCent$centralization,
                        HAW22_TD.netDensity, HAW22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
HAW22_QTg2 <- data.frame(HAW22_QT)
HAW22_QTg2 <- HAW22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW22_QTg2$player1
player2vector <- HAW22_QTg2$player2
HAW22_QTg3 <- HAW22_QTg2
HAW22_QTg3$p1inp2vec <- is.element(HAW22_QTg3$player1, player2vector)
HAW22_QTg3$p2inp1vec <- is.element(HAW22_QTg3$player2, player1vector)

addPlayer1 <- HAW22_QTg3[ which(HAW22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW22_QTg3[ which(HAW22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW22_QTg2 <- rbind(HAW22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
HAW22_QTft <- ftable(HAW22_QTg2$player1, HAW22_QTg2$player2)
HAW22_QTft2 <- as.matrix(HAW22_QTft)
numRows <- nrow(HAW22_QTft2)
numCols <- ncol(HAW22_QTft2)
HAW22_QTft3 <- HAW22_QTft2[c(2:numRows) , c(2:numCols)]
HAW22_QTTable <- graph.adjacency(HAW22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(HAW22_QTTable, vertex.label = V(HAW22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
HAW22_QT.clusterCoef <- transitivity(HAW22_QTTable, type="global") #cluster coefficient
HAW22_QT.degreeCent <- centralization.degree(HAW22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW22_QTftn <- as.network.matrix(HAW22_QTft)
HAW22_QT.netDensity <- network.density(HAW22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW22_QT.entropy <- entropy(HAW22_QTft) #entropy

HAW22_QT.netMx <- cbind(HAW22_QT.netMx, HAW22_QT.clusterCoef, HAW22_QT.degreeCent$centralization,
                        HAW22_QT.netDensity, HAW22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW22_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Goal_F"
MELB22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
MELB22_Gg2 <- data.frame(MELB22_G)
MELB22_Gg2 <- MELB22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_Gg2$player1
player2vector <- MELB22_Gg2$player2
MELB22_Gg3 <- MELB22_Gg2
MELB22_Gg3$p1inp2vec <- is.element(MELB22_Gg3$player1, player2vector)
MELB22_Gg3$p2inp1vec <- is.element(MELB22_Gg3$player2, player1vector)

addPlayer1 <- MELB22_Gg3[ which(MELB22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB22_Gg2 <- rbind(MELB22_Gg2, addPlayer1)

#ROUND 22, Goal graph using weighted edges
MELB22_Gft <- ftable(MELB22_Gg2$player1, MELB22_Gg2$player2)
MELB22_Gft2 <- as.matrix(MELB22_Gft)
numRows <- nrow(MELB22_Gft2)
numCols <- ncol(MELB22_Gft2)
MELB22_Gft3 <- MELB22_Gft2[c(2:numRows) , c(1:numCols)]
MELB22_GTable <- graph.adjacency(MELB22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(MELB22_GTable, vertex.label = V(MELB22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
MELB22_G.clusterCoef <- transitivity(MELB22_GTable, type="global") #cluster coefficient
MELB22_G.degreeCent <- centralization.degree(MELB22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_Gftn <- as.network.matrix(MELB22_Gft)
MELB22_G.netDensity <- network.density(MELB22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_G.entropy <- entropy(MELB22_Gft) #entropy

MELB22_G.netMx <- cbind(MELB22_G.netMx, MELB22_G.clusterCoef, MELB22_G.degreeCent$centralization,
                        MELB22_G.netDensity, MELB22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Behind_F"
MELB22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
MELB22_Bg2 <- data.frame(MELB22_B)
MELB22_Bg2 <- MELB22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_Bg2$player1
player2vector <- MELB22_Bg2$player2
MELB22_Bg3 <- MELB22_Bg2
MELB22_Bg3$p1inp2vec <- is.element(MELB22_Bg3$player1, player2vector)
MELB22_Bg3$p2inp1vec <- is.element(MELB22_Bg3$player2, player1vector)

addPlayer1 <- MELB22_Bg3[ which(MELB22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_Bg3[ which(MELB22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_Bg2 <- rbind(MELB22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
MELB22_Bft <- ftable(MELB22_Bg2$player1, MELB22_Bg2$player2)
MELB22_Bft2 <- as.matrix(MELB22_Bft)
numRows <- nrow(MELB22_Bft2)
numCols <- ncol(MELB22_Bft2)
MELB22_Bft3 <- MELB22_Bft2[c(2:numRows) , c(2:numCols)]
MELB22_BTable <- graph.adjacency(MELB22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(MELB22_BTable, vertex.label = V(MELB22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
MELB22_B.clusterCoef <- transitivity(MELB22_BTable, type="global") #cluster coefficient
MELB22_B.degreeCent <- centralization.degree(MELB22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_Bftn <- as.network.matrix(MELB22_Bft)
MELB22_B.netDensity <- network.density(MELB22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_B.entropy <- entropy(MELB22_Bft) #entropy

MELB22_B.netMx <- cbind(MELB22_B.netMx, MELB22_B.clusterCoef, MELB22_B.degreeCent$centralization,
                        MELB22_B.netDensity, MELB22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************

round = 22
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
MELB22_SFg2 <- data.frame(MELB22_SF)
MELB22_SFg2 <- MELB22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_SFg2$player1
player2vector <- MELB22_SFg2$player2
MELB22_SFg3 <- MELB22_SFg2
MELB22_SFg3$p1inp2vec <- is.element(MELB22_SFg3$player1, player2vector)
MELB22_SFg3$p2inp1vec <- is.element(MELB22_SFg3$player2, player1vector)

addPlayer1 <- MELB22_SFg3[ which(MELB22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_SFg3[ which(MELB22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_SFg2 <- rbind(MELB22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
MELB22_SFft <- ftable(MELB22_SFg2$player1, MELB22_SFg2$player2)
MELB22_SFft2 <- as.matrix(MELB22_SFft)
numRows <- nrow(MELB22_SFft2)
numCols <- ncol(MELB22_SFft2)
MELB22_SFft3 <- MELB22_SFft2[c(2:numRows) , c(2:numCols)]
MELB22_SFTable <- graph.adjacency(MELB22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(MELB22_SFTable, vertex.label = V(MELB22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
MELB22_SF.clusterCoef <- transitivity(MELB22_SFTable, type="global") #cluster coefficient
MELB22_SF.degreeCent <- centralization.degree(MELB22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_SFftn <- as.network.matrix(MELB22_SFft)
MELB22_SF.netDensity <- network.density(MELB22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_SF.entropy <- entropy(MELB22_SFft) #entropy

MELB22_SF.netMx <- cbind(MELB22_SF.netMx, MELB22_SF.clusterCoef, MELB22_SF.degreeCent$centralization,
                         MELB22_SF.netDensity, MELB22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
MELB22_TFg2 <- data.frame(MELB22_TF)
MELB22_TFg2 <- MELB22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_TFg2$player1
player2vector <- MELB22_TFg2$player2
MELB22_TFg3 <- MELB22_TFg2
MELB22_TFg3$p1inp2vec <- is.element(MELB22_TFg3$player1, player2vector)
MELB22_TFg3$p2inp1vec <- is.element(MELB22_TFg3$player2, player1vector)

addPlayer1 <- MELB22_TFg3[ which(MELB22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_TFg3[ which(MELB22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_TFg2 <- rbind(MELB22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
MELB22_TFft <- ftable(MELB22_TFg2$player1, MELB22_TFg2$player2)
MELB22_TFft2 <- as.matrix(MELB22_TFft)
numRows <- nrow(MELB22_TFft2)
numCols <- ncol(MELB22_TFft2)
MELB22_TFft3 <- MELB22_TFft2[c(2:numRows) , c(2:numCols)]
MELB22_TFTable <- graph.adjacency(MELB22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(MELB22_TFTable, vertex.label = V(MELB22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
MELB22_TF.clusterCoef <- transitivity(MELB22_TFTable, type="global") #cluster coefficient
MELB22_TF.degreeCent <- centralization.degree(MELB22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_TFftn <- as.network.matrix(MELB22_TFft)
MELB22_TF.netDensity <- network.density(MELB22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_TF.entropy <- entropy(MELB22_TFft) #entropy

MELB22_TF.netMx <- cbind(MELB22_TF.netMx, MELB22_TF.clusterCoef, MELB22_TF.degreeCent$centralization,
                         MELB22_TF.netDensity, MELB22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
MELB22_SAMg2 <- data.frame(MELB22_SAM)
MELB22_SAMg2 <- MELB22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_SAMg2$player1
player2vector <- MELB22_SAMg2$player2
MELB22_SAMg3 <- MELB22_SAMg2
MELB22_SAMg3$p1inp2vec <- is.element(MELB22_SAMg3$player1, player2vector)
MELB22_SAMg3$p2inp1vec <- is.element(MELB22_SAMg3$player2, player1vector)

addPlayer1 <- MELB22_SAMg3[ which(MELB22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_SAMg3[ which(MELB22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_SAMg2 <- rbind(MELB22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
MELB22_SAMft <- ftable(MELB22_SAMg2$player1, MELB22_SAMg2$player2)
MELB22_SAMft2 <- as.matrix(MELB22_SAMft)
numRows <- nrow(MELB22_SAMft2)
numCols <- ncol(MELB22_SAMft2)
MELB22_SAMft3 <- MELB22_SAMft2[c(2:numRows) , c(2:numCols)]
MELB22_SAMTable <- graph.adjacency(MELB22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(MELB22_SAMTable, vertex.label = V(MELB22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
MELB22_SAM.clusterCoef <- transitivity(MELB22_SAMTable, type="global") #cluster coefficient
MELB22_SAM.degreeCent <- centralization.degree(MELB22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_SAMftn <- as.network.matrix(MELB22_SAMft)
MELB22_SAM.netDensity <- network.density(MELB22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_SAM.entropy <- entropy(MELB22_SAMft) #entropy

MELB22_SAM.netMx <- cbind(MELB22_SAM.netMx, MELB22_SAM.clusterCoef, MELB22_SAM.degreeCent$centralization,
                          MELB22_SAM.netDensity, MELB22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
MELB22_TAMg2 <- data.frame(MELB22_TAM)
MELB22_TAMg2 <- MELB22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_TAMg2$player1
player2vector <- MELB22_TAMg2$player2
MELB22_TAMg3 <- MELB22_TAMg2
MELB22_TAMg3$p1inp2vec <- is.element(MELB22_TAMg3$player1, player2vector)
MELB22_TAMg3$p2inp1vec <- is.element(MELB22_TAMg3$player2, player1vector)

addPlayer1 <- MELB22_TAMg3[ which(MELB22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_TAMg3[ which(MELB22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_TAMg2 <- rbind(MELB22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
MELB22_TAMft <- ftable(MELB22_TAMg2$player1, MELB22_TAMg2$player2)
MELB22_TAMft2 <- as.matrix(MELB22_TAMft)
numRows <- nrow(MELB22_TAMft2)
numCols <- ncol(MELB22_TAMft2)
MELB22_TAMft3 <- MELB22_TAMft2[c(2:numRows) , c(2:numCols)]
MELB22_TAMTable <- graph.adjacency(MELB22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(MELB22_TAMTable, vertex.label = V(MELB22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
MELB22_TAM.clusterCoef <- transitivity(MELB22_TAMTable, type="global") #cluster coefficient
MELB22_TAM.degreeCent <- centralization.degree(MELB22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_TAMftn <- as.network.matrix(MELB22_TAMft)
MELB22_TAM.netDensity <- network.density(MELB22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_TAM.entropy <- entropy(MELB22_TAMft) #entropy

MELB22_TAM.netMx <- cbind(MELB22_TAM.netMx, MELB22_TAM.clusterCoef, MELB22_TAM.degreeCent$centralization,
                          MELB22_TAM.netDensity, MELB22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
MELB22_SDMg2 <- data.frame(MELB22_SDM)
MELB22_SDMg2 <- MELB22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_SDMg2$player1
player2vector <- MELB22_SDMg2$player2
MELB22_SDMg3 <- MELB22_SDMg2
MELB22_SDMg3$p1inp2vec <- is.element(MELB22_SDMg3$player1, player2vector)
MELB22_SDMg3$p2inp1vec <- is.element(MELB22_SDMg3$player2, player1vector)

addPlayer1 <- MELB22_SDMg3[ which(MELB22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_SDMg3[ which(MELB22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_SDMg2 <- rbind(MELB22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
MELB22_SDMft <- ftable(MELB22_SDMg2$player1, MELB22_SDMg2$player2)
MELB22_SDMft2 <- as.matrix(MELB22_SDMft)
numRows <- nrow(MELB22_SDMft2)
numCols <- ncol(MELB22_SDMft2)
MELB22_SDMft3 <- MELB22_SDMft2[c(2:numRows) , c(2:numCols)]
MELB22_SDMTable <- graph.adjacency(MELB22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(MELB22_SDMTable, vertex.label = V(MELB22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
MELB22_SDM.clusterCoef <- transitivity(MELB22_SDMTable, type="global") #cluster coefficient
MELB22_SDM.degreeCent <- centralization.degree(MELB22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_SDMftn <- as.network.matrix(MELB22_SDMft)
MELB22_SDM.netDensity <- network.density(MELB22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_SDM.entropy <- entropy(MELB22_SDMft) #entropy

MELB22_SDM.netMx <- cbind(MELB22_SDM.netMx, MELB22_SDM.clusterCoef, MELB22_SDM.degreeCent$centralization,
                          MELB22_SDM.netDensity, MELB22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
MELB22_TDMg2 <- data.frame(MELB22_TDM)
MELB22_TDMg2 <- MELB22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_TDMg2$player1
player2vector <- MELB22_TDMg2$player2
MELB22_TDMg3 <- MELB22_TDMg2
MELB22_TDMg3$p1inp2vec <- is.element(MELB22_TDMg3$player1, player2vector)
MELB22_TDMg3$p2inp1vec <- is.element(MELB22_TDMg3$player2, player1vector)

addPlayer1 <- MELB22_TDMg3[ which(MELB22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_TDMg3[ which(MELB22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_TDMg2 <- rbind(MELB22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
MELB22_TDMft <- ftable(MELB22_TDMg2$player1, MELB22_TDMg2$player2)
MELB22_TDMft2 <- as.matrix(MELB22_TDMft)
numRows <- nrow(MELB22_TDMft2)
numCols <- ncol(MELB22_TDMft2)
MELB22_TDMft3 <- MELB22_TDMft2[c(2:numRows) , c(2:numCols)]
MELB22_TDMTable <- graph.adjacency(MELB22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(MELB22_TDMTable, vertex.label = V(MELB22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
MELB22_TDM.clusterCoef <- transitivity(MELB22_TDMTable, type="global") #cluster coefficient
MELB22_TDM.degreeCent <- centralization.degree(MELB22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_TDMftn <- as.network.matrix(MELB22_TDMft)
MELB22_TDM.netDensity <- network.density(MELB22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_TDM.entropy <- entropy(MELB22_TDMft) #entropy

MELB22_TDM.netMx <- cbind(MELB22_TDM.netMx, MELB22_TDM.clusterCoef, MELB22_TDM.degreeCent$centralization,
                          MELB22_TDM.netDensity, MELB22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
MELB22_SDg2 <- data.frame(MELB22_SD)
MELB22_SDg2 <- MELB22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_SDg2$player1
player2vector <- MELB22_SDg2$player2
MELB22_SDg3 <- MELB22_SDg2
MELB22_SDg3$p1inp2vec <- is.element(MELB22_SDg3$player1, player2vector)
MELB22_SDg3$p2inp1vec <- is.element(MELB22_SDg3$player2, player1vector)

addPlayer1 <- MELB22_SDg3[ which(MELB22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_SDg3[ which(MELB22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_SDg2 <- rbind(MELB22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
MELB22_SDft <- ftable(MELB22_SDg2$player1, MELB22_SDg2$player2)
MELB22_SDft2 <- as.matrix(MELB22_SDft)
numRows <- nrow(MELB22_SDft2)
numCols <- ncol(MELB22_SDft2)
MELB22_SDft3 <- MELB22_SDft2[c(2:numRows) , c(2:numCols)]
MELB22_SDTable <- graph.adjacency(MELB22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(MELB22_SDTable, vertex.label = V(MELB22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
MELB22_SD.clusterCoef <- transitivity(MELB22_SDTable, type="global") #cluster coefficient
MELB22_SD.degreeCent <- centralization.degree(MELB22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_SDftn <- as.network.matrix(MELB22_SDft)
MELB22_SD.netDensity <- network.density(MELB22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_SD.entropy <- entropy(MELB22_SDft) #entropy

MELB22_SD.netMx <- cbind(MELB22_SD.netMx, MELB22_SD.clusterCoef, MELB22_SD.degreeCent$centralization,
                         MELB22_SD.netDensity, MELB22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
MELB22_TDg2 <- data.frame(MELB22_TD)
MELB22_TDg2 <- MELB22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_TDg2$player1
player2vector <- MELB22_TDg2$player2
MELB22_TDg3 <- MELB22_TDg2
MELB22_TDg3$p1inp2vec <- is.element(MELB22_TDg3$player1, player2vector)
MELB22_TDg3$p2inp1vec <- is.element(MELB22_TDg3$player2, player1vector)

addPlayer1 <- MELB22_TDg3[ which(MELB22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_TDg3[ which(MELB22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_TDg2 <- rbind(MELB22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
MELB22_TDft <- ftable(MELB22_TDg2$player1, MELB22_TDg2$player2)
MELB22_TDft2 <- as.matrix(MELB22_TDft)
numRows <- nrow(MELB22_TDft2)
numCols <- ncol(MELB22_TDft2)
MELB22_TDft3 <- MELB22_TDft2[c(2:numRows) , c(2:numCols)]
MELB22_TDTable <- graph.adjacency(MELB22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(MELB22_TDTable, vertex.label = V(MELB22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
MELB22_TD.clusterCoef <- transitivity(MELB22_TDTable, type="global") #cluster coefficient
MELB22_TD.degreeCent <- centralization.degree(MELB22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_TDftn <- as.network.matrix(MELB22_TDft)
MELB22_TD.netDensity <- network.density(MELB22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_TD.entropy <- entropy(MELB22_TDft) #entropy

MELB22_TD.netMx <- cbind(MELB22_TD.netMx, MELB22_TD.clusterCoef, MELB22_TD.degreeCent$centralization,
                         MELB22_TD.netDensity, MELB22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
MELB22_QTg2 <- data.frame(MELB22_QT)
MELB22_QTg2 <- MELB22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB22_QTg2$player1
player2vector <- MELB22_QTg2$player2
MELB22_QTg3 <- MELB22_QTg2
MELB22_QTg3$p1inp2vec <- is.element(MELB22_QTg3$player1, player2vector)
MELB22_QTg3$p2inp1vec <- is.element(MELB22_QTg3$player2, player1vector)

addPlayer1 <- MELB22_QTg3[ which(MELB22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB22_QTg3[ which(MELB22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB22_QTg2 <- rbind(MELB22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
MELB22_QTft <- ftable(MELB22_QTg2$player1, MELB22_QTg2$player2)
MELB22_QTft2 <- as.matrix(MELB22_QTft)
numRows <- nrow(MELB22_QTft2)
numCols <- ncol(MELB22_QTft2)
MELB22_QTft3 <- MELB22_QTft2[c(2:numRows) , c(2:numCols)]
MELB22_QTTable <- graph.adjacency(MELB22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(MELB22_QTTable, vertex.label = V(MELB22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
MELB22_QT.clusterCoef <- transitivity(MELB22_QTTable, type="global") #cluster coefficient
MELB22_QT.degreeCent <- centralization.degree(MELB22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB22_QTftn <- as.network.matrix(MELB22_QTft)
MELB22_QT.netDensity <- network.density(MELB22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB22_QT.entropy <- entropy(MELB22_QTft) #entropy

MELB22_QT.netMx <- cbind(MELB22_QT.netMx, MELB22_QT.clusterCoef, MELB22_QT.degreeCent$centralization,
                         MELB22_QT.netDensity, MELB22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB22_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
NMFC22_Gg2 <- data.frame(NMFC22_G)
NMFC22_Gg2 <- NMFC22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_Gg2$player1
player2vector <- NMFC22_Gg2$player2
NMFC22_Gg3 <- NMFC22_Gg2
NMFC22_Gg3$p1inp2vec <- is.element(NMFC22_Gg3$player1, player2vector)
NMFC22_Gg3$p2inp1vec <- is.element(NMFC22_Gg3$player2, player1vector)

addPlayer1 <- NMFC22_Gg3[ which(NMFC22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_Gg3[ which(NMFC22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_Gg2 <- rbind(NMFC22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
NMFC22_Gft <- ftable(NMFC22_Gg2$player1, NMFC22_Gg2$player2)
NMFC22_Gft2 <- as.matrix(NMFC22_Gft)
numRows <- nrow(NMFC22_Gft2)
numCols <- ncol(NMFC22_Gft2)
NMFC22_Gft3 <- NMFC22_Gft2[c(2:numRows) , c(2:numCols)]
NMFC22_GTable <- graph.adjacency(NMFC22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(NMFC22_GTable, vertex.label = V(NMFC22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
NMFC22_G.clusterCoef <- transitivity(NMFC22_GTable, type="global") #cluster coefficient
NMFC22_G.degreeCent <- centralization.degree(NMFC22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_Gftn <- as.network.matrix(NMFC22_Gft)
NMFC22_G.netDensity <- network.density(NMFC22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_G.entropy <- entropy(NMFC22_Gft) #entropy

NMFC22_G.netMx <- cbind(NMFC22_G.netMx, NMFC22_G.clusterCoef, NMFC22_G.degreeCent$centralization,
                        NMFC22_G.netDensity, NMFC22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
NMFC22_Bg2 <- data.frame(NMFC22_B)
NMFC22_Bg2 <- NMFC22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_Bg2$player1
player2vector <- NMFC22_Bg2$player2
NMFC22_Bg3 <- NMFC22_Bg2
NMFC22_Bg3$p1inp2vec <- is.element(NMFC22_Bg3$player1, player2vector)
NMFC22_Bg3$p2inp1vec <- is.element(NMFC22_Bg3$player2, player1vector)

addPlayer1 <- NMFC22_Bg3[ which(NMFC22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_Bg3[ which(NMFC22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_Bg2 <- rbind(NMFC22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
NMFC22_Bft <- ftable(NMFC22_Bg2$player1, NMFC22_Bg2$player2)
NMFC22_Bft2 <- as.matrix(NMFC22_Bft)
numRows <- nrow(NMFC22_Bft2)
numCols <- ncol(NMFC22_Bft2)
NMFC22_Bft3 <- NMFC22_Bft2[c(2:numRows) , c(2:numCols)]
NMFC22_BTable <- graph.adjacency(NMFC22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(NMFC22_BTable, vertex.label = V(NMFC22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
NMFC22_B.clusterCoef <- transitivity(NMFC22_BTable, type="global") #cluster coefficient
NMFC22_B.degreeCent <- centralization.degree(NMFC22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_Bftn <- as.network.matrix(NMFC22_Bft)
NMFC22_B.netDensity <- network.density(NMFC22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_B.entropy <- entropy(NMFC22_Bft) #entropy

NMFC22_B.netMx <- cbind(NMFC22_B.netMx, NMFC22_B.clusterCoef, NMFC22_B.degreeCent$centralization,
                        NMFC22_B.netDensity, NMFC22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************

round = 22
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
NMFC22_SFg2 <- data.frame(NMFC22_SF)
NMFC22_SFg2 <- NMFC22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_SFg2$player1
player2vector <- NMFC22_SFg2$player2
NMFC22_SFg3 <- NMFC22_SFg2
NMFC22_SFg3$p1inp2vec <- is.element(NMFC22_SFg3$player1, player2vector)
NMFC22_SFg3$p2inp1vec <- is.element(NMFC22_SFg3$player2, player1vector)

addPlayer1 <- NMFC22_SFg3[ which(NMFC22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_SFg3[ which(NMFC22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_SFg2 <- rbind(NMFC22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
NMFC22_SFft <- ftable(NMFC22_SFg2$player1, NMFC22_SFg2$player2)
NMFC22_SFft2 <- as.matrix(NMFC22_SFft)
numRows <- nrow(NMFC22_SFft2)
numCols <- ncol(NMFC22_SFft2)
NMFC22_SFft3 <- NMFC22_SFft2[c(2:numRows) , c(2:numCols)]
NMFC22_SFTable <- graph.adjacency(NMFC22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(NMFC22_SFTable, vertex.label = V(NMFC22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
NMFC22_SF.clusterCoef <- transitivity(NMFC22_SFTable, type="global") #cluster coefficient
NMFC22_SF.degreeCent <- centralization.degree(NMFC22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_SFftn <- as.network.matrix(NMFC22_SFft)
NMFC22_SF.netDensity <- network.density(NMFC22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_SF.entropy <- entropy(NMFC22_SFft) #entropy

NMFC22_SF.netMx <- cbind(NMFC22_SF.netMx, NMFC22_SF.clusterCoef, NMFC22_SF.degreeCent$centralization,
                         NMFC22_SF.netDensity, NMFC22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
NMFC22_TFg2 <- data.frame(NMFC22_TF)
NMFC22_TFg2 <- NMFC22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_TFg2$player1
player2vector <- NMFC22_TFg2$player2
NMFC22_TFg3 <- NMFC22_TFg2
NMFC22_TFg3$p1inp2vec <- is.element(NMFC22_TFg3$player1, player2vector)
NMFC22_TFg3$p2inp1vec <- is.element(NMFC22_TFg3$player2, player1vector)

addPlayer1 <- NMFC22_TFg3[ which(NMFC22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_TFg3[ which(NMFC22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_TFg2 <- rbind(NMFC22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
NMFC22_TFft <- ftable(NMFC22_TFg2$player1, NMFC22_TFg2$player2)
NMFC22_TFft2 <- as.matrix(NMFC22_TFft)
numRows <- nrow(NMFC22_TFft2)
numCols <- ncol(NMFC22_TFft2)
NMFC22_TFft3 <- NMFC22_TFft2[c(2:numRows) , c(2:numCols)]
NMFC22_TFTable <- graph.adjacency(NMFC22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(NMFC22_TFTable, vertex.label = V(NMFC22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
NMFC22_TF.clusterCoef <- transitivity(NMFC22_TFTable, type="global") #cluster coefficient
NMFC22_TF.degreeCent <- centralization.degree(NMFC22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_TFftn <- as.network.matrix(NMFC22_TFft)
NMFC22_TF.netDensity <- network.density(NMFC22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_TF.entropy <- entropy(NMFC22_TFft) #entropy

NMFC22_TF.netMx <- cbind(NMFC22_TF.netMx, NMFC22_TF.clusterCoef, NMFC22_TF.degreeCent$centralization,
                         NMFC22_TF.netDensity, NMFC22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
NMFC22_SAMg2 <- data.frame(NMFC22_SAM)
NMFC22_SAMg2 <- NMFC22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_SAMg2$player1
player2vector <- NMFC22_SAMg2$player2
NMFC22_SAMg3 <- NMFC22_SAMg2
NMFC22_SAMg3$p1inp2vec <- is.element(NMFC22_SAMg3$player1, player2vector)
NMFC22_SAMg3$p2inp1vec <- is.element(NMFC22_SAMg3$player2, player1vector)

addPlayer1 <- NMFC22_SAMg3[ which(NMFC22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC22_SAMg3[ which(NMFC22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_SAMg2 <- rbind(NMFC22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
NMFC22_SAMft <- ftable(NMFC22_SAMg2$player1, NMFC22_SAMg2$player2)
NMFC22_SAMft2 <- as.matrix(NMFC22_SAMft)
numRows <- nrow(NMFC22_SAMft2)
numCols <- ncol(NMFC22_SAMft2)
NMFC22_SAMft3 <- NMFC22_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC22_SAMTable <- graph.adjacency(NMFC22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(NMFC22_SAMTable, vertex.label = V(NMFC22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
NMFC22_SAM.clusterCoef <- transitivity(NMFC22_SAMTable, type="global") #cluster coefficient
NMFC22_SAM.degreeCent <- centralization.degree(NMFC22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_SAMftn <- as.network.matrix(NMFC22_SAMft)
NMFC22_SAM.netDensity <- network.density(NMFC22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_SAM.entropy <- entropy(NMFC22_SAMft) #entropy

NMFC22_SAM.netMx <- cbind(NMFC22_SAM.netMx, NMFC22_SAM.clusterCoef, NMFC22_SAM.degreeCent$centralization,
                          NMFC22_SAM.netDensity, NMFC22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************
#NA

round = 22
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
NMFC22_TAMg2 <- data.frame(NMFC22_TAM)
NMFC22_TAMg2 <- NMFC22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_TAMg2$player1
player2vector <- NMFC22_TAMg2$player2
NMFC22_TAMg3 <- NMFC22_TAMg2
NMFC22_TAMg3$p1inp2vec <- is.element(NMFC22_TAMg3$player1, player2vector)
NMFC22_TAMg3$p2inp1vec <- is.element(NMFC22_TAMg3$player2, player1vector)

addPlayer1 <- NMFC22_TAMg3[ which(NMFC22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_TAMg3[ which(NMFC22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_TAMg2 <- rbind(NMFC22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
NMFC22_TAMft <- ftable(NMFC22_TAMg2$player1, NMFC22_TAMg2$player2)
NMFC22_TAMft2 <- as.matrix(NMFC22_TAMft)
numRows <- nrow(NMFC22_TAMft2)
numCols <- ncol(NMFC22_TAMft2)
NMFC22_TAMft3 <- NMFC22_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC22_TAMTable <- graph.adjacency(NMFC22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(NMFC22_TAMTable, vertex.label = V(NMFC22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
NMFC22_TAM.clusterCoef <- transitivity(NMFC22_TAMTable, type="global") #cluster coefficient
NMFC22_TAM.degreeCent <- centralization.degree(NMFC22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_TAMftn <- as.network.matrix(NMFC22_TAMft)
NMFC22_TAM.netDensity <- network.density(NMFC22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_TAM.entropy <- entropy(NMFC22_TAMft) #entropy

NMFC22_TAM.netMx <- cbind(NMFC22_TAM.netMx, NMFC22_TAM.clusterCoef, NMFC22_TAM.degreeCent$centralization,
                          NMFC22_TAM.netDensity, NMFC22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
NMFC22_SDMg2 <- data.frame(NMFC22_SDM)
NMFC22_SDMg2 <- NMFC22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_SDMg2$player1
player2vector <- NMFC22_SDMg2$player2
NMFC22_SDMg3 <- NMFC22_SDMg2
NMFC22_SDMg3$p1inp2vec <- is.element(NMFC22_SDMg3$player1, player2vector)
NMFC22_SDMg3$p2inp1vec <- is.element(NMFC22_SDMg3$player2, player1vector)

addPlayer1 <- NMFC22_SDMg3[ which(NMFC22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_SDMg3[ which(NMFC22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_SDMg2 <- rbind(NMFC22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
NMFC22_SDMft <- ftable(NMFC22_SDMg2$player1, NMFC22_SDMg2$player2)
NMFC22_SDMft2 <- as.matrix(NMFC22_SDMft)
numRows <- nrow(NMFC22_SDMft2)
numCols <- ncol(NMFC22_SDMft2)
NMFC22_SDMft3 <- NMFC22_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC22_SDMTable <- graph.adjacency(NMFC22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(NMFC22_SDMTable, vertex.label = V(NMFC22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
NMFC22_SDM.clusterCoef <- transitivity(NMFC22_SDMTable, type="global") #cluster coefficient
NMFC22_SDM.degreeCent <- centralization.degree(NMFC22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_SDMftn <- as.network.matrix(NMFC22_SDMft)
NMFC22_SDM.netDensity <- network.density(NMFC22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_SDM.entropy <- entropy(NMFC22_SDMft) #entropy

NMFC22_SDM.netMx <- cbind(NMFC22_SDM.netMx, NMFC22_SDM.clusterCoef, NMFC22_SDM.degreeCent$centralization,
                          NMFC22_SDM.netDensity, NMFC22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
NMFC22_TDMg2 <- data.frame(NMFC22_TDM)
NMFC22_TDMg2 <- NMFC22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_TDMg2$player1
player2vector <- NMFC22_TDMg2$player2
NMFC22_TDMg3 <- NMFC22_TDMg2
NMFC22_TDMg3$p1inp2vec <- is.element(NMFC22_TDMg3$player1, player2vector)
NMFC22_TDMg3$p2inp1vec <- is.element(NMFC22_TDMg3$player2, player1vector)

addPlayer1 <- NMFC22_TDMg3[ which(NMFC22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_TDMg3[ which(NMFC22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_TDMg2 <- rbind(NMFC22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
NMFC22_TDMft <- ftable(NMFC22_TDMg2$player1, NMFC22_TDMg2$player2)
NMFC22_TDMft2 <- as.matrix(NMFC22_TDMft)
numRows <- nrow(NMFC22_TDMft2)
numCols <- ncol(NMFC22_TDMft2)
NMFC22_TDMft3 <- NMFC22_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC22_TDMTable <- graph.adjacency(NMFC22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(NMFC22_TDMTable, vertex.label = V(NMFC22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
NMFC22_TDM.clusterCoef <- transitivity(NMFC22_TDMTable, type="global") #cluster coefficient
NMFC22_TDM.degreeCent <- centralization.degree(NMFC22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_TDMftn <- as.network.matrix(NMFC22_TDMft)
NMFC22_TDM.netDensity <- network.density(NMFC22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_TDM.entropy <- entropy(NMFC22_TDMft) #entropy

NMFC22_TDM.netMx <- cbind(NMFC22_TDM.netMx, NMFC22_TDM.clusterCoef, NMFC22_TDM.degreeCent$centralization,
                          NMFC22_TDM.netDensity, NMFC22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
NMFC22_SDg2 <- data.frame(NMFC22_SD)
NMFC22_SDg2 <- NMFC22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_SDg2$player1
player2vector <- NMFC22_SDg2$player2
NMFC22_SDg3 <- NMFC22_SDg2
NMFC22_SDg3$p1inp2vec <- is.element(NMFC22_SDg3$player1, player2vector)
NMFC22_SDg3$p2inp1vec <- is.element(NMFC22_SDg3$player2, player1vector)

addPlayer1 <- NMFC22_SDg3[ which(NMFC22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_SDg3[ which(NMFC22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_SDg2 <- rbind(NMFC22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
NMFC22_SDft <- ftable(NMFC22_SDg2$player1, NMFC22_SDg2$player2)
NMFC22_SDft2 <- as.matrix(NMFC22_SDft)
numRows <- nrow(NMFC22_SDft2)
numCols <- ncol(NMFC22_SDft2)
NMFC22_SDft3 <- NMFC22_SDft2[c(2:numRows) , c(2:numCols)]
NMFC22_SDTable <- graph.adjacency(NMFC22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(NMFC22_SDTable, vertex.label = V(NMFC22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
NMFC22_SD.clusterCoef <- transitivity(NMFC22_SDTable, type="global") #cluster coefficient
NMFC22_SD.degreeCent <- centralization.degree(NMFC22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_SDftn <- as.network.matrix(NMFC22_SDft)
NMFC22_SD.netDensity <- network.density(NMFC22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_SD.entropy <- entropy(NMFC22_SDft) #entropy

NMFC22_SD.netMx <- cbind(NMFC22_SD.netMx, NMFC22_SD.clusterCoef, NMFC22_SD.degreeCent$centralization,
                         NMFC22_SD.netDensity, NMFC22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************

round = 22
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
NMFC22_TDg2 <- data.frame(NMFC22_TD)
NMFC22_TDg2 <- NMFC22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_TDg2$player1
player2vector <- NMFC22_TDg2$player2
NMFC22_TDg3 <- NMFC22_TDg2
NMFC22_TDg3$p1inp2vec <- is.element(NMFC22_TDg3$player1, player2vector)
NMFC22_TDg3$p2inp1vec <- is.element(NMFC22_TDg3$player2, player1vector)

addPlayer1 <- NMFC22_TDg3[ which(NMFC22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_TDg3[ which(NMFC22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_TDg2 <- rbind(NMFC22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
NMFC22_TDft <- ftable(NMFC22_TDg2$player1, NMFC22_TDg2$player2)
NMFC22_TDft2 <- as.matrix(NMFC22_TDft)
numRows <- nrow(NMFC22_TDft2)
numCols <- ncol(NMFC22_TDft2)
NMFC22_TDft3 <- NMFC22_TDft2[c(2:numRows) , c(2:numCols)]
NMFC22_TDTable <- graph.adjacency(NMFC22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(NMFC22_TDTable, vertex.label = V(NMFC22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
NMFC22_TD.clusterCoef <- transitivity(NMFC22_TDTable, type="global") #cluster coefficient
NMFC22_TD.degreeCent <- centralization.degree(NMFC22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_TDftn <- as.network.matrix(NMFC22_TDft)
NMFC22_TD.netDensity <- network.density(NMFC22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_TD.entropy <- entropy(NMFC22_TDft) #entropy

NMFC22_TD.netMx <- cbind(NMFC22_TD.netMx, NMFC22_TD.clusterCoef, NMFC22_TD.degreeCent$centralization,
                         NMFC22_TD.netDensity, NMFC22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
NMFC22_QTg2 <- data.frame(NMFC22_QT)
NMFC22_QTg2 <- NMFC22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC22_QTg2$player1
player2vector <- NMFC22_QTg2$player2
NMFC22_QTg3 <- NMFC22_QTg2
NMFC22_QTg3$p1inp2vec <- is.element(NMFC22_QTg3$player1, player2vector)
NMFC22_QTg3$p2inp1vec <- is.element(NMFC22_QTg3$player2, player1vector)

addPlayer1 <- NMFC22_QTg3[ which(NMFC22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC22_QTg3[ which(NMFC22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC22_QTg2 <- rbind(NMFC22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
NMFC22_QTft <- ftable(NMFC22_QTg2$player1, NMFC22_QTg2$player2)
NMFC22_QTft2 <- as.matrix(NMFC22_QTft)
numRows <- nrow(NMFC22_QTft2)
numCols <- ncol(NMFC22_QTft2)
NMFC22_QTft3 <- NMFC22_QTft2[c(2:numRows) , c(2:numCols)]
NMFC22_QTTable <- graph.adjacency(NMFC22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(NMFC22_QTTable, vertex.label = V(NMFC22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
NMFC22_QT.clusterCoef <- transitivity(NMFC22_QTTable, type="global") #cluster coefficient
NMFC22_QT.degreeCent <- centralization.degree(NMFC22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC22_QTftn <- as.network.matrix(NMFC22_QTft)
NMFC22_QT.netDensity <- network.density(NMFC22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC22_QT.entropy <- entropy(NMFC22_QTft) #entropy

NMFC22_QT.netMx <- cbind(NMFC22_QT.netMx, NMFC22_QT.clusterCoef, NMFC22_QT.degreeCent$centralization,
                         NMFC22_QT.netDensity, NMFC22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC22_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Goal_F"
PORT22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
PORT22_Gg2 <- data.frame(PORT22_G)
PORT22_Gg2 <- PORT22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_Gg2$player1
player2vector <- PORT22_Gg2$player2
PORT22_Gg3 <- PORT22_Gg2
PORT22_Gg3$p1inp2vec <- is.element(PORT22_Gg3$player1, player2vector)
PORT22_Gg3$p2inp1vec <- is.element(PORT22_Gg3$player2, player1vector)

addPlayer1 <- PORT22_Gg3[ which(PORT22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT22_Gg2 <- rbind(PORT22_Gg2, addPlayer1)

#ROUND 22, Goal graph using weighted edges
PORT22_Gft <- ftable(PORT22_Gg2$player1, PORT22_Gg2$player2)
PORT22_Gft2 <- as.matrix(PORT22_Gft)
numRows <- nrow(PORT22_Gft2)
numCols <- ncol(PORT22_Gft2)
PORT22_Gft3 <- PORT22_Gft2[c(2:numRows) , c(1:numCols)]
PORT22_GTable <- graph.adjacency(PORT22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(PORT22_GTable, vertex.label = V(PORT22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
PORT22_G.clusterCoef <- transitivity(PORT22_GTable, type="global") #cluster coefficient
PORT22_G.degreeCent <- centralization.degree(PORT22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_Gftn <- as.network.matrix(PORT22_Gft)
PORT22_G.netDensity <- network.density(PORT22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_G.entropy <- entropy(PORT22_Gft) #entropy

PORT22_G.netMx <- cbind(PORT22_G.netMx, PORT22_G.clusterCoef, PORT22_G.degreeCent$centralization,
                        PORT22_G.netDensity, PORT22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Behind_F"
PORT22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
PORT22_Bg2 <- data.frame(PORT22_B)
PORT22_Bg2 <- PORT22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_Bg2$player1
player2vector <- PORT22_Bg2$player2
PORT22_Bg3 <- PORT22_Bg2
PORT22_Bg3$p1inp2vec <- is.element(PORT22_Bg3$player1, player2vector)
PORT22_Bg3$p2inp1vec <- is.element(PORT22_Bg3$player2, player1vector)

addPlayer1 <- PORT22_Bg3[ which(PORT22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_Bg3[ which(PORT22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_Bg2 <- rbind(PORT22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
PORT22_Bft <- ftable(PORT22_Bg2$player1, PORT22_Bg2$player2)
PORT22_Bft2 <- as.matrix(PORT22_Bft)
numRows <- nrow(PORT22_Bft2)
numCols <- ncol(PORT22_Bft2)
PORT22_Bft3 <- PORT22_Bft2[c(2:numRows) , c(2:numCols)]
PORT22_BTable <- graph.adjacency(PORT22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(PORT22_BTable, vertex.label = V(PORT22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
PORT22_B.clusterCoef <- transitivity(PORT22_BTable, type="global") #cluster coefficient
PORT22_B.degreeCent <- centralization.degree(PORT22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_Bftn <- as.network.matrix(PORT22_Bft)
PORT22_B.netDensity <- network.density(PORT22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_B.entropy <- entropy(PORT22_Bft) #entropy

PORT22_B.netMx <- cbind(PORT22_B.netMx, PORT22_B.clusterCoef, PORT22_B.degreeCent$centralization,
                        PORT22_B.netDensity, PORT22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
PORT22_SFg2 <- data.frame(PORT22_SF)
PORT22_SFg2 <- PORT22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_SFg2$player1
player2vector <- PORT22_SFg2$player2
PORT22_SFg3 <- PORT22_SFg2
PORT22_SFg3$p1inp2vec <- is.element(PORT22_SFg3$player1, player2vector)
PORT22_SFg3$p2inp1vec <- is.element(PORT22_SFg3$player2, player1vector)

addPlayer1 <- PORT22_SFg3[ which(PORT22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_SFg3[ which(PORT22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_SFg2 <- rbind(PORT22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
PORT22_SFft <- ftable(PORT22_SFg2$player1, PORT22_SFg2$player2)
PORT22_SFft2 <- as.matrix(PORT22_SFft)
numRows <- nrow(PORT22_SFft2)
numCols <- ncol(PORT22_SFft2)
PORT22_SFft3 <- PORT22_SFft2[c(2:numRows) , c(2:numCols)]
PORT22_SFTable <- graph.adjacency(PORT22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(PORT22_SFTable, vertex.label = V(PORT22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
PORT22_SF.clusterCoef <- transitivity(PORT22_SFTable, type="global") #cluster coefficient
PORT22_SF.degreeCent <- centralization.degree(PORT22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_SFftn <- as.network.matrix(PORT22_SFft)
PORT22_SF.netDensity <- network.density(PORT22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_SF.entropy <- entropy(PORT22_SFft) #entropy

PORT22_SF.netMx <- cbind(PORT22_SF.netMx, PORT22_SF.clusterCoef, PORT22_SF.degreeCent$centralization,
                         PORT22_SF.netDensity, PORT22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
PORT22_TFg2 <- data.frame(PORT22_TF)
PORT22_TFg2 <- PORT22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_TFg2$player1
player2vector <- PORT22_TFg2$player2
PORT22_TFg3 <- PORT22_TFg2
PORT22_TFg3$p1inp2vec <- is.element(PORT22_TFg3$player1, player2vector)
PORT22_TFg3$p2inp1vec <- is.element(PORT22_TFg3$player2, player1vector)

addPlayer1 <- PORT22_TFg3[ which(PORT22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT22_TFg2 <- rbind(PORT22_TFg2, addPlayer1)

#ROUND 22, FWD Turnover graph using weighted edges
PORT22_TFft <- ftable(PORT22_TFg2$player1, PORT22_TFg2$player2)
PORT22_TFft2 <- as.matrix(PORT22_TFft)
numRows <- nrow(PORT22_TFft2)
numCols <- ncol(PORT22_TFft2)
PORT22_TFft3 <- PORT22_TFft2[c(2:numRows) , c(1:numCols)]
PORT22_TFTable <- graph.adjacency(PORT22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(PORT22_TFTable, vertex.label = V(PORT22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
PORT22_TF.clusterCoef <- transitivity(PORT22_TFTable, type="global") #cluster coefficient
PORT22_TF.degreeCent <- centralization.degree(PORT22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_TFftn <- as.network.matrix(PORT22_TFft)
PORT22_TF.netDensity <- network.density(PORT22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_TF.entropy <- entropy(PORT22_TFft) #entropy

PORT22_TF.netMx <- cbind(PORT22_TF.netMx, PORT22_TF.clusterCoef, PORT22_TF.degreeCent$centralization,
                         PORT22_TF.netDensity, PORT22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
PORT22_SAMg2 <- data.frame(PORT22_SAM)
PORT22_SAMg2 <- PORT22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_SAMg2$player1
player2vector <- PORT22_SAMg2$player2
PORT22_SAMg3 <- PORT22_SAMg2
PORT22_SAMg3$p1inp2vec <- is.element(PORT22_SAMg3$player1, player2vector)
PORT22_SAMg3$p2inp1vec <- is.element(PORT22_SAMg3$player2, player1vector)

addPlayer1 <- PORT22_SAMg3[ which(PORT22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_SAMg3[ which(PORT22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_SAMg2 <- rbind(PORT22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
PORT22_SAMft <- ftable(PORT22_SAMg2$player1, PORT22_SAMg2$player2)
PORT22_SAMft2 <- as.matrix(PORT22_SAMft)
numRows <- nrow(PORT22_SAMft2)
numCols <- ncol(PORT22_SAMft2)
PORT22_SAMft3 <- PORT22_SAMft2[c(2:numRows) , c(2:numCols)]
PORT22_SAMTable <- graph.adjacency(PORT22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(PORT22_SAMTable, vertex.label = V(PORT22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
PORT22_SAM.clusterCoef <- transitivity(PORT22_SAMTable, type="global") #cluster coefficient
PORT22_SAM.degreeCent <- centralization.degree(PORT22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_SAMftn <- as.network.matrix(PORT22_SAMft)
PORT22_SAM.netDensity <- network.density(PORT22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_SAM.entropy <- entropy(PORT22_SAMft) #entropy

PORT22_SAM.netMx <- cbind(PORT22_SAM.netMx, PORT22_SAM.clusterCoef, PORT22_SAM.degreeCent$centralization,
                          PORT22_SAM.netDensity, PORT22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
PORT22_TAMg2 <- data.frame(PORT22_TAM)
PORT22_TAMg2 <- PORT22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_TAMg2$player1
player2vector <- PORT22_TAMg2$player2
PORT22_TAMg3 <- PORT22_TAMg2
PORT22_TAMg3$p1inp2vec <- is.element(PORT22_TAMg3$player1, player2vector)
PORT22_TAMg3$p2inp1vec <- is.element(PORT22_TAMg3$player2, player1vector)

addPlayer1 <- PORT22_TAMg3[ which(PORT22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_TAMg3[ which(PORT22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_TAMg2 <- rbind(PORT22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
PORT22_TAMft <- ftable(PORT22_TAMg2$player1, PORT22_TAMg2$player2)
PORT22_TAMft2 <- as.matrix(PORT22_TAMft)
numRows <- nrow(PORT22_TAMft2)
numCols <- ncol(PORT22_TAMft2)
PORT22_TAMft3 <- PORT22_TAMft2[c(2:numRows) , c(2:numCols)]
PORT22_TAMTable <- graph.adjacency(PORT22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(PORT22_TAMTable, vertex.label = V(PORT22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
PORT22_TAM.clusterCoef <- transitivity(PORT22_TAMTable, type="global") #cluster coefficient
PORT22_TAM.degreeCent <- centralization.degree(PORT22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_TAMftn <- as.network.matrix(PORT22_TAMft)
PORT22_TAM.netDensity <- network.density(PORT22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_TAM.entropy <- entropy(PORT22_TAMft) #entropy

PORT22_TAM.netMx <- cbind(PORT22_TAM.netMx, PORT22_TAM.clusterCoef, PORT22_TAM.degreeCent$centralization,
                          PORT22_TAM.netDensity, PORT22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
PORT22_SDMg2 <- data.frame(PORT22_SDM)
PORT22_SDMg2 <- PORT22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_SDMg2$player1
player2vector <- PORT22_SDMg2$player2
PORT22_SDMg3 <- PORT22_SDMg2
PORT22_SDMg3$p1inp2vec <- is.element(PORT22_SDMg3$player1, player2vector)
PORT22_SDMg3$p2inp1vec <- is.element(PORT22_SDMg3$player2, player1vector)

addPlayer1 <- PORT22_SDMg3[ which(PORT22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_SDMg3[ which(PORT22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_SDMg2 <- rbind(PORT22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
PORT22_SDMft <- ftable(PORT22_SDMg2$player1, PORT22_SDMg2$player2)
PORT22_SDMft2 <- as.matrix(PORT22_SDMft)
numRows <- nrow(PORT22_SDMft2)
numCols <- ncol(PORT22_SDMft2)
PORT22_SDMft3 <- PORT22_SDMft2[c(2:numRows) , c(2:numCols)]
PORT22_SDMTable <- graph.adjacency(PORT22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(PORT22_SDMTable, vertex.label = V(PORT22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
PORT22_SDM.clusterCoef <- transitivity(PORT22_SDMTable, type="global") #cluster coefficient
PORT22_SDM.degreeCent <- centralization.degree(PORT22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_SDMftn <- as.network.matrix(PORT22_SDMft)
PORT22_SDM.netDensity <- network.density(PORT22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_SDM.entropy <- entropy(PORT22_SDMft) #entropy

PORT22_SDM.netMx <- cbind(PORT22_SDM.netMx, PORT22_SDM.clusterCoef, PORT22_SDM.degreeCent$centralization,
                          PORT22_SDM.netDensity, PORT22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
PORT22_TDMg2 <- data.frame(PORT22_TDM)
PORT22_TDMg2 <- PORT22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_TDMg2$player1
player2vector <- PORT22_TDMg2$player2
PORT22_TDMg3 <- PORT22_TDMg2
PORT22_TDMg3$p1inp2vec <- is.element(PORT22_TDMg3$player1, player2vector)
PORT22_TDMg3$p2inp1vec <- is.element(PORT22_TDMg3$player2, player1vector)

addPlayer1 <- PORT22_TDMg3[ which(PORT22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_TDMg3[ which(PORT22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_TDMg2 <- rbind(PORT22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
PORT22_TDMft <- ftable(PORT22_TDMg2$player1, PORT22_TDMg2$player2)
PORT22_TDMft2 <- as.matrix(PORT22_TDMft)
numRows <- nrow(PORT22_TDMft2)
numCols <- ncol(PORT22_TDMft2)
PORT22_TDMft3 <- PORT22_TDMft2[c(2:numRows) , c(2:numCols)]
PORT22_TDMTable <- graph.adjacency(PORT22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(PORT22_TDMTable, vertex.label = V(PORT22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
PORT22_TDM.clusterCoef <- transitivity(PORT22_TDMTable, type="global") #cluster coefficient
PORT22_TDM.degreeCent <- centralization.degree(PORT22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_TDMftn <- as.network.matrix(PORT22_TDMft)
PORT22_TDM.netDensity <- network.density(PORT22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_TDM.entropy <- entropy(PORT22_TDMft) #entropy

PORT22_TDM.netMx <- cbind(PORT22_TDM.netMx, PORT22_TDM.clusterCoef, PORT22_TDM.degreeCent$centralization,
                          PORT22_TDM.netDensity, PORT22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
PORT22_SDg2 <- data.frame(PORT22_SD)
PORT22_SDg2 <- PORT22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_SDg2$player1
player2vector <- PORT22_SDg2$player2
PORT22_SDg3 <- PORT22_SDg2
PORT22_SDg3$p1inp2vec <- is.element(PORT22_SDg3$player1, player2vector)
PORT22_SDg3$p2inp1vec <- is.element(PORT22_SDg3$player2, player1vector)

addPlayer1 <- PORT22_SDg3[ which(PORT22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_SDg3[ which(PORT22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_SDg2 <- rbind(PORT22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
PORT22_SDft <- ftable(PORT22_SDg2$player1, PORT22_SDg2$player2)
PORT22_SDft2 <- as.matrix(PORT22_SDft)
numRows <- nrow(PORT22_SDft2)
numCols <- ncol(PORT22_SDft2)
PORT22_SDft3 <- PORT22_SDft2[c(2:numRows) , c(2:numCols)]
PORT22_SDTable <- graph.adjacency(PORT22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(PORT22_SDTable, vertex.label = V(PORT22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
PORT22_SD.clusterCoef <- transitivity(PORT22_SDTable, type="global") #cluster coefficient
PORT22_SD.degreeCent <- centralization.degree(PORT22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_SDftn <- as.network.matrix(PORT22_SDft)
PORT22_SD.netDensity <- network.density(PORT22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_SD.entropy <- entropy(PORT22_SDft) #entropy

PORT22_SD.netMx <- cbind(PORT22_SD.netMx, PORT22_SD.clusterCoef, PORT22_SD.degreeCent$centralization,
                         PORT22_SD.netDensity, PORT22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
PORT22_TDg2 <- data.frame(PORT22_TD)
PORT22_TDg2 <- PORT22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_TDg2$player1
player2vector <- PORT22_TDg2$player2
PORT22_TDg3 <- PORT22_TDg2
PORT22_TDg3$p1inp2vec <- is.element(PORT22_TDg3$player1, player2vector)
PORT22_TDg3$p2inp1vec <- is.element(PORT22_TDg3$player2, player1vector)

addPlayer1 <- PORT22_TDg3[ which(PORT22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_TDg3[ which(PORT22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_TDg2 <- rbind(PORT22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
PORT22_TDft <- ftable(PORT22_TDg2$player1, PORT22_TDg2$player2)
PORT22_TDft2 <- as.matrix(PORT22_TDft)
numRows <- nrow(PORT22_TDft2)
numCols <- ncol(PORT22_TDft2)
PORT22_TDft3 <- PORT22_TDft2[c(2:numRows) , c(2:numCols)]
PORT22_TDTable <- graph.adjacency(PORT22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(PORT22_TDTable, vertex.label = V(PORT22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
PORT22_TD.clusterCoef <- transitivity(PORT22_TDTable, type="global") #cluster coefficient
PORT22_TD.degreeCent <- centralization.degree(PORT22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_TDftn <- as.network.matrix(PORT22_TDft)
PORT22_TD.netDensity <- network.density(PORT22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_TD.entropy <- entropy(PORT22_TDft) #entropy

PORT22_TD.netMx <- cbind(PORT22_TD.netMx, PORT22_TD.clusterCoef, PORT22_TD.degreeCent$centralization,
                         PORT22_TD.netDensity, PORT22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
PORT22_QTg2 <- data.frame(PORT22_QT)
PORT22_QTg2 <- PORT22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT22_QTg2$player1
player2vector <- PORT22_QTg2$player2
PORT22_QTg3 <- PORT22_QTg2
PORT22_QTg3$p1inp2vec <- is.element(PORT22_QTg3$player1, player2vector)
PORT22_QTg3$p2inp1vec <- is.element(PORT22_QTg3$player2, player1vector)

addPlayer1 <- PORT22_QTg3[ which(PORT22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT22_QTg3[ which(PORT22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT22_QTg2 <- rbind(PORT22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
PORT22_QTft <- ftable(PORT22_QTg2$player1, PORT22_QTg2$player2)
PORT22_QTft2 <- as.matrix(PORT22_QTft)
numRows <- nrow(PORT22_QTft2)
numCols <- ncol(PORT22_QTft2)
PORT22_QTft3 <- PORT22_QTft2[c(2:numRows) , c(2:numCols)]
PORT22_QTTable <- graph.adjacency(PORT22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(PORT22_QTTable, vertex.label = V(PORT22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
PORT22_QT.clusterCoef <- transitivity(PORT22_QTTable, type="global") #cluster coefficient
PORT22_QT.degreeCent <- centralization.degree(PORT22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT22_QTftn <- as.network.matrix(PORT22_QTft)
PORT22_QT.netDensity <- network.density(PORT22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT22_QT.entropy <- entropy(PORT22_QTft) #entropy

PORT22_QT.netMx <- cbind(PORT22_QT.netMx, PORT22_QT.clusterCoef, PORT22_QT.degreeCent$centralization,
                         PORT22_QT.netDensity, PORT22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT22_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Goal_F"
RICH22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
RICH22_Gg2 <- data.frame(RICH22_G)
RICH22_Gg2 <- RICH22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_Gg2$player1
player2vector <- RICH22_Gg2$player2
RICH22_Gg3 <- RICH22_Gg2
RICH22_Gg3$p1inp2vec <- is.element(RICH22_Gg3$player1, player2vector)
RICH22_Gg3$p2inp1vec <- is.element(RICH22_Gg3$player2, player1vector)

addPlayer1 <- RICH22_Gg3[ which(RICH22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_Gg3[ which(RICH22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_Gg2 <- rbind(RICH22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
RICH22_Gft <- ftable(RICH22_Gg2$player1, RICH22_Gg2$player2)
RICH22_Gft2 <- as.matrix(RICH22_Gft)
numRows <- nrow(RICH22_Gft2)
numCols <- ncol(RICH22_Gft2)
RICH22_Gft3 <- RICH22_Gft2[c(2:numRows) , c(2:numCols)]
RICH22_GTable <- graph.adjacency(RICH22_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(RICH22_GTable, vertex.label = V(RICH22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
RICH22_G.clusterCoef <- transitivity(RICH22_GTable, type="global") #cluster coefficient
RICH22_G.degreeCent <- centralization.degree(RICH22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_Gftn <- as.network.matrix(RICH22_Gft)
RICH22_G.netDensity <- network.density(RICH22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_G.entropy <- entropy(RICH22_Gft) #entropy

RICH22_G.netMx <- cbind(RICH22_G.netMx, RICH22_G.clusterCoef, RICH22_G.degreeCent$centralization,
                        RICH22_G.netDensity, RICH22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Behind_F"
RICH22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
RICH22_Bg2 <- data.frame(RICH22_B)
RICH22_Bg2 <- RICH22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_Bg2$player1
player2vector <- RICH22_Bg2$player2
RICH22_Bg3 <- RICH22_Bg2
RICH22_Bg3$p1inp2vec <- is.element(RICH22_Bg3$player1, player2vector)
RICH22_Bg3$p2inp1vec <- is.element(RICH22_Bg3$player2, player1vector)

addPlayer1 <- RICH22_Bg3[ which(RICH22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_Bg3[ which(RICH22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_Bg2 <- rbind(RICH22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
RICH22_Bft <- ftable(RICH22_Bg2$player1, RICH22_Bg2$player2)
RICH22_Bft2 <- as.matrix(RICH22_Bft)
numRows <- nrow(RICH22_Bft2)
numCols <- ncol(RICH22_Bft2)
RICH22_Bft3 <- RICH22_Bft2[c(2:numRows) , c(2:numCols)]
RICH22_BTable <- graph.adjacency(RICH22_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(RICH22_BTable, vertex.label = V(RICH22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
RICH22_B.clusterCoef <- transitivity(RICH22_BTable, type="global") #cluster coefficient
RICH22_B.degreeCent <- centralization.degree(RICH22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_Bftn <- as.network.matrix(RICH22_Bft)
RICH22_B.netDensity <- network.density(RICH22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_B.entropy <- entropy(RICH22_Bft) #entropy

RICH22_B.netMx <- cbind(RICH22_B.netMx, RICH22_B.clusterCoef, RICH22_B.degreeCent$centralization,
                        RICH22_B.netDensity, RICH22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
RICH22_SFg2 <- data.frame(RICH22_SF)
RICH22_SFg2 <- RICH22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_SFg2$player1
player2vector <- RICH22_SFg2$player2
RICH22_SFg3 <- RICH22_SFg2
RICH22_SFg3$p1inp2vec <- is.element(RICH22_SFg3$player1, player2vector)
RICH22_SFg3$p2inp1vec <- is.element(RICH22_SFg3$player2, player1vector)

addPlayer1 <- RICH22_SFg3[ which(RICH22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_SFg3[ which(RICH22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_SFg2 <- rbind(RICH22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
RICH22_SFft <- ftable(RICH22_SFg2$player1, RICH22_SFg2$player2)
RICH22_SFft2 <- as.matrix(RICH22_SFft)
numRows <- nrow(RICH22_SFft2)
numCols <- ncol(RICH22_SFft2)
RICH22_SFft3 <- RICH22_SFft2[c(2:numRows) , c(2:numCols)]
RICH22_SFTable <- graph.adjacency(RICH22_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(RICH22_SFTable, vertex.label = V(RICH22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
RICH22_SF.clusterCoef <- transitivity(RICH22_SFTable, type="global") #cluster coefficient
RICH22_SF.degreeCent <- centralization.degree(RICH22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_SFftn <- as.network.matrix(RICH22_SFft)
RICH22_SF.netDensity <- network.density(RICH22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_SF.entropy <- entropy(RICH22_SFft) #entropy

RICH22_SF.netMx <- cbind(RICH22_SF.netMx, RICH22_SF.clusterCoef, RICH22_SF.degreeCent$centralization,
                         RICH22_SF.netDensity, RICH22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
RICH22_TFg2 <- data.frame(RICH22_TF)
RICH22_TFg2 <- RICH22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_TFg2$player1
player2vector <- RICH22_TFg2$player2
RICH22_TFg3 <- RICH22_TFg2
RICH22_TFg3$p1inp2vec <- is.element(RICH22_TFg3$player1, player2vector)
RICH22_TFg3$p2inp1vec <- is.element(RICH22_TFg3$player2, player1vector)

addPlayer1 <- RICH22_TFg3[ which(RICH22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH22_TFg3[ which(RICH22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_TFg2 <- rbind(RICH22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
RICH22_TFft <- ftable(RICH22_TFg2$player1, RICH22_TFg2$player2)
RICH22_TFft2 <- as.matrix(RICH22_TFft)
numRows <- nrow(RICH22_TFft2)
numCols <- ncol(RICH22_TFft2)
RICH22_TFft3 <- RICH22_TFft2[c(2:numRows) , c(2:numCols)]
RICH22_TFTable <- graph.adjacency(RICH22_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(RICH22_TFTable, vertex.label = V(RICH22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
RICH22_TF.clusterCoef <- transitivity(RICH22_TFTable, type="global") #cluster coefficient
RICH22_TF.degreeCent <- centralization.degree(RICH22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_TFftn <- as.network.matrix(RICH22_TFft)
RICH22_TF.netDensity <- network.density(RICH22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_TF.entropy <- entropy(RICH22_TFft) #entropy

RICH22_TF.netMx <- cbind(RICH22_TF.netMx, RICH22_TF.clusterCoef, RICH22_TF.degreeCent$centralization,
                         RICH22_TF.netDensity, RICH22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
RICH22_SAMg2 <- data.frame(RICH22_SAM)
RICH22_SAMg2 <- RICH22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_SAMg2$player1
player2vector <- RICH22_SAMg2$player2
RICH22_SAMg3 <- RICH22_SAMg2
RICH22_SAMg3$p1inp2vec <- is.element(RICH22_SAMg3$player1, player2vector)
RICH22_SAMg3$p2inp1vec <- is.element(RICH22_SAMg3$player2, player1vector)

addPlayer1 <- RICH22_SAMg3[ which(RICH22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_SAMg3[ which(RICH22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_SAMg2 <- rbind(RICH22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
RICH22_SAMft <- ftable(RICH22_SAMg2$player1, RICH22_SAMg2$player2)
RICH22_SAMft2 <- as.matrix(RICH22_SAMft)
numRows <- nrow(RICH22_SAMft2)
numCols <- ncol(RICH22_SAMft2)
RICH22_SAMft3 <- RICH22_SAMft2[c(2:numRows) , c(2:numCols)]
RICH22_SAMTable <- graph.adjacency(RICH22_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(RICH22_SAMTable, vertex.label = V(RICH22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
RICH22_SAM.clusterCoef <- transitivity(RICH22_SAMTable, type="global") #cluster coefficient
RICH22_SAM.degreeCent <- centralization.degree(RICH22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_SAMftn <- as.network.matrix(RICH22_SAMft)
RICH22_SAM.netDensity <- network.density(RICH22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_SAM.entropy <- entropy(RICH22_SAMft) #entropy

RICH22_SAM.netMx <- cbind(RICH22_SAM.netMx, RICH22_SAM.clusterCoef, RICH22_SAM.degreeCent$centralization,
                          RICH22_SAM.netDensity, RICH22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
RICH22_TAMg2 <- data.frame(RICH22_TAM)
RICH22_TAMg2 <- RICH22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_TAMg2$player1
player2vector <- RICH22_TAMg2$player2
RICH22_TAMg3 <- RICH22_TAMg2
RICH22_TAMg3$p1inp2vec <- is.element(RICH22_TAMg3$player1, player2vector)
RICH22_TAMg3$p2inp1vec <- is.element(RICH22_TAMg3$player2, player1vector)

addPlayer1 <- RICH22_TAMg3[ which(RICH22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_TAMg3[ which(RICH22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_TAMg2 <- rbind(RICH22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
RICH22_TAMft <- ftable(RICH22_TAMg2$player1, RICH22_TAMg2$player2)
RICH22_TAMft2 <- as.matrix(RICH22_TAMft)
numRows <- nrow(RICH22_TAMft2)
numCols <- ncol(RICH22_TAMft2)
RICH22_TAMft3 <- RICH22_TAMft2[c(2:numRows) , c(2:numCols)]
RICH22_TAMTable <- graph.adjacency(RICH22_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(RICH22_TAMTable, vertex.label = V(RICH22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
RICH22_TAM.clusterCoef <- transitivity(RICH22_TAMTable, type="global") #cluster coefficient
RICH22_TAM.degreeCent <- centralization.degree(RICH22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_TAMftn <- as.network.matrix(RICH22_TAMft)
RICH22_TAM.netDensity <- network.density(RICH22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_TAM.entropy <- entropy(RICH22_TAMft) #entropy

RICH22_TAM.netMx <- cbind(RICH22_TAM.netMx, RICH22_TAM.clusterCoef, RICH22_TAM.degreeCent$centralization,
                          RICH22_TAM.netDensity, RICH22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
RICH22_SDMg2 <- data.frame(RICH22_SDM)
RICH22_SDMg2 <- RICH22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_SDMg2$player1
player2vector <- RICH22_SDMg2$player2
RICH22_SDMg3 <- RICH22_SDMg2
RICH22_SDMg3$p1inp2vec <- is.element(RICH22_SDMg3$player1, player2vector)
RICH22_SDMg3$p2inp1vec <- is.element(RICH22_SDMg3$player2, player1vector)

addPlayer1 <- RICH22_SDMg3[ which(RICH22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_SDMg3[ which(RICH22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_SDMg2 <- rbind(RICH22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
RICH22_SDMft <- ftable(RICH22_SDMg2$player1, RICH22_SDMg2$player2)
RICH22_SDMft2 <- as.matrix(RICH22_SDMft)
numRows <- nrow(RICH22_SDMft2)
numCols <- ncol(RICH22_SDMft2)
RICH22_SDMft3 <- RICH22_SDMft2[c(2:numRows) , c(2:numCols)]
RICH22_SDMTable <- graph.adjacency(RICH22_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(RICH22_SDMTable, vertex.label = V(RICH22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
RICH22_SDM.clusterCoef <- transitivity(RICH22_SDMTable, type="global") #cluster coefficient
RICH22_SDM.degreeCent <- centralization.degree(RICH22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_SDMftn <- as.network.matrix(RICH22_SDMft)
RICH22_SDM.netDensity <- network.density(RICH22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_SDM.entropy <- entropy(RICH22_SDMft) #entropy

RICH22_SDM.netMx <- cbind(RICH22_SDM.netMx, RICH22_SDM.clusterCoef, RICH22_SDM.degreeCent$centralization,
                          RICH22_SDM.netDensity, RICH22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
RICH22_TDMg2 <- data.frame(RICH22_TDM)
RICH22_TDMg2 <- RICH22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_TDMg2$player1
player2vector <- RICH22_TDMg2$player2
RICH22_TDMg3 <- RICH22_TDMg2
RICH22_TDMg3$p1inp2vec <- is.element(RICH22_TDMg3$player1, player2vector)
RICH22_TDMg3$p2inp1vec <- is.element(RICH22_TDMg3$player2, player1vector)

addPlayer1 <- RICH22_TDMg3[ which(RICH22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_TDMg3[ which(RICH22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_TDMg2 <- rbind(RICH22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
RICH22_TDMft <- ftable(RICH22_TDMg2$player1, RICH22_TDMg2$player2)
RICH22_TDMft2 <- as.matrix(RICH22_TDMft)
numRows <- nrow(RICH22_TDMft2)
numCols <- ncol(RICH22_TDMft2)
RICH22_TDMft3 <- RICH22_TDMft2[c(2:numRows) , c(2:numCols)]
RICH22_TDMTable <- graph.adjacency(RICH22_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(RICH22_TDMTable, vertex.label = V(RICH22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
RICH22_TDM.clusterCoef <- transitivity(RICH22_TDMTable, type="global") #cluster coefficient
RICH22_TDM.degreeCent <- centralization.degree(RICH22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_TDMftn <- as.network.matrix(RICH22_TDMft)
RICH22_TDM.netDensity <- network.density(RICH22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_TDM.entropy <- entropy(RICH22_TDMft) #entropy

RICH22_TDM.netMx <- cbind(RICH22_TDM.netMx, RICH22_TDM.clusterCoef, RICH22_TDM.degreeCent$centralization,
                          RICH22_TDM.netDensity, RICH22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
RICH22_SDg2 <- data.frame(RICH22_SD)
RICH22_SDg2 <- RICH22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_SDg2$player1
player2vector <- RICH22_SDg2$player2
RICH22_SDg3 <- RICH22_SDg2
RICH22_SDg3$p1inp2vec <- is.element(RICH22_SDg3$player1, player2vector)
RICH22_SDg3$p2inp1vec <- is.element(RICH22_SDg3$player2, player1vector)

addPlayer1 <- RICH22_SDg3[ which(RICH22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_SDg3[ which(RICH22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_SDg2 <- rbind(RICH22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
RICH22_SDft <- ftable(RICH22_SDg2$player1, RICH22_SDg2$player2)
RICH22_SDft2 <- as.matrix(RICH22_SDft)
numRows <- nrow(RICH22_SDft2)
numCols <- ncol(RICH22_SDft2)
RICH22_SDft3 <- RICH22_SDft2[c(2:numRows) , c(2:numCols)]
RICH22_SDTable <- graph.adjacency(RICH22_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(RICH22_SDTable, vertex.label = V(RICH22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
RICH22_SD.clusterCoef <- transitivity(RICH22_SDTable, type="global") #cluster coefficient
RICH22_SD.degreeCent <- centralization.degree(RICH22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_SDftn <- as.network.matrix(RICH22_SDft)
RICH22_SD.netDensity <- network.density(RICH22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_SD.entropy <- entropy(RICH22_SDft) #entropy

RICH22_SD.netMx <- cbind(RICH22_SD.netMx, RICH22_SD.clusterCoef, RICH22_SD.degreeCent$centralization,
                         RICH22_SD.netDensity, RICH22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
RICH22_TDg2 <- data.frame(RICH22_TD)
RICH22_TDg2 <- RICH22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_TDg2$player1
player2vector <- RICH22_TDg2$player2
RICH22_TDg3 <- RICH22_TDg2
RICH22_TDg3$p1inp2vec <- is.element(RICH22_TDg3$player1, player2vector)
RICH22_TDg3$p2inp1vec <- is.element(RICH22_TDg3$player2, player1vector)

addPlayer1 <- RICH22_TDg3[ which(RICH22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_TDg3[ which(RICH22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_TDg2 <- rbind(RICH22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
RICH22_TDft <- ftable(RICH22_TDg2$player1, RICH22_TDg2$player2)
RICH22_TDft2 <- as.matrix(RICH22_TDft)
numRows <- nrow(RICH22_TDft2)
numCols <- ncol(RICH22_TDft2)
RICH22_TDft3 <- RICH22_TDft2[c(2:numRows) , c(2:numCols)]
RICH22_TDTable <- graph.adjacency(RICH22_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(RICH22_TDTable, vertex.label = V(RICH22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
RICH22_TD.clusterCoef <- transitivity(RICH22_TDTable, type="global") #cluster coefficient
RICH22_TD.degreeCent <- centralization.degree(RICH22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_TDftn <- as.network.matrix(RICH22_TDft)
RICH22_TD.netDensity <- network.density(RICH22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_TD.entropy <- entropy(RICH22_TDft) #entropy

RICH22_TD.netMx <- cbind(RICH22_TD.netMx, RICH22_TD.clusterCoef, RICH22_TD.degreeCent$centralization,
                         RICH22_TD.netDensity, RICH22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
RICH22_QTg2 <- data.frame(RICH22_QT)
RICH22_QTg2 <- RICH22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH22_QTg2$player1
player2vector <- RICH22_QTg2$player2
RICH22_QTg3 <- RICH22_QTg2
RICH22_QTg3$p1inp2vec <- is.element(RICH22_QTg3$player1, player2vector)
RICH22_QTg3$p2inp1vec <- is.element(RICH22_QTg3$player2, player1vector)

addPlayer1 <- RICH22_QTg3[ which(RICH22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH22_QTg3[ which(RICH22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH22_QTg2 <- rbind(RICH22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
RICH22_QTft <- ftable(RICH22_QTg2$player1, RICH22_QTg2$player2)
RICH22_QTft2 <- as.matrix(RICH22_QTft)
numRows <- nrow(RICH22_QTft2)
numCols <- ncol(RICH22_QTft2)
RICH22_QTft3 <- RICH22_QTft2[c(2:numRows) , c(2:numCols)]
RICH22_QTTable <- graph.adjacency(RICH22_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(RICH22_QTTable, vertex.label = V(RICH22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
RICH22_QT.clusterCoef <- transitivity(RICH22_QTTable, type="global") #cluster coefficient
RICH22_QT.degreeCent <- centralization.degree(RICH22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH22_QTftn <- as.network.matrix(RICH22_QTft)
RICH22_QT.netDensity <- network.density(RICH22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH22_QT.entropy <- entropy(RICH22_QTft) #entropy

RICH22_QT.netMx <- cbind(RICH22_QT.netMx, RICH22_QT.clusterCoef, RICH22_QT.degreeCent$centralization,
                         RICH22_QT.netDensity, RICH22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH22_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "STK"
KIoutcome = "Goal_F"
STK22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
STK22_Gg2 <- data.frame(STK22_G)
STK22_Gg2 <- STK22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_Gg2$player1
player2vector <- STK22_Gg2$player2
STK22_Gg3 <- STK22_Gg2
STK22_Gg3$p1inp2vec <- is.element(STK22_Gg3$player1, player2vector)
STK22_Gg3$p2inp1vec <- is.element(STK22_Gg3$player2, player1vector)

addPlayer1 <- STK22_Gg3[ which(STK22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_Gg3[ which(STK22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_Gg2 <- rbind(STK22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
STK22_Gft <- ftable(STK22_Gg2$player1, STK22_Gg2$player2)
STK22_Gft2 <- as.matrix(STK22_Gft)
numRows <- nrow(STK22_Gft2)
numCols <- ncol(STK22_Gft2)
STK22_Gft3 <- STK22_Gft2[c(2:numRows) , c(2:numCols)]
STK22_GTable <- graph.adjacency(STK22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(STK22_GTable, vertex.label = V(STK22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
STK22_G.clusterCoef <- transitivity(STK22_GTable, type="global") #cluster coefficient
STK22_G.degreeCent <- centralization.degree(STK22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_Gftn <- as.network.matrix(STK22_Gft)
STK22_G.netDensity <- network.density(STK22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_G.entropy <- entropy(STK22_Gft) #entropy

STK22_G.netMx <- cbind(STK22_G.netMx, STK22_G.clusterCoef, STK22_G.degreeCent$centralization,
                       STK22_G.netDensity, STK22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "STK"
KIoutcome = "Behind_F"
STK22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
STK22_Bg2 <- data.frame(STK22_B)
STK22_Bg2 <- STK22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_Bg2$player1
player2vector <- STK22_Bg2$player2
STK22_Bg3 <- STK22_Bg2
STK22_Bg3$p1inp2vec <- is.element(STK22_Bg3$player1, player2vector)
STK22_Bg3$p2inp1vec <- is.element(STK22_Bg3$player2, player1vector)

addPlayer1 <- STK22_Bg3[ which(STK22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_Bg3[ which(STK22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_Bg2 <- rbind(STK22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
STK22_Bft <- ftable(STK22_Bg2$player1, STK22_Bg2$player2)
STK22_Bft2 <- as.matrix(STK22_Bft)
numRows <- nrow(STK22_Bft2)
numCols <- ncol(STK22_Bft2)
STK22_Bft3 <- STK22_Bft2[c(2:numRows) , c(2:numCols)]
STK22_BTable <- graph.adjacency(STK22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(STK22_BTable, vertex.label = V(STK22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
STK22_B.clusterCoef <- transitivity(STK22_BTable, type="global") #cluster coefficient
STK22_B.degreeCent <- centralization.degree(STK22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_Bftn <- as.network.matrix(STK22_Bft)
STK22_B.netDensity <- network.density(STK22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_B.entropy <- entropy(STK22_Bft) #entropy

STK22_B.netMx <- cbind(STK22_B.netMx, STK22_B.clusterCoef, STK22_B.degreeCent$centralization,
                       STK22_B.netDensity, STK22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "STK"
KIoutcome = "Stoppage_F"
STK22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
STK22_SFg2 <- data.frame(STK22_SF)
STK22_SFg2 <- STK22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_SFg2$player1
player2vector <- STK22_SFg2$player2
STK22_SFg3 <- STK22_SFg2
STK22_SFg3$p1inp2vec <- is.element(STK22_SFg3$player1, player2vector)
STK22_SFg3$p2inp1vec <- is.element(STK22_SFg3$player2, player1vector)

addPlayer1 <- STK22_SFg3[ which(STK22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_SFg3[ which(STK22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_SFg2 <- rbind(STK22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
STK22_SFft <- ftable(STK22_SFg2$player1, STK22_SFg2$player2)
STK22_SFft2 <- as.matrix(STK22_SFft)
numRows <- nrow(STK22_SFft2)
numCols <- ncol(STK22_SFft2)
STK22_SFft3 <- STK22_SFft2[c(2:numRows) , c(2:numCols)]
STK22_SFTable <- graph.adjacency(STK22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(STK22_SFTable, vertex.label = V(STK22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
STK22_SF.clusterCoef <- transitivity(STK22_SFTable, type="global") #cluster coefficient
STK22_SF.degreeCent <- centralization.degree(STK22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_SFftn <- as.network.matrix(STK22_SFft)
STK22_SF.netDensity <- network.density(STK22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_SF.entropy <- entropy(STK22_SFft) #entropy

STK22_SF.netMx <- cbind(STK22_SF.netMx, STK22_SF.clusterCoef, STK22_SF.degreeCent$centralization,
                        STK22_SF.netDensity, STK22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************
#NA

round = 22
teamName = "STK"
KIoutcome = "Turnover_F"
STK22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
STK22_TFg2 <- data.frame(STK22_TF)
STK22_TFg2 <- STK22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_TFg2$player1
player2vector <- STK22_TFg2$player2
STK22_TFg3 <- STK22_TFg2
STK22_TFg3$p1inp2vec <- is.element(STK22_TFg3$player1, player2vector)
STK22_TFg3$p2inp1vec <- is.element(STK22_TFg3$player2, player1vector)

addPlayer1 <- STK22_TFg3[ which(STK22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_TFg3[ which(STK22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_TFg2 <- rbind(STK22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
STK22_TFft <- ftable(STK22_TFg2$player1, STK22_TFg2$player2)
STK22_TFft2 <- as.matrix(STK22_TFft)
numRows <- nrow(STK22_TFft2)
numCols <- ncol(STK22_TFft2)
STK22_TFft3 <- STK22_TFft2[c(2:numRows) , c(2:numCols)]
STK22_TFTable <- graph.adjacency(STK22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(STK22_TFTable, vertex.label = V(STK22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
STK22_TF.clusterCoef <- transitivity(STK22_TFTable, type="global") #cluster coefficient
STK22_TF.degreeCent <- centralization.degree(STK22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_TFftn <- as.network.matrix(STK22_TFft)
STK22_TF.netDensity <- network.density(STK22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_TF.entropy <- entropy(STK22_TFft) #entropy

STK22_TF.netMx <- cbind(STK22_TF.netMx, STK22_TF.clusterCoef, STK22_TF.degreeCent$centralization,
                        STK22_TF.netDensity, STK22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
STK22_SAMg2 <- data.frame(STK22_SAM)
STK22_SAMg2 <- STK22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_SAMg2$player1
player2vector <- STK22_SAMg2$player2
STK22_SAMg3 <- STK22_SAMg2
STK22_SAMg3$p1inp2vec <- is.element(STK22_SAMg3$player1, player2vector)
STK22_SAMg3$p2inp1vec <- is.element(STK22_SAMg3$player2, player1vector)

addPlayer1 <- STK22_SAMg3[ which(STK22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_SAMg3[ which(STK22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_SAMg2 <- rbind(STK22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
STK22_SAMft <- ftable(STK22_SAMg2$player1, STK22_SAMg2$player2)
STK22_SAMft2 <- as.matrix(STK22_SAMft)
numRows <- nrow(STK22_SAMft2)
numCols <- ncol(STK22_SAMft2)
STK22_SAMft3 <- STK22_SAMft2[c(2:numRows) , c(2:numCols)]
STK22_SAMTable <- graph.adjacency(STK22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(STK22_SAMTable, vertex.label = V(STK22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
STK22_SAM.clusterCoef <- transitivity(STK22_SAMTable, type="global") #cluster coefficient
STK22_SAM.degreeCent <- centralization.degree(STK22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_SAMftn <- as.network.matrix(STK22_SAMft)
STK22_SAM.netDensity <- network.density(STK22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_SAM.entropy <- entropy(STK22_SAMft) #entropy

STK22_SAM.netMx <- cbind(STK22_SAM.netMx, STK22_SAM.clusterCoef, STK22_SAM.degreeCent$centralization,
                         STK22_SAM.netDensity, STK22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "STK"
KIoutcome = "Turnover_AM"
STK22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
STK22_TAMg2 <- data.frame(STK22_TAM)
STK22_TAMg2 <- STK22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_TAMg2$player1
player2vector <- STK22_TAMg2$player2
STK22_TAMg3 <- STK22_TAMg2
STK22_TAMg3$p1inp2vec <- is.element(STK22_TAMg3$player1, player2vector)
STK22_TAMg3$p2inp1vec <- is.element(STK22_TAMg3$player2, player1vector)

addPlayer1 <- STK22_TAMg3[ which(STK22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_TAMg3[ which(STK22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_TAMg2 <- rbind(STK22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
STK22_TAMft <- ftable(STK22_TAMg2$player1, STK22_TAMg2$player2)
STK22_TAMft2 <- as.matrix(STK22_TAMft)
numRows <- nrow(STK22_TAMft2)
numCols <- ncol(STK22_TAMft2)
STK22_TAMft3 <- STK22_TAMft2[c(2:numRows) , c(2:numCols)]
STK22_TAMTable <- graph.adjacency(STK22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(STK22_TAMTable, vertex.label = V(STK22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
STK22_TAM.clusterCoef <- transitivity(STK22_TAMTable, type="global") #cluster coefficient
STK22_TAM.degreeCent <- centralization.degree(STK22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_TAMftn <- as.network.matrix(STK22_TAMft)
STK22_TAM.netDensity <- network.density(STK22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_TAM.entropy <- entropy(STK22_TAMft) #entropy

STK22_TAM.netMx <- cbind(STK22_TAM.netMx, STK22_TAM.clusterCoef, STK22_TAM.degreeCent$centralization,
                         STK22_TAM.netDensity, STK22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
STK22_SDMg2 <- data.frame(STK22_SDM)
STK22_SDMg2 <- STK22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_SDMg2$player1
player2vector <- STK22_SDMg2$player2
STK22_SDMg3 <- STK22_SDMg2
STK22_SDMg3$p1inp2vec <- is.element(STK22_SDMg3$player1, player2vector)
STK22_SDMg3$p2inp1vec <- is.element(STK22_SDMg3$player2, player1vector)

addPlayer1 <- STK22_SDMg3[ which(STK22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_SDMg3[ which(STK22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_SDMg2 <- rbind(STK22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
STK22_SDMft <- ftable(STK22_SDMg2$player1, STK22_SDMg2$player2)
STK22_SDMft2 <- as.matrix(STK22_SDMft)
numRows <- nrow(STK22_SDMft2)
numCols <- ncol(STK22_SDMft2)
STK22_SDMft3 <- STK22_SDMft2[c(2:numRows) , c(2:numCols)]
STK22_SDMTable <- graph.adjacency(STK22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(STK22_SDMTable, vertex.label = V(STK22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
STK22_SDM.clusterCoef <- transitivity(STK22_SDMTable, type="global") #cluster coefficient
STK22_SDM.degreeCent <- centralization.degree(STK22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_SDMftn <- as.network.matrix(STK22_SDMft)
STK22_SDM.netDensity <- network.density(STK22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_SDM.entropy <- entropy(STK22_SDMft) #entropy

STK22_SDM.netMx <- cbind(STK22_SDM.netMx, STK22_SDM.clusterCoef, STK22_SDM.degreeCent$centralization,
                         STK22_SDM.netDensity, STK22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "STK"
KIoutcome = "Turnover_DM"
STK22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
STK22_TDMg2 <- data.frame(STK22_TDM)
STK22_TDMg2 <- STK22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_TDMg2$player1
player2vector <- STK22_TDMg2$player2
STK22_TDMg3 <- STK22_TDMg2
STK22_TDMg3$p1inp2vec <- is.element(STK22_TDMg3$player1, player2vector)
STK22_TDMg3$p2inp1vec <- is.element(STK22_TDMg3$player2, player1vector)

addPlayer1 <- STK22_TDMg3[ which(STK22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_TDMg3[ which(STK22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_TDMg2 <- rbind(STK22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
STK22_TDMft <- ftable(STK22_TDMg2$player1, STK22_TDMg2$player2)
STK22_TDMft2 <- as.matrix(STK22_TDMft)
numRows <- nrow(STK22_TDMft2)
numCols <- ncol(STK22_TDMft2)
STK22_TDMft3 <- STK22_TDMft2[c(2:numRows) , c(2:numCols)]
STK22_TDMTable <- graph.adjacency(STK22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(STK22_TDMTable, vertex.label = V(STK22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
STK22_TDM.clusterCoef <- transitivity(STK22_TDMTable, type="global") #cluster coefficient
STK22_TDM.degreeCent <- centralization.degree(STK22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_TDMftn <- as.network.matrix(STK22_TDMft)
STK22_TDM.netDensity <- network.density(STK22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_TDM.entropy <- entropy(STK22_TDMft) #entropy

STK22_TDM.netMx <- cbind(STK22_TDM.netMx, STK22_TDM.clusterCoef, STK22_TDM.degreeCent$centralization,
                         STK22_TDM.netDensity, STK22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************

round = 22
teamName = "STK"
KIoutcome = "Stoppage_D"
STK22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
STK22_SDg2 <- data.frame(STK22_SD)
STK22_SDg2 <- STK22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_SDg2$player1
player2vector <- STK22_SDg2$player2
STK22_SDg3 <- STK22_SDg2
STK22_SDg3$p1inp2vec <- is.element(STK22_SDg3$player1, player2vector)
STK22_SDg3$p2inp1vec <- is.element(STK22_SDg3$player2, player1vector)

addPlayer1 <- STK22_SDg3[ which(STK22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_SDg3[ which(STK22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_SDg2 <- rbind(STK22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
STK22_SDft <- ftable(STK22_SDg2$player1, STK22_SDg2$player2)
STK22_SDft2 <- as.matrix(STK22_SDft)
numRows <- nrow(STK22_SDft2)
numCols <- ncol(STK22_SDft2)
STK22_SDft3 <- STK22_SDft2[c(2:numRows) , c(2:numCols)]
STK22_SDTable <- graph.adjacency(STK22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(STK22_SDTable, vertex.label = V(STK22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
STK22_SD.clusterCoef <- transitivity(STK22_SDTable, type="global") #cluster coefficient
STK22_SD.degreeCent <- centralization.degree(STK22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_SDftn <- as.network.matrix(STK22_SDft)
STK22_SD.netDensity <- network.density(STK22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_SD.entropy <- entropy(STK22_SDft) #entropy

STK22_SD.netMx <- cbind(STK22_SD.netMx, STK22_SD.clusterCoef, STK22_SD.degreeCent$centralization,
                        STK22_SD.netDensity, STK22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************

round = 22
teamName = "STK"
KIoutcome = "Turnover_D"
STK22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
STK22_TDg2 <- data.frame(STK22_TD)
STK22_TDg2 <- STK22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_TDg2$player1
player2vector <- STK22_TDg2$player2
STK22_TDg3 <- STK22_TDg2
STK22_TDg3$p1inp2vec <- is.element(STK22_TDg3$player1, player2vector)
STK22_TDg3$p2inp1vec <- is.element(STK22_TDg3$player2, player1vector)

addPlayer1 <- STK22_TDg3[ which(STK22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_TDg3[ which(STK22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_TDg2 <- rbind(STK22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
STK22_TDft <- ftable(STK22_TDg2$player1, STK22_TDg2$player2)
STK22_TDft2 <- as.matrix(STK22_TDft)
numRows <- nrow(STK22_TDft2)
numCols <- ncol(STK22_TDft2)
STK22_TDft3 <- STK22_TDft2[c(2:numRows) , c(2:numCols)]
STK22_TDTable <- graph.adjacency(STK22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(STK22_TDTable, vertex.label = V(STK22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
STK22_TD.clusterCoef <- transitivity(STK22_TDTable, type="global") #cluster coefficient
STK22_TD.degreeCent <- centralization.degree(STK22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_TDftn <- as.network.matrix(STK22_TDft)
STK22_TD.netDensity <- network.density(STK22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_TD.entropy <- entropy(STK22_TDft) #entropy

STK22_TD.netMx <- cbind(STK22_TD.netMx, STK22_TD.clusterCoef, STK22_TD.degreeCent$centralization,
                        STK22_TD.netDensity, STK22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************

round = 22
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
STK22_QTg2 <- data.frame(STK22_QT)
STK22_QTg2 <- STK22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK22_QTg2$player1
player2vector <- STK22_QTg2$player2
STK22_QTg3 <- STK22_QTg2
STK22_QTg3$p1inp2vec <- is.element(STK22_QTg3$player1, player2vector)
STK22_QTg3$p2inp1vec <- is.element(STK22_QTg3$player2, player1vector)

addPlayer1 <- STK22_QTg3[ which(STK22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK22_QTg3[ which(STK22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK22_QTg2 <- rbind(STK22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
STK22_QTft <- ftable(STK22_QTg2$player1, STK22_QTg2$player2)
STK22_QTft2 <- as.matrix(STK22_QTft)
numRows <- nrow(STK22_QTft2)
numCols <- ncol(STK22_QTft2)
STK22_QTft3 <- STK22_QTft2[c(2:numRows) , c(2:numCols)]
STK22_QTTable <- graph.adjacency(STK22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(STK22_QTTable, vertex.label = V(STK22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
STK22_QT.clusterCoef <- transitivity(STK22_QTTable, type="global") #cluster coefficient
STK22_QT.degreeCent <- centralization.degree(STK22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK22_QTftn <- as.network.matrix(STK22_QTft)
STK22_QT.netDensity <- network.density(STK22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK22_QT.entropy <- entropy(STK22_QTft) #entropy

STK22_QT.netMx <- cbind(STK22_QT.netMx, STK22_QT.clusterCoef, STK22_QT.degreeCent$centralization,
                        STK22_QT.netDensity, STK22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK22_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************

round = 22
teamName = "SYD"
KIoutcome = "Goal_F"
SYD22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
SYD22_Gg2 <- data.frame(SYD22_G)
SYD22_Gg2 <- SYD22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_Gg2$player1
player2vector <- SYD22_Gg2$player2
SYD22_Gg3 <- SYD22_Gg2
SYD22_Gg3$p1inp2vec <- is.element(SYD22_Gg3$player1, player2vector)
SYD22_Gg3$p2inp1vec <- is.element(SYD22_Gg3$player2, player1vector)

addPlayer1 <- SYD22_Gg3[ which(SYD22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD22_Gg3[ which(SYD22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_Gg2 <- rbind(SYD22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
SYD22_Gft <- ftable(SYD22_Gg2$player1, SYD22_Gg2$player2)
SYD22_Gft2 <- as.matrix(SYD22_Gft)
numRows <- nrow(SYD22_Gft2)
numCols <- ncol(SYD22_Gft2)
SYD22_Gft3 <- SYD22_Gft2[c(2:numRows) , c(2:numCols)]
SYD22_GTable <- graph.adjacency(SYD22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(SYD22_GTable, vertex.label = V(SYD22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
SYD22_G.clusterCoef <- transitivity(SYD22_GTable, type="global") #cluster coefficient
SYD22_G.degreeCent <- centralization.degree(SYD22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_Gftn <- as.network.matrix(SYD22_Gft)
SYD22_G.netDensity <- network.density(SYD22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_G.entropy <- entropy(SYD22_Gft) #entropy

SYD22_G.netMx <- cbind(SYD22_G.netMx, SYD22_G.clusterCoef, SYD22_G.degreeCent$centralization,
                       SYD22_G.netDensity, SYD22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************

round = 22
teamName = "SYD"
KIoutcome = "Behind_F"
SYD22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
SYD22_Bg2 <- data.frame(SYD22_B)
SYD22_Bg2 <- SYD22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_Bg2$player1
player2vector <- SYD22_Bg2$player2
SYD22_Bg3 <- SYD22_Bg2
SYD22_Bg3$p1inp2vec <- is.element(SYD22_Bg3$player1, player2vector)
SYD22_Bg3$p2inp1vec <- is.element(SYD22_Bg3$player2, player1vector)

addPlayer1 <- SYD22_Bg3[ which(SYD22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_Bg3[ which(SYD22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_Bg2 <- rbind(SYD22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
SYD22_Bft <- ftable(SYD22_Bg2$player1, SYD22_Bg2$player2)
SYD22_Bft2 <- as.matrix(SYD22_Bft)
numRows <- nrow(SYD22_Bft2)
numCols <- ncol(SYD22_Bft2)
SYD22_Bft3 <- SYD22_Bft2[c(2:numRows) , c(2:numCols)]
SYD22_BTable <- graph.adjacency(SYD22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(SYD22_BTable, vertex.label = V(SYD22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
SYD22_B.clusterCoef <- transitivity(SYD22_BTable, type="global") #cluster coefficient
SYD22_B.degreeCent <- centralization.degree(SYD22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_Bftn <- as.network.matrix(SYD22_Bft)
SYD22_B.netDensity <- network.density(SYD22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_B.entropy <- entropy(SYD22_Bft) #entropy

SYD22_B.netMx <- cbind(SYD22_B.netMx, SYD22_B.clusterCoef, SYD22_B.degreeCent$centralization,
                       SYD22_B.netDensity, SYD22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
SYD22_SFg2 <- data.frame(SYD22_SF)
SYD22_SFg2 <- SYD22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_SFg2$player1
player2vector <- SYD22_SFg2$player2
SYD22_SFg3 <- SYD22_SFg2
SYD22_SFg3$p1inp2vec <- is.element(SYD22_SFg3$player1, player2vector)
SYD22_SFg3$p2inp1vec <- is.element(SYD22_SFg3$player2, player1vector)

addPlayer1 <- SYD22_SFg3[ which(SYD22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_SFg3[ which(SYD22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_SFg2 <- rbind(SYD22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
SYD22_SFft <- ftable(SYD22_SFg2$player1, SYD22_SFg2$player2)
SYD22_SFft2 <- as.matrix(SYD22_SFft)
numRows <- nrow(SYD22_SFft2)
numCols <- ncol(SYD22_SFft2)
SYD22_SFft3 <- SYD22_SFft2[c(2:numRows) , c(2:numCols)]
SYD22_SFTable <- graph.adjacency(SYD22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(SYD22_SFTable, vertex.label = V(SYD22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
SYD22_SF.clusterCoef <- transitivity(SYD22_SFTable, type="global") #cluster coefficient
SYD22_SF.degreeCent <- centralization.degree(SYD22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_SFftn <- as.network.matrix(SYD22_SFft)
SYD22_SF.netDensity <- network.density(SYD22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_SF.entropy <- entropy(SYD22_SFft) #entropy

SYD22_SF.netMx <- cbind(SYD22_SF.netMx, SYD22_SF.clusterCoef, SYD22_SF.degreeCent$centralization,
                        SYD22_SF.netDensity, SYD22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
SYD22_TFg2 <- data.frame(SYD22_TF)
SYD22_TFg2 <- SYD22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_TFg2$player1
player2vector <- SYD22_TFg2$player2
SYD22_TFg3 <- SYD22_TFg2
SYD22_TFg3$p1inp2vec <- is.element(SYD22_TFg3$player1, player2vector)
SYD22_TFg3$p2inp1vec <- is.element(SYD22_TFg3$player2, player1vector)

addPlayer1 <- SYD22_TFg3[ which(SYD22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_TFg3[ which(SYD22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_TFg2 <- rbind(SYD22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
SYD22_TFft <- ftable(SYD22_TFg2$player1, SYD22_TFg2$player2)
SYD22_TFft2 <- as.matrix(SYD22_TFft)
numRows <- nrow(SYD22_TFft2)
numCols <- ncol(SYD22_TFft2)
SYD22_TFft3 <- SYD22_TFft2[c(2:numRows) , c(2:numCols)]
SYD22_TFTable <- graph.adjacency(SYD22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(SYD22_TFTable, vertex.label = V(SYD22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
SYD22_TF.clusterCoef <- transitivity(SYD22_TFTable, type="global") #cluster coefficient
SYD22_TF.degreeCent <- centralization.degree(SYD22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_TFftn <- as.network.matrix(SYD22_TFft)
SYD22_TF.netDensity <- network.density(SYD22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_TF.entropy <- entropy(SYD22_TFft) #entropy

SYD22_TF.netMx <- cbind(SYD22_TF.netMx, SYD22_TF.clusterCoef, SYD22_TF.degreeCent$centralization,
                        SYD22_TF.netDensity, SYD22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************
#NA

round = 22
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
SYD22_SAMg2 <- data.frame(SYD22_SAM)
SYD22_SAMg2 <- SYD22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_SAMg2$player1
player2vector <- SYD22_SAMg2$player2
SYD22_SAMg3 <- SYD22_SAMg2
SYD22_SAMg3$p1inp2vec <- is.element(SYD22_SAMg3$player1, player2vector)
SYD22_SAMg3$p2inp1vec <- is.element(SYD22_SAMg3$player2, player1vector)

addPlayer1 <- SYD22_SAMg3[ which(SYD22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_SAMg3[ which(SYD22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_SAMg2 <- rbind(SYD22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
SYD22_SAMft <- ftable(SYD22_SAMg2$player1, SYD22_SAMg2$player2)
SYD22_SAMft2 <- as.matrix(SYD22_SAMft)
numRows <- nrow(SYD22_SAMft2)
numCols <- ncol(SYD22_SAMft2)
SYD22_SAMft3 <- SYD22_SAMft2[c(2:numRows) , c(2:numCols)]
SYD22_SAMTable <- graph.adjacency(SYD22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(SYD22_SAMTable, vertex.label = V(SYD22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
SYD22_SAM.clusterCoef <- transitivity(SYD22_SAMTable, type="global") #cluster coefficient
SYD22_SAM.degreeCent <- centralization.degree(SYD22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_SAMftn <- as.network.matrix(SYD22_SAMft)
SYD22_SAM.netDensity <- network.density(SYD22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_SAM.entropy <- entropy(SYD22_SAMft) #entropy

SYD22_SAM.netMx <- cbind(SYD22_SAM.netMx, SYD22_SAM.clusterCoef, SYD22_SAM.degreeCent$centralization,
                         SYD22_SAM.netDensity, SYD22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
SYD22_TAMg2 <- data.frame(SYD22_TAM)
SYD22_TAMg2 <- SYD22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_TAMg2$player1
player2vector <- SYD22_TAMg2$player2
SYD22_TAMg3 <- SYD22_TAMg2
SYD22_TAMg3$p1inp2vec <- is.element(SYD22_TAMg3$player1, player2vector)
SYD22_TAMg3$p2inp1vec <- is.element(SYD22_TAMg3$player2, player1vector)

addPlayer1 <- SYD22_TAMg3[ which(SYD22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_TAMg3[ which(SYD22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_TAMg2 <- rbind(SYD22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
SYD22_TAMft <- ftable(SYD22_TAMg2$player1, SYD22_TAMg2$player2)
SYD22_TAMft2 <- as.matrix(SYD22_TAMft)
numRows <- nrow(SYD22_TAMft2)
numCols <- ncol(SYD22_TAMft2)
SYD22_TAMft3 <- SYD22_TAMft2[c(2:numRows) , c(2:numCols)]
SYD22_TAMTable <- graph.adjacency(SYD22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(SYD22_TAMTable, vertex.label = V(SYD22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
SYD22_TAM.clusterCoef <- transitivity(SYD22_TAMTable, type="global") #cluster coefficient
SYD22_TAM.degreeCent <- centralization.degree(SYD22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_TAMftn <- as.network.matrix(SYD22_TAMft)
SYD22_TAM.netDensity <- network.density(SYD22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_TAM.entropy <- entropy(SYD22_TAMft) #entropy

SYD22_TAM.netMx <- cbind(SYD22_TAM.netMx, SYD22_TAM.clusterCoef, SYD22_TAM.degreeCent$centralization,
                         SYD22_TAM.netDensity, SYD22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
SYD22_SDMg2 <- data.frame(SYD22_SDM)
SYD22_SDMg2 <- SYD22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_SDMg2$player1
player2vector <- SYD22_SDMg2$player2
SYD22_SDMg3 <- SYD22_SDMg2
SYD22_SDMg3$p1inp2vec <- is.element(SYD22_SDMg3$player1, player2vector)
SYD22_SDMg3$p2inp1vec <- is.element(SYD22_SDMg3$player2, player1vector)

addPlayer1 <- SYD22_SDMg3[ which(SYD22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_SDMg3[ which(SYD22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_SDMg2 <- rbind(SYD22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
SYD22_SDMft <- ftable(SYD22_SDMg2$player1, SYD22_SDMg2$player2)
SYD22_SDMft2 <- as.matrix(SYD22_SDMft)
numRows <- nrow(SYD22_SDMft2)
numCols <- ncol(SYD22_SDMft2)
SYD22_SDMft3 <- SYD22_SDMft2[c(2:numRows) , c(2:numCols)]
SYD22_SDMTable <- graph.adjacency(SYD22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(SYD22_SDMTable, vertex.label = V(SYD22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
SYD22_SDM.clusterCoef <- transitivity(SYD22_SDMTable, type="global") #cluster coefficient
SYD22_SDM.degreeCent <- centralization.degree(SYD22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_SDMftn <- as.network.matrix(SYD22_SDMft)
SYD22_SDM.netDensity <- network.density(SYD22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_SDM.entropy <- entropy(SYD22_SDMft) #entropy

SYD22_SDM.netMx <- cbind(SYD22_SDM.netMx, SYD22_SDM.clusterCoef, SYD22_SDM.degreeCent$centralization,
                         SYD22_SDM.netDensity, SYD22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
SYD22_TDMg2 <- data.frame(SYD22_TDM)
SYD22_TDMg2 <- SYD22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_TDMg2$player1
player2vector <- SYD22_TDMg2$player2
SYD22_TDMg3 <- SYD22_TDMg2
SYD22_TDMg3$p1inp2vec <- is.element(SYD22_TDMg3$player1, player2vector)
SYD22_TDMg3$p2inp1vec <- is.element(SYD22_TDMg3$player2, player1vector)

addPlayer1 <- SYD22_TDMg3[ which(SYD22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_TDMg3[ which(SYD22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_TDMg2 <- rbind(SYD22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
SYD22_TDMft <- ftable(SYD22_TDMg2$player1, SYD22_TDMg2$player2)
SYD22_TDMft2 <- as.matrix(SYD22_TDMft)
numRows <- nrow(SYD22_TDMft2)
numCols <- ncol(SYD22_TDMft2)
SYD22_TDMft3 <- SYD22_TDMft2[c(2:numRows) , c(2:numCols)]
SYD22_TDMTable <- graph.adjacency(SYD22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(SYD22_TDMTable, vertex.label = V(SYD22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
SYD22_TDM.clusterCoef <- transitivity(SYD22_TDMTable, type="global") #cluster coefficient
SYD22_TDM.degreeCent <- centralization.degree(SYD22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_TDMftn <- as.network.matrix(SYD22_TDMft)
SYD22_TDM.netDensity <- network.density(SYD22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_TDM.entropy <- entropy(SYD22_TDMft) #entropy

SYD22_TDM.netMx <- cbind(SYD22_TDM.netMx, SYD22_TDM.clusterCoef, SYD22_TDM.degreeCent$centralization,
                         SYD22_TDM.netDensity, SYD22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
SYD22_SDg2 <- data.frame(SYD22_SD)
SYD22_SDg2 <- SYD22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_SDg2$player1
player2vector <- SYD22_SDg2$player2
SYD22_SDg3 <- SYD22_SDg2
SYD22_SDg3$p1inp2vec <- is.element(SYD22_SDg3$player1, player2vector)
SYD22_SDg3$p2inp1vec <- is.element(SYD22_SDg3$player2, player1vector)

addPlayer1 <- SYD22_SDg3[ which(SYD22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_SDg3[ which(SYD22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_SDg2 <- rbind(SYD22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
SYD22_SDft <- ftable(SYD22_SDg2$player1, SYD22_SDg2$player2)
SYD22_SDft2 <- as.matrix(SYD22_SDft)
numRows <- nrow(SYD22_SDft2)
numCols <- ncol(SYD22_SDft2)
SYD22_SDft3 <- SYD22_SDft2[c(2:numRows) , c(2:numCols)]
SYD22_SDTable <- graph.adjacency(SYD22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(SYD22_SDTable, vertex.label = V(SYD22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
SYD22_SD.clusterCoef <- transitivity(SYD22_SDTable, type="global") #cluster coefficient
SYD22_SD.degreeCent <- centralization.degree(SYD22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_SDftn <- as.network.matrix(SYD22_SDft)
SYD22_SD.netDensity <- network.density(SYD22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_SD.entropy <- entropy(SYD22_SDft) #entropy

SYD22_SD.netMx <- cbind(SYD22_SD.netMx, SYD22_SD.clusterCoef, SYD22_SD.degreeCent$centralization,
                        SYD22_SD.netDensity, SYD22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
SYD22_TDg2 <- data.frame(SYD22_TD)
SYD22_TDg2 <- SYD22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_TDg2$player1
player2vector <- SYD22_TDg2$player2
SYD22_TDg3 <- SYD22_TDg2
SYD22_TDg3$p1inp2vec <- is.element(SYD22_TDg3$player1, player2vector)
SYD22_TDg3$p2inp1vec <- is.element(SYD22_TDg3$player2, player1vector)

addPlayer1 <- SYD22_TDg3[ which(SYD22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_TDg3[ which(SYD22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_TDg2 <- rbind(SYD22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
SYD22_TDft <- ftable(SYD22_TDg2$player1, SYD22_TDg2$player2)
SYD22_TDft2 <- as.matrix(SYD22_TDft)
numRows <- nrow(SYD22_TDft2)
numCols <- ncol(SYD22_TDft2)
SYD22_TDft3 <- SYD22_TDft2[c(2:numRows) , c(2:numCols)]
SYD22_TDTable <- graph.adjacency(SYD22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(SYD22_TDTable, vertex.label = V(SYD22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
SYD22_TD.clusterCoef <- transitivity(SYD22_TDTable, type="global") #cluster coefficient
SYD22_TD.degreeCent <- centralization.degree(SYD22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_TDftn <- as.network.matrix(SYD22_TDft)
SYD22_TD.netDensity <- network.density(SYD22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_TD.entropy <- entropy(SYD22_TDft) #entropy

SYD22_TD.netMx <- cbind(SYD22_TD.netMx, SYD22_TD.clusterCoef, SYD22_TD.degreeCent$centralization,
                        SYD22_TD.netDensity, SYD22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
SYD22_QTg2 <- data.frame(SYD22_QT)
SYD22_QTg2 <- SYD22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD22_QTg2$player1
player2vector <- SYD22_QTg2$player2
SYD22_QTg3 <- SYD22_QTg2
SYD22_QTg3$p1inp2vec <- is.element(SYD22_QTg3$player1, player2vector)
SYD22_QTg3$p2inp1vec <- is.element(SYD22_QTg3$player2, player1vector)

addPlayer1 <- SYD22_QTg3[ which(SYD22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD22_QTg3[ which(SYD22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD22_QTg2 <- rbind(SYD22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
SYD22_QTft <- ftable(SYD22_QTg2$player1, SYD22_QTg2$player2)
SYD22_QTft2 <- as.matrix(SYD22_QTft)
numRows <- nrow(SYD22_QTft2)
numCols <- ncol(SYD22_QTft2)
SYD22_QTft3 <- SYD22_QTft2[c(2:numRows) , c(2:numCols)]
SYD22_QTTable <- graph.adjacency(SYD22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(SYD22_QTTable, vertex.label = V(SYD22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
SYD22_QT.clusterCoef <- transitivity(SYD22_QTTable, type="global") #cluster coefficient
SYD22_QT.degreeCent <- centralization.degree(SYD22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD22_QTftn <- as.network.matrix(SYD22_QTft)
SYD22_QT.netDensity <- network.density(SYD22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD22_QT.entropy <- entropy(SYD22_QTft) #entropy

SYD22_QT.netMx <- cbind(SYD22_QT.netMx, SYD22_QT.clusterCoef, SYD22_QT.degreeCent$centralization,
                        SYD22_QT.netDensity, SYD22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD22_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************
#NA

round = 22
teamName = "WB"
KIoutcome = "Goal_F"
WB22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
WB22_Gg2 <- data.frame(WB22_G)
WB22_Gg2 <- WB22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_Gg2$player1
player2vector <- WB22_Gg2$player2
WB22_Gg3 <- WB22_Gg2
WB22_Gg3$p1inp2vec <- is.element(WB22_Gg3$player1, player2vector)
WB22_Gg3$p2inp1vec <- is.element(WB22_Gg3$player2, player1vector)

addPlayer1 <- WB22_Gg3[ which(WB22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_Gg3[ which(WB22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_Gg2 <- rbind(WB22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
WB22_Gft <- ftable(WB22_Gg2$player1, WB22_Gg2$player2)
WB22_Gft2 <- as.matrix(WB22_Gft)
numRows <- nrow(WB22_Gft2)
numCols <- ncol(WB22_Gft2)
WB22_Gft3 <- WB22_Gft2[c(2:numRows) , c(2:numCols)]
WB22_GTable <- graph.adjacency(WB22_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(WB22_GTable, vertex.label = V(WB22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
WB22_G.clusterCoef <- transitivity(WB22_GTable, type="global") #cluster coefficient
WB22_G.degreeCent <- centralization.degree(WB22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_Gftn <- as.network.matrix(WB22_Gft)
WB22_G.netDensity <- network.density(WB22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_G.entropy <- entropy(WB22_Gft) #entropy

WB22_G.netMx <- cbind(WB22_G.netMx, WB22_G.clusterCoef, WB22_G.degreeCent$centralization,
                      WB22_G.netDensity, WB22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************
#NA

round = 22
teamName = "WB"
KIoutcome = "Behind_F"
WB22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
WB22_Bg2 <- data.frame(WB22_B)
WB22_Bg2 <- WB22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_Bg2$player1
player2vector <- WB22_Bg2$player2
WB22_Bg3 <- WB22_Bg2
WB22_Bg3$p1inp2vec <- is.element(WB22_Bg3$player1, player2vector)
WB22_Bg3$p2inp1vec <- is.element(WB22_Bg3$player2, player1vector)

addPlayer1 <- WB22_Bg3[ which(WB22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_Bg3[ which(WB22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_Bg2 <- rbind(WB22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
WB22_Bft <- ftable(WB22_Bg2$player1, WB22_Bg2$player2)
WB22_Bft2 <- as.matrix(WB22_Bft)
numRows <- nrow(WB22_Bft2)
numCols <- ncol(WB22_Bft2)
WB22_Bft3 <- WB22_Bft2[c(2:numRows) , c(2:numCols)]
WB22_BTable <- graph.adjacency(WB22_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(WB22_BTable, vertex.label = V(WB22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
WB22_B.clusterCoef <- transitivity(WB22_BTable, type="global") #cluster coefficient
WB22_B.degreeCent <- centralization.degree(WB22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_Bftn <- as.network.matrix(WB22_Bft)
WB22_B.netDensity <- network.density(WB22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_B.entropy <- entropy(WB22_Bft) #entropy

WB22_B.netMx <- cbind(WB22_B.netMx, WB22_B.clusterCoef, WB22_B.degreeCent$centralization,
                      WB22_B.netDensity, WB22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Stoppage_F"
WB22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
WB22_SFg2 <- data.frame(WB22_SF)
WB22_SFg2 <- WB22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_SFg2$player1
player2vector <- WB22_SFg2$player2
WB22_SFg3 <- WB22_SFg2
WB22_SFg3$p1inp2vec <- is.element(WB22_SFg3$player1, player2vector)
WB22_SFg3$p2inp1vec <- is.element(WB22_SFg3$player2, player1vector)

addPlayer1 <- WB22_SFg3[ which(WB22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB22_SFg3[ which(WB22_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_SFg2 <- rbind(WB22_SFg2, addPlayers)

#ROUND 22, FWD Stoppage graph using weighted edges
WB22_SFft <- ftable(WB22_SFg2$player1, WB22_SFg2$player2)
WB22_SFft2 <- as.matrix(WB22_SFft)
numRows <- nrow(WB22_SFft2)
numCols <- ncol(WB22_SFft2)
WB22_SFft3 <- WB22_SFft2[c(2:numRows) , c(2:numCols)]
WB22_SFTable <- graph.adjacency(WB22_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(WB22_SFTable, vertex.label = V(WB22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
WB22_SF.clusterCoef <- transitivity(WB22_SFTable, type="global") #cluster coefficient
WB22_SF.degreeCent <- centralization.degree(WB22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_SFftn <- as.network.matrix(WB22_SFft)
WB22_SF.netDensity <- network.density(WB22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_SF.entropy <- entropy(WB22_SFft) #entropy

WB22_SF.netMx <- cbind(WB22_SF.netMx, WB22_SF.clusterCoef, WB22_SF.degreeCent$centralization,
                       WB22_SF.netDensity, WB22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Turnover_F"
WB22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
WB22_TFg2 <- data.frame(WB22_TF)
WB22_TFg2 <- WB22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_TFg2$player1
player2vector <- WB22_TFg2$player2
WB22_TFg3 <- WB22_TFg2
WB22_TFg3$p1inp2vec <- is.element(WB22_TFg3$player1, player2vector)
WB22_TFg3$p2inp1vec <- is.element(WB22_TFg3$player2, player1vector)

addPlayer1 <- WB22_TFg3[ which(WB22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_TFg3[ which(WB22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_TFg2 <- rbind(WB22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
WB22_TFft <- ftable(WB22_TFg2$player1, WB22_TFg2$player2)
WB22_TFft2 <- as.matrix(WB22_TFft)
numRows <- nrow(WB22_TFft2)
numCols <- ncol(WB22_TFft2)
WB22_TFft3 <- WB22_TFft2[c(2:numRows) , c(2:numCols)]
WB22_TFTable <- graph.adjacency(WB22_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(WB22_TFTable, vertex.label = V(WB22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
WB22_TF.clusterCoef <- transitivity(WB22_TFTable, type="global") #cluster coefficient
WB22_TF.degreeCent <- centralization.degree(WB22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_TFftn <- as.network.matrix(WB22_TFft)
WB22_TF.netDensity <- network.density(WB22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_TF.entropy <- entropy(WB22_TFft) #entropy

WB22_TF.netMx <- cbind(WB22_TF.netMx, WB22_TF.clusterCoef, WB22_TF.degreeCent$centralization,
                       WB22_TF.netDensity, WB22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
WB22_SAMg2 <- data.frame(WB22_SAM)
WB22_SAMg2 <- WB22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_SAMg2$player1
player2vector <- WB22_SAMg2$player2
WB22_SAMg3 <- WB22_SAMg2
WB22_SAMg3$p1inp2vec <- is.element(WB22_SAMg3$player1, player2vector)
WB22_SAMg3$p2inp1vec <- is.element(WB22_SAMg3$player2, player1vector)

addPlayer1 <- WB22_SAMg3[ which(WB22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_SAMg3[ which(WB22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_SAMg2 <- rbind(WB22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
WB22_SAMft <- ftable(WB22_SAMg2$player1, WB22_SAMg2$player2)
WB22_SAMft2 <- as.matrix(WB22_SAMft)
numRows <- nrow(WB22_SAMft2)
numCols <- ncol(WB22_SAMft2)
WB22_SAMft3 <- WB22_SAMft2[c(2:numRows) , c(2:numCols)]
WB22_SAMTable <- graph.adjacency(WB22_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(WB22_SAMTable, vertex.label = V(WB22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
WB22_SAM.clusterCoef <- transitivity(WB22_SAMTable, type="global") #cluster coefficient
WB22_SAM.degreeCent <- centralization.degree(WB22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_SAMftn <- as.network.matrix(WB22_SAMft)
WB22_SAM.netDensity <- network.density(WB22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_SAM.entropy <- entropy(WB22_SAMft) #entropy

WB22_SAM.netMx <- cbind(WB22_SAM.netMx, WB22_SAM.clusterCoef, WB22_SAM.degreeCent$centralization,
                        WB22_SAM.netDensity, WB22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Turnover_AM"
WB22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
WB22_TAMg2 <- data.frame(WB22_TAM)
WB22_TAMg2 <- WB22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_TAMg2$player1
player2vector <- WB22_TAMg2$player2
WB22_TAMg3 <- WB22_TAMg2
WB22_TAMg3$p1inp2vec <- is.element(WB22_TAMg3$player1, player2vector)
WB22_TAMg3$p2inp1vec <- is.element(WB22_TAMg3$player2, player1vector)

addPlayer1 <- WB22_TAMg3[ which(WB22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_TAMg3[ which(WB22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_TAMg2 <- rbind(WB22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
WB22_TAMft <- ftable(WB22_TAMg2$player1, WB22_TAMg2$player2)
WB22_TAMft2 <- as.matrix(WB22_TAMft)
numRows <- nrow(WB22_TAMft2)
numCols <- ncol(WB22_TAMft2)
WB22_TAMft3 <- WB22_TAMft2[c(2:numRows) , c(2:numCols)]
WB22_TAMTable <- graph.adjacency(WB22_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(WB22_TAMTable, vertex.label = V(WB22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
WB22_TAM.clusterCoef <- transitivity(WB22_TAMTable, type="global") #cluster coefficient
WB22_TAM.degreeCent <- centralization.degree(WB22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_TAMftn <- as.network.matrix(WB22_TAMft)
WB22_TAM.netDensity <- network.density(WB22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_TAM.entropy <- entropy(WB22_TAMft) #entropy

WB22_TAM.netMx <- cbind(WB22_TAM.netMx, WB22_TAM.clusterCoef, WB22_TAM.degreeCent$centralization,
                        WB22_TAM.netDensity, WB22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
WB22_SDMg2 <- data.frame(WB22_SDM)
WB22_SDMg2 <- WB22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_SDMg2$player1
player2vector <- WB22_SDMg2$player2
WB22_SDMg3 <- WB22_SDMg2
WB22_SDMg3$p1inp2vec <- is.element(WB22_SDMg3$player1, player2vector)
WB22_SDMg3$p2inp1vec <- is.element(WB22_SDMg3$player2, player1vector)

addPlayer1 <- WB22_SDMg3[ which(WB22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB22_SDMg3[ which(WB22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_SDMg2 <- rbind(WB22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
WB22_SDMft <- ftable(WB22_SDMg2$player1, WB22_SDMg2$player2)
WB22_SDMft2 <- as.matrix(WB22_SDMft)
numRows <- nrow(WB22_SDMft2)
numCols <- ncol(WB22_SDMft2)
WB22_SDMft3 <- WB22_SDMft2[c(2:numRows) , c(2:numCols)]
WB22_SDMTable <- graph.adjacency(WB22_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(WB22_SDMTable, vertex.label = V(WB22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
WB22_SDM.clusterCoef <- transitivity(WB22_SDMTable, type="global") #cluster coefficient
WB22_SDM.degreeCent <- centralization.degree(WB22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_SDMftn <- as.network.matrix(WB22_SDMft)
WB22_SDM.netDensity <- network.density(WB22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_SDM.entropy <- entropy(WB22_SDMft) #entropy

WB22_SDM.netMx <- cbind(WB22_SDM.netMx, WB22_SDM.clusterCoef, WB22_SDM.degreeCent$centralization,
                        WB22_SDM.netDensity, WB22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Turnover_DM"
WB22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
WB22_TDMg2 <- data.frame(WB22_TDM)
WB22_TDMg2 <- WB22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_TDMg2$player1
player2vector <- WB22_TDMg2$player2
WB22_TDMg3 <- WB22_TDMg2
WB22_TDMg3$p1inp2vec <- is.element(WB22_TDMg3$player1, player2vector)
WB22_TDMg3$p2inp1vec <- is.element(WB22_TDMg3$player2, player1vector)

addPlayer1 <- WB22_TDMg3[ which(WB22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_TDMg3[ which(WB22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_TDMg2 <- rbind(WB22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
WB22_TDMft <- ftable(WB22_TDMg2$player1, WB22_TDMg2$player2)
WB22_TDMft2 <- as.matrix(WB22_TDMft)
numRows <- nrow(WB22_TDMft2)
numCols <- ncol(WB22_TDMft2)
WB22_TDMft3 <- WB22_TDMft2[c(2:numRows) , c(2:numCols)]
WB22_TDMTable <- graph.adjacency(WB22_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(WB22_TDMTable, vertex.label = V(WB22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
WB22_TDM.clusterCoef <- transitivity(WB22_TDMTable, type="global") #cluster coefficient
WB22_TDM.degreeCent <- centralization.degree(WB22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_TDMftn <- as.network.matrix(WB22_TDMft)
WB22_TDM.netDensity <- network.density(WB22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_TDM.entropy <- entropy(WB22_TDMft) #entropy

WB22_TDM.netMx <- cbind(WB22_TDM.netMx, WB22_TDM.clusterCoef, WB22_TDM.degreeCent$centralization,
                        WB22_TDM.netDensity, WB22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "WB"
KIoutcome = "Stoppage_D"
WB22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
WB22_SDg2 <- data.frame(WB22_SD)
WB22_SDg2 <- WB22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_SDg2$player1
player2vector <- WB22_SDg2$player2
WB22_SDg3 <- WB22_SDg2
WB22_SDg3$p1inp2vec <- is.element(WB22_SDg3$player1, player2vector)
WB22_SDg3$p2inp1vec <- is.element(WB22_SDg3$player2, player1vector)

addPlayer1 <- WB22_SDg3[ which(WB22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_SDg3[ which(WB22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_SDg2 <- rbind(WB22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
WB22_SDft <- ftable(WB22_SDg2$player1, WB22_SDg2$player2)
WB22_SDft2 <- as.matrix(WB22_SDft)
numRows <- nrow(WB22_SDft2)
numCols <- ncol(WB22_SDft2)
WB22_SDft3 <- WB22_SDft2[c(2:numRows) , c(2:numCols)]
WB22_SDTable <- graph.adjacency(WB22_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(WB22_SDTable, vertex.label = V(WB22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
WB22_SD.clusterCoef <- transitivity(WB22_SDTable, type="global") #cluster coefficient
WB22_SD.degreeCent <- centralization.degree(WB22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_SDftn <- as.network.matrix(WB22_SDft)
WB22_SD.netDensity <- network.density(WB22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_SD.entropy <- entropy(WB22_SDft) #entropy

WB22_SD.netMx <- cbind(WB22_SD.netMx, WB22_SD.clusterCoef, WB22_SD.degreeCent$centralization,
                       WB22_SD.netDensity, WB22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************

round = 22
teamName = "WB"
KIoutcome = "Turnover_D"
WB22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
WB22_TDg2 <- data.frame(WB22_TD)
WB22_TDg2 <- WB22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_TDg2$player1
player2vector <- WB22_TDg2$player2
WB22_TDg3 <- WB22_TDg2
WB22_TDg3$p1inp2vec <- is.element(WB22_TDg3$player1, player2vector)
WB22_TDg3$p2inp1vec <- is.element(WB22_TDg3$player2, player1vector)

addPlayer1 <- WB22_TDg3[ which(WB22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_TDg3[ which(WB22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_TDg2 <- rbind(WB22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
WB22_TDft <- ftable(WB22_TDg2$player1, WB22_TDg2$player2)
WB22_TDft2 <- as.matrix(WB22_TDft)
numRows <- nrow(WB22_TDft2)
numCols <- ncol(WB22_TDft2)
WB22_TDft3 <- WB22_TDft2[c(2:numRows) , c(2:numCols)]
WB22_TDTable <- graph.adjacency(WB22_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(WB22_TDTable, vertex.label = V(WB22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
WB22_TD.clusterCoef <- transitivity(WB22_TDTable, type="global") #cluster coefficient
WB22_TD.degreeCent <- centralization.degree(WB22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_TDftn <- as.network.matrix(WB22_TDft)
WB22_TD.netDensity <- network.density(WB22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_TD.entropy <- entropy(WB22_TDft) #entropy

WB22_TD.netMx <- cbind(WB22_TD.netMx, WB22_TD.clusterCoef, WB22_TD.degreeCent$centralization,
                       WB22_TD.netDensity, WB22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
WB22_QTg2 <- data.frame(WB22_QT)
WB22_QTg2 <- WB22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB22_QTg2$player1
player2vector <- WB22_QTg2$player2
WB22_QTg3 <- WB22_QTg2
WB22_QTg3$p1inp2vec <- is.element(WB22_QTg3$player1, player2vector)
WB22_QTg3$p2inp1vec <- is.element(WB22_QTg3$player2, player1vector)

addPlayer1 <- WB22_QTg3[ which(WB22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB22_QTg3[ which(WB22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB22_QTg2 <- rbind(WB22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
WB22_QTft <- ftable(WB22_QTg2$player1, WB22_QTg2$player2)
WB22_QTft2 <- as.matrix(WB22_QTft)
numRows <- nrow(WB22_QTft2)
numCols <- ncol(WB22_QTft2)
WB22_QTft3 <- WB22_QTft2[c(2:numRows) , c(2:numCols)]
WB22_QTTable <- graph.adjacency(WB22_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(WB22_QTTable, vertex.label = V(WB22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
WB22_QT.clusterCoef <- transitivity(WB22_QTTable, type="global") #cluster coefficient
WB22_QT.degreeCent <- centralization.degree(WB22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB22_QTftn <- as.network.matrix(WB22_QTft)
WB22_QT.netDensity <- network.density(WB22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB22_QT.entropy <- entropy(WB22_QTft) #entropy

WB22_QT.netMx <- cbind(WB22_QT.netMx, WB22_QT.clusterCoef, WB22_QT.degreeCent$centralization,
                       WB22_QT.netDensity, WB22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB22_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 22
##

#ROUND 22, Goal***************************************************************

round = 22
teamName = "WCE"
KIoutcome = "Goal_F"
WCE22_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Goal with weighted edges
WCE22_Gg2 <- data.frame(WCE22_G)
WCE22_Gg2 <- WCE22_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_Gg2$player1
player2vector <- WCE22_Gg2$player2
WCE22_Gg3 <- WCE22_Gg2
WCE22_Gg3$p1inp2vec <- is.element(WCE22_Gg3$player1, player2vector)
WCE22_Gg3$p2inp1vec <- is.element(WCE22_Gg3$player2, player1vector)

addPlayer1 <- WCE22_Gg3[ which(WCE22_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_Gg3[ which(WCE22_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_Gg2 <- rbind(WCE22_Gg2, addPlayers)

#ROUND 22, Goal graph using weighted edges
WCE22_Gft <- ftable(WCE22_Gg2$player1, WCE22_Gg2$player2)
WCE22_Gft2 <- as.matrix(WCE22_Gft)
numRows <- nrow(WCE22_Gft2)
numCols <- ncol(WCE22_Gft2)
WCE22_Gft3 <- WCE22_Gft2[c(2:numRows) , c(2:numCols)]
WCE22_GTable <- graph.adjacency(WCE22_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Goal graph=weighted
plot.igraph(WCE22_GTable, vertex.label = V(WCE22_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Goal calulation of network metrics
#igraph
WCE22_G.clusterCoef <- transitivity(WCE22_GTable, type="global") #cluster coefficient
WCE22_G.degreeCent <- centralization.degree(WCE22_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_Gftn <- as.network.matrix(WCE22_Gft)
WCE22_G.netDensity <- network.density(WCE22_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_G.entropy <- entropy(WCE22_Gft) #entropy

WCE22_G.netMx <- cbind(WCE22_G.netMx, WCE22_G.clusterCoef, WCE22_G.degreeCent$centralization,
                       WCE22_G.netDensity, WCE22_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_G.netMx) <- varnames

#ROUND 22, Behind***************************************************************

round = 22
teamName = "WCE"
KIoutcome = "Behind_F"
WCE22_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, Behind with weighted edges
WCE22_Bg2 <- data.frame(WCE22_B)
WCE22_Bg2 <- WCE22_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_Bg2$player1
player2vector <- WCE22_Bg2$player2
WCE22_Bg3 <- WCE22_Bg2
WCE22_Bg3$p1inp2vec <- is.element(WCE22_Bg3$player1, player2vector)
WCE22_Bg3$p2inp1vec <- is.element(WCE22_Bg3$player2, player1vector)

addPlayer1 <- WCE22_Bg3[ which(WCE22_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_Bg3[ which(WCE22_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_Bg2 <- rbind(WCE22_Bg2, addPlayers)

#ROUND 22, Behind graph using weighted edges
WCE22_Bft <- ftable(WCE22_Bg2$player1, WCE22_Bg2$player2)
WCE22_Bft2 <- as.matrix(WCE22_Bft)
numRows <- nrow(WCE22_Bft2)
numCols <- ncol(WCE22_Bft2)
WCE22_Bft3 <- WCE22_Bft2[c(2:numRows) , c(2:numCols)]
WCE22_BTable <- graph.adjacency(WCE22_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 22, Behind graph=weighted
plot.igraph(WCE22_BTable, vertex.label = V(WCE22_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, Behind calulation of network metrics
#igraph
WCE22_B.clusterCoef <- transitivity(WCE22_BTable, type="global") #cluster coefficient
WCE22_B.degreeCent <- centralization.degree(WCE22_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_Bftn <- as.network.matrix(WCE22_Bft)
WCE22_B.netDensity <- network.density(WCE22_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_B.entropy <- entropy(WCE22_Bft) #entropy

WCE22_B.netMx <- cbind(WCE22_B.netMx, WCE22_B.clusterCoef, WCE22_B.degreeCent$centralization,
                       WCE22_B.netDensity, WCE22_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_B.netMx) <- varnames

#ROUND 22, FWD Stoppage**********************************************************
#NA

round = 22
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE22_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Stoppage with weighted edges
WCE22_SFg2 <- data.frame(WCE22_SF)
WCE22_SFg2 <- WCE22_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_SFg2$player1
player2vector <- WCE22_SFg2$player2
WCE22_SFg3 <- WCE22_SFg2
WCE22_SFg3$p1inp2vec <- is.element(WCE22_SFg3$player1, player2vector)
WCE22_SFg3$p2inp1vec <- is.element(WCE22_SFg3$player2, player1vector)

addPlayer1 <- WCE22_SFg3[ which(WCE22_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE22_SFg2 <- rbind(WCE22_SFg2, addPlayer1)

#ROUND 22, FWD Stoppage graph using weighted edges
WCE22_SFft <- ftable(WCE22_SFg2$player1, WCE22_SFg2$player2)
WCE22_SFft2 <- as.matrix(WCE22_SFft)
numRows <- nrow(WCE22_SFft2)
numCols <- ncol(WCE22_SFft2)
WCE22_SFft3 <- WCE22_SFft2[c(2:numRows) , c(1:numCols)]
WCE22_SFTable <- graph.adjacency(WCE22_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Stoppage graph=weighted
plot.igraph(WCE22_SFTable, vertex.label = V(WCE22_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Stoppage calulation of network metrics
#igraph
WCE22_SF.clusterCoef <- transitivity(WCE22_SFTable, type="global") #cluster coefficient
WCE22_SF.degreeCent <- centralization.degree(WCE22_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_SFftn <- as.network.matrix(WCE22_SFft)
WCE22_SF.netDensity <- network.density(WCE22_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_SF.entropy <- entropy(WCE22_SFft) #entropy

WCE22_SF.netMx <- cbind(WCE22_SF.netMx, WCE22_SF.clusterCoef, WCE22_SF.degreeCent$centralization,
                        WCE22_SF.netDensity, WCE22_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_SF.netMx) <- varnames

#ROUND 22, FWD Turnover**********************************************************

round = 22
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE22_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, FWD Turnover with weighted edges
WCE22_TFg2 <- data.frame(WCE22_TF)
WCE22_TFg2 <- WCE22_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_TFg2$player1
player2vector <- WCE22_TFg2$player2
WCE22_TFg3 <- WCE22_TFg2
WCE22_TFg3$p1inp2vec <- is.element(WCE22_TFg3$player1, player2vector)
WCE22_TFg3$p2inp1vec <- is.element(WCE22_TFg3$player2, player1vector)

addPlayer1 <- WCE22_TFg3[ which(WCE22_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE22_TFg3[ which(WCE22_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_TFg2 <- rbind(WCE22_TFg2, addPlayers)

#ROUND 22, FWD Turnover graph using weighted edges
WCE22_TFft <- ftable(WCE22_TFg2$player1, WCE22_TFg2$player2)
WCE22_TFft2 <- as.matrix(WCE22_TFft)
numRows <- nrow(WCE22_TFft2)
numCols <- ncol(WCE22_TFft2)
WCE22_TFft3 <- WCE22_TFft2[c(2:numRows) , c(2:numCols)]
WCE22_TFTable <- graph.adjacency(WCE22_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, FWD Turnover graph=weighted
plot.igraph(WCE22_TFTable, vertex.label = V(WCE22_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, FWD Turnover calulation of network metrics
#igraph
WCE22_TF.clusterCoef <- transitivity(WCE22_TFTable, type="global") #cluster coefficient
WCE22_TF.degreeCent <- centralization.degree(WCE22_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_TFftn <- as.network.matrix(WCE22_TFft)
WCE22_TF.netDensity <- network.density(WCE22_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_TF.entropy <- entropy(WCE22_TFft) #entropy

WCE22_TF.netMx <- cbind(WCE22_TF.netMx, WCE22_TF.clusterCoef, WCE22_TF.degreeCent$centralization,
                        WCE22_TF.netDensity, WCE22_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_TF.netMx) <- varnames

#ROUND 22, AM Stoppage**********************************************************

round = 22
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE22_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Stoppage with weighted edges
WCE22_SAMg2 <- data.frame(WCE22_SAM)
WCE22_SAMg2 <- WCE22_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_SAMg2$player1
player2vector <- WCE22_SAMg2$player2
WCE22_SAMg3 <- WCE22_SAMg2
WCE22_SAMg3$p1inp2vec <- is.element(WCE22_SAMg3$player1, player2vector)
WCE22_SAMg3$p2inp1vec <- is.element(WCE22_SAMg3$player2, player1vector)

addPlayer1 <- WCE22_SAMg3[ which(WCE22_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_SAMg3[ which(WCE22_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_SAMg2 <- rbind(WCE22_SAMg2, addPlayers)

#ROUND 22, AM Stoppage graph using weighted edges
WCE22_SAMft <- ftable(WCE22_SAMg2$player1, WCE22_SAMg2$player2)
WCE22_SAMft2 <- as.matrix(WCE22_SAMft)
numRows <- nrow(WCE22_SAMft2)
numCols <- ncol(WCE22_SAMft2)
WCE22_SAMft3 <- WCE22_SAMft2[c(2:numRows) , c(2:numCols)]
WCE22_SAMTable <- graph.adjacency(WCE22_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Stoppage graph=weighted
plot.igraph(WCE22_SAMTable, vertex.label = V(WCE22_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Stoppage calulation of network metrics
#igraph
WCE22_SAM.clusterCoef <- transitivity(WCE22_SAMTable, type="global") #cluster coefficient
WCE22_SAM.degreeCent <- centralization.degree(WCE22_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_SAMftn <- as.network.matrix(WCE22_SAMft)
WCE22_SAM.netDensity <- network.density(WCE22_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_SAM.entropy <- entropy(WCE22_SAMft) #entropy

WCE22_SAM.netMx <- cbind(WCE22_SAM.netMx, WCE22_SAM.clusterCoef, WCE22_SAM.degreeCent$centralization,
                         WCE22_SAM.netDensity, WCE22_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_SAM.netMx) <- varnames

#ROUND 22, AM Turnover**********************************************************

round = 22
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE22_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, AM Turnover with weighted edges
WCE22_TAMg2 <- data.frame(WCE22_TAM)
WCE22_TAMg2 <- WCE22_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_TAMg2$player1
player2vector <- WCE22_TAMg2$player2
WCE22_TAMg3 <- WCE22_TAMg2
WCE22_TAMg3$p1inp2vec <- is.element(WCE22_TAMg3$player1, player2vector)
WCE22_TAMg3$p2inp1vec <- is.element(WCE22_TAMg3$player2, player1vector)

addPlayer1 <- WCE22_TAMg3[ which(WCE22_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE22_TAMg3[ which(WCE22_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_TAMg2 <- rbind(WCE22_TAMg2, addPlayers)

#ROUND 22, AM Turnover graph using weighted edges
WCE22_TAMft <- ftable(WCE22_TAMg2$player1, WCE22_TAMg2$player2)
WCE22_TAMft2 <- as.matrix(WCE22_TAMft)
numRows <- nrow(WCE22_TAMft2)
numCols <- ncol(WCE22_TAMft2)
WCE22_TAMft3 <- WCE22_TAMft2[c(2:numRows) , c(2:numCols)]
WCE22_TAMTable <- graph.adjacency(WCE22_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, AM Turnover graph=weighted
plot.igraph(WCE22_TAMTable, vertex.label = V(WCE22_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, AM Turnover calulation of network metrics
#igraph
WCE22_TAM.clusterCoef <- transitivity(WCE22_TAMTable, type="global") #cluster coefficient
WCE22_TAM.degreeCent <- centralization.degree(WCE22_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_TAMftn <- as.network.matrix(WCE22_TAMft)
WCE22_TAM.netDensity <- network.density(WCE22_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_TAM.entropy <- entropy(WCE22_TAMft) #entropy

WCE22_TAM.netMx <- cbind(WCE22_TAM.netMx, WCE22_TAM.clusterCoef, WCE22_TAM.degreeCent$centralization,
                         WCE22_TAM.netDensity, WCE22_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_TAM.netMx) <- varnames

#ROUND 22, DM Stoppage**********************************************************
#NA

round = 22
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE22_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Stoppage with weighted edges
WCE22_SDMg2 <- data.frame(WCE22_SDM)
WCE22_SDMg2 <- WCE22_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_SDMg2$player1
player2vector <- WCE22_SDMg2$player2
WCE22_SDMg3 <- WCE22_SDMg2
WCE22_SDMg3$p1inp2vec <- is.element(WCE22_SDMg3$player1, player2vector)
WCE22_SDMg3$p2inp1vec <- is.element(WCE22_SDMg3$player2, player1vector)

addPlayer1 <- WCE22_SDMg3[ which(WCE22_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_SDMg3[ which(WCE22_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_SDMg2 <- rbind(WCE22_SDMg2, addPlayers)

#ROUND 22, DM Stoppage graph using weighted edges
WCE22_SDMft <- ftable(WCE22_SDMg2$player1, WCE22_SDMg2$player2)
WCE22_SDMft2 <- as.matrix(WCE22_SDMft)
numRows <- nrow(WCE22_SDMft2)
numCols <- ncol(WCE22_SDMft2)
WCE22_SDMft3 <- WCE22_SDMft2[c(2:numRows) , c(2:numCols)]
WCE22_SDMTable <- graph.adjacency(WCE22_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Stoppage graph=weighted
plot.igraph(WCE22_SDMTable, vertex.label = V(WCE22_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Stoppage calulation of network metrics
#igraph
WCE22_SDM.clusterCoef <- transitivity(WCE22_SDMTable, type="global") #cluster coefficient
WCE22_SDM.degreeCent <- centralization.degree(WCE22_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_SDMftn <- as.network.matrix(WCE22_SDMft)
WCE22_SDM.netDensity <- network.density(WCE22_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_SDM.entropy <- entropy(WCE22_SDMft) #entropy

WCE22_SDM.netMx <- cbind(WCE22_SDM.netMx, WCE22_SDM.clusterCoef, WCE22_SDM.degreeCent$centralization,
                         WCE22_SDM.netDensity, WCE22_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_SDM.netMx) <- varnames

#ROUND 22, DM Turnover**********************************************************
#NA

round = 22
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE22_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, DM Turnover with weighted edges
WCE22_TDMg2 <- data.frame(WCE22_TDM)
WCE22_TDMg2 <- WCE22_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_TDMg2$player1
player2vector <- WCE22_TDMg2$player2
WCE22_TDMg3 <- WCE22_TDMg2
WCE22_TDMg3$p1inp2vec <- is.element(WCE22_TDMg3$player1, player2vector)
WCE22_TDMg3$p2inp1vec <- is.element(WCE22_TDMg3$player2, player1vector)

addPlayer1 <- WCE22_TDMg3[ which(WCE22_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_TDMg3[ which(WCE22_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_TDMg2 <- rbind(WCE22_TDMg2, addPlayers)

#ROUND 22, DM Turnover graph using weighted edges
WCE22_TDMft <- ftable(WCE22_TDMg2$player1, WCE22_TDMg2$player2)
WCE22_TDMft2 <- as.matrix(WCE22_TDMft)
numRows <- nrow(WCE22_TDMft2)
numCols <- ncol(WCE22_TDMft2)
WCE22_TDMft3 <- WCE22_TDMft2[c(2:numRows) , c(2:numCols)]
WCE22_TDMTable <- graph.adjacency(WCE22_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 22, DM Turnover graph=weighted
plot.igraph(WCE22_TDMTable, vertex.label = V(WCE22_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, DM Turnover calulation of network metrics
#igraph
WCE22_TDM.clusterCoef <- transitivity(WCE22_TDMTable, type="global") #cluster coefficient
WCE22_TDM.degreeCent <- centralization.degree(WCE22_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_TDMftn <- as.network.matrix(WCE22_TDMft)
WCE22_TDM.netDensity <- network.density(WCE22_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_TDM.entropy <- entropy(WCE22_TDMft) #entropy

WCE22_TDM.netMx <- cbind(WCE22_TDM.netMx, WCE22_TDM.clusterCoef, WCE22_TDM.degreeCent$centralization,
                         WCE22_TDM.netDensity, WCE22_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_TDM.netMx) <- varnames

#ROUND 22, D Stoppage**********************************************************
#NA

round = 22
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE22_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Stoppage with weighted edges
WCE22_SDg2 <- data.frame(WCE22_SD)
WCE22_SDg2 <- WCE22_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_SDg2$player1
player2vector <- WCE22_SDg2$player2
WCE22_SDg3 <- WCE22_SDg2
WCE22_SDg3$p1inp2vec <- is.element(WCE22_SDg3$player1, player2vector)
WCE22_SDg3$p2inp1vec <- is.element(WCE22_SDg3$player2, player1vector)

addPlayer1 <- WCE22_SDg3[ which(WCE22_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_SDg3[ which(WCE22_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_SDg2 <- rbind(WCE22_SDg2, addPlayers)

#ROUND 22, D Stoppage graph using weighted edges
WCE22_SDft <- ftable(WCE22_SDg2$player1, WCE22_SDg2$player2)
WCE22_SDft2 <- as.matrix(WCE22_SDft)
numRows <- nrow(WCE22_SDft2)
numCols <- ncol(WCE22_SDft2)
WCE22_SDft3 <- WCE22_SDft2[c(2:numRows) , c(2:numCols)]
WCE22_SDTable <- graph.adjacency(WCE22_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Stoppage graph=weighted
plot.igraph(WCE22_SDTable, vertex.label = V(WCE22_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Stoppage calulation of network metrics
#igraph
WCE22_SD.clusterCoef <- transitivity(WCE22_SDTable, type="global") #cluster coefficient
WCE22_SD.degreeCent <- centralization.degree(WCE22_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_SDftn <- as.network.matrix(WCE22_SDft)
WCE22_SD.netDensity <- network.density(WCE22_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_SD.entropy <- entropy(WCE22_SDft) #entropy

WCE22_SD.netMx <- cbind(WCE22_SD.netMx, WCE22_SD.clusterCoef, WCE22_SD.degreeCent$centralization,
                        WCE22_SD.netDensity, WCE22_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_SD.netMx) <- varnames

#ROUND 22, D Turnover**********************************************************
#NA

round = 22
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE22_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, D Turnover with weighted edges
WCE22_TDg2 <- data.frame(WCE22_TD)
WCE22_TDg2 <- WCE22_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_TDg2$player1
player2vector <- WCE22_TDg2$player2
WCE22_TDg3 <- WCE22_TDg2
WCE22_TDg3$p1inp2vec <- is.element(WCE22_TDg3$player1, player2vector)
WCE22_TDg3$p2inp1vec <- is.element(WCE22_TDg3$player2, player1vector)

addPlayer1 <- WCE22_TDg3[ which(WCE22_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_TDg3[ which(WCE22_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_TDg2 <- rbind(WCE22_TDg2, addPlayers)

#ROUND 22, D Turnover graph using weighted edges
WCE22_TDft <- ftable(WCE22_TDg2$player1, WCE22_TDg2$player2)
WCE22_TDft2 <- as.matrix(WCE22_TDft)
numRows <- nrow(WCE22_TDft2)
numCols <- ncol(WCE22_TDft2)
WCE22_TDft3 <- WCE22_TDft2[c(2:numRows) , c(2:numCols)]
WCE22_TDTable <- graph.adjacency(WCE22_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, D Turnover graph=weighted
plot.igraph(WCE22_TDTable, vertex.label = V(WCE22_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, D Turnover calulation of network metrics
#igraph
WCE22_TD.clusterCoef <- transitivity(WCE22_TDTable, type="global") #cluster coefficient
WCE22_TD.degreeCent <- centralization.degree(WCE22_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_TDftn <- as.network.matrix(WCE22_TDft)
WCE22_TD.netDensity <- network.density(WCE22_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_TD.entropy <- entropy(WCE22_TDft) #entropy

WCE22_TD.netMx <- cbind(WCE22_TD.netMx, WCE22_TD.clusterCoef, WCE22_TD.degreeCent$centralization,
                        WCE22_TD.netDensity, WCE22_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_TD.netMx) <- varnames

#ROUND 22, End of Qtr**********************************************************
#NA

round = 22
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE22_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 22, End of Qtr with weighted edges
WCE22_QTg2 <- data.frame(WCE22_QT)
WCE22_QTg2 <- WCE22_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE22_QTg2$player1
player2vector <- WCE22_QTg2$player2
WCE22_QTg3 <- WCE22_QTg2
WCE22_QTg3$p1inp2vec <- is.element(WCE22_QTg3$player1, player2vector)
WCE22_QTg3$p2inp1vec <- is.element(WCE22_QTg3$player2, player1vector)

addPlayer1 <- WCE22_QTg3[ which(WCE22_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE22_QTg3[ which(WCE22_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE22_QTg2 <- rbind(WCE22_QTg2, addPlayers)

#ROUND 22, End of Qtr graph using weighted edges
WCE22_QTft <- ftable(WCE22_QTg2$player1, WCE22_QTg2$player2)
WCE22_QTft2 <- as.matrix(WCE22_QTft)
numRows <- nrow(WCE22_QTft2)
numCols <- ncol(WCE22_QTft2)
WCE22_QTft3 <- WCE22_QTft2[c(2:numRows) , c(2:numCols)]
WCE22_QTTable <- graph.adjacency(WCE22_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 22, End of Qtr graph=weighted
plot.igraph(WCE22_QTTable, vertex.label = V(WCE22_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE22_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 22, End of Qtr calulation of network metrics
#igraph
WCE22_QT.clusterCoef <- transitivity(WCE22_QTTable, type="global") #cluster coefficient
WCE22_QT.degreeCent <- centralization.degree(WCE22_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE22_QTftn <- as.network.matrix(WCE22_QTft)
WCE22_QT.netDensity <- network.density(WCE22_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE22_QT.entropy <- entropy(WCE22_QTft) #entropy

WCE22_QT.netMx <- cbind(WCE22_QT.netMx, WCE22_QT.clusterCoef, WCE22_QT.degreeCent$centralization,
                        WCE22_QT.netDensity, WCE22_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE22_QT.netMx) <- varnames
