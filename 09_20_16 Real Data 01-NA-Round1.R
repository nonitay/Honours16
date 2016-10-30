#####
#09-20-16- Real data 01
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
ADEL01_Gg2 <- data.frame(ADEL01_G)
ADEL01_Gg2 <- ADEL01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_Gg2$player1
player2vector <- ADEL01_Gg2$player2
ADEL01_Gg3 <- ADEL01_Gg2
ADEL01_Gg3$p1inp2vec <- is.element(ADEL01_Gg3$player1, player2vector)
ADEL01_Gg3$p2inp1vec <- is.element(ADEL01_Gg3$player2, player1vector)

addPlayer1 <- ADEL01_Gg3[ which(ADEL01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_Gg3[ which(ADEL01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_Gg2 <- rbind(ADEL01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
ADEL01_Gft <- ftable(ADEL01_Gg2$player1, ADEL01_Gg2$player2)
ADEL01_Gft2 <- as.matrix(ADEL01_Gft)
numRows <- nrow(ADEL01_Gft2)
numCols <- ncol(ADEL01_Gft2)
ADEL01_Gft3 <- ADEL01_Gft2[c(2:numRows) , c(2:numCols)]
ADEL01_GTable <- graph.adjacency(ADEL01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(ADEL01_GTable, vertex.label = V(ADEL01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
ADEL01_G.clusterCoef <- transitivity(ADEL01_GTable, type="global") #cluster coefficient
ADEL01_G.degreeCent <- centralization.degree(ADEL01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_Gftn <- as.network.matrix(ADEL01_Gft)
ADEL01_G.netDensity <- network.density(ADEL01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_G.entropy <- entropy(ADEL01_Gft) #entropy

ADEL01_G.netMx <- cbind(ADEL01_G.netMx, ADEL01_G.clusterCoef, ADEL01_G.degreeCent$centralization,
                        ADEL01_G.netDensity, ADEL01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_G.netMx) <- varnames

#Round 1, Behind***************************************************************

round = 1
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
ADEL01_Bg2 <- data.frame(ADEL01_B)
ADEL01_Bg2 <- ADEL01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_Bg2$player1
player2vector <- ADEL01_Bg2$player2
ADEL01_Bg3 <- ADEL01_Bg2
ADEL01_Bg3$p1inp2vec <- is.element(ADEL01_Bg3$player1, player2vector)
ADEL01_Bg3$p2inp1vec <- is.element(ADEL01_Bg3$player2, player1vector)

addPlayer1 <- ADEL01_Bg3[ which(ADEL01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_Bg3[ which(ADEL01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_Bg2 <- rbind(ADEL01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
ADEL01_Bft <- ftable(ADEL01_Bg2$player1, ADEL01_Bg2$player2)
ADEL01_Bft2 <- as.matrix(ADEL01_Bft)
numRows <- nrow(ADEL01_Bft2)
numCols <- ncol(ADEL01_Bft2)
ADEL01_Bft3 <- ADEL01_Bft2[c(2:numRows) , c(2:numCols)]
ADEL01_BTable <- graph.adjacency(ADEL01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(ADEL01_BTable, vertex.label = V(ADEL01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
ADEL01_B.clusterCoef <- transitivity(ADEL01_BTable, type="global") #cluster coefficient
ADEL01_B.degreeCent <- centralization.degree(ADEL01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_Bftn <- as.network.matrix(ADEL01_Bft)
ADEL01_B.netDensity <- network.density(ADEL01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_B.entropy <- entropy(ADEL01_Bft) #entropy

ADEL01_B.netMx <- cbind(ADEL01_B.netMx, ADEL01_B.clusterCoef, ADEL01_B.degreeCent$centralization,
                        ADEL01_B.netDensity, ADEL01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
ADEL01_SFg2 <- data.frame(ADEL01_SF)
ADEL01_SFg2 <- ADEL01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_SFg2$player1
player2vector <- ADEL01_SFg2$player2
ADEL01_SFg3 <- ADEL01_SFg2
ADEL01_SFg3$p1inp2vec <- is.element(ADEL01_SFg3$player1, player2vector)
ADEL01_SFg3$p2inp1vec <- is.element(ADEL01_SFg3$player2, player1vector)

addPlayer1 <- ADEL01_SFg3[ which(ADEL01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_SFg3[ which(ADEL01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_SFg2 <- rbind(ADEL01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
ADEL01_SFft <- ftable(ADEL01_SFg2$player1, ADEL01_SFg2$player2)
ADEL01_SFft2 <- as.matrix(ADEL01_SFft)
numRows <- nrow(ADEL01_SFft2)
numCols <- ncol(ADEL01_SFft2)
ADEL01_SFft3 <- ADEL01_SFft2[c(2:numRows) , c(2:numCols)]
ADEL01_SFTable <- graph.adjacency(ADEL01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(ADEL01_SFTable, vertex.label = V(ADEL01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
ADEL01_SF.clusterCoef <- transitivity(ADEL01_SFTable, type="global") #cluster coefficient
ADEL01_SF.degreeCent <- centralization.degree(ADEL01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_SFftn <- as.network.matrix(ADEL01_SFft)
ADEL01_SF.netDensity <- network.density(ADEL01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_SF.entropy <- entropy(ADEL01_SFft) #entropy

ADEL01_SF.netMx <- cbind(ADEL01_SF.netMx, ADEL01_SF.clusterCoef, ADEL01_SF.degreeCent$centralization,
                         ADEL01_SF.netDensity, ADEL01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
ADEL01_TFg2 <- data.frame(ADEL01_TF)
ADEL01_TFg2 <- ADEL01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_TFg2$player1
player2vector <- ADEL01_TFg2$player2
ADEL01_TFg3 <- ADEL01_TFg2
ADEL01_TFg3$p1inp2vec <- is.element(ADEL01_TFg3$player1, player2vector)
ADEL01_TFg3$p2inp1vec <- is.element(ADEL01_TFg3$player2, player1vector)

addPlayer1 <- ADEL01_TFg3[ which(ADEL01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_TFg3[ which(ADEL01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_TFg2 <- rbind(ADEL01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
ADEL01_TFft <- ftable(ADEL01_TFg2$player1, ADEL01_TFg2$player2)
ADEL01_TFft2 <- as.matrix(ADEL01_TFft)
numRows <- nrow(ADEL01_TFft2)
numCols <- ncol(ADEL01_TFft2)
ADEL01_TFft3 <- ADEL01_TFft2[c(2:numRows) , c(2:numCols)]
ADEL01_TFTable <- graph.adjacency(ADEL01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(ADEL01_TFTable, vertex.label = V(ADEL01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
ADEL01_TF.clusterCoef <- transitivity(ADEL01_TFTable, type="global") #cluster coefficient
ADEL01_TF.degreeCent <- centralization.degree(ADEL01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_TFftn <- as.network.matrix(ADEL01_TFft)
ADEL01_TF.netDensity <- network.density(ADEL01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_TF.entropy <- entropy(ADEL01_TFft) #entropy

ADEL01_TF.netMx <- cbind(ADEL01_TF.netMx, ADEL01_TF.clusterCoef, ADEL01_TF.degreeCent$centralization,
                         ADEL01_TF.netDensity, ADEL01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
ADEL01_SAMg2 <- data.frame(ADEL01_SAM)
ADEL01_SAMg2 <- ADEL01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_SAMg2$player1
player2vector <- ADEL01_SAMg2$player2
ADEL01_SAMg3 <- ADEL01_SAMg2
ADEL01_SAMg3$p1inp2vec <- is.element(ADEL01_SAMg3$player1, player2vector)
ADEL01_SAMg3$p2inp1vec <- is.element(ADEL01_SAMg3$player2, player1vector)

addPlayer1 <- ADEL01_SAMg3[ which(ADEL01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_SAMg3[ which(ADEL01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_SAMg2 <- rbind(ADEL01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
ADEL01_SAMft <- ftable(ADEL01_SAMg2$player1, ADEL01_SAMg2$player2)
ADEL01_SAMft2 <- as.matrix(ADEL01_SAMft)
numRows <- nrow(ADEL01_SAMft2)
numCols <- ncol(ADEL01_SAMft2)
ADEL01_SAMft3 <- ADEL01_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL01_SAMTable <- graph.adjacency(ADEL01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(ADEL01_SAMTable, vertex.label = V(ADEL01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
ADEL01_SAM.clusterCoef <- transitivity(ADEL01_SAMTable, type="global") #cluster coefficient
ADEL01_SAM.degreeCent <- centralization.degree(ADEL01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_SAMftn <- as.network.matrix(ADEL01_SAMft)
ADEL01_SAM.netDensity <- network.density(ADEL01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_SAM.entropy <- entropy(ADEL01_SAMft) #entropy

ADEL01_SAM.netMx <- cbind(ADEL01_SAM.netMx, ADEL01_SAM.clusterCoef, ADEL01_SAM.degreeCent$centralization,
                          ADEL01_SAM.netDensity, ADEL01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
ADEL01_TAMg2 <- data.frame(ADEL01_TAM)
ADEL01_TAMg2 <- ADEL01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_TAMg2$player1
player2vector <- ADEL01_TAMg2$player2
ADEL01_TAMg3 <- ADEL01_TAMg2
ADEL01_TAMg3$p1inp2vec <- is.element(ADEL01_TAMg3$player1, player2vector)
ADEL01_TAMg3$p2inp1vec <- is.element(ADEL01_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL01_TAMg3[ which(ADEL01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL01_TAMg2 <- rbind(ADEL01_TAMg2, addPlayer2)

#Round 1, AM Turnover graph using weighted edges
ADEL01_TAMft <- ftable(ADEL01_TAMg2$player1, ADEL01_TAMg2$player2)
ADEL01_TAMft2 <- as.matrix(ADEL01_TAMft)
numRows <- nrow(ADEL01_TAMft2)
numCols <- ncol(ADEL01_TAMft2)
ADEL01_TAMft3 <- ADEL01_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL01_TAMTable <- graph.adjacency(ADEL01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(ADEL01_TAMTable, vertex.label = V(ADEL01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
ADEL01_TAM.clusterCoef <- transitivity(ADEL01_TAMTable, type="global") #cluster coefficient
ADEL01_TAM.degreeCent <- centralization.degree(ADEL01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_TAMftn <- as.network.matrix(ADEL01_TAMft)
ADEL01_TAM.netDensity <- network.density(ADEL01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_TAM.entropy <- entropy(ADEL01_TAMft) #entropy

ADEL01_TAM.netMx <- cbind(ADEL01_TAM.netMx, ADEL01_TAM.clusterCoef, ADEL01_TAM.degreeCent$centralization,
                          ADEL01_TAM.netDensity, ADEL01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
ADEL01_SDMg2 <- data.frame(ADEL01_SDM)
ADEL01_SDMg2 <- ADEL01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_SDMg2$player1
player2vector <- ADEL01_SDMg2$player2
ADEL01_SDMg3 <- ADEL01_SDMg2
ADEL01_SDMg3$p1inp2vec <- is.element(ADEL01_SDMg3$player1, player2vector)
ADEL01_SDMg3$p2inp1vec <- is.element(ADEL01_SDMg3$player2, player1vector)

addPlayer1 <- ADEL01_SDMg3[ which(ADEL01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_SDMg3[ which(ADEL01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_SDMg2 <- rbind(ADEL01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
ADEL01_SDMft <- ftable(ADEL01_SDMg2$player1, ADEL01_SDMg2$player2)
ADEL01_SDMft2 <- as.matrix(ADEL01_SDMft)
numRows <- nrow(ADEL01_SDMft2)
numCols <- ncol(ADEL01_SDMft2)
ADEL01_SDMft3 <- ADEL01_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL01_SDMTable <- graph.adjacency(ADEL01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(ADEL01_SDMTable, vertex.label = V(ADEL01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
ADEL01_SDM.clusterCoef <- transitivity(ADEL01_SDMTable, type="global") #cluster coefficient
ADEL01_SDM.degreeCent <- centralization.degree(ADEL01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_SDMftn <- as.network.matrix(ADEL01_SDMft)
ADEL01_SDM.netDensity <- network.density(ADEL01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_SDM.entropy <- entropy(ADEL01_SDMft) #entropy

ADEL01_SDM.netMx <- cbind(ADEL01_SDM.netMx, ADEL01_SDM.clusterCoef, ADEL01_SDM.degreeCent$centralization,
                          ADEL01_SDM.netDensity, ADEL01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
ADEL01_TDMg2 <- data.frame(ADEL01_TDM)
ADEL01_TDMg2 <- ADEL01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_TDMg2$player1
player2vector <- ADEL01_TDMg2$player2
ADEL01_TDMg3 <- ADEL01_TDMg2
ADEL01_TDMg3$p1inp2vec <- is.element(ADEL01_TDMg3$player1, player2vector)
ADEL01_TDMg3$p2inp1vec <- is.element(ADEL01_TDMg3$player2, player1vector)

addPlayer1 <- ADEL01_TDMg3[ which(ADEL01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_TDMg3[ which(ADEL01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_TDMg2 <- rbind(ADEL01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
ADEL01_TDMft <- ftable(ADEL01_TDMg2$player1, ADEL01_TDMg2$player2)
ADEL01_TDMft2 <- as.matrix(ADEL01_TDMft)
numRows <- nrow(ADEL01_TDMft2)
numCols <- ncol(ADEL01_TDMft2)
ADEL01_TDMft3 <- ADEL01_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL01_TDMTable <- graph.adjacency(ADEL01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(ADEL01_TDMTable, vertex.label = V(ADEL01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
ADEL01_TDM.clusterCoef <- transitivity(ADEL01_TDMTable, type="global") #cluster coefficient
ADEL01_TDM.degreeCent <- centralization.degree(ADEL01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_TDMftn <- as.network.matrix(ADEL01_TDMft)
ADEL01_TDM.netDensity <- network.density(ADEL01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_TDM.entropy <- entropy(ADEL01_TDMft) #entropy

ADEL01_TDM.netMx <- cbind(ADEL01_TDM.netMx, ADEL01_TDM.clusterCoef, ADEL01_TDM.degreeCent$centralization,
                          ADEL01_TDM.netDensity, ADEL01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
ADEL01_SDg2 <- data.frame(ADEL01_SD)
ADEL01_SDg2 <- ADEL01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_SDg2$player1
player2vector <- ADEL01_SDg2$player2
ADEL01_SDg3 <- ADEL01_SDg2
ADEL01_SDg3$p1inp2vec <- is.element(ADEL01_SDg3$player1, player2vector)
ADEL01_SDg3$p2inp1vec <- is.element(ADEL01_SDg3$player2, player1vector)

addPlayer1 <- ADEL01_SDg3[ which(ADEL01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_SDg3[ which(ADEL01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_SDg2 <- rbind(ADEL01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
ADEL01_SDft <- ftable(ADEL01_SDg2$player1, ADEL01_SDg2$player2)
ADEL01_SDft2 <- as.matrix(ADEL01_SDft)
numRows <- nrow(ADEL01_SDft2)
numCols <- ncol(ADEL01_SDft2)
ADEL01_SDft3 <- ADEL01_SDft2[c(2:numRows) , c(2:numCols)]
ADEL01_SDTable <- graph.adjacency(ADEL01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(ADEL01_SDTable, vertex.label = V(ADEL01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
ADEL01_SD.clusterCoef <- transitivity(ADEL01_SDTable, type="global") #cluster coefficient
ADEL01_SD.degreeCent <- centralization.degree(ADEL01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_SDftn <- as.network.matrix(ADEL01_SDft)
ADEL01_SD.netDensity <- network.density(ADEL01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_SD.entropy <- entropy(ADEL01_SDft) #entropy

ADEL01_SD.netMx <- cbind(ADEL01_SD.netMx, ADEL01_SD.clusterCoef, ADEL01_SD.degreeCent$centralization,
                         ADEL01_SD.netDensity, ADEL01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
ADEL01_TDg2 <- data.frame(ADEL01_TD)
ADEL01_TDg2 <- ADEL01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_TDg2$player1
player2vector <- ADEL01_TDg2$player2
ADEL01_TDg3 <- ADEL01_TDg2
ADEL01_TDg3$p1inp2vec <- is.element(ADEL01_TDg3$player1, player2vector)
ADEL01_TDg3$p2inp1vec <- is.element(ADEL01_TDg3$player2, player1vector)

addPlayer1 <- ADEL01_TDg3[ which(ADEL01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_TDg3[ which(ADEL01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_TDg2 <- rbind(ADEL01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
ADEL01_TDft <- ftable(ADEL01_TDg2$player1, ADEL01_TDg2$player2)
ADEL01_TDft2 <- as.matrix(ADEL01_TDft)
numRows <- nrow(ADEL01_TDft2)
numCols <- ncol(ADEL01_TDft2)
ADEL01_TDft3 <- ADEL01_TDft2[c(2:numRows) , c(2:numCols)]
ADEL01_TDTable <- graph.adjacency(ADEL01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(ADEL01_TDTable, vertex.label = V(ADEL01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
ADEL01_TD.clusterCoef <- transitivity(ADEL01_TDTable, type="global") #cluster coefficient
ADEL01_TD.degreeCent <- centralization.degree(ADEL01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_TDftn <- as.network.matrix(ADEL01_TDft)
ADEL01_TD.netDensity <- network.density(ADEL01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_TD.entropy <- entropy(ADEL01_TDft) #entropy

ADEL01_TD.netMx <- cbind(ADEL01_TD.netMx, ADEL01_TD.clusterCoef, ADEL01_TD.degreeCent$centralization,
                         ADEL01_TD.netDensity, ADEL01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
ADEL01_QTg2 <- data.frame(ADEL01_QT)
ADEL01_QTg2 <- ADEL01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL01_QTg2$player1
player2vector <- ADEL01_QTg2$player2
ADEL01_QTg3 <- ADEL01_QTg2
ADEL01_QTg3$p1inp2vec <- is.element(ADEL01_QTg3$player1, player2vector)
ADEL01_QTg3$p2inp1vec <- is.element(ADEL01_QTg3$player2, player1vector)

addPlayer1 <- ADEL01_QTg3[ which(ADEL01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL01_QTg3[ which(ADEL01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL01_QTg2 <- rbind(ADEL01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
ADEL01_QTft <- ftable(ADEL01_QTg2$player1, ADEL01_QTg2$player2)
ADEL01_QTft2 <- as.matrix(ADEL01_QTft)
numRows <- nrow(ADEL01_QTft2)
numCols <- ncol(ADEL01_QTft2)
ADEL01_QTft3 <- ADEL01_QTft2[c(2:numRows) , c(2:numCols)]
ADEL01_QTTable <- graph.adjacency(ADEL01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(ADEL01_QTTable, vertex.label = V(ADEL01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
ADEL01_QT.clusterCoef <- transitivity(ADEL01_QTTable, type="global") #cluster coefficient
ADEL01_QT.degreeCent <- centralization.degree(ADEL01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL01_QTftn <- as.network.matrix(ADEL01_QTft)
ADEL01_QT.netDensity <- network.density(ADEL01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL01_QT.entropy <- entropy(ADEL01_QTft) #entropy

ADEL01_QT.netMx <- cbind(ADEL01_QT.netMx, ADEL01_QT.clusterCoef, ADEL01_QT.degreeCent$centralization,
                         ADEL01_QT.netDensity, ADEL01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL01_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "BL"
KIoutcome = "Goal_F"
BL01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
BL01_Gg2 <- data.frame(BL01_G)
BL01_Gg2 <- BL01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_Gg2$player1
player2vector <- BL01_Gg2$player2
BL01_Gg3 <- BL01_Gg2
BL01_Gg3$p1inp2vec <- is.element(BL01_Gg3$player1, player2vector)
BL01_Gg3$p2inp1vec <- is.element(BL01_Gg3$player2, player1vector)

addPlayer1 <- BL01_Gg3[ which(BL01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_Gg3[ which(BL01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_Gg2 <- rbind(BL01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
BL01_Gft <- ftable(BL01_Gg2$player1, BL01_Gg2$player2)
BL01_Gft2 <- as.matrix(BL01_Gft)
numRows <- nrow(BL01_Gft2)
numCols <- ncol(BL01_Gft2)
BL01_Gft3 <- BL01_Gft2[c(2:numRows) , c(2:numCols)]
BL01_GTable <- graph.adjacency(BL01_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(BL01_GTable, vertex.label = V(BL01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
BL01_G.clusterCoef <- transitivity(BL01_GTable, type="global") #cluster coefficient
BL01_G.degreeCent <- centralization.degree(BL01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_Gftn <- as.network.matrix(BL01_Gft)
BL01_G.netDensity <- network.density(BL01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_G.entropy <- entropy(BL01_Gft) #entropy

BL01_G.netMx <- cbind(BL01_G.netMx, BL01_G.clusterCoef, BL01_G.degreeCent$centralization,
                      BL01_G.netDensity, BL01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "Behind_F"
BL01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
BL01_Bg2 <- data.frame(BL01_B)
BL01_Bg2 <- BL01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_Bg2$player1
player2vector <- BL01_Bg2$player2
BL01_Bg3 <- BL01_Bg2
BL01_Bg3$p1inp2vec <- is.element(BL01_Bg3$player1, player2vector)
BL01_Bg3$p2inp1vec <- is.element(BL01_Bg3$player2, player1vector)

addPlayer1 <- BL01_Bg3[ which(BL01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_Bg3[ which(BL01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_Bg2 <- rbind(BL01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
BL01_Bft <- ftable(BL01_Bg2$player1, BL01_Bg2$player2)
BL01_Bft2 <- as.matrix(BL01_Bft)
numRows <- nrow(BL01_Bft2)
numCols <- ncol(BL01_Bft2)
BL01_Bft3 <- BL01_Bft2[c(2:numRows) , c(2:numCols)]
BL01_BTable <- graph.adjacency(BL01_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(BL01_BTable, vertex.label = V(BL01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
BL01_B.clusterCoef <- transitivity(BL01_BTable, type="global") #cluster coefficient
BL01_B.degreeCent <- centralization.degree(BL01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_Bftn <- as.network.matrix(BL01_Bft)
BL01_B.netDensity <- network.density(BL01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_B.entropy <- entropy(BL01_Bft) #entropy

BL01_B.netMx <- cbind(BL01_B.netMx, BL01_B.clusterCoef, BL01_B.degreeCent$centralization,
                      BL01_B.netDensity, BL01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "Stoppage_F"
BL01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
BL01_SFg2 <- data.frame(BL01_SF)
BL01_SFg2 <- BL01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_SFg2$player1
player2vector <- BL01_SFg2$player2
BL01_SFg3 <- BL01_SFg2
BL01_SFg3$p1inp2vec <- is.element(BL01_SFg3$player1, player2vector)
BL01_SFg3$p2inp1vec <- is.element(BL01_SFg3$player2, player1vector)

addPlayer1 <- BL01_SFg3[ which(BL01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_SFg3[ which(BL01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_SFg2 <- rbind(BL01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
BL01_SFft <- ftable(BL01_SFg2$player1, BL01_SFg2$player2)
BL01_SFft2 <- as.matrix(BL01_SFft)
numRows <- nrow(BL01_SFft2)
numCols <- ncol(BL01_SFft2)
BL01_SFft3 <- BL01_SFft2[c(2:numRows) , c(2:numCols)]
BL01_SFTable <- graph.adjacency(BL01_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(BL01_SFTable, vertex.label = V(BL01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
BL01_SF.clusterCoef <- transitivity(BL01_SFTable, type="global") #cluster coefficient
BL01_SF.degreeCent <- centralization.degree(BL01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_SFftn <- as.network.matrix(BL01_SFft)
BL01_SF.netDensity <- network.density(BL01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_SF.entropy <- entropy(BL01_SFft) #entropy

BL01_SF.netMx <- cbind(BL01_SF.netMx, BL01_SF.clusterCoef, BL01_SF.degreeCent$centralization,
                       BL01_SF.netDensity, BL01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "Turnover_F"
BL01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
BL01_TFg2 <- data.frame(BL01_TF)
BL01_TFg2 <- BL01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_TFg2$player1
player2vector <- BL01_TFg2$player2
BL01_TFg3 <- BL01_TFg2
BL01_TFg3$p1inp2vec <- is.element(BL01_TFg3$player1, player2vector)
BL01_TFg3$p2inp1vec <- is.element(BL01_TFg3$player2, player1vector)

addPlayer1 <- BL01_TFg3[ which(BL01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_TFg3[ which(BL01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_TFg2 <- rbind(BL01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
BL01_TFft <- ftable(BL01_TFg2$player1, BL01_TFg2$player2)
BL01_TFft2 <- as.matrix(BL01_TFft)
numRows <- nrow(BL01_TFft2)
numCols <- ncol(BL01_TFft2)
BL01_TFft3 <- BL01_TFft2[c(2:numRows) , c(2:numCols)]
BL01_TFTable <- graph.adjacency(BL01_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(BL01_TFTable, vertex.label = V(BL01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
BL01_TF.clusterCoef <- transitivity(BL01_TFTable, type="global") #cluster coefficient
BL01_TF.degreeCent <- centralization.degree(BL01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_TFftn <- as.network.matrix(BL01_TFft)
BL01_TF.netDensity <- network.density(BL01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_TF.entropy <- entropy(BL01_TFft) #entropy

BL01_TF.netMx <- cbind(BL01_TF.netMx, BL01_TF.clusterCoef, BL01_TF.degreeCent$centralization,
                       BL01_TF.netDensity, BL01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
BL01_SAMg2 <- data.frame(BL01_SAM)
BL01_SAMg2 <- BL01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_SAMg2$player1
player2vector <- BL01_SAMg2$player2
BL01_SAMg3 <- BL01_SAMg2
BL01_SAMg3$p1inp2vec <- is.element(BL01_SAMg3$player1, player2vector)
BL01_SAMg3$p2inp1vec <- is.element(BL01_SAMg3$player2, player1vector)

addPlayer1 <- BL01_SAMg3[ which(BL01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_SAMg3[ which(BL01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_SAMg2 <- rbind(BL01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
BL01_SAMft <- ftable(BL01_SAMg2$player1, BL01_SAMg2$player2)
BL01_SAMft2 <- as.matrix(BL01_SAMft)
numRows <- nrow(BL01_SAMft2)
numCols <- ncol(BL01_SAMft2)
BL01_SAMft3 <- BL01_SAMft2[c(2:numRows) , c(2:numCols)]
BL01_SAMTable <- graph.adjacency(BL01_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(BL01_SAMTable, vertex.label = V(BL01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
BL01_SAM.clusterCoef <- transitivity(BL01_SAMTable, type="global") #cluster coefficient
BL01_SAM.degreeCent <- centralization.degree(BL01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_SAMftn <- as.network.matrix(BL01_SAMft)
BL01_SAM.netDensity <- network.density(BL01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_SAM.entropy <- entropy(BL01_SAMft) #entropy

BL01_SAM.netMx <- cbind(BL01_SAM.netMx, BL01_SAM.clusterCoef, BL01_SAM.degreeCent$centralization,
                        BL01_SAM.netDensity, BL01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "BL"
KIoutcome = "Turnover_AM"
BL01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
BL01_TAMg2 <- data.frame(BL01_TAM)
BL01_TAMg2 <- BL01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_TAMg2$player1
player2vector <- BL01_TAMg2$player2
BL01_TAMg3 <- BL01_TAMg2
BL01_TAMg3$p1inp2vec <- is.element(BL01_TAMg3$player1, player2vector)
BL01_TAMg3$p2inp1vec <- is.element(BL01_TAMg3$player2, player1vector)

addPlayer1 <- BL01_TAMg3[ which(BL01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_TAMg3[ which(BL01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_TAMg2 <- rbind(BL01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
BL01_TAMft <- ftable(BL01_TAMg2$player1, BL01_TAMg2$player2)
BL01_TAMft2 <- as.matrix(BL01_TAMft)
numRows <- nrow(BL01_TAMft2)
numCols <- ncol(BL01_TAMft2)
BL01_TAMft3 <- BL01_TAMft2[c(2:numRows) , c(2:numCols)]
BL01_TAMTable <- graph.adjacency(BL01_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(BL01_TAMTable, vertex.label = V(BL01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
BL01_TAM.clusterCoef <- transitivity(BL01_TAMTable, type="global") #cluster coefficient
BL01_TAM.degreeCent <- centralization.degree(BL01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_TAMftn <- as.network.matrix(BL01_TAMft)
BL01_TAM.netDensity <- network.density(BL01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_TAM.entropy <- entropy(BL01_TAMft) #entropy

BL01_TAM.netMx <- cbind(BL01_TAM.netMx, BL01_TAM.clusterCoef, BL01_TAM.degreeCent$centralization,
                        BL01_TAM.netDensity, BL01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
BL01_SDMg2 <- data.frame(BL01_SDM)
BL01_SDMg2 <- BL01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_SDMg2$player1
player2vector <- BL01_SDMg2$player2
BL01_SDMg3 <- BL01_SDMg2
BL01_SDMg3$p1inp2vec <- is.element(BL01_SDMg3$player1, player2vector)
BL01_SDMg3$p2inp1vec <- is.element(BL01_SDMg3$player2, player1vector)

addPlayer1 <- BL01_SDMg3[ which(BL01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_SDMg3[ which(BL01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_SDMg2 <- rbind(BL01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
BL01_SDMft <- ftable(BL01_SDMg2$player1, BL01_SDMg2$player2)
BL01_SDMft2 <- as.matrix(BL01_SDMft)
numRows <- nrow(BL01_SDMft2)
numCols <- ncol(BL01_SDMft2)
BL01_SDMft3 <- BL01_SDMft2[c(2:numRows) , c(2:numCols)]
BL01_SDMTable <- graph.adjacency(BL01_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(BL01_SDMTable, vertex.label = V(BL01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
BL01_SDM.clusterCoef <- transitivity(BL01_SDMTable, type="global") #cluster coefficient
BL01_SDM.degreeCent <- centralization.degree(BL01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_SDMftn <- as.network.matrix(BL01_SDMft)
BL01_SDM.netDensity <- network.density(BL01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_SDM.entropy <- entropy(BL01_SDMft) #entropy

BL01_SDM.netMx <- cbind(BL01_SDM.netMx, BL01_SDM.clusterCoef, BL01_SDM.degreeCent$centralization,
                        BL01_SDM.netDensity, BL01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "BL"
KIoutcome = "Turnover_DM"
BL01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
BL01_TDMg2 <- data.frame(BL01_TDM)
BL01_TDMg2 <- BL01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_TDMg2$player1
player2vector <- BL01_TDMg2$player2
BL01_TDMg3 <- BL01_TDMg2
BL01_TDMg3$p1inp2vec <- is.element(BL01_TDMg3$player1, player2vector)
BL01_TDMg3$p2inp1vec <- is.element(BL01_TDMg3$player2, player1vector)

addPlayer1 <- BL01_TDMg3[ which(BL01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_TDMg3[ which(BL01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_TDMg2 <- rbind(BL01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
BL01_TDMft <- ftable(BL01_TDMg2$player1, BL01_TDMg2$player2)
BL01_TDMft2 <- as.matrix(BL01_TDMft)
numRows <- nrow(BL01_TDMft2)
numCols <- ncol(BL01_TDMft2)
BL01_TDMft3 <- BL01_TDMft2[c(2:numRows) , c(2:numCols)]
BL01_TDMTable <- graph.adjacency(BL01_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(BL01_TDMTable, vertex.label = V(BL01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
BL01_TDM.clusterCoef <- transitivity(BL01_TDMTable, type="global") #cluster coefficient
BL01_TDM.degreeCent <- centralization.degree(BL01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_TDMftn <- as.network.matrix(BL01_TDMft)
BL01_TDM.netDensity <- network.density(BL01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_TDM.entropy <- entropy(BL01_TDMft) #entropy

BL01_TDM.netMx <- cbind(BL01_TDM.netMx, BL01_TDM.clusterCoef, BL01_TDM.degreeCent$centralization,
                        BL01_TDM.netDensity, BL01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "Stoppage_D"
BL01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
BL01_SDg2 <- data.frame(BL01_SD)
BL01_SDg2 <- BL01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_SDg2$player1
player2vector <- BL01_SDg2$player2
BL01_SDg3 <- BL01_SDg2
BL01_SDg3$p1inp2vec <- is.element(BL01_SDg3$player1, player2vector)
BL01_SDg3$p2inp1vec <- is.element(BL01_SDg3$player2, player1vector)

addPlayer1 <- BL01_SDg3[ which(BL01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_SDg3[ which(BL01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_SDg2 <- rbind(BL01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
BL01_SDft <- ftable(BL01_SDg2$player1, BL01_SDg2$player2)
BL01_SDft2 <- as.matrix(BL01_SDft)
numRows <- nrow(BL01_SDft2)
numCols <- ncol(BL01_SDft2)
BL01_SDft3 <- BL01_SDft2[c(2:numRows) , c(2:numCols)]
BL01_SDTable <- graph.adjacency(BL01_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(BL01_SDTable, vertex.label = V(BL01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
BL01_SD.clusterCoef <- transitivity(BL01_SDTable, type="global") #cluster coefficient
BL01_SD.degreeCent <- centralization.degree(BL01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_SDftn <- as.network.matrix(BL01_SDft)
BL01_SD.netDensity <- network.density(BL01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_SD.entropy <- entropy(BL01_SDft) #entropy

BL01_SD.netMx <- cbind(BL01_SD.netMx, BL01_SD.clusterCoef, BL01_SD.degreeCent$centralization,
                       BL01_SD.netDensity, BL01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "Turnover_D"
BL01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
BL01_TDg2 <- data.frame(BL01_TD)
BL01_TDg2 <- BL01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_TDg2$player1
player2vector <- BL01_TDg2$player2
BL01_TDg3 <- BL01_TDg2
BL01_TDg3$p1inp2vec <- is.element(BL01_TDg3$player1, player2vector)
BL01_TDg3$p2inp1vec <- is.element(BL01_TDg3$player2, player1vector)

addPlayer1 <- BL01_TDg3[ which(BL01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_TDg3[ which(BL01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_TDg2 <- rbind(BL01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
BL01_TDft <- ftable(BL01_TDg2$player1, BL01_TDg2$player2)
BL01_TDft2 <- as.matrix(BL01_TDft)
numRows <- nrow(BL01_TDft2)
numCols <- ncol(BL01_TDft2)
BL01_TDft3 <- BL01_TDft2[c(2:numRows) , c(2:numCols)]
BL01_TDTable <- graph.adjacency(BL01_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(BL01_TDTable, vertex.label = V(BL01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
BL01_TD.clusterCoef <- transitivity(BL01_TDTable, type="global") #cluster coefficient
BL01_TD.degreeCent <- centralization.degree(BL01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_TDftn <- as.network.matrix(BL01_TDft)
BL01_TD.netDensity <- network.density(BL01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_TD.entropy <- entropy(BL01_TDft) #entropy

BL01_TD.netMx <- cbind(BL01_TD.netMx, BL01_TD.clusterCoef, BL01_TD.degreeCent$centralization,
                       BL01_TD.netDensity, BL01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
BL01_QTg2 <- data.frame(BL01_QT)
BL01_QTg2 <- BL01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL01_QTg2$player1
player2vector <- BL01_QTg2$player2
BL01_QTg3 <- BL01_QTg2
BL01_QTg3$p1inp2vec <- is.element(BL01_QTg3$player1, player2vector)
BL01_QTg3$p2inp1vec <- is.element(BL01_QTg3$player2, player1vector)

addPlayer1 <- BL01_QTg3[ which(BL01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL01_QTg3[ which(BL01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL01_QTg2 <- rbind(BL01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
BL01_QTft <- ftable(BL01_QTg2$player1, BL01_QTg2$player2)
BL01_QTft2 <- as.matrix(BL01_QTft)
numRows <- nrow(BL01_QTft2)
numCols <- ncol(BL01_QTft2)
BL01_QTft3 <- BL01_QTft2[c(2:numRows) , c(2:numCols)]
BL01_QTTable <- graph.adjacency(BL01_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(BL01_QTTable, vertex.label = V(BL01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
BL01_QT.clusterCoef <- transitivity(BL01_QTTable, type="global") #cluster coefficient
BL01_QT.degreeCent <- centralization.degree(BL01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL01_QTftn <- as.network.matrix(BL01_QTft)
BL01_QT.netDensity <- network.density(BL01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL01_QT.entropy <- entropy(BL01_QTft) #entropy

BL01_QT.netMx <- cbind(BL01_QT.netMx, BL01_QT.clusterCoef, BL01_QT.degreeCent$centralization,
                       BL01_QT.netDensity, BL01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL01_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Goal_F"
CARL01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
CARL01_Gg2 <- data.frame(CARL01_G)
CARL01_Gg2 <- CARL01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_Gg2$player1
player2vector <- CARL01_Gg2$player2
CARL01_Gg3 <- CARL01_Gg2
CARL01_Gg3$p1inp2vec <- is.element(CARL01_Gg3$player1, player2vector)
CARL01_Gg3$p2inp1vec <- is.element(CARL01_Gg3$player2, player1vector)

addPlayer1 <- CARL01_Gg3[ which(CARL01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_Gg3[ which(CARL01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_Gg2 <- rbind(CARL01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
CARL01_Gft <- ftable(CARL01_Gg2$player1, CARL01_Gg2$player2)
CARL01_Gft2 <- as.matrix(CARL01_Gft)
numRows <- nrow(CARL01_Gft2)
numCols <- ncol(CARL01_Gft2)
CARL01_Gft3 <- CARL01_Gft2[c(2:numRows) , c(2:numCols)]
CARL01_GTable <- graph.adjacency(CARL01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(CARL01_GTable, vertex.label = V(CARL01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
CARL01_G.clusterCoef <- transitivity(CARL01_GTable, type="global") #cluster coefficient
CARL01_G.degreeCent <- centralization.degree(CARL01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_Gftn <- as.network.matrix(CARL01_Gft)
CARL01_G.netDensity <- network.density(CARL01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_G.entropy <- entropy(CARL01_Gft) #entropy

CARL01_G.netMx <- cbind(CARL01_G.netMx, CARL01_G.clusterCoef, CARL01_G.degreeCent$centralization,
                        CARL01_G.netDensity, CARL01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Behind_F"
CARL01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
CARL01_Bg2 <- data.frame(CARL01_B)
CARL01_Bg2 <- CARL01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_Bg2$player1
player2vector <- CARL01_Bg2$player2
CARL01_Bg3 <- CARL01_Bg2
CARL01_Bg3$p1inp2vec <- is.element(CARL01_Bg3$player1, player2vector)
CARL01_Bg3$p2inp1vec <- is.element(CARL01_Bg3$player2, player1vector)

addPlayer1 <- CARL01_Bg3[ which(CARL01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_Bg3[ which(CARL01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_Bg2 <- rbind(CARL01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
CARL01_Bft <- ftable(CARL01_Bg2$player1, CARL01_Bg2$player2)
CARL01_Bft2 <- as.matrix(CARL01_Bft)
numRows <- nrow(CARL01_Bft2)
numCols <- ncol(CARL01_Bft2)
CARL01_Bft3 <- CARL01_Bft2[c(2:numRows) , c(2:numCols)]
CARL01_BTable <- graph.adjacency(CARL01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(CARL01_BTable, vertex.label = V(CARL01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
CARL01_B.clusterCoef <- transitivity(CARL01_BTable, type="global") #cluster coefficient
CARL01_B.degreeCent <- centralization.degree(CARL01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_Bftn <- as.network.matrix(CARL01_Bft)
CARL01_B.netDensity <- network.density(CARL01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_B.entropy <- entropy(CARL01_Bft) #entropy

CARL01_B.netMx <- cbind(CARL01_B.netMx, CARL01_B.clusterCoef, CARL01_B.degreeCent$centralization,
                        CARL01_B.netDensity, CARL01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
CARL01_SFg2 <- data.frame(CARL01_SF)
CARL01_SFg2 <- CARL01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_SFg2$player1
player2vector <- CARL01_SFg2$player2
CARL01_SFg3 <- CARL01_SFg2
CARL01_SFg3$p1inp2vec <- is.element(CARL01_SFg3$player1, player2vector)
CARL01_SFg3$p2inp1vec <- is.element(CARL01_SFg3$player2, player1vector)

addPlayer1 <- CARL01_SFg3[ which(CARL01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_SFg3[ which(CARL01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_SFg2 <- rbind(CARL01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
CARL01_SFft <- ftable(CARL01_SFg2$player1, CARL01_SFg2$player2)
CARL01_SFft2 <- as.matrix(CARL01_SFft)
numRows <- nrow(CARL01_SFft2)
numCols <- ncol(CARL01_SFft2)
CARL01_SFft3 <- CARL01_SFft2[c(2:numRows) , c(2:numCols)]
CARL01_SFTable <- graph.adjacency(CARL01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(CARL01_SFTable, vertex.label = V(CARL01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
CARL01_SF.clusterCoef <- transitivity(CARL01_SFTable, type="global") #cluster coefficient
CARL01_SF.degreeCent <- centralization.degree(CARL01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_SFftn <- as.network.matrix(CARL01_SFft)
CARL01_SF.netDensity <- network.density(CARL01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_SF.entropy <- entropy(CARL01_SFft) #entropy

CARL01_SF.netMx <- cbind(CARL01_SF.netMx, CARL01_SF.clusterCoef, CARL01_SF.degreeCent$centralization,
                         CARL01_SF.netDensity, CARL01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
CARL01_TFg2 <- data.frame(CARL01_TF)
CARL01_TFg2 <- CARL01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_TFg2$player1
player2vector <- CARL01_TFg2$player2
CARL01_TFg3 <- CARL01_TFg2
CARL01_TFg3$p1inp2vec <- is.element(CARL01_TFg3$player1, player2vector)
CARL01_TFg3$p2inp1vec <- is.element(CARL01_TFg3$player2, player1vector)

addPlayer1 <- CARL01_TFg3[ which(CARL01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_TFg3[ which(CARL01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_TFg2 <- rbind(CARL01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
CARL01_TFft <- ftable(CARL01_TFg2$player1, CARL01_TFg2$player2)
CARL01_TFft2 <- as.matrix(CARL01_TFft)
numRows <- nrow(CARL01_TFft2)
numCols <- ncol(CARL01_TFft2)
CARL01_TFft3 <- CARL01_TFft2[c(2:numRows) , c(2:numCols)]
CARL01_TFTable <- graph.adjacency(CARL01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(CARL01_TFTable, vertex.label = V(CARL01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
CARL01_TF.clusterCoef <- transitivity(CARL01_TFTable, type="global") #cluster coefficient
CARL01_TF.degreeCent <- centralization.degree(CARL01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_TFftn <- as.network.matrix(CARL01_TFft)
CARL01_TF.netDensity <- network.density(CARL01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_TF.entropy <- entropy(CARL01_TFft) #entropy

CARL01_TF.netMx <- cbind(CARL01_TF.netMx, CARL01_TF.clusterCoef, CARL01_TF.degreeCent$centralization,
                         CARL01_TF.netDensity, CARL01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
CARL01_SAMg2 <- data.frame(CARL01_SAM)
CARL01_SAMg2 <- CARL01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_SAMg2$player1
player2vector <- CARL01_SAMg2$player2
CARL01_SAMg3 <- CARL01_SAMg2
CARL01_SAMg3$p1inp2vec <- is.element(CARL01_SAMg3$player1, player2vector)
CARL01_SAMg3$p2inp1vec <- is.element(CARL01_SAMg3$player2, player1vector)

addPlayer1 <- CARL01_SAMg3[ which(CARL01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_SAMg3[ which(CARL01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_SAMg2 <- rbind(CARL01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
CARL01_SAMft <- ftable(CARL01_SAMg2$player1, CARL01_SAMg2$player2)
CARL01_SAMft2 <- as.matrix(CARL01_SAMft)
numRows <- nrow(CARL01_SAMft2)
numCols <- ncol(CARL01_SAMft2)
CARL01_SAMft3 <- CARL01_SAMft2[c(2:numRows) , c(2:numCols)]
CARL01_SAMTable <- graph.adjacency(CARL01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(CARL01_SAMTable, vertex.label = V(CARL01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
CARL01_SAM.clusterCoef <- transitivity(CARL01_SAMTable, type="global") #cluster coefficient
CARL01_SAM.degreeCent <- centralization.degree(CARL01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_SAMftn <- as.network.matrix(CARL01_SAMft)
CARL01_SAM.netDensity <- network.density(CARL01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_SAM.entropy <- entropy(CARL01_SAMft) #entropy

CARL01_SAM.netMx <- cbind(CARL01_SAM.netMx, CARL01_SAM.clusterCoef, CARL01_SAM.degreeCent$centralization,
                          CARL01_SAM.netDensity, CARL01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
CARL01_TAMg2 <- data.frame(CARL01_TAM)
CARL01_TAMg2 <- CARL01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_TAMg2$player1
player2vector <- CARL01_TAMg2$player2
CARL01_TAMg3 <- CARL01_TAMg2
CARL01_TAMg3$p1inp2vec <- is.element(CARL01_TAMg3$player1, player2vector)
CARL01_TAMg3$p2inp1vec <- is.element(CARL01_TAMg3$player2, player1vector)

addPlayer1 <- CARL01_TAMg3[ which(CARL01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_TAMg3[ which(CARL01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_TAMg2 <- rbind(CARL01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
CARL01_TAMft <- ftable(CARL01_TAMg2$player1, CARL01_TAMg2$player2)
CARL01_TAMft2 <- as.matrix(CARL01_TAMft)
numRows <- nrow(CARL01_TAMft2)
numCols <- ncol(CARL01_TAMft2)
CARL01_TAMft3 <- CARL01_TAMft2[c(2:numRows) , c(2:numCols)]
CARL01_TAMTable <- graph.adjacency(CARL01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(CARL01_TAMTable, vertex.label = V(CARL01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
CARL01_TAM.clusterCoef <- transitivity(CARL01_TAMTable, type="global") #cluster coefficient
CARL01_TAM.degreeCent <- centralization.degree(CARL01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_TAMftn <- as.network.matrix(CARL01_TAMft)
CARL01_TAM.netDensity <- network.density(CARL01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_TAM.entropy <- entropy(CARL01_TAMft) #entropy

CARL01_TAM.netMx <- cbind(CARL01_TAM.netMx, CARL01_TAM.clusterCoef, CARL01_TAM.degreeCent$centralization,
                          CARL01_TAM.netDensity, CARL01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
CARL01_SDMg2 <- data.frame(CARL01_SDM)
CARL01_SDMg2 <- CARL01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_SDMg2$player1
player2vector <- CARL01_SDMg2$player2
CARL01_SDMg3 <- CARL01_SDMg2
CARL01_SDMg3$p1inp2vec <- is.element(CARL01_SDMg3$player1, player2vector)
CARL01_SDMg3$p2inp1vec <- is.element(CARL01_SDMg3$player2, player1vector)

addPlayer1 <- CARL01_SDMg3[ which(CARL01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_SDMg3[ which(CARL01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_SDMg2 <- rbind(CARL01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
CARL01_SDMft <- ftable(CARL01_SDMg2$player1, CARL01_SDMg2$player2)
CARL01_SDMft2 <- as.matrix(CARL01_SDMft)
numRows <- nrow(CARL01_SDMft2)
numCols <- ncol(CARL01_SDMft2)
CARL01_SDMft3 <- CARL01_SDMft2[c(2:numRows) , c(2:numCols)]
CARL01_SDMTable <- graph.adjacency(CARL01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(CARL01_SDMTable, vertex.label = V(CARL01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
CARL01_SDM.clusterCoef <- transitivity(CARL01_SDMTable, type="global") #cluster coefficient
CARL01_SDM.degreeCent <- centralization.degree(CARL01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_SDMftn <- as.network.matrix(CARL01_SDMft)
CARL01_SDM.netDensity <- network.density(CARL01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_SDM.entropy <- entropy(CARL01_SDMft) #entropy

CARL01_SDM.netMx <- cbind(CARL01_SDM.netMx, CARL01_SDM.clusterCoef, CARL01_SDM.degreeCent$centralization,
                          CARL01_SDM.netDensity, CARL01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
CARL01_TDMg2 <- data.frame(CARL01_TDM)
CARL01_TDMg2 <- CARL01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_TDMg2$player1
player2vector <- CARL01_TDMg2$player2
CARL01_TDMg3 <- CARL01_TDMg2
CARL01_TDMg3$p1inp2vec <- is.element(CARL01_TDMg3$player1, player2vector)
CARL01_TDMg3$p2inp1vec <- is.element(CARL01_TDMg3$player2, player1vector)

addPlayer1 <- CARL01_TDMg3[ which(CARL01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_TDMg3[ which(CARL01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_TDMg2 <- rbind(CARL01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
CARL01_TDMft <- ftable(CARL01_TDMg2$player1, CARL01_TDMg2$player2)
CARL01_TDMft2 <- as.matrix(CARL01_TDMft)
numRows <- nrow(CARL01_TDMft2)
numCols <- ncol(CARL01_TDMft2)
CARL01_TDMft3 <- CARL01_TDMft2[c(2:numRows) , c(2:numCols)]
CARL01_TDMTable <- graph.adjacency(CARL01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(CARL01_TDMTable, vertex.label = V(CARL01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
CARL01_TDM.clusterCoef <- transitivity(CARL01_TDMTable, type="global") #cluster coefficient
CARL01_TDM.degreeCent <- centralization.degree(CARL01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_TDMftn <- as.network.matrix(CARL01_TDMft)
CARL01_TDM.netDensity <- network.density(CARL01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_TDM.entropy <- entropy(CARL01_TDMft) #entropy

CARL01_TDM.netMx <- cbind(CARL01_TDM.netMx, CARL01_TDM.clusterCoef, CARL01_TDM.degreeCent$centralization,
                          CARL01_TDM.netDensity, CARL01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
CARL01_SDg2 <- data.frame(CARL01_SD)
CARL01_SDg2 <- CARL01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_SDg2$player1
player2vector <- CARL01_SDg2$player2
CARL01_SDg3 <- CARL01_SDg2
CARL01_SDg3$p1inp2vec <- is.element(CARL01_SDg3$player1, player2vector)
CARL01_SDg3$p2inp1vec <- is.element(CARL01_SDg3$player2, player1vector)

addPlayer1 <- CARL01_SDg3[ which(CARL01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_SDg3[ which(CARL01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_SDg2 <- rbind(CARL01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
CARL01_SDft <- ftable(CARL01_SDg2$player1, CARL01_SDg2$player2)
CARL01_SDft2 <- as.matrix(CARL01_SDft)
numRows <- nrow(CARL01_SDft2)
numCols <- ncol(CARL01_SDft2)
CARL01_SDft3 <- CARL01_SDft2[c(2:numRows) , c(2:numCols)]
CARL01_SDTable <- graph.adjacency(CARL01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(CARL01_SDTable, vertex.label = V(CARL01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
CARL01_SD.clusterCoef <- transitivity(CARL01_SDTable, type="global") #cluster coefficient
CARL01_SD.degreeCent <- centralization.degree(CARL01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_SDftn <- as.network.matrix(CARL01_SDft)
CARL01_SD.netDensity <- network.density(CARL01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_SD.entropy <- entropy(CARL01_SDft) #entropy

CARL01_SD.netMx <- cbind(CARL01_SD.netMx, CARL01_SD.clusterCoef, CARL01_SD.degreeCent$centralization,
                         CARL01_SD.netDensity, CARL01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
CARL01_TDg2 <- data.frame(CARL01_TD)
CARL01_TDg2 <- CARL01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_TDg2$player1
player2vector <- CARL01_TDg2$player2
CARL01_TDg3 <- CARL01_TDg2
CARL01_TDg3$p1inp2vec <- is.element(CARL01_TDg3$player1, player2vector)
CARL01_TDg3$p2inp1vec <- is.element(CARL01_TDg3$player2, player1vector)

addPlayer1 <- CARL01_TDg3[ which(CARL01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_TDg3[ which(CARL01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_TDg2 <- rbind(CARL01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
CARL01_TDft <- ftable(CARL01_TDg2$player1, CARL01_TDg2$player2)
CARL01_TDft2 <- as.matrix(CARL01_TDft)
numRows <- nrow(CARL01_TDft2)
numCols <- ncol(CARL01_TDft2)
CARL01_TDft3 <- CARL01_TDft2[c(2:numRows) , c(2:numCols)]
CARL01_TDTable <- graph.adjacency(CARL01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(CARL01_TDTable, vertex.label = V(CARL01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
CARL01_TD.clusterCoef <- transitivity(CARL01_TDTable, type="global") #cluster coefficient
CARL01_TD.degreeCent <- centralization.degree(CARL01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_TDftn <- as.network.matrix(CARL01_TDft)
CARL01_TD.netDensity <- network.density(CARL01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_TD.entropy <- entropy(CARL01_TDft) #entropy

CARL01_TD.netMx <- cbind(CARL01_TD.netMx, CARL01_TD.clusterCoef, CARL01_TD.degreeCent$centralization,
                         CARL01_TD.netDensity, CARL01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
CARL01_QTg2 <- data.frame(CARL01_QT)
CARL01_QTg2 <- CARL01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL01_QTg2$player1
player2vector <- CARL01_QTg2$player2
CARL01_QTg3 <- CARL01_QTg2
CARL01_QTg3$p1inp2vec <- is.element(CARL01_QTg3$player1, player2vector)
CARL01_QTg3$p2inp1vec <- is.element(CARL01_QTg3$player2, player1vector)

addPlayer1 <- CARL01_QTg3[ which(CARL01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL01_QTg3[ which(CARL01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL01_QTg2 <- rbind(CARL01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
CARL01_QTft <- ftable(CARL01_QTg2$player1, CARL01_QTg2$player2)
CARL01_QTft2 <- as.matrix(CARL01_QTft)
numRows <- nrow(CARL01_QTft2)
numCols <- ncol(CARL01_QTft2)
CARL01_QTft3 <- CARL01_QTft2[c(2:numRows) , c(2:numCols)]
CARL01_QTTable <- graph.adjacency(CARL01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(CARL01_QTTable, vertex.label = V(CARL01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
CARL01_QT.clusterCoef <- transitivity(CARL01_QTTable, type="global") #cluster coefficient
CARL01_QT.degreeCent <- centralization.degree(CARL01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL01_QTftn <- as.network.matrix(CARL01_QTft)
CARL01_QT.netDensity <- network.density(CARL01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL01_QT.entropy <- entropy(CARL01_QTft) #entropy

CARL01_QT.netMx <- cbind(CARL01_QT.netMx, CARL01_QT.clusterCoef, CARL01_QT.degreeCent$centralization,
                         CARL01_QT.netDensity, CARL01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL01_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "COLL"
KIoutcome = "Goal_F"
COLL01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
COLL01_Gg2 <- data.frame(COLL01_G)
COLL01_Gg2 <- COLL01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_Gg2$player1
player2vector <- COLL01_Gg2$player2
COLL01_Gg3 <- COLL01_Gg2
COLL01_Gg3$p1inp2vec <- is.element(COLL01_Gg3$player1, player2vector)
COLL01_Gg3$p2inp1vec <- is.element(COLL01_Gg3$player2, player1vector)

addPlayer1 <- COLL01_Gg3[ which(COLL01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_Gg3[ which(COLL01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_Gg2 <- rbind(COLL01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
COLL01_Gft <- ftable(COLL01_Gg2$player1, COLL01_Gg2$player2)
COLL01_Gft2 <- as.matrix(COLL01_Gft)
numRows <- nrow(COLL01_Gft2)
numCols <- ncol(COLL01_Gft2)
COLL01_Gft3 <- COLL01_Gft2[c(2:numRows) , c(2:numCols)]
COLL01_GTable <- graph.adjacency(COLL01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(COLL01_GTable, vertex.label = V(COLL01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
COLL01_G.clusterCoef <- transitivity(COLL01_GTable, type="global") #cluster coefficient
COLL01_G.degreeCent <- centralization.degree(COLL01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_Gftn <- as.network.matrix(COLL01_Gft)
COLL01_G.netDensity <- network.density(COLL01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_G.entropy <- entropy(COLL01_Gft) #entropy

COLL01_G.netMx <- cbind(COLL01_G.netMx, COLL01_G.clusterCoef, COLL01_G.degreeCent$centralization,
                        COLL01_G.netDensity, COLL01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Behind_F"
COLL01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
COLL01_Bg2 <- data.frame(COLL01_B)
COLL01_Bg2 <- COLL01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_Bg2$player1
player2vector <- COLL01_Bg2$player2
COLL01_Bg3 <- COLL01_Bg2
COLL01_Bg3$p1inp2vec <- is.element(COLL01_Bg3$player1, player2vector)
COLL01_Bg3$p2inp1vec <- is.element(COLL01_Bg3$player2, player1vector)

addPlayer1 <- COLL01_Bg3[ which(COLL01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_Bg3[ which(COLL01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_Bg2 <- rbind(COLL01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
COLL01_Bft <- ftable(COLL01_Bg2$player1, COLL01_Bg2$player2)
COLL01_Bft2 <- as.matrix(COLL01_Bft)
numRows <- nrow(COLL01_Bft2)
numCols <- ncol(COLL01_Bft2)
COLL01_Bft3 <- COLL01_Bft2[c(2:numRows) , c(2:numCols)]
COLL01_BTable <- graph.adjacency(COLL01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(COLL01_BTable, vertex.label = V(COLL01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
COLL01_B.clusterCoef <- transitivity(COLL01_BTable, type="global") #cluster coefficient
COLL01_B.degreeCent <- centralization.degree(COLL01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_Bftn <- as.network.matrix(COLL01_Bft)
COLL01_B.netDensity <- network.density(COLL01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_B.entropy <- entropy(COLL01_Bft) #entropy

COLL01_B.netMx <- cbind(COLL01_B.netMx, COLL01_B.clusterCoef, COLL01_B.degreeCent$centralization,
                        COLL01_B.netDensity, COLL01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
COLL01_SFg2 <- data.frame(COLL01_SF)
COLL01_SFg2 <- COLL01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_SFg2$player1
player2vector <- COLL01_SFg2$player2
COLL01_SFg3 <- COLL01_SFg2
COLL01_SFg3$p1inp2vec <- is.element(COLL01_SFg3$player1, player2vector)
COLL01_SFg3$p2inp1vec <- is.element(COLL01_SFg3$player2, player1vector)

addPlayer1 <- COLL01_SFg3[ which(COLL01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_SFg3[ which(COLL01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_SFg2 <- rbind(COLL01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
COLL01_SFft <- ftable(COLL01_SFg2$player1, COLL01_SFg2$player2)
COLL01_SFft2 <- as.matrix(COLL01_SFft)
numRows <- nrow(COLL01_SFft2)
numCols <- ncol(COLL01_SFft2)
COLL01_SFft3 <- COLL01_SFft2[c(2:numRows) , c(2:numCols)]
COLL01_SFTable <- graph.adjacency(COLL01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(COLL01_SFTable, vertex.label = V(COLL01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
COLL01_SF.clusterCoef <- transitivity(COLL01_SFTable, type="global") #cluster coefficient
COLL01_SF.degreeCent <- centralization.degree(COLL01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_SFftn <- as.network.matrix(COLL01_SFft)
COLL01_SF.netDensity <- network.density(COLL01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_SF.entropy <- entropy(COLL01_SFft) #entropy

COLL01_SF.netMx <- cbind(COLL01_SF.netMx, COLL01_SF.clusterCoef, COLL01_SF.degreeCent$centralization,
                         COLL01_SF.netDensity, COLL01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
COLL01_TFg2 <- data.frame(COLL01_TF)
COLL01_TFg2 <- COLL01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_TFg2$player1
player2vector <- COLL01_TFg2$player2
COLL01_TFg3 <- COLL01_TFg2
COLL01_TFg3$p1inp2vec <- is.element(COLL01_TFg3$player1, player2vector)
COLL01_TFg3$p2inp1vec <- is.element(COLL01_TFg3$player2, player1vector)

addPlayer1 <- COLL01_TFg3[ which(COLL01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_TFg3[ which(COLL01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_TFg2 <- rbind(COLL01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
COLL01_TFft <- ftable(COLL01_TFg2$player1, COLL01_TFg2$player2)
COLL01_TFft2 <- as.matrix(COLL01_TFft)
numRows <- nrow(COLL01_TFft2)
numCols <- ncol(COLL01_TFft2)
COLL01_TFft3 <- COLL01_TFft2[c(2:numRows) , c(2:numCols)]
COLL01_TFTable <- graph.adjacency(COLL01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(COLL01_TFTable, vertex.label = V(COLL01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
COLL01_TF.clusterCoef <- transitivity(COLL01_TFTable, type="global") #cluster coefficient
COLL01_TF.degreeCent <- centralization.degree(COLL01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_TFftn <- as.network.matrix(COLL01_TFft)
COLL01_TF.netDensity <- network.density(COLL01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_TF.entropy <- entropy(COLL01_TFft) #entropy

COLL01_TF.netMx <- cbind(COLL01_TF.netMx, COLL01_TF.clusterCoef, COLL01_TF.degreeCent$centralization,
                         COLL01_TF.netDensity, COLL01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
COLL01_SAMg2 <- data.frame(COLL01_SAM)
COLL01_SAMg2 <- COLL01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_SAMg2$player1
player2vector <- COLL01_SAMg2$player2
COLL01_SAMg3 <- COLL01_SAMg2
COLL01_SAMg3$p1inp2vec <- is.element(COLL01_SAMg3$player1, player2vector)
COLL01_SAMg3$p2inp1vec <- is.element(COLL01_SAMg3$player2, player1vector)

addPlayer1 <- COLL01_SAMg3[ which(COLL01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_SAMg3[ which(COLL01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_SAMg2 <- rbind(COLL01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
COLL01_SAMft <- ftable(COLL01_SAMg2$player1, COLL01_SAMg2$player2)
COLL01_SAMft2 <- as.matrix(COLL01_SAMft)
numRows <- nrow(COLL01_SAMft2)
numCols <- ncol(COLL01_SAMft2)
COLL01_SAMft3 <- COLL01_SAMft2[c(2:numRows) , c(2:numCols)]
COLL01_SAMTable <- graph.adjacency(COLL01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(COLL01_SAMTable, vertex.label = V(COLL01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
COLL01_SAM.clusterCoef <- transitivity(COLL01_SAMTable, type="global") #cluster coefficient
COLL01_SAM.degreeCent <- centralization.degree(COLL01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_SAMftn <- as.network.matrix(COLL01_SAMft)
COLL01_SAM.netDensity <- network.density(COLL01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_SAM.entropy <- entropy(COLL01_SAMft) #entropy

COLL01_SAM.netMx <- cbind(COLL01_SAM.netMx, COLL01_SAM.clusterCoef, COLL01_SAM.degreeCent$centralization,
                          COLL01_SAM.netDensity, COLL01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
COLL01_TAMg2 <- data.frame(COLL01_TAM)
COLL01_TAMg2 <- COLL01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_TAMg2$player1
player2vector <- COLL01_TAMg2$player2
COLL01_TAMg3 <- COLL01_TAMg2
COLL01_TAMg3$p1inp2vec <- is.element(COLL01_TAMg3$player1, player2vector)
COLL01_TAMg3$p2inp1vec <- is.element(COLL01_TAMg3$player2, player1vector)

addPlayer1 <- COLL01_TAMg3[ which(COLL01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_TAMg3[ which(COLL01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_TAMg2 <- rbind(COLL01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
COLL01_TAMft <- ftable(COLL01_TAMg2$player1, COLL01_TAMg2$player2)
COLL01_TAMft2 <- as.matrix(COLL01_TAMft)
numRows <- nrow(COLL01_TAMft2)
numCols <- ncol(COLL01_TAMft2)
COLL01_TAMft3 <- COLL01_TAMft2[c(2:numRows) , c(2:numCols)]
COLL01_TAMTable <- graph.adjacency(COLL01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(COLL01_TAMTable, vertex.label = V(COLL01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
COLL01_TAM.clusterCoef <- transitivity(COLL01_TAMTable, type="global") #cluster coefficient
COLL01_TAM.degreeCent <- centralization.degree(COLL01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_TAMftn <- as.network.matrix(COLL01_TAMft)
COLL01_TAM.netDensity <- network.density(COLL01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_TAM.entropy <- entropy(COLL01_TAMft) #entropy

COLL01_TAM.netMx <- cbind(COLL01_TAM.netMx, COLL01_TAM.clusterCoef, COLL01_TAM.degreeCent$centralization,
                          COLL01_TAM.netDensity, COLL01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
COLL01_SDMg2 <- data.frame(COLL01_SDM)
COLL01_SDMg2 <- COLL01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_SDMg2$player1
player2vector <- COLL01_SDMg2$player2
COLL01_SDMg3 <- COLL01_SDMg2
COLL01_SDMg3$p1inp2vec <- is.element(COLL01_SDMg3$player1, player2vector)
COLL01_SDMg3$p2inp1vec <- is.element(COLL01_SDMg3$player2, player1vector)

addPlayer1 <- COLL01_SDMg3[ which(COLL01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_SDMg3[ which(COLL01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_SDMg2 <- rbind(COLL01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
COLL01_SDMft <- ftable(COLL01_SDMg2$player1, COLL01_SDMg2$player2)
COLL01_SDMft2 <- as.matrix(COLL01_SDMft)
numRows <- nrow(COLL01_SDMft2)
numCols <- ncol(COLL01_SDMft2)
COLL01_SDMft3 <- COLL01_SDMft2[c(2:numRows) , c(2:numCols)]
COLL01_SDMTable <- graph.adjacency(COLL01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(COLL01_SDMTable, vertex.label = V(COLL01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
COLL01_SDM.clusterCoef <- transitivity(COLL01_SDMTable, type="global") #cluster coefficient
COLL01_SDM.degreeCent <- centralization.degree(COLL01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_SDMftn <- as.network.matrix(COLL01_SDMft)
COLL01_SDM.netDensity <- network.density(COLL01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_SDM.entropy <- entropy(COLL01_SDMft) #entropy

COLL01_SDM.netMx <- cbind(COLL01_SDM.netMx, COLL01_SDM.clusterCoef, COLL01_SDM.degreeCent$centralization,
                          COLL01_SDM.netDensity, COLL01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
COLL01_TDMg2 <- data.frame(COLL01_TDM)
COLL01_TDMg2 <- COLL01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_TDMg2$player1
player2vector <- COLL01_TDMg2$player2
COLL01_TDMg3 <- COLL01_TDMg2
COLL01_TDMg3$p1inp2vec <- is.element(COLL01_TDMg3$player1, player2vector)
COLL01_TDMg3$p2inp1vec <- is.element(COLL01_TDMg3$player2, player1vector)

addPlayer1 <- COLL01_TDMg3[ which(COLL01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_TDMg3[ which(COLL01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_TDMg2 <- rbind(COLL01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
COLL01_TDMft <- ftable(COLL01_TDMg2$player1, COLL01_TDMg2$player2)
COLL01_TDMft2 <- as.matrix(COLL01_TDMft)
numRows <- nrow(COLL01_TDMft2)
numCols <- ncol(COLL01_TDMft2)
COLL01_TDMft3 <- COLL01_TDMft2[c(2:numRows) , c(2:numCols)]
COLL01_TDMTable <- graph.adjacency(COLL01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(COLL01_TDMTable, vertex.label = V(COLL01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
COLL01_TDM.clusterCoef <- transitivity(COLL01_TDMTable, type="global") #cluster coefficient
COLL01_TDM.degreeCent <- centralization.degree(COLL01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_TDMftn <- as.network.matrix(COLL01_TDMft)
COLL01_TDM.netDensity <- network.density(COLL01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_TDM.entropy <- entropy(COLL01_TDMft) #entropy

COLL01_TDM.netMx <- cbind(COLL01_TDM.netMx, COLL01_TDM.clusterCoef, COLL01_TDM.degreeCent$centralization,
                          COLL01_TDM.netDensity, COLL01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
COLL01_SDg2 <- data.frame(COLL01_SD)
COLL01_SDg2 <- COLL01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_SDg2$player1
player2vector <- COLL01_SDg2$player2
COLL01_SDg3 <- COLL01_SDg2
COLL01_SDg3$p1inp2vec <- is.element(COLL01_SDg3$player1, player2vector)
COLL01_SDg3$p2inp1vec <- is.element(COLL01_SDg3$player2, player1vector)

addPlayer1 <- COLL01_SDg3[ which(COLL01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_SDg3[ which(COLL01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_SDg2 <- rbind(COLL01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
COLL01_SDft <- ftable(COLL01_SDg2$player1, COLL01_SDg2$player2)
COLL01_SDft2 <- as.matrix(COLL01_SDft)
numRows <- nrow(COLL01_SDft2)
numCols <- ncol(COLL01_SDft2)
COLL01_SDft3 <- COLL01_SDft2[c(2:numRows) , c(2:numCols)]
COLL01_SDTable <- graph.adjacency(COLL01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(COLL01_SDTable, vertex.label = V(COLL01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
COLL01_SD.clusterCoef <- transitivity(COLL01_SDTable, type="global") #cluster coefficient
COLL01_SD.degreeCent <- centralization.degree(COLL01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_SDftn <- as.network.matrix(COLL01_SDft)
COLL01_SD.netDensity <- network.density(COLL01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_SD.entropy <- entropy(COLL01_SDft) #entropy

COLL01_SD.netMx <- cbind(COLL01_SD.netMx, COLL01_SD.clusterCoef, COLL01_SD.degreeCent$centralization,
                         COLL01_SD.netDensity, COLL01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
COLL01_TDg2 <- data.frame(COLL01_TD)
COLL01_TDg2 <- COLL01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_TDg2$player1
player2vector <- COLL01_TDg2$player2
COLL01_TDg3 <- COLL01_TDg2
COLL01_TDg3$p1inp2vec <- is.element(COLL01_TDg3$player1, player2vector)
COLL01_TDg3$p2inp1vec <- is.element(COLL01_TDg3$player2, player1vector)

addPlayer1 <- COLL01_TDg3[ which(COLL01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_TDg3[ which(COLL01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_TDg2 <- rbind(COLL01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
COLL01_TDft <- ftable(COLL01_TDg2$player1, COLL01_TDg2$player2)
COLL01_TDft2 <- as.matrix(COLL01_TDft)
numRows <- nrow(COLL01_TDft2)
numCols <- ncol(COLL01_TDft2)
COLL01_TDft3 <- COLL01_TDft2[c(2:numRows) , c(2:numCols)]
COLL01_TDTable <- graph.adjacency(COLL01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(COLL01_TDTable, vertex.label = V(COLL01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
COLL01_TD.clusterCoef <- transitivity(COLL01_TDTable, type="global") #cluster coefficient
COLL01_TD.degreeCent <- centralization.degree(COLL01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_TDftn <- as.network.matrix(COLL01_TDft)
COLL01_TD.netDensity <- network.density(COLL01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_TD.entropy <- entropy(COLL01_TDft) #entropy

COLL01_TD.netMx <- cbind(COLL01_TD.netMx, COLL01_TD.clusterCoef, COLL01_TD.degreeCent$centralization,
                         COLL01_TD.netDensity, COLL01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
COLL01_QTg2 <- data.frame(COLL01_QT)
COLL01_QTg2 <- COLL01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL01_QTg2$player1
player2vector <- COLL01_QTg2$player2
COLL01_QTg3 <- COLL01_QTg2
COLL01_QTg3$p1inp2vec <- is.element(COLL01_QTg3$player1, player2vector)
COLL01_QTg3$p2inp1vec <- is.element(COLL01_QTg3$player2, player1vector)

addPlayer1 <- COLL01_QTg3[ which(COLL01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL01_QTg3[ which(COLL01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL01_QTg2 <- rbind(COLL01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
COLL01_QTft <- ftable(COLL01_QTg2$player1, COLL01_QTg2$player2)
COLL01_QTft2 <- as.matrix(COLL01_QTft)
numRows <- nrow(COLL01_QTft2)
numCols <- ncol(COLL01_QTft2)
COLL01_QTft3 <- COLL01_QTft2[c(2:numRows) , c(2:numCols)]
COLL01_QTTable <- graph.adjacency(COLL01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(COLL01_QTTable, vertex.label = V(COLL01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
COLL01_QT.clusterCoef <- transitivity(COLL01_QTTable, type="global") #cluster coefficient
COLL01_QT.degreeCent <- centralization.degree(COLL01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL01_QTftn <- as.network.matrix(COLL01_QTft)
COLL01_QT.netDensity <- network.density(COLL01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL01_QT.entropy <- entropy(COLL01_QTft) #entropy

COLL01_QT.netMx <- cbind(COLL01_QT.netMx, COLL01_QT.clusterCoef, COLL01_QT.degreeCent$centralization,
                         COLL01_QT.netDensity, COLL01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL01_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Goal_F"
ESS01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
ESS01_Gg2 <- data.frame(ESS01_G)
ESS01_Gg2 <- ESS01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_Gg2$player1
player2vector <- ESS01_Gg2$player2
ESS01_Gg3 <- ESS01_Gg2
ESS01_Gg3$p1inp2vec <- is.element(ESS01_Gg3$player1, player2vector)
ESS01_Gg3$p2inp1vec <- is.element(ESS01_Gg3$player2, player1vector)

addPlayer1 <- ESS01_Gg3[ which(ESS01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_Gg3[ which(ESS01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_Gg2 <- rbind(ESS01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
ESS01_Gft <- ftable(ESS01_Gg2$player1, ESS01_Gg2$player2)
ESS01_Gft2 <- as.matrix(ESS01_Gft)
numRows <- nrow(ESS01_Gft2)
numCols <- ncol(ESS01_Gft2)
ESS01_Gft3 <- ESS01_Gft2[c(2:numRows) , c(2:numCols)]
ESS01_GTable <- graph.adjacency(ESS01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(ESS01_GTable, vertex.label = V(ESS01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
ESS01_G.clusterCoef <- transitivity(ESS01_GTable, type="global") #cluster coefficient
ESS01_G.degreeCent <- centralization.degree(ESS01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_Gftn <- as.network.matrix(ESS01_Gft)
ESS01_G.netDensity <- network.density(ESS01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_G.entropy <- entropy(ESS01_Gft) #entropy

ESS01_G.netMx <- cbind(ESS01_G.netMx, ESS01_G.clusterCoef, ESS01_G.degreeCent$centralization,
                       ESS01_G.netDensity, ESS01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Behind_F"
ESS01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
ESS01_Bg2 <- data.frame(ESS01_B)
ESS01_Bg2 <- ESS01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_Bg2$player1
player2vector <- ESS01_Bg2$player2
ESS01_Bg3 <- ESS01_Bg2
ESS01_Bg3$p1inp2vec <- is.element(ESS01_Bg3$player1, player2vector)
ESS01_Bg3$p2inp1vec <- is.element(ESS01_Bg3$player2, player1vector)

addPlayer1 <- ESS01_Bg3[ which(ESS01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_Bg3[ which(ESS01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_Bg2 <- rbind(ESS01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
ESS01_Bft <- ftable(ESS01_Bg2$player1, ESS01_Bg2$player2)
ESS01_Bft2 <- as.matrix(ESS01_Bft)
numRows <- nrow(ESS01_Bft2)
numCols <- ncol(ESS01_Bft2)
ESS01_Bft3 <- ESS01_Bft2[c(2:numRows) , c(2:numCols)]
ESS01_BTable <- graph.adjacency(ESS01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(ESS01_BTable, vertex.label = V(ESS01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
ESS01_B.clusterCoef <- transitivity(ESS01_BTable, type="global") #cluster coefficient
ESS01_B.degreeCent <- centralization.degree(ESS01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_Bftn <- as.network.matrix(ESS01_Bft)
ESS01_B.netDensity <- network.density(ESS01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_B.entropy <- entropy(ESS01_Bft) #entropy

ESS01_B.netMx <- cbind(ESS01_B.netMx, ESS01_B.clusterCoef, ESS01_B.degreeCent$centralization,
                       ESS01_B.netDensity, ESS01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
ESS01_SFg2 <- data.frame(ESS01_SF)
ESS01_SFg2 <- ESS01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_SFg2$player1
player2vector <- ESS01_SFg2$player2
ESS01_SFg3 <- ESS01_SFg2
ESS01_SFg3$p1inp2vec <- is.element(ESS01_SFg3$player1, player2vector)
ESS01_SFg3$p2inp1vec <- is.element(ESS01_SFg3$player2, player1vector)

addPlayer1 <- ESS01_SFg3[ which(ESS01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_SFg3[ which(ESS01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_SFg2 <- rbind(ESS01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
ESS01_SFft <- ftable(ESS01_SFg2$player1, ESS01_SFg2$player2)
ESS01_SFft2 <- as.matrix(ESS01_SFft)
numRows <- nrow(ESS01_SFft2)
numCols <- ncol(ESS01_SFft2)
ESS01_SFft3 <- ESS01_SFft2[c(2:numRows) , c(2:numCols)]
ESS01_SFTable <- graph.adjacency(ESS01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(ESS01_SFTable, vertex.label = V(ESS01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
ESS01_SF.clusterCoef <- transitivity(ESS01_SFTable, type="global") #cluster coefficient
ESS01_SF.degreeCent <- centralization.degree(ESS01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_SFftn <- as.network.matrix(ESS01_SFft)
ESS01_SF.netDensity <- network.density(ESS01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_SF.entropy <- entropy(ESS01_SFft) #entropy

ESS01_SF.netMx <- cbind(ESS01_SF.netMx, ESS01_SF.clusterCoef, ESS01_SF.degreeCent$centralization,
                        ESS01_SF.netDensity, ESS01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
ESS01_TFg2 <- data.frame(ESS01_TF)
ESS01_TFg2 <- ESS01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_TFg2$player1
player2vector <- ESS01_TFg2$player2
ESS01_TFg3 <- ESS01_TFg2
ESS01_TFg3$p1inp2vec <- is.element(ESS01_TFg3$player1, player2vector)
ESS01_TFg3$p2inp1vec <- is.element(ESS01_TFg3$player2, player1vector)

addPlayer1 <- ESS01_TFg3[ which(ESS01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_TFg3[ which(ESS01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_TFg2 <- rbind(ESS01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
ESS01_TFft <- ftable(ESS01_TFg2$player1, ESS01_TFg2$player2)
ESS01_TFft2 <- as.matrix(ESS01_TFft)
numRows <- nrow(ESS01_TFft2)
numCols <- ncol(ESS01_TFft2)
ESS01_TFft3 <- ESS01_TFft2[c(2:numRows) , c(2:numCols)]
ESS01_TFTable <- graph.adjacency(ESS01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(ESS01_TFTable, vertex.label = V(ESS01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
ESS01_TF.clusterCoef <- transitivity(ESS01_TFTable, type="global") #cluster coefficient
ESS01_TF.degreeCent <- centralization.degree(ESS01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_TFftn <- as.network.matrix(ESS01_TFft)
ESS01_TF.netDensity <- network.density(ESS01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_TF.entropy <- entropy(ESS01_TFft) #entropy

ESS01_TF.netMx <- cbind(ESS01_TF.netMx, ESS01_TF.clusterCoef, ESS01_TF.degreeCent$centralization,
                        ESS01_TF.netDensity, ESS01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
ESS01_SAMg2 <- data.frame(ESS01_SAM)
ESS01_SAMg2 <- ESS01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_SAMg2$player1
player2vector <- ESS01_SAMg2$player2
ESS01_SAMg3 <- ESS01_SAMg2
ESS01_SAMg3$p1inp2vec <- is.element(ESS01_SAMg3$player1, player2vector)
ESS01_SAMg3$p2inp1vec <- is.element(ESS01_SAMg3$player2, player1vector)

addPlayer1 <- ESS01_SAMg3[ which(ESS01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_SAMg3[ which(ESS01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_SAMg2 <- rbind(ESS01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
ESS01_SAMft <- ftable(ESS01_SAMg2$player1, ESS01_SAMg2$player2)
ESS01_SAMft2 <- as.matrix(ESS01_SAMft)
numRows <- nrow(ESS01_SAMft2)
numCols <- ncol(ESS01_SAMft2)
ESS01_SAMft3 <- ESS01_SAMft2[c(2:numRows) , c(2:numCols)]
ESS01_SAMTable <- graph.adjacency(ESS01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(ESS01_SAMTable, vertex.label = V(ESS01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
ESS01_SAM.clusterCoef <- transitivity(ESS01_SAMTable, type="global") #cluster coefficient
ESS01_SAM.degreeCent <- centralization.degree(ESS01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_SAMftn <- as.network.matrix(ESS01_SAMft)
ESS01_SAM.netDensity <- network.density(ESS01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_SAM.entropy <- entropy(ESS01_SAMft) #entropy

ESS01_SAM.netMx <- cbind(ESS01_SAM.netMx, ESS01_SAM.clusterCoef, ESS01_SAM.degreeCent$centralization,
                         ESS01_SAM.netDensity, ESS01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
ESS01_TAMg2 <- data.frame(ESS01_TAM)
ESS01_TAMg2 <- ESS01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_TAMg2$player1
player2vector <- ESS01_TAMg2$player2
ESS01_TAMg3 <- ESS01_TAMg2
ESS01_TAMg3$p1inp2vec <- is.element(ESS01_TAMg3$player1, player2vector)
ESS01_TAMg3$p2inp1vec <- is.element(ESS01_TAMg3$player2, player1vector)

addPlayer1 <- ESS01_TAMg3[ which(ESS01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_TAMg3[ which(ESS01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_TAMg2 <- rbind(ESS01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
ESS01_TAMft <- ftable(ESS01_TAMg2$player1, ESS01_TAMg2$player2)
ESS01_TAMft2 <- as.matrix(ESS01_TAMft)
numRows <- nrow(ESS01_TAMft2)
numCols <- ncol(ESS01_TAMft2)
ESS01_TAMft3 <- ESS01_TAMft2[c(2:numRows) , c(2:numCols)]
ESS01_TAMTable <- graph.adjacency(ESS01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(ESS01_TAMTable, vertex.label = V(ESS01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
ESS01_TAM.clusterCoef <- transitivity(ESS01_TAMTable, type="global") #cluster coefficient
ESS01_TAM.degreeCent <- centralization.degree(ESS01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_TAMftn <- as.network.matrix(ESS01_TAMft)
ESS01_TAM.netDensity <- network.density(ESS01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_TAM.entropy <- entropy(ESS01_TAMft) #entropy

ESS01_TAM.netMx <- cbind(ESS01_TAM.netMx, ESS01_TAM.clusterCoef, ESS01_TAM.degreeCent$centralization,
                         ESS01_TAM.netDensity, ESS01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
ESS01_SDMg2 <- data.frame(ESS01_SDM)
ESS01_SDMg2 <- ESS01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_SDMg2$player1
player2vector <- ESS01_SDMg2$player2
ESS01_SDMg3 <- ESS01_SDMg2
ESS01_SDMg3$p1inp2vec <- is.element(ESS01_SDMg3$player1, player2vector)
ESS01_SDMg3$p2inp1vec <- is.element(ESS01_SDMg3$player2, player1vector)

addPlayer1 <- ESS01_SDMg3[ which(ESS01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_SDMg3[ which(ESS01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_SDMg2 <- rbind(ESS01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
ESS01_SDMft <- ftable(ESS01_SDMg2$player1, ESS01_SDMg2$player2)
ESS01_SDMft2 <- as.matrix(ESS01_SDMft)
numRows <- nrow(ESS01_SDMft2)
numCols <- ncol(ESS01_SDMft2)
ESS01_SDMft3 <- ESS01_SDMft2[c(2:numRows) , c(2:numCols)]
ESS01_SDMTable <- graph.adjacency(ESS01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(ESS01_SDMTable, vertex.label = V(ESS01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
ESS01_SDM.clusterCoef <- transitivity(ESS01_SDMTable, type="global") #cluster coefficient
ESS01_SDM.degreeCent <- centralization.degree(ESS01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_SDMftn <- as.network.matrix(ESS01_SDMft)
ESS01_SDM.netDensity <- network.density(ESS01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_SDM.entropy <- entropy(ESS01_SDMft) #entropy

ESS01_SDM.netMx <- cbind(ESS01_SDM.netMx, ESS01_SDM.clusterCoef, ESS01_SDM.degreeCent$centralization,
                         ESS01_SDM.netDensity, ESS01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
ESS01_TDMg2 <- data.frame(ESS01_TDM)
ESS01_TDMg2 <- ESS01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_TDMg2$player1
player2vector <- ESS01_TDMg2$player2
ESS01_TDMg3 <- ESS01_TDMg2
ESS01_TDMg3$p1inp2vec <- is.element(ESS01_TDMg3$player1, player2vector)
ESS01_TDMg3$p2inp1vec <- is.element(ESS01_TDMg3$player2, player1vector)

addPlayer1 <- ESS01_TDMg3[ which(ESS01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

ESS01_TDMg2 <- rbind(ESS01_TDMg2, addPlayer1)

#Round 1, DM Turnover graph using weighted edges
ESS01_TDMft <- ftable(ESS01_TDMg2$player1, ESS01_TDMg2$player2)
ESS01_TDMft2 <- as.matrix(ESS01_TDMft)
numRows <- nrow(ESS01_TDMft2)
numCols <- ncol(ESS01_TDMft2)
ESS01_TDMft3 <- ESS01_TDMft2[c(2:numRows) , c(1:numCols)] #Had to change no of cols when only adding rows
ESS01_TDMTable <- graph.adjacency(ESS01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(ESS01_TDMTable, vertex.label = V(ESS01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
ESS01_TDM.clusterCoef <- transitivity(ESS01_TDMTable, type="global") #cluster coefficient
ESS01_TDM.degreeCent <- centralization.degree(ESS01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_TDMftn <- as.network.matrix(ESS01_TDMft)
ESS01_TDM.netDensity <- network.density(ESS01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_TDM.entropy <- entropy(ESS01_TDMft) #entropy

ESS01_TDM.netMx <- cbind(ESS01_TDM.netMx, ESS01_TDM.clusterCoef, ESS01_TDM.degreeCent$centralization,
                         ESS01_TDM.netDensity, ESS01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
ESS01_SDg2 <- data.frame(ESS01_SD)
ESS01_SDg2 <- ESS01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_SDg2$player1
player2vector <- ESS01_SDg2$player2
ESS01_SDg3 <- ESS01_SDg2
ESS01_SDg3$p1inp2vec <- is.element(ESS01_SDg3$player1, player2vector)
ESS01_SDg3$p2inp1vec <- is.element(ESS01_SDg3$player2, player1vector)

addPlayer1 <- ESS01_SDg3[ which(ESS01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_SDg3[ which(ESS01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_SDg2 <- rbind(ESS01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
ESS01_SDft <- ftable(ESS01_SDg2$player1, ESS01_SDg2$player2)
ESS01_SDft2 <- as.matrix(ESS01_SDft)
numRows <- nrow(ESS01_SDft2)
numCols <- ncol(ESS01_SDft2)
ESS01_SDft3 <- ESS01_SDft2[c(2:numRows) , c(2:numCols)]
ESS01_SDTable <- graph.adjacency(ESS01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(ESS01_SDTable, vertex.label = V(ESS01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
ESS01_SD.clusterCoef <- transitivity(ESS01_SDTable, type="global") #cluster coefficient
ESS01_SD.degreeCent <- centralization.degree(ESS01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_SDftn <- as.network.matrix(ESS01_SDft)
ESS01_SD.netDensity <- network.density(ESS01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_SD.entropy <- entropy(ESS01_SDft) #entropy

ESS01_SD.netMx <- cbind(ESS01_SD.netMx, ESS01_SD.clusterCoef, ESS01_SD.degreeCent$centralization,
                        ESS01_SD.netDensity, ESS01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************

round = 1
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
ESS01_TDg2 <- data.frame(ESS01_TD)
ESS01_TDg2 <- ESS01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_TDg2$player1
player2vector <- ESS01_TDg2$player2
ESS01_TDg3 <- ESS01_TDg2
ESS01_TDg3$p1inp2vec <- is.element(ESS01_TDg3$player1, player2vector)
ESS01_TDg3$p2inp1vec <- is.element(ESS01_TDg3$player2, player1vector)

addPlayer1 <- ESS01_TDg3[ which(ESS01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_TDg3[ which(ESS01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_TDg2 <- rbind(ESS01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
ESS01_TDft <- ftable(ESS01_TDg2$player1, ESS01_TDg2$player2)
ESS01_TDft2 <- as.matrix(ESS01_TDft)
numRows <- nrow(ESS01_TDft2)
numCols <- ncol(ESS01_TDft2)
ESS01_TDft3 <- ESS01_TDft2[c(2:numRows) , c(2:numCols)]
ESS01_TDTable <- graph.adjacency(ESS01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(ESS01_TDTable, vertex.label = V(ESS01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
ESS01_TD.clusterCoef <- transitivity(ESS01_TDTable, type="global") #cluster coefficient
ESS01_TD.degreeCent <- centralization.degree(ESS01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_TDftn <- as.network.matrix(ESS01_TDft)
ESS01_TD.netDensity <- network.density(ESS01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_TD.entropy <- entropy(ESS01_TDft) #entropy

ESS01_TD.netMx <- cbind(ESS01_TD.netMx, ESS01_TD.clusterCoef, ESS01_TD.degreeCent$centralization,
                        ESS01_TD.netDensity, ESS01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
ESS01_QTg2 <- data.frame(ESS01_QT)
ESS01_QTg2 <- ESS01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS01_QTg2$player1
player2vector <- ESS01_QTg2$player2
ESS01_QTg3 <- ESS01_QTg2
ESS01_QTg3$p1inp2vec <- is.element(ESS01_QTg3$player1, player2vector)
ESS01_QTg3$p2inp1vec <- is.element(ESS01_QTg3$player2, player1vector)

addPlayer1 <- ESS01_QTg3[ which(ESS01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS01_QTg3[ which(ESS01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS01_QTg2 <- rbind(ESS01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
ESS01_QTft <- ftable(ESS01_QTg2$player1, ESS01_QTg2$player2)
ESS01_QTft2 <- as.matrix(ESS01_QTft)
numRows <- nrow(ESS01_QTft2)
numCols <- ncol(ESS01_QTft2)
ESS01_QTft3 <- ESS01_QTft2[c(2:numRows) , c(2:numCols)]
ESS01_QTTable <- graph.adjacency(ESS01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(ESS01_QTTable, vertex.label = V(ESS01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
ESS01_QT.clusterCoef <- transitivity(ESS01_QTTable, type="global") #cluster coefficient
ESS01_QT.degreeCent <- centralization.degree(ESS01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS01_QTftn <- as.network.matrix(ESS01_QTft)
ESS01_QT.netDensity <- network.density(ESS01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS01_QT.entropy <- entropy(ESS01_QTft) #entropy

ESS01_QT.netMx <- cbind(ESS01_QT.netMx, ESS01_QT.clusterCoef, ESS01_QT.degreeCent$centralization,
                        ESS01_QT.netDensity, ESS01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS01_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "Goal_F"
FRE01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
FRE01_Gg2 <- data.frame(FRE01_G)
FRE01_Gg2 <- FRE01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_Gg2$player1
player2vector <- FRE01_Gg2$player2
FRE01_Gg3 <- FRE01_Gg2
FRE01_Gg3$p1inp2vec <- is.element(FRE01_Gg3$player1, player2vector)
FRE01_Gg3$p2inp1vec <- is.element(FRE01_Gg3$player2, player1vector)

addPlayer1 <- FRE01_Gg3[ which(FRE01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_Gg3[ which(FRE01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_Gg2 <- rbind(FRE01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
FRE01_Gft <- ftable(FRE01_Gg2$player1, FRE01_Gg2$player2)
FRE01_Gft2 <- as.matrix(FRE01_Gft)
numRows <- nrow(FRE01_Gft2)
numCols <- ncol(FRE01_Gft2)
FRE01_Gft3 <- FRE01_Gft2[c(2:numRows) , c(2:numCols)]
FRE01_GTable <- graph.adjacency(FRE01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(FRE01_GTable, vertex.label = V(FRE01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
FRE01_G.clusterCoef <- transitivity(FRE01_GTable, type="global") #cluster coefficient
FRE01_G.degreeCent <- centralization.degree(FRE01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_Gftn <- as.network.matrix(FRE01_Gft)
FRE01_G.netDensity <- network.density(FRE01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_G.entropy <- entropy(FRE01_Gft) #entropy

FRE01_G.netMx <- cbind(FRE01_G.netMx, FRE01_G.clusterCoef, FRE01_G.degreeCent$centralization,
                       FRE01_G.netDensity, FRE01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "Behind_F"
FRE01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
FRE01_Bg2 <- data.frame(FRE01_B)
FRE01_Bg2 <- FRE01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_Bg2$player1
player2vector <- FRE01_Bg2$player2
FRE01_Bg3 <- FRE01_Bg2
FRE01_Bg3$p1inp2vec <- is.element(FRE01_Bg3$player1, player2vector)
FRE01_Bg3$p2inp1vec <- is.element(FRE01_Bg3$player2, player1vector)

addPlayer1 <- FRE01_Bg3[ which(FRE01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_Bg3[ which(FRE01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_Bg2 <- rbind(FRE01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
FRE01_Bft <- ftable(FRE01_Bg2$player1, FRE01_Bg2$player2)
FRE01_Bft2 <- as.matrix(FRE01_Bft)
numRows <- nrow(FRE01_Bft2)
numCols <- ncol(FRE01_Bft2)
FRE01_Bft3 <- FRE01_Bft2[c(2:numRows) , c(2:numCols)]
FRE01_BTable <- graph.adjacency(FRE01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(FRE01_BTable, vertex.label = V(FRE01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
FRE01_B.clusterCoef <- transitivity(FRE01_BTable, type="global") #cluster coefficient
FRE01_B.degreeCent <- centralization.degree(FRE01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_Bftn <- as.network.matrix(FRE01_Bft)
FRE01_B.netDensity <- network.density(FRE01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_B.entropy <- entropy(FRE01_Bft) #entropy

FRE01_B.netMx <- cbind(FRE01_B.netMx, FRE01_B.clusterCoef, FRE01_B.degreeCent$centralization,
                       FRE01_B.netDensity, FRE01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************

round = 1
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
FRE01_SFg2 <- data.frame(FRE01_SF)
FRE01_SFg2 <- FRE01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_SFg2$player1
player2vector <- FRE01_SFg2$player2
FRE01_SFg3 <- FRE01_SFg2
FRE01_SFg3$p1inp2vec <- is.element(FRE01_SFg3$player1, player2vector)
FRE01_SFg3$p2inp1vec <- is.element(FRE01_SFg3$player2, player1vector)

addPlayer1 <- FRE01_SFg3[ which(FRE01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_SFg3[ which(FRE01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_SFg2 <- rbind(FRE01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
FRE01_SFft <- ftable(FRE01_SFg2$player1, FRE01_SFg2$player2)
FRE01_SFft2 <- as.matrix(FRE01_SFft)
numRows <- nrow(FRE01_SFft2)
numCols <- ncol(FRE01_SFft2)
FRE01_SFft3 <- FRE01_SFft2[c(2:numRows) , c(2:numCols)]
FRE01_SFTable <- graph.adjacency(FRE01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(FRE01_SFTable, vertex.label = V(FRE01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
FRE01_SF.clusterCoef <- transitivity(FRE01_SFTable, type="global") #cluster coefficient
FRE01_SF.degreeCent <- centralization.degree(FRE01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_SFftn <- as.network.matrix(FRE01_SFft)
FRE01_SF.netDensity <- network.density(FRE01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_SF.entropy <- entropy(FRE01_SFft) #entropy

FRE01_SF.netMx <- cbind(FRE01_SF.netMx, FRE01_SF.clusterCoef, FRE01_SF.degreeCent$centralization,
                        FRE01_SF.netDensity, FRE01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
FRE01_TFg2 <- data.frame(FRE01_TF)
FRE01_TFg2 <- FRE01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_TFg2$player1
player2vector <- FRE01_TFg2$player2
FRE01_TFg3 <- FRE01_TFg2
FRE01_TFg3$p1inp2vec <- is.element(FRE01_TFg3$player1, player2vector)
FRE01_TFg3$p2inp1vec <- is.element(FRE01_TFg3$player2, player1vector)

addPlayer1 <- FRE01_TFg3[ which(FRE01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_TFg3[ which(FRE01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_TFg2 <- rbind(FRE01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
FRE01_TFft <- ftable(FRE01_TFg2$player1, FRE01_TFg2$player2)
FRE01_TFft2 <- as.matrix(FRE01_TFft)
numRows <- nrow(FRE01_TFft2)
numCols <- ncol(FRE01_TFft2)
FRE01_TFft3 <- FRE01_TFft2[c(2:numRows) , c(2:numCols)]
FRE01_TFTable <- graph.adjacency(FRE01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(FRE01_TFTable, vertex.label = V(FRE01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
FRE01_TF.clusterCoef <- transitivity(FRE01_TFTable, type="global") #cluster coefficient
FRE01_TF.degreeCent <- centralization.degree(FRE01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_TFftn <- as.network.matrix(FRE01_TFft)
FRE01_TF.netDensity <- network.density(FRE01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_TF.entropy <- entropy(FRE01_TFft) #entropy

FRE01_TF.netMx <- cbind(FRE01_TF.netMx, FRE01_TF.clusterCoef, FRE01_TF.degreeCent$centralization,
                        FRE01_TF.netDensity, FRE01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************

round = 1
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
FRE01_SAMg2 <- data.frame(FRE01_SAM)
FRE01_SAMg2 <- FRE01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_SAMg2$player1
player2vector <- FRE01_SAMg2$player2
FRE01_SAMg3 <- FRE01_SAMg2
FRE01_SAMg3$p1inp2vec <- is.element(FRE01_SAMg3$player1, player2vector)
FRE01_SAMg3$p2inp1vec <- is.element(FRE01_SAMg3$player2, player1vector)

addPlayer1 <- FRE01_SAMg3[ which(FRE01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_SAMg3[ which(FRE01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_SAMg2 <- rbind(FRE01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
FRE01_SAMft <- ftable(FRE01_SAMg2$player1, FRE01_SAMg2$player2)
FRE01_SAMft2 <- as.matrix(FRE01_SAMft)
numRows <- nrow(FRE01_SAMft2)
numCols <- ncol(FRE01_SAMft2)
FRE01_SAMft3 <- FRE01_SAMft2[c(2:numRows) , c(2:numCols)]
FRE01_SAMTable <- graph.adjacency(FRE01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(FRE01_SAMTable, vertex.label = V(FRE01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
FRE01_SAM.clusterCoef <- transitivity(FRE01_SAMTable, type="global") #cluster coefficient
FRE01_SAM.degreeCent <- centralization.degree(FRE01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_SAMftn <- as.network.matrix(FRE01_SAMft)
FRE01_SAM.netDensity <- network.density(FRE01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_SAM.entropy <- entropy(FRE01_SAMft) #entropy

FRE01_SAM.netMx <- cbind(FRE01_SAM.netMx, FRE01_SAM.clusterCoef, FRE01_SAM.degreeCent$centralization,
                         FRE01_SAM.netDensity, FRE01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
FRE01_TAMg2 <- data.frame(FRE01_TAM)
FRE01_TAMg2 <- FRE01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_TAMg2$player1
player2vector <- FRE01_TAMg2$player2
FRE01_TAMg3 <- FRE01_TAMg2
FRE01_TAMg3$p1inp2vec <- is.element(FRE01_TAMg3$player1, player2vector)
FRE01_TAMg3$p2inp1vec <- is.element(FRE01_TAMg3$player2, player1vector)

addPlayer1 <- FRE01_TAMg3[ which(FRE01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_TAMg3[ which(FRE01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_TAMg2 <- rbind(FRE01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
FRE01_TAMft <- ftable(FRE01_TAMg2$player1, FRE01_TAMg2$player2)
FRE01_TAMft2 <- as.matrix(FRE01_TAMft)
numRows <- nrow(FRE01_TAMft2)
numCols <- ncol(FRE01_TAMft2)
FRE01_TAMft3 <- FRE01_TAMft2[c(2:numRows) , c(2:numCols)]
FRE01_TAMTable <- graph.adjacency(FRE01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(FRE01_TAMTable, vertex.label = V(FRE01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
FRE01_TAM.clusterCoef <- transitivity(FRE01_TAMTable, type="global") #cluster coefficient
FRE01_TAM.degreeCent <- centralization.degree(FRE01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_TAMftn <- as.network.matrix(FRE01_TAMft)
FRE01_TAM.netDensity <- network.density(FRE01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_TAM.entropy <- entropy(FRE01_TAMft) #entropy

FRE01_TAM.netMx <- cbind(FRE01_TAM.netMx, FRE01_TAM.clusterCoef, FRE01_TAM.degreeCent$centralization,
                         FRE01_TAM.netDensity, FRE01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
FRE01_SDMg2 <- data.frame(FRE01_SDM)
FRE01_SDMg2 <- FRE01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_SDMg2$player1
player2vector <- FRE01_SDMg2$player2
FRE01_SDMg3 <- FRE01_SDMg2
FRE01_SDMg3$p1inp2vec <- is.element(FRE01_SDMg3$player1, player2vector)
FRE01_SDMg3$p2inp1vec <- is.element(FRE01_SDMg3$player2, player1vector)

addPlayer1 <- FRE01_SDMg3[ which(FRE01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_SDMg3[ which(FRE01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_SDMg2 <- rbind(FRE01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
FRE01_SDMft <- ftable(FRE01_SDMg2$player1, FRE01_SDMg2$player2)
FRE01_SDMft2 <- as.matrix(FRE01_SDMft)
numRows <- nrow(FRE01_SDMft2)
numCols <- ncol(FRE01_SDMft2)
FRE01_SDMft3 <- FRE01_SDMft2[c(2:numRows) , c(2:numCols)]
FRE01_SDMTable <- graph.adjacency(FRE01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(FRE01_SDMTable, vertex.label = V(FRE01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
FRE01_SDM.clusterCoef <- transitivity(FRE01_SDMTable, type="global") #cluster coefficient
FRE01_SDM.degreeCent <- centralization.degree(FRE01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_SDMftn <- as.network.matrix(FRE01_SDMft)
FRE01_SDM.netDensity <- network.density(FRE01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_SDM.entropy <- entropy(FRE01_SDMft) #entropy

FRE01_SDM.netMx <- cbind(FRE01_SDM.netMx, FRE01_SDM.clusterCoef, FRE01_SDM.degreeCent$centralization,
                         FRE01_SDM.netDensity, FRE01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
FRE01_TDMg2 <- data.frame(FRE01_TDM)
FRE01_TDMg2 <- FRE01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_TDMg2$player1
player2vector <- FRE01_TDMg2$player2
FRE01_TDMg3 <- FRE01_TDMg2
FRE01_TDMg3$p1inp2vec <- is.element(FRE01_TDMg3$player1, player2vector)
FRE01_TDMg3$p2inp1vec <- is.element(FRE01_TDMg3$player2, player1vector)

addPlayer1 <- FRE01_TDMg3[ which(FRE01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_TDMg3[ which(FRE01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_TDMg2 <- rbind(FRE01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
FRE01_TDMft <- ftable(FRE01_TDMg2$player1, FRE01_TDMg2$player2)
FRE01_TDMft2 <- as.matrix(FRE01_TDMft)
numRows <- nrow(FRE01_TDMft2)
numCols <- ncol(FRE01_TDMft2)
FRE01_TDMft3 <- FRE01_TDMft2[c(2:numRows) , c(2:numCols)]
FRE01_TDMTable <- graph.adjacency(FRE01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(FRE01_TDMTable, vertex.label = V(FRE01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
FRE01_TDM.clusterCoef <- transitivity(FRE01_TDMTable, type="global") #cluster coefficient
FRE01_TDM.degreeCent <- centralization.degree(FRE01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_TDMftn <- as.network.matrix(FRE01_TDMft)
FRE01_TDM.netDensity <- network.density(FRE01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_TDM.entropy <- entropy(FRE01_TDMft) #entropy

FRE01_TDM.netMx <- cbind(FRE01_TDM.netMx, FRE01_TDM.clusterCoef, FRE01_TDM.degreeCent$centralization,
                         FRE01_TDM.netDensity, FRE01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
FRE01_SDg2 <- data.frame(FRE01_SD)
FRE01_SDg2 <- FRE01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_SDg2$player1
player2vector <- FRE01_SDg2$player2
FRE01_SDg3 <- FRE01_SDg2
FRE01_SDg3$p1inp2vec <- is.element(FRE01_SDg3$player1, player2vector)
FRE01_SDg3$p2inp1vec <- is.element(FRE01_SDg3$player2, player1vector)

addPlayer1 <- FRE01_SDg3[ which(FRE01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_SDg3[ which(FRE01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_SDg2 <- rbind(FRE01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
FRE01_SDft <- ftable(FRE01_SDg2$player1, FRE01_SDg2$player2)
FRE01_SDft2 <- as.matrix(FRE01_SDft)
numRows <- nrow(FRE01_SDft2)
numCols <- ncol(FRE01_SDft2)
FRE01_SDft3 <- FRE01_SDft2[c(2:numRows) , c(2:numCols)]
FRE01_SDTable <- graph.adjacency(FRE01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(FRE01_SDTable, vertex.label = V(FRE01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
FRE01_SD.clusterCoef <- transitivity(FRE01_SDTable, type="global") #cluster coefficient
FRE01_SD.degreeCent <- centralization.degree(FRE01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_SDftn <- as.network.matrix(FRE01_SDft)
FRE01_SD.netDensity <- network.density(FRE01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_SD.entropy <- entropy(FRE01_SDft) #entropy

FRE01_SD.netMx <- cbind(FRE01_SD.netMx, FRE01_SD.clusterCoef, FRE01_SD.degreeCent$centralization,
                        FRE01_SD.netDensity, FRE01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
FRE01_TDg2 <- data.frame(FRE01_TD)
FRE01_TDg2 <- FRE01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_TDg2$player1
player2vector <- FRE01_TDg2$player2
FRE01_TDg3 <- FRE01_TDg2
FRE01_TDg3$p1inp2vec <- is.element(FRE01_TDg3$player1, player2vector)
FRE01_TDg3$p2inp1vec <- is.element(FRE01_TDg3$player2, player1vector)

addPlayer1 <- FRE01_TDg3[ which(FRE01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_TDg3[ which(FRE01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_TDg2 <- rbind(FRE01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
FRE01_TDft <- ftable(FRE01_TDg2$player1, FRE01_TDg2$player2)
FRE01_TDft2 <- as.matrix(FRE01_TDft)
numRows <- nrow(FRE01_TDft2)
numCols <- ncol(FRE01_TDft2)
FRE01_TDft3 <- FRE01_TDft2[c(2:numRows) , c(2:numCols)]
FRE01_TDTable <- graph.adjacency(FRE01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(FRE01_TDTable, vertex.label = V(FRE01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
FRE01_TD.clusterCoef <- transitivity(FRE01_TDTable, type="global") #cluster coefficient
FRE01_TD.degreeCent <- centralization.degree(FRE01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_TDftn <- as.network.matrix(FRE01_TDft)
FRE01_TD.netDensity <- network.density(FRE01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_TD.entropy <- entropy(FRE01_TDft) #entropy

FRE01_TD.netMx <- cbind(FRE01_TD.netMx, FRE01_TD.clusterCoef, FRE01_TD.degreeCent$centralization,
                        FRE01_TD.netDensity, FRE01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
FRE01_QTg2 <- data.frame(FRE01_QT)
FRE01_QTg2 <- FRE01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE01_QTg2$player1
player2vector <- FRE01_QTg2$player2
FRE01_QTg3 <- FRE01_QTg2
FRE01_QTg3$p1inp2vec <- is.element(FRE01_QTg3$player1, player2vector)
FRE01_QTg3$p2inp1vec <- is.element(FRE01_QTg3$player2, player1vector)

addPlayer1 <- FRE01_QTg3[ which(FRE01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE01_QTg3[ which(FRE01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE01_QTg2 <- rbind(FRE01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
FRE01_QTft <- ftable(FRE01_QTg2$player1, FRE01_QTg2$player2)
FRE01_QTft2 <- as.matrix(FRE01_QTft)
numRows <- nrow(FRE01_QTft2)
numCols <- ncol(FRE01_QTft2)
FRE01_QTft3 <- FRE01_QTft2[c(2:numRows) , c(2:numCols)]
FRE01_QTTable <- graph.adjacency(FRE01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(FRE01_QTTable, vertex.label = V(FRE01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
FRE01_QT.clusterCoef <- transitivity(FRE01_QTTable, type="global") #cluster coefficient
FRE01_QT.degreeCent <- centralization.degree(FRE01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE01_QTftn <- as.network.matrix(FRE01_QTft)
FRE01_QT.netDensity <- network.density(FRE01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE01_QT.entropy <- entropy(FRE01_QTft) #entropy

FRE01_QT.netMx <- cbind(FRE01_QT.netMx, FRE01_QT.clusterCoef, FRE01_QT.degreeCent$centralization,
                        FRE01_QT.netDensity, FRE01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE01_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
GCFC01_Gg2 <- data.frame(GCFC01_G)
GCFC01_Gg2 <- GCFC01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_Gg2$player1
player2vector <- GCFC01_Gg2$player2
GCFC01_Gg3 <- GCFC01_Gg2
GCFC01_Gg3$p1inp2vec <- is.element(GCFC01_Gg3$player1, player2vector)
GCFC01_Gg3$p2inp1vec <- is.element(GCFC01_Gg3$player2, player1vector)

addPlayer1 <- GCFC01_Gg3[ which(GCFC01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_Gg3[ which(GCFC01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_Gg2 <- rbind(GCFC01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
GCFC01_Gft <- ftable(GCFC01_Gg2$player1, GCFC01_Gg2$player2)
GCFC01_Gft2 <- as.matrix(GCFC01_Gft)
numRows <- nrow(GCFC01_Gft2)
numCols <- ncol(GCFC01_Gft2)
GCFC01_Gft3 <- GCFC01_Gft2[c(2:numRows) , c(2:numCols)]
GCFC01_GTable <- graph.adjacency(GCFC01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(GCFC01_GTable, vertex.label = V(GCFC01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
GCFC01_G.clusterCoef <- transitivity(GCFC01_GTable, type="global") #cluster coefficient
GCFC01_G.degreeCent <- centralization.degree(GCFC01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_Gftn <- as.network.matrix(GCFC01_Gft)
GCFC01_G.netDensity <- network.density(GCFC01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_G.entropy <- entropy(GCFC01_Gft) #entropy

GCFC01_G.netMx <- cbind(GCFC01_G.netMx, GCFC01_G.clusterCoef, GCFC01_G.degreeCent$centralization,
                        GCFC01_G.netDensity, GCFC01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_G.netMx) <- varnames

#Round 1, Behind***************************************************************

round = 1
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
GCFC01_Bg2 <- data.frame(GCFC01_B)
GCFC01_Bg2 <- GCFC01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_Bg2$player1
player2vector <- GCFC01_Bg2$player2
GCFC01_Bg3 <- GCFC01_Bg2
GCFC01_Bg3$p1inp2vec <- is.element(GCFC01_Bg3$player1, player2vector)
GCFC01_Bg3$p2inp1vec <- is.element(GCFC01_Bg3$player2, player1vector)

addPlayer1 <- GCFC01_Bg3[ which(GCFC01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_Bg3[ which(GCFC01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_Bg2 <- rbind(GCFC01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
GCFC01_Bft <- ftable(GCFC01_Bg2$player1, GCFC01_Bg2$player2)
GCFC01_Bft2 <- as.matrix(GCFC01_Bft)
numRows <- nrow(GCFC01_Bft2)
numCols <- ncol(GCFC01_Bft2)
GCFC01_Bft3 <- GCFC01_Bft2[c(2:numRows) , c(2:numCols)]
GCFC01_BTable <- graph.adjacency(GCFC01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(GCFC01_BTable, vertex.label = V(GCFC01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
GCFC01_B.clusterCoef <- transitivity(GCFC01_BTable, type="global") #cluster coefficient
GCFC01_B.degreeCent <- centralization.degree(GCFC01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_Bftn <- as.network.matrix(GCFC01_Bft)
GCFC01_B.netDensity <- network.density(GCFC01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_B.entropy <- entropy(GCFC01_Bft) #entropy

GCFC01_B.netMx <- cbind(GCFC01_B.netMx, GCFC01_B.clusterCoef, GCFC01_B.degreeCent$centralization,
                        GCFC01_B.netDensity, GCFC01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************

round = 1
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
GCFC01_SFg2 <- data.frame(GCFC01_SF)
GCFC01_SFg2 <- GCFC01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_SFg2$player1
player2vector <- GCFC01_SFg2$player2
GCFC01_SFg3 <- GCFC01_SFg2
GCFC01_SFg3$p1inp2vec <- is.element(GCFC01_SFg3$player1, player2vector)
GCFC01_SFg3$p2inp1vec <- is.element(GCFC01_SFg3$player2, player1vector)

addPlayer1 <- GCFC01_SFg3[ which(GCFC01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC01_SFg2 <- rbind(GCFC01_SFg2, addPlayer1)

#Round 1, FWD Stoppage graph using weighted edges
GCFC01_SFft <- ftable(GCFC01_SFg2$player1, GCFC01_SFg2$player2)
GCFC01_SFft2 <- as.matrix(GCFC01_SFft)
numRows <- nrow(GCFC01_SFft2)
numCols <- ncol(GCFC01_SFft2)
GCFC01_SFft3 <- GCFC01_SFft2[c(2:numRows) , c(1:numCols)]
GCFC01_SFTable <- graph.adjacency(GCFC01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(GCFC01_SFTable, vertex.label = V(GCFC01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
GCFC01_SF.clusterCoef <- transitivity(GCFC01_SFTable, type="global") #cluster coefficient
GCFC01_SF.degreeCent <- centralization.degree(GCFC01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_SFftn <- as.network.matrix(GCFC01_SFft)
GCFC01_SF.netDensity <- network.density(GCFC01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_SF.entropy <- entropy(GCFC01_SFft) #entropy

GCFC01_SF.netMx <- cbind(GCFC01_SF.netMx, GCFC01_SF.clusterCoef, GCFC01_SF.degreeCent$centralization,
                         GCFC01_SF.netDensity, GCFC01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
GCFC01_TFg2 <- data.frame(GCFC01_TF)
GCFC01_TFg2 <- GCFC01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_TFg2$player1
player2vector <- GCFC01_TFg2$player2
GCFC01_TFg3 <- GCFC01_TFg2
GCFC01_TFg3$p1inp2vec <- is.element(GCFC01_TFg3$player1, player2vector)
GCFC01_TFg3$p2inp1vec <- is.element(GCFC01_TFg3$player2, player1vector)

addPlayer1 <- GCFC01_TFg3[ which(GCFC01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_TFg3[ which(GCFC01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_TFg2 <- rbind(GCFC01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
GCFC01_TFft <- ftable(GCFC01_TFg2$player1, GCFC01_TFg2$player2)
GCFC01_TFft2 <- as.matrix(GCFC01_TFft)
numRows <- nrow(GCFC01_TFft2)
numCols <- ncol(GCFC01_TFft2)
GCFC01_TFft3 <- GCFC01_TFft2[c(2:numRows) , c(2:numCols)]
GCFC01_TFTable <- graph.adjacency(GCFC01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(GCFC01_TFTable, vertex.label = V(GCFC01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
GCFC01_TF.clusterCoef <- transitivity(GCFC01_TFTable, type="global") #cluster coefficient
GCFC01_TF.degreeCent <- centralization.degree(GCFC01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_TFftn <- as.network.matrix(GCFC01_TFft)
GCFC01_TF.netDensity <- network.density(GCFC01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_TF.entropy <- entropy(GCFC01_TFft) #entropy

GCFC01_TF.netMx <- cbind(GCFC01_TF.netMx, GCFC01_TF.clusterCoef, GCFC01_TF.degreeCent$centralization,
                         GCFC01_TF.netDensity, GCFC01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
GCFC01_SAMg2 <- data.frame(GCFC01_SAM)
GCFC01_SAMg2 <- GCFC01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_SAMg2$player1
player2vector <- GCFC01_SAMg2$player2
GCFC01_SAMg3 <- GCFC01_SAMg2
GCFC01_SAMg3$p1inp2vec <- is.element(GCFC01_SAMg3$player1, player2vector)
GCFC01_SAMg3$p2inp1vec <- is.element(GCFC01_SAMg3$player2, player1vector)

addPlayer1 <- GCFC01_SAMg3[ which(GCFC01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_SAMg3[ which(GCFC01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_SAMg2 <- rbind(GCFC01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
GCFC01_SAMft <- ftable(GCFC01_SAMg2$player1, GCFC01_SAMg2$player2)
GCFC01_SAMft2 <- as.matrix(GCFC01_SAMft)
numRows <- nrow(GCFC01_SAMft2)
numCols <- ncol(GCFC01_SAMft2)
GCFC01_SAMft3 <- GCFC01_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC01_SAMTable <- graph.adjacency(GCFC01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(GCFC01_SAMTable, vertex.label = V(GCFC01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
GCFC01_SAM.clusterCoef <- transitivity(GCFC01_SAMTable, type="global") #cluster coefficient
GCFC01_SAM.degreeCent <- centralization.degree(GCFC01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_SAMftn <- as.network.matrix(GCFC01_SAMft)
GCFC01_SAM.netDensity <- network.density(GCFC01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_SAM.entropy <- entropy(GCFC01_SAMft) #entropy

GCFC01_SAM.netMx <- cbind(GCFC01_SAM.netMx, GCFC01_SAM.clusterCoef, GCFC01_SAM.degreeCent$centralization,
                          GCFC01_SAM.netDensity, GCFC01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
GCFC01_TAMg2 <- data.frame(GCFC01_TAM)
GCFC01_TAMg2 <- GCFC01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_TAMg2$player1
player2vector <- GCFC01_TAMg2$player2
GCFC01_TAMg3 <- GCFC01_TAMg2
GCFC01_TAMg3$p1inp2vec <- is.element(GCFC01_TAMg3$player1, player2vector)
GCFC01_TAMg3$p2inp1vec <- is.element(GCFC01_TAMg3$player2, player1vector)

addPlayer1 <- GCFC01_TAMg3[ which(GCFC01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_TAMg3[ which(GCFC01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_TAMg2 <- rbind(GCFC01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
GCFC01_TAMft <- ftable(GCFC01_TAMg2$player1, GCFC01_TAMg2$player2)
GCFC01_TAMft2 <- as.matrix(GCFC01_TAMft)
numRows <- nrow(GCFC01_TAMft2)
numCols <- ncol(GCFC01_TAMft2)
GCFC01_TAMft3 <- GCFC01_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC01_TAMTable <- graph.adjacency(GCFC01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(GCFC01_TAMTable, vertex.label = V(GCFC01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
GCFC01_TAM.clusterCoef <- transitivity(GCFC01_TAMTable, type="global") #cluster coefficient
GCFC01_TAM.degreeCent <- centralization.degree(GCFC01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_TAMftn <- as.network.matrix(GCFC01_TAMft)
GCFC01_TAM.netDensity <- network.density(GCFC01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_TAM.entropy <- entropy(GCFC01_TAMft) #entropy

GCFC01_TAM.netMx <- cbind(GCFC01_TAM.netMx, GCFC01_TAM.clusterCoef, GCFC01_TAM.degreeCent$centralization,
                          GCFC01_TAM.netDensity, GCFC01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
GCFC01_SDMg2 <- data.frame(GCFC01_SDM)
GCFC01_SDMg2 <- GCFC01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_SDMg2$player1
player2vector <- GCFC01_SDMg2$player2
GCFC01_SDMg3 <- GCFC01_SDMg2
GCFC01_SDMg3$p1inp2vec <- is.element(GCFC01_SDMg3$player1, player2vector)
GCFC01_SDMg3$p2inp1vec <- is.element(GCFC01_SDMg3$player2, player1vector)

addPlayer1 <- GCFC01_SDMg3[ which(GCFC01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_SDMg3[ which(GCFC01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_SDMg2 <- rbind(GCFC01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
GCFC01_SDMft <- ftable(GCFC01_SDMg2$player1, GCFC01_SDMg2$player2)
GCFC01_SDMft2 <- as.matrix(GCFC01_SDMft)
numRows <- nrow(GCFC01_SDMft2)
numCols <- ncol(GCFC01_SDMft2)
GCFC01_SDMft3 <- GCFC01_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC01_SDMTable <- graph.adjacency(GCFC01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(GCFC01_SDMTable, vertex.label = V(GCFC01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
GCFC01_SDM.clusterCoef <- transitivity(GCFC01_SDMTable, type="global") #cluster coefficient
GCFC01_SDM.degreeCent <- centralization.degree(GCFC01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_SDMftn <- as.network.matrix(GCFC01_SDMft)
GCFC01_SDM.netDensity <- network.density(GCFC01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_SDM.entropy <- entropy(GCFC01_SDMft) #entropy

GCFC01_SDM.netMx <- cbind(GCFC01_SDM.netMx, GCFC01_SDM.clusterCoef, GCFC01_SDM.degreeCent$centralization,
                          GCFC01_SDM.netDensity, GCFC01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
GCFC01_TDMg2 <- data.frame(GCFC01_TDM)
GCFC01_TDMg2 <- GCFC01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_TDMg2$player1
player2vector <- GCFC01_TDMg2$player2
GCFC01_TDMg3 <- GCFC01_TDMg2
GCFC01_TDMg3$p1inp2vec <- is.element(GCFC01_TDMg3$player1, player2vector)
GCFC01_TDMg3$p2inp1vec <- is.element(GCFC01_TDMg3$player2, player1vector)

addPlayer1 <- GCFC01_TDMg3[ which(GCFC01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_TDMg3[ which(GCFC01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_TDMg2 <- rbind(GCFC01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
GCFC01_TDMft <- ftable(GCFC01_TDMg2$player1, GCFC01_TDMg2$player2)
GCFC01_TDMft2 <- as.matrix(GCFC01_TDMft)
numRows <- nrow(GCFC01_TDMft2)
numCols <- ncol(GCFC01_TDMft2)
GCFC01_TDMft3 <- GCFC01_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC01_TDMTable <- graph.adjacency(GCFC01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(GCFC01_TDMTable, vertex.label = V(GCFC01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
GCFC01_TDM.clusterCoef <- transitivity(GCFC01_TDMTable, type="global") #cluster coefficient
GCFC01_TDM.degreeCent <- centralization.degree(GCFC01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_TDMftn <- as.network.matrix(GCFC01_TDMft)
GCFC01_TDM.netDensity <- network.density(GCFC01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_TDM.entropy <- entropy(GCFC01_TDMft) #entropy

GCFC01_TDM.netMx <- cbind(GCFC01_TDM.netMx, GCFC01_TDM.clusterCoef, GCFC01_TDM.degreeCent$centralization,
                          GCFC01_TDM.netDensity, GCFC01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
GCFC01_SDg2 <- data.frame(GCFC01_SD)
GCFC01_SDg2 <- GCFC01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_SDg2$player1
player2vector <- GCFC01_SDg2$player2
GCFC01_SDg3 <- GCFC01_SDg2
GCFC01_SDg3$p1inp2vec <- is.element(GCFC01_SDg3$player1, player2vector)
GCFC01_SDg3$p2inp1vec <- is.element(GCFC01_SDg3$player2, player1vector)

addPlayer1 <- GCFC01_SDg3[ which(GCFC01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_SDg3[ which(GCFC01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_SDg2 <- rbind(GCFC01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
GCFC01_SDft <- ftable(GCFC01_SDg2$player1, GCFC01_SDg2$player2)
GCFC01_SDft2 <- as.matrix(GCFC01_SDft)
numRows <- nrow(GCFC01_SDft2)
numCols <- ncol(GCFC01_SDft2)
GCFC01_SDft3 <- GCFC01_SDft2[c(2:numRows) , c(2:numCols)]
GCFC01_SDTable <- graph.adjacency(GCFC01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(GCFC01_SDTable, vertex.label = V(GCFC01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
GCFC01_SD.clusterCoef <- transitivity(GCFC01_SDTable, type="global") #cluster coefficient
GCFC01_SD.degreeCent <- centralization.degree(GCFC01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_SDftn <- as.network.matrix(GCFC01_SDft)
GCFC01_SD.netDensity <- network.density(GCFC01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_SD.entropy <- entropy(GCFC01_SDft) #entropy

GCFC01_SD.netMx <- cbind(GCFC01_SD.netMx, GCFC01_SD.clusterCoef, GCFC01_SD.degreeCent$centralization,
                         GCFC01_SD.netDensity, GCFC01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************

round = 1
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
GCFC01_TDg2 <- data.frame(GCFC01_TD)
GCFC01_TDg2 <- GCFC01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_TDg2$player1
player2vector <- GCFC01_TDg2$player2
GCFC01_TDg3 <- GCFC01_TDg2
GCFC01_TDg3$p1inp2vec <- is.element(GCFC01_TDg3$player1, player2vector)
GCFC01_TDg3$p2inp1vec <- is.element(GCFC01_TDg3$player2, player1vector)

addPlayer1 <- GCFC01_TDg3[ which(GCFC01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_TDg3[ which(GCFC01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_TDg2 <- rbind(GCFC01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
GCFC01_TDft <- ftable(GCFC01_TDg2$player1, GCFC01_TDg2$player2)
GCFC01_TDft2 <- as.matrix(GCFC01_TDft)
numRows <- nrow(GCFC01_TDft2)
numCols <- ncol(GCFC01_TDft2)
GCFC01_TDft3 <- GCFC01_TDft2[c(2:numRows) , c(2:numCols)]
GCFC01_TDTable <- graph.adjacency(GCFC01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(GCFC01_TDTable, vertex.label = V(GCFC01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
GCFC01_TD.clusterCoef <- transitivity(GCFC01_TDTable, type="global") #cluster coefficient
GCFC01_TD.degreeCent <- centralization.degree(GCFC01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_TDftn <- as.network.matrix(GCFC01_TDft)
GCFC01_TD.netDensity <- network.density(GCFC01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_TD.entropy <- entropy(GCFC01_TDft) #entropy

GCFC01_TD.netMx <- cbind(GCFC01_TD.netMx, GCFC01_TD.clusterCoef, GCFC01_TD.degreeCent$centralization,
                         GCFC01_TD.netDensity, GCFC01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
GCFC01_QTg2 <- data.frame(GCFC01_QT)
GCFC01_QTg2 <- GCFC01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC01_QTg2$player1
player2vector <- GCFC01_QTg2$player2
GCFC01_QTg3 <- GCFC01_QTg2
GCFC01_QTg3$p1inp2vec <- is.element(GCFC01_QTg3$player1, player2vector)
GCFC01_QTg3$p2inp1vec <- is.element(GCFC01_QTg3$player2, player1vector)

addPlayer1 <- GCFC01_QTg3[ which(GCFC01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC01_QTg3[ which(GCFC01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC01_QTg2 <- rbind(GCFC01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
GCFC01_QTft <- ftable(GCFC01_QTg2$player1, GCFC01_QTg2$player2)
GCFC01_QTft2 <- as.matrix(GCFC01_QTft)
numRows <- nrow(GCFC01_QTft2)
numCols <- ncol(GCFC01_QTft2)
GCFC01_QTft3 <- GCFC01_QTft2[c(2:numRows) , c(2:numCols)]
GCFC01_QTTable <- graph.adjacency(GCFC01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(GCFC01_QTTable, vertex.label = V(GCFC01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
GCFC01_QT.clusterCoef <- transitivity(GCFC01_QTTable, type="global") #cluster coefficient
GCFC01_QT.degreeCent <- centralization.degree(GCFC01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC01_QTftn <- as.network.matrix(GCFC01_QTft)
GCFC01_QT.netDensity <- network.density(GCFC01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC01_QT.entropy <- entropy(GCFC01_QTft) #entropy

GCFC01_QT.netMx <- cbind(GCFC01_QT.netMx, GCFC01_QT.clusterCoef, GCFC01_QT.degreeCent$centralization,
                         GCFC01_QT.netDensity, GCFC01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC01_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
GEEL01_Gg2 <- data.frame(GEEL01_G)
GEEL01_Gg2 <- GEEL01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_Gg2$player1
player2vector <- GEEL01_Gg2$player2
GEEL01_Gg3 <- GEEL01_Gg2
GEEL01_Gg3$p1inp2vec <- is.element(GEEL01_Gg3$player1, player2vector)
GEEL01_Gg3$p2inp1vec <- is.element(GEEL01_Gg3$player2, player1vector)

addPlayer1 <- GEEL01_Gg3[ which(GEEL01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_Gg3[ which(GEEL01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_Gg2 <- rbind(GEEL01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
GEEL01_Gft <- ftable(GEEL01_Gg2$player1, GEEL01_Gg2$player2)
GEEL01_Gft2 <- as.matrix(GEEL01_Gft)
numRows <- nrow(GEEL01_Gft2)
numCols <- ncol(GEEL01_Gft2)
GEEL01_Gft3 <- GEEL01_Gft2[c(2:numRows) , c(2:numCols)]
GEEL01_GTable <- graph.adjacency(GEEL01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(GEEL01_GTable, vertex.label = V(GEEL01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
GEEL01_G.clusterCoef <- transitivity(GEEL01_GTable, type="global") #cluster coefficient
GEEL01_G.degreeCent <- centralization.degree(GEEL01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_Gftn <- as.network.matrix(GEEL01_Gft)
GEEL01_G.netDensity <- network.density(GEEL01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_G.entropy <- entropy(GEEL01_Gft) #entropy

GEEL01_G.netMx <- cbind(GEEL01_G.netMx, GEEL01_G.clusterCoef, GEEL01_G.degreeCent$centralization,
                        GEEL01_G.netDensity, GEEL01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_G.netMx) <- varnames

#Round 1, Behind***************************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
GEEL01_Bg2 <- data.frame(GEEL01_B)
GEEL01_Bg2 <- GEEL01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_Bg2$player1
player2vector <- GEEL01_Bg2$player2
GEEL01_Bg3 <- GEEL01_Bg2
GEEL01_Bg3$p1inp2vec <- is.element(GEEL01_Bg3$player1, player2vector)
GEEL01_Bg3$p2inp1vec <- is.element(GEEL01_Bg3$player2, player1vector)

addPlayer1 <- GEEL01_Bg3[ which(GEEL01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_Bg3[ which(GEEL01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_Bg2 <- rbind(GEEL01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
GEEL01_Bft <- ftable(GEEL01_Bg2$player1, GEEL01_Bg2$player2)
GEEL01_Bft2 <- as.matrix(GEEL01_Bft)
numRows <- nrow(GEEL01_Bft2)
numCols <- ncol(GEEL01_Bft2)
GEEL01_Bft3 <- GEEL01_Bft2[c(2:numRows) , c(2:numCols)]
GEEL01_BTable <- graph.adjacency(GEEL01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(GEEL01_BTable, vertex.label = V(GEEL01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
GEEL01_B.clusterCoef <- transitivity(GEEL01_BTable, type="global") #cluster coefficient
GEEL01_B.degreeCent <- centralization.degree(GEEL01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_Bftn <- as.network.matrix(GEEL01_Bft)
GEEL01_B.netDensity <- network.density(GEEL01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_B.entropy <- entropy(GEEL01_Bft) #entropy

GEEL01_B.netMx <- cbind(GEEL01_B.netMx, GEEL01_B.clusterCoef, GEEL01_B.degreeCent$centralization,
                        GEEL01_B.netDensity, GEEL01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
GEEL01_SFg2 <- data.frame(GEEL01_SF)
GEEL01_SFg2 <- GEEL01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_SFg2$player1
player2vector <- GEEL01_SFg2$player2
GEEL01_SFg3 <- GEEL01_SFg2
GEEL01_SFg3$p1inp2vec <- is.element(GEEL01_SFg3$player1, player2vector)
GEEL01_SFg3$p2inp1vec <- is.element(GEEL01_SFg3$player2, player1vector)

addPlayer1 <- GEEL01_SFg3[ which(GEEL01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_SFg3[ which(GEEL01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_SFg2 <- rbind(GEEL01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
GEEL01_SFft <- ftable(GEEL01_SFg2$player1, GEEL01_SFg2$player2)
GEEL01_SFft2 <- as.matrix(GEEL01_SFft)
numRows <- nrow(GEEL01_SFft2)
numCols <- ncol(GEEL01_SFft2)
GEEL01_SFft3 <- GEEL01_SFft2[c(2:numRows) , c(2:numCols)]
GEEL01_SFTable <- graph.adjacency(GEEL01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(GEEL01_SFTable, vertex.label = V(GEEL01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
GEEL01_SF.clusterCoef <- transitivity(GEEL01_SFTable, type="global") #cluster coefficient
GEEL01_SF.degreeCent <- centralization.degree(GEEL01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_SFftn <- as.network.matrix(GEEL01_SFft)
GEEL01_SF.netDensity <- network.density(GEEL01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_SF.entropy <- entropy(GEEL01_SFft) #entropy

GEEL01_SF.netMx <- cbind(GEEL01_SF.netMx, GEEL01_SF.clusterCoef, GEEL01_SF.degreeCent$centralization,
                         GEEL01_SF.netDensity, GEEL01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
GEEL01_TFg2 <- data.frame(GEEL01_TF)
GEEL01_TFg2 <- GEEL01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_TFg2$player1
player2vector <- GEEL01_TFg2$player2
GEEL01_TFg3 <- GEEL01_TFg2
GEEL01_TFg3$p1inp2vec <- is.element(GEEL01_TFg3$player1, player2vector)
GEEL01_TFg3$p2inp1vec <- is.element(GEEL01_TFg3$player2, player1vector)

addPlayer1 <- GEEL01_TFg3[ which(GEEL01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_TFg3[ which(GEEL01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_TFg2 <- rbind(GEEL01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
GEEL01_TFft <- ftable(GEEL01_TFg2$player1, GEEL01_TFg2$player2)
GEEL01_TFft2 <- as.matrix(GEEL01_TFft)
numRows <- nrow(GEEL01_TFft2)
numCols <- ncol(GEEL01_TFft2)
GEEL01_TFft3 <- GEEL01_TFft2[c(2:numRows) , c(2:numCols)]
GEEL01_TFTable <- graph.adjacency(GEEL01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(GEEL01_TFTable, vertex.label = V(GEEL01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
GEEL01_TF.clusterCoef <- transitivity(GEEL01_TFTable, type="global") #cluster coefficient
GEEL01_TF.degreeCent <- centralization.degree(GEEL01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_TFftn <- as.network.matrix(GEEL01_TFft)
GEEL01_TF.netDensity <- network.density(GEEL01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_TF.entropy <- entropy(GEEL01_TFft) #entropy

GEEL01_TF.netMx <- cbind(GEEL01_TF.netMx, GEEL01_TF.clusterCoef, GEEL01_TF.degreeCent$centralization,
                         GEEL01_TF.netDensity, GEEL01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
GEEL01_SAMg2 <- data.frame(GEEL01_SAM)
GEEL01_SAMg2 <- GEEL01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_SAMg2$player1
player2vector <- GEEL01_SAMg2$player2
GEEL01_SAMg3 <- GEEL01_SAMg2
GEEL01_SAMg3$p1inp2vec <- is.element(GEEL01_SAMg3$player1, player2vector)
GEEL01_SAMg3$p2inp1vec <- is.element(GEEL01_SAMg3$player2, player1vector)

addPlayer1 <- GEEL01_SAMg3[ which(GEEL01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_SAMg3[ which(GEEL01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_SAMg2 <- rbind(GEEL01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
GEEL01_SAMft <- ftable(GEEL01_SAMg2$player1, GEEL01_SAMg2$player2)
GEEL01_SAMft2 <- as.matrix(GEEL01_SAMft)
numRows <- nrow(GEEL01_SAMft2)
numCols <- ncol(GEEL01_SAMft2)
GEEL01_SAMft3 <- GEEL01_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL01_SAMTable <- graph.adjacency(GEEL01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(GEEL01_SAMTable, vertex.label = V(GEEL01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
GEEL01_SAM.clusterCoef <- transitivity(GEEL01_SAMTable, type="global") #cluster coefficient
GEEL01_SAM.degreeCent <- centralization.degree(GEEL01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_SAMftn <- as.network.matrix(GEEL01_SAMft)
GEEL01_SAM.netDensity <- network.density(GEEL01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_SAM.entropy <- entropy(GEEL01_SAMft) #entropy

GEEL01_SAM.netMx <- cbind(GEEL01_SAM.netMx, GEEL01_SAM.clusterCoef, GEEL01_SAM.degreeCent$centralization,
                          GEEL01_SAM.netDensity, GEEL01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
GEEL01_TAMg2 <- data.frame(GEEL01_TAM)
GEEL01_TAMg2 <- GEEL01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_TAMg2$player1
player2vector <- GEEL01_TAMg2$player2
GEEL01_TAMg3 <- GEEL01_TAMg2
GEEL01_TAMg3$p1inp2vec <- is.element(GEEL01_TAMg3$player1, player2vector)
GEEL01_TAMg3$p2inp1vec <- is.element(GEEL01_TAMg3$player2, player1vector)

addPlayer1 <- GEEL01_TAMg3[ which(GEEL01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_TAMg3[ which(GEEL01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_TAMg2 <- rbind(GEEL01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
GEEL01_TAMft <- ftable(GEEL01_TAMg2$player1, GEEL01_TAMg2$player2)
GEEL01_TAMft2 <- as.matrix(GEEL01_TAMft)
numRows <- nrow(GEEL01_TAMft2)
numCols <- ncol(GEEL01_TAMft2)
GEEL01_TAMft3 <- GEEL01_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL01_TAMTable <- graph.adjacency(GEEL01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(GEEL01_TAMTable, vertex.label = V(GEEL01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
GEEL01_TAM.clusterCoef <- transitivity(GEEL01_TAMTable, type="global") #cluster coefficient
GEEL01_TAM.degreeCent <- centralization.degree(GEEL01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_TAMftn <- as.network.matrix(GEEL01_TAMft)
GEEL01_TAM.netDensity <- network.density(GEEL01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_TAM.entropy <- entropy(GEEL01_TAMft) #entropy

GEEL01_TAM.netMx <- cbind(GEEL01_TAM.netMx, GEEL01_TAM.clusterCoef, GEEL01_TAM.degreeCent$centralization,
                          GEEL01_TAM.netDensity, GEEL01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
GEEL01_SDMg2 <- data.frame(GEEL01_SDM)
GEEL01_SDMg2 <- GEEL01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_SDMg2$player1
player2vector <- GEEL01_SDMg2$player2
GEEL01_SDMg3 <- GEEL01_SDMg2
GEEL01_SDMg3$p1inp2vec <- is.element(GEEL01_SDMg3$player1, player2vector)
GEEL01_SDMg3$p2inp1vec <- is.element(GEEL01_SDMg3$player2, player1vector)

addPlayer1 <- GEEL01_SDMg3[ which(GEEL01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_SDMg3[ which(GEEL01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_SDMg2 <- rbind(GEEL01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
GEEL01_SDMft <- ftable(GEEL01_SDMg2$player1, GEEL01_SDMg2$player2)
GEEL01_SDMft2 <- as.matrix(GEEL01_SDMft)
numRows <- nrow(GEEL01_SDMft2)
numCols <- ncol(GEEL01_SDMft2)
GEEL01_SDMft3 <- GEEL01_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL01_SDMTable <- graph.adjacency(GEEL01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(GEEL01_SDMTable, vertex.label = V(GEEL01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
GEEL01_SDM.clusterCoef <- transitivity(GEEL01_SDMTable, type="global") #cluster coefficient
GEEL01_SDM.degreeCent <- centralization.degree(GEEL01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_SDMftn <- as.network.matrix(GEEL01_SDMft)
GEEL01_SDM.netDensity <- network.density(GEEL01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_SDM.entropy <- entropy(GEEL01_SDMft) #entropy

GEEL01_SDM.netMx <- cbind(GEEL01_SDM.netMx, GEEL01_SDM.clusterCoef, GEEL01_SDM.degreeCent$centralization,
                          GEEL01_SDM.netDensity, GEEL01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
GEEL01_TDMg2 <- data.frame(GEEL01_TDM)
GEEL01_TDMg2 <- GEEL01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_TDMg2$player1
player2vector <- GEEL01_TDMg2$player2
GEEL01_TDMg3 <- GEEL01_TDMg2
GEEL01_TDMg3$p1inp2vec <- is.element(GEEL01_TDMg3$player1, player2vector)
GEEL01_TDMg3$p2inp1vec <- is.element(GEEL01_TDMg3$player2, player1vector)

addPlayer1 <- GEEL01_TDMg3[ which(GEEL01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_TDMg3[ which(GEEL01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_TDMg2 <- rbind(GEEL01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
GEEL01_TDMft <- ftable(GEEL01_TDMg2$player1, GEEL01_TDMg2$player2)
GEEL01_TDMft2 <- as.matrix(GEEL01_TDMft)
numRows <- nrow(GEEL01_TDMft2)
numCols <- ncol(GEEL01_TDMft2)
GEEL01_TDMft3 <- GEEL01_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL01_TDMTable <- graph.adjacency(GEEL01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(GEEL01_TDMTable, vertex.label = V(GEEL01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
GEEL01_TDM.clusterCoef <- transitivity(GEEL01_TDMTable, type="global") #cluster coefficient
GEEL01_TDM.degreeCent <- centralization.degree(GEEL01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_TDMftn <- as.network.matrix(GEEL01_TDMft)
GEEL01_TDM.netDensity <- network.density(GEEL01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_TDM.entropy <- entropy(GEEL01_TDMft) #entropy

GEEL01_TDM.netMx <- cbind(GEEL01_TDM.netMx, GEEL01_TDM.clusterCoef, GEEL01_TDM.degreeCent$centralization,
                          GEEL01_TDM.netDensity, GEEL01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
GEEL01_SDg2 <- data.frame(GEEL01_SD)
GEEL01_SDg2 <- GEEL01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_SDg2$player1
player2vector <- GEEL01_SDg2$player2
GEEL01_SDg3 <- GEEL01_SDg2
GEEL01_SDg3$p1inp2vec <- is.element(GEEL01_SDg3$player1, player2vector)
GEEL01_SDg3$p2inp1vec <- is.element(GEEL01_SDg3$player2, player1vector)

addPlayer1 <- GEEL01_SDg3[ which(GEEL01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_SDg3[ which(GEEL01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_SDg2 <- rbind(GEEL01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
GEEL01_SDft <- ftable(GEEL01_SDg2$player1, GEEL01_SDg2$player2)
GEEL01_SDft2 <- as.matrix(GEEL01_SDft)
numRows <- nrow(GEEL01_SDft2)
numCols <- ncol(GEEL01_SDft2)
GEEL01_SDft3 <- GEEL01_SDft2[c(2:numRows) , c(2:numCols)]
GEEL01_SDTable <- graph.adjacency(GEEL01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(GEEL01_SDTable, vertex.label = V(GEEL01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
GEEL01_SD.clusterCoef <- transitivity(GEEL01_SDTable, type="global") #cluster coefficient
GEEL01_SD.degreeCent <- centralization.degree(GEEL01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_SDftn <- as.network.matrix(GEEL01_SDft)
GEEL01_SD.netDensity <- network.density(GEEL01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_SD.entropy <- entropy(GEEL01_SDft) #entropy

GEEL01_SD.netMx <- cbind(GEEL01_SD.netMx, GEEL01_SD.clusterCoef, GEEL01_SD.degreeCent$centralization,
                         GEEL01_SD.netDensity, GEEL01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************

round = 1
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
GEEL01_TDg2 <- data.frame(GEEL01_TD)
GEEL01_TDg2 <- GEEL01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_TDg2$player1
player2vector <- GEEL01_TDg2$player2
GEEL01_TDg3 <- GEEL01_TDg2
GEEL01_TDg3$p1inp2vec <- is.element(GEEL01_TDg3$player1, player2vector)
GEEL01_TDg3$p2inp1vec <- is.element(GEEL01_TDg3$player2, player1vector)

addPlayer1 <- GEEL01_TDg3[ which(GEEL01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_TDg3[ which(GEEL01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_TDg2 <- rbind(GEEL01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
GEEL01_TDft <- ftable(GEEL01_TDg2$player1, GEEL01_TDg2$player2)
GEEL01_TDft2 <- as.matrix(GEEL01_TDft)
numRows <- nrow(GEEL01_TDft2)
numCols <- ncol(GEEL01_TDft2)
GEEL01_TDft3 <- GEEL01_TDft2[c(2:numRows) , c(2:numCols)]
GEEL01_TDTable <- graph.adjacency(GEEL01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(GEEL01_TDTable, vertex.label = V(GEEL01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
GEEL01_TD.clusterCoef <- transitivity(GEEL01_TDTable, type="global") #cluster coefficient
GEEL01_TD.degreeCent <- centralization.degree(GEEL01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_TDftn <- as.network.matrix(GEEL01_TDft)
GEEL01_TD.netDensity <- network.density(GEEL01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_TD.entropy <- entropy(GEEL01_TDft) #entropy

GEEL01_TD.netMx <- cbind(GEEL01_TD.netMx, GEEL01_TD.clusterCoef, GEEL01_TD.degreeCent$centralization,
                         GEEL01_TD.netDensity, GEEL01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
GEEL01_QTg2 <- data.frame(GEEL01_QT)
GEEL01_QTg2 <- GEEL01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL01_QTg2$player1
player2vector <- GEEL01_QTg2$player2
GEEL01_QTg3 <- GEEL01_QTg2
GEEL01_QTg3$p1inp2vec <- is.element(GEEL01_QTg3$player1, player2vector)
GEEL01_QTg3$p2inp1vec <- is.element(GEEL01_QTg3$player2, player1vector)

addPlayer1 <- GEEL01_QTg3[ which(GEEL01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL01_QTg3[ which(GEEL01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL01_QTg2 <- rbind(GEEL01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
GEEL01_QTft <- ftable(GEEL01_QTg2$player1, GEEL01_QTg2$player2)
GEEL01_QTft2 <- as.matrix(GEEL01_QTft)
numRows <- nrow(GEEL01_QTft2)
numCols <- ncol(GEEL01_QTft2)
GEEL01_QTft3 <- GEEL01_QTft2[c(2:numRows) , c(2:numCols)]
GEEL01_QTTable <- graph.adjacency(GEEL01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(GEEL01_QTTable, vertex.label = V(GEEL01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
GEEL01_QT.clusterCoef <- transitivity(GEEL01_QTTable, type="global") #cluster coefficient
GEEL01_QT.degreeCent <- centralization.degree(GEEL01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL01_QTftn <- as.network.matrix(GEEL01_QTft)
GEEL01_QT.netDensity <- network.density(GEEL01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL01_QT.entropy <- entropy(GEEL01_QTft) #entropy

GEEL01_QT.netMx <- cbind(GEEL01_QT.netMx, GEEL01_QT.clusterCoef, GEEL01_QT.degreeCent$centralization,
                         GEEL01_QT.netDensity, GEEL01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL01_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "GWS"
KIoutcome = "Goal_F"
GWS01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
GWS01_Gg2 <- data.frame(GWS01_G)
GWS01_Gg2 <- GWS01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_Gg2$player1
player2vector <- GWS01_Gg2$player2
GWS01_Gg3 <- GWS01_Gg2
GWS01_Gg3$p1inp2vec <- is.element(GWS01_Gg3$player1, player2vector)
GWS01_Gg3$p2inp1vec <- is.element(GWS01_Gg3$player2, player1vector)

addPlayer1 <- GWS01_Gg3[ which(GWS01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_Gg3[ which(GWS01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_Gg2 <- rbind(GWS01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
GWS01_Gft <- ftable(GWS01_Gg2$player1, GWS01_Gg2$player2)
GWS01_Gft2 <- as.matrix(GWS01_Gft)
numRows <- nrow(GWS01_Gft2)
numCols <- ncol(GWS01_Gft2)
GWS01_Gft3 <- GWS01_Gft2[c(1:numRows) , c(1:numCols)]
GWS01_GTable <- graph.adjacency(GWS01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(GWS01_GTable, vertex.label = V(GWS01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
GWS01_G.clusterCoef <- transitivity(GWS01_GTable, type="global") #cluster coefficient
GWS01_G.degreeCent <- centralization.degree(GWS01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_Gftn <- as.network.matrix(GWS01_Gft)
GWS01_G.netDensity <- network.density(GWS01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_G.entropy <- entropy(GWS01_Gft) #entropy

GWS01_G.netMx <- cbind(GWS01_G.netMx, GWS01_G.clusterCoef, GWS01_G.degreeCent$centralization,
                       GWS01_G.netDensity, GWS01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_G.netMx) <- varnames

#Round 1, Behind***************************************************************

round = 1
teamName = "GWS"
KIoutcome = "Behind_F"
GWS01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
GWS01_Bg2 <- data.frame(GWS01_B)
GWS01_Bg2 <- GWS01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_Bg2$player1
player2vector <- GWS01_Bg2$player2
GWS01_Bg3 <- GWS01_Bg2
GWS01_Bg3$p1inp2vec <- is.element(GWS01_Bg3$player1, player2vector)
GWS01_Bg3$p2inp1vec <- is.element(GWS01_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS01_Bg3[ which(GWS01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS01_Bg2 <- rbind(GWS01_Bg2, addPlayer2)

#Round 1, Behind graph using weighted edges
GWS01_Bft <- ftable(GWS01_Bg2$player1, GWS01_Bg2$player2)
GWS01_Bft2 <- as.matrix(GWS01_Bft)
numRows <- nrow(GWS01_Bft2)
numCols <- ncol(GWS01_Bft2)
GWS01_Bft3 <- GWS01_Bft2[c(1:numRows) , c(2:numCols)]
GWS01_BTable <- graph.adjacency(GWS01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(GWS01_BTable, vertex.label = V(GWS01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
GWS01_B.clusterCoef <- transitivity(GWS01_BTable, type="global") #cluster coefficient
GWS01_B.degreeCent <- centralization.degree(GWS01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_Bftn <- as.network.matrix(GWS01_Bft)
GWS01_B.netDensity <- network.density(GWS01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_B.entropy <- entropy(GWS01_Bft) #entropy

GWS01_B.netMx <- cbind(GWS01_B.netMx, GWS01_B.clusterCoef, GWS01_B.degreeCent$centralization,
                       GWS01_B.netDensity, GWS01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
GWS01_SFg2 <- data.frame(GWS01_SF)
GWS01_SFg2 <- GWS01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_SFg2$player1
player2vector <- GWS01_SFg2$player2
GWS01_SFg3 <- GWS01_SFg2
GWS01_SFg3$p1inp2vec <- is.element(GWS01_SFg3$player1, player2vector)
GWS01_SFg3$p2inp1vec <- is.element(GWS01_SFg3$player2, player1vector)

addPlayer1 <- GWS01_SFg3[ which(GWS01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_SFg3[ which(GWS01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_SFg2 <- rbind(GWS01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
GWS01_SFft <- ftable(GWS01_SFg2$player1, GWS01_SFg2$player2)
GWS01_SFft2 <- as.matrix(GWS01_SFft)
numRows <- nrow(GWS01_SFft2)
numCols <- ncol(GWS01_SFft2)
GWS01_SFft3 <- GWS01_SFft2[c(2:numRows) , c(2:numCols)]
GWS01_SFTable <- graph.adjacency(GWS01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(GWS01_SFTable, vertex.label = V(GWS01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
GWS01_SF.clusterCoef <- transitivity(GWS01_SFTable, type="global") #cluster coefficient
GWS01_SF.degreeCent <- centralization.degree(GWS01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_SFftn <- as.network.matrix(GWS01_SFft)
GWS01_SF.netDensity <- network.density(GWS01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_SF.entropy <- entropy(GWS01_SFft) #entropy

GWS01_SF.netMx <- cbind(GWS01_SF.netMx, GWS01_SF.clusterCoef, GWS01_SF.degreeCent$centralization,
                        GWS01_SF.netDensity, GWS01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
GWS01_TFg2 <- data.frame(GWS01_TF)
GWS01_TFg2 <- GWS01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_TFg2$player1
player2vector <- GWS01_TFg2$player2
GWS01_TFg3 <- GWS01_TFg2
GWS01_TFg3$p1inp2vec <- is.element(GWS01_TFg3$player1, player2vector)
GWS01_TFg3$p2inp1vec <- is.element(GWS01_TFg3$player2, player1vector)

addPlayer1 <- GWS01_TFg3[ which(GWS01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_TFg3[ which(GWS01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_TFg2 <- rbind(GWS01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
GWS01_TFft <- ftable(GWS01_TFg2$player1, GWS01_TFg2$player2)
GWS01_TFft2 <- as.matrix(GWS01_TFft)
numRows <- nrow(GWS01_TFft2)
numCols <- ncol(GWS01_TFft2)
GWS01_TFft3 <- GWS01_TFft2[c(2:numRows) , c(2:numCols)]
GWS01_TFTable <- graph.adjacency(GWS01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(GWS01_TFTable, vertex.label = V(GWS01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
GWS01_TF.clusterCoef <- transitivity(GWS01_TFTable, type="global") #cluster coefficient
GWS01_TF.degreeCent <- centralization.degree(GWS01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_TFftn <- as.network.matrix(GWS01_TFft)
GWS01_TF.netDensity <- network.density(GWS01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_TF.entropy <- entropy(GWS01_TFft) #entropy

GWS01_TF.netMx <- cbind(GWS01_TF.netMx, GWS01_TF.clusterCoef, GWS01_TF.degreeCent$centralization,
                        GWS01_TF.netDensity, GWS01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
GWS01_SAMg2 <- data.frame(GWS01_SAM)
GWS01_SAMg2 <- GWS01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_SAMg2$player1
player2vector <- GWS01_SAMg2$player2
GWS01_SAMg3 <- GWS01_SAMg2
GWS01_SAMg3$p1inp2vec <- is.element(GWS01_SAMg3$player1, player2vector)
GWS01_SAMg3$p2inp1vec <- is.element(GWS01_SAMg3$player2, player1vector)

addPlayer1 <- GWS01_SAMg3[ which(GWS01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_SAMg3[ which(GWS01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_SAMg2 <- rbind(GWS01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
GWS01_SAMft <- ftable(GWS01_SAMg2$player1, GWS01_SAMg2$player2)
GWS01_SAMft2 <- as.matrix(GWS01_SAMft)
numRows <- nrow(GWS01_SAMft2)
numCols <- ncol(GWS01_SAMft2)
GWS01_SAMft3 <- GWS01_SAMft2[c(2:numRows) , c(2:numCols)]
GWS01_SAMTable <- graph.adjacency(GWS01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(GWS01_SAMTable, vertex.label = V(GWS01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
GWS01_SAM.clusterCoef <- transitivity(GWS01_SAMTable, type="global") #cluster coefficient
GWS01_SAM.degreeCent <- centralization.degree(GWS01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_SAMftn <- as.network.matrix(GWS01_SAMft)
GWS01_SAM.netDensity <- network.density(GWS01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_SAM.entropy <- entropy(GWS01_SAMft) #entropy

GWS01_SAM.netMx <- cbind(GWS01_SAM.netMx, GWS01_SAM.clusterCoef, GWS01_SAM.degreeCent$centralization,
                         GWS01_SAM.netDensity, GWS01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************
#NA

round = 1
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
GWS01_TAMg2 <- data.frame(GWS01_TAM)
GWS01_TAMg2 <- GWS01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_TAMg2$player1
player2vector <- GWS01_TAMg2$player2
GWS01_TAMg3 <- GWS01_TAMg2
GWS01_TAMg3$p1inp2vec <- is.element(GWS01_TAMg3$player1, player2vector)
GWS01_TAMg3$p2inp1vec <- is.element(GWS01_TAMg3$player2, player1vector)

addPlayer1 <- GWS01_TAMg3[ which(GWS01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_TAMg3[ which(GWS01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_TAMg2 <- rbind(GWS01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
GWS01_TAMft <- ftable(GWS01_TAMg2$player1, GWS01_TAMg2$player2)
GWS01_TAMft2 <- as.matrix(GWS01_TAMft)
numRows <- nrow(GWS01_TAMft2)
numCols <- ncol(GWS01_TAMft2)
GWS01_TAMft3 <- GWS01_TAMft2[c(2:numRows) , c(2:numCols)]
GWS01_TAMTable <- graph.adjacency(GWS01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(GWS01_TAMTable, vertex.label = V(GWS01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
GWS01_TAM.clusterCoef <- transitivity(GWS01_TAMTable, type="global") #cluster coefficient
GWS01_TAM.degreeCent <- centralization.degree(GWS01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_TAMftn <- as.network.matrix(GWS01_TAMft)
GWS01_TAM.netDensity <- network.density(GWS01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_TAM.entropy <- entropy(GWS01_TAMft) #entropy

GWS01_TAM.netMx <- cbind(GWS01_TAM.netMx, GWS01_TAM.clusterCoef, GWS01_TAM.degreeCent$centralization,
                         GWS01_TAM.netDensity, GWS01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
GWS01_SDMg2 <- data.frame(GWS01_SDM)
GWS01_SDMg2 <- GWS01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_SDMg2$player1
player2vector <- GWS01_SDMg2$player2
GWS01_SDMg3 <- GWS01_SDMg2
GWS01_SDMg3$p1inp2vec <- is.element(GWS01_SDMg3$player1, player2vector)
GWS01_SDMg3$p2inp1vec <- is.element(GWS01_SDMg3$player2, player1vector)

addPlayer1 <- GWS01_SDMg3[ which(GWS01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_SDMg3[ which(GWS01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_SDMg2 <- rbind(GWS01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
GWS01_SDMft <- ftable(GWS01_SDMg2$player1, GWS01_SDMg2$player2)
GWS01_SDMft2 <- as.matrix(GWS01_SDMft)
numRows <- nrow(GWS01_SDMft2)
numCols <- ncol(GWS01_SDMft2)
GWS01_SDMft3 <- GWS01_SDMft2[c(2:numRows) , c(2:numCols)]
GWS01_SDMTable <- graph.adjacency(GWS01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(GWS01_SDMTable, vertex.label = V(GWS01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
GWS01_SDM.clusterCoef <- transitivity(GWS01_SDMTable, type="global") #cluster coefficient
GWS01_SDM.degreeCent <- centralization.degree(GWS01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_SDMftn <- as.network.matrix(GWS01_SDMft)
GWS01_SDM.netDensity <- network.density(GWS01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_SDM.entropy <- entropy(GWS01_SDMft) #entropy

GWS01_SDM.netMx <- cbind(GWS01_SDM.netMx, GWS01_SDM.clusterCoef, GWS01_SDM.degreeCent$centralization,
                         GWS01_SDM.netDensity, GWS01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
GWS01_TDMg2 <- data.frame(GWS01_TDM)
GWS01_TDMg2 <- GWS01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_TDMg2$player1
player2vector <- GWS01_TDMg2$player2
GWS01_TDMg3 <- GWS01_TDMg2
GWS01_TDMg3$p1inp2vec <- is.element(GWS01_TDMg3$player1, player2vector)
GWS01_TDMg3$p2inp1vec <- is.element(GWS01_TDMg3$player2, player1vector)

addPlayer1 <- GWS01_TDMg3[ which(GWS01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_TDMg3[ which(GWS01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_TDMg2 <- rbind(GWS01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
GWS01_TDMft <- ftable(GWS01_TDMg2$player1, GWS01_TDMg2$player2)
GWS01_TDMft2 <- as.matrix(GWS01_TDMft)
numRows <- nrow(GWS01_TDMft2)
numCols <- ncol(GWS01_TDMft2)
GWS01_TDMft3 <- GWS01_TDMft2[c(2:numRows) , c(2:numCols)]
GWS01_TDMTable <- graph.adjacency(GWS01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(GWS01_TDMTable, vertex.label = V(GWS01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
GWS01_TDM.clusterCoef <- transitivity(GWS01_TDMTable, type="global") #cluster coefficient
GWS01_TDM.degreeCent <- centralization.degree(GWS01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_TDMftn <- as.network.matrix(GWS01_TDMft)
GWS01_TDM.netDensity <- network.density(GWS01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_TDM.entropy <- entropy(GWS01_TDMft) #entropy

GWS01_TDM.netMx <- cbind(GWS01_TDM.netMx, GWS01_TDM.clusterCoef, GWS01_TDM.degreeCent$centralization,
                         GWS01_TDM.netDensity, GWS01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
GWS01_SDg2 <- data.frame(GWS01_SD)
GWS01_SDg2 <- GWS01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_SDg2$player1
player2vector <- GWS01_SDg2$player2
GWS01_SDg3 <- GWS01_SDg2
GWS01_SDg3$p1inp2vec <- is.element(GWS01_SDg3$player1, player2vector)
GWS01_SDg3$p2inp1vec <- is.element(GWS01_SDg3$player2, player1vector)

addPlayer1 <- GWS01_SDg3[ which(GWS01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_SDg3[ which(GWS01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_SDg2 <- rbind(GWS01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
GWS01_SDft <- ftable(GWS01_SDg2$player1, GWS01_SDg2$player2)
GWS01_SDft2 <- as.matrix(GWS01_SDft)
numRows <- nrow(GWS01_SDft2)
numCols <- ncol(GWS01_SDft2)
GWS01_SDft3 <- GWS01_SDft2[c(2:numRows) , c(2:numCols)]
GWS01_SDTable <- graph.adjacency(GWS01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(GWS01_SDTable, vertex.label = V(GWS01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
GWS01_SD.clusterCoef <- transitivity(GWS01_SDTable, type="global") #cluster coefficient
GWS01_SD.degreeCent <- centralization.degree(GWS01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_SDftn <- as.network.matrix(GWS01_SDft)
GWS01_SD.netDensity <- network.density(GWS01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_SD.entropy <- entropy(GWS01_SDft) #entropy

GWS01_SD.netMx <- cbind(GWS01_SD.netMx, GWS01_SD.clusterCoef, GWS01_SD.degreeCent$centralization,
                        GWS01_SD.netDensity, GWS01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
GWS01_TDg2 <- data.frame(GWS01_TD)
GWS01_TDg2 <- GWS01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_TDg2$player1
player2vector <- GWS01_TDg2$player2
GWS01_TDg3 <- GWS01_TDg2
GWS01_TDg3$p1inp2vec <- is.element(GWS01_TDg3$player1, player2vector)
GWS01_TDg3$p2inp1vec <- is.element(GWS01_TDg3$player2, player1vector)

addPlayer1 <- GWS01_TDg3[ which(GWS01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_TDg3[ which(GWS01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_TDg2 <- rbind(GWS01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
GWS01_TDft <- ftable(GWS01_TDg2$player1, GWS01_TDg2$player2)
GWS01_TDft2 <- as.matrix(GWS01_TDft)
numRows <- nrow(GWS01_TDft2)
numCols <- ncol(GWS01_TDft2)
GWS01_TDft3 <- GWS01_TDft2[c(2:numRows) , c(2:numCols)]
GWS01_TDTable <- graph.adjacency(GWS01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(GWS01_TDTable, vertex.label = V(GWS01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
GWS01_TD.clusterCoef <- transitivity(GWS01_TDTable, type="global") #cluster coefficient
GWS01_TD.degreeCent <- centralization.degree(GWS01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_TDftn <- as.network.matrix(GWS01_TDft)
GWS01_TD.netDensity <- network.density(GWS01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_TD.entropy <- entropy(GWS01_TDft) #entropy

GWS01_TD.netMx <- cbind(GWS01_TD.netMx, GWS01_TD.clusterCoef, GWS01_TD.degreeCent$centralization,
                        GWS01_TD.netDensity, GWS01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
GWS01_QTg2 <- data.frame(GWS01_QT)
GWS01_QTg2 <- GWS01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS01_QTg2$player1
player2vector <- GWS01_QTg2$player2
GWS01_QTg3 <- GWS01_QTg2
GWS01_QTg3$p1inp2vec <- is.element(GWS01_QTg3$player1, player2vector)
GWS01_QTg3$p2inp1vec <- is.element(GWS01_QTg3$player2, player1vector)

addPlayer1 <- GWS01_QTg3[ which(GWS01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS01_QTg3[ which(GWS01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS01_QTg2 <- rbind(GWS01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
GWS01_QTft <- ftable(GWS01_QTg2$player1, GWS01_QTg2$player2)
GWS01_QTft2 <- as.matrix(GWS01_QTft)
numRows <- nrow(GWS01_QTft2)
numCols <- ncol(GWS01_QTft2)
GWS01_QTft3 <- GWS01_QTft2[c(2:numRows) , c(2:numCols)]
GWS01_QTTable <- graph.adjacency(GWS01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(GWS01_QTTable, vertex.label = V(GWS01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
GWS01_QT.clusterCoef <- transitivity(GWS01_QTTable, type="global") #cluster coefficient
GWS01_QT.degreeCent <- centralization.degree(GWS01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS01_QTftn <- as.network.matrix(GWS01_QTft)
GWS01_QT.netDensity <- network.density(GWS01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS01_QT.entropy <- entropy(GWS01_QTft) #entropy

GWS01_QT.netMx <- cbind(GWS01_QT.netMx, GWS01_QT.clusterCoef, GWS01_QT.degreeCent$centralization,
                        GWS01_QT.netDensity, GWS01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS01_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "HAW"
KIoutcome = "Goal_F"
HAW01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
HAW01_Gg2 <- data.frame(HAW01_G)
HAW01_Gg2 <- HAW01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_Gg2$player1
player2vector <- HAW01_Gg2$player2
HAW01_Gg3 <- HAW01_Gg2
HAW01_Gg3$p1inp2vec <- is.element(HAW01_Gg3$player1, player2vector)
HAW01_Gg3$p2inp1vec <- is.element(HAW01_Gg3$player2, player1vector)

addPlayer1 <- HAW01_Gg3[ which(HAW01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_Gg3[ which(HAW01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_Gg2 <- rbind(HAW01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
HAW01_Gft <- ftable(HAW01_Gg2$player1, HAW01_Gg2$player2)
HAW01_Gft2 <- as.matrix(HAW01_Gft)
numRows <- nrow(HAW01_Gft2)
numCols <- ncol(HAW01_Gft2)
HAW01_Gft3 <- HAW01_Gft2[c(2:numRows) , c(2:numCols)]
HAW01_GTable <- graph.adjacency(HAW01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(HAW01_GTable, vertex.label = V(HAW01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
HAW01_G.clusterCoef <- transitivity(HAW01_GTable, type="global") #cluster coefficient
HAW01_G.degreeCent <- centralization.degree(HAW01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_Gftn <- as.network.matrix(HAW01_Gft)
HAW01_G.netDensity <- network.density(HAW01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_G.entropy <- entropy(HAW01_Gft) #entropy

HAW01_G.netMx <- cbind(HAW01_G.netMx, HAW01_G.clusterCoef, HAW01_G.degreeCent$centralization,
                       HAW01_G.netDensity, HAW01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_G.netMx) <- varnames

#Round 1, Behind***************************************************************

round = 1
teamName = "HAW"
KIoutcome = "Behind_F"
HAW01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
HAW01_Bg2 <- data.frame(HAW01_B)
HAW01_Bg2 <- HAW01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_Bg2$player1
player2vector <- HAW01_Bg2$player2
HAW01_Bg3 <- HAW01_Bg2
HAW01_Bg3$p1inp2vec <- is.element(HAW01_Bg3$player1, player2vector)
HAW01_Bg3$p2inp1vec <- is.element(HAW01_Bg3$player2, player1vector)

addPlayer1 <- HAW01_Bg3[ which(HAW01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW01_Bg2 <- rbind(HAW01_Bg2, addPlayer1)

#Round 1, Behind graph using weighted edges
HAW01_Bft <- ftable(HAW01_Bg2$player1, HAW01_Bg2$player2)
HAW01_Bft2 <- as.matrix(HAW01_Bft)
numRows <- nrow(HAW01_Bft2)
numCols <- ncol(HAW01_Bft2)
HAW01_Bft3 <- HAW01_Bft2[c(2:numRows) , c(1:numCols)]
HAW01_BTable <- graph.adjacency(HAW01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(HAW01_BTable, vertex.label = V(HAW01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
HAW01_B.clusterCoef <- transitivity(HAW01_BTable, type="global") #cluster coefficient
HAW01_B.degreeCent <- centralization.degree(HAW01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_Bftn <- as.network.matrix(HAW01_Bft)
HAW01_B.netDensity <- network.density(HAW01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_B.entropy <- entropy(HAW01_Bft) #entropy

HAW01_B.netMx <- cbind(HAW01_B.netMx, HAW01_B.clusterCoef, HAW01_B.degreeCent$centralization,
                       HAW01_B.netDensity, HAW01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
HAW01_SFg2 <- data.frame(HAW01_SF)
HAW01_SFg2 <- HAW01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_SFg2$player1
player2vector <- HAW01_SFg2$player2
HAW01_SFg3 <- HAW01_SFg2
HAW01_SFg3$p1inp2vec <- is.element(HAW01_SFg3$player1, player2vector)
HAW01_SFg3$p2inp1vec <- is.element(HAW01_SFg3$player2, player1vector)

addPlayer1 <- HAW01_SFg3[ which(HAW01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_SFg3[ which(HAW01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_SFg2 <- rbind(HAW01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
HAW01_SFft <- ftable(HAW01_SFg2$player1, HAW01_SFg2$player2)
HAW01_SFft2 <- as.matrix(HAW01_SFft)
numRows <- nrow(HAW01_SFft2)
numCols <- ncol(HAW01_SFft2)
HAW01_SFft3 <- HAW01_SFft2[c(2:numRows) , c(2:numCols)]
HAW01_SFTable <- graph.adjacency(HAW01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(HAW01_SFTable, vertex.label = V(HAW01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
HAW01_SF.clusterCoef <- transitivity(HAW01_SFTable, type="global") #cluster coefficient
HAW01_SF.degreeCent <- centralization.degree(HAW01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_SFftn <- as.network.matrix(HAW01_SFft)
HAW01_SF.netDensity <- network.density(HAW01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_SF.entropy <- entropy(HAW01_SFft) #entropy

HAW01_SF.netMx <- cbind(HAW01_SF.netMx, HAW01_SF.clusterCoef, HAW01_SF.degreeCent$centralization,
                        HAW01_SF.netDensity, HAW01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
HAW01_TFg2 <- data.frame(HAW01_TF)
HAW01_TFg2 <- HAW01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_TFg2$player1
player2vector <- HAW01_TFg2$player2
HAW01_TFg3 <- HAW01_TFg2
HAW01_TFg3$p1inp2vec <- is.element(HAW01_TFg3$player1, player2vector)
HAW01_TFg3$p2inp1vec <- is.element(HAW01_TFg3$player2, player1vector)

addPlayer1 <- HAW01_TFg3[ which(HAW01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_TFg3[ which(HAW01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_TFg2 <- rbind(HAW01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
HAW01_TFft <- ftable(HAW01_TFg2$player1, HAW01_TFg2$player2)
HAW01_TFft2 <- as.matrix(HAW01_TFft)
numRows <- nrow(HAW01_TFft2)
numCols <- ncol(HAW01_TFft2)
HAW01_TFft3 <- HAW01_TFft2[c(2:numRows) , c(2:numCols)]
HAW01_TFTable <- graph.adjacency(HAW01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(HAW01_TFTable, vertex.label = V(HAW01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
HAW01_TF.clusterCoef <- transitivity(HAW01_TFTable, type="global") #cluster coefficient
HAW01_TF.degreeCent <- centralization.degree(HAW01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_TFftn <- as.network.matrix(HAW01_TFft)
HAW01_TF.netDensity <- network.density(HAW01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_TF.entropy <- entropy(HAW01_TFft) #entropy

HAW01_TF.netMx <- cbind(HAW01_TF.netMx, HAW01_TF.clusterCoef, HAW01_TF.degreeCent$centralization,
                        HAW01_TF.netDensity, HAW01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
HAW01_SAMg2 <- data.frame(HAW01_SAM)
HAW01_SAMg2 <- HAW01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_SAMg2$player1
player2vector <- HAW01_SAMg2$player2
HAW01_SAMg3 <- HAW01_SAMg2
HAW01_SAMg3$p1inp2vec <- is.element(HAW01_SAMg3$player1, player2vector)
HAW01_SAMg3$p2inp1vec <- is.element(HAW01_SAMg3$player2, player1vector)

addPlayer1 <- HAW01_SAMg3[ which(HAW01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_SAMg3[ which(HAW01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_SAMg2 <- rbind(HAW01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
HAW01_SAMft <- ftable(HAW01_SAMg2$player1, HAW01_SAMg2$player2)
HAW01_SAMft2 <- as.matrix(HAW01_SAMft)
numRows <- nrow(HAW01_SAMft2)
numCols <- ncol(HAW01_SAMft2)
HAW01_SAMft3 <- HAW01_SAMft2[c(2:numRows) , c(2:numCols)]
HAW01_SAMTable <- graph.adjacency(HAW01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(HAW01_SAMTable, vertex.label = V(HAW01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
HAW01_SAM.clusterCoef <- transitivity(HAW01_SAMTable, type="global") #cluster coefficient
HAW01_SAM.degreeCent <- centralization.degree(HAW01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_SAMftn <- as.network.matrix(HAW01_SAMft)
HAW01_SAM.netDensity <- network.density(HAW01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_SAM.entropy <- entropy(HAW01_SAMft) #entropy

HAW01_SAM.netMx <- cbind(HAW01_SAM.netMx, HAW01_SAM.clusterCoef, HAW01_SAM.degreeCent$centralization,
                         HAW01_SAM.netDensity, HAW01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
HAW01_TAMg2 <- data.frame(HAW01_TAM)
HAW01_TAMg2 <- HAW01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_TAMg2$player1
player2vector <- HAW01_TAMg2$player2
HAW01_TAMg3 <- HAW01_TAMg2
HAW01_TAMg3$p1inp2vec <- is.element(HAW01_TAMg3$player1, player2vector)
HAW01_TAMg3$p2inp1vec <- is.element(HAW01_TAMg3$player2, player1vector)

addPlayer1 <- HAW01_TAMg3[ which(HAW01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_TAMg3[ which(HAW01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_TAMg2 <- rbind(HAW01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
HAW01_TAMft <- ftable(HAW01_TAMg2$player1, HAW01_TAMg2$player2)
HAW01_TAMft2 <- as.matrix(HAW01_TAMft)
numRows <- nrow(HAW01_TAMft2)
numCols <- ncol(HAW01_TAMft2)
HAW01_TAMft3 <- HAW01_TAMft2[c(2:numRows) , c(2:numCols)]
HAW01_TAMTable <- graph.adjacency(HAW01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(HAW01_TAMTable, vertex.label = V(HAW01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
HAW01_TAM.clusterCoef <- transitivity(HAW01_TAMTable, type="global") #cluster coefficient
HAW01_TAM.degreeCent <- centralization.degree(HAW01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_TAMftn <- as.network.matrix(HAW01_TAMft)
HAW01_TAM.netDensity <- network.density(HAW01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_TAM.entropy <- entropy(HAW01_TAMft) #entropy

HAW01_TAM.netMx <- cbind(HAW01_TAM.netMx, HAW01_TAM.clusterCoef, HAW01_TAM.degreeCent$centralization,
                         HAW01_TAM.netDensity, HAW01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
HAW01_SDMg2 <- data.frame(HAW01_SDM)
HAW01_SDMg2 <- HAW01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_SDMg2$player1
player2vector <- HAW01_SDMg2$player2
HAW01_SDMg3 <- HAW01_SDMg2
HAW01_SDMg3$p1inp2vec <- is.element(HAW01_SDMg3$player1, player2vector)
HAW01_SDMg3$p2inp1vec <- is.element(HAW01_SDMg3$player2, player1vector)

addPlayer1 <- HAW01_SDMg3[ which(HAW01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_SDMg3[ which(HAW01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_SDMg2 <- rbind(HAW01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
HAW01_SDMft <- ftable(HAW01_SDMg2$player1, HAW01_SDMg2$player2)
HAW01_SDMft2 <- as.matrix(HAW01_SDMft)
numRows <- nrow(HAW01_SDMft2)
numCols <- ncol(HAW01_SDMft2)
HAW01_SDMft3 <- HAW01_SDMft2[c(2:numRows) , c(2:numCols)]
HAW01_SDMTable <- graph.adjacency(HAW01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(HAW01_SDMTable, vertex.label = V(HAW01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
HAW01_SDM.clusterCoef <- transitivity(HAW01_SDMTable, type="global") #cluster coefficient
HAW01_SDM.degreeCent <- centralization.degree(HAW01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_SDMftn <- as.network.matrix(HAW01_SDMft)
HAW01_SDM.netDensity <- network.density(HAW01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_SDM.entropy <- entropy(HAW01_SDMft) #entropy

HAW01_SDM.netMx <- cbind(HAW01_SDM.netMx, HAW01_SDM.clusterCoef, HAW01_SDM.degreeCent$centralization,
                         HAW01_SDM.netDensity, HAW01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
HAW01_TDMg2 <- data.frame(HAW01_TDM)
HAW01_TDMg2 <- HAW01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_TDMg2$player1
player2vector <- HAW01_TDMg2$player2
HAW01_TDMg3 <- HAW01_TDMg2
HAW01_TDMg3$p1inp2vec <- is.element(HAW01_TDMg3$player1, player2vector)
HAW01_TDMg3$p2inp1vec <- is.element(HAW01_TDMg3$player2, player1vector)

addPlayer1 <- HAW01_TDMg3[ which(HAW01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_TDMg3[ which(HAW01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_TDMg2 <- rbind(HAW01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
HAW01_TDMft <- ftable(HAW01_TDMg2$player1, HAW01_TDMg2$player2)
HAW01_TDMft2 <- as.matrix(HAW01_TDMft)
numRows <- nrow(HAW01_TDMft2)
numCols <- ncol(HAW01_TDMft2)
HAW01_TDMft3 <- HAW01_TDMft2[c(2:numRows) , c(2:numCols)]
HAW01_TDMTable <- graph.adjacency(HAW01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(HAW01_TDMTable, vertex.label = V(HAW01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
HAW01_TDM.clusterCoef <- transitivity(HAW01_TDMTable, type="global") #cluster coefficient
HAW01_TDM.degreeCent <- centralization.degree(HAW01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_TDMftn <- as.network.matrix(HAW01_TDMft)
HAW01_TDM.netDensity <- network.density(HAW01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_TDM.entropy <- entropy(HAW01_TDMft) #entropy

HAW01_TDM.netMx <- cbind(HAW01_TDM.netMx, HAW01_TDM.clusterCoef, HAW01_TDM.degreeCent$centralization,
                         HAW01_TDM.netDensity, HAW01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
HAW01_SDg2 <- data.frame(HAW01_SD)
HAW01_SDg2 <- HAW01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_SDg2$player1
player2vector <- HAW01_SDg2$player2
HAW01_SDg3 <- HAW01_SDg2
HAW01_SDg3$p1inp2vec <- is.element(HAW01_SDg3$player1, player2vector)
HAW01_SDg3$p2inp1vec <- is.element(HAW01_SDg3$player2, player1vector)

addPlayer1 <- HAW01_SDg3[ which(HAW01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_SDg3[ which(HAW01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_SDg2 <- rbind(HAW01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
HAW01_SDft <- ftable(HAW01_SDg2$player1, HAW01_SDg2$player2)
HAW01_SDft2 <- as.matrix(HAW01_SDft)
numRows <- nrow(HAW01_SDft2)
numCols <- ncol(HAW01_SDft2)
HAW01_SDft3 <- HAW01_SDft2[c(2:numRows) , c(2:numCols)]
HAW01_SDTable <- graph.adjacency(HAW01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(HAW01_SDTable, vertex.label = V(HAW01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
HAW01_SD.clusterCoef <- transitivity(HAW01_SDTable, type="global") #cluster coefficient
HAW01_SD.degreeCent <- centralization.degree(HAW01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_SDftn <- as.network.matrix(HAW01_SDft)
HAW01_SD.netDensity <- network.density(HAW01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_SD.entropy <- entropy(HAW01_SDft) #entropy

HAW01_SD.netMx <- cbind(HAW01_SD.netMx, HAW01_SD.clusterCoef, HAW01_SD.degreeCent$centralization,
                        HAW01_SD.netDensity, HAW01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************

round = 1
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
HAW01_TDg2 <- data.frame(HAW01_TD)
HAW01_TDg2 <- HAW01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_TDg2$player1
player2vector <- HAW01_TDg2$player2
HAW01_TDg3 <- HAW01_TDg2
HAW01_TDg3$p1inp2vec <- is.element(HAW01_TDg3$player1, player2vector)
HAW01_TDg3$p2inp1vec <- is.element(HAW01_TDg3$player2, player1vector)

addPlayer1 <- HAW01_TDg3[ which(HAW01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_TDg3[ which(HAW01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_TDg2 <- rbind(HAW01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
HAW01_TDft <- ftable(HAW01_TDg2$player1, HAW01_TDg2$player2)
HAW01_TDft2 <- as.matrix(HAW01_TDft)
numRows <- nrow(HAW01_TDft2)
numCols <- ncol(HAW01_TDft2)
HAW01_TDft3 <- HAW01_TDft2[c(2:numRows) , c(2:numCols)]
HAW01_TDTable <- graph.adjacency(HAW01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(HAW01_TDTable, vertex.label = V(HAW01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
HAW01_TD.clusterCoef <- transitivity(HAW01_TDTable, type="global") #cluster coefficient
HAW01_TD.degreeCent <- centralization.degree(HAW01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_TDftn <- as.network.matrix(HAW01_TDft)
HAW01_TD.netDensity <- network.density(HAW01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_TD.entropy <- entropy(HAW01_TDft) #entropy

HAW01_TD.netMx <- cbind(HAW01_TD.netMx, HAW01_TD.clusterCoef, HAW01_TD.degreeCent$centralization,
                        HAW01_TD.netDensity, HAW01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
HAW01_QTg2 <- data.frame(HAW01_QT)
HAW01_QTg2 <- HAW01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW01_QTg2$player1
player2vector <- HAW01_QTg2$player2
HAW01_QTg3 <- HAW01_QTg2
HAW01_QTg3$p1inp2vec <- is.element(HAW01_QTg3$player1, player2vector)
HAW01_QTg3$p2inp1vec <- is.element(HAW01_QTg3$player2, player1vector)

addPlayer1 <- HAW01_QTg3[ which(HAW01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW01_QTg3[ which(HAW01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW01_QTg2 <- rbind(HAW01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
HAW01_QTft <- ftable(HAW01_QTg2$player1, HAW01_QTg2$player2)
HAW01_QTft2 <- as.matrix(HAW01_QTft)
numRows <- nrow(HAW01_QTft2)
numCols <- ncol(HAW01_QTft2)
HAW01_QTft3 <- HAW01_QTft2[c(2:numRows) , c(2:numCols)]
HAW01_QTTable <- graph.adjacency(HAW01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(HAW01_QTTable, vertex.label = V(HAW01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
HAW01_QT.clusterCoef <- transitivity(HAW01_QTTable, type="global") #cluster coefficient
HAW01_QT.degreeCent <- centralization.degree(HAW01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW01_QTftn <- as.network.matrix(HAW01_QTft)
HAW01_QT.netDensity <- network.density(HAW01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW01_QT.entropy <- entropy(HAW01_QTft) #entropy

HAW01_QT.netMx <- cbind(HAW01_QT.netMx, HAW01_QT.clusterCoef, HAW01_QT.degreeCent$centralization,
                        HAW01_QT.netDensity, HAW01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW01_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "MELB"
KIoutcome = "Goal_F"
MELB01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
MELB01_Gg2 <- data.frame(MELB01_G)
MELB01_Gg2 <- MELB01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_Gg2$player1
player2vector <- MELB01_Gg2$player2
MELB01_Gg3 <- MELB01_Gg2
MELB01_Gg3$p1inp2vec <- is.element(MELB01_Gg3$player1, player2vector)
MELB01_Gg3$p2inp1vec <- is.element(MELB01_Gg3$player2, player1vector)

addPlayer1 <- MELB01_Gg3[ which(MELB01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB01_Gg2 <- rbind(MELB01_Gg2, addPlayer1)

#Round 1, Goal graph using weighted edges
MELB01_Gft <- ftable(MELB01_Gg2$player1, MELB01_Gg2$player2)
MELB01_Gft2 <- as.matrix(MELB01_Gft)
numRows <- nrow(MELB01_Gft2)
numCols <- ncol(MELB01_Gft2)
MELB01_Gft3 <- MELB01_Gft2[c(2:numRows) , c(1:numCols)]
MELB01_GTable <- graph.adjacency(MELB01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(MELB01_GTable, vertex.label = V(MELB01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
MELB01_G.clusterCoef <- transitivity(MELB01_GTable, type="global") #cluster coefficient
MELB01_G.degreeCent <- centralization.degree(MELB01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_Gftn <- as.network.matrix(MELB01_Gft)
MELB01_G.netDensity <- network.density(MELB01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_G.entropy <- entropy(MELB01_Gft) #entropy

MELB01_G.netMx <- cbind(MELB01_G.netMx, MELB01_G.clusterCoef, MELB01_G.degreeCent$centralization,
                        MELB01_G.netDensity, MELB01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "MELB"
KIoutcome = "Behind_F"
MELB01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
MELB01_Bg2 <- data.frame(MELB01_B)
MELB01_Bg2 <- MELB01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_Bg2$player1
player2vector <- MELB01_Bg2$player2
MELB01_Bg3 <- MELB01_Bg2
MELB01_Bg3$p1inp2vec <- is.element(MELB01_Bg3$player1, player2vector)
MELB01_Bg3$p2inp1vec <- is.element(MELB01_Bg3$player2, player1vector)

addPlayer1 <- MELB01_Bg3[ which(MELB01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_Bg3[ which(MELB01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_Bg2 <- rbind(MELB01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
MELB01_Bft <- ftable(MELB01_Bg2$player1, MELB01_Bg2$player2)
MELB01_Bft2 <- as.matrix(MELB01_Bft)
numRows <- nrow(MELB01_Bft2)
numCols <- ncol(MELB01_Bft2)
MELB01_Bft3 <- MELB01_Bft2[c(2:numRows) , c(2:numCols)]
MELB01_BTable <- graph.adjacency(MELB01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(MELB01_BTable, vertex.label = V(MELB01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
MELB01_B.clusterCoef <- transitivity(MELB01_BTable, type="global") #cluster coefficient
MELB01_B.degreeCent <- centralization.degree(MELB01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_Bftn <- as.network.matrix(MELB01_Bft)
MELB01_B.netDensity <- network.density(MELB01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_B.entropy <- entropy(MELB01_Bft) #entropy

MELB01_B.netMx <- cbind(MELB01_B.netMx, MELB01_B.clusterCoef, MELB01_B.degreeCent$centralization,
                        MELB01_B.netDensity, MELB01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
MELB01_SFg2 <- data.frame(MELB01_SF)
MELB01_SFg2 <- MELB01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_SFg2$player1
player2vector <- MELB01_SFg2$player2
MELB01_SFg3 <- MELB01_SFg2
MELB01_SFg3$p1inp2vec <- is.element(MELB01_SFg3$player1, player2vector)
MELB01_SFg3$p2inp1vec <- is.element(MELB01_SFg3$player2, player1vector)

addPlayer1 <- MELB01_SFg3[ which(MELB01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_SFg3[ which(MELB01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_SFg2 <- rbind(MELB01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
MELB01_SFft <- ftable(MELB01_SFg2$player1, MELB01_SFg2$player2)
MELB01_SFft2 <- as.matrix(MELB01_SFft)
numRows <- nrow(MELB01_SFft2)
numCols <- ncol(MELB01_SFft2)
MELB01_SFft3 <- MELB01_SFft2[c(2:numRows) , c(2:numCols)]
MELB01_SFTable <- graph.adjacency(MELB01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(MELB01_SFTable, vertex.label = V(MELB01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
MELB01_SF.clusterCoef <- transitivity(MELB01_SFTable, type="global") #cluster coefficient
MELB01_SF.degreeCent <- centralization.degree(MELB01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_SFftn <- as.network.matrix(MELB01_SFft)
MELB01_SF.netDensity <- network.density(MELB01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_SF.entropy <- entropy(MELB01_SFft) #entropy

MELB01_SF.netMx <- cbind(MELB01_SF.netMx, MELB01_SF.clusterCoef, MELB01_SF.degreeCent$centralization,
                         MELB01_SF.netDensity, MELB01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
MELB01_TFg2 <- data.frame(MELB01_TF)
MELB01_TFg2 <- MELB01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_TFg2$player1
player2vector <- MELB01_TFg2$player2
MELB01_TFg3 <- MELB01_TFg2
MELB01_TFg3$p1inp2vec <- is.element(MELB01_TFg3$player1, player2vector)
MELB01_TFg3$p2inp1vec <- is.element(MELB01_TFg3$player2, player1vector)

addPlayer1 <- MELB01_TFg3[ which(MELB01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_TFg3[ which(MELB01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_TFg2 <- rbind(MELB01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
MELB01_TFft <- ftable(MELB01_TFg2$player1, MELB01_TFg2$player2)
MELB01_TFft2 <- as.matrix(MELB01_TFft)
numRows <- nrow(MELB01_TFft2)
numCols <- ncol(MELB01_TFft2)
MELB01_TFft3 <- MELB01_TFft2[c(2:numRows) , c(2:numCols)]
MELB01_TFTable <- graph.adjacency(MELB01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(MELB01_TFTable, vertex.label = V(MELB01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
MELB01_TF.clusterCoef <- transitivity(MELB01_TFTable, type="global") #cluster coefficient
MELB01_TF.degreeCent <- centralization.degree(MELB01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_TFftn <- as.network.matrix(MELB01_TFft)
MELB01_TF.netDensity <- network.density(MELB01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_TF.entropy <- entropy(MELB01_TFft) #entropy

MELB01_TF.netMx <- cbind(MELB01_TF.netMx, MELB01_TF.clusterCoef, MELB01_TF.degreeCent$centralization,
                         MELB01_TF.netDensity, MELB01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
MELB01_SAMg2 <- data.frame(MELB01_SAM)
MELB01_SAMg2 <- MELB01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_SAMg2$player1
player2vector <- MELB01_SAMg2$player2
MELB01_SAMg3 <- MELB01_SAMg2
MELB01_SAMg3$p1inp2vec <- is.element(MELB01_SAMg3$player1, player2vector)
MELB01_SAMg3$p2inp1vec <- is.element(MELB01_SAMg3$player2, player1vector)

addPlayer1 <- MELB01_SAMg3[ which(MELB01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_SAMg3[ which(MELB01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_SAMg2 <- rbind(MELB01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
MELB01_SAMft <- ftable(MELB01_SAMg2$player1, MELB01_SAMg2$player2)
MELB01_SAMft2 <- as.matrix(MELB01_SAMft)
numRows <- nrow(MELB01_SAMft2)
numCols <- ncol(MELB01_SAMft2)
MELB01_SAMft3 <- MELB01_SAMft2[c(2:numRows) , c(2:numCols)]
MELB01_SAMTable <- graph.adjacency(MELB01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(MELB01_SAMTable, vertex.label = V(MELB01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
MELB01_SAM.clusterCoef <- transitivity(MELB01_SAMTable, type="global") #cluster coefficient
MELB01_SAM.degreeCent <- centralization.degree(MELB01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_SAMftn <- as.network.matrix(MELB01_SAMft)
MELB01_SAM.netDensity <- network.density(MELB01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_SAM.entropy <- entropy(MELB01_SAMft) #entropy

MELB01_SAM.netMx <- cbind(MELB01_SAM.netMx, MELB01_SAM.clusterCoef, MELB01_SAM.degreeCent$centralization,
                          MELB01_SAM.netDensity, MELB01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
MELB01_TAMg2 <- data.frame(MELB01_TAM)
MELB01_TAMg2 <- MELB01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_TAMg2$player1
player2vector <- MELB01_TAMg2$player2
MELB01_TAMg3 <- MELB01_TAMg2
MELB01_TAMg3$p1inp2vec <- is.element(MELB01_TAMg3$player1, player2vector)
MELB01_TAMg3$p2inp1vec <- is.element(MELB01_TAMg3$player2, player1vector)

addPlayer1 <- MELB01_TAMg3[ which(MELB01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_TAMg3[ which(MELB01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_TAMg2 <- rbind(MELB01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
MELB01_TAMft <- ftable(MELB01_TAMg2$player1, MELB01_TAMg2$player2)
MELB01_TAMft2 <- as.matrix(MELB01_TAMft)
numRows <- nrow(MELB01_TAMft2)
numCols <- ncol(MELB01_TAMft2)
MELB01_TAMft3 <- MELB01_TAMft2[c(2:numRows) , c(2:numCols)]
MELB01_TAMTable <- graph.adjacency(MELB01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(MELB01_TAMTable, vertex.label = V(MELB01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
MELB01_TAM.clusterCoef <- transitivity(MELB01_TAMTable, type="global") #cluster coefficient
MELB01_TAM.degreeCent <- centralization.degree(MELB01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_TAMftn <- as.network.matrix(MELB01_TAMft)
MELB01_TAM.netDensity <- network.density(MELB01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_TAM.entropy <- entropy(MELB01_TAMft) #entropy

MELB01_TAM.netMx <- cbind(MELB01_TAM.netMx, MELB01_TAM.clusterCoef, MELB01_TAM.degreeCent$centralization,
                          MELB01_TAM.netDensity, MELB01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
MELB01_SDMg2 <- data.frame(MELB01_SDM)
MELB01_SDMg2 <- MELB01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_SDMg2$player1
player2vector <- MELB01_SDMg2$player2
MELB01_SDMg3 <- MELB01_SDMg2
MELB01_SDMg3$p1inp2vec <- is.element(MELB01_SDMg3$player1, player2vector)
MELB01_SDMg3$p2inp1vec <- is.element(MELB01_SDMg3$player2, player1vector)

addPlayer1 <- MELB01_SDMg3[ which(MELB01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_SDMg3[ which(MELB01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_SDMg2 <- rbind(MELB01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
MELB01_SDMft <- ftable(MELB01_SDMg2$player1, MELB01_SDMg2$player2)
MELB01_SDMft2 <- as.matrix(MELB01_SDMft)
numRows <- nrow(MELB01_SDMft2)
numCols <- ncol(MELB01_SDMft2)
MELB01_SDMft3 <- MELB01_SDMft2[c(2:numRows) , c(2:numCols)]
MELB01_SDMTable <- graph.adjacency(MELB01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(MELB01_SDMTable, vertex.label = V(MELB01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
MELB01_SDM.clusterCoef <- transitivity(MELB01_SDMTable, type="global") #cluster coefficient
MELB01_SDM.degreeCent <- centralization.degree(MELB01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_SDMftn <- as.network.matrix(MELB01_SDMft)
MELB01_SDM.netDensity <- network.density(MELB01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_SDM.entropy <- entropy(MELB01_SDMft) #entropy

MELB01_SDM.netMx <- cbind(MELB01_SDM.netMx, MELB01_SDM.clusterCoef, MELB01_SDM.degreeCent$centralization,
                          MELB01_SDM.netDensity, MELB01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
MELB01_TDMg2 <- data.frame(MELB01_TDM)
MELB01_TDMg2 <- MELB01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_TDMg2$player1
player2vector <- MELB01_TDMg2$player2
MELB01_TDMg3 <- MELB01_TDMg2
MELB01_TDMg3$p1inp2vec <- is.element(MELB01_TDMg3$player1, player2vector)
MELB01_TDMg3$p2inp1vec <- is.element(MELB01_TDMg3$player2, player1vector)

addPlayer1 <- MELB01_TDMg3[ which(MELB01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_TDMg3[ which(MELB01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_TDMg2 <- rbind(MELB01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
MELB01_TDMft <- ftable(MELB01_TDMg2$player1, MELB01_TDMg2$player2)
MELB01_TDMft2 <- as.matrix(MELB01_TDMft)
numRows <- nrow(MELB01_TDMft2)
numCols <- ncol(MELB01_TDMft2)
MELB01_TDMft3 <- MELB01_TDMft2[c(2:numRows) , c(2:numCols)]
MELB01_TDMTable <- graph.adjacency(MELB01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(MELB01_TDMTable, vertex.label = V(MELB01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
MELB01_TDM.clusterCoef <- transitivity(MELB01_TDMTable, type="global") #cluster coefficient
MELB01_TDM.degreeCent <- centralization.degree(MELB01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_TDMftn <- as.network.matrix(MELB01_TDMft)
MELB01_TDM.netDensity <- network.density(MELB01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_TDM.entropy <- entropy(MELB01_TDMft) #entropy

MELB01_TDM.netMx <- cbind(MELB01_TDM.netMx, MELB01_TDM.clusterCoef, MELB01_TDM.degreeCent$centralization,
                          MELB01_TDM.netDensity, MELB01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
MELB01_SDg2 <- data.frame(MELB01_SD)
MELB01_SDg2 <- MELB01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_SDg2$player1
player2vector <- MELB01_SDg2$player2
MELB01_SDg3 <- MELB01_SDg2
MELB01_SDg3$p1inp2vec <- is.element(MELB01_SDg3$player1, player2vector)
MELB01_SDg3$p2inp1vec <- is.element(MELB01_SDg3$player2, player1vector)

addPlayer1 <- MELB01_SDg3[ which(MELB01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_SDg3[ which(MELB01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_SDg2 <- rbind(MELB01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
MELB01_SDft <- ftable(MELB01_SDg2$player1, MELB01_SDg2$player2)
MELB01_SDft2 <- as.matrix(MELB01_SDft)
numRows <- nrow(MELB01_SDft2)
numCols <- ncol(MELB01_SDft2)
MELB01_SDft3 <- MELB01_SDft2[c(2:numRows) , c(2:numCols)]
MELB01_SDTable <- graph.adjacency(MELB01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(MELB01_SDTable, vertex.label = V(MELB01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
MELB01_SD.clusterCoef <- transitivity(MELB01_SDTable, type="global") #cluster coefficient
MELB01_SD.degreeCent <- centralization.degree(MELB01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_SDftn <- as.network.matrix(MELB01_SDft)
MELB01_SD.netDensity <- network.density(MELB01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_SD.entropy <- entropy(MELB01_SDft) #entropy

MELB01_SD.netMx <- cbind(MELB01_SD.netMx, MELB01_SD.clusterCoef, MELB01_SD.degreeCent$centralization,
                         MELB01_SD.netDensity, MELB01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
MELB01_TDg2 <- data.frame(MELB01_TD)
MELB01_TDg2 <- MELB01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_TDg2$player1
player2vector <- MELB01_TDg2$player2
MELB01_TDg3 <- MELB01_TDg2
MELB01_TDg3$p1inp2vec <- is.element(MELB01_TDg3$player1, player2vector)
MELB01_TDg3$p2inp1vec <- is.element(MELB01_TDg3$player2, player1vector)

addPlayer1 <- MELB01_TDg3[ which(MELB01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_TDg3[ which(MELB01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_TDg2 <- rbind(MELB01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
MELB01_TDft <- ftable(MELB01_TDg2$player1, MELB01_TDg2$player2)
MELB01_TDft2 <- as.matrix(MELB01_TDft)
numRows <- nrow(MELB01_TDft2)
numCols <- ncol(MELB01_TDft2)
MELB01_TDft3 <- MELB01_TDft2[c(2:numRows) , c(2:numCols)]
MELB01_TDTable <- graph.adjacency(MELB01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(MELB01_TDTable, vertex.label = V(MELB01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
MELB01_TD.clusterCoef <- transitivity(MELB01_TDTable, type="global") #cluster coefficient
MELB01_TD.degreeCent <- centralization.degree(MELB01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_TDftn <- as.network.matrix(MELB01_TDft)
MELB01_TD.netDensity <- network.density(MELB01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_TD.entropy <- entropy(MELB01_TDft) #entropy

MELB01_TD.netMx <- cbind(MELB01_TD.netMx, MELB01_TD.clusterCoef, MELB01_TD.degreeCent$centralization,
                         MELB01_TD.netDensity, MELB01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
MELB01_QTg2 <- data.frame(MELB01_QT)
MELB01_QTg2 <- MELB01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB01_QTg2$player1
player2vector <- MELB01_QTg2$player2
MELB01_QTg3 <- MELB01_QTg2
MELB01_QTg3$p1inp2vec <- is.element(MELB01_QTg3$player1, player2vector)
MELB01_QTg3$p2inp1vec <- is.element(MELB01_QTg3$player2, player1vector)

addPlayer1 <- MELB01_QTg3[ which(MELB01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB01_QTg3[ which(MELB01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB01_QTg2 <- rbind(MELB01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
MELB01_QTft <- ftable(MELB01_QTg2$player1, MELB01_QTg2$player2)
MELB01_QTft2 <- as.matrix(MELB01_QTft)
numRows <- nrow(MELB01_QTft2)
numCols <- ncol(MELB01_QTft2)
MELB01_QTft3 <- MELB01_QTft2[c(2:numRows) , c(2:numCols)]
MELB01_QTTable <- graph.adjacency(MELB01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(MELB01_QTTable, vertex.label = V(MELB01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
MELB01_QT.clusterCoef <- transitivity(MELB01_QTTable, type="global") #cluster coefficient
MELB01_QT.degreeCent <- centralization.degree(MELB01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB01_QTftn <- as.network.matrix(MELB01_QTft)
MELB01_QT.netDensity <- network.density(MELB01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB01_QT.entropy <- entropy(MELB01_QTft) #entropy

MELB01_QT.netMx <- cbind(MELB01_QT.netMx, MELB01_QT.clusterCoef, MELB01_QT.degreeCent$centralization,
                         MELB01_QT.netDensity, MELB01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB01_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
NMFC01_Gg2 <- data.frame(NMFC01_G)
NMFC01_Gg2 <- NMFC01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_Gg2$player1
player2vector <- NMFC01_Gg2$player2
NMFC01_Gg3 <- NMFC01_Gg2
NMFC01_Gg3$p1inp2vec <- is.element(NMFC01_Gg3$player1, player2vector)
NMFC01_Gg3$p2inp1vec <- is.element(NMFC01_Gg3$player2, player1vector)

addPlayer1 <- NMFC01_Gg3[ which(NMFC01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_Gg3[ which(NMFC01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_Gg2 <- rbind(NMFC01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
NMFC01_Gft <- ftable(NMFC01_Gg2$player1, NMFC01_Gg2$player2)
NMFC01_Gft2 <- as.matrix(NMFC01_Gft)
numRows <- nrow(NMFC01_Gft2)
numCols <- ncol(NMFC01_Gft2)
NMFC01_Gft3 <- NMFC01_Gft2[c(2:numRows) , c(2:numCols)]
NMFC01_GTable <- graph.adjacency(NMFC01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(NMFC01_GTable, vertex.label = V(NMFC01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
NMFC01_G.clusterCoef <- transitivity(NMFC01_GTable, type="global") #cluster coefficient
NMFC01_G.degreeCent <- centralization.degree(NMFC01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_Gftn <- as.network.matrix(NMFC01_Gft)
NMFC01_G.netDensity <- network.density(NMFC01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_G.entropy <- entropy(NMFC01_Gft) #entropy

NMFC01_G.netMx <- cbind(NMFC01_G.netMx, NMFC01_G.clusterCoef, NMFC01_G.degreeCent$centralization,
                        NMFC01_G.netDensity, NMFC01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
NMFC01_Bg2 <- data.frame(NMFC01_B)
NMFC01_Bg2 <- NMFC01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_Bg2$player1
player2vector <- NMFC01_Bg2$player2
NMFC01_Bg3 <- NMFC01_Bg2
NMFC01_Bg3$p1inp2vec <- is.element(NMFC01_Bg3$player1, player2vector)
NMFC01_Bg3$p2inp1vec <- is.element(NMFC01_Bg3$player2, player1vector)

addPlayer1 <- NMFC01_Bg3[ which(NMFC01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_Bg3[ which(NMFC01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_Bg2 <- rbind(NMFC01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
NMFC01_Bft <- ftable(NMFC01_Bg2$player1, NMFC01_Bg2$player2)
NMFC01_Bft2 <- as.matrix(NMFC01_Bft)
numRows <- nrow(NMFC01_Bft2)
numCols <- ncol(NMFC01_Bft2)
NMFC01_Bft3 <- NMFC01_Bft2[c(2:numRows) , c(2:numCols)]
NMFC01_BTable <- graph.adjacency(NMFC01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(NMFC01_BTable, vertex.label = V(NMFC01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
NMFC01_B.clusterCoef <- transitivity(NMFC01_BTable, type="global") #cluster coefficient
NMFC01_B.degreeCent <- centralization.degree(NMFC01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_Bftn <- as.network.matrix(NMFC01_Bft)
NMFC01_B.netDensity <- network.density(NMFC01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_B.entropy <- entropy(NMFC01_Bft) #entropy

NMFC01_B.netMx <- cbind(NMFC01_B.netMx, NMFC01_B.clusterCoef, NMFC01_B.degreeCent$centralization,
                        NMFC01_B.netDensity, NMFC01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
NMFC01_SFg2 <- data.frame(NMFC01_SF)
NMFC01_SFg2 <- NMFC01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_SFg2$player1
player2vector <- NMFC01_SFg2$player2
NMFC01_SFg3 <- NMFC01_SFg2
NMFC01_SFg3$p1inp2vec <- is.element(NMFC01_SFg3$player1, player2vector)
NMFC01_SFg3$p2inp1vec <- is.element(NMFC01_SFg3$player2, player1vector)

addPlayer1 <- NMFC01_SFg3[ which(NMFC01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_SFg3[ which(NMFC01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_SFg2 <- rbind(NMFC01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
NMFC01_SFft <- ftable(NMFC01_SFg2$player1, NMFC01_SFg2$player2)
NMFC01_SFft2 <- as.matrix(NMFC01_SFft)
numRows <- nrow(NMFC01_SFft2)
numCols <- ncol(NMFC01_SFft2)
NMFC01_SFft3 <- NMFC01_SFft2[c(2:numRows) , c(2:numCols)]
NMFC01_SFTable <- graph.adjacency(NMFC01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(NMFC01_SFTable, vertex.label = V(NMFC01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
NMFC01_SF.clusterCoef <- transitivity(NMFC01_SFTable, type="global") #cluster coefficient
NMFC01_SF.degreeCent <- centralization.degree(NMFC01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_SFftn <- as.network.matrix(NMFC01_SFft)
NMFC01_SF.netDensity <- network.density(NMFC01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_SF.entropy <- entropy(NMFC01_SFft) #entropy

NMFC01_SF.netMx <- cbind(NMFC01_SF.netMx, NMFC01_SF.clusterCoef, NMFC01_SF.degreeCent$centralization,
                         NMFC01_SF.netDensity, NMFC01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
NMFC01_TFg2 <- data.frame(NMFC01_TF)
NMFC01_TFg2 <- NMFC01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_TFg2$player1
player2vector <- NMFC01_TFg2$player2
NMFC01_TFg3 <- NMFC01_TFg2
NMFC01_TFg3$p1inp2vec <- is.element(NMFC01_TFg3$player1, player2vector)
NMFC01_TFg3$p2inp1vec <- is.element(NMFC01_TFg3$player2, player1vector)

addPlayer1 <- NMFC01_TFg3[ which(NMFC01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_TFg3[ which(NMFC01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_TFg2 <- rbind(NMFC01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
NMFC01_TFft <- ftable(NMFC01_TFg2$player1, NMFC01_TFg2$player2)
NMFC01_TFft2 <- as.matrix(NMFC01_TFft)
numRows <- nrow(NMFC01_TFft2)
numCols <- ncol(NMFC01_TFft2)
NMFC01_TFft3 <- NMFC01_TFft2[c(2:numRows) , c(2:numCols)]
NMFC01_TFTable <- graph.adjacency(NMFC01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(NMFC01_TFTable, vertex.label = V(NMFC01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
NMFC01_TF.clusterCoef <- transitivity(NMFC01_TFTable, type="global") #cluster coefficient
NMFC01_TF.degreeCent <- centralization.degree(NMFC01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_TFftn <- as.network.matrix(NMFC01_TFft)
NMFC01_TF.netDensity <- network.density(NMFC01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_TF.entropy <- entropy(NMFC01_TFft) #entropy

NMFC01_TF.netMx <- cbind(NMFC01_TF.netMx, NMFC01_TF.clusterCoef, NMFC01_TF.degreeCent$centralization,
                         NMFC01_TF.netDensity, NMFC01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
NMFC01_SAMg2 <- data.frame(NMFC01_SAM)
NMFC01_SAMg2 <- NMFC01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_SAMg2$player1
player2vector <- NMFC01_SAMg2$player2
NMFC01_SAMg3 <- NMFC01_SAMg2
NMFC01_SAMg3$p1inp2vec <- is.element(NMFC01_SAMg3$player1, player2vector)
NMFC01_SAMg3$p2inp1vec <- is.element(NMFC01_SAMg3$player2, player1vector)

addPlayer1 <- NMFC01_SAMg3[ which(NMFC01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_SAMg3[ which(NMFC01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_SAMg2 <- rbind(NMFC01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
NMFC01_SAMft <- ftable(NMFC01_SAMg2$player1, NMFC01_SAMg2$player2)
NMFC01_SAMft2 <- as.matrix(NMFC01_SAMft)
numRows <- nrow(NMFC01_SAMft2)
numCols <- ncol(NMFC01_SAMft2)
NMFC01_SAMft3 <- NMFC01_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC01_SAMTable <- graph.adjacency(NMFC01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(NMFC01_SAMTable, vertex.label = V(NMFC01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
NMFC01_SAM.clusterCoef <- transitivity(NMFC01_SAMTable, type="global") #cluster coefficient
NMFC01_SAM.degreeCent <- centralization.degree(NMFC01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_SAMftn <- as.network.matrix(NMFC01_SAMft)
NMFC01_SAM.netDensity <- network.density(NMFC01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_SAM.entropy <- entropy(NMFC01_SAMft) #entropy

NMFC01_SAM.netMx <- cbind(NMFC01_SAM.netMx, NMFC01_SAM.clusterCoef, NMFC01_SAM.degreeCent$centralization,
                          NMFC01_SAM.netDensity, NMFC01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
NMFC01_TAMg2 <- data.frame(NMFC01_TAM)
NMFC01_TAMg2 <- NMFC01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_TAMg2$player1
player2vector <- NMFC01_TAMg2$player2
NMFC01_TAMg3 <- NMFC01_TAMg2
NMFC01_TAMg3$p1inp2vec <- is.element(NMFC01_TAMg3$player1, player2vector)
NMFC01_TAMg3$p2inp1vec <- is.element(NMFC01_TAMg3$player2, player1vector)

addPlayer1 <- NMFC01_TAMg3[ which(NMFC01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_TAMg3[ which(NMFC01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_TAMg2 <- rbind(NMFC01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
NMFC01_TAMft <- ftable(NMFC01_TAMg2$player1, NMFC01_TAMg2$player2)
NMFC01_TAMft2 <- as.matrix(NMFC01_TAMft)
numRows <- nrow(NMFC01_TAMft2)
numCols <- ncol(NMFC01_TAMft2)
NMFC01_TAMft3 <- NMFC01_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC01_TAMTable <- graph.adjacency(NMFC01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(NMFC01_TAMTable, vertex.label = V(NMFC01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
NMFC01_TAM.clusterCoef <- transitivity(NMFC01_TAMTable, type="global") #cluster coefficient
NMFC01_TAM.degreeCent <- centralization.degree(NMFC01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_TAMftn <- as.network.matrix(NMFC01_TAMft)
NMFC01_TAM.netDensity <- network.density(NMFC01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_TAM.entropy <- entropy(NMFC01_TAMft) #entropy

NMFC01_TAM.netMx <- cbind(NMFC01_TAM.netMx, NMFC01_TAM.clusterCoef, NMFC01_TAM.degreeCent$centralization,
                          NMFC01_TAM.netDensity, NMFC01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
NMFC01_SDMg2 <- data.frame(NMFC01_SDM)
NMFC01_SDMg2 <- NMFC01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_SDMg2$player1
player2vector <- NMFC01_SDMg2$player2
NMFC01_SDMg3 <- NMFC01_SDMg2
NMFC01_SDMg3$p1inp2vec <- is.element(NMFC01_SDMg3$player1, player2vector)
NMFC01_SDMg3$p2inp1vec <- is.element(NMFC01_SDMg3$player2, player1vector)

addPlayer1 <- NMFC01_SDMg3[ which(NMFC01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_SDMg3[ which(NMFC01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_SDMg2 <- rbind(NMFC01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
NMFC01_SDMft <- ftable(NMFC01_SDMg2$player1, NMFC01_SDMg2$player2)
NMFC01_SDMft2 <- as.matrix(NMFC01_SDMft)
numRows <- nrow(NMFC01_SDMft2)
numCols <- ncol(NMFC01_SDMft2)
NMFC01_SDMft3 <- NMFC01_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC01_SDMTable <- graph.adjacency(NMFC01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(NMFC01_SDMTable, vertex.label = V(NMFC01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
NMFC01_SDM.clusterCoef <- transitivity(NMFC01_SDMTable, type="global") #cluster coefficient
NMFC01_SDM.degreeCent <- centralization.degree(NMFC01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_SDMftn <- as.network.matrix(NMFC01_SDMft)
NMFC01_SDM.netDensity <- network.density(NMFC01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_SDM.entropy <- entropy(NMFC01_SDMft) #entropy

NMFC01_SDM.netMx <- cbind(NMFC01_SDM.netMx, NMFC01_SDM.clusterCoef, NMFC01_SDM.degreeCent$centralization,
                          NMFC01_SDM.netDensity, NMFC01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
NMFC01_TDMg2 <- data.frame(NMFC01_TDM)
NMFC01_TDMg2 <- NMFC01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_TDMg2$player1
player2vector <- NMFC01_TDMg2$player2
NMFC01_TDMg3 <- NMFC01_TDMg2
NMFC01_TDMg3$p1inp2vec <- is.element(NMFC01_TDMg3$player1, player2vector)
NMFC01_TDMg3$p2inp1vec <- is.element(NMFC01_TDMg3$player2, player1vector)

addPlayer1 <- NMFC01_TDMg3[ which(NMFC01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_TDMg3[ which(NMFC01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_TDMg2 <- rbind(NMFC01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
NMFC01_TDMft <- ftable(NMFC01_TDMg2$player1, NMFC01_TDMg2$player2)
NMFC01_TDMft2 <- as.matrix(NMFC01_TDMft)
numRows <- nrow(NMFC01_TDMft2)
numCols <- ncol(NMFC01_TDMft2)
NMFC01_TDMft3 <- NMFC01_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC01_TDMTable <- graph.adjacency(NMFC01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(NMFC01_TDMTable, vertex.label = V(NMFC01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
NMFC01_TDM.clusterCoef <- transitivity(NMFC01_TDMTable, type="global") #cluster coefficient
NMFC01_TDM.degreeCent <- centralization.degree(NMFC01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_TDMftn <- as.network.matrix(NMFC01_TDMft)
NMFC01_TDM.netDensity <- network.density(NMFC01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_TDM.entropy <- entropy(NMFC01_TDMft) #entropy

NMFC01_TDM.netMx <- cbind(NMFC01_TDM.netMx, NMFC01_TDM.clusterCoef, NMFC01_TDM.degreeCent$centralization,
                          NMFC01_TDM.netDensity, NMFC01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
NMFC01_SDg2 <- data.frame(NMFC01_SD)
NMFC01_SDg2 <- NMFC01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_SDg2$player1
player2vector <- NMFC01_SDg2$player2
NMFC01_SDg3 <- NMFC01_SDg2
NMFC01_SDg3$p1inp2vec <- is.element(NMFC01_SDg3$player1, player2vector)
NMFC01_SDg3$p2inp1vec <- is.element(NMFC01_SDg3$player2, player1vector)

addPlayer1 <- NMFC01_SDg3[ which(NMFC01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_SDg3[ which(NMFC01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_SDg2 <- rbind(NMFC01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
NMFC01_SDft <- ftable(NMFC01_SDg2$player1, NMFC01_SDg2$player2)
NMFC01_SDft2 <- as.matrix(NMFC01_SDft)
numRows <- nrow(NMFC01_SDft2)
numCols <- ncol(NMFC01_SDft2)
NMFC01_SDft3 <- NMFC01_SDft2[c(2:numRows) , c(2:numCols)]
NMFC01_SDTable <- graph.adjacency(NMFC01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(NMFC01_SDTable, vertex.label = V(NMFC01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
NMFC01_SD.clusterCoef <- transitivity(NMFC01_SDTable, type="global") #cluster coefficient
NMFC01_SD.degreeCent <- centralization.degree(NMFC01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_SDftn <- as.network.matrix(NMFC01_SDft)
NMFC01_SD.netDensity <- network.density(NMFC01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_SD.entropy <- entropy(NMFC01_SDft) #entropy

NMFC01_SD.netMx <- cbind(NMFC01_SD.netMx, NMFC01_SD.clusterCoef, NMFC01_SD.degreeCent$centralization,
                         NMFC01_SD.netDensity, NMFC01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
NMFC01_TDg2 <- data.frame(NMFC01_TD)
NMFC01_TDg2 <- NMFC01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_TDg2$player1
player2vector <- NMFC01_TDg2$player2
NMFC01_TDg3 <- NMFC01_TDg2
NMFC01_TDg3$p1inp2vec <- is.element(NMFC01_TDg3$player1, player2vector)
NMFC01_TDg3$p2inp1vec <- is.element(NMFC01_TDg3$player2, player1vector)

addPlayer1 <- NMFC01_TDg3[ which(NMFC01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_TDg3[ which(NMFC01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_TDg2 <- rbind(NMFC01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
NMFC01_TDft <- ftable(NMFC01_TDg2$player1, NMFC01_TDg2$player2)
NMFC01_TDft2 <- as.matrix(NMFC01_TDft)
numRows <- nrow(NMFC01_TDft2)
numCols <- ncol(NMFC01_TDft2)
NMFC01_TDft3 <- NMFC01_TDft2[c(2:numRows) , c(2:numCols)]
NMFC01_TDTable <- graph.adjacency(NMFC01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(NMFC01_TDTable, vertex.label = V(NMFC01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
NMFC01_TD.clusterCoef <- transitivity(NMFC01_TDTable, type="global") #cluster coefficient
NMFC01_TD.degreeCent <- centralization.degree(NMFC01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_TDftn <- as.network.matrix(NMFC01_TDft)
NMFC01_TD.netDensity <- network.density(NMFC01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_TD.entropy <- entropy(NMFC01_TDft) #entropy

NMFC01_TD.netMx <- cbind(NMFC01_TD.netMx, NMFC01_TD.clusterCoef, NMFC01_TD.degreeCent$centralization,
                         NMFC01_TD.netDensity, NMFC01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
NMFC01_QTg2 <- data.frame(NMFC01_QT)
NMFC01_QTg2 <- NMFC01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC01_QTg2$player1
player2vector <- NMFC01_QTg2$player2
NMFC01_QTg3 <- NMFC01_QTg2
NMFC01_QTg3$p1inp2vec <- is.element(NMFC01_QTg3$player1, player2vector)
NMFC01_QTg3$p2inp1vec <- is.element(NMFC01_QTg3$player2, player1vector)

addPlayer1 <- NMFC01_QTg3[ which(NMFC01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC01_QTg3[ which(NMFC01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC01_QTg2 <- rbind(NMFC01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
NMFC01_QTft <- ftable(NMFC01_QTg2$player1, NMFC01_QTg2$player2)
NMFC01_QTft2 <- as.matrix(NMFC01_QTft)
numRows <- nrow(NMFC01_QTft2)
numCols <- ncol(NMFC01_QTft2)
NMFC01_QTft3 <- NMFC01_QTft2[c(2:numRows) , c(2:numCols)]
NMFC01_QTTable <- graph.adjacency(NMFC01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(NMFC01_QTTable, vertex.label = V(NMFC01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
NMFC01_QT.clusterCoef <- transitivity(NMFC01_QTTable, type="global") #cluster coefficient
NMFC01_QT.degreeCent <- centralization.degree(NMFC01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC01_QTftn <- as.network.matrix(NMFC01_QTft)
NMFC01_QT.netDensity <- network.density(NMFC01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC01_QT.entropy <- entropy(NMFC01_QTft) #entropy

NMFC01_QT.netMx <- cbind(NMFC01_QT.netMx, NMFC01_QT.clusterCoef, NMFC01_QT.degreeCent$centralization,
                         NMFC01_QT.netDensity, NMFC01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC01_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "PORT"
KIoutcome = "Goal_F"
PORT01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
PORT01_Gg2 <- data.frame(PORT01_G)
PORT01_Gg2 <- PORT01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_Gg2$player1
player2vector <- PORT01_Gg2$player2
PORT01_Gg3 <- PORT01_Gg2
PORT01_Gg3$p1inp2vec <- is.element(PORT01_Gg3$player1, player2vector)
PORT01_Gg3$p2inp1vec <- is.element(PORT01_Gg3$player2, player1vector)

addPlayer1 <- PORT01_Gg3[ which(PORT01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT01_Gg2 <- rbind(PORT01_Gg2, addPlayer1)

#Round 1, Goal graph using weighted edges
PORT01_Gft <- ftable(PORT01_Gg2$player1, PORT01_Gg2$player2)
PORT01_Gft2 <- as.matrix(PORT01_Gft)
numRows <- nrow(PORT01_Gft2)
numCols <- ncol(PORT01_Gft2)
PORT01_Gft3 <- PORT01_Gft2[c(2:numRows) , c(1:numCols)]
PORT01_GTable <- graph.adjacency(PORT01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(PORT01_GTable, vertex.label = V(PORT01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
PORT01_G.clusterCoef <- transitivity(PORT01_GTable, type="global") #cluster coefficient
PORT01_G.degreeCent <- centralization.degree(PORT01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_Gftn <- as.network.matrix(PORT01_Gft)
PORT01_G.netDensity <- network.density(PORT01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_G.entropy <- entropy(PORT01_Gft) #entropy

PORT01_G.netMx <- cbind(PORT01_G.netMx, PORT01_G.clusterCoef, PORT01_G.degreeCent$centralization,
                        PORT01_G.netDensity, PORT01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "PORT"
KIoutcome = "Behind_F"
PORT01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
PORT01_Bg2 <- data.frame(PORT01_B)
PORT01_Bg2 <- PORT01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_Bg2$player1
player2vector <- PORT01_Bg2$player2
PORT01_Bg3 <- PORT01_Bg2
PORT01_Bg3$p1inp2vec <- is.element(PORT01_Bg3$player1, player2vector)
PORT01_Bg3$p2inp1vec <- is.element(PORT01_Bg3$player2, player1vector)

addPlayer1 <- PORT01_Bg3[ which(PORT01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_Bg3[ which(PORT01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_Bg2 <- rbind(PORT01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
PORT01_Bft <- ftable(PORT01_Bg2$player1, PORT01_Bg2$player2)
PORT01_Bft2 <- as.matrix(PORT01_Bft)
numRows <- nrow(PORT01_Bft2)
numCols <- ncol(PORT01_Bft2)
PORT01_Bft3 <- PORT01_Bft2[c(2:numRows) , c(2:numCols)]
PORT01_BTable <- graph.adjacency(PORT01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(PORT01_BTable, vertex.label = V(PORT01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
PORT01_B.clusterCoef <- transitivity(PORT01_BTable, type="global") #cluster coefficient
PORT01_B.degreeCent <- centralization.degree(PORT01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_Bftn <- as.network.matrix(PORT01_Bft)
PORT01_B.netDensity <- network.density(PORT01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_B.entropy <- entropy(PORT01_Bft) #entropy

PORT01_B.netMx <- cbind(PORT01_B.netMx, PORT01_B.clusterCoef, PORT01_B.degreeCent$centralization,
                        PORT01_B.netDensity, PORT01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************

round = 1
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
PORT01_SFg2 <- data.frame(PORT01_SF)
PORT01_SFg2 <- PORT01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_SFg2$player1
player2vector <- PORT01_SFg2$player2
PORT01_SFg3 <- PORT01_SFg2
PORT01_SFg3$p1inp2vec <- is.element(PORT01_SFg3$player1, player2vector)
PORT01_SFg3$p2inp1vec <- is.element(PORT01_SFg3$player2, player1vector)

addPlayer1 <- PORT01_SFg3[ which(PORT01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_SFg3[ which(PORT01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_SFg2 <- rbind(PORT01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
PORT01_SFft <- ftable(PORT01_SFg2$player1, PORT01_SFg2$player2)
PORT01_SFft2 <- as.matrix(PORT01_SFft)
numRows <- nrow(PORT01_SFft2)
numCols <- ncol(PORT01_SFft2)
PORT01_SFft3 <- PORT01_SFft2[c(2:numRows) , c(2:numCols)]
PORT01_SFTable <- graph.adjacency(PORT01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(PORT01_SFTable, vertex.label = V(PORT01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
PORT01_SF.clusterCoef <- transitivity(PORT01_SFTable, type="global") #cluster coefficient
PORT01_SF.degreeCent <- centralization.degree(PORT01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_SFftn <- as.network.matrix(PORT01_SFft)
PORT01_SF.netDensity <- network.density(PORT01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_SF.entropy <- entropy(PORT01_SFft) #entropy

PORT01_SF.netMx <- cbind(PORT01_SF.netMx, PORT01_SF.clusterCoef, PORT01_SF.degreeCent$centralization,
                         PORT01_SF.netDensity, PORT01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
PORT01_TFg2 <- data.frame(PORT01_TF)
PORT01_TFg2 <- PORT01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_TFg2$player1
player2vector <- PORT01_TFg2$player2
PORT01_TFg3 <- PORT01_TFg2
PORT01_TFg3$p1inp2vec <- is.element(PORT01_TFg3$player1, player2vector)
PORT01_TFg3$p2inp1vec <- is.element(PORT01_TFg3$player2, player1vector)

addPlayer1 <- PORT01_TFg3[ which(PORT01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT01_TFg2 <- rbind(PORT01_TFg2, addPlayer1)

#Round 1, FWD Turnover graph using weighted edges
PORT01_TFft <- ftable(PORT01_TFg2$player1, PORT01_TFg2$player2)
PORT01_TFft2 <- as.matrix(PORT01_TFft)
numRows <- nrow(PORT01_TFft2)
numCols <- ncol(PORT01_TFft2)
PORT01_TFft3 <- PORT01_TFft2[c(2:numRows) , c(1:numCols)]
PORT01_TFTable <- graph.adjacency(PORT01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(PORT01_TFTable, vertex.label = V(PORT01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
PORT01_TF.clusterCoef <- transitivity(PORT01_TFTable, type="global") #cluster coefficient
PORT01_TF.degreeCent <- centralization.degree(PORT01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_TFftn <- as.network.matrix(PORT01_TFft)
PORT01_TF.netDensity <- network.density(PORT01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_TF.entropy <- entropy(PORT01_TFft) #entropy

PORT01_TF.netMx <- cbind(PORT01_TF.netMx, PORT01_TF.clusterCoef, PORT01_TF.degreeCent$centralization,
                         PORT01_TF.netDensity, PORT01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************

round = 1
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
PORT01_SAMg2 <- data.frame(PORT01_SAM)
PORT01_SAMg2 <- PORT01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_SAMg2$player1
player2vector <- PORT01_SAMg2$player2
PORT01_SAMg3 <- PORT01_SAMg2
PORT01_SAMg3$p1inp2vec <- is.element(PORT01_SAMg3$player1, player2vector)
PORT01_SAMg3$p2inp1vec <- is.element(PORT01_SAMg3$player2, player1vector)

addPlayer1 <- PORT01_SAMg3[ which(PORT01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_SAMg3[ which(PORT01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_SAMg2 <- rbind(PORT01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
PORT01_SAMft <- ftable(PORT01_SAMg2$player1, PORT01_SAMg2$player2)
PORT01_SAMft2 <- as.matrix(PORT01_SAMft)
numRows <- nrow(PORT01_SAMft2)
numCols <- ncol(PORT01_SAMft2)
PORT01_SAMft3 <- PORT01_SAMft2[c(2:numRows) , c(2:numCols)]
PORT01_SAMTable <- graph.adjacency(PORT01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(PORT01_SAMTable, vertex.label = V(PORT01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
PORT01_SAM.clusterCoef <- transitivity(PORT01_SAMTable, type="global") #cluster coefficient
PORT01_SAM.degreeCent <- centralization.degree(PORT01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_SAMftn <- as.network.matrix(PORT01_SAMft)
PORT01_SAM.netDensity <- network.density(PORT01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_SAM.entropy <- entropy(PORT01_SAMft) #entropy

PORT01_SAM.netMx <- cbind(PORT01_SAM.netMx, PORT01_SAM.clusterCoef, PORT01_SAM.degreeCent$centralization,
                          PORT01_SAM.netDensity, PORT01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
PORT01_TAMg2 <- data.frame(PORT01_TAM)
PORT01_TAMg2 <- PORT01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_TAMg2$player1
player2vector <- PORT01_TAMg2$player2
PORT01_TAMg3 <- PORT01_TAMg2
PORT01_TAMg3$p1inp2vec <- is.element(PORT01_TAMg3$player1, player2vector)
PORT01_TAMg3$p2inp1vec <- is.element(PORT01_TAMg3$player2, player1vector)

addPlayer1 <- PORT01_TAMg3[ which(PORT01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT01_TAMg2 <- rbind(PORT01_TAMg2, addPlayer1)

#Round 1, AM Turnover graph using weighted edges
PORT01_TAMft <- ftable(PORT01_TAMg2$player1, PORT01_TAMg2$player2)
PORT01_TAMft2 <- as.matrix(PORT01_TAMft)
numRows <- nrow(PORT01_TAMft2)
numCols <- ncol(PORT01_TAMft2)
PORT01_TAMft3 <- PORT01_TAMft2[c(2:numRows) , c(1:numCols)]
PORT01_TAMTable <- graph.adjacency(PORT01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(PORT01_TAMTable, vertex.label = V(PORT01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
PORT01_TAM.clusterCoef <- transitivity(PORT01_TAMTable, type="global") #cluster coefficient
PORT01_TAM.degreeCent <- centralization.degree(PORT01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_TAMftn <- as.network.matrix(PORT01_TAMft)
PORT01_TAM.netDensity <- network.density(PORT01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_TAM.entropy <- entropy(PORT01_TAMft) #entropy

PORT01_TAM.netMx <- cbind(PORT01_TAM.netMx, PORT01_TAM.clusterCoef, PORT01_TAM.degreeCent$centralization,
                          PORT01_TAM.netDensity, PORT01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
PORT01_SDMg2 <- data.frame(PORT01_SDM)
PORT01_SDMg2 <- PORT01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_SDMg2$player1
player2vector <- PORT01_SDMg2$player2
PORT01_SDMg3 <- PORT01_SDMg2
PORT01_SDMg3$p1inp2vec <- is.element(PORT01_SDMg3$player1, player2vector)
PORT01_SDMg3$p2inp1vec <- is.element(PORT01_SDMg3$player2, player1vector)

addPlayer1 <- PORT01_SDMg3[ which(PORT01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_SDMg3[ which(PORT01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_SDMg2 <- rbind(PORT01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
PORT01_SDMft <- ftable(PORT01_SDMg2$player1, PORT01_SDMg2$player2)
PORT01_SDMft2 <- as.matrix(PORT01_SDMft)
numRows <- nrow(PORT01_SDMft2)
numCols <- ncol(PORT01_SDMft2)
PORT01_SDMft3 <- PORT01_SDMft2[c(2:numRows) , c(2:numCols)]
PORT01_SDMTable <- graph.adjacency(PORT01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(PORT01_SDMTable, vertex.label = V(PORT01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
PORT01_SDM.clusterCoef <- transitivity(PORT01_SDMTable, type="global") #cluster coefficient
PORT01_SDM.degreeCent <- centralization.degree(PORT01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_SDMftn <- as.network.matrix(PORT01_SDMft)
PORT01_SDM.netDensity <- network.density(PORT01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_SDM.entropy <- entropy(PORT01_SDMft) #entropy

PORT01_SDM.netMx <- cbind(PORT01_SDM.netMx, PORT01_SDM.clusterCoef, PORT01_SDM.degreeCent$centralization,
                          PORT01_SDM.netDensity, PORT01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
PORT01_TDMg2 <- data.frame(PORT01_TDM)
PORT01_TDMg2 <- PORT01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_TDMg2$player1
player2vector <- PORT01_TDMg2$player2
PORT01_TDMg3 <- PORT01_TDMg2
PORT01_TDMg3$p1inp2vec <- is.element(PORT01_TDMg3$player1, player2vector)
PORT01_TDMg3$p2inp1vec <- is.element(PORT01_TDMg3$player2, player1vector)

addPlayer1 <- PORT01_TDMg3[ which(PORT01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_TDMg3[ which(PORT01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_TDMg2 <- rbind(PORT01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
PORT01_TDMft <- ftable(PORT01_TDMg2$player1, PORT01_TDMg2$player2)
PORT01_TDMft2 <- as.matrix(PORT01_TDMft)
numRows <- nrow(PORT01_TDMft2)
numCols <- ncol(PORT01_TDMft2)
PORT01_TDMft3 <- PORT01_TDMft2[c(2:numRows) , c(2:numCols)]
PORT01_TDMTable <- graph.adjacency(PORT01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(PORT01_TDMTable, vertex.label = V(PORT01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
PORT01_TDM.clusterCoef <- transitivity(PORT01_TDMTable, type="global") #cluster coefficient
PORT01_TDM.degreeCent <- centralization.degree(PORT01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_TDMftn <- as.network.matrix(PORT01_TDMft)
PORT01_TDM.netDensity <- network.density(PORT01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_TDM.entropy <- entropy(PORT01_TDMft) #entropy

PORT01_TDM.netMx <- cbind(PORT01_TDM.netMx, PORT01_TDM.clusterCoef, PORT01_TDM.degreeCent$centralization,
                          PORT01_TDM.netDensity, PORT01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
PORT01_SDg2 <- data.frame(PORT01_SD)
PORT01_SDg2 <- PORT01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_SDg2$player1
player2vector <- PORT01_SDg2$player2
PORT01_SDg3 <- PORT01_SDg2
PORT01_SDg3$p1inp2vec <- is.element(PORT01_SDg3$player1, player2vector)
PORT01_SDg3$p2inp1vec <- is.element(PORT01_SDg3$player2, player1vector)

addPlayer1 <- PORT01_SDg3[ which(PORT01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_SDg3[ which(PORT01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_SDg2 <- rbind(PORT01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
PORT01_SDft <- ftable(PORT01_SDg2$player1, PORT01_SDg2$player2)
PORT01_SDft2 <- as.matrix(PORT01_SDft)
numRows <- nrow(PORT01_SDft2)
numCols <- ncol(PORT01_SDft2)
PORT01_SDft3 <- PORT01_SDft2[c(2:numRows) , c(2:numCols)]
PORT01_SDTable <- graph.adjacency(PORT01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(PORT01_SDTable, vertex.label = V(PORT01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
PORT01_SD.clusterCoef <- transitivity(PORT01_SDTable, type="global") #cluster coefficient
PORT01_SD.degreeCent <- centralization.degree(PORT01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_SDftn <- as.network.matrix(PORT01_SDft)
PORT01_SD.netDensity <- network.density(PORT01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_SD.entropy <- entropy(PORT01_SDft) #entropy

PORT01_SD.netMx <- cbind(PORT01_SD.netMx, PORT01_SD.clusterCoef, PORT01_SD.degreeCent$centralization,
                         PORT01_SD.netDensity, PORT01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
PORT01_TDg2 <- data.frame(PORT01_TD)
PORT01_TDg2 <- PORT01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_TDg2$player1
player2vector <- PORT01_TDg2$player2
PORT01_TDg3 <- PORT01_TDg2
PORT01_TDg3$p1inp2vec <- is.element(PORT01_TDg3$player1, player2vector)
PORT01_TDg3$p2inp1vec <- is.element(PORT01_TDg3$player2, player1vector)

addPlayer1 <- PORT01_TDg3[ which(PORT01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_TDg3[ which(PORT01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_TDg2 <- rbind(PORT01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
PORT01_TDft <- ftable(PORT01_TDg2$player1, PORT01_TDg2$player2)
PORT01_TDft2 <- as.matrix(PORT01_TDft)
numRows <- nrow(PORT01_TDft2)
numCols <- ncol(PORT01_TDft2)
PORT01_TDft3 <- PORT01_TDft2[c(2:numRows) , c(2:numCols)]
PORT01_TDTable <- graph.adjacency(PORT01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(PORT01_TDTable, vertex.label = V(PORT01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
PORT01_TD.clusterCoef <- transitivity(PORT01_TDTable, type="global") #cluster coefficient
PORT01_TD.degreeCent <- centralization.degree(PORT01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_TDftn <- as.network.matrix(PORT01_TDft)
PORT01_TD.netDensity <- network.density(PORT01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_TD.entropy <- entropy(PORT01_TDft) #entropy

PORT01_TD.netMx <- cbind(PORT01_TD.netMx, PORT01_TD.clusterCoef, PORT01_TD.degreeCent$centralization,
                         PORT01_TD.netDensity, PORT01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
PORT01_QTg2 <- data.frame(PORT01_QT)
PORT01_QTg2 <- PORT01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT01_QTg2$player1
player2vector <- PORT01_QTg2$player2
PORT01_QTg3 <- PORT01_QTg2
PORT01_QTg3$p1inp2vec <- is.element(PORT01_QTg3$player1, player2vector)
PORT01_QTg3$p2inp1vec <- is.element(PORT01_QTg3$player2, player1vector)

addPlayer1 <- PORT01_QTg3[ which(PORT01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT01_QTg3[ which(PORT01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT01_QTg2 <- rbind(PORT01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
PORT01_QTft <- ftable(PORT01_QTg2$player1, PORT01_QTg2$player2)
PORT01_QTft2 <- as.matrix(PORT01_QTft)
numRows <- nrow(PORT01_QTft2)
numCols <- ncol(PORT01_QTft2)
PORT01_QTft3 <- PORT01_QTft2[c(2:numRows) , c(2:numCols)]
PORT01_QTTable <- graph.adjacency(PORT01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(PORT01_QTTable, vertex.label = V(PORT01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
PORT01_QT.clusterCoef <- transitivity(PORT01_QTTable, type="global") #cluster coefficient
PORT01_QT.degreeCent <- centralization.degree(PORT01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT01_QTftn <- as.network.matrix(PORT01_QTft)
PORT01_QT.netDensity <- network.density(PORT01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT01_QT.entropy <- entropy(PORT01_QTft) #entropy

PORT01_QT.netMx <- cbind(PORT01_QT.netMx, PORT01_QT.clusterCoef, PORT01_QT.degreeCent$centralization,
                         PORT01_QT.netDensity, PORT01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT01_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "RICH"
KIoutcome = "Goal_F"
RICH01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
RICH01_Gg2 <- data.frame(RICH01_G)
RICH01_Gg2 <- RICH01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_Gg2$player1
player2vector <- RICH01_Gg2$player2
RICH01_Gg3 <- RICH01_Gg2
RICH01_Gg3$p1inp2vec <- is.element(RICH01_Gg3$player1, player2vector)
RICH01_Gg3$p2inp1vec <- is.element(RICH01_Gg3$player2, player1vector)

addPlayer1 <- RICH01_Gg3[ which(RICH01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_Gg3[ which(RICH01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_Gg2 <- rbind(RICH01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
RICH01_Gft <- ftable(RICH01_Gg2$player1, RICH01_Gg2$player2)
RICH01_Gft2 <- as.matrix(RICH01_Gft)
numRows <- nrow(RICH01_Gft2)
numCols <- ncol(RICH01_Gft2)
RICH01_Gft3 <- RICH01_Gft2[c(2:numRows) , c(2:numCols)]
RICH01_GTable <- graph.adjacency(RICH01_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(RICH01_GTable, vertex.label = V(RICH01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
RICH01_G.clusterCoef <- transitivity(RICH01_GTable, type="global") #cluster coefficient
RICH01_G.degreeCent <- centralization.degree(RICH01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_Gftn <- as.network.matrix(RICH01_Gft)
RICH01_G.netDensity <- network.density(RICH01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_G.entropy <- entropy(RICH01_Gft) #entropy

RICH01_G.netMx <- cbind(RICH01_G.netMx, RICH01_G.clusterCoef, RICH01_G.degreeCent$centralization,
                        RICH01_G.netDensity, RICH01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "RICH"
KIoutcome = "Behind_F"
RICH01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
RICH01_Bg2 <- data.frame(RICH01_B)
RICH01_Bg2 <- RICH01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_Bg2$player1
player2vector <- RICH01_Bg2$player2
RICH01_Bg3 <- RICH01_Bg2
RICH01_Bg3$p1inp2vec <- is.element(RICH01_Bg3$player1, player2vector)
RICH01_Bg3$p2inp1vec <- is.element(RICH01_Bg3$player2, player1vector)

addPlayer1 <- RICH01_Bg3[ which(RICH01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_Bg3[ which(RICH01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_Bg2 <- rbind(RICH01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
RICH01_Bft <- ftable(RICH01_Bg2$player1, RICH01_Bg2$player2)
RICH01_Bft2 <- as.matrix(RICH01_Bft)
numRows <- nrow(RICH01_Bft2)
numCols <- ncol(RICH01_Bft2)
RICH01_Bft3 <- RICH01_Bft2[c(2:numRows) , c(2:numCols)]
RICH01_BTable <- graph.adjacency(RICH01_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(RICH01_BTable, vertex.label = V(RICH01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
RICH01_B.clusterCoef <- transitivity(RICH01_BTable, type="global") #cluster coefficient
RICH01_B.degreeCent <- centralization.degree(RICH01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_Bftn <- as.network.matrix(RICH01_Bft)
RICH01_B.netDensity <- network.density(RICH01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_B.entropy <- entropy(RICH01_Bft) #entropy

RICH01_B.netMx <- cbind(RICH01_B.netMx, RICH01_B.clusterCoef, RICH01_B.degreeCent$centralization,
                        RICH01_B.netDensity, RICH01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
RICH01_SFg2 <- data.frame(RICH01_SF)
RICH01_SFg2 <- RICH01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_SFg2$player1
player2vector <- RICH01_SFg2$player2
RICH01_SFg3 <- RICH01_SFg2
RICH01_SFg3$p1inp2vec <- is.element(RICH01_SFg3$player1, player2vector)
RICH01_SFg3$p2inp1vec <- is.element(RICH01_SFg3$player2, player1vector)

addPlayer1 <- RICH01_SFg3[ which(RICH01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_SFg3[ which(RICH01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_SFg2 <- rbind(RICH01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
RICH01_SFft <- ftable(RICH01_SFg2$player1, RICH01_SFg2$player2)
RICH01_SFft2 <- as.matrix(RICH01_SFft)
numRows <- nrow(RICH01_SFft2)
numCols <- ncol(RICH01_SFft2)
RICH01_SFft3 <- RICH01_SFft2[c(2:numRows) , c(2:numCols)]
RICH01_SFTable <- graph.adjacency(RICH01_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(RICH01_SFTable, vertex.label = V(RICH01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
RICH01_SF.clusterCoef <- transitivity(RICH01_SFTable, type="global") #cluster coefficient
RICH01_SF.degreeCent <- centralization.degree(RICH01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_SFftn <- as.network.matrix(RICH01_SFft)
RICH01_SF.netDensity <- network.density(RICH01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_SF.entropy <- entropy(RICH01_SFft) #entropy

RICH01_SF.netMx <- cbind(RICH01_SF.netMx, RICH01_SF.clusterCoef, RICH01_SF.degreeCent$centralization,
                         RICH01_SF.netDensity, RICH01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
RICH01_TFg2 <- data.frame(RICH01_TF)
RICH01_TFg2 <- RICH01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_TFg2$player1
player2vector <- RICH01_TFg2$player2
RICH01_TFg3 <- RICH01_TFg2
RICH01_TFg3$p1inp2vec <- is.element(RICH01_TFg3$player1, player2vector)
RICH01_TFg3$p2inp1vec <- is.element(RICH01_TFg3$player2, player1vector)

addPlayer1 <- RICH01_TFg3[ which(RICH01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_TFg3[ which(RICH01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_TFg2 <- rbind(RICH01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
RICH01_TFft <- ftable(RICH01_TFg2$player1, RICH01_TFg2$player2)
RICH01_TFft2 <- as.matrix(RICH01_TFft)
numRows <- nrow(RICH01_TFft2)
numCols <- ncol(RICH01_TFft2)
RICH01_TFft3 <- RICH01_TFft2[c(2:numRows) , c(2:numCols)]
RICH01_TFTable <- graph.adjacency(RICH01_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(RICH01_TFTable, vertex.label = V(RICH01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
RICH01_TF.clusterCoef <- transitivity(RICH01_TFTable, type="global") #cluster coefficient
RICH01_TF.degreeCent <- centralization.degree(RICH01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_TFftn <- as.network.matrix(RICH01_TFft)
RICH01_TF.netDensity <- network.density(RICH01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_TF.entropy <- entropy(RICH01_TFft) #entropy

RICH01_TF.netMx <- cbind(RICH01_TF.netMx, RICH01_TF.clusterCoef, RICH01_TF.degreeCent$centralization,
                         RICH01_TF.netDensity, RICH01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************

round = 1
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
RICH01_SAMg2 <- data.frame(RICH01_SAM)
RICH01_SAMg2 <- RICH01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_SAMg2$player1
player2vector <- RICH01_SAMg2$player2
RICH01_SAMg3 <- RICH01_SAMg2
RICH01_SAMg3$p1inp2vec <- is.element(RICH01_SAMg3$player1, player2vector)
RICH01_SAMg3$p2inp1vec <- is.element(RICH01_SAMg3$player2, player1vector)

addPlayer1 <- RICH01_SAMg3[ which(RICH01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_SAMg3[ which(RICH01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_SAMg2 <- rbind(RICH01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
RICH01_SAMft <- ftable(RICH01_SAMg2$player1, RICH01_SAMg2$player2)
RICH01_SAMft2 <- as.matrix(RICH01_SAMft)
numRows <- nrow(RICH01_SAMft2)
numCols <- ncol(RICH01_SAMft2)
RICH01_SAMft3 <- RICH01_SAMft2[c(2:numRows) , c(2:numCols)]
RICH01_SAMTable <- graph.adjacency(RICH01_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(RICH01_SAMTable, vertex.label = V(RICH01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
RICH01_SAM.clusterCoef <- transitivity(RICH01_SAMTable, type="global") #cluster coefficient
RICH01_SAM.degreeCent <- centralization.degree(RICH01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_SAMftn <- as.network.matrix(RICH01_SAMft)
RICH01_SAM.netDensity <- network.density(RICH01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_SAM.entropy <- entropy(RICH01_SAMft) #entropy

RICH01_SAM.netMx <- cbind(RICH01_SAM.netMx, RICH01_SAM.clusterCoef, RICH01_SAM.degreeCent$centralization,
                          RICH01_SAM.netDensity, RICH01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
RICH01_TAMg2 <- data.frame(RICH01_TAM)
RICH01_TAMg2 <- RICH01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_TAMg2$player1
player2vector <- RICH01_TAMg2$player2
RICH01_TAMg3 <- RICH01_TAMg2
RICH01_TAMg3$p1inp2vec <- is.element(RICH01_TAMg3$player1, player2vector)
RICH01_TAMg3$p2inp1vec <- is.element(RICH01_TAMg3$player2, player1vector)

addPlayer1 <- RICH01_TAMg3[ which(RICH01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_TAMg3[ which(RICH01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_TAMg2 <- rbind(RICH01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
RICH01_TAMft <- ftable(RICH01_TAMg2$player1, RICH01_TAMg2$player2)
RICH01_TAMft2 <- as.matrix(RICH01_TAMft)
numRows <- nrow(RICH01_TAMft2)
numCols <- ncol(RICH01_TAMft2)
RICH01_TAMft3 <- RICH01_TAMft2[c(2:numRows) , c(2:numCols)]
RICH01_TAMTable <- graph.adjacency(RICH01_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(RICH01_TAMTable, vertex.label = V(RICH01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
RICH01_TAM.clusterCoef <- transitivity(RICH01_TAMTable, type="global") #cluster coefficient
RICH01_TAM.degreeCent <- centralization.degree(RICH01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_TAMftn <- as.network.matrix(RICH01_TAMft)
RICH01_TAM.netDensity <- network.density(RICH01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_TAM.entropy <- entropy(RICH01_TAMft) #entropy

RICH01_TAM.netMx <- cbind(RICH01_TAM.netMx, RICH01_TAM.clusterCoef, RICH01_TAM.degreeCent$centralization,
                          RICH01_TAM.netDensity, RICH01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
RICH01_SDMg2 <- data.frame(RICH01_SDM)
RICH01_SDMg2 <- RICH01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_SDMg2$player1
player2vector <- RICH01_SDMg2$player2
RICH01_SDMg3 <- RICH01_SDMg2
RICH01_SDMg3$p1inp2vec <- is.element(RICH01_SDMg3$player1, player2vector)
RICH01_SDMg3$p2inp1vec <- is.element(RICH01_SDMg3$player2, player1vector)

addPlayer1 <- RICH01_SDMg3[ which(RICH01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_SDMg3[ which(RICH01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_SDMg2 <- rbind(RICH01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
RICH01_SDMft <- ftable(RICH01_SDMg2$player1, RICH01_SDMg2$player2)
RICH01_SDMft2 <- as.matrix(RICH01_SDMft)
numRows <- nrow(RICH01_SDMft2)
numCols <- ncol(RICH01_SDMft2)
RICH01_SDMft3 <- RICH01_SDMft2[c(2:numRows) , c(2:numCols)]
RICH01_SDMTable <- graph.adjacency(RICH01_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(RICH01_SDMTable, vertex.label = V(RICH01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
RICH01_SDM.clusterCoef <- transitivity(RICH01_SDMTable, type="global") #cluster coefficient
RICH01_SDM.degreeCent <- centralization.degree(RICH01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_SDMftn <- as.network.matrix(RICH01_SDMft)
RICH01_SDM.netDensity <- network.density(RICH01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_SDM.entropy <- entropy(RICH01_SDMft) #entropy

RICH01_SDM.netMx <- cbind(RICH01_SDM.netMx, RICH01_SDM.clusterCoef, RICH01_SDM.degreeCent$centralization,
                          RICH01_SDM.netDensity, RICH01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
RICH01_TDMg2 <- data.frame(RICH01_TDM)
RICH01_TDMg2 <- RICH01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_TDMg2$player1
player2vector <- RICH01_TDMg2$player2
RICH01_TDMg3 <- RICH01_TDMg2
RICH01_TDMg3$p1inp2vec <- is.element(RICH01_TDMg3$player1, player2vector)
RICH01_TDMg3$p2inp1vec <- is.element(RICH01_TDMg3$player2, player1vector)

addPlayer1 <- RICH01_TDMg3[ which(RICH01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_TDMg3[ which(RICH01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_TDMg2 <- rbind(RICH01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
RICH01_TDMft <- ftable(RICH01_TDMg2$player1, RICH01_TDMg2$player2)
RICH01_TDMft2 <- as.matrix(RICH01_TDMft)
numRows <- nrow(RICH01_TDMft2)
numCols <- ncol(RICH01_TDMft2)
RICH01_TDMft3 <- RICH01_TDMft2[c(2:numRows) , c(2:numCols)]
RICH01_TDMTable <- graph.adjacency(RICH01_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(RICH01_TDMTable, vertex.label = V(RICH01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
RICH01_TDM.clusterCoef <- transitivity(RICH01_TDMTable, type="global") #cluster coefficient
RICH01_TDM.degreeCent <- centralization.degree(RICH01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_TDMftn <- as.network.matrix(RICH01_TDMft)
RICH01_TDM.netDensity <- network.density(RICH01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_TDM.entropy <- entropy(RICH01_TDMft) #entropy

RICH01_TDM.netMx <- cbind(RICH01_TDM.netMx, RICH01_TDM.clusterCoef, RICH01_TDM.degreeCent$centralization,
                          RICH01_TDM.netDensity, RICH01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
RICH01_SDg2 <- data.frame(RICH01_SD)
RICH01_SDg2 <- RICH01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_SDg2$player1
player2vector <- RICH01_SDg2$player2
RICH01_SDg3 <- RICH01_SDg2
RICH01_SDg3$p1inp2vec <- is.element(RICH01_SDg3$player1, player2vector)
RICH01_SDg3$p2inp1vec <- is.element(RICH01_SDg3$player2, player1vector)

addPlayer1 <- RICH01_SDg3[ which(RICH01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_SDg3[ which(RICH01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_SDg2 <- rbind(RICH01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
RICH01_SDft <- ftable(RICH01_SDg2$player1, RICH01_SDg2$player2)
RICH01_SDft2 <- as.matrix(RICH01_SDft)
numRows <- nrow(RICH01_SDft2)
numCols <- ncol(RICH01_SDft2)
RICH01_SDft3 <- RICH01_SDft2[c(2:numRows) , c(2:numCols)]
RICH01_SDTable <- graph.adjacency(RICH01_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(RICH01_SDTable, vertex.label = V(RICH01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
RICH01_SD.clusterCoef <- transitivity(RICH01_SDTable, type="global") #cluster coefficient
RICH01_SD.degreeCent <- centralization.degree(RICH01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_SDftn <- as.network.matrix(RICH01_SDft)
RICH01_SD.netDensity <- network.density(RICH01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_SD.entropy <- entropy(RICH01_SDft) #entropy

RICH01_SD.netMx <- cbind(RICH01_SD.netMx, RICH01_SD.clusterCoef, RICH01_SD.degreeCent$centralization,
                         RICH01_SD.netDensity, RICH01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
RICH01_TDg2 <- data.frame(RICH01_TD)
RICH01_TDg2 <- RICH01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_TDg2$player1
player2vector <- RICH01_TDg2$player2
RICH01_TDg3 <- RICH01_TDg2
RICH01_TDg3$p1inp2vec <- is.element(RICH01_TDg3$player1, player2vector)
RICH01_TDg3$p2inp1vec <- is.element(RICH01_TDg3$player2, player1vector)

addPlayer1 <- RICH01_TDg3[ which(RICH01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_TDg3[ which(RICH01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_TDg2 <- rbind(RICH01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
RICH01_TDft <- ftable(RICH01_TDg2$player1, RICH01_TDg2$player2)
RICH01_TDft2 <- as.matrix(RICH01_TDft)
numRows <- nrow(RICH01_TDft2)
numCols <- ncol(RICH01_TDft2)
RICH01_TDft3 <- RICH01_TDft2[c(2:numRows) , c(2:numCols)]
RICH01_TDTable <- graph.adjacency(RICH01_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(RICH01_TDTable, vertex.label = V(RICH01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
RICH01_TD.clusterCoef <- transitivity(RICH01_TDTable, type="global") #cluster coefficient
RICH01_TD.degreeCent <- centralization.degree(RICH01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_TDftn <- as.network.matrix(RICH01_TDft)
RICH01_TD.netDensity <- network.density(RICH01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_TD.entropy <- entropy(RICH01_TDft) #entropy

RICH01_TD.netMx <- cbind(RICH01_TD.netMx, RICH01_TD.clusterCoef, RICH01_TD.degreeCent$centralization,
                         RICH01_TD.netDensity, RICH01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
RICH01_QTg2 <- data.frame(RICH01_QT)
RICH01_QTg2 <- RICH01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH01_QTg2$player1
player2vector <- RICH01_QTg2$player2
RICH01_QTg3 <- RICH01_QTg2
RICH01_QTg3$p1inp2vec <- is.element(RICH01_QTg3$player1, player2vector)
RICH01_QTg3$p2inp1vec <- is.element(RICH01_QTg3$player2, player1vector)

addPlayer1 <- RICH01_QTg3[ which(RICH01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH01_QTg3[ which(RICH01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH01_QTg2 <- rbind(RICH01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
RICH01_QTft <- ftable(RICH01_QTg2$player1, RICH01_QTg2$player2)
RICH01_QTft2 <- as.matrix(RICH01_QTft)
numRows <- nrow(RICH01_QTft2)
numCols <- ncol(RICH01_QTft2)
RICH01_QTft3 <- RICH01_QTft2[c(2:numRows) , c(2:numCols)]
RICH01_QTTable <- graph.adjacency(RICH01_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(RICH01_QTTable, vertex.label = V(RICH01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
RICH01_QT.clusterCoef <- transitivity(RICH01_QTTable, type="global") #cluster coefficient
RICH01_QT.degreeCent <- centralization.degree(RICH01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH01_QTftn <- as.network.matrix(RICH01_QTft)
RICH01_QT.netDensity <- network.density(RICH01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH01_QT.entropy <- entropy(RICH01_QTft) #entropy

RICH01_QT.netMx <- cbind(RICH01_QT.netMx, RICH01_QT.clusterCoef, RICH01_QT.degreeCent$centralization,
                         RICH01_QT.netDensity, RICH01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH01_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "STK"
KIoutcome = "Goal_F"
STK01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
STK01_Gg2 <- data.frame(STK01_G)
STK01_Gg2 <- STK01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_Gg2$player1
player2vector <- STK01_Gg2$player2
STK01_Gg3 <- STK01_Gg2
STK01_Gg3$p1inp2vec <- is.element(STK01_Gg3$player1, player2vector)
STK01_Gg3$p2inp1vec <- is.element(STK01_Gg3$player2, player1vector)

addPlayer1 <- STK01_Gg3[ which(STK01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_Gg3[ which(STK01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_Gg2 <- rbind(STK01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
STK01_Gft <- ftable(STK01_Gg2$player1, STK01_Gg2$player2)
STK01_Gft2 <- as.matrix(STK01_Gft)
numRows <- nrow(STK01_Gft2)
numCols <- ncol(STK01_Gft2)
STK01_Gft3 <- STK01_Gft2[c(2:numRows) , c(2:numCols)]
STK01_GTable <- graph.adjacency(STK01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(STK01_GTable, vertex.label = V(STK01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
STK01_G.clusterCoef <- transitivity(STK01_GTable, type="global") #cluster coefficient
STK01_G.degreeCent <- centralization.degree(STK01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_Gftn <- as.network.matrix(STK01_Gft)
STK01_G.netDensity <- network.density(STK01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_G.entropy <- entropy(STK01_Gft) #entropy

STK01_G.netMx <- cbind(STK01_G.netMx, STK01_G.clusterCoef, STK01_G.degreeCent$centralization,
                       STK01_G.netDensity, STK01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "STK"
KIoutcome = "Behind_F"
STK01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
STK01_Bg2 <- data.frame(STK01_B)
STK01_Bg2 <- STK01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_Bg2$player1
player2vector <- STK01_Bg2$player2
STK01_Bg3 <- STK01_Bg2
STK01_Bg3$p1inp2vec <- is.element(STK01_Bg3$player1, player2vector)
STK01_Bg3$p2inp1vec <- is.element(STK01_Bg3$player2, player1vector)

addPlayer1 <- STK01_Bg3[ which(STK01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_Bg3[ which(STK01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_Bg2 <- rbind(STK01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
STK01_Bft <- ftable(STK01_Bg2$player1, STK01_Bg2$player2)
STK01_Bft2 <- as.matrix(STK01_Bft)
numRows <- nrow(STK01_Bft2)
numCols <- ncol(STK01_Bft2)
STK01_Bft3 <- STK01_Bft2[c(2:numRows) , c(2:numCols)]
STK01_BTable <- graph.adjacency(STK01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(STK01_BTable, vertex.label = V(STK01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
STK01_B.clusterCoef <- transitivity(STK01_BTable, type="global") #cluster coefficient
STK01_B.degreeCent <- centralization.degree(STK01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_Bftn <- as.network.matrix(STK01_Bft)
STK01_B.netDensity <- network.density(STK01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_B.entropy <- entropy(STK01_Bft) #entropy

STK01_B.netMx <- cbind(STK01_B.netMx, STK01_B.clusterCoef, STK01_B.degreeCent$centralization,
                       STK01_B.netDensity, STK01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************

round = 1
teamName = "STK"
KIoutcome = "Stoppage_F"
STK01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
STK01_SFg2 <- data.frame(STK01_SF)
STK01_SFg2 <- STK01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_SFg2$player1
player2vector <- STK01_SFg2$player2
STK01_SFg3 <- STK01_SFg2
STK01_SFg3$p1inp2vec <- is.element(STK01_SFg3$player1, player2vector)
STK01_SFg3$p2inp1vec <- is.element(STK01_SFg3$player2, player1vector)

addPlayer1 <- STK01_SFg3[ which(STK01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_SFg3[ which(STK01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_SFg2 <- rbind(STK01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
STK01_SFft <- ftable(STK01_SFg2$player1, STK01_SFg2$player2)
STK01_SFft2 <- as.matrix(STK01_SFft)
numRows <- nrow(STK01_SFft2)
numCols <- ncol(STK01_SFft2)
STK01_SFft3 <- STK01_SFft2[c(2:numRows) , c(2:numCols)]
STK01_SFTable <- graph.adjacency(STK01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(STK01_SFTable, vertex.label = V(STK01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
STK01_SF.clusterCoef <- transitivity(STK01_SFTable, type="global") #cluster coefficient
STK01_SF.degreeCent <- centralization.degree(STK01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_SFftn <- as.network.matrix(STK01_SFft)
STK01_SF.netDensity <- network.density(STK01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_SF.entropy <- entropy(STK01_SFft) #entropy

STK01_SF.netMx <- cbind(STK01_SF.netMx, STK01_SF.clusterCoef, STK01_SF.degreeCent$centralization,
                        STK01_SF.netDensity, STK01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "STK"
KIoutcome = "Turnover_F"
STK01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
STK01_TFg2 <- data.frame(STK01_TF)
STK01_TFg2 <- STK01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_TFg2$player1
player2vector <- STK01_TFg2$player2
STK01_TFg3 <- STK01_TFg2
STK01_TFg3$p1inp2vec <- is.element(STK01_TFg3$player1, player2vector)
STK01_TFg3$p2inp1vec <- is.element(STK01_TFg3$player2, player1vector)

addPlayer1 <- STK01_TFg3[ which(STK01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_TFg3[ which(STK01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_TFg2 <- rbind(STK01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
STK01_TFft <- ftable(STK01_TFg2$player1, STK01_TFg2$player2)
STK01_TFft2 <- as.matrix(STK01_TFft)
numRows <- nrow(STK01_TFft2)
numCols <- ncol(STK01_TFft2)
STK01_TFft3 <- STK01_TFft2[c(2:numRows) , c(2:numCols)]
STK01_TFTable <- graph.adjacency(STK01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(STK01_TFTable, vertex.label = V(STK01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
STK01_TF.clusterCoef <- transitivity(STK01_TFTable, type="global") #cluster coefficient
STK01_TF.degreeCent <- centralization.degree(STK01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_TFftn <- as.network.matrix(STK01_TFft)
STK01_TF.netDensity <- network.density(STK01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_TF.entropy <- entropy(STK01_TFft) #entropy

STK01_TF.netMx <- cbind(STK01_TF.netMx, STK01_TF.clusterCoef, STK01_TF.degreeCent$centralization,
                        STK01_TF.netDensity, STK01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************

round = 1
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
STK01_SAMg2 <- data.frame(STK01_SAM)
STK01_SAMg2 <- STK01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_SAMg2$player1
player2vector <- STK01_SAMg2$player2
STK01_SAMg3 <- STK01_SAMg2
STK01_SAMg3$p1inp2vec <- is.element(STK01_SAMg3$player1, player2vector)
STK01_SAMg3$p2inp1vec <- is.element(STK01_SAMg3$player2, player1vector)

addPlayer1 <- STK01_SAMg3[ which(STK01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_SAMg3[ which(STK01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_SAMg2 <- rbind(STK01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
STK01_SAMft <- ftable(STK01_SAMg2$player1, STK01_SAMg2$player2)
STK01_SAMft2 <- as.matrix(STK01_SAMft)
numRows <- nrow(STK01_SAMft2)
numCols <- ncol(STK01_SAMft2)
STK01_SAMft3 <- STK01_SAMft2[c(2:numRows) , c(2:numCols)]
STK01_SAMTable <- graph.adjacency(STK01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(STK01_SAMTable, vertex.label = V(STK01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
STK01_SAM.clusterCoef <- transitivity(STK01_SAMTable, type="global") #cluster coefficient
STK01_SAM.degreeCent <- centralization.degree(STK01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_SAMftn <- as.network.matrix(STK01_SAMft)
STK01_SAM.netDensity <- network.density(STK01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_SAM.entropy <- entropy(STK01_SAMft) #entropy

STK01_SAM.netMx <- cbind(STK01_SAM.netMx, STK01_SAM.clusterCoef, STK01_SAM.degreeCent$centralization,
                         STK01_SAM.netDensity, STK01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "STK"
KIoutcome = "Turnover_AM"
STK01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
STK01_TAMg2 <- data.frame(STK01_TAM)
STK01_TAMg2 <- STK01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_TAMg2$player1
player2vector <- STK01_TAMg2$player2
STK01_TAMg3 <- STK01_TAMg2
STK01_TAMg3$p1inp2vec <- is.element(STK01_TAMg3$player1, player2vector)
STK01_TAMg3$p2inp1vec <- is.element(STK01_TAMg3$player2, player1vector)

addPlayer1 <- STK01_TAMg3[ which(STK01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_TAMg3[ which(STK01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_TAMg2 <- rbind(STK01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
STK01_TAMft <- ftable(STK01_TAMg2$player1, STK01_TAMg2$player2)
STK01_TAMft2 <- as.matrix(STK01_TAMft)
numRows <- nrow(STK01_TAMft2)
numCols <- ncol(STK01_TAMft2)
STK01_TAMft3 <- STK01_TAMft2[c(1:numRows) , c(1:numCols)]
STK01_TAMTable <- graph.adjacency(STK01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(STK01_TAMTable, vertex.label = V(STK01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
STK01_TAM.clusterCoef <- transitivity(STK01_TAMTable, type="global") #cluster coefficient
STK01_TAM.degreeCent <- centralization.degree(STK01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_TAMftn <- as.network.matrix(STK01_TAMft)
STK01_TAM.netDensity <- network.density(STK01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_TAM.entropy <- entropy(STK01_TAMft) #entropy

STK01_TAM.netMx <- cbind(STK01_TAM.netMx, STK01_TAM.clusterCoef, STK01_TAM.degreeCent$centralization,
                         STK01_TAM.netDensity, STK01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
STK01_SDMg2 <- data.frame(STK01_SDM)
STK01_SDMg2 <- STK01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_SDMg2$player1
player2vector <- STK01_SDMg2$player2
STK01_SDMg3 <- STK01_SDMg2
STK01_SDMg3$p1inp2vec <- is.element(STK01_SDMg3$player1, player2vector)
STK01_SDMg3$p2inp1vec <- is.element(STK01_SDMg3$player2, player1vector)

addPlayer1 <- STK01_SDMg3[ which(STK01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_SDMg3[ which(STK01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_SDMg2 <- rbind(STK01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
STK01_SDMft <- ftable(STK01_SDMg2$player1, STK01_SDMg2$player2)
STK01_SDMft2 <- as.matrix(STK01_SDMft)
numRows <- nrow(STK01_SDMft2)
numCols <- ncol(STK01_SDMft2)
STK01_SDMft3 <- STK01_SDMft2[c(2:numRows) , c(2:numCols)]
STK01_SDMTable <- graph.adjacency(STK01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(STK01_SDMTable, vertex.label = V(STK01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
STK01_SDM.clusterCoef <- transitivity(STK01_SDMTable, type="global") #cluster coefficient
STK01_SDM.degreeCent <- centralization.degree(STK01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_SDMftn <- as.network.matrix(STK01_SDMft)
STK01_SDM.netDensity <- network.density(STK01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_SDM.entropy <- entropy(STK01_SDMft) #entropy

STK01_SDM.netMx <- cbind(STK01_SDM.netMx, STK01_SDM.clusterCoef, STK01_SDM.degreeCent$centralization,
                         STK01_SDM.netDensity, STK01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "STK"
KIoutcome = "Turnover_DM"
STK01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
STK01_TDMg2 <- data.frame(STK01_TDM)
STK01_TDMg2 <- STK01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_TDMg2$player1
player2vector <- STK01_TDMg2$player2
STK01_TDMg3 <- STK01_TDMg2
STK01_TDMg3$p1inp2vec <- is.element(STK01_TDMg3$player1, player2vector)
STK01_TDMg3$p2inp1vec <- is.element(STK01_TDMg3$player2, player1vector)

addPlayer1 <- STK01_TDMg3[ which(STK01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_TDMg3[ which(STK01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_TDMg2 <- rbind(STK01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
STK01_TDMft <- ftable(STK01_TDMg2$player1, STK01_TDMg2$player2)
STK01_TDMft2 <- as.matrix(STK01_TDMft)
numRows <- nrow(STK01_TDMft2)
numCols <- ncol(STK01_TDMft2)
STK01_TDMft3 <- STK01_TDMft2[c(2:numRows) , c(2:numCols)]
STK01_TDMTable <- graph.adjacency(STK01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(STK01_TDMTable, vertex.label = V(STK01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
STK01_TDM.clusterCoef <- transitivity(STK01_TDMTable, type="global") #cluster coefficient
STK01_TDM.degreeCent <- centralization.degree(STK01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_TDMftn <- as.network.matrix(STK01_TDMft)
STK01_TDM.netDensity <- network.density(STK01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_TDM.entropy <- entropy(STK01_TDMft) #entropy

STK01_TDM.netMx <- cbind(STK01_TDM.netMx, STK01_TDM.clusterCoef, STK01_TDM.degreeCent$centralization,
                         STK01_TDM.netDensity, STK01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "STK"
KIoutcome = "Stoppage_D"
STK01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
STK01_SDg2 <- data.frame(STK01_SD)
STK01_SDg2 <- STK01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_SDg2$player1
player2vector <- STK01_SDg2$player2
STK01_SDg3 <- STK01_SDg2
STK01_SDg3$p1inp2vec <- is.element(STK01_SDg3$player1, player2vector)
STK01_SDg3$p2inp1vec <- is.element(STK01_SDg3$player2, player1vector)

addPlayer1 <- STK01_SDg3[ which(STK01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_SDg3[ which(STK01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_SDg2 <- rbind(STK01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
STK01_SDft <- ftable(STK01_SDg2$player1, STK01_SDg2$player2)
STK01_SDft2 <- as.matrix(STK01_SDft)
numRows <- nrow(STK01_SDft2)
numCols <- ncol(STK01_SDft2)
STK01_SDft3 <- STK01_SDft2[c(2:numRows) , c(2:numCols)]
STK01_SDTable <- graph.adjacency(STK01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(STK01_SDTable, vertex.label = V(STK01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
STK01_SD.clusterCoef <- transitivity(STK01_SDTable, type="global") #cluster coefficient
STK01_SD.degreeCent <- centralization.degree(STK01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_SDftn <- as.network.matrix(STK01_SDft)
STK01_SD.netDensity <- network.density(STK01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_SD.entropy <- entropy(STK01_SDft) #entropy

STK01_SD.netMx <- cbind(STK01_SD.netMx, STK01_SD.clusterCoef, STK01_SD.degreeCent$centralization,
                        STK01_SD.netDensity, STK01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************

round = 1
teamName = "STK"
KIoutcome = "Turnover_D"
STK01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
STK01_TDg2 <- data.frame(STK01_TD)
STK01_TDg2 <- STK01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_TDg2$player1
player2vector <- STK01_TDg2$player2
STK01_TDg3 <- STK01_TDg2
STK01_TDg3$p1inp2vec <- is.element(STK01_TDg3$player1, player2vector)
STK01_TDg3$p2inp1vec <- is.element(STK01_TDg3$player2, player1vector)

addPlayer1 <- STK01_TDg3[ which(STK01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_TDg3[ which(STK01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_TDg2 <- rbind(STK01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
STK01_TDft <- ftable(STK01_TDg2$player1, STK01_TDg2$player2)
STK01_TDft2 <- as.matrix(STK01_TDft)
numRows <- nrow(STK01_TDft2)
numCols <- ncol(STK01_TDft2)
STK01_TDft3 <- STK01_TDft2[c(2:numRows) , c(2:numCols)]
STK01_TDTable <- graph.adjacency(STK01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(STK01_TDTable, vertex.label = V(STK01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
STK01_TD.clusterCoef <- transitivity(STK01_TDTable, type="global") #cluster coefficient
STK01_TD.degreeCent <- centralization.degree(STK01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_TDftn <- as.network.matrix(STK01_TDft)
STK01_TD.netDensity <- network.density(STK01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_TD.entropy <- entropy(STK01_TDft) #entropy

STK01_TD.netMx <- cbind(STK01_TD.netMx, STK01_TD.clusterCoef, STK01_TD.degreeCent$centralization,
                        STK01_TD.netDensity, STK01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
STK01_QTg2 <- data.frame(STK01_QT)
STK01_QTg2 <- STK01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK01_QTg2$player1
player2vector <- STK01_QTg2$player2
STK01_QTg3 <- STK01_QTg2
STK01_QTg3$p1inp2vec <- is.element(STK01_QTg3$player1, player2vector)
STK01_QTg3$p2inp1vec <- is.element(STK01_QTg3$player2, player1vector)

addPlayer1 <- STK01_QTg3[ which(STK01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK01_QTg3[ which(STK01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK01_QTg2 <- rbind(STK01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
STK01_QTft <- ftable(STK01_QTg2$player1, STK01_QTg2$player2)
STK01_QTft2 <- as.matrix(STK01_QTft)
numRows <- nrow(STK01_QTft2)
numCols <- ncol(STK01_QTft2)
STK01_QTft3 <- STK01_QTft2[c(2:numRows) , c(2:numCols)]
STK01_QTTable <- graph.adjacency(STK01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(STK01_QTTable, vertex.label = V(STK01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
STK01_QT.clusterCoef <- transitivity(STK01_QTTable, type="global") #cluster coefficient
STK01_QT.degreeCent <- centralization.degree(STK01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK01_QTftn <- as.network.matrix(STK01_QTft)
STK01_QT.netDensity <- network.density(STK01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK01_QT.entropy <- entropy(STK01_QTft) #entropy

STK01_QT.netMx <- cbind(STK01_QT.netMx, STK01_QT.clusterCoef, STK01_QT.degreeCent$centralization,
                        STK01_QT.netDensity, STK01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK01_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Goal_F"
SYD01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
SYD01_Gg2 <- data.frame(SYD01_G)
SYD01_Gg2 <- SYD01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_Gg2$player1
player2vector <- SYD01_Gg2$player2
SYD01_Gg3 <- SYD01_Gg2
SYD01_Gg3$p1inp2vec <- is.element(SYD01_Gg3$player1, player2vector)
SYD01_Gg3$p2inp1vec <- is.element(SYD01_Gg3$player2, player1vector)

addPlayer1 <- SYD01_Gg3[ which(SYD01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_Gg3[ which(SYD01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_Gg2 <- rbind(SYD01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
SYD01_Gft <- ftable(SYD01_Gg2$player1, SYD01_Gg2$player2)
SYD01_Gft2 <- as.matrix(SYD01_Gft)
numRows <- nrow(SYD01_Gft2)
numCols <- ncol(SYD01_Gft2)
SYD01_Gft3 <- SYD01_Gft2[c(2:numRows) , c(2:numCols)]
SYD01_GTable <- graph.adjacency(SYD01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(SYD01_GTable, vertex.label = V(SYD01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
SYD01_G.clusterCoef <- transitivity(SYD01_GTable, type="global") #cluster coefficient
SYD01_G.degreeCent <- centralization.degree(SYD01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_Gftn <- as.network.matrix(SYD01_Gft)
SYD01_G.netDensity <- network.density(SYD01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_G.entropy <- entropy(SYD01_Gft) #entropy

SYD01_G.netMx <- cbind(SYD01_G.netMx, SYD01_G.clusterCoef, SYD01_G.degreeCent$centralization,
                       SYD01_G.netDensity, SYD01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Behind_F"
SYD01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
SYD01_Bg2 <- data.frame(SYD01_B)
SYD01_Bg2 <- SYD01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_Bg2$player1
player2vector <- SYD01_Bg2$player2
SYD01_Bg3 <- SYD01_Bg2
SYD01_Bg3$p1inp2vec <- is.element(SYD01_Bg3$player1, player2vector)
SYD01_Bg3$p2inp1vec <- is.element(SYD01_Bg3$player2, player1vector)

addPlayer1 <- SYD01_Bg3[ which(SYD01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_Bg3[ which(SYD01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_Bg2 <- rbind(SYD01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
SYD01_Bft <- ftable(SYD01_Bg2$player1, SYD01_Bg2$player2)
SYD01_Bft2 <- as.matrix(SYD01_Bft)
numRows <- nrow(SYD01_Bft2)
numCols <- ncol(SYD01_Bft2)
SYD01_Bft3 <- SYD01_Bft2[c(2:numRows) , c(2:numCols)]
SYD01_BTable <- graph.adjacency(SYD01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(SYD01_BTable, vertex.label = V(SYD01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
SYD01_B.clusterCoef <- transitivity(SYD01_BTable, type="global") #cluster coefficient
SYD01_B.degreeCent <- centralization.degree(SYD01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_Bftn <- as.network.matrix(SYD01_Bft)
SYD01_B.netDensity <- network.density(SYD01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_B.entropy <- entropy(SYD01_Bft) #entropy

SYD01_B.netMx <- cbind(SYD01_B.netMx, SYD01_B.clusterCoef, SYD01_B.degreeCent$centralization,
                       SYD01_B.netDensity, SYD01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************

round = 1
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
SYD01_SFg2 <- data.frame(SYD01_SF)
SYD01_SFg2 <- SYD01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_SFg2$player1
player2vector <- SYD01_SFg2$player2
SYD01_SFg3 <- SYD01_SFg2
SYD01_SFg3$p1inp2vec <- is.element(SYD01_SFg3$player1, player2vector)
SYD01_SFg3$p2inp1vec <- is.element(SYD01_SFg3$player2, player1vector)

addPlayer1 <- SYD01_SFg3[ which(SYD01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_SFg3[ which(SYD01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_SFg2 <- rbind(SYD01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
SYD01_SFft <- ftable(SYD01_SFg2$player1, SYD01_SFg2$player2)
SYD01_SFft2 <- as.matrix(SYD01_SFft)
numRows <- nrow(SYD01_SFft2)
numCols <- ncol(SYD01_SFft2)
SYD01_SFft3 <- SYD01_SFft2[c(2:numRows) , c(2:numCols)]
SYD01_SFTable <- graph.adjacency(SYD01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(SYD01_SFTable, vertex.label = V(SYD01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
SYD01_SF.clusterCoef <- transitivity(SYD01_SFTable, type="global") #cluster coefficient
SYD01_SF.degreeCent <- centralization.degree(SYD01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_SFftn <- as.network.matrix(SYD01_SFft)
SYD01_SF.netDensity <- network.density(SYD01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_SF.entropy <- entropy(SYD01_SFft) #entropy

SYD01_SF.netMx <- cbind(SYD01_SF.netMx, SYD01_SF.clusterCoef, SYD01_SF.degreeCent$centralization,
                        SYD01_SF.netDensity, SYD01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
SYD01_TFg2 <- data.frame(SYD01_TF)
SYD01_TFg2 <- SYD01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_TFg2$player1
player2vector <- SYD01_TFg2$player2
SYD01_TFg3 <- SYD01_TFg2
SYD01_TFg3$p1inp2vec <- is.element(SYD01_TFg3$player1, player2vector)
SYD01_TFg3$p2inp1vec <- is.element(SYD01_TFg3$player2, player1vector)

addPlayer1 <- SYD01_TFg3[ which(SYD01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_TFg3[ which(SYD01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_TFg2 <- rbind(SYD01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
SYD01_TFft <- ftable(SYD01_TFg2$player1, SYD01_TFg2$player2)
SYD01_TFft2 <- as.matrix(SYD01_TFft)
numRows <- nrow(SYD01_TFft2)
numCols <- ncol(SYD01_TFft2)
SYD01_TFft3 <- SYD01_TFft2[c(2:numRows) , c(2:numCols)]
SYD01_TFTable <- graph.adjacency(SYD01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(SYD01_TFTable, vertex.label = V(SYD01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
SYD01_TF.clusterCoef <- transitivity(SYD01_TFTable, type="global") #cluster coefficient
SYD01_TF.degreeCent <- centralization.degree(SYD01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_TFftn <- as.network.matrix(SYD01_TFft)
SYD01_TF.netDensity <- network.density(SYD01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_TF.entropy <- entropy(SYD01_TFft) #entropy

SYD01_TF.netMx <- cbind(SYD01_TF.netMx, SYD01_TF.clusterCoef, SYD01_TF.degreeCent$centralization,
                        SYD01_TF.netDensity, SYD01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
SYD01_SAMg2 <- data.frame(SYD01_SAM)
SYD01_SAMg2 <- SYD01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_SAMg2$player1
player2vector <- SYD01_SAMg2$player2
SYD01_SAMg3 <- SYD01_SAMg2
SYD01_SAMg3$p1inp2vec <- is.element(SYD01_SAMg3$player1, player2vector)
SYD01_SAMg3$p2inp1vec <- is.element(SYD01_SAMg3$player2, player1vector)

addPlayer1 <- SYD01_SAMg3[ which(SYD01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_SAMg3[ which(SYD01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_SAMg2 <- rbind(SYD01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
SYD01_SAMft <- ftable(SYD01_SAMg2$player1, SYD01_SAMg2$player2)
SYD01_SAMft2 <- as.matrix(SYD01_SAMft)
numRows <- nrow(SYD01_SAMft2)
numCols <- ncol(SYD01_SAMft2)
SYD01_SAMft3 <- SYD01_SAMft2[c(2:numRows) , c(2:numCols)]
SYD01_SAMTable <- graph.adjacency(SYD01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(SYD01_SAMTable, vertex.label = V(SYD01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
SYD01_SAM.clusterCoef <- transitivity(SYD01_SAMTable, type="global") #cluster coefficient
SYD01_SAM.degreeCent <- centralization.degree(SYD01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_SAMftn <- as.network.matrix(SYD01_SAMft)
SYD01_SAM.netDensity <- network.density(SYD01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_SAM.entropy <- entropy(SYD01_SAMft) #entropy

SYD01_SAM.netMx <- cbind(SYD01_SAM.netMx, SYD01_SAM.clusterCoef, SYD01_SAM.degreeCent$centralization,
                         SYD01_SAM.netDensity, SYD01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
SYD01_TAMg2 <- data.frame(SYD01_TAM)
SYD01_TAMg2 <- SYD01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_TAMg2$player1
player2vector <- SYD01_TAMg2$player2
SYD01_TAMg3 <- SYD01_TAMg2
SYD01_TAMg3$p1inp2vec <- is.element(SYD01_TAMg3$player1, player2vector)
SYD01_TAMg3$p2inp1vec <- is.element(SYD01_TAMg3$player2, player1vector)

addPlayer1 <- SYD01_TAMg3[ which(SYD01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_TAMg3[ which(SYD01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_TAMg2 <- rbind(SYD01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
SYD01_TAMft <- ftable(SYD01_TAMg2$player1, SYD01_TAMg2$player2)
SYD01_TAMft2 <- as.matrix(SYD01_TAMft)
numRows <- nrow(SYD01_TAMft2)
numCols <- ncol(SYD01_TAMft2)
SYD01_TAMft3 <- SYD01_TAMft2[c(2:numRows) , c(2:numCols)]
SYD01_TAMTable <- graph.adjacency(SYD01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(SYD01_TAMTable, vertex.label = V(SYD01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
SYD01_TAM.clusterCoef <- transitivity(SYD01_TAMTable, type="global") #cluster coefficient
SYD01_TAM.degreeCent <- centralization.degree(SYD01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_TAMftn <- as.network.matrix(SYD01_TAMft)
SYD01_TAM.netDensity <- network.density(SYD01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_TAM.entropy <- entropy(SYD01_TAMft) #entropy

SYD01_TAM.netMx <- cbind(SYD01_TAM.netMx, SYD01_TAM.clusterCoef, SYD01_TAM.degreeCent$centralization,
                         SYD01_TAM.netDensity, SYD01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
SYD01_SDMg2 <- data.frame(SYD01_SDM)
SYD01_SDMg2 <- SYD01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_SDMg2$player1
player2vector <- SYD01_SDMg2$player2
SYD01_SDMg3 <- SYD01_SDMg2
SYD01_SDMg3$p1inp2vec <- is.element(SYD01_SDMg3$player1, player2vector)
SYD01_SDMg3$p2inp1vec <- is.element(SYD01_SDMg3$player2, player1vector)

addPlayer1 <- SYD01_SDMg3[ which(SYD01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_SDMg3[ which(SYD01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_SDMg2 <- rbind(SYD01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
SYD01_SDMft <- ftable(SYD01_SDMg2$player1, SYD01_SDMg2$player2)
SYD01_SDMft2 <- as.matrix(SYD01_SDMft)
numRows <- nrow(SYD01_SDMft2)
numCols <- ncol(SYD01_SDMft2)
SYD01_SDMft3 <- SYD01_SDMft2[c(2:numRows) , c(2:numCols)]
SYD01_SDMTable <- graph.adjacency(SYD01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(SYD01_SDMTable, vertex.label = V(SYD01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
SYD01_SDM.clusterCoef <- transitivity(SYD01_SDMTable, type="global") #cluster coefficient
SYD01_SDM.degreeCent <- centralization.degree(SYD01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_SDMftn <- as.network.matrix(SYD01_SDMft)
SYD01_SDM.netDensity <- network.density(SYD01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_SDM.entropy <- entropy(SYD01_SDMft) #entropy

SYD01_SDM.netMx <- cbind(SYD01_SDM.netMx, SYD01_SDM.clusterCoef, SYD01_SDM.degreeCent$centralization,
                         SYD01_SDM.netDensity, SYD01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
SYD01_TDMg2 <- data.frame(SYD01_TDM)
SYD01_TDMg2 <- SYD01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_TDMg2$player1
player2vector <- SYD01_TDMg2$player2
SYD01_TDMg3 <- SYD01_TDMg2
SYD01_TDMg3$p1inp2vec <- is.element(SYD01_TDMg3$player1, player2vector)
SYD01_TDMg3$p2inp1vec <- is.element(SYD01_TDMg3$player2, player1vector)

addPlayer1 <- SYD01_TDMg3[ which(SYD01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_TDMg3[ which(SYD01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_TDMg2 <- rbind(SYD01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
SYD01_TDMft <- ftable(SYD01_TDMg2$player1, SYD01_TDMg2$player2)
SYD01_TDMft2 <- as.matrix(SYD01_TDMft)
numRows <- nrow(SYD01_TDMft2)
numCols <- ncol(SYD01_TDMft2)
SYD01_TDMft3 <- SYD01_TDMft2[c(2:numRows) , c(2:numCols)]
SYD01_TDMTable <- graph.adjacency(SYD01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(SYD01_TDMTable, vertex.label = V(SYD01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
SYD01_TDM.clusterCoef <- transitivity(SYD01_TDMTable, type="global") #cluster coefficient
SYD01_TDM.degreeCent <- centralization.degree(SYD01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_TDMftn <- as.network.matrix(SYD01_TDMft)
SYD01_TDM.netDensity <- network.density(SYD01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_TDM.entropy <- entropy(SYD01_TDMft) #entropy

SYD01_TDM.netMx <- cbind(SYD01_TDM.netMx, SYD01_TDM.clusterCoef, SYD01_TDM.degreeCent$centralization,
                         SYD01_TDM.netDensity, SYD01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
SYD01_SDg2 <- data.frame(SYD01_SD)
SYD01_SDg2 <- SYD01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_SDg2$player1
player2vector <- SYD01_SDg2$player2
SYD01_SDg3 <- SYD01_SDg2
SYD01_SDg3$p1inp2vec <- is.element(SYD01_SDg3$player1, player2vector)
SYD01_SDg3$p2inp1vec <- is.element(SYD01_SDg3$player2, player1vector)

addPlayer1 <- SYD01_SDg3[ which(SYD01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_SDg3[ which(SYD01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_SDg2 <- rbind(SYD01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
SYD01_SDft <- ftable(SYD01_SDg2$player1, SYD01_SDg2$player2)
SYD01_SDft2 <- as.matrix(SYD01_SDft)
numRows <- nrow(SYD01_SDft2)
numCols <- ncol(SYD01_SDft2)
SYD01_SDft3 <- SYD01_SDft2[c(2:numRows) , c(2:numCols)]
SYD01_SDTable <- graph.adjacency(SYD01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(SYD01_SDTable, vertex.label = V(SYD01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
SYD01_SD.clusterCoef <- transitivity(SYD01_SDTable, type="global") #cluster coefficient
SYD01_SD.degreeCent <- centralization.degree(SYD01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_SDftn <- as.network.matrix(SYD01_SDft)
SYD01_SD.netDensity <- network.density(SYD01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_SD.entropy <- entropy(SYD01_SDft) #entropy

SYD01_SD.netMx <- cbind(SYD01_SD.netMx, SYD01_SD.clusterCoef, SYD01_SD.degreeCent$centralization,
                        SYD01_SD.netDensity, SYD01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
SYD01_TDg2 <- data.frame(SYD01_TD)
SYD01_TDg2 <- SYD01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_TDg2$player1
player2vector <- SYD01_TDg2$player2
SYD01_TDg3 <- SYD01_TDg2
SYD01_TDg3$p1inp2vec <- is.element(SYD01_TDg3$player1, player2vector)
SYD01_TDg3$p2inp1vec <- is.element(SYD01_TDg3$player2, player1vector)

addPlayer1 <- SYD01_TDg3[ which(SYD01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_TDg3[ which(SYD01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_TDg2 <- rbind(SYD01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
SYD01_TDft <- ftable(SYD01_TDg2$player1, SYD01_TDg2$player2)
SYD01_TDft2 <- as.matrix(SYD01_TDft)
numRows <- nrow(SYD01_TDft2)
numCols <- ncol(SYD01_TDft2)
SYD01_TDft3 <- SYD01_TDft2[c(2:numRows) , c(2:numCols)]
SYD01_TDTable <- graph.adjacency(SYD01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(SYD01_TDTable, vertex.label = V(SYD01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
SYD01_TD.clusterCoef <- transitivity(SYD01_TDTable, type="global") #cluster coefficient
SYD01_TD.degreeCent <- centralization.degree(SYD01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_TDftn <- as.network.matrix(SYD01_TDft)
SYD01_TD.netDensity <- network.density(SYD01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_TD.entropy <- entropy(SYD01_TDft) #entropy

SYD01_TD.netMx <- cbind(SYD01_TD.netMx, SYD01_TD.clusterCoef, SYD01_TD.degreeCent$centralization,
                        SYD01_TD.netDensity, SYD01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
SYD01_QTg2 <- data.frame(SYD01_QT)
SYD01_QTg2 <- SYD01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD01_QTg2$player1
player2vector <- SYD01_QTg2$player2
SYD01_QTg3 <- SYD01_QTg2
SYD01_QTg3$p1inp2vec <- is.element(SYD01_QTg3$player1, player2vector)
SYD01_QTg3$p2inp1vec <- is.element(SYD01_QTg3$player2, player1vector)

addPlayer1 <- SYD01_QTg3[ which(SYD01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD01_QTg3[ which(SYD01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD01_QTg2 <- rbind(SYD01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
SYD01_QTft <- ftable(SYD01_QTg2$player1, SYD01_QTg2$player2)
SYD01_QTft2 <- as.matrix(SYD01_QTft)
numRows <- nrow(SYD01_QTft2)
numCols <- ncol(SYD01_QTft2)
SYD01_QTft3 <- SYD01_QTft2[c(2:numRows) , c(2:numCols)]
SYD01_QTTable <- graph.adjacency(SYD01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(SYD01_QTTable, vertex.label = V(SYD01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
SYD01_QT.clusterCoef <- transitivity(SYD01_QTTable, type="global") #cluster coefficient
SYD01_QT.degreeCent <- centralization.degree(SYD01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD01_QTftn <- as.network.matrix(SYD01_QTft)
SYD01_QT.netDensity <- network.density(SYD01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD01_QT.entropy <- entropy(SYD01_QTft) #entropy

SYD01_QT.netMx <- cbind(SYD01_QT.netMx, SYD01_QT.clusterCoef, SYD01_QT.degreeCent$centralization,
                        SYD01_QT.netDensity, SYD01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD01_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 1
##

#Round 1, Goal***************************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Goal_F"
WB01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
WB01_Gg2 <- data.frame(WB01_G)
WB01_Gg2 <- WB01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_Gg2$player1
player2vector <- WB01_Gg2$player2
WB01_Gg3 <- WB01_Gg2
WB01_Gg3$p1inp2vec <- is.element(WB01_Gg3$player1, player2vector)
WB01_Gg3$p2inp1vec <- is.element(WB01_Gg3$player2, player1vector)

addPlayer1 <- WB01_Gg3[ which(WB01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_Gg3[ which(WB01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_Gg2 <- rbind(WB01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
WB01_Gft <- ftable(WB01_Gg2$player1, WB01_Gg2$player2)
WB01_Gft2 <- as.matrix(WB01_Gft)
numRows <- nrow(WB01_Gft2)
numCols <- ncol(WB01_Gft2)
WB01_Gft3 <- WB01_Gft2[c(2:numRows) , c(2:numCols)]
WB01_GTable <- graph.adjacency(WB01_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(WB01_GTable, vertex.label = V(WB01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
WB01_G.clusterCoef <- transitivity(WB01_GTable, type="global") #cluster coefficient
WB01_G.degreeCent <- centralization.degree(WB01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_Gftn <- as.network.matrix(WB01_Gft)
WB01_G.netDensity <- network.density(WB01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_G.entropy <- entropy(WB01_Gft) #entropy

WB01_G.netMx <- cbind(WB01_G.netMx, WB01_G.clusterCoef, WB01_G.degreeCent$centralization,
                      WB01_G.netDensity, WB01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Behind_F"
WB01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
WB01_Bg2 <- data.frame(WB01_B)
WB01_Bg2 <- WB01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_Bg2$player1
player2vector <- WB01_Bg2$player2
WB01_Bg3 <- WB01_Bg2
WB01_Bg3$p1inp2vec <- is.element(WB01_Bg3$player1, player2vector)
WB01_Bg3$p2inp1vec <- is.element(WB01_Bg3$player2, player1vector)

addPlayer1 <- WB01_Bg3[ which(WB01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_Bg3[ which(WB01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_Bg2 <- rbind(WB01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
WB01_Bft <- ftable(WB01_Bg2$player1, WB01_Bg2$player2)
WB01_Bft2 <- as.matrix(WB01_Bft)
numRows <- nrow(WB01_Bft2)
numCols <- ncol(WB01_Bft2)
WB01_Bft3 <- WB01_Bft2[c(2:numRows) , c(2:numCols)]
WB01_BTable <- graph.adjacency(WB01_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(WB01_BTable, vertex.label = V(WB01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
WB01_B.clusterCoef <- transitivity(WB01_BTable, type="global") #cluster coefficient
WB01_B.degreeCent <- centralization.degree(WB01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_Bftn <- as.network.matrix(WB01_Bft)
WB01_B.netDensity <- network.density(WB01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_B.entropy <- entropy(WB01_Bft) #entropy

WB01_B.netMx <- cbind(WB01_B.netMx, WB01_B.clusterCoef, WB01_B.degreeCent$centralization,
                      WB01_B.netDensity, WB01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Stoppage_F"
WB01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
WB01_SFg2 <- data.frame(WB01_SF)
WB01_SFg2 <- WB01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_SFg2$player1
player2vector <- WB01_SFg2$player2
WB01_SFg3 <- WB01_SFg2
WB01_SFg3$p1inp2vec <- is.element(WB01_SFg3$player1, player2vector)
WB01_SFg3$p2inp1vec <- is.element(WB01_SFg3$player2, player1vector)

addPlayer1 <- WB01_SFg3[ which(WB01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_SFg3[ which(WB01_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_SFg2 <- rbind(WB01_SFg2, addPlayers)

#Round 1, FWD Stoppage graph using weighted edges
WB01_SFft <- ftable(WB01_SFg2$player1, WB01_SFg2$player2)
WB01_SFft2 <- as.matrix(WB01_SFft)
numRows <- nrow(WB01_SFft2)
numCols <- ncol(WB01_SFft2)
WB01_SFft3 <- WB01_SFft2[c(2:numRows) , c(2:numCols)]
WB01_SFTable <- graph.adjacency(WB01_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(WB01_SFTable, vertex.label = V(WB01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
WB01_SF.clusterCoef <- transitivity(WB01_SFTable, type="global") #cluster coefficient
WB01_SF.degreeCent <- centralization.degree(WB01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_SFftn <- as.network.matrix(WB01_SFft)
WB01_SF.netDensity <- network.density(WB01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_SF.entropy <- entropy(WB01_SFft) #entropy

WB01_SF.netMx <- cbind(WB01_SF.netMx, WB01_SF.clusterCoef, WB01_SF.degreeCent$centralization,
                       WB01_SF.netDensity, WB01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Turnover_F"
WB01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
WB01_TFg2 <- data.frame(WB01_TF)
WB01_TFg2 <- WB01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_TFg2$player1
player2vector <- WB01_TFg2$player2
WB01_TFg3 <- WB01_TFg2
WB01_TFg3$p1inp2vec <- is.element(WB01_TFg3$player1, player2vector)
WB01_TFg3$p2inp1vec <- is.element(WB01_TFg3$player2, player1vector)

addPlayer1 <- WB01_TFg3[ which(WB01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_TFg3[ which(WB01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_TFg2 <- rbind(WB01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
WB01_TFft <- ftable(WB01_TFg2$player1, WB01_TFg2$player2)
WB01_TFft2 <- as.matrix(WB01_TFft)
numRows <- nrow(WB01_TFft2)
numCols <- ncol(WB01_TFft2)
WB01_TFft3 <- WB01_TFft2[c(2:numRows) , c(2:numCols)]
WB01_TFTable <- graph.adjacency(WB01_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(WB01_TFTable, vertex.label = V(WB01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
WB01_TF.clusterCoef <- transitivity(WB01_TFTable, type="global") #cluster coefficient
WB01_TF.degreeCent <- centralization.degree(WB01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_TFftn <- as.network.matrix(WB01_TFft)
WB01_TF.netDensity <- network.density(WB01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_TF.entropy <- entropy(WB01_TFft) #entropy

WB01_TF.netMx <- cbind(WB01_TF.netMx, WB01_TF.clusterCoef, WB01_TF.degreeCent$centralization,
                       WB01_TF.netDensity, WB01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
WB01_SAMg2 <- data.frame(WB01_SAM)
WB01_SAMg2 <- WB01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_SAMg2$player1
player2vector <- WB01_SAMg2$player2
WB01_SAMg3 <- WB01_SAMg2
WB01_SAMg3$p1inp2vec <- is.element(WB01_SAMg3$player1, player2vector)
WB01_SAMg3$p2inp1vec <- is.element(WB01_SAMg3$player2, player1vector)

addPlayer1 <- WB01_SAMg3[ which(WB01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_SAMg3[ which(WB01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_SAMg2 <- rbind(WB01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
WB01_SAMft <- ftable(WB01_SAMg2$player1, WB01_SAMg2$player2)
WB01_SAMft2 <- as.matrix(WB01_SAMft)
numRows <- nrow(WB01_SAMft2)
numCols <- ncol(WB01_SAMft2)
WB01_SAMft3 <- WB01_SAMft2[c(2:numRows) , c(2:numCols)]
WB01_SAMTable <- graph.adjacency(WB01_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(WB01_SAMTable, vertex.label = V(WB01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
WB01_SAM.clusterCoef <- transitivity(WB01_SAMTable, type="global") #cluster coefficient
WB01_SAM.degreeCent <- centralization.degree(WB01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_SAMftn <- as.network.matrix(WB01_SAMft)
WB01_SAM.netDensity <- network.density(WB01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_SAM.entropy <- entropy(WB01_SAMft) #entropy

WB01_SAM.netMx <- cbind(WB01_SAM.netMx, WB01_SAM.clusterCoef, WB01_SAM.degreeCent$centralization,
                        WB01_SAM.netDensity, WB01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Turnover_AM"
WB01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
WB01_TAMg2 <- data.frame(WB01_TAM)
WB01_TAMg2 <- WB01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_TAMg2$player1
player2vector <- WB01_TAMg2$player2
WB01_TAMg3 <- WB01_TAMg2
WB01_TAMg3$p1inp2vec <- is.element(WB01_TAMg3$player1, player2vector)
WB01_TAMg3$p2inp1vec <- is.element(WB01_TAMg3$player2, player1vector)

addPlayer1 <- WB01_TAMg3[ which(WB01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_TAMg3[ which(WB01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_TAMg2 <- rbind(WB01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
WB01_TAMft <- ftable(WB01_TAMg2$player1, WB01_TAMg2$player2)
WB01_TAMft2 <- as.matrix(WB01_TAMft)
numRows <- nrow(WB01_TAMft2)
numCols <- ncol(WB01_TAMft2)
WB01_TAMft3 <- WB01_TAMft2[c(2:numRows) , c(2:numCols)]
WB01_TAMTable <- graph.adjacency(WB01_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(WB01_TAMTable, vertex.label = V(WB01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
WB01_TAM.clusterCoef <- transitivity(WB01_TAMTable, type="global") #cluster coefficient
WB01_TAM.degreeCent <- centralization.degree(WB01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_TAMftn <- as.network.matrix(WB01_TAMft)
WB01_TAM.netDensity <- network.density(WB01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_TAM.entropy <- entropy(WB01_TAMft) #entropy

WB01_TAM.netMx <- cbind(WB01_TAM.netMx, WB01_TAM.clusterCoef, WB01_TAM.degreeCent$centralization,
                        WB01_TAM.netDensity, WB01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
WB01_SDMg2 <- data.frame(WB01_SDM)
WB01_SDMg2 <- WB01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_SDMg2$player1
player2vector <- WB01_SDMg2$player2
WB01_SDMg3 <- WB01_SDMg2
WB01_SDMg3$p1inp2vec <- is.element(WB01_SDMg3$player1, player2vector)
WB01_SDMg3$p2inp1vec <- is.element(WB01_SDMg3$player2, player1vector)

addPlayer1 <- WB01_SDMg3[ which(WB01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_SDMg3[ which(WB01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_SDMg2 <- rbind(WB01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
WB01_SDMft <- ftable(WB01_SDMg2$player1, WB01_SDMg2$player2)
WB01_SDMft2 <- as.matrix(WB01_SDMft)
numRows <- nrow(WB01_SDMft2)
numCols <- ncol(WB01_SDMft2)
WB01_SDMft3 <- WB01_SDMft2[c(2:numRows) , c(2:numCols)]
WB01_SDMTable <- graph.adjacency(WB01_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(WB01_SDMTable, vertex.label = V(WB01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
WB01_SDM.clusterCoef <- transitivity(WB01_SDMTable, type="global") #cluster coefficient
WB01_SDM.degreeCent <- centralization.degree(WB01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_SDMftn <- as.network.matrix(WB01_SDMft)
WB01_SDM.netDensity <- network.density(WB01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_SDM.entropy <- entropy(WB01_SDMft) #entropy

WB01_SDM.netMx <- cbind(WB01_SDM.netMx, WB01_SDM.clusterCoef, WB01_SDM.degreeCent$centralization,
                        WB01_SDM.netDensity, WB01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "WB"
KIoutcome = "Turnover_DM"
WB01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
WB01_TDMg2 <- data.frame(WB01_TDM)
WB01_TDMg2 <- WB01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_TDMg2$player1
player2vector <- WB01_TDMg2$player2
WB01_TDMg3 <- WB01_TDMg2
WB01_TDMg3$p1inp2vec <- is.element(WB01_TDMg3$player1, player2vector)
WB01_TDMg3$p2inp1vec <- is.element(WB01_TDMg3$player2, player1vector)

addPlayer1 <- WB01_TDMg3[ which(WB01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_TDMg3[ which(WB01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_TDMg2 <- rbind(WB01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
WB01_TDMft <- ftable(WB01_TDMg2$player1, WB01_TDMg2$player2)
WB01_TDMft2 <- as.matrix(WB01_TDMft)
numRows <- nrow(WB01_TDMft2)
numCols <- ncol(WB01_TDMft2)
WB01_TDMft3 <- WB01_TDMft2[c(2:numRows) , c(2:numCols)]
WB01_TDMTable <- graph.adjacency(WB01_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(WB01_TDMTable, vertex.label = V(WB01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
WB01_TDM.clusterCoef <- transitivity(WB01_TDMTable, type="global") #cluster coefficient
WB01_TDM.degreeCent <- centralization.degree(WB01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_TDMftn <- as.network.matrix(WB01_TDMft)
WB01_TDM.netDensity <- network.density(WB01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_TDM.entropy <- entropy(WB01_TDMft) #entropy

WB01_TDM.netMx <- cbind(WB01_TDM.netMx, WB01_TDM.clusterCoef, WB01_TDM.degreeCent$centralization,
                        WB01_TDM.netDensity, WB01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Stoppage_D"
WB01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
WB01_SDg2 <- data.frame(WB01_SD)
WB01_SDg2 <- WB01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_SDg2$player1
player2vector <- WB01_SDg2$player2
WB01_SDg3 <- WB01_SDg2
WB01_SDg3$p1inp2vec <- is.element(WB01_SDg3$player1, player2vector)
WB01_SDg3$p2inp1vec <- is.element(WB01_SDg3$player2, player1vector)

addPlayer1 <- WB01_SDg3[ which(WB01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_SDg3[ which(WB01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_SDg2 <- rbind(WB01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
WB01_SDft <- ftable(WB01_SDg2$player1, WB01_SDg2$player2)
WB01_SDft2 <- as.matrix(WB01_SDft)
numRows <- nrow(WB01_SDft2)
numCols <- ncol(WB01_SDft2)
WB01_SDft3 <- WB01_SDft2[c(2:numRows) , c(2:numCols)]
WB01_SDTable <- graph.adjacency(WB01_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(WB01_SDTable, vertex.label = V(WB01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
WB01_SD.clusterCoef <- transitivity(WB01_SDTable, type="global") #cluster coefficient
WB01_SD.degreeCent <- centralization.degree(WB01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_SDftn <- as.network.matrix(WB01_SDft)
WB01_SD.netDensity <- network.density(WB01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_SD.entropy <- entropy(WB01_SDft) #entropy

WB01_SD.netMx <- cbind(WB01_SD.netMx, WB01_SD.clusterCoef, WB01_SD.degreeCent$centralization,
                       WB01_SD.netDensity, WB01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "Turnover_D"
WB01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
WB01_TDg2 <- data.frame(WB01_TD)
WB01_TDg2 <- WB01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_TDg2$player1
player2vector <- WB01_TDg2$player2
WB01_TDg3 <- WB01_TDg2
WB01_TDg3$p1inp2vec <- is.element(WB01_TDg3$player1, player2vector)
WB01_TDg3$p2inp1vec <- is.element(WB01_TDg3$player2, player1vector)

addPlayer1 <- WB01_TDg3[ which(WB01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_TDg3[ which(WB01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_TDg2 <- rbind(WB01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
WB01_TDft <- ftable(WB01_TDg2$player1, WB01_TDg2$player2)
WB01_TDft2 <- as.matrix(WB01_TDft)
numRows <- nrow(WB01_TDft2)
numCols <- ncol(WB01_TDft2)
WB01_TDft3 <- WB01_TDft2[c(2:numRows) , c(2:numCols)]
WB01_TDTable <- graph.adjacency(WB01_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(WB01_TDTable, vertex.label = V(WB01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
WB01_TD.clusterCoef <- transitivity(WB01_TDTable, type="global") #cluster coefficient
WB01_TD.degreeCent <- centralization.degree(WB01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_TDftn <- as.network.matrix(WB01_TDft)
WB01_TD.netDensity <- network.density(WB01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_TD.entropy <- entropy(WB01_TDft) #entropy

WB01_TD.netMx <- cbind(WB01_TD.netMx, WB01_TD.clusterCoef, WB01_TD.degreeCent$centralization,
                       WB01_TD.netDensity, WB01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
WB01_QTg2 <- data.frame(WB01_QT)
WB01_QTg2 <- WB01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB01_QTg2$player1
player2vector <- WB01_QTg2$player2
WB01_QTg3 <- WB01_QTg2
WB01_QTg3$p1inp2vec <- is.element(WB01_QTg3$player1, player2vector)
WB01_QTg3$p2inp1vec <- is.element(WB01_QTg3$player2, player1vector)

addPlayer1 <- WB01_QTg3[ which(WB01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB01_QTg3[ which(WB01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB01_QTg2 <- rbind(WB01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
WB01_QTft <- ftable(WB01_QTg2$player1, WB01_QTg2$player2)
WB01_QTft2 <- as.matrix(WB01_QTft)
numRows <- nrow(WB01_QTft2)
numCols <- ncol(WB01_QTft2)
WB01_QTft3 <- WB01_QTft2[c(2:numRows) , c(2:numCols)]
WB01_QTTable <- graph.adjacency(WB01_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(WB01_QTTable, vertex.label = V(WB01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
WB01_QT.clusterCoef <- transitivity(WB01_QTTable, type="global") #cluster coefficient
WB01_QT.degreeCent <- centralization.degree(WB01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB01_QTftn <- as.network.matrix(WB01_QTft)
WB01_QT.netDensity <- network.density(WB01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB01_QT.entropy <- entropy(WB01_QTft) #entropy

WB01_QT.netMx <- cbind(WB01_QT.netMx, WB01_QT.clusterCoef, WB01_QT.degreeCent$centralization,
                       WB01_QT.netDensity, WB01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB01_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 1
##

#Round 1, Goal***************************************************************

round = 1
teamName = "WCE"
KIoutcome = "Goal_F"
WCE01_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Goal with weighted edges
WCE01_Gg2 <- data.frame(WCE01_G)
WCE01_Gg2 <- WCE01_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_Gg2$player1
player2vector <- WCE01_Gg2$player2
WCE01_Gg3 <- WCE01_Gg2
WCE01_Gg3$p1inp2vec <- is.element(WCE01_Gg3$player1, player2vector)
WCE01_Gg3$p2inp1vec <- is.element(WCE01_Gg3$player2, player1vector)

addPlayer1 <- WCE01_Gg3[ which(WCE01_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_Gg3[ which(WCE01_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_Gg2 <- rbind(WCE01_Gg2, addPlayers)

#Round 1, Goal graph using weighted edges
WCE01_Gft <- ftable(WCE01_Gg2$player1, WCE01_Gg2$player2)
WCE01_Gft2 <- as.matrix(WCE01_Gft)
numRows <- nrow(WCE01_Gft2)
numCols <- ncol(WCE01_Gft2)
WCE01_Gft3 <- WCE01_Gft2[c(2:numRows) , c(2:numCols)]
WCE01_GTable <- graph.adjacency(WCE01_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Goal graph=weighted
plot.igraph(WCE01_GTable, vertex.label = V(WCE01_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Goal calulation of network metrics
#igraph
WCE01_G.clusterCoef <- transitivity(WCE01_GTable, type="global") #cluster coefficient
WCE01_G.degreeCent <- centralization.degree(WCE01_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_Gftn <- as.network.matrix(WCE01_Gft)
WCE01_G.netDensity <- network.density(WCE01_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_G.entropy <- entropy(WCE01_Gft) #entropy

WCE01_G.netMx <- cbind(WCE01_G.netMx, WCE01_G.clusterCoef, WCE01_G.degreeCent$centralization,
                       WCE01_G.netDensity, WCE01_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_G.netMx) <- varnames

#Round 1, Behind***************************************************************
#NA

round = 1
teamName = "WCE"
KIoutcome = "Behind_F"
WCE01_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, Behind with weighted edges
WCE01_Bg2 <- data.frame(WCE01_B)
WCE01_Bg2 <- WCE01_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_Bg2$player1
player2vector <- WCE01_Bg2$player2
WCE01_Bg3 <- WCE01_Bg2
WCE01_Bg3$p1inp2vec <- is.element(WCE01_Bg3$player1, player2vector)
WCE01_Bg3$p2inp1vec <- is.element(WCE01_Bg3$player2, player1vector)

addPlayer1 <- WCE01_Bg3[ which(WCE01_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_Bg3[ which(WCE01_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_Bg2 <- rbind(WCE01_Bg2, addPlayers)

#Round 1, Behind graph using weighted edges
WCE01_Bft <- ftable(WCE01_Bg2$player1, WCE01_Bg2$player2)
WCE01_Bft2 <- as.matrix(WCE01_Bft)
numRows <- nrow(WCE01_Bft2)
numCols <- ncol(WCE01_Bft2)
WCE01_Bft3 <- WCE01_Bft2[c(2:numRows) , c(2:numCols)]
WCE01_BTable <- graph.adjacency(WCE01_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 1, Behind graph=weighted
plot.igraph(WCE01_BTable, vertex.label = V(WCE01_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, Behind calulation of network metrics
#igraph
WCE01_B.clusterCoef <- transitivity(WCE01_BTable, type="global") #cluster coefficient
WCE01_B.degreeCent <- centralization.degree(WCE01_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_Bftn <- as.network.matrix(WCE01_Bft)
WCE01_B.netDensity <- network.density(WCE01_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_B.entropy <- entropy(WCE01_Bft) #entropy

WCE01_B.netMx <- cbind(WCE01_B.netMx, WCE01_B.clusterCoef, WCE01_B.degreeCent$centralization,
                       WCE01_B.netDensity, WCE01_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_B.netMx) <- varnames

#Round 1, FWD Stoppage**********************************************************

round = 1
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE01_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Stoppage with weighted edges
WCE01_SFg2 <- data.frame(WCE01_SF)
WCE01_SFg2 <- WCE01_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_SFg2$player1
player2vector <- WCE01_SFg2$player2
WCE01_SFg3 <- WCE01_SFg2
WCE01_SFg3$p1inp2vec <- is.element(WCE01_SFg3$player1, player2vector)
WCE01_SFg3$p2inp1vec <- is.element(WCE01_SFg3$player2, player1vector)

addPlayer1 <- WCE01_SFg3[ which(WCE01_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE01_SFg2 <- rbind(WCE01_SFg2, addPlayer1)

#Round 1, FWD Stoppage graph using weighted edges
WCE01_SFft <- ftable(WCE01_SFg2$player1, WCE01_SFg2$player2)
WCE01_SFft2 <- as.matrix(WCE01_SFft)
numRows <- nrow(WCE01_SFft2)
numCols <- ncol(WCE01_SFft2)
WCE01_SFft3 <- WCE01_SFft2[c(2:numRows) , c(1:numCols)]
WCE01_SFTable <- graph.adjacency(WCE01_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Stoppage graph=weighted
plot.igraph(WCE01_SFTable, vertex.label = V(WCE01_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Stoppage calulation of network metrics
#igraph
WCE01_SF.clusterCoef <- transitivity(WCE01_SFTable, type="global") #cluster coefficient
WCE01_SF.degreeCent <- centralization.degree(WCE01_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_SFftn <- as.network.matrix(WCE01_SFft)
WCE01_SF.netDensity <- network.density(WCE01_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_SF.entropy <- entropy(WCE01_SFft) #entropy

WCE01_SF.netMx <- cbind(WCE01_SF.netMx, WCE01_SF.clusterCoef, WCE01_SF.degreeCent$centralization,
                        WCE01_SF.netDensity, WCE01_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_SF.netMx) <- varnames

#Round 1, FWD Turnover**********************************************************

round = 1
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE01_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, FWD Turnover with weighted edges
WCE01_TFg2 <- data.frame(WCE01_TF)
WCE01_TFg2 <- WCE01_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_TFg2$player1
player2vector <- WCE01_TFg2$player2
WCE01_TFg3 <- WCE01_TFg2
WCE01_TFg3$p1inp2vec <- is.element(WCE01_TFg3$player1, player2vector)
WCE01_TFg3$p2inp1vec <- is.element(WCE01_TFg3$player2, player1vector)

addPlayer1 <- WCE01_TFg3[ which(WCE01_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_TFg3[ which(WCE01_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_TFg2 <- rbind(WCE01_TFg2, addPlayers)

#Round 1, FWD Turnover graph using weighted edges
WCE01_TFft <- ftable(WCE01_TFg2$player1, WCE01_TFg2$player2)
WCE01_TFft2 <- as.matrix(WCE01_TFft)
numRows <- nrow(WCE01_TFft2)
numCols <- ncol(WCE01_TFft2)
WCE01_TFft3 <- WCE01_TFft2[c(2:numRows) , c(2:numCols)]
WCE01_TFTable <- graph.adjacency(WCE01_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, FWD Turnover graph=weighted
plot.igraph(WCE01_TFTable, vertex.label = V(WCE01_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, FWD Turnover calulation of network metrics
#igraph
WCE01_TF.clusterCoef <- transitivity(WCE01_TFTable, type="global") #cluster coefficient
WCE01_TF.degreeCent <- centralization.degree(WCE01_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_TFftn <- as.network.matrix(WCE01_TFft)
WCE01_TF.netDensity <- network.density(WCE01_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_TF.entropy <- entropy(WCE01_TFft) #entropy

WCE01_TF.netMx <- cbind(WCE01_TF.netMx, WCE01_TF.clusterCoef, WCE01_TF.degreeCent$centralization,
                        WCE01_TF.netDensity, WCE01_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_TF.netMx) <- varnames

#Round 1, AM Stoppage**********************************************************
#NA

round = 1
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE01_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Stoppage with weighted edges
WCE01_SAMg2 <- data.frame(WCE01_SAM)
WCE01_SAMg2 <- WCE01_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_SAMg2$player1
player2vector <- WCE01_SAMg2$player2
WCE01_SAMg3 <- WCE01_SAMg2
WCE01_SAMg3$p1inp2vec <- is.element(WCE01_SAMg3$player1, player2vector)
WCE01_SAMg3$p2inp1vec <- is.element(WCE01_SAMg3$player2, player1vector)

addPlayer1 <- WCE01_SAMg3[ which(WCE01_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_SAMg3[ which(WCE01_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_SAMg2 <- rbind(WCE01_SAMg2, addPlayers)

#Round 1, AM Stoppage graph using weighted edges
WCE01_SAMft <- ftable(WCE01_SAMg2$player1, WCE01_SAMg2$player2)
WCE01_SAMft2 <- as.matrix(WCE01_SAMft)
numRows <- nrow(WCE01_SAMft2)
numCols <- ncol(WCE01_SAMft2)
WCE01_SAMft3 <- WCE01_SAMft2[c(2:numRows) , c(2:numCols)]
WCE01_SAMTable <- graph.adjacency(WCE01_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Stoppage graph=weighted
plot.igraph(WCE01_SAMTable, vertex.label = V(WCE01_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Stoppage calulation of network metrics
#igraph
WCE01_SAM.clusterCoef <- transitivity(WCE01_SAMTable, type="global") #cluster coefficient
WCE01_SAM.degreeCent <- centralization.degree(WCE01_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_SAMftn <- as.network.matrix(WCE01_SAMft)
WCE01_SAM.netDensity <- network.density(WCE01_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_SAM.entropy <- entropy(WCE01_SAMft) #entropy

WCE01_SAM.netMx <- cbind(WCE01_SAM.netMx, WCE01_SAM.clusterCoef, WCE01_SAM.degreeCent$centralization,
                         WCE01_SAM.netDensity, WCE01_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_SAM.netMx) <- varnames

#Round 1, AM Turnover**********************************************************

round = 1
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE01_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, AM Turnover with weighted edges
WCE01_TAMg2 <- data.frame(WCE01_TAM)
WCE01_TAMg2 <- WCE01_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_TAMg2$player1
player2vector <- WCE01_TAMg2$player2
WCE01_TAMg3 <- WCE01_TAMg2
WCE01_TAMg3$p1inp2vec <- is.element(WCE01_TAMg3$player1, player2vector)
WCE01_TAMg3$p2inp1vec <- is.element(WCE01_TAMg3$player2, player1vector)

addPlayer1 <- WCE01_TAMg3[ which(WCE01_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_TAMg3[ which(WCE01_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_TAMg2 <- rbind(WCE01_TAMg2, addPlayers)

#Round 1, AM Turnover graph using weighted edges
WCE01_TAMft <- ftable(WCE01_TAMg2$player1, WCE01_TAMg2$player2)
WCE01_TAMft2 <- as.matrix(WCE01_TAMft)
numRows <- nrow(WCE01_TAMft2)
numCols <- ncol(WCE01_TAMft2)
WCE01_TAMft3 <- WCE01_TAMft2[c(2:numRows) , c(2:numCols)]
WCE01_TAMTable <- graph.adjacency(WCE01_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, AM Turnover graph=weighted
plot.igraph(WCE01_TAMTable, vertex.label = V(WCE01_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, AM Turnover calulation of network metrics
#igraph
WCE01_TAM.clusterCoef <- transitivity(WCE01_TAMTable, type="global") #cluster coefficient
WCE01_TAM.degreeCent <- centralization.degree(WCE01_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_TAMftn <- as.network.matrix(WCE01_TAMft)
WCE01_TAM.netDensity <- network.density(WCE01_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_TAM.entropy <- entropy(WCE01_TAMft) #entropy

WCE01_TAM.netMx <- cbind(WCE01_TAM.netMx, WCE01_TAM.clusterCoef, WCE01_TAM.degreeCent$centralization,
                         WCE01_TAM.netDensity, WCE01_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_TAM.netMx) <- varnames

#Round 1, DM Stoppage**********************************************************

round = 1
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE01_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Stoppage with weighted edges
WCE01_SDMg2 <- data.frame(WCE01_SDM)
WCE01_SDMg2 <- WCE01_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_SDMg2$player1
player2vector <- WCE01_SDMg2$player2
WCE01_SDMg3 <- WCE01_SDMg2
WCE01_SDMg3$p1inp2vec <- is.element(WCE01_SDMg3$player1, player2vector)
WCE01_SDMg3$p2inp1vec <- is.element(WCE01_SDMg3$player2, player1vector)

addPlayer1 <- WCE01_SDMg3[ which(WCE01_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_SDMg3[ which(WCE01_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_SDMg2 <- rbind(WCE01_SDMg2, addPlayers)

#Round 1, DM Stoppage graph using weighted edges
WCE01_SDMft <- ftable(WCE01_SDMg2$player1, WCE01_SDMg2$player2)
WCE01_SDMft2 <- as.matrix(WCE01_SDMft)
numRows <- nrow(WCE01_SDMft2)
numCols <- ncol(WCE01_SDMft2)
WCE01_SDMft3 <- WCE01_SDMft2[c(2:numRows) , c(2:numCols)]
WCE01_SDMTable <- graph.adjacency(WCE01_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Stoppage graph=weighted
plot.igraph(WCE01_SDMTable, vertex.label = V(WCE01_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Stoppage calulation of network metrics
#igraph
WCE01_SDM.clusterCoef <- transitivity(WCE01_SDMTable, type="global") #cluster coefficient
WCE01_SDM.degreeCent <- centralization.degree(WCE01_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_SDMftn <- as.network.matrix(WCE01_SDMft)
WCE01_SDM.netDensity <- network.density(WCE01_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_SDM.entropy <- entropy(WCE01_SDMft) #entropy

WCE01_SDM.netMx <- cbind(WCE01_SDM.netMx, WCE01_SDM.clusterCoef, WCE01_SDM.degreeCent$centralization,
                         WCE01_SDM.netDensity, WCE01_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_SDM.netMx) <- varnames

#Round 1, DM Turnover**********************************************************

round = 1
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE01_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, DM Turnover with weighted edges
WCE01_TDMg2 <- data.frame(WCE01_TDM)
WCE01_TDMg2 <- WCE01_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_TDMg2$player1
player2vector <- WCE01_TDMg2$player2
WCE01_TDMg3 <- WCE01_TDMg2
WCE01_TDMg3$p1inp2vec <- is.element(WCE01_TDMg3$player1, player2vector)
WCE01_TDMg3$p2inp1vec <- is.element(WCE01_TDMg3$player2, player1vector)

addPlayer1 <- WCE01_TDMg3[ which(WCE01_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_TDMg3[ which(WCE01_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_TDMg2 <- rbind(WCE01_TDMg2, addPlayers)

#Round 1, DM Turnover graph using weighted edges
WCE01_TDMft <- ftable(WCE01_TDMg2$player1, WCE01_TDMg2$player2)
WCE01_TDMft2 <- as.matrix(WCE01_TDMft)
numRows <- nrow(WCE01_TDMft2)
numCols <- ncol(WCE01_TDMft2)
WCE01_TDMft3 <- WCE01_TDMft2[c(2:numRows) , c(2:numCols)]
WCE01_TDMTable <- graph.adjacency(WCE01_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 1, DM Turnover graph=weighted
plot.igraph(WCE01_TDMTable, vertex.label = V(WCE01_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, DM Turnover calulation of network metrics
#igraph
WCE01_TDM.clusterCoef <- transitivity(WCE01_TDMTable, type="global") #cluster coefficient
WCE01_TDM.degreeCent <- centralization.degree(WCE01_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_TDMftn <- as.network.matrix(WCE01_TDMft)
WCE01_TDM.netDensity <- network.density(WCE01_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_TDM.entropy <- entropy(WCE01_TDMft) #entropy

WCE01_TDM.netMx <- cbind(WCE01_TDM.netMx, WCE01_TDM.clusterCoef, WCE01_TDM.degreeCent$centralization,
                         WCE01_TDM.netDensity, WCE01_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_TDM.netMx) <- varnames

#Round 1, D Stoppage**********************************************************
#NA

round = 1
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE01_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Stoppage with weighted edges
WCE01_SDg2 <- data.frame(WCE01_SD)
WCE01_SDg2 <- WCE01_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_SDg2$player1
player2vector <- WCE01_SDg2$player2
WCE01_SDg3 <- WCE01_SDg2
WCE01_SDg3$p1inp2vec <- is.element(WCE01_SDg3$player1, player2vector)
WCE01_SDg3$p2inp1vec <- is.element(WCE01_SDg3$player2, player1vector)

addPlayer1 <- WCE01_SDg3[ which(WCE01_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_SDg3[ which(WCE01_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_SDg2 <- rbind(WCE01_SDg2, addPlayers)

#Round 1, D Stoppage graph using weighted edges
WCE01_SDft <- ftable(WCE01_SDg2$player1, WCE01_SDg2$player2)
WCE01_SDft2 <- as.matrix(WCE01_SDft)
numRows <- nrow(WCE01_SDft2)
numCols <- ncol(WCE01_SDft2)
WCE01_SDft3 <- WCE01_SDft2[c(2:numRows) , c(2:numCols)]
WCE01_SDTable <- graph.adjacency(WCE01_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Stoppage graph=weighted
plot.igraph(WCE01_SDTable, vertex.label = V(WCE01_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Stoppage calulation of network metrics
#igraph
WCE01_SD.clusterCoef <- transitivity(WCE01_SDTable, type="global") #cluster coefficient
WCE01_SD.degreeCent <- centralization.degree(WCE01_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_SDftn <- as.network.matrix(WCE01_SDft)
WCE01_SD.netDensity <- network.density(WCE01_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_SD.entropy <- entropy(WCE01_SDft) #entropy

WCE01_SD.netMx <- cbind(WCE01_SD.netMx, WCE01_SD.clusterCoef, WCE01_SD.degreeCent$centralization,
                        WCE01_SD.netDensity, WCE01_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_SD.netMx) <- varnames

#Round 1, D Turnover**********************************************************
#NA

round = 1
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE01_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, D Turnover with weighted edges
WCE01_TDg2 <- data.frame(WCE01_TD)
WCE01_TDg2 <- WCE01_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_TDg2$player1
player2vector <- WCE01_TDg2$player2
WCE01_TDg3 <- WCE01_TDg2
WCE01_TDg3$p1inp2vec <- is.element(WCE01_TDg3$player1, player2vector)
WCE01_TDg3$p2inp1vec <- is.element(WCE01_TDg3$player2, player1vector)

addPlayer1 <- WCE01_TDg3[ which(WCE01_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_TDg3[ which(WCE01_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_TDg2 <- rbind(WCE01_TDg2, addPlayers)

#Round 1, D Turnover graph using weighted edges
WCE01_TDft <- ftable(WCE01_TDg2$player1, WCE01_TDg2$player2)
WCE01_TDft2 <- as.matrix(WCE01_TDft)
numRows <- nrow(WCE01_TDft2)
numCols <- ncol(WCE01_TDft2)
WCE01_TDft3 <- WCE01_TDft2[c(2:numRows) , c(2:numCols)]
WCE01_TDTable <- graph.adjacency(WCE01_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, D Turnover graph=weighted
plot.igraph(WCE01_TDTable, vertex.label = V(WCE01_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, D Turnover calulation of network metrics
#igraph
WCE01_TD.clusterCoef <- transitivity(WCE01_TDTable, type="global") #cluster coefficient
WCE01_TD.degreeCent <- centralization.degree(WCE01_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_TDftn <- as.network.matrix(WCE01_TDft)
WCE01_TD.netDensity <- network.density(WCE01_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_TD.entropy <- entropy(WCE01_TDft) #entropy

WCE01_TD.netMx <- cbind(WCE01_TD.netMx, WCE01_TD.clusterCoef, WCE01_TD.degreeCent$centralization,
                        WCE01_TD.netDensity, WCE01_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_TD.netMx) <- varnames

#Round 1, End of Qtr**********************************************************
#NA

round = 1
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE01_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 1, End of Qtr with weighted edges
WCE01_QTg2 <- data.frame(WCE01_QT)
WCE01_QTg2 <- WCE01_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE01_QTg2$player1
player2vector <- WCE01_QTg2$player2
WCE01_QTg3 <- WCE01_QTg2
WCE01_QTg3$p1inp2vec <- is.element(WCE01_QTg3$player1, player2vector)
WCE01_QTg3$p2inp1vec <- is.element(WCE01_QTg3$player2, player1vector)

addPlayer1 <- WCE01_QTg3[ which(WCE01_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE01_QTg3[ which(WCE01_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE01_QTg2 <- rbind(WCE01_QTg2, addPlayers)

#Round 1, End of Qtr graph using weighted edges
WCE01_QTft <- ftable(WCE01_QTg2$player1, WCE01_QTg2$player2)
WCE01_QTft2 <- as.matrix(WCE01_QTft)
numRows <- nrow(WCE01_QTft2)
numCols <- ncol(WCE01_QTft2)
WCE01_QTft3 <- WCE01_QTft2[c(2:numRows) , c(2:numCols)]
WCE01_QTTable <- graph.adjacency(WCE01_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 1, End of Qtr graph=weighted
plot.igraph(WCE01_QTTable, vertex.label = V(WCE01_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE01_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 1, End of Qtr calulation of network metrics
#igraph
WCE01_QT.clusterCoef <- transitivity(WCE01_QTTable, type="global") #cluster coefficient
WCE01_QT.degreeCent <- centralization.degree(WCE01_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE01_QTftn <- as.network.matrix(WCE01_QTft)
WCE01_QT.netDensity <- network.density(WCE01_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE01_QT.entropy <- entropy(WCE01_QTft) #entropy

WCE01_QT.netMx <- cbind(WCE01_QT.netMx, WCE01_QT.clusterCoef, WCE01_QT.degreeCent$centralization,
                        WCE01_QT.netDensity, WCE01_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE01_QT.netMx) <- varnames
