#####
#09-23-16- Real data 03
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 3
##

#Round 3, Goal***************************************************************
#NA

round = 3
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
ADEL03_Gg2 <- data.frame(ADEL03_G)
ADEL03_Gg2 <- ADEL03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_Gg2$player1
player2vector <- ADEL03_Gg2$player2
ADEL03_Gg3 <- ADEL03_Gg2
ADEL03_Gg3$p1inp2vec <- is.element(ADEL03_Gg3$player1, player2vector)
ADEL03_Gg3$p2inp1vec <- is.element(ADEL03_Gg3$player2, player1vector)

addPlayer1 <- ADEL03_Gg3[ which(ADEL03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_Gg3[ which(ADEL03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_Gg2 <- rbind(ADEL03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
ADEL03_Gft <- ftable(ADEL03_Gg2$player1, ADEL03_Gg2$player2)
ADEL03_Gft2 <- as.matrix(ADEL03_Gft)
numRows <- nrow(ADEL03_Gft2)
numCols <- ncol(ADEL03_Gft2)
ADEL03_Gft3 <- ADEL03_Gft2[c(2:numRows) , c(2:numCols)]
ADEL03_GTable <- graph.adjacency(ADEL03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(ADEL03_GTable, vertex.label = V(ADEL03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
ADEL03_G.clusterCoef <- transitivity(ADEL03_GTable, type="global") #cluster coefficient
ADEL03_G.degreeCent <- centralization.degree(ADEL03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_Gftn <- as.network.matrix(ADEL03_Gft)
ADEL03_G.netDensity <- network.density(ADEL03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_G.entropy <- entropy(ADEL03_Gft) #entropy

ADEL03_G.netMx <- cbind(ADEL03_G.netMx, ADEL03_G.clusterCoef, ADEL03_G.degreeCent$centralization,
                        ADEL03_G.netDensity, ADEL03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
ADEL03_Bg2 <- data.frame(ADEL03_B)
ADEL03_Bg2 <- ADEL03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_Bg2$player1
player2vector <- ADEL03_Bg2$player2
ADEL03_Bg3 <- ADEL03_Bg2
ADEL03_Bg3$p1inp2vec <- is.element(ADEL03_Bg3$player1, player2vector)
ADEL03_Bg3$p2inp1vec <- is.element(ADEL03_Bg3$player2, player1vector)

addPlayer1 <- ADEL03_Bg3[ which(ADEL03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_Bg3[ which(ADEL03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_Bg2 <- rbind(ADEL03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
ADEL03_Bft <- ftable(ADEL03_Bg2$player1, ADEL03_Bg2$player2)
ADEL03_Bft2 <- as.matrix(ADEL03_Bft)
numRows <- nrow(ADEL03_Bft2)
numCols <- ncol(ADEL03_Bft2)
ADEL03_Bft3 <- ADEL03_Bft2[c(2:numRows) , c(2:numCols)]
ADEL03_BTable <- graph.adjacency(ADEL03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(ADEL03_BTable, vertex.label = V(ADEL03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
ADEL03_B.clusterCoef <- transitivity(ADEL03_BTable, type="global") #cluster coefficient
ADEL03_B.degreeCent <- centralization.degree(ADEL03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_Bftn <- as.network.matrix(ADEL03_Bft)
ADEL03_B.netDensity <- network.density(ADEL03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_B.entropy <- entropy(ADEL03_Bft) #entropy

ADEL03_B.netMx <- cbind(ADEL03_B.netMx, ADEL03_B.clusterCoef, ADEL03_B.degreeCent$centralization,
                        ADEL03_B.netDensity, ADEL03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
ADEL03_SFg2 <- data.frame(ADEL03_SF)
ADEL03_SFg2 <- ADEL03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_SFg2$player1
player2vector <- ADEL03_SFg2$player2
ADEL03_SFg3 <- ADEL03_SFg2
ADEL03_SFg3$p1inp2vec <- is.element(ADEL03_SFg3$player1, player2vector)
ADEL03_SFg3$p2inp1vec <- is.element(ADEL03_SFg3$player2, player1vector)

addPlayer1 <- ADEL03_SFg3[ which(ADEL03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_SFg3[ which(ADEL03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_SFg2 <- rbind(ADEL03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
ADEL03_SFft <- ftable(ADEL03_SFg2$player1, ADEL03_SFg2$player2)
ADEL03_SFft2 <- as.matrix(ADEL03_SFft)
numRows <- nrow(ADEL03_SFft2)
numCols <- ncol(ADEL03_SFft2)
ADEL03_SFft3 <- ADEL03_SFft2[c(2:numRows) , c(2:numCols)]
ADEL03_SFTable <- graph.adjacency(ADEL03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(ADEL03_SFTable, vertex.label = V(ADEL03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
ADEL03_SF.clusterCoef <- transitivity(ADEL03_SFTable, type="global") #cluster coefficient
ADEL03_SF.degreeCent <- centralization.degree(ADEL03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_SFftn <- as.network.matrix(ADEL03_SFft)
ADEL03_SF.netDensity <- network.density(ADEL03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_SF.entropy <- entropy(ADEL03_SFft) #entropy

ADEL03_SF.netMx <- cbind(ADEL03_SF.netMx, ADEL03_SF.clusterCoef, ADEL03_SF.degreeCent$centralization,
                         ADEL03_SF.netDensity, ADEL03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
ADEL03_TFg2 <- data.frame(ADEL03_TF)
ADEL03_TFg2 <- ADEL03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_TFg2$player1
player2vector <- ADEL03_TFg2$player2
ADEL03_TFg3 <- ADEL03_TFg2
ADEL03_TFg3$p1inp2vec <- is.element(ADEL03_TFg3$player1, player2vector)
ADEL03_TFg3$p2inp1vec <- is.element(ADEL03_TFg3$player2, player1vector)

addPlayer1 <- ADEL03_TFg3[ which(ADEL03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_TFg3[ which(ADEL03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_TFg2 <- rbind(ADEL03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
ADEL03_TFft <- ftable(ADEL03_TFg2$player1, ADEL03_TFg2$player2)
ADEL03_TFft2 <- as.matrix(ADEL03_TFft)
numRows <- nrow(ADEL03_TFft2)
numCols <- ncol(ADEL03_TFft2)
ADEL03_TFft3 <- ADEL03_TFft2[c(2:numRows) , c(2:numCols)]
ADEL03_TFTable <- graph.adjacency(ADEL03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(ADEL03_TFTable, vertex.label = V(ADEL03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
ADEL03_TF.clusterCoef <- transitivity(ADEL03_TFTable, type="global") #cluster coefficient
ADEL03_TF.degreeCent <- centralization.degree(ADEL03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_TFftn <- as.network.matrix(ADEL03_TFft)
ADEL03_TF.netDensity <- network.density(ADEL03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_TF.entropy <- entropy(ADEL03_TFft) #entropy

ADEL03_TF.netMx <- cbind(ADEL03_TF.netMx, ADEL03_TF.clusterCoef, ADEL03_TF.degreeCent$centralization,
                         ADEL03_TF.netDensity, ADEL03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
ADEL03_SAMg2 <- data.frame(ADEL03_SAM)
ADEL03_SAMg2 <- ADEL03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_SAMg2$player1
player2vector <- ADEL03_SAMg2$player2
ADEL03_SAMg3 <- ADEL03_SAMg2
ADEL03_SAMg3$p1inp2vec <- is.element(ADEL03_SAMg3$player1, player2vector)
ADEL03_SAMg3$p2inp1vec <- is.element(ADEL03_SAMg3$player2, player1vector)

addPlayer1 <- ADEL03_SAMg3[ which(ADEL03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_SAMg3[ which(ADEL03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_SAMg2 <- rbind(ADEL03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
ADEL03_SAMft <- ftable(ADEL03_SAMg2$player1, ADEL03_SAMg2$player2)
ADEL03_SAMft2 <- as.matrix(ADEL03_SAMft)
numRows <- nrow(ADEL03_SAMft2)
numCols <- ncol(ADEL03_SAMft2)
ADEL03_SAMft3 <- ADEL03_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL03_SAMTable <- graph.adjacency(ADEL03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(ADEL03_SAMTable, vertex.label = V(ADEL03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
ADEL03_SAM.clusterCoef <- transitivity(ADEL03_SAMTable, type="global") #cluster coefficient
ADEL03_SAM.degreeCent <- centralization.degree(ADEL03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_SAMftn <- as.network.matrix(ADEL03_SAMft)
ADEL03_SAM.netDensity <- network.density(ADEL03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_SAM.entropy <- entropy(ADEL03_SAMft) #entropy

ADEL03_SAM.netMx <- cbind(ADEL03_SAM.netMx, ADEL03_SAM.clusterCoef, ADEL03_SAM.degreeCent$centralization,
                          ADEL03_SAM.netDensity, ADEL03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************
#NA

round = 3
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
ADEL03_TAMg2 <- data.frame(ADEL03_TAM)
ADEL03_TAMg2 <- ADEL03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_TAMg2$player1
player2vector <- ADEL03_TAMg2$player2
ADEL03_TAMg3 <- ADEL03_TAMg2
ADEL03_TAMg3$p1inp2vec <- is.element(ADEL03_TAMg3$player1, player2vector)
ADEL03_TAMg3$p2inp1vec <- is.element(ADEL03_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL03_TAMg3[ which(ADEL03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL03_TAMg2 <- rbind(ADEL03_TAMg2, addPlayer2)

#Round 3, AM Turnover graph using weighted edges
ADEL03_TAMft <- ftable(ADEL03_TAMg2$player1, ADEL03_TAMg2$player2)
ADEL03_TAMft2 <- as.matrix(ADEL03_TAMft)
numRows <- nrow(ADEL03_TAMft2)
numCols <- ncol(ADEL03_TAMft2)
ADEL03_TAMft3 <- ADEL03_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL03_TAMTable <- graph.adjacency(ADEL03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(ADEL03_TAMTable, vertex.label = V(ADEL03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
ADEL03_TAM.clusterCoef <- transitivity(ADEL03_TAMTable, type="global") #cluster coefficient
ADEL03_TAM.degreeCent <- centralization.degree(ADEL03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_TAMftn <- as.network.matrix(ADEL03_TAMft)
ADEL03_TAM.netDensity <- network.density(ADEL03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_TAM.entropy <- entropy(ADEL03_TAMft) #entropy

ADEL03_TAM.netMx <- cbind(ADEL03_TAM.netMx, ADEL03_TAM.clusterCoef, ADEL03_TAM.degreeCent$centralization,
                          ADEL03_TAM.netDensity, ADEL03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
ADEL03_SDMg2 <- data.frame(ADEL03_SDM)
ADEL03_SDMg2 <- ADEL03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_SDMg2$player1
player2vector <- ADEL03_SDMg2$player2
ADEL03_SDMg3 <- ADEL03_SDMg2
ADEL03_SDMg3$p1inp2vec <- is.element(ADEL03_SDMg3$player1, player2vector)
ADEL03_SDMg3$p2inp1vec <- is.element(ADEL03_SDMg3$player2, player1vector)

addPlayer1 <- ADEL03_SDMg3[ which(ADEL03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_SDMg3[ which(ADEL03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_SDMg2 <- rbind(ADEL03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
ADEL03_SDMft <- ftable(ADEL03_SDMg2$player1, ADEL03_SDMg2$player2)
ADEL03_SDMft2 <- as.matrix(ADEL03_SDMft)
numRows <- nrow(ADEL03_SDMft2)
numCols <- ncol(ADEL03_SDMft2)
ADEL03_SDMft3 <- ADEL03_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL03_SDMTable <- graph.adjacency(ADEL03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(ADEL03_SDMTable, vertex.label = V(ADEL03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
ADEL03_SDM.clusterCoef <- transitivity(ADEL03_SDMTable, type="global") #cluster coefficient
ADEL03_SDM.degreeCent <- centralization.degree(ADEL03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_SDMftn <- as.network.matrix(ADEL03_SDMft)
ADEL03_SDM.netDensity <- network.density(ADEL03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_SDM.entropy <- entropy(ADEL03_SDMft) #entropy

ADEL03_SDM.netMx <- cbind(ADEL03_SDM.netMx, ADEL03_SDM.clusterCoef, ADEL03_SDM.degreeCent$centralization,
                          ADEL03_SDM.netDensity, ADEL03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
ADEL03_TDMg2 <- data.frame(ADEL03_TDM)
ADEL03_TDMg2 <- ADEL03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_TDMg2$player1
player2vector <- ADEL03_TDMg2$player2
ADEL03_TDMg3 <- ADEL03_TDMg2
ADEL03_TDMg3$p1inp2vec <- is.element(ADEL03_TDMg3$player1, player2vector)
ADEL03_TDMg3$p2inp1vec <- is.element(ADEL03_TDMg3$player2, player1vector)

addPlayer1 <- ADEL03_TDMg3[ which(ADEL03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_TDMg3[ which(ADEL03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_TDMg2 <- rbind(ADEL03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
ADEL03_TDMft <- ftable(ADEL03_TDMg2$player1, ADEL03_TDMg2$player2)
ADEL03_TDMft2 <- as.matrix(ADEL03_TDMft)
numRows <- nrow(ADEL03_TDMft2)
numCols <- ncol(ADEL03_TDMft2)
ADEL03_TDMft3 <- ADEL03_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL03_TDMTable <- graph.adjacency(ADEL03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(ADEL03_TDMTable, vertex.label = V(ADEL03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
ADEL03_TDM.clusterCoef <- transitivity(ADEL03_TDMTable, type="global") #cluster coefficient
ADEL03_TDM.degreeCent <- centralization.degree(ADEL03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_TDMftn <- as.network.matrix(ADEL03_TDMft)
ADEL03_TDM.netDensity <- network.density(ADEL03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_TDM.entropy <- entropy(ADEL03_TDMft) #entropy

ADEL03_TDM.netMx <- cbind(ADEL03_TDM.netMx, ADEL03_TDM.clusterCoef, ADEL03_TDM.degreeCent$centralization,
                          ADEL03_TDM.netDensity, ADEL03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
ADEL03_SDg2 <- data.frame(ADEL03_SD)
ADEL03_SDg2 <- ADEL03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_SDg2$player1
player2vector <- ADEL03_SDg2$player2
ADEL03_SDg3 <- ADEL03_SDg2
ADEL03_SDg3$p1inp2vec <- is.element(ADEL03_SDg3$player1, player2vector)
ADEL03_SDg3$p2inp1vec <- is.element(ADEL03_SDg3$player2, player1vector)

addPlayer1 <- ADEL03_SDg3[ which(ADEL03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_SDg3[ which(ADEL03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_SDg2 <- rbind(ADEL03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
ADEL03_SDft <- ftable(ADEL03_SDg2$player1, ADEL03_SDg2$player2)
ADEL03_SDft2 <- as.matrix(ADEL03_SDft)
numRows <- nrow(ADEL03_SDft2)
numCols <- ncol(ADEL03_SDft2)
ADEL03_SDft3 <- ADEL03_SDft2[c(2:numRows) , c(2:numCols)]
ADEL03_SDTable <- graph.adjacency(ADEL03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(ADEL03_SDTable, vertex.label = V(ADEL03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
ADEL03_SD.clusterCoef <- transitivity(ADEL03_SDTable, type="global") #cluster coefficient
ADEL03_SD.degreeCent <- centralization.degree(ADEL03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_SDftn <- as.network.matrix(ADEL03_SDft)
ADEL03_SD.netDensity <- network.density(ADEL03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_SD.entropy <- entropy(ADEL03_SDft) #entropy

ADEL03_SD.netMx <- cbind(ADEL03_SD.netMx, ADEL03_SD.clusterCoef, ADEL03_SD.degreeCent$centralization,
                         ADEL03_SD.netDensity, ADEL03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************

round = 3
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
ADEL03_TDg2 <- data.frame(ADEL03_TD)
ADEL03_TDg2 <- ADEL03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_TDg2$player1
player2vector <- ADEL03_TDg2$player2
ADEL03_TDg3 <- ADEL03_TDg2
ADEL03_TDg3$p1inp2vec <- is.element(ADEL03_TDg3$player1, player2vector)
ADEL03_TDg3$p2inp1vec <- is.element(ADEL03_TDg3$player2, player1vector)

addPlayer1 <- ADEL03_TDg3[ which(ADEL03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_TDg3[ which(ADEL03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_TDg2 <- rbind(ADEL03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
ADEL03_TDft <- ftable(ADEL03_TDg2$player1, ADEL03_TDg2$player2)
ADEL03_TDft2 <- as.matrix(ADEL03_TDft)
numRows <- nrow(ADEL03_TDft2)
numCols <- ncol(ADEL03_TDft2)
ADEL03_TDft3 <- ADEL03_TDft2[c(2:numRows) , c(2:numCols)]
ADEL03_TDTable <- graph.adjacency(ADEL03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(ADEL03_TDTable, vertex.label = V(ADEL03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
ADEL03_TD.clusterCoef <- transitivity(ADEL03_TDTable, type="global") #cluster coefficient
ADEL03_TD.degreeCent <- centralization.degree(ADEL03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_TDftn <- as.network.matrix(ADEL03_TDft)
ADEL03_TD.netDensity <- network.density(ADEL03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_TD.entropy <- entropy(ADEL03_TDft) #entropy

ADEL03_TD.netMx <- cbind(ADEL03_TD.netMx, ADEL03_TD.clusterCoef, ADEL03_TD.degreeCent$centralization,
                         ADEL03_TD.netDensity, ADEL03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
ADEL03_QTg2 <- data.frame(ADEL03_QT)
ADEL03_QTg2 <- ADEL03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL03_QTg2$player1
player2vector <- ADEL03_QTg2$player2
ADEL03_QTg3 <- ADEL03_QTg2
ADEL03_QTg3$p1inp2vec <- is.element(ADEL03_QTg3$player1, player2vector)
ADEL03_QTg3$p2inp1vec <- is.element(ADEL03_QTg3$player2, player1vector)

addPlayer1 <- ADEL03_QTg3[ which(ADEL03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL03_QTg3[ which(ADEL03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL03_QTg2 <- rbind(ADEL03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
ADEL03_QTft <- ftable(ADEL03_QTg2$player1, ADEL03_QTg2$player2)
ADEL03_QTft2 <- as.matrix(ADEL03_QTft)
numRows <- nrow(ADEL03_QTft2)
numCols <- ncol(ADEL03_QTft2)
ADEL03_QTft3 <- ADEL03_QTft2[c(2:numRows) , c(2:numCols)]
ADEL03_QTTable <- graph.adjacency(ADEL03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(ADEL03_QTTable, vertex.label = V(ADEL03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
ADEL03_QT.clusterCoef <- transitivity(ADEL03_QTTable, type="global") #cluster coefficient
ADEL03_QT.degreeCent <- centralization.degree(ADEL03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL03_QTftn <- as.network.matrix(ADEL03_QTft)
ADEL03_QT.netDensity <- network.density(ADEL03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL03_QT.entropy <- entropy(ADEL03_QTft) #entropy

ADEL03_QT.netMx <- cbind(ADEL03_QT.netMx, ADEL03_QT.clusterCoef, ADEL03_QT.degreeCent$centralization,
                         ADEL03_QT.netDensity, ADEL03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL03_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 3
##

#Round 3, Goal***************************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "Goal_F"
BL03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
BL03_Gg2 <- data.frame(BL03_G)
BL03_Gg2 <- BL03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_Gg2$player1
player2vector <- BL03_Gg2$player2
BL03_Gg3 <- BL03_Gg2
BL03_Gg3$p1inp2vec <- is.element(BL03_Gg3$player1, player2vector)
BL03_Gg3$p2inp1vec <- is.element(BL03_Gg3$player2, player1vector)

addPlayer1 <- BL03_Gg3[ which(BL03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_Gg3[ which(BL03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_Gg2 <- rbind(BL03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
BL03_Gft <- ftable(BL03_Gg2$player1, BL03_Gg2$player2)
BL03_Gft2 <- as.matrix(BL03_Gft)
numRows <- nrow(BL03_Gft2)
numCols <- ncol(BL03_Gft2)
BL03_Gft3 <- BL03_Gft2[c(2:numRows) , c(2:numCols)]
BL03_GTable <- graph.adjacency(BL03_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(BL03_GTable, vertex.label = V(BL03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
BL03_G.clusterCoef <- transitivity(BL03_GTable, type="global") #cluster coefficient
BL03_G.degreeCent <- centralization.degree(BL03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_Gftn <- as.network.matrix(BL03_Gft)
BL03_G.netDensity <- network.density(BL03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_G.entropy <- entropy(BL03_Gft) #entropy

BL03_G.netMx <- cbind(BL03_G.netMx, BL03_G.clusterCoef, BL03_G.degreeCent$centralization,
                      BL03_G.netDensity, BL03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "Behind_F"
BL03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
BL03_Bg2 <- data.frame(BL03_B)
BL03_Bg2 <- BL03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_Bg2$player1
player2vector <- BL03_Bg2$player2
BL03_Bg3 <- BL03_Bg2
BL03_Bg3$p1inp2vec <- is.element(BL03_Bg3$player1, player2vector)
BL03_Bg3$p2inp1vec <- is.element(BL03_Bg3$player2, player1vector)

addPlayer1 <- BL03_Bg3[ which(BL03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_Bg3[ which(BL03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_Bg2 <- rbind(BL03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
BL03_Bft <- ftable(BL03_Bg2$player1, BL03_Bg2$player2)
BL03_Bft2 <- as.matrix(BL03_Bft)
numRows <- nrow(BL03_Bft2)
numCols <- ncol(BL03_Bft2)
BL03_Bft3 <- BL03_Bft2[c(2:numRows) , c(2:numCols)]
BL03_BTable <- graph.adjacency(BL03_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(BL03_BTable, vertex.label = V(BL03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
BL03_B.clusterCoef <- transitivity(BL03_BTable, type="global") #cluster coefficient
BL03_B.degreeCent <- centralization.degree(BL03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_Bftn <- as.network.matrix(BL03_Bft)
BL03_B.netDensity <- network.density(BL03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_B.entropy <- entropy(BL03_Bft) #entropy

BL03_B.netMx <- cbind(BL03_B.netMx, BL03_B.clusterCoef, BL03_B.degreeCent$centralization,
                      BL03_B.netDensity, BL03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "Stoppage_F"
BL03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
BL03_SFg2 <- data.frame(BL03_SF)
BL03_SFg2 <- BL03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_SFg2$player1
player2vector <- BL03_SFg2$player2
BL03_SFg3 <- BL03_SFg2
BL03_SFg3$p1inp2vec <- is.element(BL03_SFg3$player1, player2vector)
BL03_SFg3$p2inp1vec <- is.element(BL03_SFg3$player2, player1vector)

addPlayer1 <- BL03_SFg3[ which(BL03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_SFg3[ which(BL03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_SFg2 <- rbind(BL03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
BL03_SFft <- ftable(BL03_SFg2$player1, BL03_SFg2$player2)
BL03_SFft2 <- as.matrix(BL03_SFft)
numRows <- nrow(BL03_SFft2)
numCols <- ncol(BL03_SFft2)
BL03_SFft3 <- BL03_SFft2[c(2:numRows) , c(2:numCols)]
BL03_SFTable <- graph.adjacency(BL03_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(BL03_SFTable, vertex.label = V(BL03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
BL03_SF.clusterCoef <- transitivity(BL03_SFTable, type="global") #cluster coefficient
BL03_SF.degreeCent <- centralization.degree(BL03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_SFftn <- as.network.matrix(BL03_SFft)
BL03_SF.netDensity <- network.density(BL03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_SF.entropy <- entropy(BL03_SFft) #entropy

BL03_SF.netMx <- cbind(BL03_SF.netMx, BL03_SF.clusterCoef, BL03_SF.degreeCent$centralization,
                       BL03_SF.netDensity, BL03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "BL"
KIoutcome = "Turnover_F"
BL03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
BL03_TFg2 <- data.frame(BL03_TF)
BL03_TFg2 <- BL03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_TFg2$player1
player2vector <- BL03_TFg2$player2
BL03_TFg3 <- BL03_TFg2
BL03_TFg3$p1inp2vec <- is.element(BL03_TFg3$player1, player2vector)
BL03_TFg3$p2inp1vec <- is.element(BL03_TFg3$player2, player1vector)

addPlayer1 <- BL03_TFg3[ which(BL03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

BL03_TFg2 <- rbind(BL03_TFg2, addPlayer1)

#Round 3, FWD Turnover graph using weighted edges
BL03_TFft <- ftable(BL03_TFg2$player1, BL03_TFg2$player2)
BL03_TFft2 <- as.matrix(BL03_TFft)
numRows <- nrow(BL03_TFft2)
numCols <- ncol(BL03_TFft2)
BL03_TFft3 <- BL03_TFft2[c(2:numRows) , c(1:numCols)]
BL03_TFTable <- graph.adjacency(BL03_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(BL03_TFTable, vertex.label = V(BL03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
BL03_TF.clusterCoef <- transitivity(BL03_TFTable, type="global") #cluster coefficient
BL03_TF.degreeCent <- centralization.degree(BL03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_TFftn <- as.network.matrix(BL03_TFft)
BL03_TF.netDensity <- network.density(BL03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_TF.entropy <- entropy(BL03_TFft) #entropy

BL03_TF.netMx <- cbind(BL03_TF.netMx, BL03_TF.clusterCoef, BL03_TF.degreeCent$centralization,
                       BL03_TF.netDensity, BL03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
BL03_SAMg2 <- data.frame(BL03_SAM)
BL03_SAMg2 <- BL03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_SAMg2$player1
player2vector <- BL03_SAMg2$player2
BL03_SAMg3 <- BL03_SAMg2
BL03_SAMg3$p1inp2vec <- is.element(BL03_SAMg3$player1, player2vector)
BL03_SAMg3$p2inp1vec <- is.element(BL03_SAMg3$player2, player1vector)

addPlayer1 <- BL03_SAMg3[ which(BL03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_SAMg3[ which(BL03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_SAMg2 <- rbind(BL03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
BL03_SAMft <- ftable(BL03_SAMg2$player1, BL03_SAMg2$player2)
BL03_SAMft2 <- as.matrix(BL03_SAMft)
numRows <- nrow(BL03_SAMft2)
numCols <- ncol(BL03_SAMft2)
BL03_SAMft3 <- BL03_SAMft2[c(2:numRows) , c(2:numCols)]
BL03_SAMTable <- graph.adjacency(BL03_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(BL03_SAMTable, vertex.label = V(BL03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
BL03_SAM.clusterCoef <- transitivity(BL03_SAMTable, type="global") #cluster coefficient
BL03_SAM.degreeCent <- centralization.degree(BL03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_SAMftn <- as.network.matrix(BL03_SAMft)
BL03_SAM.netDensity <- network.density(BL03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_SAM.entropy <- entropy(BL03_SAMft) #entropy

BL03_SAM.netMx <- cbind(BL03_SAM.netMx, BL03_SAM.clusterCoef, BL03_SAM.degreeCent$centralization,
                        BL03_SAM.netDensity, BL03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "Turnover_AM"
BL03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
BL03_TAMg2 <- data.frame(BL03_TAM)
BL03_TAMg2 <- BL03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_TAMg2$player1
player2vector <- BL03_TAMg2$player2
BL03_TAMg3 <- BL03_TAMg2
BL03_TAMg3$p1inp2vec <- is.element(BL03_TAMg3$player1, player2vector)
BL03_TAMg3$p2inp1vec <- is.element(BL03_TAMg3$player2, player1vector)

addPlayer1 <- BL03_TAMg3[ which(BL03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_TAMg3[ which(BL03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_TAMg2 <- rbind(BL03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
BL03_TAMft <- ftable(BL03_TAMg2$player1, BL03_TAMg2$player2)
BL03_TAMft2 <- as.matrix(BL03_TAMft)
numRows <- nrow(BL03_TAMft2)
numCols <- ncol(BL03_TAMft2)
BL03_TAMft3 <- BL03_TAMft2[c(2:numRows) , c(2:numCols)]
BL03_TAMTable <- graph.adjacency(BL03_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(BL03_TAMTable, vertex.label = V(BL03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
BL03_TAM.clusterCoef <- transitivity(BL03_TAMTable, type="global") #cluster coefficient
BL03_TAM.degreeCent <- centralization.degree(BL03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_TAMftn <- as.network.matrix(BL03_TAMft)
BL03_TAM.netDensity <- network.density(BL03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_TAM.entropy <- entropy(BL03_TAMft) #entropy

BL03_TAM.netMx <- cbind(BL03_TAM.netMx, BL03_TAM.clusterCoef, BL03_TAM.degreeCent$centralization,
                        BL03_TAM.netDensity, BL03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
BL03_SDMg2 <- data.frame(BL03_SDM)
BL03_SDMg2 <- BL03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_SDMg2$player1
player2vector <- BL03_SDMg2$player2
BL03_SDMg3 <- BL03_SDMg2
BL03_SDMg3$p1inp2vec <- is.element(BL03_SDMg3$player1, player2vector)
BL03_SDMg3$p2inp1vec <- is.element(BL03_SDMg3$player2, player1vector)

addPlayer1 <- BL03_SDMg3[ which(BL03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_SDMg3[ which(BL03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_SDMg2 <- rbind(BL03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
BL03_SDMft <- ftable(BL03_SDMg2$player1, BL03_SDMg2$player2)
BL03_SDMft2 <- as.matrix(BL03_SDMft)
numRows <- nrow(BL03_SDMft2)
numCols <- ncol(BL03_SDMft2)
BL03_SDMft3 <- BL03_SDMft2[c(2:numRows) , c(2:numCols)]
BL03_SDMTable <- graph.adjacency(BL03_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(BL03_SDMTable, vertex.label = V(BL03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
BL03_SDM.clusterCoef <- transitivity(BL03_SDMTable, type="global") #cluster coefficient
BL03_SDM.degreeCent <- centralization.degree(BL03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_SDMftn <- as.network.matrix(BL03_SDMft)
BL03_SDM.netDensity <- network.density(BL03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_SDM.entropy <- entropy(BL03_SDMft) #entropy

BL03_SDM.netMx <- cbind(BL03_SDM.netMx, BL03_SDM.clusterCoef, BL03_SDM.degreeCent$centralization,
                        BL03_SDM.netDensity, BL03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "BL"
KIoutcome = "Turnover_DM"
BL03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
BL03_TDMg2 <- data.frame(BL03_TDM)
BL03_TDMg2 <- BL03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_TDMg2$player1
player2vector <- BL03_TDMg2$player2
BL03_TDMg3 <- BL03_TDMg2
BL03_TDMg3$p1inp2vec <- is.element(BL03_TDMg3$player1, player2vector)
BL03_TDMg3$p2inp1vec <- is.element(BL03_TDMg3$player2, player1vector)

addPlayer1 <- BL03_TDMg3[ which(BL03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_TDMg3[ which(BL03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_TDMg2 <- rbind(BL03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
BL03_TDMft <- ftable(BL03_TDMg2$player1, BL03_TDMg2$player2)
BL03_TDMft2 <- as.matrix(BL03_TDMft)
numRows <- nrow(BL03_TDMft2)
numCols <- ncol(BL03_TDMft2)
BL03_TDMft3 <- BL03_TDMft2[c(2:numRows) , c(2:numCols)]
BL03_TDMTable <- graph.adjacency(BL03_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(BL03_TDMTable, vertex.label = V(BL03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
BL03_TDM.clusterCoef <- transitivity(BL03_TDMTable, type="global") #cluster coefficient
BL03_TDM.degreeCent <- centralization.degree(BL03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_TDMftn <- as.network.matrix(BL03_TDMft)
BL03_TDM.netDensity <- network.density(BL03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_TDM.entropy <- entropy(BL03_TDMft) #entropy

BL03_TDM.netMx <- cbind(BL03_TDM.netMx, BL03_TDM.clusterCoef, BL03_TDM.degreeCent$centralization,
                        BL03_TDM.netDensity, BL03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "Stoppage_D"
BL03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
BL03_SDg2 <- data.frame(BL03_SD)
BL03_SDg2 <- BL03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_SDg2$player1
player2vector <- BL03_SDg2$player2
BL03_SDg3 <- BL03_SDg2
BL03_SDg3$p1inp2vec <- is.element(BL03_SDg3$player1, player2vector)
BL03_SDg3$p2inp1vec <- is.element(BL03_SDg3$player2, player1vector)

addPlayer1 <- BL03_SDg3[ which(BL03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_SDg3[ which(BL03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_SDg2 <- rbind(BL03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
BL03_SDft <- ftable(BL03_SDg2$player1, BL03_SDg2$player2)
BL03_SDft2 <- as.matrix(BL03_SDft)
numRows <- nrow(BL03_SDft2)
numCols <- ncol(BL03_SDft2)
BL03_SDft3 <- BL03_SDft2[c(2:numRows) , c(2:numCols)]
BL03_SDTable <- graph.adjacency(BL03_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(BL03_SDTable, vertex.label = V(BL03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
BL03_SD.clusterCoef <- transitivity(BL03_SDTable, type="global") #cluster coefficient
BL03_SD.degreeCent <- centralization.degree(BL03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_SDftn <- as.network.matrix(BL03_SDft)
BL03_SD.netDensity <- network.density(BL03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_SD.entropy <- entropy(BL03_SDft) #entropy

BL03_SD.netMx <- cbind(BL03_SD.netMx, BL03_SD.clusterCoef, BL03_SD.degreeCent$centralization,
                       BL03_SD.netDensity, BL03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************

round = 3
teamName = "BL"
KIoutcome = "Turnover_D"
BL03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
BL03_TDg2 <- data.frame(BL03_TD)
BL03_TDg2 <- BL03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_TDg2$player1
player2vector <- BL03_TDg2$player2
BL03_TDg3 <- BL03_TDg2
BL03_TDg3$p1inp2vec <- is.element(BL03_TDg3$player1, player2vector)
BL03_TDg3$p2inp1vec <- is.element(BL03_TDg3$player2, player1vector)

addPlayer1 <- BL03_TDg3[ which(BL03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_TDg3[ which(BL03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_TDg2 <- rbind(BL03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
BL03_TDft <- ftable(BL03_TDg2$player1, BL03_TDg2$player2)
BL03_TDft2 <- as.matrix(BL03_TDft)
numRows <- nrow(BL03_TDft2)
numCols <- ncol(BL03_TDft2)
BL03_TDft3 <- BL03_TDft2[c(2:numRows) , c(2:numCols)]
BL03_TDTable <- graph.adjacency(BL03_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(BL03_TDTable, vertex.label = V(BL03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
BL03_TD.clusterCoef <- transitivity(BL03_TDTable, type="global") #cluster coefficient
BL03_TD.degreeCent <- centralization.degree(BL03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_TDftn <- as.network.matrix(BL03_TDft)
BL03_TD.netDensity <- network.density(BL03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_TD.entropy <- entropy(BL03_TDft) #entropy

BL03_TD.netMx <- cbind(BL03_TD.netMx, BL03_TD.clusterCoef, BL03_TD.degreeCent$centralization,
                       BL03_TD.netDensity, BL03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
BL03_QTg2 <- data.frame(BL03_QT)
BL03_QTg2 <- BL03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL03_QTg2$player1
player2vector <- BL03_QTg2$player2
BL03_QTg3 <- BL03_QTg2
BL03_QTg3$p1inp2vec <- is.element(BL03_QTg3$player1, player2vector)
BL03_QTg3$p2inp1vec <- is.element(BL03_QTg3$player2, player1vector)

addPlayer1 <- BL03_QTg3[ which(BL03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL03_QTg3[ which(BL03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL03_QTg2 <- rbind(BL03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
BL03_QTft <- ftable(BL03_QTg2$player1, BL03_QTg2$player2)
BL03_QTft2 <- as.matrix(BL03_QTft)
numRows <- nrow(BL03_QTft2)
numCols <- ncol(BL03_QTft2)
BL03_QTft3 <- BL03_QTft2[c(2:numRows) , c(2:numCols)]
BL03_QTTable <- graph.adjacency(BL03_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(BL03_QTTable, vertex.label = V(BL03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
BL03_QT.clusterCoef <- transitivity(BL03_QTTable, type="global") #cluster coefficient
BL03_QT.degreeCent <- centralization.degree(BL03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL03_QTftn <- as.network.matrix(BL03_QTft)
BL03_QT.netDensity <- network.density(BL03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL03_QT.entropy <- entropy(BL03_QTft) #entropy

BL03_QT.netMx <- cbind(BL03_QT.netMx, BL03_QT.clusterCoef, BL03_QT.degreeCent$centralization,
                       BL03_QT.netDensity, BL03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL03_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 3
##

#Round 3, Goal***************************************************************
#NA

round = 3
teamName = "CARL"
KIoutcome = "Goal_F"
CARL03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
CARL03_Gg2 <- data.frame(CARL03_G)
CARL03_Gg2 <- CARL03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_Gg2$player1
player2vector <- CARL03_Gg2$player2
CARL03_Gg3 <- CARL03_Gg2
CARL03_Gg3$p1inp2vec <- is.element(CARL03_Gg3$player1, player2vector)
CARL03_Gg3$p2inp1vec <- is.element(CARL03_Gg3$player2, player1vector)

addPlayer1 <- CARL03_Gg3[ which(CARL03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_Gg3[ which(CARL03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_Gg2 <- rbind(CARL03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
CARL03_Gft <- ftable(CARL03_Gg2$player1, CARL03_Gg2$player2)
CARL03_Gft2 <- as.matrix(CARL03_Gft)
numRows <- nrow(CARL03_Gft2)
numCols <- ncol(CARL03_Gft2)
CARL03_Gft3 <- CARL03_Gft2[c(2:numRows) , c(2:numCols)]
CARL03_GTable <- graph.adjacency(CARL03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(CARL03_GTable, vertex.label = V(CARL03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
CARL03_G.clusterCoef <- transitivity(CARL03_GTable, type="global") #cluster coefficient
CARL03_G.degreeCent <- centralization.degree(CARL03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_Gftn <- as.network.matrix(CARL03_Gft)
CARL03_G.netDensity <- network.density(CARL03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_G.entropy <- entropy(CARL03_Gft) #entropy

CARL03_G.netMx <- cbind(CARL03_G.netMx, CARL03_G.clusterCoef, CARL03_G.degreeCent$centralization,
                        CARL03_G.netDensity, CARL03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "CARL"
KIoutcome = "Behind_F"
CARL03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
CARL03_Bg2 <- data.frame(CARL03_B)
CARL03_Bg2 <- CARL03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_Bg2$player1
player2vector <- CARL03_Bg2$player2
CARL03_Bg3 <- CARL03_Bg2
CARL03_Bg3$p1inp2vec <- is.element(CARL03_Bg3$player1, player2vector)
CARL03_Bg3$p2inp1vec <- is.element(CARL03_Bg3$player2, player1vector)

addPlayer1 <- CARL03_Bg3[ which(CARL03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_Bg3[ which(CARL03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_Bg2 <- rbind(CARL03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
CARL03_Bft <- ftable(CARL03_Bg2$player1, CARL03_Bg2$player2)
CARL03_Bft2 <- as.matrix(CARL03_Bft)
numRows <- nrow(CARL03_Bft2)
numCols <- ncol(CARL03_Bft2)
CARL03_Bft3 <- CARL03_Bft2[c(2:numRows) , c(2:numCols)]
CARL03_BTable <- graph.adjacency(CARL03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(CARL03_BTable, vertex.label = V(CARL03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
CARL03_B.clusterCoef <- transitivity(CARL03_BTable, type="global") #cluster coefficient
CARL03_B.degreeCent <- centralization.degree(CARL03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_Bftn <- as.network.matrix(CARL03_Bft)
CARL03_B.netDensity <- network.density(CARL03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_B.entropy <- entropy(CARL03_Bft) #entropy

CARL03_B.netMx <- cbind(CARL03_B.netMx, CARL03_B.clusterCoef, CARL03_B.degreeCent$centralization,
                        CARL03_B.netDensity, CARL03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
CARL03_SFg2 <- data.frame(CARL03_SF)
CARL03_SFg2 <- CARL03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_SFg2$player1
player2vector <- CARL03_SFg2$player2
CARL03_SFg3 <- CARL03_SFg2
CARL03_SFg3$p1inp2vec <- is.element(CARL03_SFg3$player1, player2vector)
CARL03_SFg3$p2inp1vec <- is.element(CARL03_SFg3$player2, player1vector)

addPlayer1 <- CARL03_SFg3[ which(CARL03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_SFg3[ which(CARL03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_SFg2 <- rbind(CARL03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
CARL03_SFft <- ftable(CARL03_SFg2$player1, CARL03_SFg2$player2)
CARL03_SFft2 <- as.matrix(CARL03_SFft)
numRows <- nrow(CARL03_SFft2)
numCols <- ncol(CARL03_SFft2)
CARL03_SFft3 <- CARL03_SFft2[c(2:numRows) , c(2:numCols)]
CARL03_SFTable <- graph.adjacency(CARL03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(CARL03_SFTable, vertex.label = V(CARL03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
CARL03_SF.clusterCoef <- transitivity(CARL03_SFTable, type="global") #cluster coefficient
CARL03_SF.degreeCent <- centralization.degree(CARL03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_SFftn <- as.network.matrix(CARL03_SFft)
CARL03_SF.netDensity <- network.density(CARL03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_SF.entropy <- entropy(CARL03_SFft) #entropy

CARL03_SF.netMx <- cbind(CARL03_SF.netMx, CARL03_SF.clusterCoef, CARL03_SF.degreeCent$centralization,
                         CARL03_SF.netDensity, CARL03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
CARL03_TFg2 <- data.frame(CARL03_TF)
CARL03_TFg2 <- CARL03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_TFg2$player1
player2vector <- CARL03_TFg2$player2
CARL03_TFg3 <- CARL03_TFg2
CARL03_TFg3$p1inp2vec <- is.element(CARL03_TFg3$player1, player2vector)
CARL03_TFg3$p2inp1vec <- is.element(CARL03_TFg3$player2, player1vector)

addPlayer1 <- CARL03_TFg3[ which(CARL03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_TFg3[ which(CARL03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_TFg2 <- rbind(CARL03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
CARL03_TFft <- ftable(CARL03_TFg2$player1, CARL03_TFg2$player2)
CARL03_TFft2 <- as.matrix(CARL03_TFft)
numRows <- nrow(CARL03_TFft2)
numCols <- ncol(CARL03_TFft2)
CARL03_TFft3 <- CARL03_TFft2[c(2:numRows) , c(2:numCols)]
CARL03_TFTable <- graph.adjacency(CARL03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(CARL03_TFTable, vertex.label = V(CARL03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
CARL03_TF.clusterCoef <- transitivity(CARL03_TFTable, type="global") #cluster coefficient
CARL03_TF.degreeCent <- centralization.degree(CARL03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_TFftn <- as.network.matrix(CARL03_TFft)
CARL03_TF.netDensity <- network.density(CARL03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_TF.entropy <- entropy(CARL03_TFft) #entropy

CARL03_TF.netMx <- cbind(CARL03_TF.netMx, CARL03_TF.clusterCoef, CARL03_TF.degreeCent$centralization,
                         CARL03_TF.netDensity, CARL03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
CARL03_SAMg2 <- data.frame(CARL03_SAM)
CARL03_SAMg2 <- CARL03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_SAMg2$player1
player2vector <- CARL03_SAMg2$player2
CARL03_SAMg3 <- CARL03_SAMg2
CARL03_SAMg3$p1inp2vec <- is.element(CARL03_SAMg3$player1, player2vector)
CARL03_SAMg3$p2inp1vec <- is.element(CARL03_SAMg3$player2, player1vector)

addPlayer1 <- CARL03_SAMg3[ which(CARL03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_SAMg3[ which(CARL03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_SAMg2 <- rbind(CARL03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
CARL03_SAMft <- ftable(CARL03_SAMg2$player1, CARL03_SAMg2$player2)
CARL03_SAMft2 <- as.matrix(CARL03_SAMft)
numRows <- nrow(CARL03_SAMft2)
numCols <- ncol(CARL03_SAMft2)
CARL03_SAMft3 <- CARL03_SAMft2[c(2:numRows) , c(2:numCols)]
CARL03_SAMTable <- graph.adjacency(CARL03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(CARL03_SAMTable, vertex.label = V(CARL03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
CARL03_SAM.clusterCoef <- transitivity(CARL03_SAMTable, type="global") #cluster coefficient
CARL03_SAM.degreeCent <- centralization.degree(CARL03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_SAMftn <- as.network.matrix(CARL03_SAMft)
CARL03_SAM.netDensity <- network.density(CARL03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_SAM.entropy <- entropy(CARL03_SAMft) #entropy

CARL03_SAM.netMx <- cbind(CARL03_SAM.netMx, CARL03_SAM.clusterCoef, CARL03_SAM.degreeCent$centralization,
                          CARL03_SAM.netDensity, CARL03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
CARL03_TAMg2 <- data.frame(CARL03_TAM)
CARL03_TAMg2 <- CARL03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_TAMg2$player1
player2vector <- CARL03_TAMg2$player2
CARL03_TAMg3 <- CARL03_TAMg2
CARL03_TAMg3$p1inp2vec <- is.element(CARL03_TAMg3$player1, player2vector)
CARL03_TAMg3$p2inp1vec <- is.element(CARL03_TAMg3$player2, player1vector)

addPlayer1 <- CARL03_TAMg3[ which(CARL03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_TAMg3[ which(CARL03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_TAMg2 <- rbind(CARL03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
CARL03_TAMft <- ftable(CARL03_TAMg2$player1, CARL03_TAMg2$player2)
CARL03_TAMft2 <- as.matrix(CARL03_TAMft)
numRows <- nrow(CARL03_TAMft2)
numCols <- ncol(CARL03_TAMft2)
CARL03_TAMft3 <- CARL03_TAMft2[c(2:numRows) , c(2:numCols)]
CARL03_TAMTable <- graph.adjacency(CARL03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(CARL03_TAMTable, vertex.label = V(CARL03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
CARL03_TAM.clusterCoef <- transitivity(CARL03_TAMTable, type="global") #cluster coefficient
CARL03_TAM.degreeCent <- centralization.degree(CARL03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_TAMftn <- as.network.matrix(CARL03_TAMft)
CARL03_TAM.netDensity <- network.density(CARL03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_TAM.entropy <- entropy(CARL03_TAMft) #entropy

CARL03_TAM.netMx <- cbind(CARL03_TAM.netMx, CARL03_TAM.clusterCoef, CARL03_TAM.degreeCent$centralization,
                          CARL03_TAM.netDensity, CARL03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
CARL03_SDMg2 <- data.frame(CARL03_SDM)
CARL03_SDMg2 <- CARL03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_SDMg2$player1
player2vector <- CARL03_SDMg2$player2
CARL03_SDMg3 <- CARL03_SDMg2
CARL03_SDMg3$p1inp2vec <- is.element(CARL03_SDMg3$player1, player2vector)
CARL03_SDMg3$p2inp1vec <- is.element(CARL03_SDMg3$player2, player1vector)

addPlayer1 <- CARL03_SDMg3[ which(CARL03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_SDMg3[ which(CARL03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_SDMg2 <- rbind(CARL03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
CARL03_SDMft <- ftable(CARL03_SDMg2$player1, CARL03_SDMg2$player2)
CARL03_SDMft2 <- as.matrix(CARL03_SDMft)
numRows <- nrow(CARL03_SDMft2)
numCols <- ncol(CARL03_SDMft2)
CARL03_SDMft3 <- CARL03_SDMft2[c(2:numRows) , c(2:numCols)]
CARL03_SDMTable <- graph.adjacency(CARL03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(CARL03_SDMTable, vertex.label = V(CARL03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
CARL03_SDM.clusterCoef <- transitivity(CARL03_SDMTable, type="global") #cluster coefficient
CARL03_SDM.degreeCent <- centralization.degree(CARL03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_SDMftn <- as.network.matrix(CARL03_SDMft)
CARL03_SDM.netDensity <- network.density(CARL03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_SDM.entropy <- entropy(CARL03_SDMft) #entropy

CARL03_SDM.netMx <- cbind(CARL03_SDM.netMx, CARL03_SDM.clusterCoef, CARL03_SDM.degreeCent$centralization,
                          CARL03_SDM.netDensity, CARL03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
CARL03_TDMg2 <- data.frame(CARL03_TDM)
CARL03_TDMg2 <- CARL03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_TDMg2$player1
player2vector <- CARL03_TDMg2$player2
CARL03_TDMg3 <- CARL03_TDMg2
CARL03_TDMg3$p1inp2vec <- is.element(CARL03_TDMg3$player1, player2vector)
CARL03_TDMg3$p2inp1vec <- is.element(CARL03_TDMg3$player2, player1vector)

addPlayer1 <- CARL03_TDMg3[ which(CARL03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_TDMg3[ which(CARL03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_TDMg2 <- rbind(CARL03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
CARL03_TDMft <- ftable(CARL03_TDMg2$player1, CARL03_TDMg2$player2)
CARL03_TDMft2 <- as.matrix(CARL03_TDMft)
numRows <- nrow(CARL03_TDMft2)
numCols <- ncol(CARL03_TDMft2)
CARL03_TDMft3 <- CARL03_TDMft2[c(2:numRows) , c(2:numCols)]
CARL03_TDMTable <- graph.adjacency(CARL03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(CARL03_TDMTable, vertex.label = V(CARL03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
CARL03_TDM.clusterCoef <- transitivity(CARL03_TDMTable, type="global") #cluster coefficient
CARL03_TDM.degreeCent <- centralization.degree(CARL03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_TDMftn <- as.network.matrix(CARL03_TDMft)
CARL03_TDM.netDensity <- network.density(CARL03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_TDM.entropy <- entropy(CARL03_TDMft) #entropy

CARL03_TDM.netMx <- cbind(CARL03_TDM.netMx, CARL03_TDM.clusterCoef, CARL03_TDM.degreeCent$centralization,
                          CARL03_TDM.netDensity, CARL03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
CARL03_SDg2 <- data.frame(CARL03_SD)
CARL03_SDg2 <- CARL03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_SDg2$player1
player2vector <- CARL03_SDg2$player2
CARL03_SDg3 <- CARL03_SDg2
CARL03_SDg3$p1inp2vec <- is.element(CARL03_SDg3$player1, player2vector)
CARL03_SDg3$p2inp1vec <- is.element(CARL03_SDg3$player2, player1vector)

addPlayer1 <- CARL03_SDg3[ which(CARL03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_SDg3[ which(CARL03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_SDg2 <- rbind(CARL03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
CARL03_SDft <- ftable(CARL03_SDg2$player1, CARL03_SDg2$player2)
CARL03_SDft2 <- as.matrix(CARL03_SDft)
numRows <- nrow(CARL03_SDft2)
numCols <- ncol(CARL03_SDft2)
CARL03_SDft3 <- CARL03_SDft2[c(2:numRows) , c(2:numCols)]
CARL03_SDTable <- graph.adjacency(CARL03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(CARL03_SDTable, vertex.label = V(CARL03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
CARL03_SD.clusterCoef <- transitivity(CARL03_SDTable, type="global") #cluster coefficient
CARL03_SD.degreeCent <- centralization.degree(CARL03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_SDftn <- as.network.matrix(CARL03_SDft)
CARL03_SD.netDensity <- network.density(CARL03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_SD.entropy <- entropy(CARL03_SDft) #entropy

CARL03_SD.netMx <- cbind(CARL03_SD.netMx, CARL03_SD.clusterCoef, CARL03_SD.degreeCent$centralization,
                         CARL03_SD.netDensity, CARL03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
CARL03_TDg2 <- data.frame(CARL03_TD)
CARL03_TDg2 <- CARL03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_TDg2$player1
player2vector <- CARL03_TDg2$player2
CARL03_TDg3 <- CARL03_TDg2
CARL03_TDg3$p1inp2vec <- is.element(CARL03_TDg3$player1, player2vector)
CARL03_TDg3$p2inp1vec <- is.element(CARL03_TDg3$player2, player1vector)

addPlayer1 <- CARL03_TDg3[ which(CARL03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_TDg3[ which(CARL03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_TDg2 <- rbind(CARL03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
CARL03_TDft <- ftable(CARL03_TDg2$player1, CARL03_TDg2$player2)
CARL03_TDft2 <- as.matrix(CARL03_TDft)
numRows <- nrow(CARL03_TDft2)
numCols <- ncol(CARL03_TDft2)
CARL03_TDft3 <- CARL03_TDft2[c(2:numRows) , c(2:numCols)]
CARL03_TDTable <- graph.adjacency(CARL03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(CARL03_TDTable, vertex.label = V(CARL03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
CARL03_TD.clusterCoef <- transitivity(CARL03_TDTable, type="global") #cluster coefficient
CARL03_TD.degreeCent <- centralization.degree(CARL03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_TDftn <- as.network.matrix(CARL03_TDft)
CARL03_TD.netDensity <- network.density(CARL03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_TD.entropy <- entropy(CARL03_TDft) #entropy

CARL03_TD.netMx <- cbind(CARL03_TD.netMx, CARL03_TD.clusterCoef, CARL03_TD.degreeCent$centralization,
                         CARL03_TD.netDensity, CARL03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************

round = 3
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
CARL03_QTg2 <- data.frame(CARL03_QT)
CARL03_QTg2 <- CARL03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL03_QTg2$player1
player2vector <- CARL03_QTg2$player2
CARL03_QTg3 <- CARL03_QTg2
CARL03_QTg3$p1inp2vec <- is.element(CARL03_QTg3$player1, player2vector)
CARL03_QTg3$p2inp1vec <- is.element(CARL03_QTg3$player2, player1vector)

addPlayer1 <- CARL03_QTg3[ which(CARL03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL03_QTg3[ which(CARL03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL03_QTg2 <- rbind(CARL03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
CARL03_QTft <- ftable(CARL03_QTg2$player1, CARL03_QTg2$player2)
CARL03_QTft2 <- as.matrix(CARL03_QTft)
numRows <- nrow(CARL03_QTft2)
numCols <- ncol(CARL03_QTft2)
CARL03_QTft3 <- CARL03_QTft2[c(2:numRows) , c(2:numCols)]
CARL03_QTTable <- graph.adjacency(CARL03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(CARL03_QTTable, vertex.label = V(CARL03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
CARL03_QT.clusterCoef <- transitivity(CARL03_QTTable, type="global") #cluster coefficient
CARL03_QT.degreeCent <- centralization.degree(CARL03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL03_QTftn <- as.network.matrix(CARL03_QTft)
CARL03_QT.netDensity <- network.density(CARL03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL03_QT.entropy <- entropy(CARL03_QTft) #entropy

CARL03_QT.netMx <- cbind(CARL03_QT.netMx, CARL03_QT.clusterCoef, CARL03_QT.degreeCent$centralization,
                         CARL03_QT.netDensity, CARL03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL03_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 3
##

#Round 3, Goal***************************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Goal_F"
COLL03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
COLL03_Gg2 <- data.frame(COLL03_G)
COLL03_Gg2 <- COLL03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_Gg2$player1
player2vector <- COLL03_Gg2$player2
COLL03_Gg3 <- COLL03_Gg2
COLL03_Gg3$p1inp2vec <- is.element(COLL03_Gg3$player1, player2vector)
COLL03_Gg3$p2inp1vec <- is.element(COLL03_Gg3$player2, player1vector)

addPlayer1 <- COLL03_Gg3[ which(COLL03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_Gg3[ which(COLL03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_Gg2 <- rbind(COLL03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
COLL03_Gft <- ftable(COLL03_Gg2$player1, COLL03_Gg2$player2)
COLL03_Gft2 <- as.matrix(COLL03_Gft)
numRows <- nrow(COLL03_Gft2)
numCols <- ncol(COLL03_Gft2)
COLL03_Gft3 <- COLL03_Gft2[c(2:numRows) , c(2:numCols)]
COLL03_GTable <- graph.adjacency(COLL03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(COLL03_GTable, vertex.label = V(COLL03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
COLL03_G.clusterCoef <- transitivity(COLL03_GTable, type="global") #cluster coefficient
COLL03_G.degreeCent <- centralization.degree(COLL03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_Gftn <- as.network.matrix(COLL03_Gft)
COLL03_G.netDensity <- network.density(COLL03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_G.entropy <- entropy(COLL03_Gft) #entropy

COLL03_G.netMx <- cbind(COLL03_G.netMx, COLL03_G.clusterCoef, COLL03_G.degreeCent$centralization,
                        COLL03_G.netDensity, COLL03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Behind_F"
COLL03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
COLL03_Bg2 <- data.frame(COLL03_B)
COLL03_Bg2 <- COLL03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_Bg2$player1
player2vector <- COLL03_Bg2$player2
COLL03_Bg3 <- COLL03_Bg2
COLL03_Bg3$p1inp2vec <- is.element(COLL03_Bg3$player1, player2vector)
COLL03_Bg3$p2inp1vec <- is.element(COLL03_Bg3$player2, player1vector)

addPlayer1 <- COLL03_Bg3[ which(COLL03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_Bg3[ which(COLL03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_Bg2 <- rbind(COLL03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
COLL03_Bft <- ftable(COLL03_Bg2$player1, COLL03_Bg2$player2)
COLL03_Bft2 <- as.matrix(COLL03_Bft)
numRows <- nrow(COLL03_Bft2)
numCols <- ncol(COLL03_Bft2)
COLL03_Bft3 <- COLL03_Bft2[c(2:numRows) , c(2:numCols)]
COLL03_BTable <- graph.adjacency(COLL03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(COLL03_BTable, vertex.label = V(COLL03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
COLL03_B.clusterCoef <- transitivity(COLL03_BTable, type="global") #cluster coefficient
COLL03_B.degreeCent <- centralization.degree(COLL03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_Bftn <- as.network.matrix(COLL03_Bft)
COLL03_B.netDensity <- network.density(COLL03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_B.entropy <- entropy(COLL03_Bft) #entropy

COLL03_B.netMx <- cbind(COLL03_B.netMx, COLL03_B.clusterCoef, COLL03_B.degreeCent$centralization,
                        COLL03_B.netDensity, COLL03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
COLL03_SFg2 <- data.frame(COLL03_SF)
COLL03_SFg2 <- COLL03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_SFg2$player1
player2vector <- COLL03_SFg2$player2
COLL03_SFg3 <- COLL03_SFg2
COLL03_SFg3$p1inp2vec <- is.element(COLL03_SFg3$player1, player2vector)
COLL03_SFg3$p2inp1vec <- is.element(COLL03_SFg3$player2, player1vector)

addPlayer1 <- COLL03_SFg3[ which(COLL03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_SFg3[ which(COLL03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_SFg2 <- rbind(COLL03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
COLL03_SFft <- ftable(COLL03_SFg2$player1, COLL03_SFg2$player2)
COLL03_SFft2 <- as.matrix(COLL03_SFft)
numRows <- nrow(COLL03_SFft2)
numCols <- ncol(COLL03_SFft2)
COLL03_SFft3 <- COLL03_SFft2[c(2:numRows) , c(2:numCols)]
COLL03_SFTable <- graph.adjacency(COLL03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(COLL03_SFTable, vertex.label = V(COLL03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
COLL03_SF.clusterCoef <- transitivity(COLL03_SFTable, type="global") #cluster coefficient
COLL03_SF.degreeCent <- centralization.degree(COLL03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_SFftn <- as.network.matrix(COLL03_SFft)
COLL03_SF.netDensity <- network.density(COLL03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_SF.entropy <- entropy(COLL03_SFft) #entropy

COLL03_SF.netMx <- cbind(COLL03_SF.netMx, COLL03_SF.clusterCoef, COLL03_SF.degreeCent$centralization,
                         COLL03_SF.netDensity, COLL03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
COLL03_TFg2 <- data.frame(COLL03_TF)
COLL03_TFg2 <- COLL03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_TFg2$player1
player2vector <- COLL03_TFg2$player2
COLL03_TFg3 <- COLL03_TFg2
COLL03_TFg3$p1inp2vec <- is.element(COLL03_TFg3$player1, player2vector)
COLL03_TFg3$p2inp1vec <- is.element(COLL03_TFg3$player2, player1vector)

addPlayer1 <- COLL03_TFg3[ which(COLL03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_TFg3[ which(COLL03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_TFg2 <- rbind(COLL03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
COLL03_TFft <- ftable(COLL03_TFg2$player1, COLL03_TFg2$player2)
COLL03_TFft2 <- as.matrix(COLL03_TFft)
numRows <- nrow(COLL03_TFft2)
numCols <- ncol(COLL03_TFft2)
COLL03_TFft3 <- COLL03_TFft2[c(2:numRows) , c(2:numCols)]
COLL03_TFTable <- graph.adjacency(COLL03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(COLL03_TFTable, vertex.label = V(COLL03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
COLL03_TF.clusterCoef <- transitivity(COLL03_TFTable, type="global") #cluster coefficient
COLL03_TF.degreeCent <- centralization.degree(COLL03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_TFftn <- as.network.matrix(COLL03_TFft)
COLL03_TF.netDensity <- network.density(COLL03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_TF.entropy <- entropy(COLL03_TFft) #entropy

COLL03_TF.netMx <- cbind(COLL03_TF.netMx, COLL03_TF.clusterCoef, COLL03_TF.degreeCent$centralization,
                         COLL03_TF.netDensity, COLL03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
COLL03_SAMg2 <- data.frame(COLL03_SAM)
COLL03_SAMg2 <- COLL03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_SAMg2$player1
player2vector <- COLL03_SAMg2$player2
COLL03_SAMg3 <- COLL03_SAMg2
COLL03_SAMg3$p1inp2vec <- is.element(COLL03_SAMg3$player1, player2vector)
COLL03_SAMg3$p2inp1vec <- is.element(COLL03_SAMg3$player2, player1vector)

addPlayer1 <- COLL03_SAMg3[ which(COLL03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_SAMg3[ which(COLL03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_SAMg2 <- rbind(COLL03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
COLL03_SAMft <- ftable(COLL03_SAMg2$player1, COLL03_SAMg2$player2)
COLL03_SAMft2 <- as.matrix(COLL03_SAMft)
numRows <- nrow(COLL03_SAMft2)
numCols <- ncol(COLL03_SAMft2)
COLL03_SAMft3 <- COLL03_SAMft2[c(2:numRows) , c(2:numCols)]
COLL03_SAMTable <- graph.adjacency(COLL03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(COLL03_SAMTable, vertex.label = V(COLL03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
COLL03_SAM.clusterCoef <- transitivity(COLL03_SAMTable, type="global") #cluster coefficient
COLL03_SAM.degreeCent <- centralization.degree(COLL03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_SAMftn <- as.network.matrix(COLL03_SAMft)
COLL03_SAM.netDensity <- network.density(COLL03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_SAM.entropy <- entropy(COLL03_SAMft) #entropy

COLL03_SAM.netMx <- cbind(COLL03_SAM.netMx, COLL03_SAM.clusterCoef, COLL03_SAM.degreeCent$centralization,
                          COLL03_SAM.netDensity, COLL03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
COLL03_TAMg2 <- data.frame(COLL03_TAM)
COLL03_TAMg2 <- COLL03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_TAMg2$player1
player2vector <- COLL03_TAMg2$player2
COLL03_TAMg3 <- COLL03_TAMg2
COLL03_TAMg3$p1inp2vec <- is.element(COLL03_TAMg3$player1, player2vector)
COLL03_TAMg3$p2inp1vec <- is.element(COLL03_TAMg3$player2, player1vector)

addPlayer1 <- COLL03_TAMg3[ which(COLL03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_TAMg3[ which(COLL03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_TAMg2 <- rbind(COLL03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
COLL03_TAMft <- ftable(COLL03_TAMg2$player1, COLL03_TAMg2$player2)
COLL03_TAMft2 <- as.matrix(COLL03_TAMft)
numRows <- nrow(COLL03_TAMft2)
numCols <- ncol(COLL03_TAMft2)
COLL03_TAMft3 <- COLL03_TAMft2[c(2:numRows) , c(2:numCols)]
COLL03_TAMTable <- graph.adjacency(COLL03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(COLL03_TAMTable, vertex.label = V(COLL03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
COLL03_TAM.clusterCoef <- transitivity(COLL03_TAMTable, type="global") #cluster coefficient
COLL03_TAM.degreeCent <- centralization.degree(COLL03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_TAMftn <- as.network.matrix(COLL03_TAMft)
COLL03_TAM.netDensity <- network.density(COLL03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_TAM.entropy <- entropy(COLL03_TAMft) #entropy

COLL03_TAM.netMx <- cbind(COLL03_TAM.netMx, COLL03_TAM.clusterCoef, COLL03_TAM.degreeCent$centralization,
                          COLL03_TAM.netDensity, COLL03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
COLL03_SDMg2 <- data.frame(COLL03_SDM)
COLL03_SDMg2 <- COLL03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_SDMg2$player1
player2vector <- COLL03_SDMg2$player2
COLL03_SDMg3 <- COLL03_SDMg2
COLL03_SDMg3$p1inp2vec <- is.element(COLL03_SDMg3$player1, player2vector)
COLL03_SDMg3$p2inp1vec <- is.element(COLL03_SDMg3$player2, player1vector)

addPlayer1 <- COLL03_SDMg3[ which(COLL03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_SDMg3[ which(COLL03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_SDMg2 <- rbind(COLL03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
COLL03_SDMft <- ftable(COLL03_SDMg2$player1, COLL03_SDMg2$player2)
COLL03_SDMft2 <- as.matrix(COLL03_SDMft)
numRows <- nrow(COLL03_SDMft2)
numCols <- ncol(COLL03_SDMft2)
COLL03_SDMft3 <- COLL03_SDMft2[c(2:numRows) , c(2:numCols)]
COLL03_SDMTable <- graph.adjacency(COLL03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(COLL03_SDMTable, vertex.label = V(COLL03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
COLL03_SDM.clusterCoef <- transitivity(COLL03_SDMTable, type="global") #cluster coefficient
COLL03_SDM.degreeCent <- centralization.degree(COLL03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_SDMftn <- as.network.matrix(COLL03_SDMft)
COLL03_SDM.netDensity <- network.density(COLL03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_SDM.entropy <- entropy(COLL03_SDMft) #entropy

COLL03_SDM.netMx <- cbind(COLL03_SDM.netMx, COLL03_SDM.clusterCoef, COLL03_SDM.degreeCent$centralization,
                          COLL03_SDM.netDensity, COLL03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
COLL03_TDMg2 <- data.frame(COLL03_TDM)
COLL03_TDMg2 <- COLL03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_TDMg2$player1
player2vector <- COLL03_TDMg2$player2
COLL03_TDMg3 <- COLL03_TDMg2
COLL03_TDMg3$p1inp2vec <- is.element(COLL03_TDMg3$player1, player2vector)
COLL03_TDMg3$p2inp1vec <- is.element(COLL03_TDMg3$player2, player1vector)

addPlayer1 <- COLL03_TDMg3[ which(COLL03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_TDMg3[ which(COLL03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_TDMg2 <- rbind(COLL03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
COLL03_TDMft <- ftable(COLL03_TDMg2$player1, COLL03_TDMg2$player2)
COLL03_TDMft2 <- as.matrix(COLL03_TDMft)
numRows <- nrow(COLL03_TDMft2)
numCols <- ncol(COLL03_TDMft2)
COLL03_TDMft3 <- COLL03_TDMft2[c(2:numRows) , c(2:numCols)]
COLL03_TDMTable <- graph.adjacency(COLL03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(COLL03_TDMTable, vertex.label = V(COLL03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
COLL03_TDM.clusterCoef <- transitivity(COLL03_TDMTable, type="global") #cluster coefficient
COLL03_TDM.degreeCent <- centralization.degree(COLL03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_TDMftn <- as.network.matrix(COLL03_TDMft)
COLL03_TDM.netDensity <- network.density(COLL03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_TDM.entropy <- entropy(COLL03_TDMft) #entropy

COLL03_TDM.netMx <- cbind(COLL03_TDM.netMx, COLL03_TDM.clusterCoef, COLL03_TDM.degreeCent$centralization,
                          COLL03_TDM.netDensity, COLL03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
COLL03_SDg2 <- data.frame(COLL03_SD)
COLL03_SDg2 <- COLL03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_SDg2$player1
player2vector <- COLL03_SDg2$player2
COLL03_SDg3 <- COLL03_SDg2
COLL03_SDg3$p1inp2vec <- is.element(COLL03_SDg3$player1, player2vector)
COLL03_SDg3$p2inp1vec <- is.element(COLL03_SDg3$player2, player1vector)

addPlayer1 <- COLL03_SDg3[ which(COLL03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_SDg3[ which(COLL03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_SDg2 <- rbind(COLL03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
COLL03_SDft <- ftable(COLL03_SDg2$player1, COLL03_SDg2$player2)
COLL03_SDft2 <- as.matrix(COLL03_SDft)
numRows <- nrow(COLL03_SDft2)
numCols <- ncol(COLL03_SDft2)
COLL03_SDft3 <- COLL03_SDft2[c(2:numRows) , c(2:numCols)]
COLL03_SDTable <- graph.adjacency(COLL03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(COLL03_SDTable, vertex.label = V(COLL03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
COLL03_SD.clusterCoef <- transitivity(COLL03_SDTable, type="global") #cluster coefficient
COLL03_SD.degreeCent <- centralization.degree(COLL03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_SDftn <- as.network.matrix(COLL03_SDft)
COLL03_SD.netDensity <- network.density(COLL03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_SD.entropy <- entropy(COLL03_SDft) #entropy

COLL03_SD.netMx <- cbind(COLL03_SD.netMx, COLL03_SD.clusterCoef, COLL03_SD.degreeCent$centralization,
                         COLL03_SD.netDensity, COLL03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
COLL03_TDg2 <- data.frame(COLL03_TD)
COLL03_TDg2 <- COLL03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_TDg2$player1
player2vector <- COLL03_TDg2$player2
COLL03_TDg3 <- COLL03_TDg2
COLL03_TDg3$p1inp2vec <- is.element(COLL03_TDg3$player1, player2vector)
COLL03_TDg3$p2inp1vec <- is.element(COLL03_TDg3$player2, player1vector)

addPlayer1 <- COLL03_TDg3[ which(COLL03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_TDg3[ which(COLL03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_TDg2 <- rbind(COLL03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
COLL03_TDft <- ftable(COLL03_TDg2$player1, COLL03_TDg2$player2)
COLL03_TDft2 <- as.matrix(COLL03_TDft)
numRows <- nrow(COLL03_TDft2)
numCols <- ncol(COLL03_TDft2)
COLL03_TDft3 <- COLL03_TDft2[c(2:numRows) , c(2:numCols)]
COLL03_TDTable <- graph.adjacency(COLL03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(COLL03_TDTable, vertex.label = V(COLL03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
COLL03_TD.clusterCoef <- transitivity(COLL03_TDTable, type="global") #cluster coefficient
COLL03_TD.degreeCent <- centralization.degree(COLL03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_TDftn <- as.network.matrix(COLL03_TDft)
COLL03_TD.netDensity <- network.density(COLL03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_TD.entropy <- entropy(COLL03_TDft) #entropy

COLL03_TD.netMx <- cbind(COLL03_TD.netMx, COLL03_TD.clusterCoef, COLL03_TD.degreeCent$centralization,
                         COLL03_TD.netDensity, COLL03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
COLL03_QTg2 <- data.frame(COLL03_QT)
COLL03_QTg2 <- COLL03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL03_QTg2$player1
player2vector <- COLL03_QTg2$player2
COLL03_QTg3 <- COLL03_QTg2
COLL03_QTg3$p1inp2vec <- is.element(COLL03_QTg3$player1, player2vector)
COLL03_QTg3$p2inp1vec <- is.element(COLL03_QTg3$player2, player1vector)

addPlayer1 <- COLL03_QTg3[ which(COLL03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL03_QTg3[ which(COLL03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL03_QTg2 <- rbind(COLL03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
COLL03_QTft <- ftable(COLL03_QTg2$player1, COLL03_QTg2$player2)
COLL03_QTft2 <- as.matrix(COLL03_QTft)
numRows <- nrow(COLL03_QTft2)
numCols <- ncol(COLL03_QTft2)
COLL03_QTft3 <- COLL03_QTft2[c(2:numRows) , c(2:numCols)]
COLL03_QTTable <- graph.adjacency(COLL03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(COLL03_QTTable, vertex.label = V(COLL03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
COLL03_QT.clusterCoef <- transitivity(COLL03_QTTable, type="global") #cluster coefficient
COLL03_QT.degreeCent <- centralization.degree(COLL03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL03_QTftn <- as.network.matrix(COLL03_QTft)
COLL03_QT.netDensity <- network.density(COLL03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL03_QT.entropy <- entropy(COLL03_QTft) #entropy

COLL03_QT.netMx <- cbind(COLL03_QT.netMx, COLL03_QT.clusterCoef, COLL03_QT.degreeCent$centralization,
                         COLL03_QT.netDensity, COLL03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL03_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "ESS"
KIoutcome = "Goal_F"
ESS03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
ESS03_Gg2 <- data.frame(ESS03_G)
ESS03_Gg2 <- ESS03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_Gg2$player1
player2vector <- ESS03_Gg2$player2
ESS03_Gg3 <- ESS03_Gg2
ESS03_Gg3$p1inp2vec <- is.element(ESS03_Gg3$player1, player2vector)
ESS03_Gg3$p2inp1vec <- is.element(ESS03_Gg3$player2, player1vector)

addPlayer1 <- ESS03_Gg3[ which(ESS03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_Gg3[ which(ESS03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_Gg2 <- rbind(ESS03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
ESS03_Gft <- ftable(ESS03_Gg2$player1, ESS03_Gg2$player2)
ESS03_Gft2 <- as.matrix(ESS03_Gft)
numRows <- nrow(ESS03_Gft2)
numCols <- ncol(ESS03_Gft2)
ESS03_Gft3 <- ESS03_Gft2[c(2:numRows) , c(2:numCols)]
ESS03_GTable <- graph.adjacency(ESS03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(ESS03_GTable, vertex.label = V(ESS03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
ESS03_G.clusterCoef <- transitivity(ESS03_GTable, type="global") #cluster coefficient
ESS03_G.degreeCent <- centralization.degree(ESS03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_Gftn <- as.network.matrix(ESS03_Gft)
ESS03_G.netDensity <- network.density(ESS03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_G.entropy <- entropy(ESS03_Gft) #entropy

ESS03_G.netMx <- cbind(ESS03_G.netMx, ESS03_G.clusterCoef, ESS03_G.degreeCent$centralization,
                       ESS03_G.netDensity, ESS03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "ESS"
KIoutcome = "Behind_F"
ESS03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
ESS03_Bg2 <- data.frame(ESS03_B)
ESS03_Bg2 <- ESS03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_Bg2$player1
player2vector <- ESS03_Bg2$player2
ESS03_Bg3 <- ESS03_Bg2
ESS03_Bg3$p1inp2vec <- is.element(ESS03_Bg3$player1, player2vector)
ESS03_Bg3$p2inp1vec <- is.element(ESS03_Bg3$player2, player1vector)

addPlayer1 <- ESS03_Bg3[ which(ESS03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_Bg3[ which(ESS03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_Bg2 <- rbind(ESS03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
ESS03_Bft <- ftable(ESS03_Bg2$player1, ESS03_Bg2$player2)
ESS03_Bft2 <- as.matrix(ESS03_Bft)
numRows <- nrow(ESS03_Bft2)
numCols <- ncol(ESS03_Bft2)
ESS03_Bft3 <- ESS03_Bft2[c(2:numRows) , c(2:numCols)]
ESS03_BTable <- graph.adjacency(ESS03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(ESS03_BTable, vertex.label = V(ESS03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
ESS03_B.clusterCoef <- transitivity(ESS03_BTable, type="global") #cluster coefficient
ESS03_B.degreeCent <- centralization.degree(ESS03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_Bftn <- as.network.matrix(ESS03_Bft)
ESS03_B.netDensity <- network.density(ESS03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_B.entropy <- entropy(ESS03_Bft) #entropy

ESS03_B.netMx <- cbind(ESS03_B.netMx, ESS03_B.clusterCoef, ESS03_B.degreeCent$centralization,
                       ESS03_B.netDensity, ESS03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
ESS03_SFg2 <- data.frame(ESS03_SF)
ESS03_SFg2 <- ESS03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_SFg2$player1
player2vector <- ESS03_SFg2$player2
ESS03_SFg3 <- ESS03_SFg2
ESS03_SFg3$p1inp2vec <- is.element(ESS03_SFg3$player1, player2vector)
ESS03_SFg3$p2inp1vec <- is.element(ESS03_SFg3$player2, player1vector)

addPlayer1 <- ESS03_SFg3[ which(ESS03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_SFg3[ which(ESS03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_SFg2 <- rbind(ESS03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
ESS03_SFft <- ftable(ESS03_SFg2$player1, ESS03_SFg2$player2)
ESS03_SFft2 <- as.matrix(ESS03_SFft)
numRows <- nrow(ESS03_SFft2)
numCols <- ncol(ESS03_SFft2)
ESS03_SFft3 <- ESS03_SFft2[c(2:numRows) , c(2:numCols)]
ESS03_SFTable <- graph.adjacency(ESS03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(ESS03_SFTable, vertex.label = V(ESS03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
ESS03_SF.clusterCoef <- transitivity(ESS03_SFTable, type="global") #cluster coefficient
ESS03_SF.degreeCent <- centralization.degree(ESS03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_SFftn <- as.network.matrix(ESS03_SFft)
ESS03_SF.netDensity <- network.density(ESS03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_SF.entropy <- entropy(ESS03_SFft) #entropy

ESS03_SF.netMx <- cbind(ESS03_SF.netMx, ESS03_SF.clusterCoef, ESS03_SF.degreeCent$centralization,
                        ESS03_SF.netDensity, ESS03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
ESS03_TFg2 <- data.frame(ESS03_TF)
ESS03_TFg2 <- ESS03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_TFg2$player1
player2vector <- ESS03_TFg2$player2
ESS03_TFg3 <- ESS03_TFg2
ESS03_TFg3$p1inp2vec <- is.element(ESS03_TFg3$player1, player2vector)
ESS03_TFg3$p2inp1vec <- is.element(ESS03_TFg3$player2, player1vector)

addPlayer1 <- ESS03_TFg3[ which(ESS03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_TFg3[ which(ESS03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_TFg2 <- rbind(ESS03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
ESS03_TFft <- ftable(ESS03_TFg2$player1, ESS03_TFg2$player2)
ESS03_TFft2 <- as.matrix(ESS03_TFft)
numRows <- nrow(ESS03_TFft2)
numCols <- ncol(ESS03_TFft2)
ESS03_TFft3 <- ESS03_TFft2[c(2:numRows) , c(2:numCols)]
ESS03_TFTable <- graph.adjacency(ESS03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(ESS03_TFTable, vertex.label = V(ESS03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
ESS03_TF.clusterCoef <- transitivity(ESS03_TFTable, type="global") #cluster coefficient
ESS03_TF.degreeCent <- centralization.degree(ESS03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_TFftn <- as.network.matrix(ESS03_TFft)
ESS03_TF.netDensity <- network.density(ESS03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_TF.entropy <- entropy(ESS03_TFft) #entropy

ESS03_TF.netMx <- cbind(ESS03_TF.netMx, ESS03_TF.clusterCoef, ESS03_TF.degreeCent$centralization,
                        ESS03_TF.netDensity, ESS03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
ESS03_SAMg2 <- data.frame(ESS03_SAM)
ESS03_SAMg2 <- ESS03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_SAMg2$player1
player2vector <- ESS03_SAMg2$player2
ESS03_SAMg3 <- ESS03_SAMg2
ESS03_SAMg3$p1inp2vec <- is.element(ESS03_SAMg3$player1, player2vector)
ESS03_SAMg3$p2inp1vec <- is.element(ESS03_SAMg3$player2, player1vector)

addPlayer1 <- ESS03_SAMg3[ which(ESS03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_SAMg3[ which(ESS03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_SAMg2 <- rbind(ESS03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
ESS03_SAMft <- ftable(ESS03_SAMg2$player1, ESS03_SAMg2$player2)
ESS03_SAMft2 <- as.matrix(ESS03_SAMft)
numRows <- nrow(ESS03_SAMft2)
numCols <- ncol(ESS03_SAMft2)
ESS03_SAMft3 <- ESS03_SAMft2[c(2:numRows) , c(2:numCols)]
ESS03_SAMTable <- graph.adjacency(ESS03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(ESS03_SAMTable, vertex.label = V(ESS03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
ESS03_SAM.clusterCoef <- transitivity(ESS03_SAMTable, type="global") #cluster coefficient
ESS03_SAM.degreeCent <- centralization.degree(ESS03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_SAMftn <- as.network.matrix(ESS03_SAMft)
ESS03_SAM.netDensity <- network.density(ESS03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_SAM.entropy <- entropy(ESS03_SAMft) #entropy

ESS03_SAM.netMx <- cbind(ESS03_SAM.netMx, ESS03_SAM.clusterCoef, ESS03_SAM.degreeCent$centralization,
                         ESS03_SAM.netDensity, ESS03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
ESS03_TAMg2 <- data.frame(ESS03_TAM)
ESS03_TAMg2 <- ESS03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_TAMg2$player1
player2vector <- ESS03_TAMg2$player2
ESS03_TAMg3 <- ESS03_TAMg2
ESS03_TAMg3$p1inp2vec <- is.element(ESS03_TAMg3$player1, player2vector)
ESS03_TAMg3$p2inp1vec <- is.element(ESS03_TAMg3$player2, player1vector)

addPlayer1 <- ESS03_TAMg3[ which(ESS03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_TAMg3[ which(ESS03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_TAMg2 <- rbind(ESS03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
ESS03_TAMft <- ftable(ESS03_TAMg2$player1, ESS03_TAMg2$player2)
ESS03_TAMft2 <- as.matrix(ESS03_TAMft)
numRows <- nrow(ESS03_TAMft2)
numCols <- ncol(ESS03_TAMft2)
ESS03_TAMft3 <- ESS03_TAMft2[c(2:numRows) , c(2:numCols)]
ESS03_TAMTable <- graph.adjacency(ESS03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(ESS03_TAMTable, vertex.label = V(ESS03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
ESS03_TAM.clusterCoef <- transitivity(ESS03_TAMTable, type="global") #cluster coefficient
ESS03_TAM.degreeCent <- centralization.degree(ESS03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_TAMftn <- as.network.matrix(ESS03_TAMft)
ESS03_TAM.netDensity <- network.density(ESS03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_TAM.entropy <- entropy(ESS03_TAMft) #entropy

ESS03_TAM.netMx <- cbind(ESS03_TAM.netMx, ESS03_TAM.clusterCoef, ESS03_TAM.degreeCent$centralization,
                         ESS03_TAM.netDensity, ESS03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
ESS03_SDMg2 <- data.frame(ESS03_SDM)
ESS03_SDMg2 <- ESS03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_SDMg2$player1
player2vector <- ESS03_SDMg2$player2
ESS03_SDMg3 <- ESS03_SDMg2
ESS03_SDMg3$p1inp2vec <- is.element(ESS03_SDMg3$player1, player2vector)
ESS03_SDMg3$p2inp1vec <- is.element(ESS03_SDMg3$player2, player1vector)

addPlayer1 <- ESS03_SDMg3[ which(ESS03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_SDMg3[ which(ESS03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_SDMg2 <- rbind(ESS03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
ESS03_SDMft <- ftable(ESS03_SDMg2$player1, ESS03_SDMg2$player2)
ESS03_SDMft2 <- as.matrix(ESS03_SDMft)
numRows <- nrow(ESS03_SDMft2)
numCols <- ncol(ESS03_SDMft2)
ESS03_SDMft3 <- ESS03_SDMft2[c(2:numRows) , c(2:numCols)]
ESS03_SDMTable <- graph.adjacency(ESS03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(ESS03_SDMTable, vertex.label = V(ESS03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
ESS03_SDM.clusterCoef <- transitivity(ESS03_SDMTable, type="global") #cluster coefficient
ESS03_SDM.degreeCent <- centralization.degree(ESS03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_SDMftn <- as.network.matrix(ESS03_SDMft)
ESS03_SDM.netDensity <- network.density(ESS03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_SDM.entropy <- entropy(ESS03_SDMft) #entropy

ESS03_SDM.netMx <- cbind(ESS03_SDM.netMx, ESS03_SDM.clusterCoef, ESS03_SDM.degreeCent$centralization,
                         ESS03_SDM.netDensity, ESS03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
ESS03_TDMg2 <- data.frame(ESS03_TDM)
ESS03_TDMg2 <- ESS03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_TDMg2$player1
player2vector <- ESS03_TDMg2$player2
ESS03_TDMg3 <- ESS03_TDMg2
ESS03_TDMg3$p1inp2vec <- is.element(ESS03_TDMg3$player1, player2vector)
ESS03_TDMg3$p2inp1vec <- is.element(ESS03_TDMg3$player2, player1vector)

addPlayer1 <- ESS03_TDMg3[ which(ESS03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_TDMg3[ which(ESS03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_TDMg2 <- rbind(ESS03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
ESS03_TDMft <- ftable(ESS03_TDMg2$player1, ESS03_TDMg2$player2)
ESS03_TDMft2 <- as.matrix(ESS03_TDMft)
numRows <- nrow(ESS03_TDMft2)
numCols <- ncol(ESS03_TDMft2)
ESS03_TDMft3 <- ESS03_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS03_TDMTable <- graph.adjacency(ESS03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(ESS03_TDMTable, vertex.label = V(ESS03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
ESS03_TDM.clusterCoef <- transitivity(ESS03_TDMTable, type="global") #cluster coefficient
ESS03_TDM.degreeCent <- centralization.degree(ESS03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_TDMftn <- as.network.matrix(ESS03_TDMft)
ESS03_TDM.netDensity <- network.density(ESS03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_TDM.entropy <- entropy(ESS03_TDMft) #entropy

ESS03_TDM.netMx <- cbind(ESS03_TDM.netMx, ESS03_TDM.clusterCoef, ESS03_TDM.degreeCent$centralization,
                         ESS03_TDM.netDensity, ESS03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
ESS03_SDg2 <- data.frame(ESS03_SD)
ESS03_SDg2 <- ESS03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_SDg2$player1
player2vector <- ESS03_SDg2$player2
ESS03_SDg3 <- ESS03_SDg2
ESS03_SDg3$p1inp2vec <- is.element(ESS03_SDg3$player1, player2vector)
ESS03_SDg3$p2inp1vec <- is.element(ESS03_SDg3$player2, player1vector)

addPlayer1 <- ESS03_SDg3[ which(ESS03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_SDg3[ which(ESS03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_SDg2 <- rbind(ESS03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
ESS03_SDft <- ftable(ESS03_SDg2$player1, ESS03_SDg2$player2)
ESS03_SDft2 <- as.matrix(ESS03_SDft)
numRows <- nrow(ESS03_SDft2)
numCols <- ncol(ESS03_SDft2)
ESS03_SDft3 <- ESS03_SDft2[c(2:numRows) , c(2:numCols)]
ESS03_SDTable <- graph.adjacency(ESS03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(ESS03_SDTable, vertex.label = V(ESS03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
ESS03_SD.clusterCoef <- transitivity(ESS03_SDTable, type="global") #cluster coefficient
ESS03_SD.degreeCent <- centralization.degree(ESS03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_SDftn <- as.network.matrix(ESS03_SDft)
ESS03_SD.netDensity <- network.density(ESS03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_SD.entropy <- entropy(ESS03_SDft) #entropy

ESS03_SD.netMx <- cbind(ESS03_SD.netMx, ESS03_SD.clusterCoef, ESS03_SD.degreeCent$centralization,
                        ESS03_SD.netDensity, ESS03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************

round = 3
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
ESS03_TDg2 <- data.frame(ESS03_TD)
ESS03_TDg2 <- ESS03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_TDg2$player1
player2vector <- ESS03_TDg2$player2
ESS03_TDg3 <- ESS03_TDg2
ESS03_TDg3$p1inp2vec <- is.element(ESS03_TDg3$player1, player2vector)
ESS03_TDg3$p2inp1vec <- is.element(ESS03_TDg3$player2, player1vector)

addPlayer1 <- ESS03_TDg3[ which(ESS03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_TDg3[ which(ESS03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_TDg2 <- rbind(ESS03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
ESS03_TDft <- ftable(ESS03_TDg2$player1, ESS03_TDg2$player2)
ESS03_TDft2 <- as.matrix(ESS03_TDft)
numRows <- nrow(ESS03_TDft2)
numCols <- ncol(ESS03_TDft2)
ESS03_TDft3 <- ESS03_TDft2[c(2:numRows) , c(2:numCols)]
ESS03_TDTable <- graph.adjacency(ESS03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(ESS03_TDTable, vertex.label = V(ESS03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
ESS03_TD.clusterCoef <- transitivity(ESS03_TDTable, type="global") #cluster coefficient
ESS03_TD.degreeCent <- centralization.degree(ESS03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_TDftn <- as.network.matrix(ESS03_TDft)
ESS03_TD.netDensity <- network.density(ESS03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_TD.entropy <- entropy(ESS03_TDft) #entropy

ESS03_TD.netMx <- cbind(ESS03_TD.netMx, ESS03_TD.clusterCoef, ESS03_TD.degreeCent$centralization,
                        ESS03_TD.netDensity, ESS03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
ESS03_QTg2 <- data.frame(ESS03_QT)
ESS03_QTg2 <- ESS03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS03_QTg2$player1
player2vector <- ESS03_QTg2$player2
ESS03_QTg3 <- ESS03_QTg2
ESS03_QTg3$p1inp2vec <- is.element(ESS03_QTg3$player1, player2vector)
ESS03_QTg3$p2inp1vec <- is.element(ESS03_QTg3$player2, player1vector)

addPlayer1 <- ESS03_QTg3[ which(ESS03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS03_QTg3[ which(ESS03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS03_QTg2 <- rbind(ESS03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
ESS03_QTft <- ftable(ESS03_QTg2$player1, ESS03_QTg2$player2)
ESS03_QTft2 <- as.matrix(ESS03_QTft)
numRows <- nrow(ESS03_QTft2)
numCols <- ncol(ESS03_QTft2)
ESS03_QTft3 <- ESS03_QTft2[c(2:numRows) , c(2:numCols)]
ESS03_QTTable <- graph.adjacency(ESS03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(ESS03_QTTable, vertex.label = V(ESS03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
ESS03_QT.clusterCoef <- transitivity(ESS03_QTTable, type="global") #cluster coefficient
ESS03_QT.degreeCent <- centralization.degree(ESS03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS03_QTftn <- as.network.matrix(ESS03_QTft)
ESS03_QT.netDensity <- network.density(ESS03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS03_QT.entropy <- entropy(ESS03_QTft) #entropy

ESS03_QT.netMx <- cbind(ESS03_QT.netMx, ESS03_QT.clusterCoef, ESS03_QT.degreeCent$centralization,
                        ESS03_QT.netDensity, ESS03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS03_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "FRE"
KIoutcome = "Goal_F"
FRE03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
FRE03_Gg2 <- data.frame(FRE03_G)
FRE03_Gg2 <- FRE03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_Gg2$player1
player2vector <- FRE03_Gg2$player2
FRE03_Gg3 <- FRE03_Gg2
FRE03_Gg3$p1inp2vec <- is.element(FRE03_Gg3$player1, player2vector)
FRE03_Gg3$p2inp1vec <- is.element(FRE03_Gg3$player2, player1vector)

addPlayer1 <- FRE03_Gg3[ which(FRE03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_Gg3[ which(FRE03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_Gg2 <- rbind(FRE03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
FRE03_Gft <- ftable(FRE03_Gg2$player1, FRE03_Gg2$player2)
FRE03_Gft2 <- as.matrix(FRE03_Gft)
numRows <- nrow(FRE03_Gft2)
numCols <- ncol(FRE03_Gft2)
FRE03_Gft3 <- FRE03_Gft2[c(2:numRows) , c(2:numCols)]
FRE03_GTable <- graph.adjacency(FRE03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(FRE03_GTable, vertex.label = V(FRE03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
FRE03_G.clusterCoef <- transitivity(FRE03_GTable, type="global") #cluster coefficient
FRE03_G.degreeCent <- centralization.degree(FRE03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_Gftn <- as.network.matrix(FRE03_Gft)
FRE03_G.netDensity <- network.density(FRE03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_G.entropy <- entropy(FRE03_Gft) #entropy

FRE03_G.netMx <- cbind(FRE03_G.netMx, FRE03_G.clusterCoef, FRE03_G.degreeCent$centralization,
                       FRE03_G.netDensity, FRE03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "FRE"
KIoutcome = "Behind_F"
FRE03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
FRE03_Bg2 <- data.frame(FRE03_B)
FRE03_Bg2 <- FRE03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_Bg2$player1
player2vector <- FRE03_Bg2$player2
FRE03_Bg3 <- FRE03_Bg2
FRE03_Bg3$p1inp2vec <- is.element(FRE03_Bg3$player1, player2vector)
FRE03_Bg3$p2inp1vec <- is.element(FRE03_Bg3$player2, player1vector)

addPlayer1 <- FRE03_Bg3[ which(FRE03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_Bg3[ which(FRE03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_Bg2 <- rbind(FRE03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
FRE03_Bft <- ftable(FRE03_Bg2$player1, FRE03_Bg2$player2)
FRE03_Bft2 <- as.matrix(FRE03_Bft)
numRows <- nrow(FRE03_Bft2)
numCols <- ncol(FRE03_Bft2)
FRE03_Bft3 <- FRE03_Bft2[c(2:numRows) , c(2:numCols)]
FRE03_BTable <- graph.adjacency(FRE03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(FRE03_BTable, vertex.label = V(FRE03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
FRE03_B.clusterCoef <- transitivity(FRE03_BTable, type="global") #cluster coefficient
FRE03_B.degreeCent <- centralization.degree(FRE03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_Bftn <- as.network.matrix(FRE03_Bft)
FRE03_B.netDensity <- network.density(FRE03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_B.entropy <- entropy(FRE03_Bft) #entropy

FRE03_B.netMx <- cbind(FRE03_B.netMx, FRE03_B.clusterCoef, FRE03_B.degreeCent$centralization,
                       FRE03_B.netDensity, FRE03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA 

round = 3
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
FRE03_SFg2 <- data.frame(FRE03_SF)
FRE03_SFg2 <- FRE03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_SFg2$player1
player2vector <- FRE03_SFg2$player2
FRE03_SFg3 <- FRE03_SFg2
FRE03_SFg3$p1inp2vec <- is.element(FRE03_SFg3$player1, player2vector)
FRE03_SFg3$p2inp1vec <- is.element(FRE03_SFg3$player2, player1vector)

addPlayer1 <- FRE03_SFg3[ which(FRE03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_SFg3[ which(FRE03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_SFg2 <- rbind(FRE03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
FRE03_SFft <- ftable(FRE03_SFg2$player1, FRE03_SFg2$player2)
FRE03_SFft2 <- as.matrix(FRE03_SFft)
numRows <- nrow(FRE03_SFft2)
numCols <- ncol(FRE03_SFft2)
FRE03_SFft3 <- FRE03_SFft2[c(2:numRows) , c(2:numCols)]
FRE03_SFTable <- graph.adjacency(FRE03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(FRE03_SFTable, vertex.label = V(FRE03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
FRE03_SF.clusterCoef <- transitivity(FRE03_SFTable, type="global") #cluster coefficient
FRE03_SF.degreeCent <- centralization.degree(FRE03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_SFftn <- as.network.matrix(FRE03_SFft)
FRE03_SF.netDensity <- network.density(FRE03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_SF.entropy <- entropy(FRE03_SFft) #entropy

FRE03_SF.netMx <- cbind(FRE03_SF.netMx, FRE03_SF.clusterCoef, FRE03_SF.degreeCent$centralization,
                        FRE03_SF.netDensity, FRE03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************
#NA

round = 3
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
FRE03_TFg2 <- data.frame(FRE03_TF)
FRE03_TFg2 <- FRE03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_TFg2$player1
player2vector <- FRE03_TFg2$player2
FRE03_TFg3 <- FRE03_TFg2
FRE03_TFg3$p1inp2vec <- is.element(FRE03_TFg3$player1, player2vector)
FRE03_TFg3$p2inp1vec <- is.element(FRE03_TFg3$player2, player1vector)

addPlayer1 <- FRE03_TFg3[ which(FRE03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_TFg3[ which(FRE03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_TFg2 <- rbind(FRE03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
FRE03_TFft <- ftable(FRE03_TFg2$player1, FRE03_TFg2$player2)
FRE03_TFft2 <- as.matrix(FRE03_TFft)
numRows <- nrow(FRE03_TFft2)
numCols <- ncol(FRE03_TFft2)
FRE03_TFft3 <- FRE03_TFft2[c(2:numRows) , c(2:numCols)]
FRE03_TFTable <- graph.adjacency(FRE03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(FRE03_TFTable, vertex.label = V(FRE03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
FRE03_TF.clusterCoef <- transitivity(FRE03_TFTable, type="global") #cluster coefficient
FRE03_TF.degreeCent <- centralization.degree(FRE03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_TFftn <- as.network.matrix(FRE03_TFft)
FRE03_TF.netDensity <- network.density(FRE03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_TF.entropy <- entropy(FRE03_TFft) #entropy

FRE03_TF.netMx <- cbind(FRE03_TF.netMx, FRE03_TF.clusterCoef, FRE03_TF.degreeCent$centralization,
                        FRE03_TF.netDensity, FRE03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
FRE03_SAMg2 <- data.frame(FRE03_SAM)
FRE03_SAMg2 <- FRE03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_SAMg2$player1
player2vector <- FRE03_SAMg2$player2
FRE03_SAMg3 <- FRE03_SAMg2
FRE03_SAMg3$p1inp2vec <- is.element(FRE03_SAMg3$player1, player2vector)
FRE03_SAMg3$p2inp1vec <- is.element(FRE03_SAMg3$player2, player1vector)

addPlayer1 <- FRE03_SAMg3[ which(FRE03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_SAMg3[ which(FRE03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_SAMg2 <- rbind(FRE03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
FRE03_SAMft <- ftable(FRE03_SAMg2$player1, FRE03_SAMg2$player2)
FRE03_SAMft2 <- as.matrix(FRE03_SAMft)
numRows <- nrow(FRE03_SAMft2)
numCols <- ncol(FRE03_SAMft2)
FRE03_SAMft3 <- FRE03_SAMft2[c(2:numRows) , c(2:numCols)]
FRE03_SAMTable <- graph.adjacency(FRE03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(FRE03_SAMTable, vertex.label = V(FRE03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
FRE03_SAM.clusterCoef <- transitivity(FRE03_SAMTable, type="global") #cluster coefficient
FRE03_SAM.degreeCent <- centralization.degree(FRE03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_SAMftn <- as.network.matrix(FRE03_SAMft)
FRE03_SAM.netDensity <- network.density(FRE03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_SAM.entropy <- entropy(FRE03_SAMft) #entropy

FRE03_SAM.netMx <- cbind(FRE03_SAM.netMx, FRE03_SAM.clusterCoef, FRE03_SAM.degreeCent$centralization,
                         FRE03_SAM.netDensity, FRE03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
FRE03_TAMg2 <- data.frame(FRE03_TAM)
FRE03_TAMg2 <- FRE03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_TAMg2$player1
player2vector <- FRE03_TAMg2$player2
FRE03_TAMg3 <- FRE03_TAMg2
FRE03_TAMg3$p1inp2vec <- is.element(FRE03_TAMg3$player1, player2vector)
FRE03_TAMg3$p2inp1vec <- is.element(FRE03_TAMg3$player2, player1vector)

addPlayer1 <- FRE03_TAMg3[ which(FRE03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_TAMg3[ which(FRE03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_TAMg2 <- rbind(FRE03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
FRE03_TAMft <- ftable(FRE03_TAMg2$player1, FRE03_TAMg2$player2)
FRE03_TAMft2 <- as.matrix(FRE03_TAMft)
numRows <- nrow(FRE03_TAMft2)
numCols <- ncol(FRE03_TAMft2)
FRE03_TAMft3 <- FRE03_TAMft2[c(2:numRows) , c(2:numCols)]
FRE03_TAMTable <- graph.adjacency(FRE03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(FRE03_TAMTable, vertex.label = V(FRE03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
FRE03_TAM.clusterCoef <- transitivity(FRE03_TAMTable, type="global") #cluster coefficient
FRE03_TAM.degreeCent <- centralization.degree(FRE03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_TAMftn <- as.network.matrix(FRE03_TAMft)
FRE03_TAM.netDensity <- network.density(FRE03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_TAM.entropy <- entropy(FRE03_TAMft) #entropy

FRE03_TAM.netMx <- cbind(FRE03_TAM.netMx, FRE03_TAM.clusterCoef, FRE03_TAM.degreeCent$centralization,
                         FRE03_TAM.netDensity, FRE03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
FRE03_SDMg2 <- data.frame(FRE03_SDM)
FRE03_SDMg2 <- FRE03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_SDMg2$player1
player2vector <- FRE03_SDMg2$player2
FRE03_SDMg3 <- FRE03_SDMg2
FRE03_SDMg3$p1inp2vec <- is.element(FRE03_SDMg3$player1, player2vector)
FRE03_SDMg3$p2inp1vec <- is.element(FRE03_SDMg3$player2, player1vector)

addPlayer1 <- FRE03_SDMg3[ which(FRE03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_SDMg3[ which(FRE03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_SDMg2 <- rbind(FRE03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
FRE03_SDMft <- ftable(FRE03_SDMg2$player1, FRE03_SDMg2$player2)
FRE03_SDMft2 <- as.matrix(FRE03_SDMft)
numRows <- nrow(FRE03_SDMft2)
numCols <- ncol(FRE03_SDMft2)
FRE03_SDMft3 <- FRE03_SDMft2[c(2:numRows) , c(2:numCols)]
FRE03_SDMTable <- graph.adjacency(FRE03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(FRE03_SDMTable, vertex.label = V(FRE03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
FRE03_SDM.clusterCoef <- transitivity(FRE03_SDMTable, type="global") #cluster coefficient
FRE03_SDM.degreeCent <- centralization.degree(FRE03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_SDMftn <- as.network.matrix(FRE03_SDMft)
FRE03_SDM.netDensity <- network.density(FRE03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_SDM.entropy <- entropy(FRE03_SDMft) #entropy

FRE03_SDM.netMx <- cbind(FRE03_SDM.netMx, FRE03_SDM.clusterCoef, FRE03_SDM.degreeCent$centralization,
                         FRE03_SDM.netDensity, FRE03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
FRE03_TDMg2 <- data.frame(FRE03_TDM)
FRE03_TDMg2 <- FRE03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_TDMg2$player1
player2vector <- FRE03_TDMg2$player2
FRE03_TDMg3 <- FRE03_TDMg2
FRE03_TDMg3$p1inp2vec <- is.element(FRE03_TDMg3$player1, player2vector)
FRE03_TDMg3$p2inp1vec <- is.element(FRE03_TDMg3$player2, player1vector)

addPlayer1 <- FRE03_TDMg3[ which(FRE03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_TDMg3[ which(FRE03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_TDMg2 <- rbind(FRE03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
FRE03_TDMft <- ftable(FRE03_TDMg2$player1, FRE03_TDMg2$player2)
FRE03_TDMft2 <- as.matrix(FRE03_TDMft)
numRows <- nrow(FRE03_TDMft2)
numCols <- ncol(FRE03_TDMft2)
FRE03_TDMft3 <- FRE03_TDMft2[c(2:numRows) , c(2:numCols)]
FRE03_TDMTable <- graph.adjacency(FRE03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(FRE03_TDMTable, vertex.label = V(FRE03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
FRE03_TDM.clusterCoef <- transitivity(FRE03_TDMTable, type="global") #cluster coefficient
FRE03_TDM.degreeCent <- centralization.degree(FRE03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_TDMftn <- as.network.matrix(FRE03_TDMft)
FRE03_TDM.netDensity <- network.density(FRE03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_TDM.entropy <- entropy(FRE03_TDMft) #entropy

FRE03_TDM.netMx <- cbind(FRE03_TDM.netMx, FRE03_TDM.clusterCoef, FRE03_TDM.degreeCent$centralization,
                         FRE03_TDM.netDensity, FRE03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
FRE03_SDg2 <- data.frame(FRE03_SD)
FRE03_SDg2 <- FRE03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_SDg2$player1
player2vector <- FRE03_SDg2$player2
FRE03_SDg3 <- FRE03_SDg2
FRE03_SDg3$p1inp2vec <- is.element(FRE03_SDg3$player1, player2vector)
FRE03_SDg3$p2inp1vec <- is.element(FRE03_SDg3$player2, player1vector)

addPlayer1 <- FRE03_SDg3[ which(FRE03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_SDg3[ which(FRE03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_SDg2 <- rbind(FRE03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
FRE03_SDft <- ftable(FRE03_SDg2$player1, FRE03_SDg2$player2)
FRE03_SDft2 <- as.matrix(FRE03_SDft)
numRows <- nrow(FRE03_SDft2)
numCols <- ncol(FRE03_SDft2)
FRE03_SDft3 <- FRE03_SDft2[c(2:numRows) , c(2:numCols)]
FRE03_SDTable <- graph.adjacency(FRE03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(FRE03_SDTable, vertex.label = V(FRE03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
FRE03_SD.clusterCoef <- transitivity(FRE03_SDTable, type="global") #cluster coefficient
FRE03_SD.degreeCent <- centralization.degree(FRE03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_SDftn <- as.network.matrix(FRE03_SDft)
FRE03_SD.netDensity <- network.density(FRE03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_SD.entropy <- entropy(FRE03_SDft) #entropy

FRE03_SD.netMx <- cbind(FRE03_SD.netMx, FRE03_SD.clusterCoef, FRE03_SD.degreeCent$centralization,
                        FRE03_SD.netDensity, FRE03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
FRE03_TDg2 <- data.frame(FRE03_TD)
FRE03_TDg2 <- FRE03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_TDg2$player1
player2vector <- FRE03_TDg2$player2
FRE03_TDg3 <- FRE03_TDg2
FRE03_TDg3$p1inp2vec <- is.element(FRE03_TDg3$player1, player2vector)
FRE03_TDg3$p2inp1vec <- is.element(FRE03_TDg3$player2, player1vector)

addPlayer1 <- FRE03_TDg3[ which(FRE03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_TDg3[ which(FRE03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_TDg2 <- rbind(FRE03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
FRE03_TDft <- ftable(FRE03_TDg2$player1, FRE03_TDg2$player2)
FRE03_TDft2 <- as.matrix(FRE03_TDft)
numRows <- nrow(FRE03_TDft2)
numCols <- ncol(FRE03_TDft2)
FRE03_TDft3 <- FRE03_TDft2[c(2:numRows) , c(2:numCols)]
FRE03_TDTable <- graph.adjacency(FRE03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(FRE03_TDTable, vertex.label = V(FRE03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
FRE03_TD.clusterCoef <- transitivity(FRE03_TDTable, type="global") #cluster coefficient
FRE03_TD.degreeCent <- centralization.degree(FRE03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_TDftn <- as.network.matrix(FRE03_TDft)
FRE03_TD.netDensity <- network.density(FRE03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_TD.entropy <- entropy(FRE03_TDft) #entropy

FRE03_TD.netMx <- cbind(FRE03_TD.netMx, FRE03_TD.clusterCoef, FRE03_TD.degreeCent$centralization,
                        FRE03_TD.netDensity, FRE03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
FRE03_QTg2 <- data.frame(FRE03_QT)
FRE03_QTg2 <- FRE03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE03_QTg2$player1
player2vector <- FRE03_QTg2$player2
FRE03_QTg3 <- FRE03_QTg2
FRE03_QTg3$p1inp2vec <- is.element(FRE03_QTg3$player1, player2vector)
FRE03_QTg3$p2inp1vec <- is.element(FRE03_QTg3$player2, player1vector)

addPlayer1 <- FRE03_QTg3[ which(FRE03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE03_QTg3[ which(FRE03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE03_QTg2 <- rbind(FRE03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
FRE03_QTft <- ftable(FRE03_QTg2$player1, FRE03_QTg2$player2)
FRE03_QTft2 <- as.matrix(FRE03_QTft)
numRows <- nrow(FRE03_QTft2)
numCols <- ncol(FRE03_QTft2)
FRE03_QTft3 <- FRE03_QTft2[c(2:numRows) , c(2:numCols)]
FRE03_QTTable <- graph.adjacency(FRE03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(FRE03_QTTable, vertex.label = V(FRE03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
FRE03_QT.clusterCoef <- transitivity(FRE03_QTTable, type="global") #cluster coefficient
FRE03_QT.degreeCent <- centralization.degree(FRE03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE03_QTftn <- as.network.matrix(FRE03_QTft)
FRE03_QT.netDensity <- network.density(FRE03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE03_QT.entropy <- entropy(FRE03_QTft) #entropy

FRE03_QT.netMx <- cbind(FRE03_QT.netMx, FRE03_QT.clusterCoef, FRE03_QT.degreeCent$centralization,
                        FRE03_QT.netDensity, FRE03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE03_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
GCFC03_Gg2 <- data.frame(GCFC03_G)
GCFC03_Gg2 <- GCFC03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_Gg2$player1
player2vector <- GCFC03_Gg2$player2
GCFC03_Gg3 <- GCFC03_Gg2
GCFC03_Gg3$p1inp2vec <- is.element(GCFC03_Gg3$player1, player2vector)
GCFC03_Gg3$p2inp1vec <- is.element(GCFC03_Gg3$player2, player1vector)

addPlayer1 <- GCFC03_Gg3[ which(GCFC03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_Gg3[ which(GCFC03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_Gg2 <- rbind(GCFC03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
GCFC03_Gft <- ftable(GCFC03_Gg2$player1, GCFC03_Gg2$player2)
GCFC03_Gft2 <- as.matrix(GCFC03_Gft)
numRows <- nrow(GCFC03_Gft2)
numCols <- ncol(GCFC03_Gft2)
GCFC03_Gft3 <- GCFC03_Gft2[c(2:numRows) , c(2:numCols)]
GCFC03_GTable <- graph.adjacency(GCFC03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(GCFC03_GTable, vertex.label = V(GCFC03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
GCFC03_G.clusterCoef <- transitivity(GCFC03_GTable, type="global") #cluster coefficient
GCFC03_G.degreeCent <- centralization.degree(GCFC03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_Gftn <- as.network.matrix(GCFC03_Gft)
GCFC03_G.netDensity <- network.density(GCFC03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_G.entropy <- entropy(GCFC03_Gft) #entropy

GCFC03_G.netMx <- cbind(GCFC03_G.netMx, GCFC03_G.clusterCoef, GCFC03_G.degreeCent$centralization,
                        GCFC03_G.netDensity, GCFC03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
GCFC03_Bg2 <- data.frame(GCFC03_B)
GCFC03_Bg2 <- GCFC03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_Bg2$player1
player2vector <- GCFC03_Bg2$player2
GCFC03_Bg3 <- GCFC03_Bg2
GCFC03_Bg3$p1inp2vec <- is.element(GCFC03_Bg3$player1, player2vector)
GCFC03_Bg3$p2inp1vec <- is.element(GCFC03_Bg3$player2, player1vector)

addPlayer1 <- GCFC03_Bg3[ which(GCFC03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_Bg3[ which(GCFC03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_Bg2 <- rbind(GCFC03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
GCFC03_Bft <- ftable(GCFC03_Bg2$player1, GCFC03_Bg2$player2)
GCFC03_Bft2 <- as.matrix(GCFC03_Bft)
numRows <- nrow(GCFC03_Bft2)
numCols <- ncol(GCFC03_Bft2)
GCFC03_Bft3 <- GCFC03_Bft2[c(2:numRows) , c(2:numCols)]
GCFC03_BTable <- graph.adjacency(GCFC03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(GCFC03_BTable, vertex.label = V(GCFC03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
GCFC03_B.clusterCoef <- transitivity(GCFC03_BTable, type="global") #cluster coefficient
GCFC03_B.degreeCent <- centralization.degree(GCFC03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_Bftn <- as.network.matrix(GCFC03_Bft)
GCFC03_B.netDensity <- network.density(GCFC03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_B.entropy <- entropy(GCFC03_Bft) #entropy

GCFC03_B.netMx <- cbind(GCFC03_B.netMx, GCFC03_B.clusterCoef, GCFC03_B.degreeCent$centralization,
                        GCFC03_B.netDensity, GCFC03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
GCFC03_SFg2 <- data.frame(GCFC03_SF)
GCFC03_SFg2 <- GCFC03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_SFg2$player1
player2vector <- GCFC03_SFg2$player2
GCFC03_SFg3 <- GCFC03_SFg2
GCFC03_SFg3$p1inp2vec <- is.element(GCFC03_SFg3$player1, player2vector)
GCFC03_SFg3$p2inp1vec <- is.element(GCFC03_SFg3$player2, player1vector)

addPlayer1 <- GCFC03_SFg3[ which(GCFC03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC03_SFg2 <- rbind(GCFC03_SFg2, addPlayer1)

#Round 3, FWD Stoppage graph using weighted edges
GCFC03_SFft <- ftable(GCFC03_SFg2$player1, GCFC03_SFg2$player2)
GCFC03_SFft2 <- as.matrix(GCFC03_SFft)
numRows <- nrow(GCFC03_SFft2)
numCols <- ncol(GCFC03_SFft2)
GCFC03_SFft3 <- GCFC03_SFft2[c(2:numRows) , c(1:numCols)]
GCFC03_SFTable <- graph.adjacency(GCFC03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(GCFC03_SFTable, vertex.label = V(GCFC03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
GCFC03_SF.clusterCoef <- transitivity(GCFC03_SFTable, type="global") #cluster coefficient
GCFC03_SF.degreeCent <- centralization.degree(GCFC03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_SFftn <- as.network.matrix(GCFC03_SFft)
GCFC03_SF.netDensity <- network.density(GCFC03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_SF.entropy <- entropy(GCFC03_SFft) #entropy

GCFC03_SF.netMx <- cbind(GCFC03_SF.netMx, GCFC03_SF.clusterCoef, GCFC03_SF.degreeCent$centralization,
                         GCFC03_SF.netDensity, GCFC03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
GCFC03_TFg2 <- data.frame(GCFC03_TF)
GCFC03_TFg2 <- GCFC03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_TFg2$player1
player2vector <- GCFC03_TFg2$player2
GCFC03_TFg3 <- GCFC03_TFg2
GCFC03_TFg3$p1inp2vec <- is.element(GCFC03_TFg3$player1, player2vector)
GCFC03_TFg3$p2inp1vec <- is.element(GCFC03_TFg3$player2, player1vector)

addPlayer1 <- GCFC03_TFg3[ which(GCFC03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_TFg3[ which(GCFC03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_TFg2 <- rbind(GCFC03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
GCFC03_TFft <- ftable(GCFC03_TFg2$player1, GCFC03_TFg2$player2)
GCFC03_TFft2 <- as.matrix(GCFC03_TFft)
numRows <- nrow(GCFC03_TFft2)
numCols <- ncol(GCFC03_TFft2)
GCFC03_TFft3 <- GCFC03_TFft2[c(2:numRows) , c(2:numCols)]
GCFC03_TFTable <- graph.adjacency(GCFC03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(GCFC03_TFTable, vertex.label = V(GCFC03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
GCFC03_TF.clusterCoef <- transitivity(GCFC03_TFTable, type="global") #cluster coefficient
GCFC03_TF.degreeCent <- centralization.degree(GCFC03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_TFftn <- as.network.matrix(GCFC03_TFft)
GCFC03_TF.netDensity <- network.density(GCFC03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_TF.entropy <- entropy(GCFC03_TFft) #entropy

GCFC03_TF.netMx <- cbind(GCFC03_TF.netMx, GCFC03_TF.clusterCoef, GCFC03_TF.degreeCent$centralization,
                         GCFC03_TF.netDensity, GCFC03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
GCFC03_SAMg2 <- data.frame(GCFC03_SAM)
GCFC03_SAMg2 <- GCFC03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_SAMg2$player1
player2vector <- GCFC03_SAMg2$player2
GCFC03_SAMg3 <- GCFC03_SAMg2
GCFC03_SAMg3$p1inp2vec <- is.element(GCFC03_SAMg3$player1, player2vector)
GCFC03_SAMg3$p2inp1vec <- is.element(GCFC03_SAMg3$player2, player1vector)

addPlayer1 <- GCFC03_SAMg3[ which(GCFC03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_SAMg3[ which(GCFC03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_SAMg2 <- rbind(GCFC03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
GCFC03_SAMft <- ftable(GCFC03_SAMg2$player1, GCFC03_SAMg2$player2)
GCFC03_SAMft2 <- as.matrix(GCFC03_SAMft)
numRows <- nrow(GCFC03_SAMft2)
numCols <- ncol(GCFC03_SAMft2)
GCFC03_SAMft3 <- GCFC03_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC03_SAMTable <- graph.adjacency(GCFC03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(GCFC03_SAMTable, vertex.label = V(GCFC03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
GCFC03_SAM.clusterCoef <- transitivity(GCFC03_SAMTable, type="global") #cluster coefficient
GCFC03_SAM.degreeCent <- centralization.degree(GCFC03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_SAMftn <- as.network.matrix(GCFC03_SAMft)
GCFC03_SAM.netDensity <- network.density(GCFC03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_SAM.entropy <- entropy(GCFC03_SAMft) #entropy

GCFC03_SAM.netMx <- cbind(GCFC03_SAM.netMx, GCFC03_SAM.clusterCoef, GCFC03_SAM.degreeCent$centralization,
                          GCFC03_SAM.netDensity, GCFC03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
GCFC03_TAMg2 <- data.frame(GCFC03_TAM)
GCFC03_TAMg2 <- GCFC03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_TAMg2$player1
player2vector <- GCFC03_TAMg2$player2
GCFC03_TAMg3 <- GCFC03_TAMg2
GCFC03_TAMg3$p1inp2vec <- is.element(GCFC03_TAMg3$player1, player2vector)
GCFC03_TAMg3$p2inp1vec <- is.element(GCFC03_TAMg3$player2, player1vector)

addPlayer1 <- GCFC03_TAMg3[ which(GCFC03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_TAMg3[ which(GCFC03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_TAMg2 <- rbind(GCFC03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
GCFC03_TAMft <- ftable(GCFC03_TAMg2$player1, GCFC03_TAMg2$player2)
GCFC03_TAMft2 <- as.matrix(GCFC03_TAMft)
numRows <- nrow(GCFC03_TAMft2)
numCols <- ncol(GCFC03_TAMft2)
GCFC03_TAMft3 <- GCFC03_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC03_TAMTable <- graph.adjacency(GCFC03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(GCFC03_TAMTable, vertex.label = V(GCFC03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
GCFC03_TAM.clusterCoef <- transitivity(GCFC03_TAMTable, type="global") #cluster coefficient
GCFC03_TAM.degreeCent <- centralization.degree(GCFC03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_TAMftn <- as.network.matrix(GCFC03_TAMft)
GCFC03_TAM.netDensity <- network.density(GCFC03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_TAM.entropy <- entropy(GCFC03_TAMft) #entropy

GCFC03_TAM.netMx <- cbind(GCFC03_TAM.netMx, GCFC03_TAM.clusterCoef, GCFC03_TAM.degreeCent$centralization,
                          GCFC03_TAM.netDensity, GCFC03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
GCFC03_SDMg2 <- data.frame(GCFC03_SDM)
GCFC03_SDMg2 <- GCFC03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_SDMg2$player1
player2vector <- GCFC03_SDMg2$player2
GCFC03_SDMg3 <- GCFC03_SDMg2
GCFC03_SDMg3$p1inp2vec <- is.element(GCFC03_SDMg3$player1, player2vector)
GCFC03_SDMg3$p2inp1vec <- is.element(GCFC03_SDMg3$player2, player1vector)

addPlayer1 <- GCFC03_SDMg3[ which(GCFC03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_SDMg3[ which(GCFC03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_SDMg2 <- rbind(GCFC03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
GCFC03_SDMft <- ftable(GCFC03_SDMg2$player1, GCFC03_SDMg2$player2)
GCFC03_SDMft2 <- as.matrix(GCFC03_SDMft)
numRows <- nrow(GCFC03_SDMft2)
numCols <- ncol(GCFC03_SDMft2)
GCFC03_SDMft3 <- GCFC03_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC03_SDMTable <- graph.adjacency(GCFC03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(GCFC03_SDMTable, vertex.label = V(GCFC03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
GCFC03_SDM.clusterCoef <- transitivity(GCFC03_SDMTable, type="global") #cluster coefficient
GCFC03_SDM.degreeCent <- centralization.degree(GCFC03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_SDMftn <- as.network.matrix(GCFC03_SDMft)
GCFC03_SDM.netDensity <- network.density(GCFC03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_SDM.entropy <- entropy(GCFC03_SDMft) #entropy

GCFC03_SDM.netMx <- cbind(GCFC03_SDM.netMx, GCFC03_SDM.clusterCoef, GCFC03_SDM.degreeCent$centralization,
                          GCFC03_SDM.netDensity, GCFC03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
GCFC03_TDMg2 <- data.frame(GCFC03_TDM)
GCFC03_TDMg2 <- GCFC03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_TDMg2$player1
player2vector <- GCFC03_TDMg2$player2
GCFC03_TDMg3 <- GCFC03_TDMg2
GCFC03_TDMg3$p1inp2vec <- is.element(GCFC03_TDMg3$player1, player2vector)
GCFC03_TDMg3$p2inp1vec <- is.element(GCFC03_TDMg3$player2, player1vector)

addPlayer1 <- GCFC03_TDMg3[ which(GCFC03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_TDMg3[ which(GCFC03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_TDMg2 <- rbind(GCFC03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
GCFC03_TDMft <- ftable(GCFC03_TDMg2$player1, GCFC03_TDMg2$player2)
GCFC03_TDMft2 <- as.matrix(GCFC03_TDMft)
numRows <- nrow(GCFC03_TDMft2)
numCols <- ncol(GCFC03_TDMft2)
GCFC03_TDMft3 <- GCFC03_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC03_TDMTable <- graph.adjacency(GCFC03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(GCFC03_TDMTable, vertex.label = V(GCFC03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
GCFC03_TDM.clusterCoef <- transitivity(GCFC03_TDMTable, type="global") #cluster coefficient
GCFC03_TDM.degreeCent <- centralization.degree(GCFC03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_TDMftn <- as.network.matrix(GCFC03_TDMft)
GCFC03_TDM.netDensity <- network.density(GCFC03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_TDM.entropy <- entropy(GCFC03_TDMft) #entropy

GCFC03_TDM.netMx <- cbind(GCFC03_TDM.netMx, GCFC03_TDM.clusterCoef, GCFC03_TDM.degreeCent$centralization,
                          GCFC03_TDM.netDensity, GCFC03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
GCFC03_SDg2 <- data.frame(GCFC03_SD)
GCFC03_SDg2 <- GCFC03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_SDg2$player1
player2vector <- GCFC03_SDg2$player2
GCFC03_SDg3 <- GCFC03_SDg2
GCFC03_SDg3$p1inp2vec <- is.element(GCFC03_SDg3$player1, player2vector)
GCFC03_SDg3$p2inp1vec <- is.element(GCFC03_SDg3$player2, player1vector)

addPlayer1 <- GCFC03_SDg3[ which(GCFC03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_SDg3[ which(GCFC03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_SDg2 <- rbind(GCFC03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
GCFC03_SDft <- ftable(GCFC03_SDg2$player1, GCFC03_SDg2$player2)
GCFC03_SDft2 <- as.matrix(GCFC03_SDft)
numRows <- nrow(GCFC03_SDft2)
numCols <- ncol(GCFC03_SDft2)
GCFC03_SDft3 <- GCFC03_SDft2[c(2:numRows) , c(2:numCols)]
GCFC03_SDTable <- graph.adjacency(GCFC03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(GCFC03_SDTable, vertex.label = V(GCFC03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
GCFC03_SD.clusterCoef <- transitivity(GCFC03_SDTable, type="global") #cluster coefficient
GCFC03_SD.degreeCent <- centralization.degree(GCFC03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_SDftn <- as.network.matrix(GCFC03_SDft)
GCFC03_SD.netDensity <- network.density(GCFC03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_SD.entropy <- entropy(GCFC03_SDft) #entropy

GCFC03_SD.netMx <- cbind(GCFC03_SD.netMx, GCFC03_SD.clusterCoef, GCFC03_SD.degreeCent$centralization,
                         GCFC03_SD.netDensity, GCFC03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
GCFC03_TDg2 <- data.frame(GCFC03_TD)
GCFC03_TDg2 <- GCFC03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_TDg2$player1
player2vector <- GCFC03_TDg2$player2
GCFC03_TDg3 <- GCFC03_TDg2
GCFC03_TDg3$p1inp2vec <- is.element(GCFC03_TDg3$player1, player2vector)
GCFC03_TDg3$p2inp1vec <- is.element(GCFC03_TDg3$player2, player1vector)

addPlayer1 <- GCFC03_TDg3[ which(GCFC03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_TDg3[ which(GCFC03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_TDg2 <- rbind(GCFC03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
GCFC03_TDft <- ftable(GCFC03_TDg2$player1, GCFC03_TDg2$player2)
GCFC03_TDft2 <- as.matrix(GCFC03_TDft)
numRows <- nrow(GCFC03_TDft2)
numCols <- ncol(GCFC03_TDft2)
GCFC03_TDft3 <- GCFC03_TDft2[c(2:numRows) , c(2:numCols)]
GCFC03_TDTable <- graph.adjacency(GCFC03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(GCFC03_TDTable, vertex.label = V(GCFC03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
GCFC03_TD.clusterCoef <- transitivity(GCFC03_TDTable, type="global") #cluster coefficient
GCFC03_TD.degreeCent <- centralization.degree(GCFC03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_TDftn <- as.network.matrix(GCFC03_TDft)
GCFC03_TD.netDensity <- network.density(GCFC03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_TD.entropy <- entropy(GCFC03_TDft) #entropy

GCFC03_TD.netMx <- cbind(GCFC03_TD.netMx, GCFC03_TD.clusterCoef, GCFC03_TD.degreeCent$centralization,
                         GCFC03_TD.netDensity, GCFC03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
GCFC03_QTg2 <- data.frame(GCFC03_QT)
GCFC03_QTg2 <- GCFC03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC03_QTg2$player1
player2vector <- GCFC03_QTg2$player2
GCFC03_QTg3 <- GCFC03_QTg2
GCFC03_QTg3$p1inp2vec <- is.element(GCFC03_QTg3$player1, player2vector)
GCFC03_QTg3$p2inp1vec <- is.element(GCFC03_QTg3$player2, player1vector)

addPlayer1 <- GCFC03_QTg3[ which(GCFC03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC03_QTg3[ which(GCFC03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC03_QTg2 <- rbind(GCFC03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
GCFC03_QTft <- ftable(GCFC03_QTg2$player1, GCFC03_QTg2$player2)
GCFC03_QTft2 <- as.matrix(GCFC03_QTft)
numRows <- nrow(GCFC03_QTft2)
numCols <- ncol(GCFC03_QTft2)
GCFC03_QTft3 <- GCFC03_QTft2[c(2:numRows) , c(2:numCols)]
GCFC03_QTTable <- graph.adjacency(GCFC03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(GCFC03_QTTable, vertex.label = V(GCFC03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
GCFC03_QT.clusterCoef <- transitivity(GCFC03_QTTable, type="global") #cluster coefficient
GCFC03_QT.degreeCent <- centralization.degree(GCFC03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC03_QTftn <- as.network.matrix(GCFC03_QTft)
GCFC03_QT.netDensity <- network.density(GCFC03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC03_QT.entropy <- entropy(GCFC03_QTft) #entropy

GCFC03_QT.netMx <- cbind(GCFC03_QT.netMx, GCFC03_QT.clusterCoef, GCFC03_QT.degreeCent$centralization,
                         GCFC03_QT.netDensity, GCFC03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC03_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
GEEL03_Gg2 <- data.frame(GEEL03_G)
GEEL03_Gg2 <- GEEL03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_Gg2$player1
player2vector <- GEEL03_Gg2$player2
GEEL03_Gg3 <- GEEL03_Gg2
GEEL03_Gg3$p1inp2vec <- is.element(GEEL03_Gg3$player1, player2vector)
GEEL03_Gg3$p2inp1vec <- is.element(GEEL03_Gg3$player2, player1vector)

addPlayer1 <- GEEL03_Gg3[ which(GEEL03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_Gg3[ which(GEEL03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_Gg2 <- rbind(GEEL03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
GEEL03_Gft <- ftable(GEEL03_Gg2$player1, GEEL03_Gg2$player2)
GEEL03_Gft2 <- as.matrix(GEEL03_Gft)
numRows <- nrow(GEEL03_Gft2)
numCols <- ncol(GEEL03_Gft2)
GEEL03_Gft3 <- GEEL03_Gft2[c(2:numRows) , c(2:numCols)]
GEEL03_GTable <- graph.adjacency(GEEL03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(GEEL03_GTable, vertex.label = V(GEEL03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
GEEL03_G.clusterCoef <- transitivity(GEEL03_GTable, type="global") #cluster coefficient
GEEL03_G.degreeCent <- centralization.degree(GEEL03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_Gftn <- as.network.matrix(GEEL03_Gft)
GEEL03_G.netDensity <- network.density(GEEL03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_G.entropy <- entropy(GEEL03_Gft) #entropy

GEEL03_G.netMx <- cbind(GEEL03_G.netMx, GEEL03_G.clusterCoef, GEEL03_G.degreeCent$centralization,
                        GEEL03_G.netDensity, GEEL03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
GEEL03_Bg2 <- data.frame(GEEL03_B)
GEEL03_Bg2 <- GEEL03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_Bg2$player1
player2vector <- GEEL03_Bg2$player2
GEEL03_Bg3 <- GEEL03_Bg2
GEEL03_Bg3$p1inp2vec <- is.element(GEEL03_Bg3$player1, player2vector)
GEEL03_Bg3$p2inp1vec <- is.element(GEEL03_Bg3$player2, player1vector)

addPlayer1 <- GEEL03_Bg3[ which(GEEL03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_Bg3[ which(GEEL03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_Bg2 <- rbind(GEEL03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
GEEL03_Bft <- ftable(GEEL03_Bg2$player1, GEEL03_Bg2$player2)
GEEL03_Bft2 <- as.matrix(GEEL03_Bft)
numRows <- nrow(GEEL03_Bft2)
numCols <- ncol(GEEL03_Bft2)
GEEL03_Bft3 <- GEEL03_Bft2[c(2:numRows) , c(2:numCols)]
GEEL03_BTable <- graph.adjacency(GEEL03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(GEEL03_BTable, vertex.label = V(GEEL03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
GEEL03_B.clusterCoef <- transitivity(GEEL03_BTable, type="global") #cluster coefficient
GEEL03_B.degreeCent <- centralization.degree(GEEL03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_Bftn <- as.network.matrix(GEEL03_Bft)
GEEL03_B.netDensity <- network.density(GEEL03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_B.entropy <- entropy(GEEL03_Bft) #entropy

GEEL03_B.netMx <- cbind(GEEL03_B.netMx, GEEL03_B.clusterCoef, GEEL03_B.degreeCent$centralization,
                        GEEL03_B.netDensity, GEEL03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
GEEL03_SFg2 <- data.frame(GEEL03_SF)
GEEL03_SFg2 <- GEEL03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_SFg2$player1
player2vector <- GEEL03_SFg2$player2
GEEL03_SFg3 <- GEEL03_SFg2
GEEL03_SFg3$p1inp2vec <- is.element(GEEL03_SFg3$player1, player2vector)
GEEL03_SFg3$p2inp1vec <- is.element(GEEL03_SFg3$player2, player1vector)

addPlayer1 <- GEEL03_SFg3[ which(GEEL03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_SFg3[ which(GEEL03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_SFg2 <- rbind(GEEL03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
GEEL03_SFft <- ftable(GEEL03_SFg2$player1, GEEL03_SFg2$player2)
GEEL03_SFft2 <- as.matrix(GEEL03_SFft)
numRows <- nrow(GEEL03_SFft2)
numCols <- ncol(GEEL03_SFft2)
GEEL03_SFft3 <- GEEL03_SFft2[c(2:numRows) , c(2:numCols)]
GEEL03_SFTable <- graph.adjacency(GEEL03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(GEEL03_SFTable, vertex.label = V(GEEL03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
GEEL03_SF.clusterCoef <- transitivity(GEEL03_SFTable, type="global") #cluster coefficient
GEEL03_SF.degreeCent <- centralization.degree(GEEL03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_SFftn <- as.network.matrix(GEEL03_SFft)
GEEL03_SF.netDensity <- network.density(GEEL03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_SF.entropy <- entropy(GEEL03_SFft) #entropy

GEEL03_SF.netMx <- cbind(GEEL03_SF.netMx, GEEL03_SF.clusterCoef, GEEL03_SF.degreeCent$centralization,
                         GEEL03_SF.netDensity, GEEL03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************
#NA

round = 3
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
GEEL03_TFg2 <- data.frame(GEEL03_TF)
GEEL03_TFg2 <- GEEL03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_TFg2$player1
player2vector <- GEEL03_TFg2$player2
GEEL03_TFg3 <- GEEL03_TFg2
GEEL03_TFg3$p1inp2vec <- is.element(GEEL03_TFg3$player1, player2vector)
GEEL03_TFg3$p2inp1vec <- is.element(GEEL03_TFg3$player2, player1vector)

addPlayer1 <- GEEL03_TFg3[ which(GEEL03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_TFg3[ which(GEEL03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_TFg2 <- rbind(GEEL03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
GEEL03_TFft <- ftable(GEEL03_TFg2$player1, GEEL03_TFg2$player2)
GEEL03_TFft2 <- as.matrix(GEEL03_TFft)
numRows <- nrow(GEEL03_TFft2)
numCols <- ncol(GEEL03_TFft2)
GEEL03_TFft3 <- GEEL03_TFft2[c(2:numRows) , c(2:numCols)]
GEEL03_TFTable <- graph.adjacency(GEEL03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(GEEL03_TFTable, vertex.label = V(GEEL03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
GEEL03_TF.clusterCoef <- transitivity(GEEL03_TFTable, type="global") #cluster coefficient
GEEL03_TF.degreeCent <- centralization.degree(GEEL03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_TFftn <- as.network.matrix(GEEL03_TFft)
GEEL03_TF.netDensity <- network.density(GEEL03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_TF.entropy <- entropy(GEEL03_TFft) #entropy

GEEL03_TF.netMx <- cbind(GEEL03_TF.netMx, GEEL03_TF.clusterCoef, GEEL03_TF.degreeCent$centralization,
                         GEEL03_TF.netDensity, GEEL03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
GEEL03_SAMg2 <- data.frame(GEEL03_SAM)
GEEL03_SAMg2 <- GEEL03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_SAMg2$player1
player2vector <- GEEL03_SAMg2$player2
GEEL03_SAMg3 <- GEEL03_SAMg2
GEEL03_SAMg3$p1inp2vec <- is.element(GEEL03_SAMg3$player1, player2vector)
GEEL03_SAMg3$p2inp1vec <- is.element(GEEL03_SAMg3$player2, player1vector)


empty <- ""
zero <- 0
addPlayer2 <- GEEL03_SAMg3[ which(GEEL03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GEEL03_SAMg2 <- rbind(GEEL03_SAMg2, addPlayer2)

#Round 3, AM Stoppage graph using weighted edges
GEEL03_SAMft <- ftable(GEEL03_SAMg2$player1, GEEL03_SAMg2$player2)
GEEL03_SAMft2 <- as.matrix(GEEL03_SAMft)
numRows <- nrow(GEEL03_SAMft2)
numCols <- ncol(GEEL03_SAMft2)
GEEL03_SAMft3 <- GEEL03_SAMft2[c(1:numRows) , c(2:numCols)]
GEEL03_SAMTable <- graph.adjacency(GEEL03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(GEEL03_SAMTable, vertex.label = V(GEEL03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
GEEL03_SAM.clusterCoef <- transitivity(GEEL03_SAMTable, type="global") #cluster coefficient
GEEL03_SAM.degreeCent <- centralization.degree(GEEL03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_SAMftn <- as.network.matrix(GEEL03_SAMft)
GEEL03_SAM.netDensity <- network.density(GEEL03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_SAM.entropy <- entropy(GEEL03_SAMft) #entropy

GEEL03_SAM.netMx <- cbind(GEEL03_SAM.netMx, GEEL03_SAM.clusterCoef, GEEL03_SAM.degreeCent$centralization,
                          GEEL03_SAM.netDensity, GEEL03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
GEEL03_TAMg2 <- data.frame(GEEL03_TAM)
GEEL03_TAMg2 <- GEEL03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_TAMg2$player1
player2vector <- GEEL03_TAMg2$player2
GEEL03_TAMg3 <- GEEL03_TAMg2
GEEL03_TAMg3$p1inp2vec <- is.element(GEEL03_TAMg3$player1, player2vector)
GEEL03_TAMg3$p2inp1vec <- is.element(GEEL03_TAMg3$player2, player1vector)

addPlayer1 <- GEEL03_TAMg3[ which(GEEL03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_TAMg3[ which(GEEL03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_TAMg2 <- rbind(GEEL03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
GEEL03_TAMft <- ftable(GEEL03_TAMg2$player1, GEEL03_TAMg2$player2)
GEEL03_TAMft2 <- as.matrix(GEEL03_TAMft)
numRows <- nrow(GEEL03_TAMft2)
numCols <- ncol(GEEL03_TAMft2)
GEEL03_TAMft3 <- GEEL03_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL03_TAMTable <- graph.adjacency(GEEL03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(GEEL03_TAMTable, vertex.label = V(GEEL03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
GEEL03_TAM.clusterCoef <- transitivity(GEEL03_TAMTable, type="global") #cluster coefficient
GEEL03_TAM.degreeCent <- centralization.degree(GEEL03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_TAMftn <- as.network.matrix(GEEL03_TAMft)
GEEL03_TAM.netDensity <- network.density(GEEL03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_TAM.entropy <- entropy(GEEL03_TAMft) #entropy

GEEL03_TAM.netMx <- cbind(GEEL03_TAM.netMx, GEEL03_TAM.clusterCoef, GEEL03_TAM.degreeCent$centralization,
                          GEEL03_TAM.netDensity, GEEL03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
GEEL03_SDMg2 <- data.frame(GEEL03_SDM)
GEEL03_SDMg2 <- GEEL03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_SDMg2$player1
player2vector <- GEEL03_SDMg2$player2
GEEL03_SDMg3 <- GEEL03_SDMg2
GEEL03_SDMg3$p1inp2vec <- is.element(GEEL03_SDMg3$player1, player2vector)
GEEL03_SDMg3$p2inp1vec <- is.element(GEEL03_SDMg3$player2, player1vector)

addPlayer1 <- GEEL03_SDMg3[ which(GEEL03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_SDMg3[ which(GEEL03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_SDMg2 <- rbind(GEEL03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
GEEL03_SDMft <- ftable(GEEL03_SDMg2$player1, GEEL03_SDMg2$player2)
GEEL03_SDMft2 <- as.matrix(GEEL03_SDMft)
numRows <- nrow(GEEL03_SDMft2)
numCols <- ncol(GEEL03_SDMft2)
GEEL03_SDMft3 <- GEEL03_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL03_SDMTable <- graph.adjacency(GEEL03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(GEEL03_SDMTable, vertex.label = V(GEEL03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
GEEL03_SDM.clusterCoef <- transitivity(GEEL03_SDMTable, type="global") #cluster coefficient
GEEL03_SDM.degreeCent <- centralization.degree(GEEL03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_SDMftn <- as.network.matrix(GEEL03_SDMft)
GEEL03_SDM.netDensity <- network.density(GEEL03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_SDM.entropy <- entropy(GEEL03_SDMft) #entropy

GEEL03_SDM.netMx <- cbind(GEEL03_SDM.netMx, GEEL03_SDM.clusterCoef, GEEL03_SDM.degreeCent$centralization,
                          GEEL03_SDM.netDensity, GEEL03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
GEEL03_TDMg2 <- data.frame(GEEL03_TDM)
GEEL03_TDMg2 <- GEEL03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_TDMg2$player1
player2vector <- GEEL03_TDMg2$player2
GEEL03_TDMg3 <- GEEL03_TDMg2
GEEL03_TDMg3$p1inp2vec <- is.element(GEEL03_TDMg3$player1, player2vector)
GEEL03_TDMg3$p2inp1vec <- is.element(GEEL03_TDMg3$player2, player1vector)

addPlayer1 <- GEEL03_TDMg3[ which(GEEL03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_TDMg3[ which(GEEL03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_TDMg2 <- rbind(GEEL03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
GEEL03_TDMft <- ftable(GEEL03_TDMg2$player1, GEEL03_TDMg2$player2)
GEEL03_TDMft2 <- as.matrix(GEEL03_TDMft)
numRows <- nrow(GEEL03_TDMft2)
numCols <- ncol(GEEL03_TDMft2)
GEEL03_TDMft3 <- GEEL03_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL03_TDMTable <- graph.adjacency(GEEL03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(GEEL03_TDMTable, vertex.label = V(GEEL03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
GEEL03_TDM.clusterCoef <- transitivity(GEEL03_TDMTable, type="global") #cluster coefficient
GEEL03_TDM.degreeCent <- centralization.degree(GEEL03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_TDMftn <- as.network.matrix(GEEL03_TDMft)
GEEL03_TDM.netDensity <- network.density(GEEL03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_TDM.entropy <- entropy(GEEL03_TDMft) #entropy

GEEL03_TDM.netMx <- cbind(GEEL03_TDM.netMx, GEEL03_TDM.clusterCoef, GEEL03_TDM.degreeCent$centralization,
                          GEEL03_TDM.netDensity, GEEL03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
GEEL03_SDg2 <- data.frame(GEEL03_SD)
GEEL03_SDg2 <- GEEL03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_SDg2$player1
player2vector <- GEEL03_SDg2$player2
GEEL03_SDg3 <- GEEL03_SDg2
GEEL03_SDg3$p1inp2vec <- is.element(GEEL03_SDg3$player1, player2vector)
GEEL03_SDg3$p2inp1vec <- is.element(GEEL03_SDg3$player2, player1vector)

addPlayer1 <- GEEL03_SDg3[ which(GEEL03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_SDg3[ which(GEEL03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_SDg2 <- rbind(GEEL03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
GEEL03_SDft <- ftable(GEEL03_SDg2$player1, GEEL03_SDg2$player2)
GEEL03_SDft2 <- as.matrix(GEEL03_SDft)
numRows <- nrow(GEEL03_SDft2)
numCols <- ncol(GEEL03_SDft2)
GEEL03_SDft3 <- GEEL03_SDft2[c(2:numRows) , c(2:numCols)]
GEEL03_SDTable <- graph.adjacency(GEEL03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(GEEL03_SDTable, vertex.label = V(GEEL03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
GEEL03_SD.clusterCoef <- transitivity(GEEL03_SDTable, type="global") #cluster coefficient
GEEL03_SD.degreeCent <- centralization.degree(GEEL03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_SDftn <- as.network.matrix(GEEL03_SDft)
GEEL03_SD.netDensity <- network.density(GEEL03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_SD.entropy <- entropy(GEEL03_SDft) #entropy

GEEL03_SD.netMx <- cbind(GEEL03_SD.netMx, GEEL03_SD.clusterCoef, GEEL03_SD.degreeCent$centralization,
                         GEEL03_SD.netDensity, GEEL03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA
round = 3
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
GEEL03_TDg2 <- data.frame(GEEL03_TD)
GEEL03_TDg2 <- GEEL03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_TDg2$player1
player2vector <- GEEL03_TDg2$player2
GEEL03_TDg3 <- GEEL03_TDg2
GEEL03_TDg3$p1inp2vec <- is.element(GEEL03_TDg3$player1, player2vector)
GEEL03_TDg3$p2inp1vec <- is.element(GEEL03_TDg3$player2, player1vector)

addPlayer1 <- GEEL03_TDg3[ which(GEEL03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_TDg3[ which(GEEL03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_TDg2 <- rbind(GEEL03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
GEEL03_TDft <- ftable(GEEL03_TDg2$player1, GEEL03_TDg2$player2)
GEEL03_TDft2 <- as.matrix(GEEL03_TDft)
numRows <- nrow(GEEL03_TDft2)
numCols <- ncol(GEEL03_TDft2)
GEEL03_TDft3 <- GEEL03_TDft2[c(2:numRows) , c(2:numCols)]
GEEL03_TDTable <- graph.adjacency(GEEL03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(GEEL03_TDTable, vertex.label = V(GEEL03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
GEEL03_TD.clusterCoef <- transitivity(GEEL03_TDTable, type="global") #cluster coefficient
GEEL03_TD.degreeCent <- centralization.degree(GEEL03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_TDftn <- as.network.matrix(GEEL03_TDft)
GEEL03_TD.netDensity <- network.density(GEEL03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_TD.entropy <- entropy(GEEL03_TDft) #entropy

GEEL03_TD.netMx <- cbind(GEEL03_TD.netMx, GEEL03_TD.clusterCoef, GEEL03_TD.degreeCent$centralization,
                         GEEL03_TD.netDensity, GEEL03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
GEEL03_QTg2 <- data.frame(GEEL03_QT)
GEEL03_QTg2 <- GEEL03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL03_QTg2$player1
player2vector <- GEEL03_QTg2$player2
GEEL03_QTg3 <- GEEL03_QTg2
GEEL03_QTg3$p1inp2vec <- is.element(GEEL03_QTg3$player1, player2vector)
GEEL03_QTg3$p2inp1vec <- is.element(GEEL03_QTg3$player2, player1vector)

addPlayer1 <- GEEL03_QTg3[ which(GEEL03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL03_QTg3[ which(GEEL03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL03_QTg2 <- rbind(GEEL03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
GEEL03_QTft <- ftable(GEEL03_QTg2$player1, GEEL03_QTg2$player2)
GEEL03_QTft2 <- as.matrix(GEEL03_QTft)
numRows <- nrow(GEEL03_QTft2)
numCols <- ncol(GEEL03_QTft2)
GEEL03_QTft3 <- GEEL03_QTft2[c(2:numRows) , c(2:numCols)]
GEEL03_QTTable <- graph.adjacency(GEEL03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(GEEL03_QTTable, vertex.label = V(GEEL03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
GEEL03_QT.clusterCoef <- transitivity(GEEL03_QTTable, type="global") #cluster coefficient
GEEL03_QT.degreeCent <- centralization.degree(GEEL03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL03_QTftn <- as.network.matrix(GEEL03_QTft)
GEEL03_QT.netDensity <- network.density(GEEL03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL03_QT.entropy <- entropy(GEEL03_QTft) #entropy

GEEL03_QT.netMx <- cbind(GEEL03_QT.netMx, GEEL03_QT.clusterCoef, GEEL03_QT.degreeCent$centralization,
                         GEEL03_QT.netDensity, GEEL03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL03_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "GWS"
KIoutcome = "Goal_F"
GWS03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
GWS03_Gg2 <- data.frame(GWS03_G)
GWS03_Gg2 <- GWS03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_Gg2$player1
player2vector <- GWS03_Gg2$player2
GWS03_Gg3 <- GWS03_Gg2
GWS03_Gg3$p1inp2vec <- is.element(GWS03_Gg3$player1, player2vector)
GWS03_Gg3$p2inp1vec <- is.element(GWS03_Gg3$player2, player1vector)

addPlayer1 <- GWS03_Gg3[ which(GWS03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_Gg3[ which(GWS03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_Gg2 <- rbind(GWS03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
GWS03_Gft <- ftable(GWS03_Gg2$player1, GWS03_Gg2$player2)
GWS03_Gft2 <- as.matrix(GWS03_Gft)
numRows <- nrow(GWS03_Gft2)
numCols <- ncol(GWS03_Gft2)
GWS03_Gft3 <- GWS03_Gft2[c(2:numRows) , c(2:numCols)]
GWS03_GTable <- graph.adjacency(GWS03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(GWS03_GTable, vertex.label = V(GWS03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
GWS03_G.clusterCoef <- transitivity(GWS03_GTable, type="global") #cluster coefficient
GWS03_G.degreeCent <- centralization.degree(GWS03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_Gftn <- as.network.matrix(GWS03_Gft)
GWS03_G.netDensity <- network.density(GWS03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_G.entropy <- entropy(GWS03_Gft) #entropy

GWS03_G.netMx <- cbind(GWS03_G.netMx, GWS03_G.clusterCoef, GWS03_G.degreeCent$centralization,
                       GWS03_G.netDensity, GWS03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_G.netMx) <- varnames

#Round 3, Behind***************************************************************

round = 3
teamName = "GWS"
KIoutcome = "Behind_F"
GWS03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
GWS03_Bg2 <- data.frame(GWS03_B)
GWS03_Bg2 <- GWS03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_Bg2$player1
player2vector <- GWS03_Bg2$player2
GWS03_Bg3 <- GWS03_Bg2
GWS03_Bg3$p1inp2vec <- is.element(GWS03_Bg3$player1, player2vector)
GWS03_Bg3$p2inp1vec <- is.element(GWS03_Bg3$player2, player1vector)

addPlayer1 <- GWS03_Bg3[ which(GWS03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_Bg3[ which(GWS03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_Bg2 <- rbind(GWS03_Bg2, addPlayers)


#Round 3, Behind graph using weighted edges
GWS03_Bft <- ftable(GWS03_Bg2$player1, GWS03_Bg2$player2)
GWS03_Bft2 <- as.matrix(GWS03_Bft)
numRows <- nrow(GWS03_Bft2)
numCols <- ncol(GWS03_Bft2)
GWS03_Bft3 <- GWS03_Bft2[c(2:numRows) , c(2:numCols)]
GWS03_BTable <- graph.adjacency(GWS03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(GWS03_BTable, vertex.label = V(GWS03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
GWS03_B.clusterCoef <- transitivity(GWS03_BTable, type="global") #cluster coefficient
GWS03_B.degreeCent <- centralization.degree(GWS03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_Bftn <- as.network.matrix(GWS03_Bft)
GWS03_B.netDensity <- network.density(GWS03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_B.entropy <- entropy(GWS03_Bft) #entropy

GWS03_B.netMx <- cbind(GWS03_B.netMx, GWS03_B.clusterCoef, GWS03_B.degreeCent$centralization,
                       GWS03_B.netDensity, GWS03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
GWS03_SFg2 <- data.frame(GWS03_SF)
GWS03_SFg2 <- GWS03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_SFg2$player1
player2vector <- GWS03_SFg2$player2
GWS03_SFg3 <- GWS03_SFg2
GWS03_SFg3$p1inp2vec <- is.element(GWS03_SFg3$player1, player2vector)
GWS03_SFg3$p2inp1vec <- is.element(GWS03_SFg3$player2, player1vector)

addPlayer1 <- GWS03_SFg3[ which(GWS03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_SFg3[ which(GWS03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_SFg2 <- rbind(GWS03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
GWS03_SFft <- ftable(GWS03_SFg2$player1, GWS03_SFg2$player2)
GWS03_SFft2 <- as.matrix(GWS03_SFft)
numRows <- nrow(GWS03_SFft2)
numCols <- ncol(GWS03_SFft2)
GWS03_SFft3 <- GWS03_SFft2[c(2:numRows) , c(2:numCols)]
GWS03_SFTable <- graph.adjacency(GWS03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(GWS03_SFTable, vertex.label = V(GWS03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
GWS03_SF.clusterCoef <- transitivity(GWS03_SFTable, type="global") #cluster coefficient
GWS03_SF.degreeCent <- centralization.degree(GWS03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_SFftn <- as.network.matrix(GWS03_SFft)
GWS03_SF.netDensity <- network.density(GWS03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_SF.entropy <- entropy(GWS03_SFft) #entropy

GWS03_SF.netMx <- cbind(GWS03_SF.netMx, GWS03_SF.clusterCoef, GWS03_SF.degreeCent$centralization,
                        GWS03_SF.netDensity, GWS03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
GWS03_TFg2 <- data.frame(GWS03_TF)
GWS03_TFg2 <- GWS03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_TFg2$player1
player2vector <- GWS03_TFg2$player2
GWS03_TFg3 <- GWS03_TFg2
GWS03_TFg3$p1inp2vec <- is.element(GWS03_TFg3$player1, player2vector)
GWS03_TFg3$p2inp1vec <- is.element(GWS03_TFg3$player2, player1vector)

addPlayer1 <- GWS03_TFg3[ which(GWS03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_TFg3[ which(GWS03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_TFg2 <- rbind(GWS03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
GWS03_TFft <- ftable(GWS03_TFg2$player1, GWS03_TFg2$player2)
GWS03_TFft2 <- as.matrix(GWS03_TFft)
numRows <- nrow(GWS03_TFft2)
numCols <- ncol(GWS03_TFft2)
GWS03_TFft3 <- GWS03_TFft2[c(2:numRows) , c(2:numCols)]
GWS03_TFTable <- graph.adjacency(GWS03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(GWS03_TFTable, vertex.label = V(GWS03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
GWS03_TF.clusterCoef <- transitivity(GWS03_TFTable, type="global") #cluster coefficient
GWS03_TF.degreeCent <- centralization.degree(GWS03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_TFftn <- as.network.matrix(GWS03_TFft)
GWS03_TF.netDensity <- network.density(GWS03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_TF.entropy <- entropy(GWS03_TFft) #entropy

GWS03_TF.netMx <- cbind(GWS03_TF.netMx, GWS03_TF.clusterCoef, GWS03_TF.degreeCent$centralization,
                        GWS03_TF.netDensity, GWS03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
GWS03_SAMg2 <- data.frame(GWS03_SAM)
GWS03_SAMg2 <- GWS03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_SAMg2$player1
player2vector <- GWS03_SAMg2$player2
GWS03_SAMg3 <- GWS03_SAMg2
GWS03_SAMg3$p1inp2vec <- is.element(GWS03_SAMg3$player1, player2vector)
GWS03_SAMg3$p2inp1vec <- is.element(GWS03_SAMg3$player2, player1vector)

addPlayer1 <- GWS03_SAMg3[ which(GWS03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_SAMg3[ which(GWS03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_SAMg2 <- rbind(GWS03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
GWS03_SAMft <- ftable(GWS03_SAMg2$player1, GWS03_SAMg2$player2)
GWS03_SAMft2 <- as.matrix(GWS03_SAMft)
numRows <- nrow(GWS03_SAMft2)
numCols <- ncol(GWS03_SAMft2)
GWS03_SAMft3 <- GWS03_SAMft2[c(2:numRows) , c(2:numCols)]
GWS03_SAMTable <- graph.adjacency(GWS03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(GWS03_SAMTable, vertex.label = V(GWS03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
GWS03_SAM.clusterCoef <- transitivity(GWS03_SAMTable, type="global") #cluster coefficient
GWS03_SAM.degreeCent <- centralization.degree(GWS03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_SAMftn <- as.network.matrix(GWS03_SAMft)
GWS03_SAM.netDensity <- network.density(GWS03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_SAM.entropy <- entropy(GWS03_SAMft) #entropy

GWS03_SAM.netMx <- cbind(GWS03_SAM.netMx, GWS03_SAM.clusterCoef, GWS03_SAM.degreeCent$centralization,
                         GWS03_SAM.netDensity, GWS03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
GWS03_TAMg2 <- data.frame(GWS03_TAM)
GWS03_TAMg2 <- GWS03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_TAMg2$player1
player2vector <- GWS03_TAMg2$player2
GWS03_TAMg3 <- GWS03_TAMg2
GWS03_TAMg3$p1inp2vec <- is.element(GWS03_TAMg3$player1, player2vector)
GWS03_TAMg3$p2inp1vec <- is.element(GWS03_TAMg3$player2, player1vector)

addPlayer1 <- GWS03_TAMg3[ which(GWS03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_TAMg3[ which(GWS03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_TAMg2 <- rbind(GWS03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
GWS03_TAMft <- ftable(GWS03_TAMg2$player1, GWS03_TAMg2$player2)
GWS03_TAMft2 <- as.matrix(GWS03_TAMft)
numRows <- nrow(GWS03_TAMft2)
numCols <- ncol(GWS03_TAMft2)
GWS03_TAMft3 <- GWS03_TAMft2[c(2:numRows) , c(2:numCols)]
GWS03_TAMTable <- graph.adjacency(GWS03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(GWS03_TAMTable, vertex.label = V(GWS03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
GWS03_TAM.clusterCoef <- transitivity(GWS03_TAMTable, type="global") #cluster coefficient
GWS03_TAM.degreeCent <- centralization.degree(GWS03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_TAMftn <- as.network.matrix(GWS03_TAMft)
GWS03_TAM.netDensity <- network.density(GWS03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_TAM.entropy <- entropy(GWS03_TAMft) #entropy

GWS03_TAM.netMx <- cbind(GWS03_TAM.netMx, GWS03_TAM.clusterCoef, GWS03_TAM.degreeCent$centralization,
                         GWS03_TAM.netDensity, GWS03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
GWS03_SDMg2 <- data.frame(GWS03_SDM)
GWS03_SDMg2 <- GWS03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_SDMg2$player1
player2vector <- GWS03_SDMg2$player2
GWS03_SDMg3 <- GWS03_SDMg2
GWS03_SDMg3$p1inp2vec <- is.element(GWS03_SDMg3$player1, player2vector)
GWS03_SDMg3$p2inp1vec <- is.element(GWS03_SDMg3$player2, player1vector)

addPlayer1 <- GWS03_SDMg3[ which(GWS03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_SDMg3[ which(GWS03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_SDMg2 <- rbind(GWS03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
GWS03_SDMft <- ftable(GWS03_SDMg2$player1, GWS03_SDMg2$player2)
GWS03_SDMft2 <- as.matrix(GWS03_SDMft)
numRows <- nrow(GWS03_SDMft2)
numCols <- ncol(GWS03_SDMft2)
GWS03_SDMft3 <- GWS03_SDMft2[c(2:numRows) , c(2:numCols)]
GWS03_SDMTable <- graph.adjacency(GWS03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(GWS03_SDMTable, vertex.label = V(GWS03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
GWS03_SDM.clusterCoef <- transitivity(GWS03_SDMTable, type="global") #cluster coefficient
GWS03_SDM.degreeCent <- centralization.degree(GWS03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_SDMftn <- as.network.matrix(GWS03_SDMft)
GWS03_SDM.netDensity <- network.density(GWS03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_SDM.entropy <- entropy(GWS03_SDMft) #entropy

GWS03_SDM.netMx <- cbind(GWS03_SDM.netMx, GWS03_SDM.clusterCoef, GWS03_SDM.degreeCent$centralization,
                         GWS03_SDM.netDensity, GWS03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
GWS03_TDMg2 <- data.frame(GWS03_TDM)
GWS03_TDMg2 <- GWS03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_TDMg2$player1
player2vector <- GWS03_TDMg2$player2
GWS03_TDMg3 <- GWS03_TDMg2
GWS03_TDMg3$p1inp2vec <- is.element(GWS03_TDMg3$player1, player2vector)
GWS03_TDMg3$p2inp1vec <- is.element(GWS03_TDMg3$player2, player1vector)

addPlayer1 <- GWS03_TDMg3[ which(GWS03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_TDMg3[ which(GWS03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_TDMg2 <- rbind(GWS03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
GWS03_TDMft <- ftable(GWS03_TDMg2$player1, GWS03_TDMg2$player2)
GWS03_TDMft2 <- as.matrix(GWS03_TDMft)
numRows <- nrow(GWS03_TDMft2)
numCols <- ncol(GWS03_TDMft2)
GWS03_TDMft3 <- GWS03_TDMft2[c(2:numRows) , c(2:numCols)]
GWS03_TDMTable <- graph.adjacency(GWS03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(GWS03_TDMTable, vertex.label = V(GWS03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
GWS03_TDM.clusterCoef <- transitivity(GWS03_TDMTable, type="global") #cluster coefficient
GWS03_TDM.degreeCent <- centralization.degree(GWS03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_TDMftn <- as.network.matrix(GWS03_TDMft)
GWS03_TDM.netDensity <- network.density(GWS03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_TDM.entropy <- entropy(GWS03_TDMft) #entropy

GWS03_TDM.netMx <- cbind(GWS03_TDM.netMx, GWS03_TDM.clusterCoef, GWS03_TDM.degreeCent$centralization,
                         GWS03_TDM.netDensity, GWS03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
GWS03_SDg2 <- data.frame(GWS03_SD)
GWS03_SDg2 <- GWS03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_SDg2$player1
player2vector <- GWS03_SDg2$player2
GWS03_SDg3 <- GWS03_SDg2
GWS03_SDg3$p1inp2vec <- is.element(GWS03_SDg3$player1, player2vector)
GWS03_SDg3$p2inp1vec <- is.element(GWS03_SDg3$player2, player1vector)

addPlayer1 <- GWS03_SDg3[ which(GWS03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_SDg3[ which(GWS03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_SDg2 <- rbind(GWS03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
GWS03_SDft <- ftable(GWS03_SDg2$player1, GWS03_SDg2$player2)
GWS03_SDft2 <- as.matrix(GWS03_SDft)
numRows <- nrow(GWS03_SDft2)
numCols <- ncol(GWS03_SDft2)
GWS03_SDft3 <- GWS03_SDft2[c(2:numRows) , c(2:numCols)]
GWS03_SDTable <- graph.adjacency(GWS03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(GWS03_SDTable, vertex.label = V(GWS03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
GWS03_SD.clusterCoef <- transitivity(GWS03_SDTable, type="global") #cluster coefficient
GWS03_SD.degreeCent <- centralization.degree(GWS03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_SDftn <- as.network.matrix(GWS03_SDft)
GWS03_SD.netDensity <- network.density(GWS03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_SD.entropy <- entropy(GWS03_SDft) #entropy

GWS03_SD.netMx <- cbind(GWS03_SD.netMx, GWS03_SD.clusterCoef, GWS03_SD.degreeCent$centralization,
                        GWS03_SD.netDensity, GWS03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
GWS03_TDg2 <- data.frame(GWS03_TD)
GWS03_TDg2 <- GWS03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_TDg2$player1
player2vector <- GWS03_TDg2$player2
GWS03_TDg3 <- GWS03_TDg2
GWS03_TDg3$p1inp2vec <- is.element(GWS03_TDg3$player1, player2vector)
GWS03_TDg3$p2inp1vec <- is.element(GWS03_TDg3$player2, player1vector)

addPlayer1 <- GWS03_TDg3[ which(GWS03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_TDg3[ which(GWS03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_TDg2 <- rbind(GWS03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
GWS03_TDft <- ftable(GWS03_TDg2$player1, GWS03_TDg2$player2)
GWS03_TDft2 <- as.matrix(GWS03_TDft)
numRows <- nrow(GWS03_TDft2)
numCols <- ncol(GWS03_TDft2)
GWS03_TDft3 <- GWS03_TDft2[c(2:numRows) , c(2:numCols)]
GWS03_TDTable <- graph.adjacency(GWS03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(GWS03_TDTable, vertex.label = V(GWS03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
GWS03_TD.clusterCoef <- transitivity(GWS03_TDTable, type="global") #cluster coefficient
GWS03_TD.degreeCent <- centralization.degree(GWS03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_TDftn <- as.network.matrix(GWS03_TDft)
GWS03_TD.netDensity <- network.density(GWS03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_TD.entropy <- entropy(GWS03_TDft) #entropy

GWS03_TD.netMx <- cbind(GWS03_TD.netMx, GWS03_TD.clusterCoef, GWS03_TD.degreeCent$centralization,
                        GWS03_TD.netDensity, GWS03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************

round = 3
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
GWS03_QTg2 <- data.frame(GWS03_QT)
GWS03_QTg2 <- GWS03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS03_QTg2$player1
player2vector <- GWS03_QTg2$player2
GWS03_QTg3 <- GWS03_QTg2
GWS03_QTg3$p1inp2vec <- is.element(GWS03_QTg3$player1, player2vector)
GWS03_QTg3$p2inp1vec <- is.element(GWS03_QTg3$player2, player1vector)

addPlayer1 <- GWS03_QTg3[ which(GWS03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS03_QTg3[ which(GWS03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS03_QTg2 <- rbind(GWS03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
GWS03_QTft <- ftable(GWS03_QTg2$player1, GWS03_QTg2$player2)
GWS03_QTft2 <- as.matrix(GWS03_QTft)
numRows <- nrow(GWS03_QTft2)
numCols <- ncol(GWS03_QTft2)
GWS03_QTft3 <- GWS03_QTft2[c(2:numRows) , c(2:numCols)]
GWS03_QTTable <- graph.adjacency(GWS03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(GWS03_QTTable, vertex.label = V(GWS03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
GWS03_QT.clusterCoef <- transitivity(GWS03_QTTable, type="global") #cluster coefficient
GWS03_QT.degreeCent <- centralization.degree(GWS03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS03_QTftn <- as.network.matrix(GWS03_QTft)
GWS03_QT.netDensity <- network.density(GWS03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS03_QT.entropy <- entropy(GWS03_QTft) #entropy

GWS03_QT.netMx <- cbind(GWS03_QT.netMx, GWS03_QT.clusterCoef, GWS03_QT.degreeCent$centralization,
                        GWS03_QT.netDensity, GWS03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS03_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "HAW"
KIoutcome = "Goal_F"
HAW03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
HAW03_Gg2 <- data.frame(HAW03_G)
HAW03_Gg2 <- HAW03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_Gg2$player1
player2vector <- HAW03_Gg2$player2
HAW03_Gg3 <- HAW03_Gg2
HAW03_Gg3$p1inp2vec <- is.element(HAW03_Gg3$player1, player2vector)
HAW03_Gg3$p2inp1vec <- is.element(HAW03_Gg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- HAW03_Gg3[ which(HAW03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

HAW03_Gg2 <- rbind(HAW03_Gg2, addPlayer2)

#Round 3, Goal graph using weighted edges
HAW03_Gft <- ftable(HAW03_Gg2$player1, HAW03_Gg2$player2)
HAW03_Gft2 <- as.matrix(HAW03_Gft)
numRows <- nrow(HAW03_Gft2)
numCols <- ncol(HAW03_Gft2)
HAW03_Gft3 <- HAW03_Gft2[c(1:numRows) , c(2:numCols)]
HAW03_GTable <- graph.adjacency(HAW03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(HAW03_GTable, vertex.label = V(HAW03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
HAW03_G.clusterCoef <- transitivity(HAW03_GTable, type="global") #cluster coefficient
HAW03_G.degreeCent <- centralization.degree(HAW03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_Gftn <- as.network.matrix(HAW03_Gft)
HAW03_G.netDensity <- network.density(HAW03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_G.entropy <- entropy(HAW03_Gft) #entropy

HAW03_G.netMx <- cbind(HAW03_G.netMx, HAW03_G.clusterCoef, HAW03_G.degreeCent$centralization,
                       HAW03_G.netDensity, HAW03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_G.netMx) <- varnames

#Round 3, Behind***************************************************************

round = 3
teamName = "HAW"
KIoutcome = "Behind_F"
HAW03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
HAW03_Bg2 <- data.frame(HAW03_B)
HAW03_Bg2 <- HAW03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_Bg2$player1
player2vector <- HAW03_Bg2$player2
HAW03_Bg3 <- HAW03_Bg2
HAW03_Bg3$p1inp2vec <- is.element(HAW03_Bg3$player1, player2vector)
HAW03_Bg3$p2inp1vec <- is.element(HAW03_Bg3$player2, player1vector)

addPlayer1 <- HAW03_Bg3[ which(HAW03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_Bg3[ which(HAW03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_Bg2 <- rbind(HAW03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
HAW03_Bft <- ftable(HAW03_Bg2$player1, HAW03_Bg2$player2)
HAW03_Bft2 <- as.matrix(HAW03_Bft)
numRows <- nrow(HAW03_Bft2)
numCols <- ncol(HAW03_Bft2)
HAW03_Bft3 <- HAW03_Bft2[c(2:numRows) , c(2:numCols)]
HAW03_BTable <- graph.adjacency(HAW03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(HAW03_BTable, vertex.label = V(HAW03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
HAW03_B.clusterCoef <- transitivity(HAW03_BTable, type="global") #cluster coefficient
HAW03_B.degreeCent <- centralization.degree(HAW03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_Bftn <- as.network.matrix(HAW03_Bft)
HAW03_B.netDensity <- network.density(HAW03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_B.entropy <- entropy(HAW03_Bft) #entropy

HAW03_B.netMx <- cbind(HAW03_B.netMx, HAW03_B.clusterCoef, HAW03_B.degreeCent$centralization,
                       HAW03_B.netDensity, HAW03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
HAW03_SFg2 <- data.frame(HAW03_SF)
HAW03_SFg2 <- HAW03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_SFg2$player1
player2vector <- HAW03_SFg2$player2
HAW03_SFg3 <- HAW03_SFg2
HAW03_SFg3$p1inp2vec <- is.element(HAW03_SFg3$player1, player2vector)
HAW03_SFg3$p2inp1vec <- is.element(HAW03_SFg3$player2, player1vector)

addPlayer1 <- HAW03_SFg3[ which(HAW03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_SFg3[ which(HAW03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_SFg2 <- rbind(HAW03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
HAW03_SFft <- ftable(HAW03_SFg2$player1, HAW03_SFg2$player2)
HAW03_SFft2 <- as.matrix(HAW03_SFft)
numRows <- nrow(HAW03_SFft2)
numCols <- ncol(HAW03_SFft2)
HAW03_SFft3 <- HAW03_SFft2[c(2:numRows) , c(2:numCols)]
HAW03_SFTable <- graph.adjacency(HAW03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(HAW03_SFTable, vertex.label = V(HAW03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
HAW03_SF.clusterCoef <- transitivity(HAW03_SFTable, type="global") #cluster coefficient
HAW03_SF.degreeCent <- centralization.degree(HAW03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_SFftn <- as.network.matrix(HAW03_SFft)
HAW03_SF.netDensity <- network.density(HAW03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_SF.entropy <- entropy(HAW03_SFft) #entropy

HAW03_SF.netMx <- cbind(HAW03_SF.netMx, HAW03_SF.clusterCoef, HAW03_SF.degreeCent$centralization,
                        HAW03_SF.netDensity, HAW03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************
#NA

round = 3
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
HAW03_TFg2 <- data.frame(HAW03_TF)
HAW03_TFg2 <- HAW03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_TFg2$player1
player2vector <- HAW03_TFg2$player2
HAW03_TFg3 <- HAW03_TFg2
HAW03_TFg3$p1inp2vec <- is.element(HAW03_TFg3$player1, player2vector)
HAW03_TFg3$p2inp1vec <- is.element(HAW03_TFg3$player2, player1vector)

addPlayer1 <- HAW03_TFg3[ which(HAW03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_TFg3[ which(HAW03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_TFg2 <- rbind(HAW03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
HAW03_TFft <- ftable(HAW03_TFg2$player1, HAW03_TFg2$player2)
HAW03_TFft2 <- as.matrix(HAW03_TFft)
numRows <- nrow(HAW03_TFft2)
numCols <- ncol(HAW03_TFft2)
HAW03_TFft3 <- HAW03_TFft2[c(2:numRows) , c(2:numCols)]
HAW03_TFTable <- graph.adjacency(HAW03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(HAW03_TFTable, vertex.label = V(HAW03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
HAW03_TF.clusterCoef <- transitivity(HAW03_TFTable, type="global") #cluster coefficient
HAW03_TF.degreeCent <- centralization.degree(HAW03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_TFftn <- as.network.matrix(HAW03_TFft)
HAW03_TF.netDensity <- network.density(HAW03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_TF.entropy <- entropy(HAW03_TFft) #entropy

HAW03_TF.netMx <- cbind(HAW03_TF.netMx, HAW03_TF.clusterCoef, HAW03_TF.degreeCent$centralization,
                        HAW03_TF.netDensity, HAW03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
HAW03_SAMg2 <- data.frame(HAW03_SAM)
HAW03_SAMg2 <- HAW03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_SAMg2$player1
player2vector <- HAW03_SAMg2$player2
HAW03_SAMg3 <- HAW03_SAMg2
HAW03_SAMg3$p1inp2vec <- is.element(HAW03_SAMg3$player1, player2vector)
HAW03_SAMg3$p2inp1vec <- is.element(HAW03_SAMg3$player2, player1vector)

addPlayer1 <- HAW03_SAMg3[ which(HAW03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_SAMg3[ which(HAW03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_SAMg2 <- rbind(HAW03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
HAW03_SAMft <- ftable(HAW03_SAMg2$player1, HAW03_SAMg2$player2)
HAW03_SAMft2 <- as.matrix(HAW03_SAMft)
numRows <- nrow(HAW03_SAMft2)
numCols <- ncol(HAW03_SAMft2)
HAW03_SAMft3 <- HAW03_SAMft2[c(2:numRows) , c(2:numCols)]
HAW03_SAMTable <- graph.adjacency(HAW03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(HAW03_SAMTable, vertex.label = V(HAW03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
HAW03_SAM.clusterCoef <- transitivity(HAW03_SAMTable, type="global") #cluster coefficient
HAW03_SAM.degreeCent <- centralization.degree(HAW03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_SAMftn <- as.network.matrix(HAW03_SAMft)
HAW03_SAM.netDensity <- network.density(HAW03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_SAM.entropy <- entropy(HAW03_SAMft) #entropy

HAW03_SAM.netMx <- cbind(HAW03_SAM.netMx, HAW03_SAM.clusterCoef, HAW03_SAM.degreeCent$centralization,
                         HAW03_SAM.netDensity, HAW03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
HAW03_TAMg2 <- data.frame(HAW03_TAM)
HAW03_TAMg2 <- HAW03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_TAMg2$player1
player2vector <- HAW03_TAMg2$player2
HAW03_TAMg3 <- HAW03_TAMg2
HAW03_TAMg3$p1inp2vec <- is.element(HAW03_TAMg3$player1, player2vector)
HAW03_TAMg3$p2inp1vec <- is.element(HAW03_TAMg3$player2, player1vector)

addPlayer1 <- HAW03_TAMg3[ which(HAW03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_TAMg3[ which(HAW03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_TAMg2 <- rbind(HAW03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
HAW03_TAMft <- ftable(HAW03_TAMg2$player1, HAW03_TAMg2$player2)
HAW03_TAMft2 <- as.matrix(HAW03_TAMft)
numRows <- nrow(HAW03_TAMft2)
numCols <- ncol(HAW03_TAMft2)
HAW03_TAMft3 <- HAW03_TAMft2[c(2:numRows) , c(2:numCols)]
HAW03_TAMTable <- graph.adjacency(HAW03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(HAW03_TAMTable, vertex.label = V(HAW03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
HAW03_TAM.clusterCoef <- transitivity(HAW03_TAMTable, type="global") #cluster coefficient
HAW03_TAM.degreeCent <- centralization.degree(HAW03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_TAMftn <- as.network.matrix(HAW03_TAMft)
HAW03_TAM.netDensity <- network.density(HAW03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_TAM.entropy <- entropy(HAW03_TAMft) #entropy

HAW03_TAM.netMx <- cbind(HAW03_TAM.netMx, HAW03_TAM.clusterCoef, HAW03_TAM.degreeCent$centralization,
                         HAW03_TAM.netDensity, HAW03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
HAW03_SDMg2 <- data.frame(HAW03_SDM)
HAW03_SDMg2 <- HAW03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_SDMg2$player1
player2vector <- HAW03_SDMg2$player2
HAW03_SDMg3 <- HAW03_SDMg2
HAW03_SDMg3$p1inp2vec <- is.element(HAW03_SDMg3$player1, player2vector)
HAW03_SDMg3$p2inp1vec <- is.element(HAW03_SDMg3$player2, player1vector)

addPlayer1 <- HAW03_SDMg3[ which(HAW03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_SDMg3[ which(HAW03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_SDMg2 <- rbind(HAW03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
HAW03_SDMft <- ftable(HAW03_SDMg2$player1, HAW03_SDMg2$player2)
HAW03_SDMft2 <- as.matrix(HAW03_SDMft)
numRows <- nrow(HAW03_SDMft2)
numCols <- ncol(HAW03_SDMft2)
HAW03_SDMft3 <- HAW03_SDMft2[c(2:numRows) , c(2:numCols)]
HAW03_SDMTable <- graph.adjacency(HAW03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(HAW03_SDMTable, vertex.label = V(HAW03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
HAW03_SDM.clusterCoef <- transitivity(HAW03_SDMTable, type="global") #cluster coefficient
HAW03_SDM.degreeCent <- centralization.degree(HAW03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_SDMftn <- as.network.matrix(HAW03_SDMft)
HAW03_SDM.netDensity <- network.density(HAW03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_SDM.entropy <- entropy(HAW03_SDMft) #entropy

HAW03_SDM.netMx <- cbind(HAW03_SDM.netMx, HAW03_SDM.clusterCoef, HAW03_SDM.degreeCent$centralization,
                         HAW03_SDM.netDensity, HAW03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
HAW03_TDMg2 <- data.frame(HAW03_TDM)
HAW03_TDMg2 <- HAW03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_TDMg2$player1
player2vector <- HAW03_TDMg2$player2
HAW03_TDMg3 <- HAW03_TDMg2
HAW03_TDMg3$p1inp2vec <- is.element(HAW03_TDMg3$player1, player2vector)
HAW03_TDMg3$p2inp1vec <- is.element(HAW03_TDMg3$player2, player1vector)

addPlayer1 <- HAW03_TDMg3[ which(HAW03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_TDMg3[ which(HAW03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_TDMg2 <- rbind(HAW03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
HAW03_TDMft <- ftable(HAW03_TDMg2$player1, HAW03_TDMg2$player2)
HAW03_TDMft2 <- as.matrix(HAW03_TDMft)
numRows <- nrow(HAW03_TDMft2)
numCols <- ncol(HAW03_TDMft2)
HAW03_TDMft3 <- HAW03_TDMft2[c(2:numRows) , c(2:numCols)]
HAW03_TDMTable <- graph.adjacency(HAW03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(HAW03_TDMTable, vertex.label = V(HAW03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
HAW03_TDM.clusterCoef <- transitivity(HAW03_TDMTable, type="global") #cluster coefficient
HAW03_TDM.degreeCent <- centralization.degree(HAW03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_TDMftn <- as.network.matrix(HAW03_TDMft)
HAW03_TDM.netDensity <- network.density(HAW03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_TDM.entropy <- entropy(HAW03_TDMft) #entropy

HAW03_TDM.netMx <- cbind(HAW03_TDM.netMx, HAW03_TDM.clusterCoef, HAW03_TDM.degreeCent$centralization,
                         HAW03_TDM.netDensity, HAW03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
HAW03_SDg2 <- data.frame(HAW03_SD)
HAW03_SDg2 <- HAW03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_SDg2$player1
player2vector <- HAW03_SDg2$player2
HAW03_SDg3 <- HAW03_SDg2
HAW03_SDg3$p1inp2vec <- is.element(HAW03_SDg3$player1, player2vector)
HAW03_SDg3$p2inp1vec <- is.element(HAW03_SDg3$player2, player1vector)

addPlayer1 <- HAW03_SDg3[ which(HAW03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_SDg3[ which(HAW03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_SDg2 <- rbind(HAW03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
HAW03_SDft <- ftable(HAW03_SDg2$player1, HAW03_SDg2$player2)
HAW03_SDft2 <- as.matrix(HAW03_SDft)
numRows <- nrow(HAW03_SDft2)
numCols <- ncol(HAW03_SDft2)
HAW03_SDft3 <- HAW03_SDft2[c(2:numRows) , c(2:numCols)]
HAW03_SDTable <- graph.adjacency(HAW03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(HAW03_SDTable, vertex.label = V(HAW03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
HAW03_SD.clusterCoef <- transitivity(HAW03_SDTable, type="global") #cluster coefficient
HAW03_SD.degreeCent <- centralization.degree(HAW03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_SDftn <- as.network.matrix(HAW03_SDft)
HAW03_SD.netDensity <- network.density(HAW03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_SD.entropy <- entropy(HAW03_SDft) #entropy

HAW03_SD.netMx <- cbind(HAW03_SD.netMx, HAW03_SD.clusterCoef, HAW03_SD.degreeCent$centralization,
                        HAW03_SD.netDensity, HAW03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
HAW03_TDg2 <- data.frame(HAW03_TD)
HAW03_TDg2 <- HAW03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_TDg2$player1
player2vector <- HAW03_TDg2$player2
HAW03_TDg3 <- HAW03_TDg2
HAW03_TDg3$p1inp2vec <- is.element(HAW03_TDg3$player1, player2vector)
HAW03_TDg3$p2inp1vec <- is.element(HAW03_TDg3$player2, player1vector)

addPlayer1 <- HAW03_TDg3[ which(HAW03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_TDg3[ which(HAW03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_TDg2 <- rbind(HAW03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
HAW03_TDft <- ftable(HAW03_TDg2$player1, HAW03_TDg2$player2)
HAW03_TDft2 <- as.matrix(HAW03_TDft)
numRows <- nrow(HAW03_TDft2)
numCols <- ncol(HAW03_TDft2)
HAW03_TDft3 <- HAW03_TDft2[c(2:numRows) , c(2:numCols)]
HAW03_TDTable <- graph.adjacency(HAW03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(HAW03_TDTable, vertex.label = V(HAW03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
HAW03_TD.clusterCoef <- transitivity(HAW03_TDTable, type="global") #cluster coefficient
HAW03_TD.degreeCent <- centralization.degree(HAW03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_TDftn <- as.network.matrix(HAW03_TDft)
HAW03_TD.netDensity <- network.density(HAW03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_TD.entropy <- entropy(HAW03_TDft) #entropy

HAW03_TD.netMx <- cbind(HAW03_TD.netMx, HAW03_TD.clusterCoef, HAW03_TD.degreeCent$centralization,
                        HAW03_TD.netDensity, HAW03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
HAW03_QTg2 <- data.frame(HAW03_QT)
HAW03_QTg2 <- HAW03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW03_QTg2$player1
player2vector <- HAW03_QTg2$player2
HAW03_QTg3 <- HAW03_QTg2
HAW03_QTg3$p1inp2vec <- is.element(HAW03_QTg3$player1, player2vector)
HAW03_QTg3$p2inp1vec <- is.element(HAW03_QTg3$player2, player1vector)

addPlayer1 <- HAW03_QTg3[ which(HAW03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW03_QTg3[ which(HAW03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW03_QTg2 <- rbind(HAW03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
HAW03_QTft <- ftable(HAW03_QTg2$player1, HAW03_QTg2$player2)
HAW03_QTft2 <- as.matrix(HAW03_QTft)
numRows <- nrow(HAW03_QTft2)
numCols <- ncol(HAW03_QTft2)
HAW03_QTft3 <- HAW03_QTft2[c(2:numRows) , c(2:numCols)]
HAW03_QTTable <- graph.adjacency(HAW03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(HAW03_QTTable, vertex.label = V(HAW03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
HAW03_QT.clusterCoef <- transitivity(HAW03_QTTable, type="global") #cluster coefficient
HAW03_QT.degreeCent <- centralization.degree(HAW03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW03_QTftn <- as.network.matrix(HAW03_QTft)
HAW03_QT.netDensity <- network.density(HAW03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW03_QT.entropy <- entropy(HAW03_QTft) #entropy

HAW03_QT.netMx <- cbind(HAW03_QT.netMx, HAW03_QT.clusterCoef, HAW03_QT.degreeCent$centralization,
                        HAW03_QT.netDensity, HAW03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW03_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 3
##

#Round 3, Goal***************************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Goal_F"
MELB03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
MELB03_Gg2 <- data.frame(MELB03_G)
MELB03_Gg2 <- MELB03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_Gg2$player1
player2vector <- MELB03_Gg2$player2
MELB03_Gg3 <- MELB03_Gg2
MELB03_Gg3$p1inp2vec <- is.element(MELB03_Gg3$player1, player2vector)
MELB03_Gg3$p2inp1vec <- is.element(MELB03_Gg3$player2, player1vector)

addPlayer1 <- MELB03_Gg3[ which(MELB03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB03_Gg2 <- rbind(MELB03_Gg2, addPlayer1)

#Round 3, Goal graph using weighted edges
MELB03_Gft <- ftable(MELB03_Gg2$player1, MELB03_Gg2$player2)
MELB03_Gft2 <- as.matrix(MELB03_Gft)
numRows <- nrow(MELB03_Gft2)
numCols <- ncol(MELB03_Gft2)
MELB03_Gft3 <- MELB03_Gft2[c(2:numRows) , c(1:numCols)]
MELB03_GTable <- graph.adjacency(MELB03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(MELB03_GTable, vertex.label = V(MELB03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
MELB03_G.clusterCoef <- transitivity(MELB03_GTable, type="global") #cluster coefficient
MELB03_G.degreeCent <- centralization.degree(MELB03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_Gftn <- as.network.matrix(MELB03_Gft)
MELB03_G.netDensity <- network.density(MELB03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_G.entropy <- entropy(MELB03_Gft) #entropy

MELB03_G.netMx <- cbind(MELB03_G.netMx, MELB03_G.clusterCoef, MELB03_G.degreeCent$centralization,
                        MELB03_G.netDensity, MELB03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_G.netMx) <- varnames

#Round 3, Behind***************************************************************

round = 3
teamName = "MELB"
KIoutcome = "Behind_F"
MELB03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
MELB03_Bg2 <- data.frame(MELB03_B)
MELB03_Bg2 <- MELB03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_Bg2$player1
player2vector <- MELB03_Bg2$player2
MELB03_Bg3 <- MELB03_Bg2
MELB03_Bg3$p1inp2vec <- is.element(MELB03_Bg3$player1, player2vector)
MELB03_Bg3$p2inp1vec <- is.element(MELB03_Bg3$player2, player1vector)

addPlayer1 <- MELB03_Bg3[ which(MELB03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_Bg3[ which(MELB03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_Bg2 <- rbind(MELB03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
MELB03_Bft <- ftable(MELB03_Bg2$player1, MELB03_Bg2$player2)
MELB03_Bft2 <- as.matrix(MELB03_Bft)
numRows <- nrow(MELB03_Bft2)
numCols <- ncol(MELB03_Bft2)
MELB03_Bft3 <- MELB03_Bft2[c(2:numRows) , c(2:numCols)]
MELB03_BTable <- graph.adjacency(MELB03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(MELB03_BTable, vertex.label = V(MELB03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
MELB03_B.clusterCoef <- transitivity(MELB03_BTable, type="global") #cluster coefficient
MELB03_B.degreeCent <- centralization.degree(MELB03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_Bftn <- as.network.matrix(MELB03_Bft)
MELB03_B.netDensity <- network.density(MELB03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_B.entropy <- entropy(MELB03_Bft) #entropy

MELB03_B.netMx <- cbind(MELB03_B.netMx, MELB03_B.clusterCoef, MELB03_B.degreeCent$centralization,
                        MELB03_B.netDensity, MELB03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
MELB03_SFg2 <- data.frame(MELB03_SF)
MELB03_SFg2 <- MELB03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_SFg2$player1
player2vector <- MELB03_SFg2$player2
MELB03_SFg3 <- MELB03_SFg2
MELB03_SFg3$p1inp2vec <- is.element(MELB03_SFg3$player1, player2vector)
MELB03_SFg3$p2inp1vec <- is.element(MELB03_SFg3$player2, player1vector)

addPlayer1 <- MELB03_SFg3[ which(MELB03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_SFg3[ which(MELB03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_SFg2 <- rbind(MELB03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
MELB03_SFft <- ftable(MELB03_SFg2$player1, MELB03_SFg2$player2)
MELB03_SFft2 <- as.matrix(MELB03_SFft)
numRows <- nrow(MELB03_SFft2)
numCols <- ncol(MELB03_SFft2)
MELB03_SFft3 <- MELB03_SFft2[c(2:numRows) , c(2:numCols)]
MELB03_SFTable <- graph.adjacency(MELB03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(MELB03_SFTable, vertex.label = V(MELB03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
MELB03_SF.clusterCoef <- transitivity(MELB03_SFTable, type="global") #cluster coefficient
MELB03_SF.degreeCent <- centralization.degree(MELB03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_SFftn <- as.network.matrix(MELB03_SFft)
MELB03_SF.netDensity <- network.density(MELB03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_SF.entropy <- entropy(MELB03_SFft) #entropy

MELB03_SF.netMx <- cbind(MELB03_SF.netMx, MELB03_SF.clusterCoef, MELB03_SF.degreeCent$centralization,
                         MELB03_SF.netDensity, MELB03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
MELB03_TFg2 <- data.frame(MELB03_TF)
MELB03_TFg2 <- MELB03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_TFg2$player1
player2vector <- MELB03_TFg2$player2
MELB03_TFg3 <- MELB03_TFg2
MELB03_TFg3$p1inp2vec <- is.element(MELB03_TFg3$player1, player2vector)
MELB03_TFg3$p2inp1vec <- is.element(MELB03_TFg3$player2, player1vector)

addPlayer1 <- MELB03_TFg3[ which(MELB03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_TFg3[ which(MELB03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_TFg2 <- rbind(MELB03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
MELB03_TFft <- ftable(MELB03_TFg2$player1, MELB03_TFg2$player2)
MELB03_TFft2 <- as.matrix(MELB03_TFft)
numRows <- nrow(MELB03_TFft2)
numCols <- ncol(MELB03_TFft2)
MELB03_TFft3 <- MELB03_TFft2[c(2:numRows) , c(2:numCols)]
MELB03_TFTable <- graph.adjacency(MELB03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(MELB03_TFTable, vertex.label = V(MELB03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
MELB03_TF.clusterCoef <- transitivity(MELB03_TFTable, type="global") #cluster coefficient
MELB03_TF.degreeCent <- centralization.degree(MELB03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_TFftn <- as.network.matrix(MELB03_TFft)
MELB03_TF.netDensity <- network.density(MELB03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_TF.entropy <- entropy(MELB03_TFft) #entropy

MELB03_TF.netMx <- cbind(MELB03_TF.netMx, MELB03_TF.clusterCoef, MELB03_TF.degreeCent$centralization,
                         MELB03_TF.netDensity, MELB03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
MELB03_SAMg2 <- data.frame(MELB03_SAM)
MELB03_SAMg2 <- MELB03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_SAMg2$player1
player2vector <- MELB03_SAMg2$player2
MELB03_SAMg3 <- MELB03_SAMg2
MELB03_SAMg3$p1inp2vec <- is.element(MELB03_SAMg3$player1, player2vector)
MELB03_SAMg3$p2inp1vec <- is.element(MELB03_SAMg3$player2, player1vector)

addPlayer1 <- MELB03_SAMg3[ which(MELB03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_SAMg3[ which(MELB03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_SAMg2 <- rbind(MELB03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
MELB03_SAMft <- ftable(MELB03_SAMg2$player1, MELB03_SAMg2$player2)
MELB03_SAMft2 <- as.matrix(MELB03_SAMft)
numRows <- nrow(MELB03_SAMft2)
numCols <- ncol(MELB03_SAMft2)
MELB03_SAMft3 <- MELB03_SAMft2[c(2:numRows) , c(2:numCols)]
MELB03_SAMTable <- graph.adjacency(MELB03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(MELB03_SAMTable, vertex.label = V(MELB03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
MELB03_SAM.clusterCoef <- transitivity(MELB03_SAMTable, type="global") #cluster coefficient
MELB03_SAM.degreeCent <- centralization.degree(MELB03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_SAMftn <- as.network.matrix(MELB03_SAMft)
MELB03_SAM.netDensity <- network.density(MELB03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_SAM.entropy <- entropy(MELB03_SAMft) #entropy

MELB03_SAM.netMx <- cbind(MELB03_SAM.netMx, MELB03_SAM.clusterCoef, MELB03_SAM.degreeCent$centralization,
                          MELB03_SAM.netDensity, MELB03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
MELB03_TAMg2 <- data.frame(MELB03_TAM)
MELB03_TAMg2 <- MELB03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_TAMg2$player1
player2vector <- MELB03_TAMg2$player2
MELB03_TAMg3 <- MELB03_TAMg2
MELB03_TAMg3$p1inp2vec <- is.element(MELB03_TAMg3$player1, player2vector)
MELB03_TAMg3$p2inp1vec <- is.element(MELB03_TAMg3$player2, player1vector)

addPlayer1 <- MELB03_TAMg3[ which(MELB03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_TAMg3[ which(MELB03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_TAMg2 <- rbind(MELB03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
MELB03_TAMft <- ftable(MELB03_TAMg2$player1, MELB03_TAMg2$player2)
MELB03_TAMft2 <- as.matrix(MELB03_TAMft)
numRows <- nrow(MELB03_TAMft2)
numCols <- ncol(MELB03_TAMft2)
MELB03_TAMft3 <- MELB03_TAMft2[c(2:numRows) , c(2:numCols)]
MELB03_TAMTable <- graph.adjacency(MELB03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(MELB03_TAMTable, vertex.label = V(MELB03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
MELB03_TAM.clusterCoef <- transitivity(MELB03_TAMTable, type="global") #cluster coefficient
MELB03_TAM.degreeCent <- centralization.degree(MELB03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_TAMftn <- as.network.matrix(MELB03_TAMft)
MELB03_TAM.netDensity <- network.density(MELB03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_TAM.entropy <- entropy(MELB03_TAMft) #entropy

MELB03_TAM.netMx <- cbind(MELB03_TAM.netMx, MELB03_TAM.clusterCoef, MELB03_TAM.degreeCent$centralization,
                          MELB03_TAM.netDensity, MELB03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
MELB03_SDMg2 <- data.frame(MELB03_SDM)
MELB03_SDMg2 <- MELB03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_SDMg2$player1
player2vector <- MELB03_SDMg2$player2
MELB03_SDMg3 <- MELB03_SDMg2
MELB03_SDMg3$p1inp2vec <- is.element(MELB03_SDMg3$player1, player2vector)
MELB03_SDMg3$p2inp1vec <- is.element(MELB03_SDMg3$player2, player1vector)

addPlayer1 <- MELB03_SDMg3[ which(MELB03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_SDMg3[ which(MELB03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_SDMg2 <- rbind(MELB03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
MELB03_SDMft <- ftable(MELB03_SDMg2$player1, MELB03_SDMg2$player2)
MELB03_SDMft2 <- as.matrix(MELB03_SDMft)
numRows <- nrow(MELB03_SDMft2)
numCols <- ncol(MELB03_SDMft2)
MELB03_SDMft3 <- MELB03_SDMft2[c(2:numRows) , c(2:numCols)]
MELB03_SDMTable <- graph.adjacency(MELB03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(MELB03_SDMTable, vertex.label = V(MELB03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
MELB03_SDM.clusterCoef <- transitivity(MELB03_SDMTable, type="global") #cluster coefficient
MELB03_SDM.degreeCent <- centralization.degree(MELB03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_SDMftn <- as.network.matrix(MELB03_SDMft)
MELB03_SDM.netDensity <- network.density(MELB03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_SDM.entropy <- entropy(MELB03_SDMft) #entropy

MELB03_SDM.netMx <- cbind(MELB03_SDM.netMx, MELB03_SDM.clusterCoef, MELB03_SDM.degreeCent$centralization,
                          MELB03_SDM.netDensity, MELB03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
MELB03_TDMg2 <- data.frame(MELB03_TDM)
MELB03_TDMg2 <- MELB03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_TDMg2$player1
player2vector <- MELB03_TDMg2$player2
MELB03_TDMg3 <- MELB03_TDMg2
MELB03_TDMg3$p1inp2vec <- is.element(MELB03_TDMg3$player1, player2vector)
MELB03_TDMg3$p2inp1vec <- is.element(MELB03_TDMg3$player2, player1vector)

addPlayer1 <- MELB03_TDMg3[ which(MELB03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_TDMg3[ which(MELB03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_TDMg2 <- rbind(MELB03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
MELB03_TDMft <- ftable(MELB03_TDMg2$player1, MELB03_TDMg2$player2)
MELB03_TDMft2 <- as.matrix(MELB03_TDMft)
numRows <- nrow(MELB03_TDMft2)
numCols <- ncol(MELB03_TDMft2)
MELB03_TDMft3 <- MELB03_TDMft2[c(2:numRows) , c(2:numCols)]
MELB03_TDMTable <- graph.adjacency(MELB03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(MELB03_TDMTable, vertex.label = V(MELB03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
MELB03_TDM.clusterCoef <- transitivity(MELB03_TDMTable, type="global") #cluster coefficient
MELB03_TDM.degreeCent <- centralization.degree(MELB03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_TDMftn <- as.network.matrix(MELB03_TDMft)
MELB03_TDM.netDensity <- network.density(MELB03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_TDM.entropy <- entropy(MELB03_TDMft) #entropy

MELB03_TDM.netMx <- cbind(MELB03_TDM.netMx, MELB03_TDM.clusterCoef, MELB03_TDM.degreeCent$centralization,
                          MELB03_TDM.netDensity, MELB03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
MELB03_SDg2 <- data.frame(MELB03_SD)
MELB03_SDg2 <- MELB03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_SDg2$player1
player2vector <- MELB03_SDg2$player2
MELB03_SDg3 <- MELB03_SDg2
MELB03_SDg3$p1inp2vec <- is.element(MELB03_SDg3$player1, player2vector)
MELB03_SDg3$p2inp1vec <- is.element(MELB03_SDg3$player2, player1vector)

addPlayer1 <- MELB03_SDg3[ which(MELB03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_SDg3[ which(MELB03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_SDg2 <- rbind(MELB03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
MELB03_SDft <- ftable(MELB03_SDg2$player1, MELB03_SDg2$player2)
MELB03_SDft2 <- as.matrix(MELB03_SDft)
numRows <- nrow(MELB03_SDft2)
numCols <- ncol(MELB03_SDft2)
MELB03_SDft3 <- MELB03_SDft2[c(2:numRows) , c(2:numCols)]
MELB03_SDTable <- graph.adjacency(MELB03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(MELB03_SDTable, vertex.label = V(MELB03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
MELB03_SD.clusterCoef <- transitivity(MELB03_SDTable, type="global") #cluster coefficient
MELB03_SD.degreeCent <- centralization.degree(MELB03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_SDftn <- as.network.matrix(MELB03_SDft)
MELB03_SD.netDensity <- network.density(MELB03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_SD.entropy <- entropy(MELB03_SDft) #entropy

MELB03_SD.netMx <- cbind(MELB03_SD.netMx, MELB03_SD.clusterCoef, MELB03_SD.degreeCent$centralization,
                         MELB03_SD.netDensity, MELB03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
MELB03_TDg2 <- data.frame(MELB03_TD)
MELB03_TDg2 <- MELB03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_TDg2$player1
player2vector <- MELB03_TDg2$player2
MELB03_TDg3 <- MELB03_TDg2
MELB03_TDg3$p1inp2vec <- is.element(MELB03_TDg3$player1, player2vector)
MELB03_TDg3$p2inp1vec <- is.element(MELB03_TDg3$player2, player1vector)

addPlayer1 <- MELB03_TDg3[ which(MELB03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_TDg3[ which(MELB03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_TDg2 <- rbind(MELB03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
MELB03_TDft <- ftable(MELB03_TDg2$player1, MELB03_TDg2$player2)
MELB03_TDft2 <- as.matrix(MELB03_TDft)
numRows <- nrow(MELB03_TDft2)
numCols <- ncol(MELB03_TDft2)
MELB03_TDft3 <- MELB03_TDft2[c(2:numRows) , c(2:numCols)]
MELB03_TDTable <- graph.adjacency(MELB03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(MELB03_TDTable, vertex.label = V(MELB03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
MELB03_TD.clusterCoef <- transitivity(MELB03_TDTable, type="global") #cluster coefficient
MELB03_TD.degreeCent <- centralization.degree(MELB03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_TDftn <- as.network.matrix(MELB03_TDft)
MELB03_TD.netDensity <- network.density(MELB03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_TD.entropy <- entropy(MELB03_TDft) #entropy

MELB03_TD.netMx <- cbind(MELB03_TD.netMx, MELB03_TD.clusterCoef, MELB03_TD.degreeCent$centralization,
                         MELB03_TD.netDensity, MELB03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
MELB03_QTg2 <- data.frame(MELB03_QT)
MELB03_QTg2 <- MELB03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB03_QTg2$player1
player2vector <- MELB03_QTg2$player2
MELB03_QTg3 <- MELB03_QTg2
MELB03_QTg3$p1inp2vec <- is.element(MELB03_QTg3$player1, player2vector)
MELB03_QTg3$p2inp1vec <- is.element(MELB03_QTg3$player2, player1vector)

addPlayer1 <- MELB03_QTg3[ which(MELB03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB03_QTg3[ which(MELB03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB03_QTg2 <- rbind(MELB03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
MELB03_QTft <- ftable(MELB03_QTg2$player1, MELB03_QTg2$player2)
MELB03_QTft2 <- as.matrix(MELB03_QTft)
numRows <- nrow(MELB03_QTft2)
numCols <- ncol(MELB03_QTft2)
MELB03_QTft3 <- MELB03_QTft2[c(2:numRows) , c(2:numCols)]
MELB03_QTTable <- graph.adjacency(MELB03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(MELB03_QTTable, vertex.label = V(MELB03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
MELB03_QT.clusterCoef <- transitivity(MELB03_QTTable, type="global") #cluster coefficient
MELB03_QT.degreeCent <- centralization.degree(MELB03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB03_QTftn <- as.network.matrix(MELB03_QTft)
MELB03_QT.netDensity <- network.density(MELB03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB03_QT.entropy <- entropy(MELB03_QTft) #entropy

MELB03_QT.netMx <- cbind(MELB03_QT.netMx, MELB03_QT.clusterCoef, MELB03_QT.degreeCent$centralization,
                         MELB03_QT.netDensity, MELB03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB03_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
NMFC03_Gg2 <- data.frame(NMFC03_G)
NMFC03_Gg2 <- NMFC03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_Gg2$player1
player2vector <- NMFC03_Gg2$player2
NMFC03_Gg3 <- NMFC03_Gg2
NMFC03_Gg3$p1inp2vec <- is.element(NMFC03_Gg3$player1, player2vector)
NMFC03_Gg3$p2inp1vec <- is.element(NMFC03_Gg3$player2, player1vector)

addPlayer1 <- NMFC03_Gg3[ which(NMFC03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_Gg3[ which(NMFC03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_Gg2 <- rbind(NMFC03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
NMFC03_Gft <- ftable(NMFC03_Gg2$player1, NMFC03_Gg2$player2)
NMFC03_Gft2 <- as.matrix(NMFC03_Gft)
numRows <- nrow(NMFC03_Gft2)
numCols <- ncol(NMFC03_Gft2)
NMFC03_Gft3 <- NMFC03_Gft2[c(2:numRows) , c(2:numCols)]
NMFC03_GTable <- graph.adjacency(NMFC03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(NMFC03_GTable, vertex.label = V(NMFC03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
NMFC03_G.clusterCoef <- transitivity(NMFC03_GTable, type="global") #cluster coefficient
NMFC03_G.degreeCent <- centralization.degree(NMFC03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_Gftn <- as.network.matrix(NMFC03_Gft)
NMFC03_G.netDensity <- network.density(NMFC03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_G.entropy <- entropy(NMFC03_Gft) #entropy

NMFC03_G.netMx <- cbind(NMFC03_G.netMx, NMFC03_G.clusterCoef, NMFC03_G.degreeCent$centralization,
                        NMFC03_G.netDensity, NMFC03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_G.netMx) <- varnames

#Round 3, Behind***************************************************************

round = 3
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
NMFC03_Bg2 <- data.frame(NMFC03_B)
NMFC03_Bg2 <- NMFC03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_Bg2$player1
player2vector <- NMFC03_Bg2$player2
NMFC03_Bg3 <- NMFC03_Bg2
NMFC03_Bg3$p1inp2vec <- is.element(NMFC03_Bg3$player1, player2vector)
NMFC03_Bg3$p2inp1vec <- is.element(NMFC03_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- NMFC03_Bg3[ which(NMFC03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

NMFC03_Bg2 <- rbind(NMFC03_Bg2, addPlayer2)

#Round 3, Behind graph using weighted edges
NMFC03_Bft <- ftable(NMFC03_Bg2$player1, NMFC03_Bg2$player2)
NMFC03_Bft2 <- as.matrix(NMFC03_Bft)
numRows <- nrow(NMFC03_Bft2)
numCols <- ncol(NMFC03_Bft2)
NMFC03_Bft3 <- NMFC03_Bft2[c(1:numRows) , c(2:numCols)]
NMFC03_BTable <- graph.adjacency(NMFC03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(NMFC03_BTable, vertex.label = V(NMFC03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
NMFC03_B.clusterCoef <- transitivity(NMFC03_BTable, type="global") #cluster coefficient
NMFC03_B.degreeCent <- centralization.degree(NMFC03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_Bftn <- as.network.matrix(NMFC03_Bft)
NMFC03_B.netDensity <- network.density(NMFC03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_B.entropy <- entropy(NMFC03_Bft) #entropy

NMFC03_B.netMx <- cbind(NMFC03_B.netMx, NMFC03_B.clusterCoef, NMFC03_B.degreeCent$centralization,
                        NMFC03_B.netDensity, NMFC03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
NMFC03_SFg2 <- data.frame(NMFC03_SF)
NMFC03_SFg2 <- NMFC03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_SFg2$player1
player2vector <- NMFC03_SFg2$player2
NMFC03_SFg3 <- NMFC03_SFg2
NMFC03_SFg3$p1inp2vec <- is.element(NMFC03_SFg3$player1, player2vector)
NMFC03_SFg3$p2inp1vec <- is.element(NMFC03_SFg3$player2, player1vector)

addPlayer1 <- NMFC03_SFg3[ which(NMFC03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_SFg3[ which(NMFC03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_SFg2 <- rbind(NMFC03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
NMFC03_SFft <- ftable(NMFC03_SFg2$player1, NMFC03_SFg2$player2)
NMFC03_SFft2 <- as.matrix(NMFC03_SFft)
numRows <- nrow(NMFC03_SFft2)
numCols <- ncol(NMFC03_SFft2)
NMFC03_SFft3 <- NMFC03_SFft2[c(2:numRows) , c(2:numCols)]
NMFC03_SFTable <- graph.adjacency(NMFC03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(NMFC03_SFTable, vertex.label = V(NMFC03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
NMFC03_SF.clusterCoef <- transitivity(NMFC03_SFTable, type="global") #cluster coefficient
NMFC03_SF.degreeCent <- centralization.degree(NMFC03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_SFftn <- as.network.matrix(NMFC03_SFft)
NMFC03_SF.netDensity <- network.density(NMFC03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_SF.entropy <- entropy(NMFC03_SFft) #entropy

NMFC03_SF.netMx <- cbind(NMFC03_SF.netMx, NMFC03_SF.clusterCoef, NMFC03_SF.degreeCent$centralization,
                         NMFC03_SF.netDensity, NMFC03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************
#NA

round = 3
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
NMFC03_TFg2 <- data.frame(NMFC03_TF)
NMFC03_TFg2 <- NMFC03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_TFg2$player1
player2vector <- NMFC03_TFg2$player2
NMFC03_TFg3 <- NMFC03_TFg2
NMFC03_TFg3$p1inp2vec <- is.element(NMFC03_TFg3$player1, player2vector)
NMFC03_TFg3$p2inp1vec <- is.element(NMFC03_TFg3$player2, player1vector)

addPlayer1 <- NMFC03_TFg3[ which(NMFC03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_TFg3[ which(NMFC03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_TFg2 <- rbind(NMFC03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
NMFC03_TFft <- ftable(NMFC03_TFg2$player1, NMFC03_TFg2$player2)
NMFC03_TFft2 <- as.matrix(NMFC03_TFft)
numRows <- nrow(NMFC03_TFft2)
numCols <- ncol(NMFC03_TFft2)
NMFC03_TFft3 <- NMFC03_TFft2[c(2:numRows) , c(2:numCols)]
NMFC03_TFTable <- graph.adjacency(NMFC03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(NMFC03_TFTable, vertex.label = V(NMFC03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
NMFC03_TF.clusterCoef <- transitivity(NMFC03_TFTable, type="global") #cluster coefficient
NMFC03_TF.degreeCent <- centralization.degree(NMFC03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_TFftn <- as.network.matrix(NMFC03_TFft)
NMFC03_TF.netDensity <- network.density(NMFC03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_TF.entropy <- entropy(NMFC03_TFft) #entropy

NMFC03_TF.netMx <- cbind(NMFC03_TF.netMx, NMFC03_TF.clusterCoef, NMFC03_TF.degreeCent$centralization,
                         NMFC03_TF.netDensity, NMFC03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
NMFC03_SAMg2 <- data.frame(NMFC03_SAM)
NMFC03_SAMg2 <- NMFC03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_SAMg2$player1
player2vector <- NMFC03_SAMg2$player2
NMFC03_SAMg3 <- NMFC03_SAMg2
NMFC03_SAMg3$p1inp2vec <- is.element(NMFC03_SAMg3$player1, player2vector)
NMFC03_SAMg3$p2inp1vec <- is.element(NMFC03_SAMg3$player2, player1vector)

addPlayer1 <- NMFC03_SAMg3[ which(NMFC03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_SAMg3[ which(NMFC03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_SAMg2 <- rbind(NMFC03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
NMFC03_SAMft <- ftable(NMFC03_SAMg2$player1, NMFC03_SAMg2$player2)
NMFC03_SAMft2 <- as.matrix(NMFC03_SAMft)
numRows <- nrow(NMFC03_SAMft2)
numCols <- ncol(NMFC03_SAMft2)
NMFC03_SAMft3 <- NMFC03_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC03_SAMTable <- graph.adjacency(NMFC03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(NMFC03_SAMTable, vertex.label = V(NMFC03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
NMFC03_SAM.clusterCoef <- transitivity(NMFC03_SAMTable, type="global") #cluster coefficient
NMFC03_SAM.degreeCent <- centralization.degree(NMFC03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_SAMftn <- as.network.matrix(NMFC03_SAMft)
NMFC03_SAM.netDensity <- network.density(NMFC03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_SAM.entropy <- entropy(NMFC03_SAMft) #entropy

NMFC03_SAM.netMx <- cbind(NMFC03_SAM.netMx, NMFC03_SAM.clusterCoef, NMFC03_SAM.degreeCent$centralization,
                          NMFC03_SAM.netDensity, NMFC03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
NMFC03_TAMg2 <- data.frame(NMFC03_TAM)
NMFC03_TAMg2 <- NMFC03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_TAMg2$player1
player2vector <- NMFC03_TAMg2$player2
NMFC03_TAMg3 <- NMFC03_TAMg2
NMFC03_TAMg3$p1inp2vec <- is.element(NMFC03_TAMg3$player1, player2vector)
NMFC03_TAMg3$p2inp1vec <- is.element(NMFC03_TAMg3$player2, player1vector)

addPlayer1 <- NMFC03_TAMg3[ which(NMFC03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_TAMg3[ which(NMFC03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_TAMg2 <- rbind(NMFC03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
NMFC03_TAMft <- ftable(NMFC03_TAMg2$player1, NMFC03_TAMg2$player2)
NMFC03_TAMft2 <- as.matrix(NMFC03_TAMft)
numRows <- nrow(NMFC03_TAMft2)
numCols <- ncol(NMFC03_TAMft2)
NMFC03_TAMft3 <- NMFC03_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC03_TAMTable <- graph.adjacency(NMFC03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(NMFC03_TAMTable, vertex.label = V(NMFC03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
NMFC03_TAM.clusterCoef <- transitivity(NMFC03_TAMTable, type="global") #cluster coefficient
NMFC03_TAM.degreeCent <- centralization.degree(NMFC03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_TAMftn <- as.network.matrix(NMFC03_TAMft)
NMFC03_TAM.netDensity <- network.density(NMFC03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_TAM.entropy <- entropy(NMFC03_TAMft) #entropy

NMFC03_TAM.netMx <- cbind(NMFC03_TAM.netMx, NMFC03_TAM.clusterCoef, NMFC03_TAM.degreeCent$centralization,
                          NMFC03_TAM.netDensity, NMFC03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
NMFC03_SDMg2 <- data.frame(NMFC03_SDM)
NMFC03_SDMg2 <- NMFC03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_SDMg2$player1
player2vector <- NMFC03_SDMg2$player2
NMFC03_SDMg3 <- NMFC03_SDMg2
NMFC03_SDMg3$p1inp2vec <- is.element(NMFC03_SDMg3$player1, player2vector)
NMFC03_SDMg3$p2inp1vec <- is.element(NMFC03_SDMg3$player2, player1vector)

addPlayer1 <- NMFC03_SDMg3[ which(NMFC03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_SDMg3[ which(NMFC03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_SDMg2 <- rbind(NMFC03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
NMFC03_SDMft <- ftable(NMFC03_SDMg2$player1, NMFC03_SDMg2$player2)
NMFC03_SDMft2 <- as.matrix(NMFC03_SDMft)
numRows <- nrow(NMFC03_SDMft2)
numCols <- ncol(NMFC03_SDMft2)
NMFC03_SDMft3 <- NMFC03_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC03_SDMTable <- graph.adjacency(NMFC03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(NMFC03_SDMTable, vertex.label = V(NMFC03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
NMFC03_SDM.clusterCoef <- transitivity(NMFC03_SDMTable, type="global") #cluster coefficient
NMFC03_SDM.degreeCent <- centralization.degree(NMFC03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_SDMftn <- as.network.matrix(NMFC03_SDMft)
NMFC03_SDM.netDensity <- network.density(NMFC03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_SDM.entropy <- entropy(NMFC03_SDMft) #entropy

NMFC03_SDM.netMx <- cbind(NMFC03_SDM.netMx, NMFC03_SDM.clusterCoef, NMFC03_SDM.degreeCent$centralization,
                          NMFC03_SDM.netDensity, NMFC03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
NMFC03_TDMg2 <- data.frame(NMFC03_TDM)
NMFC03_TDMg2 <- NMFC03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_TDMg2$player1
player2vector <- NMFC03_TDMg2$player2
NMFC03_TDMg3 <- NMFC03_TDMg2
NMFC03_TDMg3$p1inp2vec <- is.element(NMFC03_TDMg3$player1, player2vector)
NMFC03_TDMg3$p2inp1vec <- is.element(NMFC03_TDMg3$player2, player1vector)

addPlayer1 <- NMFC03_TDMg3[ which(NMFC03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_TDMg3[ which(NMFC03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_TDMg2 <- rbind(NMFC03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
NMFC03_TDMft <- ftable(NMFC03_TDMg2$player1, NMFC03_TDMg2$player2)
NMFC03_TDMft2 <- as.matrix(NMFC03_TDMft)
numRows <- nrow(NMFC03_TDMft2)
numCols <- ncol(NMFC03_TDMft2)
NMFC03_TDMft3 <- NMFC03_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC03_TDMTable <- graph.adjacency(NMFC03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(NMFC03_TDMTable, vertex.label = V(NMFC03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
NMFC03_TDM.clusterCoef <- transitivity(NMFC03_TDMTable, type="global") #cluster coefficient
NMFC03_TDM.degreeCent <- centralization.degree(NMFC03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_TDMftn <- as.network.matrix(NMFC03_TDMft)
NMFC03_TDM.netDensity <- network.density(NMFC03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_TDM.entropy <- entropy(NMFC03_TDMft) #entropy

NMFC03_TDM.netMx <- cbind(NMFC03_TDM.netMx, NMFC03_TDM.clusterCoef, NMFC03_TDM.degreeCent$centralization,
                          NMFC03_TDM.netDensity, NMFC03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
NMFC03_SDg2 <- data.frame(NMFC03_SD)
NMFC03_SDg2 <- NMFC03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_SDg2$player1
player2vector <- NMFC03_SDg2$player2
NMFC03_SDg3 <- NMFC03_SDg2
NMFC03_SDg3$p1inp2vec <- is.element(NMFC03_SDg3$player1, player2vector)
NMFC03_SDg3$p2inp1vec <- is.element(NMFC03_SDg3$player2, player1vector)

addPlayer1 <- NMFC03_SDg3[ which(NMFC03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_SDg3[ which(NMFC03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_SDg2 <- rbind(NMFC03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
NMFC03_SDft <- ftable(NMFC03_SDg2$player1, NMFC03_SDg2$player2)
NMFC03_SDft2 <- as.matrix(NMFC03_SDft)
numRows <- nrow(NMFC03_SDft2)
numCols <- ncol(NMFC03_SDft2)
NMFC03_SDft3 <- NMFC03_SDft2[c(2:numRows) , c(2:numCols)]
NMFC03_SDTable <- graph.adjacency(NMFC03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(NMFC03_SDTable, vertex.label = V(NMFC03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
NMFC03_SD.clusterCoef <- transitivity(NMFC03_SDTable, type="global") #cluster coefficient
NMFC03_SD.degreeCent <- centralization.degree(NMFC03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_SDftn <- as.network.matrix(NMFC03_SDft)
NMFC03_SD.netDensity <- network.density(NMFC03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_SD.entropy <- entropy(NMFC03_SDft) #entropy

NMFC03_SD.netMx <- cbind(NMFC03_SD.netMx, NMFC03_SD.clusterCoef, NMFC03_SD.degreeCent$centralization,
                         NMFC03_SD.netDensity, NMFC03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
NMFC03_TDg2 <- data.frame(NMFC03_TD)
NMFC03_TDg2 <- NMFC03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_TDg2$player1
player2vector <- NMFC03_TDg2$player2
NMFC03_TDg3 <- NMFC03_TDg2
NMFC03_TDg3$p1inp2vec <- is.element(NMFC03_TDg3$player1, player2vector)
NMFC03_TDg3$p2inp1vec <- is.element(NMFC03_TDg3$player2, player1vector)

addPlayer1 <- NMFC03_TDg3[ which(NMFC03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_TDg3[ which(NMFC03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_TDg2 <- rbind(NMFC03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
NMFC03_TDft <- ftable(NMFC03_TDg2$player1, NMFC03_TDg2$player2)
NMFC03_TDft2 <- as.matrix(NMFC03_TDft)
numRows <- nrow(NMFC03_TDft2)
numCols <- ncol(NMFC03_TDft2)
NMFC03_TDft3 <- NMFC03_TDft2[c(2:numRows) , c(2:numCols)]
NMFC03_TDTable <- graph.adjacency(NMFC03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(NMFC03_TDTable, vertex.label = V(NMFC03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
NMFC03_TD.clusterCoef <- transitivity(NMFC03_TDTable, type="global") #cluster coefficient
NMFC03_TD.degreeCent <- centralization.degree(NMFC03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_TDftn <- as.network.matrix(NMFC03_TDft)
NMFC03_TD.netDensity <- network.density(NMFC03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_TD.entropy <- entropy(NMFC03_TDft) #entropy

NMFC03_TD.netMx <- cbind(NMFC03_TD.netMx, NMFC03_TD.clusterCoef, NMFC03_TD.degreeCent$centralization,
                         NMFC03_TD.netDensity, NMFC03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
NMFC03_QTg2 <- data.frame(NMFC03_QT)
NMFC03_QTg2 <- NMFC03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC03_QTg2$player1
player2vector <- NMFC03_QTg2$player2
NMFC03_QTg3 <- NMFC03_QTg2
NMFC03_QTg3$p1inp2vec <- is.element(NMFC03_QTg3$player1, player2vector)
NMFC03_QTg3$p2inp1vec <- is.element(NMFC03_QTg3$player2, player1vector)

addPlayer1 <- NMFC03_QTg3[ which(NMFC03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC03_QTg3[ which(NMFC03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC03_QTg2 <- rbind(NMFC03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
NMFC03_QTft <- ftable(NMFC03_QTg2$player1, NMFC03_QTg2$player2)
NMFC03_QTft2 <- as.matrix(NMFC03_QTft)
numRows <- nrow(NMFC03_QTft2)
numCols <- ncol(NMFC03_QTft2)
NMFC03_QTft3 <- NMFC03_QTft2[c(2:numRows) , c(2:numCols)]
NMFC03_QTTable <- graph.adjacency(NMFC03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(NMFC03_QTTable, vertex.label = V(NMFC03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
NMFC03_QT.clusterCoef <- transitivity(NMFC03_QTTable, type="global") #cluster coefficient
NMFC03_QT.degreeCent <- centralization.degree(NMFC03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC03_QTftn <- as.network.matrix(NMFC03_QTft)
NMFC03_QT.netDensity <- network.density(NMFC03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC03_QT.entropy <- entropy(NMFC03_QTft) #entropy

NMFC03_QT.netMx <- cbind(NMFC03_QT.netMx, NMFC03_QT.clusterCoef, NMFC03_QT.degreeCent$centralization,
                         NMFC03_QT.netDensity, NMFC03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC03_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "PORT"
KIoutcome = "Goal_F"
PORT03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
PORT03_Gg2 <- data.frame(PORT03_G)
PORT03_Gg2 <- PORT03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_Gg2$player1
player2vector <- PORT03_Gg2$player2
PORT03_Gg3 <- PORT03_Gg2
PORT03_Gg3$p1inp2vec <- is.element(PORT03_Gg3$player1, player2vector)
PORT03_Gg3$p2inp1vec <- is.element(PORT03_Gg3$player2, player1vector)

addPlayer1 <- PORT03_Gg3[ which(PORT03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_Gg3[ which(PORT03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_Gg2 <- rbind(PORT03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
PORT03_Gft <- ftable(PORT03_Gg2$player1, PORT03_Gg2$player2)
PORT03_Gft2 <- as.matrix(PORT03_Gft)
numRows <- nrow(PORT03_Gft2)
numCols <- ncol(PORT03_Gft2)
PORT03_Gft3 <- PORT03_Gft2[c(2:numRows) , c(2:numCols)]
PORT03_GTable <- graph.adjacency(PORT03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(PORT03_GTable, vertex.label = V(PORT03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
PORT03_G.clusterCoef <- transitivity(PORT03_GTable, type="global") #cluster coefficient
PORT03_G.degreeCent <- centralization.degree(PORT03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_Gftn <- as.network.matrix(PORT03_Gft)
PORT03_G.netDensity <- network.density(PORT03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_G.entropy <- entropy(PORT03_Gft) #entropy

PORT03_G.netMx <- cbind(PORT03_G.netMx, PORT03_G.clusterCoef, PORT03_G.degreeCent$centralization,
                        PORT03_G.netDensity, PORT03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_G.netMx) <- varnames

#Round 3, Behind***************************************************************

round = 3
teamName = "PORT"
KIoutcome = "Behind_F"
PORT03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
PORT03_Bg2 <- data.frame(PORT03_B)
PORT03_Bg2 <- PORT03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_Bg2$player1
player2vector <- PORT03_Bg2$player2
PORT03_Bg3 <- PORT03_Bg2
PORT03_Bg3$p1inp2vec <- is.element(PORT03_Bg3$player1, player2vector)
PORT03_Bg3$p2inp1vec <- is.element(PORT03_Bg3$player2, player1vector)

addPlayer1 <- PORT03_Bg3[ which(PORT03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_Bg3[ which(PORT03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_Bg2 <- rbind(PORT03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
PORT03_Bft <- ftable(PORT03_Bg2$player1, PORT03_Bg2$player2)
PORT03_Bft2 <- as.matrix(PORT03_Bft)
numRows <- nrow(PORT03_Bft2)
numCols <- ncol(PORT03_Bft2)
PORT03_Bft3 <- PORT03_Bft2[c(2:numRows) , c(2:numCols)]
PORT03_BTable <- graph.adjacency(PORT03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(PORT03_BTable, vertex.label = V(PORT03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
PORT03_B.clusterCoef <- transitivity(PORT03_BTable, type="global") #cluster coefficient
PORT03_B.degreeCent <- centralization.degree(PORT03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_Bftn <- as.network.matrix(PORT03_Bft)
PORT03_B.netDensity <- network.density(PORT03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_B.entropy <- entropy(PORT03_Bft) #entropy

PORT03_B.netMx <- cbind(PORT03_B.netMx, PORT03_B.clusterCoef, PORT03_B.degreeCent$centralization,
                        PORT03_B.netDensity, PORT03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
PORT03_SFg2 <- data.frame(PORT03_SF)
PORT03_SFg2 <- PORT03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_SFg2$player1
player2vector <- PORT03_SFg2$player2
PORT03_SFg3 <- PORT03_SFg2
PORT03_SFg3$p1inp2vec <- is.element(PORT03_SFg3$player1, player2vector)
PORT03_SFg3$p2inp1vec <- is.element(PORT03_SFg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- PORT03_SFg3[ which(PORT03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

PORT03_SFg2 <- rbind(PORT03_SFg2, addPlayer2)

#Round 3, FWD Stoppage graph using weighted edges
PORT03_SFft <- ftable(PORT03_SFg2$player1, PORT03_SFg2$player2)
PORT03_SFft2 <- as.matrix(PORT03_SFft)
numRows <- nrow(PORT03_SFft2)
numCols <- ncol(PORT03_SFft2)
PORT03_SFft3 <- PORT03_SFft2[c(1:numRows) , c(2:numCols)]
PORT03_SFTable <- graph.adjacency(PORT03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(PORT03_SFTable, vertex.label = V(PORT03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
PORT03_SF.clusterCoef <- transitivity(PORT03_SFTable, type="global") #cluster coefficient
PORT03_SF.degreeCent <- centralization.degree(PORT03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_SFftn <- as.network.matrix(PORT03_SFft)
PORT03_SF.netDensity <- network.density(PORT03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_SF.entropy <- entropy(PORT03_SFft) #entropy

PORT03_SF.netMx <- cbind(PORT03_SF.netMx, PORT03_SF.clusterCoef, PORT03_SF.degreeCent$centralization,
                         PORT03_SF.netDensity, PORT03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
PORT03_TFg2 <- data.frame(PORT03_TF)
PORT03_TFg2 <- PORT03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_TFg2$player1
player2vector <- PORT03_TFg2$player2
PORT03_TFg3 <- PORT03_TFg2
PORT03_TFg3$p1inp2vec <- is.element(PORT03_TFg3$player1, player2vector)
PORT03_TFg3$p2inp1vec <- is.element(PORT03_TFg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- PORT03_TFg3[ which(PORT03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

PORT03_TFg2 <- rbind(PORT03_TFg2, addPlayer2)

#Round 3, FWD Turnover graph using weighted edges
PORT03_TFft <- ftable(PORT03_TFg2$player1, PORT03_TFg2$player2)
PORT03_TFft2 <- as.matrix(PORT03_TFft)
numRows <- nrow(PORT03_TFft2)
numCols <- ncol(PORT03_TFft2)
PORT03_TFft3 <- PORT03_TFft2[c(1:numRows) , c(2:numCols)]
PORT03_TFTable <- graph.adjacency(PORT03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(PORT03_TFTable, vertex.label = V(PORT03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
PORT03_TF.clusterCoef <- transitivity(PORT03_TFTable, type="global") #cluster coefficient
PORT03_TF.degreeCent <- centralization.degree(PORT03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_TFftn <- as.network.matrix(PORT03_TFft)
PORT03_TF.netDensity <- network.density(PORT03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_TF.entropy <- entropy(PORT03_TFft) #entropy

PORT03_TF.netMx <- cbind(PORT03_TF.netMx, PORT03_TF.clusterCoef, PORT03_TF.degreeCent$centralization,
                         PORT03_TF.netDensity, PORT03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
PORT03_SAMg2 <- data.frame(PORT03_SAM)
PORT03_SAMg2 <- PORT03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_SAMg2$player1
player2vector <- PORT03_SAMg2$player2
PORT03_SAMg3 <- PORT03_SAMg2
PORT03_SAMg3$p1inp2vec <- is.element(PORT03_SAMg3$player1, player2vector)
PORT03_SAMg3$p2inp1vec <- is.element(PORT03_SAMg3$player2, player1vector)

addPlayer1 <- PORT03_SAMg3[ which(PORT03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_SAMg3[ which(PORT03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_SAMg2 <- rbind(PORT03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
PORT03_SAMft <- ftable(PORT03_SAMg2$player1, PORT03_SAMg2$player2)
PORT03_SAMft2 <- as.matrix(PORT03_SAMft)
numRows <- nrow(PORT03_SAMft2)
numCols <- ncol(PORT03_SAMft2)
PORT03_SAMft3 <- PORT03_SAMft2[c(2:numRows) , c(2:numCols)]
PORT03_SAMTable <- graph.adjacency(PORT03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(PORT03_SAMTable, vertex.label = V(PORT03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
PORT03_SAM.clusterCoef <- transitivity(PORT03_SAMTable, type="global") #cluster coefficient
PORT03_SAM.degreeCent <- centralization.degree(PORT03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_SAMftn <- as.network.matrix(PORT03_SAMft)
PORT03_SAM.netDensity <- network.density(PORT03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_SAM.entropy <- entropy(PORT03_SAMft) #entropy

PORT03_SAM.netMx <- cbind(PORT03_SAM.netMx, PORT03_SAM.clusterCoef, PORT03_SAM.degreeCent$centralization,
                          PORT03_SAM.netDensity, PORT03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
PORT03_TAMg2 <- data.frame(PORT03_TAM)
PORT03_TAMg2 <- PORT03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_TAMg2$player1
player2vector <- PORT03_TAMg2$player2
PORT03_TAMg3 <- PORT03_TAMg2
PORT03_TAMg3$p1inp2vec <- is.element(PORT03_TAMg3$player1, player2vector)
PORT03_TAMg3$p2inp1vec <- is.element(PORT03_TAMg3$player2, player1vector)

addPlayer1 <- PORT03_TAMg3[ which(PORT03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_TAMg3[ which(PORT03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_TAMg2 <- rbind(PORT03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
PORT03_TAMft <- ftable(PORT03_TAMg2$player1, PORT03_TAMg2$player2)
PORT03_TAMft2 <- as.matrix(PORT03_TAMft)
numRows <- nrow(PORT03_TAMft2)
numCols <- ncol(PORT03_TAMft2)
PORT03_TAMft3 <- PORT03_TAMft2[c(2:numRows) , c(2:numCols)]
PORT03_TAMTable <- graph.adjacency(PORT03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(PORT03_TAMTable, vertex.label = V(PORT03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
PORT03_TAM.clusterCoef <- transitivity(PORT03_TAMTable, type="global") #cluster coefficient
PORT03_TAM.degreeCent <- centralization.degree(PORT03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_TAMftn <- as.network.matrix(PORT03_TAMft)
PORT03_TAM.netDensity <- network.density(PORT03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_TAM.entropy <- entropy(PORT03_TAMft) #entropy

PORT03_TAM.netMx <- cbind(PORT03_TAM.netMx, PORT03_TAM.clusterCoef, PORT03_TAM.degreeCent$centralization,
                          PORT03_TAM.netDensity, PORT03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
PORT03_SDMg2 <- data.frame(PORT03_SDM)
PORT03_SDMg2 <- PORT03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_SDMg2$player1
player2vector <- PORT03_SDMg2$player2
PORT03_SDMg3 <- PORT03_SDMg2
PORT03_SDMg3$p1inp2vec <- is.element(PORT03_SDMg3$player1, player2vector)
PORT03_SDMg3$p2inp1vec <- is.element(PORT03_SDMg3$player2, player1vector)

addPlayer1 <- PORT03_SDMg3[ which(PORT03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT03_SDMg2 <- rbind(PORT03_SDMg2, addPlayer1)

#Round 3, DM Stoppage graph using weighted edges
PORT03_SDMft <- ftable(PORT03_SDMg2$player1, PORT03_SDMg2$player2)
PORT03_SDMft2 <- as.matrix(PORT03_SDMft)
numRows <- nrow(PORT03_SDMft2)
numCols <- ncol(PORT03_SDMft2)
PORT03_SDMft3 <- PORT03_SDMft2[c(2:numRows) , c(1:numCols)]
PORT03_SDMTable <- graph.adjacency(PORT03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(PORT03_SDMTable, vertex.label = V(PORT03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
PORT03_SDM.clusterCoef <- transitivity(PORT03_SDMTable, type="global") #cluster coefficient
PORT03_SDM.degreeCent <- centralization.degree(PORT03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_SDMftn <- as.network.matrix(PORT03_SDMft)
PORT03_SDM.netDensity <- network.density(PORT03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_SDM.entropy <- entropy(PORT03_SDMft) #entropy

PORT03_SDM.netMx <- cbind(PORT03_SDM.netMx, PORT03_SDM.clusterCoef, PORT03_SDM.degreeCent$centralization,
                          PORT03_SDM.netDensity, PORT03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************
#NA

round = 3
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
PORT03_TDMg2 <- data.frame(PORT03_TDM)
PORT03_TDMg2 <- PORT03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_TDMg2$player1
player2vector <- PORT03_TDMg2$player2
PORT03_TDMg3 <- PORT03_TDMg2
PORT03_TDMg3$p1inp2vec <- is.element(PORT03_TDMg3$player1, player2vector)
PORT03_TDMg3$p2inp1vec <- is.element(PORT03_TDMg3$player2, player1vector)

addPlayer1 <- PORT03_TDMg3[ which(PORT03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_TDMg3[ which(PORT03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_TDMg2 <- rbind(PORT03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
PORT03_TDMft <- ftable(PORT03_TDMg2$player1, PORT03_TDMg2$player2)
PORT03_TDMft2 <- as.matrix(PORT03_TDMft)
numRows <- nrow(PORT03_TDMft2)
numCols <- ncol(PORT03_TDMft2)
PORT03_TDMft3 <- PORT03_TDMft2[c(2:numRows) , c(2:numCols)]
PORT03_TDMTable <- graph.adjacency(PORT03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(PORT03_TDMTable, vertex.label = V(PORT03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
PORT03_TDM.clusterCoef <- transitivity(PORT03_TDMTable, type="global") #cluster coefficient
PORT03_TDM.degreeCent <- centralization.degree(PORT03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_TDMftn <- as.network.matrix(PORT03_TDMft)
PORT03_TDM.netDensity <- network.density(PORT03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_TDM.entropy <- entropy(PORT03_TDMft) #entropy

PORT03_TDM.netMx <- cbind(PORT03_TDM.netMx, PORT03_TDM.clusterCoef, PORT03_TDM.degreeCent$centralization,
                          PORT03_TDM.netDensity, PORT03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
PORT03_SDg2 <- data.frame(PORT03_SD)
PORT03_SDg2 <- PORT03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_SDg2$player1
player2vector <- PORT03_SDg2$player2
PORT03_SDg3 <- PORT03_SDg2
PORT03_SDg3$p1inp2vec <- is.element(PORT03_SDg3$player1, player2vector)
PORT03_SDg3$p2inp1vec <- is.element(PORT03_SDg3$player2, player1vector)

addPlayer1 <- PORT03_SDg3[ which(PORT03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_SDg3[ which(PORT03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_SDg2 <- rbind(PORT03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
PORT03_SDft <- ftable(PORT03_SDg2$player1, PORT03_SDg2$player2)
PORT03_SDft2 <- as.matrix(PORT03_SDft)
numRows <- nrow(PORT03_SDft2)
numCols <- ncol(PORT03_SDft2)
PORT03_SDft3 <- PORT03_SDft2[c(2:numRows) , c(2:numCols)]
PORT03_SDTable <- graph.adjacency(PORT03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(PORT03_SDTable, vertex.label = V(PORT03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
PORT03_SD.clusterCoef <- transitivity(PORT03_SDTable, type="global") #cluster coefficient
PORT03_SD.degreeCent <- centralization.degree(PORT03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_SDftn <- as.network.matrix(PORT03_SDft)
PORT03_SD.netDensity <- network.density(PORT03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_SD.entropy <- entropy(PORT03_SDft) #entropy

PORT03_SD.netMx <- cbind(PORT03_SD.netMx, PORT03_SD.clusterCoef, PORT03_SD.degreeCent$centralization,
                         PORT03_SD.netDensity, PORT03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
PORT03_TDg2 <- data.frame(PORT03_TD)
PORT03_TDg2 <- PORT03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_TDg2$player1
player2vector <- PORT03_TDg2$player2
PORT03_TDg3 <- PORT03_TDg2
PORT03_TDg3$p1inp2vec <- is.element(PORT03_TDg3$player1, player2vector)
PORT03_TDg3$p2inp1vec <- is.element(PORT03_TDg3$player2, player1vector)

addPlayer1 <- PORT03_TDg3[ which(PORT03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_TDg3[ which(PORT03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_TDg2 <- rbind(PORT03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
PORT03_TDft <- ftable(PORT03_TDg2$player1, PORT03_TDg2$player2)
PORT03_TDft2 <- as.matrix(PORT03_TDft)
numRows <- nrow(PORT03_TDft2)
numCols <- ncol(PORT03_TDft2)
PORT03_TDft3 <- PORT03_TDft2[c(2:numRows) , c(2:numCols)]
PORT03_TDTable <- graph.adjacency(PORT03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(PORT03_TDTable, vertex.label = V(PORT03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
PORT03_TD.clusterCoef <- transitivity(PORT03_TDTable, type="global") #cluster coefficient
PORT03_TD.degreeCent <- centralization.degree(PORT03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_TDftn <- as.network.matrix(PORT03_TDft)
PORT03_TD.netDensity <- network.density(PORT03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_TD.entropy <- entropy(PORT03_TDft) #entropy

PORT03_TD.netMx <- cbind(PORT03_TD.netMx, PORT03_TD.clusterCoef, PORT03_TD.degreeCent$centralization,
                         PORT03_TD.netDensity, PORT03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
PORT03_QTg2 <- data.frame(PORT03_QT)
PORT03_QTg2 <- PORT03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT03_QTg2$player1
player2vector <- PORT03_QTg2$player2
PORT03_QTg3 <- PORT03_QTg2
PORT03_QTg3$p1inp2vec <- is.element(PORT03_QTg3$player1, player2vector)
PORT03_QTg3$p2inp1vec <- is.element(PORT03_QTg3$player2, player1vector)

addPlayer1 <- PORT03_QTg3[ which(PORT03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT03_QTg3[ which(PORT03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT03_QTg2 <- rbind(PORT03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
PORT03_QTft <- ftable(PORT03_QTg2$player1, PORT03_QTg2$player2)
PORT03_QTft2 <- as.matrix(PORT03_QTft)
numRows <- nrow(PORT03_QTft2)
numCols <- ncol(PORT03_QTft2)
PORT03_QTft3 <- PORT03_QTft2[c(2:numRows) , c(2:numCols)]
PORT03_QTTable <- graph.adjacency(PORT03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(PORT03_QTTable, vertex.label = V(PORT03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
PORT03_QT.clusterCoef <- transitivity(PORT03_QTTable, type="global") #cluster coefficient
PORT03_QT.degreeCent <- centralization.degree(PORT03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT03_QTftn <- as.network.matrix(PORT03_QTft)
PORT03_QT.netDensity <- network.density(PORT03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT03_QT.entropy <- entropy(PORT03_QTft) #entropy

PORT03_QT.netMx <- cbind(PORT03_QT.netMx, PORT03_QT.clusterCoef, PORT03_QT.degreeCent$centralization,
                         PORT03_QT.netDensity, PORT03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT03_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "RICH"
KIoutcome = "Goal_F"
RICH03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
RICH03_Gg2 <- data.frame(RICH03_G)
RICH03_Gg2 <- RICH03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_Gg2$player1
player2vector <- RICH03_Gg2$player2
RICH03_Gg3 <- RICH03_Gg2
RICH03_Gg3$p1inp2vec <- is.element(RICH03_Gg3$player1, player2vector)
RICH03_Gg3$p2inp1vec <- is.element(RICH03_Gg3$player2, player1vector)

addPlayer1 <- RICH03_Gg3[ which(RICH03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_Gg3[ which(RICH03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_Gg2 <- rbind(RICH03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
RICH03_Gft <- ftable(RICH03_Gg2$player1, RICH03_Gg2$player2)
RICH03_Gft2 <- as.matrix(RICH03_Gft)
numRows <- nrow(RICH03_Gft2)
numCols <- ncol(RICH03_Gft2)
RICH03_Gft3 <- RICH03_Gft2[c(2:numRows) , c(2:numCols)]
RICH03_GTable <- graph.adjacency(RICH03_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(RICH03_GTable, vertex.label = V(RICH03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
RICH03_G.clusterCoef <- transitivity(RICH03_GTable, type="global") #cluster coefficient
RICH03_G.degreeCent <- centralization.degree(RICH03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_Gftn <- as.network.matrix(RICH03_Gft)
RICH03_G.netDensity <- network.density(RICH03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_G.entropy <- entropy(RICH03_Gft) #entropy

RICH03_G.netMx <- cbind(RICH03_G.netMx, RICH03_G.clusterCoef, RICH03_G.degreeCent$centralization,
                        RICH03_G.netDensity, RICH03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "Behind_F"
RICH03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
RICH03_Bg2 <- data.frame(RICH03_B)
RICH03_Bg2 <- RICH03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_Bg2$player1
player2vector <- RICH03_Bg2$player2
RICH03_Bg3 <- RICH03_Bg2
RICH03_Bg3$p1inp2vec <- is.element(RICH03_Bg3$player1, player2vector)
RICH03_Bg3$p2inp1vec <- is.element(RICH03_Bg3$player2, player1vector)

addPlayer1 <- RICH03_Bg3[ which(RICH03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_Bg3[ which(RICH03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_Bg2 <- rbind(RICH03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
RICH03_Bft <- ftable(RICH03_Bg2$player1, RICH03_Bg2$player2)
RICH03_Bft2 <- as.matrix(RICH03_Bft)
numRows <- nrow(RICH03_Bft2)
numCols <- ncol(RICH03_Bft2)
RICH03_Bft3 <- RICH03_Bft2[c(2:numRows) , c(2:numCols)]
RICH03_BTable <- graph.adjacency(RICH03_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(RICH03_BTable, vertex.label = V(RICH03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
RICH03_B.clusterCoef <- transitivity(RICH03_BTable, type="global") #cluster coefficient
RICH03_B.degreeCent <- centralization.degree(RICH03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_Bftn <- as.network.matrix(RICH03_Bft)
RICH03_B.netDensity <- network.density(RICH03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_B.entropy <- entropy(RICH03_Bft) #entropy

RICH03_B.netMx <- cbind(RICH03_B.netMx, RICH03_B.clusterCoef, RICH03_B.degreeCent$centralization,
                        RICH03_B.netDensity, RICH03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
RICH03_SFg2 <- data.frame(RICH03_SF)
RICH03_SFg2 <- RICH03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_SFg2$player1
player2vector <- RICH03_SFg2$player2
RICH03_SFg3 <- RICH03_SFg2
RICH03_SFg3$p1inp2vec <- is.element(RICH03_SFg3$player1, player2vector)
RICH03_SFg3$p2inp1vec <- is.element(RICH03_SFg3$player2, player1vector)

addPlayer1 <- RICH03_SFg3[ which(RICH03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_SFg3[ which(RICH03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_SFg2 <- rbind(RICH03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
RICH03_SFft <- ftable(RICH03_SFg2$player1, RICH03_SFg2$player2)
RICH03_SFft2 <- as.matrix(RICH03_SFft)
numRows <- nrow(RICH03_SFft2)
numCols <- ncol(RICH03_SFft2)
RICH03_SFft3 <- RICH03_SFft2[c(2:numRows) , c(2:numCols)]
RICH03_SFTable <- graph.adjacency(RICH03_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(RICH03_SFTable, vertex.label = V(RICH03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
RICH03_SF.clusterCoef <- transitivity(RICH03_SFTable, type="global") #cluster coefficient
RICH03_SF.degreeCent <- centralization.degree(RICH03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_SFftn <- as.network.matrix(RICH03_SFft)
RICH03_SF.netDensity <- network.density(RICH03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_SF.entropy <- entropy(RICH03_SFft) #entropy

RICH03_SF.netMx <- cbind(RICH03_SF.netMx, RICH03_SF.clusterCoef, RICH03_SF.degreeCent$centralization,
                         RICH03_SF.netDensity, RICH03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
RICH03_TFg2 <- data.frame(RICH03_TF)
RICH03_TFg2 <- RICH03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_TFg2$player1
player2vector <- RICH03_TFg2$player2
RICH03_TFg3 <- RICH03_TFg2
RICH03_TFg3$p1inp2vec <- is.element(RICH03_TFg3$player1, player2vector)
RICH03_TFg3$p2inp1vec <- is.element(RICH03_TFg3$player2, player1vector)

addPlayer1 <- RICH03_TFg3[ which(RICH03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_TFg3[ which(RICH03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_TFg2 <- rbind(RICH03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
RICH03_TFft <- ftable(RICH03_TFg2$player1, RICH03_TFg2$player2)
RICH03_TFft2 <- as.matrix(RICH03_TFft)
numRows <- nrow(RICH03_TFft2)
numCols <- ncol(RICH03_TFft2)
RICH03_TFft3 <- RICH03_TFft2[c(2:numRows) , c(2:numCols)]
RICH03_TFTable <- graph.adjacency(RICH03_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(RICH03_TFTable, vertex.label = V(RICH03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
RICH03_TF.clusterCoef <- transitivity(RICH03_TFTable, type="global") #cluster coefficient
RICH03_TF.degreeCent <- centralization.degree(RICH03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_TFftn <- as.network.matrix(RICH03_TFft)
RICH03_TF.netDensity <- network.density(RICH03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_TF.entropy <- entropy(RICH03_TFft) #entropy

RICH03_TF.netMx <- cbind(RICH03_TF.netMx, RICH03_TF.clusterCoef, RICH03_TF.degreeCent$centralization,
                         RICH03_TF.netDensity, RICH03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
RICH03_SAMg2 <- data.frame(RICH03_SAM)
RICH03_SAMg2 <- RICH03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_SAMg2$player1
player2vector <- RICH03_SAMg2$player2
RICH03_SAMg3 <- RICH03_SAMg2
RICH03_SAMg3$p1inp2vec <- is.element(RICH03_SAMg3$player1, player2vector)
RICH03_SAMg3$p2inp1vec <- is.element(RICH03_SAMg3$player2, player1vector)

addPlayer1 <- RICH03_SAMg3[ which(RICH03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_SAMg3[ which(RICH03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_SAMg2 <- rbind(RICH03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
RICH03_SAMft <- ftable(RICH03_SAMg2$player1, RICH03_SAMg2$player2)
RICH03_SAMft2 <- as.matrix(RICH03_SAMft)
numRows <- nrow(RICH03_SAMft2)
numCols <- ncol(RICH03_SAMft2)
RICH03_SAMft3 <- RICH03_SAMft2[c(2:numRows) , c(2:numCols)]
RICH03_SAMTable <- graph.adjacency(RICH03_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(RICH03_SAMTable, vertex.label = V(RICH03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
RICH03_SAM.clusterCoef <- transitivity(RICH03_SAMTable, type="global") #cluster coefficient
RICH03_SAM.degreeCent <- centralization.degree(RICH03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_SAMftn <- as.network.matrix(RICH03_SAMft)
RICH03_SAM.netDensity <- network.density(RICH03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_SAM.entropy <- entropy(RICH03_SAMft) #entropy

RICH03_SAM.netMx <- cbind(RICH03_SAM.netMx, RICH03_SAM.clusterCoef, RICH03_SAM.degreeCent$centralization,
                          RICH03_SAM.netDensity, RICH03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
RICH03_TAMg2 <- data.frame(RICH03_TAM)
RICH03_TAMg2 <- RICH03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_TAMg2$player1
player2vector <- RICH03_TAMg2$player2
RICH03_TAMg3 <- RICH03_TAMg2
RICH03_TAMg3$p1inp2vec <- is.element(RICH03_TAMg3$player1, player2vector)
RICH03_TAMg3$p2inp1vec <- is.element(RICH03_TAMg3$player2, player1vector)

addPlayer1 <- RICH03_TAMg3[ which(RICH03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_TAMg3[ which(RICH03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_TAMg2 <- rbind(RICH03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
RICH03_TAMft <- ftable(RICH03_TAMg2$player1, RICH03_TAMg2$player2)
RICH03_TAMft2 <- as.matrix(RICH03_TAMft)
numRows <- nrow(RICH03_TAMft2)
numCols <- ncol(RICH03_TAMft2)
RICH03_TAMft3 <- RICH03_TAMft2[c(2:numRows) , c(2:numCols)]
RICH03_TAMTable <- graph.adjacency(RICH03_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(RICH03_TAMTable, vertex.label = V(RICH03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
RICH03_TAM.clusterCoef <- transitivity(RICH03_TAMTable, type="global") #cluster coefficient
RICH03_TAM.degreeCent <- centralization.degree(RICH03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_TAMftn <- as.network.matrix(RICH03_TAMft)
RICH03_TAM.netDensity <- network.density(RICH03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_TAM.entropy <- entropy(RICH03_TAMft) #entropy

RICH03_TAM.netMx <- cbind(RICH03_TAM.netMx, RICH03_TAM.clusterCoef, RICH03_TAM.degreeCent$centralization,
                          RICH03_TAM.netDensity, RICH03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
RICH03_SDMg2 <- data.frame(RICH03_SDM)
RICH03_SDMg2 <- RICH03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_SDMg2$player1
player2vector <- RICH03_SDMg2$player2
RICH03_SDMg3 <- RICH03_SDMg2
RICH03_SDMg3$p1inp2vec <- is.element(RICH03_SDMg3$player1, player2vector)
RICH03_SDMg3$p2inp1vec <- is.element(RICH03_SDMg3$player2, player1vector)

addPlayer1 <- RICH03_SDMg3[ which(RICH03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_SDMg3[ which(RICH03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_SDMg2 <- rbind(RICH03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
RICH03_SDMft <- ftable(RICH03_SDMg2$player1, RICH03_SDMg2$player2)
RICH03_SDMft2 <- as.matrix(RICH03_SDMft)
numRows <- nrow(RICH03_SDMft2)
numCols <- ncol(RICH03_SDMft2)
RICH03_SDMft3 <- RICH03_SDMft2[c(2:numRows) , c(2:numCols)]
RICH03_SDMTable <- graph.adjacency(RICH03_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(RICH03_SDMTable, vertex.label = V(RICH03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
RICH03_SDM.clusterCoef <- transitivity(RICH03_SDMTable, type="global") #cluster coefficient
RICH03_SDM.degreeCent <- centralization.degree(RICH03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_SDMftn <- as.network.matrix(RICH03_SDMft)
RICH03_SDM.netDensity <- network.density(RICH03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_SDM.entropy <- entropy(RICH03_SDMft) #entropy

RICH03_SDM.netMx <- cbind(RICH03_SDM.netMx, RICH03_SDM.clusterCoef, RICH03_SDM.degreeCent$centralization,
                          RICH03_SDM.netDensity, RICH03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
RICH03_TDMg2 <- data.frame(RICH03_TDM)
RICH03_TDMg2 <- RICH03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_TDMg2$player1
player2vector <- RICH03_TDMg2$player2
RICH03_TDMg3 <- RICH03_TDMg2
RICH03_TDMg3$p1inp2vec <- is.element(RICH03_TDMg3$player1, player2vector)
RICH03_TDMg3$p2inp1vec <- is.element(RICH03_TDMg3$player2, player1vector)

addPlayer1 <- RICH03_TDMg3[ which(RICH03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_TDMg3[ which(RICH03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_TDMg2 <- rbind(RICH03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
RICH03_TDMft <- ftable(RICH03_TDMg2$player1, RICH03_TDMg2$player2)
RICH03_TDMft2 <- as.matrix(RICH03_TDMft)
numRows <- nrow(RICH03_TDMft2)
numCols <- ncol(RICH03_TDMft2)
RICH03_TDMft3 <- RICH03_TDMft2[c(2:numRows) , c(2:numCols)]
RICH03_TDMTable <- graph.adjacency(RICH03_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(RICH03_TDMTable, vertex.label = V(RICH03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
RICH03_TDM.clusterCoef <- transitivity(RICH03_TDMTable, type="global") #cluster coefficient
RICH03_TDM.degreeCent <- centralization.degree(RICH03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_TDMftn <- as.network.matrix(RICH03_TDMft)
RICH03_TDM.netDensity <- network.density(RICH03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_TDM.entropy <- entropy(RICH03_TDMft) #entropy

RICH03_TDM.netMx <- cbind(RICH03_TDM.netMx, RICH03_TDM.clusterCoef, RICH03_TDM.degreeCent$centralization,
                          RICH03_TDM.netDensity, RICH03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
RICH03_SDg2 <- data.frame(RICH03_SD)
RICH03_SDg2 <- RICH03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_SDg2$player1
player2vector <- RICH03_SDg2$player2
RICH03_SDg3 <- RICH03_SDg2
RICH03_SDg3$p1inp2vec <- is.element(RICH03_SDg3$player1, player2vector)
RICH03_SDg3$p2inp1vec <- is.element(RICH03_SDg3$player2, player1vector)

addPlayer1 <- RICH03_SDg3[ which(RICH03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_SDg3[ which(RICH03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_SDg2 <- rbind(RICH03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
RICH03_SDft <- ftable(RICH03_SDg2$player1, RICH03_SDg2$player2)
RICH03_SDft2 <- as.matrix(RICH03_SDft)
numRows <- nrow(RICH03_SDft2)
numCols <- ncol(RICH03_SDft2)
RICH03_SDft3 <- RICH03_SDft2[c(2:numRows) , c(2:numCols)]
RICH03_SDTable <- graph.adjacency(RICH03_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(RICH03_SDTable, vertex.label = V(RICH03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
RICH03_SD.clusterCoef <- transitivity(RICH03_SDTable, type="global") #cluster coefficient
RICH03_SD.degreeCent <- centralization.degree(RICH03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_SDftn <- as.network.matrix(RICH03_SDft)
RICH03_SD.netDensity <- network.density(RICH03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_SD.entropy <- entropy(RICH03_SDft) #entropy

RICH03_SD.netMx <- cbind(RICH03_SD.netMx, RICH03_SD.clusterCoef, RICH03_SD.degreeCent$centralization,
                         RICH03_SD.netDensity, RICH03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
RICH03_TDg2 <- data.frame(RICH03_TD)
RICH03_TDg2 <- RICH03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_TDg2$player1
player2vector <- RICH03_TDg2$player2
RICH03_TDg3 <- RICH03_TDg2
RICH03_TDg3$p1inp2vec <- is.element(RICH03_TDg3$player1, player2vector)
RICH03_TDg3$p2inp1vec <- is.element(RICH03_TDg3$player2, player1vector)

addPlayer1 <- RICH03_TDg3[ which(RICH03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_TDg3[ which(RICH03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_TDg2 <- rbind(RICH03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
RICH03_TDft <- ftable(RICH03_TDg2$player1, RICH03_TDg2$player2)
RICH03_TDft2 <- as.matrix(RICH03_TDft)
numRows <- nrow(RICH03_TDft2)
numCols <- ncol(RICH03_TDft2)
RICH03_TDft3 <- RICH03_TDft2[c(2:numRows) , c(2:numCols)]
RICH03_TDTable <- graph.adjacency(RICH03_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(RICH03_TDTable, vertex.label = V(RICH03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
RICH03_TD.clusterCoef <- transitivity(RICH03_TDTable, type="global") #cluster coefficient
RICH03_TD.degreeCent <- centralization.degree(RICH03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_TDftn <- as.network.matrix(RICH03_TDft)
RICH03_TD.netDensity <- network.density(RICH03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_TD.entropy <- entropy(RICH03_TDft) #entropy

RICH03_TD.netMx <- cbind(RICH03_TD.netMx, RICH03_TD.clusterCoef, RICH03_TD.degreeCent$centralization,
                         RICH03_TD.netDensity, RICH03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
RICH03_QTg2 <- data.frame(RICH03_QT)
RICH03_QTg2 <- RICH03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH03_QTg2$player1
player2vector <- RICH03_QTg2$player2
RICH03_QTg3 <- RICH03_QTg2
RICH03_QTg3$p1inp2vec <- is.element(RICH03_QTg3$player1, player2vector)
RICH03_QTg3$p2inp1vec <- is.element(RICH03_QTg3$player2, player1vector)

addPlayer1 <- RICH03_QTg3[ which(RICH03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH03_QTg3[ which(RICH03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH03_QTg2 <- rbind(RICH03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
RICH03_QTft <- ftable(RICH03_QTg2$player1, RICH03_QTg2$player2)
RICH03_QTft2 <- as.matrix(RICH03_QTft)
numRows <- nrow(RICH03_QTft2)
numCols <- ncol(RICH03_QTft2)
RICH03_QTft3 <- RICH03_QTft2[c(2:numRows) , c(2:numCols)]
RICH03_QTTable <- graph.adjacency(RICH03_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(RICH03_QTTable, vertex.label = V(RICH03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
RICH03_QT.clusterCoef <- transitivity(RICH03_QTTable, type="global") #cluster coefficient
RICH03_QT.degreeCent <- centralization.degree(RICH03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH03_QTftn <- as.network.matrix(RICH03_QTft)
RICH03_QT.netDensity <- network.density(RICH03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH03_QT.entropy <- entropy(RICH03_QTft) #entropy

RICH03_QT.netMx <- cbind(RICH03_QT.netMx, RICH03_QT.clusterCoef, RICH03_QT.degreeCent$centralization,
                         RICH03_QT.netDensity, RICH03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH03_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "STK"
KIoutcome = "Goal_F"
STK03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
STK03_Gg2 <- data.frame(STK03_G)
STK03_Gg2 <- STK03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_Gg2$player1
player2vector <- STK03_Gg2$player2
STK03_Gg3 <- STK03_Gg2
STK03_Gg3$p1inp2vec <- is.element(STK03_Gg3$player1, player2vector)
STK03_Gg3$p2inp1vec <- is.element(STK03_Gg3$player2, player1vector)

addPlayer1 <- STK03_Gg3[ which(STK03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_Gg3[ which(STK03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_Gg2 <- rbind(STK03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
STK03_Gft <- ftable(STK03_Gg2$player1, STK03_Gg2$player2)
STK03_Gft2 <- as.matrix(STK03_Gft)
numRows <- nrow(STK03_Gft2)
numCols <- ncol(STK03_Gft2)
STK03_Gft3 <- STK03_Gft2[c(2:numRows) , c(2:numCols)]
STK03_GTable <- graph.adjacency(STK03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(STK03_GTable, vertex.label = V(STK03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
STK03_G.clusterCoef <- transitivity(STK03_GTable, type="global") #cluster coefficient
STK03_G.degreeCent <- centralization.degree(STK03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_Gftn <- as.network.matrix(STK03_Gft)
STK03_G.netDensity <- network.density(STK03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_G.entropy <- entropy(STK03_Gft) #entropy

STK03_G.netMx <- cbind(STK03_G.netMx, STK03_G.clusterCoef, STK03_G.degreeCent$centralization,
                       STK03_G.netDensity, STK03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "Behind_F"
STK03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
STK03_Bg2 <- data.frame(STK03_B)
STK03_Bg2 <- STK03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_Bg2$player1
player2vector <- STK03_Bg2$player2
STK03_Bg3 <- STK03_Bg2
STK03_Bg3$p1inp2vec <- is.element(STK03_Bg3$player1, player2vector)
STK03_Bg3$p2inp1vec <- is.element(STK03_Bg3$player2, player1vector)

addPlayer1 <- STK03_Bg3[ which(STK03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_Bg3[ which(STK03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_Bg2 <- rbind(STK03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
STK03_Bft <- ftable(STK03_Bg2$player1, STK03_Bg2$player2)
STK03_Bft2 <- as.matrix(STK03_Bft)
numRows <- nrow(STK03_Bft2)
numCols <- ncol(STK03_Bft2)
STK03_Bft3 <- STK03_Bft2[c(2:numRows) , c(2:numCols)]
STK03_BTable <- graph.adjacency(STK03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(STK03_BTable, vertex.label = V(STK03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
STK03_B.clusterCoef <- transitivity(STK03_BTable, type="global") #cluster coefficient
STK03_B.degreeCent <- centralization.degree(STK03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_Bftn <- as.network.matrix(STK03_Bft)
STK03_B.netDensity <- network.density(STK03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_B.entropy <- entropy(STK03_Bft) #entropy

STK03_B.netMx <- cbind(STK03_B.netMx, STK03_B.clusterCoef, STK03_B.degreeCent$centralization,
                       STK03_B.netDensity, STK03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "Stoppage_F"
STK03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
STK03_SFg2 <- data.frame(STK03_SF)
STK03_SFg2 <- STK03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_SFg2$player1
player2vector <- STK03_SFg2$player2
STK03_SFg3 <- STK03_SFg2
STK03_SFg3$p1inp2vec <- is.element(STK03_SFg3$player1, player2vector)
STK03_SFg3$p2inp1vec <- is.element(STK03_SFg3$player2, player1vector)

addPlayer1 <- STK03_SFg3[ which(STK03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_SFg3[ which(STK03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_SFg2 <- rbind(STK03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
STK03_SFft <- ftable(STK03_SFg2$player1, STK03_SFg2$player2)
STK03_SFft2 <- as.matrix(STK03_SFft)
numRows <- nrow(STK03_SFft2)
numCols <- ncol(STK03_SFft2)
STK03_SFft3 <- STK03_SFft2[c(2:numRows) , c(2:numCols)]
STK03_SFTable <- graph.adjacency(STK03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(STK03_SFTable, vertex.label = V(STK03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
STK03_SF.clusterCoef <- transitivity(STK03_SFTable, type="global") #cluster coefficient
STK03_SF.degreeCent <- centralization.degree(STK03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_SFftn <- as.network.matrix(STK03_SFft)
STK03_SF.netDensity <- network.density(STK03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_SF.entropy <- entropy(STK03_SFft) #entropy

STK03_SF.netMx <- cbind(STK03_SF.netMx, STK03_SF.clusterCoef, STK03_SF.degreeCent$centralization,
                        STK03_SF.netDensity, STK03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "Turnover_F"
STK03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
STK03_TFg2 <- data.frame(STK03_TF)
STK03_TFg2 <- STK03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_TFg2$player1
player2vector <- STK03_TFg2$player2
STK03_TFg3 <- STK03_TFg2
STK03_TFg3$p1inp2vec <- is.element(STK03_TFg3$player1, player2vector)
STK03_TFg3$p2inp1vec <- is.element(STK03_TFg3$player2, player1vector)

addPlayer1 <- STK03_TFg3[ which(STK03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_TFg3[ which(STK03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_TFg2 <- rbind(STK03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
STK03_TFft <- ftable(STK03_TFg2$player1, STK03_TFg2$player2)
STK03_TFft2 <- as.matrix(STK03_TFft)
numRows <- nrow(STK03_TFft2)
numCols <- ncol(STK03_TFft2)
STK03_TFft3 <- STK03_TFft2[c(2:numRows) , c(2:numCols)]
STK03_TFTable <- graph.adjacency(STK03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(STK03_TFTable, vertex.label = V(STK03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
STK03_TF.clusterCoef <- transitivity(STK03_TFTable, type="global") #cluster coefficient
STK03_TF.degreeCent <- centralization.degree(STK03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_TFftn <- as.network.matrix(STK03_TFft)
STK03_TF.netDensity <- network.density(STK03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_TF.entropy <- entropy(STK03_TFft) #entropy

STK03_TF.netMx <- cbind(STK03_TF.netMx, STK03_TF.clusterCoef, STK03_TF.degreeCent$centralization,
                        STK03_TF.netDensity, STK03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
STK03_SAMg2 <- data.frame(STK03_SAM)
STK03_SAMg2 <- STK03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_SAMg2$player1
player2vector <- STK03_SAMg2$player2
STK03_SAMg3 <- STK03_SAMg2
STK03_SAMg3$p1inp2vec <- is.element(STK03_SAMg3$player1, player2vector)
STK03_SAMg3$p2inp1vec <- is.element(STK03_SAMg3$player2, player1vector)

addPlayer1 <- STK03_SAMg3[ which(STK03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_SAMg3[ which(STK03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_SAMg2 <- rbind(STK03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
STK03_SAMft <- ftable(STK03_SAMg2$player1, STK03_SAMg2$player2)
STK03_SAMft2 <- as.matrix(STK03_SAMft)
numRows <- nrow(STK03_SAMft2)
numCols <- ncol(STK03_SAMft2)
STK03_SAMft3 <- STK03_SAMft2[c(2:numRows) , c(2:numCols)]
STK03_SAMTable <- graph.adjacency(STK03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(STK03_SAMTable, vertex.label = V(STK03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
STK03_SAM.clusterCoef <- transitivity(STK03_SAMTable, type="global") #cluster coefficient
STK03_SAM.degreeCent <- centralization.degree(STK03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_SAMftn <- as.network.matrix(STK03_SAMft)
STK03_SAM.netDensity <- network.density(STK03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_SAM.entropy <- entropy(STK03_SAMft) #entropy

STK03_SAM.netMx <- cbind(STK03_SAM.netMx, STK03_SAM.clusterCoef, STK03_SAM.degreeCent$centralization,
                         STK03_SAM.netDensity, STK03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "Turnover_AM"
STK03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
STK03_TAMg2 <- data.frame(STK03_TAM)
STK03_TAMg2 <- STK03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_TAMg2$player1
player2vector <- STK03_TAMg2$player2
STK03_TAMg3 <- STK03_TAMg2
STK03_TAMg3$p1inp2vec <- is.element(STK03_TAMg3$player1, player2vector)
STK03_TAMg3$p2inp1vec <- is.element(STK03_TAMg3$player2, player1vector)

addPlayer1 <- STK03_TAMg3[ which(STK03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_TAMg3[ which(STK03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_TAMg2 <- rbind(STK03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
STK03_TAMft <- ftable(STK03_TAMg2$player1, STK03_TAMg2$player2)
STK03_TAMft2 <- as.matrix(STK03_TAMft)
numRows <- nrow(STK03_TAMft2)
numCols <- ncol(STK03_TAMft2)
STK03_TAMft3 <- STK03_TAMft2[c(1:numRows) , c(1:numCols)]
STK03_TAMTable <- graph.adjacency(STK03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(STK03_TAMTable, vertex.label = V(STK03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
STK03_TAM.clusterCoef <- transitivity(STK03_TAMTable, type="global") #cluster coefficient
STK03_TAM.degreeCent <- centralization.degree(STK03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_TAMftn <- as.network.matrix(STK03_TAMft)
STK03_TAM.netDensity <- network.density(STK03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_TAM.entropy <- entropy(STK03_TAMft) #entropy

STK03_TAM.netMx <- cbind(STK03_TAM.netMx, STK03_TAM.clusterCoef, STK03_TAM.degreeCent$centralization,
                         STK03_TAM.netDensity, STK03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
STK03_SDMg2 <- data.frame(STK03_SDM)
STK03_SDMg2 <- STK03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_SDMg2$player1
player2vector <- STK03_SDMg2$player2
STK03_SDMg3 <- STK03_SDMg2
STK03_SDMg3$p1inp2vec <- is.element(STK03_SDMg3$player1, player2vector)
STK03_SDMg3$p2inp1vec <- is.element(STK03_SDMg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- STK03_SDMg3[ which(STK03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

STK03_SDMg2 <- rbind(STK03_SDMg2, addPlayer2)

#Round 3, DM Stoppage graph using weighted edges
STK03_SDMft <- ftable(STK03_SDMg2$player1, STK03_SDMg2$player2)
STK03_SDMft2 <- as.matrix(STK03_SDMft)
numRows <- nrow(STK03_SDMft2)
numCols <- ncol(STK03_SDMft2)
STK03_SDMft3 <- STK03_SDMft2[c(1:numRows) , c(2:numCols)]
STK03_SDMTable <- graph.adjacency(STK03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(STK03_SDMTable, vertex.label = V(STK03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
STK03_SDM.clusterCoef <- transitivity(STK03_SDMTable, type="global") #cluster coefficient
STK03_SDM.degreeCent <- centralization.degree(STK03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_SDMftn <- as.network.matrix(STK03_SDMft)
STK03_SDM.netDensity <- network.density(STK03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_SDM.entropy <- entropy(STK03_SDMft) #entropy

STK03_SDM.netMx <- cbind(STK03_SDM.netMx, STK03_SDM.clusterCoef, STK03_SDM.degreeCent$centralization,
                         STK03_SDM.netDensity, STK03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "STK"
KIoutcome = "Turnover_DM"
STK03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
STK03_TDMg2 <- data.frame(STK03_TDM)
STK03_TDMg2 <- STK03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_TDMg2$player1
player2vector <- STK03_TDMg2$player2
STK03_TDMg3 <- STK03_TDMg2
STK03_TDMg3$p1inp2vec <- is.element(STK03_TDMg3$player1, player2vector)
STK03_TDMg3$p2inp1vec <- is.element(STK03_TDMg3$player2, player1vector)

addPlayer1 <- STK03_TDMg3[ which(STK03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_TDMg3[ which(STK03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_TDMg2 <- rbind(STK03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
STK03_TDMft <- ftable(STK03_TDMg2$player1, STK03_TDMg2$player2)
STK03_TDMft2 <- as.matrix(STK03_TDMft)
numRows <- nrow(STK03_TDMft2)
numCols <- ncol(STK03_TDMft2)
STK03_TDMft3 <- STK03_TDMft2[c(2:numRows) , c(2:numCols)]
STK03_TDMTable <- graph.adjacency(STK03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(STK03_TDMTable, vertex.label = V(STK03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
STK03_TDM.clusterCoef <- transitivity(STK03_TDMTable, type="global") #cluster coefficient
STK03_TDM.degreeCent <- centralization.degree(STK03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_TDMftn <- as.network.matrix(STK03_TDMft)
STK03_TDM.netDensity <- network.density(STK03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_TDM.entropy <- entropy(STK03_TDMft) #entropy

STK03_TDM.netMx <- cbind(STK03_TDM.netMx, STK03_TDM.clusterCoef, STK03_TDM.degreeCent$centralization,
                         STK03_TDM.netDensity, STK03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "Stoppage_D"
STK03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
STK03_SDg2 <- data.frame(STK03_SD)
STK03_SDg2 <- STK03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_SDg2$player1
player2vector <- STK03_SDg2$player2
STK03_SDg3 <- STK03_SDg2
STK03_SDg3$p1inp2vec <- is.element(STK03_SDg3$player1, player2vector)
STK03_SDg3$p2inp1vec <- is.element(STK03_SDg3$player2, player1vector)

addPlayer1 <- STK03_SDg3[ which(STK03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_SDg3[ which(STK03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_SDg2 <- rbind(STK03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
STK03_SDft <- ftable(STK03_SDg2$player1, STK03_SDg2$player2)
STK03_SDft2 <- as.matrix(STK03_SDft)
numRows <- nrow(STK03_SDft2)
numCols <- ncol(STK03_SDft2)
STK03_SDft3 <- STK03_SDft2[c(2:numRows) , c(2:numCols)]
STK03_SDTable <- graph.adjacency(STK03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(STK03_SDTable, vertex.label = V(STK03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
STK03_SD.clusterCoef <- transitivity(STK03_SDTable, type="global") #cluster coefficient
STK03_SD.degreeCent <- centralization.degree(STK03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_SDftn <- as.network.matrix(STK03_SDft)
STK03_SD.netDensity <- network.density(STK03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_SD.entropy <- entropy(STK03_SDft) #entropy

STK03_SD.netMx <- cbind(STK03_SD.netMx, STK03_SD.clusterCoef, STK03_SD.degreeCent$centralization,
                        STK03_SD.netDensity, STK03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "Turnover_D"
STK03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
STK03_TDg2 <- data.frame(STK03_TD)
STK03_TDg2 <- STK03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_TDg2$player1
player2vector <- STK03_TDg2$player2
STK03_TDg3 <- STK03_TDg2
STK03_TDg3$p1inp2vec <- is.element(STK03_TDg3$player1, player2vector)
STK03_TDg3$p2inp1vec <- is.element(STK03_TDg3$player2, player1vector)

addPlayer1 <- STK03_TDg3[ which(STK03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_TDg3[ which(STK03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_TDg2 <- rbind(STK03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
STK03_TDft <- ftable(STK03_TDg2$player1, STK03_TDg2$player2)
STK03_TDft2 <- as.matrix(STK03_TDft)
numRows <- nrow(STK03_TDft2)
numCols <- ncol(STK03_TDft2)
STK03_TDft3 <- STK03_TDft2[c(2:numRows) , c(2:numCols)]
STK03_TDTable <- graph.adjacency(STK03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(STK03_TDTable, vertex.label = V(STK03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
STK03_TD.clusterCoef <- transitivity(STK03_TDTable, type="global") #cluster coefficient
STK03_TD.degreeCent <- centralization.degree(STK03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_TDftn <- as.network.matrix(STK03_TDft)
STK03_TD.netDensity <- network.density(STK03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_TD.entropy <- entropy(STK03_TDft) #entropy

STK03_TD.netMx <- cbind(STK03_TD.netMx, STK03_TD.clusterCoef, STK03_TD.degreeCent$centralization,
                        STK03_TD.netDensity, STK03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
STK03_QTg2 <- data.frame(STK03_QT)
STK03_QTg2 <- STK03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK03_QTg2$player1
player2vector <- STK03_QTg2$player2
STK03_QTg3 <- STK03_QTg2
STK03_QTg3$p1inp2vec <- is.element(STK03_QTg3$player1, player2vector)
STK03_QTg3$p2inp1vec <- is.element(STK03_QTg3$player2, player1vector)

addPlayer1 <- STK03_QTg3[ which(STK03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK03_QTg3[ which(STK03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK03_QTg2 <- rbind(STK03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
STK03_QTft <- ftable(STK03_QTg2$player1, STK03_QTg2$player2)
STK03_QTft2 <- as.matrix(STK03_QTft)
numRows <- nrow(STK03_QTft2)
numCols <- ncol(STK03_QTft2)
STK03_QTft3 <- STK03_QTft2[c(2:numRows) , c(2:numCols)]
STK03_QTTable <- graph.adjacency(STK03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(STK03_QTTable, vertex.label = V(STK03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
STK03_QT.clusterCoef <- transitivity(STK03_QTTable, type="global") #cluster coefficient
STK03_QT.degreeCent <- centralization.degree(STK03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK03_QTftn <- as.network.matrix(STK03_QTft)
STK03_QT.netDensity <- network.density(STK03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK03_QT.entropy <- entropy(STK03_QTft) #entropy

STK03_QT.netMx <- cbind(STK03_QT.netMx, STK03_QT.clusterCoef, STK03_QT.degreeCent$centralization,
                        STK03_QT.netDensity, STK03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK03_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "SYD"
KIoutcome = "Goal_F"
SYD03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
SYD03_Gg2 <- data.frame(SYD03_G)
SYD03_Gg2 <- SYD03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_Gg2$player1
player2vector <- SYD03_Gg2$player2
SYD03_Gg3 <- SYD03_Gg2
SYD03_Gg3$p1inp2vec <- is.element(SYD03_Gg3$player1, player2vector)
SYD03_Gg3$p2inp1vec <- is.element(SYD03_Gg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- SYD03_Gg3[ which(SYD03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

SYD03_Gg2 <- rbind(SYD03_Gg2, addPlayer2)

#Round 3, Goal graph using weighted edges
SYD03_Gft <- ftable(SYD03_Gg2$player1, SYD03_Gg2$player2)
SYD03_Gft2 <- as.matrix(SYD03_Gft)
numRows <- nrow(SYD03_Gft2)
numCols <- ncol(SYD03_Gft2)
SYD03_Gft3 <- SYD03_Gft2[c(1:numRows) , c(2:numCols)]
SYD03_GTable <- graph.adjacency(SYD03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(SYD03_GTable, vertex.label = V(SYD03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
SYD03_G.clusterCoef <- transitivity(SYD03_GTable, type="global") #cluster coefficient
SYD03_G.degreeCent <- centralization.degree(SYD03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_Gftn <- as.network.matrix(SYD03_Gft)
SYD03_G.netDensity <- network.density(SYD03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_G.entropy <- entropy(SYD03_Gft) #entropy

SYD03_G.netMx <- cbind(SYD03_G.netMx, SYD03_G.clusterCoef, SYD03_G.degreeCent$centralization,
                       SYD03_G.netDensity, SYD03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_G.netMx) <- varnames

#Round 3, Behind***************************************************************

round = 3
teamName = "SYD"
KIoutcome = "Behind_F"
SYD03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
SYD03_Bg2 <- data.frame(SYD03_B)
SYD03_Bg2 <- SYD03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_Bg2$player1
player2vector <- SYD03_Bg2$player2
SYD03_Bg3 <- SYD03_Bg2
SYD03_Bg3$p1inp2vec <- is.element(SYD03_Bg3$player1, player2vector)
SYD03_Bg3$p2inp1vec <- is.element(SYD03_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- SYD03_Bg3[ which(SYD03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

SYD03_Bg2 <- rbind(SYD03_Bg2, addPlayer2)

#Round 3, Behind graph using weighted edges
SYD03_Bft <- ftable(SYD03_Bg2$player1, SYD03_Bg2$player2)
SYD03_Bft2 <- as.matrix(SYD03_Bft)
numRows <- nrow(SYD03_Bft2)
numCols <- ncol(SYD03_Bft2)
SYD03_Bft3 <- SYD03_Bft2[c(1:numRows) , c(2:numCols)]
SYD03_BTable <- graph.adjacency(SYD03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(SYD03_BTable, vertex.label = V(SYD03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
SYD03_B.clusterCoef <- transitivity(SYD03_BTable, type="global") #cluster coefficient
SYD03_B.degreeCent <- centralization.degree(SYD03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_Bftn <- as.network.matrix(SYD03_Bft)
SYD03_B.netDensity <- network.density(SYD03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_B.entropy <- entropy(SYD03_Bft) #entropy

SYD03_B.netMx <- cbind(SYD03_B.netMx, SYD03_B.clusterCoef, SYD03_B.degreeCent$centralization,
                       SYD03_B.netDensity, SYD03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
SYD03_SFg2 <- data.frame(SYD03_SF)
SYD03_SFg2 <- SYD03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_SFg2$player1
player2vector <- SYD03_SFg2$player2
SYD03_SFg3 <- SYD03_SFg2
SYD03_SFg3$p1inp2vec <- is.element(SYD03_SFg3$player1, player2vector)
SYD03_SFg3$p2inp1vec <- is.element(SYD03_SFg3$player2, player1vector)

addPlayer1 <- SYD03_SFg3[ which(SYD03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_SFg3[ which(SYD03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_SFg2 <- rbind(SYD03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
SYD03_SFft <- ftable(SYD03_SFg2$player1, SYD03_SFg2$player2)
SYD03_SFft2 <- as.matrix(SYD03_SFft)
numRows <- nrow(SYD03_SFft2)
numCols <- ncol(SYD03_SFft2)
SYD03_SFft3 <- SYD03_SFft2[c(2:numRows) , c(2:numCols)]
SYD03_SFTable <- graph.adjacency(SYD03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(SYD03_SFTable, vertex.label = V(SYD03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
SYD03_SF.clusterCoef <- transitivity(SYD03_SFTable, type="global") #cluster coefficient
SYD03_SF.degreeCent <- centralization.degree(SYD03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_SFftn <- as.network.matrix(SYD03_SFft)
SYD03_SF.netDensity <- network.density(SYD03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_SF.entropy <- entropy(SYD03_SFft) #entropy

SYD03_SF.netMx <- cbind(SYD03_SF.netMx, SYD03_SF.clusterCoef, SYD03_SF.degreeCent$centralization,
                        SYD03_SF.netDensity, SYD03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
SYD03_TFg2 <- data.frame(SYD03_TF)
SYD03_TFg2 <- SYD03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_TFg2$player1
player2vector <- SYD03_TFg2$player2
SYD03_TFg3 <- SYD03_TFg2
SYD03_TFg3$p1inp2vec <- is.element(SYD03_TFg3$player1, player2vector)
SYD03_TFg3$p2inp1vec <- is.element(SYD03_TFg3$player2, player1vector)

addPlayer1 <- SYD03_TFg3[ which(SYD03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_TFg3[ which(SYD03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_TFg2 <- rbind(SYD03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
SYD03_TFft <- ftable(SYD03_TFg2$player1, SYD03_TFg2$player2)
SYD03_TFft2 <- as.matrix(SYD03_TFft)
numRows <- nrow(SYD03_TFft2)
numCols <- ncol(SYD03_TFft2)
SYD03_TFft3 <- SYD03_TFft2[c(2:numRows) , c(2:numCols)]
SYD03_TFTable <- graph.adjacency(SYD03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(SYD03_TFTable, vertex.label = V(SYD03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
SYD03_TF.clusterCoef <- transitivity(SYD03_TFTable, type="global") #cluster coefficient
SYD03_TF.degreeCent <- centralization.degree(SYD03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_TFftn <- as.network.matrix(SYD03_TFft)
SYD03_TF.netDensity <- network.density(SYD03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_TF.entropy <- entropy(SYD03_TFft) #entropy

SYD03_TF.netMx <- cbind(SYD03_TF.netMx, SYD03_TF.clusterCoef, SYD03_TF.degreeCent$centralization,
                        SYD03_TF.netDensity, SYD03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************

round = 3
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
SYD03_SAMg2 <- data.frame(SYD03_SAM)
SYD03_SAMg2 <- SYD03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_SAMg2$player1
player2vector <- SYD03_SAMg2$player2
SYD03_SAMg3 <- SYD03_SAMg2
SYD03_SAMg3$p1inp2vec <- is.element(SYD03_SAMg3$player1, player2vector)
SYD03_SAMg3$p2inp1vec <- is.element(SYD03_SAMg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- SYD03_SAMg3[ which(SYD03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

SYD03_SAMg2 <- rbind(SYD03_SAMg2, addPlayer2)

#Round 3, AM Stoppage graph using weighted edges
SYD03_SAMft <- ftable(SYD03_SAMg2$player1, SYD03_SAMg2$player2)
SYD03_SAMft2 <- as.matrix(SYD03_SAMft)
numRows <- nrow(SYD03_SAMft2)
numCols <- ncol(SYD03_SAMft2)
SYD03_SAMft3 <- SYD03_SAMft2[c(1:numRows) , c(2:numCols)]
SYD03_SAMTable <- graph.adjacency(SYD03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(SYD03_SAMTable, vertex.label = V(SYD03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
SYD03_SAM.clusterCoef <- transitivity(SYD03_SAMTable, type="global") #cluster coefficient
SYD03_SAM.degreeCent <- centralization.degree(SYD03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_SAMftn <- as.network.matrix(SYD03_SAMft)
SYD03_SAM.netDensity <- network.density(SYD03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_SAM.entropy <- entropy(SYD03_SAMft) #entropy

SYD03_SAM.netMx <- cbind(SYD03_SAM.netMx, SYD03_SAM.clusterCoef, SYD03_SAM.degreeCent$centralization,
                         SYD03_SAM.netDensity, SYD03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
SYD03_TAMg2 <- data.frame(SYD03_TAM)
SYD03_TAMg2 <- SYD03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_TAMg2$player1
player2vector <- SYD03_TAMg2$player2
SYD03_TAMg3 <- SYD03_TAMg2
SYD03_TAMg3$p1inp2vec <- is.element(SYD03_TAMg3$player1, player2vector)
SYD03_TAMg3$p2inp1vec <- is.element(SYD03_TAMg3$player2, player1vector)

addPlayer1 <- SYD03_TAMg3[ which(SYD03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_TAMg3[ which(SYD03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_TAMg2 <- rbind(SYD03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
SYD03_TAMft <- ftable(SYD03_TAMg2$player1, SYD03_TAMg2$player2)
SYD03_TAMft2 <- as.matrix(SYD03_TAMft)
numRows <- nrow(SYD03_TAMft2)
numCols <- ncol(SYD03_TAMft2)
SYD03_TAMft3 <- SYD03_TAMft2[c(2:numRows) , c(2:numCols)]
SYD03_TAMTable <- graph.adjacency(SYD03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(SYD03_TAMTable, vertex.label = V(SYD03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
SYD03_TAM.clusterCoef <- transitivity(SYD03_TAMTable, type="global") #cluster coefficient
SYD03_TAM.degreeCent <- centralization.degree(SYD03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_TAMftn <- as.network.matrix(SYD03_TAMft)
SYD03_TAM.netDensity <- network.density(SYD03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_TAM.entropy <- entropy(SYD03_TAMft) #entropy

SYD03_TAM.netMx <- cbind(SYD03_TAM.netMx, SYD03_TAM.clusterCoef, SYD03_TAM.degreeCent$centralization,
                         SYD03_TAM.netDensity, SYD03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
SYD03_SDMg2 <- data.frame(SYD03_SDM)
SYD03_SDMg2 <- SYD03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_SDMg2$player1
player2vector <- SYD03_SDMg2$player2
SYD03_SDMg3 <- SYD03_SDMg2
SYD03_SDMg3$p1inp2vec <- is.element(SYD03_SDMg3$player1, player2vector)
SYD03_SDMg3$p2inp1vec <- is.element(SYD03_SDMg3$player2, player1vector)

addPlayer1 <- SYD03_SDMg3[ which(SYD03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_SDMg3[ which(SYD03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_SDMg2 <- rbind(SYD03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
SYD03_SDMft <- ftable(SYD03_SDMg2$player1, SYD03_SDMg2$player2)
SYD03_SDMft2 <- as.matrix(SYD03_SDMft)
numRows <- nrow(SYD03_SDMft2)
numCols <- ncol(SYD03_SDMft2)
SYD03_SDMft3 <- SYD03_SDMft2[c(2:numRows) , c(2:numCols)]
SYD03_SDMTable <- graph.adjacency(SYD03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(SYD03_SDMTable, vertex.label = V(SYD03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
SYD03_SDM.clusterCoef <- transitivity(SYD03_SDMTable, type="global") #cluster coefficient
SYD03_SDM.degreeCent <- centralization.degree(SYD03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_SDMftn <- as.network.matrix(SYD03_SDMft)
SYD03_SDM.netDensity <- network.density(SYD03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_SDM.entropy <- entropy(SYD03_SDMft) #entropy

SYD03_SDM.netMx <- cbind(SYD03_SDM.netMx, SYD03_SDM.clusterCoef, SYD03_SDM.degreeCent$centralization,
                         SYD03_SDM.netDensity, SYD03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
SYD03_TDMg2 <- data.frame(SYD03_TDM)
SYD03_TDMg2 <- SYD03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_TDMg2$player1
player2vector <- SYD03_TDMg2$player2
SYD03_TDMg3 <- SYD03_TDMg2
SYD03_TDMg3$p1inp2vec <- is.element(SYD03_TDMg3$player1, player2vector)
SYD03_TDMg3$p2inp1vec <- is.element(SYD03_TDMg3$player2, player1vector)

addPlayer1 <- SYD03_TDMg3[ which(SYD03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_TDMg3[ which(SYD03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_TDMg2 <- rbind(SYD03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
SYD03_TDMft <- ftable(SYD03_TDMg2$player1, SYD03_TDMg2$player2)
SYD03_TDMft2 <- as.matrix(SYD03_TDMft)
numRows <- nrow(SYD03_TDMft2)
numCols <- ncol(SYD03_TDMft2)
SYD03_TDMft3 <- SYD03_TDMft2[c(2:numRows) , c(2:numCols)]
SYD03_TDMTable <- graph.adjacency(SYD03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(SYD03_TDMTable, vertex.label = V(SYD03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
SYD03_TDM.clusterCoef <- transitivity(SYD03_TDMTable, type="global") #cluster coefficient
SYD03_TDM.degreeCent <- centralization.degree(SYD03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_TDMftn <- as.network.matrix(SYD03_TDMft)
SYD03_TDM.netDensity <- network.density(SYD03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_TDM.entropy <- entropy(SYD03_TDMft) #entropy

SYD03_TDM.netMx <- cbind(SYD03_TDM.netMx, SYD03_TDM.clusterCoef, SYD03_TDM.degreeCent$centralization,
                         SYD03_TDM.netDensity, SYD03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
SYD03_SDg2 <- data.frame(SYD03_SD)
SYD03_SDg2 <- SYD03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_SDg2$player1
player2vector <- SYD03_SDg2$player2
SYD03_SDg3 <- SYD03_SDg2
SYD03_SDg3$p1inp2vec <- is.element(SYD03_SDg3$player1, player2vector)
SYD03_SDg3$p2inp1vec <- is.element(SYD03_SDg3$player2, player1vector)

addPlayer1 <- SYD03_SDg3[ which(SYD03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_SDg3[ which(SYD03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_SDg2 <- rbind(SYD03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
SYD03_SDft <- ftable(SYD03_SDg2$player1, SYD03_SDg2$player2)
SYD03_SDft2 <- as.matrix(SYD03_SDft)
numRows <- nrow(SYD03_SDft2)
numCols <- ncol(SYD03_SDft2)
SYD03_SDft3 <- SYD03_SDft2[c(2:numRows) , c(2:numCols)]
SYD03_SDTable <- graph.adjacency(SYD03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(SYD03_SDTable, vertex.label = V(SYD03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
SYD03_SD.clusterCoef <- transitivity(SYD03_SDTable, type="global") #cluster coefficient
SYD03_SD.degreeCent <- centralization.degree(SYD03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_SDftn <- as.network.matrix(SYD03_SDft)
SYD03_SD.netDensity <- network.density(SYD03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_SD.entropy <- entropy(SYD03_SDft) #entropy

SYD03_SD.netMx <- cbind(SYD03_SD.netMx, SYD03_SD.clusterCoef, SYD03_SD.degreeCent$centralization,
                        SYD03_SD.netDensity, SYD03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************

round = 3
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
SYD03_TDg2 <- data.frame(SYD03_TD)
SYD03_TDg2 <- SYD03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_TDg2$player1
player2vector <- SYD03_TDg2$player2
SYD03_TDg3 <- SYD03_TDg2
SYD03_TDg3$p1inp2vec <- is.element(SYD03_TDg3$player1, player2vector)
SYD03_TDg3$p2inp1vec <- is.element(SYD03_TDg3$player2, player1vector)

addPlayer1 <- SYD03_TDg3[ which(SYD03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_TDg3[ which(SYD03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_TDg2 <- rbind(SYD03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
SYD03_TDft <- ftable(SYD03_TDg2$player1, SYD03_TDg2$player2)
SYD03_TDft2 <- as.matrix(SYD03_TDft)
numRows <- nrow(SYD03_TDft2)
numCols <- ncol(SYD03_TDft2)
SYD03_TDft3 <- SYD03_TDft2[c(2:numRows) , c(2:numCols)]
SYD03_TDTable <- graph.adjacency(SYD03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(SYD03_TDTable, vertex.label = V(SYD03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
SYD03_TD.clusterCoef <- transitivity(SYD03_TDTable, type="global") #cluster coefficient
SYD03_TD.degreeCent <- centralization.degree(SYD03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_TDftn <- as.network.matrix(SYD03_TDft)
SYD03_TD.netDensity <- network.density(SYD03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_TD.entropy <- entropy(SYD03_TDft) #entropy

SYD03_TD.netMx <- cbind(SYD03_TD.netMx, SYD03_TD.clusterCoef, SYD03_TD.degreeCent$centralization,
                        SYD03_TD.netDensity, SYD03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
SYD03_QTg2 <- data.frame(SYD03_QT)
SYD03_QTg2 <- SYD03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD03_QTg2$player1
player2vector <- SYD03_QTg2$player2
SYD03_QTg3 <- SYD03_QTg2
SYD03_QTg3$p1inp2vec <- is.element(SYD03_QTg3$player1, player2vector)
SYD03_QTg3$p2inp1vec <- is.element(SYD03_QTg3$player2, player1vector)

addPlayer1 <- SYD03_QTg3[ which(SYD03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD03_QTg3[ which(SYD03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD03_QTg2 <- rbind(SYD03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
SYD03_QTft <- ftable(SYD03_QTg2$player1, SYD03_QTg2$player2)
SYD03_QTft2 <- as.matrix(SYD03_QTft)
numRows <- nrow(SYD03_QTft2)
numCols <- ncol(SYD03_QTft2)
SYD03_QTft3 <- SYD03_QTft2[c(2:numRows) , c(2:numCols)]
SYD03_QTTable <- graph.adjacency(SYD03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(SYD03_QTTable, vertex.label = V(SYD03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
SYD03_QT.clusterCoef <- transitivity(SYD03_QTTable, type="global") #cluster coefficient
SYD03_QT.degreeCent <- centralization.degree(SYD03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD03_QTftn <- as.network.matrix(SYD03_QTft)
SYD03_QT.netDensity <- network.density(SYD03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD03_QT.entropy <- entropy(SYD03_QTft) #entropy

SYD03_QT.netMx <- cbind(SYD03_QT.netMx, SYD03_QT.clusterCoef, SYD03_QT.degreeCent$centralization,
                        SYD03_QT.netDensity, SYD03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD03_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 3
##

#Round 3, Goal***************************************************************
#NA

round = 3
teamName = "WB"
KIoutcome = "Goal_F"
WB03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
WB03_Gg2 <- data.frame(WB03_G)
WB03_Gg2 <- WB03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_Gg2$player1
player2vector <- WB03_Gg2$player2
WB03_Gg3 <- WB03_Gg2
WB03_Gg3$p1inp2vec <- is.element(WB03_Gg3$player1, player2vector)
WB03_Gg3$p2inp1vec <- is.element(WB03_Gg3$player2, player1vector)

addPlayer1 <- WB03_Gg3[ which(WB03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_Gg3[ which(WB03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_Gg2 <- rbind(WB03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
WB03_Gft <- ftable(WB03_Gg2$player1, WB03_Gg2$player2)
WB03_Gft2 <- as.matrix(WB03_Gft)
numRows <- nrow(WB03_Gft2)
numCols <- ncol(WB03_Gft2)
WB03_Gft3 <- WB03_Gft2[c(2:numRows) , c(2:numCols)]
WB03_GTable <- graph.adjacency(WB03_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(WB03_GTable, vertex.label = V(WB03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
WB03_G.clusterCoef <- transitivity(WB03_GTable, type="global") #cluster coefficient
WB03_G.degreeCent <- centralization.degree(WB03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_Gftn <- as.network.matrix(WB03_Gft)
WB03_G.netDensity <- network.density(WB03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_G.entropy <- entropy(WB03_Gft) #entropy

WB03_G.netMx <- cbind(WB03_G.netMx, WB03_G.clusterCoef, WB03_G.degreeCent$centralization,
                      WB03_G.netDensity, WB03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "WB"
KIoutcome = "Behind_F"
WB03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
WB03_Bg2 <- data.frame(WB03_B)
WB03_Bg2 <- WB03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_Bg2$player1
player2vector <- WB03_Bg2$player2
WB03_Bg3 <- WB03_Bg2
WB03_Bg3$p1inp2vec <- is.element(WB03_Bg3$player1, player2vector)
WB03_Bg3$p2inp1vec <- is.element(WB03_Bg3$player2, player1vector)

addPlayer1 <- WB03_Bg3[ which(WB03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_Bg3[ which(WB03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_Bg2 <- rbind(WB03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
WB03_Bft <- ftable(WB03_Bg2$player1, WB03_Bg2$player2)
WB03_Bft2 <- as.matrix(WB03_Bft)
numRows <- nrow(WB03_Bft2)
numCols <- ncol(WB03_Bft2)
WB03_Bft3 <- WB03_Bft2[c(2:numRows) , c(2:numCols)]
WB03_BTable <- graph.adjacency(WB03_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(WB03_BTable, vertex.label = V(WB03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
WB03_B.clusterCoef <- transitivity(WB03_BTable, type="global") #cluster coefficient
WB03_B.degreeCent <- centralization.degree(WB03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_Bftn <- as.network.matrix(WB03_Bft)
WB03_B.netDensity <- network.density(WB03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_B.entropy <- entropy(WB03_Bft) #entropy

WB03_B.netMx <- cbind(WB03_B.netMx, WB03_B.clusterCoef, WB03_B.degreeCent$centralization,
                      WB03_B.netDensity, WB03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************

round = 3
teamName = "WB"
KIoutcome = "Stoppage_F"
WB03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
WB03_SFg2 <- data.frame(WB03_SF)
WB03_SFg2 <- WB03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_SFg2$player1
player2vector <- WB03_SFg2$player2
WB03_SFg3 <- WB03_SFg2
WB03_SFg3$p1inp2vec <- is.element(WB03_SFg3$player1, player2vector)
WB03_SFg3$p2inp1vec <- is.element(WB03_SFg3$player2, player1vector)

addPlayer1 <- WB03_SFg3[ which(WB03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_SFg3[ which(WB03_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_SFg2 <- rbind(WB03_SFg2, addPlayers)

#Round 3, FWD Stoppage graph using weighted edges
WB03_SFft <- ftable(WB03_SFg2$player1, WB03_SFg2$player2)
WB03_SFft2 <- as.matrix(WB03_SFft)
numRows <- nrow(WB03_SFft2)
numCols <- ncol(WB03_SFft2)
WB03_SFft3 <- WB03_SFft2[c(2:numRows) , c(2:numCols)]
WB03_SFTable <- graph.adjacency(WB03_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(WB03_SFTable, vertex.label = V(WB03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
WB03_SF.clusterCoef <- transitivity(WB03_SFTable, type="global") #cluster coefficient
WB03_SF.degreeCent <- centralization.degree(WB03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_SFftn <- as.network.matrix(WB03_SFft)
WB03_SF.netDensity <- network.density(WB03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_SF.entropy <- entropy(WB03_SFft) #entropy

WB03_SF.netMx <- cbind(WB03_SF.netMx, WB03_SF.clusterCoef, WB03_SF.degreeCent$centralization,
                       WB03_SF.netDensity, WB03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "WB"
KIoutcome = "Turnover_F"
WB03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
WB03_TFg2 <- data.frame(WB03_TF)
WB03_TFg2 <- WB03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_TFg2$player1
player2vector <- WB03_TFg2$player2
WB03_TFg3 <- WB03_TFg2
WB03_TFg3$p1inp2vec <- is.element(WB03_TFg3$player1, player2vector)
WB03_TFg3$p2inp1vec <- is.element(WB03_TFg3$player2, player1vector)

addPlayer1 <- WB03_TFg3[ which(WB03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_TFg3[ which(WB03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_TFg2 <- rbind(WB03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
WB03_TFft <- ftable(WB03_TFg2$player1, WB03_TFg2$player2)
WB03_TFft2 <- as.matrix(WB03_TFft)
numRows <- nrow(WB03_TFft2)
numCols <- ncol(WB03_TFft2)
WB03_TFft3 <- WB03_TFft2[c(2:numRows) , c(2:numCols)]
WB03_TFTable <- graph.adjacency(WB03_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(WB03_TFTable, vertex.label = V(WB03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
WB03_TF.clusterCoef <- transitivity(WB03_TFTable, type="global") #cluster coefficient
WB03_TF.degreeCent <- centralization.degree(WB03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_TFftn <- as.network.matrix(WB03_TFft)
WB03_TF.netDensity <- network.density(WB03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_TF.entropy <- entropy(WB03_TFft) #entropy

WB03_TF.netMx <- cbind(WB03_TF.netMx, WB03_TF.clusterCoef, WB03_TF.degreeCent$centralization,
                       WB03_TF.netDensity, WB03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
WB03_SAMg2 <- data.frame(WB03_SAM)
WB03_SAMg2 <- WB03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_SAMg2$player1
player2vector <- WB03_SAMg2$player2
WB03_SAMg3 <- WB03_SAMg2
WB03_SAMg3$p1inp2vec <- is.element(WB03_SAMg3$player1, player2vector)
WB03_SAMg3$p2inp1vec <- is.element(WB03_SAMg3$player2, player1vector)

addPlayer1 <- WB03_SAMg3[ which(WB03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_SAMg3[ which(WB03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_SAMg2 <- rbind(WB03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
WB03_SAMft <- ftable(WB03_SAMg2$player1, WB03_SAMg2$player2)
WB03_SAMft2 <- as.matrix(WB03_SAMft)
numRows <- nrow(WB03_SAMft2)
numCols <- ncol(WB03_SAMft2)
WB03_SAMft3 <- WB03_SAMft2[c(2:numRows) , c(2:numCols)]
WB03_SAMTable <- graph.adjacency(WB03_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(WB03_SAMTable, vertex.label = V(WB03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
WB03_SAM.clusterCoef <- transitivity(WB03_SAMTable, type="global") #cluster coefficient
WB03_SAM.degreeCent <- centralization.degree(WB03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_SAMftn <- as.network.matrix(WB03_SAMft)
WB03_SAM.netDensity <- network.density(WB03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_SAM.entropy <- entropy(WB03_SAMft) #entropy

WB03_SAM.netMx <- cbind(WB03_SAM.netMx, WB03_SAM.clusterCoef, WB03_SAM.degreeCent$centralization,
                        WB03_SAM.netDensity, WB03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "WB"
KIoutcome = "Turnover_AM"
WB03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
WB03_TAMg2 <- data.frame(WB03_TAM)
WB03_TAMg2 <- WB03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_TAMg2$player1
player2vector <- WB03_TAMg2$player2
WB03_TAMg3 <- WB03_TAMg2
WB03_TAMg3$p1inp2vec <- is.element(WB03_TAMg3$player1, player2vector)
WB03_TAMg3$p2inp1vec <- is.element(WB03_TAMg3$player2, player1vector)

addPlayer1 <- WB03_TAMg3[ which(WB03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_TAMg3[ which(WB03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_TAMg2 <- rbind(WB03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
WB03_TAMft <- ftable(WB03_TAMg2$player1, WB03_TAMg2$player2)
WB03_TAMft2 <- as.matrix(WB03_TAMft)
numRows <- nrow(WB03_TAMft2)
numCols <- ncol(WB03_TAMft2)
WB03_TAMft3 <- WB03_TAMft2[c(2:numRows) , c(2:numCols)]
WB03_TAMTable <- graph.adjacency(WB03_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(WB03_TAMTable, vertex.label = V(WB03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
WB03_TAM.clusterCoef <- transitivity(WB03_TAMTable, type="global") #cluster coefficient
WB03_TAM.degreeCent <- centralization.degree(WB03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_TAMftn <- as.network.matrix(WB03_TAMft)
WB03_TAM.netDensity <- network.density(WB03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_TAM.entropy <- entropy(WB03_TAMft) #entropy

WB03_TAM.netMx <- cbind(WB03_TAM.netMx, WB03_TAM.clusterCoef, WB03_TAM.degreeCent$centralization,
                        WB03_TAM.netDensity, WB03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************

round = 3
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
WB03_SDMg2 <- data.frame(WB03_SDM)
WB03_SDMg2 <- WB03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_SDMg2$player1
player2vector <- WB03_SDMg2$player2
WB03_SDMg3 <- WB03_SDMg2
WB03_SDMg3$p1inp2vec <- is.element(WB03_SDMg3$player1, player2vector)
WB03_SDMg3$p2inp1vec <- is.element(WB03_SDMg3$player2, player1vector)

addPlayer1 <- WB03_SDMg3[ which(WB03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_SDMg3[ which(WB03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_SDMg2 <- rbind(WB03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
WB03_SDMft <- ftable(WB03_SDMg2$player1, WB03_SDMg2$player2)
WB03_SDMft2 <- as.matrix(WB03_SDMft)
numRows <- nrow(WB03_SDMft2)
numCols <- ncol(WB03_SDMft2)
WB03_SDMft3 <- WB03_SDMft2[c(2:numRows) , c(2:numCols)]
WB03_SDMTable <- graph.adjacency(WB03_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(WB03_SDMTable, vertex.label = V(WB03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
WB03_SDM.clusterCoef <- transitivity(WB03_SDMTable, type="global") #cluster coefficient
WB03_SDM.degreeCent <- centralization.degree(WB03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_SDMftn <- as.network.matrix(WB03_SDMft)
WB03_SDM.netDensity <- network.density(WB03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_SDM.entropy <- entropy(WB03_SDMft) #entropy

WB03_SDM.netMx <- cbind(WB03_SDM.netMx, WB03_SDM.clusterCoef, WB03_SDM.degreeCent$centralization,
                        WB03_SDM.netDensity, WB03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "WB"
KIoutcome = "Turnover_DM"
WB03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
WB03_TDMg2 <- data.frame(WB03_TDM)
WB03_TDMg2 <- WB03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_TDMg2$player1
player2vector <- WB03_TDMg2$player2
WB03_TDMg3 <- WB03_TDMg2
WB03_TDMg3$p1inp2vec <- is.element(WB03_TDMg3$player1, player2vector)
WB03_TDMg3$p2inp1vec <- is.element(WB03_TDMg3$player2, player1vector)

addPlayer1 <- WB03_TDMg3[ which(WB03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_TDMg3[ which(WB03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_TDMg2 <- rbind(WB03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
WB03_TDMft <- ftable(WB03_TDMg2$player1, WB03_TDMg2$player2)
WB03_TDMft2 <- as.matrix(WB03_TDMft)
numRows <- nrow(WB03_TDMft2)
numCols <- ncol(WB03_TDMft2)
WB03_TDMft3 <- WB03_TDMft2[c(2:numRows) , c(2:numCols)]
WB03_TDMTable <- graph.adjacency(WB03_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(WB03_TDMTable, vertex.label = V(WB03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
WB03_TDM.clusterCoef <- transitivity(WB03_TDMTable, type="global") #cluster coefficient
WB03_TDM.degreeCent <- centralization.degree(WB03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_TDMftn <- as.network.matrix(WB03_TDMft)
WB03_TDM.netDensity <- network.density(WB03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_TDM.entropy <- entropy(WB03_TDMft) #entropy

WB03_TDM.netMx <- cbind(WB03_TDM.netMx, WB03_TDM.clusterCoef, WB03_TDM.degreeCent$centralization,
                        WB03_TDM.netDensity, WB03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************

round = 3
teamName = "WB"
KIoutcome = "Stoppage_D"
WB03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
WB03_SDg2 <- data.frame(WB03_SD)
WB03_SDg2 <- WB03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_SDg2$player1
player2vector <- WB03_SDg2$player2
WB03_SDg3 <- WB03_SDg2
WB03_SDg3$p1inp2vec <- is.element(WB03_SDg3$player1, player2vector)
WB03_SDg3$p2inp1vec <- is.element(WB03_SDg3$player2, player1vector)

addPlayer1 <- WB03_SDg3[ which(WB03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_SDg3[ which(WB03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_SDg2 <- rbind(WB03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
WB03_SDft <- ftable(WB03_SDg2$player1, WB03_SDg2$player2)
WB03_SDft2 <- as.matrix(WB03_SDft)
numRows <- nrow(WB03_SDft2)
numCols <- ncol(WB03_SDft2)
WB03_SDft3 <- WB03_SDft2[c(2:numRows) , c(2:numCols)]
WB03_SDTable <- graph.adjacency(WB03_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(WB03_SDTable, vertex.label = V(WB03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
WB03_SD.clusterCoef <- transitivity(WB03_SDTable, type="global") #cluster coefficient
WB03_SD.degreeCent <- centralization.degree(WB03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_SDftn <- as.network.matrix(WB03_SDft)
WB03_SD.netDensity <- network.density(WB03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_SD.entropy <- entropy(WB03_SDft) #entropy

WB03_SD.netMx <- cbind(WB03_SD.netMx, WB03_SD.clusterCoef, WB03_SD.degreeCent$centralization,
                       WB03_SD.netDensity, WB03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "WB"
KIoutcome = "Turnover_D"
WB03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
WB03_TDg2 <- data.frame(WB03_TD)
WB03_TDg2 <- WB03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_TDg2$player1
player2vector <- WB03_TDg2$player2
WB03_TDg3 <- WB03_TDg2
WB03_TDg3$p1inp2vec <- is.element(WB03_TDg3$player1, player2vector)
WB03_TDg3$p2inp1vec <- is.element(WB03_TDg3$player2, player1vector)

addPlayer1 <- WB03_TDg3[ which(WB03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_TDg3[ which(WB03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_TDg2 <- rbind(WB03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
WB03_TDft <- ftable(WB03_TDg2$player1, WB03_TDg2$player2)
WB03_TDft2 <- as.matrix(WB03_TDft)
numRows <- nrow(WB03_TDft2)
numCols <- ncol(WB03_TDft2)
WB03_TDft3 <- WB03_TDft2[c(2:numRows) , c(2:numCols)]
WB03_TDTable <- graph.adjacency(WB03_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(WB03_TDTable, vertex.label = V(WB03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
WB03_TD.clusterCoef <- transitivity(WB03_TDTable, type="global") #cluster coefficient
WB03_TD.degreeCent <- centralization.degree(WB03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_TDftn <- as.network.matrix(WB03_TDft)
WB03_TD.netDensity <- network.density(WB03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_TD.entropy <- entropy(WB03_TDft) #entropy

WB03_TD.netMx <- cbind(WB03_TD.netMx, WB03_TD.clusterCoef, WB03_TD.degreeCent$centralization,
                       WB03_TD.netDensity, WB03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
WB03_QTg2 <- data.frame(WB03_QT)
WB03_QTg2 <- WB03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB03_QTg2$player1
player2vector <- WB03_QTg2$player2
WB03_QTg3 <- WB03_QTg2
WB03_QTg3$p1inp2vec <- is.element(WB03_QTg3$player1, player2vector)
WB03_QTg3$p2inp1vec <- is.element(WB03_QTg3$player2, player1vector)

addPlayer1 <- WB03_QTg3[ which(WB03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB03_QTg3[ which(WB03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB03_QTg2 <- rbind(WB03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
WB03_QTft <- ftable(WB03_QTg2$player1, WB03_QTg2$player2)
WB03_QTft2 <- as.matrix(WB03_QTft)
numRows <- nrow(WB03_QTft2)
numCols <- ncol(WB03_QTft2)
WB03_QTft3 <- WB03_QTft2[c(2:numRows) , c(2:numCols)]
WB03_QTTable <- graph.adjacency(WB03_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(WB03_QTTable, vertex.label = V(WB03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
WB03_QT.clusterCoef <- transitivity(WB03_QTTable, type="global") #cluster coefficient
WB03_QT.degreeCent <- centralization.degree(WB03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB03_QTftn <- as.network.matrix(WB03_QTft)
WB03_QT.netDensity <- network.density(WB03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB03_QT.entropy <- entropy(WB03_QTft) #entropy

WB03_QT.netMx <- cbind(WB03_QT.netMx, WB03_QT.clusterCoef, WB03_QT.degreeCent$centralization,
                       WB03_QT.netDensity, WB03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB03_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 3
##

#Round 3, Goal***************************************************************

round = 3
teamName = "WCE"
KIoutcome = "Goal_F"
WCE03_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Goal with weighted edges
WCE03_Gg2 <- data.frame(WCE03_G)
WCE03_Gg2 <- WCE03_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_Gg2$player1
player2vector <- WCE03_Gg2$player2
WCE03_Gg3 <- WCE03_Gg2
WCE03_Gg3$p1inp2vec <- is.element(WCE03_Gg3$player1, player2vector)
WCE03_Gg3$p2inp1vec <- is.element(WCE03_Gg3$player2, player1vector)

addPlayer1 <- WCE03_Gg3[ which(WCE03_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_Gg3[ which(WCE03_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_Gg2 <- rbind(WCE03_Gg2, addPlayers)

#Round 3, Goal graph using weighted edges
WCE03_Gft <- ftable(WCE03_Gg2$player1, WCE03_Gg2$player2)
WCE03_Gft2 <- as.matrix(WCE03_Gft)
numRows <- nrow(WCE03_Gft2)
numCols <- ncol(WCE03_Gft2)
WCE03_Gft3 <- WCE03_Gft2[c(2:numRows) , c(2:numCols)]
WCE03_GTable <- graph.adjacency(WCE03_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Goal graph=weighted
plot.igraph(WCE03_GTable, vertex.label = V(WCE03_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Goal calulation of network metrics
#igraph
WCE03_G.clusterCoef <- transitivity(WCE03_GTable, type="global") #cluster coefficient
WCE03_G.degreeCent <- centralization.degree(WCE03_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_Gftn <- as.network.matrix(WCE03_Gft)
WCE03_G.netDensity <- network.density(WCE03_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_G.entropy <- entropy(WCE03_Gft) #entropy

WCE03_G.netMx <- cbind(WCE03_G.netMx, WCE03_G.clusterCoef, WCE03_G.degreeCent$centralization,
                       WCE03_G.netDensity, WCE03_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_G.netMx) <- varnames

#Round 3, Behind***************************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "Behind_F"
WCE03_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, Behind with weighted edges
WCE03_Bg2 <- data.frame(WCE03_B)
WCE03_Bg2 <- WCE03_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_Bg2$player1
player2vector <- WCE03_Bg2$player2
WCE03_Bg3 <- WCE03_Bg2
WCE03_Bg3$p1inp2vec <- is.element(WCE03_Bg3$player1, player2vector)
WCE03_Bg3$p2inp1vec <- is.element(WCE03_Bg3$player2, player1vector)

addPlayer1 <- WCE03_Bg3[ which(WCE03_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_Bg3[ which(WCE03_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_Bg2 <- rbind(WCE03_Bg2, addPlayers)

#Round 3, Behind graph using weighted edges
WCE03_Bft <- ftable(WCE03_Bg2$player1, WCE03_Bg2$player2)
WCE03_Bft2 <- as.matrix(WCE03_Bft)
numRows <- nrow(WCE03_Bft2)
numCols <- ncol(WCE03_Bft2)
WCE03_Bft3 <- WCE03_Bft2[c(2:numRows) , c(2:numCols)]
WCE03_BTable <- graph.adjacency(WCE03_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 3, Behind graph=weighted
plot.igraph(WCE03_BTable, vertex.label = V(WCE03_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, Behind calulation of network metrics
#igraph
WCE03_B.clusterCoef <- transitivity(WCE03_BTable, type="global") #cluster coefficient
WCE03_B.degreeCent <- centralization.degree(WCE03_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_Bftn <- as.network.matrix(WCE03_Bft)
WCE03_B.netDensity <- network.density(WCE03_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_B.entropy <- entropy(WCE03_Bft) #entropy

WCE03_B.netMx <- cbind(WCE03_B.netMx, WCE03_B.clusterCoef, WCE03_B.degreeCent$centralization,
                       WCE03_B.netDensity, WCE03_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_B.netMx) <- varnames

#Round 3, FWD Stoppage**********************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE03_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Stoppage with weighted edges
WCE03_SFg2 <- data.frame(WCE03_SF)
WCE03_SFg2 <- WCE03_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_SFg2$player1
player2vector <- WCE03_SFg2$player2
WCE03_SFg3 <- WCE03_SFg2
WCE03_SFg3$p1inp2vec <- is.element(WCE03_SFg3$player1, player2vector)
WCE03_SFg3$p2inp1vec <- is.element(WCE03_SFg3$player2, player1vector)

addPlayer1 <- WCE03_SFg3[ which(WCE03_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE03_SFg2 <- rbind(WCE03_SFg2, addPlayer1)

#Round 3, FWD Stoppage graph using weighted edges
WCE03_SFft <- ftable(WCE03_SFg2$player1, WCE03_SFg2$player2)
WCE03_SFft2 <- as.matrix(WCE03_SFft)
numRows <- nrow(WCE03_SFft2)
numCols <- ncol(WCE03_SFft2)
WCE03_SFft3 <- WCE03_SFft2[c(2:numRows) , c(1:numCols)]
WCE03_SFTable <- graph.adjacency(WCE03_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Stoppage graph=weighted
plot.igraph(WCE03_SFTable, vertex.label = V(WCE03_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Stoppage calulation of network metrics
#igraph
WCE03_SF.clusterCoef <- transitivity(WCE03_SFTable, type="global") #cluster coefficient
WCE03_SF.degreeCent <- centralization.degree(WCE03_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_SFftn <- as.network.matrix(WCE03_SFft)
WCE03_SF.netDensity <- network.density(WCE03_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_SF.entropy <- entropy(WCE03_SFft) #entropy

WCE03_SF.netMx <- cbind(WCE03_SF.netMx, WCE03_SF.clusterCoef, WCE03_SF.degreeCent$centralization,
                        WCE03_SF.netDensity, WCE03_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_SF.netMx) <- varnames

#Round 3, FWD Turnover**********************************************************

round = 3
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE03_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, FWD Turnover with weighted edges
WCE03_TFg2 <- data.frame(WCE03_TF)
WCE03_TFg2 <- WCE03_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_TFg2$player1
player2vector <- WCE03_TFg2$player2
WCE03_TFg3 <- WCE03_TFg2
WCE03_TFg3$p1inp2vec <- is.element(WCE03_TFg3$player1, player2vector)
WCE03_TFg3$p2inp1vec <- is.element(WCE03_TFg3$player2, player1vector)

addPlayer1 <- WCE03_TFg3[ which(WCE03_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_TFg3[ which(WCE03_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_TFg2 <- rbind(WCE03_TFg2, addPlayers)

#Round 3, FWD Turnover graph using weighted edges
WCE03_TFft <- ftable(WCE03_TFg2$player1, WCE03_TFg2$player2)
WCE03_TFft2 <- as.matrix(WCE03_TFft)
numRows <- nrow(WCE03_TFft2)
numCols <- ncol(WCE03_TFft2)
WCE03_TFft3 <- WCE03_TFft2[c(2:numRows) , c(2:numCols)]
WCE03_TFTable <- graph.adjacency(WCE03_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, FWD Turnover graph=weighted
plot.igraph(WCE03_TFTable, vertex.label = V(WCE03_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, FWD Turnover calulation of network metrics
#igraph
WCE03_TF.clusterCoef <- transitivity(WCE03_TFTable, type="global") #cluster coefficient
WCE03_TF.degreeCent <- centralization.degree(WCE03_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_TFftn <- as.network.matrix(WCE03_TFft)
WCE03_TF.netDensity <- network.density(WCE03_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_TF.entropy <- entropy(WCE03_TFft) #entropy

WCE03_TF.netMx <- cbind(WCE03_TF.netMx, WCE03_TF.clusterCoef, WCE03_TF.degreeCent$centralization,
                        WCE03_TF.netDensity, WCE03_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_TF.netMx) <- varnames

#Round 3, AM Stoppage**********************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE03_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Stoppage with weighted edges
WCE03_SAMg2 <- data.frame(WCE03_SAM)
WCE03_SAMg2 <- WCE03_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_SAMg2$player1
player2vector <- WCE03_SAMg2$player2
WCE03_SAMg3 <- WCE03_SAMg2
WCE03_SAMg3$p1inp2vec <- is.element(WCE03_SAMg3$player1, player2vector)
WCE03_SAMg3$p2inp1vec <- is.element(WCE03_SAMg3$player2, player1vector)

addPlayer1 <- WCE03_SAMg3[ which(WCE03_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_SAMg3[ which(WCE03_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_SAMg2 <- rbind(WCE03_SAMg2, addPlayers)

#Round 3, AM Stoppage graph using weighted edges
WCE03_SAMft <- ftable(WCE03_SAMg2$player1, WCE03_SAMg2$player2)
WCE03_SAMft2 <- as.matrix(WCE03_SAMft)
numRows <- nrow(WCE03_SAMft2)
numCols <- ncol(WCE03_SAMft2)
WCE03_SAMft3 <- WCE03_SAMft2[c(2:numRows) , c(2:numCols)]
WCE03_SAMTable <- graph.adjacency(WCE03_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Stoppage graph=weighted
plot.igraph(WCE03_SAMTable, vertex.label = V(WCE03_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Stoppage calulation of network metrics
#igraph
WCE03_SAM.clusterCoef <- transitivity(WCE03_SAMTable, type="global") #cluster coefficient
WCE03_SAM.degreeCent <- centralization.degree(WCE03_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_SAMftn <- as.network.matrix(WCE03_SAMft)
WCE03_SAM.netDensity <- network.density(WCE03_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_SAM.entropy <- entropy(WCE03_SAMft) #entropy

WCE03_SAM.netMx <- cbind(WCE03_SAM.netMx, WCE03_SAM.clusterCoef, WCE03_SAM.degreeCent$centralization,
                         WCE03_SAM.netDensity, WCE03_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_SAM.netMx) <- varnames

#Round 3, AM Turnover**********************************************************

round = 3
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE03_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, AM Turnover with weighted edges
WCE03_TAMg2 <- data.frame(WCE03_TAM)
WCE03_TAMg2 <- WCE03_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_TAMg2$player1
player2vector <- WCE03_TAMg2$player2
WCE03_TAMg3 <- WCE03_TAMg2
WCE03_TAMg3$p1inp2vec <- is.element(WCE03_TAMg3$player1, player2vector)
WCE03_TAMg3$p2inp1vec <- is.element(WCE03_TAMg3$player2, player1vector)

addPlayer1 <- WCE03_TAMg3[ which(WCE03_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_TAMg3[ which(WCE03_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_TAMg2 <- rbind(WCE03_TAMg2, addPlayers)

#Round 3, AM Turnover graph using weighted edges
WCE03_TAMft <- ftable(WCE03_TAMg2$player1, WCE03_TAMg2$player2)
WCE03_TAMft2 <- as.matrix(WCE03_TAMft)
numRows <- nrow(WCE03_TAMft2)
numCols <- ncol(WCE03_TAMft2)
WCE03_TAMft3 <- WCE03_TAMft2[c(2:numRows) , c(2:numCols)]
WCE03_TAMTable <- graph.adjacency(WCE03_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, AM Turnover graph=weighted
plot.igraph(WCE03_TAMTable, vertex.label = V(WCE03_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, AM Turnover calulation of network metrics
#igraph
WCE03_TAM.clusterCoef <- transitivity(WCE03_TAMTable, type="global") #cluster coefficient
WCE03_TAM.degreeCent <- centralization.degree(WCE03_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_TAMftn <- as.network.matrix(WCE03_TAMft)
WCE03_TAM.netDensity <- network.density(WCE03_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_TAM.entropy <- entropy(WCE03_TAMft) #entropy

WCE03_TAM.netMx <- cbind(WCE03_TAM.netMx, WCE03_TAM.clusterCoef, WCE03_TAM.degreeCent$centralization,
                         WCE03_TAM.netDensity, WCE03_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_TAM.netMx) <- varnames

#Round 3, DM Stoppage**********************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE03_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Stoppage with weighted edges
WCE03_SDMg2 <- data.frame(WCE03_SDM)
WCE03_SDMg2 <- WCE03_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_SDMg2$player1
player2vector <- WCE03_SDMg2$player2
WCE03_SDMg3 <- WCE03_SDMg2
WCE03_SDMg3$p1inp2vec <- is.element(WCE03_SDMg3$player1, player2vector)
WCE03_SDMg3$p2inp1vec <- is.element(WCE03_SDMg3$player2, player1vector)

addPlayer1 <- WCE03_SDMg3[ which(WCE03_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_SDMg3[ which(WCE03_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_SDMg2 <- rbind(WCE03_SDMg2, addPlayers)

#Round 3, DM Stoppage graph using weighted edges
WCE03_SDMft <- ftable(WCE03_SDMg2$player1, WCE03_SDMg2$player2)
WCE03_SDMft2 <- as.matrix(WCE03_SDMft)
numRows <- nrow(WCE03_SDMft2)
numCols <- ncol(WCE03_SDMft2)
WCE03_SDMft3 <- WCE03_SDMft2[c(2:numRows) , c(2:numCols)]
WCE03_SDMTable <- graph.adjacency(WCE03_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Stoppage graph=weighted
plot.igraph(WCE03_SDMTable, vertex.label = V(WCE03_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Stoppage calulation of network metrics
#igraph
WCE03_SDM.clusterCoef <- transitivity(WCE03_SDMTable, type="global") #cluster coefficient
WCE03_SDM.degreeCent <- centralization.degree(WCE03_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_SDMftn <- as.network.matrix(WCE03_SDMft)
WCE03_SDM.netDensity <- network.density(WCE03_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_SDM.entropy <- entropy(WCE03_SDMft) #entropy

WCE03_SDM.netMx <- cbind(WCE03_SDM.netMx, WCE03_SDM.clusterCoef, WCE03_SDM.degreeCent$centralization,
                         WCE03_SDM.netDensity, WCE03_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_SDM.netMx) <- varnames

#Round 3, DM Turnover**********************************************************

round = 3
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE03_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, DM Turnover with weighted edges
WCE03_TDMg2 <- data.frame(WCE03_TDM)
WCE03_TDMg2 <- WCE03_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_TDMg2$player1
player2vector <- WCE03_TDMg2$player2
WCE03_TDMg3 <- WCE03_TDMg2
WCE03_TDMg3$p1inp2vec <- is.element(WCE03_TDMg3$player1, player2vector)
WCE03_TDMg3$p2inp1vec <- is.element(WCE03_TDMg3$player2, player1vector)

addPlayer1 <- WCE03_TDMg3[ which(WCE03_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_TDMg3[ which(WCE03_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_TDMg2 <- rbind(WCE03_TDMg2, addPlayers)

#Round 3, DM Turnover graph using weighted edges
WCE03_TDMft <- ftable(WCE03_TDMg2$player1, WCE03_TDMg2$player2)
WCE03_TDMft2 <- as.matrix(WCE03_TDMft)
numRows <- nrow(WCE03_TDMft2)
numCols <- ncol(WCE03_TDMft2)
WCE03_TDMft3 <- WCE03_TDMft2[c(2:numRows) , c(2:numCols)]
WCE03_TDMTable <- graph.adjacency(WCE03_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 3, DM Turnover graph=weighted
plot.igraph(WCE03_TDMTable, vertex.label = V(WCE03_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, DM Turnover calulation of network metrics
#igraph
WCE03_TDM.clusterCoef <- transitivity(WCE03_TDMTable, type="global") #cluster coefficient
WCE03_TDM.degreeCent <- centralization.degree(WCE03_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_TDMftn <- as.network.matrix(WCE03_TDMft)
WCE03_TDM.netDensity <- network.density(WCE03_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_TDM.entropy <- entropy(WCE03_TDMft) #entropy

WCE03_TDM.netMx <- cbind(WCE03_TDM.netMx, WCE03_TDM.clusterCoef, WCE03_TDM.degreeCent$centralization,
                         WCE03_TDM.netDensity, WCE03_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_TDM.netMx) <- varnames

#Round 3, D Stoppage**********************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE03_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Stoppage with weighted edges
WCE03_SDg2 <- data.frame(WCE03_SD)
WCE03_SDg2 <- WCE03_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_SDg2$player1
player2vector <- WCE03_SDg2$player2
WCE03_SDg3 <- WCE03_SDg2
WCE03_SDg3$p1inp2vec <- is.element(WCE03_SDg3$player1, player2vector)
WCE03_SDg3$p2inp1vec <- is.element(WCE03_SDg3$player2, player1vector)

addPlayer1 <- WCE03_SDg3[ which(WCE03_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_SDg3[ which(WCE03_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_SDg2 <- rbind(WCE03_SDg2, addPlayers)

#Round 3, D Stoppage graph using weighted edges
WCE03_SDft <- ftable(WCE03_SDg2$player1, WCE03_SDg2$player2)
WCE03_SDft2 <- as.matrix(WCE03_SDft)
numRows <- nrow(WCE03_SDft2)
numCols <- ncol(WCE03_SDft2)
WCE03_SDft3 <- WCE03_SDft2[c(2:numRows) , c(2:numCols)]
WCE03_SDTable <- graph.adjacency(WCE03_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Stoppage graph=weighted
plot.igraph(WCE03_SDTable, vertex.label = V(WCE03_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Stoppage calulation of network metrics
#igraph
WCE03_SD.clusterCoef <- transitivity(WCE03_SDTable, type="global") #cluster coefficient
WCE03_SD.degreeCent <- centralization.degree(WCE03_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_SDftn <- as.network.matrix(WCE03_SDft)
WCE03_SD.netDensity <- network.density(WCE03_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_SD.entropy <- entropy(WCE03_SDft) #entropy

WCE03_SD.netMx <- cbind(WCE03_SD.netMx, WCE03_SD.clusterCoef, WCE03_SD.degreeCent$centralization,
                        WCE03_SD.netDensity, WCE03_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_SD.netMx) <- varnames

#Round 3, D Turnover**********************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE03_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, D Turnover with weighted edges
WCE03_TDg2 <- data.frame(WCE03_TD)
WCE03_TDg2 <- WCE03_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_TDg2$player1
player2vector <- WCE03_TDg2$player2
WCE03_TDg3 <- WCE03_TDg2
WCE03_TDg3$p1inp2vec <- is.element(WCE03_TDg3$player1, player2vector)
WCE03_TDg3$p2inp1vec <- is.element(WCE03_TDg3$player2, player1vector)

addPlayer1 <- WCE03_TDg3[ which(WCE03_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_TDg3[ which(WCE03_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_TDg2 <- rbind(WCE03_TDg2, addPlayers)

#Round 3, D Turnover graph using weighted edges
WCE03_TDft <- ftable(WCE03_TDg2$player1, WCE03_TDg2$player2)
WCE03_TDft2 <- as.matrix(WCE03_TDft)
numRows <- nrow(WCE03_TDft2)
numCols <- ncol(WCE03_TDft2)
WCE03_TDft3 <- WCE03_TDft2[c(2:numRows) , c(2:numCols)]
WCE03_TDTable <- graph.adjacency(WCE03_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, D Turnover graph=weighted
plot.igraph(WCE03_TDTable, vertex.label = V(WCE03_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, D Turnover calulation of network metrics
#igraph
WCE03_TD.clusterCoef <- transitivity(WCE03_TDTable, type="global") #cluster coefficient
WCE03_TD.degreeCent <- centralization.degree(WCE03_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_TDftn <- as.network.matrix(WCE03_TDft)
WCE03_TD.netDensity <- network.density(WCE03_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_TD.entropy <- entropy(WCE03_TDft) #entropy

WCE03_TD.netMx <- cbind(WCE03_TD.netMx, WCE03_TD.clusterCoef, WCE03_TD.degreeCent$centralization,
                        WCE03_TD.netDensity, WCE03_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_TD.netMx) <- varnames

#Round 3, End of Qtr**********************************************************
#NA

round = 3
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE03_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 3, End of Qtr with weighted edges
WCE03_QTg2 <- data.frame(WCE03_QT)
WCE03_QTg2 <- WCE03_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE03_QTg2$player1
player2vector <- WCE03_QTg2$player2
WCE03_QTg3 <- WCE03_QTg2
WCE03_QTg3$p1inp2vec <- is.element(WCE03_QTg3$player1, player2vector)
WCE03_QTg3$p2inp1vec <- is.element(WCE03_QTg3$player2, player1vector)

addPlayer1 <- WCE03_QTg3[ which(WCE03_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE03_QTg3[ which(WCE03_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE03_QTg2 <- rbind(WCE03_QTg2, addPlayers)

#Round 3, End of Qtr graph using weighted edges
WCE03_QTft <- ftable(WCE03_QTg2$player1, WCE03_QTg2$player2)
WCE03_QTft2 <- as.matrix(WCE03_QTft)
numRows <- nrow(WCE03_QTft2)
numCols <- ncol(WCE03_QTft2)
WCE03_QTft3 <- WCE03_QTft2[c(2:numRows) , c(2:numCols)]
WCE03_QTTable <- graph.adjacency(WCE03_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 3, End of Qtr graph=weighted
plot.igraph(WCE03_QTTable, vertex.label = V(WCE03_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE03_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 3, End of Qtr calulation of network metrics
#igraph
WCE03_QT.clusterCoef <- transitivity(WCE03_QTTable, type="global") #cluster coefficient
WCE03_QT.degreeCent <- centralization.degree(WCE03_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE03_QTftn <- as.network.matrix(WCE03_QTft)
WCE03_QT.netDensity <- network.density(WCE03_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE03_QT.entropy <- entropy(WCE03_QTft) #entropy

WCE03_QT.netMx <- cbind(WCE03_QT.netMx, WCE03_QT.clusterCoef, WCE03_QT.degreeCent$centralization,
                        WCE03_QT.netDensity, WCE03_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE03_QT.netMx) <- varnames

