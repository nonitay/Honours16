#####
#09-28-16- Real data 10
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************

round = 10
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
ADEL10_Gg2 <- data.frame(ADEL10_G)
ADEL10_Gg2 <- ADEL10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_Gg2$player1
player2vector <- ADEL10_Gg2$player2
ADEL10_Gg3 <- ADEL10_Gg2
ADEL10_Gg3$p1inp2vec <- is.element(ADEL10_Gg3$player1, player2vector)
ADEL10_Gg3$p2inp1vec <- is.element(ADEL10_Gg3$player2, player1vector)

addPlayer1 <- ADEL10_Gg3[ which(ADEL10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_Gg3[ which(ADEL10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_Gg2 <- rbind(ADEL10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
ADEL10_Gft <- ftable(ADEL10_Gg2$player1, ADEL10_Gg2$player2)
ADEL10_Gft2 <- as.matrix(ADEL10_Gft)
numRows <- nrow(ADEL10_Gft2)
numCols <- ncol(ADEL10_Gft2)
ADEL10_Gft3 <- ADEL10_Gft2[c(2:numRows) , c(2:numCols)]
ADEL10_GTable <- graph.adjacency(ADEL10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(ADEL10_GTable, vertex.label = V(ADEL10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
ADEL10_G.clusterCoef <- transitivity(ADEL10_GTable, type="global") #cluster coefficient
ADEL10_G.degreeCent <- centralization.degree(ADEL10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_Gftn <- as.network.matrix(ADEL10_Gft)
ADEL10_G.netDensity <- network.density(ADEL10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_G.entropy <- entropy(ADEL10_Gft) #entropy

ADEL10_G.netMx <- cbind(ADEL10_G.netMx, ADEL10_G.clusterCoef, ADEL10_G.degreeCent$centralization,
                        ADEL10_G.netDensity, ADEL10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
ADEL10_Bg2 <- data.frame(ADEL10_B)
ADEL10_Bg2 <- ADEL10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_Bg2$player1
player2vector <- ADEL10_Bg2$player2
ADEL10_Bg3 <- ADEL10_Bg2
ADEL10_Bg3$p1inp2vec <- is.element(ADEL10_Bg3$player1, player2vector)
ADEL10_Bg3$p2inp1vec <- is.element(ADEL10_Bg3$player2, player1vector)

addPlayer1 <- ADEL10_Bg3[ which(ADEL10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_Bg3[ which(ADEL10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_Bg2 <- rbind(ADEL10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
ADEL10_Bft <- ftable(ADEL10_Bg2$player1, ADEL10_Bg2$player2)
ADEL10_Bft2 <- as.matrix(ADEL10_Bft)
numRows <- nrow(ADEL10_Bft2)
numCols <- ncol(ADEL10_Bft2)
ADEL10_Bft3 <- ADEL10_Bft2[c(2:numRows) , c(2:numCols)]
ADEL10_BTable <- graph.adjacency(ADEL10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(ADEL10_BTable, vertex.label = V(ADEL10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
ADEL10_B.clusterCoef <- transitivity(ADEL10_BTable, type="global") #cluster coefficient
ADEL10_B.degreeCent <- centralization.degree(ADEL10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_Bftn <- as.network.matrix(ADEL10_Bft)
ADEL10_B.netDensity <- network.density(ADEL10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_B.entropy <- entropy(ADEL10_Bft) #entropy

ADEL10_B.netMx <- cbind(ADEL10_B.netMx, ADEL10_B.clusterCoef, ADEL10_B.degreeCent$centralization,
                        ADEL10_B.netDensity, ADEL10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
ADEL10_SFg2 <- data.frame(ADEL10_SF)
ADEL10_SFg2 <- ADEL10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_SFg2$player1
player2vector <- ADEL10_SFg2$player2
ADEL10_SFg3 <- ADEL10_SFg2
ADEL10_SFg3$p1inp2vec <- is.element(ADEL10_SFg3$player1, player2vector)
ADEL10_SFg3$p2inp1vec <- is.element(ADEL10_SFg3$player2, player1vector)

addPlayer1 <- ADEL10_SFg3[ which(ADEL10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_SFg3[ which(ADEL10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_SFg2 <- rbind(ADEL10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
ADEL10_SFft <- ftable(ADEL10_SFg2$player1, ADEL10_SFg2$player2)
ADEL10_SFft2 <- as.matrix(ADEL10_SFft)
numRows <- nrow(ADEL10_SFft2)
numCols <- ncol(ADEL10_SFft2)
ADEL10_SFft3 <- ADEL10_SFft2[c(2:numRows) , c(2:numCols)]
ADEL10_SFTable <- graph.adjacency(ADEL10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(ADEL10_SFTable, vertex.label = V(ADEL10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
ADEL10_SF.clusterCoef <- transitivity(ADEL10_SFTable, type="global") #cluster coefficient
ADEL10_SF.degreeCent <- centralization.degree(ADEL10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_SFftn <- as.network.matrix(ADEL10_SFft)
ADEL10_SF.netDensity <- network.density(ADEL10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_SF.entropy <- entropy(ADEL10_SFft) #entropy

ADEL10_SF.netMx <- cbind(ADEL10_SF.netMx, ADEL10_SF.clusterCoef, ADEL10_SF.degreeCent$centralization,
                         ADEL10_SF.netDensity, ADEL10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
ADEL10_TFg2 <- data.frame(ADEL10_TF)
ADEL10_TFg2 <- ADEL10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_TFg2$player1
player2vector <- ADEL10_TFg2$player2
ADEL10_TFg3 <- ADEL10_TFg2
ADEL10_TFg3$p1inp2vec <- is.element(ADEL10_TFg3$player1, player2vector)
ADEL10_TFg3$p2inp1vec <- is.element(ADEL10_TFg3$player2, player1vector)

addPlayer1 <- ADEL10_TFg3[ which(ADEL10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_TFg3[ which(ADEL10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_TFg2 <- rbind(ADEL10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
ADEL10_TFft <- ftable(ADEL10_TFg2$player1, ADEL10_TFg2$player2)
ADEL10_TFft2 <- as.matrix(ADEL10_TFft)
numRows <- nrow(ADEL10_TFft2)
numCols <- ncol(ADEL10_TFft2)
ADEL10_TFft3 <- ADEL10_TFft2[c(2:numRows) , c(2:numCols)]
ADEL10_TFTable <- graph.adjacency(ADEL10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(ADEL10_TFTable, vertex.label = V(ADEL10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
ADEL10_TF.clusterCoef <- transitivity(ADEL10_TFTable, type="global") #cluster coefficient
ADEL10_TF.degreeCent <- centralization.degree(ADEL10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_TFftn <- as.network.matrix(ADEL10_TFft)
ADEL10_TF.netDensity <- network.density(ADEL10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_TF.entropy <- entropy(ADEL10_TFft) #entropy

ADEL10_TF.netMx <- cbind(ADEL10_TF.netMx, ADEL10_TF.clusterCoef, ADEL10_TF.degreeCent$centralization,
                         ADEL10_TF.netDensity, ADEL10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
ADEL10_SAMg2 <- data.frame(ADEL10_SAM)
ADEL10_SAMg2 <- ADEL10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_SAMg2$player1
player2vector <- ADEL10_SAMg2$player2
ADEL10_SAMg3 <- ADEL10_SAMg2
ADEL10_SAMg3$p1inp2vec <- is.element(ADEL10_SAMg3$player1, player2vector)
ADEL10_SAMg3$p2inp1vec <- is.element(ADEL10_SAMg3$player2, player1vector)

addPlayer1 <- ADEL10_SAMg3[ which(ADEL10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_SAMg3[ which(ADEL10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_SAMg2 <- rbind(ADEL10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
ADEL10_SAMft <- ftable(ADEL10_SAMg2$player1, ADEL10_SAMg2$player2)
ADEL10_SAMft2 <- as.matrix(ADEL10_SAMft)
numRows <- nrow(ADEL10_SAMft2)
numCols <- ncol(ADEL10_SAMft2)
ADEL10_SAMft3 <- ADEL10_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL10_SAMTable <- graph.adjacency(ADEL10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(ADEL10_SAMTable, vertex.label = V(ADEL10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
ADEL10_SAM.clusterCoef <- transitivity(ADEL10_SAMTable, type="global") #cluster coefficient
ADEL10_SAM.degreeCent <- centralization.degree(ADEL10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_SAMftn <- as.network.matrix(ADEL10_SAMft)
ADEL10_SAM.netDensity <- network.density(ADEL10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_SAM.entropy <- entropy(ADEL10_SAMft) #entropy

ADEL10_SAM.netMx <- cbind(ADEL10_SAM.netMx, ADEL10_SAM.clusterCoef, ADEL10_SAM.degreeCent$centralization,
                          ADEL10_SAM.netDensity, ADEL10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
ADEL10_TAMg2 <- data.frame(ADEL10_TAM)
ADEL10_TAMg2 <- ADEL10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_TAMg2$player1
player2vector <- ADEL10_TAMg2$player2
ADEL10_TAMg3 <- ADEL10_TAMg2
ADEL10_TAMg3$p1inp2vec <- is.element(ADEL10_TAMg3$player1, player2vector)
ADEL10_TAMg3$p2inp1vec <- is.element(ADEL10_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL10_TAMg3[ which(ADEL10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL10_TAMg2 <- rbind(ADEL10_TAMg2, addPlayer2)

#ROUND 10, AM Turnover graph using weighted edges
ADEL10_TAMft <- ftable(ADEL10_TAMg2$player1, ADEL10_TAMg2$player2)
ADEL10_TAMft2 <- as.matrix(ADEL10_TAMft)
numRows <- nrow(ADEL10_TAMft2)
numCols <- ncol(ADEL10_TAMft2)
ADEL10_TAMft3 <- ADEL10_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL10_TAMTable <- graph.adjacency(ADEL10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(ADEL10_TAMTable, vertex.label = V(ADEL10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
ADEL10_TAM.clusterCoef <- transitivity(ADEL10_TAMTable, type="global") #cluster coefficient
ADEL10_TAM.degreeCent <- centralization.degree(ADEL10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_TAMftn <- as.network.matrix(ADEL10_TAMft)
ADEL10_TAM.netDensity <- network.density(ADEL10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_TAM.entropy <- entropy(ADEL10_TAMft) #entropy

ADEL10_TAM.netMx <- cbind(ADEL10_TAM.netMx, ADEL10_TAM.clusterCoef, ADEL10_TAM.degreeCent$centralization,
                          ADEL10_TAM.netDensity, ADEL10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
ADEL10_SDMg2 <- data.frame(ADEL10_SDM)
ADEL10_SDMg2 <- ADEL10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_SDMg2$player1
player2vector <- ADEL10_SDMg2$player2
ADEL10_SDMg3 <- ADEL10_SDMg2
ADEL10_SDMg3$p1inp2vec <- is.element(ADEL10_SDMg3$player1, player2vector)
ADEL10_SDMg3$p2inp1vec <- is.element(ADEL10_SDMg3$player2, player1vector)

addPlayer1 <- ADEL10_SDMg3[ which(ADEL10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_SDMg3[ which(ADEL10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_SDMg2 <- rbind(ADEL10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
ADEL10_SDMft <- ftable(ADEL10_SDMg2$player1, ADEL10_SDMg2$player2)
ADEL10_SDMft2 <- as.matrix(ADEL10_SDMft)
numRows <- nrow(ADEL10_SDMft2)
numCols <- ncol(ADEL10_SDMft2)
ADEL10_SDMft3 <- ADEL10_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL10_SDMTable <- graph.adjacency(ADEL10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(ADEL10_SDMTable, vertex.label = V(ADEL10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
ADEL10_SDM.clusterCoef <- transitivity(ADEL10_SDMTable, type="global") #cluster coefficient
ADEL10_SDM.degreeCent <- centralization.degree(ADEL10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_SDMftn <- as.network.matrix(ADEL10_SDMft)
ADEL10_SDM.netDensity <- network.density(ADEL10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_SDM.entropy <- entropy(ADEL10_SDMft) #entropy

ADEL10_SDM.netMx <- cbind(ADEL10_SDM.netMx, ADEL10_SDM.clusterCoef, ADEL10_SDM.degreeCent$centralization,
                          ADEL10_SDM.netDensity, ADEL10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
ADEL10_TDMg2 <- data.frame(ADEL10_TDM)
ADEL10_TDMg2 <- ADEL10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_TDMg2$player1
player2vector <- ADEL10_TDMg2$player2
ADEL10_TDMg3 <- ADEL10_TDMg2
ADEL10_TDMg3$p1inp2vec <- is.element(ADEL10_TDMg3$player1, player2vector)
ADEL10_TDMg3$p2inp1vec <- is.element(ADEL10_TDMg3$player2, player1vector)

addPlayer1 <- ADEL10_TDMg3[ which(ADEL10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_TDMg3[ which(ADEL10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_TDMg2 <- rbind(ADEL10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
ADEL10_TDMft <- ftable(ADEL10_TDMg2$player1, ADEL10_TDMg2$player2)
ADEL10_TDMft2 <- as.matrix(ADEL10_TDMft)
numRows <- nrow(ADEL10_TDMft2)
numCols <- ncol(ADEL10_TDMft2)
ADEL10_TDMft3 <- ADEL10_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL10_TDMTable <- graph.adjacency(ADEL10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(ADEL10_TDMTable, vertex.label = V(ADEL10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
ADEL10_TDM.clusterCoef <- transitivity(ADEL10_TDMTable, type="global") #cluster coefficient
ADEL10_TDM.degreeCent <- centralization.degree(ADEL10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_TDMftn <- as.network.matrix(ADEL10_TDMft)
ADEL10_TDM.netDensity <- network.density(ADEL10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_TDM.entropy <- entropy(ADEL10_TDMft) #entropy

ADEL10_TDM.netMx <- cbind(ADEL10_TDM.netMx, ADEL10_TDM.clusterCoef, ADEL10_TDM.degreeCent$centralization,
                          ADEL10_TDM.netDensity, ADEL10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
ADEL10_SDg2 <- data.frame(ADEL10_SD)
ADEL10_SDg2 <- ADEL10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_SDg2$player1
player2vector <- ADEL10_SDg2$player2
ADEL10_SDg3 <- ADEL10_SDg2
ADEL10_SDg3$p1inp2vec <- is.element(ADEL10_SDg3$player1, player2vector)
ADEL10_SDg3$p2inp1vec <- is.element(ADEL10_SDg3$player2, player1vector)

addPlayer1 <- ADEL10_SDg3[ which(ADEL10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_SDg3[ which(ADEL10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_SDg2 <- rbind(ADEL10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
ADEL10_SDft <- ftable(ADEL10_SDg2$player1, ADEL10_SDg2$player2)
ADEL10_SDft2 <- as.matrix(ADEL10_SDft)
numRows <- nrow(ADEL10_SDft2)
numCols <- ncol(ADEL10_SDft2)
ADEL10_SDft3 <- ADEL10_SDft2[c(2:numRows) , c(2:numCols)]
ADEL10_SDTable <- graph.adjacency(ADEL10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(ADEL10_SDTable, vertex.label = V(ADEL10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
ADEL10_SD.clusterCoef <- transitivity(ADEL10_SDTable, type="global") #cluster coefficient
ADEL10_SD.degreeCent <- centralization.degree(ADEL10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_SDftn <- as.network.matrix(ADEL10_SDft)
ADEL10_SD.netDensity <- network.density(ADEL10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_SD.entropy <- entropy(ADEL10_SDft) #entropy

ADEL10_SD.netMx <- cbind(ADEL10_SD.netMx, ADEL10_SD.clusterCoef, ADEL10_SD.degreeCent$centralization,
                         ADEL10_SD.netDensity, ADEL10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
ADEL10_TDg2 <- data.frame(ADEL10_TD)
ADEL10_TDg2 <- ADEL10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_TDg2$player1
player2vector <- ADEL10_TDg2$player2
ADEL10_TDg3 <- ADEL10_TDg2
ADEL10_TDg3$p1inp2vec <- is.element(ADEL10_TDg3$player1, player2vector)
ADEL10_TDg3$p2inp1vec <- is.element(ADEL10_TDg3$player2, player1vector)

addPlayer1 <- ADEL10_TDg3[ which(ADEL10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_TDg3[ which(ADEL10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_TDg2 <- rbind(ADEL10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
ADEL10_TDft <- ftable(ADEL10_TDg2$player1, ADEL10_TDg2$player2)
ADEL10_TDft2 <- as.matrix(ADEL10_TDft)
numRows <- nrow(ADEL10_TDft2)
numCols <- ncol(ADEL10_TDft2)
ADEL10_TDft3 <- ADEL10_TDft2[c(2:numRows) , c(2:numCols)]
ADEL10_TDTable <- graph.adjacency(ADEL10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(ADEL10_TDTable, vertex.label = V(ADEL10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
ADEL10_TD.clusterCoef <- transitivity(ADEL10_TDTable, type="global") #cluster coefficient
ADEL10_TD.degreeCent <- centralization.degree(ADEL10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_TDftn <- as.network.matrix(ADEL10_TDft)
ADEL10_TD.netDensity <- network.density(ADEL10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_TD.entropy <- entropy(ADEL10_TDft) #entropy

ADEL10_TD.netMx <- cbind(ADEL10_TD.netMx, ADEL10_TD.clusterCoef, ADEL10_TD.degreeCent$centralization,
                         ADEL10_TD.netDensity, ADEL10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
ADEL10_QTg2 <- data.frame(ADEL10_QT)
ADEL10_QTg2 <- ADEL10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL10_QTg2$player1
player2vector <- ADEL10_QTg2$player2
ADEL10_QTg3 <- ADEL10_QTg2
ADEL10_QTg3$p1inp2vec <- is.element(ADEL10_QTg3$player1, player2vector)
ADEL10_QTg3$p2inp1vec <- is.element(ADEL10_QTg3$player2, player1vector)

addPlayer1 <- ADEL10_QTg3[ which(ADEL10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL10_QTg3[ which(ADEL10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL10_QTg2 <- rbind(ADEL10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
ADEL10_QTft <- ftable(ADEL10_QTg2$player1, ADEL10_QTg2$player2)
ADEL10_QTft2 <- as.matrix(ADEL10_QTft)
numRows <- nrow(ADEL10_QTft2)
numCols <- ncol(ADEL10_QTft2)
ADEL10_QTft3 <- ADEL10_QTft2[c(2:numRows) , c(2:numCols)]
ADEL10_QTTable <- graph.adjacency(ADEL10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(ADEL10_QTTable, vertex.label = V(ADEL10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
ADEL10_QT.clusterCoef <- transitivity(ADEL10_QTTable, type="global") #cluster coefficient
ADEL10_QT.degreeCent <- centralization.degree(ADEL10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL10_QTftn <- as.network.matrix(ADEL10_QTft)
ADEL10_QT.netDensity <- network.density(ADEL10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL10_QT.entropy <- entropy(ADEL10_QTft) #entropy

ADEL10_QT.netMx <- cbind(ADEL10_QT.netMx, ADEL10_QT.clusterCoef, ADEL10_QT.degreeCent$centralization,
                         ADEL10_QT.netDensity, ADEL10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL10_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************

round = 10
teamName = "BL"
KIoutcome = "Goal_F"
BL10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
BL10_Gg2 <- data.frame(BL10_G)
BL10_Gg2 <- BL10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_Gg2$player1
player2vector <- BL10_Gg2$player2
BL10_Gg3 <- BL10_Gg2
BL10_Gg3$p1inp2vec <- is.element(BL10_Gg3$player1, player2vector)
BL10_Gg3$p2inp1vec <- is.element(BL10_Gg3$player2, player1vector)

addPlayer1 <- BL10_Gg3[ which(BL10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL10_Gg3[ which(BL10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_Gg2 <- rbind(BL10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
BL10_Gft <- ftable(BL10_Gg2$player1, BL10_Gg2$player2)
BL10_Gft2 <- as.matrix(BL10_Gft)
numRows <- nrow(BL10_Gft2)
numCols <- ncol(BL10_Gft2)
BL10_Gft3 <- BL10_Gft2[c(2:numRows) , c(2:numCols)]
BL10_GTable <- graph.adjacency(BL10_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(BL10_GTable, vertex.label = V(BL10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
BL10_G.clusterCoef <- transitivity(BL10_GTable, type="global") #cluster coefficient
BL10_G.degreeCent <- centralization.degree(BL10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_Gftn <- as.network.matrix(BL10_Gft)
BL10_G.netDensity <- network.density(BL10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_G.entropy <- entropy(BL10_Gft) #entropy

BL10_G.netMx <- cbind(BL10_G.netMx, BL10_G.clusterCoef, BL10_G.degreeCent$centralization,
                      BL10_G.netDensity, BL10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "BL"
KIoutcome = "Behind_F"
BL10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
BL10_Bg2 <- data.frame(BL10_B)
BL10_Bg2 <- BL10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_Bg2$player1
player2vector <- BL10_Bg2$player2
BL10_Bg3 <- BL10_Bg2
BL10_Bg3$p1inp2vec <- is.element(BL10_Bg3$player1, player2vector)
BL10_Bg3$p2inp1vec <- is.element(BL10_Bg3$player2, player1vector)

addPlayer1 <- BL10_Bg3[ which(BL10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_Bg3[ which(BL10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_Bg2 <- rbind(BL10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
BL10_Bft <- ftable(BL10_Bg2$player1, BL10_Bg2$player2)
BL10_Bft2 <- as.matrix(BL10_Bft)
numRows <- nrow(BL10_Bft2)
numCols <- ncol(BL10_Bft2)
BL10_Bft3 <- BL10_Bft2[c(2:numRows) , c(2:numCols)]
BL10_BTable <- graph.adjacency(BL10_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(BL10_BTable, vertex.label = V(BL10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
BL10_B.clusterCoef <- transitivity(BL10_BTable, type="global") #cluster coefficient
BL10_B.degreeCent <- centralization.degree(BL10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_Bftn <- as.network.matrix(BL10_Bft)
BL10_B.netDensity <- network.density(BL10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_B.entropy <- entropy(BL10_Bft) #entropy

BL10_B.netMx <- cbind(BL10_B.netMx, BL10_B.clusterCoef, BL10_B.degreeCent$centralization,
                      BL10_B.netDensity, BL10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "BL"
KIoutcome = "Stoppage_F"
BL10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
BL10_SFg2 <- data.frame(BL10_SF)
BL10_SFg2 <- BL10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_SFg2$player1
player2vector <- BL10_SFg2$player2
BL10_SFg3 <- BL10_SFg2
BL10_SFg3$p1inp2vec <- is.element(BL10_SFg3$player1, player2vector)
BL10_SFg3$p2inp1vec <- is.element(BL10_SFg3$player2, player1vector)

addPlayer1 <- BL10_SFg3[ which(BL10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_SFg3[ which(BL10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_SFg2 <- rbind(BL10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
BL10_SFft <- ftable(BL10_SFg2$player1, BL10_SFg2$player2)
BL10_SFft2 <- as.matrix(BL10_SFft)
numRows <- nrow(BL10_SFft2)
numCols <- ncol(BL10_SFft2)
BL10_SFft3 <- BL10_SFft2[c(2:numRows) , c(2:numCols)]
BL10_SFTable <- graph.adjacency(BL10_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(BL10_SFTable, vertex.label = V(BL10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
BL10_SF.clusterCoef <- transitivity(BL10_SFTable, type="global") #cluster coefficient
BL10_SF.degreeCent <- centralization.degree(BL10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_SFftn <- as.network.matrix(BL10_SFft)
BL10_SF.netDensity <- network.density(BL10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_SF.entropy <- entropy(BL10_SFft) #entropy

BL10_SF.netMx <- cbind(BL10_SF.netMx, BL10_SF.clusterCoef, BL10_SF.degreeCent$centralization,
                       BL10_SF.netDensity, BL10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "BL"
KIoutcome = "Turnover_F"
BL10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
BL10_TFg2 <- data.frame(BL10_TF)
BL10_TFg2 <- BL10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_TFg2$player1
player2vector <- BL10_TFg2$player2
BL10_TFg3 <- BL10_TFg2
BL10_TFg3$p1inp2vec <- is.element(BL10_TFg3$player1, player2vector)
BL10_TFg3$p2inp1vec <- is.element(BL10_TFg3$player2, player1vector)

addPlayer1 <- BL10_TFg3[ which(BL10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_TFg3[ which(BL10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_TFg2 <- rbind(BL10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
BL10_TFft <- ftable(BL10_TFg2$player1, BL10_TFg2$player2)
BL10_TFft2 <- as.matrix(BL10_TFft)
numRows <- nrow(BL10_TFft2)
numCols <- ncol(BL10_TFft2)
BL10_TFft3 <- BL10_TFft2[c(2:numRows) , c(2:numCols)]
BL10_TFTable <- graph.adjacency(BL10_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(BL10_TFTable, vertex.label = V(BL10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
BL10_TF.clusterCoef <- transitivity(BL10_TFTable, type="global") #cluster coefficient
BL10_TF.degreeCent <- centralization.degree(BL10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_TFftn <- as.network.matrix(BL10_TFft)
BL10_TF.netDensity <- network.density(BL10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_TF.entropy <- entropy(BL10_TFft) #entropy

BL10_TF.netMx <- cbind(BL10_TF.netMx, BL10_TF.clusterCoef, BL10_TF.degreeCent$centralization,
                       BL10_TF.netDensity, BL10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************

round = 10
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
BL10_SAMg2 <- data.frame(BL10_SAM)
BL10_SAMg2 <- BL10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_SAMg2$player1
player2vector <- BL10_SAMg2$player2
BL10_SAMg3 <- BL10_SAMg2
BL10_SAMg3$p1inp2vec <- is.element(BL10_SAMg3$player1, player2vector)
BL10_SAMg3$p2inp1vec <- is.element(BL10_SAMg3$player2, player1vector)

addPlayer1 <- BL10_SAMg3[ which(BL10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_SAMg3[ which(BL10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_SAMg2 <- rbind(BL10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
BL10_SAMft <- ftable(BL10_SAMg2$player1, BL10_SAMg2$player2)
BL10_SAMft2 <- as.matrix(BL10_SAMft)
numRows <- nrow(BL10_SAMft2)
numCols <- ncol(BL10_SAMft2)
BL10_SAMft3 <- BL10_SAMft2[c(2:numRows) , c(2:numCols)]
BL10_SAMTable <- graph.adjacency(BL10_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(BL10_SAMTable, vertex.label = V(BL10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
BL10_SAM.clusterCoef <- transitivity(BL10_SAMTable, type="global") #cluster coefficient
BL10_SAM.degreeCent <- centralization.degree(BL10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_SAMftn <- as.network.matrix(BL10_SAMft)
BL10_SAM.netDensity <- network.density(BL10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_SAM.entropy <- entropy(BL10_SAMft) #entropy

BL10_SAM.netMx <- cbind(BL10_SAM.netMx, BL10_SAM.clusterCoef, BL10_SAM.degreeCent$centralization,
                        BL10_SAM.netDensity, BL10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "BL"
KIoutcome = "Turnover_AM"
BL10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
BL10_TAMg2 <- data.frame(BL10_TAM)
BL10_TAMg2 <- BL10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_TAMg2$player1
player2vector <- BL10_TAMg2$player2
BL10_TAMg3 <- BL10_TAMg2
BL10_TAMg3$p1inp2vec <- is.element(BL10_TAMg3$player1, player2vector)
BL10_TAMg3$p2inp1vec <- is.element(BL10_TAMg3$player2, player1vector)

addPlayer1 <- BL10_TAMg3[ which(BL10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_TAMg3[ which(BL10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_TAMg2 <- rbind(BL10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
BL10_TAMft <- ftable(BL10_TAMg2$player1, BL10_TAMg2$player2)
BL10_TAMft2 <- as.matrix(BL10_TAMft)
numRows <- nrow(BL10_TAMft2)
numCols <- ncol(BL10_TAMft2)
BL10_TAMft3 <- BL10_TAMft2[c(2:numRows) , c(2:numCols)]
BL10_TAMTable <- graph.adjacency(BL10_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(BL10_TAMTable, vertex.label = V(BL10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
BL10_TAM.clusterCoef <- transitivity(BL10_TAMTable, type="global") #cluster coefficient
BL10_TAM.degreeCent <- centralization.degree(BL10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_TAMftn <- as.network.matrix(BL10_TAMft)
BL10_TAM.netDensity <- network.density(BL10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_TAM.entropy <- entropy(BL10_TAMft) #entropy

BL10_TAM.netMx <- cbind(BL10_TAM.netMx, BL10_TAM.clusterCoef, BL10_TAM.degreeCent$centralization,
                        BL10_TAM.netDensity, BL10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
BL10_SDMg2 <- data.frame(BL10_SDM)
BL10_SDMg2 <- BL10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_SDMg2$player1
player2vector <- BL10_SDMg2$player2
BL10_SDMg3 <- BL10_SDMg2
BL10_SDMg3$p1inp2vec <- is.element(BL10_SDMg3$player1, player2vector)
BL10_SDMg3$p2inp1vec <- is.element(BL10_SDMg3$player2, player1vector)

addPlayer1 <- BL10_SDMg3[ which(BL10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_SDMg3[ which(BL10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_SDMg2 <- rbind(BL10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
BL10_SDMft <- ftable(BL10_SDMg2$player1, BL10_SDMg2$player2)
BL10_SDMft2 <- as.matrix(BL10_SDMft)
numRows <- nrow(BL10_SDMft2)
numCols <- ncol(BL10_SDMft2)
BL10_SDMft3 <- BL10_SDMft2[c(2:numRows) , c(2:numCols)]
BL10_SDMTable <- graph.adjacency(BL10_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(BL10_SDMTable, vertex.label = V(BL10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
BL10_SDM.clusterCoef <- transitivity(BL10_SDMTable, type="global") #cluster coefficient
BL10_SDM.degreeCent <- centralization.degree(BL10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_SDMftn <- as.network.matrix(BL10_SDMft)
BL10_SDM.netDensity <- network.density(BL10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_SDM.entropy <- entropy(BL10_SDMft) #entropy

BL10_SDM.netMx <- cbind(BL10_SDM.netMx, BL10_SDM.clusterCoef, BL10_SDM.degreeCent$centralization,
                        BL10_SDM.netDensity, BL10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "BL"
KIoutcome = "Turnover_DM"
BL10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
BL10_TDMg2 <- data.frame(BL10_TDM)
BL10_TDMg2 <- BL10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_TDMg2$player1
player2vector <- BL10_TDMg2$player2
BL10_TDMg3 <- BL10_TDMg2
BL10_TDMg3$p1inp2vec <- is.element(BL10_TDMg3$player1, player2vector)
BL10_TDMg3$p2inp1vec <- is.element(BL10_TDMg3$player2, player1vector)

addPlayer1 <- BL10_TDMg3[ which(BL10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_TDMg3[ which(BL10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_TDMg2 <- rbind(BL10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
BL10_TDMft <- ftable(BL10_TDMg2$player1, BL10_TDMg2$player2)
BL10_TDMft2 <- as.matrix(BL10_TDMft)
numRows <- nrow(BL10_TDMft2)
numCols <- ncol(BL10_TDMft2)
BL10_TDMft3 <- BL10_TDMft2[c(2:numRows) , c(2:numCols)]
BL10_TDMTable <- graph.adjacency(BL10_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(BL10_TDMTable, vertex.label = V(BL10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
BL10_TDM.clusterCoef <- transitivity(BL10_TDMTable, type="global") #cluster coefficient
BL10_TDM.degreeCent <- centralization.degree(BL10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_TDMftn <- as.network.matrix(BL10_TDMft)
BL10_TDM.netDensity <- network.density(BL10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_TDM.entropy <- entropy(BL10_TDMft) #entropy

BL10_TDM.netMx <- cbind(BL10_TDM.netMx, BL10_TDM.clusterCoef, BL10_TDM.degreeCent$centralization,
                        BL10_TDM.netDensity, BL10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "BL"
KIoutcome = "Stoppage_D"
BL10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
BL10_SDg2 <- data.frame(BL10_SD)
BL10_SDg2 <- BL10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_SDg2$player1
player2vector <- BL10_SDg2$player2
BL10_SDg3 <- BL10_SDg2
BL10_SDg3$p1inp2vec <- is.element(BL10_SDg3$player1, player2vector)
BL10_SDg3$p2inp1vec <- is.element(BL10_SDg3$player2, player1vector)

addPlayer1 <- BL10_SDg3[ which(BL10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_SDg3[ which(BL10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_SDg2 <- rbind(BL10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
BL10_SDft <- ftable(BL10_SDg2$player1, BL10_SDg2$player2)
BL10_SDft2 <- as.matrix(BL10_SDft)
numRows <- nrow(BL10_SDft2)
numCols <- ncol(BL10_SDft2)
BL10_SDft3 <- BL10_SDft2[c(2:numRows) , c(2:numCols)]
BL10_SDTable <- graph.adjacency(BL10_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(BL10_SDTable, vertex.label = V(BL10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
BL10_SD.clusterCoef <- transitivity(BL10_SDTable, type="global") #cluster coefficient
BL10_SD.degreeCent <- centralization.degree(BL10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_SDftn <- as.network.matrix(BL10_SDft)
BL10_SD.netDensity <- network.density(BL10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_SD.entropy <- entropy(BL10_SDft) #entropy

BL10_SD.netMx <- cbind(BL10_SD.netMx, BL10_SD.clusterCoef, BL10_SD.degreeCent$centralization,
                       BL10_SD.netDensity, BL10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "BL"
KIoutcome = "Turnover_D"
BL10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
BL10_TDg2 <- data.frame(BL10_TD)
BL10_TDg2 <- BL10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_TDg2$player1
player2vector <- BL10_TDg2$player2
BL10_TDg3 <- BL10_TDg2
BL10_TDg3$p1inp2vec <- is.element(BL10_TDg3$player1, player2vector)
BL10_TDg3$p2inp1vec <- is.element(BL10_TDg3$player2, player1vector)

addPlayer1 <- BL10_TDg3[ which(BL10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_TDg3[ which(BL10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_TDg2 <- rbind(BL10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
BL10_TDft <- ftable(BL10_TDg2$player1, BL10_TDg2$player2)
BL10_TDft2 <- as.matrix(BL10_TDft)
numRows <- nrow(BL10_TDft2)
numCols <- ncol(BL10_TDft2)
BL10_TDft3 <- BL10_TDft2[c(2:numRows) , c(2:numCols)]
BL10_TDTable <- graph.adjacency(BL10_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(BL10_TDTable, vertex.label = V(BL10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
BL10_TD.clusterCoef <- transitivity(BL10_TDTable, type="global") #cluster coefficient
BL10_TD.degreeCent <- centralization.degree(BL10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_TDftn <- as.network.matrix(BL10_TDft)
BL10_TD.netDensity <- network.density(BL10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_TD.entropy <- entropy(BL10_TDft) #entropy

BL10_TD.netMx <- cbind(BL10_TD.netMx, BL10_TD.clusterCoef, BL10_TD.degreeCent$centralization,
                       BL10_TD.netDensity, BL10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
BL10_QTg2 <- data.frame(BL10_QT)
BL10_QTg2 <- BL10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL10_QTg2$player1
player2vector <- BL10_QTg2$player2
BL10_QTg3 <- BL10_QTg2
BL10_QTg3$p1inp2vec <- is.element(BL10_QTg3$player1, player2vector)
BL10_QTg3$p2inp1vec <- is.element(BL10_QTg3$player2, player1vector)

addPlayer1 <- BL10_QTg3[ which(BL10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL10_QTg3[ which(BL10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL10_QTg2 <- rbind(BL10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
BL10_QTft <- ftable(BL10_QTg2$player1, BL10_QTg2$player2)
BL10_QTft2 <- as.matrix(BL10_QTft)
numRows <- nrow(BL10_QTft2)
numCols <- ncol(BL10_QTft2)
BL10_QTft3 <- BL10_QTft2[c(2:numRows) , c(2:numCols)]
BL10_QTTable <- graph.adjacency(BL10_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(BL10_QTTable, vertex.label = V(BL10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
BL10_QT.clusterCoef <- transitivity(BL10_QTTable, type="global") #cluster coefficient
BL10_QT.degreeCent <- centralization.degree(BL10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL10_QTftn <- as.network.matrix(BL10_QTft)
BL10_QT.netDensity <- network.density(BL10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL10_QT.entropy <- entropy(BL10_QTft) #entropy

BL10_QT.netMx <- cbind(BL10_QT.netMx, BL10_QT.clusterCoef, BL10_QT.degreeCent$centralization,
                       BL10_QT.netDensity, BL10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL10_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "Goal_F"
CARL10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
CARL10_Gg2 <- data.frame(CARL10_G)
CARL10_Gg2 <- CARL10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_Gg2$player1
player2vector <- CARL10_Gg2$player2
CARL10_Gg3 <- CARL10_Gg2
CARL10_Gg3$p1inp2vec <- is.element(CARL10_Gg3$player1, player2vector)
CARL10_Gg3$p2inp1vec <- is.element(CARL10_Gg3$player2, player1vector)

addPlayer1 <- CARL10_Gg3[ which(CARL10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_Gg3[ which(CARL10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_Gg2 <- rbind(CARL10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
CARL10_Gft <- ftable(CARL10_Gg2$player1, CARL10_Gg2$player2)
CARL10_Gft2 <- as.matrix(CARL10_Gft)
numRows <- nrow(CARL10_Gft2)
numCols <- ncol(CARL10_Gft2)
CARL10_Gft3 <- CARL10_Gft2[c(2:numRows) , c(2:numCols)]
CARL10_GTable <- graph.adjacency(CARL10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(CARL10_GTable, vertex.label = V(CARL10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
CARL10_G.clusterCoef <- transitivity(CARL10_GTable, type="global") #cluster coefficient
CARL10_G.degreeCent <- centralization.degree(CARL10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_Gftn <- as.network.matrix(CARL10_Gft)
CARL10_G.netDensity <- network.density(CARL10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_G.entropy <- entropy(CARL10_Gft) #entropy

CARL10_G.netMx <- cbind(CARL10_G.netMx, CARL10_G.clusterCoef, CARL10_G.degreeCent$centralization,
                        CARL10_G.netDensity, CARL10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "Behind_F"
CARL10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
CARL10_Bg2 <- data.frame(CARL10_B)
CARL10_Bg2 <- CARL10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_Bg2$player1
player2vector <- CARL10_Bg2$player2
CARL10_Bg3 <- CARL10_Bg2
CARL10_Bg3$p1inp2vec <- is.element(CARL10_Bg3$player1, player2vector)
CARL10_Bg3$p2inp1vec <- is.element(CARL10_Bg3$player2, player1vector)

addPlayer1 <- CARL10_Bg3[ which(CARL10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_Bg3[ which(CARL10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_Bg2 <- rbind(CARL10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
CARL10_Bft <- ftable(CARL10_Bg2$player1, CARL10_Bg2$player2)
CARL10_Bft2 <- as.matrix(CARL10_Bft)
numRows <- nrow(CARL10_Bft2)
numCols <- ncol(CARL10_Bft2)
CARL10_Bft3 <- CARL10_Bft2[c(2:numRows) , c(2:numCols)]
CARL10_BTable <- graph.adjacency(CARL10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(CARL10_BTable, vertex.label = V(CARL10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
CARL10_B.clusterCoef <- transitivity(CARL10_BTable, type="global") #cluster coefficient
CARL10_B.degreeCent <- centralization.degree(CARL10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_Bftn <- as.network.matrix(CARL10_Bft)
CARL10_B.netDensity <- network.density(CARL10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_B.entropy <- entropy(CARL10_Bft) #entropy

CARL10_B.netMx <- cbind(CARL10_B.netMx, CARL10_B.clusterCoef, CARL10_B.degreeCent$centralization,
                        CARL10_B.netDensity, CARL10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
CARL10_SFg2 <- data.frame(CARL10_SF)
CARL10_SFg2 <- CARL10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_SFg2$player1
player2vector <- CARL10_SFg2$player2
CARL10_SFg3 <- CARL10_SFg2
CARL10_SFg3$p1inp2vec <- is.element(CARL10_SFg3$player1, player2vector)
CARL10_SFg3$p2inp1vec <- is.element(CARL10_SFg3$player2, player1vector)

addPlayer1 <- CARL10_SFg3[ which(CARL10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_SFg3[ which(CARL10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_SFg2 <- rbind(CARL10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
CARL10_SFft <- ftable(CARL10_SFg2$player1, CARL10_SFg2$player2)
CARL10_SFft2 <- as.matrix(CARL10_SFft)
numRows <- nrow(CARL10_SFft2)
numCols <- ncol(CARL10_SFft2)
CARL10_SFft3 <- CARL10_SFft2[c(2:numRows) , c(2:numCols)]
CARL10_SFTable <- graph.adjacency(CARL10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(CARL10_SFTable, vertex.label = V(CARL10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
CARL10_SF.clusterCoef <- transitivity(CARL10_SFTable, type="global") #cluster coefficient
CARL10_SF.degreeCent <- centralization.degree(CARL10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_SFftn <- as.network.matrix(CARL10_SFft)
CARL10_SF.netDensity <- network.density(CARL10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_SF.entropy <- entropy(CARL10_SFft) #entropy

CARL10_SF.netMx <- cbind(CARL10_SF.netMx, CARL10_SF.clusterCoef, CARL10_SF.degreeCent$centralization,
                         CARL10_SF.netDensity, CARL10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
CARL10_TFg2 <- data.frame(CARL10_TF)
CARL10_TFg2 <- CARL10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_TFg2$player1
player2vector <- CARL10_TFg2$player2
CARL10_TFg3 <- CARL10_TFg2
CARL10_TFg3$p1inp2vec <- is.element(CARL10_TFg3$player1, player2vector)
CARL10_TFg3$p2inp1vec <- is.element(CARL10_TFg3$player2, player1vector)

addPlayer1 <- CARL10_TFg3[ which(CARL10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL10_TFg3[ which(CARL10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_TFg2 <- rbind(CARL10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
CARL10_TFft <- ftable(CARL10_TFg2$player1, CARL10_TFg2$player2)
CARL10_TFft2 <- as.matrix(CARL10_TFft)
numRows <- nrow(CARL10_TFft2)
numCols <- ncol(CARL10_TFft2)
CARL10_TFft3 <- CARL10_TFft2[c(2:numRows) , c(2:numCols)]
CARL10_TFTable <- graph.adjacency(CARL10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(CARL10_TFTable, vertex.label = V(CARL10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
CARL10_TF.clusterCoef <- transitivity(CARL10_TFTable, type="global") #cluster coefficient
CARL10_TF.degreeCent <- centralization.degree(CARL10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_TFftn <- as.network.matrix(CARL10_TFft)
CARL10_TF.netDensity <- network.density(CARL10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_TF.entropy <- entropy(CARL10_TFft) #entropy

CARL10_TF.netMx <- cbind(CARL10_TF.netMx, CARL10_TF.clusterCoef, CARL10_TF.degreeCent$centralization,
                         CARL10_TF.netDensity, CARL10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************

round = 10
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
CARL10_SAMg2 <- data.frame(CARL10_SAM)
CARL10_SAMg2 <- CARL10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_SAMg2$player1
player2vector <- CARL10_SAMg2$player2
CARL10_SAMg3 <- CARL10_SAMg2
CARL10_SAMg3$p1inp2vec <- is.element(CARL10_SAMg3$player1, player2vector)
CARL10_SAMg3$p2inp1vec <- is.element(CARL10_SAMg3$player2, player1vector)

addPlayer1 <- CARL10_SAMg3[ which(CARL10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL10_SAMg3[ which(CARL10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_SAMg2 <- rbind(CARL10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
CARL10_SAMft <- ftable(CARL10_SAMg2$player1, CARL10_SAMg2$player2)
CARL10_SAMft2 <- as.matrix(CARL10_SAMft)
numRows <- nrow(CARL10_SAMft2)
numCols <- ncol(CARL10_SAMft2)
CARL10_SAMft3 <- CARL10_SAMft2[c(2:numRows) , c(2:numCols)]
CARL10_SAMTable <- graph.adjacency(CARL10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(CARL10_SAMTable, vertex.label = V(CARL10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
CARL10_SAM.clusterCoef <- transitivity(CARL10_SAMTable, type="global") #cluster coefficient
CARL10_SAM.degreeCent <- centralization.degree(CARL10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_SAMftn <- as.network.matrix(CARL10_SAMft)
CARL10_SAM.netDensity <- network.density(CARL10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_SAM.entropy <- entropy(CARL10_SAMft) #entropy

CARL10_SAM.netMx <- cbind(CARL10_SAM.netMx, CARL10_SAM.clusterCoef, CARL10_SAM.degreeCent$centralization,
                          CARL10_SAM.netDensity, CARL10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
CARL10_TAMg2 <- data.frame(CARL10_TAM)
CARL10_TAMg2 <- CARL10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_TAMg2$player1
player2vector <- CARL10_TAMg2$player2
CARL10_TAMg3 <- CARL10_TAMg2
CARL10_TAMg3$p1inp2vec <- is.element(CARL10_TAMg3$player1, player2vector)
CARL10_TAMg3$p2inp1vec <- is.element(CARL10_TAMg3$player2, player1vector)

addPlayer1 <- CARL10_TAMg3[ which(CARL10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_TAMg3[ which(CARL10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_TAMg2 <- rbind(CARL10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
CARL10_TAMft <- ftable(CARL10_TAMg2$player1, CARL10_TAMg2$player2)
CARL10_TAMft2 <- as.matrix(CARL10_TAMft)
numRows <- nrow(CARL10_TAMft2)
numCols <- ncol(CARL10_TAMft2)
CARL10_TAMft3 <- CARL10_TAMft2[c(2:numRows) , c(2:numCols)]
CARL10_TAMTable <- graph.adjacency(CARL10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(CARL10_TAMTable, vertex.label = V(CARL10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
CARL10_TAM.clusterCoef <- transitivity(CARL10_TAMTable, type="global") #cluster coefficient
CARL10_TAM.degreeCent <- centralization.degree(CARL10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_TAMftn <- as.network.matrix(CARL10_TAMft)
CARL10_TAM.netDensity <- network.density(CARL10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_TAM.entropy <- entropy(CARL10_TAMft) #entropy

CARL10_TAM.netMx <- cbind(CARL10_TAM.netMx, CARL10_TAM.clusterCoef, CARL10_TAM.degreeCent$centralization,
                          CARL10_TAM.netDensity, CARL10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
CARL10_SDMg2 <- data.frame(CARL10_SDM)
CARL10_SDMg2 <- CARL10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_SDMg2$player1
player2vector <- CARL10_SDMg2$player2
CARL10_SDMg3 <- CARL10_SDMg2
CARL10_SDMg3$p1inp2vec <- is.element(CARL10_SDMg3$player1, player2vector)
CARL10_SDMg3$p2inp1vec <- is.element(CARL10_SDMg3$player2, player1vector)

addPlayer1 <- CARL10_SDMg3[ which(CARL10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL10_SDMg3[ which(CARL10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_SDMg2 <- rbind(CARL10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
CARL10_SDMft <- ftable(CARL10_SDMg2$player1, CARL10_SDMg2$player2)
CARL10_SDMft2 <- as.matrix(CARL10_SDMft)
numRows <- nrow(CARL10_SDMft2)
numCols <- ncol(CARL10_SDMft2)
CARL10_SDMft3 <- CARL10_SDMft2[c(2:numRows) , c(2:numCols)]
CARL10_SDMTable <- graph.adjacency(CARL10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(CARL10_SDMTable, vertex.label = V(CARL10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
CARL10_SDM.clusterCoef <- transitivity(CARL10_SDMTable, type="global") #cluster coefficient
CARL10_SDM.degreeCent <- centralization.degree(CARL10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_SDMftn <- as.network.matrix(CARL10_SDMft)
CARL10_SDM.netDensity <- network.density(CARL10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_SDM.entropy <- entropy(CARL10_SDMft) #entropy

CARL10_SDM.netMx <- cbind(CARL10_SDM.netMx, CARL10_SDM.clusterCoef, CARL10_SDM.degreeCent$centralization,
                          CARL10_SDM.netDensity, CARL10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
CARL10_TDMg2 <- data.frame(CARL10_TDM)
CARL10_TDMg2 <- CARL10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_TDMg2$player1
player2vector <- CARL10_TDMg2$player2
CARL10_TDMg3 <- CARL10_TDMg2
CARL10_TDMg3$p1inp2vec <- is.element(CARL10_TDMg3$player1, player2vector)
CARL10_TDMg3$p2inp1vec <- is.element(CARL10_TDMg3$player2, player1vector)

addPlayer1 <- CARL10_TDMg3[ which(CARL10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_TDMg3[ which(CARL10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_TDMg2 <- rbind(CARL10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
CARL10_TDMft <- ftable(CARL10_TDMg2$player1, CARL10_TDMg2$player2)
CARL10_TDMft2 <- as.matrix(CARL10_TDMft)
numRows <- nrow(CARL10_TDMft2)
numCols <- ncol(CARL10_TDMft2)
CARL10_TDMft3 <- CARL10_TDMft2[c(2:numRows) , c(2:numCols)]
CARL10_TDMTable <- graph.adjacency(CARL10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(CARL10_TDMTable, vertex.label = V(CARL10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
CARL10_TDM.clusterCoef <- transitivity(CARL10_TDMTable, type="global") #cluster coefficient
CARL10_TDM.degreeCent <- centralization.degree(CARL10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_TDMftn <- as.network.matrix(CARL10_TDMft)
CARL10_TDM.netDensity <- network.density(CARL10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_TDM.entropy <- entropy(CARL10_TDMft) #entropy

CARL10_TDM.netMx <- cbind(CARL10_TDM.netMx, CARL10_TDM.clusterCoef, CARL10_TDM.degreeCent$centralization,
                          CARL10_TDM.netDensity, CARL10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
CARL10_SDg2 <- data.frame(CARL10_SD)
CARL10_SDg2 <- CARL10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_SDg2$player1
player2vector <- CARL10_SDg2$player2
CARL10_SDg3 <- CARL10_SDg2
CARL10_SDg3$p1inp2vec <- is.element(CARL10_SDg3$player1, player2vector)
CARL10_SDg3$p2inp1vec <- is.element(CARL10_SDg3$player2, player1vector)

addPlayer1 <- CARL10_SDg3[ which(CARL10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_SDg3[ which(CARL10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_SDg2 <- rbind(CARL10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
CARL10_SDft <- ftable(CARL10_SDg2$player1, CARL10_SDg2$player2)
CARL10_SDft2 <- as.matrix(CARL10_SDft)
numRows <- nrow(CARL10_SDft2)
numCols <- ncol(CARL10_SDft2)
CARL10_SDft3 <- CARL10_SDft2[c(2:numRows) , c(2:numCols)]
CARL10_SDTable <- graph.adjacency(CARL10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(CARL10_SDTable, vertex.label = V(CARL10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
CARL10_SD.clusterCoef <- transitivity(CARL10_SDTable, type="global") #cluster coefficient
CARL10_SD.degreeCent <- centralization.degree(CARL10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_SDftn <- as.network.matrix(CARL10_SDft)
CARL10_SD.netDensity <- network.density(CARL10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_SD.entropy <- entropy(CARL10_SDft) #entropy

CARL10_SD.netMx <- cbind(CARL10_SD.netMx, CARL10_SD.clusterCoef, CARL10_SD.degreeCent$centralization,
                         CARL10_SD.netDensity, CARL10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
CARL10_TDg2 <- data.frame(CARL10_TD)
CARL10_TDg2 <- CARL10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_TDg2$player1
player2vector <- CARL10_TDg2$player2
CARL10_TDg3 <- CARL10_TDg2
CARL10_TDg3$p1inp2vec <- is.element(CARL10_TDg3$player1, player2vector)
CARL10_TDg3$p2inp1vec <- is.element(CARL10_TDg3$player2, player1vector)

addPlayer1 <- CARL10_TDg3[ which(CARL10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_TDg3[ which(CARL10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_TDg2 <- rbind(CARL10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
CARL10_TDft <- ftable(CARL10_TDg2$player1, CARL10_TDg2$player2)
CARL10_TDft2 <- as.matrix(CARL10_TDft)
numRows <- nrow(CARL10_TDft2)
numCols <- ncol(CARL10_TDft2)
CARL10_TDft3 <- CARL10_TDft2[c(2:numRows) , c(2:numCols)]
CARL10_TDTable <- graph.adjacency(CARL10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(CARL10_TDTable, vertex.label = V(CARL10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
CARL10_TD.clusterCoef <- transitivity(CARL10_TDTable, type="global") #cluster coefficient
CARL10_TD.degreeCent <- centralization.degree(CARL10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_TDftn <- as.network.matrix(CARL10_TDft)
CARL10_TD.netDensity <- network.density(CARL10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_TD.entropy <- entropy(CARL10_TDft) #entropy

CARL10_TD.netMx <- cbind(CARL10_TD.netMx, CARL10_TD.clusterCoef, CARL10_TD.degreeCent$centralization,
                         CARL10_TD.netDensity, CARL10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
CARL10_QTg2 <- data.frame(CARL10_QT)
CARL10_QTg2 <- CARL10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL10_QTg2$player1
player2vector <- CARL10_QTg2$player2
CARL10_QTg3 <- CARL10_QTg2
CARL10_QTg3$p1inp2vec <- is.element(CARL10_QTg3$player1, player2vector)
CARL10_QTg3$p2inp1vec <- is.element(CARL10_QTg3$player2, player1vector)

addPlayer1 <- CARL10_QTg3[ which(CARL10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL10_QTg3[ which(CARL10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL10_QTg2 <- rbind(CARL10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
CARL10_QTft <- ftable(CARL10_QTg2$player1, CARL10_QTg2$player2)
CARL10_QTft2 <- as.matrix(CARL10_QTft)
numRows <- nrow(CARL10_QTft2)
numCols <- ncol(CARL10_QTft2)
CARL10_QTft3 <- CARL10_QTft2[c(2:numRows) , c(2:numCols)]
CARL10_QTTable <- graph.adjacency(CARL10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(CARL10_QTTable, vertex.label = V(CARL10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
CARL10_QT.clusterCoef <- transitivity(CARL10_QTTable, type="global") #cluster coefficient
CARL10_QT.degreeCent <- centralization.degree(CARL10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL10_QTftn <- as.network.matrix(CARL10_QTft)
CARL10_QT.netDensity <- network.density(CARL10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL10_QT.entropy <- entropy(CARL10_QTft) #entropy

CARL10_QT.netMx <- cbind(CARL10_QT.netMx, CARL10_QT.clusterCoef, CARL10_QT.degreeCent$centralization,
                         CARL10_QT.netDensity, CARL10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL10_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "Goal_F"
COLL10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
COLL10_Gg2 <- data.frame(COLL10_G)
COLL10_Gg2 <- COLL10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_Gg2$player1
player2vector <- COLL10_Gg2$player2
COLL10_Gg3 <- COLL10_Gg2
COLL10_Gg3$p1inp2vec <- is.element(COLL10_Gg3$player1, player2vector)
COLL10_Gg3$p2inp1vec <- is.element(COLL10_Gg3$player2, player1vector)

addPlayer1 <- COLL10_Gg3[ which(COLL10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_Gg3[ which(COLL10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_Gg2 <- rbind(COLL10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
COLL10_Gft <- ftable(COLL10_Gg2$player1, COLL10_Gg2$player2)
COLL10_Gft2 <- as.matrix(COLL10_Gft)
numRows <- nrow(COLL10_Gft2)
numCols <- ncol(COLL10_Gft2)
COLL10_Gft3 <- COLL10_Gft2[c(2:numRows) , c(2:numCols)]
COLL10_GTable <- graph.adjacency(COLL10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(COLL10_GTable, vertex.label = V(COLL10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
COLL10_G.clusterCoef <- transitivity(COLL10_GTable, type="global") #cluster coefficient
COLL10_G.degreeCent <- centralization.degree(COLL10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_Gftn <- as.network.matrix(COLL10_Gft)
COLL10_G.netDensity <- network.density(COLL10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_G.entropy <- entropy(COLL10_Gft) #entropy

COLL10_G.netMx <- cbind(COLL10_G.netMx, COLL10_G.clusterCoef, COLL10_G.degreeCent$centralization,
                        COLL10_G.netDensity, COLL10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "Behind_F"
COLL10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
COLL10_Bg2 <- data.frame(COLL10_B)
COLL10_Bg2 <- COLL10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_Bg2$player1
player2vector <- COLL10_Bg2$player2
COLL10_Bg3 <- COLL10_Bg2
COLL10_Bg3$p1inp2vec <- is.element(COLL10_Bg3$player1, player2vector)
COLL10_Bg3$p2inp1vec <- is.element(COLL10_Bg3$player2, player1vector)

addPlayer1 <- COLL10_Bg3[ which(COLL10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_Bg3[ which(COLL10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_Bg2 <- rbind(COLL10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
COLL10_Bft <- ftable(COLL10_Bg2$player1, COLL10_Bg2$player2)
COLL10_Bft2 <- as.matrix(COLL10_Bft)
numRows <- nrow(COLL10_Bft2)
numCols <- ncol(COLL10_Bft2)
COLL10_Bft3 <- COLL10_Bft2[c(2:numRows) , c(2:numCols)]
COLL10_BTable <- graph.adjacency(COLL10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(COLL10_BTable, vertex.label = V(COLL10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
COLL10_B.clusterCoef <- transitivity(COLL10_BTable, type="global") #cluster coefficient
COLL10_B.degreeCent <- centralization.degree(COLL10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_Bftn <- as.network.matrix(COLL10_Bft)
COLL10_B.netDensity <- network.density(COLL10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_B.entropy <- entropy(COLL10_Bft) #entropy

COLL10_B.netMx <- cbind(COLL10_B.netMx, COLL10_B.clusterCoef, COLL10_B.degreeCent$centralization,
                        COLL10_B.netDensity, COLL10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************

round = 10
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
COLL10_SFg2 <- data.frame(COLL10_SF)
COLL10_SFg2 <- COLL10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_SFg2$player1
player2vector <- COLL10_SFg2$player2
COLL10_SFg3 <- COLL10_SFg2
COLL10_SFg3$p1inp2vec <- is.element(COLL10_SFg3$player1, player2vector)
COLL10_SFg3$p2inp1vec <- is.element(COLL10_SFg3$player2, player1vector)

addPlayer1 <- COLL10_SFg3[ which(COLL10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_SFg3[ which(COLL10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_SFg2 <- rbind(COLL10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
COLL10_SFft <- ftable(COLL10_SFg2$player1, COLL10_SFg2$player2)
COLL10_SFft2 <- as.matrix(COLL10_SFft)
numRows <- nrow(COLL10_SFft2)
numCols <- ncol(COLL10_SFft2)
COLL10_SFft3 <- COLL10_SFft2[c(2:numRows) , c(2:numCols)]
COLL10_SFTable <- graph.adjacency(COLL10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(COLL10_SFTable, vertex.label = V(COLL10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
COLL10_SF.clusterCoef <- transitivity(COLL10_SFTable, type="global") #cluster coefficient
COLL10_SF.degreeCent <- centralization.degree(COLL10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_SFftn <- as.network.matrix(COLL10_SFft)
COLL10_SF.netDensity <- network.density(COLL10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_SF.entropy <- entropy(COLL10_SFft) #entropy

COLL10_SF.netMx <- cbind(COLL10_SF.netMx, COLL10_SF.clusterCoef, COLL10_SF.degreeCent$centralization,
                         COLL10_SF.netDensity, COLL10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
COLL10_TFg2 <- data.frame(COLL10_TF)
COLL10_TFg2 <- COLL10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_TFg2$player1
player2vector <- COLL10_TFg2$player2
COLL10_TFg3 <- COLL10_TFg2
COLL10_TFg3$p1inp2vec <- is.element(COLL10_TFg3$player1, player2vector)
COLL10_TFg3$p2inp1vec <- is.element(COLL10_TFg3$player2, player1vector)

addPlayer1 <- COLL10_TFg3[ which(COLL10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_TFg3[ which(COLL10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_TFg2 <- rbind(COLL10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
COLL10_TFft <- ftable(COLL10_TFg2$player1, COLL10_TFg2$player2)
COLL10_TFft2 <- as.matrix(COLL10_TFft)
numRows <- nrow(COLL10_TFft2)
numCols <- ncol(COLL10_TFft2)
COLL10_TFft3 <- COLL10_TFft2[c(2:numRows) , c(2:numCols)]
COLL10_TFTable <- graph.adjacency(COLL10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(COLL10_TFTable, vertex.label = V(COLL10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
COLL10_TF.clusterCoef <- transitivity(COLL10_TFTable, type="global") #cluster coefficient
COLL10_TF.degreeCent <- centralization.degree(COLL10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_TFftn <- as.network.matrix(COLL10_TFft)
COLL10_TF.netDensity <- network.density(COLL10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_TF.entropy <- entropy(COLL10_TFft) #entropy

COLL10_TF.netMx <- cbind(COLL10_TF.netMx, COLL10_TF.clusterCoef, COLL10_TF.degreeCent$centralization,
                         COLL10_TF.netDensity, COLL10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
COLL10_SAMg2 <- data.frame(COLL10_SAM)
COLL10_SAMg2 <- COLL10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_SAMg2$player1
player2vector <- COLL10_SAMg2$player2
COLL10_SAMg3 <- COLL10_SAMg2
COLL10_SAMg3$p1inp2vec <- is.element(COLL10_SAMg3$player1, player2vector)
COLL10_SAMg3$p2inp1vec <- is.element(COLL10_SAMg3$player2, player1vector)

addPlayer1 <- COLL10_SAMg3[ which(COLL10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_SAMg3[ which(COLL10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_SAMg2 <- rbind(COLL10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
COLL10_SAMft <- ftable(COLL10_SAMg2$player1, COLL10_SAMg2$player2)
COLL10_SAMft2 <- as.matrix(COLL10_SAMft)
numRows <- nrow(COLL10_SAMft2)
numCols <- ncol(COLL10_SAMft2)
COLL10_SAMft3 <- COLL10_SAMft2[c(2:numRows) , c(2:numCols)]
COLL10_SAMTable <- graph.adjacency(COLL10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(COLL10_SAMTable, vertex.label = V(COLL10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
COLL10_SAM.clusterCoef <- transitivity(COLL10_SAMTable, type="global") #cluster coefficient
COLL10_SAM.degreeCent <- centralization.degree(COLL10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_SAMftn <- as.network.matrix(COLL10_SAMft)
COLL10_SAM.netDensity <- network.density(COLL10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_SAM.entropy <- entropy(COLL10_SAMft) #entropy

COLL10_SAM.netMx <- cbind(COLL10_SAM.netMx, COLL10_SAM.clusterCoef, COLL10_SAM.degreeCent$centralization,
                          COLL10_SAM.netDensity, COLL10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
COLL10_TAMg2 <- data.frame(COLL10_TAM)
COLL10_TAMg2 <- COLL10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_TAMg2$player1
player2vector <- COLL10_TAMg2$player2
COLL10_TAMg3 <- COLL10_TAMg2
COLL10_TAMg3$p1inp2vec <- is.element(COLL10_TAMg3$player1, player2vector)
COLL10_TAMg3$p2inp1vec <- is.element(COLL10_TAMg3$player2, player1vector)

addPlayer1 <- COLL10_TAMg3[ which(COLL10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_TAMg3[ which(COLL10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_TAMg2 <- rbind(COLL10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
COLL10_TAMft <- ftable(COLL10_TAMg2$player1, COLL10_TAMg2$player2)
COLL10_TAMft2 <- as.matrix(COLL10_TAMft)
numRows <- nrow(COLL10_TAMft2)
numCols <- ncol(COLL10_TAMft2)
COLL10_TAMft3 <- COLL10_TAMft2[c(2:numRows) , c(2:numCols)]
COLL10_TAMTable <- graph.adjacency(COLL10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(COLL10_TAMTable, vertex.label = V(COLL10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
COLL10_TAM.clusterCoef <- transitivity(COLL10_TAMTable, type="global") #cluster coefficient
COLL10_TAM.degreeCent <- centralization.degree(COLL10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_TAMftn <- as.network.matrix(COLL10_TAMft)
COLL10_TAM.netDensity <- network.density(COLL10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_TAM.entropy <- entropy(COLL10_TAMft) #entropy

COLL10_TAM.netMx <- cbind(COLL10_TAM.netMx, COLL10_TAM.clusterCoef, COLL10_TAM.degreeCent$centralization,
                          COLL10_TAM.netDensity, COLL10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
COLL10_SDMg2 <- data.frame(COLL10_SDM)
COLL10_SDMg2 <- COLL10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_SDMg2$player1
player2vector <- COLL10_SDMg2$player2
COLL10_SDMg3 <- COLL10_SDMg2
COLL10_SDMg3$p1inp2vec <- is.element(COLL10_SDMg3$player1, player2vector)
COLL10_SDMg3$p2inp1vec <- is.element(COLL10_SDMg3$player2, player1vector)

addPlayer1 <- COLL10_SDMg3[ which(COLL10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_SDMg3[ which(COLL10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_SDMg2 <- rbind(COLL10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
COLL10_SDMft <- ftable(COLL10_SDMg2$player1, COLL10_SDMg2$player2)
COLL10_SDMft2 <- as.matrix(COLL10_SDMft)
numRows <- nrow(COLL10_SDMft2)
numCols <- ncol(COLL10_SDMft2)
COLL10_SDMft3 <- COLL10_SDMft2[c(2:numRows) , c(2:numCols)]
COLL10_SDMTable <- graph.adjacency(COLL10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(COLL10_SDMTable, vertex.label = V(COLL10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
COLL10_SDM.clusterCoef <- transitivity(COLL10_SDMTable, type="global") #cluster coefficient
COLL10_SDM.degreeCent <- centralization.degree(COLL10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_SDMftn <- as.network.matrix(COLL10_SDMft)
COLL10_SDM.netDensity <- network.density(COLL10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_SDM.entropy <- entropy(COLL10_SDMft) #entropy

COLL10_SDM.netMx <- cbind(COLL10_SDM.netMx, COLL10_SDM.clusterCoef, COLL10_SDM.degreeCent$centralization,
                          COLL10_SDM.netDensity, COLL10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
COLL10_TDMg2 <- data.frame(COLL10_TDM)
COLL10_TDMg2 <- COLL10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_TDMg2$player1
player2vector <- COLL10_TDMg2$player2
COLL10_TDMg3 <- COLL10_TDMg2
COLL10_TDMg3$p1inp2vec <- is.element(COLL10_TDMg3$player1, player2vector)
COLL10_TDMg3$p2inp1vec <- is.element(COLL10_TDMg3$player2, player1vector)

addPlayer1 <- COLL10_TDMg3[ which(COLL10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_TDMg3[ which(COLL10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_TDMg2 <- rbind(COLL10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
COLL10_TDMft <- ftable(COLL10_TDMg2$player1, COLL10_TDMg2$player2)
COLL10_TDMft2 <- as.matrix(COLL10_TDMft)
numRows <- nrow(COLL10_TDMft2)
numCols <- ncol(COLL10_TDMft2)
COLL10_TDMft3 <- COLL10_TDMft2[c(2:numRows) , c(2:numCols)]
COLL10_TDMTable <- graph.adjacency(COLL10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(COLL10_TDMTable, vertex.label = V(COLL10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
COLL10_TDM.clusterCoef <- transitivity(COLL10_TDMTable, type="global") #cluster coefficient
COLL10_TDM.degreeCent <- centralization.degree(COLL10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_TDMftn <- as.network.matrix(COLL10_TDMft)
COLL10_TDM.netDensity <- network.density(COLL10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_TDM.entropy <- entropy(COLL10_TDMft) #entropy

COLL10_TDM.netMx <- cbind(COLL10_TDM.netMx, COLL10_TDM.clusterCoef, COLL10_TDM.degreeCent$centralization,
                          COLL10_TDM.netDensity, COLL10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
COLL10_SDg2 <- data.frame(COLL10_SD)
COLL10_SDg2 <- COLL10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_SDg2$player1
player2vector <- COLL10_SDg2$player2
COLL10_SDg3 <- COLL10_SDg2
COLL10_SDg3$p1inp2vec <- is.element(COLL10_SDg3$player1, player2vector)
COLL10_SDg3$p2inp1vec <- is.element(COLL10_SDg3$player2, player1vector)

addPlayer1 <- COLL10_SDg3[ which(COLL10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_SDg3[ which(COLL10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_SDg2 <- rbind(COLL10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
COLL10_SDft <- ftable(COLL10_SDg2$player1, COLL10_SDg2$player2)
COLL10_SDft2 <- as.matrix(COLL10_SDft)
numRows <- nrow(COLL10_SDft2)
numCols <- ncol(COLL10_SDft2)
COLL10_SDft3 <- COLL10_SDft2[c(2:numRows) , c(2:numCols)]
COLL10_SDTable <- graph.adjacency(COLL10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(COLL10_SDTable, vertex.label = V(COLL10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
COLL10_SD.clusterCoef <- transitivity(COLL10_SDTable, type="global") #cluster coefficient
COLL10_SD.degreeCent <- centralization.degree(COLL10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_SDftn <- as.network.matrix(COLL10_SDft)
COLL10_SD.netDensity <- network.density(COLL10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_SD.entropy <- entropy(COLL10_SDft) #entropy

COLL10_SD.netMx <- cbind(COLL10_SD.netMx, COLL10_SD.clusterCoef, COLL10_SD.degreeCent$centralization,
                         COLL10_SD.netDensity, COLL10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
COLL10_TDg2 <- data.frame(COLL10_TD)
COLL10_TDg2 <- COLL10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_TDg2$player1
player2vector <- COLL10_TDg2$player2
COLL10_TDg3 <- COLL10_TDg2
COLL10_TDg3$p1inp2vec <- is.element(COLL10_TDg3$player1, player2vector)
COLL10_TDg3$p2inp1vec <- is.element(COLL10_TDg3$player2, player1vector)

addPlayer1 <- COLL10_TDg3[ which(COLL10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_TDg3[ which(COLL10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_TDg2 <- rbind(COLL10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
COLL10_TDft <- ftable(COLL10_TDg2$player1, COLL10_TDg2$player2)
COLL10_TDft2 <- as.matrix(COLL10_TDft)
numRows <- nrow(COLL10_TDft2)
numCols <- ncol(COLL10_TDft2)
COLL10_TDft3 <- COLL10_TDft2[c(2:numRows) , c(2:numCols)]
COLL10_TDTable <- graph.adjacency(COLL10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(COLL10_TDTable, vertex.label = V(COLL10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
COLL10_TD.clusterCoef <- transitivity(COLL10_TDTable, type="global") #cluster coefficient
COLL10_TD.degreeCent <- centralization.degree(COLL10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_TDftn <- as.network.matrix(COLL10_TDft)
COLL10_TD.netDensity <- network.density(COLL10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_TD.entropy <- entropy(COLL10_TDft) #entropy

COLL10_TD.netMx <- cbind(COLL10_TD.netMx, COLL10_TD.clusterCoef, COLL10_TD.degreeCent$centralization,
                         COLL10_TD.netDensity, COLL10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
COLL10_QTg2 <- data.frame(COLL10_QT)
COLL10_QTg2 <- COLL10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL10_QTg2$player1
player2vector <- COLL10_QTg2$player2
COLL10_QTg3 <- COLL10_QTg2
COLL10_QTg3$p1inp2vec <- is.element(COLL10_QTg3$player1, player2vector)
COLL10_QTg3$p2inp1vec <- is.element(COLL10_QTg3$player2, player1vector)

addPlayer1 <- COLL10_QTg3[ which(COLL10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL10_QTg3[ which(COLL10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL10_QTg2 <- rbind(COLL10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
COLL10_QTft <- ftable(COLL10_QTg2$player1, COLL10_QTg2$player2)
COLL10_QTft2 <- as.matrix(COLL10_QTft)
numRows <- nrow(COLL10_QTft2)
numCols <- ncol(COLL10_QTft2)
COLL10_QTft3 <- COLL10_QTft2[c(2:numRows) , c(2:numCols)]
COLL10_QTTable <- graph.adjacency(COLL10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(COLL10_QTTable, vertex.label = V(COLL10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
COLL10_QT.clusterCoef <- transitivity(COLL10_QTTable, type="global") #cluster coefficient
COLL10_QT.degreeCent <- centralization.degree(COLL10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL10_QTftn <- as.network.matrix(COLL10_QTft)
COLL10_QT.netDensity <- network.density(COLL10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL10_QT.entropy <- entropy(COLL10_QTft) #entropy

COLL10_QT.netMx <- cbind(COLL10_QT.netMx, COLL10_QT.clusterCoef, COLL10_QT.degreeCent$centralization,
                         COLL10_QT.netDensity, COLL10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL10_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "Goal_F"
ESS10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
ESS10_Gg2 <- data.frame(ESS10_G)
ESS10_Gg2 <- ESS10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_Gg2$player1
player2vector <- ESS10_Gg2$player2
ESS10_Gg3 <- ESS10_Gg2
ESS10_Gg3$p1inp2vec <- is.element(ESS10_Gg3$player1, player2vector)
ESS10_Gg3$p2inp1vec <- is.element(ESS10_Gg3$player2, player1vector)

addPlayer1 <- ESS10_Gg3[ which(ESS10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_Gg3[ which(ESS10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_Gg2 <- rbind(ESS10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
ESS10_Gft <- ftable(ESS10_Gg2$player1, ESS10_Gg2$player2)
ESS10_Gft2 <- as.matrix(ESS10_Gft)
numRows <- nrow(ESS10_Gft2)
numCols <- ncol(ESS10_Gft2)
ESS10_Gft3 <- ESS10_Gft2[c(2:numRows) , c(2:numCols)]
ESS10_GTable <- graph.adjacency(ESS10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(ESS10_GTable, vertex.label = V(ESS10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
ESS10_G.clusterCoef <- transitivity(ESS10_GTable, type="global") #cluster coefficient
ESS10_G.degreeCent <- centralization.degree(ESS10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_Gftn <- as.network.matrix(ESS10_Gft)
ESS10_G.netDensity <- network.density(ESS10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_G.entropy <- entropy(ESS10_Gft) #entropy

ESS10_G.netMx <- cbind(ESS10_G.netMx, ESS10_G.clusterCoef, ESS10_G.degreeCent$centralization,
                       ESS10_G.netDensity, ESS10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************

round = 10
teamName = "ESS"
KIoutcome = "Behind_F"
ESS10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
ESS10_Bg2 <- data.frame(ESS10_B)
ESS10_Bg2 <- ESS10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_Bg2$player1
player2vector <- ESS10_Bg2$player2
ESS10_Bg3 <- ESS10_Bg2
ESS10_Bg3$p1inp2vec <- is.element(ESS10_Bg3$player1, player2vector)
ESS10_Bg3$p2inp1vec <- is.element(ESS10_Bg3$player2, player1vector)

addPlayer1 <- ESS10_Bg3[ which(ESS10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_Bg3[ which(ESS10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_Bg2 <- rbind(ESS10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
ESS10_Bft <- ftable(ESS10_Bg2$player1, ESS10_Bg2$player2)
ESS10_Bft2 <- as.matrix(ESS10_Bft)
numRows <- nrow(ESS10_Bft2)
numCols <- ncol(ESS10_Bft2)
ESS10_Bft3 <- ESS10_Bft2[c(2:numRows) , c(2:numCols)]
ESS10_BTable <- graph.adjacency(ESS10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(ESS10_BTable, vertex.label = V(ESS10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
ESS10_B.clusterCoef <- transitivity(ESS10_BTable, type="global") #cluster coefficient
ESS10_B.degreeCent <- centralization.degree(ESS10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_Bftn <- as.network.matrix(ESS10_Bft)
ESS10_B.netDensity <- network.density(ESS10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_B.entropy <- entropy(ESS10_Bft) #entropy

ESS10_B.netMx <- cbind(ESS10_B.netMx, ESS10_B.clusterCoef, ESS10_B.degreeCent$centralization,
                       ESS10_B.netDensity, ESS10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
ESS10_SFg2 <- data.frame(ESS10_SF)
ESS10_SFg2 <- ESS10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_SFg2$player1
player2vector <- ESS10_SFg2$player2
ESS10_SFg3 <- ESS10_SFg2
ESS10_SFg3$p1inp2vec <- is.element(ESS10_SFg3$player1, player2vector)
ESS10_SFg3$p2inp1vec <- is.element(ESS10_SFg3$player2, player1vector)

addPlayer1 <- ESS10_SFg3[ which(ESS10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_SFg3[ which(ESS10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_SFg2 <- rbind(ESS10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
ESS10_SFft <- ftable(ESS10_SFg2$player1, ESS10_SFg2$player2)
ESS10_SFft2 <- as.matrix(ESS10_SFft)
numRows <- nrow(ESS10_SFft2)
numCols <- ncol(ESS10_SFft2)
ESS10_SFft3 <- ESS10_SFft2[c(2:numRows) , c(2:numCols)]
ESS10_SFTable <- graph.adjacency(ESS10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(ESS10_SFTable, vertex.label = V(ESS10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
ESS10_SF.clusterCoef <- transitivity(ESS10_SFTable, type="global") #cluster coefficient
ESS10_SF.degreeCent <- centralization.degree(ESS10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_SFftn <- as.network.matrix(ESS10_SFft)
ESS10_SF.netDensity <- network.density(ESS10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_SF.entropy <- entropy(ESS10_SFft) #entropy

ESS10_SF.netMx <- cbind(ESS10_SF.netMx, ESS10_SF.clusterCoef, ESS10_SF.degreeCent$centralization,
                        ESS10_SF.netDensity, ESS10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
ESS10_TFg2 <- data.frame(ESS10_TF)
ESS10_TFg2 <- ESS10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_TFg2$player1
player2vector <- ESS10_TFg2$player2
ESS10_TFg3 <- ESS10_TFg2
ESS10_TFg3$p1inp2vec <- is.element(ESS10_TFg3$player1, player2vector)
ESS10_TFg3$p2inp1vec <- is.element(ESS10_TFg3$player2, player1vector)

addPlayer1 <- ESS10_TFg3[ which(ESS10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_TFg3[ which(ESS10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_TFg2 <- rbind(ESS10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
ESS10_TFft <- ftable(ESS10_TFg2$player1, ESS10_TFg2$player2)
ESS10_TFft2 <- as.matrix(ESS10_TFft)
numRows <- nrow(ESS10_TFft2)
numCols <- ncol(ESS10_TFft2)
ESS10_TFft3 <- ESS10_TFft2[c(2:numRows) , c(2:numCols)]
ESS10_TFTable <- graph.adjacency(ESS10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(ESS10_TFTable, vertex.label = V(ESS10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
ESS10_TF.clusterCoef <- transitivity(ESS10_TFTable, type="global") #cluster coefficient
ESS10_TF.degreeCent <- centralization.degree(ESS10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_TFftn <- as.network.matrix(ESS10_TFft)
ESS10_TF.netDensity <- network.density(ESS10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_TF.entropy <- entropy(ESS10_TFft) #entropy

ESS10_TF.netMx <- cbind(ESS10_TF.netMx, ESS10_TF.clusterCoef, ESS10_TF.degreeCent$centralization,
                        ESS10_TF.netDensity, ESS10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
ESS10_SAMg2 <- data.frame(ESS10_SAM)
ESS10_SAMg2 <- ESS10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_SAMg2$player1
player2vector <- ESS10_SAMg2$player2
ESS10_SAMg3 <- ESS10_SAMg2
ESS10_SAMg3$p1inp2vec <- is.element(ESS10_SAMg3$player1, player2vector)
ESS10_SAMg3$p2inp1vec <- is.element(ESS10_SAMg3$player2, player1vector)

addPlayer1 <- ESS10_SAMg3[ which(ESS10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_SAMg3[ which(ESS10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_SAMg2 <- rbind(ESS10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
ESS10_SAMft <- ftable(ESS10_SAMg2$player1, ESS10_SAMg2$player2)
ESS10_SAMft2 <- as.matrix(ESS10_SAMft)
numRows <- nrow(ESS10_SAMft2)
numCols <- ncol(ESS10_SAMft2)
ESS10_SAMft3 <- ESS10_SAMft2[c(2:numRows) , c(2:numCols)]
ESS10_SAMTable <- graph.adjacency(ESS10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(ESS10_SAMTable, vertex.label = V(ESS10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
ESS10_SAM.clusterCoef <- transitivity(ESS10_SAMTable, type="global") #cluster coefficient
ESS10_SAM.degreeCent <- centralization.degree(ESS10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_SAMftn <- as.network.matrix(ESS10_SAMft)
ESS10_SAM.netDensity <- network.density(ESS10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_SAM.entropy <- entropy(ESS10_SAMft) #entropy

ESS10_SAM.netMx <- cbind(ESS10_SAM.netMx, ESS10_SAM.clusterCoef, ESS10_SAM.degreeCent$centralization,
                         ESS10_SAM.netDensity, ESS10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
ESS10_TAMg2 <- data.frame(ESS10_TAM)
ESS10_TAMg2 <- ESS10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_TAMg2$player1
player2vector <- ESS10_TAMg2$player2
ESS10_TAMg3 <- ESS10_TAMg2
ESS10_TAMg3$p1inp2vec <- is.element(ESS10_TAMg3$player1, player2vector)
ESS10_TAMg3$p2inp1vec <- is.element(ESS10_TAMg3$player2, player1vector)

addPlayer1 <- ESS10_TAMg3[ which(ESS10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_TAMg3[ which(ESS10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_TAMg2 <- rbind(ESS10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
ESS10_TAMft <- ftable(ESS10_TAMg2$player1, ESS10_TAMg2$player2)
ESS10_TAMft2 <- as.matrix(ESS10_TAMft)
numRows <- nrow(ESS10_TAMft2)
numCols <- ncol(ESS10_TAMft2)
ESS10_TAMft3 <- ESS10_TAMft2[c(2:numRows) , c(2:numCols)]
ESS10_TAMTable <- graph.adjacency(ESS10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(ESS10_TAMTable, vertex.label = V(ESS10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
ESS10_TAM.clusterCoef <- transitivity(ESS10_TAMTable, type="global") #cluster coefficient
ESS10_TAM.degreeCent <- centralization.degree(ESS10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_TAMftn <- as.network.matrix(ESS10_TAMft)
ESS10_TAM.netDensity <- network.density(ESS10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_TAM.entropy <- entropy(ESS10_TAMft) #entropy

ESS10_TAM.netMx <- cbind(ESS10_TAM.netMx, ESS10_TAM.clusterCoef, ESS10_TAM.degreeCent$centralization,
                         ESS10_TAM.netDensity, ESS10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
ESS10_SDMg2 <- data.frame(ESS10_SDM)
ESS10_SDMg2 <- ESS10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_SDMg2$player1
player2vector <- ESS10_SDMg2$player2
ESS10_SDMg3 <- ESS10_SDMg2
ESS10_SDMg3$p1inp2vec <- is.element(ESS10_SDMg3$player1, player2vector)
ESS10_SDMg3$p2inp1vec <- is.element(ESS10_SDMg3$player2, player1vector)

addPlayer1 <- ESS10_SDMg3[ which(ESS10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_SDMg3[ which(ESS10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_SDMg2 <- rbind(ESS10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
ESS10_SDMft <- ftable(ESS10_SDMg2$player1, ESS10_SDMg2$player2)
ESS10_SDMft2 <- as.matrix(ESS10_SDMft)
numRows <- nrow(ESS10_SDMft2)
numCols <- ncol(ESS10_SDMft2)
ESS10_SDMft3 <- ESS10_SDMft2[c(2:numRows) , c(2:numCols)]
ESS10_SDMTable <- graph.adjacency(ESS10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(ESS10_SDMTable, vertex.label = V(ESS10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
ESS10_SDM.clusterCoef <- transitivity(ESS10_SDMTable, type="global") #cluster coefficient
ESS10_SDM.degreeCent <- centralization.degree(ESS10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_SDMftn <- as.network.matrix(ESS10_SDMft)
ESS10_SDM.netDensity <- network.density(ESS10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_SDM.entropy <- entropy(ESS10_SDMft) #entropy

ESS10_SDM.netMx <- cbind(ESS10_SDM.netMx, ESS10_SDM.clusterCoef, ESS10_SDM.degreeCent$centralization,
                         ESS10_SDM.netDensity, ESS10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
ESS10_TDMg2 <- data.frame(ESS10_TDM)
ESS10_TDMg2 <- ESS10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_TDMg2$player1
player2vector <- ESS10_TDMg2$player2
ESS10_TDMg3 <- ESS10_TDMg2
ESS10_TDMg3$p1inp2vec <- is.element(ESS10_TDMg3$player1, player2vector)
ESS10_TDMg3$p2inp1vec <- is.element(ESS10_TDMg3$player2, player1vector)

addPlayer1 <- ESS10_TDMg3[ which(ESS10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_TDMg3[ which(ESS10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_TDMg2 <- rbind(ESS10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
ESS10_TDMft <- ftable(ESS10_TDMg2$player1, ESS10_TDMg2$player2)
ESS10_TDMft2 <- as.matrix(ESS10_TDMft)
numRows <- nrow(ESS10_TDMft2)
numCols <- ncol(ESS10_TDMft2)
ESS10_TDMft3 <- ESS10_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS10_TDMTable <- graph.adjacency(ESS10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(ESS10_TDMTable, vertex.label = V(ESS10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
ESS10_TDM.clusterCoef <- transitivity(ESS10_TDMTable, type="global") #cluster coefficient
ESS10_TDM.degreeCent <- centralization.degree(ESS10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_TDMftn <- as.network.matrix(ESS10_TDMft)
ESS10_TDM.netDensity <- network.density(ESS10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_TDM.entropy <- entropy(ESS10_TDMft) #entropy

ESS10_TDM.netMx <- cbind(ESS10_TDM.netMx, ESS10_TDM.clusterCoef, ESS10_TDM.degreeCent$centralization,
                         ESS10_TDM.netDensity, ESS10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
ESS10_SDg2 <- data.frame(ESS10_SD)
ESS10_SDg2 <- ESS10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_SDg2$player1
player2vector <- ESS10_SDg2$player2
ESS10_SDg3 <- ESS10_SDg2
ESS10_SDg3$p1inp2vec <- is.element(ESS10_SDg3$player1, player2vector)
ESS10_SDg3$p2inp1vec <- is.element(ESS10_SDg3$player2, player1vector)

addPlayer1 <- ESS10_SDg3[ which(ESS10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_SDg3[ which(ESS10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_SDg2 <- rbind(ESS10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
ESS10_SDft <- ftable(ESS10_SDg2$player1, ESS10_SDg2$player2)
ESS10_SDft2 <- as.matrix(ESS10_SDft)
numRows <- nrow(ESS10_SDft2)
numCols <- ncol(ESS10_SDft2)
ESS10_SDft3 <- ESS10_SDft2[c(2:numRows) , c(2:numCols)]
ESS10_SDTable <- graph.adjacency(ESS10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(ESS10_SDTable, vertex.label = V(ESS10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
ESS10_SD.clusterCoef <- transitivity(ESS10_SDTable, type="global") #cluster coefficient
ESS10_SD.degreeCent <- centralization.degree(ESS10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_SDftn <- as.network.matrix(ESS10_SDft)
ESS10_SD.netDensity <- network.density(ESS10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_SD.entropy <- entropy(ESS10_SDft) #entropy

ESS10_SD.netMx <- cbind(ESS10_SD.netMx, ESS10_SD.clusterCoef, ESS10_SD.degreeCent$centralization,
                        ESS10_SD.netDensity, ESS10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
ESS10_TDg2 <- data.frame(ESS10_TD)
ESS10_TDg2 <- ESS10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_TDg2$player1
player2vector <- ESS10_TDg2$player2
ESS10_TDg3 <- ESS10_TDg2
ESS10_TDg3$p1inp2vec <- is.element(ESS10_TDg3$player1, player2vector)
ESS10_TDg3$p2inp1vec <- is.element(ESS10_TDg3$player2, player1vector)

addPlayer1 <- ESS10_TDg3[ which(ESS10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_TDg3[ which(ESS10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_TDg2 <- rbind(ESS10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
ESS10_TDft <- ftable(ESS10_TDg2$player1, ESS10_TDg2$player2)
ESS10_TDft2 <- as.matrix(ESS10_TDft)
numRows <- nrow(ESS10_TDft2)
numCols <- ncol(ESS10_TDft2)
ESS10_TDft3 <- ESS10_TDft2[c(2:numRows) , c(2:numCols)]
ESS10_TDTable <- graph.adjacency(ESS10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(ESS10_TDTable, vertex.label = V(ESS10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
ESS10_TD.clusterCoef <- transitivity(ESS10_TDTable, type="global") #cluster coefficient
ESS10_TD.degreeCent <- centralization.degree(ESS10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_TDftn <- as.network.matrix(ESS10_TDft)
ESS10_TD.netDensity <- network.density(ESS10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_TD.entropy <- entropy(ESS10_TDft) #entropy

ESS10_TD.netMx <- cbind(ESS10_TD.netMx, ESS10_TD.clusterCoef, ESS10_TD.degreeCent$centralization,
                        ESS10_TD.netDensity, ESS10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
ESS10_QTg2 <- data.frame(ESS10_QT)
ESS10_QTg2 <- ESS10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS10_QTg2$player1
player2vector <- ESS10_QTg2$player2
ESS10_QTg3 <- ESS10_QTg2
ESS10_QTg3$p1inp2vec <- is.element(ESS10_QTg3$player1, player2vector)
ESS10_QTg3$p2inp1vec <- is.element(ESS10_QTg3$player2, player1vector)

addPlayer1 <- ESS10_QTg3[ which(ESS10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS10_QTg3[ which(ESS10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS10_QTg2 <- rbind(ESS10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
ESS10_QTft <- ftable(ESS10_QTg2$player1, ESS10_QTg2$player2)
ESS10_QTft2 <- as.matrix(ESS10_QTft)
numRows <- nrow(ESS10_QTft2)
numCols <- ncol(ESS10_QTft2)
ESS10_QTft3 <- ESS10_QTft2[c(2:numRows) , c(2:numCols)]
ESS10_QTTable <- graph.adjacency(ESS10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(ESS10_QTTable, vertex.label = V(ESS10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
ESS10_QT.clusterCoef <- transitivity(ESS10_QTTable, type="global") #cluster coefficient
ESS10_QT.degreeCent <- centralization.degree(ESS10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS10_QTftn <- as.network.matrix(ESS10_QTft)
ESS10_QT.netDensity <- network.density(ESS10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS10_QT.entropy <- entropy(ESS10_QTft) #entropy

ESS10_QT.netMx <- cbind(ESS10_QT.netMx, ESS10_QT.clusterCoef, ESS10_QT.degreeCent$centralization,
                        ESS10_QT.netDensity, ESS10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS10_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Goal_F"
FRE10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
FRE10_Gg2 <- data.frame(FRE10_G)
FRE10_Gg2 <- FRE10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_Gg2$player1
player2vector <- FRE10_Gg2$player2
FRE10_Gg3 <- FRE10_Gg2
FRE10_Gg3$p1inp2vec <- is.element(FRE10_Gg3$player1, player2vector)
FRE10_Gg3$p2inp1vec <- is.element(FRE10_Gg3$player2, player1vector)

addPlayer1 <- FRE10_Gg3[ which(FRE10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_Gg3[ which(FRE10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_Gg2 <- rbind(FRE10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
FRE10_Gft <- ftable(FRE10_Gg2$player1, FRE10_Gg2$player2)
FRE10_Gft2 <- as.matrix(FRE10_Gft)
numRows <- nrow(FRE10_Gft2)
numCols <- ncol(FRE10_Gft2)
FRE10_Gft3 <- FRE10_Gft2[c(2:numRows) , c(2:numCols)]
FRE10_GTable <- graph.adjacency(FRE10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(FRE10_GTable, vertex.label = V(FRE10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
FRE10_G.clusterCoef <- transitivity(FRE10_GTable, type="global") #cluster coefficient
FRE10_G.degreeCent <- centralization.degree(FRE10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_Gftn <- as.network.matrix(FRE10_Gft)
FRE10_G.netDensity <- network.density(FRE10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_G.entropy <- entropy(FRE10_Gft) #entropy

FRE10_G.netMx <- cbind(FRE10_G.netMx, FRE10_G.clusterCoef, FRE10_G.degreeCent$centralization,
                       FRE10_G.netDensity, FRE10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Behind_F"
FRE10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
FRE10_Bg2 <- data.frame(FRE10_B)
FRE10_Bg2 <- FRE10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_Bg2$player1
player2vector <- FRE10_Bg2$player2
FRE10_Bg3 <- FRE10_Bg2
FRE10_Bg3$p1inp2vec <- is.element(FRE10_Bg3$player1, player2vector)
FRE10_Bg3$p2inp1vec <- is.element(FRE10_Bg3$player2, player1vector)

addPlayer1 <- FRE10_Bg3[ which(FRE10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_Bg3[ which(FRE10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_Bg2 <- rbind(FRE10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
FRE10_Bft <- ftable(FRE10_Bg2$player1, FRE10_Bg2$player2)
FRE10_Bft2 <- as.matrix(FRE10_Bft)
numRows <- nrow(FRE10_Bft2)
numCols <- ncol(FRE10_Bft2)
FRE10_Bft3 <- FRE10_Bft2[c(2:numRows) , c(2:numCols)]
FRE10_BTable <- graph.adjacency(FRE10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(FRE10_BTable, vertex.label = V(FRE10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
FRE10_B.clusterCoef <- transitivity(FRE10_BTable, type="global") #cluster coefficient
FRE10_B.degreeCent <- centralization.degree(FRE10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_Bftn <- as.network.matrix(FRE10_Bft)
FRE10_B.netDensity <- network.density(FRE10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_B.entropy <- entropy(FRE10_Bft) #entropy

FRE10_B.netMx <- cbind(FRE10_B.netMx, FRE10_B.clusterCoef, FRE10_B.degreeCent$centralization,
                       FRE10_B.netDensity, FRE10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
FRE10_SFg2 <- data.frame(FRE10_SF)
FRE10_SFg2 <- FRE10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_SFg2$player1
player2vector <- FRE10_SFg2$player2
FRE10_SFg3 <- FRE10_SFg2
FRE10_SFg3$p1inp2vec <- is.element(FRE10_SFg3$player1, player2vector)
FRE10_SFg3$p2inp1vec <- is.element(FRE10_SFg3$player2, player1vector)

addPlayer1 <- FRE10_SFg3[ which(FRE10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_SFg3[ which(FRE10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_SFg2 <- rbind(FRE10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
FRE10_SFft <- ftable(FRE10_SFg2$player1, FRE10_SFg2$player2)
FRE10_SFft2 <- as.matrix(FRE10_SFft)
numRows <- nrow(FRE10_SFft2)
numCols <- ncol(FRE10_SFft2)
FRE10_SFft3 <- FRE10_SFft2[c(2:numRows) , c(2:numCols)]
FRE10_SFTable <- graph.adjacency(FRE10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(FRE10_SFTable, vertex.label = V(FRE10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
FRE10_SF.clusterCoef <- transitivity(FRE10_SFTable, type="global") #cluster coefficient
FRE10_SF.degreeCent <- centralization.degree(FRE10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_SFftn <- as.network.matrix(FRE10_SFft)
FRE10_SF.netDensity <- network.density(FRE10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_SF.entropy <- entropy(FRE10_SFft) #entropy

FRE10_SF.netMx <- cbind(FRE10_SF.netMx, FRE10_SF.clusterCoef, FRE10_SF.degreeCent$centralization,
                        FRE10_SF.netDensity, FRE10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
FRE10_TFg2 <- data.frame(FRE10_TF)
FRE10_TFg2 <- FRE10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_TFg2$player1
player2vector <- FRE10_TFg2$player2
FRE10_TFg3 <- FRE10_TFg2
FRE10_TFg3$p1inp2vec <- is.element(FRE10_TFg3$player1, player2vector)
FRE10_TFg3$p2inp1vec <- is.element(FRE10_TFg3$player2, player1vector)

addPlayer1 <- FRE10_TFg3[ which(FRE10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_TFg3[ which(FRE10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_TFg2 <- rbind(FRE10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
FRE10_TFft <- ftable(FRE10_TFg2$player1, FRE10_TFg2$player2)
FRE10_TFft2 <- as.matrix(FRE10_TFft)
numRows <- nrow(FRE10_TFft2)
numCols <- ncol(FRE10_TFft2)
FRE10_TFft3 <- FRE10_TFft2[c(2:numRows) , c(2:numCols)]
FRE10_TFTable <- graph.adjacency(FRE10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(FRE10_TFTable, vertex.label = V(FRE10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
FRE10_TF.clusterCoef <- transitivity(FRE10_TFTable, type="global") #cluster coefficient
FRE10_TF.degreeCent <- centralization.degree(FRE10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_TFftn <- as.network.matrix(FRE10_TFft)
FRE10_TF.netDensity <- network.density(FRE10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_TF.entropy <- entropy(FRE10_TFft) #entropy

FRE10_TF.netMx <- cbind(FRE10_TF.netMx, FRE10_TF.clusterCoef, FRE10_TF.degreeCent$centralization,
                        FRE10_TF.netDensity, FRE10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
FRE10_SAMg2 <- data.frame(FRE10_SAM)
FRE10_SAMg2 <- FRE10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_SAMg2$player1
player2vector <- FRE10_SAMg2$player2
FRE10_SAMg3 <- FRE10_SAMg2
FRE10_SAMg3$p1inp2vec <- is.element(FRE10_SAMg3$player1, player2vector)
FRE10_SAMg3$p2inp1vec <- is.element(FRE10_SAMg3$player2, player1vector)

addPlayer1 <- FRE10_SAMg3[ which(FRE10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_SAMg3[ which(FRE10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_SAMg2 <- rbind(FRE10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
FRE10_SAMft <- ftable(FRE10_SAMg2$player1, FRE10_SAMg2$player2)
FRE10_SAMft2 <- as.matrix(FRE10_SAMft)
numRows <- nrow(FRE10_SAMft2)
numCols <- ncol(FRE10_SAMft2)
FRE10_SAMft3 <- FRE10_SAMft2[c(2:numRows) , c(2:numCols)]
FRE10_SAMTable <- graph.adjacency(FRE10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(FRE10_SAMTable, vertex.label = V(FRE10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
FRE10_SAM.clusterCoef <- transitivity(FRE10_SAMTable, type="global") #cluster coefficient
FRE10_SAM.degreeCent <- centralization.degree(FRE10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_SAMftn <- as.network.matrix(FRE10_SAMft)
FRE10_SAM.netDensity <- network.density(FRE10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_SAM.entropy <- entropy(FRE10_SAMft) #entropy

FRE10_SAM.netMx <- cbind(FRE10_SAM.netMx, FRE10_SAM.clusterCoef, FRE10_SAM.degreeCent$centralization,
                         FRE10_SAM.netDensity, FRE10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
FRE10_TAMg2 <- data.frame(FRE10_TAM)
FRE10_TAMg2 <- FRE10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_TAMg2$player1
player2vector <- FRE10_TAMg2$player2
FRE10_TAMg3 <- FRE10_TAMg2
FRE10_TAMg3$p1inp2vec <- is.element(FRE10_TAMg3$player1, player2vector)
FRE10_TAMg3$p2inp1vec <- is.element(FRE10_TAMg3$player2, player1vector)

addPlayer1 <- FRE10_TAMg3[ which(FRE10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_TAMg3[ which(FRE10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_TAMg2 <- rbind(FRE10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
FRE10_TAMft <- ftable(FRE10_TAMg2$player1, FRE10_TAMg2$player2)
FRE10_TAMft2 <- as.matrix(FRE10_TAMft)
numRows <- nrow(FRE10_TAMft2)
numCols <- ncol(FRE10_TAMft2)
FRE10_TAMft3 <- FRE10_TAMft2[c(2:numRows) , c(2:numCols)]
FRE10_TAMTable <- graph.adjacency(FRE10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(FRE10_TAMTable, vertex.label = V(FRE10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
FRE10_TAM.clusterCoef <- transitivity(FRE10_TAMTable, type="global") #cluster coefficient
FRE10_TAM.degreeCent <- centralization.degree(FRE10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_TAMftn <- as.network.matrix(FRE10_TAMft)
FRE10_TAM.netDensity <- network.density(FRE10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_TAM.entropy <- entropy(FRE10_TAMft) #entropy

FRE10_TAM.netMx <- cbind(FRE10_TAM.netMx, FRE10_TAM.clusterCoef, FRE10_TAM.degreeCent$centralization,
                         FRE10_TAM.netDensity, FRE10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
FRE10_SDMg2 <- data.frame(FRE10_SDM)
FRE10_SDMg2 <- FRE10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_SDMg2$player1
player2vector <- FRE10_SDMg2$player2
FRE10_SDMg3 <- FRE10_SDMg2
FRE10_SDMg3$p1inp2vec <- is.element(FRE10_SDMg3$player1, player2vector)
FRE10_SDMg3$p2inp1vec <- is.element(FRE10_SDMg3$player2, player1vector)

addPlayer1 <- FRE10_SDMg3[ which(FRE10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_SDMg3[ which(FRE10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_SDMg2 <- rbind(FRE10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
FRE10_SDMft <- ftable(FRE10_SDMg2$player1, FRE10_SDMg2$player2)
FRE10_SDMft2 <- as.matrix(FRE10_SDMft)
numRows <- nrow(FRE10_SDMft2)
numCols <- ncol(FRE10_SDMft2)
FRE10_SDMft3 <- FRE10_SDMft2[c(2:numRows) , c(2:numCols)]
FRE10_SDMTable <- graph.adjacency(FRE10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(FRE10_SDMTable, vertex.label = V(FRE10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
FRE10_SDM.clusterCoef <- transitivity(FRE10_SDMTable, type="global") #cluster coefficient
FRE10_SDM.degreeCent <- centralization.degree(FRE10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_SDMftn <- as.network.matrix(FRE10_SDMft)
FRE10_SDM.netDensity <- network.density(FRE10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_SDM.entropy <- entropy(FRE10_SDMft) #entropy

FRE10_SDM.netMx <- cbind(FRE10_SDM.netMx, FRE10_SDM.clusterCoef, FRE10_SDM.degreeCent$centralization,
                         FRE10_SDM.netDensity, FRE10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
FRE10_TDMg2 <- data.frame(FRE10_TDM)
FRE10_TDMg2 <- FRE10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_TDMg2$player1
player2vector <- FRE10_TDMg2$player2
FRE10_TDMg3 <- FRE10_TDMg2
FRE10_TDMg3$p1inp2vec <- is.element(FRE10_TDMg3$player1, player2vector)
FRE10_TDMg3$p2inp1vec <- is.element(FRE10_TDMg3$player2, player1vector)

addPlayer1 <- FRE10_TDMg3[ which(FRE10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE10_TDMg3[ which(FRE10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_TDMg2 <- rbind(FRE10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
FRE10_TDMft <- ftable(FRE10_TDMg2$player1, FRE10_TDMg2$player2)
FRE10_TDMft2 <- as.matrix(FRE10_TDMft)
numRows <- nrow(FRE10_TDMft2)
numCols <- ncol(FRE10_TDMft2)
FRE10_TDMft3 <- FRE10_TDMft2[c(2:numRows) , c(2:numCols)]
FRE10_TDMTable <- graph.adjacency(FRE10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(FRE10_TDMTable, vertex.label = V(FRE10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
FRE10_TDM.clusterCoef <- transitivity(FRE10_TDMTable, type="global") #cluster coefficient
FRE10_TDM.degreeCent <- centralization.degree(FRE10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_TDMftn <- as.network.matrix(FRE10_TDMft)
FRE10_TDM.netDensity <- network.density(FRE10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_TDM.entropy <- entropy(FRE10_TDMft) #entropy

FRE10_TDM.netMx <- cbind(FRE10_TDM.netMx, FRE10_TDM.clusterCoef, FRE10_TDM.degreeCent$centralization,
                         FRE10_TDM.netDensity, FRE10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************

round = 10
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
FRE10_SDg2 <- data.frame(FRE10_SD)
FRE10_SDg2 <- FRE10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_SDg2$player1
player2vector <- FRE10_SDg2$player2
FRE10_SDg3 <- FRE10_SDg2
FRE10_SDg3$p1inp2vec <- is.element(FRE10_SDg3$player1, player2vector)
FRE10_SDg3$p2inp1vec <- is.element(FRE10_SDg3$player2, player1vector)

addPlayer1 <- FRE10_SDg3[ which(FRE10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_SDg3[ which(FRE10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_SDg2 <- rbind(FRE10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
FRE10_SDft <- ftable(FRE10_SDg2$player1, FRE10_SDg2$player2)
FRE10_SDft2 <- as.matrix(FRE10_SDft)
numRows <- nrow(FRE10_SDft2)
numCols <- ncol(FRE10_SDft2)
FRE10_SDft3 <- FRE10_SDft2[c(2:numRows) , c(2:numCols)]
FRE10_SDTable <- graph.adjacency(FRE10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(FRE10_SDTable, vertex.label = V(FRE10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
FRE10_SD.clusterCoef <- transitivity(FRE10_SDTable, type="global") #cluster coefficient
FRE10_SD.degreeCent <- centralization.degree(FRE10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_SDftn <- as.network.matrix(FRE10_SDft)
FRE10_SD.netDensity <- network.density(FRE10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_SD.entropy <- entropy(FRE10_SDft) #entropy

FRE10_SD.netMx <- cbind(FRE10_SD.netMx, FRE10_SD.clusterCoef, FRE10_SD.degreeCent$centralization,
                        FRE10_SD.netDensity, FRE10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
FRE10_TDg2 <- data.frame(FRE10_TD)
FRE10_TDg2 <- FRE10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_TDg2$player1
player2vector <- FRE10_TDg2$player2
FRE10_TDg3 <- FRE10_TDg2
FRE10_TDg3$p1inp2vec <- is.element(FRE10_TDg3$player1, player2vector)
FRE10_TDg3$p2inp1vec <- is.element(FRE10_TDg3$player2, player1vector)

addPlayer1 <- FRE10_TDg3[ which(FRE10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_TDg3[ which(FRE10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_TDg2 <- rbind(FRE10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
FRE10_TDft <- ftable(FRE10_TDg2$player1, FRE10_TDg2$player2)
FRE10_TDft2 <- as.matrix(FRE10_TDft)
numRows <- nrow(FRE10_TDft2)
numCols <- ncol(FRE10_TDft2)
FRE10_TDft3 <- FRE10_TDft2[c(2:numRows) , c(2:numCols)]
FRE10_TDTable <- graph.adjacency(FRE10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(FRE10_TDTable, vertex.label = V(FRE10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
FRE10_TD.clusterCoef <- transitivity(FRE10_TDTable, type="global") #cluster coefficient
FRE10_TD.degreeCent <- centralization.degree(FRE10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_TDftn <- as.network.matrix(FRE10_TDft)
FRE10_TD.netDensity <- network.density(FRE10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_TD.entropy <- entropy(FRE10_TDft) #entropy

FRE10_TD.netMx <- cbind(FRE10_TD.netMx, FRE10_TD.clusterCoef, FRE10_TD.degreeCent$centralization,
                        FRE10_TD.netDensity, FRE10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
FRE10_QTg2 <- data.frame(FRE10_QT)
FRE10_QTg2 <- FRE10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE10_QTg2$player1
player2vector <- FRE10_QTg2$player2
FRE10_QTg3 <- FRE10_QTg2
FRE10_QTg3$p1inp2vec <- is.element(FRE10_QTg3$player1, player2vector)
FRE10_QTg3$p2inp1vec <- is.element(FRE10_QTg3$player2, player1vector)

addPlayer1 <- FRE10_QTg3[ which(FRE10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE10_QTg3[ which(FRE10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE10_QTg2 <- rbind(FRE10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
FRE10_QTft <- ftable(FRE10_QTg2$player1, FRE10_QTg2$player2)
FRE10_QTft2 <- as.matrix(FRE10_QTft)
numRows <- nrow(FRE10_QTft2)
numCols <- ncol(FRE10_QTft2)
FRE10_QTft3 <- FRE10_QTft2[c(2:numRows) , c(2:numCols)]
FRE10_QTTable <- graph.adjacency(FRE10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(FRE10_QTTable, vertex.label = V(FRE10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
FRE10_QT.clusterCoef <- transitivity(FRE10_QTTable, type="global") #cluster coefficient
FRE10_QT.degreeCent <- centralization.degree(FRE10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE10_QTftn <- as.network.matrix(FRE10_QTft)
FRE10_QT.netDensity <- network.density(FRE10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE10_QT.entropy <- entropy(FRE10_QTft) #entropy

FRE10_QT.netMx <- cbind(FRE10_QT.netMx, FRE10_QT.clusterCoef, FRE10_QT.degreeCent$centralization,
                        FRE10_QT.netDensity, FRE10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE10_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
GCFC10_Gg2 <- data.frame(GCFC10_G)
GCFC10_Gg2 <- GCFC10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_Gg2$player1
player2vector <- GCFC10_Gg2$player2
GCFC10_Gg3 <- GCFC10_Gg2
GCFC10_Gg3$p1inp2vec <- is.element(GCFC10_Gg3$player1, player2vector)
GCFC10_Gg3$p2inp1vec <- is.element(GCFC10_Gg3$player2, player1vector)

addPlayer1 <- GCFC10_Gg3[ which(GCFC10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_Gg3[ which(GCFC10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_Gg2 <- rbind(GCFC10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
GCFC10_Gft <- ftable(GCFC10_Gg2$player1, GCFC10_Gg2$player2)
GCFC10_Gft2 <- as.matrix(GCFC10_Gft)
numRows <- nrow(GCFC10_Gft2)
numCols <- ncol(GCFC10_Gft2)
GCFC10_Gft3 <- GCFC10_Gft2[c(2:numRows) , c(2:numCols)]
GCFC10_GTable <- graph.adjacency(GCFC10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(GCFC10_GTable, vertex.label = V(GCFC10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
GCFC10_G.clusterCoef <- transitivity(GCFC10_GTable, type="global") #cluster coefficient
GCFC10_G.degreeCent <- centralization.degree(GCFC10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_Gftn <- as.network.matrix(GCFC10_Gft)
GCFC10_G.netDensity <- network.density(GCFC10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_G.entropy <- entropy(GCFC10_Gft) #entropy

GCFC10_G.netMx <- cbind(GCFC10_G.netMx, GCFC10_G.clusterCoef, GCFC10_G.degreeCent$centralization,
                        GCFC10_G.netDensity, GCFC10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
GCFC10_Bg2 <- data.frame(GCFC10_B)
GCFC10_Bg2 <- GCFC10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_Bg2$player1
player2vector <- GCFC10_Bg2$player2
GCFC10_Bg3 <- GCFC10_Bg2
GCFC10_Bg3$p1inp2vec <- is.element(GCFC10_Bg3$player1, player2vector)
GCFC10_Bg3$p2inp1vec <- is.element(GCFC10_Bg3$player2, player1vector)

addPlayer1 <- GCFC10_Bg3[ which(GCFC10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_Bg3[ which(GCFC10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_Bg2 <- rbind(GCFC10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
GCFC10_Bft <- ftable(GCFC10_Bg2$player1, GCFC10_Bg2$player2)
GCFC10_Bft2 <- as.matrix(GCFC10_Bft)
numRows <- nrow(GCFC10_Bft2)
numCols <- ncol(GCFC10_Bft2)
GCFC10_Bft3 <- GCFC10_Bft2[c(2:numRows) , c(2:numCols)]
GCFC10_BTable <- graph.adjacency(GCFC10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(GCFC10_BTable, vertex.label = V(GCFC10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
GCFC10_B.clusterCoef <- transitivity(GCFC10_BTable, type="global") #cluster coefficient
GCFC10_B.degreeCent <- centralization.degree(GCFC10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_Bftn <- as.network.matrix(GCFC10_Bft)
GCFC10_B.netDensity <- network.density(GCFC10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_B.entropy <- entropy(GCFC10_Bft) #entropy

GCFC10_B.netMx <- cbind(GCFC10_B.netMx, GCFC10_B.clusterCoef, GCFC10_B.degreeCent$centralization,
                        GCFC10_B.netDensity, GCFC10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
GCFC10_SFg2 <- data.frame(GCFC10_SF)
GCFC10_SFg2 <- GCFC10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_SFg2$player1
player2vector <- GCFC10_SFg2$player2
GCFC10_SFg3 <- GCFC10_SFg2
GCFC10_SFg3$p1inp2vec <- is.element(GCFC10_SFg3$player1, player2vector)
GCFC10_SFg3$p2inp1vec <- is.element(GCFC10_SFg3$player2, player1vector)

addPlayer1 <- GCFC10_SFg3[ which(GCFC10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC10_SFg2 <- rbind(GCFC10_SFg2, addPlayer1)

#ROUND 10, FWD Stoppage graph using weighted edges
GCFC10_SFft <- ftable(GCFC10_SFg2$player1, GCFC10_SFg2$player2)
GCFC10_SFft2 <- as.matrix(GCFC10_SFft)
numRows <- nrow(GCFC10_SFft2)
numCols <- ncol(GCFC10_SFft2)
GCFC10_SFft3 <- GCFC10_SFft2[c(2:numRows) , c(1:numCols)]
GCFC10_SFTable <- graph.adjacency(GCFC10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(GCFC10_SFTable, vertex.label = V(GCFC10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
GCFC10_SF.clusterCoef <- transitivity(GCFC10_SFTable, type="global") #cluster coefficient
GCFC10_SF.degreeCent <- centralization.degree(GCFC10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_SFftn <- as.network.matrix(GCFC10_SFft)
GCFC10_SF.netDensity <- network.density(GCFC10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_SF.entropy <- entropy(GCFC10_SFft) #entropy

GCFC10_SF.netMx <- cbind(GCFC10_SF.netMx, GCFC10_SF.clusterCoef, GCFC10_SF.degreeCent$centralization,
                         GCFC10_SF.netDensity, GCFC10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
GCFC10_TFg2 <- data.frame(GCFC10_TF)
GCFC10_TFg2 <- GCFC10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_TFg2$player1
player2vector <- GCFC10_TFg2$player2
GCFC10_TFg3 <- GCFC10_TFg2
GCFC10_TFg3$p1inp2vec <- is.element(GCFC10_TFg3$player1, player2vector)
GCFC10_TFg3$p2inp1vec <- is.element(GCFC10_TFg3$player2, player1vector)

addPlayer1 <- GCFC10_TFg3[ which(GCFC10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_TFg3[ which(GCFC10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_TFg2 <- rbind(GCFC10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
GCFC10_TFft <- ftable(GCFC10_TFg2$player1, GCFC10_TFg2$player2)
GCFC10_TFft2 <- as.matrix(GCFC10_TFft)
numRows <- nrow(GCFC10_TFft2)
numCols <- ncol(GCFC10_TFft2)
GCFC10_TFft3 <- GCFC10_TFft2[c(2:numRows) , c(2:numCols)]
GCFC10_TFTable <- graph.adjacency(GCFC10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(GCFC10_TFTable, vertex.label = V(GCFC10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
GCFC10_TF.clusterCoef <- transitivity(GCFC10_TFTable, type="global") #cluster coefficient
GCFC10_TF.degreeCent <- centralization.degree(GCFC10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_TFftn <- as.network.matrix(GCFC10_TFft)
GCFC10_TF.netDensity <- network.density(GCFC10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_TF.entropy <- entropy(GCFC10_TFft) #entropy

GCFC10_TF.netMx <- cbind(GCFC10_TF.netMx, GCFC10_TF.clusterCoef, GCFC10_TF.degreeCent$centralization,
                         GCFC10_TF.netDensity, GCFC10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
GCFC10_SAMg2 <- data.frame(GCFC10_SAM)
GCFC10_SAMg2 <- GCFC10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_SAMg2$player1
player2vector <- GCFC10_SAMg2$player2
GCFC10_SAMg3 <- GCFC10_SAMg2
GCFC10_SAMg3$p1inp2vec <- is.element(GCFC10_SAMg3$player1, player2vector)
GCFC10_SAMg3$p2inp1vec <- is.element(GCFC10_SAMg3$player2, player1vector)

addPlayer1 <- GCFC10_SAMg3[ which(GCFC10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_SAMg3[ which(GCFC10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_SAMg2 <- rbind(GCFC10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
GCFC10_SAMft <- ftable(GCFC10_SAMg2$player1, GCFC10_SAMg2$player2)
GCFC10_SAMft2 <- as.matrix(GCFC10_SAMft)
numRows <- nrow(GCFC10_SAMft2)
numCols <- ncol(GCFC10_SAMft2)
GCFC10_SAMft3 <- GCFC10_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC10_SAMTable <- graph.adjacency(GCFC10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(GCFC10_SAMTable, vertex.label = V(GCFC10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
GCFC10_SAM.clusterCoef <- transitivity(GCFC10_SAMTable, type="global") #cluster coefficient
GCFC10_SAM.degreeCent <- centralization.degree(GCFC10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_SAMftn <- as.network.matrix(GCFC10_SAMft)
GCFC10_SAM.netDensity <- network.density(GCFC10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_SAM.entropy <- entropy(GCFC10_SAMft) #entropy

GCFC10_SAM.netMx <- cbind(GCFC10_SAM.netMx, GCFC10_SAM.clusterCoef, GCFC10_SAM.degreeCent$centralization,
                          GCFC10_SAM.netDensity, GCFC10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
GCFC10_TAMg2 <- data.frame(GCFC10_TAM)
GCFC10_TAMg2 <- GCFC10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_TAMg2$player1
player2vector <- GCFC10_TAMg2$player2
GCFC10_TAMg3 <- GCFC10_TAMg2
GCFC10_TAMg3$p1inp2vec <- is.element(GCFC10_TAMg3$player1, player2vector)
GCFC10_TAMg3$p2inp1vec <- is.element(GCFC10_TAMg3$player2, player1vector)

addPlayer1 <- GCFC10_TAMg3[ which(GCFC10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_TAMg3[ which(GCFC10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_TAMg2 <- rbind(GCFC10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
GCFC10_TAMft <- ftable(GCFC10_TAMg2$player1, GCFC10_TAMg2$player2)
GCFC10_TAMft2 <- as.matrix(GCFC10_TAMft)
numRows <- nrow(GCFC10_TAMft2)
numCols <- ncol(GCFC10_TAMft2)
GCFC10_TAMft3 <- GCFC10_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC10_TAMTable <- graph.adjacency(GCFC10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(GCFC10_TAMTable, vertex.label = V(GCFC10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
GCFC10_TAM.clusterCoef <- transitivity(GCFC10_TAMTable, type="global") #cluster coefficient
GCFC10_TAM.degreeCent <- centralization.degree(GCFC10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_TAMftn <- as.network.matrix(GCFC10_TAMft)
GCFC10_TAM.netDensity <- network.density(GCFC10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_TAM.entropy <- entropy(GCFC10_TAMft) #entropy

GCFC10_TAM.netMx <- cbind(GCFC10_TAM.netMx, GCFC10_TAM.clusterCoef, GCFC10_TAM.degreeCent$centralization,
                          GCFC10_TAM.netDensity, GCFC10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
GCFC10_SDMg2 <- data.frame(GCFC10_SDM)
GCFC10_SDMg2 <- GCFC10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_SDMg2$player1
player2vector <- GCFC10_SDMg2$player2
GCFC10_SDMg3 <- GCFC10_SDMg2
GCFC10_SDMg3$p1inp2vec <- is.element(GCFC10_SDMg3$player1, player2vector)
GCFC10_SDMg3$p2inp1vec <- is.element(GCFC10_SDMg3$player2, player1vector)

addPlayer1 <- GCFC10_SDMg3[ which(GCFC10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_SDMg3[ which(GCFC10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_SDMg2 <- rbind(GCFC10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
GCFC10_SDMft <- ftable(GCFC10_SDMg2$player1, GCFC10_SDMg2$player2)
GCFC10_SDMft2 <- as.matrix(GCFC10_SDMft)
numRows <- nrow(GCFC10_SDMft2)
numCols <- ncol(GCFC10_SDMft2)
GCFC10_SDMft3 <- GCFC10_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC10_SDMTable <- graph.adjacency(GCFC10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(GCFC10_SDMTable, vertex.label = V(GCFC10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
GCFC10_SDM.clusterCoef <- transitivity(GCFC10_SDMTable, type="global") #cluster coefficient
GCFC10_SDM.degreeCent <- centralization.degree(GCFC10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_SDMftn <- as.network.matrix(GCFC10_SDMft)
GCFC10_SDM.netDensity <- network.density(GCFC10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_SDM.entropy <- entropy(GCFC10_SDMft) #entropy

GCFC10_SDM.netMx <- cbind(GCFC10_SDM.netMx, GCFC10_SDM.clusterCoef, GCFC10_SDM.degreeCent$centralization,
                          GCFC10_SDM.netDensity, GCFC10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
GCFC10_TDMg2 <- data.frame(GCFC10_TDM)
GCFC10_TDMg2 <- GCFC10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_TDMg2$player1
player2vector <- GCFC10_TDMg2$player2
GCFC10_TDMg3 <- GCFC10_TDMg2
GCFC10_TDMg3$p1inp2vec <- is.element(GCFC10_TDMg3$player1, player2vector)
GCFC10_TDMg3$p2inp1vec <- is.element(GCFC10_TDMg3$player2, player1vector)

addPlayer1 <- GCFC10_TDMg3[ which(GCFC10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_TDMg3[ which(GCFC10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_TDMg2 <- rbind(GCFC10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
GCFC10_TDMft <- ftable(GCFC10_TDMg2$player1, GCFC10_TDMg2$player2)
GCFC10_TDMft2 <- as.matrix(GCFC10_TDMft)
numRows <- nrow(GCFC10_TDMft2)
numCols <- ncol(GCFC10_TDMft2)
GCFC10_TDMft3 <- GCFC10_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC10_TDMTable <- graph.adjacency(GCFC10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(GCFC10_TDMTable, vertex.label = V(GCFC10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
GCFC10_TDM.clusterCoef <- transitivity(GCFC10_TDMTable, type="global") #cluster coefficient
GCFC10_TDM.degreeCent <- centralization.degree(GCFC10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_TDMftn <- as.network.matrix(GCFC10_TDMft)
GCFC10_TDM.netDensity <- network.density(GCFC10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_TDM.entropy <- entropy(GCFC10_TDMft) #entropy

GCFC10_TDM.netMx <- cbind(GCFC10_TDM.netMx, GCFC10_TDM.clusterCoef, GCFC10_TDM.degreeCent$centralization,
                          GCFC10_TDM.netDensity, GCFC10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
GCFC10_SDg2 <- data.frame(GCFC10_SD)
GCFC10_SDg2 <- GCFC10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_SDg2$player1
player2vector <- GCFC10_SDg2$player2
GCFC10_SDg3 <- GCFC10_SDg2
GCFC10_SDg3$p1inp2vec <- is.element(GCFC10_SDg3$player1, player2vector)
GCFC10_SDg3$p2inp1vec <- is.element(GCFC10_SDg3$player2, player1vector)

addPlayer1 <- GCFC10_SDg3[ which(GCFC10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_SDg3[ which(GCFC10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_SDg2 <- rbind(GCFC10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
GCFC10_SDft <- ftable(GCFC10_SDg2$player1, GCFC10_SDg2$player2)
GCFC10_SDft2 <- as.matrix(GCFC10_SDft)
numRows <- nrow(GCFC10_SDft2)
numCols <- ncol(GCFC10_SDft2)
GCFC10_SDft3 <- GCFC10_SDft2[c(2:numRows) , c(2:numCols)]
GCFC10_SDTable <- graph.adjacency(GCFC10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(GCFC10_SDTable, vertex.label = V(GCFC10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
GCFC10_SD.clusterCoef <- transitivity(GCFC10_SDTable, type="global") #cluster coefficient
GCFC10_SD.degreeCent <- centralization.degree(GCFC10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_SDftn <- as.network.matrix(GCFC10_SDft)
GCFC10_SD.netDensity <- network.density(GCFC10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_SD.entropy <- entropy(GCFC10_SDft) #entropy

GCFC10_SD.netMx <- cbind(GCFC10_SD.netMx, GCFC10_SD.clusterCoef, GCFC10_SD.degreeCent$centralization,
                         GCFC10_SD.netDensity, GCFC10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
GCFC10_TDg2 <- data.frame(GCFC10_TD)
GCFC10_TDg2 <- GCFC10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_TDg2$player1
player2vector <- GCFC10_TDg2$player2
GCFC10_TDg3 <- GCFC10_TDg2
GCFC10_TDg3$p1inp2vec <- is.element(GCFC10_TDg3$player1, player2vector)
GCFC10_TDg3$p2inp1vec <- is.element(GCFC10_TDg3$player2, player1vector)

addPlayer1 <- GCFC10_TDg3[ which(GCFC10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_TDg3[ which(GCFC10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_TDg2 <- rbind(GCFC10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
GCFC10_TDft <- ftable(GCFC10_TDg2$player1, GCFC10_TDg2$player2)
GCFC10_TDft2 <- as.matrix(GCFC10_TDft)
numRows <- nrow(GCFC10_TDft2)
numCols <- ncol(GCFC10_TDft2)
GCFC10_TDft3 <- GCFC10_TDft2[c(2:numRows) , c(2:numCols)]
GCFC10_TDTable <- graph.adjacency(GCFC10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(GCFC10_TDTable, vertex.label = V(GCFC10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
GCFC10_TD.clusterCoef <- transitivity(GCFC10_TDTable, type="global") #cluster coefficient
GCFC10_TD.degreeCent <- centralization.degree(GCFC10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_TDftn <- as.network.matrix(GCFC10_TDft)
GCFC10_TD.netDensity <- network.density(GCFC10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_TD.entropy <- entropy(GCFC10_TDft) #entropy

GCFC10_TD.netMx <- cbind(GCFC10_TD.netMx, GCFC10_TD.clusterCoef, GCFC10_TD.degreeCent$centralization,
                         GCFC10_TD.netDensity, GCFC10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
GCFC10_QTg2 <- data.frame(GCFC10_QT)
GCFC10_QTg2 <- GCFC10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC10_QTg2$player1
player2vector <- GCFC10_QTg2$player2
GCFC10_QTg3 <- GCFC10_QTg2
GCFC10_QTg3$p1inp2vec <- is.element(GCFC10_QTg3$player1, player2vector)
GCFC10_QTg3$p2inp1vec <- is.element(GCFC10_QTg3$player2, player1vector)

addPlayer1 <- GCFC10_QTg3[ which(GCFC10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC10_QTg3[ which(GCFC10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC10_QTg2 <- rbind(GCFC10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
GCFC10_QTft <- ftable(GCFC10_QTg2$player1, GCFC10_QTg2$player2)
GCFC10_QTft2 <- as.matrix(GCFC10_QTft)
numRows <- nrow(GCFC10_QTft2)
numCols <- ncol(GCFC10_QTft2)
GCFC10_QTft3 <- GCFC10_QTft2[c(2:numRows) , c(2:numCols)]
GCFC10_QTTable <- graph.adjacency(GCFC10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(GCFC10_QTTable, vertex.label = V(GCFC10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
GCFC10_QT.clusterCoef <- transitivity(GCFC10_QTTable, type="global") #cluster coefficient
GCFC10_QT.degreeCent <- centralization.degree(GCFC10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC10_QTftn <- as.network.matrix(GCFC10_QTft)
GCFC10_QT.netDensity <- network.density(GCFC10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC10_QT.entropy <- entropy(GCFC10_QTft) #entropy

GCFC10_QT.netMx <- cbind(GCFC10_QT.netMx, GCFC10_QT.clusterCoef, GCFC10_QT.degreeCent$centralization,
                         GCFC10_QT.netDensity, GCFC10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC10_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
GEEL10_Gg2 <- data.frame(GEEL10_G)
GEEL10_Gg2 <- GEEL10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_Gg2$player1
player2vector <- GEEL10_Gg2$player2
GEEL10_Gg3 <- GEEL10_Gg2
GEEL10_Gg3$p1inp2vec <- is.element(GEEL10_Gg3$player1, player2vector)
GEEL10_Gg3$p2inp1vec <- is.element(GEEL10_Gg3$player2, player1vector)

addPlayer1 <- GEEL10_Gg3[ which(GEEL10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_Gg3[ which(GEEL10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_Gg2 <- rbind(GEEL10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
GEEL10_Gft <- ftable(GEEL10_Gg2$player1, GEEL10_Gg2$player2)
GEEL10_Gft2 <- as.matrix(GEEL10_Gft)
numRows <- nrow(GEEL10_Gft2)
numCols <- ncol(GEEL10_Gft2)
GEEL10_Gft3 <- GEEL10_Gft2[c(2:numRows) , c(2:numCols)]
GEEL10_GTable <- graph.adjacency(GEEL10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(GEEL10_GTable, vertex.label = V(GEEL10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
GEEL10_G.clusterCoef <- transitivity(GEEL10_GTable, type="global") #cluster coefficient
GEEL10_G.degreeCent <- centralization.degree(GEEL10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_Gftn <- as.network.matrix(GEEL10_Gft)
GEEL10_G.netDensity <- network.density(GEEL10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_G.entropy <- entropy(GEEL10_Gft) #entropy

GEEL10_G.netMx <- cbind(GEEL10_G.netMx, GEEL10_G.clusterCoef, GEEL10_G.degreeCent$centralization,
                        GEEL10_G.netDensity, GEEL10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
GEEL10_Bg2 <- data.frame(GEEL10_B)
GEEL10_Bg2 <- GEEL10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_Bg2$player1
player2vector <- GEEL10_Bg2$player2
GEEL10_Bg3 <- GEEL10_Bg2
GEEL10_Bg3$p1inp2vec <- is.element(GEEL10_Bg3$player1, player2vector)
GEEL10_Bg3$p2inp1vec <- is.element(GEEL10_Bg3$player2, player1vector)

addPlayer1 <- GEEL10_Bg3[ which(GEEL10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_Bg3[ which(GEEL10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_Bg2 <- rbind(GEEL10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
GEEL10_Bft <- ftable(GEEL10_Bg2$player1, GEEL10_Bg2$player2)
GEEL10_Bft2 <- as.matrix(GEEL10_Bft)
numRows <- nrow(GEEL10_Bft2)
numCols <- ncol(GEEL10_Bft2)
GEEL10_Bft3 <- GEEL10_Bft2[c(2:numRows) , c(2:numCols)]
GEEL10_BTable <- graph.adjacency(GEEL10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(GEEL10_BTable, vertex.label = V(GEEL10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
GEEL10_B.clusterCoef <- transitivity(GEEL10_BTable, type="global") #cluster coefficient
GEEL10_B.degreeCent <- centralization.degree(GEEL10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_Bftn <- as.network.matrix(GEEL10_Bft)
GEEL10_B.netDensity <- network.density(GEEL10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_B.entropy <- entropy(GEEL10_Bft) #entropy

GEEL10_B.netMx <- cbind(GEEL10_B.netMx, GEEL10_B.clusterCoef, GEEL10_B.degreeCent$centralization,
                        GEEL10_B.netDensity, GEEL10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
GEEL10_SFg2 <- data.frame(GEEL10_SF)
GEEL10_SFg2 <- GEEL10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_SFg2$player1
player2vector <- GEEL10_SFg2$player2
GEEL10_SFg3 <- GEEL10_SFg2
GEEL10_SFg3$p1inp2vec <- is.element(GEEL10_SFg3$player1, player2vector)
GEEL10_SFg3$p2inp1vec <- is.element(GEEL10_SFg3$player2, player1vector)

addPlayer1 <- GEEL10_SFg3[ which(GEEL10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_SFg3[ which(GEEL10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_SFg2 <- rbind(GEEL10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
GEEL10_SFft <- ftable(GEEL10_SFg2$player1, GEEL10_SFg2$player2)
GEEL10_SFft2 <- as.matrix(GEEL10_SFft)
numRows <- nrow(GEEL10_SFft2)
numCols <- ncol(GEEL10_SFft2)
GEEL10_SFft3 <- GEEL10_SFft2[c(2:numRows) , c(2:numCols)]
GEEL10_SFTable <- graph.adjacency(GEEL10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(GEEL10_SFTable, vertex.label = V(GEEL10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
GEEL10_SF.clusterCoef <- transitivity(GEEL10_SFTable, type="global") #cluster coefficient
GEEL10_SF.degreeCent <- centralization.degree(GEEL10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_SFftn <- as.network.matrix(GEEL10_SFft)
GEEL10_SF.netDensity <- network.density(GEEL10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_SF.entropy <- entropy(GEEL10_SFft) #entropy

GEEL10_SF.netMx <- cbind(GEEL10_SF.netMx, GEEL10_SF.clusterCoef, GEEL10_SF.degreeCent$centralization,
                         GEEL10_SF.netDensity, GEEL10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
GEEL10_TFg2 <- data.frame(GEEL10_TF)
GEEL10_TFg2 <- GEEL10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_TFg2$player1
player2vector <- GEEL10_TFg2$player2
GEEL10_TFg3 <- GEEL10_TFg2
GEEL10_TFg3$p1inp2vec <- is.element(GEEL10_TFg3$player1, player2vector)
GEEL10_TFg3$p2inp1vec <- is.element(GEEL10_TFg3$player2, player1vector)

addPlayer1 <- GEEL10_TFg3[ which(GEEL10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_TFg3[ which(GEEL10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_TFg2 <- rbind(GEEL10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
GEEL10_TFft <- ftable(GEEL10_TFg2$player1, GEEL10_TFg2$player2)
GEEL10_TFft2 <- as.matrix(GEEL10_TFft)
numRows <- nrow(GEEL10_TFft2)
numCols <- ncol(GEEL10_TFft2)
GEEL10_TFft3 <- GEEL10_TFft2[c(2:numRows) , c(2:numCols)]
GEEL10_TFTable <- graph.adjacency(GEEL10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(GEEL10_TFTable, vertex.label = V(GEEL10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
GEEL10_TF.clusterCoef <- transitivity(GEEL10_TFTable, type="global") #cluster coefficient
GEEL10_TF.degreeCent <- centralization.degree(GEEL10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_TFftn <- as.network.matrix(GEEL10_TFft)
GEEL10_TF.netDensity <- network.density(GEEL10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_TF.entropy <- entropy(GEEL10_TFft) #entropy

GEEL10_TF.netMx <- cbind(GEEL10_TF.netMx, GEEL10_TF.clusterCoef, GEEL10_TF.degreeCent$centralization,
                         GEEL10_TF.netDensity, GEEL10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
GEEL10_SAMg2 <- data.frame(GEEL10_SAM)
GEEL10_SAMg2 <- GEEL10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_SAMg2$player1
player2vector <- GEEL10_SAMg2$player2
GEEL10_SAMg3 <- GEEL10_SAMg2
GEEL10_SAMg3$p1inp2vec <- is.element(GEEL10_SAMg3$player1, player2vector)
GEEL10_SAMg3$p2inp1vec <- is.element(GEEL10_SAMg3$player2, player1vector)

addPlayer1 <- GEEL10_SAMg3[ which(GEEL10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_SAMg3[ which(GEEL10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_SAMg2 <- rbind(GEEL10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
GEEL10_SAMft <- ftable(GEEL10_SAMg2$player1, GEEL10_SAMg2$player2)
GEEL10_SAMft2 <- as.matrix(GEEL10_SAMft)
numRows <- nrow(GEEL10_SAMft2)
numCols <- ncol(GEEL10_SAMft2)
GEEL10_SAMft3 <- GEEL10_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL10_SAMTable <- graph.adjacency(GEEL10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(GEEL10_SAMTable, vertex.label = V(GEEL10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
GEEL10_SAM.clusterCoef <- transitivity(GEEL10_SAMTable, type="global") #cluster coefficient
GEEL10_SAM.degreeCent <- centralization.degree(GEEL10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_SAMftn <- as.network.matrix(GEEL10_SAMft)
GEEL10_SAM.netDensity <- network.density(GEEL10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_SAM.entropy <- entropy(GEEL10_SAMft) #entropy

GEEL10_SAM.netMx <- cbind(GEEL10_SAM.netMx, GEEL10_SAM.clusterCoef, GEEL10_SAM.degreeCent$centralization,
                          GEEL10_SAM.netDensity, GEEL10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
GEEL10_TAMg2 <- data.frame(GEEL10_TAM)
GEEL10_TAMg2 <- GEEL10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_TAMg2$player1
player2vector <- GEEL10_TAMg2$player2
GEEL10_TAMg3 <- GEEL10_TAMg2
GEEL10_TAMg3$p1inp2vec <- is.element(GEEL10_TAMg3$player1, player2vector)
GEEL10_TAMg3$p2inp1vec <- is.element(GEEL10_TAMg3$player2, player1vector)

addPlayer1 <- GEEL10_TAMg3[ which(GEEL10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL10_TAMg3[ which(GEEL10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_TAMg2 <- rbind(GEEL10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
GEEL10_TAMft <- ftable(GEEL10_TAMg2$player1, GEEL10_TAMg2$player2)
GEEL10_TAMft2 <- as.matrix(GEEL10_TAMft)
numRows <- nrow(GEEL10_TAMft2)
numCols <- ncol(GEEL10_TAMft2)
GEEL10_TAMft3 <- GEEL10_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL10_TAMTable <- graph.adjacency(GEEL10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(GEEL10_TAMTable, vertex.label = V(GEEL10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
GEEL10_TAM.clusterCoef <- transitivity(GEEL10_TAMTable, type="global") #cluster coefficient
GEEL10_TAM.degreeCent <- centralization.degree(GEEL10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_TAMftn <- as.network.matrix(GEEL10_TAMft)
GEEL10_TAM.netDensity <- network.density(GEEL10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_TAM.entropy <- entropy(GEEL10_TAMft) #entropy

GEEL10_TAM.netMx <- cbind(GEEL10_TAM.netMx, GEEL10_TAM.clusterCoef, GEEL10_TAM.degreeCent$centralization,
                          GEEL10_TAM.netDensity, GEEL10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
GEEL10_SDMg2 <- data.frame(GEEL10_SDM)
GEEL10_SDMg2 <- GEEL10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_SDMg2$player1
player2vector <- GEEL10_SDMg2$player2
GEEL10_SDMg3 <- GEEL10_SDMg2
GEEL10_SDMg3$p1inp2vec <- is.element(GEEL10_SDMg3$player1, player2vector)
GEEL10_SDMg3$p2inp1vec <- is.element(GEEL10_SDMg3$player2, player1vector)

addPlayer1 <- GEEL10_SDMg3[ which(GEEL10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_SDMg3[ which(GEEL10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_SDMg2 <- rbind(GEEL10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
GEEL10_SDMft <- ftable(GEEL10_SDMg2$player1, GEEL10_SDMg2$player2)
GEEL10_SDMft2 <- as.matrix(GEEL10_SDMft)
numRows <- nrow(GEEL10_SDMft2)
numCols <- ncol(GEEL10_SDMft2)
GEEL10_SDMft3 <- GEEL10_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL10_SDMTable <- graph.adjacency(GEEL10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(GEEL10_SDMTable, vertex.label = V(GEEL10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
GEEL10_SDM.clusterCoef <- transitivity(GEEL10_SDMTable, type="global") #cluster coefficient
GEEL10_SDM.degreeCent <- centralization.degree(GEEL10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_SDMftn <- as.network.matrix(GEEL10_SDMft)
GEEL10_SDM.netDensity <- network.density(GEEL10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_SDM.entropy <- entropy(GEEL10_SDMft) #entropy

GEEL10_SDM.netMx <- cbind(GEEL10_SDM.netMx, GEEL10_SDM.clusterCoef, GEEL10_SDM.degreeCent$centralization,
                          GEEL10_SDM.netDensity, GEEL10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
GEEL10_TDMg2 <- data.frame(GEEL10_TDM)
GEEL10_TDMg2 <- GEEL10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_TDMg2$player1
player2vector <- GEEL10_TDMg2$player2
GEEL10_TDMg3 <- GEEL10_TDMg2
GEEL10_TDMg3$p1inp2vec <- is.element(GEEL10_TDMg3$player1, player2vector)
GEEL10_TDMg3$p2inp1vec <- is.element(GEEL10_TDMg3$player2, player1vector)

addPlayer1 <- GEEL10_TDMg3[ which(GEEL10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_TDMg3[ which(GEEL10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_TDMg2 <- rbind(GEEL10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
GEEL10_TDMft <- ftable(GEEL10_TDMg2$player1, GEEL10_TDMg2$player2)
GEEL10_TDMft2 <- as.matrix(GEEL10_TDMft)
numRows <- nrow(GEEL10_TDMft2)
numCols <- ncol(GEEL10_TDMft2)
GEEL10_TDMft3 <- GEEL10_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL10_TDMTable <- graph.adjacency(GEEL10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(GEEL10_TDMTable, vertex.label = V(GEEL10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
GEEL10_TDM.clusterCoef <- transitivity(GEEL10_TDMTable, type="global") #cluster coefficient
GEEL10_TDM.degreeCent <- centralization.degree(GEEL10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_TDMftn <- as.network.matrix(GEEL10_TDMft)
GEEL10_TDM.netDensity <- network.density(GEEL10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_TDM.entropy <- entropy(GEEL10_TDMft) #entropy

GEEL10_TDM.netMx <- cbind(GEEL10_TDM.netMx, GEEL10_TDM.clusterCoef, GEEL10_TDM.degreeCent$centralization,
                          GEEL10_TDM.netDensity, GEEL10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
GEEL10_SDg2 <- data.frame(GEEL10_SD)
GEEL10_SDg2 <- GEEL10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_SDg2$player1
player2vector <- GEEL10_SDg2$player2
GEEL10_SDg3 <- GEEL10_SDg2
GEEL10_SDg3$p1inp2vec <- is.element(GEEL10_SDg3$player1, player2vector)
GEEL10_SDg3$p2inp1vec <- is.element(GEEL10_SDg3$player2, player1vector)

addPlayer1 <- GEEL10_SDg3[ which(GEEL10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_SDg3[ which(GEEL10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_SDg2 <- rbind(GEEL10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
GEEL10_SDft <- ftable(GEEL10_SDg2$player1, GEEL10_SDg2$player2)
GEEL10_SDft2 <- as.matrix(GEEL10_SDft)
numRows <- nrow(GEEL10_SDft2)
numCols <- ncol(GEEL10_SDft2)
GEEL10_SDft3 <- GEEL10_SDft2[c(2:numRows) , c(2:numCols)]
GEEL10_SDTable <- graph.adjacency(GEEL10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(GEEL10_SDTable, vertex.label = V(GEEL10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
GEEL10_SD.clusterCoef <- transitivity(GEEL10_SDTable, type="global") #cluster coefficient
GEEL10_SD.degreeCent <- centralization.degree(GEEL10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_SDftn <- as.network.matrix(GEEL10_SDft)
GEEL10_SD.netDensity <- network.density(GEEL10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_SD.entropy <- entropy(GEEL10_SDft) #entropy

GEEL10_SD.netMx <- cbind(GEEL10_SD.netMx, GEEL10_SD.clusterCoef, GEEL10_SD.degreeCent$centralization,
                         GEEL10_SD.netDensity, GEEL10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
GEEL10_TDg2 <- data.frame(GEEL10_TD)
GEEL10_TDg2 <- GEEL10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_TDg2$player1
player2vector <- GEEL10_TDg2$player2
GEEL10_TDg3 <- GEEL10_TDg2
GEEL10_TDg3$p1inp2vec <- is.element(GEEL10_TDg3$player1, player2vector)
GEEL10_TDg3$p2inp1vec <- is.element(GEEL10_TDg3$player2, player1vector)

addPlayer1 <- GEEL10_TDg3[ which(GEEL10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_TDg3[ which(GEEL10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_TDg2 <- rbind(GEEL10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
GEEL10_TDft <- ftable(GEEL10_TDg2$player1, GEEL10_TDg2$player2)
GEEL10_TDft2 <- as.matrix(GEEL10_TDft)
numRows <- nrow(GEEL10_TDft2)
numCols <- ncol(GEEL10_TDft2)
GEEL10_TDft3 <- GEEL10_TDft2[c(2:numRows) , c(2:numCols)]
GEEL10_TDTable <- graph.adjacency(GEEL10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(GEEL10_TDTable, vertex.label = V(GEEL10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
GEEL10_TD.clusterCoef <- transitivity(GEEL10_TDTable, type="global") #cluster coefficient
GEEL10_TD.degreeCent <- centralization.degree(GEEL10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_TDftn <- as.network.matrix(GEEL10_TDft)
GEEL10_TD.netDensity <- network.density(GEEL10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_TD.entropy <- entropy(GEEL10_TDft) #entropy

GEEL10_TD.netMx <- cbind(GEEL10_TD.netMx, GEEL10_TD.clusterCoef, GEEL10_TD.degreeCent$centralization,
                         GEEL10_TD.netDensity, GEEL10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
GEEL10_QTg2 <- data.frame(GEEL10_QT)
GEEL10_QTg2 <- GEEL10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL10_QTg2$player1
player2vector <- GEEL10_QTg2$player2
GEEL10_QTg3 <- GEEL10_QTg2
GEEL10_QTg3$p1inp2vec <- is.element(GEEL10_QTg3$player1, player2vector)
GEEL10_QTg3$p2inp1vec <- is.element(GEEL10_QTg3$player2, player1vector)

addPlayer1 <- GEEL10_QTg3[ which(GEEL10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL10_QTg3[ which(GEEL10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL10_QTg2 <- rbind(GEEL10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
GEEL10_QTft <- ftable(GEEL10_QTg2$player1, GEEL10_QTg2$player2)
GEEL10_QTft2 <- as.matrix(GEEL10_QTft)
numRows <- nrow(GEEL10_QTft2)
numCols <- ncol(GEEL10_QTft2)
GEEL10_QTft3 <- GEEL10_QTft2[c(2:numRows) , c(2:numCols)]
GEEL10_QTTable <- graph.adjacency(GEEL10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(GEEL10_QTTable, vertex.label = V(GEEL10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
GEEL10_QT.clusterCoef <- transitivity(GEEL10_QTTable, type="global") #cluster coefficient
GEEL10_QT.degreeCent <- centralization.degree(GEEL10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL10_QTftn <- as.network.matrix(GEEL10_QTft)
GEEL10_QT.netDensity <- network.density(GEEL10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL10_QT.entropy <- entropy(GEEL10_QTft) #entropy

GEEL10_QT.netMx <- cbind(GEEL10_QT.netMx, GEEL10_QT.clusterCoef, GEEL10_QT.degreeCent$centralization,
                         GEEL10_QT.netDensity, GEEL10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL10_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Goal_F"
GWS10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
GWS10_Gg2 <- data.frame(GWS10_G)
GWS10_Gg2 <- GWS10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_Gg2$player1
player2vector <- GWS10_Gg2$player2
GWS10_Gg3 <- GWS10_Gg2
GWS10_Gg3$p1inp2vec <- is.element(GWS10_Gg3$player1, player2vector)
GWS10_Gg3$p2inp1vec <- is.element(GWS10_Gg3$player2, player1vector)

addPlayer1 <- GWS10_Gg3[ which(GWS10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_Gg3[ which(GWS10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_Gg2 <- rbind(GWS10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
GWS10_Gft <- ftable(GWS10_Gg2$player1, GWS10_Gg2$player2)
GWS10_Gft2 <- as.matrix(GWS10_Gft)
numRows <- nrow(GWS10_Gft2)
numCols <- ncol(GWS10_Gft2)
GWS10_Gft3 <- GWS10_Gft2[c(1:numRows) , c(1:numCols)]
GWS10_GTable <- graph.adjacency(GWS10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(GWS10_GTable, vertex.label = V(GWS10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
GWS10_G.clusterCoef <- transitivity(GWS10_GTable, type="global") #cluster coefficient
GWS10_G.degreeCent <- centralization.degree(GWS10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_Gftn <- as.network.matrix(GWS10_Gft)
GWS10_G.netDensity <- network.density(GWS10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_G.entropy <- entropy(GWS10_Gft) #entropy

GWS10_G.netMx <- cbind(GWS10_G.netMx, GWS10_G.clusterCoef, GWS10_G.degreeCent$centralization,
                       GWS10_G.netDensity, GWS10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************

round = 10
teamName = "GWS"
KIoutcome = "Behind_F"
GWS10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
GWS10_Bg2 <- data.frame(GWS10_B)
GWS10_Bg2 <- GWS10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_Bg2$player1
player2vector <- GWS10_Bg2$player2
GWS10_Bg3 <- GWS10_Bg2
GWS10_Bg3$p1inp2vec <- is.element(GWS10_Bg3$player1, player2vector)
GWS10_Bg3$p2inp1vec <- is.element(GWS10_Bg3$player2, player1vector)

addPlayer1 <- GWS10_Bg3[ which(GWS10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS10_Bg3[ which(GWS10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_Bg2 <- rbind(GWS10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
GWS10_Bft <- ftable(GWS10_Bg2$player1, GWS10_Bg2$player2)
GWS10_Bft2 <- as.matrix(GWS10_Bft)
numRows <- nrow(GWS10_Bft2)
numCols <- ncol(GWS10_Bft2)
GWS10_Bft3 <- GWS10_Bft2[c(2:numRows) , c(2:numCols)]
GWS10_BTable <- graph.adjacency(GWS10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(GWS10_BTable, vertex.label = V(GWS10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
GWS10_B.clusterCoef <- transitivity(GWS10_BTable, type="global") #cluster coefficient
GWS10_B.degreeCent <- centralization.degree(GWS10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_Bftn <- as.network.matrix(GWS10_Bft)
GWS10_B.netDensity <- network.density(GWS10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_B.entropy <- entropy(GWS10_Bft) #entropy

GWS10_B.netMx <- cbind(GWS10_B.netMx, GWS10_B.clusterCoef, GWS10_B.degreeCent$centralization,
                       GWS10_B.netDensity, GWS10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
GWS10_SFg2 <- data.frame(GWS10_SF)
GWS10_SFg2 <- GWS10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_SFg2$player1
player2vector <- GWS10_SFg2$player2
GWS10_SFg3 <- GWS10_SFg2
GWS10_SFg3$p1inp2vec <- is.element(GWS10_SFg3$player1, player2vector)
GWS10_SFg3$p2inp1vec <- is.element(GWS10_SFg3$player2, player1vector)

addPlayer1 <- GWS10_SFg3[ which(GWS10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_SFg3[ which(GWS10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_SFg2 <- rbind(GWS10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
GWS10_SFft <- ftable(GWS10_SFg2$player1, GWS10_SFg2$player2)
GWS10_SFft2 <- as.matrix(GWS10_SFft)
numRows <- nrow(GWS10_SFft2)
numCols <- ncol(GWS10_SFft2)
GWS10_SFft3 <- GWS10_SFft2[c(2:numRows) , c(2:numCols)]
GWS10_SFTable <- graph.adjacency(GWS10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(GWS10_SFTable, vertex.label = V(GWS10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
GWS10_SF.clusterCoef <- transitivity(GWS10_SFTable, type="global") #cluster coefficient
GWS10_SF.degreeCent <- centralization.degree(GWS10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_SFftn <- as.network.matrix(GWS10_SFft)
GWS10_SF.netDensity <- network.density(GWS10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_SF.entropy <- entropy(GWS10_SFft) #entropy

GWS10_SF.netMx <- cbind(GWS10_SF.netMx, GWS10_SF.clusterCoef, GWS10_SF.degreeCent$centralization,
                        GWS10_SF.netDensity, GWS10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
GWS10_TFg2 <- data.frame(GWS10_TF)
GWS10_TFg2 <- GWS10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_TFg2$player1
player2vector <- GWS10_TFg2$player2
GWS10_TFg3 <- GWS10_TFg2
GWS10_TFg3$p1inp2vec <- is.element(GWS10_TFg3$player1, player2vector)
GWS10_TFg3$p2inp1vec <- is.element(GWS10_TFg3$player2, player1vector)

addPlayer1 <- GWS10_TFg3[ which(GWS10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_TFg3[ which(GWS10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_TFg2 <- rbind(GWS10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
GWS10_TFft <- ftable(GWS10_TFg2$player1, GWS10_TFg2$player2)
GWS10_TFft2 <- as.matrix(GWS10_TFft)
numRows <- nrow(GWS10_TFft2)
numCols <- ncol(GWS10_TFft2)
GWS10_TFft3 <- GWS10_TFft2[c(2:numRows) , c(2:numCols)]
GWS10_TFTable <- graph.adjacency(GWS10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(GWS10_TFTable, vertex.label = V(GWS10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
GWS10_TF.clusterCoef <- transitivity(GWS10_TFTable, type="global") #cluster coefficient
GWS10_TF.degreeCent <- centralization.degree(GWS10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_TFftn <- as.network.matrix(GWS10_TFft)
GWS10_TF.netDensity <- network.density(GWS10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_TF.entropy <- entropy(GWS10_TFft) #entropy

GWS10_TF.netMx <- cbind(GWS10_TF.netMx, GWS10_TF.clusterCoef, GWS10_TF.degreeCent$centralization,
                        GWS10_TF.netDensity, GWS10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
GWS10_SAMg2 <- data.frame(GWS10_SAM)
GWS10_SAMg2 <- GWS10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_SAMg2$player1
player2vector <- GWS10_SAMg2$player2
GWS10_SAMg3 <- GWS10_SAMg2
GWS10_SAMg3$p1inp2vec <- is.element(GWS10_SAMg3$player1, player2vector)
GWS10_SAMg3$p2inp1vec <- is.element(GWS10_SAMg3$player2, player1vector)

addPlayer1 <- GWS10_SAMg3[ which(GWS10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_SAMg3[ which(GWS10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_SAMg2 <- rbind(GWS10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
GWS10_SAMft <- ftable(GWS10_SAMg2$player1, GWS10_SAMg2$player2)
GWS10_SAMft2 <- as.matrix(GWS10_SAMft)
numRows <- nrow(GWS10_SAMft2)
numCols <- ncol(GWS10_SAMft2)
GWS10_SAMft3 <- GWS10_SAMft2[c(2:numRows) , c(2:numCols)]
GWS10_SAMTable <- graph.adjacency(GWS10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(GWS10_SAMTable, vertex.label = V(GWS10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
GWS10_SAM.clusterCoef <- transitivity(GWS10_SAMTable, type="global") #cluster coefficient
GWS10_SAM.degreeCent <- centralization.degree(GWS10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_SAMftn <- as.network.matrix(GWS10_SAMft)
GWS10_SAM.netDensity <- network.density(GWS10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_SAM.entropy <- entropy(GWS10_SAMft) #entropy

GWS10_SAM.netMx <- cbind(GWS10_SAM.netMx, GWS10_SAM.clusterCoef, GWS10_SAM.degreeCent$centralization,
                         GWS10_SAM.netDensity, GWS10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
GWS10_TAMg2 <- data.frame(GWS10_TAM)
GWS10_TAMg2 <- GWS10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_TAMg2$player1
player2vector <- GWS10_TAMg2$player2
GWS10_TAMg3 <- GWS10_TAMg2
GWS10_TAMg3$p1inp2vec <- is.element(GWS10_TAMg3$player1, player2vector)
GWS10_TAMg3$p2inp1vec <- is.element(GWS10_TAMg3$player2, player1vector)

addPlayer1 <- GWS10_TAMg3[ which(GWS10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_TAMg3[ which(GWS10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_TAMg2 <- rbind(GWS10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
GWS10_TAMft <- ftable(GWS10_TAMg2$player1, GWS10_TAMg2$player2)
GWS10_TAMft2 <- as.matrix(GWS10_TAMft)
numRows <- nrow(GWS10_TAMft2)
numCols <- ncol(GWS10_TAMft2)
GWS10_TAMft3 <- GWS10_TAMft2[c(2:numRows) , c(2:numCols)]
GWS10_TAMTable <- graph.adjacency(GWS10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(GWS10_TAMTable, vertex.label = V(GWS10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
GWS10_TAM.clusterCoef <- transitivity(GWS10_TAMTable, type="global") #cluster coefficient
GWS10_TAM.degreeCent <- centralization.degree(GWS10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_TAMftn <- as.network.matrix(GWS10_TAMft)
GWS10_TAM.netDensity <- network.density(GWS10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_TAM.entropy <- entropy(GWS10_TAMft) #entropy

GWS10_TAM.netMx <- cbind(GWS10_TAM.netMx, GWS10_TAM.clusterCoef, GWS10_TAM.degreeCent$centralization,
                         GWS10_TAM.netDensity, GWS10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
GWS10_SDMg2 <- data.frame(GWS10_SDM)
GWS10_SDMg2 <- GWS10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_SDMg2$player1
player2vector <- GWS10_SDMg2$player2
GWS10_SDMg3 <- GWS10_SDMg2
GWS10_SDMg3$p1inp2vec <- is.element(GWS10_SDMg3$player1, player2vector)
GWS10_SDMg3$p2inp1vec <- is.element(GWS10_SDMg3$player2, player1vector)

addPlayer1 <- GWS10_SDMg3[ which(GWS10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_SDMg3[ which(GWS10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_SDMg2 <- rbind(GWS10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
GWS10_SDMft <- ftable(GWS10_SDMg2$player1, GWS10_SDMg2$player2)
GWS10_SDMft2 <- as.matrix(GWS10_SDMft)
numRows <- nrow(GWS10_SDMft2)
numCols <- ncol(GWS10_SDMft2)
GWS10_SDMft3 <- GWS10_SDMft2[c(2:numRows) , c(2:numCols)]
GWS10_SDMTable <- graph.adjacency(GWS10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(GWS10_SDMTable, vertex.label = V(GWS10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
GWS10_SDM.clusterCoef <- transitivity(GWS10_SDMTable, type="global") #cluster coefficient
GWS10_SDM.degreeCent <- centralization.degree(GWS10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_SDMftn <- as.network.matrix(GWS10_SDMft)
GWS10_SDM.netDensity <- network.density(GWS10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_SDM.entropy <- entropy(GWS10_SDMft) #entropy

GWS10_SDM.netMx <- cbind(GWS10_SDM.netMx, GWS10_SDM.clusterCoef, GWS10_SDM.degreeCent$centralization,
                         GWS10_SDM.netDensity, GWS10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
GWS10_TDMg2 <- data.frame(GWS10_TDM)
GWS10_TDMg2 <- GWS10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_TDMg2$player1
player2vector <- GWS10_TDMg2$player2
GWS10_TDMg3 <- GWS10_TDMg2
GWS10_TDMg3$p1inp2vec <- is.element(GWS10_TDMg3$player1, player2vector)
GWS10_TDMg3$p2inp1vec <- is.element(GWS10_TDMg3$player2, player1vector)

addPlayer1 <- GWS10_TDMg3[ which(GWS10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_TDMg3[ which(GWS10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_TDMg2 <- rbind(GWS10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
GWS10_TDMft <- ftable(GWS10_TDMg2$player1, GWS10_TDMg2$player2)
GWS10_TDMft2 <- as.matrix(GWS10_TDMft)
numRows <- nrow(GWS10_TDMft2)
numCols <- ncol(GWS10_TDMft2)
GWS10_TDMft3 <- GWS10_TDMft2[c(2:numRows) , c(2:numCols)]
GWS10_TDMTable <- graph.adjacency(GWS10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(GWS10_TDMTable, vertex.label = V(GWS10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
GWS10_TDM.clusterCoef <- transitivity(GWS10_TDMTable, type="global") #cluster coefficient
GWS10_TDM.degreeCent <- centralization.degree(GWS10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_TDMftn <- as.network.matrix(GWS10_TDMft)
GWS10_TDM.netDensity <- network.density(GWS10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_TDM.entropy <- entropy(GWS10_TDMft) #entropy

GWS10_TDM.netMx <- cbind(GWS10_TDM.netMx, GWS10_TDM.clusterCoef, GWS10_TDM.degreeCent$centralization,
                         GWS10_TDM.netDensity, GWS10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
GWS10_SDg2 <- data.frame(GWS10_SD)
GWS10_SDg2 <- GWS10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_SDg2$player1
player2vector <- GWS10_SDg2$player2
GWS10_SDg3 <- GWS10_SDg2
GWS10_SDg3$p1inp2vec <- is.element(GWS10_SDg3$player1, player2vector)
GWS10_SDg3$p2inp1vec <- is.element(GWS10_SDg3$player2, player1vector)

addPlayer1 <- GWS10_SDg3[ which(GWS10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_SDg3[ which(GWS10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_SDg2 <- rbind(GWS10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
GWS10_SDft <- ftable(GWS10_SDg2$player1, GWS10_SDg2$player2)
GWS10_SDft2 <- as.matrix(GWS10_SDft)
numRows <- nrow(GWS10_SDft2)
numCols <- ncol(GWS10_SDft2)
GWS10_SDft3 <- GWS10_SDft2[c(2:numRows) , c(2:numCols)]
GWS10_SDTable <- graph.adjacency(GWS10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(GWS10_SDTable, vertex.label = V(GWS10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
GWS10_SD.clusterCoef <- transitivity(GWS10_SDTable, type="global") #cluster coefficient
GWS10_SD.degreeCent <- centralization.degree(GWS10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_SDftn <- as.network.matrix(GWS10_SDft)
GWS10_SD.netDensity <- network.density(GWS10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_SD.entropy <- entropy(GWS10_SDft) #entropy

GWS10_SD.netMx <- cbind(GWS10_SD.netMx, GWS10_SD.clusterCoef, GWS10_SD.degreeCent$centralization,
                        GWS10_SD.netDensity, GWS10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************

round = 10
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
GWS10_TDg2 <- data.frame(GWS10_TD)
GWS10_TDg2 <- GWS10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_TDg2$player1
player2vector <- GWS10_TDg2$player2
GWS10_TDg3 <- GWS10_TDg2
GWS10_TDg3$p1inp2vec <- is.element(GWS10_TDg3$player1, player2vector)
GWS10_TDg3$p2inp1vec <- is.element(GWS10_TDg3$player2, player1vector)

addPlayer1 <- GWS10_TDg3[ which(GWS10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_TDg3[ which(GWS10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_TDg2 <- rbind(GWS10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
GWS10_TDft <- ftable(GWS10_TDg2$player1, GWS10_TDg2$player2)
GWS10_TDft2 <- as.matrix(GWS10_TDft)
numRows <- nrow(GWS10_TDft2)
numCols <- ncol(GWS10_TDft2)
GWS10_TDft3 <- GWS10_TDft2[c(2:numRows) , c(2:numCols)]
GWS10_TDTable <- graph.adjacency(GWS10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(GWS10_TDTable, vertex.label = V(GWS10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
GWS10_TD.clusterCoef <- transitivity(GWS10_TDTable, type="global") #cluster coefficient
GWS10_TD.degreeCent <- centralization.degree(GWS10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_TDftn <- as.network.matrix(GWS10_TDft)
GWS10_TD.netDensity <- network.density(GWS10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_TD.entropy <- entropy(GWS10_TDft) #entropy

GWS10_TD.netMx <- cbind(GWS10_TD.netMx, GWS10_TD.clusterCoef, GWS10_TD.degreeCent$centralization,
                        GWS10_TD.netDensity, GWS10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
GWS10_QTg2 <- data.frame(GWS10_QT)
GWS10_QTg2 <- GWS10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS10_QTg2$player1
player2vector <- GWS10_QTg2$player2
GWS10_QTg3 <- GWS10_QTg2
GWS10_QTg3$p1inp2vec <- is.element(GWS10_QTg3$player1, player2vector)
GWS10_QTg3$p2inp1vec <- is.element(GWS10_QTg3$player2, player1vector)

addPlayer1 <- GWS10_QTg3[ which(GWS10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS10_QTg3[ which(GWS10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS10_QTg2 <- rbind(GWS10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
GWS10_QTft <- ftable(GWS10_QTg2$player1, GWS10_QTg2$player2)
GWS10_QTft2 <- as.matrix(GWS10_QTft)
numRows <- nrow(GWS10_QTft2)
numCols <- ncol(GWS10_QTft2)
GWS10_QTft3 <- GWS10_QTft2[c(2:numRows) , c(2:numCols)]
GWS10_QTTable <- graph.adjacency(GWS10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(GWS10_QTTable, vertex.label = V(GWS10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
GWS10_QT.clusterCoef <- transitivity(GWS10_QTTable, type="global") #cluster coefficient
GWS10_QT.degreeCent <- centralization.degree(GWS10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS10_QTftn <- as.network.matrix(GWS10_QTft)
GWS10_QT.netDensity <- network.density(GWS10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS10_QT.entropy <- entropy(GWS10_QTft) #entropy

GWS10_QT.netMx <- cbind(GWS10_QT.netMx, GWS10_QT.clusterCoef, GWS10_QT.degreeCent$centralization,
                        GWS10_QT.netDensity, GWS10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS10_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************

round = 10
teamName = "HAW"
KIoutcome = "Goal_F"
HAW10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
HAW10_Gg2 <- data.frame(HAW10_G)
HAW10_Gg2 <- HAW10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_Gg2$player1
player2vector <- HAW10_Gg2$player2
HAW10_Gg3 <- HAW10_Gg2
HAW10_Gg3$p1inp2vec <- is.element(HAW10_Gg3$player1, player2vector)
HAW10_Gg3$p2inp1vec <- is.element(HAW10_Gg3$player2, player1vector)

addPlayer1 <- HAW10_Gg3[ which(HAW10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW10_Gg3[ which(HAW10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_Gg2 <- rbind(HAW10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
HAW10_Gft <- ftable(HAW10_Gg2$player1, HAW10_Gg2$player2)
HAW10_Gft2 <- as.matrix(HAW10_Gft)
numRows <- nrow(HAW10_Gft2)
numCols <- ncol(HAW10_Gft2)
HAW10_Gft3 <- HAW10_Gft2[c(2:numRows) , c(2:numCols)]
HAW10_GTable <- graph.adjacency(HAW10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(HAW10_GTable, vertex.label = V(HAW10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
HAW10_G.clusterCoef <- transitivity(HAW10_GTable, type="global") #cluster coefficient
HAW10_G.degreeCent <- centralization.degree(HAW10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_Gftn <- as.network.matrix(HAW10_Gft)
HAW10_G.netDensity <- network.density(HAW10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_G.entropy <- entropy(HAW10_Gft) #entropy

HAW10_G.netMx <- cbind(HAW10_G.netMx, HAW10_G.clusterCoef, HAW10_G.degreeCent$centralization,
                       HAW10_G.netDensity, HAW10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************

round = 10
teamName = "HAW"
KIoutcome = "Behind_F"
HAW10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
HAW10_Bg2 <- data.frame(HAW10_B)
HAW10_Bg2 <- HAW10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_Bg2$player1
player2vector <- HAW10_Bg2$player2
HAW10_Bg3 <- HAW10_Bg2
HAW10_Bg3$p1inp2vec <- is.element(HAW10_Bg3$player1, player2vector)
HAW10_Bg3$p2inp1vec <- is.element(HAW10_Bg3$player2, player1vector)

addPlayer1 <- HAW10_Bg3[ which(HAW10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_Bg3[ which(HAW10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_Bg2 <- rbind(HAW10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
HAW10_Bft <- ftable(HAW10_Bg2$player1, HAW10_Bg2$player2)
HAW10_Bft2 <- as.matrix(HAW10_Bft)
numRows <- nrow(HAW10_Bft2)
numCols <- ncol(HAW10_Bft2)
HAW10_Bft3 <- HAW10_Bft2[c(2:numRows) , c(2:numCols)]
HAW10_BTable <- graph.adjacency(HAW10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(HAW10_BTable, vertex.label = V(HAW10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
HAW10_B.clusterCoef <- transitivity(HAW10_BTable, type="global") #cluster coefficient
HAW10_B.degreeCent <- centralization.degree(HAW10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_Bftn <- as.network.matrix(HAW10_Bft)
HAW10_B.netDensity <- network.density(HAW10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_B.entropy <- entropy(HAW10_Bft) #entropy

HAW10_B.netMx <- cbind(HAW10_B.netMx, HAW10_B.clusterCoef, HAW10_B.degreeCent$centralization,
                       HAW10_B.netDensity, HAW10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************

round = 10
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
HAW10_SFg2 <- data.frame(HAW10_SF)
HAW10_SFg2 <- HAW10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_SFg2$player1
player2vector <- HAW10_SFg2$player2
HAW10_SFg3 <- HAW10_SFg2
HAW10_SFg3$p1inp2vec <- is.element(HAW10_SFg3$player1, player2vector)
HAW10_SFg3$p2inp1vec <- is.element(HAW10_SFg3$player2, player1vector)

addPlayer1 <- HAW10_SFg3[ which(HAW10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW10_SFg3[ which(HAW10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_SFg2 <- rbind(HAW10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
HAW10_SFft <- ftable(HAW10_SFg2$player1, HAW10_SFg2$player2)
HAW10_SFft2 <- as.matrix(HAW10_SFft)
numRows <- nrow(HAW10_SFft2)
numCols <- ncol(HAW10_SFft2)
HAW10_SFft3 <- HAW10_SFft2[c(2:numRows) , c(2:numCols)]
HAW10_SFTable <- graph.adjacency(HAW10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(HAW10_SFTable, vertex.label = V(HAW10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
HAW10_SF.clusterCoef <- transitivity(HAW10_SFTable, type="global") #cluster coefficient
HAW10_SF.degreeCent <- centralization.degree(HAW10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_SFftn <- as.network.matrix(HAW10_SFft)
HAW10_SF.netDensity <- network.density(HAW10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_SF.entropy <- entropy(HAW10_SFft) #entropy

HAW10_SF.netMx <- cbind(HAW10_SF.netMx, HAW10_SF.clusterCoef, HAW10_SF.degreeCent$centralization,
                        HAW10_SF.netDensity, HAW10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
HAW10_TFg2 <- data.frame(HAW10_TF)
HAW10_TFg2 <- HAW10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_TFg2$player1
player2vector <- HAW10_TFg2$player2
HAW10_TFg3 <- HAW10_TFg2
HAW10_TFg3$p1inp2vec <- is.element(HAW10_TFg3$player1, player2vector)
HAW10_TFg3$p2inp1vec <- is.element(HAW10_TFg3$player2, player1vector)

addPlayer1 <- HAW10_TFg3[ which(HAW10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_TFg3[ which(HAW10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_TFg2 <- rbind(HAW10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
HAW10_TFft <- ftable(HAW10_TFg2$player1, HAW10_TFg2$player2)
HAW10_TFft2 <- as.matrix(HAW10_TFft)
numRows <- nrow(HAW10_TFft2)
numCols <- ncol(HAW10_TFft2)
HAW10_TFft3 <- HAW10_TFft2[c(2:numRows) , c(2:numCols)]
HAW10_TFTable <- graph.adjacency(HAW10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(HAW10_TFTable, vertex.label = V(HAW10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
HAW10_TF.clusterCoef <- transitivity(HAW10_TFTable, type="global") #cluster coefficient
HAW10_TF.degreeCent <- centralization.degree(HAW10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_TFftn <- as.network.matrix(HAW10_TFft)
HAW10_TF.netDensity <- network.density(HAW10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_TF.entropy <- entropy(HAW10_TFft) #entropy

HAW10_TF.netMx <- cbind(HAW10_TF.netMx, HAW10_TF.clusterCoef, HAW10_TF.degreeCent$centralization,
                        HAW10_TF.netDensity, HAW10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
HAW10_SAMg2 <- data.frame(HAW10_SAM)
HAW10_SAMg2 <- HAW10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_SAMg2$player1
player2vector <- HAW10_SAMg2$player2
HAW10_SAMg3 <- HAW10_SAMg2
HAW10_SAMg3$p1inp2vec <- is.element(HAW10_SAMg3$player1, player2vector)
HAW10_SAMg3$p2inp1vec <- is.element(HAW10_SAMg3$player2, player1vector)

addPlayer1 <- HAW10_SAMg3[ which(HAW10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_SAMg3[ which(HAW10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_SAMg2 <- rbind(HAW10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
HAW10_SAMft <- ftable(HAW10_SAMg2$player1, HAW10_SAMg2$player2)
HAW10_SAMft2 <- as.matrix(HAW10_SAMft)
numRows <- nrow(HAW10_SAMft2)
numCols <- ncol(HAW10_SAMft2)
HAW10_SAMft3 <- HAW10_SAMft2[c(2:numRows) , c(2:numCols)]
HAW10_SAMTable <- graph.adjacency(HAW10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(HAW10_SAMTable, vertex.label = V(HAW10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
HAW10_SAM.clusterCoef <- transitivity(HAW10_SAMTable, type="global") #cluster coefficient
HAW10_SAM.degreeCent <- centralization.degree(HAW10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_SAMftn <- as.network.matrix(HAW10_SAMft)
HAW10_SAM.netDensity <- network.density(HAW10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_SAM.entropy <- entropy(HAW10_SAMft) #entropy

HAW10_SAM.netMx <- cbind(HAW10_SAM.netMx, HAW10_SAM.clusterCoef, HAW10_SAM.degreeCent$centralization,
                         HAW10_SAM.netDensity, HAW10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
HAW10_TAMg2 <- data.frame(HAW10_TAM)
HAW10_TAMg2 <- HAW10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_TAMg2$player1
player2vector <- HAW10_TAMg2$player2
HAW10_TAMg3 <- HAW10_TAMg2
HAW10_TAMg3$p1inp2vec <- is.element(HAW10_TAMg3$player1, player2vector)
HAW10_TAMg3$p2inp1vec <- is.element(HAW10_TAMg3$player2, player1vector)

addPlayer1 <- HAW10_TAMg3[ which(HAW10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_TAMg3[ which(HAW10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_TAMg2 <- rbind(HAW10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
HAW10_TAMft <- ftable(HAW10_TAMg2$player1, HAW10_TAMg2$player2)
HAW10_TAMft2 <- as.matrix(HAW10_TAMft)
numRows <- nrow(HAW10_TAMft2)
numCols <- ncol(HAW10_TAMft2)
HAW10_TAMft3 <- HAW10_TAMft2[c(2:numRows) , c(2:numCols)]
HAW10_TAMTable <- graph.adjacency(HAW10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(HAW10_TAMTable, vertex.label = V(HAW10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
HAW10_TAM.clusterCoef <- transitivity(HAW10_TAMTable, type="global") #cluster coefficient
HAW10_TAM.degreeCent <- centralization.degree(HAW10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_TAMftn <- as.network.matrix(HAW10_TAMft)
HAW10_TAM.netDensity <- network.density(HAW10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_TAM.entropy <- entropy(HAW10_TAMft) #entropy

HAW10_TAM.netMx <- cbind(HAW10_TAM.netMx, HAW10_TAM.clusterCoef, HAW10_TAM.degreeCent$centralization,
                         HAW10_TAM.netDensity, HAW10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA

round = 10
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
HAW10_SDMg2 <- data.frame(HAW10_SDM)
HAW10_SDMg2 <- HAW10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_SDMg2$player1
player2vector <- HAW10_SDMg2$player2
HAW10_SDMg3 <- HAW10_SDMg2
HAW10_SDMg3$p1inp2vec <- is.element(HAW10_SDMg3$player1, player2vector)
HAW10_SDMg3$p2inp1vec <- is.element(HAW10_SDMg3$player2, player1vector)

addPlayer1 <- HAW10_SDMg3[ which(HAW10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_SDMg3[ which(HAW10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_SDMg2 <- rbind(HAW10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
HAW10_SDMft <- ftable(HAW10_SDMg2$player1, HAW10_SDMg2$player2)
HAW10_SDMft2 <- as.matrix(HAW10_SDMft)
numRows <- nrow(HAW10_SDMft2)
numCols <- ncol(HAW10_SDMft2)
HAW10_SDMft3 <- HAW10_SDMft2[c(2:numRows) , c(2:numCols)]
HAW10_SDMTable <- graph.adjacency(HAW10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(HAW10_SDMTable, vertex.label = V(HAW10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
HAW10_SDM.clusterCoef <- transitivity(HAW10_SDMTable, type="global") #cluster coefficient
HAW10_SDM.degreeCent <- centralization.degree(HAW10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_SDMftn <- as.network.matrix(HAW10_SDMft)
HAW10_SDM.netDensity <- network.density(HAW10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_SDM.entropy <- entropy(HAW10_SDMft) #entropy

HAW10_SDM.netMx <- cbind(HAW10_SDM.netMx, HAW10_SDM.clusterCoef, HAW10_SDM.degreeCent$centralization,
                         HAW10_SDM.netDensity, HAW10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************
#NA

round = 10
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
HAW10_TDMg2 <- data.frame(HAW10_TDM)
HAW10_TDMg2 <- HAW10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_TDMg2$player1
player2vector <- HAW10_TDMg2$player2
HAW10_TDMg3 <- HAW10_TDMg2
HAW10_TDMg3$p1inp2vec <- is.element(HAW10_TDMg3$player1, player2vector)
HAW10_TDMg3$p2inp1vec <- is.element(HAW10_TDMg3$player2, player1vector)

addPlayer1 <- HAW10_TDMg3[ which(HAW10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_TDMg3[ which(HAW10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_TDMg2 <- rbind(HAW10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
HAW10_TDMft <- ftable(HAW10_TDMg2$player1, HAW10_TDMg2$player2)
HAW10_TDMft2 <- as.matrix(HAW10_TDMft)
numRows <- nrow(HAW10_TDMft2)
numCols <- ncol(HAW10_TDMft2)
HAW10_TDMft3 <- HAW10_TDMft2[c(2:numRows) , c(2:numCols)]
HAW10_TDMTable <- graph.adjacency(HAW10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(HAW10_TDMTable, vertex.label = V(HAW10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
HAW10_TDM.clusterCoef <- transitivity(HAW10_TDMTable, type="global") #cluster coefficient
HAW10_TDM.degreeCent <- centralization.degree(HAW10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_TDMftn <- as.network.matrix(HAW10_TDMft)
HAW10_TDM.netDensity <- network.density(HAW10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_TDM.entropy <- entropy(HAW10_TDMft) #entropy

HAW10_TDM.netMx <- cbind(HAW10_TDM.netMx, HAW10_TDM.clusterCoef, HAW10_TDM.degreeCent$centralization,
                         HAW10_TDM.netDensity, HAW10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
HAW10_SDg2 <- data.frame(HAW10_SD)
HAW10_SDg2 <- HAW10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_SDg2$player1
player2vector <- HAW10_SDg2$player2
HAW10_SDg3 <- HAW10_SDg2
HAW10_SDg3$p1inp2vec <- is.element(HAW10_SDg3$player1, player2vector)
HAW10_SDg3$p2inp1vec <- is.element(HAW10_SDg3$player2, player1vector)

addPlayer1 <- HAW10_SDg3[ which(HAW10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_SDg3[ which(HAW10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_SDg2 <- rbind(HAW10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
HAW10_SDft <- ftable(HAW10_SDg2$player1, HAW10_SDg2$player2)
HAW10_SDft2 <- as.matrix(HAW10_SDft)
numRows <- nrow(HAW10_SDft2)
numCols <- ncol(HAW10_SDft2)
HAW10_SDft3 <- HAW10_SDft2[c(2:numRows) , c(2:numCols)]
HAW10_SDTable <- graph.adjacency(HAW10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(HAW10_SDTable, vertex.label = V(HAW10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
HAW10_SD.clusterCoef <- transitivity(HAW10_SDTable, type="global") #cluster coefficient
HAW10_SD.degreeCent <- centralization.degree(HAW10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_SDftn <- as.network.matrix(HAW10_SDft)
HAW10_SD.netDensity <- network.density(HAW10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_SD.entropy <- entropy(HAW10_SDft) #entropy

HAW10_SD.netMx <- cbind(HAW10_SD.netMx, HAW10_SD.clusterCoef, HAW10_SD.degreeCent$centralization,
                        HAW10_SD.netDensity, HAW10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
HAW10_TDg2 <- data.frame(HAW10_TD)
HAW10_TDg2 <- HAW10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_TDg2$player1
player2vector <- HAW10_TDg2$player2
HAW10_TDg3 <- HAW10_TDg2
HAW10_TDg3$p1inp2vec <- is.element(HAW10_TDg3$player1, player2vector)
HAW10_TDg3$p2inp1vec <- is.element(HAW10_TDg3$player2, player1vector)

addPlayer1 <- HAW10_TDg3[ which(HAW10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_TDg3[ which(HAW10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_TDg2 <- rbind(HAW10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
HAW10_TDft <- ftable(HAW10_TDg2$player1, HAW10_TDg2$player2)
HAW10_TDft2 <- as.matrix(HAW10_TDft)
numRows <- nrow(HAW10_TDft2)
numCols <- ncol(HAW10_TDft2)
HAW10_TDft3 <- HAW10_TDft2[c(2:numRows) , c(2:numCols)]
HAW10_TDTable <- graph.adjacency(HAW10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(HAW10_TDTable, vertex.label = V(HAW10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
HAW10_TD.clusterCoef <- transitivity(HAW10_TDTable, type="global") #cluster coefficient
HAW10_TD.degreeCent <- centralization.degree(HAW10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_TDftn <- as.network.matrix(HAW10_TDft)
HAW10_TD.netDensity <- network.density(HAW10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_TD.entropy <- entropy(HAW10_TDft) #entropy

HAW10_TD.netMx <- cbind(HAW10_TD.netMx, HAW10_TD.clusterCoef, HAW10_TD.degreeCent$centralization,
                        HAW10_TD.netDensity, HAW10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
HAW10_QTg2 <- data.frame(HAW10_QT)
HAW10_QTg2 <- HAW10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW10_QTg2$player1
player2vector <- HAW10_QTg2$player2
HAW10_QTg3 <- HAW10_QTg2
HAW10_QTg3$p1inp2vec <- is.element(HAW10_QTg3$player1, player2vector)
HAW10_QTg3$p2inp1vec <- is.element(HAW10_QTg3$player2, player1vector)

addPlayer1 <- HAW10_QTg3[ which(HAW10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW10_QTg3[ which(HAW10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW10_QTg2 <- rbind(HAW10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
HAW10_QTft <- ftable(HAW10_QTg2$player1, HAW10_QTg2$player2)
HAW10_QTft2 <- as.matrix(HAW10_QTft)
numRows <- nrow(HAW10_QTft2)
numCols <- ncol(HAW10_QTft2)
HAW10_QTft3 <- HAW10_QTft2[c(2:numRows) , c(2:numCols)]
HAW10_QTTable <- graph.adjacency(HAW10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(HAW10_QTTable, vertex.label = V(HAW10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
HAW10_QT.clusterCoef <- transitivity(HAW10_QTTable, type="global") #cluster coefficient
HAW10_QT.degreeCent <- centralization.degree(HAW10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW10_QTftn <- as.network.matrix(HAW10_QTft)
HAW10_QT.netDensity <- network.density(HAW10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW10_QT.entropy <- entropy(HAW10_QTft) #entropy

HAW10_QT.netMx <- cbind(HAW10_QT.netMx, HAW10_QT.clusterCoef, HAW10_QT.degreeCent$centralization,
                        HAW10_QT.netDensity, HAW10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW10_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************

round = 10
teamName = "MELB"
KIoutcome = "Goal_F"
MELB10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
MELB10_Gg2 <- data.frame(MELB10_G)
MELB10_Gg2 <- MELB10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_Gg2$player1
player2vector <- MELB10_Gg2$player2
MELB10_Gg3 <- MELB10_Gg2
MELB10_Gg3$p1inp2vec <- is.element(MELB10_Gg3$player1, player2vector)
MELB10_Gg3$p2inp1vec <- is.element(MELB10_Gg3$player2, player1vector)

addPlayer1 <- MELB10_Gg3[ which(MELB10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB10_Gg3[ which(MELB10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_Gg2 <- rbind(MELB10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
MELB10_Gft <- ftable(MELB10_Gg2$player1, MELB10_Gg2$player2)
MELB10_Gft2 <- as.matrix(MELB10_Gft)
numRows <- nrow(MELB10_Gft2)
numCols <- ncol(MELB10_Gft2)
MELB10_Gft3 <- MELB10_Gft2[c(2:numRows) , c(2:numCols)]
MELB10_GTable <- graph.adjacency(MELB10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(MELB10_GTable, vertex.label = V(MELB10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
MELB10_G.clusterCoef <- transitivity(MELB10_GTable, type="global") #cluster coefficient
MELB10_G.degreeCent <- centralization.degree(MELB10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_Gftn <- as.network.matrix(MELB10_Gft)
MELB10_G.netDensity <- network.density(MELB10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_G.entropy <- entropy(MELB10_Gft) #entropy

MELB10_G.netMx <- cbind(MELB10_G.netMx, MELB10_G.clusterCoef, MELB10_G.degreeCent$centralization,
                        MELB10_G.netDensity, MELB10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Behind_F"
MELB10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
MELB10_Bg2 <- data.frame(MELB10_B)
MELB10_Bg2 <- MELB10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_Bg2$player1
player2vector <- MELB10_Bg2$player2
MELB10_Bg3 <- MELB10_Bg2
MELB10_Bg3$p1inp2vec <- is.element(MELB10_Bg3$player1, player2vector)
MELB10_Bg3$p2inp1vec <- is.element(MELB10_Bg3$player2, player1vector)

addPlayer1 <- MELB10_Bg3[ which(MELB10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_Bg3[ which(MELB10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_Bg2 <- rbind(MELB10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
MELB10_Bft <- ftable(MELB10_Bg2$player1, MELB10_Bg2$player2)
MELB10_Bft2 <- as.matrix(MELB10_Bft)
numRows <- nrow(MELB10_Bft2)
numCols <- ncol(MELB10_Bft2)
MELB10_Bft3 <- MELB10_Bft2[c(2:numRows) , c(2:numCols)]
MELB10_BTable <- graph.adjacency(MELB10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(MELB10_BTable, vertex.label = V(MELB10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
MELB10_B.clusterCoef <- transitivity(MELB10_BTable, type="global") #cluster coefficient
MELB10_B.degreeCent <- centralization.degree(MELB10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_Bftn <- as.network.matrix(MELB10_Bft)
MELB10_B.netDensity <- network.density(MELB10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_B.entropy <- entropy(MELB10_Bft) #entropy

MELB10_B.netMx <- cbind(MELB10_B.netMx, MELB10_B.clusterCoef, MELB10_B.degreeCent$centralization,
                        MELB10_B.netDensity, MELB10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
MELB10_SFg2 <- data.frame(MELB10_SF)
MELB10_SFg2 <- MELB10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_SFg2$player1
player2vector <- MELB10_SFg2$player2
MELB10_SFg3 <- MELB10_SFg2
MELB10_SFg3$p1inp2vec <- is.element(MELB10_SFg3$player1, player2vector)
MELB10_SFg3$p2inp1vec <- is.element(MELB10_SFg3$player2, player1vector)

addPlayer1 <- MELB10_SFg3[ which(MELB10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_SFg3[ which(MELB10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_SFg2 <- rbind(MELB10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
MELB10_SFft <- ftable(MELB10_SFg2$player1, MELB10_SFg2$player2)
MELB10_SFft2 <- as.matrix(MELB10_SFft)
numRows <- nrow(MELB10_SFft2)
numCols <- ncol(MELB10_SFft2)
MELB10_SFft3 <- MELB10_SFft2[c(2:numRows) , c(2:numCols)]
MELB10_SFTable <- graph.adjacency(MELB10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(MELB10_SFTable, vertex.label = V(MELB10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
MELB10_SF.clusterCoef <- transitivity(MELB10_SFTable, type="global") #cluster coefficient
MELB10_SF.degreeCent <- centralization.degree(MELB10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_SFftn <- as.network.matrix(MELB10_SFft)
MELB10_SF.netDensity <- network.density(MELB10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_SF.entropy <- entropy(MELB10_SFft) #entropy

MELB10_SF.netMx <- cbind(MELB10_SF.netMx, MELB10_SF.clusterCoef, MELB10_SF.degreeCent$centralization,
                         MELB10_SF.netDensity, MELB10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
MELB10_TFg2 <- data.frame(MELB10_TF)
MELB10_TFg2 <- MELB10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_TFg2$player1
player2vector <- MELB10_TFg2$player2
MELB10_TFg3 <- MELB10_TFg2
MELB10_TFg3$p1inp2vec <- is.element(MELB10_TFg3$player1, player2vector)
MELB10_TFg3$p2inp1vec <- is.element(MELB10_TFg3$player2, player1vector)

addPlayer1 <- MELB10_TFg3[ which(MELB10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_TFg3[ which(MELB10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_TFg2 <- rbind(MELB10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
MELB10_TFft <- ftable(MELB10_TFg2$player1, MELB10_TFg2$player2)
MELB10_TFft2 <- as.matrix(MELB10_TFft)
numRows <- nrow(MELB10_TFft2)
numCols <- ncol(MELB10_TFft2)
MELB10_TFft3 <- MELB10_TFft2[c(2:numRows) , c(2:numCols)]
MELB10_TFTable <- graph.adjacency(MELB10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(MELB10_TFTable, vertex.label = V(MELB10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
MELB10_TF.clusterCoef <- transitivity(MELB10_TFTable, type="global") #cluster coefficient
MELB10_TF.degreeCent <- centralization.degree(MELB10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_TFftn <- as.network.matrix(MELB10_TFft)
MELB10_TF.netDensity <- network.density(MELB10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_TF.entropy <- entropy(MELB10_TFft) #entropy

MELB10_TF.netMx <- cbind(MELB10_TF.netMx, MELB10_TF.clusterCoef, MELB10_TF.degreeCent$centralization,
                         MELB10_TF.netDensity, MELB10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
MELB10_SAMg2 <- data.frame(MELB10_SAM)
MELB10_SAMg2 <- MELB10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_SAMg2$player1
player2vector <- MELB10_SAMg2$player2
MELB10_SAMg3 <- MELB10_SAMg2
MELB10_SAMg3$p1inp2vec <- is.element(MELB10_SAMg3$player1, player2vector)
MELB10_SAMg3$p2inp1vec <- is.element(MELB10_SAMg3$player2, player1vector)

addPlayer1 <- MELB10_SAMg3[ which(MELB10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_SAMg3[ which(MELB10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_SAMg2 <- rbind(MELB10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
MELB10_SAMft <- ftable(MELB10_SAMg2$player1, MELB10_SAMg2$player2)
MELB10_SAMft2 <- as.matrix(MELB10_SAMft)
numRows <- nrow(MELB10_SAMft2)
numCols <- ncol(MELB10_SAMft2)
MELB10_SAMft3 <- MELB10_SAMft2[c(2:numRows) , c(2:numCols)]
MELB10_SAMTable <- graph.adjacency(MELB10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(MELB10_SAMTable, vertex.label = V(MELB10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
MELB10_SAM.clusterCoef <- transitivity(MELB10_SAMTable, type="global") #cluster coefficient
MELB10_SAM.degreeCent <- centralization.degree(MELB10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_SAMftn <- as.network.matrix(MELB10_SAMft)
MELB10_SAM.netDensity <- network.density(MELB10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_SAM.entropy <- entropy(MELB10_SAMft) #entropy

MELB10_SAM.netMx <- cbind(MELB10_SAM.netMx, MELB10_SAM.clusterCoef, MELB10_SAM.degreeCent$centralization,
                          MELB10_SAM.netDensity, MELB10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
MELB10_TAMg2 <- data.frame(MELB10_TAM)
MELB10_TAMg2 <- MELB10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_TAMg2$player1
player2vector <- MELB10_TAMg2$player2
MELB10_TAMg3 <- MELB10_TAMg2
MELB10_TAMg3$p1inp2vec <- is.element(MELB10_TAMg3$player1, player2vector)
MELB10_TAMg3$p2inp1vec <- is.element(MELB10_TAMg3$player2, player1vector)

addPlayer1 <- MELB10_TAMg3[ which(MELB10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_TAMg3[ which(MELB10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_TAMg2 <- rbind(MELB10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
MELB10_TAMft <- ftable(MELB10_TAMg2$player1, MELB10_TAMg2$player2)
MELB10_TAMft2 <- as.matrix(MELB10_TAMft)
numRows <- nrow(MELB10_TAMft2)
numCols <- ncol(MELB10_TAMft2)
MELB10_TAMft3 <- MELB10_TAMft2[c(2:numRows) , c(2:numCols)]
MELB10_TAMTable <- graph.adjacency(MELB10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(MELB10_TAMTable, vertex.label = V(MELB10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
MELB10_TAM.clusterCoef <- transitivity(MELB10_TAMTable, type="global") #cluster coefficient
MELB10_TAM.degreeCent <- centralization.degree(MELB10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_TAMftn <- as.network.matrix(MELB10_TAMft)
MELB10_TAM.netDensity <- network.density(MELB10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_TAM.entropy <- entropy(MELB10_TAMft) #entropy

MELB10_TAM.netMx <- cbind(MELB10_TAM.netMx, MELB10_TAM.clusterCoef, MELB10_TAM.degreeCent$centralization,
                          MELB10_TAM.netDensity, MELB10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
MELB10_SDMg2 <- data.frame(MELB10_SDM)
MELB10_SDMg2 <- MELB10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_SDMg2$player1
player2vector <- MELB10_SDMg2$player2
MELB10_SDMg3 <- MELB10_SDMg2
MELB10_SDMg3$p1inp2vec <- is.element(MELB10_SDMg3$player1, player2vector)
MELB10_SDMg3$p2inp1vec <- is.element(MELB10_SDMg3$player2, player1vector)

addPlayer1 <- MELB10_SDMg3[ which(MELB10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_SDMg3[ which(MELB10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_SDMg2 <- rbind(MELB10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
MELB10_SDMft <- ftable(MELB10_SDMg2$player1, MELB10_SDMg2$player2)
MELB10_SDMft2 <- as.matrix(MELB10_SDMft)
numRows <- nrow(MELB10_SDMft2)
numCols <- ncol(MELB10_SDMft2)
MELB10_SDMft3 <- MELB10_SDMft2[c(2:numRows) , c(2:numCols)]
MELB10_SDMTable <- graph.adjacency(MELB10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(MELB10_SDMTable, vertex.label = V(MELB10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
MELB10_SDM.clusterCoef <- transitivity(MELB10_SDMTable, type="global") #cluster coefficient
MELB10_SDM.degreeCent <- centralization.degree(MELB10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_SDMftn <- as.network.matrix(MELB10_SDMft)
MELB10_SDM.netDensity <- network.density(MELB10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_SDM.entropy <- entropy(MELB10_SDMft) #entropy

MELB10_SDM.netMx <- cbind(MELB10_SDM.netMx, MELB10_SDM.clusterCoef, MELB10_SDM.degreeCent$centralization,
                          MELB10_SDM.netDensity, MELB10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
MELB10_TDMg2 <- data.frame(MELB10_TDM)
MELB10_TDMg2 <- MELB10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_TDMg2$player1
player2vector <- MELB10_TDMg2$player2
MELB10_TDMg3 <- MELB10_TDMg2
MELB10_TDMg3$p1inp2vec <- is.element(MELB10_TDMg3$player1, player2vector)
MELB10_TDMg3$p2inp1vec <- is.element(MELB10_TDMg3$player2, player1vector)

addPlayer1 <- MELB10_TDMg3[ which(MELB10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_TDMg3[ which(MELB10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_TDMg2 <- rbind(MELB10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
MELB10_TDMft <- ftable(MELB10_TDMg2$player1, MELB10_TDMg2$player2)
MELB10_TDMft2 <- as.matrix(MELB10_TDMft)
numRows <- nrow(MELB10_TDMft2)
numCols <- ncol(MELB10_TDMft2)
MELB10_TDMft3 <- MELB10_TDMft2[c(2:numRows) , c(2:numCols)]
MELB10_TDMTable <- graph.adjacency(MELB10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(MELB10_TDMTable, vertex.label = V(MELB10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
MELB10_TDM.clusterCoef <- transitivity(MELB10_TDMTable, type="global") #cluster coefficient
MELB10_TDM.degreeCent <- centralization.degree(MELB10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_TDMftn <- as.network.matrix(MELB10_TDMft)
MELB10_TDM.netDensity <- network.density(MELB10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_TDM.entropy <- entropy(MELB10_TDMft) #entropy

MELB10_TDM.netMx <- cbind(MELB10_TDM.netMx, MELB10_TDM.clusterCoef, MELB10_TDM.degreeCent$centralization,
                          MELB10_TDM.netDensity, MELB10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
MELB10_SDg2 <- data.frame(MELB10_SD)
MELB10_SDg2 <- MELB10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_SDg2$player1
player2vector <- MELB10_SDg2$player2
MELB10_SDg3 <- MELB10_SDg2
MELB10_SDg3$p1inp2vec <- is.element(MELB10_SDg3$player1, player2vector)
MELB10_SDg3$p2inp1vec <- is.element(MELB10_SDg3$player2, player1vector)

addPlayer1 <- MELB10_SDg3[ which(MELB10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_SDg3[ which(MELB10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_SDg2 <- rbind(MELB10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
MELB10_SDft <- ftable(MELB10_SDg2$player1, MELB10_SDg2$player2)
MELB10_SDft2 <- as.matrix(MELB10_SDft)
numRows <- nrow(MELB10_SDft2)
numCols <- ncol(MELB10_SDft2)
MELB10_SDft3 <- MELB10_SDft2[c(2:numRows) , c(2:numCols)]
MELB10_SDTable <- graph.adjacency(MELB10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(MELB10_SDTable, vertex.label = V(MELB10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
MELB10_SD.clusterCoef <- transitivity(MELB10_SDTable, type="global") #cluster coefficient
MELB10_SD.degreeCent <- centralization.degree(MELB10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_SDftn <- as.network.matrix(MELB10_SDft)
MELB10_SD.netDensity <- network.density(MELB10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_SD.entropy <- entropy(MELB10_SDft) #entropy

MELB10_SD.netMx <- cbind(MELB10_SD.netMx, MELB10_SD.clusterCoef, MELB10_SD.degreeCent$centralization,
                         MELB10_SD.netDensity, MELB10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
MELB10_TDg2 <- data.frame(MELB10_TD)
MELB10_TDg2 <- MELB10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_TDg2$player1
player2vector <- MELB10_TDg2$player2
MELB10_TDg3 <- MELB10_TDg2
MELB10_TDg3$p1inp2vec <- is.element(MELB10_TDg3$player1, player2vector)
MELB10_TDg3$p2inp1vec <- is.element(MELB10_TDg3$player2, player1vector)

addPlayer1 <- MELB10_TDg3[ which(MELB10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_TDg3[ which(MELB10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_TDg2 <- rbind(MELB10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
MELB10_TDft <- ftable(MELB10_TDg2$player1, MELB10_TDg2$player2)
MELB10_TDft2 <- as.matrix(MELB10_TDft)
numRows <- nrow(MELB10_TDft2)
numCols <- ncol(MELB10_TDft2)
MELB10_TDft3 <- MELB10_TDft2[c(2:numRows) , c(2:numCols)]
MELB10_TDTable <- graph.adjacency(MELB10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(MELB10_TDTable, vertex.label = V(MELB10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
MELB10_TD.clusterCoef <- transitivity(MELB10_TDTable, type="global") #cluster coefficient
MELB10_TD.degreeCent <- centralization.degree(MELB10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_TDftn <- as.network.matrix(MELB10_TDft)
MELB10_TD.netDensity <- network.density(MELB10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_TD.entropy <- entropy(MELB10_TDft) #entropy

MELB10_TD.netMx <- cbind(MELB10_TD.netMx, MELB10_TD.clusterCoef, MELB10_TD.degreeCent$centralization,
                         MELB10_TD.netDensity, MELB10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
MELB10_QTg2 <- data.frame(MELB10_QT)
MELB10_QTg2 <- MELB10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB10_QTg2$player1
player2vector <- MELB10_QTg2$player2
MELB10_QTg3 <- MELB10_QTg2
MELB10_QTg3$p1inp2vec <- is.element(MELB10_QTg3$player1, player2vector)
MELB10_QTg3$p2inp1vec <- is.element(MELB10_QTg3$player2, player1vector)

addPlayer1 <- MELB10_QTg3[ which(MELB10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB10_QTg3[ which(MELB10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB10_QTg2 <- rbind(MELB10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
MELB10_QTft <- ftable(MELB10_QTg2$player1, MELB10_QTg2$player2)
MELB10_QTft2 <- as.matrix(MELB10_QTft)
numRows <- nrow(MELB10_QTft2)
numCols <- ncol(MELB10_QTft2)
MELB10_QTft3 <- MELB10_QTft2[c(2:numRows) , c(2:numCols)]
MELB10_QTTable <- graph.adjacency(MELB10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(MELB10_QTTable, vertex.label = V(MELB10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
MELB10_QT.clusterCoef <- transitivity(MELB10_QTTable, type="global") #cluster coefficient
MELB10_QT.degreeCent <- centralization.degree(MELB10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB10_QTftn <- as.network.matrix(MELB10_QTft)
MELB10_QT.netDensity <- network.density(MELB10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB10_QT.entropy <- entropy(MELB10_QTft) #entropy

MELB10_QT.netMx <- cbind(MELB10_QT.netMx, MELB10_QT.clusterCoef, MELB10_QT.degreeCent$centralization,
                         MELB10_QT.netDensity, MELB10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB10_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
NMFC10_Gg2 <- data.frame(NMFC10_G)
NMFC10_Gg2 <- NMFC10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_Gg2$player1
player2vector <- NMFC10_Gg2$player2
NMFC10_Gg3 <- NMFC10_Gg2
NMFC10_Gg3$p1inp2vec <- is.element(NMFC10_Gg3$player1, player2vector)
NMFC10_Gg3$p2inp1vec <- is.element(NMFC10_Gg3$player2, player1vector)

addPlayer1 <- NMFC10_Gg3[ which(NMFC10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_Gg3[ which(NMFC10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_Gg2 <- rbind(NMFC10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
NMFC10_Gft <- ftable(NMFC10_Gg2$player1, NMFC10_Gg2$player2)
NMFC10_Gft2 <- as.matrix(NMFC10_Gft)
numRows <- nrow(NMFC10_Gft2)
numCols <- ncol(NMFC10_Gft2)
NMFC10_Gft3 <- NMFC10_Gft2[c(2:numRows) , c(2:numCols)]
NMFC10_GTable <- graph.adjacency(NMFC10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(NMFC10_GTable, vertex.label = V(NMFC10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
NMFC10_G.clusterCoef <- transitivity(NMFC10_GTable, type="global") #cluster coefficient
NMFC10_G.degreeCent <- centralization.degree(NMFC10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_Gftn <- as.network.matrix(NMFC10_Gft)
NMFC10_G.netDensity <- network.density(NMFC10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_G.entropy <- entropy(NMFC10_Gft) #entropy

NMFC10_G.netMx <- cbind(NMFC10_G.netMx, NMFC10_G.clusterCoef, NMFC10_G.degreeCent$centralization,
                        NMFC10_G.netDensity, NMFC10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************

round = 10
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
NMFC10_Bg2 <- data.frame(NMFC10_B)
NMFC10_Bg2 <- NMFC10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_Bg2$player1
player2vector <- NMFC10_Bg2$player2
NMFC10_Bg3 <- NMFC10_Bg2
NMFC10_Bg3$p1inp2vec <- is.element(NMFC10_Bg3$player1, player2vector)
NMFC10_Bg3$p2inp1vec <- is.element(NMFC10_Bg3$player2, player1vector)

addPlayer1 <- NMFC10_Bg3[ which(NMFC10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_Bg3[ which(NMFC10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_Bg2 <- rbind(NMFC10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
NMFC10_Bft <- ftable(NMFC10_Bg2$player1, NMFC10_Bg2$player2)
NMFC10_Bft2 <- as.matrix(NMFC10_Bft)
numRows <- nrow(NMFC10_Bft2)
numCols <- ncol(NMFC10_Bft2)
NMFC10_Bft3 <- NMFC10_Bft2[c(2:numRows) , c(2:numCols)]
NMFC10_BTable <- graph.adjacency(NMFC10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(NMFC10_BTable, vertex.label = V(NMFC10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
NMFC10_B.clusterCoef <- transitivity(NMFC10_BTable, type="global") #cluster coefficient
NMFC10_B.degreeCent <- centralization.degree(NMFC10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_Bftn <- as.network.matrix(NMFC10_Bft)
NMFC10_B.netDensity <- network.density(NMFC10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_B.entropy <- entropy(NMFC10_Bft) #entropy

NMFC10_B.netMx <- cbind(NMFC10_B.netMx, NMFC10_B.clusterCoef, NMFC10_B.degreeCent$centralization,
                        NMFC10_B.netDensity, NMFC10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************

round = 10
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
NMFC10_SFg2 <- data.frame(NMFC10_SF)
NMFC10_SFg2 <- NMFC10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_SFg2$player1
player2vector <- NMFC10_SFg2$player2
NMFC10_SFg3 <- NMFC10_SFg2
NMFC10_SFg3$p1inp2vec <- is.element(NMFC10_SFg3$player1, player2vector)
NMFC10_SFg3$p2inp1vec <- is.element(NMFC10_SFg3$player2, player1vector)

addPlayer1 <- NMFC10_SFg3[ which(NMFC10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_SFg3[ which(NMFC10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_SFg2 <- rbind(NMFC10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
NMFC10_SFft <- ftable(NMFC10_SFg2$player1, NMFC10_SFg2$player2)
NMFC10_SFft2 <- as.matrix(NMFC10_SFft)
numRows <- nrow(NMFC10_SFft2)
numCols <- ncol(NMFC10_SFft2)
NMFC10_SFft3 <- NMFC10_SFft2[c(2:numRows) , c(2:numCols)]
NMFC10_SFTable <- graph.adjacency(NMFC10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(NMFC10_SFTable, vertex.label = V(NMFC10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
NMFC10_SF.clusterCoef <- transitivity(NMFC10_SFTable, type="global") #cluster coefficient
NMFC10_SF.degreeCent <- centralization.degree(NMFC10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_SFftn <- as.network.matrix(NMFC10_SFft)
NMFC10_SF.netDensity <- network.density(NMFC10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_SF.entropy <- entropy(NMFC10_SFft) #entropy

NMFC10_SF.netMx <- cbind(NMFC10_SF.netMx, NMFC10_SF.clusterCoef, NMFC10_SF.degreeCent$centralization,
                         NMFC10_SF.netDensity, NMFC10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
NMFC10_TFg2 <- data.frame(NMFC10_TF)
NMFC10_TFg2 <- NMFC10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_TFg2$player1
player2vector <- NMFC10_TFg2$player2
NMFC10_TFg3 <- NMFC10_TFg2
NMFC10_TFg3$p1inp2vec <- is.element(NMFC10_TFg3$player1, player2vector)
NMFC10_TFg3$p2inp1vec <- is.element(NMFC10_TFg3$player2, player1vector)

addPlayer1 <- NMFC10_TFg3[ which(NMFC10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_TFg3[ which(NMFC10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_TFg2 <- rbind(NMFC10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
NMFC10_TFft <- ftable(NMFC10_TFg2$player1, NMFC10_TFg2$player2)
NMFC10_TFft2 <- as.matrix(NMFC10_TFft)
numRows <- nrow(NMFC10_TFft2)
numCols <- ncol(NMFC10_TFft2)
NMFC10_TFft3 <- NMFC10_TFft2[c(2:numRows) , c(2:numCols)]
NMFC10_TFTable <- graph.adjacency(NMFC10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(NMFC10_TFTable, vertex.label = V(NMFC10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
NMFC10_TF.clusterCoef <- transitivity(NMFC10_TFTable, type="global") #cluster coefficient
NMFC10_TF.degreeCent <- centralization.degree(NMFC10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_TFftn <- as.network.matrix(NMFC10_TFft)
NMFC10_TF.netDensity <- network.density(NMFC10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_TF.entropy <- entropy(NMFC10_TFft) #entropy

NMFC10_TF.netMx <- cbind(NMFC10_TF.netMx, NMFC10_TF.clusterCoef, NMFC10_TF.degreeCent$centralization,
                         NMFC10_TF.netDensity, NMFC10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
NMFC10_SAMg2 <- data.frame(NMFC10_SAM)
NMFC10_SAMg2 <- NMFC10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_SAMg2$player1
player2vector <- NMFC10_SAMg2$player2
NMFC10_SAMg3 <- NMFC10_SAMg2
NMFC10_SAMg3$p1inp2vec <- is.element(NMFC10_SAMg3$player1, player2vector)
NMFC10_SAMg3$p2inp1vec <- is.element(NMFC10_SAMg3$player2, player1vector)

addPlayer1 <- NMFC10_SAMg3[ which(NMFC10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_SAMg3[ which(NMFC10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_SAMg2 <- rbind(NMFC10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
NMFC10_SAMft <- ftable(NMFC10_SAMg2$player1, NMFC10_SAMg2$player2)
NMFC10_SAMft2 <- as.matrix(NMFC10_SAMft)
numRows <- nrow(NMFC10_SAMft2)
numCols <- ncol(NMFC10_SAMft2)
NMFC10_SAMft3 <- NMFC10_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC10_SAMTable <- graph.adjacency(NMFC10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(NMFC10_SAMTable, vertex.label = V(NMFC10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
NMFC10_SAM.clusterCoef <- transitivity(NMFC10_SAMTable, type="global") #cluster coefficient
NMFC10_SAM.degreeCent <- centralization.degree(NMFC10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_SAMftn <- as.network.matrix(NMFC10_SAMft)
NMFC10_SAM.netDensity <- network.density(NMFC10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_SAM.entropy <- entropy(NMFC10_SAMft) #entropy

NMFC10_SAM.netMx <- cbind(NMFC10_SAM.netMx, NMFC10_SAM.clusterCoef, NMFC10_SAM.degreeCent$centralization,
                          NMFC10_SAM.netDensity, NMFC10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
NMFC10_TAMg2 <- data.frame(NMFC10_TAM)
NMFC10_TAMg2 <- NMFC10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_TAMg2$player1
player2vector <- NMFC10_TAMg2$player2
NMFC10_TAMg3 <- NMFC10_TAMg2
NMFC10_TAMg3$p1inp2vec <- is.element(NMFC10_TAMg3$player1, player2vector)
NMFC10_TAMg3$p2inp1vec <- is.element(NMFC10_TAMg3$player2, player1vector)

addPlayer1 <- NMFC10_TAMg3[ which(NMFC10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_TAMg3[ which(NMFC10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_TAMg2 <- rbind(NMFC10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
NMFC10_TAMft <- ftable(NMFC10_TAMg2$player1, NMFC10_TAMg2$player2)
NMFC10_TAMft2 <- as.matrix(NMFC10_TAMft)
numRows <- nrow(NMFC10_TAMft2)
numCols <- ncol(NMFC10_TAMft2)
NMFC10_TAMft3 <- NMFC10_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC10_TAMTable <- graph.adjacency(NMFC10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(NMFC10_TAMTable, vertex.label = V(NMFC10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
NMFC10_TAM.clusterCoef <- transitivity(NMFC10_TAMTable, type="global") #cluster coefficient
NMFC10_TAM.degreeCent <- centralization.degree(NMFC10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_TAMftn <- as.network.matrix(NMFC10_TAMft)
NMFC10_TAM.netDensity <- network.density(NMFC10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_TAM.entropy <- entropy(NMFC10_TAMft) #entropy

NMFC10_TAM.netMx <- cbind(NMFC10_TAM.netMx, NMFC10_TAM.clusterCoef, NMFC10_TAM.degreeCent$centralization,
                          NMFC10_TAM.netDensity, NMFC10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
NMFC10_SDMg2 <- data.frame(NMFC10_SDM)
NMFC10_SDMg2 <- NMFC10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_SDMg2$player1
player2vector <- NMFC10_SDMg2$player2
NMFC10_SDMg3 <- NMFC10_SDMg2
NMFC10_SDMg3$p1inp2vec <- is.element(NMFC10_SDMg3$player1, player2vector)
NMFC10_SDMg3$p2inp1vec <- is.element(NMFC10_SDMg3$player2, player1vector)

addPlayer1 <- NMFC10_SDMg3[ which(NMFC10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_SDMg3[ which(NMFC10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_SDMg2 <- rbind(NMFC10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
NMFC10_SDMft <- ftable(NMFC10_SDMg2$player1, NMFC10_SDMg2$player2)
NMFC10_SDMft2 <- as.matrix(NMFC10_SDMft)
numRows <- nrow(NMFC10_SDMft2)
numCols <- ncol(NMFC10_SDMft2)
NMFC10_SDMft3 <- NMFC10_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC10_SDMTable <- graph.adjacency(NMFC10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(NMFC10_SDMTable, vertex.label = V(NMFC10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
NMFC10_SDM.clusterCoef <- transitivity(NMFC10_SDMTable, type="global") #cluster coefficient
NMFC10_SDM.degreeCent <- centralization.degree(NMFC10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_SDMftn <- as.network.matrix(NMFC10_SDMft)
NMFC10_SDM.netDensity <- network.density(NMFC10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_SDM.entropy <- entropy(NMFC10_SDMft) #entropy

NMFC10_SDM.netMx <- cbind(NMFC10_SDM.netMx, NMFC10_SDM.clusterCoef, NMFC10_SDM.degreeCent$centralization,
                          NMFC10_SDM.netDensity, NMFC10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
NMFC10_TDMg2 <- data.frame(NMFC10_TDM)
NMFC10_TDMg2 <- NMFC10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_TDMg2$player1
player2vector <- NMFC10_TDMg2$player2
NMFC10_TDMg3 <- NMFC10_TDMg2
NMFC10_TDMg3$p1inp2vec <- is.element(NMFC10_TDMg3$player1, player2vector)
NMFC10_TDMg3$p2inp1vec <- is.element(NMFC10_TDMg3$player2, player1vector)

addPlayer1 <- NMFC10_TDMg3[ which(NMFC10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_TDMg3[ which(NMFC10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_TDMg2 <- rbind(NMFC10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
NMFC10_TDMft <- ftable(NMFC10_TDMg2$player1, NMFC10_TDMg2$player2)
NMFC10_TDMft2 <- as.matrix(NMFC10_TDMft)
numRows <- nrow(NMFC10_TDMft2)
numCols <- ncol(NMFC10_TDMft2)
NMFC10_TDMft3 <- NMFC10_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC10_TDMTable <- graph.adjacency(NMFC10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(NMFC10_TDMTable, vertex.label = V(NMFC10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
NMFC10_TDM.clusterCoef <- transitivity(NMFC10_TDMTable, type="global") #cluster coefficient
NMFC10_TDM.degreeCent <- centralization.degree(NMFC10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_TDMftn <- as.network.matrix(NMFC10_TDMft)
NMFC10_TDM.netDensity <- network.density(NMFC10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_TDM.entropy <- entropy(NMFC10_TDMft) #entropy

NMFC10_TDM.netMx <- cbind(NMFC10_TDM.netMx, NMFC10_TDM.clusterCoef, NMFC10_TDM.degreeCent$centralization,
                          NMFC10_TDM.netDensity, NMFC10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
NMFC10_SDg2 <- data.frame(NMFC10_SD)
NMFC10_SDg2 <- NMFC10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_SDg2$player1
player2vector <- NMFC10_SDg2$player2
NMFC10_SDg3 <- NMFC10_SDg2
NMFC10_SDg3$p1inp2vec <- is.element(NMFC10_SDg3$player1, player2vector)
NMFC10_SDg3$p2inp1vec <- is.element(NMFC10_SDg3$player2, player1vector)

addPlayer1 <- NMFC10_SDg3[ which(NMFC10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_SDg3[ which(NMFC10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_SDg2 <- rbind(NMFC10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
NMFC10_SDft <- ftable(NMFC10_SDg2$player1, NMFC10_SDg2$player2)
NMFC10_SDft2 <- as.matrix(NMFC10_SDft)
numRows <- nrow(NMFC10_SDft2)
numCols <- ncol(NMFC10_SDft2)
NMFC10_SDft3 <- NMFC10_SDft2[c(2:numRows) , c(2:numCols)]
NMFC10_SDTable <- graph.adjacency(NMFC10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(NMFC10_SDTable, vertex.label = V(NMFC10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
NMFC10_SD.clusterCoef <- transitivity(NMFC10_SDTable, type="global") #cluster coefficient
NMFC10_SD.degreeCent <- centralization.degree(NMFC10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_SDftn <- as.network.matrix(NMFC10_SDft)
NMFC10_SD.netDensity <- network.density(NMFC10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_SD.entropy <- entropy(NMFC10_SDft) #entropy

NMFC10_SD.netMx <- cbind(NMFC10_SD.netMx, NMFC10_SD.clusterCoef, NMFC10_SD.degreeCent$centralization,
                         NMFC10_SD.netDensity, NMFC10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
NMFC10_TDg2 <- data.frame(NMFC10_TD)
NMFC10_TDg2 <- NMFC10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_TDg2$player1
player2vector <- NMFC10_TDg2$player2
NMFC10_TDg3 <- NMFC10_TDg2
NMFC10_TDg3$p1inp2vec <- is.element(NMFC10_TDg3$player1, player2vector)
NMFC10_TDg3$p2inp1vec <- is.element(NMFC10_TDg3$player2, player1vector)

addPlayer1 <- NMFC10_TDg3[ which(NMFC10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_TDg3[ which(NMFC10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_TDg2 <- rbind(NMFC10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
NMFC10_TDft <- ftable(NMFC10_TDg2$player1, NMFC10_TDg2$player2)
NMFC10_TDft2 <- as.matrix(NMFC10_TDft)
numRows <- nrow(NMFC10_TDft2)
numCols <- ncol(NMFC10_TDft2)
NMFC10_TDft3 <- NMFC10_TDft2[c(2:numRows) , c(2:numCols)]
NMFC10_TDTable <- graph.adjacency(NMFC10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(NMFC10_TDTable, vertex.label = V(NMFC10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
NMFC10_TD.clusterCoef <- transitivity(NMFC10_TDTable, type="global") #cluster coefficient
NMFC10_TD.degreeCent <- centralization.degree(NMFC10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_TDftn <- as.network.matrix(NMFC10_TDft)
NMFC10_TD.netDensity <- network.density(NMFC10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_TD.entropy <- entropy(NMFC10_TDft) #entropy

NMFC10_TD.netMx <- cbind(NMFC10_TD.netMx, NMFC10_TD.clusterCoef, NMFC10_TD.degreeCent$centralization,
                         NMFC10_TD.netDensity, NMFC10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
NMFC10_QTg2 <- data.frame(NMFC10_QT)
NMFC10_QTg2 <- NMFC10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC10_QTg2$player1
player2vector <- NMFC10_QTg2$player2
NMFC10_QTg3 <- NMFC10_QTg2
NMFC10_QTg3$p1inp2vec <- is.element(NMFC10_QTg3$player1, player2vector)
NMFC10_QTg3$p2inp1vec <- is.element(NMFC10_QTg3$player2, player1vector)

addPlayer1 <- NMFC10_QTg3[ which(NMFC10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC10_QTg3[ which(NMFC10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC10_QTg2 <- rbind(NMFC10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
NMFC10_QTft <- ftable(NMFC10_QTg2$player1, NMFC10_QTg2$player2)
NMFC10_QTft2 <- as.matrix(NMFC10_QTft)
numRows <- nrow(NMFC10_QTft2)
numCols <- ncol(NMFC10_QTft2)
NMFC10_QTft3 <- NMFC10_QTft2[c(2:numRows) , c(2:numCols)]
NMFC10_QTTable <- graph.adjacency(NMFC10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(NMFC10_QTTable, vertex.label = V(NMFC10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
NMFC10_QT.clusterCoef <- transitivity(NMFC10_QTTable, type="global") #cluster coefficient
NMFC10_QT.degreeCent <- centralization.degree(NMFC10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC10_QTftn <- as.network.matrix(NMFC10_QTft)
NMFC10_QT.netDensity <- network.density(NMFC10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC10_QT.entropy <- entropy(NMFC10_QTft) #entropy

NMFC10_QT.netMx <- cbind(NMFC10_QT.netMx, NMFC10_QT.clusterCoef, NMFC10_QT.degreeCent$centralization,
                         NMFC10_QT.netDensity, NMFC10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC10_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Goal_F"
PORT10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
PORT10_Gg2 <- data.frame(PORT10_G)
PORT10_Gg2 <- PORT10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_Gg2$player1
player2vector <- PORT10_Gg2$player2
PORT10_Gg3 <- PORT10_Gg2
PORT10_Gg3$p1inp2vec <- is.element(PORT10_Gg3$player1, player2vector)
PORT10_Gg3$p2inp1vec <- is.element(PORT10_Gg3$player2, player1vector)

addPlayer1 <- PORT10_Gg3[ which(PORT10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT10_Gg2 <- rbind(PORT10_Gg2, addPlayer1)

#ROUND 10, Goal graph using weighted edges
PORT10_Gft <- ftable(PORT10_Gg2$player1, PORT10_Gg2$player2)
PORT10_Gft2 <- as.matrix(PORT10_Gft)
numRows <- nrow(PORT10_Gft2)
numCols <- ncol(PORT10_Gft2)
PORT10_Gft3 <- PORT10_Gft2[c(2:numRows) , c(1:numCols)]
PORT10_GTable <- graph.adjacency(PORT10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(PORT10_GTable, vertex.label = V(PORT10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
PORT10_G.clusterCoef <- transitivity(PORT10_GTable, type="global") #cluster coefficient
PORT10_G.degreeCent <- centralization.degree(PORT10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_Gftn <- as.network.matrix(PORT10_Gft)
PORT10_G.netDensity <- network.density(PORT10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_G.entropy <- entropy(PORT10_Gft) #entropy

PORT10_G.netMx <- cbind(PORT10_G.netMx, PORT10_G.clusterCoef, PORT10_G.degreeCent$centralization,
                        PORT10_G.netDensity, PORT10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Behind_F"
PORT10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
PORT10_Bg2 <- data.frame(PORT10_B)
PORT10_Bg2 <- PORT10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_Bg2$player1
player2vector <- PORT10_Bg2$player2
PORT10_Bg3 <- PORT10_Bg2
PORT10_Bg3$p1inp2vec <- is.element(PORT10_Bg3$player1, player2vector)
PORT10_Bg3$p2inp1vec <- is.element(PORT10_Bg3$player2, player1vector)

addPlayer1 <- PORT10_Bg3[ which(PORT10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_Bg3[ which(PORT10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_Bg2 <- rbind(PORT10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
PORT10_Bft <- ftable(PORT10_Bg2$player1, PORT10_Bg2$player2)
PORT10_Bft2 <- as.matrix(PORT10_Bft)
numRows <- nrow(PORT10_Bft2)
numCols <- ncol(PORT10_Bft2)
PORT10_Bft3 <- PORT10_Bft2[c(2:numRows) , c(2:numCols)]
PORT10_BTable <- graph.adjacency(PORT10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(PORT10_BTable, vertex.label = V(PORT10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
PORT10_B.clusterCoef <- transitivity(PORT10_BTable, type="global") #cluster coefficient
PORT10_B.degreeCent <- centralization.degree(PORT10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_Bftn <- as.network.matrix(PORT10_Bft)
PORT10_B.netDensity <- network.density(PORT10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_B.entropy <- entropy(PORT10_Bft) #entropy

PORT10_B.netMx <- cbind(PORT10_B.netMx, PORT10_B.clusterCoef, PORT10_B.degreeCent$centralization,
                        PORT10_B.netDensity, PORT10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
PORT10_SFg2 <- data.frame(PORT10_SF)
PORT10_SFg2 <- PORT10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_SFg2$player1
player2vector <- PORT10_SFg2$player2
PORT10_SFg3 <- PORT10_SFg2
PORT10_SFg3$p1inp2vec <- is.element(PORT10_SFg3$player1, player2vector)
PORT10_SFg3$p2inp1vec <- is.element(PORT10_SFg3$player2, player1vector)

addPlayer1 <- PORT10_SFg3[ which(PORT10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_SFg3[ which(PORT10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_SFg2 <- rbind(PORT10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
PORT10_SFft <- ftable(PORT10_SFg2$player1, PORT10_SFg2$player2)
PORT10_SFft2 <- as.matrix(PORT10_SFft)
numRows <- nrow(PORT10_SFft2)
numCols <- ncol(PORT10_SFft2)
PORT10_SFft3 <- PORT10_SFft2[c(2:numRows) , c(2:numCols)]
PORT10_SFTable <- graph.adjacency(PORT10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(PORT10_SFTable, vertex.label = V(PORT10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
PORT10_SF.clusterCoef <- transitivity(PORT10_SFTable, type="global") #cluster coefficient
PORT10_SF.degreeCent <- centralization.degree(PORT10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_SFftn <- as.network.matrix(PORT10_SFft)
PORT10_SF.netDensity <- network.density(PORT10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_SF.entropy <- entropy(PORT10_SFft) #entropy

PORT10_SF.netMx <- cbind(PORT10_SF.netMx, PORT10_SF.clusterCoef, PORT10_SF.degreeCent$centralization,
                         PORT10_SF.netDensity, PORT10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
PORT10_TFg2 <- data.frame(PORT10_TF)
PORT10_TFg2 <- PORT10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_TFg2$player1
player2vector <- PORT10_TFg2$player2
PORT10_TFg3 <- PORT10_TFg2
PORT10_TFg3$p1inp2vec <- is.element(PORT10_TFg3$player1, player2vector)
PORT10_TFg3$p2inp1vec <- is.element(PORT10_TFg3$player2, player1vector)

addPlayer1 <- PORT10_TFg3[ which(PORT10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT10_TFg2 <- rbind(PORT10_TFg2, addPlayer1)

#ROUND 10, FWD Turnover graph using weighted edges
PORT10_TFft <- ftable(PORT10_TFg2$player1, PORT10_TFg2$player2)
PORT10_TFft2 <- as.matrix(PORT10_TFft)
numRows <- nrow(PORT10_TFft2)
numCols <- ncol(PORT10_TFft2)
PORT10_TFft3 <- PORT10_TFft2[c(2:numRows) , c(1:numCols)]
PORT10_TFTable <- graph.adjacency(PORT10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(PORT10_TFTable, vertex.label = V(PORT10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
PORT10_TF.clusterCoef <- transitivity(PORT10_TFTable, type="global") #cluster coefficient
PORT10_TF.degreeCent <- centralization.degree(PORT10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_TFftn <- as.network.matrix(PORT10_TFft)
PORT10_TF.netDensity <- network.density(PORT10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_TF.entropy <- entropy(PORT10_TFft) #entropy

PORT10_TF.netMx <- cbind(PORT10_TF.netMx, PORT10_TF.clusterCoef, PORT10_TF.degreeCent$centralization,
                         PORT10_TF.netDensity, PORT10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
PORT10_SAMg2 <- data.frame(PORT10_SAM)
PORT10_SAMg2 <- PORT10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_SAMg2$player1
player2vector <- PORT10_SAMg2$player2
PORT10_SAMg3 <- PORT10_SAMg2
PORT10_SAMg3$p1inp2vec <- is.element(PORT10_SAMg3$player1, player2vector)
PORT10_SAMg3$p2inp1vec <- is.element(PORT10_SAMg3$player2, player1vector)

addPlayer1 <- PORT10_SAMg3[ which(PORT10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_SAMg3[ which(PORT10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_SAMg2 <- rbind(PORT10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
PORT10_SAMft <- ftable(PORT10_SAMg2$player1, PORT10_SAMg2$player2)
PORT10_SAMft2 <- as.matrix(PORT10_SAMft)
numRows <- nrow(PORT10_SAMft2)
numCols <- ncol(PORT10_SAMft2)
PORT10_SAMft3 <- PORT10_SAMft2[c(2:numRows) , c(2:numCols)]
PORT10_SAMTable <- graph.adjacency(PORT10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(PORT10_SAMTable, vertex.label = V(PORT10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
PORT10_SAM.clusterCoef <- transitivity(PORT10_SAMTable, type="global") #cluster coefficient
PORT10_SAM.degreeCent <- centralization.degree(PORT10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_SAMftn <- as.network.matrix(PORT10_SAMft)
PORT10_SAM.netDensity <- network.density(PORT10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_SAM.entropy <- entropy(PORT10_SAMft) #entropy

PORT10_SAM.netMx <- cbind(PORT10_SAM.netMx, PORT10_SAM.clusterCoef, PORT10_SAM.degreeCent$centralization,
                          PORT10_SAM.netDensity, PORT10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
PORT10_TAMg2 <- data.frame(PORT10_TAM)
PORT10_TAMg2 <- PORT10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_TAMg2$player1
player2vector <- PORT10_TAMg2$player2
PORT10_TAMg3 <- PORT10_TAMg2
PORT10_TAMg3$p1inp2vec <- is.element(PORT10_TAMg3$player1, player2vector)
PORT10_TAMg3$p2inp1vec <- is.element(PORT10_TAMg3$player2, player1vector)

addPlayer1 <- PORT10_TAMg3[ which(PORT10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT10_TAMg2 <- rbind(PORT10_TAMg2, addPlayer1)

#ROUND 10, AM Turnover graph using weighted edges
PORT10_TAMft <- ftable(PORT10_TAMg2$player1, PORT10_TAMg2$player2)
PORT10_TAMft2 <- as.matrix(PORT10_TAMft)
numRows <- nrow(PORT10_TAMft2)
numCols <- ncol(PORT10_TAMft2)
PORT10_TAMft3 <- PORT10_TAMft2[c(2:numRows) , c(1:numCols)]
PORT10_TAMTable <- graph.adjacency(PORT10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(PORT10_TAMTable, vertex.label = V(PORT10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
PORT10_TAM.clusterCoef <- transitivity(PORT10_TAMTable, type="global") #cluster coefficient
PORT10_TAM.degreeCent <- centralization.degree(PORT10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_TAMftn <- as.network.matrix(PORT10_TAMft)
PORT10_TAM.netDensity <- network.density(PORT10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_TAM.entropy <- entropy(PORT10_TAMft) #entropy

PORT10_TAM.netMx <- cbind(PORT10_TAM.netMx, PORT10_TAM.clusterCoef, PORT10_TAM.degreeCent$centralization,
                          PORT10_TAM.netDensity, PORT10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
PORT10_SDMg2 <- data.frame(PORT10_SDM)
PORT10_SDMg2 <- PORT10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_SDMg2$player1
player2vector <- PORT10_SDMg2$player2
PORT10_SDMg3 <- PORT10_SDMg2
PORT10_SDMg3$p1inp2vec <- is.element(PORT10_SDMg3$player1, player2vector)
PORT10_SDMg3$p2inp1vec <- is.element(PORT10_SDMg3$player2, player1vector)

addPlayer1 <- PORT10_SDMg3[ which(PORT10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_SDMg3[ which(PORT10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_SDMg2 <- rbind(PORT10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
PORT10_SDMft <- ftable(PORT10_SDMg2$player1, PORT10_SDMg2$player2)
PORT10_SDMft2 <- as.matrix(PORT10_SDMft)
numRows <- nrow(PORT10_SDMft2)
numCols <- ncol(PORT10_SDMft2)
PORT10_SDMft3 <- PORT10_SDMft2[c(2:numRows) , c(2:numCols)]
PORT10_SDMTable <- graph.adjacency(PORT10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(PORT10_SDMTable, vertex.label = V(PORT10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
PORT10_SDM.clusterCoef <- transitivity(PORT10_SDMTable, type="global") #cluster coefficient
PORT10_SDM.degreeCent <- centralization.degree(PORT10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_SDMftn <- as.network.matrix(PORT10_SDMft)
PORT10_SDM.netDensity <- network.density(PORT10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_SDM.entropy <- entropy(PORT10_SDMft) #entropy

PORT10_SDM.netMx <- cbind(PORT10_SDM.netMx, PORT10_SDM.clusterCoef, PORT10_SDM.degreeCent$centralization,
                          PORT10_SDM.netDensity, PORT10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
PORT10_TDMg2 <- data.frame(PORT10_TDM)
PORT10_TDMg2 <- PORT10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_TDMg2$player1
player2vector <- PORT10_TDMg2$player2
PORT10_TDMg3 <- PORT10_TDMg2
PORT10_TDMg3$p1inp2vec <- is.element(PORT10_TDMg3$player1, player2vector)
PORT10_TDMg3$p2inp1vec <- is.element(PORT10_TDMg3$player2, player1vector)

addPlayer1 <- PORT10_TDMg3[ which(PORT10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_TDMg3[ which(PORT10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_TDMg2 <- rbind(PORT10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
PORT10_TDMft <- ftable(PORT10_TDMg2$player1, PORT10_TDMg2$player2)
PORT10_TDMft2 <- as.matrix(PORT10_TDMft)
numRows <- nrow(PORT10_TDMft2)
numCols <- ncol(PORT10_TDMft2)
PORT10_TDMft3 <- PORT10_TDMft2[c(2:numRows) , c(2:numCols)]
PORT10_TDMTable <- graph.adjacency(PORT10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(PORT10_TDMTable, vertex.label = V(PORT10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
PORT10_TDM.clusterCoef <- transitivity(PORT10_TDMTable, type="global") #cluster coefficient
PORT10_TDM.degreeCent <- centralization.degree(PORT10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_TDMftn <- as.network.matrix(PORT10_TDMft)
PORT10_TDM.netDensity <- network.density(PORT10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_TDM.entropy <- entropy(PORT10_TDMft) #entropy

PORT10_TDM.netMx <- cbind(PORT10_TDM.netMx, PORT10_TDM.clusterCoef, PORT10_TDM.degreeCent$centralization,
                          PORT10_TDM.netDensity, PORT10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
PORT10_SDg2 <- data.frame(PORT10_SD)
PORT10_SDg2 <- PORT10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_SDg2$player1
player2vector <- PORT10_SDg2$player2
PORT10_SDg3 <- PORT10_SDg2
PORT10_SDg3$p1inp2vec <- is.element(PORT10_SDg3$player1, player2vector)
PORT10_SDg3$p2inp1vec <- is.element(PORT10_SDg3$player2, player1vector)

addPlayer1 <- PORT10_SDg3[ which(PORT10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_SDg3[ which(PORT10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_SDg2 <- rbind(PORT10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
PORT10_SDft <- ftable(PORT10_SDg2$player1, PORT10_SDg2$player2)
PORT10_SDft2 <- as.matrix(PORT10_SDft)
numRows <- nrow(PORT10_SDft2)
numCols <- ncol(PORT10_SDft2)
PORT10_SDft3 <- PORT10_SDft2[c(2:numRows) , c(2:numCols)]
PORT10_SDTable <- graph.adjacency(PORT10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(PORT10_SDTable, vertex.label = V(PORT10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
PORT10_SD.clusterCoef <- transitivity(PORT10_SDTable, type="global") #cluster coefficient
PORT10_SD.degreeCent <- centralization.degree(PORT10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_SDftn <- as.network.matrix(PORT10_SDft)
PORT10_SD.netDensity <- network.density(PORT10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_SD.entropy <- entropy(PORT10_SDft) #entropy

PORT10_SD.netMx <- cbind(PORT10_SD.netMx, PORT10_SD.clusterCoef, PORT10_SD.degreeCent$centralization,
                         PORT10_SD.netDensity, PORT10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
PORT10_TDg2 <- data.frame(PORT10_TD)
PORT10_TDg2 <- PORT10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_TDg2$player1
player2vector <- PORT10_TDg2$player2
PORT10_TDg3 <- PORT10_TDg2
PORT10_TDg3$p1inp2vec <- is.element(PORT10_TDg3$player1, player2vector)
PORT10_TDg3$p2inp1vec <- is.element(PORT10_TDg3$player2, player1vector)

addPlayer1 <- PORT10_TDg3[ which(PORT10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_TDg3[ which(PORT10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_TDg2 <- rbind(PORT10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
PORT10_TDft <- ftable(PORT10_TDg2$player1, PORT10_TDg2$player2)
PORT10_TDft2 <- as.matrix(PORT10_TDft)
numRows <- nrow(PORT10_TDft2)
numCols <- ncol(PORT10_TDft2)
PORT10_TDft3 <- PORT10_TDft2[c(2:numRows) , c(2:numCols)]
PORT10_TDTable <- graph.adjacency(PORT10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(PORT10_TDTable, vertex.label = V(PORT10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
PORT10_TD.clusterCoef <- transitivity(PORT10_TDTable, type="global") #cluster coefficient
PORT10_TD.degreeCent <- centralization.degree(PORT10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_TDftn <- as.network.matrix(PORT10_TDft)
PORT10_TD.netDensity <- network.density(PORT10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_TD.entropy <- entropy(PORT10_TDft) #entropy

PORT10_TD.netMx <- cbind(PORT10_TD.netMx, PORT10_TD.clusterCoef, PORT10_TD.degreeCent$centralization,
                         PORT10_TD.netDensity, PORT10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
PORT10_QTg2 <- data.frame(PORT10_QT)
PORT10_QTg2 <- PORT10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT10_QTg2$player1
player2vector <- PORT10_QTg2$player2
PORT10_QTg3 <- PORT10_QTg2
PORT10_QTg3$p1inp2vec <- is.element(PORT10_QTg3$player1, player2vector)
PORT10_QTg3$p2inp1vec <- is.element(PORT10_QTg3$player2, player1vector)

addPlayer1 <- PORT10_QTg3[ which(PORT10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT10_QTg3[ which(PORT10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT10_QTg2 <- rbind(PORT10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
PORT10_QTft <- ftable(PORT10_QTg2$player1, PORT10_QTg2$player2)
PORT10_QTft2 <- as.matrix(PORT10_QTft)
numRows <- nrow(PORT10_QTft2)
numCols <- ncol(PORT10_QTft2)
PORT10_QTft3 <- PORT10_QTft2[c(2:numRows) , c(2:numCols)]
PORT10_QTTable <- graph.adjacency(PORT10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(PORT10_QTTable, vertex.label = V(PORT10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
PORT10_QT.clusterCoef <- transitivity(PORT10_QTTable, type="global") #cluster coefficient
PORT10_QT.degreeCent <- centralization.degree(PORT10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT10_QTftn <- as.network.matrix(PORT10_QTft)
PORT10_QT.netDensity <- network.density(PORT10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT10_QT.entropy <- entropy(PORT10_QTft) #entropy

PORT10_QT.netMx <- cbind(PORT10_QT.netMx, PORT10_QT.clusterCoef, PORT10_QT.degreeCent$centralization,
                         PORT10_QT.netDensity, PORT10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT10_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Goal_F"
RICH10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
RICH10_Gg2 <- data.frame(RICH10_G)
RICH10_Gg2 <- RICH10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_Gg2$player1
player2vector <- RICH10_Gg2$player2
RICH10_Gg3 <- RICH10_Gg2
RICH10_Gg3$p1inp2vec <- is.element(RICH10_Gg3$player1, player2vector)
RICH10_Gg3$p2inp1vec <- is.element(RICH10_Gg3$player2, player1vector)

addPlayer1 <- RICH10_Gg3[ which(RICH10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_Gg3[ which(RICH10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_Gg2 <- rbind(RICH10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
RICH10_Gft <- ftable(RICH10_Gg2$player1, RICH10_Gg2$player2)
RICH10_Gft2 <- as.matrix(RICH10_Gft)
numRows <- nrow(RICH10_Gft2)
numCols <- ncol(RICH10_Gft2)
RICH10_Gft3 <- RICH10_Gft2[c(2:numRows) , c(2:numCols)]
RICH10_GTable <- graph.adjacency(RICH10_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(RICH10_GTable, vertex.label = V(RICH10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
RICH10_G.clusterCoef <- transitivity(RICH10_GTable, type="global") #cluster coefficient
RICH10_G.degreeCent <- centralization.degree(RICH10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_Gftn <- as.network.matrix(RICH10_Gft)
RICH10_G.netDensity <- network.density(RICH10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_G.entropy <- entropy(RICH10_Gft) #entropy

RICH10_G.netMx <- cbind(RICH10_G.netMx, RICH10_G.clusterCoef, RICH10_G.degreeCent$centralization,
                        RICH10_G.netDensity, RICH10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Behind_F"
RICH10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
RICH10_Bg2 <- data.frame(RICH10_B)
RICH10_Bg2 <- RICH10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_Bg2$player1
player2vector <- RICH10_Bg2$player2
RICH10_Bg3 <- RICH10_Bg2
RICH10_Bg3$p1inp2vec <- is.element(RICH10_Bg3$player1, player2vector)
RICH10_Bg3$p2inp1vec <- is.element(RICH10_Bg3$player2, player1vector)

addPlayer1 <- RICH10_Bg3[ which(RICH10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_Bg3[ which(RICH10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_Bg2 <- rbind(RICH10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
RICH10_Bft <- ftable(RICH10_Bg2$player1, RICH10_Bg2$player2)
RICH10_Bft2 <- as.matrix(RICH10_Bft)
numRows <- nrow(RICH10_Bft2)
numCols <- ncol(RICH10_Bft2)
RICH10_Bft3 <- RICH10_Bft2[c(2:numRows) , c(2:numCols)]
RICH10_BTable <- graph.adjacency(RICH10_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(RICH10_BTable, vertex.label = V(RICH10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
RICH10_B.clusterCoef <- transitivity(RICH10_BTable, type="global") #cluster coefficient
RICH10_B.degreeCent <- centralization.degree(RICH10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_Bftn <- as.network.matrix(RICH10_Bft)
RICH10_B.netDensity <- network.density(RICH10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_B.entropy <- entropy(RICH10_Bft) #entropy

RICH10_B.netMx <- cbind(RICH10_B.netMx, RICH10_B.clusterCoef, RICH10_B.degreeCent$centralization,
                        RICH10_B.netDensity, RICH10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
RICH10_SFg2 <- data.frame(RICH10_SF)
RICH10_SFg2 <- RICH10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_SFg2$player1
player2vector <- RICH10_SFg2$player2
RICH10_SFg3 <- RICH10_SFg2
RICH10_SFg3$p1inp2vec <- is.element(RICH10_SFg3$player1, player2vector)
RICH10_SFg3$p2inp1vec <- is.element(RICH10_SFg3$player2, player1vector)

addPlayer1 <- RICH10_SFg3[ which(RICH10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_SFg3[ which(RICH10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_SFg2 <- rbind(RICH10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
RICH10_SFft <- ftable(RICH10_SFg2$player1, RICH10_SFg2$player2)
RICH10_SFft2 <- as.matrix(RICH10_SFft)
numRows <- nrow(RICH10_SFft2)
numCols <- ncol(RICH10_SFft2)
RICH10_SFft3 <- RICH10_SFft2[c(2:numRows) , c(2:numCols)]
RICH10_SFTable <- graph.adjacency(RICH10_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(RICH10_SFTable, vertex.label = V(RICH10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
RICH10_SF.clusterCoef <- transitivity(RICH10_SFTable, type="global") #cluster coefficient
RICH10_SF.degreeCent <- centralization.degree(RICH10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_SFftn <- as.network.matrix(RICH10_SFft)
RICH10_SF.netDensity <- network.density(RICH10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_SF.entropy <- entropy(RICH10_SFft) #entropy

RICH10_SF.netMx <- cbind(RICH10_SF.netMx, RICH10_SF.clusterCoef, RICH10_SF.degreeCent$centralization,
                         RICH10_SF.netDensity, RICH10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
RICH10_TFg2 <- data.frame(RICH10_TF)
RICH10_TFg2 <- RICH10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_TFg2$player1
player2vector <- RICH10_TFg2$player2
RICH10_TFg3 <- RICH10_TFg2
RICH10_TFg3$p1inp2vec <- is.element(RICH10_TFg3$player1, player2vector)
RICH10_TFg3$p2inp1vec <- is.element(RICH10_TFg3$player2, player1vector)

addPlayer1 <- RICH10_TFg3[ which(RICH10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_TFg3[ which(RICH10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_TFg2 <- rbind(RICH10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
RICH10_TFft <- ftable(RICH10_TFg2$player1, RICH10_TFg2$player2)
RICH10_TFft2 <- as.matrix(RICH10_TFft)
numRows <- nrow(RICH10_TFft2)
numCols <- ncol(RICH10_TFft2)
RICH10_TFft3 <- RICH10_TFft2[c(2:numRows) , c(2:numCols)]
RICH10_TFTable <- graph.adjacency(RICH10_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(RICH10_TFTable, vertex.label = V(RICH10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
RICH10_TF.clusterCoef <- transitivity(RICH10_TFTable, type="global") #cluster coefficient
RICH10_TF.degreeCent <- centralization.degree(RICH10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_TFftn <- as.network.matrix(RICH10_TFft)
RICH10_TF.netDensity <- network.density(RICH10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_TF.entropy <- entropy(RICH10_TFft) #entropy

RICH10_TF.netMx <- cbind(RICH10_TF.netMx, RICH10_TF.clusterCoef, RICH10_TF.degreeCent$centralization,
                         RICH10_TF.netDensity, RICH10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************

round = 10
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
RICH10_SAMg2 <- data.frame(RICH10_SAM)
RICH10_SAMg2 <- RICH10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_SAMg2$player1
player2vector <- RICH10_SAMg2$player2
RICH10_SAMg3 <- RICH10_SAMg2
RICH10_SAMg3$p1inp2vec <- is.element(RICH10_SAMg3$player1, player2vector)
RICH10_SAMg3$p2inp1vec <- is.element(RICH10_SAMg3$player2, player1vector)

addPlayer1 <- RICH10_SAMg3[ which(RICH10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_SAMg3[ which(RICH10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_SAMg2 <- rbind(RICH10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
RICH10_SAMft <- ftable(RICH10_SAMg2$player1, RICH10_SAMg2$player2)
RICH10_SAMft2 <- as.matrix(RICH10_SAMft)
numRows <- nrow(RICH10_SAMft2)
numCols <- ncol(RICH10_SAMft2)
RICH10_SAMft3 <- RICH10_SAMft2[c(2:numRows) , c(2:numCols)]
RICH10_SAMTable <- graph.adjacency(RICH10_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(RICH10_SAMTable, vertex.label = V(RICH10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
RICH10_SAM.clusterCoef <- transitivity(RICH10_SAMTable, type="global") #cluster coefficient
RICH10_SAM.degreeCent <- centralization.degree(RICH10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_SAMftn <- as.network.matrix(RICH10_SAMft)
RICH10_SAM.netDensity <- network.density(RICH10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_SAM.entropy <- entropy(RICH10_SAMft) #entropy

RICH10_SAM.netMx <- cbind(RICH10_SAM.netMx, RICH10_SAM.clusterCoef, RICH10_SAM.degreeCent$centralization,
                          RICH10_SAM.netDensity, RICH10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
RICH10_TAMg2 <- data.frame(RICH10_TAM)
RICH10_TAMg2 <- RICH10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_TAMg2$player1
player2vector <- RICH10_TAMg2$player2
RICH10_TAMg3 <- RICH10_TAMg2
RICH10_TAMg3$p1inp2vec <- is.element(RICH10_TAMg3$player1, player2vector)
RICH10_TAMg3$p2inp1vec <- is.element(RICH10_TAMg3$player2, player1vector)

addPlayer1 <- RICH10_TAMg3[ which(RICH10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_TAMg3[ which(RICH10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_TAMg2 <- rbind(RICH10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
RICH10_TAMft <- ftable(RICH10_TAMg2$player1, RICH10_TAMg2$player2)
RICH10_TAMft2 <- as.matrix(RICH10_TAMft)
numRows <- nrow(RICH10_TAMft2)
numCols <- ncol(RICH10_TAMft2)
RICH10_TAMft3 <- RICH10_TAMft2[c(2:numRows) , c(2:numCols)]
RICH10_TAMTable <- graph.adjacency(RICH10_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(RICH10_TAMTable, vertex.label = V(RICH10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
RICH10_TAM.clusterCoef <- transitivity(RICH10_TAMTable, type="global") #cluster coefficient
RICH10_TAM.degreeCent <- centralization.degree(RICH10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_TAMftn <- as.network.matrix(RICH10_TAMft)
RICH10_TAM.netDensity <- network.density(RICH10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_TAM.entropy <- entropy(RICH10_TAMft) #entropy

RICH10_TAM.netMx <- cbind(RICH10_TAM.netMx, RICH10_TAM.clusterCoef, RICH10_TAM.degreeCent$centralization,
                          RICH10_TAM.netDensity, RICH10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
RICH10_SDMg2 <- data.frame(RICH10_SDM)
RICH10_SDMg2 <- RICH10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_SDMg2$player1
player2vector <- RICH10_SDMg2$player2
RICH10_SDMg3 <- RICH10_SDMg2
RICH10_SDMg3$p1inp2vec <- is.element(RICH10_SDMg3$player1, player2vector)
RICH10_SDMg3$p2inp1vec <- is.element(RICH10_SDMg3$player2, player1vector)

addPlayer1 <- RICH10_SDMg3[ which(RICH10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_SDMg3[ which(RICH10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_SDMg2 <- rbind(RICH10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
RICH10_SDMft <- ftable(RICH10_SDMg2$player1, RICH10_SDMg2$player2)
RICH10_SDMft2 <- as.matrix(RICH10_SDMft)
numRows <- nrow(RICH10_SDMft2)
numCols <- ncol(RICH10_SDMft2)
RICH10_SDMft3 <- RICH10_SDMft2[c(2:numRows) , c(2:numCols)]
RICH10_SDMTable <- graph.adjacency(RICH10_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(RICH10_SDMTable, vertex.label = V(RICH10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
RICH10_SDM.clusterCoef <- transitivity(RICH10_SDMTable, type="global") #cluster coefficient
RICH10_SDM.degreeCent <- centralization.degree(RICH10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_SDMftn <- as.network.matrix(RICH10_SDMft)
RICH10_SDM.netDensity <- network.density(RICH10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_SDM.entropy <- entropy(RICH10_SDMft) #entropy

RICH10_SDM.netMx <- cbind(RICH10_SDM.netMx, RICH10_SDM.clusterCoef, RICH10_SDM.degreeCent$centralization,
                          RICH10_SDM.netDensity, RICH10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
RICH10_TDMg2 <- data.frame(RICH10_TDM)
RICH10_TDMg2 <- RICH10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_TDMg2$player1
player2vector <- RICH10_TDMg2$player2
RICH10_TDMg3 <- RICH10_TDMg2
RICH10_TDMg3$p1inp2vec <- is.element(RICH10_TDMg3$player1, player2vector)
RICH10_TDMg3$p2inp1vec <- is.element(RICH10_TDMg3$player2, player1vector)

addPlayer1 <- RICH10_TDMg3[ which(RICH10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_TDMg3[ which(RICH10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_TDMg2 <- rbind(RICH10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
RICH10_TDMft <- ftable(RICH10_TDMg2$player1, RICH10_TDMg2$player2)
RICH10_TDMft2 <- as.matrix(RICH10_TDMft)
numRows <- nrow(RICH10_TDMft2)
numCols <- ncol(RICH10_TDMft2)
RICH10_TDMft3 <- RICH10_TDMft2[c(2:numRows) , c(2:numCols)]
RICH10_TDMTable <- graph.adjacency(RICH10_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(RICH10_TDMTable, vertex.label = V(RICH10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
RICH10_TDM.clusterCoef <- transitivity(RICH10_TDMTable, type="global") #cluster coefficient
RICH10_TDM.degreeCent <- centralization.degree(RICH10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_TDMftn <- as.network.matrix(RICH10_TDMft)
RICH10_TDM.netDensity <- network.density(RICH10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_TDM.entropy <- entropy(RICH10_TDMft) #entropy

RICH10_TDM.netMx <- cbind(RICH10_TDM.netMx, RICH10_TDM.clusterCoef, RICH10_TDM.degreeCent$centralization,
                          RICH10_TDM.netDensity, RICH10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
RICH10_SDg2 <- data.frame(RICH10_SD)
RICH10_SDg2 <- RICH10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_SDg2$player1
player2vector <- RICH10_SDg2$player2
RICH10_SDg3 <- RICH10_SDg2
RICH10_SDg3$p1inp2vec <- is.element(RICH10_SDg3$player1, player2vector)
RICH10_SDg3$p2inp1vec <- is.element(RICH10_SDg3$player2, player1vector)

addPlayer1 <- RICH10_SDg3[ which(RICH10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_SDg3[ which(RICH10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_SDg2 <- rbind(RICH10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
RICH10_SDft <- ftable(RICH10_SDg2$player1, RICH10_SDg2$player2)
RICH10_SDft2 <- as.matrix(RICH10_SDft)
numRows <- nrow(RICH10_SDft2)
numCols <- ncol(RICH10_SDft2)
RICH10_SDft3 <- RICH10_SDft2[c(2:numRows) , c(2:numCols)]
RICH10_SDTable <- graph.adjacency(RICH10_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(RICH10_SDTable, vertex.label = V(RICH10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
RICH10_SD.clusterCoef <- transitivity(RICH10_SDTable, type="global") #cluster coefficient
RICH10_SD.degreeCent <- centralization.degree(RICH10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_SDftn <- as.network.matrix(RICH10_SDft)
RICH10_SD.netDensity <- network.density(RICH10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_SD.entropy <- entropy(RICH10_SDft) #entropy

RICH10_SD.netMx <- cbind(RICH10_SD.netMx, RICH10_SD.clusterCoef, RICH10_SD.degreeCent$centralization,
                         RICH10_SD.netDensity, RICH10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
RICH10_TDg2 <- data.frame(RICH10_TD)
RICH10_TDg2 <- RICH10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_TDg2$player1
player2vector <- RICH10_TDg2$player2
RICH10_TDg3 <- RICH10_TDg2
RICH10_TDg3$p1inp2vec <- is.element(RICH10_TDg3$player1, player2vector)
RICH10_TDg3$p2inp1vec <- is.element(RICH10_TDg3$player2, player1vector)

addPlayer1 <- RICH10_TDg3[ which(RICH10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_TDg3[ which(RICH10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_TDg2 <- rbind(RICH10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
RICH10_TDft <- ftable(RICH10_TDg2$player1, RICH10_TDg2$player2)
RICH10_TDft2 <- as.matrix(RICH10_TDft)
numRows <- nrow(RICH10_TDft2)
numCols <- ncol(RICH10_TDft2)
RICH10_TDft3 <- RICH10_TDft2[c(2:numRows) , c(2:numCols)]
RICH10_TDTable <- graph.adjacency(RICH10_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(RICH10_TDTable, vertex.label = V(RICH10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
RICH10_TD.clusterCoef <- transitivity(RICH10_TDTable, type="global") #cluster coefficient
RICH10_TD.degreeCent <- centralization.degree(RICH10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_TDftn <- as.network.matrix(RICH10_TDft)
RICH10_TD.netDensity <- network.density(RICH10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_TD.entropy <- entropy(RICH10_TDft) #entropy

RICH10_TD.netMx <- cbind(RICH10_TD.netMx, RICH10_TD.clusterCoef, RICH10_TD.degreeCent$centralization,
                         RICH10_TD.netDensity, RICH10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************

round = 10
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
RICH10_QTg2 <- data.frame(RICH10_QT)
RICH10_QTg2 <- RICH10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH10_QTg2$player1
player2vector <- RICH10_QTg2$player2
RICH10_QTg3 <- RICH10_QTg2
RICH10_QTg3$p1inp2vec <- is.element(RICH10_QTg3$player1, player2vector)
RICH10_QTg3$p2inp1vec <- is.element(RICH10_QTg3$player2, player1vector)

addPlayer1 <- RICH10_QTg3[ which(RICH10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH10_QTg3[ which(RICH10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH10_QTg2 <- rbind(RICH10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
RICH10_QTft <- ftable(RICH10_QTg2$player1, RICH10_QTg2$player2)
RICH10_QTft2 <- as.matrix(RICH10_QTft)
numRows <- nrow(RICH10_QTft2)
numCols <- ncol(RICH10_QTft2)
RICH10_QTft3 <- RICH10_QTft2[c(2:numRows) , c(2:numCols)]
RICH10_QTTable <- graph.adjacency(RICH10_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(RICH10_QTTable, vertex.label = V(RICH10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
RICH10_QT.clusterCoef <- transitivity(RICH10_QTTable, type="global") #cluster coefficient
RICH10_QT.degreeCent <- centralization.degree(RICH10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH10_QTftn <- as.network.matrix(RICH10_QTft)
RICH10_QT.netDensity <- network.density(RICH10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH10_QT.entropy <- entropy(RICH10_QTft) #entropy

RICH10_QT.netMx <- cbind(RICH10_QT.netMx, RICH10_QT.clusterCoef, RICH10_QT.degreeCent$centralization,
                         RICH10_QT.netDensity, RICH10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH10_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "Goal_F"
STK10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
STK10_Gg2 <- data.frame(STK10_G)
STK10_Gg2 <- STK10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_Gg2$player1
player2vector <- STK10_Gg2$player2
STK10_Gg3 <- STK10_Gg2
STK10_Gg3$p1inp2vec <- is.element(STK10_Gg3$player1, player2vector)
STK10_Gg3$p2inp1vec <- is.element(STK10_Gg3$player2, player1vector)

addPlayer1 <- STK10_Gg3[ which(STK10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_Gg3[ which(STK10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_Gg2 <- rbind(STK10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
STK10_Gft <- ftable(STK10_Gg2$player1, STK10_Gg2$player2)
STK10_Gft2 <- as.matrix(STK10_Gft)
numRows <- nrow(STK10_Gft2)
numCols <- ncol(STK10_Gft2)
STK10_Gft3 <- STK10_Gft2[c(2:numRows) , c(2:numCols)]
STK10_GTable <- graph.adjacency(STK10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(STK10_GTable, vertex.label = V(STK10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
STK10_G.clusterCoef <- transitivity(STK10_GTable, type="global") #cluster coefficient
STK10_G.degreeCent <- centralization.degree(STK10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_Gftn <- as.network.matrix(STK10_Gft)
STK10_G.netDensity <- network.density(STK10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_G.entropy <- entropy(STK10_Gft) #entropy

STK10_G.netMx <- cbind(STK10_G.netMx, STK10_G.clusterCoef, STK10_G.degreeCent$centralization,
                       STK10_G.netDensity, STK10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "Behind_F"
STK10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
STK10_Bg2 <- data.frame(STK10_B)
STK10_Bg2 <- STK10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_Bg2$player1
player2vector <- STK10_Bg2$player2
STK10_Bg3 <- STK10_Bg2
STK10_Bg3$p1inp2vec <- is.element(STK10_Bg3$player1, player2vector)
STK10_Bg3$p2inp1vec <- is.element(STK10_Bg3$player2, player1vector)

addPlayer1 <- STK10_Bg3[ which(STK10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_Bg3[ which(STK10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_Bg2 <- rbind(STK10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
STK10_Bft <- ftable(STK10_Bg2$player1, STK10_Bg2$player2)
STK10_Bft2 <- as.matrix(STK10_Bft)
numRows <- nrow(STK10_Bft2)
numCols <- ncol(STK10_Bft2)
STK10_Bft3 <- STK10_Bft2[c(2:numRows) , c(2:numCols)]
STK10_BTable <- graph.adjacency(STK10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(STK10_BTable, vertex.label = V(STK10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
STK10_B.clusterCoef <- transitivity(STK10_BTable, type="global") #cluster coefficient
STK10_B.degreeCent <- centralization.degree(STK10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_Bftn <- as.network.matrix(STK10_Bft)
STK10_B.netDensity <- network.density(STK10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_B.entropy <- entropy(STK10_Bft) #entropy

STK10_B.netMx <- cbind(STK10_B.netMx, STK10_B.clusterCoef, STK10_B.degreeCent$centralization,
                       STK10_B.netDensity, STK10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "Stoppage_F"
STK10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
STK10_SFg2 <- data.frame(STK10_SF)
STK10_SFg2 <- STK10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_SFg2$player1
player2vector <- STK10_SFg2$player2
STK10_SFg3 <- STK10_SFg2
STK10_SFg3$p1inp2vec <- is.element(STK10_SFg3$player1, player2vector)
STK10_SFg3$p2inp1vec <- is.element(STK10_SFg3$player2, player1vector)

addPlayer1 <- STK10_SFg3[ which(STK10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_SFg3[ which(STK10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_SFg2 <- rbind(STK10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
STK10_SFft <- ftable(STK10_SFg2$player1, STK10_SFg2$player2)
STK10_SFft2 <- as.matrix(STK10_SFft)
numRows <- nrow(STK10_SFft2)
numCols <- ncol(STK10_SFft2)
STK10_SFft3 <- STK10_SFft2[c(2:numRows) , c(2:numCols)]
STK10_SFTable <- graph.adjacency(STK10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(STK10_SFTable, vertex.label = V(STK10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
STK10_SF.clusterCoef <- transitivity(STK10_SFTable, type="global") #cluster coefficient
STK10_SF.degreeCent <- centralization.degree(STK10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_SFftn <- as.network.matrix(STK10_SFft)
STK10_SF.netDensity <- network.density(STK10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_SF.entropy <- entropy(STK10_SFft) #entropy

STK10_SF.netMx <- cbind(STK10_SF.netMx, STK10_SF.clusterCoef, STK10_SF.degreeCent$centralization,
                        STK10_SF.netDensity, STK10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "Turnover_F"
STK10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
STK10_TFg2 <- data.frame(STK10_TF)
STK10_TFg2 <- STK10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_TFg2$player1
player2vector <- STK10_TFg2$player2
STK10_TFg3 <- STK10_TFg2
STK10_TFg3$p1inp2vec <- is.element(STK10_TFg3$player1, player2vector)
STK10_TFg3$p2inp1vec <- is.element(STK10_TFg3$player2, player1vector)

addPlayer1 <- STK10_TFg3[ which(STK10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_TFg3[ which(STK10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_TFg2 <- rbind(STK10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
STK10_TFft <- ftable(STK10_TFg2$player1, STK10_TFg2$player2)
STK10_TFft2 <- as.matrix(STK10_TFft)
numRows <- nrow(STK10_TFft2)
numCols <- ncol(STK10_TFft2)
STK10_TFft3 <- STK10_TFft2[c(2:numRows) , c(2:numCols)]
STK10_TFTable <- graph.adjacency(STK10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(STK10_TFTable, vertex.label = V(STK10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
STK10_TF.clusterCoef <- transitivity(STK10_TFTable, type="global") #cluster coefficient
STK10_TF.degreeCent <- centralization.degree(STK10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_TFftn <- as.network.matrix(STK10_TFft)
STK10_TF.netDensity <- network.density(STK10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_TF.entropy <- entropy(STK10_TFft) #entropy

STK10_TF.netMx <- cbind(STK10_TF.netMx, STK10_TF.clusterCoef, STK10_TF.degreeCent$centralization,
                        STK10_TF.netDensity, STK10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
STK10_SAMg2 <- data.frame(STK10_SAM)
STK10_SAMg2 <- STK10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_SAMg2$player1
player2vector <- STK10_SAMg2$player2
STK10_SAMg3 <- STK10_SAMg2
STK10_SAMg3$p1inp2vec <- is.element(STK10_SAMg3$player1, player2vector)
STK10_SAMg3$p2inp1vec <- is.element(STK10_SAMg3$player2, player1vector)

addPlayer1 <- STK10_SAMg3[ which(STK10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_SAMg3[ which(STK10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_SAMg2 <- rbind(STK10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
STK10_SAMft <- ftable(STK10_SAMg2$player1, STK10_SAMg2$player2)
STK10_SAMft2 <- as.matrix(STK10_SAMft)
numRows <- nrow(STK10_SAMft2)
numCols <- ncol(STK10_SAMft2)
STK10_SAMft3 <- STK10_SAMft2[c(2:numRows) , c(2:numCols)]
STK10_SAMTable <- graph.adjacency(STK10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(STK10_SAMTable, vertex.label = V(STK10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
STK10_SAM.clusterCoef <- transitivity(STK10_SAMTable, type="global") #cluster coefficient
STK10_SAM.degreeCent <- centralization.degree(STK10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_SAMftn <- as.network.matrix(STK10_SAMft)
STK10_SAM.netDensity <- network.density(STK10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_SAM.entropy <- entropy(STK10_SAMft) #entropy

STK10_SAM.netMx <- cbind(STK10_SAM.netMx, STK10_SAM.clusterCoef, STK10_SAM.degreeCent$centralization,
                         STK10_SAM.netDensity, STK10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "STK"
KIoutcome = "Turnover_AM"
STK10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
STK10_TAMg2 <- data.frame(STK10_TAM)
STK10_TAMg2 <- STK10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_TAMg2$player1
player2vector <- STK10_TAMg2$player2
STK10_TAMg3 <- STK10_TAMg2
STK10_TAMg3$p1inp2vec <- is.element(STK10_TAMg3$player1, player2vector)
STK10_TAMg3$p2inp1vec <- is.element(STK10_TAMg3$player2, player1vector)

addPlayer1 <- STK10_TAMg3[ which(STK10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_TAMg3[ which(STK10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_TAMg2 <- rbind(STK10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
STK10_TAMft <- ftable(STK10_TAMg2$player1, STK10_TAMg2$player2)
STK10_TAMft2 <- as.matrix(STK10_TAMft)
numRows <- nrow(STK10_TAMft2)
numCols <- ncol(STK10_TAMft2)
STK10_TAMft3 <- STK10_TAMft2[c(2:numRows) , c(2:numCols)]
STK10_TAMTable <- graph.adjacency(STK10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(STK10_TAMTable, vertex.label = V(STK10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
STK10_TAM.clusterCoef <- transitivity(STK10_TAMTable, type="global") #cluster coefficient
STK10_TAM.degreeCent <- centralization.degree(STK10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_TAMftn <- as.network.matrix(STK10_TAMft)
STK10_TAM.netDensity <- network.density(STK10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_TAM.entropy <- entropy(STK10_TAMft) #entropy

STK10_TAM.netMx <- cbind(STK10_TAM.netMx, STK10_TAM.clusterCoef, STK10_TAM.degreeCent$centralization,
                         STK10_TAM.netDensity, STK10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
STK10_SDMg2 <- data.frame(STK10_SDM)
STK10_SDMg2 <- STK10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_SDMg2$player1
player2vector <- STK10_SDMg2$player2
STK10_SDMg3 <- STK10_SDMg2
STK10_SDMg3$p1inp2vec <- is.element(STK10_SDMg3$player1, player2vector)
STK10_SDMg3$p2inp1vec <- is.element(STK10_SDMg3$player2, player1vector)

addPlayer1 <- STK10_SDMg3[ which(STK10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_SDMg3[ which(STK10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_SDMg2 <- rbind(STK10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
STK10_SDMft <- ftable(STK10_SDMg2$player1, STK10_SDMg2$player2)
STK10_SDMft2 <- as.matrix(STK10_SDMft)
numRows <- nrow(STK10_SDMft2)
numCols <- ncol(STK10_SDMft2)
STK10_SDMft3 <- STK10_SDMft2[c(2:numRows) , c(2:numCols)]
STK10_SDMTable <- graph.adjacency(STK10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(STK10_SDMTable, vertex.label = V(STK10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
STK10_SDM.clusterCoef <- transitivity(STK10_SDMTable, type="global") #cluster coefficient
STK10_SDM.degreeCent <- centralization.degree(STK10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_SDMftn <- as.network.matrix(STK10_SDMft)
STK10_SDM.netDensity <- network.density(STK10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_SDM.entropy <- entropy(STK10_SDMft) #entropy

STK10_SDM.netMx <- cbind(STK10_SDM.netMx, STK10_SDM.clusterCoef, STK10_SDM.degreeCent$centralization,
                         STK10_SDM.netDensity, STK10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "STK"
KIoutcome = "Turnover_DM"
STK10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
STK10_TDMg2 <- data.frame(STK10_TDM)
STK10_TDMg2 <- STK10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_TDMg2$player1
player2vector <- STK10_TDMg2$player2
STK10_TDMg3 <- STK10_TDMg2
STK10_TDMg3$p1inp2vec <- is.element(STK10_TDMg3$player1, player2vector)
STK10_TDMg3$p2inp1vec <- is.element(STK10_TDMg3$player2, player1vector)

addPlayer1 <- STK10_TDMg3[ which(STK10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_TDMg3[ which(STK10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_TDMg2 <- rbind(STK10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
STK10_TDMft <- ftable(STK10_TDMg2$player1, STK10_TDMg2$player2)
STK10_TDMft2 <- as.matrix(STK10_TDMft)
numRows <- nrow(STK10_TDMft2)
numCols <- ncol(STK10_TDMft2)
STK10_TDMft3 <- STK10_TDMft2[c(2:numRows) , c(2:numCols)]
STK10_TDMTable <- graph.adjacency(STK10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(STK10_TDMTable, vertex.label = V(STK10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
STK10_TDM.clusterCoef <- transitivity(STK10_TDMTable, type="global") #cluster coefficient
STK10_TDM.degreeCent <- centralization.degree(STK10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_TDMftn <- as.network.matrix(STK10_TDMft)
STK10_TDM.netDensity <- network.density(STK10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_TDM.entropy <- entropy(STK10_TDMft) #entropy

STK10_TDM.netMx <- cbind(STK10_TDM.netMx, STK10_TDM.clusterCoef, STK10_TDM.degreeCent$centralization,
                         STK10_TDM.netDensity, STK10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "Stoppage_D"
STK10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
STK10_SDg2 <- data.frame(STK10_SD)
STK10_SDg2 <- STK10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_SDg2$player1
player2vector <- STK10_SDg2$player2
STK10_SDg3 <- STK10_SDg2
STK10_SDg3$p1inp2vec <- is.element(STK10_SDg3$player1, player2vector)
STK10_SDg3$p2inp1vec <- is.element(STK10_SDg3$player2, player1vector)

addPlayer1 <- STK10_SDg3[ which(STK10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_SDg3[ which(STK10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_SDg2 <- rbind(STK10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
STK10_SDft <- ftable(STK10_SDg2$player1, STK10_SDg2$player2)
STK10_SDft2 <- as.matrix(STK10_SDft)
numRows <- nrow(STK10_SDft2)
numCols <- ncol(STK10_SDft2)
STK10_SDft3 <- STK10_SDft2[c(2:numRows) , c(2:numCols)]
STK10_SDTable <- graph.adjacency(STK10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(STK10_SDTable, vertex.label = V(STK10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
STK10_SD.clusterCoef <- transitivity(STK10_SDTable, type="global") #cluster coefficient
STK10_SD.degreeCent <- centralization.degree(STK10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_SDftn <- as.network.matrix(STK10_SDft)
STK10_SD.netDensity <- network.density(STK10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_SD.entropy <- entropy(STK10_SDft) #entropy

STK10_SD.netMx <- cbind(STK10_SD.netMx, STK10_SD.clusterCoef, STK10_SD.degreeCent$centralization,
                        STK10_SD.netDensity, STK10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************

round = 10
teamName = "STK"
KIoutcome = "Turnover_D"
STK10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
STK10_TDg2 <- data.frame(STK10_TD)
STK10_TDg2 <- STK10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_TDg2$player1
player2vector <- STK10_TDg2$player2
STK10_TDg3 <- STK10_TDg2
STK10_TDg3$p1inp2vec <- is.element(STK10_TDg3$player1, player2vector)
STK10_TDg3$p2inp1vec <- is.element(STK10_TDg3$player2, player1vector)

addPlayer1 <- STK10_TDg3[ which(STK10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK10_TDg3[ which(STK10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_TDg2 <- rbind(STK10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
STK10_TDft <- ftable(STK10_TDg2$player1, STK10_TDg2$player2)
STK10_TDft2 <- as.matrix(STK10_TDft)
numRows <- nrow(STK10_TDft2)
numCols <- ncol(STK10_TDft2)
STK10_TDft3 <- STK10_TDft2[c(2:numRows) , c(2:numCols)]
STK10_TDTable <- graph.adjacency(STK10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(STK10_TDTable, vertex.label = V(STK10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
STK10_TD.clusterCoef <- transitivity(STK10_TDTable, type="global") #cluster coefficient
STK10_TD.degreeCent <- centralization.degree(STK10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_TDftn <- as.network.matrix(STK10_TDft)
STK10_TD.netDensity <- network.density(STK10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_TD.entropy <- entropy(STK10_TDft) #entropy

STK10_TD.netMx <- cbind(STK10_TD.netMx, STK10_TD.clusterCoef, STK10_TD.degreeCent$centralization,
                        STK10_TD.netDensity, STK10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
STK10_QTg2 <- data.frame(STK10_QT)
STK10_QTg2 <- STK10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK10_QTg2$player1
player2vector <- STK10_QTg2$player2
STK10_QTg3 <- STK10_QTg2
STK10_QTg3$p1inp2vec <- is.element(STK10_QTg3$player1, player2vector)
STK10_QTg3$p2inp1vec <- is.element(STK10_QTg3$player2, player1vector)

addPlayer1 <- STK10_QTg3[ which(STK10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK10_QTg3[ which(STK10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK10_QTg2 <- rbind(STK10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
STK10_QTft <- ftable(STK10_QTg2$player1, STK10_QTg2$player2)
STK10_QTft2 <- as.matrix(STK10_QTft)
numRows <- nrow(STK10_QTft2)
numCols <- ncol(STK10_QTft2)
STK10_QTft3 <- STK10_QTft2[c(2:numRows) , c(2:numCols)]
STK10_QTTable <- graph.adjacency(STK10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(STK10_QTTable, vertex.label = V(STK10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
STK10_QT.clusterCoef <- transitivity(STK10_QTTable, type="global") #cluster coefficient
STK10_QT.degreeCent <- centralization.degree(STK10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK10_QTftn <- as.network.matrix(STK10_QTft)
STK10_QT.netDensity <- network.density(STK10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK10_QT.entropy <- entropy(STK10_QTft) #entropy

STK10_QT.netMx <- cbind(STK10_QT.netMx, STK10_QT.clusterCoef, STK10_QT.degreeCent$centralization,
                        STK10_QT.netDensity, STK10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK10_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************

round = 10
teamName = "SYD"
KIoutcome = "Goal_F"
SYD10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
SYD10_Gg2 <- data.frame(SYD10_G)
SYD10_Gg2 <- SYD10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_Gg2$player1
player2vector <- SYD10_Gg2$player2
SYD10_Gg3 <- SYD10_Gg2
SYD10_Gg3$p1inp2vec <- is.element(SYD10_Gg3$player1, player2vector)
SYD10_Gg3$p2inp1vec <- is.element(SYD10_Gg3$player2, player1vector)

addPlayer1 <- SYD10_Gg3[ which(SYD10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_Gg3[ which(SYD10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_Gg2 <- rbind(SYD10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
SYD10_Gft <- ftable(SYD10_Gg2$player1, SYD10_Gg2$player2)
SYD10_Gft2 <- as.matrix(SYD10_Gft)
numRows <- nrow(SYD10_Gft2)
numCols <- ncol(SYD10_Gft2)
SYD10_Gft3 <- SYD10_Gft2[c(2:numRows) , c(2:numCols)]
SYD10_GTable <- graph.adjacency(SYD10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(SYD10_GTable, vertex.label = V(SYD10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
SYD10_G.clusterCoef <- transitivity(SYD10_GTable, type="global") #cluster coefficient
SYD10_G.degreeCent <- centralization.degree(SYD10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_Gftn <- as.network.matrix(SYD10_Gft)
SYD10_G.netDensity <- network.density(SYD10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_G.entropy <- entropy(SYD10_Gft) #entropy

SYD10_G.netMx <- cbind(SYD10_G.netMx, SYD10_G.clusterCoef, SYD10_G.degreeCent$centralization,
                       SYD10_G.netDensity, SYD10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "SYD"
KIoutcome = "Behind_F"
SYD10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
SYD10_Bg2 <- data.frame(SYD10_B)
SYD10_Bg2 <- SYD10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_Bg2$player1
player2vector <- SYD10_Bg2$player2
SYD10_Bg3 <- SYD10_Bg2
SYD10_Bg3$p1inp2vec <- is.element(SYD10_Bg3$player1, player2vector)
SYD10_Bg3$p2inp1vec <- is.element(SYD10_Bg3$player2, player1vector)

addPlayer1 <- SYD10_Bg3[ which(SYD10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_Bg3[ which(SYD10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_Bg2 <- rbind(SYD10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
SYD10_Bft <- ftable(SYD10_Bg2$player1, SYD10_Bg2$player2)
SYD10_Bft2 <- as.matrix(SYD10_Bft)
numRows <- nrow(SYD10_Bft2)
numCols <- ncol(SYD10_Bft2)
SYD10_Bft3 <- SYD10_Bft2[c(2:numRows) , c(2:numCols)]
SYD10_BTable <- graph.adjacency(SYD10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(SYD10_BTable, vertex.label = V(SYD10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
SYD10_B.clusterCoef <- transitivity(SYD10_BTable, type="global") #cluster coefficient
SYD10_B.degreeCent <- centralization.degree(SYD10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_Bftn <- as.network.matrix(SYD10_Bft)
SYD10_B.netDensity <- network.density(SYD10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_B.entropy <- entropy(SYD10_Bft) #entropy

SYD10_B.netMx <- cbind(SYD10_B.netMx, SYD10_B.clusterCoef, SYD10_B.degreeCent$centralization,
                       SYD10_B.netDensity, SYD10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************

round = 10
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
SYD10_SFg2 <- data.frame(SYD10_SF)
SYD10_SFg2 <- SYD10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_SFg2$player1
player2vector <- SYD10_SFg2$player2
SYD10_SFg3 <- SYD10_SFg2
SYD10_SFg3$p1inp2vec <- is.element(SYD10_SFg3$player1, player2vector)
SYD10_SFg3$p2inp1vec <- is.element(SYD10_SFg3$player2, player1vector)

addPlayer1 <- SYD10_SFg3[ which(SYD10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD10_SFg3[ which(SYD10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_SFg2 <- rbind(SYD10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
SYD10_SFft <- ftable(SYD10_SFg2$player1, SYD10_SFg2$player2)
SYD10_SFft2 <- as.matrix(SYD10_SFft)
numRows <- nrow(SYD10_SFft2)
numCols <- ncol(SYD10_SFft2)
SYD10_SFft3 <- SYD10_SFft2[c(2:numRows) , c(2:numCols)]
SYD10_SFTable <- graph.adjacency(SYD10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(SYD10_SFTable, vertex.label = V(SYD10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
SYD10_SF.clusterCoef <- transitivity(SYD10_SFTable, type="global") #cluster coefficient
SYD10_SF.degreeCent <- centralization.degree(SYD10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_SFftn <- as.network.matrix(SYD10_SFft)
SYD10_SF.netDensity <- network.density(SYD10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_SF.entropy <- entropy(SYD10_SFft) #entropy

SYD10_SF.netMx <- cbind(SYD10_SF.netMx, SYD10_SF.clusterCoef, SYD10_SF.degreeCent$centralization,
                        SYD10_SF.netDensity, SYD10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
SYD10_TFg2 <- data.frame(SYD10_TF)
SYD10_TFg2 <- SYD10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_TFg2$player1
player2vector <- SYD10_TFg2$player2
SYD10_TFg3 <- SYD10_TFg2
SYD10_TFg3$p1inp2vec <- is.element(SYD10_TFg3$player1, player2vector)
SYD10_TFg3$p2inp1vec <- is.element(SYD10_TFg3$player2, player1vector)

addPlayer1 <- SYD10_TFg3[ which(SYD10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_TFg3[ which(SYD10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_TFg2 <- rbind(SYD10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
SYD10_TFft <- ftable(SYD10_TFg2$player1, SYD10_TFg2$player2)
SYD10_TFft2 <- as.matrix(SYD10_TFft)
numRows <- nrow(SYD10_TFft2)
numCols <- ncol(SYD10_TFft2)
SYD10_TFft3 <- SYD10_TFft2[c(2:numRows) , c(2:numCols)]
SYD10_TFTable <- graph.adjacency(SYD10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(SYD10_TFTable, vertex.label = V(SYD10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
SYD10_TF.clusterCoef <- transitivity(SYD10_TFTable, type="global") #cluster coefficient
SYD10_TF.degreeCent <- centralization.degree(SYD10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_TFftn <- as.network.matrix(SYD10_TFft)
SYD10_TF.netDensity <- network.density(SYD10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_TF.entropy <- entropy(SYD10_TFft) #entropy

SYD10_TF.netMx <- cbind(SYD10_TF.netMx, SYD10_TF.clusterCoef, SYD10_TF.degreeCent$centralization,
                        SYD10_TF.netDensity, SYD10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
SYD10_SAMg2 <- data.frame(SYD10_SAM)
SYD10_SAMg2 <- SYD10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_SAMg2$player1
player2vector <- SYD10_SAMg2$player2
SYD10_SAMg3 <- SYD10_SAMg2
SYD10_SAMg3$p1inp2vec <- is.element(SYD10_SAMg3$player1, player2vector)
SYD10_SAMg3$p2inp1vec <- is.element(SYD10_SAMg3$player2, player1vector)

addPlayer1 <- SYD10_SAMg3[ which(SYD10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_SAMg3[ which(SYD10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_SAMg2 <- rbind(SYD10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
SYD10_SAMft <- ftable(SYD10_SAMg2$player1, SYD10_SAMg2$player2)
SYD10_SAMft2 <- as.matrix(SYD10_SAMft)
numRows <- nrow(SYD10_SAMft2)
numCols <- ncol(SYD10_SAMft2)
SYD10_SAMft3 <- SYD10_SAMft2[c(2:numRows) , c(2:numCols)]
SYD10_SAMTable <- graph.adjacency(SYD10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(SYD10_SAMTable, vertex.label = V(SYD10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
SYD10_SAM.clusterCoef <- transitivity(SYD10_SAMTable, type="global") #cluster coefficient
SYD10_SAM.degreeCent <- centralization.degree(SYD10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_SAMftn <- as.network.matrix(SYD10_SAMft)
SYD10_SAM.netDensity <- network.density(SYD10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_SAM.entropy <- entropy(SYD10_SAMft) #entropy

SYD10_SAM.netMx <- cbind(SYD10_SAM.netMx, SYD10_SAM.clusterCoef, SYD10_SAM.degreeCent$centralization,
                         SYD10_SAM.netDensity, SYD10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
SYD10_TAMg2 <- data.frame(SYD10_TAM)
SYD10_TAMg2 <- SYD10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_TAMg2$player1
player2vector <- SYD10_TAMg2$player2
SYD10_TAMg3 <- SYD10_TAMg2
SYD10_TAMg3$p1inp2vec <- is.element(SYD10_TAMg3$player1, player2vector)
SYD10_TAMg3$p2inp1vec <- is.element(SYD10_TAMg3$player2, player1vector)

addPlayer1 <- SYD10_TAMg3[ which(SYD10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD10_TAMg3[ which(SYD10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_TAMg2 <- rbind(SYD10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
SYD10_TAMft <- ftable(SYD10_TAMg2$player1, SYD10_TAMg2$player2)
SYD10_TAMft2 <- as.matrix(SYD10_TAMft)
numRows <- nrow(SYD10_TAMft2)
numCols <- ncol(SYD10_TAMft2)
SYD10_TAMft3 <- SYD10_TAMft2[c(2:numRows) , c(2:numCols)]
SYD10_TAMTable <- graph.adjacency(SYD10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(SYD10_TAMTable, vertex.label = V(SYD10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
SYD10_TAM.clusterCoef <- transitivity(SYD10_TAMTable, type="global") #cluster coefficient
SYD10_TAM.degreeCent <- centralization.degree(SYD10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_TAMftn <- as.network.matrix(SYD10_TAMft)
SYD10_TAM.netDensity <- network.density(SYD10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_TAM.entropy <- entropy(SYD10_TAMft) #entropy

SYD10_TAM.netMx <- cbind(SYD10_TAM.netMx, SYD10_TAM.clusterCoef, SYD10_TAM.degreeCent$centralization,
                         SYD10_TAM.netDensity, SYD10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************

round = 10
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
SYD10_SDMg2 <- data.frame(SYD10_SDM)
SYD10_SDMg2 <- SYD10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_SDMg2$player1
player2vector <- SYD10_SDMg2$player2
SYD10_SDMg3 <- SYD10_SDMg2
SYD10_SDMg3$p1inp2vec <- is.element(SYD10_SDMg3$player1, player2vector)
SYD10_SDMg3$p2inp1vec <- is.element(SYD10_SDMg3$player2, player1vector)

addPlayer1 <- SYD10_SDMg3[ which(SYD10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_SDMg3[ which(SYD10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_SDMg2 <- rbind(SYD10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
SYD10_SDMft <- ftable(SYD10_SDMg2$player1, SYD10_SDMg2$player2)
SYD10_SDMft2 <- as.matrix(SYD10_SDMft)
numRows <- nrow(SYD10_SDMft2)
numCols <- ncol(SYD10_SDMft2)
SYD10_SDMft3 <- SYD10_SDMft2[c(2:numRows) , c(2:numCols)]
SYD10_SDMTable <- graph.adjacency(SYD10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(SYD10_SDMTable, vertex.label = V(SYD10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
SYD10_SDM.clusterCoef <- transitivity(SYD10_SDMTable, type="global") #cluster coefficient
SYD10_SDM.degreeCent <- centralization.degree(SYD10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_SDMftn <- as.network.matrix(SYD10_SDMft)
SYD10_SDM.netDensity <- network.density(SYD10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_SDM.entropy <- entropy(SYD10_SDMft) #entropy

SYD10_SDM.netMx <- cbind(SYD10_SDM.netMx, SYD10_SDM.clusterCoef, SYD10_SDM.degreeCent$centralization,
                         SYD10_SDM.netDensity, SYD10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
SYD10_TDMg2 <- data.frame(SYD10_TDM)
SYD10_TDMg2 <- SYD10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_TDMg2$player1
player2vector <- SYD10_TDMg2$player2
SYD10_TDMg3 <- SYD10_TDMg2
SYD10_TDMg3$p1inp2vec <- is.element(SYD10_TDMg3$player1, player2vector)
SYD10_TDMg3$p2inp1vec <- is.element(SYD10_TDMg3$player2, player1vector)

addPlayer1 <- SYD10_TDMg3[ which(SYD10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_TDMg3[ which(SYD10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_TDMg2 <- rbind(SYD10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
SYD10_TDMft <- ftable(SYD10_TDMg2$player1, SYD10_TDMg2$player2)
SYD10_TDMft2 <- as.matrix(SYD10_TDMft)
numRows <- nrow(SYD10_TDMft2)
numCols <- ncol(SYD10_TDMft2)
SYD10_TDMft3 <- SYD10_TDMft2[c(2:numRows) , c(2:numCols)]
SYD10_TDMTable <- graph.adjacency(SYD10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(SYD10_TDMTable, vertex.label = V(SYD10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
SYD10_TDM.clusterCoef <- transitivity(SYD10_TDMTable, type="global") #cluster coefficient
SYD10_TDM.degreeCent <- centralization.degree(SYD10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_TDMftn <- as.network.matrix(SYD10_TDMft)
SYD10_TDM.netDensity <- network.density(SYD10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_TDM.entropy <- entropy(SYD10_TDMft) #entropy

SYD10_TDM.netMx <- cbind(SYD10_TDM.netMx, SYD10_TDM.clusterCoef, SYD10_TDM.degreeCent$centralization,
                         SYD10_TDM.netDensity, SYD10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
SYD10_SDg2 <- data.frame(SYD10_SD)
SYD10_SDg2 <- SYD10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_SDg2$player1
player2vector <- SYD10_SDg2$player2
SYD10_SDg3 <- SYD10_SDg2
SYD10_SDg3$p1inp2vec <- is.element(SYD10_SDg3$player1, player2vector)
SYD10_SDg3$p2inp1vec <- is.element(SYD10_SDg3$player2, player1vector)

addPlayer1 <- SYD10_SDg3[ which(SYD10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_SDg3[ which(SYD10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_SDg2 <- rbind(SYD10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
SYD10_SDft <- ftable(SYD10_SDg2$player1, SYD10_SDg2$player2)
SYD10_SDft2 <- as.matrix(SYD10_SDft)
numRows <- nrow(SYD10_SDft2)
numCols <- ncol(SYD10_SDft2)
SYD10_SDft3 <- SYD10_SDft2[c(2:numRows) , c(2:numCols)]
SYD10_SDTable <- graph.adjacency(SYD10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(SYD10_SDTable, vertex.label = V(SYD10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
SYD10_SD.clusterCoef <- transitivity(SYD10_SDTable, type="global") #cluster coefficient
SYD10_SD.degreeCent <- centralization.degree(SYD10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_SDftn <- as.network.matrix(SYD10_SDft)
SYD10_SD.netDensity <- network.density(SYD10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_SD.entropy <- entropy(SYD10_SDft) #entropy

SYD10_SD.netMx <- cbind(SYD10_SD.netMx, SYD10_SD.clusterCoef, SYD10_SD.degreeCent$centralization,
                        SYD10_SD.netDensity, SYD10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************

round = 10
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
SYD10_TDg2 <- data.frame(SYD10_TD)
SYD10_TDg2 <- SYD10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_TDg2$player1
player2vector <- SYD10_TDg2$player2
SYD10_TDg3 <- SYD10_TDg2
SYD10_TDg3$p1inp2vec <- is.element(SYD10_TDg3$player1, player2vector)
SYD10_TDg3$p2inp1vec <- is.element(SYD10_TDg3$player2, player1vector)

addPlayer1 <- SYD10_TDg3[ which(SYD10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_TDg3[ which(SYD10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_TDg2 <- rbind(SYD10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
SYD10_TDft <- ftable(SYD10_TDg2$player1, SYD10_TDg2$player2)
SYD10_TDft2 <- as.matrix(SYD10_TDft)
numRows <- nrow(SYD10_TDft2)
numCols <- ncol(SYD10_TDft2)
SYD10_TDft3 <- SYD10_TDft2[c(2:numRows) , c(2:numCols)]
SYD10_TDTable <- graph.adjacency(SYD10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(SYD10_TDTable, vertex.label = V(SYD10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
SYD10_TD.clusterCoef <- transitivity(SYD10_TDTable, type="global") #cluster coefficient
SYD10_TD.degreeCent <- centralization.degree(SYD10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_TDftn <- as.network.matrix(SYD10_TDft)
SYD10_TD.netDensity <- network.density(SYD10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_TD.entropy <- entropy(SYD10_TDft) #entropy

SYD10_TD.netMx <- cbind(SYD10_TD.netMx, SYD10_TD.clusterCoef, SYD10_TD.degreeCent$centralization,
                        SYD10_TD.netDensity, SYD10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
SYD10_QTg2 <- data.frame(SYD10_QT)
SYD10_QTg2 <- SYD10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD10_QTg2$player1
player2vector <- SYD10_QTg2$player2
SYD10_QTg3 <- SYD10_QTg2
SYD10_QTg3$p1inp2vec <- is.element(SYD10_QTg3$player1, player2vector)
SYD10_QTg3$p2inp1vec <- is.element(SYD10_QTg3$player2, player1vector)

addPlayer1 <- SYD10_QTg3[ which(SYD10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD10_QTg3[ which(SYD10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD10_QTg2 <- rbind(SYD10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
SYD10_QTft <- ftable(SYD10_QTg2$player1, SYD10_QTg2$player2)
SYD10_QTft2 <- as.matrix(SYD10_QTft)
numRows <- nrow(SYD10_QTft2)
numCols <- ncol(SYD10_QTft2)
SYD10_QTft3 <- SYD10_QTft2[c(2:numRows) , c(2:numCols)]
SYD10_QTTable <- graph.adjacency(SYD10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(SYD10_QTTable, vertex.label = V(SYD10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
SYD10_QT.clusterCoef <- transitivity(SYD10_QTTable, type="global") #cluster coefficient
SYD10_QT.degreeCent <- centralization.degree(SYD10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD10_QTftn <- as.network.matrix(SYD10_QTft)
SYD10_QT.netDensity <- network.density(SYD10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD10_QT.entropy <- entropy(SYD10_QTft) #entropy

SYD10_QT.netMx <- cbind(SYD10_QT.netMx, SYD10_QT.clusterCoef, SYD10_QT.degreeCent$centralization,
                        SYD10_QT.netDensity, SYD10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD10_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Goal_F"
WB10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
WB10_Gg2 <- data.frame(WB10_G)
WB10_Gg2 <- WB10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_Gg2$player1
player2vector <- WB10_Gg2$player2
WB10_Gg3 <- WB10_Gg2
WB10_Gg3$p1inp2vec <- is.element(WB10_Gg3$player1, player2vector)
WB10_Gg3$p2inp1vec <- is.element(WB10_Gg3$player2, player1vector)

addPlayer1 <- WB10_Gg3[ which(WB10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_Gg3[ which(WB10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_Gg2 <- rbind(WB10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
WB10_Gft <- ftable(WB10_Gg2$player1, WB10_Gg2$player2)
WB10_Gft2 <- as.matrix(WB10_Gft)
numRows <- nrow(WB10_Gft2)
numCols <- ncol(WB10_Gft2)
WB10_Gft3 <- WB10_Gft2[c(2:numRows) , c(2:numCols)]
WB10_GTable <- graph.adjacency(WB10_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(WB10_GTable, vertex.label = V(WB10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
WB10_G.clusterCoef <- transitivity(WB10_GTable, type="global") #cluster coefficient
WB10_G.degreeCent <- centralization.degree(WB10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_Gftn <- as.network.matrix(WB10_Gft)
WB10_G.netDensity <- network.density(WB10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_G.entropy <- entropy(WB10_Gft) #entropy

WB10_G.netMx <- cbind(WB10_G.netMx, WB10_G.clusterCoef, WB10_G.degreeCent$centralization,
                      WB10_G.netDensity, WB10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Behind_F"
WB10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
WB10_Bg2 <- data.frame(WB10_B)
WB10_Bg2 <- WB10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_Bg2$player1
player2vector <- WB10_Bg2$player2
WB10_Bg3 <- WB10_Bg2
WB10_Bg3$p1inp2vec <- is.element(WB10_Bg3$player1, player2vector)
WB10_Bg3$p2inp1vec <- is.element(WB10_Bg3$player2, player1vector)

addPlayer1 <- WB10_Bg3[ which(WB10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_Bg3[ which(WB10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_Bg2 <- rbind(WB10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
WB10_Bft <- ftable(WB10_Bg2$player1, WB10_Bg2$player2)
WB10_Bft2 <- as.matrix(WB10_Bft)
numRows <- nrow(WB10_Bft2)
numCols <- ncol(WB10_Bft2)
WB10_Bft3 <- WB10_Bft2[c(2:numRows) , c(2:numCols)]
WB10_BTable <- graph.adjacency(WB10_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(WB10_BTable, vertex.label = V(WB10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
WB10_B.clusterCoef <- transitivity(WB10_BTable, type="global") #cluster coefficient
WB10_B.degreeCent <- centralization.degree(WB10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_Bftn <- as.network.matrix(WB10_Bft)
WB10_B.netDensity <- network.density(WB10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_B.entropy <- entropy(WB10_Bft) #entropy

WB10_B.netMx <- cbind(WB10_B.netMx, WB10_B.clusterCoef, WB10_B.degreeCent$centralization,
                      WB10_B.netDensity, WB10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Stoppage_F"
WB10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
WB10_SFg2 <- data.frame(WB10_SF)
WB10_SFg2 <- WB10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_SFg2$player1
player2vector <- WB10_SFg2$player2
WB10_SFg3 <- WB10_SFg2
WB10_SFg3$p1inp2vec <- is.element(WB10_SFg3$player1, player2vector)
WB10_SFg3$p2inp1vec <- is.element(WB10_SFg3$player2, player1vector)

addPlayer1 <- WB10_SFg3[ which(WB10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_SFg3[ which(WB10_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_SFg2 <- rbind(WB10_SFg2, addPlayers)

#ROUND 10, FWD Stoppage graph using weighted edges
WB10_SFft <- ftable(WB10_SFg2$player1, WB10_SFg2$player2)
WB10_SFft2 <- as.matrix(WB10_SFft)
numRows <- nrow(WB10_SFft2)
numCols <- ncol(WB10_SFft2)
WB10_SFft3 <- WB10_SFft2[c(2:numRows) , c(2:numCols)]
WB10_SFTable <- graph.adjacency(WB10_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(WB10_SFTable, vertex.label = V(WB10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
WB10_SF.clusterCoef <- transitivity(WB10_SFTable, type="global") #cluster coefficient
WB10_SF.degreeCent <- centralization.degree(WB10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_SFftn <- as.network.matrix(WB10_SFft)
WB10_SF.netDensity <- network.density(WB10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_SF.entropy <- entropy(WB10_SFft) #entropy

WB10_SF.netMx <- cbind(WB10_SF.netMx, WB10_SF.clusterCoef, WB10_SF.degreeCent$centralization,
                       WB10_SF.netDensity, WB10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Turnover_F"
WB10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
WB10_TFg2 <- data.frame(WB10_TF)
WB10_TFg2 <- WB10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_TFg2$player1
player2vector <- WB10_TFg2$player2
WB10_TFg3 <- WB10_TFg2
WB10_TFg3$p1inp2vec <- is.element(WB10_TFg3$player1, player2vector)
WB10_TFg3$p2inp1vec <- is.element(WB10_TFg3$player2, player1vector)

addPlayer1 <- WB10_TFg3[ which(WB10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_TFg3[ which(WB10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_TFg2 <- rbind(WB10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
WB10_TFft <- ftable(WB10_TFg2$player1, WB10_TFg2$player2)
WB10_TFft2 <- as.matrix(WB10_TFft)
numRows <- nrow(WB10_TFft2)
numCols <- ncol(WB10_TFft2)
WB10_TFft3 <- WB10_TFft2[c(2:numRows) , c(2:numCols)]
WB10_TFTable <- graph.adjacency(WB10_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(WB10_TFTable, vertex.label = V(WB10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
WB10_TF.clusterCoef <- transitivity(WB10_TFTable, type="global") #cluster coefficient
WB10_TF.degreeCent <- centralization.degree(WB10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_TFftn <- as.network.matrix(WB10_TFft)
WB10_TF.netDensity <- network.density(WB10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_TF.entropy <- entropy(WB10_TFft) #entropy

WB10_TF.netMx <- cbind(WB10_TF.netMx, WB10_TF.clusterCoef, WB10_TF.degreeCent$centralization,
                       WB10_TF.netDensity, WB10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
WB10_SAMg2 <- data.frame(WB10_SAM)
WB10_SAMg2 <- WB10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_SAMg2$player1
player2vector <- WB10_SAMg2$player2
WB10_SAMg3 <- WB10_SAMg2
WB10_SAMg3$p1inp2vec <- is.element(WB10_SAMg3$player1, player2vector)
WB10_SAMg3$p2inp1vec <- is.element(WB10_SAMg3$player2, player1vector)

addPlayer1 <- WB10_SAMg3[ which(WB10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_SAMg3[ which(WB10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_SAMg2 <- rbind(WB10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
WB10_SAMft <- ftable(WB10_SAMg2$player1, WB10_SAMg2$player2)
WB10_SAMft2 <- as.matrix(WB10_SAMft)
numRows <- nrow(WB10_SAMft2)
numCols <- ncol(WB10_SAMft2)
WB10_SAMft3 <- WB10_SAMft2[c(2:numRows) , c(2:numCols)]
WB10_SAMTable <- graph.adjacency(WB10_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(WB10_SAMTable, vertex.label = V(WB10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
WB10_SAM.clusterCoef <- transitivity(WB10_SAMTable, type="global") #cluster coefficient
WB10_SAM.degreeCent <- centralization.degree(WB10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_SAMftn <- as.network.matrix(WB10_SAMft)
WB10_SAM.netDensity <- network.density(WB10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_SAM.entropy <- entropy(WB10_SAMft) #entropy

WB10_SAM.netMx <- cbind(WB10_SAM.netMx, WB10_SAM.clusterCoef, WB10_SAM.degreeCent$centralization,
                        WB10_SAM.netDensity, WB10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Turnover_AM"
WB10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
WB10_TAMg2 <- data.frame(WB10_TAM)
WB10_TAMg2 <- WB10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_TAMg2$player1
player2vector <- WB10_TAMg2$player2
WB10_TAMg3 <- WB10_TAMg2
WB10_TAMg3$p1inp2vec <- is.element(WB10_TAMg3$player1, player2vector)
WB10_TAMg3$p2inp1vec <- is.element(WB10_TAMg3$player2, player1vector)

addPlayer1 <- WB10_TAMg3[ which(WB10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_TAMg3[ which(WB10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_TAMg2 <- rbind(WB10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
WB10_TAMft <- ftable(WB10_TAMg2$player1, WB10_TAMg2$player2)
WB10_TAMft2 <- as.matrix(WB10_TAMft)
numRows <- nrow(WB10_TAMft2)
numCols <- ncol(WB10_TAMft2)
WB10_TAMft3 <- WB10_TAMft2[c(2:numRows) , c(2:numCols)]
WB10_TAMTable <- graph.adjacency(WB10_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(WB10_TAMTable, vertex.label = V(WB10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
WB10_TAM.clusterCoef <- transitivity(WB10_TAMTable, type="global") #cluster coefficient
WB10_TAM.degreeCent <- centralization.degree(WB10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_TAMftn <- as.network.matrix(WB10_TAMft)
WB10_TAM.netDensity <- network.density(WB10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_TAM.entropy <- entropy(WB10_TAMft) #entropy

WB10_TAM.netMx <- cbind(WB10_TAM.netMx, WB10_TAM.clusterCoef, WB10_TAM.degreeCent$centralization,
                        WB10_TAM.netDensity, WB10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
WB10_SDMg2 <- data.frame(WB10_SDM)
WB10_SDMg2 <- WB10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_SDMg2$player1
player2vector <- WB10_SDMg2$player2
WB10_SDMg3 <- WB10_SDMg2
WB10_SDMg3$p1inp2vec <- is.element(WB10_SDMg3$player1, player2vector)
WB10_SDMg3$p2inp1vec <- is.element(WB10_SDMg3$player2, player1vector)

addPlayer1 <- WB10_SDMg3[ which(WB10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_SDMg3[ which(WB10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_SDMg2 <- rbind(WB10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
WB10_SDMft <- ftable(WB10_SDMg2$player1, WB10_SDMg2$player2)
WB10_SDMft2 <- as.matrix(WB10_SDMft)
numRows <- nrow(WB10_SDMft2)
numCols <- ncol(WB10_SDMft2)
WB10_SDMft3 <- WB10_SDMft2[c(2:numRows) , c(2:numCols)]
WB10_SDMTable <- graph.adjacency(WB10_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(WB10_SDMTable, vertex.label = V(WB10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
WB10_SDM.clusterCoef <- transitivity(WB10_SDMTable, type="global") #cluster coefficient
WB10_SDM.degreeCent <- centralization.degree(WB10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_SDMftn <- as.network.matrix(WB10_SDMft)
WB10_SDM.netDensity <- network.density(WB10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_SDM.entropy <- entropy(WB10_SDMft) #entropy

WB10_SDM.netMx <- cbind(WB10_SDM.netMx, WB10_SDM.clusterCoef, WB10_SDM.degreeCent$centralization,
                        WB10_SDM.netDensity, WB10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************

round = 10
teamName = "WB"
KIoutcome = "Turnover_DM"
WB10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
WB10_TDMg2 <- data.frame(WB10_TDM)
WB10_TDMg2 <- WB10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_TDMg2$player1
player2vector <- WB10_TDMg2$player2
WB10_TDMg3 <- WB10_TDMg2
WB10_TDMg3$p1inp2vec <- is.element(WB10_TDMg3$player1, player2vector)
WB10_TDMg3$p2inp1vec <- is.element(WB10_TDMg3$player2, player1vector)

addPlayer1 <- WB10_TDMg3[ which(WB10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_TDMg3[ which(WB10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_TDMg2 <- rbind(WB10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
WB10_TDMft <- ftable(WB10_TDMg2$player1, WB10_TDMg2$player2)
WB10_TDMft2 <- as.matrix(WB10_TDMft)
numRows <- nrow(WB10_TDMft2)
numCols <- ncol(WB10_TDMft2)
WB10_TDMft3 <- WB10_TDMft2[c(2:numRows) , c(2:numCols)]
WB10_TDMTable <- graph.adjacency(WB10_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(WB10_TDMTable, vertex.label = V(WB10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
WB10_TDM.clusterCoef <- transitivity(WB10_TDMTable, type="global") #cluster coefficient
WB10_TDM.degreeCent <- centralization.degree(WB10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_TDMftn <- as.network.matrix(WB10_TDMft)
WB10_TDM.netDensity <- network.density(WB10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_TDM.entropy <- entropy(WB10_TDMft) #entropy

WB10_TDM.netMx <- cbind(WB10_TDM.netMx, WB10_TDM.clusterCoef, WB10_TDM.degreeCent$centralization,
                        WB10_TDM.netDensity, WB10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Stoppage_D"
WB10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
WB10_SDg2 <- data.frame(WB10_SD)
WB10_SDg2 <- WB10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_SDg2$player1
player2vector <- WB10_SDg2$player2
WB10_SDg3 <- WB10_SDg2
WB10_SDg3$p1inp2vec <- is.element(WB10_SDg3$player1, player2vector)
WB10_SDg3$p2inp1vec <- is.element(WB10_SDg3$player2, player1vector)

addPlayer1 <- WB10_SDg3[ which(WB10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_SDg3[ which(WB10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_SDg2 <- rbind(WB10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
WB10_SDft <- ftable(WB10_SDg2$player1, WB10_SDg2$player2)
WB10_SDft2 <- as.matrix(WB10_SDft)
numRows <- nrow(WB10_SDft2)
numCols <- ncol(WB10_SDft2)
WB10_SDft3 <- WB10_SDft2[c(2:numRows) , c(2:numCols)]
WB10_SDTable <- graph.adjacency(WB10_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(WB10_SDTable, vertex.label = V(WB10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
WB10_SD.clusterCoef <- transitivity(WB10_SDTable, type="global") #cluster coefficient
WB10_SD.degreeCent <- centralization.degree(WB10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_SDftn <- as.network.matrix(WB10_SDft)
WB10_SD.netDensity <- network.density(WB10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_SD.entropy <- entropy(WB10_SDft) #entropy

WB10_SD.netMx <- cbind(WB10_SD.netMx, WB10_SD.clusterCoef, WB10_SD.degreeCent$centralization,
                       WB10_SD.netDensity, WB10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "Turnover_D"
WB10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
WB10_TDg2 <- data.frame(WB10_TD)
WB10_TDg2 <- WB10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_TDg2$player1
player2vector <- WB10_TDg2$player2
WB10_TDg3 <- WB10_TDg2
WB10_TDg3$p1inp2vec <- is.element(WB10_TDg3$player1, player2vector)
WB10_TDg3$p2inp1vec <- is.element(WB10_TDg3$player2, player1vector)

addPlayer1 <- WB10_TDg3[ which(WB10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_TDg3[ which(WB10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_TDg2 <- rbind(WB10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
WB10_TDft <- ftable(WB10_TDg2$player1, WB10_TDg2$player2)
WB10_TDft2 <- as.matrix(WB10_TDft)
numRows <- nrow(WB10_TDft2)
numCols <- ncol(WB10_TDft2)
WB10_TDft3 <- WB10_TDft2[c(2:numRows) , c(2:numCols)]
WB10_TDTable <- graph.adjacency(WB10_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(WB10_TDTable, vertex.label = V(WB10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
WB10_TD.clusterCoef <- transitivity(WB10_TDTable, type="global") #cluster coefficient
WB10_TD.degreeCent <- centralization.degree(WB10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_TDftn <- as.network.matrix(WB10_TDft)
WB10_TD.netDensity <- network.density(WB10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_TD.entropy <- entropy(WB10_TDft) #entropy

WB10_TD.netMx <- cbind(WB10_TD.netMx, WB10_TD.clusterCoef, WB10_TD.degreeCent$centralization,
                       WB10_TD.netDensity, WB10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
WB10_QTg2 <- data.frame(WB10_QT)
WB10_QTg2 <- WB10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB10_QTg2$player1
player2vector <- WB10_QTg2$player2
WB10_QTg3 <- WB10_QTg2
WB10_QTg3$p1inp2vec <- is.element(WB10_QTg3$player1, player2vector)
WB10_QTg3$p2inp1vec <- is.element(WB10_QTg3$player2, player1vector)

addPlayer1 <- WB10_QTg3[ which(WB10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB10_QTg3[ which(WB10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB10_QTg2 <- rbind(WB10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
WB10_QTft <- ftable(WB10_QTg2$player1, WB10_QTg2$player2)
WB10_QTft2 <- as.matrix(WB10_QTft)
numRows <- nrow(WB10_QTft2)
numCols <- ncol(WB10_QTft2)
WB10_QTft3 <- WB10_QTft2[c(2:numRows) , c(2:numCols)]
WB10_QTTable <- graph.adjacency(WB10_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(WB10_QTTable, vertex.label = V(WB10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
WB10_QT.clusterCoef <- transitivity(WB10_QTTable, type="global") #cluster coefficient
WB10_QT.degreeCent <- centralization.degree(WB10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB10_QTftn <- as.network.matrix(WB10_QTft)
WB10_QT.netDensity <- network.density(WB10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB10_QT.entropy <- entropy(WB10_QTft) #entropy

WB10_QT.netMx <- cbind(WB10_QT.netMx, WB10_QT.clusterCoef, WB10_QT.degreeCent$centralization,
                       WB10_QT.netDensity, WB10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB10_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 10
##

#ROUND 10, Goal***************************************************************

round = 10
teamName = "WCE"
KIoutcome = "Goal_F"
WCE10_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Goal with weighted edges
WCE10_Gg2 <- data.frame(WCE10_G)
WCE10_Gg2 <- WCE10_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_Gg2$player1
player2vector <- WCE10_Gg2$player2
WCE10_Gg3 <- WCE10_Gg2
WCE10_Gg3$p1inp2vec <- is.element(WCE10_Gg3$player1, player2vector)
WCE10_Gg3$p2inp1vec <- is.element(WCE10_Gg3$player2, player1vector)

addPlayer1 <- WCE10_Gg3[ which(WCE10_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_Gg3[ which(WCE10_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_Gg2 <- rbind(WCE10_Gg2, addPlayers)

#ROUND 10, Goal graph using weighted edges
WCE10_Gft <- ftable(WCE10_Gg2$player1, WCE10_Gg2$player2)
WCE10_Gft2 <- as.matrix(WCE10_Gft)
numRows <- nrow(WCE10_Gft2)
numCols <- ncol(WCE10_Gft2)
WCE10_Gft3 <- WCE10_Gft2[c(2:numRows) , c(2:numCols)]
WCE10_GTable <- graph.adjacency(WCE10_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Goal graph=weighted
plot.igraph(WCE10_GTable, vertex.label = V(WCE10_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Goal calulation of network metrics
#igraph
WCE10_G.clusterCoef <- transitivity(WCE10_GTable, type="global") #cluster coefficient
WCE10_G.degreeCent <- centralization.degree(WCE10_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_Gftn <- as.network.matrix(WCE10_Gft)
WCE10_G.netDensity <- network.density(WCE10_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_G.entropy <- entropy(WCE10_Gft) #entropy

WCE10_G.netMx <- cbind(WCE10_G.netMx, WCE10_G.clusterCoef, WCE10_G.degreeCent$centralization,
                       WCE10_G.netDensity, WCE10_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_G.netMx) <- varnames

#ROUND 10, Behind***************************************************************
#NA

round = 10
teamName = "WCE"
KIoutcome = "Behind_F"
WCE10_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, Behind with weighted edges
WCE10_Bg2 <- data.frame(WCE10_B)
WCE10_Bg2 <- WCE10_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_Bg2$player1
player2vector <- WCE10_Bg2$player2
WCE10_Bg3 <- WCE10_Bg2
WCE10_Bg3$p1inp2vec <- is.element(WCE10_Bg3$player1, player2vector)
WCE10_Bg3$p2inp1vec <- is.element(WCE10_Bg3$player2, player1vector)

addPlayer1 <- WCE10_Bg3[ which(WCE10_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_Bg3[ which(WCE10_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_Bg2 <- rbind(WCE10_Bg2, addPlayers)

#ROUND 10, Behind graph using weighted edges
WCE10_Bft <- ftable(WCE10_Bg2$player1, WCE10_Bg2$player2)
WCE10_Bft2 <- as.matrix(WCE10_Bft)
numRows <- nrow(WCE10_Bft2)
numCols <- ncol(WCE10_Bft2)
WCE10_Bft3 <- WCE10_Bft2[c(2:numRows) , c(2:numCols)]
WCE10_BTable <- graph.adjacency(WCE10_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 10, Behind graph=weighted
plot.igraph(WCE10_BTable, vertex.label = V(WCE10_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, Behind calulation of network metrics
#igraph
WCE10_B.clusterCoef <- transitivity(WCE10_BTable, type="global") #cluster coefficient
WCE10_B.degreeCent <- centralization.degree(WCE10_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_Bftn <- as.network.matrix(WCE10_Bft)
WCE10_B.netDensity <- network.density(WCE10_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_B.entropy <- entropy(WCE10_Bft) #entropy

WCE10_B.netMx <- cbind(WCE10_B.netMx, WCE10_B.clusterCoef, WCE10_B.degreeCent$centralization,
                       WCE10_B.netDensity, WCE10_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_B.netMx) <- varnames

#ROUND 10, FWD Stoppage**********************************************************
#NA

round = 10
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE10_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Stoppage with weighted edges
WCE10_SFg2 <- data.frame(WCE10_SF)
WCE10_SFg2 <- WCE10_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_SFg2$player1
player2vector <- WCE10_SFg2$player2
WCE10_SFg3 <- WCE10_SFg2
WCE10_SFg3$p1inp2vec <- is.element(WCE10_SFg3$player1, player2vector)
WCE10_SFg3$p2inp1vec <- is.element(WCE10_SFg3$player2, player1vector)

addPlayer1 <- WCE10_SFg3[ which(WCE10_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE10_SFg2 <- rbind(WCE10_SFg2, addPlayer1)

#ROUND 10, FWD Stoppage graph using weighted edges
WCE10_SFft <- ftable(WCE10_SFg2$player1, WCE10_SFg2$player2)
WCE10_SFft2 <- as.matrix(WCE10_SFft)
numRows <- nrow(WCE10_SFft2)
numCols <- ncol(WCE10_SFft2)
WCE10_SFft3 <- WCE10_SFft2[c(2:numRows) , c(1:numCols)]
WCE10_SFTable <- graph.adjacency(WCE10_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Stoppage graph=weighted
plot.igraph(WCE10_SFTable, vertex.label = V(WCE10_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Stoppage calulation of network metrics
#igraph
WCE10_SF.clusterCoef <- transitivity(WCE10_SFTable, type="global") #cluster coefficient
WCE10_SF.degreeCent <- centralization.degree(WCE10_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_SFftn <- as.network.matrix(WCE10_SFft)
WCE10_SF.netDensity <- network.density(WCE10_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_SF.entropy <- entropy(WCE10_SFft) #entropy

WCE10_SF.netMx <- cbind(WCE10_SF.netMx, WCE10_SF.clusterCoef, WCE10_SF.degreeCent$centralization,
                        WCE10_SF.netDensity, WCE10_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_SF.netMx) <- varnames

#ROUND 10, FWD Turnover**********************************************************

round = 10
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE10_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, FWD Turnover with weighted edges
WCE10_TFg2 <- data.frame(WCE10_TF)
WCE10_TFg2 <- WCE10_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_TFg2$player1
player2vector <- WCE10_TFg2$player2
WCE10_TFg3 <- WCE10_TFg2
WCE10_TFg3$p1inp2vec <- is.element(WCE10_TFg3$player1, player2vector)
WCE10_TFg3$p2inp1vec <- is.element(WCE10_TFg3$player2, player1vector)

addPlayer1 <- WCE10_TFg3[ which(WCE10_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_TFg3[ which(WCE10_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_TFg2 <- rbind(WCE10_TFg2, addPlayers)

#ROUND 10, FWD Turnover graph using weighted edges
WCE10_TFft <- ftable(WCE10_TFg2$player1, WCE10_TFg2$player2)
WCE10_TFft2 <- as.matrix(WCE10_TFft)
numRows <- nrow(WCE10_TFft2)
numCols <- ncol(WCE10_TFft2)
WCE10_TFft3 <- WCE10_TFft2[c(2:numRows) , c(2:numCols)]
WCE10_TFTable <- graph.adjacency(WCE10_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, FWD Turnover graph=weighted
plot.igraph(WCE10_TFTable, vertex.label = V(WCE10_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, FWD Turnover calulation of network metrics
#igraph
WCE10_TF.clusterCoef <- transitivity(WCE10_TFTable, type="global") #cluster coefficient
WCE10_TF.degreeCent <- centralization.degree(WCE10_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_TFftn <- as.network.matrix(WCE10_TFft)
WCE10_TF.netDensity <- network.density(WCE10_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_TF.entropy <- entropy(WCE10_TFft) #entropy

WCE10_TF.netMx <- cbind(WCE10_TF.netMx, WCE10_TF.clusterCoef, WCE10_TF.degreeCent$centralization,
                        WCE10_TF.netDensity, WCE10_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_TF.netMx) <- varnames

#ROUND 10, AM Stoppage**********************************************************

round = 10
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE10_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Stoppage with weighted edges
WCE10_SAMg2 <- data.frame(WCE10_SAM)
WCE10_SAMg2 <- WCE10_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_SAMg2$player1
player2vector <- WCE10_SAMg2$player2
WCE10_SAMg3 <- WCE10_SAMg2
WCE10_SAMg3$p1inp2vec <- is.element(WCE10_SAMg3$player1, player2vector)
WCE10_SAMg3$p2inp1vec <- is.element(WCE10_SAMg3$player2, player1vector)

addPlayer1 <- WCE10_SAMg3[ which(WCE10_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_SAMg3[ which(WCE10_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_SAMg2 <- rbind(WCE10_SAMg2, addPlayers)

#ROUND 10, AM Stoppage graph using weighted edges
WCE10_SAMft <- ftable(WCE10_SAMg2$player1, WCE10_SAMg2$player2)
WCE10_SAMft2 <- as.matrix(WCE10_SAMft)
numRows <- nrow(WCE10_SAMft2)
numCols <- ncol(WCE10_SAMft2)
WCE10_SAMft3 <- WCE10_SAMft2[c(2:numRows) , c(2:numCols)]
WCE10_SAMTable <- graph.adjacency(WCE10_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Stoppage graph=weighted
plot.igraph(WCE10_SAMTable, vertex.label = V(WCE10_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Stoppage calulation of network metrics
#igraph
WCE10_SAM.clusterCoef <- transitivity(WCE10_SAMTable, type="global") #cluster coefficient
WCE10_SAM.degreeCent <- centralization.degree(WCE10_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_SAMftn <- as.network.matrix(WCE10_SAMft)
WCE10_SAM.netDensity <- network.density(WCE10_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_SAM.entropy <- entropy(WCE10_SAMft) #entropy

WCE10_SAM.netMx <- cbind(WCE10_SAM.netMx, WCE10_SAM.clusterCoef, WCE10_SAM.degreeCent$centralization,
                         WCE10_SAM.netDensity, WCE10_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_SAM.netMx) <- varnames

#ROUND 10, AM Turnover**********************************************************

round = 10
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE10_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, AM Turnover with weighted edges
WCE10_TAMg2 <- data.frame(WCE10_TAM)
WCE10_TAMg2 <- WCE10_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_TAMg2$player1
player2vector <- WCE10_TAMg2$player2
WCE10_TAMg3 <- WCE10_TAMg2
WCE10_TAMg3$p1inp2vec <- is.element(WCE10_TAMg3$player1, player2vector)
WCE10_TAMg3$p2inp1vec <- is.element(WCE10_TAMg3$player2, player1vector)

addPlayer1 <- WCE10_TAMg3[ which(WCE10_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_TAMg3[ which(WCE10_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_TAMg2 <- rbind(WCE10_TAMg2, addPlayers)

#ROUND 10, AM Turnover graph using weighted edges
WCE10_TAMft <- ftable(WCE10_TAMg2$player1, WCE10_TAMg2$player2)
WCE10_TAMft2 <- as.matrix(WCE10_TAMft)
numRows <- nrow(WCE10_TAMft2)
numCols <- ncol(WCE10_TAMft2)
WCE10_TAMft3 <- WCE10_TAMft2[c(2:numRows) , c(2:numCols)]
WCE10_TAMTable <- graph.adjacency(WCE10_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, AM Turnover graph=weighted
plot.igraph(WCE10_TAMTable, vertex.label = V(WCE10_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, AM Turnover calulation of network metrics
#igraph
WCE10_TAM.clusterCoef <- transitivity(WCE10_TAMTable, type="global") #cluster coefficient
WCE10_TAM.degreeCent <- centralization.degree(WCE10_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_TAMftn <- as.network.matrix(WCE10_TAMft)
WCE10_TAM.netDensity <- network.density(WCE10_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_TAM.entropy <- entropy(WCE10_TAMft) #entropy

WCE10_TAM.netMx <- cbind(WCE10_TAM.netMx, WCE10_TAM.clusterCoef, WCE10_TAM.degreeCent$centralization,
                         WCE10_TAM.netDensity, WCE10_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_TAM.netMx) <- varnames

#ROUND 10, DM Stoppage**********************************************************
#NA
round = 10
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE10_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Stoppage with weighted edges
WCE10_SDMg2 <- data.frame(WCE10_SDM)
WCE10_SDMg2 <- WCE10_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_SDMg2$player1
player2vector <- WCE10_SDMg2$player2
WCE10_SDMg3 <- WCE10_SDMg2
WCE10_SDMg3$p1inp2vec <- is.element(WCE10_SDMg3$player1, player2vector)
WCE10_SDMg3$p2inp1vec <- is.element(WCE10_SDMg3$player2, player1vector)

addPlayer1 <- WCE10_SDMg3[ which(WCE10_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_SDMg3[ which(WCE10_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_SDMg2 <- rbind(WCE10_SDMg2, addPlayers)

#ROUND 10, DM Stoppage graph using weighted edges
WCE10_SDMft <- ftable(WCE10_SDMg2$player1, WCE10_SDMg2$player2)
WCE10_SDMft2 <- as.matrix(WCE10_SDMft)
numRows <- nrow(WCE10_SDMft2)
numCols <- ncol(WCE10_SDMft2)
WCE10_SDMft3 <- WCE10_SDMft2[c(2:numRows) , c(2:numCols)]
WCE10_SDMTable <- graph.adjacency(WCE10_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Stoppage graph=weighted
plot.igraph(WCE10_SDMTable, vertex.label = V(WCE10_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Stoppage calulation of network metrics
#igraph
WCE10_SDM.clusterCoef <- transitivity(WCE10_SDMTable, type="global") #cluster coefficient
WCE10_SDM.degreeCent <- centralization.degree(WCE10_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_SDMftn <- as.network.matrix(WCE10_SDMft)
WCE10_SDM.netDensity <- network.density(WCE10_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_SDM.entropy <- entropy(WCE10_SDMft) #entropy

WCE10_SDM.netMx <- cbind(WCE10_SDM.netMx, WCE10_SDM.clusterCoef, WCE10_SDM.degreeCent$centralization,
                         WCE10_SDM.netDensity, WCE10_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_SDM.netMx) <- varnames

#ROUND 10, DM Turnover**********************************************************
#NA

round = 10
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE10_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, DM Turnover with weighted edges
WCE10_TDMg2 <- data.frame(WCE10_TDM)
WCE10_TDMg2 <- WCE10_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_TDMg2$player1
player2vector <- WCE10_TDMg2$player2
WCE10_TDMg3 <- WCE10_TDMg2
WCE10_TDMg3$p1inp2vec <- is.element(WCE10_TDMg3$player1, player2vector)
WCE10_TDMg3$p2inp1vec <- is.element(WCE10_TDMg3$player2, player1vector)

addPlayer1 <- WCE10_TDMg3[ which(WCE10_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_TDMg3[ which(WCE10_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_TDMg2 <- rbind(WCE10_TDMg2, addPlayers)

#ROUND 10, DM Turnover graph using weighted edges
WCE10_TDMft <- ftable(WCE10_TDMg2$player1, WCE10_TDMg2$player2)
WCE10_TDMft2 <- as.matrix(WCE10_TDMft)
numRows <- nrow(WCE10_TDMft2)
numCols <- ncol(WCE10_TDMft2)
WCE10_TDMft3 <- WCE10_TDMft2[c(2:numRows) , c(2:numCols)]
WCE10_TDMTable <- graph.adjacency(WCE10_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 10, DM Turnover graph=weighted
plot.igraph(WCE10_TDMTable, vertex.label = V(WCE10_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, DM Turnover calulation of network metrics
#igraph
WCE10_TDM.clusterCoef <- transitivity(WCE10_TDMTable, type="global") #cluster coefficient
WCE10_TDM.degreeCent <- centralization.degree(WCE10_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_TDMftn <- as.network.matrix(WCE10_TDMft)
WCE10_TDM.netDensity <- network.density(WCE10_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_TDM.entropy <- entropy(WCE10_TDMft) #entropy

WCE10_TDM.netMx <- cbind(WCE10_TDM.netMx, WCE10_TDM.clusterCoef, WCE10_TDM.degreeCent$centralization,
                         WCE10_TDM.netDensity, WCE10_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_TDM.netMx) <- varnames

#ROUND 10, D Stoppage**********************************************************
#NA

round = 10
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE10_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Stoppage with weighted edges
WCE10_SDg2 <- data.frame(WCE10_SD)
WCE10_SDg2 <- WCE10_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_SDg2$player1
player2vector <- WCE10_SDg2$player2
WCE10_SDg3 <- WCE10_SDg2
WCE10_SDg3$p1inp2vec <- is.element(WCE10_SDg3$player1, player2vector)
WCE10_SDg3$p2inp1vec <- is.element(WCE10_SDg3$player2, player1vector)

addPlayer1 <- WCE10_SDg3[ which(WCE10_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_SDg3[ which(WCE10_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_SDg2 <- rbind(WCE10_SDg2, addPlayers)

#ROUND 10, D Stoppage graph using weighted edges
WCE10_SDft <- ftable(WCE10_SDg2$player1, WCE10_SDg2$player2)
WCE10_SDft2 <- as.matrix(WCE10_SDft)
numRows <- nrow(WCE10_SDft2)
numCols <- ncol(WCE10_SDft2)
WCE10_SDft3 <- WCE10_SDft2[c(2:numRows) , c(2:numCols)]
WCE10_SDTable <- graph.adjacency(WCE10_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Stoppage graph=weighted
plot.igraph(WCE10_SDTable, vertex.label = V(WCE10_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Stoppage calulation of network metrics
#igraph
WCE10_SD.clusterCoef <- transitivity(WCE10_SDTable, type="global") #cluster coefficient
WCE10_SD.degreeCent <- centralization.degree(WCE10_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_SDftn <- as.network.matrix(WCE10_SDft)
WCE10_SD.netDensity <- network.density(WCE10_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_SD.entropy <- entropy(WCE10_SDft) #entropy

WCE10_SD.netMx <- cbind(WCE10_SD.netMx, WCE10_SD.clusterCoef, WCE10_SD.degreeCent$centralization,
                        WCE10_SD.netDensity, WCE10_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_SD.netMx) <- varnames

#ROUND 10, D Turnover**********************************************************
#NA

round = 10
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE10_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, D Turnover with weighted edges
WCE10_TDg2 <- data.frame(WCE10_TD)
WCE10_TDg2 <- WCE10_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_TDg2$player1
player2vector <- WCE10_TDg2$player2
WCE10_TDg3 <- WCE10_TDg2
WCE10_TDg3$p1inp2vec <- is.element(WCE10_TDg3$player1, player2vector)
WCE10_TDg3$p2inp1vec <- is.element(WCE10_TDg3$player2, player1vector)

addPlayer1 <- WCE10_TDg3[ which(WCE10_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_TDg3[ which(WCE10_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_TDg2 <- rbind(WCE10_TDg2, addPlayers)

#ROUND 10, D Turnover graph using weighted edges
WCE10_TDft <- ftable(WCE10_TDg2$player1, WCE10_TDg2$player2)
WCE10_TDft2 <- as.matrix(WCE10_TDft)
numRows <- nrow(WCE10_TDft2)
numCols <- ncol(WCE10_TDft2)
WCE10_TDft3 <- WCE10_TDft2[c(2:numRows) , c(2:numCols)]
WCE10_TDTable <- graph.adjacency(WCE10_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, D Turnover graph=weighted
plot.igraph(WCE10_TDTable, vertex.label = V(WCE10_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, D Turnover calulation of network metrics
#igraph
WCE10_TD.clusterCoef <- transitivity(WCE10_TDTable, type="global") #cluster coefficient
WCE10_TD.degreeCent <- centralization.degree(WCE10_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_TDftn <- as.network.matrix(WCE10_TDft)
WCE10_TD.netDensity <- network.density(WCE10_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_TD.entropy <- entropy(WCE10_TDft) #entropy

WCE10_TD.netMx <- cbind(WCE10_TD.netMx, WCE10_TD.clusterCoef, WCE10_TD.degreeCent$centralization,
                        WCE10_TD.netDensity, WCE10_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_TD.netMx) <- varnames

#ROUND 10, End of Qtr**********************************************************
#NA

round = 10
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE10_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 10, End of Qtr with weighted edges
WCE10_QTg2 <- data.frame(WCE10_QT)
WCE10_QTg2 <- WCE10_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE10_QTg2$player1
player2vector <- WCE10_QTg2$player2
WCE10_QTg3 <- WCE10_QTg2
WCE10_QTg3$p1inp2vec <- is.element(WCE10_QTg3$player1, player2vector)
WCE10_QTg3$p2inp1vec <- is.element(WCE10_QTg3$player2, player1vector)

addPlayer1 <- WCE10_QTg3[ which(WCE10_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE10_QTg3[ which(WCE10_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE10_QTg2 <- rbind(WCE10_QTg2, addPlayers)

#ROUND 10, End of Qtr graph using weighted edges
WCE10_QTft <- ftable(WCE10_QTg2$player1, WCE10_QTg2$player2)
WCE10_QTft2 <- as.matrix(WCE10_QTft)
numRows <- nrow(WCE10_QTft2)
numCols <- ncol(WCE10_QTft2)
WCE10_QTft3 <- WCE10_QTft2[c(2:numRows) , c(2:numCols)]
WCE10_QTTable <- graph.adjacency(WCE10_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 10, End of Qtr graph=weighted
plot.igraph(WCE10_QTTable, vertex.label = V(WCE10_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE10_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 10, End of Qtr calulation of network metrics
#igraph
WCE10_QT.clusterCoef <- transitivity(WCE10_QTTable, type="global") #cluster coefficient
WCE10_QT.degreeCent <- centralization.degree(WCE10_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE10_QTftn <- as.network.matrix(WCE10_QTft)
WCE10_QT.netDensity <- network.density(WCE10_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE10_QT.entropy <- entropy(WCE10_QTft) #entropy

WCE10_QT.netMx <- cbind(WCE10_QT.netMx, WCE10_QT.clusterCoef, WCE10_QT.degreeCent$centralization,
                        WCE10_QT.netDensity, WCE10_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE10_QT.netMx) <- varnames
