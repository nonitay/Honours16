#####
#09-28-16- Real data 11
#Network Analysis
####

library(igraph)
library(network)
library(entropy)


#############################################################################
#COLLINGWOOD

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "Goal_F"
COLL11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
COLL11_Gg2 <- data.frame(COLL11_G)
COLL11_Gg2 <- COLL11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_Gg2$player1
player2vector <- COLL11_Gg2$player2
COLL11_Gg3 <- COLL11_Gg2
COLL11_Gg3$p1inp2vec <- is.element(COLL11_Gg3$player1, player2vector)
COLL11_Gg3$p2inp1vec <- is.element(COLL11_Gg3$player2, player1vector)

addPlayer1 <- COLL11_Gg3[ which(COLL11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_Gg3[ which(COLL11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_Gg2 <- rbind(COLL11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
COLL11_Gft <- ftable(COLL11_Gg2$player1, COLL11_Gg2$player2)
COLL11_Gft2 <- as.matrix(COLL11_Gft)
numRows <- nrow(COLL11_Gft2)
numCols <- ncol(COLL11_Gft2)
COLL11_Gft3 <- COLL11_Gft2[c(2:numRows) , c(2:numCols)]
COLL11_GTable <- graph.adjacency(COLL11_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(COLL11_GTable, vertex.label = V(COLL11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
COLL11_G.clusterCoef <- transitivity(COLL11_GTable, type="global") #cluster coefficient
COLL11_G.degreeCent <- centralization.degree(COLL11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_Gftn <- as.network.matrix(COLL11_Gft)
COLL11_G.netDensity <- network.density(COLL11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_G.entropy <- entropy(COLL11_Gft) #entropy

COLL11_G.netMx <- cbind(COLL11_G.netMx, COLL11_G.clusterCoef, COLL11_G.degreeCent$centralization,
                        COLL11_G.netDensity, COLL11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "Behind_F"
COLL11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
COLL11_Bg2 <- data.frame(COLL11_B)
COLL11_Bg2 <- COLL11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_Bg2$player1
player2vector <- COLL11_Bg2$player2
COLL11_Bg3 <- COLL11_Bg2
COLL11_Bg3$p1inp2vec <- is.element(COLL11_Bg3$player1, player2vector)
COLL11_Bg3$p2inp1vec <- is.element(COLL11_Bg3$player2, player1vector)

addPlayer1 <- COLL11_Bg3[ which(COLL11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_Bg3[ which(COLL11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_Bg2 <- rbind(COLL11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
COLL11_Bft <- ftable(COLL11_Bg2$player1, COLL11_Bg2$player2)
COLL11_Bft2 <- as.matrix(COLL11_Bft)
numRows <- nrow(COLL11_Bft2)
numCols <- ncol(COLL11_Bft2)
COLL11_Bft3 <- COLL11_Bft2[c(2:numRows) , c(2:numCols)]
COLL11_BTable <- graph.adjacency(COLL11_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(COLL11_BTable, vertex.label = V(COLL11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
COLL11_B.clusterCoef <- transitivity(COLL11_BTable, type="global") #cluster coefficient
COLL11_B.degreeCent <- centralization.degree(COLL11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_Bftn <- as.network.matrix(COLL11_Bft)
COLL11_B.netDensity <- network.density(COLL11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_B.entropy <- entropy(COLL11_Bft) #entropy

COLL11_B.netMx <- cbind(COLL11_B.netMx, COLL11_B.clusterCoef, COLL11_B.degreeCent$centralization,
                        COLL11_B.netDensity, COLL11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
COLL11_SFg2 <- data.frame(COLL11_SF)
COLL11_SFg2 <- COLL11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_SFg2$player1
player2vector <- COLL11_SFg2$player2
COLL11_SFg3 <- COLL11_SFg2
COLL11_SFg3$p1inp2vec <- is.element(COLL11_SFg3$player1, player2vector)
COLL11_SFg3$p2inp1vec <- is.element(COLL11_SFg3$player2, player1vector)

addPlayer1 <- COLL11_SFg3[ which(COLL11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_SFg3[ which(COLL11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_SFg2 <- rbind(COLL11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
COLL11_SFft <- ftable(COLL11_SFg2$player1, COLL11_SFg2$player2)
COLL11_SFft2 <- as.matrix(COLL11_SFft)
numRows <- nrow(COLL11_SFft2)
numCols <- ncol(COLL11_SFft2)
COLL11_SFft3 <- COLL11_SFft2[c(2:numRows) , c(2:numCols)]
COLL11_SFTable <- graph.adjacency(COLL11_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(COLL11_SFTable, vertex.label = V(COLL11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
COLL11_SF.clusterCoef <- transitivity(COLL11_SFTable, type="global") #cluster coefficient
COLL11_SF.degreeCent <- centralization.degree(COLL11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_SFftn <- as.network.matrix(COLL11_SFft)
COLL11_SF.netDensity <- network.density(COLL11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_SF.entropy <- entropy(COLL11_SFft) #entropy

COLL11_SF.netMx <- cbind(COLL11_SF.netMx, COLL11_SF.clusterCoef, COLL11_SF.degreeCent$centralization,
                         COLL11_SF.netDensity, COLL11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************

round = 11
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
COLL11_TFg2 <- data.frame(COLL11_TF)
COLL11_TFg2 <- COLL11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_TFg2$player1
player2vector <- COLL11_TFg2$player2
COLL11_TFg3 <- COLL11_TFg2
COLL11_TFg3$p1inp2vec <- is.element(COLL11_TFg3$player1, player2vector)
COLL11_TFg3$p2inp1vec <- is.element(COLL11_TFg3$player2, player1vector)

addPlayer1 <- COLL11_TFg3[ which(COLL11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_TFg3[ which(COLL11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_TFg2 <- rbind(COLL11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
COLL11_TFft <- ftable(COLL11_TFg2$player1, COLL11_TFg2$player2)
COLL11_TFft2 <- as.matrix(COLL11_TFft)
numRows <- nrow(COLL11_TFft2)
numCols <- ncol(COLL11_TFft2)
COLL11_TFft3 <- COLL11_TFft2[c(2:numRows) , c(2:numCols)]
COLL11_TFTable <- graph.adjacency(COLL11_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(COLL11_TFTable, vertex.label = V(COLL11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
COLL11_TF.clusterCoef <- transitivity(COLL11_TFTable, type="global") #cluster coefficient
COLL11_TF.degreeCent <- centralization.degree(COLL11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_TFftn <- as.network.matrix(COLL11_TFft)
COLL11_TF.netDensity <- network.density(COLL11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_TF.entropy <- entropy(COLL11_TFft) #entropy

COLL11_TF.netMx <- cbind(COLL11_TF.netMx, COLL11_TF.clusterCoef, COLL11_TF.degreeCent$centralization,
                         COLL11_TF.netDensity, COLL11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
COLL11_SAMg2 <- data.frame(COLL11_SAM)
COLL11_SAMg2 <- COLL11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_SAMg2$player1
player2vector <- COLL11_SAMg2$player2
COLL11_SAMg3 <- COLL11_SAMg2
COLL11_SAMg3$p1inp2vec <- is.element(COLL11_SAMg3$player1, player2vector)
COLL11_SAMg3$p2inp1vec <- is.element(COLL11_SAMg3$player2, player1vector)

addPlayer1 <- COLL11_SAMg3[ which(COLL11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_SAMg3[ which(COLL11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_SAMg2 <- rbind(COLL11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
COLL11_SAMft <- ftable(COLL11_SAMg2$player1, COLL11_SAMg2$player2)
COLL11_SAMft2 <- as.matrix(COLL11_SAMft)
numRows <- nrow(COLL11_SAMft2)
numCols <- ncol(COLL11_SAMft2)
COLL11_SAMft3 <- COLL11_SAMft2[c(2:numRows) , c(2:numCols)]
COLL11_SAMTable <- graph.adjacency(COLL11_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(COLL11_SAMTable, vertex.label = V(COLL11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
COLL11_SAM.clusterCoef <- transitivity(COLL11_SAMTable, type="global") #cluster coefficient
COLL11_SAM.degreeCent <- centralization.degree(COLL11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_SAMftn <- as.network.matrix(COLL11_SAMft)
COLL11_SAM.netDensity <- network.density(COLL11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_SAM.entropy <- entropy(COLL11_SAMft) #entropy

COLL11_SAM.netMx <- cbind(COLL11_SAM.netMx, COLL11_SAM.clusterCoef, COLL11_SAM.degreeCent$centralization,
                          COLL11_SAM.netDensity, COLL11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
COLL11_TAMg2 <- data.frame(COLL11_TAM)
COLL11_TAMg2 <- COLL11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_TAMg2$player1
player2vector <- COLL11_TAMg2$player2
COLL11_TAMg3 <- COLL11_TAMg2
COLL11_TAMg3$p1inp2vec <- is.element(COLL11_TAMg3$player1, player2vector)
COLL11_TAMg3$p2inp1vec <- is.element(COLL11_TAMg3$player2, player1vector)

addPlayer1 <- COLL11_TAMg3[ which(COLL11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_TAMg3[ which(COLL11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_TAMg2 <- rbind(COLL11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
COLL11_TAMft <- ftable(COLL11_TAMg2$player1, COLL11_TAMg2$player2)
COLL11_TAMft2 <- as.matrix(COLL11_TAMft)
numRows <- nrow(COLL11_TAMft2)
numCols <- ncol(COLL11_TAMft2)
COLL11_TAMft3 <- COLL11_TAMft2[c(2:numRows) , c(2:numCols)]
COLL11_TAMTable <- graph.adjacency(COLL11_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(COLL11_TAMTable, vertex.label = V(COLL11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
COLL11_TAM.clusterCoef <- transitivity(COLL11_TAMTable, type="global") #cluster coefficient
COLL11_TAM.degreeCent <- centralization.degree(COLL11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_TAMftn <- as.network.matrix(COLL11_TAMft)
COLL11_TAM.netDensity <- network.density(COLL11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_TAM.entropy <- entropy(COLL11_TAMft) #entropy

COLL11_TAM.netMx <- cbind(COLL11_TAM.netMx, COLL11_TAM.clusterCoef, COLL11_TAM.degreeCent$centralization,
                          COLL11_TAM.netDensity, COLL11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
COLL11_SDMg2 <- data.frame(COLL11_SDM)
COLL11_SDMg2 <- COLL11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_SDMg2$player1
player2vector <- COLL11_SDMg2$player2
COLL11_SDMg3 <- COLL11_SDMg2
COLL11_SDMg3$p1inp2vec <- is.element(COLL11_SDMg3$player1, player2vector)
COLL11_SDMg3$p2inp1vec <- is.element(COLL11_SDMg3$player2, player1vector)

addPlayer1 <- COLL11_SDMg3[ which(COLL11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL11_SDMg3[ which(COLL11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_SDMg2 <- rbind(COLL11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
COLL11_SDMft <- ftable(COLL11_SDMg2$player1, COLL11_SDMg2$player2)
COLL11_SDMft2 <- as.matrix(COLL11_SDMft)
numRows <- nrow(COLL11_SDMft2)
numCols <- ncol(COLL11_SDMft2)
COLL11_SDMft3 <- COLL11_SDMft2[c(2:numRows) , c(2:numCols)]
COLL11_SDMTable <- graph.adjacency(COLL11_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(COLL11_SDMTable, vertex.label = V(COLL11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
COLL11_SDM.clusterCoef <- transitivity(COLL11_SDMTable, type="global") #cluster coefficient
COLL11_SDM.degreeCent <- centralization.degree(COLL11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_SDMftn <- as.network.matrix(COLL11_SDMft)
COLL11_SDM.netDensity <- network.density(COLL11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_SDM.entropy <- entropy(COLL11_SDMft) #entropy

COLL11_SDM.netMx <- cbind(COLL11_SDM.netMx, COLL11_SDM.clusterCoef, COLL11_SDM.degreeCent$centralization,
                          COLL11_SDM.netDensity, COLL11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
COLL11_TDMg2 <- data.frame(COLL11_TDM)
COLL11_TDMg2 <- COLL11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_TDMg2$player1
player2vector <- COLL11_TDMg2$player2
COLL11_TDMg3 <- COLL11_TDMg2
COLL11_TDMg3$p1inp2vec <- is.element(COLL11_TDMg3$player1, player2vector)
COLL11_TDMg3$p2inp1vec <- is.element(COLL11_TDMg3$player2, player1vector)

addPlayer1 <- COLL11_TDMg3[ which(COLL11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_TDMg3[ which(COLL11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_TDMg2 <- rbind(COLL11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
COLL11_TDMft <- ftable(COLL11_TDMg2$player1, COLL11_TDMg2$player2)
COLL11_TDMft2 <- as.matrix(COLL11_TDMft)
numRows <- nrow(COLL11_TDMft2)
numCols <- ncol(COLL11_TDMft2)
COLL11_TDMft3 <- COLL11_TDMft2[c(2:numRows) , c(2:numCols)]
COLL11_TDMTable <- graph.adjacency(COLL11_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(COLL11_TDMTable, vertex.label = V(COLL11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
COLL11_TDM.clusterCoef <- transitivity(COLL11_TDMTable, type="global") #cluster coefficient
COLL11_TDM.degreeCent <- centralization.degree(COLL11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_TDMftn <- as.network.matrix(COLL11_TDMft)
COLL11_TDM.netDensity <- network.density(COLL11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_TDM.entropy <- entropy(COLL11_TDMft) #entropy

COLL11_TDM.netMx <- cbind(COLL11_TDM.netMx, COLL11_TDM.clusterCoef, COLL11_TDM.degreeCent$centralization,
                          COLL11_TDM.netDensity, COLL11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
COLL11_SDg2 <- data.frame(COLL11_SD)
COLL11_SDg2 <- COLL11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_SDg2$player1
player2vector <- COLL11_SDg2$player2
COLL11_SDg3 <- COLL11_SDg2
COLL11_SDg3$p1inp2vec <- is.element(COLL11_SDg3$player1, player2vector)
COLL11_SDg3$p2inp1vec <- is.element(COLL11_SDg3$player2, player1vector)

addPlayer1 <- COLL11_SDg3[ which(COLL11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_SDg3[ which(COLL11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_SDg2 <- rbind(COLL11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
COLL11_SDft <- ftable(COLL11_SDg2$player1, COLL11_SDg2$player2)
COLL11_SDft2 <- as.matrix(COLL11_SDft)
numRows <- nrow(COLL11_SDft2)
numCols <- ncol(COLL11_SDft2)
COLL11_SDft3 <- COLL11_SDft2[c(2:numRows) , c(2:numCols)]
COLL11_SDTable <- graph.adjacency(COLL11_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(COLL11_SDTable, vertex.label = V(COLL11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
COLL11_SD.clusterCoef <- transitivity(COLL11_SDTable, type="global") #cluster coefficient
COLL11_SD.degreeCent <- centralization.degree(COLL11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_SDftn <- as.network.matrix(COLL11_SDft)
COLL11_SD.netDensity <- network.density(COLL11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_SD.entropy <- entropy(COLL11_SDft) #entropy

COLL11_SD.netMx <- cbind(COLL11_SD.netMx, COLL11_SD.clusterCoef, COLL11_SD.degreeCent$centralization,
                         COLL11_SD.netDensity, COLL11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
COLL11_TDg2 <- data.frame(COLL11_TD)
COLL11_TDg2 <- COLL11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_TDg2$player1
player2vector <- COLL11_TDg2$player2
COLL11_TDg3 <- COLL11_TDg2
COLL11_TDg3$p1inp2vec <- is.element(COLL11_TDg3$player1, player2vector)
COLL11_TDg3$p2inp1vec <- is.element(COLL11_TDg3$player2, player1vector)

addPlayer1 <- COLL11_TDg3[ which(COLL11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_TDg3[ which(COLL11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_TDg2 <- rbind(COLL11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
COLL11_TDft <- ftable(COLL11_TDg2$player1, COLL11_TDg2$player2)
COLL11_TDft2 <- as.matrix(COLL11_TDft)
numRows <- nrow(COLL11_TDft2)
numCols <- ncol(COLL11_TDft2)
COLL11_TDft3 <- COLL11_TDft2[c(2:numRows) , c(2:numCols)]
COLL11_TDTable <- graph.adjacency(COLL11_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(COLL11_TDTable, vertex.label = V(COLL11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
COLL11_TD.clusterCoef <- transitivity(COLL11_TDTable, type="global") #cluster coefficient
COLL11_TD.degreeCent <- centralization.degree(COLL11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_TDftn <- as.network.matrix(COLL11_TDft)
COLL11_TD.netDensity <- network.density(COLL11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_TD.entropy <- entropy(COLL11_TDft) #entropy

COLL11_TD.netMx <- cbind(COLL11_TD.netMx, COLL11_TD.clusterCoef, COLL11_TD.degreeCent$centralization,
                         COLL11_TD.netDensity, COLL11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
COLL11_QTg2 <- data.frame(COLL11_QT)
COLL11_QTg2 <- COLL11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL11_QTg2$player1
player2vector <- COLL11_QTg2$player2
COLL11_QTg3 <- COLL11_QTg2
COLL11_QTg3$p1inp2vec <- is.element(COLL11_QTg3$player1, player2vector)
COLL11_QTg3$p2inp1vec <- is.element(COLL11_QTg3$player2, player1vector)

addPlayer1 <- COLL11_QTg3[ which(COLL11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL11_QTg3[ which(COLL11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL11_QTg2 <- rbind(COLL11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
COLL11_QTft <- ftable(COLL11_QTg2$player1, COLL11_QTg2$player2)
COLL11_QTft2 <- as.matrix(COLL11_QTft)
numRows <- nrow(COLL11_QTft2)
numCols <- ncol(COLL11_QTft2)
COLL11_QTft3 <- COLL11_QTft2[c(2:numRows) , c(2:numCols)]
COLL11_QTTable <- graph.adjacency(COLL11_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(COLL11_QTTable, vertex.label = V(COLL11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
COLL11_QT.clusterCoef <- transitivity(COLL11_QTTable, type="global") #cluster coefficient
COLL11_QT.degreeCent <- centralization.degree(COLL11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL11_QTftn <- as.network.matrix(COLL11_QTft)
COLL11_QT.netDensity <- network.density(COLL11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL11_QT.entropy <- entropy(COLL11_QTft) #entropy

COLL11_QT.netMx <- cbind(COLL11_QT.netMx, COLL11_QT.clusterCoef, COLL11_QT.degreeCent$centralization,
                         COLL11_QT.netDensity, COLL11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL11_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "ESS"
KIoutcome = "Goal_F"
ESS11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
ESS11_Gg2 <- data.frame(ESS11_G)
ESS11_Gg2 <- ESS11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_Gg2$player1
player2vector <- ESS11_Gg2$player2
ESS11_Gg3 <- ESS11_Gg2
ESS11_Gg3$p1inp2vec <- is.element(ESS11_Gg3$player1, player2vector)
ESS11_Gg3$p2inp1vec <- is.element(ESS11_Gg3$player2, player1vector)

addPlayer1 <- ESS11_Gg3[ which(ESS11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_Gg3[ which(ESS11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_Gg2 <- rbind(ESS11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
ESS11_Gft <- ftable(ESS11_Gg2$player1, ESS11_Gg2$player2)
ESS11_Gft2 <- as.matrix(ESS11_Gft)
numRows <- nrow(ESS11_Gft2)
numCols <- ncol(ESS11_Gft2)
ESS11_Gft3 <- ESS11_Gft2[c(2:numRows) , c(2:numCols)]
ESS11_GTable <- graph.adjacency(ESS11_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(ESS11_GTable, vertex.label = V(ESS11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
ESS11_G.clusterCoef <- transitivity(ESS11_GTable, type="global") #cluster coefficient
ESS11_G.degreeCent <- centralization.degree(ESS11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_Gftn <- as.network.matrix(ESS11_Gft)
ESS11_G.netDensity <- network.density(ESS11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_G.entropy <- entropy(ESS11_Gft) #entropy

ESS11_G.netMx <- cbind(ESS11_G.netMx, ESS11_G.clusterCoef, ESS11_G.degreeCent$centralization,
                       ESS11_G.netDensity, ESS11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "ESS"
KIoutcome = "Behind_F"
ESS11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
ESS11_Bg2 <- data.frame(ESS11_B)
ESS11_Bg2 <- ESS11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_Bg2$player1
player2vector <- ESS11_Bg2$player2
ESS11_Bg3 <- ESS11_Bg2
ESS11_Bg3$p1inp2vec <- is.element(ESS11_Bg3$player1, player2vector)
ESS11_Bg3$p2inp1vec <- is.element(ESS11_Bg3$player2, player1vector)

addPlayer1 <- ESS11_Bg3[ which(ESS11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_Bg3[ which(ESS11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_Bg2 <- rbind(ESS11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
ESS11_Bft <- ftable(ESS11_Bg2$player1, ESS11_Bg2$player2)
ESS11_Bft2 <- as.matrix(ESS11_Bft)
numRows <- nrow(ESS11_Bft2)
numCols <- ncol(ESS11_Bft2)
ESS11_Bft3 <- ESS11_Bft2[c(2:numRows) , c(2:numCols)]
ESS11_BTable <- graph.adjacency(ESS11_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(ESS11_BTable, vertex.label = V(ESS11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
ESS11_B.clusterCoef <- transitivity(ESS11_BTable, type="global") #cluster coefficient
ESS11_B.degreeCent <- centralization.degree(ESS11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_Bftn <- as.network.matrix(ESS11_Bft)
ESS11_B.netDensity <- network.density(ESS11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_B.entropy <- entropy(ESS11_Bft) #entropy

ESS11_B.netMx <- cbind(ESS11_B.netMx, ESS11_B.clusterCoef, ESS11_B.degreeCent$centralization,
                       ESS11_B.netDensity, ESS11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
ESS11_SFg2 <- data.frame(ESS11_SF)
ESS11_SFg2 <- ESS11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_SFg2$player1
player2vector <- ESS11_SFg2$player2
ESS11_SFg3 <- ESS11_SFg2
ESS11_SFg3$p1inp2vec <- is.element(ESS11_SFg3$player1, player2vector)
ESS11_SFg3$p2inp1vec <- is.element(ESS11_SFg3$player2, player1vector)

addPlayer1 <- ESS11_SFg3[ which(ESS11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_SFg3[ which(ESS11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_SFg2 <- rbind(ESS11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
ESS11_SFft <- ftable(ESS11_SFg2$player1, ESS11_SFg2$player2)
ESS11_SFft2 <- as.matrix(ESS11_SFft)
numRows <- nrow(ESS11_SFft2)
numCols <- ncol(ESS11_SFft2)
ESS11_SFft3 <- ESS11_SFft2[c(2:numRows) , c(2:numCols)]
ESS11_SFTable <- graph.adjacency(ESS11_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(ESS11_SFTable, vertex.label = V(ESS11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
ESS11_SF.clusterCoef <- transitivity(ESS11_SFTable, type="global") #cluster coefficient
ESS11_SF.degreeCent <- centralization.degree(ESS11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_SFftn <- as.network.matrix(ESS11_SFft)
ESS11_SF.netDensity <- network.density(ESS11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_SF.entropy <- entropy(ESS11_SFft) #entropy

ESS11_SF.netMx <- cbind(ESS11_SF.netMx, ESS11_SF.clusterCoef, ESS11_SF.degreeCent$centralization,
                        ESS11_SF.netDensity, ESS11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************

round = 11
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
ESS11_TFg2 <- data.frame(ESS11_TF)
ESS11_TFg2 <- ESS11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_TFg2$player1
player2vector <- ESS11_TFg2$player2
ESS11_TFg3 <- ESS11_TFg2
ESS11_TFg3$p1inp2vec <- is.element(ESS11_TFg3$player1, player2vector)
ESS11_TFg3$p2inp1vec <- is.element(ESS11_TFg3$player2, player1vector)

addPlayer1 <- ESS11_TFg3[ which(ESS11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_TFg3[ which(ESS11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_TFg2 <- rbind(ESS11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
ESS11_TFft <- ftable(ESS11_TFg2$player1, ESS11_TFg2$player2)
ESS11_TFft2 <- as.matrix(ESS11_TFft)
numRows <- nrow(ESS11_TFft2)
numCols <- ncol(ESS11_TFft2)
ESS11_TFft3 <- ESS11_TFft2[c(2:numRows) , c(2:numCols)]
ESS11_TFTable <- graph.adjacency(ESS11_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(ESS11_TFTable, vertex.label = V(ESS11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
ESS11_TF.clusterCoef <- transitivity(ESS11_TFTable, type="global") #cluster coefficient
ESS11_TF.degreeCent <- centralization.degree(ESS11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_TFftn <- as.network.matrix(ESS11_TFft)
ESS11_TF.netDensity <- network.density(ESS11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_TF.entropy <- entropy(ESS11_TFft) #entropy

ESS11_TF.netMx <- cbind(ESS11_TF.netMx, ESS11_TF.clusterCoef, ESS11_TF.degreeCent$centralization,
                        ESS11_TF.netDensity, ESS11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************
#NA

round = 11
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
ESS11_SAMg2 <- data.frame(ESS11_SAM)
ESS11_SAMg2 <- ESS11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_SAMg2$player1
player2vector <- ESS11_SAMg2$player2
ESS11_SAMg3 <- ESS11_SAMg2
ESS11_SAMg3$p1inp2vec <- is.element(ESS11_SAMg3$player1, player2vector)
ESS11_SAMg3$p2inp1vec <- is.element(ESS11_SAMg3$player2, player1vector)

addPlayer1 <- ESS11_SAMg3[ which(ESS11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_SAMg3[ which(ESS11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_SAMg2 <- rbind(ESS11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
ESS11_SAMft <- ftable(ESS11_SAMg2$player1, ESS11_SAMg2$player2)
ESS11_SAMft2 <- as.matrix(ESS11_SAMft)
numRows <- nrow(ESS11_SAMft2)
numCols <- ncol(ESS11_SAMft2)
ESS11_SAMft3 <- ESS11_SAMft2[c(2:numRows) , c(2:numCols)]
ESS11_SAMTable <- graph.adjacency(ESS11_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(ESS11_SAMTable, vertex.label = V(ESS11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
ESS11_SAM.clusterCoef <- transitivity(ESS11_SAMTable, type="global") #cluster coefficient
ESS11_SAM.degreeCent <- centralization.degree(ESS11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_SAMftn <- as.network.matrix(ESS11_SAMft)
ESS11_SAM.netDensity <- network.density(ESS11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_SAM.entropy <- entropy(ESS11_SAMft) #entropy

ESS11_SAM.netMx <- cbind(ESS11_SAM.netMx, ESS11_SAM.clusterCoef, ESS11_SAM.degreeCent$centralization,
                         ESS11_SAM.netDensity, ESS11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
ESS11_TAMg2 <- data.frame(ESS11_TAM)
ESS11_TAMg2 <- ESS11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_TAMg2$player1
player2vector <- ESS11_TAMg2$player2
ESS11_TAMg3 <- ESS11_TAMg2
ESS11_TAMg3$p1inp2vec <- is.element(ESS11_TAMg3$player1, player2vector)
ESS11_TAMg3$p2inp1vec <- is.element(ESS11_TAMg3$player2, player1vector)

addPlayer1 <- ESS11_TAMg3[ which(ESS11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_TAMg3[ which(ESS11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_TAMg2 <- rbind(ESS11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
ESS11_TAMft <- ftable(ESS11_TAMg2$player1, ESS11_TAMg2$player2)
ESS11_TAMft2 <- as.matrix(ESS11_TAMft)
numRows <- nrow(ESS11_TAMft2)
numCols <- ncol(ESS11_TAMft2)
ESS11_TAMft3 <- ESS11_TAMft2[c(2:numRows) , c(2:numCols)]
ESS11_TAMTable <- graph.adjacency(ESS11_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(ESS11_TAMTable, vertex.label = V(ESS11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
ESS11_TAM.clusterCoef <- transitivity(ESS11_TAMTable, type="global") #cluster coefficient
ESS11_TAM.degreeCent <- centralization.degree(ESS11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_TAMftn <- as.network.matrix(ESS11_TAMft)
ESS11_TAM.netDensity <- network.density(ESS11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_TAM.entropy <- entropy(ESS11_TAMft) #entropy

ESS11_TAM.netMx <- cbind(ESS11_TAM.netMx, ESS11_TAM.clusterCoef, ESS11_TAM.degreeCent$centralization,
                         ESS11_TAM.netDensity, ESS11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
ESS11_SDMg2 <- data.frame(ESS11_SDM)
ESS11_SDMg2 <- ESS11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_SDMg2$player1
player2vector <- ESS11_SDMg2$player2
ESS11_SDMg3 <- ESS11_SDMg2
ESS11_SDMg3$p1inp2vec <- is.element(ESS11_SDMg3$player1, player2vector)
ESS11_SDMg3$p2inp1vec <- is.element(ESS11_SDMg3$player2, player1vector)

addPlayer1 <- ESS11_SDMg3[ which(ESS11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS11_SDMg3[ which(ESS11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_SDMg2 <- rbind(ESS11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
ESS11_SDMft <- ftable(ESS11_SDMg2$player1, ESS11_SDMg2$player2)
ESS11_SDMft2 <- as.matrix(ESS11_SDMft)
numRows <- nrow(ESS11_SDMft2)
numCols <- ncol(ESS11_SDMft2)
ESS11_SDMft3 <- ESS11_SDMft2[c(2:numRows) , c(2:numCols)]
ESS11_SDMTable <- graph.adjacency(ESS11_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(ESS11_SDMTable, vertex.label = V(ESS11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
ESS11_SDM.clusterCoef <- transitivity(ESS11_SDMTable, type="global") #cluster coefficient
ESS11_SDM.degreeCent <- centralization.degree(ESS11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_SDMftn <- as.network.matrix(ESS11_SDMft)
ESS11_SDM.netDensity <- network.density(ESS11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_SDM.entropy <- entropy(ESS11_SDMft) #entropy

ESS11_SDM.netMx <- cbind(ESS11_SDM.netMx, ESS11_SDM.clusterCoef, ESS11_SDM.degreeCent$centralization,
                         ESS11_SDM.netDensity, ESS11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
ESS11_TDMg2 <- data.frame(ESS11_TDM)
ESS11_TDMg2 <- ESS11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_TDMg2$player1
player2vector <- ESS11_TDMg2$player2
ESS11_TDMg3 <- ESS11_TDMg2
ESS11_TDMg3$p1inp2vec <- is.element(ESS11_TDMg3$player1, player2vector)
ESS11_TDMg3$p2inp1vec <- is.element(ESS11_TDMg3$player2, player1vector)

addPlayer1 <- ESS11_TDMg3[ which(ESS11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_TDMg3[ which(ESS11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_TDMg2 <- rbind(ESS11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
ESS11_TDMft <- ftable(ESS11_TDMg2$player1, ESS11_TDMg2$player2)
ESS11_TDMft2 <- as.matrix(ESS11_TDMft)
numRows <- nrow(ESS11_TDMft2)
numCols <- ncol(ESS11_TDMft2)
ESS11_TDMft3 <- ESS11_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS11_TDMTable <- graph.adjacency(ESS11_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(ESS11_TDMTable, vertex.label = V(ESS11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
ESS11_TDM.clusterCoef <- transitivity(ESS11_TDMTable, type="global") #cluster coefficient
ESS11_TDM.degreeCent <- centralization.degree(ESS11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_TDMftn <- as.network.matrix(ESS11_TDMft)
ESS11_TDM.netDensity <- network.density(ESS11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_TDM.entropy <- entropy(ESS11_TDMft) #entropy

ESS11_TDM.netMx <- cbind(ESS11_TDM.netMx, ESS11_TDM.clusterCoef, ESS11_TDM.degreeCent$centralization,
                         ESS11_TDM.netDensity, ESS11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
ESS11_SDg2 <- data.frame(ESS11_SD)
ESS11_SDg2 <- ESS11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_SDg2$player1
player2vector <- ESS11_SDg2$player2
ESS11_SDg3 <- ESS11_SDg2
ESS11_SDg3$p1inp2vec <- is.element(ESS11_SDg3$player1, player2vector)
ESS11_SDg3$p2inp1vec <- is.element(ESS11_SDg3$player2, player1vector)

addPlayer1 <- ESS11_SDg3[ which(ESS11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_SDg3[ which(ESS11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_SDg2 <- rbind(ESS11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
ESS11_SDft <- ftable(ESS11_SDg2$player1, ESS11_SDg2$player2)
ESS11_SDft2 <- as.matrix(ESS11_SDft)
numRows <- nrow(ESS11_SDft2)
numCols <- ncol(ESS11_SDft2)
ESS11_SDft3 <- ESS11_SDft2[c(2:numRows) , c(2:numCols)]
ESS11_SDTable <- graph.adjacency(ESS11_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(ESS11_SDTable, vertex.label = V(ESS11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
ESS11_SD.clusterCoef <- transitivity(ESS11_SDTable, type="global") #cluster coefficient
ESS11_SD.degreeCent <- centralization.degree(ESS11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_SDftn <- as.network.matrix(ESS11_SDft)
ESS11_SD.netDensity <- network.density(ESS11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_SD.entropy <- entropy(ESS11_SDft) #entropy

ESS11_SD.netMx <- cbind(ESS11_SD.netMx, ESS11_SD.clusterCoef, ESS11_SD.degreeCent$centralization,
                        ESS11_SD.netDensity, ESS11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************

round = 11
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
ESS11_TDg2 <- data.frame(ESS11_TD)
ESS11_TDg2 <- ESS11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_TDg2$player1
player2vector <- ESS11_TDg2$player2
ESS11_TDg3 <- ESS11_TDg2
ESS11_TDg3$p1inp2vec <- is.element(ESS11_TDg3$player1, player2vector)
ESS11_TDg3$p2inp1vec <- is.element(ESS11_TDg3$player2, player1vector)

addPlayer1 <- ESS11_TDg3[ which(ESS11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS11_TDg3[ which(ESS11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_TDg2 <- rbind(ESS11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
ESS11_TDft <- ftable(ESS11_TDg2$player1, ESS11_TDg2$player2)
ESS11_TDft2 <- as.matrix(ESS11_TDft)
numRows <- nrow(ESS11_TDft2)
numCols <- ncol(ESS11_TDft2)
ESS11_TDft3 <- ESS11_TDft2[c(2:numRows) , c(2:numCols)]
ESS11_TDTable <- graph.adjacency(ESS11_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(ESS11_TDTable, vertex.label = V(ESS11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
ESS11_TD.clusterCoef <- transitivity(ESS11_TDTable, type="global") #cluster coefficient
ESS11_TD.degreeCent <- centralization.degree(ESS11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_TDftn <- as.network.matrix(ESS11_TDft)
ESS11_TD.netDensity <- network.density(ESS11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_TD.entropy <- entropy(ESS11_TDft) #entropy

ESS11_TD.netMx <- cbind(ESS11_TD.netMx, ESS11_TD.clusterCoef, ESS11_TD.degreeCent$centralization,
                        ESS11_TD.netDensity, ESS11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
ESS11_QTg2 <- data.frame(ESS11_QT)
ESS11_QTg2 <- ESS11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS11_QTg2$player1
player2vector <- ESS11_QTg2$player2
ESS11_QTg3 <- ESS11_QTg2
ESS11_QTg3$p1inp2vec <- is.element(ESS11_QTg3$player1, player2vector)
ESS11_QTg3$p2inp1vec <- is.element(ESS11_QTg3$player2, player1vector)

addPlayer1 <- ESS11_QTg3[ which(ESS11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS11_QTg3[ which(ESS11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS11_QTg2 <- rbind(ESS11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
ESS11_QTft <- ftable(ESS11_QTg2$player1, ESS11_QTg2$player2)
ESS11_QTft2 <- as.matrix(ESS11_QTft)
numRows <- nrow(ESS11_QTft2)
numCols <- ncol(ESS11_QTft2)
ESS11_QTft3 <- ESS11_QTft2[c(2:numRows) , c(2:numCols)]
ESS11_QTTable <- graph.adjacency(ESS11_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(ESS11_QTTable, vertex.label = V(ESS11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
ESS11_QT.clusterCoef <- transitivity(ESS11_QTTable, type="global") #cluster coefficient
ESS11_QT.degreeCent <- centralization.degree(ESS11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS11_QTftn <- as.network.matrix(ESS11_QTft)
ESS11_QT.netDensity <- network.density(ESS11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS11_QT.entropy <- entropy(ESS11_QTft) #entropy

ESS11_QT.netMx <- cbind(ESS11_QT.netMx, ESS11_QT.clusterCoef, ESS11_QT.degreeCent$centralization,
                        ESS11_QT.netDensity, ESS11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS11_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Goal_F"
FRE11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
FRE11_Gg2 <- data.frame(FRE11_G)
FRE11_Gg2 <- FRE11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_Gg2$player1
player2vector <- FRE11_Gg2$player2
FRE11_Gg3 <- FRE11_Gg2
FRE11_Gg3$p1inp2vec <- is.element(FRE11_Gg3$player1, player2vector)
FRE11_Gg3$p2inp1vec <- is.element(FRE11_Gg3$player2, player1vector)

addPlayer1 <- FRE11_Gg3[ which(FRE11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_Gg3[ which(FRE11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_Gg2 <- rbind(FRE11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
FRE11_Gft <- ftable(FRE11_Gg2$player1, FRE11_Gg2$player2)
FRE11_Gft2 <- as.matrix(FRE11_Gft)
numRows <- nrow(FRE11_Gft2)
numCols <- ncol(FRE11_Gft2)
FRE11_Gft3 <- FRE11_Gft2[c(2:numRows) , c(2:numCols)]
FRE11_GTable <- graph.adjacency(FRE11_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(FRE11_GTable, vertex.label = V(FRE11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
FRE11_G.clusterCoef <- transitivity(FRE11_GTable, type="global") #cluster coefficient
FRE11_G.degreeCent <- centralization.degree(FRE11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_Gftn <- as.network.matrix(FRE11_Gft)
FRE11_G.netDensity <- network.density(FRE11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_G.entropy <- entropy(FRE11_Gft) #entropy

FRE11_G.netMx <- cbind(FRE11_G.netMx, FRE11_G.clusterCoef, FRE11_G.degreeCent$centralization,
                       FRE11_G.netDensity, FRE11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Behind_F"
FRE11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
FRE11_Bg2 <- data.frame(FRE11_B)
FRE11_Bg2 <- FRE11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_Bg2$player1
player2vector <- FRE11_Bg2$player2
FRE11_Bg3 <- FRE11_Bg2
FRE11_Bg3$p1inp2vec <- is.element(FRE11_Bg3$player1, player2vector)
FRE11_Bg3$p2inp1vec <- is.element(FRE11_Bg3$player2, player1vector)

addPlayer1 <- FRE11_Bg3[ which(FRE11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_Bg3[ which(FRE11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_Bg2 <- rbind(FRE11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
FRE11_Bft <- ftable(FRE11_Bg2$player1, FRE11_Bg2$player2)
FRE11_Bft2 <- as.matrix(FRE11_Bft)
numRows <- nrow(FRE11_Bft2)
numCols <- ncol(FRE11_Bft2)
FRE11_Bft3 <- FRE11_Bft2[c(2:numRows) , c(2:numCols)]
FRE11_BTable <- graph.adjacency(FRE11_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(FRE11_BTable, vertex.label = V(FRE11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
FRE11_B.clusterCoef <- transitivity(FRE11_BTable, type="global") #cluster coefficient
FRE11_B.degreeCent <- centralization.degree(FRE11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_Bftn <- as.network.matrix(FRE11_Bft)
FRE11_B.netDensity <- network.density(FRE11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_B.entropy <- entropy(FRE11_Bft) #entropy

FRE11_B.netMx <- cbind(FRE11_B.netMx, FRE11_B.clusterCoef, FRE11_B.degreeCent$centralization,
                       FRE11_B.netDensity, FRE11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
FRE11_SFg2 <- data.frame(FRE11_SF)
FRE11_SFg2 <- FRE11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_SFg2$player1
player2vector <- FRE11_SFg2$player2
FRE11_SFg3 <- FRE11_SFg2
FRE11_SFg3$p1inp2vec <- is.element(FRE11_SFg3$player1, player2vector)
FRE11_SFg3$p2inp1vec <- is.element(FRE11_SFg3$player2, player1vector)

addPlayer1 <- FRE11_SFg3[ which(FRE11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_SFg3[ which(FRE11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_SFg2 <- rbind(FRE11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
FRE11_SFft <- ftable(FRE11_SFg2$player1, FRE11_SFg2$player2)
FRE11_SFft2 <- as.matrix(FRE11_SFft)
numRows <- nrow(FRE11_SFft2)
numCols <- ncol(FRE11_SFft2)
FRE11_SFft3 <- FRE11_SFft2[c(2:numRows) , c(2:numCols)]
FRE11_SFTable <- graph.adjacency(FRE11_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(FRE11_SFTable, vertex.label = V(FRE11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
FRE11_SF.clusterCoef <- transitivity(FRE11_SFTable, type="global") #cluster coefficient
FRE11_SF.degreeCent <- centralization.degree(FRE11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_SFftn <- as.network.matrix(FRE11_SFft)
FRE11_SF.netDensity <- network.density(FRE11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_SF.entropy <- entropy(FRE11_SFft) #entropy

FRE11_SF.netMx <- cbind(FRE11_SF.netMx, FRE11_SF.clusterCoef, FRE11_SF.degreeCent$centralization,
                        FRE11_SF.netDensity, FRE11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
FRE11_TFg2 <- data.frame(FRE11_TF)
FRE11_TFg2 <- FRE11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_TFg2$player1
player2vector <- FRE11_TFg2$player2
FRE11_TFg3 <- FRE11_TFg2
FRE11_TFg3$p1inp2vec <- is.element(FRE11_TFg3$player1, player2vector)
FRE11_TFg3$p2inp1vec <- is.element(FRE11_TFg3$player2, player1vector)

addPlayer1 <- FRE11_TFg3[ which(FRE11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_TFg3[ which(FRE11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_TFg2 <- rbind(FRE11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
FRE11_TFft <- ftable(FRE11_TFg2$player1, FRE11_TFg2$player2)
FRE11_TFft2 <- as.matrix(FRE11_TFft)
numRows <- nrow(FRE11_TFft2)
numCols <- ncol(FRE11_TFft2)
FRE11_TFft3 <- FRE11_TFft2[c(2:numRows) , c(2:numCols)]
FRE11_TFTable <- graph.adjacency(FRE11_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(FRE11_TFTable, vertex.label = V(FRE11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
FRE11_TF.clusterCoef <- transitivity(FRE11_TFTable, type="global") #cluster coefficient
FRE11_TF.degreeCent <- centralization.degree(FRE11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_TFftn <- as.network.matrix(FRE11_TFft)
FRE11_TF.netDensity <- network.density(FRE11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_TF.entropy <- entropy(FRE11_TFft) #entropy

FRE11_TF.netMx <- cbind(FRE11_TF.netMx, FRE11_TF.clusterCoef, FRE11_TF.degreeCent$centralization,
                        FRE11_TF.netDensity, FRE11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
FRE11_SAMg2 <- data.frame(FRE11_SAM)
FRE11_SAMg2 <- FRE11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_SAMg2$player1
player2vector <- FRE11_SAMg2$player2
FRE11_SAMg3 <- FRE11_SAMg2
FRE11_SAMg3$p1inp2vec <- is.element(FRE11_SAMg3$player1, player2vector)
FRE11_SAMg3$p2inp1vec <- is.element(FRE11_SAMg3$player2, player1vector)

addPlayer1 <- FRE11_SAMg3[ which(FRE11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_SAMg3[ which(FRE11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_SAMg2 <- rbind(FRE11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
FRE11_SAMft <- ftable(FRE11_SAMg2$player1, FRE11_SAMg2$player2)
FRE11_SAMft2 <- as.matrix(FRE11_SAMft)
numRows <- nrow(FRE11_SAMft2)
numCols <- ncol(FRE11_SAMft2)
FRE11_SAMft3 <- FRE11_SAMft2[c(2:numRows) , c(2:numCols)]
FRE11_SAMTable <- graph.adjacency(FRE11_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(FRE11_SAMTable, vertex.label = V(FRE11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
FRE11_SAM.clusterCoef <- transitivity(FRE11_SAMTable, type="global") #cluster coefficient
FRE11_SAM.degreeCent <- centralization.degree(FRE11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_SAMftn <- as.network.matrix(FRE11_SAMft)
FRE11_SAM.netDensity <- network.density(FRE11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_SAM.entropy <- entropy(FRE11_SAMft) #entropy

FRE11_SAM.netMx <- cbind(FRE11_SAM.netMx, FRE11_SAM.clusterCoef, FRE11_SAM.degreeCent$centralization,
                         FRE11_SAM.netDensity, FRE11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
FRE11_TAMg2 <- data.frame(FRE11_TAM)
FRE11_TAMg2 <- FRE11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_TAMg2$player1
player2vector <- FRE11_TAMg2$player2
FRE11_TAMg3 <- FRE11_TAMg2
FRE11_TAMg3$p1inp2vec <- is.element(FRE11_TAMg3$player1, player2vector)
FRE11_TAMg3$p2inp1vec <- is.element(FRE11_TAMg3$player2, player1vector)

addPlayer1 <- FRE11_TAMg3[ which(FRE11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_TAMg3[ which(FRE11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_TAMg2 <- rbind(FRE11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
FRE11_TAMft <- ftable(FRE11_TAMg2$player1, FRE11_TAMg2$player2)
FRE11_TAMft2 <- as.matrix(FRE11_TAMft)
numRows <- nrow(FRE11_TAMft2)
numCols <- ncol(FRE11_TAMft2)
FRE11_TAMft3 <- FRE11_TAMft2[c(2:numRows) , c(2:numCols)]
FRE11_TAMTable <- graph.adjacency(FRE11_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(FRE11_TAMTable, vertex.label = V(FRE11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
FRE11_TAM.clusterCoef <- transitivity(FRE11_TAMTable, type="global") #cluster coefficient
FRE11_TAM.degreeCent <- centralization.degree(FRE11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_TAMftn <- as.network.matrix(FRE11_TAMft)
FRE11_TAM.netDensity <- network.density(FRE11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_TAM.entropy <- entropy(FRE11_TAMft) #entropy

FRE11_TAM.netMx <- cbind(FRE11_TAM.netMx, FRE11_TAM.clusterCoef, FRE11_TAM.degreeCent$centralization,
                         FRE11_TAM.netDensity, FRE11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
FRE11_SDMg2 <- data.frame(FRE11_SDM)
FRE11_SDMg2 <- FRE11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_SDMg2$player1
player2vector <- FRE11_SDMg2$player2
FRE11_SDMg3 <- FRE11_SDMg2
FRE11_SDMg3$p1inp2vec <- is.element(FRE11_SDMg3$player1, player2vector)
FRE11_SDMg3$p2inp1vec <- is.element(FRE11_SDMg3$player2, player1vector)

addPlayer1 <- FRE11_SDMg3[ which(FRE11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_SDMg3[ which(FRE11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_SDMg2 <- rbind(FRE11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
FRE11_SDMft <- ftable(FRE11_SDMg2$player1, FRE11_SDMg2$player2)
FRE11_SDMft2 <- as.matrix(FRE11_SDMft)
numRows <- nrow(FRE11_SDMft2)
numCols <- ncol(FRE11_SDMft2)
FRE11_SDMft3 <- FRE11_SDMft2[c(2:numRows) , c(2:numCols)]
FRE11_SDMTable <- graph.adjacency(FRE11_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(FRE11_SDMTable, vertex.label = V(FRE11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
FRE11_SDM.clusterCoef <- transitivity(FRE11_SDMTable, type="global") #cluster coefficient
FRE11_SDM.degreeCent <- centralization.degree(FRE11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_SDMftn <- as.network.matrix(FRE11_SDMft)
FRE11_SDM.netDensity <- network.density(FRE11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_SDM.entropy <- entropy(FRE11_SDMft) #entropy

FRE11_SDM.netMx <- cbind(FRE11_SDM.netMx, FRE11_SDM.clusterCoef, FRE11_SDM.degreeCent$centralization,
                         FRE11_SDM.netDensity, FRE11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
FRE11_TDMg2 <- data.frame(FRE11_TDM)
FRE11_TDMg2 <- FRE11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_TDMg2$player1
player2vector <- FRE11_TDMg2$player2
FRE11_TDMg3 <- FRE11_TDMg2
FRE11_TDMg3$p1inp2vec <- is.element(FRE11_TDMg3$player1, player2vector)
FRE11_TDMg3$p2inp1vec <- is.element(FRE11_TDMg3$player2, player1vector)

addPlayer1 <- FRE11_TDMg3[ which(FRE11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_TDMg3[ which(FRE11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_TDMg2 <- rbind(FRE11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
FRE11_TDMft <- ftable(FRE11_TDMg2$player1, FRE11_TDMg2$player2)
FRE11_TDMft2 <- as.matrix(FRE11_TDMft)
numRows <- nrow(FRE11_TDMft2)
numCols <- ncol(FRE11_TDMft2)
FRE11_TDMft3 <- FRE11_TDMft2[c(2:numRows) , c(2:numCols)]
FRE11_TDMTable <- graph.adjacency(FRE11_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(FRE11_TDMTable, vertex.label = V(FRE11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
FRE11_TDM.clusterCoef <- transitivity(FRE11_TDMTable, type="global") #cluster coefficient
FRE11_TDM.degreeCent <- centralization.degree(FRE11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_TDMftn <- as.network.matrix(FRE11_TDMft)
FRE11_TDM.netDensity <- network.density(FRE11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_TDM.entropy <- entropy(FRE11_TDMft) #entropy

FRE11_TDM.netMx <- cbind(FRE11_TDM.netMx, FRE11_TDM.clusterCoef, FRE11_TDM.degreeCent$centralization,
                         FRE11_TDM.netDensity, FRE11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
FRE11_SDg2 <- data.frame(FRE11_SD)
FRE11_SDg2 <- FRE11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_SDg2$player1
player2vector <- FRE11_SDg2$player2
FRE11_SDg3 <- FRE11_SDg2
FRE11_SDg3$p1inp2vec <- is.element(FRE11_SDg3$player1, player2vector)
FRE11_SDg3$p2inp1vec <- is.element(FRE11_SDg3$player2, player1vector)

addPlayer1 <- FRE11_SDg3[ which(FRE11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_SDg3[ which(FRE11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_SDg2 <- rbind(FRE11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
FRE11_SDft <- ftable(FRE11_SDg2$player1, FRE11_SDg2$player2)
FRE11_SDft2 <- as.matrix(FRE11_SDft)
numRows <- nrow(FRE11_SDft2)
numCols <- ncol(FRE11_SDft2)
FRE11_SDft3 <- FRE11_SDft2[c(2:numRows) , c(2:numCols)]
FRE11_SDTable <- graph.adjacency(FRE11_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(FRE11_SDTable, vertex.label = V(FRE11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
FRE11_SD.clusterCoef <- transitivity(FRE11_SDTable, type="global") #cluster coefficient
FRE11_SD.degreeCent <- centralization.degree(FRE11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_SDftn <- as.network.matrix(FRE11_SDft)
FRE11_SD.netDensity <- network.density(FRE11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_SD.entropy <- entropy(FRE11_SDft) #entropy

FRE11_SD.netMx <- cbind(FRE11_SD.netMx, FRE11_SD.clusterCoef, FRE11_SD.degreeCent$centralization,
                        FRE11_SD.netDensity, FRE11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
FRE11_TDg2 <- data.frame(FRE11_TD)
FRE11_TDg2 <- FRE11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_TDg2$player1
player2vector <- FRE11_TDg2$player2
FRE11_TDg3 <- FRE11_TDg2
FRE11_TDg3$p1inp2vec <- is.element(FRE11_TDg3$player1, player2vector)
FRE11_TDg3$p2inp1vec <- is.element(FRE11_TDg3$player2, player1vector)

addPlayer1 <- FRE11_TDg3[ which(FRE11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_TDg3[ which(FRE11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_TDg2 <- rbind(FRE11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
FRE11_TDft <- ftable(FRE11_TDg2$player1, FRE11_TDg2$player2)
FRE11_TDft2 <- as.matrix(FRE11_TDft)
numRows <- nrow(FRE11_TDft2)
numCols <- ncol(FRE11_TDft2)
FRE11_TDft3 <- FRE11_TDft2[c(2:numRows) , c(2:numCols)]
FRE11_TDTable <- graph.adjacency(FRE11_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(FRE11_TDTable, vertex.label = V(FRE11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
FRE11_TD.clusterCoef <- transitivity(FRE11_TDTable, type="global") #cluster coefficient
FRE11_TD.degreeCent <- centralization.degree(FRE11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_TDftn <- as.network.matrix(FRE11_TDft)
FRE11_TD.netDensity <- network.density(FRE11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_TD.entropy <- entropy(FRE11_TDft) #entropy

FRE11_TD.netMx <- cbind(FRE11_TD.netMx, FRE11_TD.clusterCoef, FRE11_TD.degreeCent$centralization,
                        FRE11_TD.netDensity, FRE11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
FRE11_QTg2 <- data.frame(FRE11_QT)
FRE11_QTg2 <- FRE11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE11_QTg2$player1
player2vector <- FRE11_QTg2$player2
FRE11_QTg3 <- FRE11_QTg2
FRE11_QTg3$p1inp2vec <- is.element(FRE11_QTg3$player1, player2vector)
FRE11_QTg3$p2inp1vec <- is.element(FRE11_QTg3$player2, player1vector)

addPlayer1 <- FRE11_QTg3[ which(FRE11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE11_QTg3[ which(FRE11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE11_QTg2 <- rbind(FRE11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
FRE11_QTft <- ftable(FRE11_QTg2$player1, FRE11_QTg2$player2)
FRE11_QTft2 <- as.matrix(FRE11_QTft)
numRows <- nrow(FRE11_QTft2)
numCols <- ncol(FRE11_QTft2)
FRE11_QTft3 <- FRE11_QTft2[c(2:numRows) , c(2:numCols)]
FRE11_QTTable <- graph.adjacency(FRE11_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(FRE11_QTTable, vertex.label = V(FRE11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
FRE11_QT.clusterCoef <- transitivity(FRE11_QTTable, type="global") #cluster coefficient
FRE11_QT.degreeCent <- centralization.degree(FRE11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE11_QTftn <- as.network.matrix(FRE11_QTft)
FRE11_QT.netDensity <- network.density(FRE11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE11_QT.entropy <- entropy(FRE11_QTft) #entropy

FRE11_QT.netMx <- cbind(FRE11_QT.netMx, FRE11_QT.clusterCoef, FRE11_QT.degreeCent$centralization,
                        FRE11_QT.netDensity, FRE11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE11_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************

round = 11
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
GCFC11_Gg2 <- data.frame(GCFC11_G)
GCFC11_Gg2 <- GCFC11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_Gg2$player1
player2vector <- GCFC11_Gg2$player2
GCFC11_Gg3 <- GCFC11_Gg2
GCFC11_Gg3$p1inp2vec <- is.element(GCFC11_Gg3$player1, player2vector)
GCFC11_Gg3$p2inp1vec <- is.element(GCFC11_Gg3$player2, player1vector)

addPlayer1 <- GCFC11_Gg3[ which(GCFC11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_Gg3[ which(GCFC11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_Gg2 <- rbind(GCFC11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
GCFC11_Gft <- ftable(GCFC11_Gg2$player1, GCFC11_Gg2$player2)
GCFC11_Gft2 <- as.matrix(GCFC11_Gft)
numRows <- nrow(GCFC11_Gft2)
numCols <- ncol(GCFC11_Gft2)
GCFC11_Gft3 <- GCFC11_Gft2[c(2:numRows) , c(2:numCols)]
GCFC11_GTable <- graph.adjacency(GCFC11_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(GCFC11_GTable, vertex.label = V(GCFC11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
GCFC11_G.clusterCoef <- transitivity(GCFC11_GTable, type="global") #cluster coefficient
GCFC11_G.degreeCent <- centralization.degree(GCFC11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_Gftn <- as.network.matrix(GCFC11_Gft)
GCFC11_G.netDensity <- network.density(GCFC11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_G.entropy <- entropy(GCFC11_Gft) #entropy

GCFC11_G.netMx <- cbind(GCFC11_G.netMx, GCFC11_G.clusterCoef, GCFC11_G.degreeCent$centralization,
                        GCFC11_G.netDensity, GCFC11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
GCFC11_Bg2 <- data.frame(GCFC11_B)
GCFC11_Bg2 <- GCFC11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_Bg2$player1
player2vector <- GCFC11_Bg2$player2
GCFC11_Bg3 <- GCFC11_Bg2
GCFC11_Bg3$p1inp2vec <- is.element(GCFC11_Bg3$player1, player2vector)
GCFC11_Bg3$p2inp1vec <- is.element(GCFC11_Bg3$player2, player1vector)

addPlayer1 <- GCFC11_Bg3[ which(GCFC11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_Bg3[ which(GCFC11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_Bg2 <- rbind(GCFC11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
GCFC11_Bft <- ftable(GCFC11_Bg2$player1, GCFC11_Bg2$player2)
GCFC11_Bft2 <- as.matrix(GCFC11_Bft)
numRows <- nrow(GCFC11_Bft2)
numCols <- ncol(GCFC11_Bft2)
GCFC11_Bft3 <- GCFC11_Bft2[c(2:numRows) , c(2:numCols)]
GCFC11_BTable <- graph.adjacency(GCFC11_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(GCFC11_BTable, vertex.label = V(GCFC11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
GCFC11_B.clusterCoef <- transitivity(GCFC11_BTable, type="global") #cluster coefficient
GCFC11_B.degreeCent <- centralization.degree(GCFC11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_Bftn <- as.network.matrix(GCFC11_Bft)
GCFC11_B.netDensity <- network.density(GCFC11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_B.entropy <- entropy(GCFC11_Bft) #entropy

GCFC11_B.netMx <- cbind(GCFC11_B.netMx, GCFC11_B.clusterCoef, GCFC11_B.degreeCent$centralization,
                        GCFC11_B.netDensity, GCFC11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
GCFC11_SFg2 <- data.frame(GCFC11_SF)
GCFC11_SFg2 <- GCFC11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_SFg2$player1
player2vector <- GCFC11_SFg2$player2
GCFC11_SFg3 <- GCFC11_SFg2
GCFC11_SFg3$p1inp2vec <- is.element(GCFC11_SFg3$player1, player2vector)
GCFC11_SFg3$p2inp1vec <- is.element(GCFC11_SFg3$player2, player1vector)

addPlayer1 <- GCFC11_SFg3[ which(GCFC11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC11_SFg2 <- rbind(GCFC11_SFg2, addPlayer1)

#ROUND 11, FWD Stoppage graph using weighted edges
GCFC11_SFft <- ftable(GCFC11_SFg2$player1, GCFC11_SFg2$player2)
GCFC11_SFft2 <- as.matrix(GCFC11_SFft)
numRows <- nrow(GCFC11_SFft2)
numCols <- ncol(GCFC11_SFft2)
GCFC11_SFft3 <- GCFC11_SFft2[c(2:numRows) , c(1:numCols)]
GCFC11_SFTable <- graph.adjacency(GCFC11_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(GCFC11_SFTable, vertex.label = V(GCFC11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
GCFC11_SF.clusterCoef <- transitivity(GCFC11_SFTable, type="global") #cluster coefficient
GCFC11_SF.degreeCent <- centralization.degree(GCFC11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_SFftn <- as.network.matrix(GCFC11_SFft)
GCFC11_SF.netDensity <- network.density(GCFC11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_SF.entropy <- entropy(GCFC11_SFft) #entropy

GCFC11_SF.netMx <- cbind(GCFC11_SF.netMx, GCFC11_SF.clusterCoef, GCFC11_SF.degreeCent$centralization,
                         GCFC11_SF.netDensity, GCFC11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
GCFC11_TFg2 <- data.frame(GCFC11_TF)
GCFC11_TFg2 <- GCFC11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_TFg2$player1
player2vector <- GCFC11_TFg2$player2
GCFC11_TFg3 <- GCFC11_TFg2
GCFC11_TFg3$p1inp2vec <- is.element(GCFC11_TFg3$player1, player2vector)
GCFC11_TFg3$p2inp1vec <- is.element(GCFC11_TFg3$player2, player1vector)

addPlayer1 <- GCFC11_TFg3[ which(GCFC11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_TFg3[ which(GCFC11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_TFg2 <- rbind(GCFC11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
GCFC11_TFft <- ftable(GCFC11_TFg2$player1, GCFC11_TFg2$player2)
GCFC11_TFft2 <- as.matrix(GCFC11_TFft)
numRows <- nrow(GCFC11_TFft2)
numCols <- ncol(GCFC11_TFft2)
GCFC11_TFft3 <- GCFC11_TFft2[c(2:numRows) , c(2:numCols)]
GCFC11_TFTable <- graph.adjacency(GCFC11_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(GCFC11_TFTable, vertex.label = V(GCFC11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
GCFC11_TF.clusterCoef <- transitivity(GCFC11_TFTable, type="global") #cluster coefficient
GCFC11_TF.degreeCent <- centralization.degree(GCFC11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_TFftn <- as.network.matrix(GCFC11_TFft)
GCFC11_TF.netDensity <- network.density(GCFC11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_TF.entropy <- entropy(GCFC11_TFft) #entropy

GCFC11_TF.netMx <- cbind(GCFC11_TF.netMx, GCFC11_TF.clusterCoef, GCFC11_TF.degreeCent$centralization,
                         GCFC11_TF.netDensity, GCFC11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
GCFC11_SAMg2 <- data.frame(GCFC11_SAM)
GCFC11_SAMg2 <- GCFC11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_SAMg2$player1
player2vector <- GCFC11_SAMg2$player2
GCFC11_SAMg3 <- GCFC11_SAMg2
GCFC11_SAMg3$p1inp2vec <- is.element(GCFC11_SAMg3$player1, player2vector)
GCFC11_SAMg3$p2inp1vec <- is.element(GCFC11_SAMg3$player2, player1vector)

addPlayer1 <- GCFC11_SAMg3[ which(GCFC11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_SAMg3[ which(GCFC11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_SAMg2 <- rbind(GCFC11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
GCFC11_SAMft <- ftable(GCFC11_SAMg2$player1, GCFC11_SAMg2$player2)
GCFC11_SAMft2 <- as.matrix(GCFC11_SAMft)
numRows <- nrow(GCFC11_SAMft2)
numCols <- ncol(GCFC11_SAMft2)
GCFC11_SAMft3 <- GCFC11_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC11_SAMTable <- graph.adjacency(GCFC11_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(GCFC11_SAMTable, vertex.label = V(GCFC11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
GCFC11_SAM.clusterCoef <- transitivity(GCFC11_SAMTable, type="global") #cluster coefficient
GCFC11_SAM.degreeCent <- centralization.degree(GCFC11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_SAMftn <- as.network.matrix(GCFC11_SAMft)
GCFC11_SAM.netDensity <- network.density(GCFC11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_SAM.entropy <- entropy(GCFC11_SAMft) #entropy

GCFC11_SAM.netMx <- cbind(GCFC11_SAM.netMx, GCFC11_SAM.clusterCoef, GCFC11_SAM.degreeCent$centralization,
                          GCFC11_SAM.netDensity, GCFC11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
GCFC11_TAMg2 <- data.frame(GCFC11_TAM)
GCFC11_TAMg2 <- GCFC11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_TAMg2$player1
player2vector <- GCFC11_TAMg2$player2
GCFC11_TAMg3 <- GCFC11_TAMg2
GCFC11_TAMg3$p1inp2vec <- is.element(GCFC11_TAMg3$player1, player2vector)
GCFC11_TAMg3$p2inp1vec <- is.element(GCFC11_TAMg3$player2, player1vector)

addPlayer1 <- GCFC11_TAMg3[ which(GCFC11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_TAMg3[ which(GCFC11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_TAMg2 <- rbind(GCFC11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
GCFC11_TAMft <- ftable(GCFC11_TAMg2$player1, GCFC11_TAMg2$player2)
GCFC11_TAMft2 <- as.matrix(GCFC11_TAMft)
numRows <- nrow(GCFC11_TAMft2)
numCols <- ncol(GCFC11_TAMft2)
GCFC11_TAMft3 <- GCFC11_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC11_TAMTable <- graph.adjacency(GCFC11_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(GCFC11_TAMTable, vertex.label = V(GCFC11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
GCFC11_TAM.clusterCoef <- transitivity(GCFC11_TAMTable, type="global") #cluster coefficient
GCFC11_TAM.degreeCent <- centralization.degree(GCFC11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_TAMftn <- as.network.matrix(GCFC11_TAMft)
GCFC11_TAM.netDensity <- network.density(GCFC11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_TAM.entropy <- entropy(GCFC11_TAMft) #entropy

GCFC11_TAM.netMx <- cbind(GCFC11_TAM.netMx, GCFC11_TAM.clusterCoef, GCFC11_TAM.degreeCent$centralization,
                          GCFC11_TAM.netDensity, GCFC11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
GCFC11_SDMg2 <- data.frame(GCFC11_SDM)
GCFC11_SDMg2 <- GCFC11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_SDMg2$player1
player2vector <- GCFC11_SDMg2$player2
GCFC11_SDMg3 <- GCFC11_SDMg2
GCFC11_SDMg3$p1inp2vec <- is.element(GCFC11_SDMg3$player1, player2vector)
GCFC11_SDMg3$p2inp1vec <- is.element(GCFC11_SDMg3$player2, player1vector)

addPlayer1 <- GCFC11_SDMg3[ which(GCFC11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_SDMg3[ which(GCFC11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_SDMg2 <- rbind(GCFC11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
GCFC11_SDMft <- ftable(GCFC11_SDMg2$player1, GCFC11_SDMg2$player2)
GCFC11_SDMft2 <- as.matrix(GCFC11_SDMft)
numRows <- nrow(GCFC11_SDMft2)
numCols <- ncol(GCFC11_SDMft2)
GCFC11_SDMft3 <- GCFC11_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC11_SDMTable <- graph.adjacency(GCFC11_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(GCFC11_SDMTable, vertex.label = V(GCFC11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
GCFC11_SDM.clusterCoef <- transitivity(GCFC11_SDMTable, type="global") #cluster coefficient
GCFC11_SDM.degreeCent <- centralization.degree(GCFC11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_SDMftn <- as.network.matrix(GCFC11_SDMft)
GCFC11_SDM.netDensity <- network.density(GCFC11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_SDM.entropy <- entropy(GCFC11_SDMft) #entropy

GCFC11_SDM.netMx <- cbind(GCFC11_SDM.netMx, GCFC11_SDM.clusterCoef, GCFC11_SDM.degreeCent$centralization,
                          GCFC11_SDM.netDensity, GCFC11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
GCFC11_TDMg2 <- data.frame(GCFC11_TDM)
GCFC11_TDMg2 <- GCFC11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_TDMg2$player1
player2vector <- GCFC11_TDMg2$player2
GCFC11_TDMg3 <- GCFC11_TDMg2
GCFC11_TDMg3$p1inp2vec <- is.element(GCFC11_TDMg3$player1, player2vector)
GCFC11_TDMg3$p2inp1vec <- is.element(GCFC11_TDMg3$player2, player1vector)

addPlayer1 <- GCFC11_TDMg3[ which(GCFC11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_TDMg3[ which(GCFC11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_TDMg2 <- rbind(GCFC11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
GCFC11_TDMft <- ftable(GCFC11_TDMg2$player1, GCFC11_TDMg2$player2)
GCFC11_TDMft2 <- as.matrix(GCFC11_TDMft)
numRows <- nrow(GCFC11_TDMft2)
numCols <- ncol(GCFC11_TDMft2)
GCFC11_TDMft3 <- GCFC11_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC11_TDMTable <- graph.adjacency(GCFC11_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(GCFC11_TDMTable, vertex.label = V(GCFC11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
GCFC11_TDM.clusterCoef <- transitivity(GCFC11_TDMTable, type="global") #cluster coefficient
GCFC11_TDM.degreeCent <- centralization.degree(GCFC11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_TDMftn <- as.network.matrix(GCFC11_TDMft)
GCFC11_TDM.netDensity <- network.density(GCFC11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_TDM.entropy <- entropy(GCFC11_TDMft) #entropy

GCFC11_TDM.netMx <- cbind(GCFC11_TDM.netMx, GCFC11_TDM.clusterCoef, GCFC11_TDM.degreeCent$centralization,
                          GCFC11_TDM.netDensity, GCFC11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
GCFC11_SDg2 <- data.frame(GCFC11_SD)
GCFC11_SDg2 <- GCFC11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_SDg2$player1
player2vector <- GCFC11_SDg2$player2
GCFC11_SDg3 <- GCFC11_SDg2
GCFC11_SDg3$p1inp2vec <- is.element(GCFC11_SDg3$player1, player2vector)
GCFC11_SDg3$p2inp1vec <- is.element(GCFC11_SDg3$player2, player1vector)

addPlayer1 <- GCFC11_SDg3[ which(GCFC11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_SDg3[ which(GCFC11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_SDg2 <- rbind(GCFC11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
GCFC11_SDft <- ftable(GCFC11_SDg2$player1, GCFC11_SDg2$player2)
GCFC11_SDft2 <- as.matrix(GCFC11_SDft)
numRows <- nrow(GCFC11_SDft2)
numCols <- ncol(GCFC11_SDft2)
GCFC11_SDft3 <- GCFC11_SDft2[c(2:numRows) , c(2:numCols)]
GCFC11_SDTable <- graph.adjacency(GCFC11_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(GCFC11_SDTable, vertex.label = V(GCFC11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
GCFC11_SD.clusterCoef <- transitivity(GCFC11_SDTable, type="global") #cluster coefficient
GCFC11_SD.degreeCent <- centralization.degree(GCFC11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_SDftn <- as.network.matrix(GCFC11_SDft)
GCFC11_SD.netDensity <- network.density(GCFC11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_SD.entropy <- entropy(GCFC11_SDft) #entropy

GCFC11_SD.netMx <- cbind(GCFC11_SD.netMx, GCFC11_SD.clusterCoef, GCFC11_SD.degreeCent$centralization,
                         GCFC11_SD.netDensity, GCFC11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
GCFC11_TDg2 <- data.frame(GCFC11_TD)
GCFC11_TDg2 <- GCFC11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_TDg2$player1
player2vector <- GCFC11_TDg2$player2
GCFC11_TDg3 <- GCFC11_TDg2
GCFC11_TDg3$p1inp2vec <- is.element(GCFC11_TDg3$player1, player2vector)
GCFC11_TDg3$p2inp1vec <- is.element(GCFC11_TDg3$player2, player1vector)

addPlayer1 <- GCFC11_TDg3[ which(GCFC11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_TDg3[ which(GCFC11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_TDg2 <- rbind(GCFC11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
GCFC11_TDft <- ftable(GCFC11_TDg2$player1, GCFC11_TDg2$player2)
GCFC11_TDft2 <- as.matrix(GCFC11_TDft)
numRows <- nrow(GCFC11_TDft2)
numCols <- ncol(GCFC11_TDft2)
GCFC11_TDft3 <- GCFC11_TDft2[c(2:numRows) , c(2:numCols)]
GCFC11_TDTable <- graph.adjacency(GCFC11_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(GCFC11_TDTable, vertex.label = V(GCFC11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
GCFC11_TD.clusterCoef <- transitivity(GCFC11_TDTable, type="global") #cluster coefficient
GCFC11_TD.degreeCent <- centralization.degree(GCFC11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_TDftn <- as.network.matrix(GCFC11_TDft)
GCFC11_TD.netDensity <- network.density(GCFC11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_TD.entropy <- entropy(GCFC11_TDft) #entropy

GCFC11_TD.netMx <- cbind(GCFC11_TD.netMx, GCFC11_TD.clusterCoef, GCFC11_TD.degreeCent$centralization,
                         GCFC11_TD.netDensity, GCFC11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
GCFC11_QTg2 <- data.frame(GCFC11_QT)
GCFC11_QTg2 <- GCFC11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC11_QTg2$player1
player2vector <- GCFC11_QTg2$player2
GCFC11_QTg3 <- GCFC11_QTg2
GCFC11_QTg3$p1inp2vec <- is.element(GCFC11_QTg3$player1, player2vector)
GCFC11_QTg3$p2inp1vec <- is.element(GCFC11_QTg3$player2, player1vector)

addPlayer1 <- GCFC11_QTg3[ which(GCFC11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC11_QTg3[ which(GCFC11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC11_QTg2 <- rbind(GCFC11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
GCFC11_QTft <- ftable(GCFC11_QTg2$player1, GCFC11_QTg2$player2)
GCFC11_QTft2 <- as.matrix(GCFC11_QTft)
numRows <- nrow(GCFC11_QTft2)
numCols <- ncol(GCFC11_QTft2)
GCFC11_QTft3 <- GCFC11_QTft2[c(2:numRows) , c(2:numCols)]
GCFC11_QTTable <- graph.adjacency(GCFC11_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(GCFC11_QTTable, vertex.label = V(GCFC11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
GCFC11_QT.clusterCoef <- transitivity(GCFC11_QTTable, type="global") #cluster coefficient
GCFC11_QT.degreeCent <- centralization.degree(GCFC11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC11_QTftn <- as.network.matrix(GCFC11_QTft)
GCFC11_QT.netDensity <- network.density(GCFC11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC11_QT.entropy <- entropy(GCFC11_QTft) #entropy

GCFC11_QT.netMx <- cbind(GCFC11_QT.netMx, GCFC11_QT.clusterCoef, GCFC11_QT.degreeCent$centralization,
                         GCFC11_QT.netDensity, GCFC11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC11_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
GEEL11_Gg2 <- data.frame(GEEL11_G)
GEEL11_Gg2 <- GEEL11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_Gg2$player1
player2vector <- GEEL11_Gg2$player2
GEEL11_Gg3 <- GEEL11_Gg2
GEEL11_Gg3$p1inp2vec <- is.element(GEEL11_Gg3$player1, player2vector)
GEEL11_Gg3$p2inp1vec <- is.element(GEEL11_Gg3$player2, player1vector)

addPlayer1 <- GEEL11_Gg3[ which(GEEL11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_Gg3[ which(GEEL11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_Gg2 <- rbind(GEEL11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
GEEL11_Gft <- ftable(GEEL11_Gg2$player1, GEEL11_Gg2$player2)
GEEL11_Gft2 <- as.matrix(GEEL11_Gft)
numRows <- nrow(GEEL11_Gft2)
numCols <- ncol(GEEL11_Gft2)
GEEL11_Gft3 <- GEEL11_Gft2[c(2:numRows) , c(2:numCols)]
GEEL11_GTable <- graph.adjacency(GEEL11_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(GEEL11_GTable, vertex.label = V(GEEL11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
GEEL11_G.clusterCoef <- transitivity(GEEL11_GTable, type="global") #cluster coefficient
GEEL11_G.degreeCent <- centralization.degree(GEEL11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_Gftn <- as.network.matrix(GEEL11_Gft)
GEEL11_G.netDensity <- network.density(GEEL11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_G.entropy <- entropy(GEEL11_Gft) #entropy

GEEL11_G.netMx <- cbind(GEEL11_G.netMx, GEEL11_G.clusterCoef, GEEL11_G.degreeCent$centralization,
                        GEEL11_G.netDensity, GEEL11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
GEEL11_Bg2 <- data.frame(GEEL11_B)
GEEL11_Bg2 <- GEEL11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_Bg2$player1
player2vector <- GEEL11_Bg2$player2
GEEL11_Bg3 <- GEEL11_Bg2
GEEL11_Bg3$p1inp2vec <- is.element(GEEL11_Bg3$player1, player2vector)
GEEL11_Bg3$p2inp1vec <- is.element(GEEL11_Bg3$player2, player1vector)

addPlayer1 <- GEEL11_Bg3[ which(GEEL11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_Bg3[ which(GEEL11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_Bg2 <- rbind(GEEL11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
GEEL11_Bft <- ftable(GEEL11_Bg2$player1, GEEL11_Bg2$player2)
GEEL11_Bft2 <- as.matrix(GEEL11_Bft)
numRows <- nrow(GEEL11_Bft2)
numCols <- ncol(GEEL11_Bft2)
GEEL11_Bft3 <- GEEL11_Bft2[c(2:numRows) , c(2:numCols)]
GEEL11_BTable <- graph.adjacency(GEEL11_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(GEEL11_BTable, vertex.label = V(GEEL11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
GEEL11_B.clusterCoef <- transitivity(GEEL11_BTable, type="global") #cluster coefficient
GEEL11_B.degreeCent <- centralization.degree(GEEL11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_Bftn <- as.network.matrix(GEEL11_Bft)
GEEL11_B.netDensity <- network.density(GEEL11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_B.entropy <- entropy(GEEL11_Bft) #entropy

GEEL11_B.netMx <- cbind(GEEL11_B.netMx, GEEL11_B.clusterCoef, GEEL11_B.degreeCent$centralization,
                        GEEL11_B.netDensity, GEEL11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
GEEL11_SFg2 <- data.frame(GEEL11_SF)
GEEL11_SFg2 <- GEEL11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_SFg2$player1
player2vector <- GEEL11_SFg2$player2
GEEL11_SFg3 <- GEEL11_SFg2
GEEL11_SFg3$p1inp2vec <- is.element(GEEL11_SFg3$player1, player2vector)
GEEL11_SFg3$p2inp1vec <- is.element(GEEL11_SFg3$player2, player1vector)

addPlayer1 <- GEEL11_SFg3[ which(GEEL11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_SFg3[ which(GEEL11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_SFg2 <- rbind(GEEL11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
GEEL11_SFft <- ftable(GEEL11_SFg2$player1, GEEL11_SFg2$player2)
GEEL11_SFft2 <- as.matrix(GEEL11_SFft)
numRows <- nrow(GEEL11_SFft2)
numCols <- ncol(GEEL11_SFft2)
GEEL11_SFft3 <- GEEL11_SFft2[c(2:numRows) , c(2:numCols)]
GEEL11_SFTable <- graph.adjacency(GEEL11_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(GEEL11_SFTable, vertex.label = V(GEEL11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
GEEL11_SF.clusterCoef <- transitivity(GEEL11_SFTable, type="global") #cluster coefficient
GEEL11_SF.degreeCent <- centralization.degree(GEEL11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_SFftn <- as.network.matrix(GEEL11_SFft)
GEEL11_SF.netDensity <- network.density(GEEL11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_SF.entropy <- entropy(GEEL11_SFft) #entropy

GEEL11_SF.netMx <- cbind(GEEL11_SF.netMx, GEEL11_SF.clusterCoef, GEEL11_SF.degreeCent$centralization,
                         GEEL11_SF.netDensity, GEEL11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
GEEL11_TFg2 <- data.frame(GEEL11_TF)
GEEL11_TFg2 <- GEEL11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_TFg2$player1
player2vector <- GEEL11_TFg2$player2
GEEL11_TFg3 <- GEEL11_TFg2
GEEL11_TFg3$p1inp2vec <- is.element(GEEL11_TFg3$player1, player2vector)
GEEL11_TFg3$p2inp1vec <- is.element(GEEL11_TFg3$player2, player1vector)

addPlayer1 <- GEEL11_TFg3[ which(GEEL11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_TFg3[ which(GEEL11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_TFg2 <- rbind(GEEL11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
GEEL11_TFft <- ftable(GEEL11_TFg2$player1, GEEL11_TFg2$player2)
GEEL11_TFft2 <- as.matrix(GEEL11_TFft)
numRows <- nrow(GEEL11_TFft2)
numCols <- ncol(GEEL11_TFft2)
GEEL11_TFft3 <- GEEL11_TFft2[c(2:numRows) , c(2:numCols)]
GEEL11_TFTable <- graph.adjacency(GEEL11_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(GEEL11_TFTable, vertex.label = V(GEEL11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
GEEL11_TF.clusterCoef <- transitivity(GEEL11_TFTable, type="global") #cluster coefficient
GEEL11_TF.degreeCent <- centralization.degree(GEEL11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_TFftn <- as.network.matrix(GEEL11_TFft)
GEEL11_TF.netDensity <- network.density(GEEL11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_TF.entropy <- entropy(GEEL11_TFft) #entropy

GEEL11_TF.netMx <- cbind(GEEL11_TF.netMx, GEEL11_TF.clusterCoef, GEEL11_TF.degreeCent$centralization,
                         GEEL11_TF.netDensity, GEEL11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
GEEL11_SAMg2 <- data.frame(GEEL11_SAM)
GEEL11_SAMg2 <- GEEL11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_SAMg2$player1
player2vector <- GEEL11_SAMg2$player2
GEEL11_SAMg3 <- GEEL11_SAMg2
GEEL11_SAMg3$p1inp2vec <- is.element(GEEL11_SAMg3$player1, player2vector)
GEEL11_SAMg3$p2inp1vec <- is.element(GEEL11_SAMg3$player2, player1vector)

addPlayer1 <- GEEL11_SAMg3[ which(GEEL11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_SAMg3[ which(GEEL11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_SAMg2 <- rbind(GEEL11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
GEEL11_SAMft <- ftable(GEEL11_SAMg2$player1, GEEL11_SAMg2$player2)
GEEL11_SAMft2 <- as.matrix(GEEL11_SAMft)
numRows <- nrow(GEEL11_SAMft2)
numCols <- ncol(GEEL11_SAMft2)
GEEL11_SAMft3 <- GEEL11_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL11_SAMTable <- graph.adjacency(GEEL11_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(GEEL11_SAMTable, vertex.label = V(GEEL11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
GEEL11_SAM.clusterCoef <- transitivity(GEEL11_SAMTable, type="global") #cluster coefficient
GEEL11_SAM.degreeCent <- centralization.degree(GEEL11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_SAMftn <- as.network.matrix(GEEL11_SAMft)
GEEL11_SAM.netDensity <- network.density(GEEL11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_SAM.entropy <- entropy(GEEL11_SAMft) #entropy

GEEL11_SAM.netMx <- cbind(GEEL11_SAM.netMx, GEEL11_SAM.clusterCoef, GEEL11_SAM.degreeCent$centralization,
                          GEEL11_SAM.netDensity, GEEL11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
GEEL11_TAMg2 <- data.frame(GEEL11_TAM)
GEEL11_TAMg2 <- GEEL11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_TAMg2$player1
player2vector <- GEEL11_TAMg2$player2
GEEL11_TAMg3 <- GEEL11_TAMg2
GEEL11_TAMg3$p1inp2vec <- is.element(GEEL11_TAMg3$player1, player2vector)
GEEL11_TAMg3$p2inp1vec <- is.element(GEEL11_TAMg3$player2, player1vector)

addPlayer1 <- GEEL11_TAMg3[ which(GEEL11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_TAMg3[ which(GEEL11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_TAMg2 <- rbind(GEEL11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
GEEL11_TAMft <- ftable(GEEL11_TAMg2$player1, GEEL11_TAMg2$player2)
GEEL11_TAMft2 <- as.matrix(GEEL11_TAMft)
numRows <- nrow(GEEL11_TAMft2)
numCols <- ncol(GEEL11_TAMft2)
GEEL11_TAMft3 <- GEEL11_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL11_TAMTable <- graph.adjacency(GEEL11_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(GEEL11_TAMTable, vertex.label = V(GEEL11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
GEEL11_TAM.clusterCoef <- transitivity(GEEL11_TAMTable, type="global") #cluster coefficient
GEEL11_TAM.degreeCent <- centralization.degree(GEEL11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_TAMftn <- as.network.matrix(GEEL11_TAMft)
GEEL11_TAM.netDensity <- network.density(GEEL11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_TAM.entropy <- entropy(GEEL11_TAMft) #entropy

GEEL11_TAM.netMx <- cbind(GEEL11_TAM.netMx, GEEL11_TAM.clusterCoef, GEEL11_TAM.degreeCent$centralization,
                          GEEL11_TAM.netDensity, GEEL11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
GEEL11_SDMg2 <- data.frame(GEEL11_SDM)
GEEL11_SDMg2 <- GEEL11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_SDMg2$player1
player2vector <- GEEL11_SDMg2$player2
GEEL11_SDMg3 <- GEEL11_SDMg2
GEEL11_SDMg3$p1inp2vec <- is.element(GEEL11_SDMg3$player1, player2vector)
GEEL11_SDMg3$p2inp1vec <- is.element(GEEL11_SDMg3$player2, player1vector)

addPlayer1 <- GEEL11_SDMg3[ which(GEEL11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_SDMg3[ which(GEEL11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_SDMg2 <- rbind(GEEL11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
GEEL11_SDMft <- ftable(GEEL11_SDMg2$player1, GEEL11_SDMg2$player2)
GEEL11_SDMft2 <- as.matrix(GEEL11_SDMft)
numRows <- nrow(GEEL11_SDMft2)
numCols <- ncol(GEEL11_SDMft2)
GEEL11_SDMft3 <- GEEL11_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL11_SDMTable <- graph.adjacency(GEEL11_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(GEEL11_SDMTable, vertex.label = V(GEEL11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
GEEL11_SDM.clusterCoef <- transitivity(GEEL11_SDMTable, type="global") #cluster coefficient
GEEL11_SDM.degreeCent <- centralization.degree(GEEL11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_SDMftn <- as.network.matrix(GEEL11_SDMft)
GEEL11_SDM.netDensity <- network.density(GEEL11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_SDM.entropy <- entropy(GEEL11_SDMft) #entropy

GEEL11_SDM.netMx <- cbind(GEEL11_SDM.netMx, GEEL11_SDM.clusterCoef, GEEL11_SDM.degreeCent$centralization,
                          GEEL11_SDM.netDensity, GEEL11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
GEEL11_TDMg2 <- data.frame(GEEL11_TDM)
GEEL11_TDMg2 <- GEEL11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_TDMg2$player1
player2vector <- GEEL11_TDMg2$player2
GEEL11_TDMg3 <- GEEL11_TDMg2
GEEL11_TDMg3$p1inp2vec <- is.element(GEEL11_TDMg3$player1, player2vector)
GEEL11_TDMg3$p2inp1vec <- is.element(GEEL11_TDMg3$player2, player1vector)

addPlayer1 <- GEEL11_TDMg3[ which(GEEL11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_TDMg3[ which(GEEL11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_TDMg2 <- rbind(GEEL11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
GEEL11_TDMft <- ftable(GEEL11_TDMg2$player1, GEEL11_TDMg2$player2)
GEEL11_TDMft2 <- as.matrix(GEEL11_TDMft)
numRows <- nrow(GEEL11_TDMft2)
numCols <- ncol(GEEL11_TDMft2)
GEEL11_TDMft3 <- GEEL11_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL11_TDMTable <- graph.adjacency(GEEL11_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(GEEL11_TDMTable, vertex.label = V(GEEL11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
GEEL11_TDM.clusterCoef <- transitivity(GEEL11_TDMTable, type="global") #cluster coefficient
GEEL11_TDM.degreeCent <- centralization.degree(GEEL11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_TDMftn <- as.network.matrix(GEEL11_TDMft)
GEEL11_TDM.netDensity <- network.density(GEEL11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_TDM.entropy <- entropy(GEEL11_TDMft) #entropy

GEEL11_TDM.netMx <- cbind(GEEL11_TDM.netMx, GEEL11_TDM.clusterCoef, GEEL11_TDM.degreeCent$centralization,
                          GEEL11_TDM.netDensity, GEEL11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
GEEL11_SDg2 <- data.frame(GEEL11_SD)
GEEL11_SDg2 <- GEEL11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_SDg2$player1
player2vector <- GEEL11_SDg2$player2
GEEL11_SDg3 <- GEEL11_SDg2
GEEL11_SDg3$p1inp2vec <- is.element(GEEL11_SDg3$player1, player2vector)
GEEL11_SDg3$p2inp1vec <- is.element(GEEL11_SDg3$player2, player1vector)

addPlayer1 <- GEEL11_SDg3[ which(GEEL11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_SDg3[ which(GEEL11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_SDg2 <- rbind(GEEL11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
GEEL11_SDft <- ftable(GEEL11_SDg2$player1, GEEL11_SDg2$player2)
GEEL11_SDft2 <- as.matrix(GEEL11_SDft)
numRows <- nrow(GEEL11_SDft2)
numCols <- ncol(GEEL11_SDft2)
GEEL11_SDft3 <- GEEL11_SDft2[c(2:numRows) , c(2:numCols)]
GEEL11_SDTable <- graph.adjacency(GEEL11_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(GEEL11_SDTable, vertex.label = V(GEEL11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
GEEL11_SD.clusterCoef <- transitivity(GEEL11_SDTable, type="global") #cluster coefficient
GEEL11_SD.degreeCent <- centralization.degree(GEEL11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_SDftn <- as.network.matrix(GEEL11_SDft)
GEEL11_SD.netDensity <- network.density(GEEL11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_SD.entropy <- entropy(GEEL11_SDft) #entropy

GEEL11_SD.netMx <- cbind(GEEL11_SD.netMx, GEEL11_SD.clusterCoef, GEEL11_SD.degreeCent$centralization,
                         GEEL11_SD.netDensity, GEEL11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
GEEL11_TDg2 <- data.frame(GEEL11_TD)
GEEL11_TDg2 <- GEEL11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_TDg2$player1
player2vector <- GEEL11_TDg2$player2
GEEL11_TDg3 <- GEEL11_TDg2
GEEL11_TDg3$p1inp2vec <- is.element(GEEL11_TDg3$player1, player2vector)
GEEL11_TDg3$p2inp1vec <- is.element(GEEL11_TDg3$player2, player1vector)

addPlayer1 <- GEEL11_TDg3[ which(GEEL11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_TDg3[ which(GEEL11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_TDg2 <- rbind(GEEL11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
GEEL11_TDft <- ftable(GEEL11_TDg2$player1, GEEL11_TDg2$player2)
GEEL11_TDft2 <- as.matrix(GEEL11_TDft)
numRows <- nrow(GEEL11_TDft2)
numCols <- ncol(GEEL11_TDft2)
GEEL11_TDft3 <- GEEL11_TDft2[c(2:numRows) , c(2:numCols)]
GEEL11_TDTable <- graph.adjacency(GEEL11_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(GEEL11_TDTable, vertex.label = V(GEEL11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
GEEL11_TD.clusterCoef <- transitivity(GEEL11_TDTable, type="global") #cluster coefficient
GEEL11_TD.degreeCent <- centralization.degree(GEEL11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_TDftn <- as.network.matrix(GEEL11_TDft)
GEEL11_TD.netDensity <- network.density(GEEL11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_TD.entropy <- entropy(GEEL11_TDft) #entropy

GEEL11_TD.netMx <- cbind(GEEL11_TD.netMx, GEEL11_TD.clusterCoef, GEEL11_TD.degreeCent$centralization,
                         GEEL11_TD.netDensity, GEEL11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
GEEL11_QTg2 <- data.frame(GEEL11_QT)
GEEL11_QTg2 <- GEEL11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL11_QTg2$player1
player2vector <- GEEL11_QTg2$player2
GEEL11_QTg3 <- GEEL11_QTg2
GEEL11_QTg3$p1inp2vec <- is.element(GEEL11_QTg3$player1, player2vector)
GEEL11_QTg3$p2inp1vec <- is.element(GEEL11_QTg3$player2, player1vector)

addPlayer1 <- GEEL11_QTg3[ which(GEEL11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL11_QTg3[ which(GEEL11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL11_QTg2 <- rbind(GEEL11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
GEEL11_QTft <- ftable(GEEL11_QTg2$player1, GEEL11_QTg2$player2)
GEEL11_QTft2 <- as.matrix(GEEL11_QTft)
numRows <- nrow(GEEL11_QTft2)
numCols <- ncol(GEEL11_QTft2)
GEEL11_QTft3 <- GEEL11_QTft2[c(2:numRows) , c(2:numCols)]
GEEL11_QTTable <- graph.adjacency(GEEL11_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(GEEL11_QTTable, vertex.label = V(GEEL11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
GEEL11_QT.clusterCoef <- transitivity(GEEL11_QTTable, type="global") #cluster coefficient
GEEL11_QT.degreeCent <- centralization.degree(GEEL11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL11_QTftn <- as.network.matrix(GEEL11_QTft)
GEEL11_QT.netDensity <- network.density(GEEL11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL11_QT.entropy <- entropy(GEEL11_QTft) #entropy

GEEL11_QT.netMx <- cbind(GEEL11_QT.netMx, GEEL11_QT.clusterCoef, GEEL11_QT.degreeCent$centralization,
                         GEEL11_QT.netDensity, GEEL11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL11_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "Goal_F"
GWS11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
GWS11_Gg2 <- data.frame(GWS11_G)
GWS11_Gg2 <- GWS11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_Gg2$player1
player2vector <- GWS11_Gg2$player2
GWS11_Gg3 <- GWS11_Gg2
GWS11_Gg3$p1inp2vec <- is.element(GWS11_Gg3$player1, player2vector)
GWS11_Gg3$p2inp1vec <- is.element(GWS11_Gg3$player2, player1vector)

addPlayer1 <- GWS11_Gg3[ which(GWS11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_Gg3[ which(GWS11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_Gg2 <- rbind(GWS11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
GWS11_Gft <- ftable(GWS11_Gg2$player1, GWS11_Gg2$player2)
GWS11_Gft2 <- as.matrix(GWS11_Gft)
numRows <- nrow(GWS11_Gft2)
numCols <- ncol(GWS11_Gft2)
GWS11_Gft3 <- GWS11_Gft2[c(1:numRows) , c(1:numCols)]
GWS11_GTable <- graph.adjacency(GWS11_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(GWS11_GTable, vertex.label = V(GWS11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
GWS11_G.clusterCoef <- transitivity(GWS11_GTable, type="global") #cluster coefficient
GWS11_G.degreeCent <- centralization.degree(GWS11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_Gftn <- as.network.matrix(GWS11_Gft)
GWS11_G.netDensity <- network.density(GWS11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_G.entropy <- entropy(GWS11_Gft) #entropy

GWS11_G.netMx <- cbind(GWS11_G.netMx, GWS11_G.clusterCoef, GWS11_G.degreeCent$centralization,
                       GWS11_G.netDensity, GWS11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************

round = 11
teamName = "GWS"
KIoutcome = "Behind_F"
GWS11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
GWS11_Bg2 <- data.frame(GWS11_B)
GWS11_Bg2 <- GWS11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_Bg2$player1
player2vector <- GWS11_Bg2$player2
GWS11_Bg3 <- GWS11_Bg2
GWS11_Bg3$p1inp2vec <- is.element(GWS11_Bg3$player1, player2vector)
GWS11_Bg3$p2inp1vec <- is.element(GWS11_Bg3$player2, player1vector)

addPlayer1 <- GWS11_Bg3[ which(GWS11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_Bg3[ which(GWS11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_Bg2 <- rbind(GWS11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
GWS11_Bft <- ftable(GWS11_Bg2$player1, GWS11_Bg2$player2)
GWS11_Bft2 <- as.matrix(GWS11_Bft)
numRows <- nrow(GWS11_Bft2)
numCols <- ncol(GWS11_Bft2)
GWS11_Bft3 <- GWS11_Bft2[c(2:numRows) , c(2:numCols)]
GWS11_BTable <- graph.adjacency(GWS11_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(GWS11_BTable, vertex.label = V(GWS11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
GWS11_B.clusterCoef <- transitivity(GWS11_BTable, type="global") #cluster coefficient
GWS11_B.degreeCent <- centralization.degree(GWS11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_Bftn <- as.network.matrix(GWS11_Bft)
GWS11_B.netDensity <- network.density(GWS11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_B.entropy <- entropy(GWS11_Bft) #entropy

GWS11_B.netMx <- cbind(GWS11_B.netMx, GWS11_B.clusterCoef, GWS11_B.degreeCent$centralization,
                       GWS11_B.netDensity, GWS11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
GWS11_SFg2 <- data.frame(GWS11_SF)
GWS11_SFg2 <- GWS11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_SFg2$player1
player2vector <- GWS11_SFg2$player2
GWS11_SFg3 <- GWS11_SFg2
GWS11_SFg3$p1inp2vec <- is.element(GWS11_SFg3$player1, player2vector)
GWS11_SFg3$p2inp1vec <- is.element(GWS11_SFg3$player2, player1vector)

addPlayer1 <- GWS11_SFg3[ which(GWS11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_SFg3[ which(GWS11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_SFg2 <- rbind(GWS11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
GWS11_SFft <- ftable(GWS11_SFg2$player1, GWS11_SFg2$player2)
GWS11_SFft2 <- as.matrix(GWS11_SFft)
numRows <- nrow(GWS11_SFft2)
numCols <- ncol(GWS11_SFft2)
GWS11_SFft3 <- GWS11_SFft2[c(2:numRows) , c(2:numCols)]
GWS11_SFTable <- graph.adjacency(GWS11_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(GWS11_SFTable, vertex.label = V(GWS11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
GWS11_SF.clusterCoef <- transitivity(GWS11_SFTable, type="global") #cluster coefficient
GWS11_SF.degreeCent <- centralization.degree(GWS11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_SFftn <- as.network.matrix(GWS11_SFft)
GWS11_SF.netDensity <- network.density(GWS11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_SF.entropy <- entropy(GWS11_SFft) #entropy

GWS11_SF.netMx <- cbind(GWS11_SF.netMx, GWS11_SF.clusterCoef, GWS11_SF.degreeCent$centralization,
                        GWS11_SF.netDensity, GWS11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
GWS11_TFg2 <- data.frame(GWS11_TF)
GWS11_TFg2 <- GWS11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_TFg2$player1
player2vector <- GWS11_TFg2$player2
GWS11_TFg3 <- GWS11_TFg2
GWS11_TFg3$p1inp2vec <- is.element(GWS11_TFg3$player1, player2vector)
GWS11_TFg3$p2inp1vec <- is.element(GWS11_TFg3$player2, player1vector)

addPlayer1 <- GWS11_TFg3[ which(GWS11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_TFg3[ which(GWS11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_TFg2 <- rbind(GWS11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
GWS11_TFft <- ftable(GWS11_TFg2$player1, GWS11_TFg2$player2)
GWS11_TFft2 <- as.matrix(GWS11_TFft)
numRows <- nrow(GWS11_TFft2)
numCols <- ncol(GWS11_TFft2)
GWS11_TFft3 <- GWS11_TFft2[c(2:numRows) , c(2:numCols)]
GWS11_TFTable <- graph.adjacency(GWS11_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(GWS11_TFTable, vertex.label = V(GWS11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
GWS11_TF.clusterCoef <- transitivity(GWS11_TFTable, type="global") #cluster coefficient
GWS11_TF.degreeCent <- centralization.degree(GWS11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_TFftn <- as.network.matrix(GWS11_TFft)
GWS11_TF.netDensity <- network.density(GWS11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_TF.entropy <- entropy(GWS11_TFft) #entropy

GWS11_TF.netMx <- cbind(GWS11_TF.netMx, GWS11_TF.clusterCoef, GWS11_TF.degreeCent$centralization,
                        GWS11_TF.netDensity, GWS11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
GWS11_SAMg2 <- data.frame(GWS11_SAM)
GWS11_SAMg2 <- GWS11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_SAMg2$player1
player2vector <- GWS11_SAMg2$player2
GWS11_SAMg3 <- GWS11_SAMg2
GWS11_SAMg3$p1inp2vec <- is.element(GWS11_SAMg3$player1, player2vector)
GWS11_SAMg3$p2inp1vec <- is.element(GWS11_SAMg3$player2, player1vector)

addPlayer1 <- GWS11_SAMg3[ which(GWS11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_SAMg3[ which(GWS11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_SAMg2 <- rbind(GWS11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
GWS11_SAMft <- ftable(GWS11_SAMg2$player1, GWS11_SAMg2$player2)
GWS11_SAMft2 <- as.matrix(GWS11_SAMft)
numRows <- nrow(GWS11_SAMft2)
numCols <- ncol(GWS11_SAMft2)
GWS11_SAMft3 <- GWS11_SAMft2[c(2:numRows) , c(2:numCols)]
GWS11_SAMTable <- graph.adjacency(GWS11_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(GWS11_SAMTable, vertex.label = V(GWS11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
GWS11_SAM.clusterCoef <- transitivity(GWS11_SAMTable, type="global") #cluster coefficient
GWS11_SAM.degreeCent <- centralization.degree(GWS11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_SAMftn <- as.network.matrix(GWS11_SAMft)
GWS11_SAM.netDensity <- network.density(GWS11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_SAM.entropy <- entropy(GWS11_SAMft) #entropy

GWS11_SAM.netMx <- cbind(GWS11_SAM.netMx, GWS11_SAM.clusterCoef, GWS11_SAM.degreeCent$centralization,
                         GWS11_SAM.netDensity, GWS11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
GWS11_TAMg2 <- data.frame(GWS11_TAM)
GWS11_TAMg2 <- GWS11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_TAMg2$player1
player2vector <- GWS11_TAMg2$player2
GWS11_TAMg3 <- GWS11_TAMg2
GWS11_TAMg3$p1inp2vec <- is.element(GWS11_TAMg3$player1, player2vector)
GWS11_TAMg3$p2inp1vec <- is.element(GWS11_TAMg3$player2, player1vector)

addPlayer1 <- GWS11_TAMg3[ which(GWS11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_TAMg3[ which(GWS11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_TAMg2 <- rbind(GWS11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
GWS11_TAMft <- ftable(GWS11_TAMg2$player1, GWS11_TAMg2$player2)
GWS11_TAMft2 <- as.matrix(GWS11_TAMft)
numRows <- nrow(GWS11_TAMft2)
numCols <- ncol(GWS11_TAMft2)
GWS11_TAMft3 <- GWS11_TAMft2[c(2:numRows) , c(2:numCols)]
GWS11_TAMTable <- graph.adjacency(GWS11_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(GWS11_TAMTable, vertex.label = V(GWS11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
GWS11_TAM.clusterCoef <- transitivity(GWS11_TAMTable, type="global") #cluster coefficient
GWS11_TAM.degreeCent <- centralization.degree(GWS11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_TAMftn <- as.network.matrix(GWS11_TAMft)
GWS11_TAM.netDensity <- network.density(GWS11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_TAM.entropy <- entropy(GWS11_TAMft) #entropy

GWS11_TAM.netMx <- cbind(GWS11_TAM.netMx, GWS11_TAM.clusterCoef, GWS11_TAM.degreeCent$centralization,
                         GWS11_TAM.netDensity, GWS11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
GWS11_SDMg2 <- data.frame(GWS11_SDM)
GWS11_SDMg2 <- GWS11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_SDMg2$player1
player2vector <- GWS11_SDMg2$player2
GWS11_SDMg3 <- GWS11_SDMg2
GWS11_SDMg3$p1inp2vec <- is.element(GWS11_SDMg3$player1, player2vector)
GWS11_SDMg3$p2inp1vec <- is.element(GWS11_SDMg3$player2, player1vector)

addPlayer1 <- GWS11_SDMg3[ which(GWS11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_SDMg3[ which(GWS11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_SDMg2 <- rbind(GWS11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
GWS11_SDMft <- ftable(GWS11_SDMg2$player1, GWS11_SDMg2$player2)
GWS11_SDMft2 <- as.matrix(GWS11_SDMft)
numRows <- nrow(GWS11_SDMft2)
numCols <- ncol(GWS11_SDMft2)
GWS11_SDMft3 <- GWS11_SDMft2[c(2:numRows) , c(2:numCols)]
GWS11_SDMTable <- graph.adjacency(GWS11_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(GWS11_SDMTable, vertex.label = V(GWS11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
GWS11_SDM.clusterCoef <- transitivity(GWS11_SDMTable, type="global") #cluster coefficient
GWS11_SDM.degreeCent <- centralization.degree(GWS11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_SDMftn <- as.network.matrix(GWS11_SDMft)
GWS11_SDM.netDensity <- network.density(GWS11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_SDM.entropy <- entropy(GWS11_SDMft) #entropy

GWS11_SDM.netMx <- cbind(GWS11_SDM.netMx, GWS11_SDM.clusterCoef, GWS11_SDM.degreeCent$centralization,
                         GWS11_SDM.netDensity, GWS11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
GWS11_TDMg2 <- data.frame(GWS11_TDM)
GWS11_TDMg2 <- GWS11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_TDMg2$player1
player2vector <- GWS11_TDMg2$player2
GWS11_TDMg3 <- GWS11_TDMg2
GWS11_TDMg3$p1inp2vec <- is.element(GWS11_TDMg3$player1, player2vector)
GWS11_TDMg3$p2inp1vec <- is.element(GWS11_TDMg3$player2, player1vector)

addPlayer1 <- GWS11_TDMg3[ which(GWS11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_TDMg3[ which(GWS11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_TDMg2 <- rbind(GWS11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
GWS11_TDMft <- ftable(GWS11_TDMg2$player1, GWS11_TDMg2$player2)
GWS11_TDMft2 <- as.matrix(GWS11_TDMft)
numRows <- nrow(GWS11_TDMft2)
numCols <- ncol(GWS11_TDMft2)
GWS11_TDMft3 <- GWS11_TDMft2[c(2:numRows) , c(2:numCols)]
GWS11_TDMTable <- graph.adjacency(GWS11_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(GWS11_TDMTable, vertex.label = V(GWS11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
GWS11_TDM.clusterCoef <- transitivity(GWS11_TDMTable, type="global") #cluster coefficient
GWS11_TDM.degreeCent <- centralization.degree(GWS11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_TDMftn <- as.network.matrix(GWS11_TDMft)
GWS11_TDM.netDensity <- network.density(GWS11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_TDM.entropy <- entropy(GWS11_TDMft) #entropy

GWS11_TDM.netMx <- cbind(GWS11_TDM.netMx, GWS11_TDM.clusterCoef, GWS11_TDM.degreeCent$centralization,
                         GWS11_TDM.netDensity, GWS11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************

round = 11
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
GWS11_SDg2 <- data.frame(GWS11_SD)
GWS11_SDg2 <- GWS11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_SDg2$player1
player2vector <- GWS11_SDg2$player2
GWS11_SDg3 <- GWS11_SDg2
GWS11_SDg3$p1inp2vec <- is.element(GWS11_SDg3$player1, player2vector)
GWS11_SDg3$p2inp1vec <- is.element(GWS11_SDg3$player2, player1vector)

addPlayer1 <- GWS11_SDg3[ which(GWS11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_SDg3[ which(GWS11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_SDg2 <- rbind(GWS11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
GWS11_SDft <- ftable(GWS11_SDg2$player1, GWS11_SDg2$player2)
GWS11_SDft2 <- as.matrix(GWS11_SDft)
numRows <- nrow(GWS11_SDft2)
numCols <- ncol(GWS11_SDft2)
GWS11_SDft3 <- GWS11_SDft2[c(2:numRows) , c(2:numCols)]
GWS11_SDTable <- graph.adjacency(GWS11_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(GWS11_SDTable, vertex.label = V(GWS11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
GWS11_SD.clusterCoef <- transitivity(GWS11_SDTable, type="global") #cluster coefficient
GWS11_SD.degreeCent <- centralization.degree(GWS11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_SDftn <- as.network.matrix(GWS11_SDft)
GWS11_SD.netDensity <- network.density(GWS11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_SD.entropy <- entropy(GWS11_SDft) #entropy

GWS11_SD.netMx <- cbind(GWS11_SD.netMx, GWS11_SD.clusterCoef, GWS11_SD.degreeCent$centralization,
                        GWS11_SD.netDensity, GWS11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
GWS11_TDg2 <- data.frame(GWS11_TD)
GWS11_TDg2 <- GWS11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_TDg2$player1
player2vector <- GWS11_TDg2$player2
GWS11_TDg3 <- GWS11_TDg2
GWS11_TDg3$p1inp2vec <- is.element(GWS11_TDg3$player1, player2vector)
GWS11_TDg3$p2inp1vec <- is.element(GWS11_TDg3$player2, player1vector)

addPlayer1 <- GWS11_TDg3[ which(GWS11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_TDg3[ which(GWS11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_TDg2 <- rbind(GWS11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
GWS11_TDft <- ftable(GWS11_TDg2$player1, GWS11_TDg2$player2)
GWS11_TDft2 <- as.matrix(GWS11_TDft)
numRows <- nrow(GWS11_TDft2)
numCols <- ncol(GWS11_TDft2)
GWS11_TDft3 <- GWS11_TDft2[c(2:numRows) , c(2:numCols)]
GWS11_TDTable <- graph.adjacency(GWS11_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(GWS11_TDTable, vertex.label = V(GWS11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
GWS11_TD.clusterCoef <- transitivity(GWS11_TDTable, type="global") #cluster coefficient
GWS11_TD.degreeCent <- centralization.degree(GWS11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_TDftn <- as.network.matrix(GWS11_TDft)
GWS11_TD.netDensity <- network.density(GWS11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_TD.entropy <- entropy(GWS11_TDft) #entropy

GWS11_TD.netMx <- cbind(GWS11_TD.netMx, GWS11_TD.clusterCoef, GWS11_TD.degreeCent$centralization,
                        GWS11_TD.netDensity, GWS11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
GWS11_QTg2 <- data.frame(GWS11_QT)
GWS11_QTg2 <- GWS11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS11_QTg2$player1
player2vector <- GWS11_QTg2$player2
GWS11_QTg3 <- GWS11_QTg2
GWS11_QTg3$p1inp2vec <- is.element(GWS11_QTg3$player1, player2vector)
GWS11_QTg3$p2inp1vec <- is.element(GWS11_QTg3$player2, player1vector)

addPlayer1 <- GWS11_QTg3[ which(GWS11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS11_QTg3[ which(GWS11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS11_QTg2 <- rbind(GWS11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
GWS11_QTft <- ftable(GWS11_QTg2$player1, GWS11_QTg2$player2)
GWS11_QTft2 <- as.matrix(GWS11_QTft)
numRows <- nrow(GWS11_QTft2)
numCols <- ncol(GWS11_QTft2)
GWS11_QTft3 <- GWS11_QTft2[c(2:numRows) , c(2:numCols)]
GWS11_QTTable <- graph.adjacency(GWS11_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(GWS11_QTTable, vertex.label = V(GWS11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
GWS11_QT.clusterCoef <- transitivity(GWS11_QTTable, type="global") #cluster coefficient
GWS11_QT.degreeCent <- centralization.degree(GWS11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS11_QTftn <- as.network.matrix(GWS11_QTft)
GWS11_QT.netDensity <- network.density(GWS11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS11_QT.entropy <- entropy(GWS11_QTft) #entropy

GWS11_QT.netMx <- cbind(GWS11_QT.netMx, GWS11_QT.clusterCoef, GWS11_QT.degreeCent$centralization,
                        GWS11_QT.netDensity, GWS11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS11_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "Goal_F"
MELB11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
MELB11_Gg2 <- data.frame(MELB11_G)
MELB11_Gg2 <- MELB11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_Gg2$player1
player2vector <- MELB11_Gg2$player2
MELB11_Gg3 <- MELB11_Gg2
MELB11_Gg3$p1inp2vec <- is.element(MELB11_Gg3$player1, player2vector)
MELB11_Gg3$p2inp1vec <- is.element(MELB11_Gg3$player2, player1vector)

addPlayer1 <- MELB11_Gg3[ which(MELB11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB11_Gg2 <- rbind(MELB11_Gg2, addPlayer1)

#ROUND 11, Goal graph using weighted edges
MELB11_Gft <- ftable(MELB11_Gg2$player1, MELB11_Gg2$player2)
MELB11_Gft2 <- as.matrix(MELB11_Gft)
numRows <- nrow(MELB11_Gft2)
numCols <- ncol(MELB11_Gft2)
MELB11_Gft3 <- MELB11_Gft2[c(2:numRows) , c(1:numCols)]
MELB11_GTable <- graph.adjacency(MELB11_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(MELB11_GTable, vertex.label = V(MELB11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
MELB11_G.clusterCoef <- transitivity(MELB11_GTable, type="global") #cluster coefficient
MELB11_G.degreeCent <- centralization.degree(MELB11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_Gftn <- as.network.matrix(MELB11_Gft)
MELB11_G.netDensity <- network.density(MELB11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_G.entropy <- entropy(MELB11_Gft) #entropy

MELB11_G.netMx <- cbind(MELB11_G.netMx, MELB11_G.clusterCoef, MELB11_G.degreeCent$centralization,
                        MELB11_G.netDensity, MELB11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************

round = 11
teamName = "MELB"
KIoutcome = "Behind_F"
MELB11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
MELB11_Bg2 <- data.frame(MELB11_B)
MELB11_Bg2 <- MELB11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_Bg2$player1
player2vector <- MELB11_Bg2$player2
MELB11_Bg3 <- MELB11_Bg2
MELB11_Bg3$p1inp2vec <- is.element(MELB11_Bg3$player1, player2vector)
MELB11_Bg3$p2inp1vec <- is.element(MELB11_Bg3$player2, player1vector)

addPlayer1 <- MELB11_Bg3[ which(MELB11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_Bg3[ which(MELB11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_Bg2 <- rbind(MELB11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
MELB11_Bft <- ftable(MELB11_Bg2$player1, MELB11_Bg2$player2)
MELB11_Bft2 <- as.matrix(MELB11_Bft)
numRows <- nrow(MELB11_Bft2)
numCols <- ncol(MELB11_Bft2)
MELB11_Bft3 <- MELB11_Bft2[c(2:numRows) , c(2:numCols)]
MELB11_BTable <- graph.adjacency(MELB11_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(MELB11_BTable, vertex.label = V(MELB11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
MELB11_B.clusterCoef <- transitivity(MELB11_BTable, type="global") #cluster coefficient
MELB11_B.degreeCent <- centralization.degree(MELB11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_Bftn <- as.network.matrix(MELB11_Bft)
MELB11_B.netDensity <- network.density(MELB11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_B.entropy <- entropy(MELB11_Bft) #entropy

MELB11_B.netMx <- cbind(MELB11_B.netMx, MELB11_B.clusterCoef, MELB11_B.degreeCent$centralization,
                        MELB11_B.netDensity, MELB11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
MELB11_SFg2 <- data.frame(MELB11_SF)
MELB11_SFg2 <- MELB11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_SFg2$player1
player2vector <- MELB11_SFg2$player2
MELB11_SFg3 <- MELB11_SFg2
MELB11_SFg3$p1inp2vec <- is.element(MELB11_SFg3$player1, player2vector)
MELB11_SFg3$p2inp1vec <- is.element(MELB11_SFg3$player2, player1vector)

addPlayer1 <- MELB11_SFg3[ which(MELB11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_SFg3[ which(MELB11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_SFg2 <- rbind(MELB11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
MELB11_SFft <- ftable(MELB11_SFg2$player1, MELB11_SFg2$player2)
MELB11_SFft2 <- as.matrix(MELB11_SFft)
numRows <- nrow(MELB11_SFft2)
numCols <- ncol(MELB11_SFft2)
MELB11_SFft3 <- MELB11_SFft2[c(2:numRows) , c(2:numCols)]
MELB11_SFTable <- graph.adjacency(MELB11_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(MELB11_SFTable, vertex.label = V(MELB11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
MELB11_SF.clusterCoef <- transitivity(MELB11_SFTable, type="global") #cluster coefficient
MELB11_SF.degreeCent <- centralization.degree(MELB11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_SFftn <- as.network.matrix(MELB11_SFft)
MELB11_SF.netDensity <- network.density(MELB11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_SF.entropy <- entropy(MELB11_SFft) #entropy

MELB11_SF.netMx <- cbind(MELB11_SF.netMx, MELB11_SF.clusterCoef, MELB11_SF.degreeCent$centralization,
                         MELB11_SF.netDensity, MELB11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************

round = 11
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
MELB11_TFg2 <- data.frame(MELB11_TF)
MELB11_TFg2 <- MELB11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_TFg2$player1
player2vector <- MELB11_TFg2$player2
MELB11_TFg3 <- MELB11_TFg2
MELB11_TFg3$p1inp2vec <- is.element(MELB11_TFg3$player1, player2vector)
MELB11_TFg3$p2inp1vec <- is.element(MELB11_TFg3$player2, player1vector)

addPlayer1 <- MELB11_TFg3[ which(MELB11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_TFg3[ which(MELB11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_TFg2 <- rbind(MELB11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
MELB11_TFft <- ftable(MELB11_TFg2$player1, MELB11_TFg2$player2)
MELB11_TFft2 <- as.matrix(MELB11_TFft)
numRows <- nrow(MELB11_TFft2)
numCols <- ncol(MELB11_TFft2)
MELB11_TFft3 <- MELB11_TFft2[c(2:numRows) , c(2:numCols)]
MELB11_TFTable <- graph.adjacency(MELB11_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(MELB11_TFTable, vertex.label = V(MELB11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
MELB11_TF.clusterCoef <- transitivity(MELB11_TFTable, type="global") #cluster coefficient
MELB11_TF.degreeCent <- centralization.degree(MELB11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_TFftn <- as.network.matrix(MELB11_TFft)
MELB11_TF.netDensity <- network.density(MELB11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_TF.entropy <- entropy(MELB11_TFft) #entropy

MELB11_TF.netMx <- cbind(MELB11_TF.netMx, MELB11_TF.clusterCoef, MELB11_TF.degreeCent$centralization,
                         MELB11_TF.netDensity, MELB11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
MELB11_SAMg2 <- data.frame(MELB11_SAM)
MELB11_SAMg2 <- MELB11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_SAMg2$player1
player2vector <- MELB11_SAMg2$player2
MELB11_SAMg3 <- MELB11_SAMg2
MELB11_SAMg3$p1inp2vec <- is.element(MELB11_SAMg3$player1, player2vector)
MELB11_SAMg3$p2inp1vec <- is.element(MELB11_SAMg3$player2, player1vector)

addPlayer1 <- MELB11_SAMg3[ which(MELB11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_SAMg3[ which(MELB11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_SAMg2 <- rbind(MELB11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
MELB11_SAMft <- ftable(MELB11_SAMg2$player1, MELB11_SAMg2$player2)
MELB11_SAMft2 <- as.matrix(MELB11_SAMft)
numRows <- nrow(MELB11_SAMft2)
numCols <- ncol(MELB11_SAMft2)
MELB11_SAMft3 <- MELB11_SAMft2[c(2:numRows) , c(2:numCols)]
MELB11_SAMTable <- graph.adjacency(MELB11_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(MELB11_SAMTable, vertex.label = V(MELB11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
MELB11_SAM.clusterCoef <- transitivity(MELB11_SAMTable, type="global") #cluster coefficient
MELB11_SAM.degreeCent <- centralization.degree(MELB11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_SAMftn <- as.network.matrix(MELB11_SAMft)
MELB11_SAM.netDensity <- network.density(MELB11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_SAM.entropy <- entropy(MELB11_SAMft) #entropy

MELB11_SAM.netMx <- cbind(MELB11_SAM.netMx, MELB11_SAM.clusterCoef, MELB11_SAM.degreeCent$centralization,
                          MELB11_SAM.netDensity, MELB11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
MELB11_TAMg2 <- data.frame(MELB11_TAM)
MELB11_TAMg2 <- MELB11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_TAMg2$player1
player2vector <- MELB11_TAMg2$player2
MELB11_TAMg3 <- MELB11_TAMg2
MELB11_TAMg3$p1inp2vec <- is.element(MELB11_TAMg3$player1, player2vector)
MELB11_TAMg3$p2inp1vec <- is.element(MELB11_TAMg3$player2, player1vector)

addPlayer1 <- MELB11_TAMg3[ which(MELB11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_TAMg3[ which(MELB11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_TAMg2 <- rbind(MELB11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
MELB11_TAMft <- ftable(MELB11_TAMg2$player1, MELB11_TAMg2$player2)
MELB11_TAMft2 <- as.matrix(MELB11_TAMft)
numRows <- nrow(MELB11_TAMft2)
numCols <- ncol(MELB11_TAMft2)
MELB11_TAMft3 <- MELB11_TAMft2[c(2:numRows) , c(2:numCols)]
MELB11_TAMTable <- graph.adjacency(MELB11_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(MELB11_TAMTable, vertex.label = V(MELB11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
MELB11_TAM.clusterCoef <- transitivity(MELB11_TAMTable, type="global") #cluster coefficient
MELB11_TAM.degreeCent <- centralization.degree(MELB11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_TAMftn <- as.network.matrix(MELB11_TAMft)
MELB11_TAM.netDensity <- network.density(MELB11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_TAM.entropy <- entropy(MELB11_TAMft) #entropy

MELB11_TAM.netMx <- cbind(MELB11_TAM.netMx, MELB11_TAM.clusterCoef, MELB11_TAM.degreeCent$centralization,
                          MELB11_TAM.netDensity, MELB11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
MELB11_SDMg2 <- data.frame(MELB11_SDM)
MELB11_SDMg2 <- MELB11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_SDMg2$player1
player2vector <- MELB11_SDMg2$player2
MELB11_SDMg3 <- MELB11_SDMg2
MELB11_SDMg3$p1inp2vec <- is.element(MELB11_SDMg3$player1, player2vector)
MELB11_SDMg3$p2inp1vec <- is.element(MELB11_SDMg3$player2, player1vector)

addPlayer1 <- MELB11_SDMg3[ which(MELB11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_SDMg3[ which(MELB11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_SDMg2 <- rbind(MELB11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
MELB11_SDMft <- ftable(MELB11_SDMg2$player1, MELB11_SDMg2$player2)
MELB11_SDMft2 <- as.matrix(MELB11_SDMft)
numRows <- nrow(MELB11_SDMft2)
numCols <- ncol(MELB11_SDMft2)
MELB11_SDMft3 <- MELB11_SDMft2[c(2:numRows) , c(2:numCols)]
MELB11_SDMTable <- graph.adjacency(MELB11_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(MELB11_SDMTable, vertex.label = V(MELB11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
MELB11_SDM.clusterCoef <- transitivity(MELB11_SDMTable, type="global") #cluster coefficient
MELB11_SDM.degreeCent <- centralization.degree(MELB11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_SDMftn <- as.network.matrix(MELB11_SDMft)
MELB11_SDM.netDensity <- network.density(MELB11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_SDM.entropy <- entropy(MELB11_SDMft) #entropy

MELB11_SDM.netMx <- cbind(MELB11_SDM.netMx, MELB11_SDM.clusterCoef, MELB11_SDM.degreeCent$centralization,
                          MELB11_SDM.netDensity, MELB11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
MELB11_TDMg2 <- data.frame(MELB11_TDM)
MELB11_TDMg2 <- MELB11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_TDMg2$player1
player2vector <- MELB11_TDMg2$player2
MELB11_TDMg3 <- MELB11_TDMg2
MELB11_TDMg3$p1inp2vec <- is.element(MELB11_TDMg3$player1, player2vector)
MELB11_TDMg3$p2inp1vec <- is.element(MELB11_TDMg3$player2, player1vector)

addPlayer1 <- MELB11_TDMg3[ which(MELB11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_TDMg3[ which(MELB11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_TDMg2 <- rbind(MELB11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
MELB11_TDMft <- ftable(MELB11_TDMg2$player1, MELB11_TDMg2$player2)
MELB11_TDMft2 <- as.matrix(MELB11_TDMft)
numRows <- nrow(MELB11_TDMft2)
numCols <- ncol(MELB11_TDMft2)
MELB11_TDMft3 <- MELB11_TDMft2[c(2:numRows) , c(2:numCols)]
MELB11_TDMTable <- graph.adjacency(MELB11_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(MELB11_TDMTable, vertex.label = V(MELB11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
MELB11_TDM.clusterCoef <- transitivity(MELB11_TDMTable, type="global") #cluster coefficient
MELB11_TDM.degreeCent <- centralization.degree(MELB11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_TDMftn <- as.network.matrix(MELB11_TDMft)
MELB11_TDM.netDensity <- network.density(MELB11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_TDM.entropy <- entropy(MELB11_TDMft) #entropy

MELB11_TDM.netMx <- cbind(MELB11_TDM.netMx, MELB11_TDM.clusterCoef, MELB11_TDM.degreeCent$centralization,
                          MELB11_TDM.netDensity, MELB11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
MELB11_SDg2 <- data.frame(MELB11_SD)
MELB11_SDg2 <- MELB11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_SDg2$player1
player2vector <- MELB11_SDg2$player2
MELB11_SDg3 <- MELB11_SDg2
MELB11_SDg3$p1inp2vec <- is.element(MELB11_SDg3$player1, player2vector)
MELB11_SDg3$p2inp1vec <- is.element(MELB11_SDg3$player2, player1vector)

addPlayer1 <- MELB11_SDg3[ which(MELB11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_SDg3[ which(MELB11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_SDg2 <- rbind(MELB11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
MELB11_SDft <- ftable(MELB11_SDg2$player1, MELB11_SDg2$player2)
MELB11_SDft2 <- as.matrix(MELB11_SDft)
numRows <- nrow(MELB11_SDft2)
numCols <- ncol(MELB11_SDft2)
MELB11_SDft3 <- MELB11_SDft2[c(2:numRows) , c(2:numCols)]
MELB11_SDTable <- graph.adjacency(MELB11_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(MELB11_SDTable, vertex.label = V(MELB11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
MELB11_SD.clusterCoef <- transitivity(MELB11_SDTable, type="global") #cluster coefficient
MELB11_SD.degreeCent <- centralization.degree(MELB11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_SDftn <- as.network.matrix(MELB11_SDft)
MELB11_SD.netDensity <- network.density(MELB11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_SD.entropy <- entropy(MELB11_SDft) #entropy

MELB11_SD.netMx <- cbind(MELB11_SD.netMx, MELB11_SD.clusterCoef, MELB11_SD.degreeCent$centralization,
                         MELB11_SD.netDensity, MELB11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
MELB11_TDg2 <- data.frame(MELB11_TD)
MELB11_TDg2 <- MELB11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_TDg2$player1
player2vector <- MELB11_TDg2$player2
MELB11_TDg3 <- MELB11_TDg2
MELB11_TDg3$p1inp2vec <- is.element(MELB11_TDg3$player1, player2vector)
MELB11_TDg3$p2inp1vec <- is.element(MELB11_TDg3$player2, player1vector)

addPlayer1 <- MELB11_TDg3[ which(MELB11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_TDg3[ which(MELB11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_TDg2 <- rbind(MELB11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
MELB11_TDft <- ftable(MELB11_TDg2$player1, MELB11_TDg2$player2)
MELB11_TDft2 <- as.matrix(MELB11_TDft)
numRows <- nrow(MELB11_TDft2)
numCols <- ncol(MELB11_TDft2)
MELB11_TDft3 <- MELB11_TDft2[c(2:numRows) , c(2:numCols)]
MELB11_TDTable <- graph.adjacency(MELB11_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(MELB11_TDTable, vertex.label = V(MELB11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
MELB11_TD.clusterCoef <- transitivity(MELB11_TDTable, type="global") #cluster coefficient
MELB11_TD.degreeCent <- centralization.degree(MELB11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_TDftn <- as.network.matrix(MELB11_TDft)
MELB11_TD.netDensity <- network.density(MELB11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_TD.entropy <- entropy(MELB11_TDft) #entropy

MELB11_TD.netMx <- cbind(MELB11_TD.netMx, MELB11_TD.clusterCoef, MELB11_TD.degreeCent$centralization,
                         MELB11_TD.netDensity, MELB11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
MELB11_QTg2 <- data.frame(MELB11_QT)
MELB11_QTg2 <- MELB11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB11_QTg2$player1
player2vector <- MELB11_QTg2$player2
MELB11_QTg3 <- MELB11_QTg2
MELB11_QTg3$p1inp2vec <- is.element(MELB11_QTg3$player1, player2vector)
MELB11_QTg3$p2inp1vec <- is.element(MELB11_QTg3$player2, player1vector)

addPlayer1 <- MELB11_QTg3[ which(MELB11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB11_QTg3[ which(MELB11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB11_QTg2 <- rbind(MELB11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
MELB11_QTft <- ftable(MELB11_QTg2$player1, MELB11_QTg2$player2)
MELB11_QTft2 <- as.matrix(MELB11_QTft)
numRows <- nrow(MELB11_QTft2)
numCols <- ncol(MELB11_QTft2)
MELB11_QTft3 <- MELB11_QTft2[c(2:numRows) , c(2:numCols)]
MELB11_QTTable <- graph.adjacency(MELB11_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(MELB11_QTTable, vertex.label = V(MELB11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
MELB11_QT.clusterCoef <- transitivity(MELB11_QTTable, type="global") #cluster coefficient
MELB11_QT.degreeCent <- centralization.degree(MELB11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB11_QTftn <- as.network.matrix(MELB11_QTft)
MELB11_QT.netDensity <- network.density(MELB11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB11_QT.entropy <- entropy(MELB11_QTft) #entropy

MELB11_QT.netMx <- cbind(MELB11_QT.netMx, MELB11_QT.clusterCoef, MELB11_QT.degreeCent$centralization,
                         MELB11_QT.netDensity, MELB11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB11_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************

round = 11
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
NMFC11_Gg2 <- data.frame(NMFC11_G)
NMFC11_Gg2 <- NMFC11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_Gg2$player1
player2vector <- NMFC11_Gg2$player2
NMFC11_Gg3 <- NMFC11_Gg2
NMFC11_Gg3$p1inp2vec <- is.element(NMFC11_Gg3$player1, player2vector)
NMFC11_Gg3$p2inp1vec <- is.element(NMFC11_Gg3$player2, player1vector)

addPlayer1 <- NMFC11_Gg3[ which(NMFC11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC11_Gg3[ which(NMFC11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_Gg2 <- rbind(NMFC11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
NMFC11_Gft <- ftable(NMFC11_Gg2$player1, NMFC11_Gg2$player2)
NMFC11_Gft2 <- as.matrix(NMFC11_Gft)
numRows <- nrow(NMFC11_Gft2)
numCols <- ncol(NMFC11_Gft2)
NMFC11_Gft3 <- NMFC11_Gft2[c(2:numRows) , c(2:numCols)]
NMFC11_GTable <- graph.adjacency(NMFC11_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(NMFC11_GTable, vertex.label = V(NMFC11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
NMFC11_G.clusterCoef <- transitivity(NMFC11_GTable, type="global") #cluster coefficient
NMFC11_G.degreeCent <- centralization.degree(NMFC11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_Gftn <- as.network.matrix(NMFC11_Gft)
NMFC11_G.netDensity <- network.density(NMFC11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_G.entropy <- entropy(NMFC11_Gft) #entropy

NMFC11_G.netMx <- cbind(NMFC11_G.netMx, NMFC11_G.clusterCoef, NMFC11_G.degreeCent$centralization,
                        NMFC11_G.netDensity, NMFC11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
NMFC11_Bg2 <- data.frame(NMFC11_B)
NMFC11_Bg2 <- NMFC11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_Bg2$player1
player2vector <- NMFC11_Bg2$player2
NMFC11_Bg3 <- NMFC11_Bg2
NMFC11_Bg3$p1inp2vec <- is.element(NMFC11_Bg3$player1, player2vector)
NMFC11_Bg3$p2inp1vec <- is.element(NMFC11_Bg3$player2, player1vector)

addPlayer1 <- NMFC11_Bg3[ which(NMFC11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_Bg3[ which(NMFC11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_Bg2 <- rbind(NMFC11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
NMFC11_Bft <- ftable(NMFC11_Bg2$player1, NMFC11_Bg2$player2)
NMFC11_Bft2 <- as.matrix(NMFC11_Bft)
numRows <- nrow(NMFC11_Bft2)
numCols <- ncol(NMFC11_Bft2)
NMFC11_Bft3 <- NMFC11_Bft2[c(2:numRows) , c(2:numCols)]
NMFC11_BTable <- graph.adjacency(NMFC11_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(NMFC11_BTable, vertex.label = V(NMFC11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
NMFC11_B.clusterCoef <- transitivity(NMFC11_BTable, type="global") #cluster coefficient
NMFC11_B.degreeCent <- centralization.degree(NMFC11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_Bftn <- as.network.matrix(NMFC11_Bft)
NMFC11_B.netDensity <- network.density(NMFC11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_B.entropy <- entropy(NMFC11_Bft) #entropy

NMFC11_B.netMx <- cbind(NMFC11_B.netMx, NMFC11_B.clusterCoef, NMFC11_B.degreeCent$centralization,
                        NMFC11_B.netDensity, NMFC11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************

round = 11
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
NMFC11_SFg2 <- data.frame(NMFC11_SF)
NMFC11_SFg2 <- NMFC11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_SFg2$player1
player2vector <- NMFC11_SFg2$player2
NMFC11_SFg3 <- NMFC11_SFg2
NMFC11_SFg3$p1inp2vec <- is.element(NMFC11_SFg3$player1, player2vector)
NMFC11_SFg3$p2inp1vec <- is.element(NMFC11_SFg3$player2, player1vector)

addPlayer1 <- NMFC11_SFg3[ which(NMFC11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_SFg3[ which(NMFC11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_SFg2 <- rbind(NMFC11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
NMFC11_SFft <- ftable(NMFC11_SFg2$player1, NMFC11_SFg2$player2)
NMFC11_SFft2 <- as.matrix(NMFC11_SFft)
numRows <- nrow(NMFC11_SFft2)
numCols <- ncol(NMFC11_SFft2)
NMFC11_SFft3 <- NMFC11_SFft2[c(2:numRows) , c(2:numCols)]
NMFC11_SFTable <- graph.adjacency(NMFC11_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(NMFC11_SFTable, vertex.label = V(NMFC11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
NMFC11_SF.clusterCoef <- transitivity(NMFC11_SFTable, type="global") #cluster coefficient
NMFC11_SF.degreeCent <- centralization.degree(NMFC11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_SFftn <- as.network.matrix(NMFC11_SFft)
NMFC11_SF.netDensity <- network.density(NMFC11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_SF.entropy <- entropy(NMFC11_SFft) #entropy

NMFC11_SF.netMx <- cbind(NMFC11_SF.netMx, NMFC11_SF.clusterCoef, NMFC11_SF.degreeCent$centralization,
                         NMFC11_SF.netDensity, NMFC11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
NMFC11_TFg2 <- data.frame(NMFC11_TF)
NMFC11_TFg2 <- NMFC11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_TFg2$player1
player2vector <- NMFC11_TFg2$player2
NMFC11_TFg3 <- NMFC11_TFg2
NMFC11_TFg3$p1inp2vec <- is.element(NMFC11_TFg3$player1, player2vector)
NMFC11_TFg3$p2inp1vec <- is.element(NMFC11_TFg3$player2, player1vector)

addPlayer1 <- NMFC11_TFg3[ which(NMFC11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_TFg3[ which(NMFC11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_TFg2 <- rbind(NMFC11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
NMFC11_TFft <- ftable(NMFC11_TFg2$player1, NMFC11_TFg2$player2)
NMFC11_TFft2 <- as.matrix(NMFC11_TFft)
numRows <- nrow(NMFC11_TFft2)
numCols <- ncol(NMFC11_TFft2)
NMFC11_TFft3 <- NMFC11_TFft2[c(2:numRows) , c(2:numCols)]
NMFC11_TFTable <- graph.adjacency(NMFC11_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(NMFC11_TFTable, vertex.label = V(NMFC11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
NMFC11_TF.clusterCoef <- transitivity(NMFC11_TFTable, type="global") #cluster coefficient
NMFC11_TF.degreeCent <- centralization.degree(NMFC11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_TFftn <- as.network.matrix(NMFC11_TFft)
NMFC11_TF.netDensity <- network.density(NMFC11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_TF.entropy <- entropy(NMFC11_TFft) #entropy

NMFC11_TF.netMx <- cbind(NMFC11_TF.netMx, NMFC11_TF.clusterCoef, NMFC11_TF.degreeCent$centralization,
                         NMFC11_TF.netDensity, NMFC11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************
#NA

round = 11
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
NMFC11_SAMg2 <- data.frame(NMFC11_SAM)
NMFC11_SAMg2 <- NMFC11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_SAMg2$player1
player2vector <- NMFC11_SAMg2$player2
NMFC11_SAMg3 <- NMFC11_SAMg2
NMFC11_SAMg3$p1inp2vec <- is.element(NMFC11_SAMg3$player1, player2vector)
NMFC11_SAMg3$p2inp1vec <- is.element(NMFC11_SAMg3$player2, player1vector)

addPlayer1 <- NMFC11_SAMg3[ which(NMFC11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_SAMg3[ which(NMFC11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_SAMg2 <- rbind(NMFC11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
NMFC11_SAMft <- ftable(NMFC11_SAMg2$player1, NMFC11_SAMg2$player2)
NMFC11_SAMft2 <- as.matrix(NMFC11_SAMft)
numRows <- nrow(NMFC11_SAMft2)
numCols <- ncol(NMFC11_SAMft2)
NMFC11_SAMft3 <- NMFC11_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC11_SAMTable <- graph.adjacency(NMFC11_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(NMFC11_SAMTable, vertex.label = V(NMFC11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
NMFC11_SAM.clusterCoef <- transitivity(NMFC11_SAMTable, type="global") #cluster coefficient
NMFC11_SAM.degreeCent <- centralization.degree(NMFC11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_SAMftn <- as.network.matrix(NMFC11_SAMft)
NMFC11_SAM.netDensity <- network.density(NMFC11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_SAM.entropy <- entropy(NMFC11_SAMft) #entropy

NMFC11_SAM.netMx <- cbind(NMFC11_SAM.netMx, NMFC11_SAM.clusterCoef, NMFC11_SAM.degreeCent$centralization,
                          NMFC11_SAM.netDensity, NMFC11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
NMFC11_TAMg2 <- data.frame(NMFC11_TAM)
NMFC11_TAMg2 <- NMFC11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_TAMg2$player1
player2vector <- NMFC11_TAMg2$player2
NMFC11_TAMg3 <- NMFC11_TAMg2
NMFC11_TAMg3$p1inp2vec <- is.element(NMFC11_TAMg3$player1, player2vector)
NMFC11_TAMg3$p2inp1vec <- is.element(NMFC11_TAMg3$player2, player1vector)

addPlayer1 <- NMFC11_TAMg3[ which(NMFC11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_TAMg3[ which(NMFC11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_TAMg2 <- rbind(NMFC11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
NMFC11_TAMft <- ftable(NMFC11_TAMg2$player1, NMFC11_TAMg2$player2)
NMFC11_TAMft2 <- as.matrix(NMFC11_TAMft)
numRows <- nrow(NMFC11_TAMft2)
numCols <- ncol(NMFC11_TAMft2)
NMFC11_TAMft3 <- NMFC11_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC11_TAMTable <- graph.adjacency(NMFC11_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(NMFC11_TAMTable, vertex.label = V(NMFC11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
NMFC11_TAM.clusterCoef <- transitivity(NMFC11_TAMTable, type="global") #cluster coefficient
NMFC11_TAM.degreeCent <- centralization.degree(NMFC11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_TAMftn <- as.network.matrix(NMFC11_TAMft)
NMFC11_TAM.netDensity <- network.density(NMFC11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_TAM.entropy <- entropy(NMFC11_TAMft) #entropy

NMFC11_TAM.netMx <- cbind(NMFC11_TAM.netMx, NMFC11_TAM.clusterCoef, NMFC11_TAM.degreeCent$centralization,
                          NMFC11_TAM.netDensity, NMFC11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
NMFC11_SDMg2 <- data.frame(NMFC11_SDM)
NMFC11_SDMg2 <- NMFC11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_SDMg2$player1
player2vector <- NMFC11_SDMg2$player2
NMFC11_SDMg3 <- NMFC11_SDMg2
NMFC11_SDMg3$p1inp2vec <- is.element(NMFC11_SDMg3$player1, player2vector)
NMFC11_SDMg3$p2inp1vec <- is.element(NMFC11_SDMg3$player2, player1vector)

addPlayer1 <- NMFC11_SDMg3[ which(NMFC11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_SDMg3[ which(NMFC11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_SDMg2 <- rbind(NMFC11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
NMFC11_SDMft <- ftable(NMFC11_SDMg2$player1, NMFC11_SDMg2$player2)
NMFC11_SDMft2 <- as.matrix(NMFC11_SDMft)
numRows <- nrow(NMFC11_SDMft2)
numCols <- ncol(NMFC11_SDMft2)
NMFC11_SDMft3 <- NMFC11_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC11_SDMTable <- graph.adjacency(NMFC11_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(NMFC11_SDMTable, vertex.label = V(NMFC11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
NMFC11_SDM.clusterCoef <- transitivity(NMFC11_SDMTable, type="global") #cluster coefficient
NMFC11_SDM.degreeCent <- centralization.degree(NMFC11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_SDMftn <- as.network.matrix(NMFC11_SDMft)
NMFC11_SDM.netDensity <- network.density(NMFC11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_SDM.entropy <- entropy(NMFC11_SDMft) #entropy

NMFC11_SDM.netMx <- cbind(NMFC11_SDM.netMx, NMFC11_SDM.clusterCoef, NMFC11_SDM.degreeCent$centralization,
                          NMFC11_SDM.netDensity, NMFC11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
NMFC11_TDMg2 <- data.frame(NMFC11_TDM)
NMFC11_TDMg2 <- NMFC11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_TDMg2$player1
player2vector <- NMFC11_TDMg2$player2
NMFC11_TDMg3 <- NMFC11_TDMg2
NMFC11_TDMg3$p1inp2vec <- is.element(NMFC11_TDMg3$player1, player2vector)
NMFC11_TDMg3$p2inp1vec <- is.element(NMFC11_TDMg3$player2, player1vector)

addPlayer1 <- NMFC11_TDMg3[ which(NMFC11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_TDMg3[ which(NMFC11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_TDMg2 <- rbind(NMFC11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
NMFC11_TDMft <- ftable(NMFC11_TDMg2$player1, NMFC11_TDMg2$player2)
NMFC11_TDMft2 <- as.matrix(NMFC11_TDMft)
numRows <- nrow(NMFC11_TDMft2)
numCols <- ncol(NMFC11_TDMft2)
NMFC11_TDMft3 <- NMFC11_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC11_TDMTable <- graph.adjacency(NMFC11_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(NMFC11_TDMTable, vertex.label = V(NMFC11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
NMFC11_TDM.clusterCoef <- transitivity(NMFC11_TDMTable, type="global") #cluster coefficient
NMFC11_TDM.degreeCent <- centralization.degree(NMFC11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_TDMftn <- as.network.matrix(NMFC11_TDMft)
NMFC11_TDM.netDensity <- network.density(NMFC11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_TDM.entropy <- entropy(NMFC11_TDMft) #entropy

NMFC11_TDM.netMx <- cbind(NMFC11_TDM.netMx, NMFC11_TDM.clusterCoef, NMFC11_TDM.degreeCent$centralization,
                          NMFC11_TDM.netDensity, NMFC11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
NMFC11_SDg2 <- data.frame(NMFC11_SD)
NMFC11_SDg2 <- NMFC11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_SDg2$player1
player2vector <- NMFC11_SDg2$player2
NMFC11_SDg3 <- NMFC11_SDg2
NMFC11_SDg3$p1inp2vec <- is.element(NMFC11_SDg3$player1, player2vector)
NMFC11_SDg3$p2inp1vec <- is.element(NMFC11_SDg3$player2, player1vector)

addPlayer1 <- NMFC11_SDg3[ which(NMFC11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_SDg3[ which(NMFC11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_SDg2 <- rbind(NMFC11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
NMFC11_SDft <- ftable(NMFC11_SDg2$player1, NMFC11_SDg2$player2)
NMFC11_SDft2 <- as.matrix(NMFC11_SDft)
numRows <- nrow(NMFC11_SDft2)
numCols <- ncol(NMFC11_SDft2)
NMFC11_SDft3 <- NMFC11_SDft2[c(2:numRows) , c(2:numCols)]
NMFC11_SDTable <- graph.adjacency(NMFC11_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(NMFC11_SDTable, vertex.label = V(NMFC11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
NMFC11_SD.clusterCoef <- transitivity(NMFC11_SDTable, type="global") #cluster coefficient
NMFC11_SD.degreeCent <- centralization.degree(NMFC11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_SDftn <- as.network.matrix(NMFC11_SDft)
NMFC11_SD.netDensity <- network.density(NMFC11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_SD.entropy <- entropy(NMFC11_SDft) #entropy

NMFC11_SD.netMx <- cbind(NMFC11_SD.netMx, NMFC11_SD.clusterCoef, NMFC11_SD.degreeCent$centralization,
                         NMFC11_SD.netDensity, NMFC11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
NMFC11_TDg2 <- data.frame(NMFC11_TD)
NMFC11_TDg2 <- NMFC11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_TDg2$player1
player2vector <- NMFC11_TDg2$player2
NMFC11_TDg3 <- NMFC11_TDg2
NMFC11_TDg3$p1inp2vec <- is.element(NMFC11_TDg3$player1, player2vector)
NMFC11_TDg3$p2inp1vec <- is.element(NMFC11_TDg3$player2, player1vector)

addPlayer1 <- NMFC11_TDg3[ which(NMFC11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_TDg3[ which(NMFC11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_TDg2 <- rbind(NMFC11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
NMFC11_TDft <- ftable(NMFC11_TDg2$player1, NMFC11_TDg2$player2)
NMFC11_TDft2 <- as.matrix(NMFC11_TDft)
numRows <- nrow(NMFC11_TDft2)
numCols <- ncol(NMFC11_TDft2)
NMFC11_TDft3 <- NMFC11_TDft2[c(2:numRows) , c(2:numCols)]
NMFC11_TDTable <- graph.adjacency(NMFC11_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(NMFC11_TDTable, vertex.label = V(NMFC11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
NMFC11_TD.clusterCoef <- transitivity(NMFC11_TDTable, type="global") #cluster coefficient
NMFC11_TD.degreeCent <- centralization.degree(NMFC11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_TDftn <- as.network.matrix(NMFC11_TDft)
NMFC11_TD.netDensity <- network.density(NMFC11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_TD.entropy <- entropy(NMFC11_TDft) #entropy

NMFC11_TD.netMx <- cbind(NMFC11_TD.netMx, NMFC11_TD.clusterCoef, NMFC11_TD.degreeCent$centralization,
                         NMFC11_TD.netDensity, NMFC11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
NMFC11_QTg2 <- data.frame(NMFC11_QT)
NMFC11_QTg2 <- NMFC11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC11_QTg2$player1
player2vector <- NMFC11_QTg2$player2
NMFC11_QTg3 <- NMFC11_QTg2
NMFC11_QTg3$p1inp2vec <- is.element(NMFC11_QTg3$player1, player2vector)
NMFC11_QTg3$p2inp1vec <- is.element(NMFC11_QTg3$player2, player1vector)

addPlayer1 <- NMFC11_QTg3[ which(NMFC11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC11_QTg3[ which(NMFC11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC11_QTg2 <- rbind(NMFC11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
NMFC11_QTft <- ftable(NMFC11_QTg2$player1, NMFC11_QTg2$player2)
NMFC11_QTft2 <- as.matrix(NMFC11_QTft)
numRows <- nrow(NMFC11_QTft2)
numCols <- ncol(NMFC11_QTft2)
NMFC11_QTft3 <- NMFC11_QTft2[c(2:numRows) , c(2:numCols)]
NMFC11_QTTable <- graph.adjacency(NMFC11_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(NMFC11_QTTable, vertex.label = V(NMFC11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
NMFC11_QT.clusterCoef <- transitivity(NMFC11_QTTable, type="global") #cluster coefficient
NMFC11_QT.degreeCent <- centralization.degree(NMFC11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC11_QTftn <- as.network.matrix(NMFC11_QTft)
NMFC11_QT.netDensity <- network.density(NMFC11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC11_QT.entropy <- entropy(NMFC11_QTft) #entropy

NMFC11_QT.netMx <- cbind(NMFC11_QT.netMx, NMFC11_QT.clusterCoef, NMFC11_QT.degreeCent$centralization,
                         NMFC11_QT.netDensity, NMFC11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC11_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Goal_F"
PORT11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
PORT11_Gg2 <- data.frame(PORT11_G)
PORT11_Gg2 <- PORT11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_Gg2$player1
player2vector <- PORT11_Gg2$player2
PORT11_Gg3 <- PORT11_Gg2
PORT11_Gg3$p1inp2vec <- is.element(PORT11_Gg3$player1, player2vector)
PORT11_Gg3$p2inp1vec <- is.element(PORT11_Gg3$player2, player1vector)

addPlayer1 <- PORT11_Gg3[ which(PORT11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT11_Gg2 <- rbind(PORT11_Gg2, addPlayer1)

#ROUND 11, Goal graph using weighted edges
PORT11_Gft <- ftable(PORT11_Gg2$player1, PORT11_Gg2$player2)
PORT11_Gft2 <- as.matrix(PORT11_Gft)
numRows <- nrow(PORT11_Gft2)
numCols <- ncol(PORT11_Gft2)
PORT11_Gft3 <- PORT11_Gft2[c(2:numRows) , c(1:numCols)]
PORT11_GTable <- graph.adjacency(PORT11_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(PORT11_GTable, vertex.label = V(PORT11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
PORT11_G.clusterCoef <- transitivity(PORT11_GTable, type="global") #cluster coefficient
PORT11_G.degreeCent <- centralization.degree(PORT11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_Gftn <- as.network.matrix(PORT11_Gft)
PORT11_G.netDensity <- network.density(PORT11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_G.entropy <- entropy(PORT11_Gft) #entropy

PORT11_G.netMx <- cbind(PORT11_G.netMx, PORT11_G.clusterCoef, PORT11_G.degreeCent$centralization,
                        PORT11_G.netDensity, PORT11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Behind_F"
PORT11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
PORT11_Bg2 <- data.frame(PORT11_B)
PORT11_Bg2 <- PORT11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_Bg2$player1
player2vector <- PORT11_Bg2$player2
PORT11_Bg3 <- PORT11_Bg2
PORT11_Bg3$p1inp2vec <- is.element(PORT11_Bg3$player1, player2vector)
PORT11_Bg3$p2inp1vec <- is.element(PORT11_Bg3$player2, player1vector)

addPlayer1 <- PORT11_Bg3[ which(PORT11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_Bg3[ which(PORT11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_Bg2 <- rbind(PORT11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
PORT11_Bft <- ftable(PORT11_Bg2$player1, PORT11_Bg2$player2)
PORT11_Bft2 <- as.matrix(PORT11_Bft)
numRows <- nrow(PORT11_Bft2)
numCols <- ncol(PORT11_Bft2)
PORT11_Bft3 <- PORT11_Bft2[c(2:numRows) , c(2:numCols)]
PORT11_BTable <- graph.adjacency(PORT11_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(PORT11_BTable, vertex.label = V(PORT11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
PORT11_B.clusterCoef <- transitivity(PORT11_BTable, type="global") #cluster coefficient
PORT11_B.degreeCent <- centralization.degree(PORT11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_Bftn <- as.network.matrix(PORT11_Bft)
PORT11_B.netDensity <- network.density(PORT11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_B.entropy <- entropy(PORT11_Bft) #entropy

PORT11_B.netMx <- cbind(PORT11_B.netMx, PORT11_B.clusterCoef, PORT11_B.degreeCent$centralization,
                        PORT11_B.netDensity, PORT11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
PORT11_SFg2 <- data.frame(PORT11_SF)
PORT11_SFg2 <- PORT11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_SFg2$player1
player2vector <- PORT11_SFg2$player2
PORT11_SFg3 <- PORT11_SFg2
PORT11_SFg3$p1inp2vec <- is.element(PORT11_SFg3$player1, player2vector)
PORT11_SFg3$p2inp1vec <- is.element(PORT11_SFg3$player2, player1vector)

addPlayer1 <- PORT11_SFg3[ which(PORT11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_SFg3[ which(PORT11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_SFg2 <- rbind(PORT11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
PORT11_SFft <- ftable(PORT11_SFg2$player1, PORT11_SFg2$player2)
PORT11_SFft2 <- as.matrix(PORT11_SFft)
numRows <- nrow(PORT11_SFft2)
numCols <- ncol(PORT11_SFft2)
PORT11_SFft3 <- PORT11_SFft2[c(2:numRows) , c(2:numCols)]
PORT11_SFTable <- graph.adjacency(PORT11_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(PORT11_SFTable, vertex.label = V(PORT11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
PORT11_SF.clusterCoef <- transitivity(PORT11_SFTable, type="global") #cluster coefficient
PORT11_SF.degreeCent <- centralization.degree(PORT11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_SFftn <- as.network.matrix(PORT11_SFft)
PORT11_SF.netDensity <- network.density(PORT11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_SF.entropy <- entropy(PORT11_SFft) #entropy

PORT11_SF.netMx <- cbind(PORT11_SF.netMx, PORT11_SF.clusterCoef, PORT11_SF.degreeCent$centralization,
                         PORT11_SF.netDensity, PORT11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
PORT11_TFg2 <- data.frame(PORT11_TF)
PORT11_TFg2 <- PORT11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_TFg2$player1
player2vector <- PORT11_TFg2$player2
PORT11_TFg3 <- PORT11_TFg2
PORT11_TFg3$p1inp2vec <- is.element(PORT11_TFg3$player1, player2vector)
PORT11_TFg3$p2inp1vec <- is.element(PORT11_TFg3$player2, player1vector)

addPlayer1 <- PORT11_TFg3[ which(PORT11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT11_TFg2 <- rbind(PORT11_TFg2, addPlayer1)

#ROUND 11, FWD Turnover graph using weighted edges
PORT11_TFft <- ftable(PORT11_TFg2$player1, PORT11_TFg2$player2)
PORT11_TFft2 <- as.matrix(PORT11_TFft)
numRows <- nrow(PORT11_TFft2)
numCols <- ncol(PORT11_TFft2)
PORT11_TFft3 <- PORT11_TFft2[c(2:numRows) , c(1:numCols)]
PORT11_TFTable <- graph.adjacency(PORT11_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(PORT11_TFTable, vertex.label = V(PORT11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
PORT11_TF.clusterCoef <- transitivity(PORT11_TFTable, type="global") #cluster coefficient
PORT11_TF.degreeCent <- centralization.degree(PORT11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_TFftn <- as.network.matrix(PORT11_TFft)
PORT11_TF.netDensity <- network.density(PORT11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_TF.entropy <- entropy(PORT11_TFft) #entropy

PORT11_TF.netMx <- cbind(PORT11_TF.netMx, PORT11_TF.clusterCoef, PORT11_TF.degreeCent$centralization,
                         PORT11_TF.netDensity, PORT11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
PORT11_SAMg2 <- data.frame(PORT11_SAM)
PORT11_SAMg2 <- PORT11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_SAMg2$player1
player2vector <- PORT11_SAMg2$player2
PORT11_SAMg3 <- PORT11_SAMg2
PORT11_SAMg3$p1inp2vec <- is.element(PORT11_SAMg3$player1, player2vector)
PORT11_SAMg3$p2inp1vec <- is.element(PORT11_SAMg3$player2, player1vector)

addPlayer1 <- PORT11_SAMg3[ which(PORT11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_SAMg3[ which(PORT11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_SAMg2 <- rbind(PORT11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
PORT11_SAMft <- ftable(PORT11_SAMg2$player1, PORT11_SAMg2$player2)
PORT11_SAMft2 <- as.matrix(PORT11_SAMft)
numRows <- nrow(PORT11_SAMft2)
numCols <- ncol(PORT11_SAMft2)
PORT11_SAMft3 <- PORT11_SAMft2[c(2:numRows) , c(2:numCols)]
PORT11_SAMTable <- graph.adjacency(PORT11_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(PORT11_SAMTable, vertex.label = V(PORT11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
PORT11_SAM.clusterCoef <- transitivity(PORT11_SAMTable, type="global") #cluster coefficient
PORT11_SAM.degreeCent <- centralization.degree(PORT11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_SAMftn <- as.network.matrix(PORT11_SAMft)
PORT11_SAM.netDensity <- network.density(PORT11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_SAM.entropy <- entropy(PORT11_SAMft) #entropy

PORT11_SAM.netMx <- cbind(PORT11_SAM.netMx, PORT11_SAM.clusterCoef, PORT11_SAM.degreeCent$centralization,
                          PORT11_SAM.netDensity, PORT11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
PORT11_TAMg2 <- data.frame(PORT11_TAM)
PORT11_TAMg2 <- PORT11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_TAMg2$player1
player2vector <- PORT11_TAMg2$player2
PORT11_TAMg3 <- PORT11_TAMg2
PORT11_TAMg3$p1inp2vec <- is.element(PORT11_TAMg3$player1, player2vector)
PORT11_TAMg3$p2inp1vec <- is.element(PORT11_TAMg3$player2, player1vector)

addPlayer1 <- PORT11_TAMg3[ which(PORT11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT11_TAMg2 <- rbind(PORT11_TAMg2, addPlayer1)

#ROUND 11, AM Turnover graph using weighted edges
PORT11_TAMft <- ftable(PORT11_TAMg2$player1, PORT11_TAMg2$player2)
PORT11_TAMft2 <- as.matrix(PORT11_TAMft)
numRows <- nrow(PORT11_TAMft2)
numCols <- ncol(PORT11_TAMft2)
PORT11_TAMft3 <- PORT11_TAMft2[c(2:numRows) , c(1:numCols)]
PORT11_TAMTable <- graph.adjacency(PORT11_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(PORT11_TAMTable, vertex.label = V(PORT11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
PORT11_TAM.clusterCoef <- transitivity(PORT11_TAMTable, type="global") #cluster coefficient
PORT11_TAM.degreeCent <- centralization.degree(PORT11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_TAMftn <- as.network.matrix(PORT11_TAMft)
PORT11_TAM.netDensity <- network.density(PORT11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_TAM.entropy <- entropy(PORT11_TAMft) #entropy

PORT11_TAM.netMx <- cbind(PORT11_TAM.netMx, PORT11_TAM.clusterCoef, PORT11_TAM.degreeCent$centralization,
                          PORT11_TAM.netDensity, PORT11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
PORT11_SDMg2 <- data.frame(PORT11_SDM)
PORT11_SDMg2 <- PORT11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_SDMg2$player1
player2vector <- PORT11_SDMg2$player2
PORT11_SDMg3 <- PORT11_SDMg2
PORT11_SDMg3$p1inp2vec <- is.element(PORT11_SDMg3$player1, player2vector)
PORT11_SDMg3$p2inp1vec <- is.element(PORT11_SDMg3$player2, player1vector)

addPlayer1 <- PORT11_SDMg3[ which(PORT11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_SDMg3[ which(PORT11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_SDMg2 <- rbind(PORT11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
PORT11_SDMft <- ftable(PORT11_SDMg2$player1, PORT11_SDMg2$player2)
PORT11_SDMft2 <- as.matrix(PORT11_SDMft)
numRows <- nrow(PORT11_SDMft2)
numCols <- ncol(PORT11_SDMft2)
PORT11_SDMft3 <- PORT11_SDMft2[c(2:numRows) , c(2:numCols)]
PORT11_SDMTable <- graph.adjacency(PORT11_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(PORT11_SDMTable, vertex.label = V(PORT11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
PORT11_SDM.clusterCoef <- transitivity(PORT11_SDMTable, type="global") #cluster coefficient
PORT11_SDM.degreeCent <- centralization.degree(PORT11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_SDMftn <- as.network.matrix(PORT11_SDMft)
PORT11_SDM.netDensity <- network.density(PORT11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_SDM.entropy <- entropy(PORT11_SDMft) #entropy

PORT11_SDM.netMx <- cbind(PORT11_SDM.netMx, PORT11_SDM.clusterCoef, PORT11_SDM.degreeCent$centralization,
                          PORT11_SDM.netDensity, PORT11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
PORT11_TDMg2 <- data.frame(PORT11_TDM)
PORT11_TDMg2 <- PORT11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_TDMg2$player1
player2vector <- PORT11_TDMg2$player2
PORT11_TDMg3 <- PORT11_TDMg2
PORT11_TDMg3$p1inp2vec <- is.element(PORT11_TDMg3$player1, player2vector)
PORT11_TDMg3$p2inp1vec <- is.element(PORT11_TDMg3$player2, player1vector)

addPlayer1 <- PORT11_TDMg3[ which(PORT11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_TDMg3[ which(PORT11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_TDMg2 <- rbind(PORT11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
PORT11_TDMft <- ftable(PORT11_TDMg2$player1, PORT11_TDMg2$player2)
PORT11_TDMft2 <- as.matrix(PORT11_TDMft)
numRows <- nrow(PORT11_TDMft2)
numCols <- ncol(PORT11_TDMft2)
PORT11_TDMft3 <- PORT11_TDMft2[c(2:numRows) , c(2:numCols)]
PORT11_TDMTable <- graph.adjacency(PORT11_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(PORT11_TDMTable, vertex.label = V(PORT11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
PORT11_TDM.clusterCoef <- transitivity(PORT11_TDMTable, type="global") #cluster coefficient
PORT11_TDM.degreeCent <- centralization.degree(PORT11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_TDMftn <- as.network.matrix(PORT11_TDMft)
PORT11_TDM.netDensity <- network.density(PORT11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_TDM.entropy <- entropy(PORT11_TDMft) #entropy

PORT11_TDM.netMx <- cbind(PORT11_TDM.netMx, PORT11_TDM.clusterCoef, PORT11_TDM.degreeCent$centralization,
                          PORT11_TDM.netDensity, PORT11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
PORT11_SDg2 <- data.frame(PORT11_SD)
PORT11_SDg2 <- PORT11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_SDg2$player1
player2vector <- PORT11_SDg2$player2
PORT11_SDg3 <- PORT11_SDg2
PORT11_SDg3$p1inp2vec <- is.element(PORT11_SDg3$player1, player2vector)
PORT11_SDg3$p2inp1vec <- is.element(PORT11_SDg3$player2, player1vector)

addPlayer1 <- PORT11_SDg3[ which(PORT11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_SDg3[ which(PORT11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_SDg2 <- rbind(PORT11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
PORT11_SDft <- ftable(PORT11_SDg2$player1, PORT11_SDg2$player2)
PORT11_SDft2 <- as.matrix(PORT11_SDft)
numRows <- nrow(PORT11_SDft2)
numCols <- ncol(PORT11_SDft2)
PORT11_SDft3 <- PORT11_SDft2[c(2:numRows) , c(2:numCols)]
PORT11_SDTable <- graph.adjacency(PORT11_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(PORT11_SDTable, vertex.label = V(PORT11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
PORT11_SD.clusterCoef <- transitivity(PORT11_SDTable, type="global") #cluster coefficient
PORT11_SD.degreeCent <- centralization.degree(PORT11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_SDftn <- as.network.matrix(PORT11_SDft)
PORT11_SD.netDensity <- network.density(PORT11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_SD.entropy <- entropy(PORT11_SDft) #entropy

PORT11_SD.netMx <- cbind(PORT11_SD.netMx, PORT11_SD.clusterCoef, PORT11_SD.degreeCent$centralization,
                         PORT11_SD.netDensity, PORT11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
PORT11_TDg2 <- data.frame(PORT11_TD)
PORT11_TDg2 <- PORT11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_TDg2$player1
player2vector <- PORT11_TDg2$player2
PORT11_TDg3 <- PORT11_TDg2
PORT11_TDg3$p1inp2vec <- is.element(PORT11_TDg3$player1, player2vector)
PORT11_TDg3$p2inp1vec <- is.element(PORT11_TDg3$player2, player1vector)

addPlayer1 <- PORT11_TDg3[ which(PORT11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_TDg3[ which(PORT11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_TDg2 <- rbind(PORT11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
PORT11_TDft <- ftable(PORT11_TDg2$player1, PORT11_TDg2$player2)
PORT11_TDft2 <- as.matrix(PORT11_TDft)
numRows <- nrow(PORT11_TDft2)
numCols <- ncol(PORT11_TDft2)
PORT11_TDft3 <- PORT11_TDft2[c(2:numRows) , c(2:numCols)]
PORT11_TDTable <- graph.adjacency(PORT11_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(PORT11_TDTable, vertex.label = V(PORT11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
PORT11_TD.clusterCoef <- transitivity(PORT11_TDTable, type="global") #cluster coefficient
PORT11_TD.degreeCent <- centralization.degree(PORT11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_TDftn <- as.network.matrix(PORT11_TDft)
PORT11_TD.netDensity <- network.density(PORT11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_TD.entropy <- entropy(PORT11_TDft) #entropy

PORT11_TD.netMx <- cbind(PORT11_TD.netMx, PORT11_TD.clusterCoef, PORT11_TD.degreeCent$centralization,
                         PORT11_TD.netDensity, PORT11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
PORT11_QTg2 <- data.frame(PORT11_QT)
PORT11_QTg2 <- PORT11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT11_QTg2$player1
player2vector <- PORT11_QTg2$player2
PORT11_QTg3 <- PORT11_QTg2
PORT11_QTg3$p1inp2vec <- is.element(PORT11_QTg3$player1, player2vector)
PORT11_QTg3$p2inp1vec <- is.element(PORT11_QTg3$player2, player1vector)

addPlayer1 <- PORT11_QTg3[ which(PORT11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT11_QTg3[ which(PORT11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT11_QTg2 <- rbind(PORT11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
PORT11_QTft <- ftable(PORT11_QTg2$player1, PORT11_QTg2$player2)
PORT11_QTft2 <- as.matrix(PORT11_QTft)
numRows <- nrow(PORT11_QTft2)
numCols <- ncol(PORT11_QTft2)
PORT11_QTft3 <- PORT11_QTft2[c(2:numRows) , c(2:numCols)]
PORT11_QTTable <- graph.adjacency(PORT11_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(PORT11_QTTable, vertex.label = V(PORT11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
PORT11_QT.clusterCoef <- transitivity(PORT11_QTTable, type="global") #cluster coefficient
PORT11_QT.degreeCent <- centralization.degree(PORT11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT11_QTftn <- as.network.matrix(PORT11_QTft)
PORT11_QT.netDensity <- network.density(PORT11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT11_QT.entropy <- entropy(PORT11_QTft) #entropy

PORT11_QT.netMx <- cbind(PORT11_QT.netMx, PORT11_QT.clusterCoef, PORT11_QT.degreeCent$centralization,
                         PORT11_QT.netDensity, PORT11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT11_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "Goal_F"
STK11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
STK11_Gg2 <- data.frame(STK11_G)
STK11_Gg2 <- STK11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_Gg2$player1
player2vector <- STK11_Gg2$player2
STK11_Gg3 <- STK11_Gg2
STK11_Gg3$p1inp2vec <- is.element(STK11_Gg3$player1, player2vector)
STK11_Gg3$p2inp1vec <- is.element(STK11_Gg3$player2, player1vector)

addPlayer1 <- STK11_Gg3[ which(STK11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_Gg3[ which(STK11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_Gg2 <- rbind(STK11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
STK11_Gft <- ftable(STK11_Gg2$player1, STK11_Gg2$player2)
STK11_Gft2 <- as.matrix(STK11_Gft)
numRows <- nrow(STK11_Gft2)
numCols <- ncol(STK11_Gft2)
STK11_Gft3 <- STK11_Gft2[c(2:numRows) , c(2:numCols)]
STK11_GTable <- graph.adjacency(STK11_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(STK11_GTable, vertex.label = V(STK11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
STK11_G.clusterCoef <- transitivity(STK11_GTable, type="global") #cluster coefficient
STK11_G.degreeCent <- centralization.degree(STK11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_Gftn <- as.network.matrix(STK11_Gft)
STK11_G.netDensity <- network.density(STK11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_G.entropy <- entropy(STK11_Gft) #entropy

STK11_G.netMx <- cbind(STK11_G.netMx, STK11_G.clusterCoef, STK11_G.degreeCent$centralization,
                       STK11_G.netDensity, STK11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "Behind_F"
STK11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
STK11_Bg2 <- data.frame(STK11_B)
STK11_Bg2 <- STK11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_Bg2$player1
player2vector <- STK11_Bg2$player2
STK11_Bg3 <- STK11_Bg2
STK11_Bg3$p1inp2vec <- is.element(STK11_Bg3$player1, player2vector)
STK11_Bg3$p2inp1vec <- is.element(STK11_Bg3$player2, player1vector)

addPlayer1 <- STK11_Bg3[ which(STK11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_Bg3[ which(STK11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_Bg2 <- rbind(STK11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
STK11_Bft <- ftable(STK11_Bg2$player1, STK11_Bg2$player2)
STK11_Bft2 <- as.matrix(STK11_Bft)
numRows <- nrow(STK11_Bft2)
numCols <- ncol(STK11_Bft2)
STK11_Bft3 <- STK11_Bft2[c(2:numRows) , c(2:numCols)]
STK11_BTable <- graph.adjacency(STK11_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(STK11_BTable, vertex.label = V(STK11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
STK11_B.clusterCoef <- transitivity(STK11_BTable, type="global") #cluster coefficient
STK11_B.degreeCent <- centralization.degree(STK11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_Bftn <- as.network.matrix(STK11_Bft)
STK11_B.netDensity <- network.density(STK11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_B.entropy <- entropy(STK11_Bft) #entropy

STK11_B.netMx <- cbind(STK11_B.netMx, STK11_B.clusterCoef, STK11_B.degreeCent$centralization,
                       STK11_B.netDensity, STK11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "Stoppage_F"
STK11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
STK11_SFg2 <- data.frame(STK11_SF)
STK11_SFg2 <- STK11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_SFg2$player1
player2vector <- STK11_SFg2$player2
STK11_SFg3 <- STK11_SFg2
STK11_SFg3$p1inp2vec <- is.element(STK11_SFg3$player1, player2vector)
STK11_SFg3$p2inp1vec <- is.element(STK11_SFg3$player2, player1vector)

addPlayer1 <- STK11_SFg3[ which(STK11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_SFg3[ which(STK11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_SFg2 <- rbind(STK11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
STK11_SFft <- ftable(STK11_SFg2$player1, STK11_SFg2$player2)
STK11_SFft2 <- as.matrix(STK11_SFft)
numRows <- nrow(STK11_SFft2)
numCols <- ncol(STK11_SFft2)
STK11_SFft3 <- STK11_SFft2[c(2:numRows) , c(2:numCols)]
STK11_SFTable <- graph.adjacency(STK11_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(STK11_SFTable, vertex.label = V(STK11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
STK11_SF.clusterCoef <- transitivity(STK11_SFTable, type="global") #cluster coefficient
STK11_SF.degreeCent <- centralization.degree(STK11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_SFftn <- as.network.matrix(STK11_SFft)
STK11_SF.netDensity <- network.density(STK11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_SF.entropy <- entropy(STK11_SFft) #entropy

STK11_SF.netMx <- cbind(STK11_SF.netMx, STK11_SF.clusterCoef, STK11_SF.degreeCent$centralization,
                        STK11_SF.netDensity, STK11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************

round = 11
teamName = "STK"
KIoutcome = "Turnover_F"
STK11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
STK11_TFg2 <- data.frame(STK11_TF)
STK11_TFg2 <- STK11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_TFg2$player1
player2vector <- STK11_TFg2$player2
STK11_TFg3 <- STK11_TFg2
STK11_TFg3$p1inp2vec <- is.element(STK11_TFg3$player1, player2vector)
STK11_TFg3$p2inp1vec <- is.element(STK11_TFg3$player2, player1vector)

addPlayer1 <- STK11_TFg3[ which(STK11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_TFg3[ which(STK11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_TFg2 <- rbind(STK11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
STK11_TFft <- ftable(STK11_TFg2$player1, STK11_TFg2$player2)
STK11_TFft2 <- as.matrix(STK11_TFft)
numRows <- nrow(STK11_TFft2)
numCols <- ncol(STK11_TFft2)
STK11_TFft3 <- STK11_TFft2[c(2:numRows) , c(2:numCols)]
STK11_TFTable <- graph.adjacency(STK11_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(STK11_TFTable, vertex.label = V(STK11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
STK11_TF.clusterCoef <- transitivity(STK11_TFTable, type="global") #cluster coefficient
STK11_TF.degreeCent <- centralization.degree(STK11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_TFftn <- as.network.matrix(STK11_TFft)
STK11_TF.netDensity <- network.density(STK11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_TF.entropy <- entropy(STK11_TFft) #entropy

STK11_TF.netMx <- cbind(STK11_TF.netMx, STK11_TF.clusterCoef, STK11_TF.degreeCent$centralization,
                        STK11_TF.netDensity, STK11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
STK11_SAMg2 <- data.frame(STK11_SAM)
STK11_SAMg2 <- STK11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_SAMg2$player1
player2vector <- STK11_SAMg2$player2
STK11_SAMg3 <- STK11_SAMg2
STK11_SAMg3$p1inp2vec <- is.element(STK11_SAMg3$player1, player2vector)
STK11_SAMg3$p2inp1vec <- is.element(STK11_SAMg3$player2, player1vector)

addPlayer1 <- STK11_SAMg3[ which(STK11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_SAMg3[ which(STK11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_SAMg2 <- rbind(STK11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
STK11_SAMft <- ftable(STK11_SAMg2$player1, STK11_SAMg2$player2)
STK11_SAMft2 <- as.matrix(STK11_SAMft)
numRows <- nrow(STK11_SAMft2)
numCols <- ncol(STK11_SAMft2)
STK11_SAMft3 <- STK11_SAMft2[c(2:numRows) , c(2:numCols)]
STK11_SAMTable <- graph.adjacency(STK11_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(STK11_SAMTable, vertex.label = V(STK11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
STK11_SAM.clusterCoef <- transitivity(STK11_SAMTable, type="global") #cluster coefficient
STK11_SAM.degreeCent <- centralization.degree(STK11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_SAMftn <- as.network.matrix(STK11_SAMft)
STK11_SAM.netDensity <- network.density(STK11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_SAM.entropy <- entropy(STK11_SAMft) #entropy

STK11_SAM.netMx <- cbind(STK11_SAM.netMx, STK11_SAM.clusterCoef, STK11_SAM.degreeCent$centralization,
                         STK11_SAM.netDensity, STK11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "Turnover_AM"
STK11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
STK11_TAMg2 <- data.frame(STK11_TAM)
STK11_TAMg2 <- STK11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_TAMg2$player1
player2vector <- STK11_TAMg2$player2
STK11_TAMg3 <- STK11_TAMg2
STK11_TAMg3$p1inp2vec <- is.element(STK11_TAMg3$player1, player2vector)
STK11_TAMg3$p2inp1vec <- is.element(STK11_TAMg3$player2, player1vector)

addPlayer1 <- STK11_TAMg3[ which(STK11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_TAMg3[ which(STK11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_TAMg2 <- rbind(STK11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
STK11_TAMft <- ftable(STK11_TAMg2$player1, STK11_TAMg2$player2)
STK11_TAMft2 <- as.matrix(STK11_TAMft)
numRows <- nrow(STK11_TAMft2)
numCols <- ncol(STK11_TAMft2)
STK11_TAMft3 <- STK11_TAMft2[c(1:numRows) , c(1:numCols)]
STK11_TAMTable <- graph.adjacency(STK11_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(STK11_TAMTable, vertex.label = V(STK11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
STK11_TAM.clusterCoef <- transitivity(STK11_TAMTable, type="global") #cluster coefficient
STK11_TAM.degreeCent <- centralization.degree(STK11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_TAMftn <- as.network.matrix(STK11_TAMft)
STK11_TAM.netDensity <- network.density(STK11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_TAM.entropy <- entropy(STK11_TAMft) #entropy

STK11_TAM.netMx <- cbind(STK11_TAM.netMx, STK11_TAM.clusterCoef, STK11_TAM.degreeCent$centralization,
                         STK11_TAM.netDensity, STK11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
STK11_SDMg2 <- data.frame(STK11_SDM)
STK11_SDMg2 <- STK11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_SDMg2$player1
player2vector <- STK11_SDMg2$player2
STK11_SDMg3 <- STK11_SDMg2
STK11_SDMg3$p1inp2vec <- is.element(STK11_SDMg3$player1, player2vector)
STK11_SDMg3$p2inp1vec <- is.element(STK11_SDMg3$player2, player1vector)

addPlayer1 <- STK11_SDMg3[ which(STK11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_SDMg3[ which(STK11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_SDMg2 <- rbind(STK11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
STK11_SDMft <- ftable(STK11_SDMg2$player1, STK11_SDMg2$player2)
STK11_SDMft2 <- as.matrix(STK11_SDMft)
numRows <- nrow(STK11_SDMft2)
numCols <- ncol(STK11_SDMft2)
STK11_SDMft3 <- STK11_SDMft2[c(2:numRows) , c(2:numCols)]
STK11_SDMTable <- graph.adjacency(STK11_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(STK11_SDMTable, vertex.label = V(STK11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
STK11_SDM.clusterCoef <- transitivity(STK11_SDMTable, type="global") #cluster coefficient
STK11_SDM.degreeCent <- centralization.degree(STK11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_SDMftn <- as.network.matrix(STK11_SDMft)
STK11_SDM.netDensity <- network.density(STK11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_SDM.entropy <- entropy(STK11_SDMft) #entropy

STK11_SDM.netMx <- cbind(STK11_SDM.netMx, STK11_SDM.clusterCoef, STK11_SDM.degreeCent$centralization,
                         STK11_SDM.netDensity, STK11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "STK"
KIoutcome = "Turnover_DM"
STK11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
STK11_TDMg2 <- data.frame(STK11_TDM)
STK11_TDMg2 <- STK11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_TDMg2$player1
player2vector <- STK11_TDMg2$player2
STK11_TDMg3 <- STK11_TDMg2
STK11_TDMg3$p1inp2vec <- is.element(STK11_TDMg3$player1, player2vector)
STK11_TDMg3$p2inp1vec <- is.element(STK11_TDMg3$player2, player1vector)

addPlayer1 <- STK11_TDMg3[ which(STK11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_TDMg3[ which(STK11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_TDMg2 <- rbind(STK11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
STK11_TDMft <- ftable(STK11_TDMg2$player1, STK11_TDMg2$player2)
STK11_TDMft2 <- as.matrix(STK11_TDMft)
numRows <- nrow(STK11_TDMft2)
numCols <- ncol(STK11_TDMft2)
STK11_TDMft3 <- STK11_TDMft2[c(2:numRows) , c(2:numCols)]
STK11_TDMTable <- graph.adjacency(STK11_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(STK11_TDMTable, vertex.label = V(STK11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
STK11_TDM.clusterCoef <- transitivity(STK11_TDMTable, type="global") #cluster coefficient
STK11_TDM.degreeCent <- centralization.degree(STK11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_TDMftn <- as.network.matrix(STK11_TDMft)
STK11_TDM.netDensity <- network.density(STK11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_TDM.entropy <- entropy(STK11_TDMft) #entropy

STK11_TDM.netMx <- cbind(STK11_TDM.netMx, STK11_TDM.clusterCoef, STK11_TDM.degreeCent$centralization,
                         STK11_TDM.netDensity, STK11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "Stoppage_D"
STK11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
STK11_SDg2 <- data.frame(STK11_SD)
STK11_SDg2 <- STK11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_SDg2$player1
player2vector <- STK11_SDg2$player2
STK11_SDg3 <- STK11_SDg2
STK11_SDg3$p1inp2vec <- is.element(STK11_SDg3$player1, player2vector)
STK11_SDg3$p2inp1vec <- is.element(STK11_SDg3$player2, player1vector)

addPlayer1 <- STK11_SDg3[ which(STK11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_SDg3[ which(STK11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_SDg2 <- rbind(STK11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
STK11_SDft <- ftable(STK11_SDg2$player1, STK11_SDg2$player2)
STK11_SDft2 <- as.matrix(STK11_SDft)
numRows <- nrow(STK11_SDft2)
numCols <- ncol(STK11_SDft2)
STK11_SDft3 <- STK11_SDft2[c(2:numRows) , c(2:numCols)]
STK11_SDTable <- graph.adjacency(STK11_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(STK11_SDTable, vertex.label = V(STK11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
STK11_SD.clusterCoef <- transitivity(STK11_SDTable, type="global") #cluster coefficient
STK11_SD.degreeCent <- centralization.degree(STK11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_SDftn <- as.network.matrix(STK11_SDft)
STK11_SD.netDensity <- network.density(STK11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_SD.entropy <- entropy(STK11_SDft) #entropy

STK11_SD.netMx <- cbind(STK11_SD.netMx, STK11_SD.clusterCoef, STK11_SD.degreeCent$centralization,
                        STK11_SD.netDensity, STK11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "Turnover_D"
STK11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
STK11_TDg2 <- data.frame(STK11_TD)
STK11_TDg2 <- STK11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_TDg2$player1
player2vector <- STK11_TDg2$player2
STK11_TDg3 <- STK11_TDg2
STK11_TDg3$p1inp2vec <- is.element(STK11_TDg3$player1, player2vector)
STK11_TDg3$p2inp1vec <- is.element(STK11_TDg3$player2, player1vector)

addPlayer1 <- STK11_TDg3[ which(STK11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_TDg3[ which(STK11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_TDg2 <- rbind(STK11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
STK11_TDft <- ftable(STK11_TDg2$player1, STK11_TDg2$player2)
STK11_TDft2 <- as.matrix(STK11_TDft)
numRows <- nrow(STK11_TDft2)
numCols <- ncol(STK11_TDft2)
STK11_TDft3 <- STK11_TDft2[c(2:numRows) , c(2:numCols)]
STK11_TDTable <- graph.adjacency(STK11_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(STK11_TDTable, vertex.label = V(STK11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
STK11_TD.clusterCoef <- transitivity(STK11_TDTable, type="global") #cluster coefficient
STK11_TD.degreeCent <- centralization.degree(STK11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_TDftn <- as.network.matrix(STK11_TDft)
STK11_TD.netDensity <- network.density(STK11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_TD.entropy <- entropy(STK11_TDft) #entropy

STK11_TD.netMx <- cbind(STK11_TD.netMx, STK11_TD.clusterCoef, STK11_TD.degreeCent$centralization,
                        STK11_TD.netDensity, STK11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
STK11_QTg2 <- data.frame(STK11_QT)
STK11_QTg2 <- STK11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK11_QTg2$player1
player2vector <- STK11_QTg2$player2
STK11_QTg3 <- STK11_QTg2
STK11_QTg3$p1inp2vec <- is.element(STK11_QTg3$player1, player2vector)
STK11_QTg3$p2inp1vec <- is.element(STK11_QTg3$player2, player1vector)

addPlayer1 <- STK11_QTg3[ which(STK11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK11_QTg3[ which(STK11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK11_QTg2 <- rbind(STK11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
STK11_QTft <- ftable(STK11_QTg2$player1, STK11_QTg2$player2)
STK11_QTft2 <- as.matrix(STK11_QTft)
numRows <- nrow(STK11_QTft2)
numCols <- ncol(STK11_QTft2)
STK11_QTft3 <- STK11_QTft2[c(2:numRows) , c(2:numCols)]
STK11_QTTable <- graph.adjacency(STK11_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(STK11_QTTable, vertex.label = V(STK11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
STK11_QT.clusterCoef <- transitivity(STK11_QTTable, type="global") #cluster coefficient
STK11_QT.degreeCent <- centralization.degree(STK11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK11_QTftn <- as.network.matrix(STK11_QTft)
STK11_QT.netDensity <- network.density(STK11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK11_QT.entropy <- entropy(STK11_QTft) #entropy

STK11_QT.netMx <- cbind(STK11_QT.netMx, STK11_QT.clusterCoef, STK11_QT.degreeCent$centralization,
                        STK11_QT.netDensity, STK11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK11_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************

round = 11
teamName = "SYD"
KIoutcome = "Goal_F"
SYD11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
SYD11_Gg2 <- data.frame(SYD11_G)
SYD11_Gg2 <- SYD11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_Gg2$player1
player2vector <- SYD11_Gg2$player2
SYD11_Gg3 <- SYD11_Gg2
SYD11_Gg3$p1inp2vec <- is.element(SYD11_Gg3$player1, player2vector)
SYD11_Gg3$p2inp1vec <- is.element(SYD11_Gg3$player2, player1vector)

addPlayer1 <- SYD11_Gg3[ which(SYD11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_Gg3[ which(SYD11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_Gg2 <- rbind(SYD11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
SYD11_Gft <- ftable(SYD11_Gg2$player1, SYD11_Gg2$player2)
SYD11_Gft2 <- as.matrix(SYD11_Gft)
numRows <- nrow(SYD11_Gft2)
numCols <- ncol(SYD11_Gft2)
SYD11_Gft3 <- SYD11_Gft2[c(2:numRows) , c(2:numCols)]
SYD11_GTable <- graph.adjacency(SYD11_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(SYD11_GTable, vertex.label = V(SYD11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
SYD11_G.clusterCoef <- transitivity(SYD11_GTable, type="global") #cluster coefficient
SYD11_G.degreeCent <- centralization.degree(SYD11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_Gftn <- as.network.matrix(SYD11_Gft)
SYD11_G.netDensity <- network.density(SYD11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_G.entropy <- entropy(SYD11_Gft) #entropy

SYD11_G.netMx <- cbind(SYD11_G.netMx, SYD11_G.clusterCoef, SYD11_G.degreeCent$centralization,
                       SYD11_G.netDensity, SYD11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************

round = 11
teamName = "SYD"
KIoutcome = "Behind_F"
SYD11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
SYD11_Bg2 <- data.frame(SYD11_B)
SYD11_Bg2 <- SYD11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_Bg2$player1
player2vector <- SYD11_Bg2$player2
SYD11_Bg3 <- SYD11_Bg2
SYD11_Bg3$p1inp2vec <- is.element(SYD11_Bg3$player1, player2vector)
SYD11_Bg3$p2inp1vec <- is.element(SYD11_Bg3$player2, player1vector)

addPlayer1 <- SYD11_Bg3[ which(SYD11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_Bg3[ which(SYD11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_Bg2 <- rbind(SYD11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
SYD11_Bft <- ftable(SYD11_Bg2$player1, SYD11_Bg2$player2)
SYD11_Bft2 <- as.matrix(SYD11_Bft)
numRows <- nrow(SYD11_Bft2)
numCols <- ncol(SYD11_Bft2)
SYD11_Bft3 <- SYD11_Bft2[c(2:numRows) , c(2:numCols)]
SYD11_BTable <- graph.adjacency(SYD11_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(SYD11_BTable, vertex.label = V(SYD11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
SYD11_B.clusterCoef <- transitivity(SYD11_BTable, type="global") #cluster coefficient
SYD11_B.degreeCent <- centralization.degree(SYD11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_Bftn <- as.network.matrix(SYD11_Bft)
SYD11_B.netDensity <- network.density(SYD11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_B.entropy <- entropy(SYD11_Bft) #entropy

SYD11_B.netMx <- cbind(SYD11_B.netMx, SYD11_B.clusterCoef, SYD11_B.degreeCent$centralization,
                       SYD11_B.netDensity, SYD11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************

round = 11
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
SYD11_SFg2 <- data.frame(SYD11_SF)
SYD11_SFg2 <- SYD11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_SFg2$player1
player2vector <- SYD11_SFg2$player2
SYD11_SFg3 <- SYD11_SFg2
SYD11_SFg3$p1inp2vec <- is.element(SYD11_SFg3$player1, player2vector)
SYD11_SFg3$p2inp1vec <- is.element(SYD11_SFg3$player2, player1vector)

addPlayer1 <- SYD11_SFg3[ which(SYD11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_SFg3[ which(SYD11_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_SFg2 <- rbind(SYD11_SFg2, addPlayers)

#ROUND 11, FWD Stoppage graph using weighted edges
SYD11_SFft <- ftable(SYD11_SFg2$player1, SYD11_SFg2$player2)
SYD11_SFft2 <- as.matrix(SYD11_SFft)
numRows <- nrow(SYD11_SFft2)
numCols <- ncol(SYD11_SFft2)
SYD11_SFft3 <- SYD11_SFft2[c(2:numRows) , c(2:numCols)]
SYD11_SFTable <- graph.adjacency(SYD11_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(SYD11_SFTable, vertex.label = V(SYD11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
SYD11_SF.clusterCoef <- transitivity(SYD11_SFTable, type="global") #cluster coefficient
SYD11_SF.degreeCent <- centralization.degree(SYD11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_SFftn <- as.network.matrix(SYD11_SFft)
SYD11_SF.netDensity <- network.density(SYD11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_SF.entropy <- entropy(SYD11_SFft) #entropy

SYD11_SF.netMx <- cbind(SYD11_SF.netMx, SYD11_SF.clusterCoef, SYD11_SF.degreeCent$centralization,
                        SYD11_SF.netDensity, SYD11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************
#NA

round = 11
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
SYD11_TFg2 <- data.frame(SYD11_TF)
SYD11_TFg2 <- SYD11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_TFg2$player1
player2vector <- SYD11_TFg2$player2
SYD11_TFg3 <- SYD11_TFg2
SYD11_TFg3$p1inp2vec <- is.element(SYD11_TFg3$player1, player2vector)
SYD11_TFg3$p2inp1vec <- is.element(SYD11_TFg3$player2, player1vector)

addPlayer1 <- SYD11_TFg3[ which(SYD11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_TFg3[ which(SYD11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_TFg2 <- rbind(SYD11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
SYD11_TFft <- ftable(SYD11_TFg2$player1, SYD11_TFg2$player2)
SYD11_TFft2 <- as.matrix(SYD11_TFft)
numRows <- nrow(SYD11_TFft2)
numCols <- ncol(SYD11_TFft2)
SYD11_TFft3 <- SYD11_TFft2[c(2:numRows) , c(2:numCols)]
SYD11_TFTable <- graph.adjacency(SYD11_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(SYD11_TFTable, vertex.label = V(SYD11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
SYD11_TF.clusterCoef <- transitivity(SYD11_TFTable, type="global") #cluster coefficient
SYD11_TF.degreeCent <- centralization.degree(SYD11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_TFftn <- as.network.matrix(SYD11_TFft)
SYD11_TF.netDensity <- network.density(SYD11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_TF.entropy <- entropy(SYD11_TFft) #entropy

SYD11_TF.netMx <- cbind(SYD11_TF.netMx, SYD11_TF.clusterCoef, SYD11_TF.degreeCent$centralization,
                        SYD11_TF.netDensity, SYD11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
SYD11_SAMg2 <- data.frame(SYD11_SAM)
SYD11_SAMg2 <- SYD11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_SAMg2$player1
player2vector <- SYD11_SAMg2$player2
SYD11_SAMg3 <- SYD11_SAMg2
SYD11_SAMg3$p1inp2vec <- is.element(SYD11_SAMg3$player1, player2vector)
SYD11_SAMg3$p2inp1vec <- is.element(SYD11_SAMg3$player2, player1vector)

addPlayer1 <- SYD11_SAMg3[ which(SYD11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_SAMg3[ which(SYD11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_SAMg2 <- rbind(SYD11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
SYD11_SAMft <- ftable(SYD11_SAMg2$player1, SYD11_SAMg2$player2)
SYD11_SAMft2 <- as.matrix(SYD11_SAMft)
numRows <- nrow(SYD11_SAMft2)
numCols <- ncol(SYD11_SAMft2)
SYD11_SAMft3 <- SYD11_SAMft2[c(2:numRows) , c(2:numCols)]
SYD11_SAMTable <- graph.adjacency(SYD11_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(SYD11_SAMTable, vertex.label = V(SYD11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
SYD11_SAM.clusterCoef <- transitivity(SYD11_SAMTable, type="global") #cluster coefficient
SYD11_SAM.degreeCent <- centralization.degree(SYD11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_SAMftn <- as.network.matrix(SYD11_SAMft)
SYD11_SAM.netDensity <- network.density(SYD11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_SAM.entropy <- entropy(SYD11_SAMft) #entropy

SYD11_SAM.netMx <- cbind(SYD11_SAM.netMx, SYD11_SAM.clusterCoef, SYD11_SAM.degreeCent$centralization,
                         SYD11_SAM.netDensity, SYD11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************

round = 11
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
SYD11_TAMg2 <- data.frame(SYD11_TAM)
SYD11_TAMg2 <- SYD11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_TAMg2$player1
player2vector <- SYD11_TAMg2$player2
SYD11_TAMg3 <- SYD11_TAMg2
SYD11_TAMg3$p1inp2vec <- is.element(SYD11_TAMg3$player1, player2vector)
SYD11_TAMg3$p2inp1vec <- is.element(SYD11_TAMg3$player2, player1vector)

addPlayer1 <- SYD11_TAMg3[ which(SYD11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_TAMg3[ which(SYD11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_TAMg2 <- rbind(SYD11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
SYD11_TAMft <- ftable(SYD11_TAMg2$player1, SYD11_TAMg2$player2)
SYD11_TAMft2 <- as.matrix(SYD11_TAMft)
numRows <- nrow(SYD11_TAMft2)
numCols <- ncol(SYD11_TAMft2)
SYD11_TAMft3 <- SYD11_TAMft2[c(2:numRows) , c(2:numCols)]
SYD11_TAMTable <- graph.adjacency(SYD11_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(SYD11_TAMTable, vertex.label = V(SYD11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
SYD11_TAM.clusterCoef <- transitivity(SYD11_TAMTable, type="global") #cluster coefficient
SYD11_TAM.degreeCent <- centralization.degree(SYD11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_TAMftn <- as.network.matrix(SYD11_TAMft)
SYD11_TAM.netDensity <- network.density(SYD11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_TAM.entropy <- entropy(SYD11_TAMft) #entropy

SYD11_TAM.netMx <- cbind(SYD11_TAM.netMx, SYD11_TAM.clusterCoef, SYD11_TAM.degreeCent$centralization,
                         SYD11_TAM.netDensity, SYD11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
SYD11_SDMg2 <- data.frame(SYD11_SDM)
SYD11_SDMg2 <- SYD11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_SDMg2$player1
player2vector <- SYD11_SDMg2$player2
SYD11_SDMg3 <- SYD11_SDMg2
SYD11_SDMg3$p1inp2vec <- is.element(SYD11_SDMg3$player1, player2vector)
SYD11_SDMg3$p2inp1vec <- is.element(SYD11_SDMg3$player2, player1vector)

addPlayer1 <- SYD11_SDMg3[ which(SYD11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_SDMg3[ which(SYD11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_SDMg2 <- rbind(SYD11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
SYD11_SDMft <- ftable(SYD11_SDMg2$player1, SYD11_SDMg2$player2)
SYD11_SDMft2 <- as.matrix(SYD11_SDMft)
numRows <- nrow(SYD11_SDMft2)
numCols <- ncol(SYD11_SDMft2)
SYD11_SDMft3 <- SYD11_SDMft2[c(2:numRows) , c(2:numCols)]
SYD11_SDMTable <- graph.adjacency(SYD11_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(SYD11_SDMTable, vertex.label = V(SYD11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
SYD11_SDM.clusterCoef <- transitivity(SYD11_SDMTable, type="global") #cluster coefficient
SYD11_SDM.degreeCent <- centralization.degree(SYD11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_SDMftn <- as.network.matrix(SYD11_SDMft)
SYD11_SDM.netDensity <- network.density(SYD11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_SDM.entropy <- entropy(SYD11_SDMft) #entropy

SYD11_SDM.netMx <- cbind(SYD11_SDM.netMx, SYD11_SDM.clusterCoef, SYD11_SDM.degreeCent$centralization,
                         SYD11_SDM.netDensity, SYD11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
SYD11_TDMg2 <- data.frame(SYD11_TDM)
SYD11_TDMg2 <- SYD11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_TDMg2$player1
player2vector <- SYD11_TDMg2$player2
SYD11_TDMg3 <- SYD11_TDMg2
SYD11_TDMg3$p1inp2vec <- is.element(SYD11_TDMg3$player1, player2vector)
SYD11_TDMg3$p2inp1vec <- is.element(SYD11_TDMg3$player2, player1vector)

addPlayer1 <- SYD11_TDMg3[ which(SYD11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_TDMg3[ which(SYD11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_TDMg2 <- rbind(SYD11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
SYD11_TDMft <- ftable(SYD11_TDMg2$player1, SYD11_TDMg2$player2)
SYD11_TDMft2 <- as.matrix(SYD11_TDMft)
numRows <- nrow(SYD11_TDMft2)
numCols <- ncol(SYD11_TDMft2)
SYD11_TDMft3 <- SYD11_TDMft2[c(2:numRows) , c(2:numCols)]
SYD11_TDMTable <- graph.adjacency(SYD11_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(SYD11_TDMTable, vertex.label = V(SYD11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
SYD11_TDM.clusterCoef <- transitivity(SYD11_TDMTable, type="global") #cluster coefficient
SYD11_TDM.degreeCent <- centralization.degree(SYD11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_TDMftn <- as.network.matrix(SYD11_TDMft)
SYD11_TDM.netDensity <- network.density(SYD11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_TDM.entropy <- entropy(SYD11_TDMft) #entropy

SYD11_TDM.netMx <- cbind(SYD11_TDM.netMx, SYD11_TDM.clusterCoef, SYD11_TDM.degreeCent$centralization,
                         SYD11_TDM.netDensity, SYD11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
SYD11_SDg2 <- data.frame(SYD11_SD)
SYD11_SDg2 <- SYD11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_SDg2$player1
player2vector <- SYD11_SDg2$player2
SYD11_SDg3 <- SYD11_SDg2
SYD11_SDg3$p1inp2vec <- is.element(SYD11_SDg3$player1, player2vector)
SYD11_SDg3$p2inp1vec <- is.element(SYD11_SDg3$player2, player1vector)

addPlayer1 <- SYD11_SDg3[ which(SYD11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_SDg3[ which(SYD11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_SDg2 <- rbind(SYD11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
SYD11_SDft <- ftable(SYD11_SDg2$player1, SYD11_SDg2$player2)
SYD11_SDft2 <- as.matrix(SYD11_SDft)
numRows <- nrow(SYD11_SDft2)
numCols <- ncol(SYD11_SDft2)
SYD11_SDft3 <- SYD11_SDft2[c(2:numRows) , c(2:numCols)]
SYD11_SDTable <- graph.adjacency(SYD11_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(SYD11_SDTable, vertex.label = V(SYD11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
SYD11_SD.clusterCoef <- transitivity(SYD11_SDTable, type="global") #cluster coefficient
SYD11_SD.degreeCent <- centralization.degree(SYD11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_SDftn <- as.network.matrix(SYD11_SDft)
SYD11_SD.netDensity <- network.density(SYD11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_SD.entropy <- entropy(SYD11_SDft) #entropy

SYD11_SD.netMx <- cbind(SYD11_SD.netMx, SYD11_SD.clusterCoef, SYD11_SD.degreeCent$centralization,
                        SYD11_SD.netDensity, SYD11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
SYD11_TDg2 <- data.frame(SYD11_TD)
SYD11_TDg2 <- SYD11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_TDg2$player1
player2vector <- SYD11_TDg2$player2
SYD11_TDg3 <- SYD11_TDg2
SYD11_TDg3$p1inp2vec <- is.element(SYD11_TDg3$player1, player2vector)
SYD11_TDg3$p2inp1vec <- is.element(SYD11_TDg3$player2, player1vector)

addPlayer1 <- SYD11_TDg3[ which(SYD11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_TDg3[ which(SYD11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_TDg2 <- rbind(SYD11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
SYD11_TDft <- ftable(SYD11_TDg2$player1, SYD11_TDg2$player2)
SYD11_TDft2 <- as.matrix(SYD11_TDft)
numRows <- nrow(SYD11_TDft2)
numCols <- ncol(SYD11_TDft2)
SYD11_TDft3 <- SYD11_TDft2[c(2:numRows) , c(2:numCols)]
SYD11_TDTable <- graph.adjacency(SYD11_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(SYD11_TDTable, vertex.label = V(SYD11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
SYD11_TD.clusterCoef <- transitivity(SYD11_TDTable, type="global") #cluster coefficient
SYD11_TD.degreeCent <- centralization.degree(SYD11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_TDftn <- as.network.matrix(SYD11_TDft)
SYD11_TD.netDensity <- network.density(SYD11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_TD.entropy <- entropy(SYD11_TDft) #entropy

SYD11_TD.netMx <- cbind(SYD11_TD.netMx, SYD11_TD.clusterCoef, SYD11_TD.degreeCent$centralization,
                        SYD11_TD.netDensity, SYD11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
SYD11_QTg2 <- data.frame(SYD11_QT)
SYD11_QTg2 <- SYD11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD11_QTg2$player1
player2vector <- SYD11_QTg2$player2
SYD11_QTg3 <- SYD11_QTg2
SYD11_QTg3$p1inp2vec <- is.element(SYD11_QTg3$player1, player2vector)
SYD11_QTg3$p2inp1vec <- is.element(SYD11_QTg3$player2, player1vector)

addPlayer1 <- SYD11_QTg3[ which(SYD11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD11_QTg3[ which(SYD11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD11_QTg2 <- rbind(SYD11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
SYD11_QTft <- ftable(SYD11_QTg2$player1, SYD11_QTg2$player2)
SYD11_QTft2 <- as.matrix(SYD11_QTft)
numRows <- nrow(SYD11_QTft2)
numCols <- ncol(SYD11_QTft2)
SYD11_QTft3 <- SYD11_QTft2[c(2:numRows) , c(2:numCols)]
SYD11_QTTable <- graph.adjacency(SYD11_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(SYD11_QTTable, vertex.label = V(SYD11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
SYD11_QT.clusterCoef <- transitivity(SYD11_QTTable, type="global") #cluster coefficient
SYD11_QT.degreeCent <- centralization.degree(SYD11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD11_QTftn <- as.network.matrix(SYD11_QTft)
SYD11_QT.netDensity <- network.density(SYD11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD11_QT.entropy <- entropy(SYD11_QTft) #entropy

SYD11_QT.netMx <- cbind(SYD11_QT.netMx, SYD11_QT.clusterCoef, SYD11_QT.degreeCent$centralization,
                        SYD11_QT.netDensity, SYD11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD11_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 11
##

#ROUND 11, Goal***************************************************************

round = 11
teamName = "WCE"
KIoutcome = "Goal_F"
WCE11_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Goal with weighted edges
WCE11_Gg2 <- data.frame(WCE11_G)
WCE11_Gg2 <- WCE11_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_Gg2$player1
player2vector <- WCE11_Gg2$player2
WCE11_Gg3 <- WCE11_Gg2
WCE11_Gg3$p1inp2vec <- is.element(WCE11_Gg3$player1, player2vector)
WCE11_Gg3$p2inp1vec <- is.element(WCE11_Gg3$player2, player1vector)

addPlayer1 <- WCE11_Gg3[ which(WCE11_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_Gg3[ which(WCE11_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_Gg2 <- rbind(WCE11_Gg2, addPlayers)

#ROUND 11, Goal graph using weighted edges
WCE11_Gft <- ftable(WCE11_Gg2$player1, WCE11_Gg2$player2)
WCE11_Gft2 <- as.matrix(WCE11_Gft)
numRows <- nrow(WCE11_Gft2)
numCols <- ncol(WCE11_Gft2)
WCE11_Gft3 <- WCE11_Gft2[c(2:numRows) , c(2:numCols)]
WCE11_GTable <- graph.adjacency(WCE11_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Goal graph=weighted
plot.igraph(WCE11_GTable, vertex.label = V(WCE11_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Goal calulation of network metrics
#igraph
WCE11_G.clusterCoef <- transitivity(WCE11_GTable, type="global") #cluster coefficient
WCE11_G.degreeCent <- centralization.degree(WCE11_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_Gftn <- as.network.matrix(WCE11_Gft)
WCE11_G.netDensity <- network.density(WCE11_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_G.entropy <- entropy(WCE11_Gft) #entropy

WCE11_G.netMx <- cbind(WCE11_G.netMx, WCE11_G.clusterCoef, WCE11_G.degreeCent$centralization,
                       WCE11_G.netDensity, WCE11_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_G.netMx) <- varnames

#ROUND 11, Behind***************************************************************
#NA

round = 11
teamName = "WCE"
KIoutcome = "Behind_F"
WCE11_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, Behind with weighted edges
WCE11_Bg2 <- data.frame(WCE11_B)
WCE11_Bg2 <- WCE11_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_Bg2$player1
player2vector <- WCE11_Bg2$player2
WCE11_Bg3 <- WCE11_Bg2
WCE11_Bg3$p1inp2vec <- is.element(WCE11_Bg3$player1, player2vector)
WCE11_Bg3$p2inp1vec <- is.element(WCE11_Bg3$player2, player1vector)

addPlayer1 <- WCE11_Bg3[ which(WCE11_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_Bg3[ which(WCE11_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_Bg2 <- rbind(WCE11_Bg2, addPlayers)

#ROUND 11, Behind graph using weighted edges
WCE11_Bft <- ftable(WCE11_Bg2$player1, WCE11_Bg2$player2)
WCE11_Bft2 <- as.matrix(WCE11_Bft)
numRows <- nrow(WCE11_Bft2)
numCols <- ncol(WCE11_Bft2)
WCE11_Bft3 <- WCE11_Bft2[c(2:numRows) , c(2:numCols)]
WCE11_BTable <- graph.adjacency(WCE11_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 11, Behind graph=weighted
plot.igraph(WCE11_BTable, vertex.label = V(WCE11_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, Behind calulation of network metrics
#igraph
WCE11_B.clusterCoef <- transitivity(WCE11_BTable, type="global") #cluster coefficient
WCE11_B.degreeCent <- centralization.degree(WCE11_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_Bftn <- as.network.matrix(WCE11_Bft)
WCE11_B.netDensity <- network.density(WCE11_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_B.entropy <- entropy(WCE11_Bft) #entropy

WCE11_B.netMx <- cbind(WCE11_B.netMx, WCE11_B.clusterCoef, WCE11_B.degreeCent$centralization,
                       WCE11_B.netDensity, WCE11_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_B.netMx) <- varnames

#ROUND 11, FWD Stoppage**********************************************************
#NA

round = 11
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE11_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Stoppage with weighted edges
WCE11_SFg2 <- data.frame(WCE11_SF)
WCE11_SFg2 <- WCE11_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_SFg2$player1
player2vector <- WCE11_SFg2$player2
WCE11_SFg3 <- WCE11_SFg2
WCE11_SFg3$p1inp2vec <- is.element(WCE11_SFg3$player1, player2vector)
WCE11_SFg3$p2inp1vec <- is.element(WCE11_SFg3$player2, player1vector)

addPlayer1 <- WCE11_SFg3[ which(WCE11_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE11_SFg2 <- rbind(WCE11_SFg2, addPlayer1)

#ROUND 11, FWD Stoppage graph using weighted edges
WCE11_SFft <- ftable(WCE11_SFg2$player1, WCE11_SFg2$player2)
WCE11_SFft2 <- as.matrix(WCE11_SFft)
numRows <- nrow(WCE11_SFft2)
numCols <- ncol(WCE11_SFft2)
WCE11_SFft3 <- WCE11_SFft2[c(2:numRows) , c(1:numCols)]
WCE11_SFTable <- graph.adjacency(WCE11_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Stoppage graph=weighted
plot.igraph(WCE11_SFTable, vertex.label = V(WCE11_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Stoppage calulation of network metrics
#igraph
WCE11_SF.clusterCoef <- transitivity(WCE11_SFTable, type="global") #cluster coefficient
WCE11_SF.degreeCent <- centralization.degree(WCE11_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_SFftn <- as.network.matrix(WCE11_SFft)
WCE11_SF.netDensity <- network.density(WCE11_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_SF.entropy <- entropy(WCE11_SFft) #entropy

WCE11_SF.netMx <- cbind(WCE11_SF.netMx, WCE11_SF.clusterCoef, WCE11_SF.degreeCent$centralization,
                        WCE11_SF.netDensity, WCE11_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_SF.netMx) <- varnames

#ROUND 11, FWD Turnover**********************************************************

round = 11
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE11_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, FWD Turnover with weighted edges
WCE11_TFg2 <- data.frame(WCE11_TF)
WCE11_TFg2 <- WCE11_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_TFg2$player1
player2vector <- WCE11_TFg2$player2
WCE11_TFg3 <- WCE11_TFg2
WCE11_TFg3$p1inp2vec <- is.element(WCE11_TFg3$player1, player2vector)
WCE11_TFg3$p2inp1vec <- is.element(WCE11_TFg3$player2, player1vector)

addPlayer1 <- WCE11_TFg3[ which(WCE11_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE11_TFg3[ which(WCE11_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_TFg2 <- rbind(WCE11_TFg2, addPlayers)

#ROUND 11, FWD Turnover graph using weighted edges
WCE11_TFft <- ftable(WCE11_TFg2$player1, WCE11_TFg2$player2)
WCE11_TFft2 <- as.matrix(WCE11_TFft)
numRows <- nrow(WCE11_TFft2)
numCols <- ncol(WCE11_TFft2)
WCE11_TFft3 <- WCE11_TFft2[c(2:numRows) , c(2:numCols)]
WCE11_TFTable <- graph.adjacency(WCE11_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, FWD Turnover graph=weighted
plot.igraph(WCE11_TFTable, vertex.label = V(WCE11_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, FWD Turnover calulation of network metrics
#igraph
WCE11_TF.clusterCoef <- transitivity(WCE11_TFTable, type="global") #cluster coefficient
WCE11_TF.degreeCent <- centralization.degree(WCE11_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_TFftn <- as.network.matrix(WCE11_TFft)
WCE11_TF.netDensity <- network.density(WCE11_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_TF.entropy <- entropy(WCE11_TFft) #entropy

WCE11_TF.netMx <- cbind(WCE11_TF.netMx, WCE11_TF.clusterCoef, WCE11_TF.degreeCent$centralization,
                        WCE11_TF.netDensity, WCE11_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_TF.netMx) <- varnames

#ROUND 11, AM Stoppage**********************************************************

round = 11
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE11_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Stoppage with weighted edges
WCE11_SAMg2 <- data.frame(WCE11_SAM)
WCE11_SAMg2 <- WCE11_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_SAMg2$player1
player2vector <- WCE11_SAMg2$player2
WCE11_SAMg3 <- WCE11_SAMg2
WCE11_SAMg3$p1inp2vec <- is.element(WCE11_SAMg3$player1, player2vector)
WCE11_SAMg3$p2inp1vec <- is.element(WCE11_SAMg3$player2, player1vector)

addPlayer1 <- WCE11_SAMg3[ which(WCE11_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_SAMg3[ which(WCE11_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_SAMg2 <- rbind(WCE11_SAMg2, addPlayers)

#ROUND 11, AM Stoppage graph using weighted edges
WCE11_SAMft <- ftable(WCE11_SAMg2$player1, WCE11_SAMg2$player2)
WCE11_SAMft2 <- as.matrix(WCE11_SAMft)
numRows <- nrow(WCE11_SAMft2)
numCols <- ncol(WCE11_SAMft2)
WCE11_SAMft3 <- WCE11_SAMft2[c(2:numRows) , c(2:numCols)]
WCE11_SAMTable <- graph.adjacency(WCE11_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Stoppage graph=weighted
plot.igraph(WCE11_SAMTable, vertex.label = V(WCE11_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Stoppage calulation of network metrics
#igraph
WCE11_SAM.clusterCoef <- transitivity(WCE11_SAMTable, type="global") #cluster coefficient
WCE11_SAM.degreeCent <- centralization.degree(WCE11_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_SAMftn <- as.network.matrix(WCE11_SAMft)
WCE11_SAM.netDensity <- network.density(WCE11_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_SAM.entropy <- entropy(WCE11_SAMft) #entropy

WCE11_SAM.netMx <- cbind(WCE11_SAM.netMx, WCE11_SAM.clusterCoef, WCE11_SAM.degreeCent$centralization,
                         WCE11_SAM.netDensity, WCE11_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_SAM.netMx) <- varnames

#ROUND 11, AM Turnover**********************************************************
#NA

round = 11
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE11_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, AM Turnover with weighted edges
WCE11_TAMg2 <- data.frame(WCE11_TAM)
WCE11_TAMg2 <- WCE11_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_TAMg2$player1
player2vector <- WCE11_TAMg2$player2
WCE11_TAMg3 <- WCE11_TAMg2
WCE11_TAMg3$p1inp2vec <- is.element(WCE11_TAMg3$player1, player2vector)
WCE11_TAMg3$p2inp1vec <- is.element(WCE11_TAMg3$player2, player1vector)

addPlayer1 <- WCE11_TAMg3[ which(WCE11_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_TAMg3[ which(WCE11_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_TAMg2 <- rbind(WCE11_TAMg2, addPlayers)

#ROUND 11, AM Turnover graph using weighted edges
WCE11_TAMft <- ftable(WCE11_TAMg2$player1, WCE11_TAMg2$player2)
WCE11_TAMft2 <- as.matrix(WCE11_TAMft)
numRows <- nrow(WCE11_TAMft2)
numCols <- ncol(WCE11_TAMft2)
WCE11_TAMft3 <- WCE11_TAMft2[c(2:numRows) , c(2:numCols)]
WCE11_TAMTable <- graph.adjacency(WCE11_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, AM Turnover graph=weighted
plot.igraph(WCE11_TAMTable, vertex.label = V(WCE11_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, AM Turnover calulation of network metrics
#igraph
WCE11_TAM.clusterCoef <- transitivity(WCE11_TAMTable, type="global") #cluster coefficient
WCE11_TAM.degreeCent <- centralization.degree(WCE11_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_TAMftn <- as.network.matrix(WCE11_TAMft)
WCE11_TAM.netDensity <- network.density(WCE11_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_TAM.entropy <- entropy(WCE11_TAMft) #entropy

WCE11_TAM.netMx <- cbind(WCE11_TAM.netMx, WCE11_TAM.clusterCoef, WCE11_TAM.degreeCent$centralization,
                         WCE11_TAM.netDensity, WCE11_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_TAM.netMx) <- varnames

#ROUND 11, DM Stoppage**********************************************************

round = 11
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE11_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Stoppage with weighted edges
WCE11_SDMg2 <- data.frame(WCE11_SDM)
WCE11_SDMg2 <- WCE11_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_SDMg2$player1
player2vector <- WCE11_SDMg2$player2
WCE11_SDMg3 <- WCE11_SDMg2
WCE11_SDMg3$p1inp2vec <- is.element(WCE11_SDMg3$player1, player2vector)
WCE11_SDMg3$p2inp1vec <- is.element(WCE11_SDMg3$player2, player1vector)

addPlayer1 <- WCE11_SDMg3[ which(WCE11_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_SDMg3[ which(WCE11_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_SDMg2 <- rbind(WCE11_SDMg2, addPlayers)

#ROUND 11, DM Stoppage graph using weighted edges
WCE11_SDMft <- ftable(WCE11_SDMg2$player1, WCE11_SDMg2$player2)
WCE11_SDMft2 <- as.matrix(WCE11_SDMft)
numRows <- nrow(WCE11_SDMft2)
numCols <- ncol(WCE11_SDMft2)
WCE11_SDMft3 <- WCE11_SDMft2[c(2:numRows) , c(2:numCols)]
WCE11_SDMTable <- graph.adjacency(WCE11_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Stoppage graph=weighted
plot.igraph(WCE11_SDMTable, vertex.label = V(WCE11_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Stoppage calulation of network metrics
#igraph
WCE11_SDM.clusterCoef <- transitivity(WCE11_SDMTable, type="global") #cluster coefficient
WCE11_SDM.degreeCent <- centralization.degree(WCE11_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_SDMftn <- as.network.matrix(WCE11_SDMft)
WCE11_SDM.netDensity <- network.density(WCE11_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_SDM.entropy <- entropy(WCE11_SDMft) #entropy

WCE11_SDM.netMx <- cbind(WCE11_SDM.netMx, WCE11_SDM.clusterCoef, WCE11_SDM.degreeCent$centralization,
                         WCE11_SDM.netDensity, WCE11_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_SDM.netMx) <- varnames

#ROUND 11, DM Turnover**********************************************************

round = 11
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE11_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, DM Turnover with weighted edges
WCE11_TDMg2 <- data.frame(WCE11_TDM)
WCE11_TDMg2 <- WCE11_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_TDMg2$player1
player2vector <- WCE11_TDMg2$player2
WCE11_TDMg3 <- WCE11_TDMg2
WCE11_TDMg3$p1inp2vec <- is.element(WCE11_TDMg3$player1, player2vector)
WCE11_TDMg3$p2inp1vec <- is.element(WCE11_TDMg3$player2, player1vector)

addPlayer1 <- WCE11_TDMg3[ which(WCE11_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_TDMg3[ which(WCE11_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_TDMg2 <- rbind(WCE11_TDMg2, addPlayers)

#ROUND 11, DM Turnover graph using weighted edges
WCE11_TDMft <- ftable(WCE11_TDMg2$player1, WCE11_TDMg2$player2)
WCE11_TDMft2 <- as.matrix(WCE11_TDMft)
numRows <- nrow(WCE11_TDMft2)
numCols <- ncol(WCE11_TDMft2)
WCE11_TDMft3 <- WCE11_TDMft2[c(2:numRows) , c(2:numCols)]
WCE11_TDMTable <- graph.adjacency(WCE11_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 11, DM Turnover graph=weighted
plot.igraph(WCE11_TDMTable, vertex.label = V(WCE11_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, DM Turnover calulation of network metrics
#igraph
WCE11_TDM.clusterCoef <- transitivity(WCE11_TDMTable, type="global") #cluster coefficient
WCE11_TDM.degreeCent <- centralization.degree(WCE11_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_TDMftn <- as.network.matrix(WCE11_TDMft)
WCE11_TDM.netDensity <- network.density(WCE11_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_TDM.entropy <- entropy(WCE11_TDMft) #entropy

WCE11_TDM.netMx <- cbind(WCE11_TDM.netMx, WCE11_TDM.clusterCoef, WCE11_TDM.degreeCent$centralization,
                         WCE11_TDM.netDensity, WCE11_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_TDM.netMx) <- varnames

#ROUND 11, D Stoppage**********************************************************
#NA

round = 11
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE11_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Stoppage with weighted edges
WCE11_SDg2 <- data.frame(WCE11_SD)
WCE11_SDg2 <- WCE11_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_SDg2$player1
player2vector <- WCE11_SDg2$player2
WCE11_SDg3 <- WCE11_SDg2
WCE11_SDg3$p1inp2vec <- is.element(WCE11_SDg3$player1, player2vector)
WCE11_SDg3$p2inp1vec <- is.element(WCE11_SDg3$player2, player1vector)

addPlayer1 <- WCE11_SDg3[ which(WCE11_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_SDg3[ which(WCE11_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_SDg2 <- rbind(WCE11_SDg2, addPlayers)

#ROUND 11, D Stoppage graph using weighted edges
WCE11_SDft <- ftable(WCE11_SDg2$player1, WCE11_SDg2$player2)
WCE11_SDft2 <- as.matrix(WCE11_SDft)
numRows <- nrow(WCE11_SDft2)
numCols <- ncol(WCE11_SDft2)
WCE11_SDft3 <- WCE11_SDft2[c(2:numRows) , c(2:numCols)]
WCE11_SDTable <- graph.adjacency(WCE11_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Stoppage graph=weighted
plot.igraph(WCE11_SDTable, vertex.label = V(WCE11_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Stoppage calulation of network metrics
#igraph
WCE11_SD.clusterCoef <- transitivity(WCE11_SDTable, type="global") #cluster coefficient
WCE11_SD.degreeCent <- centralization.degree(WCE11_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_SDftn <- as.network.matrix(WCE11_SDft)
WCE11_SD.netDensity <- network.density(WCE11_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_SD.entropy <- entropy(WCE11_SDft) #entropy

WCE11_SD.netMx <- cbind(WCE11_SD.netMx, WCE11_SD.clusterCoef, WCE11_SD.degreeCent$centralization,
                        WCE11_SD.netDensity, WCE11_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_SD.netMx) <- varnames

#ROUND 11, D Turnover**********************************************************
#NA

round = 11
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE11_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, D Turnover with weighted edges
WCE11_TDg2 <- data.frame(WCE11_TD)
WCE11_TDg2 <- WCE11_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_TDg2$player1
player2vector <- WCE11_TDg2$player2
WCE11_TDg3 <- WCE11_TDg2
WCE11_TDg3$p1inp2vec <- is.element(WCE11_TDg3$player1, player2vector)
WCE11_TDg3$p2inp1vec <- is.element(WCE11_TDg3$player2, player1vector)

addPlayer1 <- WCE11_TDg3[ which(WCE11_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_TDg3[ which(WCE11_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_TDg2 <- rbind(WCE11_TDg2, addPlayers)

#ROUND 11, D Turnover graph using weighted edges
WCE11_TDft <- ftable(WCE11_TDg2$player1, WCE11_TDg2$player2)
WCE11_TDft2 <- as.matrix(WCE11_TDft)
numRows <- nrow(WCE11_TDft2)
numCols <- ncol(WCE11_TDft2)
WCE11_TDft3 <- WCE11_TDft2[c(2:numRows) , c(2:numCols)]
WCE11_TDTable <- graph.adjacency(WCE11_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, D Turnover graph=weighted
plot.igraph(WCE11_TDTable, vertex.label = V(WCE11_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, D Turnover calulation of network metrics
#igraph
WCE11_TD.clusterCoef <- transitivity(WCE11_TDTable, type="global") #cluster coefficient
WCE11_TD.degreeCent <- centralization.degree(WCE11_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_TDftn <- as.network.matrix(WCE11_TDft)
WCE11_TD.netDensity <- network.density(WCE11_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_TD.entropy <- entropy(WCE11_TDft) #entropy

WCE11_TD.netMx <- cbind(WCE11_TD.netMx, WCE11_TD.clusterCoef, WCE11_TD.degreeCent$centralization,
                        WCE11_TD.netDensity, WCE11_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_TD.netMx) <- varnames

#ROUND 11, End of Qtr**********************************************************
#NA

round = 11
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE11_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 11, End of Qtr with weighted edges
WCE11_QTg2 <- data.frame(WCE11_QT)
WCE11_QTg2 <- WCE11_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE11_QTg2$player1
player2vector <- WCE11_QTg2$player2
WCE11_QTg3 <- WCE11_QTg2
WCE11_QTg3$p1inp2vec <- is.element(WCE11_QTg3$player1, player2vector)
WCE11_QTg3$p2inp1vec <- is.element(WCE11_QTg3$player2, player1vector)

addPlayer1 <- WCE11_QTg3[ which(WCE11_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE11_QTg3[ which(WCE11_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE11_QTg2 <- rbind(WCE11_QTg2, addPlayers)

#ROUND 11, End of Qtr graph using weighted edges
WCE11_QTft <- ftable(WCE11_QTg2$player1, WCE11_QTg2$player2)
WCE11_QTft2 <- as.matrix(WCE11_QTft)
numRows <- nrow(WCE11_QTft2)
numCols <- ncol(WCE11_QTft2)
WCE11_QTft3 <- WCE11_QTft2[c(2:numRows) , c(2:numCols)]
WCE11_QTTable <- graph.adjacency(WCE11_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 11, End of Qtr graph=weighted
plot.igraph(WCE11_QTTable, vertex.label = V(WCE11_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE11_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 11, End of Qtr calulation of network metrics
#igraph
WCE11_QT.clusterCoef <- transitivity(WCE11_QTTable, type="global") #cluster coefficient
WCE11_QT.degreeCent <- centralization.degree(WCE11_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE11_QTftn <- as.network.matrix(WCE11_QTft)
WCE11_QT.netDensity <- network.density(WCE11_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE11_QT.entropy <- entropy(WCE11_QTft) #entropy

WCE11_QT.netMx <- cbind(WCE11_QT.netMx, WCE11_QT.clusterCoef, WCE11_QT.degreeCent$centralization,
                        WCE11_QT.netDensity, WCE11_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE11_QT.netMx) <- varnames
