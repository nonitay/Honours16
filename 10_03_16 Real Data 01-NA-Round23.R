#####
#10-03-16- Real data 23
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
ADEL23_Gg2 <- data.frame(ADEL23_G)
ADEL23_Gg2 <- ADEL23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_Gg2$player1
player2vector <- ADEL23_Gg2$player2
ADEL23_Gg3 <- ADEL23_Gg2
ADEL23_Gg3$p1inp2vec <- is.element(ADEL23_Gg3$player1, player2vector)
ADEL23_Gg3$p2inp1vec <- is.element(ADEL23_Gg3$player2, player1vector)

addPlayer1 <- ADEL23_Gg3[ which(ADEL23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_Gg3[ which(ADEL23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_Gg2 <- rbind(ADEL23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
ADEL23_Gft <- ftable(ADEL23_Gg2$player1, ADEL23_Gg2$player2)
ADEL23_Gft2 <- as.matrix(ADEL23_Gft)
numRows <- nrow(ADEL23_Gft2)
numCols <- ncol(ADEL23_Gft2)
ADEL23_Gft3 <- ADEL23_Gft2[c(2:numRows) , c(2:numCols)]
ADEL23_GTable <- graph.adjacency(ADEL23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(ADEL23_GTable, vertex.label = V(ADEL23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
ADEL23_G.clusterCoef <- transitivity(ADEL23_GTable, type="global") #cluster coefficient
ADEL23_G.degreeCent <- centralization.degree(ADEL23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_Gftn <- as.network.matrix(ADEL23_Gft)
ADEL23_G.netDensity <- network.density(ADEL23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_G.entropy <- entropy(ADEL23_Gft) #entropy

ADEL23_G.netMx <- cbind(ADEL23_G.netMx, ADEL23_G.clusterCoef, ADEL23_G.degreeCent$centralization,
                        ADEL23_G.netDensity, ADEL23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
ADEL23_Bg2 <- data.frame(ADEL23_B)
ADEL23_Bg2 <- ADEL23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_Bg2$player1
player2vector <- ADEL23_Bg2$player2
ADEL23_Bg3 <- ADEL23_Bg2
ADEL23_Bg3$p1inp2vec <- is.element(ADEL23_Bg3$player1, player2vector)
ADEL23_Bg3$p2inp1vec <- is.element(ADEL23_Bg3$player2, player1vector)

addPlayer1 <- ADEL23_Bg3[ which(ADEL23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_Bg3[ which(ADEL23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_Bg2 <- rbind(ADEL23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
ADEL23_Bft <- ftable(ADEL23_Bg2$player1, ADEL23_Bg2$player2)
ADEL23_Bft2 <- as.matrix(ADEL23_Bft)
numRows <- nrow(ADEL23_Bft2)
numCols <- ncol(ADEL23_Bft2)
ADEL23_Bft3 <- ADEL23_Bft2[c(2:numRows) , c(2:numCols)]
ADEL23_BTable <- graph.adjacency(ADEL23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(ADEL23_BTable, vertex.label = V(ADEL23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
ADEL23_B.clusterCoef <- transitivity(ADEL23_BTable, type="global") #cluster coefficient
ADEL23_B.degreeCent <- centralization.degree(ADEL23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_Bftn <- as.network.matrix(ADEL23_Bft)
ADEL23_B.netDensity <- network.density(ADEL23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_B.entropy <- entropy(ADEL23_Bft) #entropy

ADEL23_B.netMx <- cbind(ADEL23_B.netMx, ADEL23_B.clusterCoef, ADEL23_B.degreeCent$centralization,
                        ADEL23_B.netDensity, ADEL23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
ADEL23_SFg2 <- data.frame(ADEL23_SF)
ADEL23_SFg2 <- ADEL23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_SFg2$player1
player2vector <- ADEL23_SFg2$player2
ADEL23_SFg3 <- ADEL23_SFg2
ADEL23_SFg3$p1inp2vec <- is.element(ADEL23_SFg3$player1, player2vector)
ADEL23_SFg3$p2inp1vec <- is.element(ADEL23_SFg3$player2, player1vector)

addPlayer1 <- ADEL23_SFg3[ which(ADEL23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_SFg3[ which(ADEL23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_SFg2 <- rbind(ADEL23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
ADEL23_SFft <- ftable(ADEL23_SFg2$player1, ADEL23_SFg2$player2)
ADEL23_SFft2 <- as.matrix(ADEL23_SFft)
numRows <- nrow(ADEL23_SFft2)
numCols <- ncol(ADEL23_SFft2)
ADEL23_SFft3 <- ADEL23_SFft2[c(2:numRows) , c(2:numCols)]
ADEL23_SFTable <- graph.adjacency(ADEL23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(ADEL23_SFTable, vertex.label = V(ADEL23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
ADEL23_SF.clusterCoef <- transitivity(ADEL23_SFTable, type="global") #cluster coefficient
ADEL23_SF.degreeCent <- centralization.degree(ADEL23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_SFftn <- as.network.matrix(ADEL23_SFft)
ADEL23_SF.netDensity <- network.density(ADEL23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_SF.entropy <- entropy(ADEL23_SFft) #entropy

ADEL23_SF.netMx <- cbind(ADEL23_SF.netMx, ADEL23_SF.clusterCoef, ADEL23_SF.degreeCent$centralization,
                         ADEL23_SF.netDensity, ADEL23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
ADEL23_TFg2 <- data.frame(ADEL23_TF)
ADEL23_TFg2 <- ADEL23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_TFg2$player1
player2vector <- ADEL23_TFg2$player2
ADEL23_TFg3 <- ADEL23_TFg2
ADEL23_TFg3$p1inp2vec <- is.element(ADEL23_TFg3$player1, player2vector)
ADEL23_TFg3$p2inp1vec <- is.element(ADEL23_TFg3$player2, player1vector)

addPlayer1 <- ADEL23_TFg3[ which(ADEL23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_TFg3[ which(ADEL23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_TFg2 <- rbind(ADEL23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
ADEL23_TFft <- ftable(ADEL23_TFg2$player1, ADEL23_TFg2$player2)
ADEL23_TFft2 <- as.matrix(ADEL23_TFft)
numRows <- nrow(ADEL23_TFft2)
numCols <- ncol(ADEL23_TFft2)
ADEL23_TFft3 <- ADEL23_TFft2[c(2:numRows) , c(2:numCols)]
ADEL23_TFTable <- graph.adjacency(ADEL23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(ADEL23_TFTable, vertex.label = V(ADEL23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
ADEL23_TF.clusterCoef <- transitivity(ADEL23_TFTable, type="global") #cluster coefficient
ADEL23_TF.degreeCent <- centralization.degree(ADEL23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_TFftn <- as.network.matrix(ADEL23_TFft)
ADEL23_TF.netDensity <- network.density(ADEL23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_TF.entropy <- entropy(ADEL23_TFft) #entropy

ADEL23_TF.netMx <- cbind(ADEL23_TF.netMx, ADEL23_TF.clusterCoef, ADEL23_TF.degreeCent$centralization,
                         ADEL23_TF.netDensity, ADEL23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************

round = 23
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
ADEL23_SAMg2 <- data.frame(ADEL23_SAM)
ADEL23_SAMg2 <- ADEL23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_SAMg2$player1
player2vector <- ADEL23_SAMg2$player2
ADEL23_SAMg3 <- ADEL23_SAMg2
ADEL23_SAMg3$p1inp2vec <- is.element(ADEL23_SAMg3$player1, player2vector)
ADEL23_SAMg3$p2inp1vec <- is.element(ADEL23_SAMg3$player2, player1vector)

addPlayer1 <- ADEL23_SAMg3[ which(ADEL23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_SAMg3[ which(ADEL23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_SAMg2 <- rbind(ADEL23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
ADEL23_SAMft <- ftable(ADEL23_SAMg2$player1, ADEL23_SAMg2$player2)
ADEL23_SAMft2 <- as.matrix(ADEL23_SAMft)
numRows <- nrow(ADEL23_SAMft2)
numCols <- ncol(ADEL23_SAMft2)
ADEL23_SAMft3 <- ADEL23_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL23_SAMTable <- graph.adjacency(ADEL23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(ADEL23_SAMTable, vertex.label = V(ADEL23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
ADEL23_SAM.clusterCoef <- transitivity(ADEL23_SAMTable, type="global") #cluster coefficient
ADEL23_SAM.degreeCent <- centralization.degree(ADEL23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_SAMftn <- as.network.matrix(ADEL23_SAMft)
ADEL23_SAM.netDensity <- network.density(ADEL23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_SAM.entropy <- entropy(ADEL23_SAMft) #entropy

ADEL23_SAM.netMx <- cbind(ADEL23_SAM.netMx, ADEL23_SAM.clusterCoef, ADEL23_SAM.degreeCent$centralization,
                          ADEL23_SAM.netDensity, ADEL23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
ADEL23_TAMg2 <- data.frame(ADEL23_TAM)
ADEL23_TAMg2 <- ADEL23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_TAMg2$player1
player2vector <- ADEL23_TAMg2$player2
ADEL23_TAMg3 <- ADEL23_TAMg2
ADEL23_TAMg3$p1inp2vec <- is.element(ADEL23_TAMg3$player1, player2vector)
ADEL23_TAMg3$p2inp1vec <- is.element(ADEL23_TAMg3$player2, player1vector)

addPlayer1 <- ADEL23_TAMg3[ which(ADEL23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_TAMg3[ which(ADEL23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_TAMg2 <- rbind(ADEL23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
ADEL23_TAMft <- ftable(ADEL23_TAMg2$player1, ADEL23_TAMg2$player2)
ADEL23_TAMft2 <- as.matrix(ADEL23_TAMft)
numRows <- nrow(ADEL23_TAMft2)
numCols <- ncol(ADEL23_TAMft2)
ADEL23_TAMft3 <- ADEL23_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL23_TAMTable <- graph.adjacency(ADEL23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(ADEL23_TAMTable, vertex.label = V(ADEL23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
ADEL23_TAM.clusterCoef <- transitivity(ADEL23_TAMTable, type="global") #cluster coefficient
ADEL23_TAM.degreeCent <- centralization.degree(ADEL23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_TAMftn <- as.network.matrix(ADEL23_TAMft)
ADEL23_TAM.netDensity <- network.density(ADEL23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_TAM.entropy <- entropy(ADEL23_TAMft) #entropy

ADEL23_TAM.netMx <- cbind(ADEL23_TAM.netMx, ADEL23_TAM.clusterCoef, ADEL23_TAM.degreeCent$centralization,
                          ADEL23_TAM.netDensity, ADEL23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
ADEL23_SDMg2 <- data.frame(ADEL23_SDM)
ADEL23_SDMg2 <- ADEL23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_SDMg2$player1
player2vector <- ADEL23_SDMg2$player2
ADEL23_SDMg3 <- ADEL23_SDMg2
ADEL23_SDMg3$p1inp2vec <- is.element(ADEL23_SDMg3$player1, player2vector)
ADEL23_SDMg3$p2inp1vec <- is.element(ADEL23_SDMg3$player2, player1vector)

addPlayer1 <- ADEL23_SDMg3[ which(ADEL23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_SDMg3[ which(ADEL23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_SDMg2 <- rbind(ADEL23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
ADEL23_SDMft <- ftable(ADEL23_SDMg2$player1, ADEL23_SDMg2$player2)
ADEL23_SDMft2 <- as.matrix(ADEL23_SDMft)
numRows <- nrow(ADEL23_SDMft2)
numCols <- ncol(ADEL23_SDMft2)
ADEL23_SDMft3 <- ADEL23_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL23_SDMTable <- graph.adjacency(ADEL23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(ADEL23_SDMTable, vertex.label = V(ADEL23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
ADEL23_SDM.clusterCoef <- transitivity(ADEL23_SDMTable, type="global") #cluster coefficient
ADEL23_SDM.degreeCent <- centralization.degree(ADEL23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_SDMftn <- as.network.matrix(ADEL23_SDMft)
ADEL23_SDM.netDensity <- network.density(ADEL23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_SDM.entropy <- entropy(ADEL23_SDMft) #entropy

ADEL23_SDM.netMx <- cbind(ADEL23_SDM.netMx, ADEL23_SDM.clusterCoef, ADEL23_SDM.degreeCent$centralization,
                          ADEL23_SDM.netDensity, ADEL23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
ADEL23_TDMg2 <- data.frame(ADEL23_TDM)
ADEL23_TDMg2 <- ADEL23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_TDMg2$player1
player2vector <- ADEL23_TDMg2$player2
ADEL23_TDMg3 <- ADEL23_TDMg2
ADEL23_TDMg3$p1inp2vec <- is.element(ADEL23_TDMg3$player1, player2vector)
ADEL23_TDMg3$p2inp1vec <- is.element(ADEL23_TDMg3$player2, player1vector)

addPlayer1 <- ADEL23_TDMg3[ which(ADEL23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL23_TDMg3[ which(ADEL23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_TDMg2 <- rbind(ADEL23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
ADEL23_TDMft <- ftable(ADEL23_TDMg2$player1, ADEL23_TDMg2$player2)
ADEL23_TDMft2 <- as.matrix(ADEL23_TDMft)
numRows <- nrow(ADEL23_TDMft2)
numCols <- ncol(ADEL23_TDMft2)
ADEL23_TDMft3 <- ADEL23_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL23_TDMTable <- graph.adjacency(ADEL23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(ADEL23_TDMTable, vertex.label = V(ADEL23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
ADEL23_TDM.clusterCoef <- transitivity(ADEL23_TDMTable, type="global") #cluster coefficient
ADEL23_TDM.degreeCent <- centralization.degree(ADEL23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_TDMftn <- as.network.matrix(ADEL23_TDMft)
ADEL23_TDM.netDensity <- network.density(ADEL23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_TDM.entropy <- entropy(ADEL23_TDMft) #entropy

ADEL23_TDM.netMx <- cbind(ADEL23_TDM.netMx, ADEL23_TDM.clusterCoef, ADEL23_TDM.degreeCent$centralization,
                          ADEL23_TDM.netDensity, ADEL23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
ADEL23_SDg2 <- data.frame(ADEL23_SD)
ADEL23_SDg2 <- ADEL23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_SDg2$player1
player2vector <- ADEL23_SDg2$player2
ADEL23_SDg3 <- ADEL23_SDg2
ADEL23_SDg3$p1inp2vec <- is.element(ADEL23_SDg3$player1, player2vector)
ADEL23_SDg3$p2inp1vec <- is.element(ADEL23_SDg3$player2, player1vector)

addPlayer1 <- ADEL23_SDg3[ which(ADEL23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_SDg3[ which(ADEL23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_SDg2 <- rbind(ADEL23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
ADEL23_SDft <- ftable(ADEL23_SDg2$player1, ADEL23_SDg2$player2)
ADEL23_SDft2 <- as.matrix(ADEL23_SDft)
numRows <- nrow(ADEL23_SDft2)
numCols <- ncol(ADEL23_SDft2)
ADEL23_SDft3 <- ADEL23_SDft2[c(2:numRows) , c(2:numCols)]
ADEL23_SDTable <- graph.adjacency(ADEL23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(ADEL23_SDTable, vertex.label = V(ADEL23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
ADEL23_SD.clusterCoef <- transitivity(ADEL23_SDTable, type="global") #cluster coefficient
ADEL23_SD.degreeCent <- centralization.degree(ADEL23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_SDftn <- as.network.matrix(ADEL23_SDft)
ADEL23_SD.netDensity <- network.density(ADEL23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_SD.entropy <- entropy(ADEL23_SDft) #entropy

ADEL23_SD.netMx <- cbind(ADEL23_SD.netMx, ADEL23_SD.clusterCoef, ADEL23_SD.degreeCent$centralization,
                         ADEL23_SD.netDensity, ADEL23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************

round = 23
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
ADEL23_TDg2 <- data.frame(ADEL23_TD)
ADEL23_TDg2 <- ADEL23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_TDg2$player1
player2vector <- ADEL23_TDg2$player2
ADEL23_TDg3 <- ADEL23_TDg2
ADEL23_TDg3$p1inp2vec <- is.element(ADEL23_TDg3$player1, player2vector)
ADEL23_TDg3$p2inp1vec <- is.element(ADEL23_TDg3$player2, player1vector)

addPlayer1 <- ADEL23_TDg3[ which(ADEL23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_TDg3[ which(ADEL23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_TDg2 <- rbind(ADEL23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
ADEL23_TDft <- ftable(ADEL23_TDg2$player1, ADEL23_TDg2$player2)
ADEL23_TDft2 <- as.matrix(ADEL23_TDft)
numRows <- nrow(ADEL23_TDft2)
numCols <- ncol(ADEL23_TDft2)
ADEL23_TDft3 <- ADEL23_TDft2[c(2:numRows) , c(2:numCols)]
ADEL23_TDTable <- graph.adjacency(ADEL23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(ADEL23_TDTable, vertex.label = V(ADEL23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
ADEL23_TD.clusterCoef <- transitivity(ADEL23_TDTable, type="global") #cluster coefficient
ADEL23_TD.degreeCent <- centralization.degree(ADEL23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_TDftn <- as.network.matrix(ADEL23_TDft)
ADEL23_TD.netDensity <- network.density(ADEL23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_TD.entropy <- entropy(ADEL23_TDft) #entropy

ADEL23_TD.netMx <- cbind(ADEL23_TD.netMx, ADEL23_TD.clusterCoef, ADEL23_TD.degreeCent$centralization,
                         ADEL23_TD.netDensity, ADEL23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
ADEL23_QTg2 <- data.frame(ADEL23_QT)
ADEL23_QTg2 <- ADEL23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL23_QTg2$player1
player2vector <- ADEL23_QTg2$player2
ADEL23_QTg3 <- ADEL23_QTg2
ADEL23_QTg3$p1inp2vec <- is.element(ADEL23_QTg3$player1, player2vector)
ADEL23_QTg3$p2inp1vec <- is.element(ADEL23_QTg3$player2, player1vector)

addPlayer1 <- ADEL23_QTg3[ which(ADEL23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL23_QTg3[ which(ADEL23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL23_QTg2 <- rbind(ADEL23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
ADEL23_QTft <- ftable(ADEL23_QTg2$player1, ADEL23_QTg2$player2)
ADEL23_QTft2 <- as.matrix(ADEL23_QTft)
numRows <- nrow(ADEL23_QTft2)
numCols <- ncol(ADEL23_QTft2)
ADEL23_QTft3 <- ADEL23_QTft2[c(2:numRows) , c(2:numCols)]
ADEL23_QTTable <- graph.adjacency(ADEL23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(ADEL23_QTTable, vertex.label = V(ADEL23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
ADEL23_QT.clusterCoef <- transitivity(ADEL23_QTTable, type="global") #cluster coefficient
ADEL23_QT.degreeCent <- centralization.degree(ADEL23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL23_QTftn <- as.network.matrix(ADEL23_QTft)
ADEL23_QT.netDensity <- network.density(ADEL23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL23_QT.entropy <- entropy(ADEL23_QTft) #entropy

ADEL23_QT.netMx <- cbind(ADEL23_QT.netMx, ADEL23_QT.clusterCoef, ADEL23_QT.degreeCent$centralization,
                         ADEL23_QT.netDensity, ADEL23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL23_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "BL"
KIoutcome = "Goal_F"
BL23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
BL23_Gg2 <- data.frame(BL23_G)
BL23_Gg2 <- BL23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_Gg2$player1
player2vector <- BL23_Gg2$player2
BL23_Gg3 <- BL23_Gg2
BL23_Gg3$p1inp2vec <- is.element(BL23_Gg3$player1, player2vector)
BL23_Gg3$p2inp1vec <- is.element(BL23_Gg3$player2, player1vector)

addPlayer1 <- BL23_Gg3[ which(BL23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_Gg3[ which(BL23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_Gg2 <- rbind(BL23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
BL23_Gft <- ftable(BL23_Gg2$player1, BL23_Gg2$player2)
BL23_Gft2 <- as.matrix(BL23_Gft)
numRows <- nrow(BL23_Gft2)
numCols <- ncol(BL23_Gft2)
BL23_Gft3 <- BL23_Gft2[c(2:numRows) , c(2:numCols)]
BL23_GTable <- graph.adjacency(BL23_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(BL23_GTable, vertex.label = V(BL23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
BL23_G.clusterCoef <- transitivity(BL23_GTable, type="global") #cluster coefficient
BL23_G.degreeCent <- centralization.degree(BL23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_Gftn <- as.network.matrix(BL23_Gft)
BL23_G.netDensity <- network.density(BL23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_G.entropy <- entropy(BL23_Gft) #entropy

BL23_G.netMx <- cbind(BL23_G.netMx, BL23_G.clusterCoef, BL23_G.degreeCent$centralization,
                      BL23_G.netDensity, BL23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Behind_F"
BL23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
BL23_Bg2 <- data.frame(BL23_B)
BL23_Bg2 <- BL23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_Bg2$player1
player2vector <- BL23_Bg2$player2
BL23_Bg3 <- BL23_Bg2
BL23_Bg3$p1inp2vec <- is.element(BL23_Bg3$player1, player2vector)
BL23_Bg3$p2inp1vec <- is.element(BL23_Bg3$player2, player1vector)

addPlayer1 <- BL23_Bg3[ which(BL23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_Bg3[ which(BL23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_Bg2 <- rbind(BL23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
BL23_Bft <- ftable(BL23_Bg2$player1, BL23_Bg2$player2)
BL23_Bft2 <- as.matrix(BL23_Bft)
numRows <- nrow(BL23_Bft2)
numCols <- ncol(BL23_Bft2)
BL23_Bft3 <- BL23_Bft2[c(2:numRows) , c(2:numCols)]
BL23_BTable <- graph.adjacency(BL23_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(BL23_BTable, vertex.label = V(BL23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
BL23_B.clusterCoef <- transitivity(BL23_BTable, type="global") #cluster coefficient
BL23_B.degreeCent <- centralization.degree(BL23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_Bftn <- as.network.matrix(BL23_Bft)
BL23_B.netDensity <- network.density(BL23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_B.entropy <- entropy(BL23_Bft) #entropy

BL23_B.netMx <- cbind(BL23_B.netMx, BL23_B.clusterCoef, BL23_B.degreeCent$centralization,
                      BL23_B.netDensity, BL23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Stoppage_F"
BL23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
BL23_SFg2 <- data.frame(BL23_SF)
BL23_SFg2 <- BL23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_SFg2$player1
player2vector <- BL23_SFg2$player2
BL23_SFg3 <- BL23_SFg2
BL23_SFg3$p1inp2vec <- is.element(BL23_SFg3$player1, player2vector)
BL23_SFg3$p2inp1vec <- is.element(BL23_SFg3$player2, player1vector)

addPlayer1 <- BL23_SFg3[ which(BL23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_SFg3[ which(BL23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_SFg2 <- rbind(BL23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
BL23_SFft <- ftable(BL23_SFg2$player1, BL23_SFg2$player2)
BL23_SFft2 <- as.matrix(BL23_SFft)
numRows <- nrow(BL23_SFft2)
numCols <- ncol(BL23_SFft2)
BL23_SFft3 <- BL23_SFft2[c(2:numRows) , c(2:numCols)]
BL23_SFTable <- graph.adjacency(BL23_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(BL23_SFTable, vertex.label = V(BL23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
BL23_SF.clusterCoef <- transitivity(BL23_SFTable, type="global") #cluster coefficient
BL23_SF.degreeCent <- centralization.degree(BL23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_SFftn <- as.network.matrix(BL23_SFft)
BL23_SF.netDensity <- network.density(BL23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_SF.entropy <- entropy(BL23_SFft) #entropy

BL23_SF.netMx <- cbind(BL23_SF.netMx, BL23_SF.clusterCoef, BL23_SF.degreeCent$centralization,
                       BL23_SF.netDensity, BL23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Turnover_F"
BL23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
BL23_TFg2 <- data.frame(BL23_TF)
BL23_TFg2 <- BL23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_TFg2$player1
player2vector <- BL23_TFg2$player2
BL23_TFg3 <- BL23_TFg2
BL23_TFg3$p1inp2vec <- is.element(BL23_TFg3$player1, player2vector)
BL23_TFg3$p2inp1vec <- is.element(BL23_TFg3$player2, player1vector)

addPlayer1 <- BL23_TFg3[ which(BL23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_TFg3[ which(BL23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_TFg2 <- rbind(BL23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
BL23_TFft <- ftable(BL23_TFg2$player1, BL23_TFg2$player2)
BL23_TFft2 <- as.matrix(BL23_TFft)
numRows <- nrow(BL23_TFft2)
numCols <- ncol(BL23_TFft2)
BL23_TFft3 <- BL23_TFft2[c(2:numRows) , c(2:numCols)]
BL23_TFTable <- graph.adjacency(BL23_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(BL23_TFTable, vertex.label = V(BL23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
BL23_TF.clusterCoef <- transitivity(BL23_TFTable, type="global") #cluster coefficient
BL23_TF.degreeCent <- centralization.degree(BL23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_TFftn <- as.network.matrix(BL23_TFft)
BL23_TF.netDensity <- network.density(BL23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_TF.entropy <- entropy(BL23_TFft) #entropy

BL23_TF.netMx <- cbind(BL23_TF.netMx, BL23_TF.clusterCoef, BL23_TF.degreeCent$centralization,
                       BL23_TF.netDensity, BL23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
BL23_SAMg2 <- data.frame(BL23_SAM)
BL23_SAMg2 <- BL23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_SAMg2$player1
player2vector <- BL23_SAMg2$player2
BL23_SAMg3 <- BL23_SAMg2
BL23_SAMg3$p1inp2vec <- is.element(BL23_SAMg3$player1, player2vector)
BL23_SAMg3$p2inp1vec <- is.element(BL23_SAMg3$player2, player1vector)

addPlayer1 <- BL23_SAMg3[ which(BL23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_SAMg3[ which(BL23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_SAMg2 <- rbind(BL23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
BL23_SAMft <- ftable(BL23_SAMg2$player1, BL23_SAMg2$player2)
BL23_SAMft2 <- as.matrix(BL23_SAMft)
numRows <- nrow(BL23_SAMft2)
numCols <- ncol(BL23_SAMft2)
BL23_SAMft3 <- BL23_SAMft2[c(2:numRows) , c(2:numCols)]
BL23_SAMTable <- graph.adjacency(BL23_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(BL23_SAMTable, vertex.label = V(BL23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
BL23_SAM.clusterCoef <- transitivity(BL23_SAMTable, type="global") #cluster coefficient
BL23_SAM.degreeCent <- centralization.degree(BL23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_SAMftn <- as.network.matrix(BL23_SAMft)
BL23_SAM.netDensity <- network.density(BL23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_SAM.entropy <- entropy(BL23_SAMft) #entropy

BL23_SAM.netMx <- cbind(BL23_SAM.netMx, BL23_SAM.clusterCoef, BL23_SAM.degreeCent$centralization,
                        BL23_SAM.netDensity, BL23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Turnover_AM"
BL23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
BL23_TAMg2 <- data.frame(BL23_TAM)
BL23_TAMg2 <- BL23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_TAMg2$player1
player2vector <- BL23_TAMg2$player2
BL23_TAMg3 <- BL23_TAMg2
BL23_TAMg3$p1inp2vec <- is.element(BL23_TAMg3$player1, player2vector)
BL23_TAMg3$p2inp1vec <- is.element(BL23_TAMg3$player2, player1vector)

addPlayer1 <- BL23_TAMg3[ which(BL23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_TAMg3[ which(BL23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_TAMg2 <- rbind(BL23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
BL23_TAMft <- ftable(BL23_TAMg2$player1, BL23_TAMg2$player2)
BL23_TAMft2 <- as.matrix(BL23_TAMft)
numRows <- nrow(BL23_TAMft2)
numCols <- ncol(BL23_TAMft2)
BL23_TAMft3 <- BL23_TAMft2[c(2:numRows) , c(2:numCols)]
BL23_TAMTable <- graph.adjacency(BL23_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(BL23_TAMTable, vertex.label = V(BL23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
BL23_TAM.clusterCoef <- transitivity(BL23_TAMTable, type="global") #cluster coefficient
BL23_TAM.degreeCent <- centralization.degree(BL23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_TAMftn <- as.network.matrix(BL23_TAMft)
BL23_TAM.netDensity <- network.density(BL23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_TAM.entropy <- entropy(BL23_TAMft) #entropy

BL23_TAM.netMx <- cbind(BL23_TAM.netMx, BL23_TAM.clusterCoef, BL23_TAM.degreeCent$centralization,
                        BL23_TAM.netDensity, BL23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
BL23_SDMg2 <- data.frame(BL23_SDM)
BL23_SDMg2 <- BL23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_SDMg2$player1
player2vector <- BL23_SDMg2$player2
BL23_SDMg3 <- BL23_SDMg2
BL23_SDMg3$p1inp2vec <- is.element(BL23_SDMg3$player1, player2vector)
BL23_SDMg3$p2inp1vec <- is.element(BL23_SDMg3$player2, player1vector)

addPlayer1 <- BL23_SDMg3[ which(BL23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_SDMg3[ which(BL23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_SDMg2 <- rbind(BL23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
BL23_SDMft <- ftable(BL23_SDMg2$player1, BL23_SDMg2$player2)
BL23_SDMft2 <- as.matrix(BL23_SDMft)
numRows <- nrow(BL23_SDMft2)
numCols <- ncol(BL23_SDMft2)
BL23_SDMft3 <- BL23_SDMft2[c(2:numRows) , c(2:numCols)]
BL23_SDMTable <- graph.adjacency(BL23_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(BL23_SDMTable, vertex.label = V(BL23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
BL23_SDM.clusterCoef <- transitivity(BL23_SDMTable, type="global") #cluster coefficient
BL23_SDM.degreeCent <- centralization.degree(BL23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_SDMftn <- as.network.matrix(BL23_SDMft)
BL23_SDM.netDensity <- network.density(BL23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_SDM.entropy <- entropy(BL23_SDMft) #entropy

BL23_SDM.netMx <- cbind(BL23_SDM.netMx, BL23_SDM.clusterCoef, BL23_SDM.degreeCent$centralization,
                        BL23_SDM.netDensity, BL23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "BL"
KIoutcome = "Turnover_DM"
BL23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
BL23_TDMg2 <- data.frame(BL23_TDM)
BL23_TDMg2 <- BL23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_TDMg2$player1
player2vector <- BL23_TDMg2$player2
BL23_TDMg3 <- BL23_TDMg2
BL23_TDMg3$p1inp2vec <- is.element(BL23_TDMg3$player1, player2vector)
BL23_TDMg3$p2inp1vec <- is.element(BL23_TDMg3$player2, player1vector)

addPlayer1 <- BL23_TDMg3[ which(BL23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL23_TDMg3[ which(BL23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_TDMg2 <- rbind(BL23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
BL23_TDMft <- ftable(BL23_TDMg2$player1, BL23_TDMg2$player2)
BL23_TDMft2 <- as.matrix(BL23_TDMft)
numRows <- nrow(BL23_TDMft2)
numCols <- ncol(BL23_TDMft2)
BL23_TDMft3 <- BL23_TDMft2[c(2:numRows) , c(2:numCols)]
BL23_TDMTable <- graph.adjacency(BL23_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(BL23_TDMTable, vertex.label = V(BL23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
BL23_TDM.clusterCoef <- transitivity(BL23_TDMTable, type="global") #cluster coefficient
BL23_TDM.degreeCent <- centralization.degree(BL23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_TDMftn <- as.network.matrix(BL23_TDMft)
BL23_TDM.netDensity <- network.density(BL23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_TDM.entropy <- entropy(BL23_TDMft) #entropy

BL23_TDM.netMx <- cbind(BL23_TDM.netMx, BL23_TDM.clusterCoef, BL23_TDM.degreeCent$centralization,
                        BL23_TDM.netDensity, BL23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "Stoppage_D"
BL23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
BL23_SDg2 <- data.frame(BL23_SD)
BL23_SDg2 <- BL23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_SDg2$player1
player2vector <- BL23_SDg2$player2
BL23_SDg3 <- BL23_SDg2
BL23_SDg3$p1inp2vec <- is.element(BL23_SDg3$player1, player2vector)
BL23_SDg3$p2inp1vec <- is.element(BL23_SDg3$player2, player1vector)

addPlayer1 <- BL23_SDg3[ which(BL23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_SDg3[ which(BL23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_SDg2 <- rbind(BL23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
BL23_SDft <- ftable(BL23_SDg2$player1, BL23_SDg2$player2)
BL23_SDft2 <- as.matrix(BL23_SDft)
numRows <- nrow(BL23_SDft2)
numCols <- ncol(BL23_SDft2)
BL23_SDft3 <- BL23_SDft2[c(2:numRows) , c(2:numCols)]
BL23_SDTable <- graph.adjacency(BL23_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(BL23_SDTable, vertex.label = V(BL23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
BL23_SD.clusterCoef <- transitivity(BL23_SDTable, type="global") #cluster coefficient
BL23_SD.degreeCent <- centralization.degree(BL23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_SDftn <- as.network.matrix(BL23_SDft)
BL23_SD.netDensity <- network.density(BL23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_SD.entropy <- entropy(BL23_SDft) #entropy

BL23_SD.netMx <- cbind(BL23_SD.netMx, BL23_SD.clusterCoef, BL23_SD.degreeCent$centralization,
                       BL23_SD.netDensity, BL23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************

round = 23
teamName = "BL"
KIoutcome = "Turnover_D"
BL23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
BL23_TDg2 <- data.frame(BL23_TD)
BL23_TDg2 <- BL23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_TDg2$player1
player2vector <- BL23_TDg2$player2
BL23_TDg3 <- BL23_TDg2
BL23_TDg3$p1inp2vec <- is.element(BL23_TDg3$player1, player2vector)
BL23_TDg3$p2inp1vec <- is.element(BL23_TDg3$player2, player1vector)

addPlayer1 <- BL23_TDg3[ which(BL23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_TDg3[ which(BL23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_TDg2 <- rbind(BL23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
BL23_TDft <- ftable(BL23_TDg2$player1, BL23_TDg2$player2)
BL23_TDft2 <- as.matrix(BL23_TDft)
numRows <- nrow(BL23_TDft2)
numCols <- ncol(BL23_TDft2)
BL23_TDft3 <- BL23_TDft2[c(2:numRows) , c(2:numCols)]
BL23_TDTable <- graph.adjacency(BL23_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(BL23_TDTable, vertex.label = V(BL23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
BL23_TD.clusterCoef <- transitivity(BL23_TDTable, type="global") #cluster coefficient
BL23_TD.degreeCent <- centralization.degree(BL23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_TDftn <- as.network.matrix(BL23_TDft)
BL23_TD.netDensity <- network.density(BL23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_TD.entropy <- entropy(BL23_TDft) #entropy

BL23_TD.netMx <- cbind(BL23_TD.netMx, BL23_TD.clusterCoef, BL23_TD.degreeCent$centralization,
                       BL23_TD.netDensity, BL23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
BL23_QTg2 <- data.frame(BL23_QT)
BL23_QTg2 <- BL23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL23_QTg2$player1
player2vector <- BL23_QTg2$player2
BL23_QTg3 <- BL23_QTg2
BL23_QTg3$p1inp2vec <- is.element(BL23_QTg3$player1, player2vector)
BL23_QTg3$p2inp1vec <- is.element(BL23_QTg3$player2, player1vector)

addPlayer1 <- BL23_QTg3[ which(BL23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL23_QTg3[ which(BL23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL23_QTg2 <- rbind(BL23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
BL23_QTft <- ftable(BL23_QTg2$player1, BL23_QTg2$player2)
BL23_QTft2 <- as.matrix(BL23_QTft)
numRows <- nrow(BL23_QTft2)
numCols <- ncol(BL23_QTft2)
BL23_QTft3 <- BL23_QTft2[c(2:numRows) , c(2:numCols)]
BL23_QTTable <- graph.adjacency(BL23_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(BL23_QTTable, vertex.label = V(BL23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
BL23_QT.clusterCoef <- transitivity(BL23_QTTable, type="global") #cluster coefficient
BL23_QT.degreeCent <- centralization.degree(BL23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL23_QTftn <- as.network.matrix(BL23_QTft)
BL23_QT.netDensity <- network.density(BL23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL23_QT.entropy <- entropy(BL23_QTft) #entropy

BL23_QT.netMx <- cbind(BL23_QT.netMx, BL23_QT.clusterCoef, BL23_QT.degreeCent$centralization,
                       BL23_QT.netDensity, BL23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL23_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "CARL"
KIoutcome = "Goal_F"
CARL23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
CARL23_Gg2 <- data.frame(CARL23_G)
CARL23_Gg2 <- CARL23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_Gg2$player1
player2vector <- CARL23_Gg2$player2
CARL23_Gg3 <- CARL23_Gg2
CARL23_Gg3$p1inp2vec <- is.element(CARL23_Gg3$player1, player2vector)
CARL23_Gg3$p2inp1vec <- is.element(CARL23_Gg3$player2, player1vector)

addPlayer1 <- CARL23_Gg3[ which(CARL23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_Gg3[ which(CARL23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_Gg2 <- rbind(CARL23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
CARL23_Gft <- ftable(CARL23_Gg2$player1, CARL23_Gg2$player2)
CARL23_Gft2 <- as.matrix(CARL23_Gft)
numRows <- nrow(CARL23_Gft2)
numCols <- ncol(CARL23_Gft2)
CARL23_Gft3 <- CARL23_Gft2[c(2:numRows) , c(2:numCols)]
CARL23_GTable <- graph.adjacency(CARL23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(CARL23_GTable, vertex.label = V(CARL23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
CARL23_G.clusterCoef <- transitivity(CARL23_GTable, type="global") #cluster coefficient
CARL23_G.degreeCent <- centralization.degree(CARL23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_Gftn <- as.network.matrix(CARL23_Gft)
CARL23_G.netDensity <- network.density(CARL23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_G.entropy <- entropy(CARL23_Gft) #entropy

CARL23_G.netMx <- cbind(CARL23_G.netMx, CARL23_G.clusterCoef, CARL23_G.degreeCent$centralization,
                        CARL23_G.netDensity, CARL23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "CARL"
KIoutcome = "Behind_F"
CARL23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
CARL23_Bg2 <- data.frame(CARL23_B)
CARL23_Bg2 <- CARL23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_Bg2$player1
player2vector <- CARL23_Bg2$player2
CARL23_Bg3 <- CARL23_Bg2
CARL23_Bg3$p1inp2vec <- is.element(CARL23_Bg3$player1, player2vector)
CARL23_Bg3$p2inp1vec <- is.element(CARL23_Bg3$player2, player1vector)

addPlayer1 <- CARL23_Bg3[ which(CARL23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_Bg3[ which(CARL23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_Bg2 <- rbind(CARL23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
CARL23_Bft <- ftable(CARL23_Bg2$player1, CARL23_Bg2$player2)
CARL23_Bft2 <- as.matrix(CARL23_Bft)
numRows <- nrow(CARL23_Bft2)
numCols <- ncol(CARL23_Bft2)
CARL23_Bft3 <- CARL23_Bft2[c(2:numRows) , c(2:numCols)]
CARL23_BTable <- graph.adjacency(CARL23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(CARL23_BTable, vertex.label = V(CARL23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
CARL23_B.clusterCoef <- transitivity(CARL23_BTable, type="global") #cluster coefficient
CARL23_B.degreeCent <- centralization.degree(CARL23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_Bftn <- as.network.matrix(CARL23_Bft)
CARL23_B.netDensity <- network.density(CARL23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_B.entropy <- entropy(CARL23_Bft) #entropy

CARL23_B.netMx <- cbind(CARL23_B.netMx, CARL23_B.clusterCoef, CARL23_B.degreeCent$centralization,
                        CARL23_B.netDensity, CARL23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
CARL23_SFg2 <- data.frame(CARL23_SF)
CARL23_SFg2 <- CARL23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_SFg2$player1
player2vector <- CARL23_SFg2$player2
CARL23_SFg3 <- CARL23_SFg2
CARL23_SFg3$p1inp2vec <- is.element(CARL23_SFg3$player1, player2vector)
CARL23_SFg3$p2inp1vec <- is.element(CARL23_SFg3$player2, player1vector)

addPlayer1 <- CARL23_SFg3[ which(CARL23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_SFg3[ which(CARL23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_SFg2 <- rbind(CARL23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
CARL23_SFft <- ftable(CARL23_SFg2$player1, CARL23_SFg2$player2)
CARL23_SFft2 <- as.matrix(CARL23_SFft)
numRows <- nrow(CARL23_SFft2)
numCols <- ncol(CARL23_SFft2)
CARL23_SFft3 <- CARL23_SFft2[c(2:numRows) , c(2:numCols)]
CARL23_SFTable <- graph.adjacency(CARL23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(CARL23_SFTable, vertex.label = V(CARL23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
CARL23_SF.clusterCoef <- transitivity(CARL23_SFTable, type="global") #cluster coefficient
CARL23_SF.degreeCent <- centralization.degree(CARL23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_SFftn <- as.network.matrix(CARL23_SFft)
CARL23_SF.netDensity <- network.density(CARL23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_SF.entropy <- entropy(CARL23_SFft) #entropy

CARL23_SF.netMx <- cbind(CARL23_SF.netMx, CARL23_SF.clusterCoef, CARL23_SF.degreeCent$centralization,
                         CARL23_SF.netDensity, CARL23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
CARL23_TFg2 <- data.frame(CARL23_TF)
CARL23_TFg2 <- CARL23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_TFg2$player1
player2vector <- CARL23_TFg2$player2
CARL23_TFg3 <- CARL23_TFg2
CARL23_TFg3$p1inp2vec <- is.element(CARL23_TFg3$player1, player2vector)
CARL23_TFg3$p2inp1vec <- is.element(CARL23_TFg3$player2, player1vector)

addPlayer1 <- CARL23_TFg3[ which(CARL23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_TFg3[ which(CARL23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_TFg2 <- rbind(CARL23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
CARL23_TFft <- ftable(CARL23_TFg2$player1, CARL23_TFg2$player2)
CARL23_TFft2 <- as.matrix(CARL23_TFft)
numRows <- nrow(CARL23_TFft2)
numCols <- ncol(CARL23_TFft2)
CARL23_TFft3 <- CARL23_TFft2[c(2:numRows) , c(2:numCols)]
CARL23_TFTable <- graph.adjacency(CARL23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(CARL23_TFTable, vertex.label = V(CARL23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
CARL23_TF.clusterCoef <- transitivity(CARL23_TFTable, type="global") #cluster coefficient
CARL23_TF.degreeCent <- centralization.degree(CARL23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_TFftn <- as.network.matrix(CARL23_TFft)
CARL23_TF.netDensity <- network.density(CARL23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_TF.entropy <- entropy(CARL23_TFft) #entropy

CARL23_TF.netMx <- cbind(CARL23_TF.netMx, CARL23_TF.clusterCoef, CARL23_TF.degreeCent$centralization,
                         CARL23_TF.netDensity, CARL23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************

round = 23
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
CARL23_SAMg2 <- data.frame(CARL23_SAM)
CARL23_SAMg2 <- CARL23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_SAMg2$player1
player2vector <- CARL23_SAMg2$player2
CARL23_SAMg3 <- CARL23_SAMg2
CARL23_SAMg3$p1inp2vec <- is.element(CARL23_SAMg3$player1, player2vector)
CARL23_SAMg3$p2inp1vec <- is.element(CARL23_SAMg3$player2, player1vector)

addPlayer1 <- CARL23_SAMg3[ which(CARL23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_SAMg3[ which(CARL23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_SAMg2 <- rbind(CARL23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
CARL23_SAMft <- ftable(CARL23_SAMg2$player1, CARL23_SAMg2$player2)
CARL23_SAMft2 <- as.matrix(CARL23_SAMft)
numRows <- nrow(CARL23_SAMft2)
numCols <- ncol(CARL23_SAMft2)
CARL23_SAMft3 <- CARL23_SAMft2[c(2:numRows) , c(2:numCols)]
CARL23_SAMTable <- graph.adjacency(CARL23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(CARL23_SAMTable, vertex.label = V(CARL23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
CARL23_SAM.clusterCoef <- transitivity(CARL23_SAMTable, type="global") #cluster coefficient
CARL23_SAM.degreeCent <- centralization.degree(CARL23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_SAMftn <- as.network.matrix(CARL23_SAMft)
CARL23_SAM.netDensity <- network.density(CARL23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_SAM.entropy <- entropy(CARL23_SAMft) #entropy

CARL23_SAM.netMx <- cbind(CARL23_SAM.netMx, CARL23_SAM.clusterCoef, CARL23_SAM.degreeCent$centralization,
                          CARL23_SAM.netDensity, CARL23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
CARL23_TAMg2 <- data.frame(CARL23_TAM)
CARL23_TAMg2 <- CARL23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_TAMg2$player1
player2vector <- CARL23_TAMg2$player2
CARL23_TAMg3 <- CARL23_TAMg2
CARL23_TAMg3$p1inp2vec <- is.element(CARL23_TAMg3$player1, player2vector)
CARL23_TAMg3$p2inp1vec <- is.element(CARL23_TAMg3$player2, player1vector)

addPlayer1 <- CARL23_TAMg3[ which(CARL23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_TAMg3[ which(CARL23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_TAMg2 <- rbind(CARL23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
CARL23_TAMft <- ftable(CARL23_TAMg2$player1, CARL23_TAMg2$player2)
CARL23_TAMft2 <- as.matrix(CARL23_TAMft)
numRows <- nrow(CARL23_TAMft2)
numCols <- ncol(CARL23_TAMft2)
CARL23_TAMft3 <- CARL23_TAMft2[c(2:numRows) , c(2:numCols)]
CARL23_TAMTable <- graph.adjacency(CARL23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(CARL23_TAMTable, vertex.label = V(CARL23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
CARL23_TAM.clusterCoef <- transitivity(CARL23_TAMTable, type="global") #cluster coefficient
CARL23_TAM.degreeCent <- centralization.degree(CARL23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_TAMftn <- as.network.matrix(CARL23_TAMft)
CARL23_TAM.netDensity <- network.density(CARL23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_TAM.entropy <- entropy(CARL23_TAMft) #entropy

CARL23_TAM.netMx <- cbind(CARL23_TAM.netMx, CARL23_TAM.clusterCoef, CARL23_TAM.degreeCent$centralization,
                          CARL23_TAM.netDensity, CARL23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
CARL23_SDMg2 <- data.frame(CARL23_SDM)
CARL23_SDMg2 <- CARL23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_SDMg2$player1
player2vector <- CARL23_SDMg2$player2
CARL23_SDMg3 <- CARL23_SDMg2
CARL23_SDMg3$p1inp2vec <- is.element(CARL23_SDMg3$player1, player2vector)
CARL23_SDMg3$p2inp1vec <- is.element(CARL23_SDMg3$player2, player1vector)

addPlayer1 <- CARL23_SDMg3[ which(CARL23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_SDMg3[ which(CARL23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_SDMg2 <- rbind(CARL23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
CARL23_SDMft <- ftable(CARL23_SDMg2$player1, CARL23_SDMg2$player2)
CARL23_SDMft2 <- as.matrix(CARL23_SDMft)
numRows <- nrow(CARL23_SDMft2)
numCols <- ncol(CARL23_SDMft2)
CARL23_SDMft3 <- CARL23_SDMft2[c(2:numRows) , c(2:numCols)]
CARL23_SDMTable <- graph.adjacency(CARL23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(CARL23_SDMTable, vertex.label = V(CARL23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
CARL23_SDM.clusterCoef <- transitivity(CARL23_SDMTable, type="global") #cluster coefficient
CARL23_SDM.degreeCent <- centralization.degree(CARL23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_SDMftn <- as.network.matrix(CARL23_SDMft)
CARL23_SDM.netDensity <- network.density(CARL23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_SDM.entropy <- entropy(CARL23_SDMft) #entropy

CARL23_SDM.netMx <- cbind(CARL23_SDM.netMx, CARL23_SDM.clusterCoef, CARL23_SDM.degreeCent$centralization,
                          CARL23_SDM.netDensity, CARL23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************
#NA

round = 23
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
CARL23_TDMg2 <- data.frame(CARL23_TDM)
CARL23_TDMg2 <- CARL23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_TDMg2$player1
player2vector <- CARL23_TDMg2$player2
CARL23_TDMg3 <- CARL23_TDMg2
CARL23_TDMg3$p1inp2vec <- is.element(CARL23_TDMg3$player1, player2vector)
CARL23_TDMg3$p2inp1vec <- is.element(CARL23_TDMg3$player2, player1vector)

addPlayer1 <- CARL23_TDMg3[ which(CARL23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_TDMg3[ which(CARL23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_TDMg2 <- rbind(CARL23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
CARL23_TDMft <- ftable(CARL23_TDMg2$player1, CARL23_TDMg2$player2)
CARL23_TDMft2 <- as.matrix(CARL23_TDMft)
numRows <- nrow(CARL23_TDMft2)
numCols <- ncol(CARL23_TDMft2)
CARL23_TDMft3 <- CARL23_TDMft2[c(2:numRows) , c(2:numCols)]
CARL23_TDMTable <- graph.adjacency(CARL23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(CARL23_TDMTable, vertex.label = V(CARL23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
CARL23_TDM.clusterCoef <- transitivity(CARL23_TDMTable, type="global") #cluster coefficient
CARL23_TDM.degreeCent <- centralization.degree(CARL23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_TDMftn <- as.network.matrix(CARL23_TDMft)
CARL23_TDM.netDensity <- network.density(CARL23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_TDM.entropy <- entropy(CARL23_TDMft) #entropy

CARL23_TDM.netMx <- cbind(CARL23_TDM.netMx, CARL23_TDM.clusterCoef, CARL23_TDM.degreeCent$centralization,
                          CARL23_TDM.netDensity, CARL23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
CARL23_SDg2 <- data.frame(CARL23_SD)
CARL23_SDg2 <- CARL23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_SDg2$player1
player2vector <- CARL23_SDg2$player2
CARL23_SDg3 <- CARL23_SDg2
CARL23_SDg3$p1inp2vec <- is.element(CARL23_SDg3$player1, player2vector)
CARL23_SDg3$p2inp1vec <- is.element(CARL23_SDg3$player2, player1vector)

addPlayer1 <- CARL23_SDg3[ which(CARL23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_SDg3[ which(CARL23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_SDg2 <- rbind(CARL23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
CARL23_SDft <- ftable(CARL23_SDg2$player1, CARL23_SDg2$player2)
CARL23_SDft2 <- as.matrix(CARL23_SDft)
numRows <- nrow(CARL23_SDft2)
numCols <- ncol(CARL23_SDft2)
CARL23_SDft3 <- CARL23_SDft2[c(2:numRows) , c(2:numCols)]
CARL23_SDTable <- graph.adjacency(CARL23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(CARL23_SDTable, vertex.label = V(CARL23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
CARL23_SD.clusterCoef <- transitivity(CARL23_SDTable, type="global") #cluster coefficient
CARL23_SD.degreeCent <- centralization.degree(CARL23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_SDftn <- as.network.matrix(CARL23_SDft)
CARL23_SD.netDensity <- network.density(CARL23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_SD.entropy <- entropy(CARL23_SDft) #entropy

CARL23_SD.netMx <- cbind(CARL23_SD.netMx, CARL23_SD.clusterCoef, CARL23_SD.degreeCent$centralization,
                         CARL23_SD.netDensity, CARL23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************

round = 23
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
CARL23_TDg2 <- data.frame(CARL23_TD)
CARL23_TDg2 <- CARL23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_TDg2$player1
player2vector <- CARL23_TDg2$player2
CARL23_TDg3 <- CARL23_TDg2
CARL23_TDg3$p1inp2vec <- is.element(CARL23_TDg3$player1, player2vector)
CARL23_TDg3$p2inp1vec <- is.element(CARL23_TDg3$player2, player1vector)

addPlayer1 <- CARL23_TDg3[ which(CARL23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_TDg3[ which(CARL23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_TDg2 <- rbind(CARL23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
CARL23_TDft <- ftable(CARL23_TDg2$player1, CARL23_TDg2$player2)
CARL23_TDft2 <- as.matrix(CARL23_TDft)
numRows <- nrow(CARL23_TDft2)
numCols <- ncol(CARL23_TDft2)
CARL23_TDft3 <- CARL23_TDft2[c(2:numRows) , c(2:numCols)]
CARL23_TDTable <- graph.adjacency(CARL23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(CARL23_TDTable, vertex.label = V(CARL23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
CARL23_TD.clusterCoef <- transitivity(CARL23_TDTable, type="global") #cluster coefficient
CARL23_TD.degreeCent <- centralization.degree(CARL23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_TDftn <- as.network.matrix(CARL23_TDft)
CARL23_TD.netDensity <- network.density(CARL23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_TD.entropy <- entropy(CARL23_TDft) #entropy

CARL23_TD.netMx <- cbind(CARL23_TD.netMx, CARL23_TD.clusterCoef, CARL23_TD.degreeCent$centralization,
                         CARL23_TD.netDensity, CARL23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
CARL23_QTg2 <- data.frame(CARL23_QT)
CARL23_QTg2 <- CARL23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL23_QTg2$player1
player2vector <- CARL23_QTg2$player2
CARL23_QTg3 <- CARL23_QTg2
CARL23_QTg3$p1inp2vec <- is.element(CARL23_QTg3$player1, player2vector)
CARL23_QTg3$p2inp1vec <- is.element(CARL23_QTg3$player2, player1vector)

addPlayer1 <- CARL23_QTg3[ which(CARL23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL23_QTg3[ which(CARL23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL23_QTg2 <- rbind(CARL23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
CARL23_QTft <- ftable(CARL23_QTg2$player1, CARL23_QTg2$player2)
CARL23_QTft2 <- as.matrix(CARL23_QTft)
numRows <- nrow(CARL23_QTft2)
numCols <- ncol(CARL23_QTft2)
CARL23_QTft3 <- CARL23_QTft2[c(2:numRows) , c(2:numCols)]
CARL23_QTTable <- graph.adjacency(CARL23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(CARL23_QTTable, vertex.label = V(CARL23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
CARL23_QT.clusterCoef <- transitivity(CARL23_QTTable, type="global") #cluster coefficient
CARL23_QT.degreeCent <- centralization.degree(CARL23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL23_QTftn <- as.network.matrix(CARL23_QTft)
CARL23_QT.netDensity <- network.density(CARL23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL23_QT.entropy <- entropy(CARL23_QTft) #entropy

CARL23_QT.netMx <- cbind(CARL23_QT.netMx, CARL23_QT.clusterCoef, CARL23_QT.degreeCent$centralization,
                         CARL23_QT.netDensity, CARL23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL23_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "COLL"
KIoutcome = "Goal_F"
COLL23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
COLL23_Gg2 <- data.frame(COLL23_G)
COLL23_Gg2 <- COLL23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_Gg2$player1
player2vector <- COLL23_Gg2$player2
COLL23_Gg3 <- COLL23_Gg2
COLL23_Gg3$p1inp2vec <- is.element(COLL23_Gg3$player1, player2vector)
COLL23_Gg3$p2inp1vec <- is.element(COLL23_Gg3$player2, player1vector)

addPlayer1 <- COLL23_Gg3[ which(COLL23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_Gg3[ which(COLL23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_Gg2 <- rbind(COLL23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
COLL23_Gft <- ftable(COLL23_Gg2$player1, COLL23_Gg2$player2)
COLL23_Gft2 <- as.matrix(COLL23_Gft)
numRows <- nrow(COLL23_Gft2)
numCols <- ncol(COLL23_Gft2)
COLL23_Gft3 <- COLL23_Gft2[c(2:numRows) , c(2:numCols)]
COLL23_GTable <- graph.adjacency(COLL23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(COLL23_GTable, vertex.label = V(COLL23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
COLL23_G.clusterCoef <- transitivity(COLL23_GTable, type="global") #cluster coefficient
COLL23_G.degreeCent <- centralization.degree(COLL23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_Gftn <- as.network.matrix(COLL23_Gft)
COLL23_G.netDensity <- network.density(COLL23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_G.entropy <- entropy(COLL23_Gft) #entropy

COLL23_G.netMx <- cbind(COLL23_G.netMx, COLL23_G.clusterCoef, COLL23_G.degreeCent$centralization,
                        COLL23_G.netDensity, COLL23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "COLL"
KIoutcome = "Behind_F"
COLL23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
COLL23_Bg2 <- data.frame(COLL23_B)
COLL23_Bg2 <- COLL23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_Bg2$player1
player2vector <- COLL23_Bg2$player2
COLL23_Bg3 <- COLL23_Bg2
COLL23_Bg3$p1inp2vec <- is.element(COLL23_Bg3$player1, player2vector)
COLL23_Bg3$p2inp1vec <- is.element(COLL23_Bg3$player2, player1vector)

addPlayer1 <- COLL23_Bg3[ which(COLL23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_Bg3[ which(COLL23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_Bg2 <- rbind(COLL23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
COLL23_Bft <- ftable(COLL23_Bg2$player1, COLL23_Bg2$player2)
COLL23_Bft2 <- as.matrix(COLL23_Bft)
numRows <- nrow(COLL23_Bft2)
numCols <- ncol(COLL23_Bft2)
COLL23_Bft3 <- COLL23_Bft2[c(2:numRows) , c(2:numCols)]
COLL23_BTable <- graph.adjacency(COLL23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(COLL23_BTable, vertex.label = V(COLL23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
COLL23_B.clusterCoef <- transitivity(COLL23_BTable, type="global") #cluster coefficient
COLL23_B.degreeCent <- centralization.degree(COLL23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_Bftn <- as.network.matrix(COLL23_Bft)
COLL23_B.netDensity <- network.density(COLL23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_B.entropy <- entropy(COLL23_Bft) #entropy

COLL23_B.netMx <- cbind(COLL23_B.netMx, COLL23_B.clusterCoef, COLL23_B.degreeCent$centralization,
                        COLL23_B.netDensity, COLL23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
COLL23_SFg2 <- data.frame(COLL23_SF)
COLL23_SFg2 <- COLL23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_SFg2$player1
player2vector <- COLL23_SFg2$player2
COLL23_SFg3 <- COLL23_SFg2
COLL23_SFg3$p1inp2vec <- is.element(COLL23_SFg3$player1, player2vector)
COLL23_SFg3$p2inp1vec <- is.element(COLL23_SFg3$player2, player1vector)

addPlayer1 <- COLL23_SFg3[ which(COLL23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_SFg3[ which(COLL23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_SFg2 <- rbind(COLL23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
COLL23_SFft <- ftable(COLL23_SFg2$player1, COLL23_SFg2$player2)
COLL23_SFft2 <- as.matrix(COLL23_SFft)
numRows <- nrow(COLL23_SFft2)
numCols <- ncol(COLL23_SFft2)
COLL23_SFft3 <- COLL23_SFft2[c(2:numRows) , c(2:numCols)]
COLL23_SFTable <- graph.adjacency(COLL23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(COLL23_SFTable, vertex.label = V(COLL23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
COLL23_SF.clusterCoef <- transitivity(COLL23_SFTable, type="global") #cluster coefficient
COLL23_SF.degreeCent <- centralization.degree(COLL23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_SFftn <- as.network.matrix(COLL23_SFft)
COLL23_SF.netDensity <- network.density(COLL23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_SF.entropy <- entropy(COLL23_SFft) #entropy

COLL23_SF.netMx <- cbind(COLL23_SF.netMx, COLL23_SF.clusterCoef, COLL23_SF.degreeCent$centralization,
                         COLL23_SF.netDensity, COLL23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
COLL23_TFg2 <- data.frame(COLL23_TF)
COLL23_TFg2 <- COLL23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_TFg2$player1
player2vector <- COLL23_TFg2$player2
COLL23_TFg3 <- COLL23_TFg2
COLL23_TFg3$p1inp2vec <- is.element(COLL23_TFg3$player1, player2vector)
COLL23_TFg3$p2inp1vec <- is.element(COLL23_TFg3$player2, player1vector)

addPlayer1 <- COLL23_TFg3[ which(COLL23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL23_TFg3[ which(COLL23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_TFg2 <- rbind(COLL23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
COLL23_TFft <- ftable(COLL23_TFg2$player1, COLL23_TFg2$player2)
COLL23_TFft2 <- as.matrix(COLL23_TFft)
numRows <- nrow(COLL23_TFft2)
numCols <- ncol(COLL23_TFft2)
COLL23_TFft3 <- COLL23_TFft2[c(2:numRows) , c(2:numCols)]
COLL23_TFTable <- graph.adjacency(COLL23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(COLL23_TFTable, vertex.label = V(COLL23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
COLL23_TF.clusterCoef <- transitivity(COLL23_TFTable, type="global") #cluster coefficient
COLL23_TF.degreeCent <- centralization.degree(COLL23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_TFftn <- as.network.matrix(COLL23_TFft)
COLL23_TF.netDensity <- network.density(COLL23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_TF.entropy <- entropy(COLL23_TFft) #entropy

COLL23_TF.netMx <- cbind(COLL23_TF.netMx, COLL23_TF.clusterCoef, COLL23_TF.degreeCent$centralization,
                         COLL23_TF.netDensity, COLL23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************

round = 23
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
COLL23_SAMg2 <- data.frame(COLL23_SAM)
COLL23_SAMg2 <- COLL23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_SAMg2$player1
player2vector <- COLL23_SAMg2$player2
COLL23_SAMg3 <- COLL23_SAMg2
COLL23_SAMg3$p1inp2vec <- is.element(COLL23_SAMg3$player1, player2vector)
COLL23_SAMg3$p2inp1vec <- is.element(COLL23_SAMg3$player2, player1vector)

addPlayer1 <- COLL23_SAMg3[ which(COLL23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL23_SAMg3[ which(COLL23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_SAMg2 <- rbind(COLL23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
COLL23_SAMft <- ftable(COLL23_SAMg2$player1, COLL23_SAMg2$player2)
COLL23_SAMft2 <- as.matrix(COLL23_SAMft)
numRows <- nrow(COLL23_SAMft2)
numCols <- ncol(COLL23_SAMft2)
COLL23_SAMft3 <- COLL23_SAMft2[c(2:numRows) , c(2:numCols)]
COLL23_SAMTable <- graph.adjacency(COLL23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(COLL23_SAMTable, vertex.label = V(COLL23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
COLL23_SAM.clusterCoef <- transitivity(COLL23_SAMTable, type="global") #cluster coefficient
COLL23_SAM.degreeCent <- centralization.degree(COLL23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_SAMftn <- as.network.matrix(COLL23_SAMft)
COLL23_SAM.netDensity <- network.density(COLL23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_SAM.entropy <- entropy(COLL23_SAMft) #entropy

COLL23_SAM.netMx <- cbind(COLL23_SAM.netMx, COLL23_SAM.clusterCoef, COLL23_SAM.degreeCent$centralization,
                          COLL23_SAM.netDensity, COLL23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************
#NA

round = 23
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
COLL23_TAMg2 <- data.frame(COLL23_TAM)
COLL23_TAMg2 <- COLL23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_TAMg2$player1
player2vector <- COLL23_TAMg2$player2
COLL23_TAMg3 <- COLL23_TAMg2
COLL23_TAMg3$p1inp2vec <- is.element(COLL23_TAMg3$player1, player2vector)
COLL23_TAMg3$p2inp1vec <- is.element(COLL23_TAMg3$player2, player1vector)

addPlayer1 <- COLL23_TAMg3[ which(COLL23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_TAMg3[ which(COLL23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_TAMg2 <- rbind(COLL23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
COLL23_TAMft <- ftable(COLL23_TAMg2$player1, COLL23_TAMg2$player2)
COLL23_TAMft2 <- as.matrix(COLL23_TAMft)
numRows <- nrow(COLL23_TAMft2)
numCols <- ncol(COLL23_TAMft2)
COLL23_TAMft3 <- COLL23_TAMft2[c(2:numRows) , c(2:numCols)]
COLL23_TAMTable <- graph.adjacency(COLL23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(COLL23_TAMTable, vertex.label = V(COLL23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
COLL23_TAM.clusterCoef <- transitivity(COLL23_TAMTable, type="global") #cluster coefficient
COLL23_TAM.degreeCent <- centralization.degree(COLL23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_TAMftn <- as.network.matrix(COLL23_TAMft)
COLL23_TAM.netDensity <- network.density(COLL23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_TAM.entropy <- entropy(COLL23_TAMft) #entropy

COLL23_TAM.netMx <- cbind(COLL23_TAM.netMx, COLL23_TAM.clusterCoef, COLL23_TAM.degreeCent$centralization,
                          COLL23_TAM.netDensity, COLL23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
COLL23_SDMg2 <- data.frame(COLL23_SDM)
COLL23_SDMg2 <- COLL23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_SDMg2$player1
player2vector <- COLL23_SDMg2$player2
COLL23_SDMg3 <- COLL23_SDMg2
COLL23_SDMg3$p1inp2vec <- is.element(COLL23_SDMg3$player1, player2vector)
COLL23_SDMg3$p2inp1vec <- is.element(COLL23_SDMg3$player2, player1vector)

addPlayer1 <- COLL23_SDMg3[ which(COLL23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_SDMg3[ which(COLL23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_SDMg2 <- rbind(COLL23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
COLL23_SDMft <- ftable(COLL23_SDMg2$player1, COLL23_SDMg2$player2)
COLL23_SDMft2 <- as.matrix(COLL23_SDMft)
numRows <- nrow(COLL23_SDMft2)
numCols <- ncol(COLL23_SDMft2)
COLL23_SDMft3 <- COLL23_SDMft2[c(2:numRows) , c(2:numCols)]
COLL23_SDMTable <- graph.adjacency(COLL23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(COLL23_SDMTable, vertex.label = V(COLL23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
COLL23_SDM.clusterCoef <- transitivity(COLL23_SDMTable, type="global") #cluster coefficient
COLL23_SDM.degreeCent <- centralization.degree(COLL23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_SDMftn <- as.network.matrix(COLL23_SDMft)
COLL23_SDM.netDensity <- network.density(COLL23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_SDM.entropy <- entropy(COLL23_SDMft) #entropy

COLL23_SDM.netMx <- cbind(COLL23_SDM.netMx, COLL23_SDM.clusterCoef, COLL23_SDM.degreeCent$centralization,
                          COLL23_SDM.netDensity, COLL23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
COLL23_TDMg2 <- data.frame(COLL23_TDM)
COLL23_TDMg2 <- COLL23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_TDMg2$player1
player2vector <- COLL23_TDMg2$player2
COLL23_TDMg3 <- COLL23_TDMg2
COLL23_TDMg3$p1inp2vec <- is.element(COLL23_TDMg3$player1, player2vector)
COLL23_TDMg3$p2inp1vec <- is.element(COLL23_TDMg3$player2, player1vector)

addPlayer1 <- COLL23_TDMg3[ which(COLL23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_TDMg3[ which(COLL23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_TDMg2 <- rbind(COLL23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
COLL23_TDMft <- ftable(COLL23_TDMg2$player1, COLL23_TDMg2$player2)
COLL23_TDMft2 <- as.matrix(COLL23_TDMft)
numRows <- nrow(COLL23_TDMft2)
numCols <- ncol(COLL23_TDMft2)
COLL23_TDMft3 <- COLL23_TDMft2[c(2:numRows) , c(2:numCols)]
COLL23_TDMTable <- graph.adjacency(COLL23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(COLL23_TDMTable, vertex.label = V(COLL23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
COLL23_TDM.clusterCoef <- transitivity(COLL23_TDMTable, type="global") #cluster coefficient
COLL23_TDM.degreeCent <- centralization.degree(COLL23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_TDMftn <- as.network.matrix(COLL23_TDMft)
COLL23_TDM.netDensity <- network.density(COLL23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_TDM.entropy <- entropy(COLL23_TDMft) #entropy

COLL23_TDM.netMx <- cbind(COLL23_TDM.netMx, COLL23_TDM.clusterCoef, COLL23_TDM.degreeCent$centralization,
                          COLL23_TDM.netDensity, COLL23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
COLL23_SDg2 <- data.frame(COLL23_SD)
COLL23_SDg2 <- COLL23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_SDg2$player1
player2vector <- COLL23_SDg2$player2
COLL23_SDg3 <- COLL23_SDg2
COLL23_SDg3$p1inp2vec <- is.element(COLL23_SDg3$player1, player2vector)
COLL23_SDg3$p2inp1vec <- is.element(COLL23_SDg3$player2, player1vector)

addPlayer1 <- COLL23_SDg3[ which(COLL23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_SDg3[ which(COLL23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_SDg2 <- rbind(COLL23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
COLL23_SDft <- ftable(COLL23_SDg2$player1, COLL23_SDg2$player2)
COLL23_SDft2 <- as.matrix(COLL23_SDft)
numRows <- nrow(COLL23_SDft2)
numCols <- ncol(COLL23_SDft2)
COLL23_SDft3 <- COLL23_SDft2[c(2:numRows) , c(2:numCols)]
COLL23_SDTable <- graph.adjacency(COLL23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(COLL23_SDTable, vertex.label = V(COLL23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
COLL23_SD.clusterCoef <- transitivity(COLL23_SDTable, type="global") #cluster coefficient
COLL23_SD.degreeCent <- centralization.degree(COLL23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_SDftn <- as.network.matrix(COLL23_SDft)
COLL23_SD.netDensity <- network.density(COLL23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_SD.entropy <- entropy(COLL23_SDft) #entropy

COLL23_SD.netMx <- cbind(COLL23_SD.netMx, COLL23_SD.clusterCoef, COLL23_SD.degreeCent$centralization,
                         COLL23_SD.netDensity, COLL23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
COLL23_TDg2 <- data.frame(COLL23_TD)
COLL23_TDg2 <- COLL23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_TDg2$player1
player2vector <- COLL23_TDg2$player2
COLL23_TDg3 <- COLL23_TDg2
COLL23_TDg3$p1inp2vec <- is.element(COLL23_TDg3$player1, player2vector)
COLL23_TDg3$p2inp1vec <- is.element(COLL23_TDg3$player2, player1vector)

addPlayer1 <- COLL23_TDg3[ which(COLL23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_TDg3[ which(COLL23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_TDg2 <- rbind(COLL23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
COLL23_TDft <- ftable(COLL23_TDg2$player1, COLL23_TDg2$player2)
COLL23_TDft2 <- as.matrix(COLL23_TDft)
numRows <- nrow(COLL23_TDft2)
numCols <- ncol(COLL23_TDft2)
COLL23_TDft3 <- COLL23_TDft2[c(2:numRows) , c(2:numCols)]
COLL23_TDTable <- graph.adjacency(COLL23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(COLL23_TDTable, vertex.label = V(COLL23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
COLL23_TD.clusterCoef <- transitivity(COLL23_TDTable, type="global") #cluster coefficient
COLL23_TD.degreeCent <- centralization.degree(COLL23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_TDftn <- as.network.matrix(COLL23_TDft)
COLL23_TD.netDensity <- network.density(COLL23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_TD.entropy <- entropy(COLL23_TDft) #entropy

COLL23_TD.netMx <- cbind(COLL23_TD.netMx, COLL23_TD.clusterCoef, COLL23_TD.degreeCent$centralization,
                         COLL23_TD.netDensity, COLL23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
COLL23_QTg2 <- data.frame(COLL23_QT)
COLL23_QTg2 <- COLL23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL23_QTg2$player1
player2vector <- COLL23_QTg2$player2
COLL23_QTg3 <- COLL23_QTg2
COLL23_QTg3$p1inp2vec <- is.element(COLL23_QTg3$player1, player2vector)
COLL23_QTg3$p2inp1vec <- is.element(COLL23_QTg3$player2, player1vector)

addPlayer1 <- COLL23_QTg3[ which(COLL23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL23_QTg3[ which(COLL23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL23_QTg2 <- rbind(COLL23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
COLL23_QTft <- ftable(COLL23_QTg2$player1, COLL23_QTg2$player2)
COLL23_QTft2 <- as.matrix(COLL23_QTft)
numRows <- nrow(COLL23_QTft2)
numCols <- ncol(COLL23_QTft2)
COLL23_QTft3 <- COLL23_QTft2[c(2:numRows) , c(2:numCols)]
COLL23_QTTable <- graph.adjacency(COLL23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(COLL23_QTTable, vertex.label = V(COLL23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
COLL23_QT.clusterCoef <- transitivity(COLL23_QTTable, type="global") #cluster coefficient
COLL23_QT.degreeCent <- centralization.degree(COLL23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL23_QTftn <- as.network.matrix(COLL23_QTft)
COLL23_QT.netDensity <- network.density(COLL23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL23_QT.entropy <- entropy(COLL23_QTft) #entropy

COLL23_QT.netMx <- cbind(COLL23_QT.netMx, COLL23_QT.clusterCoef, COLL23_QT.degreeCent$centralization,
                         COLL23_QT.netDensity, COLL23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL23_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "ESS"
KIoutcome = "Goal_F"
ESS23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
ESS23_Gg2 <- data.frame(ESS23_G)
ESS23_Gg2 <- ESS23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_Gg2$player1
player2vector <- ESS23_Gg2$player2
ESS23_Gg3 <- ESS23_Gg2
ESS23_Gg3$p1inp2vec <- is.element(ESS23_Gg3$player1, player2vector)
ESS23_Gg3$p2inp1vec <- is.element(ESS23_Gg3$player2, player1vector)

addPlayer1 <- ESS23_Gg3[ which(ESS23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS23_Gg3[ which(ESS23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_Gg2 <- rbind(ESS23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
ESS23_Gft <- ftable(ESS23_Gg2$player1, ESS23_Gg2$player2)
ESS23_Gft2 <- as.matrix(ESS23_Gft)
numRows <- nrow(ESS23_Gft2)
numCols <- ncol(ESS23_Gft2)
ESS23_Gft3 <- ESS23_Gft2[c(2:numRows) , c(2:numCols)]
ESS23_GTable <- graph.adjacency(ESS23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(ESS23_GTable, vertex.label = V(ESS23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
ESS23_G.clusterCoef <- transitivity(ESS23_GTable, type="global") #cluster coefficient
ESS23_G.degreeCent <- centralization.degree(ESS23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_Gftn <- as.network.matrix(ESS23_Gft)
ESS23_G.netDensity <- network.density(ESS23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_G.entropy <- entropy(ESS23_Gft) #entropy

ESS23_G.netMx <- cbind(ESS23_G.netMx, ESS23_G.clusterCoef, ESS23_G.degreeCent$centralization,
                       ESS23_G.netDensity, ESS23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "ESS"
KIoutcome = "Behind_F"
ESS23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
ESS23_Bg2 <- data.frame(ESS23_B)
ESS23_Bg2 <- ESS23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_Bg2$player1
player2vector <- ESS23_Bg2$player2
ESS23_Bg3 <- ESS23_Bg2
ESS23_Bg3$p1inp2vec <- is.element(ESS23_Bg3$player1, player2vector)
ESS23_Bg3$p2inp1vec <- is.element(ESS23_Bg3$player2, player1vector)

addPlayer1 <- ESS23_Bg3[ which(ESS23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_Bg3[ which(ESS23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_Bg2 <- rbind(ESS23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
ESS23_Bft <- ftable(ESS23_Bg2$player1, ESS23_Bg2$player2)
ESS23_Bft2 <- as.matrix(ESS23_Bft)
numRows <- nrow(ESS23_Bft2)
numCols <- ncol(ESS23_Bft2)
ESS23_Bft3 <- ESS23_Bft2[c(2:numRows) , c(2:numCols)]
ESS23_BTable <- graph.adjacency(ESS23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(ESS23_BTable, vertex.label = V(ESS23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
ESS23_B.clusterCoef <- transitivity(ESS23_BTable, type="global") #cluster coefficient
ESS23_B.degreeCent <- centralization.degree(ESS23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_Bftn <- as.network.matrix(ESS23_Bft)
ESS23_B.netDensity <- network.density(ESS23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_B.entropy <- entropy(ESS23_Bft) #entropy

ESS23_B.netMx <- cbind(ESS23_B.netMx, ESS23_B.clusterCoef, ESS23_B.degreeCent$centralization,
                       ESS23_B.netDensity, ESS23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************

round = 23
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
ESS23_SFg2 <- data.frame(ESS23_SF)
ESS23_SFg2 <- ESS23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_SFg2$player1
player2vector <- ESS23_SFg2$player2
ESS23_SFg3 <- ESS23_SFg2
ESS23_SFg3$p1inp2vec <- is.element(ESS23_SFg3$player1, player2vector)
ESS23_SFg3$p2inp1vec <- is.element(ESS23_SFg3$player2, player1vector)

addPlayer1 <- ESS23_SFg3[ which(ESS23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_SFg3[ which(ESS23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_SFg2 <- rbind(ESS23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
ESS23_SFft <- ftable(ESS23_SFg2$player1, ESS23_SFg2$player2)
ESS23_SFft2 <- as.matrix(ESS23_SFft)
numRows <- nrow(ESS23_SFft2)
numCols <- ncol(ESS23_SFft2)
ESS23_SFft3 <- ESS23_SFft2[c(2:numRows) , c(2:numCols)]
ESS23_SFTable <- graph.adjacency(ESS23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(ESS23_SFTable, vertex.label = V(ESS23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
ESS23_SF.clusterCoef <- transitivity(ESS23_SFTable, type="global") #cluster coefficient
ESS23_SF.degreeCent <- centralization.degree(ESS23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_SFftn <- as.network.matrix(ESS23_SFft)
ESS23_SF.netDensity <- network.density(ESS23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_SF.entropy <- entropy(ESS23_SFft) #entropy

ESS23_SF.netMx <- cbind(ESS23_SF.netMx, ESS23_SF.clusterCoef, ESS23_SF.degreeCent$centralization,
                        ESS23_SF.netDensity, ESS23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
ESS23_TFg2 <- data.frame(ESS23_TF)
ESS23_TFg2 <- ESS23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_TFg2$player1
player2vector <- ESS23_TFg2$player2
ESS23_TFg3 <- ESS23_TFg2
ESS23_TFg3$p1inp2vec <- is.element(ESS23_TFg3$player1, player2vector)
ESS23_TFg3$p2inp1vec <- is.element(ESS23_TFg3$player2, player1vector)

addPlayer1 <- ESS23_TFg3[ which(ESS23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_TFg3[ which(ESS23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_TFg2 <- rbind(ESS23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
ESS23_TFft <- ftable(ESS23_TFg2$player1, ESS23_TFg2$player2)
ESS23_TFft2 <- as.matrix(ESS23_TFft)
numRows <- nrow(ESS23_TFft2)
numCols <- ncol(ESS23_TFft2)
ESS23_TFft3 <- ESS23_TFft2[c(2:numRows) , c(2:numCols)]
ESS23_TFTable <- graph.adjacency(ESS23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(ESS23_TFTable, vertex.label = V(ESS23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
ESS23_TF.clusterCoef <- transitivity(ESS23_TFTable, type="global") #cluster coefficient
ESS23_TF.degreeCent <- centralization.degree(ESS23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_TFftn <- as.network.matrix(ESS23_TFft)
ESS23_TF.netDensity <- network.density(ESS23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_TF.entropy <- entropy(ESS23_TFft) #entropy

ESS23_TF.netMx <- cbind(ESS23_TF.netMx, ESS23_TF.clusterCoef, ESS23_TF.degreeCent$centralization,
                        ESS23_TF.netDensity, ESS23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
ESS23_SAMg2 <- data.frame(ESS23_SAM)
ESS23_SAMg2 <- ESS23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_SAMg2$player1
player2vector <- ESS23_SAMg2$player2
ESS23_SAMg3 <- ESS23_SAMg2
ESS23_SAMg3$p1inp2vec <- is.element(ESS23_SAMg3$player1, player2vector)
ESS23_SAMg3$p2inp1vec <- is.element(ESS23_SAMg3$player2, player1vector)

addPlayer1 <- ESS23_SAMg3[ which(ESS23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_SAMg3[ which(ESS23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_SAMg2 <- rbind(ESS23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
ESS23_SAMft <- ftable(ESS23_SAMg2$player1, ESS23_SAMg2$player2)
ESS23_SAMft2 <- as.matrix(ESS23_SAMft)
numRows <- nrow(ESS23_SAMft2)
numCols <- ncol(ESS23_SAMft2)
ESS23_SAMft3 <- ESS23_SAMft2[c(2:numRows) , c(2:numCols)]
ESS23_SAMTable <- graph.adjacency(ESS23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(ESS23_SAMTable, vertex.label = V(ESS23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
ESS23_SAM.clusterCoef <- transitivity(ESS23_SAMTable, type="global") #cluster coefficient
ESS23_SAM.degreeCent <- centralization.degree(ESS23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_SAMftn <- as.network.matrix(ESS23_SAMft)
ESS23_SAM.netDensity <- network.density(ESS23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_SAM.entropy <- entropy(ESS23_SAMft) #entropy

ESS23_SAM.netMx <- cbind(ESS23_SAM.netMx, ESS23_SAM.clusterCoef, ESS23_SAM.degreeCent$centralization,
                         ESS23_SAM.netDensity, ESS23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
ESS23_TAMg2 <- data.frame(ESS23_TAM)
ESS23_TAMg2 <- ESS23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_TAMg2$player1
player2vector <- ESS23_TAMg2$player2
ESS23_TAMg3 <- ESS23_TAMg2
ESS23_TAMg3$p1inp2vec <- is.element(ESS23_TAMg3$player1, player2vector)
ESS23_TAMg3$p2inp1vec <- is.element(ESS23_TAMg3$player2, player1vector)

addPlayer1 <- ESS23_TAMg3[ which(ESS23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_TAMg3[ which(ESS23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_TAMg2 <- rbind(ESS23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
ESS23_TAMft <- ftable(ESS23_TAMg2$player1, ESS23_TAMg2$player2)
ESS23_TAMft2 <- as.matrix(ESS23_TAMft)
numRows <- nrow(ESS23_TAMft2)
numCols <- ncol(ESS23_TAMft2)
ESS23_TAMft3 <- ESS23_TAMft2[c(2:numRows) , c(2:numCols)]
ESS23_TAMTable <- graph.adjacency(ESS23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(ESS23_TAMTable, vertex.label = V(ESS23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
ESS23_TAM.clusterCoef <- transitivity(ESS23_TAMTable, type="global") #cluster coefficient
ESS23_TAM.degreeCent <- centralization.degree(ESS23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_TAMftn <- as.network.matrix(ESS23_TAMft)
ESS23_TAM.netDensity <- network.density(ESS23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_TAM.entropy <- entropy(ESS23_TAMft) #entropy

ESS23_TAM.netMx <- cbind(ESS23_TAM.netMx, ESS23_TAM.clusterCoef, ESS23_TAM.degreeCent$centralization,
                         ESS23_TAM.netDensity, ESS23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
ESS23_SDMg2 <- data.frame(ESS23_SDM)
ESS23_SDMg2 <- ESS23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_SDMg2$player1
player2vector <- ESS23_SDMg2$player2
ESS23_SDMg3 <- ESS23_SDMg2
ESS23_SDMg3$p1inp2vec <- is.element(ESS23_SDMg3$player1, player2vector)
ESS23_SDMg3$p2inp1vec <- is.element(ESS23_SDMg3$player2, player1vector)

addPlayer1 <- ESS23_SDMg3[ which(ESS23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_SDMg3[ which(ESS23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_SDMg2 <- rbind(ESS23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
ESS23_SDMft <- ftable(ESS23_SDMg2$player1, ESS23_SDMg2$player2)
ESS23_SDMft2 <- as.matrix(ESS23_SDMft)
numRows <- nrow(ESS23_SDMft2)
numCols <- ncol(ESS23_SDMft2)
ESS23_SDMft3 <- ESS23_SDMft2[c(2:numRows) , c(2:numCols)]
ESS23_SDMTable <- graph.adjacency(ESS23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(ESS23_SDMTable, vertex.label = V(ESS23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
ESS23_SDM.clusterCoef <- transitivity(ESS23_SDMTable, type="global") #cluster coefficient
ESS23_SDM.degreeCent <- centralization.degree(ESS23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_SDMftn <- as.network.matrix(ESS23_SDMft)
ESS23_SDM.netDensity <- network.density(ESS23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_SDM.entropy <- entropy(ESS23_SDMft) #entropy

ESS23_SDM.netMx <- cbind(ESS23_SDM.netMx, ESS23_SDM.clusterCoef, ESS23_SDM.degreeCent$centralization,
                         ESS23_SDM.netDensity, ESS23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
ESS23_TDMg2 <- data.frame(ESS23_TDM)
ESS23_TDMg2 <- ESS23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_TDMg2$player1
player2vector <- ESS23_TDMg2$player2
ESS23_TDMg3 <- ESS23_TDMg2
ESS23_TDMg3$p1inp2vec <- is.element(ESS23_TDMg3$player1, player2vector)
ESS23_TDMg3$p2inp1vec <- is.element(ESS23_TDMg3$player2, player1vector)

addPlayer1 <- ESS23_TDMg3[ which(ESS23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_TDMg3[ which(ESS23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_TDMg2 <- rbind(ESS23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
ESS23_TDMft <- ftable(ESS23_TDMg2$player1, ESS23_TDMg2$player2)
ESS23_TDMft2 <- as.matrix(ESS23_TDMft)
numRows <- nrow(ESS23_TDMft2)
numCols <- ncol(ESS23_TDMft2)
ESS23_TDMft3 <- ESS23_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS23_TDMTable <- graph.adjacency(ESS23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(ESS23_TDMTable, vertex.label = V(ESS23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
ESS23_TDM.clusterCoef <- transitivity(ESS23_TDMTable, type="global") #cluster coefficient
ESS23_TDM.degreeCent <- centralization.degree(ESS23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_TDMftn <- as.network.matrix(ESS23_TDMft)
ESS23_TDM.netDensity <- network.density(ESS23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_TDM.entropy <- entropy(ESS23_TDMft) #entropy

ESS23_TDM.netMx <- cbind(ESS23_TDM.netMx, ESS23_TDM.clusterCoef, ESS23_TDM.degreeCent$centralization,
                         ESS23_TDM.netDensity, ESS23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
ESS23_SDg2 <- data.frame(ESS23_SD)
ESS23_SDg2 <- ESS23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_SDg2$player1
player2vector <- ESS23_SDg2$player2
ESS23_SDg3 <- ESS23_SDg2
ESS23_SDg3$p1inp2vec <- is.element(ESS23_SDg3$player1, player2vector)
ESS23_SDg3$p2inp1vec <- is.element(ESS23_SDg3$player2, player1vector)

addPlayer1 <- ESS23_SDg3[ which(ESS23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_SDg3[ which(ESS23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_SDg2 <- rbind(ESS23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
ESS23_SDft <- ftable(ESS23_SDg2$player1, ESS23_SDg2$player2)
ESS23_SDft2 <- as.matrix(ESS23_SDft)
numRows <- nrow(ESS23_SDft2)
numCols <- ncol(ESS23_SDft2)
ESS23_SDft3 <- ESS23_SDft2[c(2:numRows) , c(2:numCols)]
ESS23_SDTable <- graph.adjacency(ESS23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(ESS23_SDTable, vertex.label = V(ESS23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
ESS23_SD.clusterCoef <- transitivity(ESS23_SDTable, type="global") #cluster coefficient
ESS23_SD.degreeCent <- centralization.degree(ESS23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_SDftn <- as.network.matrix(ESS23_SDft)
ESS23_SD.netDensity <- network.density(ESS23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_SD.entropy <- entropy(ESS23_SDft) #entropy

ESS23_SD.netMx <- cbind(ESS23_SD.netMx, ESS23_SD.clusterCoef, ESS23_SD.degreeCent$centralization,
                        ESS23_SD.netDensity, ESS23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
ESS23_TDg2 <- data.frame(ESS23_TD)
ESS23_TDg2 <- ESS23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_TDg2$player1
player2vector <- ESS23_TDg2$player2
ESS23_TDg3 <- ESS23_TDg2
ESS23_TDg3$p1inp2vec <- is.element(ESS23_TDg3$player1, player2vector)
ESS23_TDg3$p2inp1vec <- is.element(ESS23_TDg3$player2, player1vector)

addPlayer1 <- ESS23_TDg3[ which(ESS23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_TDg3[ which(ESS23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_TDg2 <- rbind(ESS23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
ESS23_TDft <- ftable(ESS23_TDg2$player1, ESS23_TDg2$player2)
ESS23_TDft2 <- as.matrix(ESS23_TDft)
numRows <- nrow(ESS23_TDft2)
numCols <- ncol(ESS23_TDft2)
ESS23_TDft3 <- ESS23_TDft2[c(2:numRows) , c(2:numCols)]
ESS23_TDTable <- graph.adjacency(ESS23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(ESS23_TDTable, vertex.label = V(ESS23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
ESS23_TD.clusterCoef <- transitivity(ESS23_TDTable, type="global") #cluster coefficient
ESS23_TD.degreeCent <- centralization.degree(ESS23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_TDftn <- as.network.matrix(ESS23_TDft)
ESS23_TD.netDensity <- network.density(ESS23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_TD.entropy <- entropy(ESS23_TDft) #entropy

ESS23_TD.netMx <- cbind(ESS23_TD.netMx, ESS23_TD.clusterCoef, ESS23_TD.degreeCent$centralization,
                        ESS23_TD.netDensity, ESS23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
ESS23_QTg2 <- data.frame(ESS23_QT)
ESS23_QTg2 <- ESS23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS23_QTg2$player1
player2vector <- ESS23_QTg2$player2
ESS23_QTg3 <- ESS23_QTg2
ESS23_QTg3$p1inp2vec <- is.element(ESS23_QTg3$player1, player2vector)
ESS23_QTg3$p2inp1vec <- is.element(ESS23_QTg3$player2, player1vector)

addPlayer1 <- ESS23_QTg3[ which(ESS23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS23_QTg3[ which(ESS23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS23_QTg2 <- rbind(ESS23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
ESS23_QTft <- ftable(ESS23_QTg2$player1, ESS23_QTg2$player2)
ESS23_QTft2 <- as.matrix(ESS23_QTft)
numRows <- nrow(ESS23_QTft2)
numCols <- ncol(ESS23_QTft2)
ESS23_QTft3 <- ESS23_QTft2[c(2:numRows) , c(2:numCols)]
ESS23_QTTable <- graph.adjacency(ESS23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(ESS23_QTTable, vertex.label = V(ESS23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
ESS23_QT.clusterCoef <- transitivity(ESS23_QTTable, type="global") #cluster coefficient
ESS23_QT.degreeCent <- centralization.degree(ESS23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS23_QTftn <- as.network.matrix(ESS23_QTft)
ESS23_QT.netDensity <- network.density(ESS23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS23_QT.entropy <- entropy(ESS23_QTft) #entropy

ESS23_QT.netMx <- cbind(ESS23_QT.netMx, ESS23_QT.clusterCoef, ESS23_QT.degreeCent$centralization,
                        ESS23_QT.netDensity, ESS23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS23_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "FRE"
KIoutcome = "Goal_F"
FRE23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
FRE23_Gg2 <- data.frame(FRE23_G)
FRE23_Gg2 <- FRE23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_Gg2$player1
player2vector <- FRE23_Gg2$player2
FRE23_Gg3 <- FRE23_Gg2
FRE23_Gg3$p1inp2vec <- is.element(FRE23_Gg3$player1, player2vector)
FRE23_Gg3$p2inp1vec <- is.element(FRE23_Gg3$player2, player1vector)

addPlayer1 <- FRE23_Gg3[ which(FRE23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE23_Gg3[ which(FRE23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_Gg2 <- rbind(FRE23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
FRE23_Gft <- ftable(FRE23_Gg2$player1, FRE23_Gg2$player2)
FRE23_Gft2 <- as.matrix(FRE23_Gft)
numRows <- nrow(FRE23_Gft2)
numCols <- ncol(FRE23_Gft2)
FRE23_Gft3 <- FRE23_Gft2[c(2:numRows) , c(2:numCols)]
FRE23_GTable <- graph.adjacency(FRE23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(FRE23_GTable, vertex.label = V(FRE23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
FRE23_G.clusterCoef <- transitivity(FRE23_GTable, type="global") #cluster coefficient
FRE23_G.degreeCent <- centralization.degree(FRE23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_Gftn <- as.network.matrix(FRE23_Gft)
FRE23_G.netDensity <- network.density(FRE23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_G.entropy <- entropy(FRE23_Gft) #entropy

FRE23_G.netMx <- cbind(FRE23_G.netMx, FRE23_G.clusterCoef, FRE23_G.degreeCent$centralization,
                       FRE23_G.netDensity, FRE23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "Behind_F"
FRE23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
FRE23_Bg2 <- data.frame(FRE23_B)
FRE23_Bg2 <- FRE23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_Bg2$player1
player2vector <- FRE23_Bg2$player2
FRE23_Bg3 <- FRE23_Bg2
FRE23_Bg3$p1inp2vec <- is.element(FRE23_Bg3$player1, player2vector)
FRE23_Bg3$p2inp1vec <- is.element(FRE23_Bg3$player2, player1vector)

addPlayer1 <- FRE23_Bg3[ which(FRE23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_Bg3[ which(FRE23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_Bg2 <- rbind(FRE23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
FRE23_Bft <- ftable(FRE23_Bg2$player1, FRE23_Bg2$player2)
FRE23_Bft2 <- as.matrix(FRE23_Bft)
numRows <- nrow(FRE23_Bft2)
numCols <- ncol(FRE23_Bft2)
FRE23_Bft3 <- FRE23_Bft2[c(2:numRows) , c(2:numCols)]
FRE23_BTable <- graph.adjacency(FRE23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(FRE23_BTable, vertex.label = V(FRE23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
FRE23_B.clusterCoef <- transitivity(FRE23_BTable, type="global") #cluster coefficient
FRE23_B.degreeCent <- centralization.degree(FRE23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_Bftn <- as.network.matrix(FRE23_Bft)
FRE23_B.netDensity <- network.density(FRE23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_B.entropy <- entropy(FRE23_Bft) #entropy

FRE23_B.netMx <- cbind(FRE23_B.netMx, FRE23_B.clusterCoef, FRE23_B.degreeCent$centralization,
                       FRE23_B.netDensity, FRE23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
FRE23_SFg2 <- data.frame(FRE23_SF)
FRE23_SFg2 <- FRE23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_SFg2$player1
player2vector <- FRE23_SFg2$player2
FRE23_SFg3 <- FRE23_SFg2
FRE23_SFg3$p1inp2vec <- is.element(FRE23_SFg3$player1, player2vector)
FRE23_SFg3$p2inp1vec <- is.element(FRE23_SFg3$player2, player1vector)

addPlayer1 <- FRE23_SFg3[ which(FRE23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_SFg3[ which(FRE23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_SFg2 <- rbind(FRE23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
FRE23_SFft <- ftable(FRE23_SFg2$player1, FRE23_SFg2$player2)
FRE23_SFft2 <- as.matrix(FRE23_SFft)
numRows <- nrow(FRE23_SFft2)
numCols <- ncol(FRE23_SFft2)
FRE23_SFft3 <- FRE23_SFft2[c(2:numRows) , c(2:numCols)]
FRE23_SFTable <- graph.adjacency(FRE23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(FRE23_SFTable, vertex.label = V(FRE23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
FRE23_SF.clusterCoef <- transitivity(FRE23_SFTable, type="global") #cluster coefficient
FRE23_SF.degreeCent <- centralization.degree(FRE23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_SFftn <- as.network.matrix(FRE23_SFft)
FRE23_SF.netDensity <- network.density(FRE23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_SF.entropy <- entropy(FRE23_SFft) #entropy

FRE23_SF.netMx <- cbind(FRE23_SF.netMx, FRE23_SF.clusterCoef, FRE23_SF.degreeCent$centralization,
                        FRE23_SF.netDensity, FRE23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
FRE23_TFg2 <- data.frame(FRE23_TF)
FRE23_TFg2 <- FRE23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_TFg2$player1
player2vector <- FRE23_TFg2$player2
FRE23_TFg3 <- FRE23_TFg2
FRE23_TFg3$p1inp2vec <- is.element(FRE23_TFg3$player1, player2vector)
FRE23_TFg3$p2inp1vec <- is.element(FRE23_TFg3$player2, player1vector)

addPlayer1 <- FRE23_TFg3[ which(FRE23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_TFg3[ which(FRE23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_TFg2 <- rbind(FRE23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
FRE23_TFft <- ftable(FRE23_TFg2$player1, FRE23_TFg2$player2)
FRE23_TFft2 <- as.matrix(FRE23_TFft)
numRows <- nrow(FRE23_TFft2)
numCols <- ncol(FRE23_TFft2)
FRE23_TFft3 <- FRE23_TFft2[c(2:numRows) , c(2:numCols)]
FRE23_TFTable <- graph.adjacency(FRE23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(FRE23_TFTable, vertex.label = V(FRE23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
FRE23_TF.clusterCoef <- transitivity(FRE23_TFTable, type="global") #cluster coefficient
FRE23_TF.degreeCent <- centralization.degree(FRE23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_TFftn <- as.network.matrix(FRE23_TFft)
FRE23_TF.netDensity <- network.density(FRE23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_TF.entropy <- entropy(FRE23_TFft) #entropy

FRE23_TF.netMx <- cbind(FRE23_TF.netMx, FRE23_TF.clusterCoef, FRE23_TF.degreeCent$centralization,
                        FRE23_TF.netDensity, FRE23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
FRE23_SAMg2 <- data.frame(FRE23_SAM)
FRE23_SAMg2 <- FRE23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_SAMg2$player1
player2vector <- FRE23_SAMg2$player2
FRE23_SAMg3 <- FRE23_SAMg2
FRE23_SAMg3$p1inp2vec <- is.element(FRE23_SAMg3$player1, player2vector)
FRE23_SAMg3$p2inp1vec <- is.element(FRE23_SAMg3$player2, player1vector)

addPlayer1 <- FRE23_SAMg3[ which(FRE23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_SAMg3[ which(FRE23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_SAMg2 <- rbind(FRE23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
FRE23_SAMft <- ftable(FRE23_SAMg2$player1, FRE23_SAMg2$player2)
FRE23_SAMft2 <- as.matrix(FRE23_SAMft)
numRows <- nrow(FRE23_SAMft2)
numCols <- ncol(FRE23_SAMft2)
FRE23_SAMft3 <- FRE23_SAMft2[c(2:numRows) , c(2:numCols)]
FRE23_SAMTable <- graph.adjacency(FRE23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(FRE23_SAMTable, vertex.label = V(FRE23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
FRE23_SAM.clusterCoef <- transitivity(FRE23_SAMTable, type="global") #cluster coefficient
FRE23_SAM.degreeCent <- centralization.degree(FRE23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_SAMftn <- as.network.matrix(FRE23_SAMft)
FRE23_SAM.netDensity <- network.density(FRE23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_SAM.entropy <- entropy(FRE23_SAMft) #entropy

FRE23_SAM.netMx <- cbind(FRE23_SAM.netMx, FRE23_SAM.clusterCoef, FRE23_SAM.degreeCent$centralization,
                         FRE23_SAM.netDensity, FRE23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
FRE23_TAMg2 <- data.frame(FRE23_TAM)
FRE23_TAMg2 <- FRE23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_TAMg2$player1
player2vector <- FRE23_TAMg2$player2
FRE23_TAMg3 <- FRE23_TAMg2
FRE23_TAMg3$p1inp2vec <- is.element(FRE23_TAMg3$player1, player2vector)
FRE23_TAMg3$p2inp1vec <- is.element(FRE23_TAMg3$player2, player1vector)

addPlayer1 <- FRE23_TAMg3[ which(FRE23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_TAMg3[ which(FRE23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_TAMg2 <- rbind(FRE23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
FRE23_TAMft <- ftable(FRE23_TAMg2$player1, FRE23_TAMg2$player2)
FRE23_TAMft2 <- as.matrix(FRE23_TAMft)
numRows <- nrow(FRE23_TAMft2)
numCols <- ncol(FRE23_TAMft2)
FRE23_TAMft3 <- FRE23_TAMft2[c(2:numRows) , c(2:numCols)]
FRE23_TAMTable <- graph.adjacency(FRE23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(FRE23_TAMTable, vertex.label = V(FRE23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
FRE23_TAM.clusterCoef <- transitivity(FRE23_TAMTable, type="global") #cluster coefficient
FRE23_TAM.degreeCent <- centralization.degree(FRE23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_TAMftn <- as.network.matrix(FRE23_TAMft)
FRE23_TAM.netDensity <- network.density(FRE23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_TAM.entropy <- entropy(FRE23_TAMft) #entropy

FRE23_TAM.netMx <- cbind(FRE23_TAM.netMx, FRE23_TAM.clusterCoef, FRE23_TAM.degreeCent$centralization,
                         FRE23_TAM.netDensity, FRE23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
FRE23_SDMg2 <- data.frame(FRE23_SDM)
FRE23_SDMg2 <- FRE23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_SDMg2$player1
player2vector <- FRE23_SDMg2$player2
FRE23_SDMg3 <- FRE23_SDMg2
FRE23_SDMg3$p1inp2vec <- is.element(FRE23_SDMg3$player1, player2vector)
FRE23_SDMg3$p2inp1vec <- is.element(FRE23_SDMg3$player2, player1vector)

addPlayer1 <- FRE23_SDMg3[ which(FRE23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_SDMg3[ which(FRE23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_SDMg2 <- rbind(FRE23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
FRE23_SDMft <- ftable(FRE23_SDMg2$player1, FRE23_SDMg2$player2)
FRE23_SDMft2 <- as.matrix(FRE23_SDMft)
numRows <- nrow(FRE23_SDMft2)
numCols <- ncol(FRE23_SDMft2)
FRE23_SDMft3 <- FRE23_SDMft2[c(2:numRows) , c(2:numCols)]
FRE23_SDMTable <- graph.adjacency(FRE23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(FRE23_SDMTable, vertex.label = V(FRE23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
FRE23_SDM.clusterCoef <- transitivity(FRE23_SDMTable, type="global") #cluster coefficient
FRE23_SDM.degreeCent <- centralization.degree(FRE23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_SDMftn <- as.network.matrix(FRE23_SDMft)
FRE23_SDM.netDensity <- network.density(FRE23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_SDM.entropy <- entropy(FRE23_SDMft) #entropy

FRE23_SDM.netMx <- cbind(FRE23_SDM.netMx, FRE23_SDM.clusterCoef, FRE23_SDM.degreeCent$centralization,
                         FRE23_SDM.netDensity, FRE23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
FRE23_TDMg2 <- data.frame(FRE23_TDM)
FRE23_TDMg2 <- FRE23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_TDMg2$player1
player2vector <- FRE23_TDMg2$player2
FRE23_TDMg3 <- FRE23_TDMg2
FRE23_TDMg3$p1inp2vec <- is.element(FRE23_TDMg3$player1, player2vector)
FRE23_TDMg3$p2inp1vec <- is.element(FRE23_TDMg3$player2, player1vector)

addPlayer1 <- FRE23_TDMg3[ which(FRE23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_TDMg3[ which(FRE23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_TDMg2 <- rbind(FRE23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
FRE23_TDMft <- ftable(FRE23_TDMg2$player1, FRE23_TDMg2$player2)
FRE23_TDMft2 <- as.matrix(FRE23_TDMft)
numRows <- nrow(FRE23_TDMft2)
numCols <- ncol(FRE23_TDMft2)
FRE23_TDMft3 <- FRE23_TDMft2[c(2:numRows) , c(2:numCols)]
FRE23_TDMTable <- graph.adjacency(FRE23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(FRE23_TDMTable, vertex.label = V(FRE23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
FRE23_TDM.clusterCoef <- transitivity(FRE23_TDMTable, type="global") #cluster coefficient
FRE23_TDM.degreeCent <- centralization.degree(FRE23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_TDMftn <- as.network.matrix(FRE23_TDMft)
FRE23_TDM.netDensity <- network.density(FRE23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_TDM.entropy <- entropy(FRE23_TDMft) #entropy

FRE23_TDM.netMx <- cbind(FRE23_TDM.netMx, FRE23_TDM.clusterCoef, FRE23_TDM.degreeCent$centralization,
                         FRE23_TDM.netDensity, FRE23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
FRE23_SDg2 <- data.frame(FRE23_SD)
FRE23_SDg2 <- FRE23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_SDg2$player1
player2vector <- FRE23_SDg2$player2
FRE23_SDg3 <- FRE23_SDg2
FRE23_SDg3$p1inp2vec <- is.element(FRE23_SDg3$player1, player2vector)
FRE23_SDg3$p2inp1vec <- is.element(FRE23_SDg3$player2, player1vector)

addPlayer1 <- FRE23_SDg3[ which(FRE23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_SDg3[ which(FRE23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_SDg2 <- rbind(FRE23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
FRE23_SDft <- ftable(FRE23_SDg2$player1, FRE23_SDg2$player2)
FRE23_SDft2 <- as.matrix(FRE23_SDft)
numRows <- nrow(FRE23_SDft2)
numCols <- ncol(FRE23_SDft2)
FRE23_SDft3 <- FRE23_SDft2[c(2:numRows) , c(2:numCols)]
FRE23_SDTable <- graph.adjacency(FRE23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(FRE23_SDTable, vertex.label = V(FRE23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
FRE23_SD.clusterCoef <- transitivity(FRE23_SDTable, type="global") #cluster coefficient
FRE23_SD.degreeCent <- centralization.degree(FRE23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_SDftn <- as.network.matrix(FRE23_SDft)
FRE23_SD.netDensity <- network.density(FRE23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_SD.entropy <- entropy(FRE23_SDft) #entropy

FRE23_SD.netMx <- cbind(FRE23_SD.netMx, FRE23_SD.clusterCoef, FRE23_SD.degreeCent$centralization,
                        FRE23_SD.netDensity, FRE23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
FRE23_TDg2 <- data.frame(FRE23_TD)
FRE23_TDg2 <- FRE23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_TDg2$player1
player2vector <- FRE23_TDg2$player2
FRE23_TDg3 <- FRE23_TDg2
FRE23_TDg3$p1inp2vec <- is.element(FRE23_TDg3$player1, player2vector)
FRE23_TDg3$p2inp1vec <- is.element(FRE23_TDg3$player2, player1vector)

addPlayer1 <- FRE23_TDg3[ which(FRE23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_TDg3[ which(FRE23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_TDg2 <- rbind(FRE23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
FRE23_TDft <- ftable(FRE23_TDg2$player1, FRE23_TDg2$player2)
FRE23_TDft2 <- as.matrix(FRE23_TDft)
numRows <- nrow(FRE23_TDft2)
numCols <- ncol(FRE23_TDft2)
FRE23_TDft3 <- FRE23_TDft2[c(2:numRows) , c(2:numCols)]
FRE23_TDTable <- graph.adjacency(FRE23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(FRE23_TDTable, vertex.label = V(FRE23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
FRE23_TD.clusterCoef <- transitivity(FRE23_TDTable, type="global") #cluster coefficient
FRE23_TD.degreeCent <- centralization.degree(FRE23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_TDftn <- as.network.matrix(FRE23_TDft)
FRE23_TD.netDensity <- network.density(FRE23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_TD.entropy <- entropy(FRE23_TDft) #entropy

FRE23_TD.netMx <- cbind(FRE23_TD.netMx, FRE23_TD.clusterCoef, FRE23_TD.degreeCent$centralization,
                        FRE23_TD.netDensity, FRE23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
FRE23_QTg2 <- data.frame(FRE23_QT)
FRE23_QTg2 <- FRE23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE23_QTg2$player1
player2vector <- FRE23_QTg2$player2
FRE23_QTg3 <- FRE23_QTg2
FRE23_QTg3$p1inp2vec <- is.element(FRE23_QTg3$player1, player2vector)
FRE23_QTg3$p2inp1vec <- is.element(FRE23_QTg3$player2, player1vector)

addPlayer1 <- FRE23_QTg3[ which(FRE23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE23_QTg3[ which(FRE23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE23_QTg2 <- rbind(FRE23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
FRE23_QTft <- ftable(FRE23_QTg2$player1, FRE23_QTg2$player2)
FRE23_QTft2 <- as.matrix(FRE23_QTft)
numRows <- nrow(FRE23_QTft2)
numCols <- ncol(FRE23_QTft2)
FRE23_QTft3 <- FRE23_QTft2[c(2:numRows) , c(2:numCols)]
FRE23_QTTable <- graph.adjacency(FRE23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(FRE23_QTTable, vertex.label = V(FRE23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
FRE23_QT.clusterCoef <- transitivity(FRE23_QTTable, type="global") #cluster coefficient
FRE23_QT.degreeCent <- centralization.degree(FRE23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE23_QTftn <- as.network.matrix(FRE23_QTft)
FRE23_QT.netDensity <- network.density(FRE23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE23_QT.entropy <- entropy(FRE23_QTft) #entropy

FRE23_QT.netMx <- cbind(FRE23_QT.netMx, FRE23_QT.clusterCoef, FRE23_QT.degreeCent$centralization,
                        FRE23_QT.netDensity, FRE23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE23_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
GCFC23_Gg2 <- data.frame(GCFC23_G)
GCFC23_Gg2 <- GCFC23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_Gg2$player1
player2vector <- GCFC23_Gg2$player2
GCFC23_Gg3 <- GCFC23_Gg2
GCFC23_Gg3$p1inp2vec <- is.element(GCFC23_Gg3$player1, player2vector)
GCFC23_Gg3$p2inp1vec <- is.element(GCFC23_Gg3$player2, player1vector)

addPlayer1 <- GCFC23_Gg3[ which(GCFC23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_Gg3[ which(GCFC23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_Gg2 <- rbind(GCFC23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
GCFC23_Gft <- ftable(GCFC23_Gg2$player1, GCFC23_Gg2$player2)
GCFC23_Gft2 <- as.matrix(GCFC23_Gft)
numRows <- nrow(GCFC23_Gft2)
numCols <- ncol(GCFC23_Gft2)
GCFC23_Gft3 <- GCFC23_Gft2[c(2:numRows) , c(2:numCols)]
GCFC23_GTable <- graph.adjacency(GCFC23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(GCFC23_GTable, vertex.label = V(GCFC23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
GCFC23_G.clusterCoef <- transitivity(GCFC23_GTable, type="global") #cluster coefficient
GCFC23_G.degreeCent <- centralization.degree(GCFC23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_Gftn <- as.network.matrix(GCFC23_Gft)
GCFC23_G.netDensity <- network.density(GCFC23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_G.entropy <- entropy(GCFC23_Gft) #entropy

GCFC23_G.netMx <- cbind(GCFC23_G.netMx, GCFC23_G.clusterCoef, GCFC23_G.degreeCent$centralization,
                        GCFC23_G.netDensity, GCFC23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
GCFC23_Bg2 <- data.frame(GCFC23_B)
GCFC23_Bg2 <- GCFC23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_Bg2$player1
player2vector <- GCFC23_Bg2$player2
GCFC23_Bg3 <- GCFC23_Bg2
GCFC23_Bg3$p1inp2vec <- is.element(GCFC23_Bg3$player1, player2vector)
GCFC23_Bg3$p2inp1vec <- is.element(GCFC23_Bg3$player2, player1vector)

addPlayer1 <- GCFC23_Bg3[ which(GCFC23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_Bg3[ which(GCFC23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_Bg2 <- rbind(GCFC23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
GCFC23_Bft <- ftable(GCFC23_Bg2$player1, GCFC23_Bg2$player2)
GCFC23_Bft2 <- as.matrix(GCFC23_Bft)
numRows <- nrow(GCFC23_Bft2)
numCols <- ncol(GCFC23_Bft2)
GCFC23_Bft3 <- GCFC23_Bft2[c(2:numRows) , c(2:numCols)]
GCFC23_BTable <- graph.adjacency(GCFC23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(GCFC23_BTable, vertex.label = V(GCFC23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
GCFC23_B.clusterCoef <- transitivity(GCFC23_BTable, type="global") #cluster coefficient
GCFC23_B.degreeCent <- centralization.degree(GCFC23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_Bftn <- as.network.matrix(GCFC23_Bft)
GCFC23_B.netDensity <- network.density(GCFC23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_B.entropy <- entropy(GCFC23_Bft) #entropy

GCFC23_B.netMx <- cbind(GCFC23_B.netMx, GCFC23_B.clusterCoef, GCFC23_B.degreeCent$centralization,
                        GCFC23_B.netDensity, GCFC23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
GCFC23_SFg2 <- data.frame(GCFC23_SF)
GCFC23_SFg2 <- GCFC23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_SFg2$player1
player2vector <- GCFC23_SFg2$player2
GCFC23_SFg3 <- GCFC23_SFg2
GCFC23_SFg3$p1inp2vec <- is.element(GCFC23_SFg3$player1, player2vector)
GCFC23_SFg3$p2inp1vec <- is.element(GCFC23_SFg3$player2, player1vector)

addPlayer1 <- GCFC23_SFg3[ which(GCFC23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC23_SFg2 <- rbind(GCFC23_SFg2, addPlayer1)

#ROUND 23, FWD Stoppage graph using weighted edges
GCFC23_SFft <- ftable(GCFC23_SFg2$player1, GCFC23_SFg2$player2)
GCFC23_SFft2 <- as.matrix(GCFC23_SFft)
numRows <- nrow(GCFC23_SFft2)
numCols <- ncol(GCFC23_SFft2)
GCFC23_SFft3 <- GCFC23_SFft2[c(2:numRows) , c(1:numCols)]
GCFC23_SFTable <- graph.adjacency(GCFC23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(GCFC23_SFTable, vertex.label = V(GCFC23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
GCFC23_SF.clusterCoef <- transitivity(GCFC23_SFTable, type="global") #cluster coefficient
GCFC23_SF.degreeCent <- centralization.degree(GCFC23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_SFftn <- as.network.matrix(GCFC23_SFft)
GCFC23_SF.netDensity <- network.density(GCFC23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_SF.entropy <- entropy(GCFC23_SFft) #entropy

GCFC23_SF.netMx <- cbind(GCFC23_SF.netMx, GCFC23_SF.clusterCoef, GCFC23_SF.degreeCent$centralization,
                         GCFC23_SF.netDensity, GCFC23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
GCFC23_TFg2 <- data.frame(GCFC23_TF)
GCFC23_TFg2 <- GCFC23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_TFg2$player1
player2vector <- GCFC23_TFg2$player2
GCFC23_TFg3 <- GCFC23_TFg2
GCFC23_TFg3$p1inp2vec <- is.element(GCFC23_TFg3$player1, player2vector)
GCFC23_TFg3$p2inp1vec <- is.element(GCFC23_TFg3$player2, player1vector)

addPlayer1 <- GCFC23_TFg3[ which(GCFC23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC23_TFg3[ which(GCFC23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_TFg2 <- rbind(GCFC23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
GCFC23_TFft <- ftable(GCFC23_TFg2$player1, GCFC23_TFg2$player2)
GCFC23_TFft2 <- as.matrix(GCFC23_TFft)
numRows <- nrow(GCFC23_TFft2)
numCols <- ncol(GCFC23_TFft2)
GCFC23_TFft3 <- GCFC23_TFft2[c(2:numRows) , c(2:numCols)]
GCFC23_TFTable <- graph.adjacency(GCFC23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(GCFC23_TFTable, vertex.label = V(GCFC23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
GCFC23_TF.clusterCoef <- transitivity(GCFC23_TFTable, type="global") #cluster coefficient
GCFC23_TF.degreeCent <- centralization.degree(GCFC23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_TFftn <- as.network.matrix(GCFC23_TFft)
GCFC23_TF.netDensity <- network.density(GCFC23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_TF.entropy <- entropy(GCFC23_TFft) #entropy

GCFC23_TF.netMx <- cbind(GCFC23_TF.netMx, GCFC23_TF.clusterCoef, GCFC23_TF.degreeCent$centralization,
                         GCFC23_TF.netDensity, GCFC23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
GCFC23_SAMg2 <- data.frame(GCFC23_SAM)
GCFC23_SAMg2 <- GCFC23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_SAMg2$player1
player2vector <- GCFC23_SAMg2$player2
GCFC23_SAMg3 <- GCFC23_SAMg2
GCFC23_SAMg3$p1inp2vec <- is.element(GCFC23_SAMg3$player1, player2vector)
GCFC23_SAMg3$p2inp1vec <- is.element(GCFC23_SAMg3$player2, player1vector)

addPlayer1 <- GCFC23_SAMg3[ which(GCFC23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_SAMg3[ which(GCFC23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_SAMg2 <- rbind(GCFC23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
GCFC23_SAMft <- ftable(GCFC23_SAMg2$player1, GCFC23_SAMg2$player2)
GCFC23_SAMft2 <- as.matrix(GCFC23_SAMft)
numRows <- nrow(GCFC23_SAMft2)
numCols <- ncol(GCFC23_SAMft2)
GCFC23_SAMft3 <- GCFC23_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC23_SAMTable <- graph.adjacency(GCFC23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(GCFC23_SAMTable, vertex.label = V(GCFC23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
GCFC23_SAM.clusterCoef <- transitivity(GCFC23_SAMTable, type="global") #cluster coefficient
GCFC23_SAM.degreeCent <- centralization.degree(GCFC23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_SAMftn <- as.network.matrix(GCFC23_SAMft)
GCFC23_SAM.netDensity <- network.density(GCFC23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_SAM.entropy <- entropy(GCFC23_SAMft) #entropy

GCFC23_SAM.netMx <- cbind(GCFC23_SAM.netMx, GCFC23_SAM.clusterCoef, GCFC23_SAM.degreeCent$centralization,
                          GCFC23_SAM.netDensity, GCFC23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
GCFC23_TAMg2 <- data.frame(GCFC23_TAM)
GCFC23_TAMg2 <- GCFC23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_TAMg2$player1
player2vector <- GCFC23_TAMg2$player2
GCFC23_TAMg3 <- GCFC23_TAMg2
GCFC23_TAMg3$p1inp2vec <- is.element(GCFC23_TAMg3$player1, player2vector)
GCFC23_TAMg3$p2inp1vec <- is.element(GCFC23_TAMg3$player2, player1vector)

addPlayer1 <- GCFC23_TAMg3[ which(GCFC23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_TAMg3[ which(GCFC23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_TAMg2 <- rbind(GCFC23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
GCFC23_TAMft <- ftable(GCFC23_TAMg2$player1, GCFC23_TAMg2$player2)
GCFC23_TAMft2 <- as.matrix(GCFC23_TAMft)
numRows <- nrow(GCFC23_TAMft2)
numCols <- ncol(GCFC23_TAMft2)
GCFC23_TAMft3 <- GCFC23_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC23_TAMTable <- graph.adjacency(GCFC23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(GCFC23_TAMTable, vertex.label = V(GCFC23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
GCFC23_TAM.clusterCoef <- transitivity(GCFC23_TAMTable, type="global") #cluster coefficient
GCFC23_TAM.degreeCent <- centralization.degree(GCFC23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_TAMftn <- as.network.matrix(GCFC23_TAMft)
GCFC23_TAM.netDensity <- network.density(GCFC23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_TAM.entropy <- entropy(GCFC23_TAMft) #entropy

GCFC23_TAM.netMx <- cbind(GCFC23_TAM.netMx, GCFC23_TAM.clusterCoef, GCFC23_TAM.degreeCent$centralization,
                          GCFC23_TAM.netDensity, GCFC23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
GCFC23_SDMg2 <- data.frame(GCFC23_SDM)
GCFC23_SDMg2 <- GCFC23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_SDMg2$player1
player2vector <- GCFC23_SDMg2$player2
GCFC23_SDMg3 <- GCFC23_SDMg2
GCFC23_SDMg3$p1inp2vec <- is.element(GCFC23_SDMg3$player1, player2vector)
GCFC23_SDMg3$p2inp1vec <- is.element(GCFC23_SDMg3$player2, player1vector)

addPlayer1 <- GCFC23_SDMg3[ which(GCFC23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_SDMg3[ which(GCFC23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_SDMg2 <- rbind(GCFC23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
GCFC23_SDMft <- ftable(GCFC23_SDMg2$player1, GCFC23_SDMg2$player2)
GCFC23_SDMft2 <- as.matrix(GCFC23_SDMft)
numRows <- nrow(GCFC23_SDMft2)
numCols <- ncol(GCFC23_SDMft2)
GCFC23_SDMft3 <- GCFC23_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC23_SDMTable <- graph.adjacency(GCFC23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(GCFC23_SDMTable, vertex.label = V(GCFC23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
GCFC23_SDM.clusterCoef <- transitivity(GCFC23_SDMTable, type="global") #cluster coefficient
GCFC23_SDM.degreeCent <- centralization.degree(GCFC23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_SDMftn <- as.network.matrix(GCFC23_SDMft)
GCFC23_SDM.netDensity <- network.density(GCFC23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_SDM.entropy <- entropy(GCFC23_SDMft) #entropy

GCFC23_SDM.netMx <- cbind(GCFC23_SDM.netMx, GCFC23_SDM.clusterCoef, GCFC23_SDM.degreeCent$centralization,
                          GCFC23_SDM.netDensity, GCFC23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
GCFC23_TDMg2 <- data.frame(GCFC23_TDM)
GCFC23_TDMg2 <- GCFC23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_TDMg2$player1
player2vector <- GCFC23_TDMg2$player2
GCFC23_TDMg3 <- GCFC23_TDMg2
GCFC23_TDMg3$p1inp2vec <- is.element(GCFC23_TDMg3$player1, player2vector)
GCFC23_TDMg3$p2inp1vec <- is.element(GCFC23_TDMg3$player2, player1vector)

addPlayer1 <- GCFC23_TDMg3[ which(GCFC23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_TDMg3[ which(GCFC23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_TDMg2 <- rbind(GCFC23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
GCFC23_TDMft <- ftable(GCFC23_TDMg2$player1, GCFC23_TDMg2$player2)
GCFC23_TDMft2 <- as.matrix(GCFC23_TDMft)
numRows <- nrow(GCFC23_TDMft2)
numCols <- ncol(GCFC23_TDMft2)
GCFC23_TDMft3 <- GCFC23_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC23_TDMTable <- graph.adjacency(GCFC23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(GCFC23_TDMTable, vertex.label = V(GCFC23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
GCFC23_TDM.clusterCoef <- transitivity(GCFC23_TDMTable, type="global") #cluster coefficient
GCFC23_TDM.degreeCent <- centralization.degree(GCFC23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_TDMftn <- as.network.matrix(GCFC23_TDMft)
GCFC23_TDM.netDensity <- network.density(GCFC23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_TDM.entropy <- entropy(GCFC23_TDMft) #entropy

GCFC23_TDM.netMx <- cbind(GCFC23_TDM.netMx, GCFC23_TDM.clusterCoef, GCFC23_TDM.degreeCent$centralization,
                          GCFC23_TDM.netDensity, GCFC23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
GCFC23_SDg2 <- data.frame(GCFC23_SD)
GCFC23_SDg2 <- GCFC23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_SDg2$player1
player2vector <- GCFC23_SDg2$player2
GCFC23_SDg3 <- GCFC23_SDg2
GCFC23_SDg3$p1inp2vec <- is.element(GCFC23_SDg3$player1, player2vector)
GCFC23_SDg3$p2inp1vec <- is.element(GCFC23_SDg3$player2, player1vector)

addPlayer1 <- GCFC23_SDg3[ which(GCFC23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_SDg3[ which(GCFC23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_SDg2 <- rbind(GCFC23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
GCFC23_SDft <- ftable(GCFC23_SDg2$player1, GCFC23_SDg2$player2)
GCFC23_SDft2 <- as.matrix(GCFC23_SDft)
numRows <- nrow(GCFC23_SDft2)
numCols <- ncol(GCFC23_SDft2)
GCFC23_SDft3 <- GCFC23_SDft2[c(2:numRows) , c(2:numCols)]
GCFC23_SDTable <- graph.adjacency(GCFC23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(GCFC23_SDTable, vertex.label = V(GCFC23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
GCFC23_SD.clusterCoef <- transitivity(GCFC23_SDTable, type="global") #cluster coefficient
GCFC23_SD.degreeCent <- centralization.degree(GCFC23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_SDftn <- as.network.matrix(GCFC23_SDft)
GCFC23_SD.netDensity <- network.density(GCFC23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_SD.entropy <- entropy(GCFC23_SDft) #entropy

GCFC23_SD.netMx <- cbind(GCFC23_SD.netMx, GCFC23_SD.clusterCoef, GCFC23_SD.degreeCent$centralization,
                         GCFC23_SD.netDensity, GCFC23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
GCFC23_TDg2 <- data.frame(GCFC23_TD)
GCFC23_TDg2 <- GCFC23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_TDg2$player1
player2vector <- GCFC23_TDg2$player2
GCFC23_TDg3 <- GCFC23_TDg2
GCFC23_TDg3$p1inp2vec <- is.element(GCFC23_TDg3$player1, player2vector)
GCFC23_TDg3$p2inp1vec <- is.element(GCFC23_TDg3$player2, player1vector)

addPlayer1 <- GCFC23_TDg3[ which(GCFC23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_TDg3[ which(GCFC23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_TDg2 <- rbind(GCFC23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
GCFC23_TDft <- ftable(GCFC23_TDg2$player1, GCFC23_TDg2$player2)
GCFC23_TDft2 <- as.matrix(GCFC23_TDft)
numRows <- nrow(GCFC23_TDft2)
numCols <- ncol(GCFC23_TDft2)
GCFC23_TDft3 <- GCFC23_TDft2[c(2:numRows) , c(2:numCols)]
GCFC23_TDTable <- graph.adjacency(GCFC23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(GCFC23_TDTable, vertex.label = V(GCFC23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
GCFC23_TD.clusterCoef <- transitivity(GCFC23_TDTable, type="global") #cluster coefficient
GCFC23_TD.degreeCent <- centralization.degree(GCFC23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_TDftn <- as.network.matrix(GCFC23_TDft)
GCFC23_TD.netDensity <- network.density(GCFC23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_TD.entropy <- entropy(GCFC23_TDft) #entropy

GCFC23_TD.netMx <- cbind(GCFC23_TD.netMx, GCFC23_TD.clusterCoef, GCFC23_TD.degreeCent$centralization,
                         GCFC23_TD.netDensity, GCFC23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
GCFC23_QTg2 <- data.frame(GCFC23_QT)
GCFC23_QTg2 <- GCFC23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC23_QTg2$player1
player2vector <- GCFC23_QTg2$player2
GCFC23_QTg3 <- GCFC23_QTg2
GCFC23_QTg3$p1inp2vec <- is.element(GCFC23_QTg3$player1, player2vector)
GCFC23_QTg3$p2inp1vec <- is.element(GCFC23_QTg3$player2, player1vector)

addPlayer1 <- GCFC23_QTg3[ which(GCFC23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC23_QTg3[ which(GCFC23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC23_QTg2 <- rbind(GCFC23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
GCFC23_QTft <- ftable(GCFC23_QTg2$player1, GCFC23_QTg2$player2)
GCFC23_QTft2 <- as.matrix(GCFC23_QTft)
numRows <- nrow(GCFC23_QTft2)
numCols <- ncol(GCFC23_QTft2)
GCFC23_QTft3 <- GCFC23_QTft2[c(2:numRows) , c(2:numCols)]
GCFC23_QTTable <- graph.adjacency(GCFC23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(GCFC23_QTTable, vertex.label = V(GCFC23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
GCFC23_QT.clusterCoef <- transitivity(GCFC23_QTTable, type="global") #cluster coefficient
GCFC23_QT.degreeCent <- centralization.degree(GCFC23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC23_QTftn <- as.network.matrix(GCFC23_QTft)
GCFC23_QT.netDensity <- network.density(GCFC23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC23_QT.entropy <- entropy(GCFC23_QTft) #entropy

GCFC23_QT.netMx <- cbind(GCFC23_QT.netMx, GCFC23_QT.clusterCoef, GCFC23_QT.degreeCent$centralization,
                         GCFC23_QT.netDensity, GCFC23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC23_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
GEEL23_Gg2 <- data.frame(GEEL23_G)
GEEL23_Gg2 <- GEEL23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_Gg2$player1
player2vector <- GEEL23_Gg2$player2
GEEL23_Gg3 <- GEEL23_Gg2
GEEL23_Gg3$p1inp2vec <- is.element(GEEL23_Gg3$player1, player2vector)
GEEL23_Gg3$p2inp1vec <- is.element(GEEL23_Gg3$player2, player1vector)

addPlayer1 <- GEEL23_Gg3[ which(GEEL23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_Gg3[ which(GEEL23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_Gg2 <- rbind(GEEL23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
GEEL23_Gft <- ftable(GEEL23_Gg2$player1, GEEL23_Gg2$player2)
GEEL23_Gft2 <- as.matrix(GEEL23_Gft)
numRows <- nrow(GEEL23_Gft2)
numCols <- ncol(GEEL23_Gft2)
GEEL23_Gft3 <- GEEL23_Gft2[c(2:numRows) , c(2:numCols)]
GEEL23_GTable <- graph.adjacency(GEEL23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(GEEL23_GTable, vertex.label = V(GEEL23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
GEEL23_G.clusterCoef <- transitivity(GEEL23_GTable, type="global") #cluster coefficient
GEEL23_G.degreeCent <- centralization.degree(GEEL23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_Gftn <- as.network.matrix(GEEL23_Gft)
GEEL23_G.netDensity <- network.density(GEEL23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_G.entropy <- entropy(GEEL23_Gft) #entropy

GEEL23_G.netMx <- cbind(GEEL23_G.netMx, GEEL23_G.clusterCoef, GEEL23_G.degreeCent$centralization,
                        GEEL23_G.netDensity, GEEL23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
GEEL23_Bg2 <- data.frame(GEEL23_B)
GEEL23_Bg2 <- GEEL23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_Bg2$player1
player2vector <- GEEL23_Bg2$player2
GEEL23_Bg3 <- GEEL23_Bg2
GEEL23_Bg3$p1inp2vec <- is.element(GEEL23_Bg3$player1, player2vector)
GEEL23_Bg3$p2inp1vec <- is.element(GEEL23_Bg3$player2, player1vector)

addPlayer1 <- GEEL23_Bg3[ which(GEEL23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_Bg3[ which(GEEL23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_Bg2 <- rbind(GEEL23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
GEEL23_Bft <- ftable(GEEL23_Bg2$player1, GEEL23_Bg2$player2)
GEEL23_Bft2 <- as.matrix(GEEL23_Bft)
numRows <- nrow(GEEL23_Bft2)
numCols <- ncol(GEEL23_Bft2)
GEEL23_Bft3 <- GEEL23_Bft2[c(2:numRows) , c(2:numCols)]
GEEL23_BTable <- graph.adjacency(GEEL23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(GEEL23_BTable, vertex.label = V(GEEL23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
GEEL23_B.clusterCoef <- transitivity(GEEL23_BTable, type="global") #cluster coefficient
GEEL23_B.degreeCent <- centralization.degree(GEEL23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_Bftn <- as.network.matrix(GEEL23_Bft)
GEEL23_B.netDensity <- network.density(GEEL23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_B.entropy <- entropy(GEEL23_Bft) #entropy

GEEL23_B.netMx <- cbind(GEEL23_B.netMx, GEEL23_B.clusterCoef, GEEL23_B.degreeCent$centralization,
                        GEEL23_B.netDensity, GEEL23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************

round = 23
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
GEEL23_SFg2 <- data.frame(GEEL23_SF)
GEEL23_SFg2 <- GEEL23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_SFg2$player1
player2vector <- GEEL23_SFg2$player2
GEEL23_SFg3 <- GEEL23_SFg2
GEEL23_SFg3$p1inp2vec <- is.element(GEEL23_SFg3$player1, player2vector)
GEEL23_SFg3$p2inp1vec <- is.element(GEEL23_SFg3$player2, player1vector)

addPlayer1 <- GEEL23_SFg3[ which(GEEL23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL23_SFg3[ which(GEEL23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_SFg2 <- rbind(GEEL23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
GEEL23_SFft <- ftable(GEEL23_SFg2$player1, GEEL23_SFg2$player2)
GEEL23_SFft2 <- as.matrix(GEEL23_SFft)
numRows <- nrow(GEEL23_SFft2)
numCols <- ncol(GEEL23_SFft2)
GEEL23_SFft3 <- GEEL23_SFft2[c(2:numRows) , c(2:numCols)]
GEEL23_SFTable <- graph.adjacency(GEEL23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(GEEL23_SFTable, vertex.label = V(GEEL23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
GEEL23_SF.clusterCoef <- transitivity(GEEL23_SFTable, type="global") #cluster coefficient
GEEL23_SF.degreeCent <- centralization.degree(GEEL23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_SFftn <- as.network.matrix(GEEL23_SFft)
GEEL23_SF.netDensity <- network.density(GEEL23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_SF.entropy <- entropy(GEEL23_SFft) #entropy

GEEL23_SF.netMx <- cbind(GEEL23_SF.netMx, GEEL23_SF.clusterCoef, GEEL23_SF.degreeCent$centralization,
                         GEEL23_SF.netDensity, GEEL23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
GEEL23_TFg2 <- data.frame(GEEL23_TF)
GEEL23_TFg2 <- GEEL23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_TFg2$player1
player2vector <- GEEL23_TFg2$player2
GEEL23_TFg3 <- GEEL23_TFg2
GEEL23_TFg3$p1inp2vec <- is.element(GEEL23_TFg3$player1, player2vector)
GEEL23_TFg3$p2inp1vec <- is.element(GEEL23_TFg3$player2, player1vector)

addPlayer1 <- GEEL23_TFg3[ which(GEEL23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_TFg3[ which(GEEL23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_TFg2 <- rbind(GEEL23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
GEEL23_TFft <- ftable(GEEL23_TFg2$player1, GEEL23_TFg2$player2)
GEEL23_TFft2 <- as.matrix(GEEL23_TFft)
numRows <- nrow(GEEL23_TFft2)
numCols <- ncol(GEEL23_TFft2)
GEEL23_TFft3 <- GEEL23_TFft2[c(2:numRows) , c(2:numCols)]
GEEL23_TFTable <- graph.adjacency(GEEL23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(GEEL23_TFTable, vertex.label = V(GEEL23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
GEEL23_TF.clusterCoef <- transitivity(GEEL23_TFTable, type="global") #cluster coefficient
GEEL23_TF.degreeCent <- centralization.degree(GEEL23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_TFftn <- as.network.matrix(GEEL23_TFft)
GEEL23_TF.netDensity <- network.density(GEEL23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_TF.entropy <- entropy(GEEL23_TFft) #entropy

GEEL23_TF.netMx <- cbind(GEEL23_TF.netMx, GEEL23_TF.clusterCoef, GEEL23_TF.degreeCent$centralization,
                         GEEL23_TF.netDensity, GEEL23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************

round = 23
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
GEEL23_SAMg2 <- data.frame(GEEL23_SAM)
GEEL23_SAMg2 <- GEEL23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_SAMg2$player1
player2vector <- GEEL23_SAMg2$player2
GEEL23_SAMg3 <- GEEL23_SAMg2
GEEL23_SAMg3$p1inp2vec <- is.element(GEEL23_SAMg3$player1, player2vector)
GEEL23_SAMg3$p2inp1vec <- is.element(GEEL23_SAMg3$player2, player1vector)

addPlayer1 <- GEEL23_SAMg3[ which(GEEL23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_SAMg3[ which(GEEL23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_SAMg2 <- rbind(GEEL23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
GEEL23_SAMft <- ftable(GEEL23_SAMg2$player1, GEEL23_SAMg2$player2)
GEEL23_SAMft2 <- as.matrix(GEEL23_SAMft)
numRows <- nrow(GEEL23_SAMft2)
numCols <- ncol(GEEL23_SAMft2)
GEEL23_SAMft3 <- GEEL23_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL23_SAMTable <- graph.adjacency(GEEL23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(GEEL23_SAMTable, vertex.label = V(GEEL23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
GEEL23_SAM.clusterCoef <- transitivity(GEEL23_SAMTable, type="global") #cluster coefficient
GEEL23_SAM.degreeCent <- centralization.degree(GEEL23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_SAMftn <- as.network.matrix(GEEL23_SAMft)
GEEL23_SAM.netDensity <- network.density(GEEL23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_SAM.entropy <- entropy(GEEL23_SAMft) #entropy

GEEL23_SAM.netMx <- cbind(GEEL23_SAM.netMx, GEEL23_SAM.clusterCoef, GEEL23_SAM.degreeCent$centralization,
                          GEEL23_SAM.netDensity, GEEL23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
GEEL23_TAMg2 <- data.frame(GEEL23_TAM)
GEEL23_TAMg2 <- GEEL23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_TAMg2$player1
player2vector <- GEEL23_TAMg2$player2
GEEL23_TAMg3 <- GEEL23_TAMg2
GEEL23_TAMg3$p1inp2vec <- is.element(GEEL23_TAMg3$player1, player2vector)
GEEL23_TAMg3$p2inp1vec <- is.element(GEEL23_TAMg3$player2, player1vector)

addPlayer1 <- GEEL23_TAMg3[ which(GEEL23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_TAMg3[ which(GEEL23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_TAMg2 <- rbind(GEEL23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
GEEL23_TAMft <- ftable(GEEL23_TAMg2$player1, GEEL23_TAMg2$player2)
GEEL23_TAMft2 <- as.matrix(GEEL23_TAMft)
numRows <- nrow(GEEL23_TAMft2)
numCols <- ncol(GEEL23_TAMft2)
GEEL23_TAMft3 <- GEEL23_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL23_TAMTable <- graph.adjacency(GEEL23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(GEEL23_TAMTable, vertex.label = V(GEEL23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
GEEL23_TAM.clusterCoef <- transitivity(GEEL23_TAMTable, type="global") #cluster coefficient
GEEL23_TAM.degreeCent <- centralization.degree(GEEL23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_TAMftn <- as.network.matrix(GEEL23_TAMft)
GEEL23_TAM.netDensity <- network.density(GEEL23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_TAM.entropy <- entropy(GEEL23_TAMft) #entropy

GEEL23_TAM.netMx <- cbind(GEEL23_TAM.netMx, GEEL23_TAM.clusterCoef, GEEL23_TAM.degreeCent$centralization,
                          GEEL23_TAM.netDensity, GEEL23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
GEEL23_SDMg2 <- data.frame(GEEL23_SDM)
GEEL23_SDMg2 <- GEEL23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_SDMg2$player1
player2vector <- GEEL23_SDMg2$player2
GEEL23_SDMg3 <- GEEL23_SDMg2
GEEL23_SDMg3$p1inp2vec <- is.element(GEEL23_SDMg3$player1, player2vector)
GEEL23_SDMg3$p2inp1vec <- is.element(GEEL23_SDMg3$player2, player1vector)

addPlayer1 <- GEEL23_SDMg3[ which(GEEL23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_SDMg3[ which(GEEL23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_SDMg2 <- rbind(GEEL23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
GEEL23_SDMft <- ftable(GEEL23_SDMg2$player1, GEEL23_SDMg2$player2)
GEEL23_SDMft2 <- as.matrix(GEEL23_SDMft)
numRows <- nrow(GEEL23_SDMft2)
numCols <- ncol(GEEL23_SDMft2)
GEEL23_SDMft3 <- GEEL23_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL23_SDMTable <- graph.adjacency(GEEL23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(GEEL23_SDMTable, vertex.label = V(GEEL23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
GEEL23_SDM.clusterCoef <- transitivity(GEEL23_SDMTable, type="global") #cluster coefficient
GEEL23_SDM.degreeCent <- centralization.degree(GEEL23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_SDMftn <- as.network.matrix(GEEL23_SDMft)
GEEL23_SDM.netDensity <- network.density(GEEL23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_SDM.entropy <- entropy(GEEL23_SDMft) #entropy

GEEL23_SDM.netMx <- cbind(GEEL23_SDM.netMx, GEEL23_SDM.clusterCoef, GEEL23_SDM.degreeCent$centralization,
                          GEEL23_SDM.netDensity, GEEL23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
GEEL23_TDMg2 <- data.frame(GEEL23_TDM)
GEEL23_TDMg2 <- GEEL23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_TDMg2$player1
player2vector <- GEEL23_TDMg2$player2
GEEL23_TDMg3 <- GEEL23_TDMg2
GEEL23_TDMg3$p1inp2vec <- is.element(GEEL23_TDMg3$player1, player2vector)
GEEL23_TDMg3$p2inp1vec <- is.element(GEEL23_TDMg3$player2, player1vector)

addPlayer1 <- GEEL23_TDMg3[ which(GEEL23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_TDMg3[ which(GEEL23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_TDMg2 <- rbind(GEEL23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
GEEL23_TDMft <- ftable(GEEL23_TDMg2$player1, GEEL23_TDMg2$player2)
GEEL23_TDMft2 <- as.matrix(GEEL23_TDMft)
numRows <- nrow(GEEL23_TDMft2)
numCols <- ncol(GEEL23_TDMft2)
GEEL23_TDMft3 <- GEEL23_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL23_TDMTable <- graph.adjacency(GEEL23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(GEEL23_TDMTable, vertex.label = V(GEEL23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
GEEL23_TDM.clusterCoef <- transitivity(GEEL23_TDMTable, type="global") #cluster coefficient
GEEL23_TDM.degreeCent <- centralization.degree(GEEL23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_TDMftn <- as.network.matrix(GEEL23_TDMft)
GEEL23_TDM.netDensity <- network.density(GEEL23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_TDM.entropy <- entropy(GEEL23_TDMft) #entropy

GEEL23_TDM.netMx <- cbind(GEEL23_TDM.netMx, GEEL23_TDM.clusterCoef, GEEL23_TDM.degreeCent$centralization,
                          GEEL23_TDM.netDensity, GEEL23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
GEEL23_SDg2 <- data.frame(GEEL23_SD)
GEEL23_SDg2 <- GEEL23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_SDg2$player1
player2vector <- GEEL23_SDg2$player2
GEEL23_SDg3 <- GEEL23_SDg2
GEEL23_SDg3$p1inp2vec <- is.element(GEEL23_SDg3$player1, player2vector)
GEEL23_SDg3$p2inp1vec <- is.element(GEEL23_SDg3$player2, player1vector)

addPlayer1 <- GEEL23_SDg3[ which(GEEL23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_SDg3[ which(GEEL23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_SDg2 <- rbind(GEEL23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
GEEL23_SDft <- ftable(GEEL23_SDg2$player1, GEEL23_SDg2$player2)
GEEL23_SDft2 <- as.matrix(GEEL23_SDft)
numRows <- nrow(GEEL23_SDft2)
numCols <- ncol(GEEL23_SDft2)
GEEL23_SDft3 <- GEEL23_SDft2[c(2:numRows) , c(2:numCols)]
GEEL23_SDTable <- graph.adjacency(GEEL23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(GEEL23_SDTable, vertex.label = V(GEEL23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
GEEL23_SD.clusterCoef <- transitivity(GEEL23_SDTable, type="global") #cluster coefficient
GEEL23_SD.degreeCent <- centralization.degree(GEEL23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_SDftn <- as.network.matrix(GEEL23_SDft)
GEEL23_SD.netDensity <- network.density(GEEL23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_SD.entropy <- entropy(GEEL23_SDft) #entropy

GEEL23_SD.netMx <- cbind(GEEL23_SD.netMx, GEEL23_SD.clusterCoef, GEEL23_SD.degreeCent$centralization,
                         GEEL23_SD.netDensity, GEEL23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************

round = 23
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
GEEL23_TDg2 <- data.frame(GEEL23_TD)
GEEL23_TDg2 <- GEEL23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_TDg2$player1
player2vector <- GEEL23_TDg2$player2
GEEL23_TDg3 <- GEEL23_TDg2
GEEL23_TDg3$p1inp2vec <- is.element(GEEL23_TDg3$player1, player2vector)
GEEL23_TDg3$p2inp1vec <- is.element(GEEL23_TDg3$player2, player1vector)

addPlayer1 <- GEEL23_TDg3[ which(GEEL23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_TDg3[ which(GEEL23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_TDg2 <- rbind(GEEL23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
GEEL23_TDft <- ftable(GEEL23_TDg2$player1, GEEL23_TDg2$player2)
GEEL23_TDft2 <- as.matrix(GEEL23_TDft)
numRows <- nrow(GEEL23_TDft2)
numCols <- ncol(GEEL23_TDft2)
GEEL23_TDft3 <- GEEL23_TDft2[c(2:numRows) , c(2:numCols)]
GEEL23_TDTable <- graph.adjacency(GEEL23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(GEEL23_TDTable, vertex.label = V(GEEL23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
GEEL23_TD.clusterCoef <- transitivity(GEEL23_TDTable, type="global") #cluster coefficient
GEEL23_TD.degreeCent <- centralization.degree(GEEL23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_TDftn <- as.network.matrix(GEEL23_TDft)
GEEL23_TD.netDensity <- network.density(GEEL23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_TD.entropy <- entropy(GEEL23_TDft) #entropy

GEEL23_TD.netMx <- cbind(GEEL23_TD.netMx, GEEL23_TD.clusterCoef, GEEL23_TD.degreeCent$centralization,
                         GEEL23_TD.netDensity, GEEL23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
GEEL23_QTg2 <- data.frame(GEEL23_QT)
GEEL23_QTg2 <- GEEL23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL23_QTg2$player1
player2vector <- GEEL23_QTg2$player2
GEEL23_QTg3 <- GEEL23_QTg2
GEEL23_QTg3$p1inp2vec <- is.element(GEEL23_QTg3$player1, player2vector)
GEEL23_QTg3$p2inp1vec <- is.element(GEEL23_QTg3$player2, player1vector)

addPlayer1 <- GEEL23_QTg3[ which(GEEL23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL23_QTg3[ which(GEEL23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL23_QTg2 <- rbind(GEEL23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
GEEL23_QTft <- ftable(GEEL23_QTg2$player1, GEEL23_QTg2$player2)
GEEL23_QTft2 <- as.matrix(GEEL23_QTft)
numRows <- nrow(GEEL23_QTft2)
numCols <- ncol(GEEL23_QTft2)
GEEL23_QTft3 <- GEEL23_QTft2[c(2:numRows) , c(2:numCols)]
GEEL23_QTTable <- graph.adjacency(GEEL23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(GEEL23_QTTable, vertex.label = V(GEEL23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
GEEL23_QT.clusterCoef <- transitivity(GEEL23_QTTable, type="global") #cluster coefficient
GEEL23_QT.degreeCent <- centralization.degree(GEEL23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL23_QTftn <- as.network.matrix(GEEL23_QTft)
GEEL23_QT.netDensity <- network.density(GEEL23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL23_QT.entropy <- entropy(GEEL23_QTft) #entropy

GEEL23_QT.netMx <- cbind(GEEL23_QT.netMx, GEEL23_QT.clusterCoef, GEEL23_QT.degreeCent$centralization,
                         GEEL23_QT.netDensity, GEEL23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL23_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "GWS"
KIoutcome = "Goal_F"
GWS23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
GWS23_Gg2 <- data.frame(GWS23_G)
GWS23_Gg2 <- GWS23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_Gg2$player1
player2vector <- GWS23_Gg2$player2
GWS23_Gg3 <- GWS23_Gg2
GWS23_Gg3$p1inp2vec <- is.element(GWS23_Gg3$player1, player2vector)
GWS23_Gg3$p2inp1vec <- is.element(GWS23_Gg3$player2, player1vector)

addPlayer1 <- GWS23_Gg3[ which(GWS23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_Gg3[ which(GWS23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_Gg2 <- rbind(GWS23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
GWS23_Gft <- ftable(GWS23_Gg2$player1, GWS23_Gg2$player2)
GWS23_Gft2 <- as.matrix(GWS23_Gft)
numRows <- nrow(GWS23_Gft2)
numCols <- ncol(GWS23_Gft2)
GWS23_Gft3 <- GWS23_Gft2[c(1:numRows) , c(1:numCols)]
GWS23_GTable <- graph.adjacency(GWS23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(GWS23_GTable, vertex.label = V(GWS23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
GWS23_G.clusterCoef <- transitivity(GWS23_GTable, type="global") #cluster coefficient
GWS23_G.degreeCent <- centralization.degree(GWS23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_Gftn <- as.network.matrix(GWS23_Gft)
GWS23_G.netDensity <- network.density(GWS23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_G.entropy <- entropy(GWS23_Gft) #entropy

GWS23_G.netMx <- cbind(GWS23_G.netMx, GWS23_G.clusterCoef, GWS23_G.degreeCent$centralization,
                       GWS23_G.netDensity, GWS23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "GWS"
KIoutcome = "Behind_F"
GWS23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
GWS23_Bg2 <- data.frame(GWS23_B)
GWS23_Bg2 <- GWS23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_Bg2$player1
player2vector <- GWS23_Bg2$player2
GWS23_Bg3 <- GWS23_Bg2
GWS23_Bg3$p1inp2vec <- is.element(GWS23_Bg3$player1, player2vector)
GWS23_Bg3$p2inp1vec <- is.element(GWS23_Bg3$player2, player1vector)

addPlayer1 <- GWS23_Bg3[ which(GWS23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_Bg3[ which(GWS23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_Bg2 <- rbind(GWS23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
GWS23_Bft <- ftable(GWS23_Bg2$player1, GWS23_Bg2$player2)
GWS23_Bft2 <- as.matrix(GWS23_Bft)
numRows <- nrow(GWS23_Bft2)
numCols <- ncol(GWS23_Bft2)
GWS23_Bft3 <- GWS23_Bft2[c(2:numRows) , c(2:numCols)]
GWS23_BTable <- graph.adjacency(GWS23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(GWS23_BTable, vertex.label = V(GWS23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
GWS23_B.clusterCoef <- transitivity(GWS23_BTable, type="global") #cluster coefficient
GWS23_B.degreeCent <- centralization.degree(GWS23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_Bftn <- as.network.matrix(GWS23_Bft)
GWS23_B.netDensity <- network.density(GWS23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_B.entropy <- entropy(GWS23_Bft) #entropy

GWS23_B.netMx <- cbind(GWS23_B.netMx, GWS23_B.clusterCoef, GWS23_B.degreeCent$centralization,
                       GWS23_B.netDensity, GWS23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************

round = 23
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
GWS23_SFg2 <- data.frame(GWS23_SF)
GWS23_SFg2 <- GWS23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_SFg2$player1
player2vector <- GWS23_SFg2$player2
GWS23_SFg3 <- GWS23_SFg2
GWS23_SFg3$p1inp2vec <- is.element(GWS23_SFg3$player1, player2vector)
GWS23_SFg3$p2inp1vec <- is.element(GWS23_SFg3$player2, player1vector)

addPlayer1 <- GWS23_SFg3[ which(GWS23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_SFg3[ which(GWS23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_SFg2 <- rbind(GWS23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
GWS23_SFft <- ftable(GWS23_SFg2$player1, GWS23_SFg2$player2)
GWS23_SFft2 <- as.matrix(GWS23_SFft)
numRows <- nrow(GWS23_SFft2)
numCols <- ncol(GWS23_SFft2)
GWS23_SFft3 <- GWS23_SFft2[c(2:numRows) , c(2:numCols)]
GWS23_SFTable <- graph.adjacency(GWS23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(GWS23_SFTable, vertex.label = V(GWS23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
GWS23_SF.clusterCoef <- transitivity(GWS23_SFTable, type="global") #cluster coefficient
GWS23_SF.degreeCent <- centralization.degree(GWS23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_SFftn <- as.network.matrix(GWS23_SFft)
GWS23_SF.netDensity <- network.density(GWS23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_SF.entropy <- entropy(GWS23_SFft) #entropy

GWS23_SF.netMx <- cbind(GWS23_SF.netMx, GWS23_SF.clusterCoef, GWS23_SF.degreeCent$centralization,
                        GWS23_SF.netDensity, GWS23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
GWS23_TFg2 <- data.frame(GWS23_TF)
GWS23_TFg2 <- GWS23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_TFg2$player1
player2vector <- GWS23_TFg2$player2
GWS23_TFg3 <- GWS23_TFg2
GWS23_TFg3$p1inp2vec <- is.element(GWS23_TFg3$player1, player2vector)
GWS23_TFg3$p2inp1vec <- is.element(GWS23_TFg3$player2, player1vector)

addPlayer1 <- GWS23_TFg3[ which(GWS23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_TFg3[ which(GWS23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_TFg2 <- rbind(GWS23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
GWS23_TFft <- ftable(GWS23_TFg2$player1, GWS23_TFg2$player2)
GWS23_TFft2 <- as.matrix(GWS23_TFft)
numRows <- nrow(GWS23_TFft2)
numCols <- ncol(GWS23_TFft2)
GWS23_TFft3 <- GWS23_TFft2[c(2:numRows) , c(2:numCols)]
GWS23_TFTable <- graph.adjacency(GWS23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(GWS23_TFTable, vertex.label = V(GWS23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
GWS23_TF.clusterCoef <- transitivity(GWS23_TFTable, type="global") #cluster coefficient
GWS23_TF.degreeCent <- centralization.degree(GWS23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_TFftn <- as.network.matrix(GWS23_TFft)
GWS23_TF.netDensity <- network.density(GWS23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_TF.entropy <- entropy(GWS23_TFft) #entropy

GWS23_TF.netMx <- cbind(GWS23_TF.netMx, GWS23_TF.clusterCoef, GWS23_TF.degreeCent$centralization,
                        GWS23_TF.netDensity, GWS23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
GWS23_SAMg2 <- data.frame(GWS23_SAM)
GWS23_SAMg2 <- GWS23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_SAMg2$player1
player2vector <- GWS23_SAMg2$player2
GWS23_SAMg3 <- GWS23_SAMg2
GWS23_SAMg3$p1inp2vec <- is.element(GWS23_SAMg3$player1, player2vector)
GWS23_SAMg3$p2inp1vec <- is.element(GWS23_SAMg3$player2, player1vector)

addPlayer1 <- GWS23_SAMg3[ which(GWS23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_SAMg3[ which(GWS23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_SAMg2 <- rbind(GWS23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
GWS23_SAMft <- ftable(GWS23_SAMg2$player1, GWS23_SAMg2$player2)
GWS23_SAMft2 <- as.matrix(GWS23_SAMft)
numRows <- nrow(GWS23_SAMft2)
numCols <- ncol(GWS23_SAMft2)
GWS23_SAMft3 <- GWS23_SAMft2[c(2:numRows) , c(2:numCols)]
GWS23_SAMTable <- graph.adjacency(GWS23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(GWS23_SAMTable, vertex.label = V(GWS23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
GWS23_SAM.clusterCoef <- transitivity(GWS23_SAMTable, type="global") #cluster coefficient
GWS23_SAM.degreeCent <- centralization.degree(GWS23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_SAMftn <- as.network.matrix(GWS23_SAMft)
GWS23_SAM.netDensity <- network.density(GWS23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_SAM.entropy <- entropy(GWS23_SAMft) #entropy

GWS23_SAM.netMx <- cbind(GWS23_SAM.netMx, GWS23_SAM.clusterCoef, GWS23_SAM.degreeCent$centralization,
                         GWS23_SAM.netDensity, GWS23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
GWS23_TAMg2 <- data.frame(GWS23_TAM)
GWS23_TAMg2 <- GWS23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_TAMg2$player1
player2vector <- GWS23_TAMg2$player2
GWS23_TAMg3 <- GWS23_TAMg2
GWS23_TAMg3$p1inp2vec <- is.element(GWS23_TAMg3$player1, player2vector)
GWS23_TAMg3$p2inp1vec <- is.element(GWS23_TAMg3$player2, player1vector)

addPlayer1 <- GWS23_TAMg3[ which(GWS23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_TAMg3[ which(GWS23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_TAMg2 <- rbind(GWS23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
GWS23_TAMft <- ftable(GWS23_TAMg2$player1, GWS23_TAMg2$player2)
GWS23_TAMft2 <- as.matrix(GWS23_TAMft)
numRows <- nrow(GWS23_TAMft2)
numCols <- ncol(GWS23_TAMft2)
GWS23_TAMft3 <- GWS23_TAMft2[c(2:numRows) , c(2:numCols)]
GWS23_TAMTable <- graph.adjacency(GWS23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(GWS23_TAMTable, vertex.label = V(GWS23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
GWS23_TAM.clusterCoef <- transitivity(GWS23_TAMTable, type="global") #cluster coefficient
GWS23_TAM.degreeCent <- centralization.degree(GWS23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_TAMftn <- as.network.matrix(GWS23_TAMft)
GWS23_TAM.netDensity <- network.density(GWS23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_TAM.entropy <- entropy(GWS23_TAMft) #entropy

GWS23_TAM.netMx <- cbind(GWS23_TAM.netMx, GWS23_TAM.clusterCoef, GWS23_TAM.degreeCent$centralization,
                         GWS23_TAM.netDensity, GWS23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
GWS23_SDMg2 <- data.frame(GWS23_SDM)
GWS23_SDMg2 <- GWS23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_SDMg2$player1
player2vector <- GWS23_SDMg2$player2
GWS23_SDMg3 <- GWS23_SDMg2
GWS23_SDMg3$p1inp2vec <- is.element(GWS23_SDMg3$player1, player2vector)
GWS23_SDMg3$p2inp1vec <- is.element(GWS23_SDMg3$player2, player1vector)

addPlayer1 <- GWS23_SDMg3[ which(GWS23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_SDMg3[ which(GWS23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_SDMg2 <- rbind(GWS23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
GWS23_SDMft <- ftable(GWS23_SDMg2$player1, GWS23_SDMg2$player2)
GWS23_SDMft2 <- as.matrix(GWS23_SDMft)
numRows <- nrow(GWS23_SDMft2)
numCols <- ncol(GWS23_SDMft2)
GWS23_SDMft3 <- GWS23_SDMft2[c(2:numRows) , c(2:numCols)]
GWS23_SDMTable <- graph.adjacency(GWS23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(GWS23_SDMTable, vertex.label = V(GWS23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
GWS23_SDM.clusterCoef <- transitivity(GWS23_SDMTable, type="global") #cluster coefficient
GWS23_SDM.degreeCent <- centralization.degree(GWS23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_SDMftn <- as.network.matrix(GWS23_SDMft)
GWS23_SDM.netDensity <- network.density(GWS23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_SDM.entropy <- entropy(GWS23_SDMft) #entropy

GWS23_SDM.netMx <- cbind(GWS23_SDM.netMx, GWS23_SDM.clusterCoef, GWS23_SDM.degreeCent$centralization,
                         GWS23_SDM.netDensity, GWS23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
GWS23_TDMg2 <- data.frame(GWS23_TDM)
GWS23_TDMg2 <- GWS23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_TDMg2$player1
player2vector <- GWS23_TDMg2$player2
GWS23_TDMg3 <- GWS23_TDMg2
GWS23_TDMg3$p1inp2vec <- is.element(GWS23_TDMg3$player1, player2vector)
GWS23_TDMg3$p2inp1vec <- is.element(GWS23_TDMg3$player2, player1vector)

addPlayer1 <- GWS23_TDMg3[ which(GWS23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS23_TDMg3[ which(GWS23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_TDMg2 <- rbind(GWS23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
GWS23_TDMft <- ftable(GWS23_TDMg2$player1, GWS23_TDMg2$player2)
GWS23_TDMft2 <- as.matrix(GWS23_TDMft)
numRows <- nrow(GWS23_TDMft2)
numCols <- ncol(GWS23_TDMft2)
GWS23_TDMft3 <- GWS23_TDMft2[c(2:numRows) , c(2:numCols)]
GWS23_TDMTable <- graph.adjacency(GWS23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(GWS23_TDMTable, vertex.label = V(GWS23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
GWS23_TDM.clusterCoef <- transitivity(GWS23_TDMTable, type="global") #cluster coefficient
GWS23_TDM.degreeCent <- centralization.degree(GWS23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_TDMftn <- as.network.matrix(GWS23_TDMft)
GWS23_TDM.netDensity <- network.density(GWS23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_TDM.entropy <- entropy(GWS23_TDMft) #entropy

GWS23_TDM.netMx <- cbind(GWS23_TDM.netMx, GWS23_TDM.clusterCoef, GWS23_TDM.degreeCent$centralization,
                         GWS23_TDM.netDensity, GWS23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
GWS23_SDg2 <- data.frame(GWS23_SD)
GWS23_SDg2 <- GWS23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_SDg2$player1
player2vector <- GWS23_SDg2$player2
GWS23_SDg3 <- GWS23_SDg2
GWS23_SDg3$p1inp2vec <- is.element(GWS23_SDg3$player1, player2vector)
GWS23_SDg3$p2inp1vec <- is.element(GWS23_SDg3$player2, player1vector)

addPlayer1 <- GWS23_SDg3[ which(GWS23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_SDg3[ which(GWS23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_SDg2 <- rbind(GWS23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
GWS23_SDft <- ftable(GWS23_SDg2$player1, GWS23_SDg2$player2)
GWS23_SDft2 <- as.matrix(GWS23_SDft)
numRows <- nrow(GWS23_SDft2)
numCols <- ncol(GWS23_SDft2)
GWS23_SDft3 <- GWS23_SDft2[c(2:numRows) , c(2:numCols)]
GWS23_SDTable <- graph.adjacency(GWS23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(GWS23_SDTable, vertex.label = V(GWS23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
GWS23_SD.clusterCoef <- transitivity(GWS23_SDTable, type="global") #cluster coefficient
GWS23_SD.degreeCent <- centralization.degree(GWS23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_SDftn <- as.network.matrix(GWS23_SDft)
GWS23_SD.netDensity <- network.density(GWS23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_SD.entropy <- entropy(GWS23_SDft) #entropy

GWS23_SD.netMx <- cbind(GWS23_SD.netMx, GWS23_SD.clusterCoef, GWS23_SD.degreeCent$centralization,
                        GWS23_SD.netDensity, GWS23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************

round = 23
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
GWS23_TDg2 <- data.frame(GWS23_TD)
GWS23_TDg2 <- GWS23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_TDg2$player1
player2vector <- GWS23_TDg2$player2
GWS23_TDg3 <- GWS23_TDg2
GWS23_TDg3$p1inp2vec <- is.element(GWS23_TDg3$player1, player2vector)
GWS23_TDg3$p2inp1vec <- is.element(GWS23_TDg3$player2, player1vector)

addPlayer1 <- GWS23_TDg3[ which(GWS23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_TDg3[ which(GWS23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_TDg2 <- rbind(GWS23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
GWS23_TDft <- ftable(GWS23_TDg2$player1, GWS23_TDg2$player2)
GWS23_TDft2 <- as.matrix(GWS23_TDft)
numRows <- nrow(GWS23_TDft2)
numCols <- ncol(GWS23_TDft2)
GWS23_TDft3 <- GWS23_TDft2[c(2:numRows) , c(2:numCols)]
GWS23_TDTable <- graph.adjacency(GWS23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(GWS23_TDTable, vertex.label = V(GWS23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
GWS23_TD.clusterCoef <- transitivity(GWS23_TDTable, type="global") #cluster coefficient
GWS23_TD.degreeCent <- centralization.degree(GWS23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_TDftn <- as.network.matrix(GWS23_TDft)
GWS23_TD.netDensity <- network.density(GWS23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_TD.entropy <- entropy(GWS23_TDft) #entropy

GWS23_TD.netMx <- cbind(GWS23_TD.netMx, GWS23_TD.clusterCoef, GWS23_TD.degreeCent$centralization,
                        GWS23_TD.netDensity, GWS23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
GWS23_QTg2 <- data.frame(GWS23_QT)
GWS23_QTg2 <- GWS23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS23_QTg2$player1
player2vector <- GWS23_QTg2$player2
GWS23_QTg3 <- GWS23_QTg2
GWS23_QTg3$p1inp2vec <- is.element(GWS23_QTg3$player1, player2vector)
GWS23_QTg3$p2inp1vec <- is.element(GWS23_QTg3$player2, player1vector)

addPlayer1 <- GWS23_QTg3[ which(GWS23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS23_QTg3[ which(GWS23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS23_QTg2 <- rbind(GWS23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
GWS23_QTft <- ftable(GWS23_QTg2$player1, GWS23_QTg2$player2)
GWS23_QTft2 <- as.matrix(GWS23_QTft)
numRows <- nrow(GWS23_QTft2)
numCols <- ncol(GWS23_QTft2)
GWS23_QTft3 <- GWS23_QTft2[c(2:numRows) , c(2:numCols)]
GWS23_QTTable <- graph.adjacency(GWS23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(GWS23_QTTable, vertex.label = V(GWS23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
GWS23_QT.clusterCoef <- transitivity(GWS23_QTTable, type="global") #cluster coefficient
GWS23_QT.degreeCent <- centralization.degree(GWS23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS23_QTftn <- as.network.matrix(GWS23_QTft)
GWS23_QT.netDensity <- network.density(GWS23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS23_QT.entropy <- entropy(GWS23_QTft) #entropy

GWS23_QT.netMx <- cbind(GWS23_QT.netMx, GWS23_QT.clusterCoef, GWS23_QT.degreeCent$centralization,
                        GWS23_QT.netDensity, GWS23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS23_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "HAW"
KIoutcome = "Goal_F"
HAW23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
HAW23_Gg2 <- data.frame(HAW23_G)
HAW23_Gg2 <- HAW23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_Gg2$player1
player2vector <- HAW23_Gg2$player2
HAW23_Gg3 <- HAW23_Gg2
HAW23_Gg3$p1inp2vec <- is.element(HAW23_Gg3$player1, player2vector)
HAW23_Gg3$p2inp1vec <- is.element(HAW23_Gg3$player2, player1vector)

addPlayer1 <- HAW23_Gg3[ which(HAW23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_Gg3[ which(HAW23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_Gg2 <- rbind(HAW23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
HAW23_Gft <- ftable(HAW23_Gg2$player1, HAW23_Gg2$player2)
HAW23_Gft2 <- as.matrix(HAW23_Gft)
numRows <- nrow(HAW23_Gft2)
numCols <- ncol(HAW23_Gft2)
HAW23_Gft3 <- HAW23_Gft2[c(2:numRows) , c(2:numCols)]
HAW23_GTable <- graph.adjacency(HAW23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(HAW23_GTable, vertex.label = V(HAW23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
HAW23_G.clusterCoef <- transitivity(HAW23_GTable, type="global") #cluster coefficient
HAW23_G.degreeCent <- centralization.degree(HAW23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_Gftn <- as.network.matrix(HAW23_Gft)
HAW23_G.netDensity <- network.density(HAW23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_G.entropy <- entropy(HAW23_Gft) #entropy

HAW23_G.netMx <- cbind(HAW23_G.netMx, HAW23_G.clusterCoef, HAW23_G.degreeCent$centralization,
                       HAW23_G.netDensity, HAW23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "HAW"
KIoutcome = "Behind_F"
HAW23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
HAW23_Bg2 <- data.frame(HAW23_B)
HAW23_Bg2 <- HAW23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_Bg2$player1
player2vector <- HAW23_Bg2$player2
HAW23_Bg3 <- HAW23_Bg2
HAW23_Bg3$p1inp2vec <- is.element(HAW23_Bg3$player1, player2vector)
HAW23_Bg3$p2inp1vec <- is.element(HAW23_Bg3$player2, player1vector)

addPlayer1 <- HAW23_Bg3[ which(HAW23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW23_Bg2 <- rbind(HAW23_Bg2, addPlayer1)

#ROUND 23, Behind graph using weighted edges
HAW23_Bft <- ftable(HAW23_Bg2$player1, HAW23_Bg2$player2)
HAW23_Bft2 <- as.matrix(HAW23_Bft)
numRows <- nrow(HAW23_Bft2)
numCols <- ncol(HAW23_Bft2)
HAW23_Bft3 <- HAW23_Bft2[c(2:numRows) , c(1:numCols)]
HAW23_BTable <- graph.adjacency(HAW23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(HAW23_BTable, vertex.label = V(HAW23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
HAW23_B.clusterCoef <- transitivity(HAW23_BTable, type="global") #cluster coefficient
HAW23_B.degreeCent <- centralization.degree(HAW23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_Bftn <- as.network.matrix(HAW23_Bft)
HAW23_B.netDensity <- network.density(HAW23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_B.entropy <- entropy(HAW23_Bft) #entropy

HAW23_B.netMx <- cbind(HAW23_B.netMx, HAW23_B.clusterCoef, HAW23_B.degreeCent$centralization,
                       HAW23_B.netDensity, HAW23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************

round = 23
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
HAW23_SFg2 <- data.frame(HAW23_SF)
HAW23_SFg2 <- HAW23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_SFg2$player1
player2vector <- HAW23_SFg2$player2
HAW23_SFg3 <- HAW23_SFg2
HAW23_SFg3$p1inp2vec <- is.element(HAW23_SFg3$player1, player2vector)
HAW23_SFg3$p2inp1vec <- is.element(HAW23_SFg3$player2, player1vector)

addPlayer1 <- HAW23_SFg3[ which(HAW23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW23_SFg3[ which(HAW23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_SFg2 <- rbind(HAW23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
HAW23_SFft <- ftable(HAW23_SFg2$player1, HAW23_SFg2$player2)
HAW23_SFft2 <- as.matrix(HAW23_SFft)
numRows <- nrow(HAW23_SFft2)
numCols <- ncol(HAW23_SFft2)
HAW23_SFft3 <- HAW23_SFft2[c(2:numRows) , c(2:numCols)]
HAW23_SFTable <- graph.adjacency(HAW23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(HAW23_SFTable, vertex.label = V(HAW23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
HAW23_SF.clusterCoef <- transitivity(HAW23_SFTable, type="global") #cluster coefficient
HAW23_SF.degreeCent <- centralization.degree(HAW23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_SFftn <- as.network.matrix(HAW23_SFft)
HAW23_SF.netDensity <- network.density(HAW23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_SF.entropy <- entropy(HAW23_SFft) #entropy

HAW23_SF.netMx <- cbind(HAW23_SF.netMx, HAW23_SF.clusterCoef, HAW23_SF.degreeCent$centralization,
                        HAW23_SF.netDensity, HAW23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
HAW23_TFg2 <- data.frame(HAW23_TF)
HAW23_TFg2 <- HAW23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_TFg2$player1
player2vector <- HAW23_TFg2$player2
HAW23_TFg3 <- HAW23_TFg2
HAW23_TFg3$p1inp2vec <- is.element(HAW23_TFg3$player1, player2vector)
HAW23_TFg3$p2inp1vec <- is.element(HAW23_TFg3$player2, player1vector)

addPlayer1 <- HAW23_TFg3[ which(HAW23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW23_TFg3[ which(HAW23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_TFg2 <- rbind(HAW23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
HAW23_TFft <- ftable(HAW23_TFg2$player1, HAW23_TFg2$player2)
HAW23_TFft2 <- as.matrix(HAW23_TFft)
numRows <- nrow(HAW23_TFft2)
numCols <- ncol(HAW23_TFft2)
HAW23_TFft3 <- HAW23_TFft2[c(2:numRows) , c(2:numCols)]
HAW23_TFTable <- graph.adjacency(HAW23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(HAW23_TFTable, vertex.label = V(HAW23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
HAW23_TF.clusterCoef <- transitivity(HAW23_TFTable, type="global") #cluster coefficient
HAW23_TF.degreeCent <- centralization.degree(HAW23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_TFftn <- as.network.matrix(HAW23_TFft)
HAW23_TF.netDensity <- network.density(HAW23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_TF.entropy <- entropy(HAW23_TFft) #entropy

HAW23_TF.netMx <- cbind(HAW23_TF.netMx, HAW23_TF.clusterCoef, HAW23_TF.degreeCent$centralization,
                        HAW23_TF.netDensity, HAW23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
HAW23_SAMg2 <- data.frame(HAW23_SAM)
HAW23_SAMg2 <- HAW23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_SAMg2$player1
player2vector <- HAW23_SAMg2$player2
HAW23_SAMg3 <- HAW23_SAMg2
HAW23_SAMg3$p1inp2vec <- is.element(HAW23_SAMg3$player1, player2vector)
HAW23_SAMg3$p2inp1vec <- is.element(HAW23_SAMg3$player2, player1vector)

addPlayer1 <- HAW23_SAMg3[ which(HAW23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_SAMg3[ which(HAW23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_SAMg2 <- rbind(HAW23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
HAW23_SAMft <- ftable(HAW23_SAMg2$player1, HAW23_SAMg2$player2)
HAW23_SAMft2 <- as.matrix(HAW23_SAMft)
numRows <- nrow(HAW23_SAMft2)
numCols <- ncol(HAW23_SAMft2)
HAW23_SAMft3 <- HAW23_SAMft2[c(2:numRows) , c(2:numCols)]
HAW23_SAMTable <- graph.adjacency(HAW23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(HAW23_SAMTable, vertex.label = V(HAW23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
HAW23_SAM.clusterCoef <- transitivity(HAW23_SAMTable, type="global") #cluster coefficient
HAW23_SAM.degreeCent <- centralization.degree(HAW23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_SAMftn <- as.network.matrix(HAW23_SAMft)
HAW23_SAM.netDensity <- network.density(HAW23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_SAM.entropy <- entropy(HAW23_SAMft) #entropy

HAW23_SAM.netMx <- cbind(HAW23_SAM.netMx, HAW23_SAM.clusterCoef, HAW23_SAM.degreeCent$centralization,
                         HAW23_SAM.netDensity, HAW23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************
#NA

round = 23
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
HAW23_TAMg2 <- data.frame(HAW23_TAM)
HAW23_TAMg2 <- HAW23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_TAMg2$player1
player2vector <- HAW23_TAMg2$player2
HAW23_TAMg3 <- HAW23_TAMg2
HAW23_TAMg3$p1inp2vec <- is.element(HAW23_TAMg3$player1, player2vector)
HAW23_TAMg3$p2inp1vec <- is.element(HAW23_TAMg3$player2, player1vector)

addPlayer1 <- HAW23_TAMg3[ which(HAW23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_TAMg3[ which(HAW23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_TAMg2 <- rbind(HAW23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
HAW23_TAMft <- ftable(HAW23_TAMg2$player1, HAW23_TAMg2$player2)
HAW23_TAMft2 <- as.matrix(HAW23_TAMft)
numRows <- nrow(HAW23_TAMft2)
numCols <- ncol(HAW23_TAMft2)
HAW23_TAMft3 <- HAW23_TAMft2[c(2:numRows) , c(2:numCols)]
HAW23_TAMTable <- graph.adjacency(HAW23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(HAW23_TAMTable, vertex.label = V(HAW23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
HAW23_TAM.clusterCoef <- transitivity(HAW23_TAMTable, type="global") #cluster coefficient
HAW23_TAM.degreeCent <- centralization.degree(HAW23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_TAMftn <- as.network.matrix(HAW23_TAMft)
HAW23_TAM.netDensity <- network.density(HAW23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_TAM.entropy <- entropy(HAW23_TAMft) #entropy

HAW23_TAM.netMx <- cbind(HAW23_TAM.netMx, HAW23_TAM.clusterCoef, HAW23_TAM.degreeCent$centralization,
                         HAW23_TAM.netDensity, HAW23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
HAW23_SDMg2 <- data.frame(HAW23_SDM)
HAW23_SDMg2 <- HAW23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_SDMg2$player1
player2vector <- HAW23_SDMg2$player2
HAW23_SDMg3 <- HAW23_SDMg2
HAW23_SDMg3$p1inp2vec <- is.element(HAW23_SDMg3$player1, player2vector)
HAW23_SDMg3$p2inp1vec <- is.element(HAW23_SDMg3$player2, player1vector)

addPlayer1 <- HAW23_SDMg3[ which(HAW23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW23_SDMg3[ which(HAW23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_SDMg2 <- rbind(HAW23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
HAW23_SDMft <- ftable(HAW23_SDMg2$player1, HAW23_SDMg2$player2)
HAW23_SDMft2 <- as.matrix(HAW23_SDMft)
numRows <- nrow(HAW23_SDMft2)
numCols <- ncol(HAW23_SDMft2)
HAW23_SDMft3 <- HAW23_SDMft2[c(2:numRows) , c(2:numCols)]
HAW23_SDMTable <- graph.adjacency(HAW23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(HAW23_SDMTable, vertex.label = V(HAW23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
HAW23_SDM.clusterCoef <- transitivity(HAW23_SDMTable, type="global") #cluster coefficient
HAW23_SDM.degreeCent <- centralization.degree(HAW23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_SDMftn <- as.network.matrix(HAW23_SDMft)
HAW23_SDM.netDensity <- network.density(HAW23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_SDM.entropy <- entropy(HAW23_SDMft) #entropy

HAW23_SDM.netMx <- cbind(HAW23_SDM.netMx, HAW23_SDM.clusterCoef, HAW23_SDM.degreeCent$centralization,
                         HAW23_SDM.netDensity, HAW23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
HAW23_TDMg2 <- data.frame(HAW23_TDM)
HAW23_TDMg2 <- HAW23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_TDMg2$player1
player2vector <- HAW23_TDMg2$player2
HAW23_TDMg3 <- HAW23_TDMg2
HAW23_TDMg3$p1inp2vec <- is.element(HAW23_TDMg3$player1, player2vector)
HAW23_TDMg3$p2inp1vec <- is.element(HAW23_TDMg3$player2, player1vector)

addPlayer1 <- HAW23_TDMg3[ which(HAW23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_TDMg3[ which(HAW23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_TDMg2 <- rbind(HAW23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
HAW23_TDMft <- ftable(HAW23_TDMg2$player1, HAW23_TDMg2$player2)
HAW23_TDMft2 <- as.matrix(HAW23_TDMft)
numRows <- nrow(HAW23_TDMft2)
numCols <- ncol(HAW23_TDMft2)
HAW23_TDMft3 <- HAW23_TDMft2[c(2:numRows) , c(2:numCols)]
HAW23_TDMTable <- graph.adjacency(HAW23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(HAW23_TDMTable, vertex.label = V(HAW23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
HAW23_TDM.clusterCoef <- transitivity(HAW23_TDMTable, type="global") #cluster coefficient
HAW23_TDM.degreeCent <- centralization.degree(HAW23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_TDMftn <- as.network.matrix(HAW23_TDMft)
HAW23_TDM.netDensity <- network.density(HAW23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_TDM.entropy <- entropy(HAW23_TDMft) #entropy

HAW23_TDM.netMx <- cbind(HAW23_TDM.netMx, HAW23_TDM.clusterCoef, HAW23_TDM.degreeCent$centralization,
                         HAW23_TDM.netDensity, HAW23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
HAW23_SDg2 <- data.frame(HAW23_SD)
HAW23_SDg2 <- HAW23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_SDg2$player1
player2vector <- HAW23_SDg2$player2
HAW23_SDg3 <- HAW23_SDg2
HAW23_SDg3$p1inp2vec <- is.element(HAW23_SDg3$player1, player2vector)
HAW23_SDg3$p2inp1vec <- is.element(HAW23_SDg3$player2, player1vector)

addPlayer1 <- HAW23_SDg3[ which(HAW23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_SDg3[ which(HAW23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_SDg2 <- rbind(HAW23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
HAW23_SDft <- ftable(HAW23_SDg2$player1, HAW23_SDg2$player2)
HAW23_SDft2 <- as.matrix(HAW23_SDft)
numRows <- nrow(HAW23_SDft2)
numCols <- ncol(HAW23_SDft2)
HAW23_SDft3 <- HAW23_SDft2[c(2:numRows) , c(2:numCols)]
HAW23_SDTable <- graph.adjacency(HAW23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(HAW23_SDTable, vertex.label = V(HAW23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
HAW23_SD.clusterCoef <- transitivity(HAW23_SDTable, type="global") #cluster coefficient
HAW23_SD.degreeCent <- centralization.degree(HAW23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_SDftn <- as.network.matrix(HAW23_SDft)
HAW23_SD.netDensity <- network.density(HAW23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_SD.entropy <- entropy(HAW23_SDft) #entropy

HAW23_SD.netMx <- cbind(HAW23_SD.netMx, HAW23_SD.clusterCoef, HAW23_SD.degreeCent$centralization,
                        HAW23_SD.netDensity, HAW23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
HAW23_TDg2 <- data.frame(HAW23_TD)
HAW23_TDg2 <- HAW23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_TDg2$player1
player2vector <- HAW23_TDg2$player2
HAW23_TDg3 <- HAW23_TDg2
HAW23_TDg3$p1inp2vec <- is.element(HAW23_TDg3$player1, player2vector)
HAW23_TDg3$p2inp1vec <- is.element(HAW23_TDg3$player2, player1vector)

addPlayer1 <- HAW23_TDg3[ which(HAW23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_TDg3[ which(HAW23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_TDg2 <- rbind(HAW23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
HAW23_TDft <- ftable(HAW23_TDg2$player1, HAW23_TDg2$player2)
HAW23_TDft2 <- as.matrix(HAW23_TDft)
numRows <- nrow(HAW23_TDft2)
numCols <- ncol(HAW23_TDft2)
HAW23_TDft3 <- HAW23_TDft2[c(2:numRows) , c(2:numCols)]
HAW23_TDTable <- graph.adjacency(HAW23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(HAW23_TDTable, vertex.label = V(HAW23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
HAW23_TD.clusterCoef <- transitivity(HAW23_TDTable, type="global") #cluster coefficient
HAW23_TD.degreeCent <- centralization.degree(HAW23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_TDftn <- as.network.matrix(HAW23_TDft)
HAW23_TD.netDensity <- network.density(HAW23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_TD.entropy <- entropy(HAW23_TDft) #entropy

HAW23_TD.netMx <- cbind(HAW23_TD.netMx, HAW23_TD.clusterCoef, HAW23_TD.degreeCent$centralization,
                        HAW23_TD.netDensity, HAW23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************

round = 23
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
HAW23_QTg2 <- data.frame(HAW23_QT)
HAW23_QTg2 <- HAW23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW23_QTg2$player1
player2vector <- HAW23_QTg2$player2
HAW23_QTg3 <- HAW23_QTg2
HAW23_QTg3$p1inp2vec <- is.element(HAW23_QTg3$player1, player2vector)
HAW23_QTg3$p2inp1vec <- is.element(HAW23_QTg3$player2, player1vector)

addPlayer1 <- HAW23_QTg3[ which(HAW23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW23_QTg3[ which(HAW23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW23_QTg2 <- rbind(HAW23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
HAW23_QTft <- ftable(HAW23_QTg2$player1, HAW23_QTg2$player2)
HAW23_QTft2 <- as.matrix(HAW23_QTft)
numRows <- nrow(HAW23_QTft2)
numCols <- ncol(HAW23_QTft2)
HAW23_QTft3 <- HAW23_QTft2[c(2:numRows) , c(2:numCols)]
HAW23_QTTable <- graph.adjacency(HAW23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(HAW23_QTTable, vertex.label = V(HAW23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
HAW23_QT.clusterCoef <- transitivity(HAW23_QTTable, type="global") #cluster coefficient
HAW23_QT.degreeCent <- centralization.degree(HAW23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW23_QTftn <- as.network.matrix(HAW23_QTft)
HAW23_QT.netDensity <- network.density(HAW23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW23_QT.entropy <- entropy(HAW23_QTft) #entropy

HAW23_QT.netMx <- cbind(HAW23_QT.netMx, HAW23_QT.clusterCoef, HAW23_QT.degreeCent$centralization,
                        HAW23_QT.netDensity, HAW23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW23_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "MELB"
KIoutcome = "Goal_F"
MELB23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
MELB23_Gg2 <- data.frame(MELB23_G)
MELB23_Gg2 <- MELB23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_Gg2$player1
player2vector <- MELB23_Gg2$player2
MELB23_Gg3 <- MELB23_Gg2
MELB23_Gg3$p1inp2vec <- is.element(MELB23_Gg3$player1, player2vector)
MELB23_Gg3$p2inp1vec <- is.element(MELB23_Gg3$player2, player1vector)

addPlayer1 <- MELB23_Gg3[ which(MELB23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_Gg3[ which(MELB23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_Gg2 <- rbind(MELB23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
MELB23_Gft <- ftable(MELB23_Gg2$player1, MELB23_Gg2$player2)
MELB23_Gft2 <- as.matrix(MELB23_Gft)
numRows <- nrow(MELB23_Gft2)
numCols <- ncol(MELB23_Gft2)
MELB23_Gft3 <- MELB23_Gft2[c(2:numRows) , c(2:numCols)]
MELB23_GTable <- graph.adjacency(MELB23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(MELB23_GTable, vertex.label = V(MELB23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
MELB23_G.clusterCoef <- transitivity(MELB23_GTable, type="global") #cluster coefficient
MELB23_G.degreeCent <- centralization.degree(MELB23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_Gftn <- as.network.matrix(MELB23_Gft)
MELB23_G.netDensity <- network.density(MELB23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_G.entropy <- entropy(MELB23_Gft) #entropy

MELB23_G.netMx <- cbind(MELB23_G.netMx, MELB23_G.clusterCoef, MELB23_G.degreeCent$centralization,
                        MELB23_G.netDensity, MELB23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "Behind_F"
MELB23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
MELB23_Bg2 <- data.frame(MELB23_B)
MELB23_Bg2 <- MELB23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_Bg2$player1
player2vector <- MELB23_Bg2$player2
MELB23_Bg3 <- MELB23_Bg2
MELB23_Bg3$p1inp2vec <- is.element(MELB23_Bg3$player1, player2vector)
MELB23_Bg3$p2inp1vec <- is.element(MELB23_Bg3$player2, player1vector)

addPlayer1 <- MELB23_Bg3[ which(MELB23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_Bg3[ which(MELB23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_Bg2 <- rbind(MELB23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
MELB23_Bft <- ftable(MELB23_Bg2$player1, MELB23_Bg2$player2)
MELB23_Bft2 <- as.matrix(MELB23_Bft)
numRows <- nrow(MELB23_Bft2)
numCols <- ncol(MELB23_Bft2)
MELB23_Bft3 <- MELB23_Bft2[c(2:numRows) , c(2:numCols)]
MELB23_BTable <- graph.adjacency(MELB23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(MELB23_BTable, vertex.label = V(MELB23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
MELB23_B.clusterCoef <- transitivity(MELB23_BTable, type="global") #cluster coefficient
MELB23_B.degreeCent <- centralization.degree(MELB23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_Bftn <- as.network.matrix(MELB23_Bft)
MELB23_B.netDensity <- network.density(MELB23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_B.entropy <- entropy(MELB23_Bft) #entropy

MELB23_B.netMx <- cbind(MELB23_B.netMx, MELB23_B.clusterCoef, MELB23_B.degreeCent$centralization,
                        MELB23_B.netDensity, MELB23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
MELB23_SFg2 <- data.frame(MELB23_SF)
MELB23_SFg2 <- MELB23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_SFg2$player1
player2vector <- MELB23_SFg2$player2
MELB23_SFg3 <- MELB23_SFg2
MELB23_SFg3$p1inp2vec <- is.element(MELB23_SFg3$player1, player2vector)
MELB23_SFg3$p2inp1vec <- is.element(MELB23_SFg3$player2, player1vector)

addPlayer1 <- MELB23_SFg3[ which(MELB23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_SFg3[ which(MELB23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_SFg2 <- rbind(MELB23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
MELB23_SFft <- ftable(MELB23_SFg2$player1, MELB23_SFg2$player2)
MELB23_SFft2 <- as.matrix(MELB23_SFft)
numRows <- nrow(MELB23_SFft2)
numCols <- ncol(MELB23_SFft2)
MELB23_SFft3 <- MELB23_SFft2[c(2:numRows) , c(2:numCols)]
MELB23_SFTable <- graph.adjacency(MELB23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(MELB23_SFTable, vertex.label = V(MELB23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
MELB23_SF.clusterCoef <- transitivity(MELB23_SFTable, type="global") #cluster coefficient
MELB23_SF.degreeCent <- centralization.degree(MELB23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_SFftn <- as.network.matrix(MELB23_SFft)
MELB23_SF.netDensity <- network.density(MELB23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_SF.entropy <- entropy(MELB23_SFft) #entropy

MELB23_SF.netMx <- cbind(MELB23_SF.netMx, MELB23_SF.clusterCoef, MELB23_SF.degreeCent$centralization,
                         MELB23_SF.netDensity, MELB23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
MELB23_TFg2 <- data.frame(MELB23_TF)
MELB23_TFg2 <- MELB23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_TFg2$player1
player2vector <- MELB23_TFg2$player2
MELB23_TFg3 <- MELB23_TFg2
MELB23_TFg3$p1inp2vec <- is.element(MELB23_TFg3$player1, player2vector)
MELB23_TFg3$p2inp1vec <- is.element(MELB23_TFg3$player2, player1vector)

addPlayer1 <- MELB23_TFg3[ which(MELB23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_TFg3[ which(MELB23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_TFg2 <- rbind(MELB23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
MELB23_TFft <- ftable(MELB23_TFg2$player1, MELB23_TFg2$player2)
MELB23_TFft2 <- as.matrix(MELB23_TFft)
numRows <- nrow(MELB23_TFft2)
numCols <- ncol(MELB23_TFft2)
MELB23_TFft3 <- MELB23_TFft2[c(2:numRows) , c(2:numCols)]
MELB23_TFTable <- graph.adjacency(MELB23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(MELB23_TFTable, vertex.label = V(MELB23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
MELB23_TF.clusterCoef <- transitivity(MELB23_TFTable, type="global") #cluster coefficient
MELB23_TF.degreeCent <- centralization.degree(MELB23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_TFftn <- as.network.matrix(MELB23_TFft)
MELB23_TF.netDensity <- network.density(MELB23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_TF.entropy <- entropy(MELB23_TFft) #entropy

MELB23_TF.netMx <- cbind(MELB23_TF.netMx, MELB23_TF.clusterCoef, MELB23_TF.degreeCent$centralization,
                         MELB23_TF.netDensity, MELB23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
MELB23_SAMg2 <- data.frame(MELB23_SAM)
MELB23_SAMg2 <- MELB23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_SAMg2$player1
player2vector <- MELB23_SAMg2$player2
MELB23_SAMg3 <- MELB23_SAMg2
MELB23_SAMg3$p1inp2vec <- is.element(MELB23_SAMg3$player1, player2vector)
MELB23_SAMg3$p2inp1vec <- is.element(MELB23_SAMg3$player2, player1vector)

addPlayer1 <- MELB23_SAMg3[ which(MELB23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_SAMg3[ which(MELB23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_SAMg2 <- rbind(MELB23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
MELB23_SAMft <- ftable(MELB23_SAMg2$player1, MELB23_SAMg2$player2)
MELB23_SAMft2 <- as.matrix(MELB23_SAMft)
numRows <- nrow(MELB23_SAMft2)
numCols <- ncol(MELB23_SAMft2)
MELB23_SAMft3 <- MELB23_SAMft2[c(2:numRows) , c(2:numCols)]
MELB23_SAMTable <- graph.adjacency(MELB23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(MELB23_SAMTable, vertex.label = V(MELB23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
MELB23_SAM.clusterCoef <- transitivity(MELB23_SAMTable, type="global") #cluster coefficient
MELB23_SAM.degreeCent <- centralization.degree(MELB23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_SAMftn <- as.network.matrix(MELB23_SAMft)
MELB23_SAM.netDensity <- network.density(MELB23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_SAM.entropy <- entropy(MELB23_SAMft) #entropy

MELB23_SAM.netMx <- cbind(MELB23_SAM.netMx, MELB23_SAM.clusterCoef, MELB23_SAM.degreeCent$centralization,
                          MELB23_SAM.netDensity, MELB23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
MELB23_TAMg2 <- data.frame(MELB23_TAM)
MELB23_TAMg2 <- MELB23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_TAMg2$player1
player2vector <- MELB23_TAMg2$player2
MELB23_TAMg3 <- MELB23_TAMg2
MELB23_TAMg3$p1inp2vec <- is.element(MELB23_TAMg3$player1, player2vector)
MELB23_TAMg3$p2inp1vec <- is.element(MELB23_TAMg3$player2, player1vector)

addPlayer1 <- MELB23_TAMg3[ which(MELB23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_TAMg3[ which(MELB23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_TAMg2 <- rbind(MELB23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
MELB23_TAMft <- ftable(MELB23_TAMg2$player1, MELB23_TAMg2$player2)
MELB23_TAMft2 <- as.matrix(MELB23_TAMft)
numRows <- nrow(MELB23_TAMft2)
numCols <- ncol(MELB23_TAMft2)
MELB23_TAMft3 <- MELB23_TAMft2[c(2:numRows) , c(2:numCols)]
MELB23_TAMTable <- graph.adjacency(MELB23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(MELB23_TAMTable, vertex.label = V(MELB23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
MELB23_TAM.clusterCoef <- transitivity(MELB23_TAMTable, type="global") #cluster coefficient
MELB23_TAM.degreeCent <- centralization.degree(MELB23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_TAMftn <- as.network.matrix(MELB23_TAMft)
MELB23_TAM.netDensity <- network.density(MELB23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_TAM.entropy <- entropy(MELB23_TAMft) #entropy

MELB23_TAM.netMx <- cbind(MELB23_TAM.netMx, MELB23_TAM.clusterCoef, MELB23_TAM.degreeCent$centralization,
                          MELB23_TAM.netDensity, MELB23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
MELB23_SDMg2 <- data.frame(MELB23_SDM)
MELB23_SDMg2 <- MELB23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_SDMg2$player1
player2vector <- MELB23_SDMg2$player2
MELB23_SDMg3 <- MELB23_SDMg2
MELB23_SDMg3$p1inp2vec <- is.element(MELB23_SDMg3$player1, player2vector)
MELB23_SDMg3$p2inp1vec <- is.element(MELB23_SDMg3$player2, player1vector)

addPlayer1 <- MELB23_SDMg3[ which(MELB23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_SDMg3[ which(MELB23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_SDMg2 <- rbind(MELB23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
MELB23_SDMft <- ftable(MELB23_SDMg2$player1, MELB23_SDMg2$player2)
MELB23_SDMft2 <- as.matrix(MELB23_SDMft)
numRows <- nrow(MELB23_SDMft2)
numCols <- ncol(MELB23_SDMft2)
MELB23_SDMft3 <- MELB23_SDMft2[c(2:numRows) , c(2:numCols)]
MELB23_SDMTable <- graph.adjacency(MELB23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(MELB23_SDMTable, vertex.label = V(MELB23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
MELB23_SDM.clusterCoef <- transitivity(MELB23_SDMTable, type="global") #cluster coefficient
MELB23_SDM.degreeCent <- centralization.degree(MELB23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_SDMftn <- as.network.matrix(MELB23_SDMft)
MELB23_SDM.netDensity <- network.density(MELB23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_SDM.entropy <- entropy(MELB23_SDMft) #entropy

MELB23_SDM.netMx <- cbind(MELB23_SDM.netMx, MELB23_SDM.clusterCoef, MELB23_SDM.degreeCent$centralization,
                          MELB23_SDM.netDensity, MELB23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
MELB23_TDMg2 <- data.frame(MELB23_TDM)
MELB23_TDMg2 <- MELB23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_TDMg2$player1
player2vector <- MELB23_TDMg2$player2
MELB23_TDMg3 <- MELB23_TDMg2
MELB23_TDMg3$p1inp2vec <- is.element(MELB23_TDMg3$player1, player2vector)
MELB23_TDMg3$p2inp1vec <- is.element(MELB23_TDMg3$player2, player1vector)

addPlayer1 <- MELB23_TDMg3[ which(MELB23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_TDMg3[ which(MELB23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_TDMg2 <- rbind(MELB23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
MELB23_TDMft <- ftable(MELB23_TDMg2$player1, MELB23_TDMg2$player2)
MELB23_TDMft2 <- as.matrix(MELB23_TDMft)
numRows <- nrow(MELB23_TDMft2)
numCols <- ncol(MELB23_TDMft2)
MELB23_TDMft3 <- MELB23_TDMft2[c(2:numRows) , c(2:numCols)]
MELB23_TDMTable <- graph.adjacency(MELB23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(MELB23_TDMTable, vertex.label = V(MELB23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
MELB23_TDM.clusterCoef <- transitivity(MELB23_TDMTable, type="global") #cluster coefficient
MELB23_TDM.degreeCent <- centralization.degree(MELB23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_TDMftn <- as.network.matrix(MELB23_TDMft)
MELB23_TDM.netDensity <- network.density(MELB23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_TDM.entropy <- entropy(MELB23_TDMft) #entropy

MELB23_TDM.netMx <- cbind(MELB23_TDM.netMx, MELB23_TDM.clusterCoef, MELB23_TDM.degreeCent$centralization,
                          MELB23_TDM.netDensity, MELB23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
MELB23_SDg2 <- data.frame(MELB23_SD)
MELB23_SDg2 <- MELB23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_SDg2$player1
player2vector <- MELB23_SDg2$player2
MELB23_SDg3 <- MELB23_SDg2
MELB23_SDg3$p1inp2vec <- is.element(MELB23_SDg3$player1, player2vector)
MELB23_SDg3$p2inp1vec <- is.element(MELB23_SDg3$player2, player1vector)

addPlayer1 <- MELB23_SDg3[ which(MELB23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_SDg3[ which(MELB23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_SDg2 <- rbind(MELB23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
MELB23_SDft <- ftable(MELB23_SDg2$player1, MELB23_SDg2$player2)
MELB23_SDft2 <- as.matrix(MELB23_SDft)
numRows <- nrow(MELB23_SDft2)
numCols <- ncol(MELB23_SDft2)
MELB23_SDft3 <- MELB23_SDft2[c(2:numRows) , c(2:numCols)]
MELB23_SDTable <- graph.adjacency(MELB23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(MELB23_SDTable, vertex.label = V(MELB23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
MELB23_SD.clusterCoef <- transitivity(MELB23_SDTable, type="global") #cluster coefficient
MELB23_SD.degreeCent <- centralization.degree(MELB23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_SDftn <- as.network.matrix(MELB23_SDft)
MELB23_SD.netDensity <- network.density(MELB23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_SD.entropy <- entropy(MELB23_SDft) #entropy

MELB23_SD.netMx <- cbind(MELB23_SD.netMx, MELB23_SD.clusterCoef, MELB23_SD.degreeCent$centralization,
                         MELB23_SD.netDensity, MELB23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
MELB23_TDg2 <- data.frame(MELB23_TD)
MELB23_TDg2 <- MELB23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_TDg2$player1
player2vector <- MELB23_TDg2$player2
MELB23_TDg3 <- MELB23_TDg2
MELB23_TDg3$p1inp2vec <- is.element(MELB23_TDg3$player1, player2vector)
MELB23_TDg3$p2inp1vec <- is.element(MELB23_TDg3$player2, player1vector)

addPlayer1 <- MELB23_TDg3[ which(MELB23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_TDg3[ which(MELB23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_TDg2 <- rbind(MELB23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
MELB23_TDft <- ftable(MELB23_TDg2$player1, MELB23_TDg2$player2)
MELB23_TDft2 <- as.matrix(MELB23_TDft)
numRows <- nrow(MELB23_TDft2)
numCols <- ncol(MELB23_TDft2)
MELB23_TDft3 <- MELB23_TDft2[c(2:numRows) , c(2:numCols)]
MELB23_TDTable <- graph.adjacency(MELB23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(MELB23_TDTable, vertex.label = V(MELB23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
MELB23_TD.clusterCoef <- transitivity(MELB23_TDTable, type="global") #cluster coefficient
MELB23_TD.degreeCent <- centralization.degree(MELB23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_TDftn <- as.network.matrix(MELB23_TDft)
MELB23_TD.netDensity <- network.density(MELB23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_TD.entropy <- entropy(MELB23_TDft) #entropy

MELB23_TD.netMx <- cbind(MELB23_TD.netMx, MELB23_TD.clusterCoef, MELB23_TD.degreeCent$centralization,
                         MELB23_TD.netDensity, MELB23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
MELB23_QTg2 <- data.frame(MELB23_QT)
MELB23_QTg2 <- MELB23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB23_QTg2$player1
player2vector <- MELB23_QTg2$player2
MELB23_QTg3 <- MELB23_QTg2
MELB23_QTg3$p1inp2vec <- is.element(MELB23_QTg3$player1, player2vector)
MELB23_QTg3$p2inp1vec <- is.element(MELB23_QTg3$player2, player1vector)

addPlayer1 <- MELB23_QTg3[ which(MELB23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB23_QTg3[ which(MELB23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB23_QTg2 <- rbind(MELB23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
MELB23_QTft <- ftable(MELB23_QTg2$player1, MELB23_QTg2$player2)
MELB23_QTft2 <- as.matrix(MELB23_QTft)
numRows <- nrow(MELB23_QTft2)
numCols <- ncol(MELB23_QTft2)
MELB23_QTft3 <- MELB23_QTft2[c(2:numRows) , c(2:numCols)]
MELB23_QTTable <- graph.adjacency(MELB23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(MELB23_QTTable, vertex.label = V(MELB23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
MELB23_QT.clusterCoef <- transitivity(MELB23_QTTable, type="global") #cluster coefficient
MELB23_QT.degreeCent <- centralization.degree(MELB23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB23_QTftn <- as.network.matrix(MELB23_QTft)
MELB23_QT.netDensity <- network.density(MELB23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB23_QT.entropy <- entropy(MELB23_QTft) #entropy

MELB23_QT.netMx <- cbind(MELB23_QT.netMx, MELB23_QT.clusterCoef, MELB23_QT.degreeCent$centralization,
                         MELB23_QT.netDensity, MELB23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB23_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
NMFC23_Gg2 <- data.frame(NMFC23_G)
NMFC23_Gg2 <- NMFC23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_Gg2$player1
player2vector <- NMFC23_Gg2$player2
NMFC23_Gg3 <- NMFC23_Gg2
NMFC23_Gg3$p1inp2vec <- is.element(NMFC23_Gg3$player1, player2vector)
NMFC23_Gg3$p2inp1vec <- is.element(NMFC23_Gg3$player2, player1vector)

addPlayer1 <- NMFC23_Gg3[ which(NMFC23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC23_Gg3[ which(NMFC23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_Gg2 <- rbind(NMFC23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
NMFC23_Gft <- ftable(NMFC23_Gg2$player1, NMFC23_Gg2$player2)
NMFC23_Gft2 <- as.matrix(NMFC23_Gft)
numRows <- nrow(NMFC23_Gft2)
numCols <- ncol(NMFC23_Gft2)
NMFC23_Gft3 <- NMFC23_Gft2[c(2:numRows) , c(2:numCols)]
NMFC23_GTable <- graph.adjacency(NMFC23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(NMFC23_GTable, vertex.label = V(NMFC23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
NMFC23_G.clusterCoef <- transitivity(NMFC23_GTable, type="global") #cluster coefficient
NMFC23_G.degreeCent <- centralization.degree(NMFC23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_Gftn <- as.network.matrix(NMFC23_Gft)
NMFC23_G.netDensity <- network.density(NMFC23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_G.entropy <- entropy(NMFC23_Gft) #entropy

NMFC23_G.netMx <- cbind(NMFC23_G.netMx, NMFC23_G.clusterCoef, NMFC23_G.degreeCent$centralization,
                        NMFC23_G.netDensity, NMFC23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
NMFC23_Bg2 <- data.frame(NMFC23_B)
NMFC23_Bg2 <- NMFC23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_Bg2$player1
player2vector <- NMFC23_Bg2$player2
NMFC23_Bg3 <- NMFC23_Bg2
NMFC23_Bg3$p1inp2vec <- is.element(NMFC23_Bg3$player1, player2vector)
NMFC23_Bg3$p2inp1vec <- is.element(NMFC23_Bg3$player2, player1vector)

addPlayer1 <- NMFC23_Bg3[ which(NMFC23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_Bg3[ which(NMFC23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_Bg2 <- rbind(NMFC23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
NMFC23_Bft <- ftable(NMFC23_Bg2$player1, NMFC23_Bg2$player2)
NMFC23_Bft2 <- as.matrix(NMFC23_Bft)
numRows <- nrow(NMFC23_Bft2)
numCols <- ncol(NMFC23_Bft2)
NMFC23_Bft3 <- NMFC23_Bft2[c(2:numRows) , c(2:numCols)]
NMFC23_BTable <- graph.adjacency(NMFC23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(NMFC23_BTable, vertex.label = V(NMFC23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
NMFC23_B.clusterCoef <- transitivity(NMFC23_BTable, type="global") #cluster coefficient
NMFC23_B.degreeCent <- centralization.degree(NMFC23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_Bftn <- as.network.matrix(NMFC23_Bft)
NMFC23_B.netDensity <- network.density(NMFC23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_B.entropy <- entropy(NMFC23_Bft) #entropy

NMFC23_B.netMx <- cbind(NMFC23_B.netMx, NMFC23_B.clusterCoef, NMFC23_B.degreeCent$centralization,
                        NMFC23_B.netDensity, NMFC23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
NMFC23_SFg2 <- data.frame(NMFC23_SF)
NMFC23_SFg2 <- NMFC23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_SFg2$player1
player2vector <- NMFC23_SFg2$player2
NMFC23_SFg3 <- NMFC23_SFg2
NMFC23_SFg3$p1inp2vec <- is.element(NMFC23_SFg3$player1, player2vector)
NMFC23_SFg3$p2inp1vec <- is.element(NMFC23_SFg3$player2, player1vector)

addPlayer1 <- NMFC23_SFg3[ which(NMFC23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_SFg3[ which(NMFC23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_SFg2 <- rbind(NMFC23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
NMFC23_SFft <- ftable(NMFC23_SFg2$player1, NMFC23_SFg2$player2)
NMFC23_SFft2 <- as.matrix(NMFC23_SFft)
numRows <- nrow(NMFC23_SFft2)
numCols <- ncol(NMFC23_SFft2)
NMFC23_SFft3 <- NMFC23_SFft2[c(2:numRows) , c(2:numCols)]
NMFC23_SFTable <- graph.adjacency(NMFC23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(NMFC23_SFTable, vertex.label = V(NMFC23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
NMFC23_SF.clusterCoef <- transitivity(NMFC23_SFTable, type="global") #cluster coefficient
NMFC23_SF.degreeCent <- centralization.degree(NMFC23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_SFftn <- as.network.matrix(NMFC23_SFft)
NMFC23_SF.netDensity <- network.density(NMFC23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_SF.entropy <- entropy(NMFC23_SFft) #entropy

NMFC23_SF.netMx <- cbind(NMFC23_SF.netMx, NMFC23_SF.clusterCoef, NMFC23_SF.degreeCent$centralization,
                         NMFC23_SF.netDensity, NMFC23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
NMFC23_TFg2 <- data.frame(NMFC23_TF)
NMFC23_TFg2 <- NMFC23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_TFg2$player1
player2vector <- NMFC23_TFg2$player2
NMFC23_TFg3 <- NMFC23_TFg2
NMFC23_TFg3$p1inp2vec <- is.element(NMFC23_TFg3$player1, player2vector)
NMFC23_TFg3$p2inp1vec <- is.element(NMFC23_TFg3$player2, player1vector)

addPlayer1 <- NMFC23_TFg3[ which(NMFC23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_TFg3[ which(NMFC23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_TFg2 <- rbind(NMFC23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
NMFC23_TFft <- ftable(NMFC23_TFg2$player1, NMFC23_TFg2$player2)
NMFC23_TFft2 <- as.matrix(NMFC23_TFft)
numRows <- nrow(NMFC23_TFft2)
numCols <- ncol(NMFC23_TFft2)
NMFC23_TFft3 <- NMFC23_TFft2[c(2:numRows) , c(2:numCols)]
NMFC23_TFTable <- graph.adjacency(NMFC23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(NMFC23_TFTable, vertex.label = V(NMFC23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
NMFC23_TF.clusterCoef <- transitivity(NMFC23_TFTable, type="global") #cluster coefficient
NMFC23_TF.degreeCent <- centralization.degree(NMFC23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_TFftn <- as.network.matrix(NMFC23_TFft)
NMFC23_TF.netDensity <- network.density(NMFC23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_TF.entropy <- entropy(NMFC23_TFft) #entropy

NMFC23_TF.netMx <- cbind(NMFC23_TF.netMx, NMFC23_TF.clusterCoef, NMFC23_TF.degreeCent$centralization,
                         NMFC23_TF.netDensity, NMFC23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
NMFC23_SAMg2 <- data.frame(NMFC23_SAM)
NMFC23_SAMg2 <- NMFC23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_SAMg2$player1
player2vector <- NMFC23_SAMg2$player2
NMFC23_SAMg3 <- NMFC23_SAMg2
NMFC23_SAMg3$p1inp2vec <- is.element(NMFC23_SAMg3$player1, player2vector)
NMFC23_SAMg3$p2inp1vec <- is.element(NMFC23_SAMg3$player2, player1vector)

addPlayer1 <- NMFC23_SAMg3[ which(NMFC23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_SAMg3[ which(NMFC23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_SAMg2 <- rbind(NMFC23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
NMFC23_SAMft <- ftable(NMFC23_SAMg2$player1, NMFC23_SAMg2$player2)
NMFC23_SAMft2 <- as.matrix(NMFC23_SAMft)
numRows <- nrow(NMFC23_SAMft2)
numCols <- ncol(NMFC23_SAMft2)
NMFC23_SAMft3 <- NMFC23_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC23_SAMTable <- graph.adjacency(NMFC23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(NMFC23_SAMTable, vertex.label = V(NMFC23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
NMFC23_SAM.clusterCoef <- transitivity(NMFC23_SAMTable, type="global") #cluster coefficient
NMFC23_SAM.degreeCent <- centralization.degree(NMFC23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_SAMftn <- as.network.matrix(NMFC23_SAMft)
NMFC23_SAM.netDensity <- network.density(NMFC23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_SAM.entropy <- entropy(NMFC23_SAMft) #entropy

NMFC23_SAM.netMx <- cbind(NMFC23_SAM.netMx, NMFC23_SAM.clusterCoef, NMFC23_SAM.degreeCent$centralization,
                          NMFC23_SAM.netDensity, NMFC23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
NMFC23_TAMg2 <- data.frame(NMFC23_TAM)
NMFC23_TAMg2 <- NMFC23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_TAMg2$player1
player2vector <- NMFC23_TAMg2$player2
NMFC23_TAMg3 <- NMFC23_TAMg2
NMFC23_TAMg3$p1inp2vec <- is.element(NMFC23_TAMg3$player1, player2vector)
NMFC23_TAMg3$p2inp1vec <- is.element(NMFC23_TAMg3$player2, player1vector)

addPlayer1 <- NMFC23_TAMg3[ which(NMFC23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC23_TAMg3[ which(NMFC23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_TAMg2 <- rbind(NMFC23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
NMFC23_TAMft <- ftable(NMFC23_TAMg2$player1, NMFC23_TAMg2$player2)
NMFC23_TAMft2 <- as.matrix(NMFC23_TAMft)
numRows <- nrow(NMFC23_TAMft2)
numCols <- ncol(NMFC23_TAMft2)
NMFC23_TAMft3 <- NMFC23_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC23_TAMTable <- graph.adjacency(NMFC23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(NMFC23_TAMTable, vertex.label = V(NMFC23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
NMFC23_TAM.clusterCoef <- transitivity(NMFC23_TAMTable, type="global") #cluster coefficient
NMFC23_TAM.degreeCent <- centralization.degree(NMFC23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_TAMftn <- as.network.matrix(NMFC23_TAMft)
NMFC23_TAM.netDensity <- network.density(NMFC23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_TAM.entropy <- entropy(NMFC23_TAMft) #entropy

NMFC23_TAM.netMx <- cbind(NMFC23_TAM.netMx, NMFC23_TAM.clusterCoef, NMFC23_TAM.degreeCent$centralization,
                          NMFC23_TAM.netDensity, NMFC23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
NMFC23_SDMg2 <- data.frame(NMFC23_SDM)
NMFC23_SDMg2 <- NMFC23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_SDMg2$player1
player2vector <- NMFC23_SDMg2$player2
NMFC23_SDMg3 <- NMFC23_SDMg2
NMFC23_SDMg3$p1inp2vec <- is.element(NMFC23_SDMg3$player1, player2vector)
NMFC23_SDMg3$p2inp1vec <- is.element(NMFC23_SDMg3$player2, player1vector)

addPlayer1 <- NMFC23_SDMg3[ which(NMFC23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_SDMg3[ which(NMFC23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_SDMg2 <- rbind(NMFC23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
NMFC23_SDMft <- ftable(NMFC23_SDMg2$player1, NMFC23_SDMg2$player2)
NMFC23_SDMft2 <- as.matrix(NMFC23_SDMft)
numRows <- nrow(NMFC23_SDMft2)
numCols <- ncol(NMFC23_SDMft2)
NMFC23_SDMft3 <- NMFC23_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC23_SDMTable <- graph.adjacency(NMFC23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(NMFC23_SDMTable, vertex.label = V(NMFC23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
NMFC23_SDM.clusterCoef <- transitivity(NMFC23_SDMTable, type="global") #cluster coefficient
NMFC23_SDM.degreeCent <- centralization.degree(NMFC23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_SDMftn <- as.network.matrix(NMFC23_SDMft)
NMFC23_SDM.netDensity <- network.density(NMFC23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_SDM.entropy <- entropy(NMFC23_SDMft) #entropy

NMFC23_SDM.netMx <- cbind(NMFC23_SDM.netMx, NMFC23_SDM.clusterCoef, NMFC23_SDM.degreeCent$centralization,
                          NMFC23_SDM.netDensity, NMFC23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
NMFC23_TDMg2 <- data.frame(NMFC23_TDM)
NMFC23_TDMg2 <- NMFC23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_TDMg2$player1
player2vector <- NMFC23_TDMg2$player2
NMFC23_TDMg3 <- NMFC23_TDMg2
NMFC23_TDMg3$p1inp2vec <- is.element(NMFC23_TDMg3$player1, player2vector)
NMFC23_TDMg3$p2inp1vec <- is.element(NMFC23_TDMg3$player2, player1vector)

addPlayer1 <- NMFC23_TDMg3[ which(NMFC23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_TDMg3[ which(NMFC23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_TDMg2 <- rbind(NMFC23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
NMFC23_TDMft <- ftable(NMFC23_TDMg2$player1, NMFC23_TDMg2$player2)
NMFC23_TDMft2 <- as.matrix(NMFC23_TDMft)
numRows <- nrow(NMFC23_TDMft2)
numCols <- ncol(NMFC23_TDMft2)
NMFC23_TDMft3 <- NMFC23_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC23_TDMTable <- graph.adjacency(NMFC23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(NMFC23_TDMTable, vertex.label = V(NMFC23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
NMFC23_TDM.clusterCoef <- transitivity(NMFC23_TDMTable, type="global") #cluster coefficient
NMFC23_TDM.degreeCent <- centralization.degree(NMFC23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_TDMftn <- as.network.matrix(NMFC23_TDMft)
NMFC23_TDM.netDensity <- network.density(NMFC23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_TDM.entropy <- entropy(NMFC23_TDMft) #entropy

NMFC23_TDM.netMx <- cbind(NMFC23_TDM.netMx, NMFC23_TDM.clusterCoef, NMFC23_TDM.degreeCent$centralization,
                          NMFC23_TDM.netDensity, NMFC23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
NMFC23_SDg2 <- data.frame(NMFC23_SD)
NMFC23_SDg2 <- NMFC23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_SDg2$player1
player2vector <- NMFC23_SDg2$player2
NMFC23_SDg3 <- NMFC23_SDg2
NMFC23_SDg3$p1inp2vec <- is.element(NMFC23_SDg3$player1, player2vector)
NMFC23_SDg3$p2inp1vec <- is.element(NMFC23_SDg3$player2, player1vector)

addPlayer1 <- NMFC23_SDg3[ which(NMFC23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_SDg3[ which(NMFC23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_SDg2 <- rbind(NMFC23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
NMFC23_SDft <- ftable(NMFC23_SDg2$player1, NMFC23_SDg2$player2)
NMFC23_SDft2 <- as.matrix(NMFC23_SDft)
numRows <- nrow(NMFC23_SDft2)
numCols <- ncol(NMFC23_SDft2)
NMFC23_SDft3 <- NMFC23_SDft2[c(2:numRows) , c(2:numCols)]
NMFC23_SDTable <- graph.adjacency(NMFC23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(NMFC23_SDTable, vertex.label = V(NMFC23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
NMFC23_SD.clusterCoef <- transitivity(NMFC23_SDTable, type="global") #cluster coefficient
NMFC23_SD.degreeCent <- centralization.degree(NMFC23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_SDftn <- as.network.matrix(NMFC23_SDft)
NMFC23_SD.netDensity <- network.density(NMFC23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_SD.entropy <- entropy(NMFC23_SDft) #entropy

NMFC23_SD.netMx <- cbind(NMFC23_SD.netMx, NMFC23_SD.clusterCoef, NMFC23_SD.degreeCent$centralization,
                         NMFC23_SD.netDensity, NMFC23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
NMFC23_TDg2 <- data.frame(NMFC23_TD)
NMFC23_TDg2 <- NMFC23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_TDg2$player1
player2vector <- NMFC23_TDg2$player2
NMFC23_TDg3 <- NMFC23_TDg2
NMFC23_TDg3$p1inp2vec <- is.element(NMFC23_TDg3$player1, player2vector)
NMFC23_TDg3$p2inp1vec <- is.element(NMFC23_TDg3$player2, player1vector)

addPlayer1 <- NMFC23_TDg3[ which(NMFC23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_TDg3[ which(NMFC23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_TDg2 <- rbind(NMFC23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
NMFC23_TDft <- ftable(NMFC23_TDg2$player1, NMFC23_TDg2$player2)
NMFC23_TDft2 <- as.matrix(NMFC23_TDft)
numRows <- nrow(NMFC23_TDft2)
numCols <- ncol(NMFC23_TDft2)
NMFC23_TDft3 <- NMFC23_TDft2[c(2:numRows) , c(2:numCols)]
NMFC23_TDTable <- graph.adjacency(NMFC23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(NMFC23_TDTable, vertex.label = V(NMFC23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
NMFC23_TD.clusterCoef <- transitivity(NMFC23_TDTable, type="global") #cluster coefficient
NMFC23_TD.degreeCent <- centralization.degree(NMFC23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_TDftn <- as.network.matrix(NMFC23_TDft)
NMFC23_TD.netDensity <- network.density(NMFC23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_TD.entropy <- entropy(NMFC23_TDft) #entropy

NMFC23_TD.netMx <- cbind(NMFC23_TD.netMx, NMFC23_TD.clusterCoef, NMFC23_TD.degreeCent$centralization,
                         NMFC23_TD.netDensity, NMFC23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
NMFC23_QTg2 <- data.frame(NMFC23_QT)
NMFC23_QTg2 <- NMFC23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC23_QTg2$player1
player2vector <- NMFC23_QTg2$player2
NMFC23_QTg3 <- NMFC23_QTg2
NMFC23_QTg3$p1inp2vec <- is.element(NMFC23_QTg3$player1, player2vector)
NMFC23_QTg3$p2inp1vec <- is.element(NMFC23_QTg3$player2, player1vector)

addPlayer1 <- NMFC23_QTg3[ which(NMFC23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC23_QTg3[ which(NMFC23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC23_QTg2 <- rbind(NMFC23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
NMFC23_QTft <- ftable(NMFC23_QTg2$player1, NMFC23_QTg2$player2)
NMFC23_QTft2 <- as.matrix(NMFC23_QTft)
numRows <- nrow(NMFC23_QTft2)
numCols <- ncol(NMFC23_QTft2)
NMFC23_QTft3 <- NMFC23_QTft2[c(2:numRows) , c(2:numCols)]
NMFC23_QTTable <- graph.adjacency(NMFC23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(NMFC23_QTTable, vertex.label = V(NMFC23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
NMFC23_QT.clusterCoef <- transitivity(NMFC23_QTTable, type="global") #cluster coefficient
NMFC23_QT.degreeCent <- centralization.degree(NMFC23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC23_QTftn <- as.network.matrix(NMFC23_QTft)
NMFC23_QT.netDensity <- network.density(NMFC23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC23_QT.entropy <- entropy(NMFC23_QTft) #entropy

NMFC23_QT.netMx <- cbind(NMFC23_QT.netMx, NMFC23_QT.clusterCoef, NMFC23_QT.degreeCent$centralization,
                         NMFC23_QT.netDensity, NMFC23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC23_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Goal_F"
PORT23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
PORT23_Gg2 <- data.frame(PORT23_G)
PORT23_Gg2 <- PORT23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_Gg2$player1
player2vector <- PORT23_Gg2$player2
PORT23_Gg3 <- PORT23_Gg2
PORT23_Gg3$p1inp2vec <- is.element(PORT23_Gg3$player1, player2vector)
PORT23_Gg3$p2inp1vec <- is.element(PORT23_Gg3$player2, player1vector)

addPlayer1 <- PORT23_Gg3[ which(PORT23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT23_Gg2 <- rbind(PORT23_Gg2, addPlayer1)

#ROUND 23, Goal graph using weighted edges
PORT23_Gft <- ftable(PORT23_Gg2$player1, PORT23_Gg2$player2)
PORT23_Gft2 <- as.matrix(PORT23_Gft)
numRows <- nrow(PORT23_Gft2)
numCols <- ncol(PORT23_Gft2)
PORT23_Gft3 <- PORT23_Gft2[c(2:numRows) , c(1:numCols)]
PORT23_GTable <- graph.adjacency(PORT23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(PORT23_GTable, vertex.label = V(PORT23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
PORT23_G.clusterCoef <- transitivity(PORT23_GTable, type="global") #cluster coefficient
PORT23_G.degreeCent <- centralization.degree(PORT23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_Gftn <- as.network.matrix(PORT23_Gft)
PORT23_G.netDensity <- network.density(PORT23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_G.entropy <- entropy(PORT23_Gft) #entropy

PORT23_G.netMx <- cbind(PORT23_G.netMx, PORT23_G.clusterCoef, PORT23_G.degreeCent$centralization,
                        PORT23_G.netDensity, PORT23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "PORT"
KIoutcome = "Behind_F"
PORT23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
PORT23_Bg2 <- data.frame(PORT23_B)
PORT23_Bg2 <- PORT23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_Bg2$player1
player2vector <- PORT23_Bg2$player2
PORT23_Bg3 <- PORT23_Bg2
PORT23_Bg3$p1inp2vec <- is.element(PORT23_Bg3$player1, player2vector)
PORT23_Bg3$p2inp1vec <- is.element(PORT23_Bg3$player2, player1vector)

addPlayer1 <- PORT23_Bg3[ which(PORT23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_Bg3[ which(PORT23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_Bg2 <- rbind(PORT23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
PORT23_Bft <- ftable(PORT23_Bg2$player1, PORT23_Bg2$player2)
PORT23_Bft2 <- as.matrix(PORT23_Bft)
numRows <- nrow(PORT23_Bft2)
numCols <- ncol(PORT23_Bft2)
PORT23_Bft3 <- PORT23_Bft2[c(2:numRows) , c(2:numCols)]
PORT23_BTable <- graph.adjacency(PORT23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(PORT23_BTable, vertex.label = V(PORT23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
PORT23_B.clusterCoef <- transitivity(PORT23_BTable, type="global") #cluster coefficient
PORT23_B.degreeCent <- centralization.degree(PORT23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_Bftn <- as.network.matrix(PORT23_Bft)
PORT23_B.netDensity <- network.density(PORT23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_B.entropy <- entropy(PORT23_Bft) #entropy

PORT23_B.netMx <- cbind(PORT23_B.netMx, PORT23_B.clusterCoef, PORT23_B.degreeCent$centralization,
                        PORT23_B.netDensity, PORT23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
PORT23_SFg2 <- data.frame(PORT23_SF)
PORT23_SFg2 <- PORT23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_SFg2$player1
player2vector <- PORT23_SFg2$player2
PORT23_SFg3 <- PORT23_SFg2
PORT23_SFg3$p1inp2vec <- is.element(PORT23_SFg3$player1, player2vector)
PORT23_SFg3$p2inp1vec <- is.element(PORT23_SFg3$player2, player1vector)

addPlayer1 <- PORT23_SFg3[ which(PORT23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_SFg3[ which(PORT23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_SFg2 <- rbind(PORT23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
PORT23_SFft <- ftable(PORT23_SFg2$player1, PORT23_SFg2$player2)
PORT23_SFft2 <- as.matrix(PORT23_SFft)
numRows <- nrow(PORT23_SFft2)
numCols <- ncol(PORT23_SFft2)
PORT23_SFft3 <- PORT23_SFft2[c(2:numRows) , c(2:numCols)]
PORT23_SFTable <- graph.adjacency(PORT23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(PORT23_SFTable, vertex.label = V(PORT23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
PORT23_SF.clusterCoef <- transitivity(PORT23_SFTable, type="global") #cluster coefficient
PORT23_SF.degreeCent <- centralization.degree(PORT23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_SFftn <- as.network.matrix(PORT23_SFft)
PORT23_SF.netDensity <- network.density(PORT23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_SF.entropy <- entropy(PORT23_SFft) #entropy

PORT23_SF.netMx <- cbind(PORT23_SF.netMx, PORT23_SF.clusterCoef, PORT23_SF.degreeCent$centralization,
                         PORT23_SF.netDensity, PORT23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
PORT23_TFg2 <- data.frame(PORT23_TF)
PORT23_TFg2 <- PORT23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_TFg2$player1
player2vector <- PORT23_TFg2$player2
PORT23_TFg3 <- PORT23_TFg2
PORT23_TFg3$p1inp2vec <- is.element(PORT23_TFg3$player1, player2vector)
PORT23_TFg3$p2inp1vec <- is.element(PORT23_TFg3$player2, player1vector)

addPlayer1 <- PORT23_TFg3[ which(PORT23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT23_TFg2 <- rbind(PORT23_TFg2, addPlayer1)

#ROUND 23, FWD Turnover graph using weighted edges
PORT23_TFft <- ftable(PORT23_TFg2$player1, PORT23_TFg2$player2)
PORT23_TFft2 <- as.matrix(PORT23_TFft)
numRows <- nrow(PORT23_TFft2)
numCols <- ncol(PORT23_TFft2)
PORT23_TFft3 <- PORT23_TFft2[c(2:numRows) , c(1:numCols)]
PORT23_TFTable <- graph.adjacency(PORT23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(PORT23_TFTable, vertex.label = V(PORT23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
PORT23_TF.clusterCoef <- transitivity(PORT23_TFTable, type="global") #cluster coefficient
PORT23_TF.degreeCent <- centralization.degree(PORT23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_TFftn <- as.network.matrix(PORT23_TFft)
PORT23_TF.netDensity <- network.density(PORT23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_TF.entropy <- entropy(PORT23_TFft) #entropy

PORT23_TF.netMx <- cbind(PORT23_TF.netMx, PORT23_TF.clusterCoef, PORT23_TF.degreeCent$centralization,
                         PORT23_TF.netDensity, PORT23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
PORT23_SAMg2 <- data.frame(PORT23_SAM)
PORT23_SAMg2 <- PORT23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_SAMg2$player1
player2vector <- PORT23_SAMg2$player2
PORT23_SAMg3 <- PORT23_SAMg2
PORT23_SAMg3$p1inp2vec <- is.element(PORT23_SAMg3$player1, player2vector)
PORT23_SAMg3$p2inp1vec <- is.element(PORT23_SAMg3$player2, player1vector)

addPlayer1 <- PORT23_SAMg3[ which(PORT23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_SAMg3[ which(PORT23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_SAMg2 <- rbind(PORT23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
PORT23_SAMft <- ftable(PORT23_SAMg2$player1, PORT23_SAMg2$player2)
PORT23_SAMft2 <- as.matrix(PORT23_SAMft)
numRows <- nrow(PORT23_SAMft2)
numCols <- ncol(PORT23_SAMft2)
PORT23_SAMft3 <- PORT23_SAMft2[c(2:numRows) , c(2:numCols)]
PORT23_SAMTable <- graph.adjacency(PORT23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(PORT23_SAMTable, vertex.label = V(PORT23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
PORT23_SAM.clusterCoef <- transitivity(PORT23_SAMTable, type="global") #cluster coefficient
PORT23_SAM.degreeCent <- centralization.degree(PORT23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_SAMftn <- as.network.matrix(PORT23_SAMft)
PORT23_SAM.netDensity <- network.density(PORT23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_SAM.entropy <- entropy(PORT23_SAMft) #entropy

PORT23_SAM.netMx <- cbind(PORT23_SAM.netMx, PORT23_SAM.clusterCoef, PORT23_SAM.degreeCent$centralization,
                          PORT23_SAM.netDensity, PORT23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
PORT23_TAMg2 <- data.frame(PORT23_TAM)
PORT23_TAMg2 <- PORT23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_TAMg2$player1
player2vector <- PORT23_TAMg2$player2
PORT23_TAMg3 <- PORT23_TAMg2
PORT23_TAMg3$p1inp2vec <- is.element(PORT23_TAMg3$player1, player2vector)
PORT23_TAMg3$p2inp1vec <- is.element(PORT23_TAMg3$player2, player1vector)

addPlayer1 <- PORT23_TAMg3[ which(PORT23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_TAMg3[ which(PORT23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_TAMg2 <- rbind(PORT23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
PORT23_TAMft <- ftable(PORT23_TAMg2$player1, PORT23_TAMg2$player2)
PORT23_TAMft2 <- as.matrix(PORT23_TAMft)
numRows <- nrow(PORT23_TAMft2)
numCols <- ncol(PORT23_TAMft2)
PORT23_TAMft3 <- PORT23_TAMft2[c(2:numRows) , c(2:numCols)]
PORT23_TAMTable <- graph.adjacency(PORT23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(PORT23_TAMTable, vertex.label = V(PORT23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
PORT23_TAM.clusterCoef <- transitivity(PORT23_TAMTable, type="global") #cluster coefficient
PORT23_TAM.degreeCent <- centralization.degree(PORT23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_TAMftn <- as.network.matrix(PORT23_TAMft)
PORT23_TAM.netDensity <- network.density(PORT23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_TAM.entropy <- entropy(PORT23_TAMft) #entropy

PORT23_TAM.netMx <- cbind(PORT23_TAM.netMx, PORT23_TAM.clusterCoef, PORT23_TAM.degreeCent$centralization,
                          PORT23_TAM.netDensity, PORT23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
PORT23_SDMg2 <- data.frame(PORT23_SDM)
PORT23_SDMg2 <- PORT23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_SDMg2$player1
player2vector <- PORT23_SDMg2$player2
PORT23_SDMg3 <- PORT23_SDMg2
PORT23_SDMg3$p1inp2vec <- is.element(PORT23_SDMg3$player1, player2vector)
PORT23_SDMg3$p2inp1vec <- is.element(PORT23_SDMg3$player2, player1vector)

addPlayer1 <- PORT23_SDMg3[ which(PORT23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_SDMg3[ which(PORT23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_SDMg2 <- rbind(PORT23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
PORT23_SDMft <- ftable(PORT23_SDMg2$player1, PORT23_SDMg2$player2)
PORT23_SDMft2 <- as.matrix(PORT23_SDMft)
numRows <- nrow(PORT23_SDMft2)
numCols <- ncol(PORT23_SDMft2)
PORT23_SDMft3 <- PORT23_SDMft2[c(2:numRows) , c(2:numCols)]
PORT23_SDMTable <- graph.adjacency(PORT23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(PORT23_SDMTable, vertex.label = V(PORT23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
PORT23_SDM.clusterCoef <- transitivity(PORT23_SDMTable, type="global") #cluster coefficient
PORT23_SDM.degreeCent <- centralization.degree(PORT23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_SDMftn <- as.network.matrix(PORT23_SDMft)
PORT23_SDM.netDensity <- network.density(PORT23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_SDM.entropy <- entropy(PORT23_SDMft) #entropy

PORT23_SDM.netMx <- cbind(PORT23_SDM.netMx, PORT23_SDM.clusterCoef, PORT23_SDM.degreeCent$centralization,
                          PORT23_SDM.netDensity, PORT23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
PORT23_TDMg2 <- data.frame(PORT23_TDM)
PORT23_TDMg2 <- PORT23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_TDMg2$player1
player2vector <- PORT23_TDMg2$player2
PORT23_TDMg3 <- PORT23_TDMg2
PORT23_TDMg3$p1inp2vec <- is.element(PORT23_TDMg3$player1, player2vector)
PORT23_TDMg3$p2inp1vec <- is.element(PORT23_TDMg3$player2, player1vector)

addPlayer1 <- PORT23_TDMg3[ which(PORT23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_TDMg3[ which(PORT23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_TDMg2 <- rbind(PORT23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
PORT23_TDMft <- ftable(PORT23_TDMg2$player1, PORT23_TDMg2$player2)
PORT23_TDMft2 <- as.matrix(PORT23_TDMft)
numRows <- nrow(PORT23_TDMft2)
numCols <- ncol(PORT23_TDMft2)
PORT23_TDMft3 <- PORT23_TDMft2[c(2:numRows) , c(2:numCols)]
PORT23_TDMTable <- graph.adjacency(PORT23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(PORT23_TDMTable, vertex.label = V(PORT23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
PORT23_TDM.clusterCoef <- transitivity(PORT23_TDMTable, type="global") #cluster coefficient
PORT23_TDM.degreeCent <- centralization.degree(PORT23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_TDMftn <- as.network.matrix(PORT23_TDMft)
PORT23_TDM.netDensity <- network.density(PORT23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_TDM.entropy <- entropy(PORT23_TDMft) #entropy

PORT23_TDM.netMx <- cbind(PORT23_TDM.netMx, PORT23_TDM.clusterCoef, PORT23_TDM.degreeCent$centralization,
                          PORT23_TDM.netDensity, PORT23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
PORT23_SDg2 <- data.frame(PORT23_SD)
PORT23_SDg2 <- PORT23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_SDg2$player1
player2vector <- PORT23_SDg2$player2
PORT23_SDg3 <- PORT23_SDg2
PORT23_SDg3$p1inp2vec <- is.element(PORT23_SDg3$player1, player2vector)
PORT23_SDg3$p2inp1vec <- is.element(PORT23_SDg3$player2, player1vector)

addPlayer1 <- PORT23_SDg3[ which(PORT23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_SDg3[ which(PORT23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_SDg2 <- rbind(PORT23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
PORT23_SDft <- ftable(PORT23_SDg2$player1, PORT23_SDg2$player2)
PORT23_SDft2 <- as.matrix(PORT23_SDft)
numRows <- nrow(PORT23_SDft2)
numCols <- ncol(PORT23_SDft2)
PORT23_SDft3 <- PORT23_SDft2[c(2:numRows) , c(2:numCols)]
PORT23_SDTable <- graph.adjacency(PORT23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(PORT23_SDTable, vertex.label = V(PORT23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
PORT23_SD.clusterCoef <- transitivity(PORT23_SDTable, type="global") #cluster coefficient
PORT23_SD.degreeCent <- centralization.degree(PORT23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_SDftn <- as.network.matrix(PORT23_SDft)
PORT23_SD.netDensity <- network.density(PORT23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_SD.entropy <- entropy(PORT23_SDft) #entropy

PORT23_SD.netMx <- cbind(PORT23_SD.netMx, PORT23_SD.clusterCoef, PORT23_SD.degreeCent$centralization,
                         PORT23_SD.netDensity, PORT23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
PORT23_TDg2 <- data.frame(PORT23_TD)
PORT23_TDg2 <- PORT23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_TDg2$player1
player2vector <- PORT23_TDg2$player2
PORT23_TDg3 <- PORT23_TDg2
PORT23_TDg3$p1inp2vec <- is.element(PORT23_TDg3$player1, player2vector)
PORT23_TDg3$p2inp1vec <- is.element(PORT23_TDg3$player2, player1vector)

addPlayer1 <- PORT23_TDg3[ which(PORT23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_TDg3[ which(PORT23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_TDg2 <- rbind(PORT23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
PORT23_TDft <- ftable(PORT23_TDg2$player1, PORT23_TDg2$player2)
PORT23_TDft2 <- as.matrix(PORT23_TDft)
numRows <- nrow(PORT23_TDft2)
numCols <- ncol(PORT23_TDft2)
PORT23_TDft3 <- PORT23_TDft2[c(2:numRows) , c(2:numCols)]
PORT23_TDTable <- graph.adjacency(PORT23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(PORT23_TDTable, vertex.label = V(PORT23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
PORT23_TD.clusterCoef <- transitivity(PORT23_TDTable, type="global") #cluster coefficient
PORT23_TD.degreeCent <- centralization.degree(PORT23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_TDftn <- as.network.matrix(PORT23_TDft)
PORT23_TD.netDensity <- network.density(PORT23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_TD.entropy <- entropy(PORT23_TDft) #entropy

PORT23_TD.netMx <- cbind(PORT23_TD.netMx, PORT23_TD.clusterCoef, PORT23_TD.degreeCent$centralization,
                         PORT23_TD.netDensity, PORT23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************

round = 23
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
PORT23_QTg2 <- data.frame(PORT23_QT)
PORT23_QTg2 <- PORT23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT23_QTg2$player1
player2vector <- PORT23_QTg2$player2
PORT23_QTg3 <- PORT23_QTg2
PORT23_QTg3$p1inp2vec <- is.element(PORT23_QTg3$player1, player2vector)
PORT23_QTg3$p2inp1vec <- is.element(PORT23_QTg3$player2, player1vector)

addPlayer1 <- PORT23_QTg3[ which(PORT23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT23_QTg3[ which(PORT23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT23_QTg2 <- rbind(PORT23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
PORT23_QTft <- ftable(PORT23_QTg2$player1, PORT23_QTg2$player2)
PORT23_QTft2 <- as.matrix(PORT23_QTft)
numRows <- nrow(PORT23_QTft2)
numCols <- ncol(PORT23_QTft2)
PORT23_QTft3 <- PORT23_QTft2[c(2:numRows) , c(2:numCols)]
PORT23_QTTable <- graph.adjacency(PORT23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(PORT23_QTTable, vertex.label = V(PORT23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
PORT23_QT.clusterCoef <- transitivity(PORT23_QTTable, type="global") #cluster coefficient
PORT23_QT.degreeCent <- centralization.degree(PORT23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT23_QTftn <- as.network.matrix(PORT23_QTft)
PORT23_QT.netDensity <- network.density(PORT23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT23_QT.entropy <- entropy(PORT23_QTft) #entropy

PORT23_QT.netMx <- cbind(PORT23_QT.netMx, PORT23_QT.clusterCoef, PORT23_QT.degreeCent$centralization,
                         PORT23_QT.netDensity, PORT23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT23_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "RICH"
KIoutcome = "Goal_F"
RICH23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
RICH23_Gg2 <- data.frame(RICH23_G)
RICH23_Gg2 <- RICH23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_Gg2$player1
player2vector <- RICH23_Gg2$player2
RICH23_Gg3 <- RICH23_Gg2
RICH23_Gg3$p1inp2vec <- is.element(RICH23_Gg3$player1, player2vector)
RICH23_Gg3$p2inp1vec <- is.element(RICH23_Gg3$player2, player1vector)

addPlayer1 <- RICH23_Gg3[ which(RICH23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_Gg3[ which(RICH23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_Gg2 <- rbind(RICH23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
RICH23_Gft <- ftable(RICH23_Gg2$player1, RICH23_Gg2$player2)
RICH23_Gft2 <- as.matrix(RICH23_Gft)
numRows <- nrow(RICH23_Gft2)
numCols <- ncol(RICH23_Gft2)
RICH23_Gft3 <- RICH23_Gft2[c(2:numRows) , c(2:numCols)]
RICH23_GTable <- graph.adjacency(RICH23_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(RICH23_GTable, vertex.label = V(RICH23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
RICH23_G.clusterCoef <- transitivity(RICH23_GTable, type="global") #cluster coefficient
RICH23_G.degreeCent <- centralization.degree(RICH23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_Gftn <- as.network.matrix(RICH23_Gft)
RICH23_G.netDensity <- network.density(RICH23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_G.entropy <- entropy(RICH23_Gft) #entropy

RICH23_G.netMx <- cbind(RICH23_G.netMx, RICH23_G.clusterCoef, RICH23_G.degreeCent$centralization,
                        RICH23_G.netDensity, RICH23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "RICH"
KIoutcome = "Behind_F"
RICH23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
RICH23_Bg2 <- data.frame(RICH23_B)
RICH23_Bg2 <- RICH23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_Bg2$player1
player2vector <- RICH23_Bg2$player2
RICH23_Bg3 <- RICH23_Bg2
RICH23_Bg3$p1inp2vec <- is.element(RICH23_Bg3$player1, player2vector)
RICH23_Bg3$p2inp1vec <- is.element(RICH23_Bg3$player2, player1vector)

addPlayer1 <- RICH23_Bg3[ which(RICH23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_Bg3[ which(RICH23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_Bg2 <- rbind(RICH23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
RICH23_Bft <- ftable(RICH23_Bg2$player1, RICH23_Bg2$player2)
RICH23_Bft2 <- as.matrix(RICH23_Bft)
numRows <- nrow(RICH23_Bft2)
numCols <- ncol(RICH23_Bft2)
RICH23_Bft3 <- RICH23_Bft2[c(2:numRows) , c(2:numCols)]
RICH23_BTable <- graph.adjacency(RICH23_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(RICH23_BTable, vertex.label = V(RICH23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
RICH23_B.clusterCoef <- transitivity(RICH23_BTable, type="global") #cluster coefficient
RICH23_B.degreeCent <- centralization.degree(RICH23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_Bftn <- as.network.matrix(RICH23_Bft)
RICH23_B.netDensity <- network.density(RICH23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_B.entropy <- entropy(RICH23_Bft) #entropy

RICH23_B.netMx <- cbind(RICH23_B.netMx, RICH23_B.clusterCoef, RICH23_B.degreeCent$centralization,
                        RICH23_B.netDensity, RICH23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
RICH23_SFg2 <- data.frame(RICH23_SF)
RICH23_SFg2 <- RICH23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_SFg2$player1
player2vector <- RICH23_SFg2$player2
RICH23_SFg3 <- RICH23_SFg2
RICH23_SFg3$p1inp2vec <- is.element(RICH23_SFg3$player1, player2vector)
RICH23_SFg3$p2inp1vec <- is.element(RICH23_SFg3$player2, player1vector)

addPlayer1 <- RICH23_SFg3[ which(RICH23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_SFg3[ which(RICH23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_SFg2 <- rbind(RICH23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
RICH23_SFft <- ftable(RICH23_SFg2$player1, RICH23_SFg2$player2)
RICH23_SFft2 <- as.matrix(RICH23_SFft)
numRows <- nrow(RICH23_SFft2)
numCols <- ncol(RICH23_SFft2)
RICH23_SFft3 <- RICH23_SFft2[c(2:numRows) , c(2:numCols)]
RICH23_SFTable <- graph.adjacency(RICH23_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(RICH23_SFTable, vertex.label = V(RICH23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
RICH23_SF.clusterCoef <- transitivity(RICH23_SFTable, type="global") #cluster coefficient
RICH23_SF.degreeCent <- centralization.degree(RICH23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_SFftn <- as.network.matrix(RICH23_SFft)
RICH23_SF.netDensity <- network.density(RICH23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_SF.entropy <- entropy(RICH23_SFft) #entropy

RICH23_SF.netMx <- cbind(RICH23_SF.netMx, RICH23_SF.clusterCoef, RICH23_SF.degreeCent$centralization,
                         RICH23_SF.netDensity, RICH23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
RICH23_TFg2 <- data.frame(RICH23_TF)
RICH23_TFg2 <- RICH23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_TFg2$player1
player2vector <- RICH23_TFg2$player2
RICH23_TFg3 <- RICH23_TFg2
RICH23_TFg3$p1inp2vec <- is.element(RICH23_TFg3$player1, player2vector)
RICH23_TFg3$p2inp1vec <- is.element(RICH23_TFg3$player2, player1vector)

addPlayer1 <- RICH23_TFg3[ which(RICH23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_TFg3[ which(RICH23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_TFg2 <- rbind(RICH23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
RICH23_TFft <- ftable(RICH23_TFg2$player1, RICH23_TFg2$player2)
RICH23_TFft2 <- as.matrix(RICH23_TFft)
numRows <- nrow(RICH23_TFft2)
numCols <- ncol(RICH23_TFft2)
RICH23_TFft3 <- RICH23_TFft2[c(2:numRows) , c(2:numCols)]
RICH23_TFTable <- graph.adjacency(RICH23_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(RICH23_TFTable, vertex.label = V(RICH23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
RICH23_TF.clusterCoef <- transitivity(RICH23_TFTable, type="global") #cluster coefficient
RICH23_TF.degreeCent <- centralization.degree(RICH23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_TFftn <- as.network.matrix(RICH23_TFft)
RICH23_TF.netDensity <- network.density(RICH23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_TF.entropy <- entropy(RICH23_TFft) #entropy

RICH23_TF.netMx <- cbind(RICH23_TF.netMx, RICH23_TF.clusterCoef, RICH23_TF.degreeCent$centralization,
                         RICH23_TF.netDensity, RICH23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
RICH23_SAMg2 <- data.frame(RICH23_SAM)
RICH23_SAMg2 <- RICH23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_SAMg2$player1
player2vector <- RICH23_SAMg2$player2
RICH23_SAMg3 <- RICH23_SAMg2
RICH23_SAMg3$p1inp2vec <- is.element(RICH23_SAMg3$player1, player2vector)
RICH23_SAMg3$p2inp1vec <- is.element(RICH23_SAMg3$player2, player1vector)

addPlayer1 <- RICH23_SAMg3[ which(RICH23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_SAMg3[ which(RICH23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_SAMg2 <- rbind(RICH23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
RICH23_SAMft <- ftable(RICH23_SAMg2$player1, RICH23_SAMg2$player2)
RICH23_SAMft2 <- as.matrix(RICH23_SAMft)
numRows <- nrow(RICH23_SAMft2)
numCols <- ncol(RICH23_SAMft2)
RICH23_SAMft3 <- RICH23_SAMft2[c(2:numRows) , c(2:numCols)]
RICH23_SAMTable <- graph.adjacency(RICH23_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(RICH23_SAMTable, vertex.label = V(RICH23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
RICH23_SAM.clusterCoef <- transitivity(RICH23_SAMTable, type="global") #cluster coefficient
RICH23_SAM.degreeCent <- centralization.degree(RICH23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_SAMftn <- as.network.matrix(RICH23_SAMft)
RICH23_SAM.netDensity <- network.density(RICH23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_SAM.entropy <- entropy(RICH23_SAMft) #entropy

RICH23_SAM.netMx <- cbind(RICH23_SAM.netMx, RICH23_SAM.clusterCoef, RICH23_SAM.degreeCent$centralization,
                          RICH23_SAM.netDensity, RICH23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
RICH23_TAMg2 <- data.frame(RICH23_TAM)
RICH23_TAMg2 <- RICH23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_TAMg2$player1
player2vector <- RICH23_TAMg2$player2
RICH23_TAMg3 <- RICH23_TAMg2
RICH23_TAMg3$p1inp2vec <- is.element(RICH23_TAMg3$player1, player2vector)
RICH23_TAMg3$p2inp1vec <- is.element(RICH23_TAMg3$player2, player1vector)

addPlayer1 <- RICH23_TAMg3[ which(RICH23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_TAMg3[ which(RICH23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_TAMg2 <- rbind(RICH23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
RICH23_TAMft <- ftable(RICH23_TAMg2$player1, RICH23_TAMg2$player2)
RICH23_TAMft2 <- as.matrix(RICH23_TAMft)
numRows <- nrow(RICH23_TAMft2)
numCols <- ncol(RICH23_TAMft2)
RICH23_TAMft3 <- RICH23_TAMft2[c(2:numRows) , c(2:numCols)]
RICH23_TAMTable <- graph.adjacency(RICH23_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(RICH23_TAMTable, vertex.label = V(RICH23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
RICH23_TAM.clusterCoef <- transitivity(RICH23_TAMTable, type="global") #cluster coefficient
RICH23_TAM.degreeCent <- centralization.degree(RICH23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_TAMftn <- as.network.matrix(RICH23_TAMft)
RICH23_TAM.netDensity <- network.density(RICH23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_TAM.entropy <- entropy(RICH23_TAMft) #entropy

RICH23_TAM.netMx <- cbind(RICH23_TAM.netMx, RICH23_TAM.clusterCoef, RICH23_TAM.degreeCent$centralization,
                          RICH23_TAM.netDensity, RICH23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
RICH23_SDMg2 <- data.frame(RICH23_SDM)
RICH23_SDMg2 <- RICH23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_SDMg2$player1
player2vector <- RICH23_SDMg2$player2
RICH23_SDMg3 <- RICH23_SDMg2
RICH23_SDMg3$p1inp2vec <- is.element(RICH23_SDMg3$player1, player2vector)
RICH23_SDMg3$p2inp1vec <- is.element(RICH23_SDMg3$player2, player1vector)

addPlayer1 <- RICH23_SDMg3[ which(RICH23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_SDMg3[ which(RICH23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_SDMg2 <- rbind(RICH23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
RICH23_SDMft <- ftable(RICH23_SDMg2$player1, RICH23_SDMg2$player2)
RICH23_SDMft2 <- as.matrix(RICH23_SDMft)
numRows <- nrow(RICH23_SDMft2)
numCols <- ncol(RICH23_SDMft2)
RICH23_SDMft3 <- RICH23_SDMft2[c(2:numRows) , c(2:numCols)]
RICH23_SDMTable <- graph.adjacency(RICH23_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(RICH23_SDMTable, vertex.label = V(RICH23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
RICH23_SDM.clusterCoef <- transitivity(RICH23_SDMTable, type="global") #cluster coefficient
RICH23_SDM.degreeCent <- centralization.degree(RICH23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_SDMftn <- as.network.matrix(RICH23_SDMft)
RICH23_SDM.netDensity <- network.density(RICH23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_SDM.entropy <- entropy(RICH23_SDMft) #entropy

RICH23_SDM.netMx <- cbind(RICH23_SDM.netMx, RICH23_SDM.clusterCoef, RICH23_SDM.degreeCent$centralization,
                          RICH23_SDM.netDensity, RICH23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
RICH23_TDMg2 <- data.frame(RICH23_TDM)
RICH23_TDMg2 <- RICH23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_TDMg2$player1
player2vector <- RICH23_TDMg2$player2
RICH23_TDMg3 <- RICH23_TDMg2
RICH23_TDMg3$p1inp2vec <- is.element(RICH23_TDMg3$player1, player2vector)
RICH23_TDMg3$p2inp1vec <- is.element(RICH23_TDMg3$player2, player1vector)

addPlayer1 <- RICH23_TDMg3[ which(RICH23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_TDMg3[ which(RICH23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_TDMg2 <- rbind(RICH23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
RICH23_TDMft <- ftable(RICH23_TDMg2$player1, RICH23_TDMg2$player2)
RICH23_TDMft2 <- as.matrix(RICH23_TDMft)
numRows <- nrow(RICH23_TDMft2)
numCols <- ncol(RICH23_TDMft2)
RICH23_TDMft3 <- RICH23_TDMft2[c(2:numRows) , c(2:numCols)]
RICH23_TDMTable <- graph.adjacency(RICH23_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(RICH23_TDMTable, vertex.label = V(RICH23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
RICH23_TDM.clusterCoef <- transitivity(RICH23_TDMTable, type="global") #cluster coefficient
RICH23_TDM.degreeCent <- centralization.degree(RICH23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_TDMftn <- as.network.matrix(RICH23_TDMft)
RICH23_TDM.netDensity <- network.density(RICH23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_TDM.entropy <- entropy(RICH23_TDMft) #entropy

RICH23_TDM.netMx <- cbind(RICH23_TDM.netMx, RICH23_TDM.clusterCoef, RICH23_TDM.degreeCent$centralization,
                          RICH23_TDM.netDensity, RICH23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
RICH23_SDg2 <- data.frame(RICH23_SD)
RICH23_SDg2 <- RICH23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_SDg2$player1
player2vector <- RICH23_SDg2$player2
RICH23_SDg3 <- RICH23_SDg2
RICH23_SDg3$p1inp2vec <- is.element(RICH23_SDg3$player1, player2vector)
RICH23_SDg3$p2inp1vec <- is.element(RICH23_SDg3$player2, player1vector)

addPlayer1 <- RICH23_SDg3[ which(RICH23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_SDg3[ which(RICH23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_SDg2 <- rbind(RICH23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
RICH23_SDft <- ftable(RICH23_SDg2$player1, RICH23_SDg2$player2)
RICH23_SDft2 <- as.matrix(RICH23_SDft)
numRows <- nrow(RICH23_SDft2)
numCols <- ncol(RICH23_SDft2)
RICH23_SDft3 <- RICH23_SDft2[c(2:numRows) , c(2:numCols)]
RICH23_SDTable <- graph.adjacency(RICH23_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(RICH23_SDTable, vertex.label = V(RICH23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
RICH23_SD.clusterCoef <- transitivity(RICH23_SDTable, type="global") #cluster coefficient
RICH23_SD.degreeCent <- centralization.degree(RICH23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_SDftn <- as.network.matrix(RICH23_SDft)
RICH23_SD.netDensity <- network.density(RICH23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_SD.entropy <- entropy(RICH23_SDft) #entropy

RICH23_SD.netMx <- cbind(RICH23_SD.netMx, RICH23_SD.clusterCoef, RICH23_SD.degreeCent$centralization,
                         RICH23_SD.netDensity, RICH23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
RICH23_TDg2 <- data.frame(RICH23_TD)
RICH23_TDg2 <- RICH23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_TDg2$player1
player2vector <- RICH23_TDg2$player2
RICH23_TDg3 <- RICH23_TDg2
RICH23_TDg3$p1inp2vec <- is.element(RICH23_TDg3$player1, player2vector)
RICH23_TDg3$p2inp1vec <- is.element(RICH23_TDg3$player2, player1vector)

addPlayer1 <- RICH23_TDg3[ which(RICH23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_TDg3[ which(RICH23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_TDg2 <- rbind(RICH23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
RICH23_TDft <- ftable(RICH23_TDg2$player1, RICH23_TDg2$player2)
RICH23_TDft2 <- as.matrix(RICH23_TDft)
numRows <- nrow(RICH23_TDft2)
numCols <- ncol(RICH23_TDft2)
RICH23_TDft3 <- RICH23_TDft2[c(2:numRows) , c(2:numCols)]
RICH23_TDTable <- graph.adjacency(RICH23_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(RICH23_TDTable, vertex.label = V(RICH23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
RICH23_TD.clusterCoef <- transitivity(RICH23_TDTable, type="global") #cluster coefficient
RICH23_TD.degreeCent <- centralization.degree(RICH23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_TDftn <- as.network.matrix(RICH23_TDft)
RICH23_TD.netDensity <- network.density(RICH23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_TD.entropy <- entropy(RICH23_TDft) #entropy

RICH23_TD.netMx <- cbind(RICH23_TD.netMx, RICH23_TD.clusterCoef, RICH23_TD.degreeCent$centralization,
                         RICH23_TD.netDensity, RICH23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
RICH23_QTg2 <- data.frame(RICH23_QT)
RICH23_QTg2 <- RICH23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH23_QTg2$player1
player2vector <- RICH23_QTg2$player2
RICH23_QTg3 <- RICH23_QTg2
RICH23_QTg3$p1inp2vec <- is.element(RICH23_QTg3$player1, player2vector)
RICH23_QTg3$p2inp1vec <- is.element(RICH23_QTg3$player2, player1vector)

addPlayer1 <- RICH23_QTg3[ which(RICH23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH23_QTg3[ which(RICH23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH23_QTg2 <- rbind(RICH23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
RICH23_QTft <- ftable(RICH23_QTg2$player1, RICH23_QTg2$player2)
RICH23_QTft2 <- as.matrix(RICH23_QTft)
numRows <- nrow(RICH23_QTft2)
numCols <- ncol(RICH23_QTft2)
RICH23_QTft3 <- RICH23_QTft2[c(2:numRows) , c(2:numCols)]
RICH23_QTTable <- graph.adjacency(RICH23_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(RICH23_QTTable, vertex.label = V(RICH23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
RICH23_QT.clusterCoef <- transitivity(RICH23_QTTable, type="global") #cluster coefficient
RICH23_QT.degreeCent <- centralization.degree(RICH23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH23_QTftn <- as.network.matrix(RICH23_QTft)
RICH23_QT.netDensity <- network.density(RICH23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH23_QT.entropy <- entropy(RICH23_QTft) #entropy

RICH23_QT.netMx <- cbind(RICH23_QT.netMx, RICH23_QT.clusterCoef, RICH23_QT.degreeCent$centralization,
                         RICH23_QT.netDensity, RICH23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH23_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "STK"
KIoutcome = "Goal_F"
STK23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
STK23_Gg2 <- data.frame(STK23_G)
STK23_Gg2 <- STK23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_Gg2$player1
player2vector <- STK23_Gg2$player2
STK23_Gg3 <- STK23_Gg2
STK23_Gg3$p1inp2vec <- is.element(STK23_Gg3$player1, player2vector)
STK23_Gg3$p2inp1vec <- is.element(STK23_Gg3$player2, player1vector)

addPlayer1 <- STK23_Gg3[ which(STK23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK23_Gg3[ which(STK23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_Gg2 <- rbind(STK23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
STK23_Gft <- ftable(STK23_Gg2$player1, STK23_Gg2$player2)
STK23_Gft2 <- as.matrix(STK23_Gft)
numRows <- nrow(STK23_Gft2)
numCols <- ncol(STK23_Gft2)
STK23_Gft3 <- STK23_Gft2[c(2:numRows) , c(2:numCols)]
STK23_GTable <- graph.adjacency(STK23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(STK23_GTable, vertex.label = V(STK23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
STK23_G.clusterCoef <- transitivity(STK23_GTable, type="global") #cluster coefficient
STK23_G.degreeCent <- centralization.degree(STK23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_Gftn <- as.network.matrix(STK23_Gft)
STK23_G.netDensity <- network.density(STK23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_G.entropy <- entropy(STK23_Gft) #entropy

STK23_G.netMx <- cbind(STK23_G.netMx, STK23_G.clusterCoef, STK23_G.degreeCent$centralization,
                       STK23_G.netDensity, STK23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "STK"
KIoutcome = "Behind_F"
STK23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
STK23_Bg2 <- data.frame(STK23_B)
STK23_Bg2 <- STK23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_Bg2$player1
player2vector <- STK23_Bg2$player2
STK23_Bg3 <- STK23_Bg2
STK23_Bg3$p1inp2vec <- is.element(STK23_Bg3$player1, player2vector)
STK23_Bg3$p2inp1vec <- is.element(STK23_Bg3$player2, player1vector)

addPlayer1 <- STK23_Bg3[ which(STK23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_Bg3[ which(STK23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_Bg2 <- rbind(STK23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
STK23_Bft <- ftable(STK23_Bg2$player1, STK23_Bg2$player2)
STK23_Bft2 <- as.matrix(STK23_Bft)
numRows <- nrow(STK23_Bft2)
numCols <- ncol(STK23_Bft2)
STK23_Bft3 <- STK23_Bft2[c(2:numRows) , c(2:numCols)]
STK23_BTable <- graph.adjacency(STK23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(STK23_BTable, vertex.label = V(STK23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
STK23_B.clusterCoef <- transitivity(STK23_BTable, type="global") #cluster coefficient
STK23_B.degreeCent <- centralization.degree(STK23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_Bftn <- as.network.matrix(STK23_Bft)
STK23_B.netDensity <- network.density(STK23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_B.entropy <- entropy(STK23_Bft) #entropy

STK23_B.netMx <- cbind(STK23_B.netMx, STK23_B.clusterCoef, STK23_B.degreeCent$centralization,
                       STK23_B.netDensity, STK23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "STK"
KIoutcome = "Stoppage_F"
STK23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
STK23_SFg2 <- data.frame(STK23_SF)
STK23_SFg2 <- STK23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_SFg2$player1
player2vector <- STK23_SFg2$player2
STK23_SFg3 <- STK23_SFg2
STK23_SFg3$p1inp2vec <- is.element(STK23_SFg3$player1, player2vector)
STK23_SFg3$p2inp1vec <- is.element(STK23_SFg3$player2, player1vector)

addPlayer1 <- STK23_SFg3[ which(STK23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_SFg3[ which(STK23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_SFg2 <- rbind(STK23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
STK23_SFft <- ftable(STK23_SFg2$player1, STK23_SFg2$player2)
STK23_SFft2 <- as.matrix(STK23_SFft)
numRows <- nrow(STK23_SFft2)
numCols <- ncol(STK23_SFft2)
STK23_SFft3 <- STK23_SFft2[c(2:numRows) , c(2:numCols)]
STK23_SFTable <- graph.adjacency(STK23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(STK23_SFTable, vertex.label = V(STK23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
STK23_SF.clusterCoef <- transitivity(STK23_SFTable, type="global") #cluster coefficient
STK23_SF.degreeCent <- centralization.degree(STK23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_SFftn <- as.network.matrix(STK23_SFft)
STK23_SF.netDensity <- network.density(STK23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_SF.entropy <- entropy(STK23_SFft) #entropy

STK23_SF.netMx <- cbind(STK23_SF.netMx, STK23_SF.clusterCoef, STK23_SF.degreeCent$centralization,
                        STK23_SF.netDensity, STK23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "STK"
KIoutcome = "Turnover_F"
STK23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
STK23_TFg2 <- data.frame(STK23_TF)
STK23_TFg2 <- STK23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_TFg2$player1
player2vector <- STK23_TFg2$player2
STK23_TFg3 <- STK23_TFg2
STK23_TFg3$p1inp2vec <- is.element(STK23_TFg3$player1, player2vector)
STK23_TFg3$p2inp1vec <- is.element(STK23_TFg3$player2, player1vector)

addPlayer1 <- STK23_TFg3[ which(STK23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_TFg3[ which(STK23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_TFg2 <- rbind(STK23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
STK23_TFft <- ftable(STK23_TFg2$player1, STK23_TFg2$player2)
STK23_TFft2 <- as.matrix(STK23_TFft)
numRows <- nrow(STK23_TFft2)
numCols <- ncol(STK23_TFft2)
STK23_TFft3 <- STK23_TFft2[c(2:numRows) , c(2:numCols)]
STK23_TFTable <- graph.adjacency(STK23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(STK23_TFTable, vertex.label = V(STK23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
STK23_TF.clusterCoef <- transitivity(STK23_TFTable, type="global") #cluster coefficient
STK23_TF.degreeCent <- centralization.degree(STK23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_TFftn <- as.network.matrix(STK23_TFft)
STK23_TF.netDensity <- network.density(STK23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_TF.entropy <- entropy(STK23_TFft) #entropy

STK23_TF.netMx <- cbind(STK23_TF.netMx, STK23_TF.clusterCoef, STK23_TF.degreeCent$centralization,
                        STK23_TF.netDensity, STK23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************

round = 23
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
STK23_SAMg2 <- data.frame(STK23_SAM)
STK23_SAMg2 <- STK23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_SAMg2$player1
player2vector <- STK23_SAMg2$player2
STK23_SAMg3 <- STK23_SAMg2
STK23_SAMg3$p1inp2vec <- is.element(STK23_SAMg3$player1, player2vector)
STK23_SAMg3$p2inp1vec <- is.element(STK23_SAMg3$player2, player1vector)

addPlayer1 <- STK23_SAMg3[ which(STK23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_SAMg3[ which(STK23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_SAMg2 <- rbind(STK23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
STK23_SAMft <- ftable(STK23_SAMg2$player1, STK23_SAMg2$player2)
STK23_SAMft2 <- as.matrix(STK23_SAMft)
numRows <- nrow(STK23_SAMft2)
numCols <- ncol(STK23_SAMft2)
STK23_SAMft3 <- STK23_SAMft2[c(2:numRows) , c(2:numCols)]
STK23_SAMTable <- graph.adjacency(STK23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(STK23_SAMTable, vertex.label = V(STK23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
STK23_SAM.clusterCoef <- transitivity(STK23_SAMTable, type="global") #cluster coefficient
STK23_SAM.degreeCent <- centralization.degree(STK23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_SAMftn <- as.network.matrix(STK23_SAMft)
STK23_SAM.netDensity <- network.density(STK23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_SAM.entropy <- entropy(STK23_SAMft) #entropy

STK23_SAM.netMx <- cbind(STK23_SAM.netMx, STK23_SAM.clusterCoef, STK23_SAM.degreeCent$centralization,
                         STK23_SAM.netDensity, STK23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "STK"
KIoutcome = "Turnover_AM"
STK23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
STK23_TAMg2 <- data.frame(STK23_TAM)
STK23_TAMg2 <- STK23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_TAMg2$player1
player2vector <- STK23_TAMg2$player2
STK23_TAMg3 <- STK23_TAMg2
STK23_TAMg3$p1inp2vec <- is.element(STK23_TAMg3$player1, player2vector)
STK23_TAMg3$p2inp1vec <- is.element(STK23_TAMg3$player2, player1vector)

addPlayer1 <- STK23_TAMg3[ which(STK23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK23_TAMg3[ which(STK23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_TAMg2 <- rbind(STK23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
STK23_TAMft <- ftable(STK23_TAMg2$player1, STK23_TAMg2$player2)
STK23_TAMft2 <- as.matrix(STK23_TAMft)
numRows <- nrow(STK23_TAMft2)
numCols <- ncol(STK23_TAMft2)
STK23_TAMft3 <- STK23_TAMft2[c(2:numRows) , c(2:numCols)]
STK23_TAMTable <- graph.adjacency(STK23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(STK23_TAMTable, vertex.label = V(STK23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
STK23_TAM.clusterCoef <- transitivity(STK23_TAMTable, type="global") #cluster coefficient
STK23_TAM.degreeCent <- centralization.degree(STK23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_TAMftn <- as.network.matrix(STK23_TAMft)
STK23_TAM.netDensity <- network.density(STK23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_TAM.entropy <- entropy(STK23_TAMft) #entropy

STK23_TAM.netMx <- cbind(STK23_TAM.netMx, STK23_TAM.clusterCoef, STK23_TAM.degreeCent$centralization,
                         STK23_TAM.netDensity, STK23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
STK23_SDMg2 <- data.frame(STK23_SDM)
STK23_SDMg2 <- STK23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_SDMg2$player1
player2vector <- STK23_SDMg2$player2
STK23_SDMg3 <- STK23_SDMg2
STK23_SDMg3$p1inp2vec <- is.element(STK23_SDMg3$player1, player2vector)
STK23_SDMg3$p2inp1vec <- is.element(STK23_SDMg3$player2, player1vector)

addPlayer1 <- STK23_SDMg3[ which(STK23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_SDMg3[ which(STK23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_SDMg2 <- rbind(STK23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
STK23_SDMft <- ftable(STK23_SDMg2$player1, STK23_SDMg2$player2)
STK23_SDMft2 <- as.matrix(STK23_SDMft)
numRows <- nrow(STK23_SDMft2)
numCols <- ncol(STK23_SDMft2)
STK23_SDMft3 <- STK23_SDMft2[c(2:numRows) , c(2:numCols)]
STK23_SDMTable <- graph.adjacency(STK23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(STK23_SDMTable, vertex.label = V(STK23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
STK23_SDM.clusterCoef <- transitivity(STK23_SDMTable, type="global") #cluster coefficient
STK23_SDM.degreeCent <- centralization.degree(STK23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_SDMftn <- as.network.matrix(STK23_SDMft)
STK23_SDM.netDensity <- network.density(STK23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_SDM.entropy <- entropy(STK23_SDMft) #entropy

STK23_SDM.netMx <- cbind(STK23_SDM.netMx, STK23_SDM.clusterCoef, STK23_SDM.degreeCent$centralization,
                         STK23_SDM.netDensity, STK23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "STK"
KIoutcome = "Turnover_DM"
STK23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
STK23_TDMg2 <- data.frame(STK23_TDM)
STK23_TDMg2 <- STK23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_TDMg2$player1
player2vector <- STK23_TDMg2$player2
STK23_TDMg3 <- STK23_TDMg2
STK23_TDMg3$p1inp2vec <- is.element(STK23_TDMg3$player1, player2vector)
STK23_TDMg3$p2inp1vec <- is.element(STK23_TDMg3$player2, player1vector)

addPlayer1 <- STK23_TDMg3[ which(STK23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_TDMg3[ which(STK23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_TDMg2 <- rbind(STK23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
STK23_TDMft <- ftable(STK23_TDMg2$player1, STK23_TDMg2$player2)
STK23_TDMft2 <- as.matrix(STK23_TDMft)
numRows <- nrow(STK23_TDMft2)
numCols <- ncol(STK23_TDMft2)
STK23_TDMft3 <- STK23_TDMft2[c(2:numRows) , c(2:numCols)]
STK23_TDMTable <- graph.adjacency(STK23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(STK23_TDMTable, vertex.label = V(STK23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
STK23_TDM.clusterCoef <- transitivity(STK23_TDMTable, type="global") #cluster coefficient
STK23_TDM.degreeCent <- centralization.degree(STK23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_TDMftn <- as.network.matrix(STK23_TDMft)
STK23_TDM.netDensity <- network.density(STK23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_TDM.entropy <- entropy(STK23_TDMft) #entropy

STK23_TDM.netMx <- cbind(STK23_TDM.netMx, STK23_TDM.clusterCoef, STK23_TDM.degreeCent$centralization,
                         STK23_TDM.netDensity, STK23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "STK"
KIoutcome = "Stoppage_D"
STK23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
STK23_SDg2 <- data.frame(STK23_SD)
STK23_SDg2 <- STK23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_SDg2$player1
player2vector <- STK23_SDg2$player2
STK23_SDg3 <- STK23_SDg2
STK23_SDg3$p1inp2vec <- is.element(STK23_SDg3$player1, player2vector)
STK23_SDg3$p2inp1vec <- is.element(STK23_SDg3$player2, player1vector)

addPlayer1 <- STK23_SDg3[ which(STK23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_SDg3[ which(STK23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_SDg2 <- rbind(STK23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
STK23_SDft <- ftable(STK23_SDg2$player1, STK23_SDg2$player2)
STK23_SDft2 <- as.matrix(STK23_SDft)
numRows <- nrow(STK23_SDft2)
numCols <- ncol(STK23_SDft2)
STK23_SDft3 <- STK23_SDft2[c(2:numRows) , c(2:numCols)]
STK23_SDTable <- graph.adjacency(STK23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(STK23_SDTable, vertex.label = V(STK23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
STK23_SD.clusterCoef <- transitivity(STK23_SDTable, type="global") #cluster coefficient
STK23_SD.degreeCent <- centralization.degree(STK23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_SDftn <- as.network.matrix(STK23_SDft)
STK23_SD.netDensity <- network.density(STK23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_SD.entropy <- entropy(STK23_SDft) #entropy

STK23_SD.netMx <- cbind(STK23_SD.netMx, STK23_SD.clusterCoef, STK23_SD.degreeCent$centralization,
                        STK23_SD.netDensity, STK23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "STK"
KIoutcome = "Turnover_D"
STK23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
STK23_TDg2 <- data.frame(STK23_TD)
STK23_TDg2 <- STK23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_TDg2$player1
player2vector <- STK23_TDg2$player2
STK23_TDg3 <- STK23_TDg2
STK23_TDg3$p1inp2vec <- is.element(STK23_TDg3$player1, player2vector)
STK23_TDg3$p2inp1vec <- is.element(STK23_TDg3$player2, player1vector)

addPlayer1 <- STK23_TDg3[ which(STK23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_TDg3[ which(STK23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_TDg2 <- rbind(STK23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
STK23_TDft <- ftable(STK23_TDg2$player1, STK23_TDg2$player2)
STK23_TDft2 <- as.matrix(STK23_TDft)
numRows <- nrow(STK23_TDft2)
numCols <- ncol(STK23_TDft2)
STK23_TDft3 <- STK23_TDft2[c(2:numRows) , c(2:numCols)]
STK23_TDTable <- graph.adjacency(STK23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(STK23_TDTable, vertex.label = V(STK23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
STK23_TD.clusterCoef <- transitivity(STK23_TDTable, type="global") #cluster coefficient
STK23_TD.degreeCent <- centralization.degree(STK23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_TDftn <- as.network.matrix(STK23_TDft)
STK23_TD.netDensity <- network.density(STK23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_TD.entropy <- entropy(STK23_TDft) #entropy

STK23_TD.netMx <- cbind(STK23_TD.netMx, STK23_TD.clusterCoef, STK23_TD.degreeCent$centralization,
                        STK23_TD.netDensity, STK23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
STK23_QTg2 <- data.frame(STK23_QT)
STK23_QTg2 <- STK23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK23_QTg2$player1
player2vector <- STK23_QTg2$player2
STK23_QTg3 <- STK23_QTg2
STK23_QTg3$p1inp2vec <- is.element(STK23_QTg3$player1, player2vector)
STK23_QTg3$p2inp1vec <- is.element(STK23_QTg3$player2, player1vector)

addPlayer1 <- STK23_QTg3[ which(STK23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK23_QTg3[ which(STK23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK23_QTg2 <- rbind(STK23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
STK23_QTft <- ftable(STK23_QTg2$player1, STK23_QTg2$player2)
STK23_QTft2 <- as.matrix(STK23_QTft)
numRows <- nrow(STK23_QTft2)
numCols <- ncol(STK23_QTft2)
STK23_QTft3 <- STK23_QTft2[c(2:numRows) , c(2:numCols)]
STK23_QTTable <- graph.adjacency(STK23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(STK23_QTTable, vertex.label = V(STK23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
STK23_QT.clusterCoef <- transitivity(STK23_QTTable, type="global") #cluster coefficient
STK23_QT.degreeCent <- centralization.degree(STK23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK23_QTftn <- as.network.matrix(STK23_QTft)
STK23_QT.netDensity <- network.density(STK23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK23_QT.entropy <- entropy(STK23_QTft) #entropy

STK23_QT.netMx <- cbind(STK23_QT.netMx, STK23_QT.clusterCoef, STK23_QT.degreeCent$centralization,
                        STK23_QT.netDensity, STK23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK23_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************

round = 23
teamName = "SYD"
KIoutcome = "Goal_F"
SYD23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
SYD23_Gg2 <- data.frame(SYD23_G)
SYD23_Gg2 <- SYD23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_Gg2$player1
player2vector <- SYD23_Gg2$player2
SYD23_Gg3 <- SYD23_Gg2
SYD23_Gg3$p1inp2vec <- is.element(SYD23_Gg3$player1, player2vector)
SYD23_Gg3$p2inp1vec <- is.element(SYD23_Gg3$player2, player1vector)

addPlayer1 <- SYD23_Gg3[ which(SYD23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_Gg3[ which(SYD23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_Gg2 <- rbind(SYD23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
SYD23_Gft <- ftable(SYD23_Gg2$player1, SYD23_Gg2$player2)
SYD23_Gft2 <- as.matrix(SYD23_Gft)
numRows <- nrow(SYD23_Gft2)
numCols <- ncol(SYD23_Gft2)
SYD23_Gft3 <- SYD23_Gft2[c(2:numRows) , c(2:numCols)]
SYD23_GTable <- graph.adjacency(SYD23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(SYD23_GTable, vertex.label = V(SYD23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
SYD23_G.clusterCoef <- transitivity(SYD23_GTable, type="global") #cluster coefficient
SYD23_G.degreeCent <- centralization.degree(SYD23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_Gftn <- as.network.matrix(SYD23_Gft)
SYD23_G.netDensity <- network.density(SYD23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_G.entropy <- entropy(SYD23_Gft) #entropy

SYD23_G.netMx <- cbind(SYD23_G.netMx, SYD23_G.clusterCoef, SYD23_G.degreeCent$centralization,
                       SYD23_G.netDensity, SYD23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************

round = 23
teamName = "SYD"
KIoutcome = "Behind_F"
SYD23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
SYD23_Bg2 <- data.frame(SYD23_B)
SYD23_Bg2 <- SYD23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_Bg2$player1
player2vector <- SYD23_Bg2$player2
SYD23_Bg3 <- SYD23_Bg2
SYD23_Bg3$p1inp2vec <- is.element(SYD23_Bg3$player1, player2vector)
SYD23_Bg3$p2inp1vec <- is.element(SYD23_Bg3$player2, player1vector)

addPlayer1 <- SYD23_Bg3[ which(SYD23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_Bg3[ which(SYD23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_Bg2 <- rbind(SYD23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
SYD23_Bft <- ftable(SYD23_Bg2$player1, SYD23_Bg2$player2)
SYD23_Bft2 <- as.matrix(SYD23_Bft)
numRows <- nrow(SYD23_Bft2)
numCols <- ncol(SYD23_Bft2)
SYD23_Bft3 <- SYD23_Bft2[c(2:numRows) , c(2:numCols)]
SYD23_BTable <- graph.adjacency(SYD23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(SYD23_BTable, vertex.label = V(SYD23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
SYD23_B.clusterCoef <- transitivity(SYD23_BTable, type="global") #cluster coefficient
SYD23_B.degreeCent <- centralization.degree(SYD23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_Bftn <- as.network.matrix(SYD23_Bft)
SYD23_B.netDensity <- network.density(SYD23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_B.entropy <- entropy(SYD23_Bft) #entropy

SYD23_B.netMx <- cbind(SYD23_B.netMx, SYD23_B.clusterCoef, SYD23_B.degreeCent$centralization,
                       SYD23_B.netDensity, SYD23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
SYD23_SFg2 <- data.frame(SYD23_SF)
SYD23_SFg2 <- SYD23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_SFg2$player1
player2vector <- SYD23_SFg2$player2
SYD23_SFg3 <- SYD23_SFg2
SYD23_SFg3$p1inp2vec <- is.element(SYD23_SFg3$player1, player2vector)
SYD23_SFg3$p2inp1vec <- is.element(SYD23_SFg3$player2, player1vector)

addPlayer1 <- SYD23_SFg3[ which(SYD23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_SFg3[ which(SYD23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_SFg2 <- rbind(SYD23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
SYD23_SFft <- ftable(SYD23_SFg2$player1, SYD23_SFg2$player2)
SYD23_SFft2 <- as.matrix(SYD23_SFft)
numRows <- nrow(SYD23_SFft2)
numCols <- ncol(SYD23_SFft2)
SYD23_SFft3 <- SYD23_SFft2[c(2:numRows) , c(2:numCols)]
SYD23_SFTable <- graph.adjacency(SYD23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(SYD23_SFTable, vertex.label = V(SYD23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
SYD23_SF.clusterCoef <- transitivity(SYD23_SFTable, type="global") #cluster coefficient
SYD23_SF.degreeCent <- centralization.degree(SYD23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_SFftn <- as.network.matrix(SYD23_SFft)
SYD23_SF.netDensity <- network.density(SYD23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_SF.entropy <- entropy(SYD23_SFft) #entropy

SYD23_SF.netMx <- cbind(SYD23_SF.netMx, SYD23_SF.clusterCoef, SYD23_SF.degreeCent$centralization,
                        SYD23_SF.netDensity, SYD23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
SYD23_TFg2 <- data.frame(SYD23_TF)
SYD23_TFg2 <- SYD23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_TFg2$player1
player2vector <- SYD23_TFg2$player2
SYD23_TFg3 <- SYD23_TFg2
SYD23_TFg3$p1inp2vec <- is.element(SYD23_TFg3$player1, player2vector)
SYD23_TFg3$p2inp1vec <- is.element(SYD23_TFg3$player2, player1vector)

addPlayer1 <- SYD23_TFg3[ which(SYD23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_TFg3[ which(SYD23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_TFg2 <- rbind(SYD23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
SYD23_TFft <- ftable(SYD23_TFg2$player1, SYD23_TFg2$player2)
SYD23_TFft2 <- as.matrix(SYD23_TFft)
numRows <- nrow(SYD23_TFft2)
numCols <- ncol(SYD23_TFft2)
SYD23_TFft3 <- SYD23_TFft2[c(2:numRows) , c(2:numCols)]
SYD23_TFTable <- graph.adjacency(SYD23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(SYD23_TFTable, vertex.label = V(SYD23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
SYD23_TF.clusterCoef <- transitivity(SYD23_TFTable, type="global") #cluster coefficient
SYD23_TF.degreeCent <- centralization.degree(SYD23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_TFftn <- as.network.matrix(SYD23_TFft)
SYD23_TF.netDensity <- network.density(SYD23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_TF.entropy <- entropy(SYD23_TFft) #entropy

SYD23_TF.netMx <- cbind(SYD23_TF.netMx, SYD23_TF.clusterCoef, SYD23_TF.degreeCent$centralization,
                        SYD23_TF.netDensity, SYD23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
SYD23_SAMg2 <- data.frame(SYD23_SAM)
SYD23_SAMg2 <- SYD23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_SAMg2$player1
player2vector <- SYD23_SAMg2$player2
SYD23_SAMg3 <- SYD23_SAMg2
SYD23_SAMg3$p1inp2vec <- is.element(SYD23_SAMg3$player1, player2vector)
SYD23_SAMg3$p2inp1vec <- is.element(SYD23_SAMg3$player2, player1vector)

addPlayer1 <- SYD23_SAMg3[ which(SYD23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_SAMg3[ which(SYD23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_SAMg2 <- rbind(SYD23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
SYD23_SAMft <- ftable(SYD23_SAMg2$player1, SYD23_SAMg2$player2)
SYD23_SAMft2 <- as.matrix(SYD23_SAMft)
numRows <- nrow(SYD23_SAMft2)
numCols <- ncol(SYD23_SAMft2)
SYD23_SAMft3 <- SYD23_SAMft2[c(2:numRows) , c(2:numCols)]
SYD23_SAMTable <- graph.adjacency(SYD23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(SYD23_SAMTable, vertex.label = V(SYD23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
SYD23_SAM.clusterCoef <- transitivity(SYD23_SAMTable, type="global") #cluster coefficient
SYD23_SAM.degreeCent <- centralization.degree(SYD23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_SAMftn <- as.network.matrix(SYD23_SAMft)
SYD23_SAM.netDensity <- network.density(SYD23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_SAM.entropy <- entropy(SYD23_SAMft) #entropy

SYD23_SAM.netMx <- cbind(SYD23_SAM.netMx, SYD23_SAM.clusterCoef, SYD23_SAM.degreeCent$centralization,
                         SYD23_SAM.netDensity, SYD23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
SYD23_TAMg2 <- data.frame(SYD23_TAM)
SYD23_TAMg2 <- SYD23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_TAMg2$player1
player2vector <- SYD23_TAMg2$player2
SYD23_TAMg3 <- SYD23_TAMg2
SYD23_TAMg3$p1inp2vec <- is.element(SYD23_TAMg3$player1, player2vector)
SYD23_TAMg3$p2inp1vec <- is.element(SYD23_TAMg3$player2, player1vector)

addPlayer1 <- SYD23_TAMg3[ which(SYD23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_TAMg3[ which(SYD23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_TAMg2 <- rbind(SYD23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
SYD23_TAMft <- ftable(SYD23_TAMg2$player1, SYD23_TAMg2$player2)
SYD23_TAMft2 <- as.matrix(SYD23_TAMft)
numRows <- nrow(SYD23_TAMft2)
numCols <- ncol(SYD23_TAMft2)
SYD23_TAMft3 <- SYD23_TAMft2[c(2:numRows) , c(2:numCols)]
SYD23_TAMTable <- graph.adjacency(SYD23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(SYD23_TAMTable, vertex.label = V(SYD23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
SYD23_TAM.clusterCoef <- transitivity(SYD23_TAMTable, type="global") #cluster coefficient
SYD23_TAM.degreeCent <- centralization.degree(SYD23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_TAMftn <- as.network.matrix(SYD23_TAMft)
SYD23_TAM.netDensity <- network.density(SYD23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_TAM.entropy <- entropy(SYD23_TAMft) #entropy

SYD23_TAM.netMx <- cbind(SYD23_TAM.netMx, SYD23_TAM.clusterCoef, SYD23_TAM.degreeCent$centralization,
                         SYD23_TAM.netDensity, SYD23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
SYD23_SDMg2 <- data.frame(SYD23_SDM)
SYD23_SDMg2 <- SYD23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_SDMg2$player1
player2vector <- SYD23_SDMg2$player2
SYD23_SDMg3 <- SYD23_SDMg2
SYD23_SDMg3$p1inp2vec <- is.element(SYD23_SDMg3$player1, player2vector)
SYD23_SDMg3$p2inp1vec <- is.element(SYD23_SDMg3$player2, player1vector)

addPlayer1 <- SYD23_SDMg3[ which(SYD23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_SDMg3[ which(SYD23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_SDMg2 <- rbind(SYD23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
SYD23_SDMft <- ftable(SYD23_SDMg2$player1, SYD23_SDMg2$player2)
SYD23_SDMft2 <- as.matrix(SYD23_SDMft)
numRows <- nrow(SYD23_SDMft2)
numCols <- ncol(SYD23_SDMft2)
SYD23_SDMft3 <- SYD23_SDMft2[c(2:numRows) , c(2:numCols)]
SYD23_SDMTable <- graph.adjacency(SYD23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(SYD23_SDMTable, vertex.label = V(SYD23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
SYD23_SDM.clusterCoef <- transitivity(SYD23_SDMTable, type="global") #cluster coefficient
SYD23_SDM.degreeCent <- centralization.degree(SYD23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_SDMftn <- as.network.matrix(SYD23_SDMft)
SYD23_SDM.netDensity <- network.density(SYD23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_SDM.entropy <- entropy(SYD23_SDMft) #entropy

SYD23_SDM.netMx <- cbind(SYD23_SDM.netMx, SYD23_SDM.clusterCoef, SYD23_SDM.degreeCent$centralization,
                         SYD23_SDM.netDensity, SYD23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
SYD23_TDMg2 <- data.frame(SYD23_TDM)
SYD23_TDMg2 <- SYD23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_TDMg2$player1
player2vector <- SYD23_TDMg2$player2
SYD23_TDMg3 <- SYD23_TDMg2
SYD23_TDMg3$p1inp2vec <- is.element(SYD23_TDMg3$player1, player2vector)
SYD23_TDMg3$p2inp1vec <- is.element(SYD23_TDMg3$player2, player1vector)

addPlayer1 <- SYD23_TDMg3[ which(SYD23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_TDMg3[ which(SYD23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_TDMg2 <- rbind(SYD23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
SYD23_TDMft <- ftable(SYD23_TDMg2$player1, SYD23_TDMg2$player2)
SYD23_TDMft2 <- as.matrix(SYD23_TDMft)
numRows <- nrow(SYD23_TDMft2)
numCols <- ncol(SYD23_TDMft2)
SYD23_TDMft3 <- SYD23_TDMft2[c(2:numRows) , c(2:numCols)]
SYD23_TDMTable <- graph.adjacency(SYD23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(SYD23_TDMTable, vertex.label = V(SYD23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
SYD23_TDM.clusterCoef <- transitivity(SYD23_TDMTable, type="global") #cluster coefficient
SYD23_TDM.degreeCent <- centralization.degree(SYD23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_TDMftn <- as.network.matrix(SYD23_TDMft)
SYD23_TDM.netDensity <- network.density(SYD23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_TDM.entropy <- entropy(SYD23_TDMft) #entropy

SYD23_TDM.netMx <- cbind(SYD23_TDM.netMx, SYD23_TDM.clusterCoef, SYD23_TDM.degreeCent$centralization,
                         SYD23_TDM.netDensity, SYD23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
SYD23_SDg2 <- data.frame(SYD23_SD)
SYD23_SDg2 <- SYD23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_SDg2$player1
player2vector <- SYD23_SDg2$player2
SYD23_SDg3 <- SYD23_SDg2
SYD23_SDg3$p1inp2vec <- is.element(SYD23_SDg3$player1, player2vector)
SYD23_SDg3$p2inp1vec <- is.element(SYD23_SDg3$player2, player1vector)

addPlayer1 <- SYD23_SDg3[ which(SYD23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_SDg3[ which(SYD23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_SDg2 <- rbind(SYD23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
SYD23_SDft <- ftable(SYD23_SDg2$player1, SYD23_SDg2$player2)
SYD23_SDft2 <- as.matrix(SYD23_SDft)
numRows <- nrow(SYD23_SDft2)
numCols <- ncol(SYD23_SDft2)
SYD23_SDft3 <- SYD23_SDft2[c(2:numRows) , c(2:numCols)]
SYD23_SDTable <- graph.adjacency(SYD23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(SYD23_SDTable, vertex.label = V(SYD23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
SYD23_SD.clusterCoef <- transitivity(SYD23_SDTable, type="global") #cluster coefficient
SYD23_SD.degreeCent <- centralization.degree(SYD23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_SDftn <- as.network.matrix(SYD23_SDft)
SYD23_SD.netDensity <- network.density(SYD23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_SD.entropy <- entropy(SYD23_SDft) #entropy

SYD23_SD.netMx <- cbind(SYD23_SD.netMx, SYD23_SD.clusterCoef, SYD23_SD.degreeCent$centralization,
                        SYD23_SD.netDensity, SYD23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************

round = 23
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
SYD23_TDg2 <- data.frame(SYD23_TD)
SYD23_TDg2 <- SYD23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_TDg2$player1
player2vector <- SYD23_TDg2$player2
SYD23_TDg3 <- SYD23_TDg2
SYD23_TDg3$p1inp2vec <- is.element(SYD23_TDg3$player1, player2vector)
SYD23_TDg3$p2inp1vec <- is.element(SYD23_TDg3$player2, player1vector)

addPlayer1 <- SYD23_TDg3[ which(SYD23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_TDg3[ which(SYD23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_TDg2 <- rbind(SYD23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
SYD23_TDft <- ftable(SYD23_TDg2$player1, SYD23_TDg2$player2)
SYD23_TDft2 <- as.matrix(SYD23_TDft)
numRows <- nrow(SYD23_TDft2)
numCols <- ncol(SYD23_TDft2)
SYD23_TDft3 <- SYD23_TDft2[c(2:numRows) , c(2:numCols)]
SYD23_TDTable <- graph.adjacency(SYD23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(SYD23_TDTable, vertex.label = V(SYD23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
SYD23_TD.clusterCoef <- transitivity(SYD23_TDTable, type="global") #cluster coefficient
SYD23_TD.degreeCent <- centralization.degree(SYD23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_TDftn <- as.network.matrix(SYD23_TDft)
SYD23_TD.netDensity <- network.density(SYD23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_TD.entropy <- entropy(SYD23_TDft) #entropy

SYD23_TD.netMx <- cbind(SYD23_TD.netMx, SYD23_TD.clusterCoef, SYD23_TD.degreeCent$centralization,
                        SYD23_TD.netDensity, SYD23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
SYD23_QTg2 <- data.frame(SYD23_QT)
SYD23_QTg2 <- SYD23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD23_QTg2$player1
player2vector <- SYD23_QTg2$player2
SYD23_QTg3 <- SYD23_QTg2
SYD23_QTg3$p1inp2vec <- is.element(SYD23_QTg3$player1, player2vector)
SYD23_QTg3$p2inp1vec <- is.element(SYD23_QTg3$player2, player1vector)

addPlayer1 <- SYD23_QTg3[ which(SYD23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD23_QTg3[ which(SYD23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD23_QTg2 <- rbind(SYD23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
SYD23_QTft <- ftable(SYD23_QTg2$player1, SYD23_QTg2$player2)
SYD23_QTft2 <- as.matrix(SYD23_QTft)
numRows <- nrow(SYD23_QTft2)
numCols <- ncol(SYD23_QTft2)
SYD23_QTft3 <- SYD23_QTft2[c(2:numRows) , c(2:numCols)]
SYD23_QTTable <- graph.adjacency(SYD23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(SYD23_QTTable, vertex.label = V(SYD23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
SYD23_QT.clusterCoef <- transitivity(SYD23_QTTable, type="global") #cluster coefficient
SYD23_QT.degreeCent <- centralization.degree(SYD23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD23_QTftn <- as.network.matrix(SYD23_QTft)
SYD23_QT.netDensity <- network.density(SYD23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD23_QT.entropy <- entropy(SYD23_QTft) #entropy

SYD23_QT.netMx <- cbind(SYD23_QT.netMx, SYD23_QT.clusterCoef, SYD23_QT.degreeCent$centralization,
                        SYD23_QT.netDensity, SYD23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD23_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "Goal_F"
WB23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
WB23_Gg2 <- data.frame(WB23_G)
WB23_Gg2 <- WB23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_Gg2$player1
player2vector <- WB23_Gg2$player2
WB23_Gg3 <- WB23_Gg2
WB23_Gg3$p1inp2vec <- is.element(WB23_Gg3$player1, player2vector)
WB23_Gg3$p2inp1vec <- is.element(WB23_Gg3$player2, player1vector)

addPlayer1 <- WB23_Gg3[ which(WB23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_Gg3[ which(WB23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_Gg2 <- rbind(WB23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
WB23_Gft <- ftable(WB23_Gg2$player1, WB23_Gg2$player2)
WB23_Gft2 <- as.matrix(WB23_Gft)
numRows <- nrow(WB23_Gft2)
numCols <- ncol(WB23_Gft2)
WB23_Gft3 <- WB23_Gft2[c(2:numRows) , c(2:numCols)]
WB23_GTable <- graph.adjacency(WB23_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(WB23_GTable, vertex.label = V(WB23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
WB23_G.clusterCoef <- transitivity(WB23_GTable, type="global") #cluster coefficient
WB23_G.degreeCent <- centralization.degree(WB23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_Gftn <- as.network.matrix(WB23_Gft)
WB23_G.netDensity <- network.density(WB23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_G.entropy <- entropy(WB23_Gft) #entropy

WB23_G.netMx <- cbind(WB23_G.netMx, WB23_G.clusterCoef, WB23_G.degreeCent$centralization,
                      WB23_G.netDensity, WB23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "Behind_F"
WB23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
WB23_Bg2 <- data.frame(WB23_B)
WB23_Bg2 <- WB23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_Bg2$player1
player2vector <- WB23_Bg2$player2
WB23_Bg3 <- WB23_Bg2
WB23_Bg3$p1inp2vec <- is.element(WB23_Bg3$player1, player2vector)
WB23_Bg3$p2inp1vec <- is.element(WB23_Bg3$player2, player1vector)

addPlayer1 <- WB23_Bg3[ which(WB23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_Bg3[ which(WB23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_Bg2 <- rbind(WB23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
WB23_Bft <- ftable(WB23_Bg2$player1, WB23_Bg2$player2)
WB23_Bft2 <- as.matrix(WB23_Bft)
numRows <- nrow(WB23_Bft2)
numCols <- ncol(WB23_Bft2)
WB23_Bft3 <- WB23_Bft2[c(2:numRows) , c(2:numCols)]
WB23_BTable <- graph.adjacency(WB23_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(WB23_BTable, vertex.label = V(WB23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
WB23_B.clusterCoef <- transitivity(WB23_BTable, type="global") #cluster coefficient
WB23_B.degreeCent <- centralization.degree(WB23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_Bftn <- as.network.matrix(WB23_Bft)
WB23_B.netDensity <- network.density(WB23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_B.entropy <- entropy(WB23_Bft) #entropy

WB23_B.netMx <- cbind(WB23_B.netMx, WB23_B.clusterCoef, WB23_B.degreeCent$centralization,
                      WB23_B.netDensity, WB23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "Stoppage_F"
WB23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
WB23_SFg2 <- data.frame(WB23_SF)
WB23_SFg2 <- WB23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_SFg2$player1
player2vector <- WB23_SFg2$player2
WB23_SFg3 <- WB23_SFg2
WB23_SFg3$p1inp2vec <- is.element(WB23_SFg3$player1, player2vector)
WB23_SFg3$p2inp1vec <- is.element(WB23_SFg3$player2, player1vector)

addPlayer1 <- WB23_SFg3[ which(WB23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_SFg3[ which(WB23_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_SFg2 <- rbind(WB23_SFg2, addPlayers)

#ROUND 23, FWD Stoppage graph using weighted edges
WB23_SFft <- ftable(WB23_SFg2$player1, WB23_SFg2$player2)
WB23_SFft2 <- as.matrix(WB23_SFft)
numRows <- nrow(WB23_SFft2)
numCols <- ncol(WB23_SFft2)
WB23_SFft3 <- WB23_SFft2[c(2:numRows) , c(2:numCols)]
WB23_SFTable <- graph.adjacency(WB23_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(WB23_SFTable, vertex.label = V(WB23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
WB23_SF.clusterCoef <- transitivity(WB23_SFTable, type="global") #cluster coefficient
WB23_SF.degreeCent <- centralization.degree(WB23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_SFftn <- as.network.matrix(WB23_SFft)
WB23_SF.netDensity <- network.density(WB23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_SF.entropy <- entropy(WB23_SFft) #entropy

WB23_SF.netMx <- cbind(WB23_SF.netMx, WB23_SF.clusterCoef, WB23_SF.degreeCent$centralization,
                       WB23_SF.netDensity, WB23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "WB"
KIoutcome = "Turnover_F"
WB23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
WB23_TFg2 <- data.frame(WB23_TF)
WB23_TFg2 <- WB23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_TFg2$player1
player2vector <- WB23_TFg2$player2
WB23_TFg3 <- WB23_TFg2
WB23_TFg3$p1inp2vec <- is.element(WB23_TFg3$player1, player2vector)
WB23_TFg3$p2inp1vec <- is.element(WB23_TFg3$player2, player1vector)

addPlayer1 <- WB23_TFg3[ which(WB23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_TFg3[ which(WB23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_TFg2 <- rbind(WB23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
WB23_TFft <- ftable(WB23_TFg2$player1, WB23_TFg2$player2)
WB23_TFft2 <- as.matrix(WB23_TFft)
numRows <- nrow(WB23_TFft2)
numCols <- ncol(WB23_TFft2)
WB23_TFft3 <- WB23_TFft2[c(2:numRows) , c(2:numCols)]
WB23_TFTable <- graph.adjacency(WB23_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(WB23_TFTable, vertex.label = V(WB23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
WB23_TF.clusterCoef <- transitivity(WB23_TFTable, type="global") #cluster coefficient
WB23_TF.degreeCent <- centralization.degree(WB23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_TFftn <- as.network.matrix(WB23_TFft)
WB23_TF.netDensity <- network.density(WB23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_TF.entropy <- entropy(WB23_TFft) #entropy

WB23_TF.netMx <- cbind(WB23_TF.netMx, WB23_TF.clusterCoef, WB23_TF.degreeCent$centralization,
                       WB23_TF.netDensity, WB23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
WB23_SAMg2 <- data.frame(WB23_SAM)
WB23_SAMg2 <- WB23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_SAMg2$player1
player2vector <- WB23_SAMg2$player2
WB23_SAMg3 <- WB23_SAMg2
WB23_SAMg3$p1inp2vec <- is.element(WB23_SAMg3$player1, player2vector)
WB23_SAMg3$p2inp1vec <- is.element(WB23_SAMg3$player2, player1vector)

addPlayer1 <- WB23_SAMg3[ which(WB23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_SAMg3[ which(WB23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_SAMg2 <- rbind(WB23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
WB23_SAMft <- ftable(WB23_SAMg2$player1, WB23_SAMg2$player2)
WB23_SAMft2 <- as.matrix(WB23_SAMft)
numRows <- nrow(WB23_SAMft2)
numCols <- ncol(WB23_SAMft2)
WB23_SAMft3 <- WB23_SAMft2[c(2:numRows) , c(2:numCols)]
WB23_SAMTable <- graph.adjacency(WB23_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(WB23_SAMTable, vertex.label = V(WB23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
WB23_SAM.clusterCoef <- transitivity(WB23_SAMTable, type="global") #cluster coefficient
WB23_SAM.degreeCent <- centralization.degree(WB23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_SAMftn <- as.network.matrix(WB23_SAMft)
WB23_SAM.netDensity <- network.density(WB23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_SAM.entropy <- entropy(WB23_SAMft) #entropy

WB23_SAM.netMx <- cbind(WB23_SAM.netMx, WB23_SAM.clusterCoef, WB23_SAM.degreeCent$centralization,
                        WB23_SAM.netDensity, WB23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "WB"
KIoutcome = "Turnover_AM"
WB23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
WB23_TAMg2 <- data.frame(WB23_TAM)
WB23_TAMg2 <- WB23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_TAMg2$player1
player2vector <- WB23_TAMg2$player2
WB23_TAMg3 <- WB23_TAMg2
WB23_TAMg3$p1inp2vec <- is.element(WB23_TAMg3$player1, player2vector)
WB23_TAMg3$p2inp1vec <- is.element(WB23_TAMg3$player2, player1vector)

addPlayer1 <- WB23_TAMg3[ which(WB23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_TAMg3[ which(WB23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_TAMg2 <- rbind(WB23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
WB23_TAMft <- ftable(WB23_TAMg2$player1, WB23_TAMg2$player2)
WB23_TAMft2 <- as.matrix(WB23_TAMft)
numRows <- nrow(WB23_TAMft2)
numCols <- ncol(WB23_TAMft2)
WB23_TAMft3 <- WB23_TAMft2[c(2:numRows) , c(2:numCols)]
WB23_TAMTable <- graph.adjacency(WB23_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(WB23_TAMTable, vertex.label = V(WB23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
WB23_TAM.clusterCoef <- transitivity(WB23_TAMTable, type="global") #cluster coefficient
WB23_TAM.degreeCent <- centralization.degree(WB23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_TAMftn <- as.network.matrix(WB23_TAMft)
WB23_TAM.netDensity <- network.density(WB23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_TAM.entropy <- entropy(WB23_TAMft) #entropy

WB23_TAM.netMx <- cbind(WB23_TAM.netMx, WB23_TAM.clusterCoef, WB23_TAM.degreeCent$centralization,
                        WB23_TAM.netDensity, WB23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
WB23_SDMg2 <- data.frame(WB23_SDM)
WB23_SDMg2 <- WB23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_SDMg2$player1
player2vector <- WB23_SDMg2$player2
WB23_SDMg3 <- WB23_SDMg2
WB23_SDMg3$p1inp2vec <- is.element(WB23_SDMg3$player1, player2vector)
WB23_SDMg3$p2inp1vec <- is.element(WB23_SDMg3$player2, player1vector)

addPlayer1 <- WB23_SDMg3[ which(WB23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_SDMg3[ which(WB23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_SDMg2 <- rbind(WB23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
WB23_SDMft <- ftable(WB23_SDMg2$player1, WB23_SDMg2$player2)
WB23_SDMft2 <- as.matrix(WB23_SDMft)
numRows <- nrow(WB23_SDMft2)
numCols <- ncol(WB23_SDMft2)
WB23_SDMft3 <- WB23_SDMft2[c(2:numRows) , c(2:numCols)]
WB23_SDMTable <- graph.adjacency(WB23_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(WB23_SDMTable, vertex.label = V(WB23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
WB23_SDM.clusterCoef <- transitivity(WB23_SDMTable, type="global") #cluster coefficient
WB23_SDM.degreeCent <- centralization.degree(WB23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_SDMftn <- as.network.matrix(WB23_SDMft)
WB23_SDM.netDensity <- network.density(WB23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_SDM.entropy <- entropy(WB23_SDMft) #entropy

WB23_SDM.netMx <- cbind(WB23_SDM.netMx, WB23_SDM.clusterCoef, WB23_SDM.degreeCent$centralization,
                        WB23_SDM.netDensity, WB23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************

round = 23
teamName = "WB"
KIoutcome = "Turnover_DM"
WB23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
WB23_TDMg2 <- data.frame(WB23_TDM)
WB23_TDMg2 <- WB23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_TDMg2$player1
player2vector <- WB23_TDMg2$player2
WB23_TDMg3 <- WB23_TDMg2
WB23_TDMg3$p1inp2vec <- is.element(WB23_TDMg3$player1, player2vector)
WB23_TDMg3$p2inp1vec <- is.element(WB23_TDMg3$player2, player1vector)

addPlayer1 <- WB23_TDMg3[ which(WB23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_TDMg3[ which(WB23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_TDMg2 <- rbind(WB23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
WB23_TDMft <- ftable(WB23_TDMg2$player1, WB23_TDMg2$player2)
WB23_TDMft2 <- as.matrix(WB23_TDMft)
numRows <- nrow(WB23_TDMft2)
numCols <- ncol(WB23_TDMft2)
WB23_TDMft3 <- WB23_TDMft2[c(2:numRows) , c(2:numCols)]
WB23_TDMTable <- graph.adjacency(WB23_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(WB23_TDMTable, vertex.label = V(WB23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
WB23_TDM.clusterCoef <- transitivity(WB23_TDMTable, type="global") #cluster coefficient
WB23_TDM.degreeCent <- centralization.degree(WB23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_TDMftn <- as.network.matrix(WB23_TDMft)
WB23_TDM.netDensity <- network.density(WB23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_TDM.entropy <- entropy(WB23_TDMft) #entropy

WB23_TDM.netMx <- cbind(WB23_TDM.netMx, WB23_TDM.clusterCoef, WB23_TDM.degreeCent$centralization,
                        WB23_TDM.netDensity, WB23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "Stoppage_D"
WB23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
WB23_SDg2 <- data.frame(WB23_SD)
WB23_SDg2 <- WB23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_SDg2$player1
player2vector <- WB23_SDg2$player2
WB23_SDg3 <- WB23_SDg2
WB23_SDg3$p1inp2vec <- is.element(WB23_SDg3$player1, player2vector)
WB23_SDg3$p2inp1vec <- is.element(WB23_SDg3$player2, player1vector)

addPlayer1 <- WB23_SDg3[ which(WB23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_SDg3[ which(WB23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_SDg2 <- rbind(WB23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
WB23_SDft <- ftable(WB23_SDg2$player1, WB23_SDg2$player2)
WB23_SDft2 <- as.matrix(WB23_SDft)
numRows <- nrow(WB23_SDft2)
numCols <- ncol(WB23_SDft2)
WB23_SDft3 <- WB23_SDft2[c(2:numRows) , c(2:numCols)]
WB23_SDTable <- graph.adjacency(WB23_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(WB23_SDTable, vertex.label = V(WB23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
WB23_SD.clusterCoef <- transitivity(WB23_SDTable, type="global") #cluster coefficient
WB23_SD.degreeCent <- centralization.degree(WB23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_SDftn <- as.network.matrix(WB23_SDft)
WB23_SD.netDensity <- network.density(WB23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_SD.entropy <- entropy(WB23_SDft) #entropy

WB23_SD.netMx <- cbind(WB23_SD.netMx, WB23_SD.clusterCoef, WB23_SD.degreeCent$centralization,
                       WB23_SD.netDensity, WB23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "Turnover_D"
WB23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
WB23_TDg2 <- data.frame(WB23_TD)
WB23_TDg2 <- WB23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_TDg2$player1
player2vector <- WB23_TDg2$player2
WB23_TDg3 <- WB23_TDg2
WB23_TDg3$p1inp2vec <- is.element(WB23_TDg3$player1, player2vector)
WB23_TDg3$p2inp1vec <- is.element(WB23_TDg3$player2, player1vector)

addPlayer1 <- WB23_TDg3[ which(WB23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_TDg3[ which(WB23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_TDg2 <- rbind(WB23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
WB23_TDft <- ftable(WB23_TDg2$player1, WB23_TDg2$player2)
WB23_TDft2 <- as.matrix(WB23_TDft)
numRows <- nrow(WB23_TDft2)
numCols <- ncol(WB23_TDft2)
WB23_TDft3 <- WB23_TDft2[c(2:numRows) , c(2:numCols)]
WB23_TDTable <- graph.adjacency(WB23_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(WB23_TDTable, vertex.label = V(WB23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
WB23_TD.clusterCoef <- transitivity(WB23_TDTable, type="global") #cluster coefficient
WB23_TD.degreeCent <- centralization.degree(WB23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_TDftn <- as.network.matrix(WB23_TDft)
WB23_TD.netDensity <- network.density(WB23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_TD.entropy <- entropy(WB23_TDft) #entropy

WB23_TD.netMx <- cbind(WB23_TD.netMx, WB23_TD.clusterCoef, WB23_TD.degreeCent$centralization,
                       WB23_TD.netDensity, WB23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
WB23_QTg2 <- data.frame(WB23_QT)
WB23_QTg2 <- WB23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB23_QTg2$player1
player2vector <- WB23_QTg2$player2
WB23_QTg3 <- WB23_QTg2
WB23_QTg3$p1inp2vec <- is.element(WB23_QTg3$player1, player2vector)
WB23_QTg3$p2inp1vec <- is.element(WB23_QTg3$player2, player1vector)

addPlayer1 <- WB23_QTg3[ which(WB23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB23_QTg3[ which(WB23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB23_QTg2 <- rbind(WB23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
WB23_QTft <- ftable(WB23_QTg2$player1, WB23_QTg2$player2)
WB23_QTft2 <- as.matrix(WB23_QTft)
numRows <- nrow(WB23_QTft2)
numCols <- ncol(WB23_QTft2)
WB23_QTft3 <- WB23_QTft2[c(2:numRows) , c(2:numCols)]
WB23_QTTable <- graph.adjacency(WB23_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(WB23_QTTable, vertex.label = V(WB23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
WB23_QT.clusterCoef <- transitivity(WB23_QTTable, type="global") #cluster coefficient
WB23_QT.degreeCent <- centralization.degree(WB23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB23_QTftn <- as.network.matrix(WB23_QTft)
WB23_QT.netDensity <- network.density(WB23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB23_QT.entropy <- entropy(WB23_QTft) #entropy

WB23_QT.netMx <- cbind(WB23_QT.netMx, WB23_QT.clusterCoef, WB23_QT.degreeCent$centralization,
                       WB23_QT.netDensity, WB23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB23_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 23
##

#ROUND 23, Goal***************************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Goal_F"
WCE23_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Goal with weighted edges
WCE23_Gg2 <- data.frame(WCE23_G)
WCE23_Gg2 <- WCE23_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_Gg2$player1
player2vector <- WCE23_Gg2$player2
WCE23_Gg3 <- WCE23_Gg2
WCE23_Gg3$p1inp2vec <- is.element(WCE23_Gg3$player1, player2vector)
WCE23_Gg3$p2inp1vec <- is.element(WCE23_Gg3$player2, player1vector)

addPlayer1 <- WCE23_Gg3[ which(WCE23_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_Gg3[ which(WCE23_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_Gg2 <- rbind(WCE23_Gg2, addPlayers)

#ROUND 23, Goal graph using weighted edges
WCE23_Gft <- ftable(WCE23_Gg2$player1, WCE23_Gg2$player2)
WCE23_Gft2 <- as.matrix(WCE23_Gft)
numRows <- nrow(WCE23_Gft2)
numCols <- ncol(WCE23_Gft2)
WCE23_Gft3 <- WCE23_Gft2[c(2:numRows) , c(2:numCols)]
WCE23_GTable <- graph.adjacency(WCE23_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Goal graph=weighted
plot.igraph(WCE23_GTable, vertex.label = V(WCE23_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Goal calulation of network metrics
#igraph
WCE23_G.clusterCoef <- transitivity(WCE23_GTable, type="global") #cluster coefficient
WCE23_G.degreeCent <- centralization.degree(WCE23_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_Gftn <- as.network.matrix(WCE23_Gft)
WCE23_G.netDensity <- network.density(WCE23_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_G.entropy <- entropy(WCE23_Gft) #entropy

WCE23_G.netMx <- cbind(WCE23_G.netMx, WCE23_G.clusterCoef, WCE23_G.degreeCent$centralization,
                       WCE23_G.netDensity, WCE23_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_G.netMx) <- varnames

#ROUND 23, Behind***************************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Behind_F"
WCE23_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, Behind with weighted edges
WCE23_Bg2 <- data.frame(WCE23_B)
WCE23_Bg2 <- WCE23_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_Bg2$player1
player2vector <- WCE23_Bg2$player2
WCE23_Bg3 <- WCE23_Bg2
WCE23_Bg3$p1inp2vec <- is.element(WCE23_Bg3$player1, player2vector)
WCE23_Bg3$p2inp1vec <- is.element(WCE23_Bg3$player2, player1vector)

addPlayer1 <- WCE23_Bg3[ which(WCE23_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_Bg3[ which(WCE23_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_Bg2 <- rbind(WCE23_Bg2, addPlayers)

#ROUND 23, Behind graph using weighted edges
WCE23_Bft <- ftable(WCE23_Bg2$player1, WCE23_Bg2$player2)
WCE23_Bft2 <- as.matrix(WCE23_Bft)
numRows <- nrow(WCE23_Bft2)
numCols <- ncol(WCE23_Bft2)
WCE23_Bft3 <- WCE23_Bft2[c(2:numRows) , c(2:numCols)]
WCE23_BTable <- graph.adjacency(WCE23_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 23, Behind graph=weighted
plot.igraph(WCE23_BTable, vertex.label = V(WCE23_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, Behind calulation of network metrics
#igraph
WCE23_B.clusterCoef <- transitivity(WCE23_BTable, type="global") #cluster coefficient
WCE23_B.degreeCent <- centralization.degree(WCE23_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_Bftn <- as.network.matrix(WCE23_Bft)
WCE23_B.netDensity <- network.density(WCE23_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_B.entropy <- entropy(WCE23_Bft) #entropy

WCE23_B.netMx <- cbind(WCE23_B.netMx, WCE23_B.clusterCoef, WCE23_B.degreeCent$centralization,
                       WCE23_B.netDensity, WCE23_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_B.netMx) <- varnames

#ROUND 23, FWD Stoppage**********************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE23_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Stoppage with weighted edges
WCE23_SFg2 <- data.frame(WCE23_SF)
WCE23_SFg2 <- WCE23_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_SFg2$player1
player2vector <- WCE23_SFg2$player2
WCE23_SFg3 <- WCE23_SFg2
WCE23_SFg3$p1inp2vec <- is.element(WCE23_SFg3$player1, player2vector)
WCE23_SFg3$p2inp1vec <- is.element(WCE23_SFg3$player2, player1vector)

addPlayer1 <- WCE23_SFg3[ which(WCE23_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE23_SFg2 <- rbind(WCE23_SFg2, addPlayer1)

#ROUND 23, FWD Stoppage graph using weighted edges
WCE23_SFft <- ftable(WCE23_SFg2$player1, WCE23_SFg2$player2)
WCE23_SFft2 <- as.matrix(WCE23_SFft)
numRows <- nrow(WCE23_SFft2)
numCols <- ncol(WCE23_SFft2)
WCE23_SFft3 <- WCE23_SFft2[c(2:numRows) , c(1:numCols)]
WCE23_SFTable <- graph.adjacency(WCE23_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Stoppage graph=weighted
plot.igraph(WCE23_SFTable, vertex.label = V(WCE23_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Stoppage calulation of network metrics
#igraph
WCE23_SF.clusterCoef <- transitivity(WCE23_SFTable, type="global") #cluster coefficient
WCE23_SF.degreeCent <- centralization.degree(WCE23_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_SFftn <- as.network.matrix(WCE23_SFft)
WCE23_SF.netDensity <- network.density(WCE23_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_SF.entropy <- entropy(WCE23_SFft) #entropy

WCE23_SF.netMx <- cbind(WCE23_SF.netMx, WCE23_SF.clusterCoef, WCE23_SF.degreeCent$centralization,
                        WCE23_SF.netDensity, WCE23_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_SF.netMx) <- varnames

#ROUND 23, FWD Turnover**********************************************************

round = 23
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE23_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, FWD Turnover with weighted edges
WCE23_TFg2 <- data.frame(WCE23_TF)
WCE23_TFg2 <- WCE23_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_TFg2$player1
player2vector <- WCE23_TFg2$player2
WCE23_TFg3 <- WCE23_TFg2
WCE23_TFg3$p1inp2vec <- is.element(WCE23_TFg3$player1, player2vector)
WCE23_TFg3$p2inp1vec <- is.element(WCE23_TFg3$player2, player1vector)

addPlayer1 <- WCE23_TFg3[ which(WCE23_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_TFg3[ which(WCE23_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_TFg2 <- rbind(WCE23_TFg2, addPlayers)

#ROUND 23, FWD Turnover graph using weighted edges
WCE23_TFft <- ftable(WCE23_TFg2$player1, WCE23_TFg2$player2)
WCE23_TFft2 <- as.matrix(WCE23_TFft)
numRows <- nrow(WCE23_TFft2)
numCols <- ncol(WCE23_TFft2)
WCE23_TFft3 <- WCE23_TFft2[c(2:numRows) , c(2:numCols)]
WCE23_TFTable <- graph.adjacency(WCE23_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, FWD Turnover graph=weighted
plot.igraph(WCE23_TFTable, vertex.label = V(WCE23_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, FWD Turnover calulation of network metrics
#igraph
WCE23_TF.clusterCoef <- transitivity(WCE23_TFTable, type="global") #cluster coefficient
WCE23_TF.degreeCent <- centralization.degree(WCE23_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_TFftn <- as.network.matrix(WCE23_TFft)
WCE23_TF.netDensity <- network.density(WCE23_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_TF.entropy <- entropy(WCE23_TFft) #entropy

WCE23_TF.netMx <- cbind(WCE23_TF.netMx, WCE23_TF.clusterCoef, WCE23_TF.degreeCent$centralization,
                        WCE23_TF.netDensity, WCE23_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_TF.netMx) <- varnames

#ROUND 23, AM Stoppage**********************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE23_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Stoppage with weighted edges
WCE23_SAMg2 <- data.frame(WCE23_SAM)
WCE23_SAMg2 <- WCE23_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_SAMg2$player1
player2vector <- WCE23_SAMg2$player2
WCE23_SAMg3 <- WCE23_SAMg2
WCE23_SAMg3$p1inp2vec <- is.element(WCE23_SAMg3$player1, player2vector)
WCE23_SAMg3$p2inp1vec <- is.element(WCE23_SAMg3$player2, player1vector)

addPlayer1 <- WCE23_SAMg3[ which(WCE23_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_SAMg3[ which(WCE23_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_SAMg2 <- rbind(WCE23_SAMg2, addPlayers)

#ROUND 23, AM Stoppage graph using weighted edges
WCE23_SAMft <- ftable(WCE23_SAMg2$player1, WCE23_SAMg2$player2)
WCE23_SAMft2 <- as.matrix(WCE23_SAMft)
numRows <- nrow(WCE23_SAMft2)
numCols <- ncol(WCE23_SAMft2)
WCE23_SAMft3 <- WCE23_SAMft2[c(2:numRows) , c(2:numCols)]
WCE23_SAMTable <- graph.adjacency(WCE23_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Stoppage graph=weighted
plot.igraph(WCE23_SAMTable, vertex.label = V(WCE23_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Stoppage calulation of network metrics
#igraph
WCE23_SAM.clusterCoef <- transitivity(WCE23_SAMTable, type="global") #cluster coefficient
WCE23_SAM.degreeCent <- centralization.degree(WCE23_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_SAMftn <- as.network.matrix(WCE23_SAMft)
WCE23_SAM.netDensity <- network.density(WCE23_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_SAM.entropy <- entropy(WCE23_SAMft) #entropy

WCE23_SAM.netMx <- cbind(WCE23_SAM.netMx, WCE23_SAM.clusterCoef, WCE23_SAM.degreeCent$centralization,
                         WCE23_SAM.netDensity, WCE23_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_SAM.netMx) <- varnames

#ROUND 23, AM Turnover**********************************************************

round = 23
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE23_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, AM Turnover with weighted edges
WCE23_TAMg2 <- data.frame(WCE23_TAM)
WCE23_TAMg2 <- WCE23_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_TAMg2$player1
player2vector <- WCE23_TAMg2$player2
WCE23_TAMg3 <- WCE23_TAMg2
WCE23_TAMg3$p1inp2vec <- is.element(WCE23_TAMg3$player1, player2vector)
WCE23_TAMg3$p2inp1vec <- is.element(WCE23_TAMg3$player2, player1vector)

addPlayer1 <- WCE23_TAMg3[ which(WCE23_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_TAMg3[ which(WCE23_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_TAMg2 <- rbind(WCE23_TAMg2, addPlayers)

#ROUND 23, AM Turnover graph using weighted edges
WCE23_TAMft <- ftable(WCE23_TAMg2$player1, WCE23_TAMg2$player2)
WCE23_TAMft2 <- as.matrix(WCE23_TAMft)
numRows <- nrow(WCE23_TAMft2)
numCols <- ncol(WCE23_TAMft2)
WCE23_TAMft3 <- WCE23_TAMft2[c(2:numRows) , c(2:numCols)]
WCE23_TAMTable <- graph.adjacency(WCE23_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, AM Turnover graph=weighted
plot.igraph(WCE23_TAMTable, vertex.label = V(WCE23_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, AM Turnover calulation of network metrics
#igraph
WCE23_TAM.clusterCoef <- transitivity(WCE23_TAMTable, type="global") #cluster coefficient
WCE23_TAM.degreeCent <- centralization.degree(WCE23_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_TAMftn <- as.network.matrix(WCE23_TAMft)
WCE23_TAM.netDensity <- network.density(WCE23_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_TAM.entropy <- entropy(WCE23_TAMft) #entropy

WCE23_TAM.netMx <- cbind(WCE23_TAM.netMx, WCE23_TAM.clusterCoef, WCE23_TAM.degreeCent$centralization,
                         WCE23_TAM.netDensity, WCE23_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_TAM.netMx) <- varnames

#ROUND 23, DM Stoppage**********************************************************

round = 23
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE23_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Stoppage with weighted edges
WCE23_SDMg2 <- data.frame(WCE23_SDM)
WCE23_SDMg2 <- WCE23_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_SDMg2$player1
player2vector <- WCE23_SDMg2$player2
WCE23_SDMg3 <- WCE23_SDMg2
WCE23_SDMg3$p1inp2vec <- is.element(WCE23_SDMg3$player1, player2vector)
WCE23_SDMg3$p2inp1vec <- is.element(WCE23_SDMg3$player2, player1vector)

addPlayer1 <- WCE23_SDMg3[ which(WCE23_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_SDMg3[ which(WCE23_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_SDMg2 <- rbind(WCE23_SDMg2, addPlayers)

#ROUND 23, DM Stoppage graph using weighted edges
WCE23_SDMft <- ftable(WCE23_SDMg2$player1, WCE23_SDMg2$player2)
WCE23_SDMft2 <- as.matrix(WCE23_SDMft)
numRows <- nrow(WCE23_SDMft2)
numCols <- ncol(WCE23_SDMft2)
WCE23_SDMft3 <- WCE23_SDMft2[c(2:numRows) , c(2:numCols)]
WCE23_SDMTable <- graph.adjacency(WCE23_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Stoppage graph=weighted
plot.igraph(WCE23_SDMTable, vertex.label = V(WCE23_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Stoppage calulation of network metrics
#igraph
WCE23_SDM.clusterCoef <- transitivity(WCE23_SDMTable, type="global") #cluster coefficient
WCE23_SDM.degreeCent <- centralization.degree(WCE23_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_SDMftn <- as.network.matrix(WCE23_SDMft)
WCE23_SDM.netDensity <- network.density(WCE23_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_SDM.entropy <- entropy(WCE23_SDMft) #entropy

WCE23_SDM.netMx <- cbind(WCE23_SDM.netMx, WCE23_SDM.clusterCoef, WCE23_SDM.degreeCent$centralization,
                         WCE23_SDM.netDensity, WCE23_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_SDM.netMx) <- varnames

#ROUND 23, DM Turnover**********************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE23_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, DM Turnover with weighted edges
WCE23_TDMg2 <- data.frame(WCE23_TDM)
WCE23_TDMg2 <- WCE23_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_TDMg2$player1
player2vector <- WCE23_TDMg2$player2
WCE23_TDMg3 <- WCE23_TDMg2
WCE23_TDMg3$p1inp2vec <- is.element(WCE23_TDMg3$player1, player2vector)
WCE23_TDMg3$p2inp1vec <- is.element(WCE23_TDMg3$player2, player1vector)

addPlayer1 <- WCE23_TDMg3[ which(WCE23_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_TDMg3[ which(WCE23_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_TDMg2 <- rbind(WCE23_TDMg2, addPlayers)

#ROUND 23, DM Turnover graph using weighted edges
WCE23_TDMft <- ftable(WCE23_TDMg2$player1, WCE23_TDMg2$player2)
WCE23_TDMft2 <- as.matrix(WCE23_TDMft)
numRows <- nrow(WCE23_TDMft2)
numCols <- ncol(WCE23_TDMft2)
WCE23_TDMft3 <- WCE23_TDMft2[c(2:numRows) , c(2:numCols)]
WCE23_TDMTable <- graph.adjacency(WCE23_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 23, DM Turnover graph=weighted
plot.igraph(WCE23_TDMTable, vertex.label = V(WCE23_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, DM Turnover calulation of network metrics
#igraph
WCE23_TDM.clusterCoef <- transitivity(WCE23_TDMTable, type="global") #cluster coefficient
WCE23_TDM.degreeCent <- centralization.degree(WCE23_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_TDMftn <- as.network.matrix(WCE23_TDMft)
WCE23_TDM.netDensity <- network.density(WCE23_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_TDM.entropy <- entropy(WCE23_TDMft) #entropy

WCE23_TDM.netMx <- cbind(WCE23_TDM.netMx, WCE23_TDM.clusterCoef, WCE23_TDM.degreeCent$centralization,
                         WCE23_TDM.netDensity, WCE23_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_TDM.netMx) <- varnames

#ROUND 23, D Stoppage**********************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE23_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Stoppage with weighted edges
WCE23_SDg2 <- data.frame(WCE23_SD)
WCE23_SDg2 <- WCE23_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_SDg2$player1
player2vector <- WCE23_SDg2$player2
WCE23_SDg3 <- WCE23_SDg2
WCE23_SDg3$p1inp2vec <- is.element(WCE23_SDg3$player1, player2vector)
WCE23_SDg3$p2inp1vec <- is.element(WCE23_SDg3$player2, player1vector)

addPlayer1 <- WCE23_SDg3[ which(WCE23_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_SDg3[ which(WCE23_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_SDg2 <- rbind(WCE23_SDg2, addPlayers)

#ROUND 23, D Stoppage graph using weighted edges
WCE23_SDft <- ftable(WCE23_SDg2$player1, WCE23_SDg2$player2)
WCE23_SDft2 <- as.matrix(WCE23_SDft)
numRows <- nrow(WCE23_SDft2)
numCols <- ncol(WCE23_SDft2)
WCE23_SDft3 <- WCE23_SDft2[c(2:numRows) , c(2:numCols)]
WCE23_SDTable <- graph.adjacency(WCE23_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Stoppage graph=weighted
plot.igraph(WCE23_SDTable, vertex.label = V(WCE23_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Stoppage calulation of network metrics
#igraph
WCE23_SD.clusterCoef <- transitivity(WCE23_SDTable, type="global") #cluster coefficient
WCE23_SD.degreeCent <- centralization.degree(WCE23_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_SDftn <- as.network.matrix(WCE23_SDft)
WCE23_SD.netDensity <- network.density(WCE23_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_SD.entropy <- entropy(WCE23_SDft) #entropy

WCE23_SD.netMx <- cbind(WCE23_SD.netMx, WCE23_SD.clusterCoef, WCE23_SD.degreeCent$centralization,
                        WCE23_SD.netDensity, WCE23_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_SD.netMx) <- varnames

#ROUND 23, D Turnover**********************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE23_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, D Turnover with weighted edges
WCE23_TDg2 <- data.frame(WCE23_TD)
WCE23_TDg2 <- WCE23_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_TDg2$player1
player2vector <- WCE23_TDg2$player2
WCE23_TDg3 <- WCE23_TDg2
WCE23_TDg3$p1inp2vec <- is.element(WCE23_TDg3$player1, player2vector)
WCE23_TDg3$p2inp1vec <- is.element(WCE23_TDg3$player2, player1vector)

addPlayer1 <- WCE23_TDg3[ which(WCE23_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_TDg3[ which(WCE23_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_TDg2 <- rbind(WCE23_TDg2, addPlayers)

#ROUND 23, D Turnover graph using weighted edges
WCE23_TDft <- ftable(WCE23_TDg2$player1, WCE23_TDg2$player2)
WCE23_TDft2 <- as.matrix(WCE23_TDft)
numRows <- nrow(WCE23_TDft2)
numCols <- ncol(WCE23_TDft2)
WCE23_TDft3 <- WCE23_TDft2[c(2:numRows) , c(2:numCols)]
WCE23_TDTable <- graph.adjacency(WCE23_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, D Turnover graph=weighted
plot.igraph(WCE23_TDTable, vertex.label = V(WCE23_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, D Turnover calulation of network metrics
#igraph
WCE23_TD.clusterCoef <- transitivity(WCE23_TDTable, type="global") #cluster coefficient
WCE23_TD.degreeCent <- centralization.degree(WCE23_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_TDftn <- as.network.matrix(WCE23_TDft)
WCE23_TD.netDensity <- network.density(WCE23_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_TD.entropy <- entropy(WCE23_TDft) #entropy

WCE23_TD.netMx <- cbind(WCE23_TD.netMx, WCE23_TD.clusterCoef, WCE23_TD.degreeCent$centralization,
                        WCE23_TD.netDensity, WCE23_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_TD.netMx) <- varnames

#ROUND 23, End of Qtr**********************************************************
#NA

round = 23
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE23_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 23, End of Qtr with weighted edges
WCE23_QTg2 <- data.frame(WCE23_QT)
WCE23_QTg2 <- WCE23_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE23_QTg2$player1
player2vector <- WCE23_QTg2$player2
WCE23_QTg3 <- WCE23_QTg2
WCE23_QTg3$p1inp2vec <- is.element(WCE23_QTg3$player1, player2vector)
WCE23_QTg3$p2inp1vec <- is.element(WCE23_QTg3$player2, player1vector)

addPlayer1 <- WCE23_QTg3[ which(WCE23_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE23_QTg3[ which(WCE23_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE23_QTg2 <- rbind(WCE23_QTg2, addPlayers)

#ROUND 23, End of Qtr graph using weighted edges
WCE23_QTft <- ftable(WCE23_QTg2$player1, WCE23_QTg2$player2)
WCE23_QTft2 <- as.matrix(WCE23_QTft)
numRows <- nrow(WCE23_QTft2)
numCols <- ncol(WCE23_QTft2)
WCE23_QTft3 <- WCE23_QTft2[c(2:numRows) , c(2:numCols)]
WCE23_QTTable <- graph.adjacency(WCE23_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 23, End of Qtr graph=weighted
plot.igraph(WCE23_QTTable, vertex.label = V(WCE23_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE23_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 23, End of Qtr calulation of network metrics
#igraph
WCE23_QT.clusterCoef <- transitivity(WCE23_QTTable, type="global") #cluster coefficient
WCE23_QT.degreeCent <- centralization.degree(WCE23_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE23_QTftn <- as.network.matrix(WCE23_QTft)
WCE23_QT.netDensity <- network.density(WCE23_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE23_QT.entropy <- entropy(WCE23_QTft) #entropy

WCE23_QT.netMx <- cbind(WCE23_QT.netMx, WCE23_QT.clusterCoef, WCE23_QT.degreeCent$centralization,
                        WCE23_QT.netDensity, WCE23_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE23_QT.netMx) <- varnames
