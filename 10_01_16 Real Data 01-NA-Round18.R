#####
#10-01-16 Real data 18
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
ADEL18_Gg2 <- data.frame(ADEL18_G)
ADEL18_Gg2 <- ADEL18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_Gg2$player1
player2vector <- ADEL18_Gg2$player2
ADEL18_Gg3 <- ADEL18_Gg2
ADEL18_Gg3$p1inp2vec <- is.element(ADEL18_Gg3$player1, player2vector)
ADEL18_Gg3$p2inp1vec <- is.element(ADEL18_Gg3$player2, player1vector)

addPlayer1 <- ADEL18_Gg3[ which(ADEL18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_Gg3[ which(ADEL18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_Gg2 <- rbind(ADEL18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
ADEL18_Gft <- ftable(ADEL18_Gg2$player1, ADEL18_Gg2$player2)
ADEL18_Gft2 <- as.matrix(ADEL18_Gft)
numRows <- nrow(ADEL18_Gft2)
numCols <- ncol(ADEL18_Gft2)
ADEL18_Gft3 <- ADEL18_Gft2[c(2:numRows) , c(2:numCols)]
ADEL18_GTable <- graph.adjacency(ADEL18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(ADEL18_GTable, vertex.label = V(ADEL18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
ADEL18_G.clusterCoef <- transitivity(ADEL18_GTable, type="global") #cluster coefficient
ADEL18_G.degreeCent <- centralization.degree(ADEL18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_Gftn <- as.network.matrix(ADEL18_Gft)
ADEL18_G.netDensity <- network.density(ADEL18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_G.entropy <- entropy(ADEL18_Gft) #entropy

ADEL18_G.netMx <- cbind(ADEL18_G.netMx, ADEL18_G.clusterCoef, ADEL18_G.degreeCent$centralization,
                        ADEL18_G.netDensity, ADEL18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
ADEL18_Bg2 <- data.frame(ADEL18_B)
ADEL18_Bg2 <- ADEL18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_Bg2$player1
player2vector <- ADEL18_Bg2$player2
ADEL18_Bg3 <- ADEL18_Bg2
ADEL18_Bg3$p1inp2vec <- is.element(ADEL18_Bg3$player1, player2vector)
ADEL18_Bg3$p2inp1vec <- is.element(ADEL18_Bg3$player2, player1vector)

addPlayer1 <- ADEL18_Bg3[ which(ADEL18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_Bg3[ which(ADEL18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_Bg2 <- rbind(ADEL18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
ADEL18_Bft <- ftable(ADEL18_Bg2$player1, ADEL18_Bg2$player2)
ADEL18_Bft2 <- as.matrix(ADEL18_Bft)
numRows <- nrow(ADEL18_Bft2)
numCols <- ncol(ADEL18_Bft2)
ADEL18_Bft3 <- ADEL18_Bft2[c(2:numRows) , c(2:numCols)]
ADEL18_BTable <- graph.adjacency(ADEL18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(ADEL18_BTable, vertex.label = V(ADEL18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
ADEL18_B.clusterCoef <- transitivity(ADEL18_BTable, type="global") #cluster coefficient
ADEL18_B.degreeCent <- centralization.degree(ADEL18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_Bftn <- as.network.matrix(ADEL18_Bft)
ADEL18_B.netDensity <- network.density(ADEL18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_B.entropy <- entropy(ADEL18_Bft) #entropy

ADEL18_B.netMx <- cbind(ADEL18_B.netMx, ADEL18_B.clusterCoef, ADEL18_B.degreeCent$centralization,
                        ADEL18_B.netDensity, ADEL18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
ADEL18_SFg2 <- data.frame(ADEL18_SF)
ADEL18_SFg2 <- ADEL18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_SFg2$player1
player2vector <- ADEL18_SFg2$player2
ADEL18_SFg3 <- ADEL18_SFg2
ADEL18_SFg3$p1inp2vec <- is.element(ADEL18_SFg3$player1, player2vector)
ADEL18_SFg3$p2inp1vec <- is.element(ADEL18_SFg3$player2, player1vector)

addPlayer1 <- ADEL18_SFg3[ which(ADEL18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_SFg3[ which(ADEL18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_SFg2 <- rbind(ADEL18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
ADEL18_SFft <- ftable(ADEL18_SFg2$player1, ADEL18_SFg2$player2)
ADEL18_SFft2 <- as.matrix(ADEL18_SFft)
numRows <- nrow(ADEL18_SFft2)
numCols <- ncol(ADEL18_SFft2)
ADEL18_SFft3 <- ADEL18_SFft2[c(2:numRows) , c(2:numCols)]
ADEL18_SFTable <- graph.adjacency(ADEL18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(ADEL18_SFTable, vertex.label = V(ADEL18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
ADEL18_SF.clusterCoef <- transitivity(ADEL18_SFTable, type="global") #cluster coefficient
ADEL18_SF.degreeCent <- centralization.degree(ADEL18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_SFftn <- as.network.matrix(ADEL18_SFft)
ADEL18_SF.netDensity <- network.density(ADEL18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_SF.entropy <- entropy(ADEL18_SFft) #entropy

ADEL18_SF.netMx <- cbind(ADEL18_SF.netMx, ADEL18_SF.clusterCoef, ADEL18_SF.degreeCent$centralization,
                         ADEL18_SF.netDensity, ADEL18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
ADEL18_TFg2 <- data.frame(ADEL18_TF)
ADEL18_TFg2 <- ADEL18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_TFg2$player1
player2vector <- ADEL18_TFg2$player2
ADEL18_TFg3 <- ADEL18_TFg2
ADEL18_TFg3$p1inp2vec <- is.element(ADEL18_TFg3$player1, player2vector)
ADEL18_TFg3$p2inp1vec <- is.element(ADEL18_TFg3$player2, player1vector)

addPlayer1 <- ADEL18_TFg3[ which(ADEL18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_TFg3[ which(ADEL18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_TFg2 <- rbind(ADEL18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
ADEL18_TFft <- ftable(ADEL18_TFg2$player1, ADEL18_TFg2$player2)
ADEL18_TFft2 <- as.matrix(ADEL18_TFft)
numRows <- nrow(ADEL18_TFft2)
numCols <- ncol(ADEL18_TFft2)
ADEL18_TFft3 <- ADEL18_TFft2[c(2:numRows) , c(2:numCols)]
ADEL18_TFTable <- graph.adjacency(ADEL18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(ADEL18_TFTable, vertex.label = V(ADEL18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
ADEL18_TF.clusterCoef <- transitivity(ADEL18_TFTable, type="global") #cluster coefficient
ADEL18_TF.degreeCent <- centralization.degree(ADEL18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_TFftn <- as.network.matrix(ADEL18_TFft)
ADEL18_TF.netDensity <- network.density(ADEL18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_TF.entropy <- entropy(ADEL18_TFft) #entropy

ADEL18_TF.netMx <- cbind(ADEL18_TF.netMx, ADEL18_TF.clusterCoef, ADEL18_TF.degreeCent$centralization,
                         ADEL18_TF.netDensity, ADEL18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
ADEL18_SAMg2 <- data.frame(ADEL18_SAM)
ADEL18_SAMg2 <- ADEL18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_SAMg2$player1
player2vector <- ADEL18_SAMg2$player2
ADEL18_SAMg3 <- ADEL18_SAMg2
ADEL18_SAMg3$p1inp2vec <- is.element(ADEL18_SAMg3$player1, player2vector)
ADEL18_SAMg3$p2inp1vec <- is.element(ADEL18_SAMg3$player2, player1vector)

addPlayer1 <- ADEL18_SAMg3[ which(ADEL18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_SAMg3[ which(ADEL18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_SAMg2 <- rbind(ADEL18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
ADEL18_SAMft <- ftable(ADEL18_SAMg2$player1, ADEL18_SAMg2$player2)
ADEL18_SAMft2 <- as.matrix(ADEL18_SAMft)
numRows <- nrow(ADEL18_SAMft2)
numCols <- ncol(ADEL18_SAMft2)
ADEL18_SAMft3 <- ADEL18_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL18_SAMTable <- graph.adjacency(ADEL18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(ADEL18_SAMTable, vertex.label = V(ADEL18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
ADEL18_SAM.clusterCoef <- transitivity(ADEL18_SAMTable, type="global") #cluster coefficient
ADEL18_SAM.degreeCent <- centralization.degree(ADEL18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_SAMftn <- as.network.matrix(ADEL18_SAMft)
ADEL18_SAM.netDensity <- network.density(ADEL18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_SAM.entropy <- entropy(ADEL18_SAMft) #entropy

ADEL18_SAM.netMx <- cbind(ADEL18_SAM.netMx, ADEL18_SAM.clusterCoef, ADEL18_SAM.degreeCent$centralization,
                          ADEL18_SAM.netDensity, ADEL18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
ADEL18_TAMg2 <- data.frame(ADEL18_TAM)
ADEL18_TAMg2 <- ADEL18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_TAMg2$player1
player2vector <- ADEL18_TAMg2$player2
ADEL18_TAMg3 <- ADEL18_TAMg2
ADEL18_TAMg3$p1inp2vec <- is.element(ADEL18_TAMg3$player1, player2vector)
ADEL18_TAMg3$p2inp1vec <- is.element(ADEL18_TAMg3$player2, player1vector)

addPlayer1 <- ADEL18_TAMg3[ which(ADEL18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL18_TAMg3[ which(ADEL18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_TAMg2 <- rbind(ADEL18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
ADEL18_TAMft <- ftable(ADEL18_TAMg2$player1, ADEL18_TAMg2$player2)
ADEL18_TAMft2 <- as.matrix(ADEL18_TAMft)
numRows <- nrow(ADEL18_TAMft2)
numCols <- ncol(ADEL18_TAMft2)
ADEL18_TAMft3 <- ADEL18_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL18_TAMTable <- graph.adjacency(ADEL18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(ADEL18_TAMTable, vertex.label = V(ADEL18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
ADEL18_TAM.clusterCoef <- transitivity(ADEL18_TAMTable, type="global") #cluster coefficient
ADEL18_TAM.degreeCent <- centralization.degree(ADEL18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_TAMftn <- as.network.matrix(ADEL18_TAMft)
ADEL18_TAM.netDensity <- network.density(ADEL18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_TAM.entropy <- entropy(ADEL18_TAMft) #entropy

ADEL18_TAM.netMx <- cbind(ADEL18_TAM.netMx, ADEL18_TAM.clusterCoef, ADEL18_TAM.degreeCent$centralization,
                          ADEL18_TAM.netDensity, ADEL18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
ADEL18_SDMg2 <- data.frame(ADEL18_SDM)
ADEL18_SDMg2 <- ADEL18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_SDMg2$player1
player2vector <- ADEL18_SDMg2$player2
ADEL18_SDMg3 <- ADEL18_SDMg2
ADEL18_SDMg3$p1inp2vec <- is.element(ADEL18_SDMg3$player1, player2vector)
ADEL18_SDMg3$p2inp1vec <- is.element(ADEL18_SDMg3$player2, player1vector)

addPlayer1 <- ADEL18_SDMg3[ which(ADEL18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_SDMg3[ which(ADEL18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_SDMg2 <- rbind(ADEL18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
ADEL18_SDMft <- ftable(ADEL18_SDMg2$player1, ADEL18_SDMg2$player2)
ADEL18_SDMft2 <- as.matrix(ADEL18_SDMft)
numRows <- nrow(ADEL18_SDMft2)
numCols <- ncol(ADEL18_SDMft2)
ADEL18_SDMft3 <- ADEL18_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL18_SDMTable <- graph.adjacency(ADEL18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(ADEL18_SDMTable, vertex.label = V(ADEL18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
ADEL18_SDM.clusterCoef <- transitivity(ADEL18_SDMTable, type="global") #cluster coefficient
ADEL18_SDM.degreeCent <- centralization.degree(ADEL18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_SDMftn <- as.network.matrix(ADEL18_SDMft)
ADEL18_SDM.netDensity <- network.density(ADEL18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_SDM.entropy <- entropy(ADEL18_SDMft) #entropy

ADEL18_SDM.netMx <- cbind(ADEL18_SDM.netMx, ADEL18_SDM.clusterCoef, ADEL18_SDM.degreeCent$centralization,
                          ADEL18_SDM.netDensity, ADEL18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
ADEL18_TDMg2 <- data.frame(ADEL18_TDM)
ADEL18_TDMg2 <- ADEL18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_TDMg2$player1
player2vector <- ADEL18_TDMg2$player2
ADEL18_TDMg3 <- ADEL18_TDMg2
ADEL18_TDMg3$p1inp2vec <- is.element(ADEL18_TDMg3$player1, player2vector)
ADEL18_TDMg3$p2inp1vec <- is.element(ADEL18_TDMg3$player2, player1vector)

addPlayer1 <- ADEL18_TDMg3[ which(ADEL18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_TDMg3[ which(ADEL18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_TDMg2 <- rbind(ADEL18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
ADEL18_TDMft <- ftable(ADEL18_TDMg2$player1, ADEL18_TDMg2$player2)
ADEL18_TDMft2 <- as.matrix(ADEL18_TDMft)
numRows <- nrow(ADEL18_TDMft2)
numCols <- ncol(ADEL18_TDMft2)
ADEL18_TDMft3 <- ADEL18_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL18_TDMTable <- graph.adjacency(ADEL18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(ADEL18_TDMTable, vertex.label = V(ADEL18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
ADEL18_TDM.clusterCoef <- transitivity(ADEL18_TDMTable, type="global") #cluster coefficient
ADEL18_TDM.degreeCent <- centralization.degree(ADEL18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_TDMftn <- as.network.matrix(ADEL18_TDMft)
ADEL18_TDM.netDensity <- network.density(ADEL18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_TDM.entropy <- entropy(ADEL18_TDMft) #entropy

ADEL18_TDM.netMx <- cbind(ADEL18_TDM.netMx, ADEL18_TDM.clusterCoef, ADEL18_TDM.degreeCent$centralization,
                          ADEL18_TDM.netDensity, ADEL18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
ADEL18_SDg2 <- data.frame(ADEL18_SD)
ADEL18_SDg2 <- ADEL18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_SDg2$player1
player2vector <- ADEL18_SDg2$player2
ADEL18_SDg3 <- ADEL18_SDg2
ADEL18_SDg3$p1inp2vec <- is.element(ADEL18_SDg3$player1, player2vector)
ADEL18_SDg3$p2inp1vec <- is.element(ADEL18_SDg3$player2, player1vector)

addPlayer1 <- ADEL18_SDg3[ which(ADEL18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_SDg3[ which(ADEL18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_SDg2 <- rbind(ADEL18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
ADEL18_SDft <- ftable(ADEL18_SDg2$player1, ADEL18_SDg2$player2)
ADEL18_SDft2 <- as.matrix(ADEL18_SDft)
numRows <- nrow(ADEL18_SDft2)
numCols <- ncol(ADEL18_SDft2)
ADEL18_SDft3 <- ADEL18_SDft2[c(2:numRows) , c(2:numCols)]
ADEL18_SDTable <- graph.adjacency(ADEL18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(ADEL18_SDTable, vertex.label = V(ADEL18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
ADEL18_SD.clusterCoef <- transitivity(ADEL18_SDTable, type="global") #cluster coefficient
ADEL18_SD.degreeCent <- centralization.degree(ADEL18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_SDftn <- as.network.matrix(ADEL18_SDft)
ADEL18_SD.netDensity <- network.density(ADEL18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_SD.entropy <- entropy(ADEL18_SDft) #entropy

ADEL18_SD.netMx <- cbind(ADEL18_SD.netMx, ADEL18_SD.clusterCoef, ADEL18_SD.degreeCent$centralization,
                         ADEL18_SD.netDensity, ADEL18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
ADEL18_TDg2 <- data.frame(ADEL18_TD)
ADEL18_TDg2 <- ADEL18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_TDg2$player1
player2vector <- ADEL18_TDg2$player2
ADEL18_TDg3 <- ADEL18_TDg2
ADEL18_TDg3$p1inp2vec <- is.element(ADEL18_TDg3$player1, player2vector)
ADEL18_TDg3$p2inp1vec <- is.element(ADEL18_TDg3$player2, player1vector)

addPlayer1 <- ADEL18_TDg3[ which(ADEL18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_TDg3[ which(ADEL18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_TDg2 <- rbind(ADEL18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
ADEL18_TDft <- ftable(ADEL18_TDg2$player1, ADEL18_TDg2$player2)
ADEL18_TDft2 <- as.matrix(ADEL18_TDft)
numRows <- nrow(ADEL18_TDft2)
numCols <- ncol(ADEL18_TDft2)
ADEL18_TDft3 <- ADEL18_TDft2[c(2:numRows) , c(2:numCols)]
ADEL18_TDTable <- graph.adjacency(ADEL18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(ADEL18_TDTable, vertex.label = V(ADEL18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
ADEL18_TD.clusterCoef <- transitivity(ADEL18_TDTable, type="global") #cluster coefficient
ADEL18_TD.degreeCent <- centralization.degree(ADEL18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_TDftn <- as.network.matrix(ADEL18_TDft)
ADEL18_TD.netDensity <- network.density(ADEL18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_TD.entropy <- entropy(ADEL18_TDft) #entropy

ADEL18_TD.netMx <- cbind(ADEL18_TD.netMx, ADEL18_TD.clusterCoef, ADEL18_TD.degreeCent$centralization,
                         ADEL18_TD.netDensity, ADEL18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
ADEL18_QTg2 <- data.frame(ADEL18_QT)
ADEL18_QTg2 <- ADEL18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL18_QTg2$player1
player2vector <- ADEL18_QTg2$player2
ADEL18_QTg3 <- ADEL18_QTg2
ADEL18_QTg3$p1inp2vec <- is.element(ADEL18_QTg3$player1, player2vector)
ADEL18_QTg3$p2inp1vec <- is.element(ADEL18_QTg3$player2, player1vector)

addPlayer1 <- ADEL18_QTg3[ which(ADEL18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL18_QTg3[ which(ADEL18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL18_QTg2 <- rbind(ADEL18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
ADEL18_QTft <- ftable(ADEL18_QTg2$player1, ADEL18_QTg2$player2)
ADEL18_QTft2 <- as.matrix(ADEL18_QTft)
numRows <- nrow(ADEL18_QTft2)
numCols <- ncol(ADEL18_QTft2)
ADEL18_QTft3 <- ADEL18_QTft2[c(2:numRows) , c(2:numCols)]
ADEL18_QTTable <- graph.adjacency(ADEL18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(ADEL18_QTTable, vertex.label = V(ADEL18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
ADEL18_QT.clusterCoef <- transitivity(ADEL18_QTTable, type="global") #cluster coefficient
ADEL18_QT.degreeCent <- centralization.degree(ADEL18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL18_QTftn <- as.network.matrix(ADEL18_QTft)
ADEL18_QT.netDensity <- network.density(ADEL18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL18_QT.entropy <- entropy(ADEL18_QTft) #entropy

ADEL18_QT.netMx <- cbind(ADEL18_QT.netMx, ADEL18_QT.clusterCoef, ADEL18_QT.degreeCent$centralization,
                         ADEL18_QT.netDensity, ADEL18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL18_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "BL"
KIoutcome = "Goal_F"
BL18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
BL18_Gg2 <- data.frame(BL18_G)
BL18_Gg2 <- BL18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_Gg2$player1
player2vector <- BL18_Gg2$player2
BL18_Gg3 <- BL18_Gg2
BL18_Gg3$p1inp2vec <- is.element(BL18_Gg3$player1, player2vector)
BL18_Gg3$p2inp1vec <- is.element(BL18_Gg3$player2, player1vector)

addPlayer1 <- BL18_Gg3[ which(BL18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_Gg3[ which(BL18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_Gg2 <- rbind(BL18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
BL18_Gft <- ftable(BL18_Gg2$player1, BL18_Gg2$player2)
BL18_Gft2 <- as.matrix(BL18_Gft)
numRows <- nrow(BL18_Gft2)
numCols <- ncol(BL18_Gft2)
BL18_Gft3 <- BL18_Gft2[c(2:numRows) , c(2:numCols)]
BL18_GTable <- graph.adjacency(BL18_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(BL18_GTable, vertex.label = V(BL18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
BL18_G.clusterCoef <- transitivity(BL18_GTable, type="global") #cluster coefficient
BL18_G.degreeCent <- centralization.degree(BL18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_Gftn <- as.network.matrix(BL18_Gft)
BL18_G.netDensity <- network.density(BL18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_G.entropy <- entropy(BL18_Gft) #entropy

BL18_G.netMx <- cbind(BL18_G.netMx, BL18_G.clusterCoef, BL18_G.degreeCent$centralization,
                      BL18_G.netDensity, BL18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "BL"
KIoutcome = "Behind_F"
BL18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
BL18_Bg2 <- data.frame(BL18_B)
BL18_Bg2 <- BL18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_Bg2$player1
player2vector <- BL18_Bg2$player2
BL18_Bg3 <- BL18_Bg2
BL18_Bg3$p1inp2vec <- is.element(BL18_Bg3$player1, player2vector)
BL18_Bg3$p2inp1vec <- is.element(BL18_Bg3$player2, player1vector)

addPlayer1 <- BL18_Bg3[ which(BL18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_Bg3[ which(BL18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_Bg2 <- rbind(BL18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
BL18_Bft <- ftable(BL18_Bg2$player1, BL18_Bg2$player2)
BL18_Bft2 <- as.matrix(BL18_Bft)
numRows <- nrow(BL18_Bft2)
numCols <- ncol(BL18_Bft2)
BL18_Bft3 <- BL18_Bft2[c(2:numRows) , c(2:numCols)]
BL18_BTable <- graph.adjacency(BL18_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(BL18_BTable, vertex.label = V(BL18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
BL18_B.clusterCoef <- transitivity(BL18_BTable, type="global") #cluster coefficient
BL18_B.degreeCent <- centralization.degree(BL18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_Bftn <- as.network.matrix(BL18_Bft)
BL18_B.netDensity <- network.density(BL18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_B.entropy <- entropy(BL18_Bft) #entropy

BL18_B.netMx <- cbind(BL18_B.netMx, BL18_B.clusterCoef, BL18_B.degreeCent$centralization,
                      BL18_B.netDensity, BL18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "BL"
KIoutcome = "Stoppage_F"
BL18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
BL18_SFg2 <- data.frame(BL18_SF)
BL18_SFg2 <- BL18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_SFg2$player1
player2vector <- BL18_SFg2$player2
BL18_SFg3 <- BL18_SFg2
BL18_SFg3$p1inp2vec <- is.element(BL18_SFg3$player1, player2vector)
BL18_SFg3$p2inp1vec <- is.element(BL18_SFg3$player2, player1vector)

addPlayer1 <- BL18_SFg3[ which(BL18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_SFg3[ which(BL18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_SFg2 <- rbind(BL18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
BL18_SFft <- ftable(BL18_SFg2$player1, BL18_SFg2$player2)
BL18_SFft2 <- as.matrix(BL18_SFft)
numRows <- nrow(BL18_SFft2)
numCols <- ncol(BL18_SFft2)
BL18_SFft3 <- BL18_SFft2[c(2:numRows) , c(2:numCols)]
BL18_SFTable <- graph.adjacency(BL18_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(BL18_SFTable, vertex.label = V(BL18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
BL18_SF.clusterCoef <- transitivity(BL18_SFTable, type="global") #cluster coefficient
BL18_SF.degreeCent <- centralization.degree(BL18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_SFftn <- as.network.matrix(BL18_SFft)
BL18_SF.netDensity <- network.density(BL18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_SF.entropy <- entropy(BL18_SFft) #entropy

BL18_SF.netMx <- cbind(BL18_SF.netMx, BL18_SF.clusterCoef, BL18_SF.degreeCent$centralization,
                       BL18_SF.netDensity, BL18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "BL"
KIoutcome = "Turnover_F"
BL18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
BL18_TFg2 <- data.frame(BL18_TF)
BL18_TFg2 <- BL18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_TFg2$player1
player2vector <- BL18_TFg2$player2
BL18_TFg3 <- BL18_TFg2
BL18_TFg3$p1inp2vec <- is.element(BL18_TFg3$player1, player2vector)
BL18_TFg3$p2inp1vec <- is.element(BL18_TFg3$player2, player1vector)

addPlayer1 <- BL18_TFg3[ which(BL18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_TFg3[ which(BL18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_TFg2 <- rbind(BL18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
BL18_TFft <- ftable(BL18_TFg2$player1, BL18_TFg2$player2)
BL18_TFft2 <- as.matrix(BL18_TFft)
numRows <- nrow(BL18_TFft2)
numCols <- ncol(BL18_TFft2)
BL18_TFft3 <- BL18_TFft2[c(2:numRows) , c(2:numCols)]
BL18_TFTable <- graph.adjacency(BL18_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(BL18_TFTable, vertex.label = V(BL18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
BL18_TF.clusterCoef <- transitivity(BL18_TFTable, type="global") #cluster coefficient
BL18_TF.degreeCent <- centralization.degree(BL18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_TFftn <- as.network.matrix(BL18_TFft)
BL18_TF.netDensity <- network.density(BL18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_TF.entropy <- entropy(BL18_TFft) #entropy

BL18_TF.netMx <- cbind(BL18_TF.netMx, BL18_TF.clusterCoef, BL18_TF.degreeCent$centralization,
                       BL18_TF.netDensity, BL18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
BL18_SAMg2 <- data.frame(BL18_SAM)
BL18_SAMg2 <- BL18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_SAMg2$player1
player2vector <- BL18_SAMg2$player2
BL18_SAMg3 <- BL18_SAMg2
BL18_SAMg3$p1inp2vec <- is.element(BL18_SAMg3$player1, player2vector)
BL18_SAMg3$p2inp1vec <- is.element(BL18_SAMg3$player2, player1vector)

addPlayer1 <- BL18_SAMg3[ which(BL18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_SAMg3[ which(BL18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_SAMg2 <- rbind(BL18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
BL18_SAMft <- ftable(BL18_SAMg2$player1, BL18_SAMg2$player2)
BL18_SAMft2 <- as.matrix(BL18_SAMft)
numRows <- nrow(BL18_SAMft2)
numCols <- ncol(BL18_SAMft2)
BL18_SAMft3 <- BL18_SAMft2[c(2:numRows) , c(2:numCols)]
BL18_SAMTable <- graph.adjacency(BL18_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(BL18_SAMTable, vertex.label = V(BL18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
BL18_SAM.clusterCoef <- transitivity(BL18_SAMTable, type="global") #cluster coefficient
BL18_SAM.degreeCent <- centralization.degree(BL18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_SAMftn <- as.network.matrix(BL18_SAMft)
BL18_SAM.netDensity <- network.density(BL18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_SAM.entropy <- entropy(BL18_SAMft) #entropy

BL18_SAM.netMx <- cbind(BL18_SAM.netMx, BL18_SAM.clusterCoef, BL18_SAM.degreeCent$centralization,
                        BL18_SAM.netDensity, BL18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "BL"
KIoutcome = "Turnover_AM"
BL18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
BL18_TAMg2 <- data.frame(BL18_TAM)
BL18_TAMg2 <- BL18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_TAMg2$player1
player2vector <- BL18_TAMg2$player2
BL18_TAMg3 <- BL18_TAMg2
BL18_TAMg3$p1inp2vec <- is.element(BL18_TAMg3$player1, player2vector)
BL18_TAMg3$p2inp1vec <- is.element(BL18_TAMg3$player2, player1vector)

addPlayer1 <- BL18_TAMg3[ which(BL18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_TAMg3[ which(BL18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_TAMg2 <- rbind(BL18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
BL18_TAMft <- ftable(BL18_TAMg2$player1, BL18_TAMg2$player2)
BL18_TAMft2 <- as.matrix(BL18_TAMft)
numRows <- nrow(BL18_TAMft2)
numCols <- ncol(BL18_TAMft2)
BL18_TAMft3 <- BL18_TAMft2[c(2:numRows) , c(2:numCols)]
BL18_TAMTable <- graph.adjacency(BL18_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(BL18_TAMTable, vertex.label = V(BL18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
BL18_TAM.clusterCoef <- transitivity(BL18_TAMTable, type="global") #cluster coefficient
BL18_TAM.degreeCent <- centralization.degree(BL18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_TAMftn <- as.network.matrix(BL18_TAMft)
BL18_TAM.netDensity <- network.density(BL18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_TAM.entropy <- entropy(BL18_TAMft) #entropy

BL18_TAM.netMx <- cbind(BL18_TAM.netMx, BL18_TAM.clusterCoef, BL18_TAM.degreeCent$centralization,
                        BL18_TAM.netDensity, BL18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
BL18_SDMg2 <- data.frame(BL18_SDM)
BL18_SDMg2 <- BL18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_SDMg2$player1
player2vector <- BL18_SDMg2$player2
BL18_SDMg3 <- BL18_SDMg2
BL18_SDMg3$p1inp2vec <- is.element(BL18_SDMg3$player1, player2vector)
BL18_SDMg3$p2inp1vec <- is.element(BL18_SDMg3$player2, player1vector)

addPlayer1 <- BL18_SDMg3[ which(BL18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_SDMg3[ which(BL18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_SDMg2 <- rbind(BL18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
BL18_SDMft <- ftable(BL18_SDMg2$player1, BL18_SDMg2$player2)
BL18_SDMft2 <- as.matrix(BL18_SDMft)
numRows <- nrow(BL18_SDMft2)
numCols <- ncol(BL18_SDMft2)
BL18_SDMft3 <- BL18_SDMft2[c(2:numRows) , c(2:numCols)]
BL18_SDMTable <- graph.adjacency(BL18_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(BL18_SDMTable, vertex.label = V(BL18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
BL18_SDM.clusterCoef <- transitivity(BL18_SDMTable, type="global") #cluster coefficient
BL18_SDM.degreeCent <- centralization.degree(BL18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_SDMftn <- as.network.matrix(BL18_SDMft)
BL18_SDM.netDensity <- network.density(BL18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_SDM.entropy <- entropy(BL18_SDMft) #entropy

BL18_SDM.netMx <- cbind(BL18_SDM.netMx, BL18_SDM.clusterCoef, BL18_SDM.degreeCent$centralization,
                        BL18_SDM.netDensity, BL18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "BL"
KIoutcome = "Turnover_DM"
BL18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
BL18_TDMg2 <- data.frame(BL18_TDM)
BL18_TDMg2 <- BL18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_TDMg2$player1
player2vector <- BL18_TDMg2$player2
BL18_TDMg3 <- BL18_TDMg2
BL18_TDMg3$p1inp2vec <- is.element(BL18_TDMg3$player1, player2vector)
BL18_TDMg3$p2inp1vec <- is.element(BL18_TDMg3$player2, player1vector)

addPlayer1 <- BL18_TDMg3[ which(BL18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_TDMg3[ which(BL18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_TDMg2 <- rbind(BL18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
BL18_TDMft <- ftable(BL18_TDMg2$player1, BL18_TDMg2$player2)
BL18_TDMft2 <- as.matrix(BL18_TDMft)
numRows <- nrow(BL18_TDMft2)
numCols <- ncol(BL18_TDMft2)
BL18_TDMft3 <- BL18_TDMft2[c(2:numRows) , c(2:numCols)]
BL18_TDMTable <- graph.adjacency(BL18_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(BL18_TDMTable, vertex.label = V(BL18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
BL18_TDM.clusterCoef <- transitivity(BL18_TDMTable, type="global") #cluster coefficient
BL18_TDM.degreeCent <- centralization.degree(BL18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_TDMftn <- as.network.matrix(BL18_TDMft)
BL18_TDM.netDensity <- network.density(BL18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_TDM.entropy <- entropy(BL18_TDMft) #entropy

BL18_TDM.netMx <- cbind(BL18_TDM.netMx, BL18_TDM.clusterCoef, BL18_TDM.degreeCent$centralization,
                        BL18_TDM.netDensity, BL18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "BL"
KIoutcome = "Stoppage_D"
BL18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
BL18_SDg2 <- data.frame(BL18_SD)
BL18_SDg2 <- BL18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_SDg2$player1
player2vector <- BL18_SDg2$player2
BL18_SDg3 <- BL18_SDg2
BL18_SDg3$p1inp2vec <- is.element(BL18_SDg3$player1, player2vector)
BL18_SDg3$p2inp1vec <- is.element(BL18_SDg3$player2, player1vector)

addPlayer1 <- BL18_SDg3[ which(BL18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_SDg3[ which(BL18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_SDg2 <- rbind(BL18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
BL18_SDft <- ftable(BL18_SDg2$player1, BL18_SDg2$player2)
BL18_SDft2 <- as.matrix(BL18_SDft)
numRows <- nrow(BL18_SDft2)
numCols <- ncol(BL18_SDft2)
BL18_SDft3 <- BL18_SDft2[c(2:numRows) , c(2:numCols)]
BL18_SDTable <- graph.adjacency(BL18_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(BL18_SDTable, vertex.label = V(BL18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
BL18_SD.clusterCoef <- transitivity(BL18_SDTable, type="global") #cluster coefficient
BL18_SD.degreeCent <- centralization.degree(BL18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_SDftn <- as.network.matrix(BL18_SDft)
BL18_SD.netDensity <- network.density(BL18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_SD.entropy <- entropy(BL18_SDft) #entropy

BL18_SD.netMx <- cbind(BL18_SD.netMx, BL18_SD.clusterCoef, BL18_SD.degreeCent$centralization,
                       BL18_SD.netDensity, BL18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "BL"
KIoutcome = "Turnover_D"
BL18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
BL18_TDg2 <- data.frame(BL18_TD)
BL18_TDg2 <- BL18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_TDg2$player1
player2vector <- BL18_TDg2$player2
BL18_TDg3 <- BL18_TDg2
BL18_TDg3$p1inp2vec <- is.element(BL18_TDg3$player1, player2vector)
BL18_TDg3$p2inp1vec <- is.element(BL18_TDg3$player2, player1vector)

addPlayer1 <- BL18_TDg3[ which(BL18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_TDg3[ which(BL18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_TDg2 <- rbind(BL18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
BL18_TDft <- ftable(BL18_TDg2$player1, BL18_TDg2$player2)
BL18_TDft2 <- as.matrix(BL18_TDft)
numRows <- nrow(BL18_TDft2)
numCols <- ncol(BL18_TDft2)
BL18_TDft3 <- BL18_TDft2[c(2:numRows) , c(2:numCols)]
BL18_TDTable <- graph.adjacency(BL18_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(BL18_TDTable, vertex.label = V(BL18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
BL18_TD.clusterCoef <- transitivity(BL18_TDTable, type="global") #cluster coefficient
BL18_TD.degreeCent <- centralization.degree(BL18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_TDftn <- as.network.matrix(BL18_TDft)
BL18_TD.netDensity <- network.density(BL18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_TD.entropy <- entropy(BL18_TDft) #entropy

BL18_TD.netMx <- cbind(BL18_TD.netMx, BL18_TD.clusterCoef, BL18_TD.degreeCent$centralization,
                       BL18_TD.netDensity, BL18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
BL18_QTg2 <- data.frame(BL18_QT)
BL18_QTg2 <- BL18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL18_QTg2$player1
player2vector <- BL18_QTg2$player2
BL18_QTg3 <- BL18_QTg2
BL18_QTg3$p1inp2vec <- is.element(BL18_QTg3$player1, player2vector)
BL18_QTg3$p2inp1vec <- is.element(BL18_QTg3$player2, player1vector)

addPlayer1 <- BL18_QTg3[ which(BL18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL18_QTg3[ which(BL18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL18_QTg2 <- rbind(BL18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
BL18_QTft <- ftable(BL18_QTg2$player1, BL18_QTg2$player2)
BL18_QTft2 <- as.matrix(BL18_QTft)
numRows <- nrow(BL18_QTft2)
numCols <- ncol(BL18_QTft2)
BL18_QTft3 <- BL18_QTft2[c(2:numRows) , c(2:numCols)]
BL18_QTTable <- graph.adjacency(BL18_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(BL18_QTTable, vertex.label = V(BL18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
BL18_QT.clusterCoef <- transitivity(BL18_QTTable, type="global") #cluster coefficient
BL18_QT.degreeCent <- centralization.degree(BL18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL18_QTftn <- as.network.matrix(BL18_QTft)
BL18_QT.netDensity <- network.density(BL18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL18_QT.entropy <- entropy(BL18_QTft) #entropy

BL18_QT.netMx <- cbind(BL18_QT.netMx, BL18_QT.clusterCoef, BL18_QT.degreeCent$centralization,
                       BL18_QT.netDensity, BL18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL18_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "CARL"
KIoutcome = "Goal_F"
CARL18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
CARL18_Gg2 <- data.frame(CARL18_G)
CARL18_Gg2 <- CARL18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_Gg2$player1
player2vector <- CARL18_Gg2$player2
CARL18_Gg3 <- CARL18_Gg2
CARL18_Gg3$p1inp2vec <- is.element(CARL18_Gg3$player1, player2vector)
CARL18_Gg3$p2inp1vec <- is.element(CARL18_Gg3$player2, player1vector)

addPlayer1 <- CARL18_Gg3[ which(CARL18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_Gg3[ which(CARL18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_Gg2 <- rbind(CARL18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
CARL18_Gft <- ftable(CARL18_Gg2$player1, CARL18_Gg2$player2)
CARL18_Gft2 <- as.matrix(CARL18_Gft)
numRows <- nrow(CARL18_Gft2)
numCols <- ncol(CARL18_Gft2)
CARL18_Gft3 <- CARL18_Gft2[c(2:numRows) , c(2:numCols)]
CARL18_GTable <- graph.adjacency(CARL18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(CARL18_GTable, vertex.label = V(CARL18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
CARL18_G.clusterCoef <- transitivity(CARL18_GTable, type="global") #cluster coefficient
CARL18_G.degreeCent <- centralization.degree(CARL18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_Gftn <- as.network.matrix(CARL18_Gft)
CARL18_G.netDensity <- network.density(CARL18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_G.entropy <- entropy(CARL18_Gft) #entropy

CARL18_G.netMx <- cbind(CARL18_G.netMx, CARL18_G.clusterCoef, CARL18_G.degreeCent$centralization,
                        CARL18_G.netDensity, CARL18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "CARL"
KIoutcome = "Behind_F"
CARL18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
CARL18_Bg2 <- data.frame(CARL18_B)
CARL18_Bg2 <- CARL18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_Bg2$player1
player2vector <- CARL18_Bg2$player2
CARL18_Bg3 <- CARL18_Bg2
CARL18_Bg3$p1inp2vec <- is.element(CARL18_Bg3$player1, player2vector)
CARL18_Bg3$p2inp1vec <- is.element(CARL18_Bg3$player2, player1vector)

addPlayer1 <- CARL18_Bg3[ which(CARL18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_Bg3[ which(CARL18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_Bg2 <- rbind(CARL18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
CARL18_Bft <- ftable(CARL18_Bg2$player1, CARL18_Bg2$player2)
CARL18_Bft2 <- as.matrix(CARL18_Bft)
numRows <- nrow(CARL18_Bft2)
numCols <- ncol(CARL18_Bft2)
CARL18_Bft3 <- CARL18_Bft2[c(2:numRows) , c(2:numCols)]
CARL18_BTable <- graph.adjacency(CARL18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(CARL18_BTable, vertex.label = V(CARL18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
CARL18_B.clusterCoef <- transitivity(CARL18_BTable, type="global") #cluster coefficient
CARL18_B.degreeCent <- centralization.degree(CARL18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_Bftn <- as.network.matrix(CARL18_Bft)
CARL18_B.netDensity <- network.density(CARL18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_B.entropy <- entropy(CARL18_Bft) #entropy

CARL18_B.netMx <- cbind(CARL18_B.netMx, CARL18_B.clusterCoef, CARL18_B.degreeCent$centralization,
                        CARL18_B.netDensity, CARL18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
CARL18_SFg2 <- data.frame(CARL18_SF)
CARL18_SFg2 <- CARL18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_SFg2$player1
player2vector <- CARL18_SFg2$player2
CARL18_SFg3 <- CARL18_SFg2
CARL18_SFg3$p1inp2vec <- is.element(CARL18_SFg3$player1, player2vector)
CARL18_SFg3$p2inp1vec <- is.element(CARL18_SFg3$player2, player1vector)

addPlayer1 <- CARL18_SFg3[ which(CARL18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_SFg3[ which(CARL18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_SFg2 <- rbind(CARL18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
CARL18_SFft <- ftable(CARL18_SFg2$player1, CARL18_SFg2$player2)
CARL18_SFft2 <- as.matrix(CARL18_SFft)
numRows <- nrow(CARL18_SFft2)
numCols <- ncol(CARL18_SFft2)
CARL18_SFft3 <- CARL18_SFft2[c(2:numRows) , c(2:numCols)]
CARL18_SFTable <- graph.adjacency(CARL18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(CARL18_SFTable, vertex.label = V(CARL18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
CARL18_SF.clusterCoef <- transitivity(CARL18_SFTable, type="global") #cluster coefficient
CARL18_SF.degreeCent <- centralization.degree(CARL18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_SFftn <- as.network.matrix(CARL18_SFft)
CARL18_SF.netDensity <- network.density(CARL18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_SF.entropy <- entropy(CARL18_SFft) #entropy

CARL18_SF.netMx <- cbind(CARL18_SF.netMx, CARL18_SF.clusterCoef, CARL18_SF.degreeCent$centralization,
                         CARL18_SF.netDensity, CARL18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
CARL18_TFg2 <- data.frame(CARL18_TF)
CARL18_TFg2 <- CARL18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_TFg2$player1
player2vector <- CARL18_TFg2$player2
CARL18_TFg3 <- CARL18_TFg2
CARL18_TFg3$p1inp2vec <- is.element(CARL18_TFg3$player1, player2vector)
CARL18_TFg3$p2inp1vec <- is.element(CARL18_TFg3$player2, player1vector)

addPlayer1 <- CARL18_TFg3[ which(CARL18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_TFg3[ which(CARL18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_TFg2 <- rbind(CARL18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
CARL18_TFft <- ftable(CARL18_TFg2$player1, CARL18_TFg2$player2)
CARL18_TFft2 <- as.matrix(CARL18_TFft)
numRows <- nrow(CARL18_TFft2)
numCols <- ncol(CARL18_TFft2)
CARL18_TFft3 <- CARL18_TFft2[c(2:numRows) , c(2:numCols)]
CARL18_TFTable <- graph.adjacency(CARL18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(CARL18_TFTable, vertex.label = V(CARL18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
CARL18_TF.clusterCoef <- transitivity(CARL18_TFTable, type="global") #cluster coefficient
CARL18_TF.degreeCent <- centralization.degree(CARL18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_TFftn <- as.network.matrix(CARL18_TFft)
CARL18_TF.netDensity <- network.density(CARL18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_TF.entropy <- entropy(CARL18_TFft) #entropy

CARL18_TF.netMx <- cbind(CARL18_TF.netMx, CARL18_TF.clusterCoef, CARL18_TF.degreeCent$centralization,
                         CARL18_TF.netDensity, CARL18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
CARL18_SAMg2 <- data.frame(CARL18_SAM)
CARL18_SAMg2 <- CARL18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_SAMg2$player1
player2vector <- CARL18_SAMg2$player2
CARL18_SAMg3 <- CARL18_SAMg2
CARL18_SAMg3$p1inp2vec <- is.element(CARL18_SAMg3$player1, player2vector)
CARL18_SAMg3$p2inp1vec <- is.element(CARL18_SAMg3$player2, player1vector)

addPlayer1 <- CARL18_SAMg3[ which(CARL18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_SAMg3[ which(CARL18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_SAMg2 <- rbind(CARL18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
CARL18_SAMft <- ftable(CARL18_SAMg2$player1, CARL18_SAMg2$player2)
CARL18_SAMft2 <- as.matrix(CARL18_SAMft)
numRows <- nrow(CARL18_SAMft2)
numCols <- ncol(CARL18_SAMft2)
CARL18_SAMft3 <- CARL18_SAMft2[c(2:numRows) , c(2:numCols)]
CARL18_SAMTable <- graph.adjacency(CARL18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(CARL18_SAMTable, vertex.label = V(CARL18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
CARL18_SAM.clusterCoef <- transitivity(CARL18_SAMTable, type="global") #cluster coefficient
CARL18_SAM.degreeCent <- centralization.degree(CARL18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_SAMftn <- as.network.matrix(CARL18_SAMft)
CARL18_SAM.netDensity <- network.density(CARL18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_SAM.entropy <- entropy(CARL18_SAMft) #entropy

CARL18_SAM.netMx <- cbind(CARL18_SAM.netMx, CARL18_SAM.clusterCoef, CARL18_SAM.degreeCent$centralization,
                          CARL18_SAM.netDensity, CARL18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
CARL18_TAMg2 <- data.frame(CARL18_TAM)
CARL18_TAMg2 <- CARL18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_TAMg2$player1
player2vector <- CARL18_TAMg2$player2
CARL18_TAMg3 <- CARL18_TAMg2
CARL18_TAMg3$p1inp2vec <- is.element(CARL18_TAMg3$player1, player2vector)
CARL18_TAMg3$p2inp1vec <- is.element(CARL18_TAMg3$player2, player1vector)

addPlayer1 <- CARL18_TAMg3[ which(CARL18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL18_TAMg3[ which(CARL18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_TAMg2 <- rbind(CARL18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
CARL18_TAMft <- ftable(CARL18_TAMg2$player1, CARL18_TAMg2$player2)
CARL18_TAMft2 <- as.matrix(CARL18_TAMft)
numRows <- nrow(CARL18_TAMft2)
numCols <- ncol(CARL18_TAMft2)
CARL18_TAMft3 <- CARL18_TAMft2[c(2:numRows) , c(2:numCols)]
CARL18_TAMTable <- graph.adjacency(CARL18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(CARL18_TAMTable, vertex.label = V(CARL18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
CARL18_TAM.clusterCoef <- transitivity(CARL18_TAMTable, type="global") #cluster coefficient
CARL18_TAM.degreeCent <- centralization.degree(CARL18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_TAMftn <- as.network.matrix(CARL18_TAMft)
CARL18_TAM.netDensity <- network.density(CARL18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_TAM.entropy <- entropy(CARL18_TAMft) #entropy

CARL18_TAM.netMx <- cbind(CARL18_TAM.netMx, CARL18_TAM.clusterCoef, CARL18_TAM.degreeCent$centralization,
                          CARL18_TAM.netDensity, CARL18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
CARL18_SDMg2 <- data.frame(CARL18_SDM)
CARL18_SDMg2 <- CARL18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_SDMg2$player1
player2vector <- CARL18_SDMg2$player2
CARL18_SDMg3 <- CARL18_SDMg2
CARL18_SDMg3$p1inp2vec <- is.element(CARL18_SDMg3$player1, player2vector)
CARL18_SDMg3$p2inp1vec <- is.element(CARL18_SDMg3$player2, player1vector)

addPlayer1 <- CARL18_SDMg3[ which(CARL18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_SDMg3[ which(CARL18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_SDMg2 <- rbind(CARL18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
CARL18_SDMft <- ftable(CARL18_SDMg2$player1, CARL18_SDMg2$player2)
CARL18_SDMft2 <- as.matrix(CARL18_SDMft)
numRows <- nrow(CARL18_SDMft2)
numCols <- ncol(CARL18_SDMft2)
CARL18_SDMft3 <- CARL18_SDMft2[c(2:numRows) , c(2:numCols)]
CARL18_SDMTable <- graph.adjacency(CARL18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(CARL18_SDMTable, vertex.label = V(CARL18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
CARL18_SDM.clusterCoef <- transitivity(CARL18_SDMTable, type="global") #cluster coefficient
CARL18_SDM.degreeCent <- centralization.degree(CARL18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_SDMftn <- as.network.matrix(CARL18_SDMft)
CARL18_SDM.netDensity <- network.density(CARL18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_SDM.entropy <- entropy(CARL18_SDMft) #entropy

CARL18_SDM.netMx <- cbind(CARL18_SDM.netMx, CARL18_SDM.clusterCoef, CARL18_SDM.degreeCent$centralization,
                          CARL18_SDM.netDensity, CARL18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
CARL18_TDMg2 <- data.frame(CARL18_TDM)
CARL18_TDMg2 <- CARL18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_TDMg2$player1
player2vector <- CARL18_TDMg2$player2
CARL18_TDMg3 <- CARL18_TDMg2
CARL18_TDMg3$p1inp2vec <- is.element(CARL18_TDMg3$player1, player2vector)
CARL18_TDMg3$p2inp1vec <- is.element(CARL18_TDMg3$player2, player1vector)

addPlayer1 <- CARL18_TDMg3[ which(CARL18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_TDMg3[ which(CARL18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_TDMg2 <- rbind(CARL18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
CARL18_TDMft <- ftable(CARL18_TDMg2$player1, CARL18_TDMg2$player2)
CARL18_TDMft2 <- as.matrix(CARL18_TDMft)
numRows <- nrow(CARL18_TDMft2)
numCols <- ncol(CARL18_TDMft2)
CARL18_TDMft3 <- CARL18_TDMft2[c(2:numRows) , c(2:numCols)]
CARL18_TDMTable <- graph.adjacency(CARL18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(CARL18_TDMTable, vertex.label = V(CARL18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
CARL18_TDM.clusterCoef <- transitivity(CARL18_TDMTable, type="global") #cluster coefficient
CARL18_TDM.degreeCent <- centralization.degree(CARL18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_TDMftn <- as.network.matrix(CARL18_TDMft)
CARL18_TDM.netDensity <- network.density(CARL18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_TDM.entropy <- entropy(CARL18_TDMft) #entropy

CARL18_TDM.netMx <- cbind(CARL18_TDM.netMx, CARL18_TDM.clusterCoef, CARL18_TDM.degreeCent$centralization,
                          CARL18_TDM.netDensity, CARL18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
CARL18_SDg2 <- data.frame(CARL18_SD)
CARL18_SDg2 <- CARL18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_SDg2$player1
player2vector <- CARL18_SDg2$player2
CARL18_SDg3 <- CARL18_SDg2
CARL18_SDg3$p1inp2vec <- is.element(CARL18_SDg3$player1, player2vector)
CARL18_SDg3$p2inp1vec <- is.element(CARL18_SDg3$player2, player1vector)

addPlayer1 <- CARL18_SDg3[ which(CARL18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_SDg3[ which(CARL18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_SDg2 <- rbind(CARL18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
CARL18_SDft <- ftable(CARL18_SDg2$player1, CARL18_SDg2$player2)
CARL18_SDft2 <- as.matrix(CARL18_SDft)
numRows <- nrow(CARL18_SDft2)
numCols <- ncol(CARL18_SDft2)
CARL18_SDft3 <- CARL18_SDft2[c(2:numRows) , c(2:numCols)]
CARL18_SDTable <- graph.adjacency(CARL18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(CARL18_SDTable, vertex.label = V(CARL18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
CARL18_SD.clusterCoef <- transitivity(CARL18_SDTable, type="global") #cluster coefficient
CARL18_SD.degreeCent <- centralization.degree(CARL18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_SDftn <- as.network.matrix(CARL18_SDft)
CARL18_SD.netDensity <- network.density(CARL18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_SD.entropy <- entropy(CARL18_SDft) #entropy

CARL18_SD.netMx <- cbind(CARL18_SD.netMx, CARL18_SD.clusterCoef, CARL18_SD.degreeCent$centralization,
                         CARL18_SD.netDensity, CARL18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
CARL18_TDg2 <- data.frame(CARL18_TD)
CARL18_TDg2 <- CARL18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_TDg2$player1
player2vector <- CARL18_TDg2$player2
CARL18_TDg3 <- CARL18_TDg2
CARL18_TDg3$p1inp2vec <- is.element(CARL18_TDg3$player1, player2vector)
CARL18_TDg3$p2inp1vec <- is.element(CARL18_TDg3$player2, player1vector)

addPlayer1 <- CARL18_TDg3[ which(CARL18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_TDg3[ which(CARL18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_TDg2 <- rbind(CARL18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
CARL18_TDft <- ftable(CARL18_TDg2$player1, CARL18_TDg2$player2)
CARL18_TDft2 <- as.matrix(CARL18_TDft)
numRows <- nrow(CARL18_TDft2)
numCols <- ncol(CARL18_TDft2)
CARL18_TDft3 <- CARL18_TDft2[c(2:numRows) , c(2:numCols)]
CARL18_TDTable <- graph.adjacency(CARL18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(CARL18_TDTable, vertex.label = V(CARL18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
CARL18_TD.clusterCoef <- transitivity(CARL18_TDTable, type="global") #cluster coefficient
CARL18_TD.degreeCent <- centralization.degree(CARL18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_TDftn <- as.network.matrix(CARL18_TDft)
CARL18_TD.netDensity <- network.density(CARL18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_TD.entropy <- entropy(CARL18_TDft) #entropy

CARL18_TD.netMx <- cbind(CARL18_TD.netMx, CARL18_TD.clusterCoef, CARL18_TD.degreeCent$centralization,
                         CARL18_TD.netDensity, CARL18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
CARL18_QTg2 <- data.frame(CARL18_QT)
CARL18_QTg2 <- CARL18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL18_QTg2$player1
player2vector <- CARL18_QTg2$player2
CARL18_QTg3 <- CARL18_QTg2
CARL18_QTg3$p1inp2vec <- is.element(CARL18_QTg3$player1, player2vector)
CARL18_QTg3$p2inp1vec <- is.element(CARL18_QTg3$player2, player1vector)

addPlayer1 <- CARL18_QTg3[ which(CARL18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL18_QTg3[ which(CARL18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL18_QTg2 <- rbind(CARL18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
CARL18_QTft <- ftable(CARL18_QTg2$player1, CARL18_QTg2$player2)
CARL18_QTft2 <- as.matrix(CARL18_QTft)
numRows <- nrow(CARL18_QTft2)
numCols <- ncol(CARL18_QTft2)
CARL18_QTft3 <- CARL18_QTft2[c(2:numRows) , c(2:numCols)]
CARL18_QTTable <- graph.adjacency(CARL18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(CARL18_QTTable, vertex.label = V(CARL18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
CARL18_QT.clusterCoef <- transitivity(CARL18_QTTable, type="global") #cluster coefficient
CARL18_QT.degreeCent <- centralization.degree(CARL18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL18_QTftn <- as.network.matrix(CARL18_QTft)
CARL18_QT.netDensity <- network.density(CARL18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL18_QT.entropy <- entropy(CARL18_QTft) #entropy

CARL18_QT.netMx <- cbind(CARL18_QT.netMx, CARL18_QT.clusterCoef, CARL18_QT.degreeCent$centralization,
                         CARL18_QT.netDensity, CARL18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL18_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Goal_F"
COLL18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
COLL18_Gg2 <- data.frame(COLL18_G)
COLL18_Gg2 <- COLL18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_Gg2$player1
player2vector <- COLL18_Gg2$player2
COLL18_Gg3 <- COLL18_Gg2
COLL18_Gg3$p1inp2vec <- is.element(COLL18_Gg3$player1, player2vector)
COLL18_Gg3$p2inp1vec <- is.element(COLL18_Gg3$player2, player1vector)

addPlayer1 <- COLL18_Gg3[ which(COLL18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_Gg3[ which(COLL18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_Gg2 <- rbind(COLL18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
COLL18_Gft <- ftable(COLL18_Gg2$player1, COLL18_Gg2$player2)
COLL18_Gft2 <- as.matrix(COLL18_Gft)
numRows <- nrow(COLL18_Gft2)
numCols <- ncol(COLL18_Gft2)
COLL18_Gft3 <- COLL18_Gft2[c(2:numRows) , c(2:numCols)]
COLL18_GTable <- graph.adjacency(COLL18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(COLL18_GTable, vertex.label = V(COLL18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
COLL18_G.clusterCoef <- transitivity(COLL18_GTable, type="global") #cluster coefficient
COLL18_G.degreeCent <- centralization.degree(COLL18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_Gftn <- as.network.matrix(COLL18_Gft)
COLL18_G.netDensity <- network.density(COLL18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_G.entropy <- entropy(COLL18_Gft) #entropy

COLL18_G.netMx <- cbind(COLL18_G.netMx, COLL18_G.clusterCoef, COLL18_G.degreeCent$centralization,
                        COLL18_G.netDensity, COLL18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Behind_F"
COLL18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
COLL18_Bg2 <- data.frame(COLL18_B)
COLL18_Bg2 <- COLL18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_Bg2$player1
player2vector <- COLL18_Bg2$player2
COLL18_Bg3 <- COLL18_Bg2
COLL18_Bg3$p1inp2vec <- is.element(COLL18_Bg3$player1, player2vector)
COLL18_Bg3$p2inp1vec <- is.element(COLL18_Bg3$player2, player1vector)

addPlayer1 <- COLL18_Bg3[ which(COLL18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_Bg3[ which(COLL18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_Bg2 <- rbind(COLL18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
COLL18_Bft <- ftable(COLL18_Bg2$player1, COLL18_Bg2$player2)
COLL18_Bft2 <- as.matrix(COLL18_Bft)
numRows <- nrow(COLL18_Bft2)
numCols <- ncol(COLL18_Bft2)
COLL18_Bft3 <- COLL18_Bft2[c(2:numRows) , c(2:numCols)]
COLL18_BTable <- graph.adjacency(COLL18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(COLL18_BTable, vertex.label = V(COLL18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
COLL18_B.clusterCoef <- transitivity(COLL18_BTable, type="global") #cluster coefficient
COLL18_B.degreeCent <- centralization.degree(COLL18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_Bftn <- as.network.matrix(COLL18_Bft)
COLL18_B.netDensity <- network.density(COLL18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_B.entropy <- entropy(COLL18_Bft) #entropy

COLL18_B.netMx <- cbind(COLL18_B.netMx, COLL18_B.clusterCoef, COLL18_B.degreeCent$centralization,
                        COLL18_B.netDensity, COLL18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
COLL18_SFg2 <- data.frame(COLL18_SF)
COLL18_SFg2 <- COLL18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_SFg2$player1
player2vector <- COLL18_SFg2$player2
COLL18_SFg3 <- COLL18_SFg2
COLL18_SFg3$p1inp2vec <- is.element(COLL18_SFg3$player1, player2vector)
COLL18_SFg3$p2inp1vec <- is.element(COLL18_SFg3$player2, player1vector)

addPlayer1 <- COLL18_SFg3[ which(COLL18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_SFg3[ which(COLL18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_SFg2 <- rbind(COLL18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
COLL18_SFft <- ftable(COLL18_SFg2$player1, COLL18_SFg2$player2)
COLL18_SFft2 <- as.matrix(COLL18_SFft)
numRows <- nrow(COLL18_SFft2)
numCols <- ncol(COLL18_SFft2)
COLL18_SFft3 <- COLL18_SFft2[c(2:numRows) , c(2:numCols)]
COLL18_SFTable <- graph.adjacency(COLL18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(COLL18_SFTable, vertex.label = V(COLL18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
COLL18_SF.clusterCoef <- transitivity(COLL18_SFTable, type="global") #cluster coefficient
COLL18_SF.degreeCent <- centralization.degree(COLL18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_SFftn <- as.network.matrix(COLL18_SFft)
COLL18_SF.netDensity <- network.density(COLL18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_SF.entropy <- entropy(COLL18_SFft) #entropy

COLL18_SF.netMx <- cbind(COLL18_SF.netMx, COLL18_SF.clusterCoef, COLL18_SF.degreeCent$centralization,
                         COLL18_SF.netDensity, COLL18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
COLL18_TFg2 <- data.frame(COLL18_TF)
COLL18_TFg2 <- COLL18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_TFg2$player1
player2vector <- COLL18_TFg2$player2
COLL18_TFg3 <- COLL18_TFg2
COLL18_TFg3$p1inp2vec <- is.element(COLL18_TFg3$player1, player2vector)
COLL18_TFg3$p2inp1vec <- is.element(COLL18_TFg3$player2, player1vector)

addPlayer1 <- COLL18_TFg3[ which(COLL18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL18_TFg3[ which(COLL18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_TFg2 <- rbind(COLL18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
COLL18_TFft <- ftable(COLL18_TFg2$player1, COLL18_TFg2$player2)
COLL18_TFft2 <- as.matrix(COLL18_TFft)
numRows <- nrow(COLL18_TFft2)
numCols <- ncol(COLL18_TFft2)
COLL18_TFft3 <- COLL18_TFft2[c(2:numRows) , c(2:numCols)]
COLL18_TFTable <- graph.adjacency(COLL18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(COLL18_TFTable, vertex.label = V(COLL18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
COLL18_TF.clusterCoef <- transitivity(COLL18_TFTable, type="global") #cluster coefficient
COLL18_TF.degreeCent <- centralization.degree(COLL18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_TFftn <- as.network.matrix(COLL18_TFft)
COLL18_TF.netDensity <- network.density(COLL18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_TF.entropy <- entropy(COLL18_TFft) #entropy

COLL18_TF.netMx <- cbind(COLL18_TF.netMx, COLL18_TF.clusterCoef, COLL18_TF.degreeCent$centralization,
                         COLL18_TF.netDensity, COLL18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
COLL18_SAMg2 <- data.frame(COLL18_SAM)
COLL18_SAMg2 <- COLL18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_SAMg2$player1
player2vector <- COLL18_SAMg2$player2
COLL18_SAMg3 <- COLL18_SAMg2
COLL18_SAMg3$p1inp2vec <- is.element(COLL18_SAMg3$player1, player2vector)
COLL18_SAMg3$p2inp1vec <- is.element(COLL18_SAMg3$player2, player1vector)

addPlayer1 <- COLL18_SAMg3[ which(COLL18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_SAMg3[ which(COLL18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_SAMg2 <- rbind(COLL18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
COLL18_SAMft <- ftable(COLL18_SAMg2$player1, COLL18_SAMg2$player2)
COLL18_SAMft2 <- as.matrix(COLL18_SAMft)
numRows <- nrow(COLL18_SAMft2)
numCols <- ncol(COLL18_SAMft2)
COLL18_SAMft3 <- COLL18_SAMft2[c(2:numRows) , c(2:numCols)]
COLL18_SAMTable <- graph.adjacency(COLL18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(COLL18_SAMTable, vertex.label = V(COLL18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
COLL18_SAM.clusterCoef <- transitivity(COLL18_SAMTable, type="global") #cluster coefficient
COLL18_SAM.degreeCent <- centralization.degree(COLL18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_SAMftn <- as.network.matrix(COLL18_SAMft)
COLL18_SAM.netDensity <- network.density(COLL18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_SAM.entropy <- entropy(COLL18_SAMft) #entropy

COLL18_SAM.netMx <- cbind(COLL18_SAM.netMx, COLL18_SAM.clusterCoef, COLL18_SAM.degreeCent$centralization,
                          COLL18_SAM.netDensity, COLL18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
COLL18_TAMg2 <- data.frame(COLL18_TAM)
COLL18_TAMg2 <- COLL18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_TAMg2$player1
player2vector <- COLL18_TAMg2$player2
COLL18_TAMg3 <- COLL18_TAMg2
COLL18_TAMg3$p1inp2vec <- is.element(COLL18_TAMg3$player1, player2vector)
COLL18_TAMg3$p2inp1vec <- is.element(COLL18_TAMg3$player2, player1vector)

addPlayer1 <- COLL18_TAMg3[ which(COLL18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_TAMg3[ which(COLL18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_TAMg2 <- rbind(COLL18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
COLL18_TAMft <- ftable(COLL18_TAMg2$player1, COLL18_TAMg2$player2)
COLL18_TAMft2 <- as.matrix(COLL18_TAMft)
numRows <- nrow(COLL18_TAMft2)
numCols <- ncol(COLL18_TAMft2)
COLL18_TAMft3 <- COLL18_TAMft2[c(2:numRows) , c(2:numCols)]
COLL18_TAMTable <- graph.adjacency(COLL18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(COLL18_TAMTable, vertex.label = V(COLL18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
COLL18_TAM.clusterCoef <- transitivity(COLL18_TAMTable, type="global") #cluster coefficient
COLL18_TAM.degreeCent <- centralization.degree(COLL18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_TAMftn <- as.network.matrix(COLL18_TAMft)
COLL18_TAM.netDensity <- network.density(COLL18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_TAM.entropy <- entropy(COLL18_TAMft) #entropy

COLL18_TAM.netMx <- cbind(COLL18_TAM.netMx, COLL18_TAM.clusterCoef, COLL18_TAM.degreeCent$centralization,
                          COLL18_TAM.netDensity, COLL18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
COLL18_SDMg2 <- data.frame(COLL18_SDM)
COLL18_SDMg2 <- COLL18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_SDMg2$player1
player2vector <- COLL18_SDMg2$player2
COLL18_SDMg3 <- COLL18_SDMg2
COLL18_SDMg3$p1inp2vec <- is.element(COLL18_SDMg3$player1, player2vector)
COLL18_SDMg3$p2inp1vec <- is.element(COLL18_SDMg3$player2, player1vector)

addPlayer1 <- COLL18_SDMg3[ which(COLL18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_SDMg3[ which(COLL18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_SDMg2 <- rbind(COLL18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
COLL18_SDMft <- ftable(COLL18_SDMg2$player1, COLL18_SDMg2$player2)
COLL18_SDMft2 <- as.matrix(COLL18_SDMft)
numRows <- nrow(COLL18_SDMft2)
numCols <- ncol(COLL18_SDMft2)
COLL18_SDMft3 <- COLL18_SDMft2[c(2:numRows) , c(2:numCols)]
COLL18_SDMTable <- graph.adjacency(COLL18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(COLL18_SDMTable, vertex.label = V(COLL18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
COLL18_SDM.clusterCoef <- transitivity(COLL18_SDMTable, type="global") #cluster coefficient
COLL18_SDM.degreeCent <- centralization.degree(COLL18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_SDMftn <- as.network.matrix(COLL18_SDMft)
COLL18_SDM.netDensity <- network.density(COLL18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_SDM.entropy <- entropy(COLL18_SDMft) #entropy

COLL18_SDM.netMx <- cbind(COLL18_SDM.netMx, COLL18_SDM.clusterCoef, COLL18_SDM.degreeCent$centralization,
                          COLL18_SDM.netDensity, COLL18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
COLL18_TDMg2 <- data.frame(COLL18_TDM)
COLL18_TDMg2 <- COLL18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_TDMg2$player1
player2vector <- COLL18_TDMg2$player2
COLL18_TDMg3 <- COLL18_TDMg2
COLL18_TDMg3$p1inp2vec <- is.element(COLL18_TDMg3$player1, player2vector)
COLL18_TDMg3$p2inp1vec <- is.element(COLL18_TDMg3$player2, player1vector)

addPlayer1 <- COLL18_TDMg3[ which(COLL18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_TDMg3[ which(COLL18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_TDMg2 <- rbind(COLL18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
COLL18_TDMft <- ftable(COLL18_TDMg2$player1, COLL18_TDMg2$player2)
COLL18_TDMft2 <- as.matrix(COLL18_TDMft)
numRows <- nrow(COLL18_TDMft2)
numCols <- ncol(COLL18_TDMft2)
COLL18_TDMft3 <- COLL18_TDMft2[c(2:numRows) , c(2:numCols)]
COLL18_TDMTable <- graph.adjacency(COLL18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(COLL18_TDMTable, vertex.label = V(COLL18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
COLL18_TDM.clusterCoef <- transitivity(COLL18_TDMTable, type="global") #cluster coefficient
COLL18_TDM.degreeCent <- centralization.degree(COLL18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_TDMftn <- as.network.matrix(COLL18_TDMft)
COLL18_TDM.netDensity <- network.density(COLL18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_TDM.entropy <- entropy(COLL18_TDMft) #entropy

COLL18_TDM.netMx <- cbind(COLL18_TDM.netMx, COLL18_TDM.clusterCoef, COLL18_TDM.degreeCent$centralization,
                          COLL18_TDM.netDensity, COLL18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
COLL18_SDg2 <- data.frame(COLL18_SD)
COLL18_SDg2 <- COLL18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_SDg2$player1
player2vector <- COLL18_SDg2$player2
COLL18_SDg3 <- COLL18_SDg2
COLL18_SDg3$p1inp2vec <- is.element(COLL18_SDg3$player1, player2vector)
COLL18_SDg3$p2inp1vec <- is.element(COLL18_SDg3$player2, player1vector)

addPlayer1 <- COLL18_SDg3[ which(COLL18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_SDg3[ which(COLL18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_SDg2 <- rbind(COLL18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
COLL18_SDft <- ftable(COLL18_SDg2$player1, COLL18_SDg2$player2)
COLL18_SDft2 <- as.matrix(COLL18_SDft)
numRows <- nrow(COLL18_SDft2)
numCols <- ncol(COLL18_SDft2)
COLL18_SDft3 <- COLL18_SDft2[c(2:numRows) , c(2:numCols)]
COLL18_SDTable <- graph.adjacency(COLL18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(COLL18_SDTable, vertex.label = V(COLL18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
COLL18_SD.clusterCoef <- transitivity(COLL18_SDTable, type="global") #cluster coefficient
COLL18_SD.degreeCent <- centralization.degree(COLL18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_SDftn <- as.network.matrix(COLL18_SDft)
COLL18_SD.netDensity <- network.density(COLL18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_SD.entropy <- entropy(COLL18_SDft) #entropy

COLL18_SD.netMx <- cbind(COLL18_SD.netMx, COLL18_SD.clusterCoef, COLL18_SD.degreeCent$centralization,
                         COLL18_SD.netDensity, COLL18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************

round = 18
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
COLL18_TDg2 <- data.frame(COLL18_TD)
COLL18_TDg2 <- COLL18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_TDg2$player1
player2vector <- COLL18_TDg2$player2
COLL18_TDg3 <- COLL18_TDg2
COLL18_TDg3$p1inp2vec <- is.element(COLL18_TDg3$player1, player2vector)
COLL18_TDg3$p2inp1vec <- is.element(COLL18_TDg3$player2, player1vector)

addPlayer1 <- COLL18_TDg3[ which(COLL18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_TDg3[ which(COLL18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_TDg2 <- rbind(COLL18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
COLL18_TDft <- ftable(COLL18_TDg2$player1, COLL18_TDg2$player2)
COLL18_TDft2 <- as.matrix(COLL18_TDft)
numRows <- nrow(COLL18_TDft2)
numCols <- ncol(COLL18_TDft2)
COLL18_TDft3 <- COLL18_TDft2[c(2:numRows) , c(2:numCols)]
COLL18_TDTable <- graph.adjacency(COLL18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(COLL18_TDTable, vertex.label = V(COLL18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
COLL18_TD.clusterCoef <- transitivity(COLL18_TDTable, type="global") #cluster coefficient
COLL18_TD.degreeCent <- centralization.degree(COLL18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_TDftn <- as.network.matrix(COLL18_TDft)
COLL18_TD.netDensity <- network.density(COLL18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_TD.entropy <- entropy(COLL18_TDft) #entropy

COLL18_TD.netMx <- cbind(COLL18_TD.netMx, COLL18_TD.clusterCoef, COLL18_TD.degreeCent$centralization,
                         COLL18_TD.netDensity, COLL18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
COLL18_QTg2 <- data.frame(COLL18_QT)
COLL18_QTg2 <- COLL18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL18_QTg2$player1
player2vector <- COLL18_QTg2$player2
COLL18_QTg3 <- COLL18_QTg2
COLL18_QTg3$p1inp2vec <- is.element(COLL18_QTg3$player1, player2vector)
COLL18_QTg3$p2inp1vec <- is.element(COLL18_QTg3$player2, player1vector)

addPlayer1 <- COLL18_QTg3[ which(COLL18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL18_QTg3[ which(COLL18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL18_QTg2 <- rbind(COLL18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
COLL18_QTft <- ftable(COLL18_QTg2$player1, COLL18_QTg2$player2)
COLL18_QTft2 <- as.matrix(COLL18_QTft)
numRows <- nrow(COLL18_QTft2)
numCols <- ncol(COLL18_QTft2)
COLL18_QTft3 <- COLL18_QTft2[c(2:numRows) , c(2:numCols)]
COLL18_QTTable <- graph.adjacency(COLL18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(COLL18_QTTable, vertex.label = V(COLL18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
COLL18_QT.clusterCoef <- transitivity(COLL18_QTTable, type="global") #cluster coefficient
COLL18_QT.degreeCent <- centralization.degree(COLL18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL18_QTftn <- as.network.matrix(COLL18_QTft)
COLL18_QT.netDensity <- network.density(COLL18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL18_QT.entropy <- entropy(COLL18_QTft) #entropy

COLL18_QT.netMx <- cbind(COLL18_QT.netMx, COLL18_QT.clusterCoef, COLL18_QT.degreeCent$centralization,
                         COLL18_QT.netDensity, COLL18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL18_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "Goal_F"
ESS18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
ESS18_Gg2 <- data.frame(ESS18_G)
ESS18_Gg2 <- ESS18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_Gg2$player1
player2vector <- ESS18_Gg2$player2
ESS18_Gg3 <- ESS18_Gg2
ESS18_Gg3$p1inp2vec <- is.element(ESS18_Gg3$player1, player2vector)
ESS18_Gg3$p2inp1vec <- is.element(ESS18_Gg3$player2, player1vector)

addPlayer1 <- ESS18_Gg3[ which(ESS18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_Gg3[ which(ESS18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_Gg2 <- rbind(ESS18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
ESS18_Gft <- ftable(ESS18_Gg2$player1, ESS18_Gg2$player2)
ESS18_Gft2 <- as.matrix(ESS18_Gft)
numRows <- nrow(ESS18_Gft2)
numCols <- ncol(ESS18_Gft2)
ESS18_Gft3 <- ESS18_Gft2[c(2:numRows) , c(2:numCols)]
ESS18_GTable <- graph.adjacency(ESS18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(ESS18_GTable, vertex.label = V(ESS18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
ESS18_G.clusterCoef <- transitivity(ESS18_GTable, type="global") #cluster coefficient
ESS18_G.degreeCent <- centralization.degree(ESS18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_Gftn <- as.network.matrix(ESS18_Gft)
ESS18_G.netDensity <- network.density(ESS18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_G.entropy <- entropy(ESS18_Gft) #entropy

ESS18_G.netMx <- cbind(ESS18_G.netMx, ESS18_G.clusterCoef, ESS18_G.degreeCent$centralization,
                       ESS18_G.netDensity, ESS18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "Behind_F"
ESS18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
ESS18_Bg2 <- data.frame(ESS18_B)
ESS18_Bg2 <- ESS18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_Bg2$player1
player2vector <- ESS18_Bg2$player2
ESS18_Bg3 <- ESS18_Bg2
ESS18_Bg3$p1inp2vec <- is.element(ESS18_Bg3$player1, player2vector)
ESS18_Bg3$p2inp1vec <- is.element(ESS18_Bg3$player2, player1vector)

addPlayer1 <- ESS18_Bg3[ which(ESS18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_Bg3[ which(ESS18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_Bg2 <- rbind(ESS18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
ESS18_Bft <- ftable(ESS18_Bg2$player1, ESS18_Bg2$player2)
ESS18_Bft2 <- as.matrix(ESS18_Bft)
numRows <- nrow(ESS18_Bft2)
numCols <- ncol(ESS18_Bft2)
ESS18_Bft3 <- ESS18_Bft2[c(2:numRows) , c(2:numCols)]
ESS18_BTable <- graph.adjacency(ESS18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(ESS18_BTable, vertex.label = V(ESS18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
ESS18_B.clusterCoef <- transitivity(ESS18_BTable, type="global") #cluster coefficient
ESS18_B.degreeCent <- centralization.degree(ESS18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_Bftn <- as.network.matrix(ESS18_Bft)
ESS18_B.netDensity <- network.density(ESS18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_B.entropy <- entropy(ESS18_Bft) #entropy

ESS18_B.netMx <- cbind(ESS18_B.netMx, ESS18_B.clusterCoef, ESS18_B.degreeCent$centralization,
                       ESS18_B.netDensity, ESS18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
ESS18_SFg2 <- data.frame(ESS18_SF)
ESS18_SFg2 <- ESS18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_SFg2$player1
player2vector <- ESS18_SFg2$player2
ESS18_SFg3 <- ESS18_SFg2
ESS18_SFg3$p1inp2vec <- is.element(ESS18_SFg3$player1, player2vector)
ESS18_SFg3$p2inp1vec <- is.element(ESS18_SFg3$player2, player1vector)

addPlayer1 <- ESS18_SFg3[ which(ESS18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_SFg3[ which(ESS18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_SFg2 <- rbind(ESS18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
ESS18_SFft <- ftable(ESS18_SFg2$player1, ESS18_SFg2$player2)
ESS18_SFft2 <- as.matrix(ESS18_SFft)
numRows <- nrow(ESS18_SFft2)
numCols <- ncol(ESS18_SFft2)
ESS18_SFft3 <- ESS18_SFft2[c(2:numRows) , c(2:numCols)]
ESS18_SFTable <- graph.adjacency(ESS18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(ESS18_SFTable, vertex.label = V(ESS18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
ESS18_SF.clusterCoef <- transitivity(ESS18_SFTable, type="global") #cluster coefficient
ESS18_SF.degreeCent <- centralization.degree(ESS18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_SFftn <- as.network.matrix(ESS18_SFft)
ESS18_SF.netDensity <- network.density(ESS18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_SF.entropy <- entropy(ESS18_SFft) #entropy

ESS18_SF.netMx <- cbind(ESS18_SF.netMx, ESS18_SF.clusterCoef, ESS18_SF.degreeCent$centralization,
                        ESS18_SF.netDensity, ESS18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
ESS18_TFg2 <- data.frame(ESS18_TF)
ESS18_TFg2 <- ESS18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_TFg2$player1
player2vector <- ESS18_TFg2$player2
ESS18_TFg3 <- ESS18_TFg2
ESS18_TFg3$p1inp2vec <- is.element(ESS18_TFg3$player1, player2vector)
ESS18_TFg3$p2inp1vec <- is.element(ESS18_TFg3$player2, player1vector)

addPlayer1 <- ESS18_TFg3[ which(ESS18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS18_TFg3[ which(ESS18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_TFg2 <- rbind(ESS18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
ESS18_TFft <- ftable(ESS18_TFg2$player1, ESS18_TFg2$player2)
ESS18_TFft2 <- as.matrix(ESS18_TFft)
numRows <- nrow(ESS18_TFft2)
numCols <- ncol(ESS18_TFft2)
ESS18_TFft3 <- ESS18_TFft2[c(2:numRows) , c(2:numCols)]
ESS18_TFTable <- graph.adjacency(ESS18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(ESS18_TFTable, vertex.label = V(ESS18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
ESS18_TF.clusterCoef <- transitivity(ESS18_TFTable, type="global") #cluster coefficient
ESS18_TF.degreeCent <- centralization.degree(ESS18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_TFftn <- as.network.matrix(ESS18_TFft)
ESS18_TF.netDensity <- network.density(ESS18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_TF.entropy <- entropy(ESS18_TFft) #entropy

ESS18_TF.netMx <- cbind(ESS18_TF.netMx, ESS18_TF.clusterCoef, ESS18_TF.degreeCent$centralization,
                        ESS18_TF.netDensity, ESS18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
ESS18_SAMg2 <- data.frame(ESS18_SAM)
ESS18_SAMg2 <- ESS18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_SAMg2$player1
player2vector <- ESS18_SAMg2$player2
ESS18_SAMg3 <- ESS18_SAMg2
ESS18_SAMg3$p1inp2vec <- is.element(ESS18_SAMg3$player1, player2vector)
ESS18_SAMg3$p2inp1vec <- is.element(ESS18_SAMg3$player2, player1vector)

addPlayer1 <- ESS18_SAMg3[ which(ESS18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_SAMg3[ which(ESS18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_SAMg2 <- rbind(ESS18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
ESS18_SAMft <- ftable(ESS18_SAMg2$player1, ESS18_SAMg2$player2)
ESS18_SAMft2 <- as.matrix(ESS18_SAMft)
numRows <- nrow(ESS18_SAMft2)
numCols <- ncol(ESS18_SAMft2)
ESS18_SAMft3 <- ESS18_SAMft2[c(2:numRows) , c(2:numCols)]
ESS18_SAMTable <- graph.adjacency(ESS18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(ESS18_SAMTable, vertex.label = V(ESS18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
ESS18_SAM.clusterCoef <- transitivity(ESS18_SAMTable, type="global") #cluster coefficient
ESS18_SAM.degreeCent <- centralization.degree(ESS18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_SAMftn <- as.network.matrix(ESS18_SAMft)
ESS18_SAM.netDensity <- network.density(ESS18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_SAM.entropy <- entropy(ESS18_SAMft) #entropy

ESS18_SAM.netMx <- cbind(ESS18_SAM.netMx, ESS18_SAM.clusterCoef, ESS18_SAM.degreeCent$centralization,
                         ESS18_SAM.netDensity, ESS18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
ESS18_TAMg2 <- data.frame(ESS18_TAM)
ESS18_TAMg2 <- ESS18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_TAMg2$player1
player2vector <- ESS18_TAMg2$player2
ESS18_TAMg3 <- ESS18_TAMg2
ESS18_TAMg3$p1inp2vec <- is.element(ESS18_TAMg3$player1, player2vector)
ESS18_TAMg3$p2inp1vec <- is.element(ESS18_TAMg3$player2, player1vector)

addPlayer1 <- ESS18_TAMg3[ which(ESS18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_TAMg3[ which(ESS18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_TAMg2 <- rbind(ESS18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
ESS18_TAMft <- ftable(ESS18_TAMg2$player1, ESS18_TAMg2$player2)
ESS18_TAMft2 <- as.matrix(ESS18_TAMft)
numRows <- nrow(ESS18_TAMft2)
numCols <- ncol(ESS18_TAMft2)
ESS18_TAMft3 <- ESS18_TAMft2[c(2:numRows) , c(2:numCols)]
ESS18_TAMTable <- graph.adjacency(ESS18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(ESS18_TAMTable, vertex.label = V(ESS18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
ESS18_TAM.clusterCoef <- transitivity(ESS18_TAMTable, type="global") #cluster coefficient
ESS18_TAM.degreeCent <- centralization.degree(ESS18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_TAMftn <- as.network.matrix(ESS18_TAMft)
ESS18_TAM.netDensity <- network.density(ESS18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_TAM.entropy <- entropy(ESS18_TAMft) #entropy

ESS18_TAM.netMx <- cbind(ESS18_TAM.netMx, ESS18_TAM.clusterCoef, ESS18_TAM.degreeCent$centralization,
                         ESS18_TAM.netDensity, ESS18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
ESS18_SDMg2 <- data.frame(ESS18_SDM)
ESS18_SDMg2 <- ESS18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_SDMg2$player1
player2vector <- ESS18_SDMg2$player2
ESS18_SDMg3 <- ESS18_SDMg2
ESS18_SDMg3$p1inp2vec <- is.element(ESS18_SDMg3$player1, player2vector)
ESS18_SDMg3$p2inp1vec <- is.element(ESS18_SDMg3$player2, player1vector)

addPlayer1 <- ESS18_SDMg3[ which(ESS18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_SDMg3[ which(ESS18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_SDMg2 <- rbind(ESS18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
ESS18_SDMft <- ftable(ESS18_SDMg2$player1, ESS18_SDMg2$player2)
ESS18_SDMft2 <- as.matrix(ESS18_SDMft)
numRows <- nrow(ESS18_SDMft2)
numCols <- ncol(ESS18_SDMft2)
ESS18_SDMft3 <- ESS18_SDMft2[c(2:numRows) , c(2:numCols)]
ESS18_SDMTable <- graph.adjacency(ESS18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(ESS18_SDMTable, vertex.label = V(ESS18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
ESS18_SDM.clusterCoef <- transitivity(ESS18_SDMTable, type="global") #cluster coefficient
ESS18_SDM.degreeCent <- centralization.degree(ESS18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_SDMftn <- as.network.matrix(ESS18_SDMft)
ESS18_SDM.netDensity <- network.density(ESS18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_SDM.entropy <- entropy(ESS18_SDMft) #entropy

ESS18_SDM.netMx <- cbind(ESS18_SDM.netMx, ESS18_SDM.clusterCoef, ESS18_SDM.degreeCent$centralization,
                         ESS18_SDM.netDensity, ESS18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
ESS18_TDMg2 <- data.frame(ESS18_TDM)
ESS18_TDMg2 <- ESS18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_TDMg2$player1
player2vector <- ESS18_TDMg2$player2
ESS18_TDMg3 <- ESS18_TDMg2
ESS18_TDMg3$p1inp2vec <- is.element(ESS18_TDMg3$player1, player2vector)
ESS18_TDMg3$p2inp1vec <- is.element(ESS18_TDMg3$player2, player1vector)

addPlayer1 <- ESS18_TDMg3[ which(ESS18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS18_TDMg3[ which(ESS18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_TDMg2 <- rbind(ESS18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
ESS18_TDMft <- ftable(ESS18_TDMg2$player1, ESS18_TDMg2$player2)
ESS18_TDMft2 <- as.matrix(ESS18_TDMft)
numRows <- nrow(ESS18_TDMft2)
numCols <- ncol(ESS18_TDMft2)
ESS18_TDMft3 <- ESS18_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS18_TDMTable <- graph.adjacency(ESS18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(ESS18_TDMTable, vertex.label = V(ESS18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
ESS18_TDM.clusterCoef <- transitivity(ESS18_TDMTable, type="global") #cluster coefficient
ESS18_TDM.degreeCent <- centralization.degree(ESS18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_TDMftn <- as.network.matrix(ESS18_TDMft)
ESS18_TDM.netDensity <- network.density(ESS18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_TDM.entropy <- entropy(ESS18_TDMft) #entropy

ESS18_TDM.netMx <- cbind(ESS18_TDM.netMx, ESS18_TDM.clusterCoef, ESS18_TDM.degreeCent$centralization,
                         ESS18_TDM.netDensity, ESS18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
ESS18_SDg2 <- data.frame(ESS18_SD)
ESS18_SDg2 <- ESS18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_SDg2$player1
player2vector <- ESS18_SDg2$player2
ESS18_SDg3 <- ESS18_SDg2
ESS18_SDg3$p1inp2vec <- is.element(ESS18_SDg3$player1, player2vector)
ESS18_SDg3$p2inp1vec <- is.element(ESS18_SDg3$player2, player1vector)

addPlayer1 <- ESS18_SDg3[ which(ESS18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_SDg3[ which(ESS18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_SDg2 <- rbind(ESS18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
ESS18_SDft <- ftable(ESS18_SDg2$player1, ESS18_SDg2$player2)
ESS18_SDft2 <- as.matrix(ESS18_SDft)
numRows <- nrow(ESS18_SDft2)
numCols <- ncol(ESS18_SDft2)
ESS18_SDft3 <- ESS18_SDft2[c(2:numRows) , c(2:numCols)]
ESS18_SDTable <- graph.adjacency(ESS18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(ESS18_SDTable, vertex.label = V(ESS18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
ESS18_SD.clusterCoef <- transitivity(ESS18_SDTable, type="global") #cluster coefficient
ESS18_SD.degreeCent <- centralization.degree(ESS18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_SDftn <- as.network.matrix(ESS18_SDft)
ESS18_SD.netDensity <- network.density(ESS18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_SD.entropy <- entropy(ESS18_SDft) #entropy

ESS18_SD.netMx <- cbind(ESS18_SD.netMx, ESS18_SD.clusterCoef, ESS18_SD.degreeCent$centralization,
                        ESS18_SD.netDensity, ESS18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
ESS18_TDg2 <- data.frame(ESS18_TD)
ESS18_TDg2 <- ESS18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_TDg2$player1
player2vector <- ESS18_TDg2$player2
ESS18_TDg3 <- ESS18_TDg2
ESS18_TDg3$p1inp2vec <- is.element(ESS18_TDg3$player1, player2vector)
ESS18_TDg3$p2inp1vec <- is.element(ESS18_TDg3$player2, player1vector)

addPlayer1 <- ESS18_TDg3[ which(ESS18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_TDg3[ which(ESS18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_TDg2 <- rbind(ESS18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
ESS18_TDft <- ftable(ESS18_TDg2$player1, ESS18_TDg2$player2)
ESS18_TDft2 <- as.matrix(ESS18_TDft)
numRows <- nrow(ESS18_TDft2)
numCols <- ncol(ESS18_TDft2)
ESS18_TDft3 <- ESS18_TDft2[c(2:numRows) , c(2:numCols)]
ESS18_TDTable <- graph.adjacency(ESS18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(ESS18_TDTable, vertex.label = V(ESS18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
ESS18_TD.clusterCoef <- transitivity(ESS18_TDTable, type="global") #cluster coefficient
ESS18_TD.degreeCent <- centralization.degree(ESS18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_TDftn <- as.network.matrix(ESS18_TDft)
ESS18_TD.netDensity <- network.density(ESS18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_TD.entropy <- entropy(ESS18_TDft) #entropy

ESS18_TD.netMx <- cbind(ESS18_TD.netMx, ESS18_TD.clusterCoef, ESS18_TD.degreeCent$centralization,
                        ESS18_TD.netDensity, ESS18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
ESS18_QTg2 <- data.frame(ESS18_QT)
ESS18_QTg2 <- ESS18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS18_QTg2$player1
player2vector <- ESS18_QTg2$player2
ESS18_QTg3 <- ESS18_QTg2
ESS18_QTg3$p1inp2vec <- is.element(ESS18_QTg3$player1, player2vector)
ESS18_QTg3$p2inp1vec <- is.element(ESS18_QTg3$player2, player1vector)

addPlayer1 <- ESS18_QTg3[ which(ESS18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS18_QTg3[ which(ESS18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS18_QTg2 <- rbind(ESS18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
ESS18_QTft <- ftable(ESS18_QTg2$player1, ESS18_QTg2$player2)
ESS18_QTft2 <- as.matrix(ESS18_QTft)
numRows <- nrow(ESS18_QTft2)
numCols <- ncol(ESS18_QTft2)
ESS18_QTft3 <- ESS18_QTft2[c(2:numRows) , c(2:numCols)]
ESS18_QTTable <- graph.adjacency(ESS18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(ESS18_QTTable, vertex.label = V(ESS18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
ESS18_QT.clusterCoef <- transitivity(ESS18_QTTable, type="global") #cluster coefficient
ESS18_QT.degreeCent <- centralization.degree(ESS18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS18_QTftn <- as.network.matrix(ESS18_QTft)
ESS18_QT.netDensity <- network.density(ESS18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS18_QT.entropy <- entropy(ESS18_QTft) #entropy

ESS18_QT.netMx <- cbind(ESS18_QT.netMx, ESS18_QT.clusterCoef, ESS18_QT.degreeCent$centralization,
                        ESS18_QT.netDensity, ESS18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS18_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "Goal_F"
FRE18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
FRE18_Gg2 <- data.frame(FRE18_G)
FRE18_Gg2 <- FRE18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_Gg2$player1
player2vector <- FRE18_Gg2$player2
FRE18_Gg3 <- FRE18_Gg2
FRE18_Gg3$p1inp2vec <- is.element(FRE18_Gg3$player1, player2vector)
FRE18_Gg3$p2inp1vec <- is.element(FRE18_Gg3$player2, player1vector)

addPlayer1 <- FRE18_Gg3[ which(FRE18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_Gg3[ which(FRE18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_Gg2 <- rbind(FRE18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
FRE18_Gft <- ftable(FRE18_Gg2$player1, FRE18_Gg2$player2)
FRE18_Gft2 <- as.matrix(FRE18_Gft)
numRows <- nrow(FRE18_Gft2)
numCols <- ncol(FRE18_Gft2)
FRE18_Gft3 <- FRE18_Gft2[c(2:numRows) , c(2:numCols)]
FRE18_GTable <- graph.adjacency(FRE18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(FRE18_GTable, vertex.label = V(FRE18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
FRE18_G.clusterCoef <- transitivity(FRE18_GTable, type="global") #cluster coefficient
FRE18_G.degreeCent <- centralization.degree(FRE18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_Gftn <- as.network.matrix(FRE18_Gft)
FRE18_G.netDensity <- network.density(FRE18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_G.entropy <- entropy(FRE18_Gft) #entropy

FRE18_G.netMx <- cbind(FRE18_G.netMx, FRE18_G.clusterCoef, FRE18_G.degreeCent$centralization,
                       FRE18_G.netDensity, FRE18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "FRE"
KIoutcome = "Behind_F"
FRE18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
FRE18_Bg2 <- data.frame(FRE18_B)
FRE18_Bg2 <- FRE18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_Bg2$player1
player2vector <- FRE18_Bg2$player2
FRE18_Bg3 <- FRE18_Bg2
FRE18_Bg3$p1inp2vec <- is.element(FRE18_Bg3$player1, player2vector)
FRE18_Bg3$p2inp1vec <- is.element(FRE18_Bg3$player2, player1vector)

addPlayer1 <- FRE18_Bg3[ which(FRE18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_Bg3[ which(FRE18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_Bg2 <- rbind(FRE18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
FRE18_Bft <- ftable(FRE18_Bg2$player1, FRE18_Bg2$player2)
FRE18_Bft2 <- as.matrix(FRE18_Bft)
numRows <- nrow(FRE18_Bft2)
numCols <- ncol(FRE18_Bft2)
FRE18_Bft3 <- FRE18_Bft2[c(2:numRows) , c(2:numCols)]
FRE18_BTable <- graph.adjacency(FRE18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(FRE18_BTable, vertex.label = V(FRE18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
FRE18_B.clusterCoef <- transitivity(FRE18_BTable, type="global") #cluster coefficient
FRE18_B.degreeCent <- centralization.degree(FRE18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_Bftn <- as.network.matrix(FRE18_Bft)
FRE18_B.netDensity <- network.density(FRE18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_B.entropy <- entropy(FRE18_Bft) #entropy

FRE18_B.netMx <- cbind(FRE18_B.netMx, FRE18_B.clusterCoef, FRE18_B.degreeCent$centralization,
                       FRE18_B.netDensity, FRE18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
FRE18_SFg2 <- data.frame(FRE18_SF)
FRE18_SFg2 <- FRE18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_SFg2$player1
player2vector <- FRE18_SFg2$player2
FRE18_SFg3 <- FRE18_SFg2
FRE18_SFg3$p1inp2vec <- is.element(FRE18_SFg3$player1, player2vector)
FRE18_SFg3$p2inp1vec <- is.element(FRE18_SFg3$player2, player1vector)

addPlayer1 <- FRE18_SFg3[ which(FRE18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_SFg3[ which(FRE18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_SFg2 <- rbind(FRE18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
FRE18_SFft <- ftable(FRE18_SFg2$player1, FRE18_SFg2$player2)
FRE18_SFft2 <- as.matrix(FRE18_SFft)
numRows <- nrow(FRE18_SFft2)
numCols <- ncol(FRE18_SFft2)
FRE18_SFft3 <- FRE18_SFft2[c(2:numRows) , c(2:numCols)]
FRE18_SFTable <- graph.adjacency(FRE18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(FRE18_SFTable, vertex.label = V(FRE18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
FRE18_SF.clusterCoef <- transitivity(FRE18_SFTable, type="global") #cluster coefficient
FRE18_SF.degreeCent <- centralization.degree(FRE18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_SFftn <- as.network.matrix(FRE18_SFft)
FRE18_SF.netDensity <- network.density(FRE18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_SF.entropy <- entropy(FRE18_SFft) #entropy

FRE18_SF.netMx <- cbind(FRE18_SF.netMx, FRE18_SF.clusterCoef, FRE18_SF.degreeCent$centralization,
                        FRE18_SF.netDensity, FRE18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
FRE18_TFg2 <- data.frame(FRE18_TF)
FRE18_TFg2 <- FRE18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_TFg2$player1
player2vector <- FRE18_TFg2$player2
FRE18_TFg3 <- FRE18_TFg2
FRE18_TFg3$p1inp2vec <- is.element(FRE18_TFg3$player1, player2vector)
FRE18_TFg3$p2inp1vec <- is.element(FRE18_TFg3$player2, player1vector)

addPlayer1 <- FRE18_TFg3[ which(FRE18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_TFg3[ which(FRE18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_TFg2 <- rbind(FRE18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
FRE18_TFft <- ftable(FRE18_TFg2$player1, FRE18_TFg2$player2)
FRE18_TFft2 <- as.matrix(FRE18_TFft)
numRows <- nrow(FRE18_TFft2)
numCols <- ncol(FRE18_TFft2)
FRE18_TFft3 <- FRE18_TFft2[c(2:numRows) , c(2:numCols)]
FRE18_TFTable <- graph.adjacency(FRE18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(FRE18_TFTable, vertex.label = V(FRE18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
FRE18_TF.clusterCoef <- transitivity(FRE18_TFTable, type="global") #cluster coefficient
FRE18_TF.degreeCent <- centralization.degree(FRE18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_TFftn <- as.network.matrix(FRE18_TFft)
FRE18_TF.netDensity <- network.density(FRE18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_TF.entropy <- entropy(FRE18_TFft) #entropy

FRE18_TF.netMx <- cbind(FRE18_TF.netMx, FRE18_TF.clusterCoef, FRE18_TF.degreeCent$centralization,
                        FRE18_TF.netDensity, FRE18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************

round = 18
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
FRE18_SAMg2 <- data.frame(FRE18_SAM)
FRE18_SAMg2 <- FRE18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_SAMg2$player1
player2vector <- FRE18_SAMg2$player2
FRE18_SAMg3 <- FRE18_SAMg2
FRE18_SAMg3$p1inp2vec <- is.element(FRE18_SAMg3$player1, player2vector)
FRE18_SAMg3$p2inp1vec <- is.element(FRE18_SAMg3$player2, player1vector)

addPlayer1 <- FRE18_SAMg3[ which(FRE18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_SAMg3[ which(FRE18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_SAMg2 <- rbind(FRE18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
FRE18_SAMft <- ftable(FRE18_SAMg2$player1, FRE18_SAMg2$player2)
FRE18_SAMft2 <- as.matrix(FRE18_SAMft)
numRows <- nrow(FRE18_SAMft2)
numCols <- ncol(FRE18_SAMft2)
FRE18_SAMft3 <- FRE18_SAMft2[c(2:numRows) , c(2:numCols)]
FRE18_SAMTable <- graph.adjacency(FRE18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(FRE18_SAMTable, vertex.label = V(FRE18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
FRE18_SAM.clusterCoef <- transitivity(FRE18_SAMTable, type="global") #cluster coefficient
FRE18_SAM.degreeCent <- centralization.degree(FRE18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_SAMftn <- as.network.matrix(FRE18_SAMft)
FRE18_SAM.netDensity <- network.density(FRE18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_SAM.entropy <- entropy(FRE18_SAMft) #entropy

FRE18_SAM.netMx <- cbind(FRE18_SAM.netMx, FRE18_SAM.clusterCoef, FRE18_SAM.degreeCent$centralization,
                         FRE18_SAM.netDensity, FRE18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
FRE18_TAMg2 <- data.frame(FRE18_TAM)
FRE18_TAMg2 <- FRE18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_TAMg2$player1
player2vector <- FRE18_TAMg2$player2
FRE18_TAMg3 <- FRE18_TAMg2
FRE18_TAMg3$p1inp2vec <- is.element(FRE18_TAMg3$player1, player2vector)
FRE18_TAMg3$p2inp1vec <- is.element(FRE18_TAMg3$player2, player1vector)

addPlayer1 <- FRE18_TAMg3[ which(FRE18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_TAMg3[ which(FRE18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_TAMg2 <- rbind(FRE18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
FRE18_TAMft <- ftable(FRE18_TAMg2$player1, FRE18_TAMg2$player2)
FRE18_TAMft2 <- as.matrix(FRE18_TAMft)
numRows <- nrow(FRE18_TAMft2)
numCols <- ncol(FRE18_TAMft2)
FRE18_TAMft3 <- FRE18_TAMft2[c(2:numRows) , c(2:numCols)]
FRE18_TAMTable <- graph.adjacency(FRE18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(FRE18_TAMTable, vertex.label = V(FRE18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
FRE18_TAM.clusterCoef <- transitivity(FRE18_TAMTable, type="global") #cluster coefficient
FRE18_TAM.degreeCent <- centralization.degree(FRE18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_TAMftn <- as.network.matrix(FRE18_TAMft)
FRE18_TAM.netDensity <- network.density(FRE18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_TAM.entropy <- entropy(FRE18_TAMft) #entropy

FRE18_TAM.netMx <- cbind(FRE18_TAM.netMx, FRE18_TAM.clusterCoef, FRE18_TAM.degreeCent$centralization,
                         FRE18_TAM.netDensity, FRE18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
FRE18_SDMg2 <- data.frame(FRE18_SDM)
FRE18_SDMg2 <- FRE18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_SDMg2$player1
player2vector <- FRE18_SDMg2$player2
FRE18_SDMg3 <- FRE18_SDMg2
FRE18_SDMg3$p1inp2vec <- is.element(FRE18_SDMg3$player1, player2vector)
FRE18_SDMg3$p2inp1vec <- is.element(FRE18_SDMg3$player2, player1vector)

addPlayer1 <- FRE18_SDMg3[ which(FRE18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_SDMg3[ which(FRE18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_SDMg2 <- rbind(FRE18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
FRE18_SDMft <- ftable(FRE18_SDMg2$player1, FRE18_SDMg2$player2)
FRE18_SDMft2 <- as.matrix(FRE18_SDMft)
numRows <- nrow(FRE18_SDMft2)
numCols <- ncol(FRE18_SDMft2)
FRE18_SDMft3 <- FRE18_SDMft2[c(2:numRows) , c(2:numCols)]
FRE18_SDMTable <- graph.adjacency(FRE18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(FRE18_SDMTable, vertex.label = V(FRE18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
FRE18_SDM.clusterCoef <- transitivity(FRE18_SDMTable, type="global") #cluster coefficient
FRE18_SDM.degreeCent <- centralization.degree(FRE18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_SDMftn <- as.network.matrix(FRE18_SDMft)
FRE18_SDM.netDensity <- network.density(FRE18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_SDM.entropy <- entropy(FRE18_SDMft) #entropy

FRE18_SDM.netMx <- cbind(FRE18_SDM.netMx, FRE18_SDM.clusterCoef, FRE18_SDM.degreeCent$centralization,
                         FRE18_SDM.netDensity, FRE18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
FRE18_TDMg2 <- data.frame(FRE18_TDM)
FRE18_TDMg2 <- FRE18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_TDMg2$player1
player2vector <- FRE18_TDMg2$player2
FRE18_TDMg3 <- FRE18_TDMg2
FRE18_TDMg3$p1inp2vec <- is.element(FRE18_TDMg3$player1, player2vector)
FRE18_TDMg3$p2inp1vec <- is.element(FRE18_TDMg3$player2, player1vector)

addPlayer1 <- FRE18_TDMg3[ which(FRE18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_TDMg3[ which(FRE18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_TDMg2 <- rbind(FRE18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
FRE18_TDMft <- ftable(FRE18_TDMg2$player1, FRE18_TDMg2$player2)
FRE18_TDMft2 <- as.matrix(FRE18_TDMft)
numRows <- nrow(FRE18_TDMft2)
numCols <- ncol(FRE18_TDMft2)
FRE18_TDMft3 <- FRE18_TDMft2[c(2:numRows) , c(2:numCols)]
FRE18_TDMTable <- graph.adjacency(FRE18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(FRE18_TDMTable, vertex.label = V(FRE18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
FRE18_TDM.clusterCoef <- transitivity(FRE18_TDMTable, type="global") #cluster coefficient
FRE18_TDM.degreeCent <- centralization.degree(FRE18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_TDMftn <- as.network.matrix(FRE18_TDMft)
FRE18_TDM.netDensity <- network.density(FRE18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_TDM.entropy <- entropy(FRE18_TDMft) #entropy

FRE18_TDM.netMx <- cbind(FRE18_TDM.netMx, FRE18_TDM.clusterCoef, FRE18_TDM.degreeCent$centralization,
                         FRE18_TDM.netDensity, FRE18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
FRE18_SDg2 <- data.frame(FRE18_SD)
FRE18_SDg2 <- FRE18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_SDg2$player1
player2vector <- FRE18_SDg2$player2
FRE18_SDg3 <- FRE18_SDg2
FRE18_SDg3$p1inp2vec <- is.element(FRE18_SDg3$player1, player2vector)
FRE18_SDg3$p2inp1vec <- is.element(FRE18_SDg3$player2, player1vector)

addPlayer1 <- FRE18_SDg3[ which(FRE18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_SDg3[ which(FRE18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_SDg2 <- rbind(FRE18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
FRE18_SDft <- ftable(FRE18_SDg2$player1, FRE18_SDg2$player2)
FRE18_SDft2 <- as.matrix(FRE18_SDft)
numRows <- nrow(FRE18_SDft2)
numCols <- ncol(FRE18_SDft2)
FRE18_SDft3 <- FRE18_SDft2[c(2:numRows) , c(2:numCols)]
FRE18_SDTable <- graph.adjacency(FRE18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(FRE18_SDTable, vertex.label = V(FRE18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
FRE18_SD.clusterCoef <- transitivity(FRE18_SDTable, type="global") #cluster coefficient
FRE18_SD.degreeCent <- centralization.degree(FRE18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_SDftn <- as.network.matrix(FRE18_SDft)
FRE18_SD.netDensity <- network.density(FRE18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_SD.entropy <- entropy(FRE18_SDft) #entropy

FRE18_SD.netMx <- cbind(FRE18_SD.netMx, FRE18_SD.clusterCoef, FRE18_SD.degreeCent$centralization,
                        FRE18_SD.netDensity, FRE18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
FRE18_TDg2 <- data.frame(FRE18_TD)
FRE18_TDg2 <- FRE18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_TDg2$player1
player2vector <- FRE18_TDg2$player2
FRE18_TDg3 <- FRE18_TDg2
FRE18_TDg3$p1inp2vec <- is.element(FRE18_TDg3$player1, player2vector)
FRE18_TDg3$p2inp1vec <- is.element(FRE18_TDg3$player2, player1vector)

addPlayer1 <- FRE18_TDg3[ which(FRE18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_TDg3[ which(FRE18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_TDg2 <- rbind(FRE18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
FRE18_TDft <- ftable(FRE18_TDg2$player1, FRE18_TDg2$player2)
FRE18_TDft2 <- as.matrix(FRE18_TDft)
numRows <- nrow(FRE18_TDft2)
numCols <- ncol(FRE18_TDft2)
FRE18_TDft3 <- FRE18_TDft2[c(2:numRows) , c(2:numCols)]
FRE18_TDTable <- graph.adjacency(FRE18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(FRE18_TDTable, vertex.label = V(FRE18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
FRE18_TD.clusterCoef <- transitivity(FRE18_TDTable, type="global") #cluster coefficient
FRE18_TD.degreeCent <- centralization.degree(FRE18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_TDftn <- as.network.matrix(FRE18_TDft)
FRE18_TD.netDensity <- network.density(FRE18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_TD.entropy <- entropy(FRE18_TDft) #entropy

FRE18_TD.netMx <- cbind(FRE18_TD.netMx, FRE18_TD.clusterCoef, FRE18_TD.degreeCent$centralization,
                        FRE18_TD.netDensity, FRE18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
FRE18_QTg2 <- data.frame(FRE18_QT)
FRE18_QTg2 <- FRE18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE18_QTg2$player1
player2vector <- FRE18_QTg2$player2
FRE18_QTg3 <- FRE18_QTg2
FRE18_QTg3$p1inp2vec <- is.element(FRE18_QTg3$player1, player2vector)
FRE18_QTg3$p2inp1vec <- is.element(FRE18_QTg3$player2, player1vector)

addPlayer1 <- FRE18_QTg3[ which(FRE18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE18_QTg3[ which(FRE18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE18_QTg2 <- rbind(FRE18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
FRE18_QTft <- ftable(FRE18_QTg2$player1, FRE18_QTg2$player2)
FRE18_QTft2 <- as.matrix(FRE18_QTft)
numRows <- nrow(FRE18_QTft2)
numCols <- ncol(FRE18_QTft2)
FRE18_QTft3 <- FRE18_QTft2[c(2:numRows) , c(2:numCols)]
FRE18_QTTable <- graph.adjacency(FRE18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(FRE18_QTTable, vertex.label = V(FRE18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
FRE18_QT.clusterCoef <- transitivity(FRE18_QTTable, type="global") #cluster coefficient
FRE18_QT.degreeCent <- centralization.degree(FRE18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE18_QTftn <- as.network.matrix(FRE18_QTft)
FRE18_QT.netDensity <- network.density(FRE18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE18_QT.entropy <- entropy(FRE18_QTft) #entropy

FRE18_QT.netMx <- cbind(FRE18_QT.netMx, FRE18_QT.clusterCoef, FRE18_QT.degreeCent$centralization,
                        FRE18_QT.netDensity, FRE18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE18_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
GCFC18_Gg2 <- data.frame(GCFC18_G)
GCFC18_Gg2 <- GCFC18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_Gg2$player1
player2vector <- GCFC18_Gg2$player2
GCFC18_Gg3 <- GCFC18_Gg2
GCFC18_Gg3$p1inp2vec <- is.element(GCFC18_Gg3$player1, player2vector)
GCFC18_Gg3$p2inp1vec <- is.element(GCFC18_Gg3$player2, player1vector)

addPlayer1 <- GCFC18_Gg3[ which(GCFC18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_Gg3[ which(GCFC18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_Gg2 <- rbind(GCFC18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
GCFC18_Gft <- ftable(GCFC18_Gg2$player1, GCFC18_Gg2$player2)
GCFC18_Gft2 <- as.matrix(GCFC18_Gft)
numRows <- nrow(GCFC18_Gft2)
numCols <- ncol(GCFC18_Gft2)
GCFC18_Gft3 <- GCFC18_Gft2[c(2:numRows) , c(2:numCols)]
GCFC18_GTable <- graph.adjacency(GCFC18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(GCFC18_GTable, vertex.label = V(GCFC18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
GCFC18_G.clusterCoef <- transitivity(GCFC18_GTable, type="global") #cluster coefficient
GCFC18_G.degreeCent <- centralization.degree(GCFC18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_Gftn <- as.network.matrix(GCFC18_Gft)
GCFC18_G.netDensity <- network.density(GCFC18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_G.entropy <- entropy(GCFC18_Gft) #entropy

GCFC18_G.netMx <- cbind(GCFC18_G.netMx, GCFC18_G.clusterCoef, GCFC18_G.degreeCent$centralization,
                        GCFC18_G.netDensity, GCFC18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
GCFC18_Bg2 <- data.frame(GCFC18_B)
GCFC18_Bg2 <- GCFC18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_Bg2$player1
player2vector <- GCFC18_Bg2$player2
GCFC18_Bg3 <- GCFC18_Bg2
GCFC18_Bg3$p1inp2vec <- is.element(GCFC18_Bg3$player1, player2vector)
GCFC18_Bg3$p2inp1vec <- is.element(GCFC18_Bg3$player2, player1vector)

addPlayer1 <- GCFC18_Bg3[ which(GCFC18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_Bg3[ which(GCFC18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_Bg2 <- rbind(GCFC18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
GCFC18_Bft <- ftable(GCFC18_Bg2$player1, GCFC18_Bg2$player2)
GCFC18_Bft2 <- as.matrix(GCFC18_Bft)
numRows <- nrow(GCFC18_Bft2)
numCols <- ncol(GCFC18_Bft2)
GCFC18_Bft3 <- GCFC18_Bft2[c(2:numRows) , c(2:numCols)]
GCFC18_BTable <- graph.adjacency(GCFC18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(GCFC18_BTable, vertex.label = V(GCFC18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
GCFC18_B.clusterCoef <- transitivity(GCFC18_BTable, type="global") #cluster coefficient
GCFC18_B.degreeCent <- centralization.degree(GCFC18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_Bftn <- as.network.matrix(GCFC18_Bft)
GCFC18_B.netDensity <- network.density(GCFC18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_B.entropy <- entropy(GCFC18_Bft) #entropy

GCFC18_B.netMx <- cbind(GCFC18_B.netMx, GCFC18_B.clusterCoef, GCFC18_B.degreeCent$centralization,
                        GCFC18_B.netDensity, GCFC18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
GCFC18_SFg2 <- data.frame(GCFC18_SF)
GCFC18_SFg2 <- GCFC18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_SFg2$player1
player2vector <- GCFC18_SFg2$player2
GCFC18_SFg3 <- GCFC18_SFg2
GCFC18_SFg3$p1inp2vec <- is.element(GCFC18_SFg3$player1, player2vector)
GCFC18_SFg3$p2inp1vec <- is.element(GCFC18_SFg3$player2, player1vector)

addPlayer1 <- GCFC18_SFg3[ which(GCFC18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC18_SFg2 <- rbind(GCFC18_SFg2, addPlayer1)

#ROUND 18, FWD Stoppage graph using weighted edges
GCFC18_SFft <- ftable(GCFC18_SFg2$player1, GCFC18_SFg2$player2)
GCFC18_SFft2 <- as.matrix(GCFC18_SFft)
numRows <- nrow(GCFC18_SFft2)
numCols <- ncol(GCFC18_SFft2)
GCFC18_SFft3 <- GCFC18_SFft2[c(2:numRows) , c(1:numCols)]
GCFC18_SFTable <- graph.adjacency(GCFC18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(GCFC18_SFTable, vertex.label = V(GCFC18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
GCFC18_SF.clusterCoef <- transitivity(GCFC18_SFTable, type="global") #cluster coefficient
GCFC18_SF.degreeCent <- centralization.degree(GCFC18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_SFftn <- as.network.matrix(GCFC18_SFft)
GCFC18_SF.netDensity <- network.density(GCFC18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_SF.entropy <- entropy(GCFC18_SFft) #entropy

GCFC18_SF.netMx <- cbind(GCFC18_SF.netMx, GCFC18_SF.clusterCoef, GCFC18_SF.degreeCent$centralization,
                         GCFC18_SF.netDensity, GCFC18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
GCFC18_TFg2 <- data.frame(GCFC18_TF)
GCFC18_TFg2 <- GCFC18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_TFg2$player1
player2vector <- GCFC18_TFg2$player2
GCFC18_TFg3 <- GCFC18_TFg2
GCFC18_TFg3$p1inp2vec <- is.element(GCFC18_TFg3$player1, player2vector)
GCFC18_TFg3$p2inp1vec <- is.element(GCFC18_TFg3$player2, player1vector)

addPlayer1 <- GCFC18_TFg3[ which(GCFC18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_TFg3[ which(GCFC18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_TFg2 <- rbind(GCFC18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
GCFC18_TFft <- ftable(GCFC18_TFg2$player1, GCFC18_TFg2$player2)
GCFC18_TFft2 <- as.matrix(GCFC18_TFft)
numRows <- nrow(GCFC18_TFft2)
numCols <- ncol(GCFC18_TFft2)
GCFC18_TFft3 <- GCFC18_TFft2[c(2:numRows) , c(2:numCols)]
GCFC18_TFTable <- graph.adjacency(GCFC18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(GCFC18_TFTable, vertex.label = V(GCFC18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
GCFC18_TF.clusterCoef <- transitivity(GCFC18_TFTable, type="global") #cluster coefficient
GCFC18_TF.degreeCent <- centralization.degree(GCFC18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_TFftn <- as.network.matrix(GCFC18_TFft)
GCFC18_TF.netDensity <- network.density(GCFC18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_TF.entropy <- entropy(GCFC18_TFft) #entropy

GCFC18_TF.netMx <- cbind(GCFC18_TF.netMx, GCFC18_TF.clusterCoef, GCFC18_TF.degreeCent$centralization,
                         GCFC18_TF.netDensity, GCFC18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************

round = 18
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
GCFC18_SAMg2 <- data.frame(GCFC18_SAM)
GCFC18_SAMg2 <- GCFC18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_SAMg2$player1
player2vector <- GCFC18_SAMg2$player2
GCFC18_SAMg3 <- GCFC18_SAMg2
GCFC18_SAMg3$p1inp2vec <- is.element(GCFC18_SAMg3$player1, player2vector)
GCFC18_SAMg3$p2inp1vec <- is.element(GCFC18_SAMg3$player2, player1vector)

addPlayer1 <- GCFC18_SAMg3[ which(GCFC18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC18_SAMg3[ which(GCFC18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_SAMg2 <- rbind(GCFC18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
GCFC18_SAMft <- ftable(GCFC18_SAMg2$player1, GCFC18_SAMg2$player2)
GCFC18_SAMft2 <- as.matrix(GCFC18_SAMft)
numRows <- nrow(GCFC18_SAMft2)
numCols <- ncol(GCFC18_SAMft2)
GCFC18_SAMft3 <- GCFC18_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC18_SAMTable <- graph.adjacency(GCFC18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(GCFC18_SAMTable, vertex.label = V(GCFC18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
GCFC18_SAM.clusterCoef <- transitivity(GCFC18_SAMTable, type="global") #cluster coefficient
GCFC18_SAM.degreeCent <- centralization.degree(GCFC18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_SAMftn <- as.network.matrix(GCFC18_SAMft)
GCFC18_SAM.netDensity <- network.density(GCFC18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_SAM.entropy <- entropy(GCFC18_SAMft) #entropy

GCFC18_SAM.netMx <- cbind(GCFC18_SAM.netMx, GCFC18_SAM.clusterCoef, GCFC18_SAM.degreeCent$centralization,
                          GCFC18_SAM.netDensity, GCFC18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
GCFC18_TAMg2 <- data.frame(GCFC18_TAM)
GCFC18_TAMg2 <- GCFC18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_TAMg2$player1
player2vector <- GCFC18_TAMg2$player2
GCFC18_TAMg3 <- GCFC18_TAMg2
GCFC18_TAMg3$p1inp2vec <- is.element(GCFC18_TAMg3$player1, player2vector)
GCFC18_TAMg3$p2inp1vec <- is.element(GCFC18_TAMg3$player2, player1vector)

addPlayer1 <- GCFC18_TAMg3[ which(GCFC18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_TAMg3[ which(GCFC18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_TAMg2 <- rbind(GCFC18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
GCFC18_TAMft <- ftable(GCFC18_TAMg2$player1, GCFC18_TAMg2$player2)
GCFC18_TAMft2 <- as.matrix(GCFC18_TAMft)
numRows <- nrow(GCFC18_TAMft2)
numCols <- ncol(GCFC18_TAMft2)
GCFC18_TAMft3 <- GCFC18_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC18_TAMTable <- graph.adjacency(GCFC18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(GCFC18_TAMTable, vertex.label = V(GCFC18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
GCFC18_TAM.clusterCoef <- transitivity(GCFC18_TAMTable, type="global") #cluster coefficient
GCFC18_TAM.degreeCent <- centralization.degree(GCFC18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_TAMftn <- as.network.matrix(GCFC18_TAMft)
GCFC18_TAM.netDensity <- network.density(GCFC18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_TAM.entropy <- entropy(GCFC18_TAMft) #entropy

GCFC18_TAM.netMx <- cbind(GCFC18_TAM.netMx, GCFC18_TAM.clusterCoef, GCFC18_TAM.degreeCent$centralization,
                          GCFC18_TAM.netDensity, GCFC18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
GCFC18_SDMg2 <- data.frame(GCFC18_SDM)
GCFC18_SDMg2 <- GCFC18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_SDMg2$player1
player2vector <- GCFC18_SDMg2$player2
GCFC18_SDMg3 <- GCFC18_SDMg2
GCFC18_SDMg3$p1inp2vec <- is.element(GCFC18_SDMg3$player1, player2vector)
GCFC18_SDMg3$p2inp1vec <- is.element(GCFC18_SDMg3$player2, player1vector)

addPlayer1 <- GCFC18_SDMg3[ which(GCFC18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_SDMg3[ which(GCFC18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_SDMg2 <- rbind(GCFC18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
GCFC18_SDMft <- ftable(GCFC18_SDMg2$player1, GCFC18_SDMg2$player2)
GCFC18_SDMft2 <- as.matrix(GCFC18_SDMft)
numRows <- nrow(GCFC18_SDMft2)
numCols <- ncol(GCFC18_SDMft2)
GCFC18_SDMft3 <- GCFC18_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC18_SDMTable <- graph.adjacency(GCFC18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(GCFC18_SDMTable, vertex.label = V(GCFC18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
GCFC18_SDM.clusterCoef <- transitivity(GCFC18_SDMTable, type="global") #cluster coefficient
GCFC18_SDM.degreeCent <- centralization.degree(GCFC18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_SDMftn <- as.network.matrix(GCFC18_SDMft)
GCFC18_SDM.netDensity <- network.density(GCFC18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_SDM.entropy <- entropy(GCFC18_SDMft) #entropy

GCFC18_SDM.netMx <- cbind(GCFC18_SDM.netMx, GCFC18_SDM.clusterCoef, GCFC18_SDM.degreeCent$centralization,
                          GCFC18_SDM.netDensity, GCFC18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
GCFC18_TDMg2 <- data.frame(GCFC18_TDM)
GCFC18_TDMg2 <- GCFC18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_TDMg2$player1
player2vector <- GCFC18_TDMg2$player2
GCFC18_TDMg3 <- GCFC18_TDMg2
GCFC18_TDMg3$p1inp2vec <- is.element(GCFC18_TDMg3$player1, player2vector)
GCFC18_TDMg3$p2inp1vec <- is.element(GCFC18_TDMg3$player2, player1vector)

addPlayer1 <- GCFC18_TDMg3[ which(GCFC18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_TDMg3[ which(GCFC18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_TDMg2 <- rbind(GCFC18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
GCFC18_TDMft <- ftable(GCFC18_TDMg2$player1, GCFC18_TDMg2$player2)
GCFC18_TDMft2 <- as.matrix(GCFC18_TDMft)
numRows <- nrow(GCFC18_TDMft2)
numCols <- ncol(GCFC18_TDMft2)
GCFC18_TDMft3 <- GCFC18_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC18_TDMTable <- graph.adjacency(GCFC18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(GCFC18_TDMTable, vertex.label = V(GCFC18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
GCFC18_TDM.clusterCoef <- transitivity(GCFC18_TDMTable, type="global") #cluster coefficient
GCFC18_TDM.degreeCent <- centralization.degree(GCFC18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_TDMftn <- as.network.matrix(GCFC18_TDMft)
GCFC18_TDM.netDensity <- network.density(GCFC18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_TDM.entropy <- entropy(GCFC18_TDMft) #entropy

GCFC18_TDM.netMx <- cbind(GCFC18_TDM.netMx, GCFC18_TDM.clusterCoef, GCFC18_TDM.degreeCent$centralization,
                          GCFC18_TDM.netDensity, GCFC18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
GCFC18_SDg2 <- data.frame(GCFC18_SD)
GCFC18_SDg2 <- GCFC18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_SDg2$player1
player2vector <- GCFC18_SDg2$player2
GCFC18_SDg3 <- GCFC18_SDg2
GCFC18_SDg3$p1inp2vec <- is.element(GCFC18_SDg3$player1, player2vector)
GCFC18_SDg3$p2inp1vec <- is.element(GCFC18_SDg3$player2, player1vector)

addPlayer1 <- GCFC18_SDg3[ which(GCFC18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_SDg3[ which(GCFC18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_SDg2 <- rbind(GCFC18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
GCFC18_SDft <- ftable(GCFC18_SDg2$player1, GCFC18_SDg2$player2)
GCFC18_SDft2 <- as.matrix(GCFC18_SDft)
numRows <- nrow(GCFC18_SDft2)
numCols <- ncol(GCFC18_SDft2)
GCFC18_SDft3 <- GCFC18_SDft2[c(2:numRows) , c(2:numCols)]
GCFC18_SDTable <- graph.adjacency(GCFC18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(GCFC18_SDTable, vertex.label = V(GCFC18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
GCFC18_SD.clusterCoef <- transitivity(GCFC18_SDTable, type="global") #cluster coefficient
GCFC18_SD.degreeCent <- centralization.degree(GCFC18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_SDftn <- as.network.matrix(GCFC18_SDft)
GCFC18_SD.netDensity <- network.density(GCFC18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_SD.entropy <- entropy(GCFC18_SDft) #entropy

GCFC18_SD.netMx <- cbind(GCFC18_SD.netMx, GCFC18_SD.clusterCoef, GCFC18_SD.degreeCent$centralization,
                         GCFC18_SD.netDensity, GCFC18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
GCFC18_TDg2 <- data.frame(GCFC18_TD)
GCFC18_TDg2 <- GCFC18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_TDg2$player1
player2vector <- GCFC18_TDg2$player2
GCFC18_TDg3 <- GCFC18_TDg2
GCFC18_TDg3$p1inp2vec <- is.element(GCFC18_TDg3$player1, player2vector)
GCFC18_TDg3$p2inp1vec <- is.element(GCFC18_TDg3$player2, player1vector)

addPlayer1 <- GCFC18_TDg3[ which(GCFC18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_TDg3[ which(GCFC18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_TDg2 <- rbind(GCFC18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
GCFC18_TDft <- ftable(GCFC18_TDg2$player1, GCFC18_TDg2$player2)
GCFC18_TDft2 <- as.matrix(GCFC18_TDft)
numRows <- nrow(GCFC18_TDft2)
numCols <- ncol(GCFC18_TDft2)
GCFC18_TDft3 <- GCFC18_TDft2[c(2:numRows) , c(2:numCols)]
GCFC18_TDTable <- graph.adjacency(GCFC18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(GCFC18_TDTable, vertex.label = V(GCFC18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
GCFC18_TD.clusterCoef <- transitivity(GCFC18_TDTable, type="global") #cluster coefficient
GCFC18_TD.degreeCent <- centralization.degree(GCFC18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_TDftn <- as.network.matrix(GCFC18_TDft)
GCFC18_TD.netDensity <- network.density(GCFC18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_TD.entropy <- entropy(GCFC18_TDft) #entropy

GCFC18_TD.netMx <- cbind(GCFC18_TD.netMx, GCFC18_TD.clusterCoef, GCFC18_TD.degreeCent$centralization,
                         GCFC18_TD.netDensity, GCFC18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
GCFC18_QTg2 <- data.frame(GCFC18_QT)
GCFC18_QTg2 <- GCFC18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC18_QTg2$player1
player2vector <- GCFC18_QTg2$player2
GCFC18_QTg3 <- GCFC18_QTg2
GCFC18_QTg3$p1inp2vec <- is.element(GCFC18_QTg3$player1, player2vector)
GCFC18_QTg3$p2inp1vec <- is.element(GCFC18_QTg3$player2, player1vector)

addPlayer1 <- GCFC18_QTg3[ which(GCFC18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC18_QTg3[ which(GCFC18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC18_QTg2 <- rbind(GCFC18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
GCFC18_QTft <- ftable(GCFC18_QTg2$player1, GCFC18_QTg2$player2)
GCFC18_QTft2 <- as.matrix(GCFC18_QTft)
numRows <- nrow(GCFC18_QTft2)
numCols <- ncol(GCFC18_QTft2)
GCFC18_QTft3 <- GCFC18_QTft2[c(2:numRows) , c(2:numCols)]
GCFC18_QTTable <- graph.adjacency(GCFC18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(GCFC18_QTTable, vertex.label = V(GCFC18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
GCFC18_QT.clusterCoef <- transitivity(GCFC18_QTTable, type="global") #cluster coefficient
GCFC18_QT.degreeCent <- centralization.degree(GCFC18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC18_QTftn <- as.network.matrix(GCFC18_QTft)
GCFC18_QT.netDensity <- network.density(GCFC18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC18_QT.entropy <- entropy(GCFC18_QTft) #entropy

GCFC18_QT.netMx <- cbind(GCFC18_QT.netMx, GCFC18_QT.clusterCoef, GCFC18_QT.degreeCent$centralization,
                         GCFC18_QT.netDensity, GCFC18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC18_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
GEEL18_Gg2 <- data.frame(GEEL18_G)
GEEL18_Gg2 <- GEEL18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_Gg2$player1
player2vector <- GEEL18_Gg2$player2
GEEL18_Gg3 <- GEEL18_Gg2
GEEL18_Gg3$p1inp2vec <- is.element(GEEL18_Gg3$player1, player2vector)
GEEL18_Gg3$p2inp1vec <- is.element(GEEL18_Gg3$player2, player1vector)

addPlayer1 <- GEEL18_Gg3[ which(GEEL18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL18_Gg3[ which(GEEL18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_Gg2 <- rbind(GEEL18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
GEEL18_Gft <- ftable(GEEL18_Gg2$player1, GEEL18_Gg2$player2)
GEEL18_Gft2 <- as.matrix(GEEL18_Gft)
numRows <- nrow(GEEL18_Gft2)
numCols <- ncol(GEEL18_Gft2)
GEEL18_Gft3 <- GEEL18_Gft2[c(2:numRows) , c(2:numCols)]
GEEL18_GTable <- graph.adjacency(GEEL18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(GEEL18_GTable, vertex.label = V(GEEL18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
GEEL18_G.clusterCoef <- transitivity(GEEL18_GTable, type="global") #cluster coefficient
GEEL18_G.degreeCent <- centralization.degree(GEEL18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_Gftn <- as.network.matrix(GEEL18_Gft)
GEEL18_G.netDensity <- network.density(GEEL18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_G.entropy <- entropy(GEEL18_Gft) #entropy

GEEL18_G.netMx <- cbind(GEEL18_G.netMx, GEEL18_G.clusterCoef, GEEL18_G.degreeCent$centralization,
                        GEEL18_G.netDensity, GEEL18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
GEEL18_Bg2 <- data.frame(GEEL18_B)
GEEL18_Bg2 <- GEEL18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_Bg2$player1
player2vector <- GEEL18_Bg2$player2
GEEL18_Bg3 <- GEEL18_Bg2
GEEL18_Bg3$p1inp2vec <- is.element(GEEL18_Bg3$player1, player2vector)
GEEL18_Bg3$p2inp1vec <- is.element(GEEL18_Bg3$player2, player1vector)

addPlayer1 <- GEEL18_Bg3[ which(GEEL18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_Bg3[ which(GEEL18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_Bg2 <- rbind(GEEL18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
GEEL18_Bft <- ftable(GEEL18_Bg2$player1, GEEL18_Bg2$player2)
GEEL18_Bft2 <- as.matrix(GEEL18_Bft)
numRows <- nrow(GEEL18_Bft2)
numCols <- ncol(GEEL18_Bft2)
GEEL18_Bft3 <- GEEL18_Bft2[c(2:numRows) , c(2:numCols)]
GEEL18_BTable <- graph.adjacency(GEEL18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(GEEL18_BTable, vertex.label = V(GEEL18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
GEEL18_B.clusterCoef <- transitivity(GEEL18_BTable, type="global") #cluster coefficient
GEEL18_B.degreeCent <- centralization.degree(GEEL18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_Bftn <- as.network.matrix(GEEL18_Bft)
GEEL18_B.netDensity <- network.density(GEEL18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_B.entropy <- entropy(GEEL18_Bft) #entropy

GEEL18_B.netMx <- cbind(GEEL18_B.netMx, GEEL18_B.clusterCoef, GEEL18_B.degreeCent$centralization,
                        GEEL18_B.netDensity, GEEL18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
GEEL18_SFg2 <- data.frame(GEEL18_SF)
GEEL18_SFg2 <- GEEL18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_SFg2$player1
player2vector <- GEEL18_SFg2$player2
GEEL18_SFg3 <- GEEL18_SFg2
GEEL18_SFg3$p1inp2vec <- is.element(GEEL18_SFg3$player1, player2vector)
GEEL18_SFg3$p2inp1vec <- is.element(GEEL18_SFg3$player2, player1vector)

addPlayer1 <- GEEL18_SFg3[ which(GEEL18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_SFg3[ which(GEEL18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_SFg2 <- rbind(GEEL18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
GEEL18_SFft <- ftable(GEEL18_SFg2$player1, GEEL18_SFg2$player2)
GEEL18_SFft2 <- as.matrix(GEEL18_SFft)
numRows <- nrow(GEEL18_SFft2)
numCols <- ncol(GEEL18_SFft2)
GEEL18_SFft3 <- GEEL18_SFft2[c(2:numRows) , c(2:numCols)]
GEEL18_SFTable <- graph.adjacency(GEEL18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(GEEL18_SFTable, vertex.label = V(GEEL18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
GEEL18_SF.clusterCoef <- transitivity(GEEL18_SFTable, type="global") #cluster coefficient
GEEL18_SF.degreeCent <- centralization.degree(GEEL18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_SFftn <- as.network.matrix(GEEL18_SFft)
GEEL18_SF.netDensity <- network.density(GEEL18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_SF.entropy <- entropy(GEEL18_SFft) #entropy

GEEL18_SF.netMx <- cbind(GEEL18_SF.netMx, GEEL18_SF.clusterCoef, GEEL18_SF.degreeCent$centralization,
                         GEEL18_SF.netDensity, GEEL18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
GEEL18_TFg2 <- data.frame(GEEL18_TF)
GEEL18_TFg2 <- GEEL18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_TFg2$player1
player2vector <- GEEL18_TFg2$player2
GEEL18_TFg3 <- GEEL18_TFg2
GEEL18_TFg3$p1inp2vec <- is.element(GEEL18_TFg3$player1, player2vector)
GEEL18_TFg3$p2inp1vec <- is.element(GEEL18_TFg3$player2, player1vector)

addPlayer1 <- GEEL18_TFg3[ which(GEEL18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_TFg3[ which(GEEL18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_TFg2 <- rbind(GEEL18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
GEEL18_TFft <- ftable(GEEL18_TFg2$player1, GEEL18_TFg2$player2)
GEEL18_TFft2 <- as.matrix(GEEL18_TFft)
numRows <- nrow(GEEL18_TFft2)
numCols <- ncol(GEEL18_TFft2)
GEEL18_TFft3 <- GEEL18_TFft2[c(2:numRows) , c(2:numCols)]
GEEL18_TFTable <- graph.adjacency(GEEL18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(GEEL18_TFTable, vertex.label = V(GEEL18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
GEEL18_TF.clusterCoef <- transitivity(GEEL18_TFTable, type="global") #cluster coefficient
GEEL18_TF.degreeCent <- centralization.degree(GEEL18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_TFftn <- as.network.matrix(GEEL18_TFft)
GEEL18_TF.netDensity <- network.density(GEEL18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_TF.entropy <- entropy(GEEL18_TFft) #entropy

GEEL18_TF.netMx <- cbind(GEEL18_TF.netMx, GEEL18_TF.clusterCoef, GEEL18_TF.degreeCent$centralization,
                         GEEL18_TF.netDensity, GEEL18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
GEEL18_SAMg2 <- data.frame(GEEL18_SAM)
GEEL18_SAMg2 <- GEEL18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_SAMg2$player1
player2vector <- GEEL18_SAMg2$player2
GEEL18_SAMg3 <- GEEL18_SAMg2
GEEL18_SAMg3$p1inp2vec <- is.element(GEEL18_SAMg3$player1, player2vector)
GEEL18_SAMg3$p2inp1vec <- is.element(GEEL18_SAMg3$player2, player1vector)

addPlayer1 <- GEEL18_SAMg3[ which(GEEL18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_SAMg3[ which(GEEL18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_SAMg2 <- rbind(GEEL18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
GEEL18_SAMft <- ftable(GEEL18_SAMg2$player1, GEEL18_SAMg2$player2)
GEEL18_SAMft2 <- as.matrix(GEEL18_SAMft)
numRows <- nrow(GEEL18_SAMft2)
numCols <- ncol(GEEL18_SAMft2)
GEEL18_SAMft3 <- GEEL18_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL18_SAMTable <- graph.adjacency(GEEL18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(GEEL18_SAMTable, vertex.label = V(GEEL18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
GEEL18_SAM.clusterCoef <- transitivity(GEEL18_SAMTable, type="global") #cluster coefficient
GEEL18_SAM.degreeCent <- centralization.degree(GEEL18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_SAMftn <- as.network.matrix(GEEL18_SAMft)
GEEL18_SAM.netDensity <- network.density(GEEL18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_SAM.entropy <- entropy(GEEL18_SAMft) #entropy

GEEL18_SAM.netMx <- cbind(GEEL18_SAM.netMx, GEEL18_SAM.clusterCoef, GEEL18_SAM.degreeCent$centralization,
                          GEEL18_SAM.netDensity, GEEL18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
GEEL18_TAMg2 <- data.frame(GEEL18_TAM)
GEEL18_TAMg2 <- GEEL18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_TAMg2$player1
player2vector <- GEEL18_TAMg2$player2
GEEL18_TAMg3 <- GEEL18_TAMg2
GEEL18_TAMg3$p1inp2vec <- is.element(GEEL18_TAMg3$player1, player2vector)
GEEL18_TAMg3$p2inp1vec <- is.element(GEEL18_TAMg3$player2, player1vector)

addPlayer1 <- GEEL18_TAMg3[ which(GEEL18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_TAMg3[ which(GEEL18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_TAMg2 <- rbind(GEEL18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
GEEL18_TAMft <- ftable(GEEL18_TAMg2$player1, GEEL18_TAMg2$player2)
GEEL18_TAMft2 <- as.matrix(GEEL18_TAMft)
numRows <- nrow(GEEL18_TAMft2)
numCols <- ncol(GEEL18_TAMft2)
GEEL18_TAMft3 <- GEEL18_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL18_TAMTable <- graph.adjacency(GEEL18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(GEEL18_TAMTable, vertex.label = V(GEEL18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
GEEL18_TAM.clusterCoef <- transitivity(GEEL18_TAMTable, type="global") #cluster coefficient
GEEL18_TAM.degreeCent <- centralization.degree(GEEL18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_TAMftn <- as.network.matrix(GEEL18_TAMft)
GEEL18_TAM.netDensity <- network.density(GEEL18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_TAM.entropy <- entropy(GEEL18_TAMft) #entropy

GEEL18_TAM.netMx <- cbind(GEEL18_TAM.netMx, GEEL18_TAM.clusterCoef, GEEL18_TAM.degreeCent$centralization,
                          GEEL18_TAM.netDensity, GEEL18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
GEEL18_SDMg2 <- data.frame(GEEL18_SDM)
GEEL18_SDMg2 <- GEEL18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_SDMg2$player1
player2vector <- GEEL18_SDMg2$player2
GEEL18_SDMg3 <- GEEL18_SDMg2
GEEL18_SDMg3$p1inp2vec <- is.element(GEEL18_SDMg3$player1, player2vector)
GEEL18_SDMg3$p2inp1vec <- is.element(GEEL18_SDMg3$player2, player1vector)

addPlayer1 <- GEEL18_SDMg3[ which(GEEL18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_SDMg3[ which(GEEL18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_SDMg2 <- rbind(GEEL18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
GEEL18_SDMft <- ftable(GEEL18_SDMg2$player1, GEEL18_SDMg2$player2)
GEEL18_SDMft2 <- as.matrix(GEEL18_SDMft)
numRows <- nrow(GEEL18_SDMft2)
numCols <- ncol(GEEL18_SDMft2)
GEEL18_SDMft3 <- GEEL18_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL18_SDMTable <- graph.adjacency(GEEL18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(GEEL18_SDMTable, vertex.label = V(GEEL18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
GEEL18_SDM.clusterCoef <- transitivity(GEEL18_SDMTable, type="global") #cluster coefficient
GEEL18_SDM.degreeCent <- centralization.degree(GEEL18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_SDMftn <- as.network.matrix(GEEL18_SDMft)
GEEL18_SDM.netDensity <- network.density(GEEL18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_SDM.entropy <- entropy(GEEL18_SDMft) #entropy

GEEL18_SDM.netMx <- cbind(GEEL18_SDM.netMx, GEEL18_SDM.clusterCoef, GEEL18_SDM.degreeCent$centralization,
                          GEEL18_SDM.netDensity, GEEL18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
GEEL18_TDMg2 <- data.frame(GEEL18_TDM)
GEEL18_TDMg2 <- GEEL18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_TDMg2$player1
player2vector <- GEEL18_TDMg2$player2
GEEL18_TDMg3 <- GEEL18_TDMg2
GEEL18_TDMg3$p1inp2vec <- is.element(GEEL18_TDMg3$player1, player2vector)
GEEL18_TDMg3$p2inp1vec <- is.element(GEEL18_TDMg3$player2, player1vector)

addPlayer1 <- GEEL18_TDMg3[ which(GEEL18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL18_TDMg3[ which(GEEL18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_TDMg2 <- rbind(GEEL18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
GEEL18_TDMft <- ftable(GEEL18_TDMg2$player1, GEEL18_TDMg2$player2)
GEEL18_TDMft2 <- as.matrix(GEEL18_TDMft)
numRows <- nrow(GEEL18_TDMft2)
numCols <- ncol(GEEL18_TDMft2)
GEEL18_TDMft3 <- GEEL18_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL18_TDMTable <- graph.adjacency(GEEL18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(GEEL18_TDMTable, vertex.label = V(GEEL18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
GEEL18_TDM.clusterCoef <- transitivity(GEEL18_TDMTable, type="global") #cluster coefficient
GEEL18_TDM.degreeCent <- centralization.degree(GEEL18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_TDMftn <- as.network.matrix(GEEL18_TDMft)
GEEL18_TDM.netDensity <- network.density(GEEL18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_TDM.entropy <- entropy(GEEL18_TDMft) #entropy

GEEL18_TDM.netMx <- cbind(GEEL18_TDM.netMx, GEEL18_TDM.clusterCoef, GEEL18_TDM.degreeCent$centralization,
                          GEEL18_TDM.netDensity, GEEL18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
GEEL18_SDg2 <- data.frame(GEEL18_SD)
GEEL18_SDg2 <- GEEL18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_SDg2$player1
player2vector <- GEEL18_SDg2$player2
GEEL18_SDg3 <- GEEL18_SDg2
GEEL18_SDg3$p1inp2vec <- is.element(GEEL18_SDg3$player1, player2vector)
GEEL18_SDg3$p2inp1vec <- is.element(GEEL18_SDg3$player2, player1vector)

addPlayer1 <- GEEL18_SDg3[ which(GEEL18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_SDg3[ which(GEEL18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_SDg2 <- rbind(GEEL18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
GEEL18_SDft <- ftable(GEEL18_SDg2$player1, GEEL18_SDg2$player2)
GEEL18_SDft2 <- as.matrix(GEEL18_SDft)
numRows <- nrow(GEEL18_SDft2)
numCols <- ncol(GEEL18_SDft2)
GEEL18_SDft3 <- GEEL18_SDft2[c(2:numRows) , c(2:numCols)]
GEEL18_SDTable <- graph.adjacency(GEEL18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(GEEL18_SDTable, vertex.label = V(GEEL18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
GEEL18_SD.clusterCoef <- transitivity(GEEL18_SDTable, type="global") #cluster coefficient
GEEL18_SD.degreeCent <- centralization.degree(GEEL18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_SDftn <- as.network.matrix(GEEL18_SDft)
GEEL18_SD.netDensity <- network.density(GEEL18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_SD.entropy <- entropy(GEEL18_SDft) #entropy

GEEL18_SD.netMx <- cbind(GEEL18_SD.netMx, GEEL18_SD.clusterCoef, GEEL18_SD.degreeCent$centralization,
                         GEEL18_SD.netDensity, GEEL18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
GEEL18_TDg2 <- data.frame(GEEL18_TD)
GEEL18_TDg2 <- GEEL18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_TDg2$player1
player2vector <- GEEL18_TDg2$player2
GEEL18_TDg3 <- GEEL18_TDg2
GEEL18_TDg3$p1inp2vec <- is.element(GEEL18_TDg3$player1, player2vector)
GEEL18_TDg3$p2inp1vec <- is.element(GEEL18_TDg3$player2, player1vector)

addPlayer1 <- GEEL18_TDg3[ which(GEEL18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_TDg3[ which(GEEL18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_TDg2 <- rbind(GEEL18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
GEEL18_TDft <- ftable(GEEL18_TDg2$player1, GEEL18_TDg2$player2)
GEEL18_TDft2 <- as.matrix(GEEL18_TDft)
numRows <- nrow(GEEL18_TDft2)
numCols <- ncol(GEEL18_TDft2)
GEEL18_TDft3 <- GEEL18_TDft2[c(2:numRows) , c(2:numCols)]
GEEL18_TDTable <- graph.adjacency(GEEL18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(GEEL18_TDTable, vertex.label = V(GEEL18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
GEEL18_TD.clusterCoef <- transitivity(GEEL18_TDTable, type="global") #cluster coefficient
GEEL18_TD.degreeCent <- centralization.degree(GEEL18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_TDftn <- as.network.matrix(GEEL18_TDft)
GEEL18_TD.netDensity <- network.density(GEEL18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_TD.entropy <- entropy(GEEL18_TDft) #entropy

GEEL18_TD.netMx <- cbind(GEEL18_TD.netMx, GEEL18_TD.clusterCoef, GEEL18_TD.degreeCent$centralization,
                         GEEL18_TD.netDensity, GEEL18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
GEEL18_QTg2 <- data.frame(GEEL18_QT)
GEEL18_QTg2 <- GEEL18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL18_QTg2$player1
player2vector <- GEEL18_QTg2$player2
GEEL18_QTg3 <- GEEL18_QTg2
GEEL18_QTg3$p1inp2vec <- is.element(GEEL18_QTg3$player1, player2vector)
GEEL18_QTg3$p2inp1vec <- is.element(GEEL18_QTg3$player2, player1vector)

addPlayer1 <- GEEL18_QTg3[ which(GEEL18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL18_QTg3[ which(GEEL18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL18_QTg2 <- rbind(GEEL18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
GEEL18_QTft <- ftable(GEEL18_QTg2$player1, GEEL18_QTg2$player2)
GEEL18_QTft2 <- as.matrix(GEEL18_QTft)
numRows <- nrow(GEEL18_QTft2)
numCols <- ncol(GEEL18_QTft2)
GEEL18_QTft3 <- GEEL18_QTft2[c(2:numRows) , c(2:numCols)]
GEEL18_QTTable <- graph.adjacency(GEEL18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(GEEL18_QTTable, vertex.label = V(GEEL18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
GEEL18_QT.clusterCoef <- transitivity(GEEL18_QTTable, type="global") #cluster coefficient
GEEL18_QT.degreeCent <- centralization.degree(GEEL18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL18_QTftn <- as.network.matrix(GEEL18_QTft)
GEEL18_QT.netDensity <- network.density(GEEL18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL18_QT.entropy <- entropy(GEEL18_QTft) #entropy

GEEL18_QT.netMx <- cbind(GEEL18_QT.netMx, GEEL18_QT.clusterCoef, GEEL18_QT.degreeCent$centralization,
                         GEEL18_QT.netDensity, GEEL18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL18_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Goal_F"
GWS18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
GWS18_Gg2 <- data.frame(GWS18_G)
GWS18_Gg2 <- GWS18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_Gg2$player1
player2vector <- GWS18_Gg2$player2
GWS18_Gg3 <- GWS18_Gg2
GWS18_Gg3$p1inp2vec <- is.element(GWS18_Gg3$player1, player2vector)
GWS18_Gg3$p2inp1vec <- is.element(GWS18_Gg3$player2, player1vector)

addPlayer1 <- GWS18_Gg3[ which(GWS18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_Gg3[ which(GWS18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_Gg2 <- rbind(GWS18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
GWS18_Gft <- ftable(GWS18_Gg2$player1, GWS18_Gg2$player2)
GWS18_Gft2 <- as.matrix(GWS18_Gft)
numRows <- nrow(GWS18_Gft2)
numCols <- ncol(GWS18_Gft2)
GWS18_Gft3 <- GWS18_Gft2[c(1:numRows) , c(1:numCols)]
GWS18_GTable <- graph.adjacency(GWS18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(GWS18_GTable, vertex.label = V(GWS18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
GWS18_G.clusterCoef <- transitivity(GWS18_GTable, type="global") #cluster coefficient
GWS18_G.degreeCent <- centralization.degree(GWS18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_Gftn <- as.network.matrix(GWS18_Gft)
GWS18_G.netDensity <- network.density(GWS18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_G.entropy <- entropy(GWS18_Gft) #entropy

GWS18_G.netMx <- cbind(GWS18_G.netMx, GWS18_G.clusterCoef, GWS18_G.degreeCent$centralization,
                       GWS18_G.netDensity, GWS18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Behind_F"
GWS18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
GWS18_Bg2 <- data.frame(GWS18_B)
GWS18_Bg2 <- GWS18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_Bg2$player1
player2vector <- GWS18_Bg2$player2
GWS18_Bg3 <- GWS18_Bg2
GWS18_Bg3$p1inp2vec <- is.element(GWS18_Bg3$player1, player2vector)
GWS18_Bg3$p2inp1vec <- is.element(GWS18_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS18_Bg3[ which(GWS18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS18_Bg2 <- rbind(GWS18_Bg2, addPlayer2)

#ROUND 18, Behind graph using weighted edges
GWS18_Bft <- ftable(GWS18_Bg2$player1, GWS18_Bg2$player2)
GWS18_Bft2 <- as.matrix(GWS18_Bft)
numRows <- nrow(GWS18_Bft2)
numCols <- ncol(GWS18_Bft2)
GWS18_Bft3 <- GWS18_Bft2[c(1:numRows) , c(2:numCols)]
GWS18_BTable <- graph.adjacency(GWS18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(GWS18_BTable, vertex.label = V(GWS18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
GWS18_B.clusterCoef <- transitivity(GWS18_BTable, type="global") #cluster coefficient
GWS18_B.degreeCent <- centralization.degree(GWS18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_Bftn <- as.network.matrix(GWS18_Bft)
GWS18_B.netDensity <- network.density(GWS18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_B.entropy <- entropy(GWS18_Bft) #entropy

GWS18_B.netMx <- cbind(GWS18_B.netMx, GWS18_B.clusterCoef, GWS18_B.degreeCent$centralization,
                       GWS18_B.netDensity, GWS18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
GWS18_SFg2 <- data.frame(GWS18_SF)
GWS18_SFg2 <- GWS18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_SFg2$player1
player2vector <- GWS18_SFg2$player2
GWS18_SFg3 <- GWS18_SFg2
GWS18_SFg3$p1inp2vec <- is.element(GWS18_SFg3$player1, player2vector)
GWS18_SFg3$p2inp1vec <- is.element(GWS18_SFg3$player2, player1vector)

addPlayer1 <- GWS18_SFg3[ which(GWS18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_SFg3[ which(GWS18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_SFg2 <- rbind(GWS18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
GWS18_SFft <- ftable(GWS18_SFg2$player1, GWS18_SFg2$player2)
GWS18_SFft2 <- as.matrix(GWS18_SFft)
numRows <- nrow(GWS18_SFft2)
numCols <- ncol(GWS18_SFft2)
GWS18_SFft3 <- GWS18_SFft2[c(2:numRows) , c(2:numCols)]
GWS18_SFTable <- graph.adjacency(GWS18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(GWS18_SFTable, vertex.label = V(GWS18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
GWS18_SF.clusterCoef <- transitivity(GWS18_SFTable, type="global") #cluster coefficient
GWS18_SF.degreeCent <- centralization.degree(GWS18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_SFftn <- as.network.matrix(GWS18_SFft)
GWS18_SF.netDensity <- network.density(GWS18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_SF.entropy <- entropy(GWS18_SFft) #entropy

GWS18_SF.netMx <- cbind(GWS18_SF.netMx, GWS18_SF.clusterCoef, GWS18_SF.degreeCent$centralization,
                        GWS18_SF.netDensity, GWS18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
GWS18_TFg2 <- data.frame(GWS18_TF)
GWS18_TFg2 <- GWS18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_TFg2$player1
player2vector <- GWS18_TFg2$player2
GWS18_TFg3 <- GWS18_TFg2
GWS18_TFg3$p1inp2vec <- is.element(GWS18_TFg3$player1, player2vector)
GWS18_TFg3$p2inp1vec <- is.element(GWS18_TFg3$player2, player1vector)

addPlayer1 <- GWS18_TFg3[ which(GWS18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_TFg3[ which(GWS18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_TFg2 <- rbind(GWS18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
GWS18_TFft <- ftable(GWS18_TFg2$player1, GWS18_TFg2$player2)
GWS18_TFft2 <- as.matrix(GWS18_TFft)
numRows <- nrow(GWS18_TFft2)
numCols <- ncol(GWS18_TFft2)
GWS18_TFft3 <- GWS18_TFft2[c(2:numRows) , c(2:numCols)]
GWS18_TFTable <- graph.adjacency(GWS18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(GWS18_TFTable, vertex.label = V(GWS18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
GWS18_TF.clusterCoef <- transitivity(GWS18_TFTable, type="global") #cluster coefficient
GWS18_TF.degreeCent <- centralization.degree(GWS18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_TFftn <- as.network.matrix(GWS18_TFft)
GWS18_TF.netDensity <- network.density(GWS18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_TF.entropy <- entropy(GWS18_TFft) #entropy

GWS18_TF.netMx <- cbind(GWS18_TF.netMx, GWS18_TF.clusterCoef, GWS18_TF.degreeCent$centralization,
                        GWS18_TF.netDensity, GWS18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
GWS18_SAMg2 <- data.frame(GWS18_SAM)
GWS18_SAMg2 <- GWS18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_SAMg2$player1
player2vector <- GWS18_SAMg2$player2
GWS18_SAMg3 <- GWS18_SAMg2
GWS18_SAMg3$p1inp2vec <- is.element(GWS18_SAMg3$player1, player2vector)
GWS18_SAMg3$p2inp1vec <- is.element(GWS18_SAMg3$player2, player1vector)

addPlayer1 <- GWS18_SAMg3[ which(GWS18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_SAMg3[ which(GWS18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_SAMg2 <- rbind(GWS18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
GWS18_SAMft <- ftable(GWS18_SAMg2$player1, GWS18_SAMg2$player2)
GWS18_SAMft2 <- as.matrix(GWS18_SAMft)
numRows <- nrow(GWS18_SAMft2)
numCols <- ncol(GWS18_SAMft2)
GWS18_SAMft3 <- GWS18_SAMft2[c(2:numRows) , c(2:numCols)]
GWS18_SAMTable <- graph.adjacency(GWS18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(GWS18_SAMTable, vertex.label = V(GWS18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
GWS18_SAM.clusterCoef <- transitivity(GWS18_SAMTable, type="global") #cluster coefficient
GWS18_SAM.degreeCent <- centralization.degree(GWS18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_SAMftn <- as.network.matrix(GWS18_SAMft)
GWS18_SAM.netDensity <- network.density(GWS18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_SAM.entropy <- entropy(GWS18_SAMft) #entropy

GWS18_SAM.netMx <- cbind(GWS18_SAM.netMx, GWS18_SAM.clusterCoef, GWS18_SAM.degreeCent$centralization,
                         GWS18_SAM.netDensity, GWS18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
GWS18_TAMg2 <- data.frame(GWS18_TAM)
GWS18_TAMg2 <- GWS18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_TAMg2$player1
player2vector <- GWS18_TAMg2$player2
GWS18_TAMg3 <- GWS18_TAMg2
GWS18_TAMg3$p1inp2vec <- is.element(GWS18_TAMg3$player1, player2vector)
GWS18_TAMg3$p2inp1vec <- is.element(GWS18_TAMg3$player2, player1vector)

addPlayer1 <- GWS18_TAMg3[ which(GWS18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_TAMg3[ which(GWS18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_TAMg2 <- rbind(GWS18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
GWS18_TAMft <- ftable(GWS18_TAMg2$player1, GWS18_TAMg2$player2)
GWS18_TAMft2 <- as.matrix(GWS18_TAMft)
numRows <- nrow(GWS18_TAMft2)
numCols <- ncol(GWS18_TAMft2)
GWS18_TAMft3 <- GWS18_TAMft2[c(2:numRows) , c(2:numCols)]
GWS18_TAMTable <- graph.adjacency(GWS18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(GWS18_TAMTable, vertex.label = V(GWS18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
GWS18_TAM.clusterCoef <- transitivity(GWS18_TAMTable, type="global") #cluster coefficient
GWS18_TAM.degreeCent <- centralization.degree(GWS18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_TAMftn <- as.network.matrix(GWS18_TAMft)
GWS18_TAM.netDensity <- network.density(GWS18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_TAM.entropy <- entropy(GWS18_TAMft) #entropy

GWS18_TAM.netMx <- cbind(GWS18_TAM.netMx, GWS18_TAM.clusterCoef, GWS18_TAM.degreeCent$centralization,
                         GWS18_TAM.netDensity, GWS18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
GWS18_SDMg2 <- data.frame(GWS18_SDM)
GWS18_SDMg2 <- GWS18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_SDMg2$player1
player2vector <- GWS18_SDMg2$player2
GWS18_SDMg3 <- GWS18_SDMg2
GWS18_SDMg3$p1inp2vec <- is.element(GWS18_SDMg3$player1, player2vector)
GWS18_SDMg3$p2inp1vec <- is.element(GWS18_SDMg3$player2, player1vector)

addPlayer1 <- GWS18_SDMg3[ which(GWS18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_SDMg3[ which(GWS18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_SDMg2 <- rbind(GWS18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
GWS18_SDMft <- ftable(GWS18_SDMg2$player1, GWS18_SDMg2$player2)
GWS18_SDMft2 <- as.matrix(GWS18_SDMft)
numRows <- nrow(GWS18_SDMft2)
numCols <- ncol(GWS18_SDMft2)
GWS18_SDMft3 <- GWS18_SDMft2[c(2:numRows) , c(2:numCols)]
GWS18_SDMTable <- graph.adjacency(GWS18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(GWS18_SDMTable, vertex.label = V(GWS18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
GWS18_SDM.clusterCoef <- transitivity(GWS18_SDMTable, type="global") #cluster coefficient
GWS18_SDM.degreeCent <- centralization.degree(GWS18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_SDMftn <- as.network.matrix(GWS18_SDMft)
GWS18_SDM.netDensity <- network.density(GWS18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_SDM.entropy <- entropy(GWS18_SDMft) #entropy

GWS18_SDM.netMx <- cbind(GWS18_SDM.netMx, GWS18_SDM.clusterCoef, GWS18_SDM.degreeCent$centralization,
                         GWS18_SDM.netDensity, GWS18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
GWS18_TDMg2 <- data.frame(GWS18_TDM)
GWS18_TDMg2 <- GWS18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_TDMg2$player1
player2vector <- GWS18_TDMg2$player2
GWS18_TDMg3 <- GWS18_TDMg2
GWS18_TDMg3$p1inp2vec <- is.element(GWS18_TDMg3$player1, player2vector)
GWS18_TDMg3$p2inp1vec <- is.element(GWS18_TDMg3$player2, player1vector)

addPlayer1 <- GWS18_TDMg3[ which(GWS18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_TDMg3[ which(GWS18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_TDMg2 <- rbind(GWS18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
GWS18_TDMft <- ftable(GWS18_TDMg2$player1, GWS18_TDMg2$player2)
GWS18_TDMft2 <- as.matrix(GWS18_TDMft)
numRows <- nrow(GWS18_TDMft2)
numCols <- ncol(GWS18_TDMft2)
GWS18_TDMft3 <- GWS18_TDMft2[c(2:numRows) , c(2:numCols)]
GWS18_TDMTable <- graph.adjacency(GWS18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(GWS18_TDMTable, vertex.label = V(GWS18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
GWS18_TDM.clusterCoef <- transitivity(GWS18_TDMTable, type="global") #cluster coefficient
GWS18_TDM.degreeCent <- centralization.degree(GWS18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_TDMftn <- as.network.matrix(GWS18_TDMft)
GWS18_TDM.netDensity <- network.density(GWS18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_TDM.entropy <- entropy(GWS18_TDMft) #entropy

GWS18_TDM.netMx <- cbind(GWS18_TDM.netMx, GWS18_TDM.clusterCoef, GWS18_TDM.degreeCent$centralization,
                         GWS18_TDM.netDensity, GWS18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
GWS18_SDg2 <- data.frame(GWS18_SD)
GWS18_SDg2 <- GWS18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_SDg2$player1
player2vector <- GWS18_SDg2$player2
GWS18_SDg3 <- GWS18_SDg2
GWS18_SDg3$p1inp2vec <- is.element(GWS18_SDg3$player1, player2vector)
GWS18_SDg3$p2inp1vec <- is.element(GWS18_SDg3$player2, player1vector)

addPlayer1 <- GWS18_SDg3[ which(GWS18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_SDg3[ which(GWS18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_SDg2 <- rbind(GWS18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
GWS18_SDft <- ftable(GWS18_SDg2$player1, GWS18_SDg2$player2)
GWS18_SDft2 <- as.matrix(GWS18_SDft)
numRows <- nrow(GWS18_SDft2)
numCols <- ncol(GWS18_SDft2)
GWS18_SDft3 <- GWS18_SDft2[c(2:numRows) , c(2:numCols)]
GWS18_SDTable <- graph.adjacency(GWS18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(GWS18_SDTable, vertex.label = V(GWS18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
GWS18_SD.clusterCoef <- transitivity(GWS18_SDTable, type="global") #cluster coefficient
GWS18_SD.degreeCent <- centralization.degree(GWS18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_SDftn <- as.network.matrix(GWS18_SDft)
GWS18_SD.netDensity <- network.density(GWS18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_SD.entropy <- entropy(GWS18_SDft) #entropy

GWS18_SD.netMx <- cbind(GWS18_SD.netMx, GWS18_SD.clusterCoef, GWS18_SD.degreeCent$centralization,
                        GWS18_SD.netDensity, GWS18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
GWS18_TDg2 <- data.frame(GWS18_TD)
GWS18_TDg2 <- GWS18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_TDg2$player1
player2vector <- GWS18_TDg2$player2
GWS18_TDg3 <- GWS18_TDg2
GWS18_TDg3$p1inp2vec <- is.element(GWS18_TDg3$player1, player2vector)
GWS18_TDg3$p2inp1vec <- is.element(GWS18_TDg3$player2, player1vector)

addPlayer1 <- GWS18_TDg3[ which(GWS18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_TDg3[ which(GWS18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_TDg2 <- rbind(GWS18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
GWS18_TDft <- ftable(GWS18_TDg2$player1, GWS18_TDg2$player2)
GWS18_TDft2 <- as.matrix(GWS18_TDft)
numRows <- nrow(GWS18_TDft2)
numCols <- ncol(GWS18_TDft2)
GWS18_TDft3 <- GWS18_TDft2[c(2:numRows) , c(2:numCols)]
GWS18_TDTable <- graph.adjacency(GWS18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(GWS18_TDTable, vertex.label = V(GWS18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
GWS18_TD.clusterCoef <- transitivity(GWS18_TDTable, type="global") #cluster coefficient
GWS18_TD.degreeCent <- centralization.degree(GWS18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_TDftn <- as.network.matrix(GWS18_TDft)
GWS18_TD.netDensity <- network.density(GWS18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_TD.entropy <- entropy(GWS18_TDft) #entropy

GWS18_TD.netMx <- cbind(GWS18_TD.netMx, GWS18_TD.clusterCoef, GWS18_TD.degreeCent$centralization,
                        GWS18_TD.netDensity, GWS18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************

round = 18
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
GWS18_QTg2 <- data.frame(GWS18_QT)
GWS18_QTg2 <- GWS18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS18_QTg2$player1
player2vector <- GWS18_QTg2$player2
GWS18_QTg3 <- GWS18_QTg2
GWS18_QTg3$p1inp2vec <- is.element(GWS18_QTg3$player1, player2vector)
GWS18_QTg3$p2inp1vec <- is.element(GWS18_QTg3$player2, player1vector)

addPlayer1 <- GWS18_QTg3[ which(GWS18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS18_QTg3[ which(GWS18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS18_QTg2 <- rbind(GWS18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
GWS18_QTft <- ftable(GWS18_QTg2$player1, GWS18_QTg2$player2)
GWS18_QTft2 <- as.matrix(GWS18_QTft)
numRows <- nrow(GWS18_QTft2)
numCols <- ncol(GWS18_QTft2)
GWS18_QTft3 <- GWS18_QTft2[c(2:numRows) , c(2:numCols)]
GWS18_QTTable <- graph.adjacency(GWS18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(GWS18_QTTable, vertex.label = V(GWS18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
GWS18_QT.clusterCoef <- transitivity(GWS18_QTTable, type="global") #cluster coefficient
GWS18_QT.degreeCent <- centralization.degree(GWS18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS18_QTftn <- as.network.matrix(GWS18_QTft)
GWS18_QT.netDensity <- network.density(GWS18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS18_QT.entropy <- entropy(GWS18_QTft) #entropy

GWS18_QT.netMx <- cbind(GWS18_QT.netMx, GWS18_QT.clusterCoef, GWS18_QT.degreeCent$centralization,
                        GWS18_QT.netDensity, GWS18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS18_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "Goal_F"
HAW18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
HAW18_Gg2 <- data.frame(HAW18_G)
HAW18_Gg2 <- HAW18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_Gg2$player1
player2vector <- HAW18_Gg2$player2
HAW18_Gg3 <- HAW18_Gg2
HAW18_Gg3$p1inp2vec <- is.element(HAW18_Gg3$player1, player2vector)
HAW18_Gg3$p2inp1vec <- is.element(HAW18_Gg3$player2, player1vector)

addPlayer1 <- HAW18_Gg3[ which(HAW18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_Gg3[ which(HAW18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_Gg2 <- rbind(HAW18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
HAW18_Gft <- ftable(HAW18_Gg2$player1, HAW18_Gg2$player2)
HAW18_Gft2 <- as.matrix(HAW18_Gft)
numRows <- nrow(HAW18_Gft2)
numCols <- ncol(HAW18_Gft2)
HAW18_Gft3 <- HAW18_Gft2[c(2:numRows) , c(2:numCols)]
HAW18_GTable <- graph.adjacency(HAW18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(HAW18_GTable, vertex.label = V(HAW18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
HAW18_G.clusterCoef <- transitivity(HAW18_GTable, type="global") #cluster coefficient
HAW18_G.degreeCent <- centralization.degree(HAW18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_Gftn <- as.network.matrix(HAW18_Gft)
HAW18_G.netDensity <- network.density(HAW18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_G.entropy <- entropy(HAW18_Gft) #entropy

HAW18_G.netMx <- cbind(HAW18_G.netMx, HAW18_G.clusterCoef, HAW18_G.degreeCent$centralization,
                       HAW18_G.netDensity, HAW18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "Behind_F"
HAW18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
HAW18_Bg2 <- data.frame(HAW18_B)
HAW18_Bg2 <- HAW18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_Bg2$player1
player2vector <- HAW18_Bg2$player2
HAW18_Bg3 <- HAW18_Bg2
HAW18_Bg3$p1inp2vec <- is.element(HAW18_Bg3$player1, player2vector)
HAW18_Bg3$p2inp1vec <- is.element(HAW18_Bg3$player2, player1vector)

addPlayer1 <- HAW18_Bg3[ which(HAW18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW18_Bg2 <- rbind(HAW18_Bg2, addPlayer1)

#ROUND 18, Behind graph using weighted edges
HAW18_Bft <- ftable(HAW18_Bg2$player1, HAW18_Bg2$player2)
HAW18_Bft2 <- as.matrix(HAW18_Bft)
numRows <- nrow(HAW18_Bft2)
numCols <- ncol(HAW18_Bft2)
HAW18_Bft3 <- HAW18_Bft2[c(2:numRows) , c(1:numCols)]
HAW18_BTable <- graph.adjacency(HAW18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(HAW18_BTable, vertex.label = V(HAW18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
HAW18_B.clusterCoef <- transitivity(HAW18_BTable, type="global") #cluster coefficient
HAW18_B.degreeCent <- centralization.degree(HAW18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_Bftn <- as.network.matrix(HAW18_Bft)
HAW18_B.netDensity <- network.density(HAW18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_B.entropy <- entropy(HAW18_Bft) #entropy

HAW18_B.netMx <- cbind(HAW18_B.netMx, HAW18_B.clusterCoef, HAW18_B.degreeCent$centralization,
                       HAW18_B.netDensity, HAW18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
HAW18_SFg2 <- data.frame(HAW18_SF)
HAW18_SFg2 <- HAW18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_SFg2$player1
player2vector <- HAW18_SFg2$player2
HAW18_SFg3 <- HAW18_SFg2
HAW18_SFg3$p1inp2vec <- is.element(HAW18_SFg3$player1, player2vector)
HAW18_SFg3$p2inp1vec <- is.element(HAW18_SFg3$player2, player1vector)

addPlayer1 <- HAW18_SFg3[ which(HAW18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_SFg3[ which(HAW18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_SFg2 <- rbind(HAW18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
HAW18_SFft <- ftable(HAW18_SFg2$player1, HAW18_SFg2$player2)
HAW18_SFft2 <- as.matrix(HAW18_SFft)
numRows <- nrow(HAW18_SFft2)
numCols <- ncol(HAW18_SFft2)
HAW18_SFft3 <- HAW18_SFft2[c(2:numRows) , c(2:numCols)]
HAW18_SFTable <- graph.adjacency(HAW18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(HAW18_SFTable, vertex.label = V(HAW18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
HAW18_SF.clusterCoef <- transitivity(HAW18_SFTable, type="global") #cluster coefficient
HAW18_SF.degreeCent <- centralization.degree(HAW18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_SFftn <- as.network.matrix(HAW18_SFft)
HAW18_SF.netDensity <- network.density(HAW18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_SF.entropy <- entropy(HAW18_SFft) #entropy

HAW18_SF.netMx <- cbind(HAW18_SF.netMx, HAW18_SF.clusterCoef, HAW18_SF.degreeCent$centralization,
                        HAW18_SF.netDensity, HAW18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
HAW18_TFg2 <- data.frame(HAW18_TF)
HAW18_TFg2 <- HAW18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_TFg2$player1
player2vector <- HAW18_TFg2$player2
HAW18_TFg3 <- HAW18_TFg2
HAW18_TFg3$p1inp2vec <- is.element(HAW18_TFg3$player1, player2vector)
HAW18_TFg3$p2inp1vec <- is.element(HAW18_TFg3$player2, player1vector)

addPlayer1 <- HAW18_TFg3[ which(HAW18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW18_TFg3[ which(HAW18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_TFg2 <- rbind(HAW18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
HAW18_TFft <- ftable(HAW18_TFg2$player1, HAW18_TFg2$player2)
HAW18_TFft2 <- as.matrix(HAW18_TFft)
numRows <- nrow(HAW18_TFft2)
numCols <- ncol(HAW18_TFft2)
HAW18_TFft3 <- HAW18_TFft2[c(2:numRows) , c(2:numCols)]
HAW18_TFTable <- graph.adjacency(HAW18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(HAW18_TFTable, vertex.label = V(HAW18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
HAW18_TF.clusterCoef <- transitivity(HAW18_TFTable, type="global") #cluster coefficient
HAW18_TF.degreeCent <- centralization.degree(HAW18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_TFftn <- as.network.matrix(HAW18_TFft)
HAW18_TF.netDensity <- network.density(HAW18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_TF.entropy <- entropy(HAW18_TFft) #entropy

HAW18_TF.netMx <- cbind(HAW18_TF.netMx, HAW18_TF.clusterCoef, HAW18_TF.degreeCent$centralization,
                        HAW18_TF.netDensity, HAW18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
HAW18_SAMg2 <- data.frame(HAW18_SAM)
HAW18_SAMg2 <- HAW18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_SAMg2$player1
player2vector <- HAW18_SAMg2$player2
HAW18_SAMg3 <- HAW18_SAMg2
HAW18_SAMg3$p1inp2vec <- is.element(HAW18_SAMg3$player1, player2vector)
HAW18_SAMg3$p2inp1vec <- is.element(HAW18_SAMg3$player2, player1vector)

addPlayer1 <- HAW18_SAMg3[ which(HAW18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_SAMg3[ which(HAW18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_SAMg2 <- rbind(HAW18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
HAW18_SAMft <- ftable(HAW18_SAMg2$player1, HAW18_SAMg2$player2)
HAW18_SAMft2 <- as.matrix(HAW18_SAMft)
numRows <- nrow(HAW18_SAMft2)
numCols <- ncol(HAW18_SAMft2)
HAW18_SAMft3 <- HAW18_SAMft2[c(2:numRows) , c(2:numCols)]
HAW18_SAMTable <- graph.adjacency(HAW18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(HAW18_SAMTable, vertex.label = V(HAW18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
HAW18_SAM.clusterCoef <- transitivity(HAW18_SAMTable, type="global") #cluster coefficient
HAW18_SAM.degreeCent <- centralization.degree(HAW18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_SAMftn <- as.network.matrix(HAW18_SAMft)
HAW18_SAM.netDensity <- network.density(HAW18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_SAM.entropy <- entropy(HAW18_SAMft) #entropy

HAW18_SAM.netMx <- cbind(HAW18_SAM.netMx, HAW18_SAM.clusterCoef, HAW18_SAM.degreeCent$centralization,
                         HAW18_SAM.netDensity, HAW18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
HAW18_TAMg2 <- data.frame(HAW18_TAM)
HAW18_TAMg2 <- HAW18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_TAMg2$player1
player2vector <- HAW18_TAMg2$player2
HAW18_TAMg3 <- HAW18_TAMg2
HAW18_TAMg3$p1inp2vec <- is.element(HAW18_TAMg3$player1, player2vector)
HAW18_TAMg3$p2inp1vec <- is.element(HAW18_TAMg3$player2, player1vector)

addPlayer1 <- HAW18_TAMg3[ which(HAW18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_TAMg3[ which(HAW18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_TAMg2 <- rbind(HAW18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
HAW18_TAMft <- ftable(HAW18_TAMg2$player1, HAW18_TAMg2$player2)
HAW18_TAMft2 <- as.matrix(HAW18_TAMft)
numRows <- nrow(HAW18_TAMft2)
numCols <- ncol(HAW18_TAMft2)
HAW18_TAMft3 <- HAW18_TAMft2[c(2:numRows) , c(2:numCols)]
HAW18_TAMTable <- graph.adjacency(HAW18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(HAW18_TAMTable, vertex.label = V(HAW18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
HAW18_TAM.clusterCoef <- transitivity(HAW18_TAMTable, type="global") #cluster coefficient
HAW18_TAM.degreeCent <- centralization.degree(HAW18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_TAMftn <- as.network.matrix(HAW18_TAMft)
HAW18_TAM.netDensity <- network.density(HAW18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_TAM.entropy <- entropy(HAW18_TAMft) #entropy

HAW18_TAM.netMx <- cbind(HAW18_TAM.netMx, HAW18_TAM.clusterCoef, HAW18_TAM.degreeCent$centralization,
                         HAW18_TAM.netDensity, HAW18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
HAW18_SDMg2 <- data.frame(HAW18_SDM)
HAW18_SDMg2 <- HAW18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_SDMg2$player1
player2vector <- HAW18_SDMg2$player2
HAW18_SDMg3 <- HAW18_SDMg2
HAW18_SDMg3$p1inp2vec <- is.element(HAW18_SDMg3$player1, player2vector)
HAW18_SDMg3$p2inp1vec <- is.element(HAW18_SDMg3$player2, player1vector)

addPlayer1 <- HAW18_SDMg3[ which(HAW18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_SDMg3[ which(HAW18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_SDMg2 <- rbind(HAW18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
HAW18_SDMft <- ftable(HAW18_SDMg2$player1, HAW18_SDMg2$player2)
HAW18_SDMft2 <- as.matrix(HAW18_SDMft)
numRows <- nrow(HAW18_SDMft2)
numCols <- ncol(HAW18_SDMft2)
HAW18_SDMft3 <- HAW18_SDMft2[c(2:numRows) , c(2:numCols)]
HAW18_SDMTable <- graph.adjacency(HAW18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(HAW18_SDMTable, vertex.label = V(HAW18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
HAW18_SDM.clusterCoef <- transitivity(HAW18_SDMTable, type="global") #cluster coefficient
HAW18_SDM.degreeCent <- centralization.degree(HAW18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_SDMftn <- as.network.matrix(HAW18_SDMft)
HAW18_SDM.netDensity <- network.density(HAW18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_SDM.entropy <- entropy(HAW18_SDMft) #entropy

HAW18_SDM.netMx <- cbind(HAW18_SDM.netMx, HAW18_SDM.clusterCoef, HAW18_SDM.degreeCent$centralization,
                         HAW18_SDM.netDensity, HAW18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
HAW18_TDMg2 <- data.frame(HAW18_TDM)
HAW18_TDMg2 <- HAW18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_TDMg2$player1
player2vector <- HAW18_TDMg2$player2
HAW18_TDMg3 <- HAW18_TDMg2
HAW18_TDMg3$p1inp2vec <- is.element(HAW18_TDMg3$player1, player2vector)
HAW18_TDMg3$p2inp1vec <- is.element(HAW18_TDMg3$player2, player1vector)

addPlayer1 <- HAW18_TDMg3[ which(HAW18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_TDMg3[ which(HAW18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_TDMg2 <- rbind(HAW18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
HAW18_TDMft <- ftable(HAW18_TDMg2$player1, HAW18_TDMg2$player2)
HAW18_TDMft2 <- as.matrix(HAW18_TDMft)
numRows <- nrow(HAW18_TDMft2)
numCols <- ncol(HAW18_TDMft2)
HAW18_TDMft3 <- HAW18_TDMft2[c(2:numRows) , c(2:numCols)]
HAW18_TDMTable <- graph.adjacency(HAW18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(HAW18_TDMTable, vertex.label = V(HAW18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
HAW18_TDM.clusterCoef <- transitivity(HAW18_TDMTable, type="global") #cluster coefficient
HAW18_TDM.degreeCent <- centralization.degree(HAW18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_TDMftn <- as.network.matrix(HAW18_TDMft)
HAW18_TDM.netDensity <- network.density(HAW18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_TDM.entropy <- entropy(HAW18_TDMft) #entropy

HAW18_TDM.netMx <- cbind(HAW18_TDM.netMx, HAW18_TDM.clusterCoef, HAW18_TDM.degreeCent$centralization,
                         HAW18_TDM.netDensity, HAW18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
HAW18_SDg2 <- data.frame(HAW18_SD)
HAW18_SDg2 <- HAW18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_SDg2$player1
player2vector <- HAW18_SDg2$player2
HAW18_SDg3 <- HAW18_SDg2
HAW18_SDg3$p1inp2vec <- is.element(HAW18_SDg3$player1, player2vector)
HAW18_SDg3$p2inp1vec <- is.element(HAW18_SDg3$player2, player1vector)

addPlayer1 <- HAW18_SDg3[ which(HAW18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_SDg3[ which(HAW18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_SDg2 <- rbind(HAW18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
HAW18_SDft <- ftable(HAW18_SDg2$player1, HAW18_SDg2$player2)
HAW18_SDft2 <- as.matrix(HAW18_SDft)
numRows <- nrow(HAW18_SDft2)
numCols <- ncol(HAW18_SDft2)
HAW18_SDft3 <- HAW18_SDft2[c(2:numRows) , c(2:numCols)]
HAW18_SDTable <- graph.adjacency(HAW18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(HAW18_SDTable, vertex.label = V(HAW18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
HAW18_SD.clusterCoef <- transitivity(HAW18_SDTable, type="global") #cluster coefficient
HAW18_SD.degreeCent <- centralization.degree(HAW18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_SDftn <- as.network.matrix(HAW18_SDft)
HAW18_SD.netDensity <- network.density(HAW18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_SD.entropy <- entropy(HAW18_SDft) #entropy

HAW18_SD.netMx <- cbind(HAW18_SD.netMx, HAW18_SD.clusterCoef, HAW18_SD.degreeCent$centralization,
                        HAW18_SD.netDensity, HAW18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
HAW18_TDg2 <- data.frame(HAW18_TD)
HAW18_TDg2 <- HAW18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_TDg2$player1
player2vector <- HAW18_TDg2$player2
HAW18_TDg3 <- HAW18_TDg2
HAW18_TDg3$p1inp2vec <- is.element(HAW18_TDg3$player1, player2vector)
HAW18_TDg3$p2inp1vec <- is.element(HAW18_TDg3$player2, player1vector)

addPlayer1 <- HAW18_TDg3[ which(HAW18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_TDg3[ which(HAW18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_TDg2 <- rbind(HAW18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
HAW18_TDft <- ftable(HAW18_TDg2$player1, HAW18_TDg2$player2)
HAW18_TDft2 <- as.matrix(HAW18_TDft)
numRows <- nrow(HAW18_TDft2)
numCols <- ncol(HAW18_TDft2)
HAW18_TDft3 <- HAW18_TDft2[c(2:numRows) , c(2:numCols)]
HAW18_TDTable <- graph.adjacency(HAW18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(HAW18_TDTable, vertex.label = V(HAW18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
HAW18_TD.clusterCoef <- transitivity(HAW18_TDTable, type="global") #cluster coefficient
HAW18_TD.degreeCent <- centralization.degree(HAW18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_TDftn <- as.network.matrix(HAW18_TDft)
HAW18_TD.netDensity <- network.density(HAW18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_TD.entropy <- entropy(HAW18_TDft) #entropy

HAW18_TD.netMx <- cbind(HAW18_TD.netMx, HAW18_TD.clusterCoef, HAW18_TD.degreeCent$centralization,
                        HAW18_TD.netDensity, HAW18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
HAW18_QTg2 <- data.frame(HAW18_QT)
HAW18_QTg2 <- HAW18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW18_QTg2$player1
player2vector <- HAW18_QTg2$player2
HAW18_QTg3 <- HAW18_QTg2
HAW18_QTg3$p1inp2vec <- is.element(HAW18_QTg3$player1, player2vector)
HAW18_QTg3$p2inp1vec <- is.element(HAW18_QTg3$player2, player1vector)

addPlayer1 <- HAW18_QTg3[ which(HAW18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW18_QTg3[ which(HAW18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW18_QTg2 <- rbind(HAW18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
HAW18_QTft <- ftable(HAW18_QTg2$player1, HAW18_QTg2$player2)
HAW18_QTft2 <- as.matrix(HAW18_QTft)
numRows <- nrow(HAW18_QTft2)
numCols <- ncol(HAW18_QTft2)
HAW18_QTft3 <- HAW18_QTft2[c(2:numRows) , c(2:numCols)]
HAW18_QTTable <- graph.adjacency(HAW18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(HAW18_QTTable, vertex.label = V(HAW18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
HAW18_QT.clusterCoef <- transitivity(HAW18_QTTable, type="global") #cluster coefficient
HAW18_QT.degreeCent <- centralization.degree(HAW18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW18_QTftn <- as.network.matrix(HAW18_QTft)
HAW18_QT.netDensity <- network.density(HAW18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW18_QT.entropy <- entropy(HAW18_QTft) #entropy

HAW18_QT.netMx <- cbind(HAW18_QT.netMx, HAW18_QT.clusterCoef, HAW18_QT.degreeCent$centralization,
                        HAW18_QT.netDensity, HAW18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW18_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "MELB"
KIoutcome = "Goal_F"
MELB18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
MELB18_Gg2 <- data.frame(MELB18_G)
MELB18_Gg2 <- MELB18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_Gg2$player1
player2vector <- MELB18_Gg2$player2
MELB18_Gg3 <- MELB18_Gg2
MELB18_Gg3$p1inp2vec <- is.element(MELB18_Gg3$player1, player2vector)
MELB18_Gg3$p2inp1vec <- is.element(MELB18_Gg3$player2, player1vector)

addPlayer1 <- MELB18_Gg3[ which(MELB18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_Gg3[ which(MELB18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_Gg2 <- rbind(MELB18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
MELB18_Gft <- ftable(MELB18_Gg2$player1, MELB18_Gg2$player2)
MELB18_Gft2 <- as.matrix(MELB18_Gft)
numRows <- nrow(MELB18_Gft2)
numCols <- ncol(MELB18_Gft2)
MELB18_Gft3 <- MELB18_Gft2[c(2:numRows) , c(2:numCols)]
MELB18_GTable <- graph.adjacency(MELB18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(MELB18_GTable, vertex.label = V(MELB18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
MELB18_G.clusterCoef <- transitivity(MELB18_GTable, type="global") #cluster coefficient
MELB18_G.degreeCent <- centralization.degree(MELB18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_Gftn <- as.network.matrix(MELB18_Gft)
MELB18_G.netDensity <- network.density(MELB18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_G.entropy <- entropy(MELB18_Gft) #entropy

MELB18_G.netMx <- cbind(MELB18_G.netMx, MELB18_G.clusterCoef, MELB18_G.degreeCent$centralization,
                        MELB18_G.netDensity, MELB18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "MELB"
KIoutcome = "Behind_F"
MELB18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
MELB18_Bg2 <- data.frame(MELB18_B)
MELB18_Bg2 <- MELB18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_Bg2$player1
player2vector <- MELB18_Bg2$player2
MELB18_Bg3 <- MELB18_Bg2
MELB18_Bg3$p1inp2vec <- is.element(MELB18_Bg3$player1, player2vector)
MELB18_Bg3$p2inp1vec <- is.element(MELB18_Bg3$player2, player1vector)

addPlayer1 <- MELB18_Bg3[ which(MELB18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_Bg3[ which(MELB18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_Bg2 <- rbind(MELB18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
MELB18_Bft <- ftable(MELB18_Bg2$player1, MELB18_Bg2$player2)
MELB18_Bft2 <- as.matrix(MELB18_Bft)
numRows <- nrow(MELB18_Bft2)
numCols <- ncol(MELB18_Bft2)
MELB18_Bft3 <- MELB18_Bft2[c(2:numRows) , c(2:numCols)]
MELB18_BTable <- graph.adjacency(MELB18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(MELB18_BTable, vertex.label = V(MELB18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
MELB18_B.clusterCoef <- transitivity(MELB18_BTable, type="global") #cluster coefficient
MELB18_B.degreeCent <- centralization.degree(MELB18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_Bftn <- as.network.matrix(MELB18_Bft)
MELB18_B.netDensity <- network.density(MELB18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_B.entropy <- entropy(MELB18_Bft) #entropy

MELB18_B.netMx <- cbind(MELB18_B.netMx, MELB18_B.clusterCoef, MELB18_B.degreeCent$centralization,
                        MELB18_B.netDensity, MELB18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************

round = 18
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
MELB18_SFg2 <- data.frame(MELB18_SF)
MELB18_SFg2 <- MELB18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_SFg2$player1
player2vector <- MELB18_SFg2$player2
MELB18_SFg3 <- MELB18_SFg2
MELB18_SFg3$p1inp2vec <- is.element(MELB18_SFg3$player1, player2vector)
MELB18_SFg3$p2inp1vec <- is.element(MELB18_SFg3$player2, player1vector)

addPlayer1 <- MELB18_SFg3[ which(MELB18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_SFg3[ which(MELB18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_SFg2 <- rbind(MELB18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
MELB18_SFft <- ftable(MELB18_SFg2$player1, MELB18_SFg2$player2)
MELB18_SFft2 <- as.matrix(MELB18_SFft)
numRows <- nrow(MELB18_SFft2)
numCols <- ncol(MELB18_SFft2)
MELB18_SFft3 <- MELB18_SFft2[c(2:numRows) , c(2:numCols)]
MELB18_SFTable <- graph.adjacency(MELB18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(MELB18_SFTable, vertex.label = V(MELB18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
MELB18_SF.clusterCoef <- transitivity(MELB18_SFTable, type="global") #cluster coefficient
MELB18_SF.degreeCent <- centralization.degree(MELB18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_SFftn <- as.network.matrix(MELB18_SFft)
MELB18_SF.netDensity <- network.density(MELB18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_SF.entropy <- entropy(MELB18_SFft) #entropy

MELB18_SF.netMx <- cbind(MELB18_SF.netMx, MELB18_SF.clusterCoef, MELB18_SF.degreeCent$centralization,
                         MELB18_SF.netDensity, MELB18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
MELB18_TFg2 <- data.frame(MELB18_TF)
MELB18_TFg2 <- MELB18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_TFg2$player1
player2vector <- MELB18_TFg2$player2
MELB18_TFg3 <- MELB18_TFg2
MELB18_TFg3$p1inp2vec <- is.element(MELB18_TFg3$player1, player2vector)
MELB18_TFg3$p2inp1vec <- is.element(MELB18_TFg3$player2, player1vector)

addPlayer1 <- MELB18_TFg3[ which(MELB18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_TFg3[ which(MELB18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_TFg2 <- rbind(MELB18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
MELB18_TFft <- ftable(MELB18_TFg2$player1, MELB18_TFg2$player2)
MELB18_TFft2 <- as.matrix(MELB18_TFft)
numRows <- nrow(MELB18_TFft2)
numCols <- ncol(MELB18_TFft2)
MELB18_TFft3 <- MELB18_TFft2[c(2:numRows) , c(2:numCols)]
MELB18_TFTable <- graph.adjacency(MELB18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(MELB18_TFTable, vertex.label = V(MELB18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
MELB18_TF.clusterCoef <- transitivity(MELB18_TFTable, type="global") #cluster coefficient
MELB18_TF.degreeCent <- centralization.degree(MELB18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_TFftn <- as.network.matrix(MELB18_TFft)
MELB18_TF.netDensity <- network.density(MELB18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_TF.entropy <- entropy(MELB18_TFft) #entropy

MELB18_TF.netMx <- cbind(MELB18_TF.netMx, MELB18_TF.clusterCoef, MELB18_TF.degreeCent$centralization,
                         MELB18_TF.netDensity, MELB18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
MELB18_SAMg2 <- data.frame(MELB18_SAM)
MELB18_SAMg2 <- MELB18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_SAMg2$player1
player2vector <- MELB18_SAMg2$player2
MELB18_SAMg3 <- MELB18_SAMg2
MELB18_SAMg3$p1inp2vec <- is.element(MELB18_SAMg3$player1, player2vector)
MELB18_SAMg3$p2inp1vec <- is.element(MELB18_SAMg3$player2, player1vector)

addPlayer1 <- MELB18_SAMg3[ which(MELB18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_SAMg3[ which(MELB18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_SAMg2 <- rbind(MELB18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
MELB18_SAMft <- ftable(MELB18_SAMg2$player1, MELB18_SAMg2$player2)
MELB18_SAMft2 <- as.matrix(MELB18_SAMft)
numRows <- nrow(MELB18_SAMft2)
numCols <- ncol(MELB18_SAMft2)
MELB18_SAMft3 <- MELB18_SAMft2[c(2:numRows) , c(2:numCols)]
MELB18_SAMTable <- graph.adjacency(MELB18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(MELB18_SAMTable, vertex.label = V(MELB18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
MELB18_SAM.clusterCoef <- transitivity(MELB18_SAMTable, type="global") #cluster coefficient
MELB18_SAM.degreeCent <- centralization.degree(MELB18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_SAMftn <- as.network.matrix(MELB18_SAMft)
MELB18_SAM.netDensity <- network.density(MELB18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_SAM.entropy <- entropy(MELB18_SAMft) #entropy

MELB18_SAM.netMx <- cbind(MELB18_SAM.netMx, MELB18_SAM.clusterCoef, MELB18_SAM.degreeCent$centralization,
                          MELB18_SAM.netDensity, MELB18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
MELB18_TAMg2 <- data.frame(MELB18_TAM)
MELB18_TAMg2 <- MELB18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_TAMg2$player1
player2vector <- MELB18_TAMg2$player2
MELB18_TAMg3 <- MELB18_TAMg2
MELB18_TAMg3$p1inp2vec <- is.element(MELB18_TAMg3$player1, player2vector)
MELB18_TAMg3$p2inp1vec <- is.element(MELB18_TAMg3$player2, player1vector)

addPlayer1 <- MELB18_TAMg3[ which(MELB18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_TAMg3[ which(MELB18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_TAMg2 <- rbind(MELB18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
MELB18_TAMft <- ftable(MELB18_TAMg2$player1, MELB18_TAMg2$player2)
MELB18_TAMft2 <- as.matrix(MELB18_TAMft)
numRows <- nrow(MELB18_TAMft2)
numCols <- ncol(MELB18_TAMft2)
MELB18_TAMft3 <- MELB18_TAMft2[c(2:numRows) , c(2:numCols)]
MELB18_TAMTable <- graph.adjacency(MELB18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(MELB18_TAMTable, vertex.label = V(MELB18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
MELB18_TAM.clusterCoef <- transitivity(MELB18_TAMTable, type="global") #cluster coefficient
MELB18_TAM.degreeCent <- centralization.degree(MELB18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_TAMftn <- as.network.matrix(MELB18_TAMft)
MELB18_TAM.netDensity <- network.density(MELB18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_TAM.entropy <- entropy(MELB18_TAMft) #entropy

MELB18_TAM.netMx <- cbind(MELB18_TAM.netMx, MELB18_TAM.clusterCoef, MELB18_TAM.degreeCent$centralization,
                          MELB18_TAM.netDensity, MELB18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************
#NA

round = 18
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
MELB18_SDMg2 <- data.frame(MELB18_SDM)
MELB18_SDMg2 <- MELB18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_SDMg2$player1
player2vector <- MELB18_SDMg2$player2
MELB18_SDMg3 <- MELB18_SDMg2
MELB18_SDMg3$p1inp2vec <- is.element(MELB18_SDMg3$player1, player2vector)
MELB18_SDMg3$p2inp1vec <- is.element(MELB18_SDMg3$player2, player1vector)

addPlayer1 <- MELB18_SDMg3[ which(MELB18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_SDMg3[ which(MELB18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_SDMg2 <- rbind(MELB18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
MELB18_SDMft <- ftable(MELB18_SDMg2$player1, MELB18_SDMg2$player2)
MELB18_SDMft2 <- as.matrix(MELB18_SDMft)
numRows <- nrow(MELB18_SDMft2)
numCols <- ncol(MELB18_SDMft2)
MELB18_SDMft3 <- MELB18_SDMft2[c(2:numRows) , c(2:numCols)]
MELB18_SDMTable <- graph.adjacency(MELB18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(MELB18_SDMTable, vertex.label = V(MELB18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
MELB18_SDM.clusterCoef <- transitivity(MELB18_SDMTable, type="global") #cluster coefficient
MELB18_SDM.degreeCent <- centralization.degree(MELB18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_SDMftn <- as.network.matrix(MELB18_SDMft)
MELB18_SDM.netDensity <- network.density(MELB18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_SDM.entropy <- entropy(MELB18_SDMft) #entropy

MELB18_SDM.netMx <- cbind(MELB18_SDM.netMx, MELB18_SDM.clusterCoef, MELB18_SDM.degreeCent$centralization,
                          MELB18_SDM.netDensity, MELB18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
MELB18_TDMg2 <- data.frame(MELB18_TDM)
MELB18_TDMg2 <- MELB18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_TDMg2$player1
player2vector <- MELB18_TDMg2$player2
MELB18_TDMg3 <- MELB18_TDMg2
MELB18_TDMg3$p1inp2vec <- is.element(MELB18_TDMg3$player1, player2vector)
MELB18_TDMg3$p2inp1vec <- is.element(MELB18_TDMg3$player2, player1vector)

addPlayer1 <- MELB18_TDMg3[ which(MELB18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB18_TDMg3[ which(MELB18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_TDMg2 <- rbind(MELB18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
MELB18_TDMft <- ftable(MELB18_TDMg2$player1, MELB18_TDMg2$player2)
MELB18_TDMft2 <- as.matrix(MELB18_TDMft)
numRows <- nrow(MELB18_TDMft2)
numCols <- ncol(MELB18_TDMft2)
MELB18_TDMft3 <- MELB18_TDMft2[c(2:numRows) , c(2:numCols)]
MELB18_TDMTable <- graph.adjacency(MELB18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(MELB18_TDMTable, vertex.label = V(MELB18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
MELB18_TDM.clusterCoef <- transitivity(MELB18_TDMTable, type="global") #cluster coefficient
MELB18_TDM.degreeCent <- centralization.degree(MELB18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_TDMftn <- as.network.matrix(MELB18_TDMft)
MELB18_TDM.netDensity <- network.density(MELB18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_TDM.entropy <- entropy(MELB18_TDMft) #entropy

MELB18_TDM.netMx <- cbind(MELB18_TDM.netMx, MELB18_TDM.clusterCoef, MELB18_TDM.degreeCent$centralization,
                          MELB18_TDM.netDensity, MELB18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
MELB18_SDg2 <- data.frame(MELB18_SD)
MELB18_SDg2 <- MELB18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_SDg2$player1
player2vector <- MELB18_SDg2$player2
MELB18_SDg3 <- MELB18_SDg2
MELB18_SDg3$p1inp2vec <- is.element(MELB18_SDg3$player1, player2vector)
MELB18_SDg3$p2inp1vec <- is.element(MELB18_SDg3$player2, player1vector)

addPlayer1 <- MELB18_SDg3[ which(MELB18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_SDg3[ which(MELB18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_SDg2 <- rbind(MELB18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
MELB18_SDft <- ftable(MELB18_SDg2$player1, MELB18_SDg2$player2)
MELB18_SDft2 <- as.matrix(MELB18_SDft)
numRows <- nrow(MELB18_SDft2)
numCols <- ncol(MELB18_SDft2)
MELB18_SDft3 <- MELB18_SDft2[c(2:numRows) , c(2:numCols)]
MELB18_SDTable <- graph.adjacency(MELB18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(MELB18_SDTable, vertex.label = V(MELB18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
MELB18_SD.clusterCoef <- transitivity(MELB18_SDTable, type="global") #cluster coefficient
MELB18_SD.degreeCent <- centralization.degree(MELB18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_SDftn <- as.network.matrix(MELB18_SDft)
MELB18_SD.netDensity <- network.density(MELB18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_SD.entropy <- entropy(MELB18_SDft) #entropy

MELB18_SD.netMx <- cbind(MELB18_SD.netMx, MELB18_SD.clusterCoef, MELB18_SD.degreeCent$centralization,
                         MELB18_SD.netDensity, MELB18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
MELB18_TDg2 <- data.frame(MELB18_TD)
MELB18_TDg2 <- MELB18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_TDg2$player1
player2vector <- MELB18_TDg2$player2
MELB18_TDg3 <- MELB18_TDg2
MELB18_TDg3$p1inp2vec <- is.element(MELB18_TDg3$player1, player2vector)
MELB18_TDg3$p2inp1vec <- is.element(MELB18_TDg3$player2, player1vector)

addPlayer1 <- MELB18_TDg3[ which(MELB18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_TDg3[ which(MELB18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_TDg2 <- rbind(MELB18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
MELB18_TDft <- ftable(MELB18_TDg2$player1, MELB18_TDg2$player2)
MELB18_TDft2 <- as.matrix(MELB18_TDft)
numRows <- nrow(MELB18_TDft2)
numCols <- ncol(MELB18_TDft2)
MELB18_TDft3 <- MELB18_TDft2[c(2:numRows) , c(2:numCols)]
MELB18_TDTable <- graph.adjacency(MELB18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(MELB18_TDTable, vertex.label = V(MELB18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
MELB18_TD.clusterCoef <- transitivity(MELB18_TDTable, type="global") #cluster coefficient
MELB18_TD.degreeCent <- centralization.degree(MELB18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_TDftn <- as.network.matrix(MELB18_TDft)
MELB18_TD.netDensity <- network.density(MELB18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_TD.entropy <- entropy(MELB18_TDft) #entropy

MELB18_TD.netMx <- cbind(MELB18_TD.netMx, MELB18_TD.clusterCoef, MELB18_TD.degreeCent$centralization,
                         MELB18_TD.netDensity, MELB18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
MELB18_QTg2 <- data.frame(MELB18_QT)
MELB18_QTg2 <- MELB18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB18_QTg2$player1
player2vector <- MELB18_QTg2$player2
MELB18_QTg3 <- MELB18_QTg2
MELB18_QTg3$p1inp2vec <- is.element(MELB18_QTg3$player1, player2vector)
MELB18_QTg3$p2inp1vec <- is.element(MELB18_QTg3$player2, player1vector)

addPlayer1 <- MELB18_QTg3[ which(MELB18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB18_QTg3[ which(MELB18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB18_QTg2 <- rbind(MELB18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
MELB18_QTft <- ftable(MELB18_QTg2$player1, MELB18_QTg2$player2)
MELB18_QTft2 <- as.matrix(MELB18_QTft)
numRows <- nrow(MELB18_QTft2)
numCols <- ncol(MELB18_QTft2)
MELB18_QTft3 <- MELB18_QTft2[c(2:numRows) , c(2:numCols)]
MELB18_QTTable <- graph.adjacency(MELB18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(MELB18_QTTable, vertex.label = V(MELB18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
MELB18_QT.clusterCoef <- transitivity(MELB18_QTTable, type="global") #cluster coefficient
MELB18_QT.degreeCent <- centralization.degree(MELB18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB18_QTftn <- as.network.matrix(MELB18_QTft)
MELB18_QT.netDensity <- network.density(MELB18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB18_QT.entropy <- entropy(MELB18_QTft) #entropy

MELB18_QT.netMx <- cbind(MELB18_QT.netMx, MELB18_QT.clusterCoef, MELB18_QT.degreeCent$centralization,
                         MELB18_QT.netDensity, MELB18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB18_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
NMFC18_Gg2 <- data.frame(NMFC18_G)
NMFC18_Gg2 <- NMFC18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_Gg2$player1
player2vector <- NMFC18_Gg2$player2
NMFC18_Gg3 <- NMFC18_Gg2
NMFC18_Gg3$p1inp2vec <- is.element(NMFC18_Gg3$player1, player2vector)
NMFC18_Gg3$p2inp1vec <- is.element(NMFC18_Gg3$player2, player1vector)

addPlayer1 <- NMFC18_Gg3[ which(NMFC18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_Gg3[ which(NMFC18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_Gg2 <- rbind(NMFC18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
NMFC18_Gft <- ftable(NMFC18_Gg2$player1, NMFC18_Gg2$player2)
NMFC18_Gft2 <- as.matrix(NMFC18_Gft)
numRows <- nrow(NMFC18_Gft2)
numCols <- ncol(NMFC18_Gft2)
NMFC18_Gft3 <- NMFC18_Gft2[c(2:numRows) , c(2:numCols)]
NMFC18_GTable <- graph.adjacency(NMFC18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(NMFC18_GTable, vertex.label = V(NMFC18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
NMFC18_G.clusterCoef <- transitivity(NMFC18_GTable, type="global") #cluster coefficient
NMFC18_G.degreeCent <- centralization.degree(NMFC18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_Gftn <- as.network.matrix(NMFC18_Gft)
NMFC18_G.netDensity <- network.density(NMFC18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_G.entropy <- entropy(NMFC18_Gft) #entropy

NMFC18_G.netMx <- cbind(NMFC18_G.netMx, NMFC18_G.clusterCoef, NMFC18_G.degreeCent$centralization,
                        NMFC18_G.netDensity, NMFC18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
NMFC18_Bg2 <- data.frame(NMFC18_B)
NMFC18_Bg2 <- NMFC18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_Bg2$player1
player2vector <- NMFC18_Bg2$player2
NMFC18_Bg3 <- NMFC18_Bg2
NMFC18_Bg3$p1inp2vec <- is.element(NMFC18_Bg3$player1, player2vector)
NMFC18_Bg3$p2inp1vec <- is.element(NMFC18_Bg3$player2, player1vector)

addPlayer1 <- NMFC18_Bg3[ which(NMFC18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_Bg3[ which(NMFC18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_Bg2 <- rbind(NMFC18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
NMFC18_Bft <- ftable(NMFC18_Bg2$player1, NMFC18_Bg2$player2)
NMFC18_Bft2 <- as.matrix(NMFC18_Bft)
numRows <- nrow(NMFC18_Bft2)
numCols <- ncol(NMFC18_Bft2)
NMFC18_Bft3 <- NMFC18_Bft2[c(2:numRows) , c(2:numCols)]
NMFC18_BTable <- graph.adjacency(NMFC18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(NMFC18_BTable, vertex.label = V(NMFC18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
NMFC18_B.clusterCoef <- transitivity(NMFC18_BTable, type="global") #cluster coefficient
NMFC18_B.degreeCent <- centralization.degree(NMFC18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_Bftn <- as.network.matrix(NMFC18_Bft)
NMFC18_B.netDensity <- network.density(NMFC18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_B.entropy <- entropy(NMFC18_Bft) #entropy

NMFC18_B.netMx <- cbind(NMFC18_B.netMx, NMFC18_B.clusterCoef, NMFC18_B.degreeCent$centralization,
                        NMFC18_B.netDensity, NMFC18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
NMFC18_SFg2 <- data.frame(NMFC18_SF)
NMFC18_SFg2 <- NMFC18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_SFg2$player1
player2vector <- NMFC18_SFg2$player2
NMFC18_SFg3 <- NMFC18_SFg2
NMFC18_SFg3$p1inp2vec <- is.element(NMFC18_SFg3$player1, player2vector)
NMFC18_SFg3$p2inp1vec <- is.element(NMFC18_SFg3$player2, player1vector)

addPlayer1 <- NMFC18_SFg3[ which(NMFC18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_SFg3[ which(NMFC18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_SFg2 <- rbind(NMFC18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
NMFC18_SFft <- ftable(NMFC18_SFg2$player1, NMFC18_SFg2$player2)
NMFC18_SFft2 <- as.matrix(NMFC18_SFft)
numRows <- nrow(NMFC18_SFft2)
numCols <- ncol(NMFC18_SFft2)
NMFC18_SFft3 <- NMFC18_SFft2[c(2:numRows) , c(2:numCols)]
NMFC18_SFTable <- graph.adjacency(NMFC18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(NMFC18_SFTable, vertex.label = V(NMFC18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
NMFC18_SF.clusterCoef <- transitivity(NMFC18_SFTable, type="global") #cluster coefficient
NMFC18_SF.degreeCent <- centralization.degree(NMFC18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_SFftn <- as.network.matrix(NMFC18_SFft)
NMFC18_SF.netDensity <- network.density(NMFC18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_SF.entropy <- entropy(NMFC18_SFft) #entropy

NMFC18_SF.netMx <- cbind(NMFC18_SF.netMx, NMFC18_SF.clusterCoef, NMFC18_SF.degreeCent$centralization,
                         NMFC18_SF.netDensity, NMFC18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
NMFC18_TFg2 <- data.frame(NMFC18_TF)
NMFC18_TFg2 <- NMFC18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_TFg2$player1
player2vector <- NMFC18_TFg2$player2
NMFC18_TFg3 <- NMFC18_TFg2
NMFC18_TFg3$p1inp2vec <- is.element(NMFC18_TFg3$player1, player2vector)
NMFC18_TFg3$p2inp1vec <- is.element(NMFC18_TFg3$player2, player1vector)

addPlayer1 <- NMFC18_TFg3[ which(NMFC18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_TFg3[ which(NMFC18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_TFg2 <- rbind(NMFC18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
NMFC18_TFft <- ftable(NMFC18_TFg2$player1, NMFC18_TFg2$player2)
NMFC18_TFft2 <- as.matrix(NMFC18_TFft)
numRows <- nrow(NMFC18_TFft2)
numCols <- ncol(NMFC18_TFft2)
NMFC18_TFft3 <- NMFC18_TFft2[c(2:numRows) , c(2:numCols)]
NMFC18_TFTable <- graph.adjacency(NMFC18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(NMFC18_TFTable, vertex.label = V(NMFC18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
NMFC18_TF.clusterCoef <- transitivity(NMFC18_TFTable, type="global") #cluster coefficient
NMFC18_TF.degreeCent <- centralization.degree(NMFC18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_TFftn <- as.network.matrix(NMFC18_TFft)
NMFC18_TF.netDensity <- network.density(NMFC18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_TF.entropy <- entropy(NMFC18_TFft) #entropy

NMFC18_TF.netMx <- cbind(NMFC18_TF.netMx, NMFC18_TF.clusterCoef, NMFC18_TF.degreeCent$centralization,
                         NMFC18_TF.netDensity, NMFC18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
NMFC18_SAMg2 <- data.frame(NMFC18_SAM)
NMFC18_SAMg2 <- NMFC18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_SAMg2$player1
player2vector <- NMFC18_SAMg2$player2
NMFC18_SAMg3 <- NMFC18_SAMg2
NMFC18_SAMg3$p1inp2vec <- is.element(NMFC18_SAMg3$player1, player2vector)
NMFC18_SAMg3$p2inp1vec <- is.element(NMFC18_SAMg3$player2, player1vector)

addPlayer1 <- NMFC18_SAMg3[ which(NMFC18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_SAMg3[ which(NMFC18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_SAMg2 <- rbind(NMFC18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
NMFC18_SAMft <- ftable(NMFC18_SAMg2$player1, NMFC18_SAMg2$player2)
NMFC18_SAMft2 <- as.matrix(NMFC18_SAMft)
numRows <- nrow(NMFC18_SAMft2)
numCols <- ncol(NMFC18_SAMft2)
NMFC18_SAMft3 <- NMFC18_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC18_SAMTable <- graph.adjacency(NMFC18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(NMFC18_SAMTable, vertex.label = V(NMFC18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
NMFC18_SAM.clusterCoef <- transitivity(NMFC18_SAMTable, type="global") #cluster coefficient
NMFC18_SAM.degreeCent <- centralization.degree(NMFC18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_SAMftn <- as.network.matrix(NMFC18_SAMft)
NMFC18_SAM.netDensity <- network.density(NMFC18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_SAM.entropy <- entropy(NMFC18_SAMft) #entropy

NMFC18_SAM.netMx <- cbind(NMFC18_SAM.netMx, NMFC18_SAM.clusterCoef, NMFC18_SAM.degreeCent$centralization,
                          NMFC18_SAM.netDensity, NMFC18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
NMFC18_TAMg2 <- data.frame(NMFC18_TAM)
NMFC18_TAMg2 <- NMFC18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_TAMg2$player1
player2vector <- NMFC18_TAMg2$player2
NMFC18_TAMg3 <- NMFC18_TAMg2
NMFC18_TAMg3$p1inp2vec <- is.element(NMFC18_TAMg3$player1, player2vector)
NMFC18_TAMg3$p2inp1vec <- is.element(NMFC18_TAMg3$player2, player1vector)

addPlayer1 <- NMFC18_TAMg3[ which(NMFC18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_TAMg3[ which(NMFC18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_TAMg2 <- rbind(NMFC18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
NMFC18_TAMft <- ftable(NMFC18_TAMg2$player1, NMFC18_TAMg2$player2)
NMFC18_TAMft2 <- as.matrix(NMFC18_TAMft)
numRows <- nrow(NMFC18_TAMft2)
numCols <- ncol(NMFC18_TAMft2)
NMFC18_TAMft3 <- NMFC18_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC18_TAMTable <- graph.adjacency(NMFC18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(NMFC18_TAMTable, vertex.label = V(NMFC18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
NMFC18_TAM.clusterCoef <- transitivity(NMFC18_TAMTable, type="global") #cluster coefficient
NMFC18_TAM.degreeCent <- centralization.degree(NMFC18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_TAMftn <- as.network.matrix(NMFC18_TAMft)
NMFC18_TAM.netDensity <- network.density(NMFC18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_TAM.entropy <- entropy(NMFC18_TAMft) #entropy

NMFC18_TAM.netMx <- cbind(NMFC18_TAM.netMx, NMFC18_TAM.clusterCoef, NMFC18_TAM.degreeCent$centralization,
                          NMFC18_TAM.netDensity, NMFC18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
NMFC18_SDMg2 <- data.frame(NMFC18_SDM)
NMFC18_SDMg2 <- NMFC18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_SDMg2$player1
player2vector <- NMFC18_SDMg2$player2
NMFC18_SDMg3 <- NMFC18_SDMg2
NMFC18_SDMg3$p1inp2vec <- is.element(NMFC18_SDMg3$player1, player2vector)
NMFC18_SDMg3$p2inp1vec <- is.element(NMFC18_SDMg3$player2, player1vector)

addPlayer1 <- NMFC18_SDMg3[ which(NMFC18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_SDMg3[ which(NMFC18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_SDMg2 <- rbind(NMFC18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
NMFC18_SDMft <- ftable(NMFC18_SDMg2$player1, NMFC18_SDMg2$player2)
NMFC18_SDMft2 <- as.matrix(NMFC18_SDMft)
numRows <- nrow(NMFC18_SDMft2)
numCols <- ncol(NMFC18_SDMft2)
NMFC18_SDMft3 <- NMFC18_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC18_SDMTable <- graph.adjacency(NMFC18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(NMFC18_SDMTable, vertex.label = V(NMFC18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
NMFC18_SDM.clusterCoef <- transitivity(NMFC18_SDMTable, type="global") #cluster coefficient
NMFC18_SDM.degreeCent <- centralization.degree(NMFC18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_SDMftn <- as.network.matrix(NMFC18_SDMft)
NMFC18_SDM.netDensity <- network.density(NMFC18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_SDM.entropy <- entropy(NMFC18_SDMft) #entropy

NMFC18_SDM.netMx <- cbind(NMFC18_SDM.netMx, NMFC18_SDM.clusterCoef, NMFC18_SDM.degreeCent$centralization,
                          NMFC18_SDM.netDensity, NMFC18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
NMFC18_TDMg2 <- data.frame(NMFC18_TDM)
NMFC18_TDMg2 <- NMFC18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_TDMg2$player1
player2vector <- NMFC18_TDMg2$player2
NMFC18_TDMg3 <- NMFC18_TDMg2
NMFC18_TDMg3$p1inp2vec <- is.element(NMFC18_TDMg3$player1, player2vector)
NMFC18_TDMg3$p2inp1vec <- is.element(NMFC18_TDMg3$player2, player1vector)

addPlayer1 <- NMFC18_TDMg3[ which(NMFC18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_TDMg3[ which(NMFC18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_TDMg2 <- rbind(NMFC18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
NMFC18_TDMft <- ftable(NMFC18_TDMg2$player1, NMFC18_TDMg2$player2)
NMFC18_TDMft2 <- as.matrix(NMFC18_TDMft)
numRows <- nrow(NMFC18_TDMft2)
numCols <- ncol(NMFC18_TDMft2)
NMFC18_TDMft3 <- NMFC18_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC18_TDMTable <- graph.adjacency(NMFC18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(NMFC18_TDMTable, vertex.label = V(NMFC18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
NMFC18_TDM.clusterCoef <- transitivity(NMFC18_TDMTable, type="global") #cluster coefficient
NMFC18_TDM.degreeCent <- centralization.degree(NMFC18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_TDMftn <- as.network.matrix(NMFC18_TDMft)
NMFC18_TDM.netDensity <- network.density(NMFC18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_TDM.entropy <- entropy(NMFC18_TDMft) #entropy

NMFC18_TDM.netMx <- cbind(NMFC18_TDM.netMx, NMFC18_TDM.clusterCoef, NMFC18_TDM.degreeCent$centralization,
                          NMFC18_TDM.netDensity, NMFC18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
NMFC18_SDg2 <- data.frame(NMFC18_SD)
NMFC18_SDg2 <- NMFC18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_SDg2$player1
player2vector <- NMFC18_SDg2$player2
NMFC18_SDg3 <- NMFC18_SDg2
NMFC18_SDg3$p1inp2vec <- is.element(NMFC18_SDg3$player1, player2vector)
NMFC18_SDg3$p2inp1vec <- is.element(NMFC18_SDg3$player2, player1vector)

addPlayer1 <- NMFC18_SDg3[ which(NMFC18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_SDg3[ which(NMFC18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_SDg2 <- rbind(NMFC18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
NMFC18_SDft <- ftable(NMFC18_SDg2$player1, NMFC18_SDg2$player2)
NMFC18_SDft2 <- as.matrix(NMFC18_SDft)
numRows <- nrow(NMFC18_SDft2)
numCols <- ncol(NMFC18_SDft2)
NMFC18_SDft3 <- NMFC18_SDft2[c(2:numRows) , c(2:numCols)]
NMFC18_SDTable <- graph.adjacency(NMFC18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(NMFC18_SDTable, vertex.label = V(NMFC18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
NMFC18_SD.clusterCoef <- transitivity(NMFC18_SDTable, type="global") #cluster coefficient
NMFC18_SD.degreeCent <- centralization.degree(NMFC18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_SDftn <- as.network.matrix(NMFC18_SDft)
NMFC18_SD.netDensity <- network.density(NMFC18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_SD.entropy <- entropy(NMFC18_SDft) #entropy

NMFC18_SD.netMx <- cbind(NMFC18_SD.netMx, NMFC18_SD.clusterCoef, NMFC18_SD.degreeCent$centralization,
                         NMFC18_SD.netDensity, NMFC18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************

round = 18
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
NMFC18_TDg2 <- data.frame(NMFC18_TD)
NMFC18_TDg2 <- NMFC18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_TDg2$player1
player2vector <- NMFC18_TDg2$player2
NMFC18_TDg3 <- NMFC18_TDg2
NMFC18_TDg3$p1inp2vec <- is.element(NMFC18_TDg3$player1, player2vector)
NMFC18_TDg3$p2inp1vec <- is.element(NMFC18_TDg3$player2, player1vector)

addPlayer1 <- NMFC18_TDg3[ which(NMFC18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_TDg3[ which(NMFC18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_TDg2 <- rbind(NMFC18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
NMFC18_TDft <- ftable(NMFC18_TDg2$player1, NMFC18_TDg2$player2)
NMFC18_TDft2 <- as.matrix(NMFC18_TDft)
numRows <- nrow(NMFC18_TDft2)
numCols <- ncol(NMFC18_TDft2)
NMFC18_TDft3 <- NMFC18_TDft2[c(2:numRows) , c(2:numCols)]
NMFC18_TDTable <- graph.adjacency(NMFC18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(NMFC18_TDTable, vertex.label = V(NMFC18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
NMFC18_TD.clusterCoef <- transitivity(NMFC18_TDTable, type="global") #cluster coefficient
NMFC18_TD.degreeCent <- centralization.degree(NMFC18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_TDftn <- as.network.matrix(NMFC18_TDft)
NMFC18_TD.netDensity <- network.density(NMFC18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_TD.entropy <- entropy(NMFC18_TDft) #entropy

NMFC18_TD.netMx <- cbind(NMFC18_TD.netMx, NMFC18_TD.clusterCoef, NMFC18_TD.degreeCent$centralization,
                         NMFC18_TD.netDensity, NMFC18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
NMFC18_QTg2 <- data.frame(NMFC18_QT)
NMFC18_QTg2 <- NMFC18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC18_QTg2$player1
player2vector <- NMFC18_QTg2$player2
NMFC18_QTg3 <- NMFC18_QTg2
NMFC18_QTg3$p1inp2vec <- is.element(NMFC18_QTg3$player1, player2vector)
NMFC18_QTg3$p2inp1vec <- is.element(NMFC18_QTg3$player2, player1vector)

addPlayer1 <- NMFC18_QTg3[ which(NMFC18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC18_QTg3[ which(NMFC18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC18_QTg2 <- rbind(NMFC18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
NMFC18_QTft <- ftable(NMFC18_QTg2$player1, NMFC18_QTg2$player2)
NMFC18_QTft2 <- as.matrix(NMFC18_QTft)
numRows <- nrow(NMFC18_QTft2)
numCols <- ncol(NMFC18_QTft2)
NMFC18_QTft3 <- NMFC18_QTft2[c(2:numRows) , c(2:numCols)]
NMFC18_QTTable <- graph.adjacency(NMFC18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(NMFC18_QTTable, vertex.label = V(NMFC18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
NMFC18_QT.clusterCoef <- transitivity(NMFC18_QTTable, type="global") #cluster coefficient
NMFC18_QT.degreeCent <- centralization.degree(NMFC18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC18_QTftn <- as.network.matrix(NMFC18_QTft)
NMFC18_QT.netDensity <- network.density(NMFC18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC18_QT.entropy <- entropy(NMFC18_QTft) #entropy

NMFC18_QT.netMx <- cbind(NMFC18_QT.netMx, NMFC18_QT.clusterCoef, NMFC18_QT.degreeCent$centralization,
                         NMFC18_QT.netDensity, NMFC18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC18_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "PORT"
KIoutcome = "Goal_F"
PORT18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
PORT18_Gg2 <- data.frame(PORT18_G)
PORT18_Gg2 <- PORT18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_Gg2$player1
player2vector <- PORT18_Gg2$player2
PORT18_Gg3 <- PORT18_Gg2
PORT18_Gg3$p1inp2vec <- is.element(PORT18_Gg3$player1, player2vector)
PORT18_Gg3$p2inp1vec <- is.element(PORT18_Gg3$player2, player1vector)

addPlayer1 <- PORT18_Gg3[ which(PORT18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_Gg3[ which(PORT18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_Gg2 <- rbind(PORT18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
PORT18_Gft <- ftable(PORT18_Gg2$player1, PORT18_Gg2$player2)
PORT18_Gft2 <- as.matrix(PORT18_Gft)
numRows <- nrow(PORT18_Gft2)
numCols <- ncol(PORT18_Gft2)
PORT18_Gft3 <- PORT18_Gft2[c(2:numRows) , c(2:numCols)]
PORT18_GTable <- graph.adjacency(PORT18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(PORT18_GTable, vertex.label = V(PORT18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
PORT18_G.clusterCoef <- transitivity(PORT18_GTable, type="global") #cluster coefficient
PORT18_G.degreeCent <- centralization.degree(PORT18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_Gftn <- as.network.matrix(PORT18_Gft)
PORT18_G.netDensity <- network.density(PORT18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_G.entropy <- entropy(PORT18_Gft) #entropy

PORT18_G.netMx <- cbind(PORT18_G.netMx, PORT18_G.clusterCoef, PORT18_G.degreeCent$centralization,
                        PORT18_G.netDensity, PORT18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************

round = 18
teamName = "PORT"
KIoutcome = "Behind_F"
PORT18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
PORT18_Bg2 <- data.frame(PORT18_B)
PORT18_Bg2 <- PORT18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_Bg2$player1
player2vector <- PORT18_Bg2$player2
PORT18_Bg3 <- PORT18_Bg2
PORT18_Bg3$p1inp2vec <- is.element(PORT18_Bg3$player1, player2vector)
PORT18_Bg3$p2inp1vec <- is.element(PORT18_Bg3$player2, player1vector)

addPlayer1 <- PORT18_Bg3[ which(PORT18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_Bg3[ which(PORT18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_Bg2 <- rbind(PORT18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
PORT18_Bft <- ftable(PORT18_Bg2$player1, PORT18_Bg2$player2)
PORT18_Bft2 <- as.matrix(PORT18_Bft)
numRows <- nrow(PORT18_Bft2)
numCols <- ncol(PORT18_Bft2)
PORT18_Bft3 <- PORT18_Bft2[c(2:numRows) , c(2:numCols)]
PORT18_BTable <- graph.adjacency(PORT18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(PORT18_BTable, vertex.label = V(PORT18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
PORT18_B.clusterCoef <- transitivity(PORT18_BTable, type="global") #cluster coefficient
PORT18_B.degreeCent <- centralization.degree(PORT18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_Bftn <- as.network.matrix(PORT18_Bft)
PORT18_B.netDensity <- network.density(PORT18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_B.entropy <- entropy(PORT18_Bft) #entropy

PORT18_B.netMx <- cbind(PORT18_B.netMx, PORT18_B.clusterCoef, PORT18_B.degreeCent$centralization,
                        PORT18_B.netDensity, PORT18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
PORT18_SFg2 <- data.frame(PORT18_SF)
PORT18_SFg2 <- PORT18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_SFg2$player1
player2vector <- PORT18_SFg2$player2
PORT18_SFg3 <- PORT18_SFg2
PORT18_SFg3$p1inp2vec <- is.element(PORT18_SFg3$player1, player2vector)
PORT18_SFg3$p2inp1vec <- is.element(PORT18_SFg3$player2, player1vector)

addPlayer1 <- PORT18_SFg3[ which(PORT18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_SFg3[ which(PORT18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_SFg2 <- rbind(PORT18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
PORT18_SFft <- ftable(PORT18_SFg2$player1, PORT18_SFg2$player2)
PORT18_SFft2 <- as.matrix(PORT18_SFft)
numRows <- nrow(PORT18_SFft2)
numCols <- ncol(PORT18_SFft2)
PORT18_SFft3 <- PORT18_SFft2[c(2:numRows) , c(2:numCols)]
PORT18_SFTable <- graph.adjacency(PORT18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(PORT18_SFTable, vertex.label = V(PORT18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
PORT18_SF.clusterCoef <- transitivity(PORT18_SFTable, type="global") #cluster coefficient
PORT18_SF.degreeCent <- centralization.degree(PORT18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_SFftn <- as.network.matrix(PORT18_SFft)
PORT18_SF.netDensity <- network.density(PORT18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_SF.entropy <- entropy(PORT18_SFft) #entropy

PORT18_SF.netMx <- cbind(PORT18_SF.netMx, PORT18_SF.clusterCoef, PORT18_SF.degreeCent$centralization,
                         PORT18_SF.netDensity, PORT18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
PORT18_TFg2 <- data.frame(PORT18_TF)
PORT18_TFg2 <- PORT18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_TFg2$player1
player2vector <- PORT18_TFg2$player2
PORT18_TFg3 <- PORT18_TFg2
PORT18_TFg3$p1inp2vec <- is.element(PORT18_TFg3$player1, player2vector)
PORT18_TFg3$p2inp1vec <- is.element(PORT18_TFg3$player2, player1vector)

addPlayer1 <- PORT18_TFg3[ which(PORT18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_TFg3[ which(PORT18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_TFg2 <- rbind(PORT18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
PORT18_TFft <- ftable(PORT18_TFg2$player1, PORT18_TFg2$player2)
PORT18_TFft2 <- as.matrix(PORT18_TFft)
numRows <- nrow(PORT18_TFft2)
numCols <- ncol(PORT18_TFft2)
PORT18_TFft3 <- PORT18_TFft2[c(2:numRows) , c(2:numCols)]
PORT18_TFTable <- graph.adjacency(PORT18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(PORT18_TFTable, vertex.label = V(PORT18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
PORT18_TF.clusterCoef <- transitivity(PORT18_TFTable, type="global") #cluster coefficient
PORT18_TF.degreeCent <- centralization.degree(PORT18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_TFftn <- as.network.matrix(PORT18_TFft)
PORT18_TF.netDensity <- network.density(PORT18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_TF.entropy <- entropy(PORT18_TFft) #entropy

PORT18_TF.netMx <- cbind(PORT18_TF.netMx, PORT18_TF.clusterCoef, PORT18_TF.degreeCent$centralization,
                         PORT18_TF.netDensity, PORT18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
PORT18_SAMg2 <- data.frame(PORT18_SAM)
PORT18_SAMg2 <- PORT18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_SAMg2$player1
player2vector <- PORT18_SAMg2$player2
PORT18_SAMg3 <- PORT18_SAMg2
PORT18_SAMg3$p1inp2vec <- is.element(PORT18_SAMg3$player1, player2vector)
PORT18_SAMg3$p2inp1vec <- is.element(PORT18_SAMg3$player2, player1vector)

addPlayer1 <- PORT18_SAMg3[ which(PORT18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_SAMg3[ which(PORT18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_SAMg2 <- rbind(PORT18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
PORT18_SAMft <- ftable(PORT18_SAMg2$player1, PORT18_SAMg2$player2)
PORT18_SAMft2 <- as.matrix(PORT18_SAMft)
numRows <- nrow(PORT18_SAMft2)
numCols <- ncol(PORT18_SAMft2)
PORT18_SAMft3 <- PORT18_SAMft2[c(2:numRows) , c(2:numCols)]
PORT18_SAMTable <- graph.adjacency(PORT18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(PORT18_SAMTable, vertex.label = V(PORT18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
PORT18_SAM.clusterCoef <- transitivity(PORT18_SAMTable, type="global") #cluster coefficient
PORT18_SAM.degreeCent <- centralization.degree(PORT18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_SAMftn <- as.network.matrix(PORT18_SAMft)
PORT18_SAM.netDensity <- network.density(PORT18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_SAM.entropy <- entropy(PORT18_SAMft) #entropy

PORT18_SAM.netMx <- cbind(PORT18_SAM.netMx, PORT18_SAM.clusterCoef, PORT18_SAM.degreeCent$centralization,
                          PORT18_SAM.netDensity, PORT18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
PORT18_TAMg2 <- data.frame(PORT18_TAM)
PORT18_TAMg2 <- PORT18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_TAMg2$player1
player2vector <- PORT18_TAMg2$player2
PORT18_TAMg3 <- PORT18_TAMg2
PORT18_TAMg3$p1inp2vec <- is.element(PORT18_TAMg3$player1, player2vector)
PORT18_TAMg3$p2inp1vec <- is.element(PORT18_TAMg3$player2, player1vector)

addPlayer1 <- PORT18_TAMg3[ which(PORT18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_TAMg3[ which(PORT18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_TAMg2 <- rbind(PORT18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
PORT18_TAMft <- ftable(PORT18_TAMg2$player1, PORT18_TAMg2$player2)
PORT18_TAMft2 <- as.matrix(PORT18_TAMft)
numRows <- nrow(PORT18_TAMft2)
numCols <- ncol(PORT18_TAMft2)
PORT18_TAMft3 <- PORT18_TAMft2[c(2:numRows) , c(2:numCols)]
PORT18_TAMTable <- graph.adjacency(PORT18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(PORT18_TAMTable, vertex.label = V(PORT18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
PORT18_TAM.clusterCoef <- transitivity(PORT18_TAMTable, type="global") #cluster coefficient
PORT18_TAM.degreeCent <- centralization.degree(PORT18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_TAMftn <- as.network.matrix(PORT18_TAMft)
PORT18_TAM.netDensity <- network.density(PORT18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_TAM.entropy <- entropy(PORT18_TAMft) #entropy

PORT18_TAM.netMx <- cbind(PORT18_TAM.netMx, PORT18_TAM.clusterCoef, PORT18_TAM.degreeCent$centralization,
                          PORT18_TAM.netDensity, PORT18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************
#NA

round = 18
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
PORT18_SDMg2 <- data.frame(PORT18_SDM)
PORT18_SDMg2 <- PORT18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_SDMg2$player1
player2vector <- PORT18_SDMg2$player2
PORT18_SDMg3 <- PORT18_SDMg2
PORT18_SDMg3$p1inp2vec <- is.element(PORT18_SDMg3$player1, player2vector)
PORT18_SDMg3$p2inp1vec <- is.element(PORT18_SDMg3$player2, player1vector)

addPlayer1 <- PORT18_SDMg3[ which(PORT18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_SDMg3[ which(PORT18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_SDMg2 <- rbind(PORT18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
PORT18_SDMft <- ftable(PORT18_SDMg2$player1, PORT18_SDMg2$player2)
PORT18_SDMft2 <- as.matrix(PORT18_SDMft)
numRows <- nrow(PORT18_SDMft2)
numCols <- ncol(PORT18_SDMft2)
PORT18_SDMft3 <- PORT18_SDMft2[c(2:numRows) , c(2:numCols)]
PORT18_SDMTable <- graph.adjacency(PORT18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(PORT18_SDMTable, vertex.label = V(PORT18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
PORT18_SDM.clusterCoef <- transitivity(PORT18_SDMTable, type="global") #cluster coefficient
PORT18_SDM.degreeCent <- centralization.degree(PORT18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_SDMftn <- as.network.matrix(PORT18_SDMft)
PORT18_SDM.netDensity <- network.density(PORT18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_SDM.entropy <- entropy(PORT18_SDMft) #entropy

PORT18_SDM.netMx <- cbind(PORT18_SDM.netMx, PORT18_SDM.clusterCoef, PORT18_SDM.degreeCent$centralization,
                          PORT18_SDM.netDensity, PORT18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
PORT18_TDMg2 <- data.frame(PORT18_TDM)
PORT18_TDMg2 <- PORT18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_TDMg2$player1
player2vector <- PORT18_TDMg2$player2
PORT18_TDMg3 <- PORT18_TDMg2
PORT18_TDMg3$p1inp2vec <- is.element(PORT18_TDMg3$player1, player2vector)
PORT18_TDMg3$p2inp1vec <- is.element(PORT18_TDMg3$player2, player1vector)

addPlayer1 <- PORT18_TDMg3[ which(PORT18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_TDMg3[ which(PORT18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_TDMg2 <- rbind(PORT18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
PORT18_TDMft <- ftable(PORT18_TDMg2$player1, PORT18_TDMg2$player2)
PORT18_TDMft2 <- as.matrix(PORT18_TDMft)
numRows <- nrow(PORT18_TDMft2)
numCols <- ncol(PORT18_TDMft2)
PORT18_TDMft3 <- PORT18_TDMft2[c(2:numRows) , c(2:numCols)]
PORT18_TDMTable <- graph.adjacency(PORT18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(PORT18_TDMTable, vertex.label = V(PORT18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
PORT18_TDM.clusterCoef <- transitivity(PORT18_TDMTable, type="global") #cluster coefficient
PORT18_TDM.degreeCent <- centralization.degree(PORT18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_TDMftn <- as.network.matrix(PORT18_TDMft)
PORT18_TDM.netDensity <- network.density(PORT18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_TDM.entropy <- entropy(PORT18_TDMft) #entropy

PORT18_TDM.netMx <- cbind(PORT18_TDM.netMx, PORT18_TDM.clusterCoef, PORT18_TDM.degreeCent$centralization,
                          PORT18_TDM.netDensity, PORT18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
PORT18_SDg2 <- data.frame(PORT18_SD)
PORT18_SDg2 <- PORT18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_SDg2$player1
player2vector <- PORT18_SDg2$player2
PORT18_SDg3 <- PORT18_SDg2
PORT18_SDg3$p1inp2vec <- is.element(PORT18_SDg3$player1, player2vector)
PORT18_SDg3$p2inp1vec <- is.element(PORT18_SDg3$player2, player1vector)

addPlayer1 <- PORT18_SDg3[ which(PORT18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_SDg3[ which(PORT18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_SDg2 <- rbind(PORT18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
PORT18_SDft <- ftable(PORT18_SDg2$player1, PORT18_SDg2$player2)
PORT18_SDft2 <- as.matrix(PORT18_SDft)
numRows <- nrow(PORT18_SDft2)
numCols <- ncol(PORT18_SDft2)
PORT18_SDft3 <- PORT18_SDft2[c(2:numRows) , c(2:numCols)]
PORT18_SDTable <- graph.adjacency(PORT18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(PORT18_SDTable, vertex.label = V(PORT18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
PORT18_SD.clusterCoef <- transitivity(PORT18_SDTable, type="global") #cluster coefficient
PORT18_SD.degreeCent <- centralization.degree(PORT18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_SDftn <- as.network.matrix(PORT18_SDft)
PORT18_SD.netDensity <- network.density(PORT18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_SD.entropy <- entropy(PORT18_SDft) #entropy

PORT18_SD.netMx <- cbind(PORT18_SD.netMx, PORT18_SD.clusterCoef, PORT18_SD.degreeCent$centralization,
                         PORT18_SD.netDensity, PORT18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
PORT18_TDg2 <- data.frame(PORT18_TD)
PORT18_TDg2 <- PORT18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_TDg2$player1
player2vector <- PORT18_TDg2$player2
PORT18_TDg3 <- PORT18_TDg2
PORT18_TDg3$p1inp2vec <- is.element(PORT18_TDg3$player1, player2vector)
PORT18_TDg3$p2inp1vec <- is.element(PORT18_TDg3$player2, player1vector)

addPlayer1 <- PORT18_TDg3[ which(PORT18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_TDg3[ which(PORT18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_TDg2 <- rbind(PORT18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
PORT18_TDft <- ftable(PORT18_TDg2$player1, PORT18_TDg2$player2)
PORT18_TDft2 <- as.matrix(PORT18_TDft)
numRows <- nrow(PORT18_TDft2)
numCols <- ncol(PORT18_TDft2)
PORT18_TDft3 <- PORT18_TDft2[c(2:numRows) , c(2:numCols)]
PORT18_TDTable <- graph.adjacency(PORT18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(PORT18_TDTable, vertex.label = V(PORT18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
PORT18_TD.clusterCoef <- transitivity(PORT18_TDTable, type="global") #cluster coefficient
PORT18_TD.degreeCent <- centralization.degree(PORT18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_TDftn <- as.network.matrix(PORT18_TDft)
PORT18_TD.netDensity <- network.density(PORT18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_TD.entropy <- entropy(PORT18_TDft) #entropy

PORT18_TD.netMx <- cbind(PORT18_TD.netMx, PORT18_TD.clusterCoef, PORT18_TD.degreeCent$centralization,
                         PORT18_TD.netDensity, PORT18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
PORT18_QTg2 <- data.frame(PORT18_QT)
PORT18_QTg2 <- PORT18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT18_QTg2$player1
player2vector <- PORT18_QTg2$player2
PORT18_QTg3 <- PORT18_QTg2
PORT18_QTg3$p1inp2vec <- is.element(PORT18_QTg3$player1, player2vector)
PORT18_QTg3$p2inp1vec <- is.element(PORT18_QTg3$player2, player1vector)

addPlayer1 <- PORT18_QTg3[ which(PORT18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT18_QTg3[ which(PORT18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT18_QTg2 <- rbind(PORT18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
PORT18_QTft <- ftable(PORT18_QTg2$player1, PORT18_QTg2$player2)
PORT18_QTft2 <- as.matrix(PORT18_QTft)
numRows <- nrow(PORT18_QTft2)
numCols <- ncol(PORT18_QTft2)
PORT18_QTft3 <- PORT18_QTft2[c(2:numRows) , c(2:numCols)]
PORT18_QTTable <- graph.adjacency(PORT18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(PORT18_QTTable, vertex.label = V(PORT18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
PORT18_QT.clusterCoef <- transitivity(PORT18_QTTable, type="global") #cluster coefficient
PORT18_QT.degreeCent <- centralization.degree(PORT18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT18_QTftn <- as.network.matrix(PORT18_QTft)
PORT18_QT.netDensity <- network.density(PORT18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT18_QT.entropy <- entropy(PORT18_QTft) #entropy

PORT18_QT.netMx <- cbind(PORT18_QT.netMx, PORT18_QT.clusterCoef, PORT18_QT.degreeCent$centralization,
                         PORT18_QT.netDensity, PORT18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT18_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Goal_F"
RICH18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
RICH18_Gg2 <- data.frame(RICH18_G)
RICH18_Gg2 <- RICH18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_Gg2$player1
player2vector <- RICH18_Gg2$player2
RICH18_Gg3 <- RICH18_Gg2
RICH18_Gg3$p1inp2vec <- is.element(RICH18_Gg3$player1, player2vector)
RICH18_Gg3$p2inp1vec <- is.element(RICH18_Gg3$player2, player1vector)

addPlayer1 <- RICH18_Gg3[ which(RICH18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_Gg3[ which(RICH18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_Gg2 <- rbind(RICH18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
RICH18_Gft <- ftable(RICH18_Gg2$player1, RICH18_Gg2$player2)
RICH18_Gft2 <- as.matrix(RICH18_Gft)
numRows <- nrow(RICH18_Gft2)
numCols <- ncol(RICH18_Gft2)
RICH18_Gft3 <- RICH18_Gft2[c(2:numRows) , c(2:numCols)]
RICH18_GTable <- graph.adjacency(RICH18_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(RICH18_GTable, vertex.label = V(RICH18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
RICH18_G.clusterCoef <- transitivity(RICH18_GTable, type="global") #cluster coefficient
RICH18_G.degreeCent <- centralization.degree(RICH18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_Gftn <- as.network.matrix(RICH18_Gft)
RICH18_G.netDensity <- network.density(RICH18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_G.entropy <- entropy(RICH18_Gft) #entropy

RICH18_G.netMx <- cbind(RICH18_G.netMx, RICH18_G.clusterCoef, RICH18_G.degreeCent$centralization,
                        RICH18_G.netDensity, RICH18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Behind_F"
RICH18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
RICH18_Bg2 <- data.frame(RICH18_B)
RICH18_Bg2 <- RICH18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_Bg2$player1
player2vector <- RICH18_Bg2$player2
RICH18_Bg3 <- RICH18_Bg2
RICH18_Bg3$p1inp2vec <- is.element(RICH18_Bg3$player1, player2vector)
RICH18_Bg3$p2inp1vec <- is.element(RICH18_Bg3$player2, player1vector)

addPlayer1 <- RICH18_Bg3[ which(RICH18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_Bg3[ which(RICH18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_Bg2 <- rbind(RICH18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
RICH18_Bft <- ftable(RICH18_Bg2$player1, RICH18_Bg2$player2)
RICH18_Bft2 <- as.matrix(RICH18_Bft)
numRows <- nrow(RICH18_Bft2)
numCols <- ncol(RICH18_Bft2)
RICH18_Bft3 <- RICH18_Bft2[c(2:numRows) , c(2:numCols)]
RICH18_BTable <- graph.adjacency(RICH18_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(RICH18_BTable, vertex.label = V(RICH18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
RICH18_B.clusterCoef <- transitivity(RICH18_BTable, type="global") #cluster coefficient
RICH18_B.degreeCent <- centralization.degree(RICH18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_Bftn <- as.network.matrix(RICH18_Bft)
RICH18_B.netDensity <- network.density(RICH18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_B.entropy <- entropy(RICH18_Bft) #entropy

RICH18_B.netMx <- cbind(RICH18_B.netMx, RICH18_B.clusterCoef, RICH18_B.degreeCent$centralization,
                        RICH18_B.netDensity, RICH18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
RICH18_SFg2 <- data.frame(RICH18_SF)
RICH18_SFg2 <- RICH18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_SFg2$player1
player2vector <- RICH18_SFg2$player2
RICH18_SFg3 <- RICH18_SFg2
RICH18_SFg3$p1inp2vec <- is.element(RICH18_SFg3$player1, player2vector)
RICH18_SFg3$p2inp1vec <- is.element(RICH18_SFg3$player2, player1vector)

addPlayer1 <- RICH18_SFg3[ which(RICH18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_SFg3[ which(RICH18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_SFg2 <- rbind(RICH18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
RICH18_SFft <- ftable(RICH18_SFg2$player1, RICH18_SFg2$player2)
RICH18_SFft2 <- as.matrix(RICH18_SFft)
numRows <- nrow(RICH18_SFft2)
numCols <- ncol(RICH18_SFft2)
RICH18_SFft3 <- RICH18_SFft2[c(2:numRows) , c(2:numCols)]
RICH18_SFTable <- graph.adjacency(RICH18_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(RICH18_SFTable, vertex.label = V(RICH18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
RICH18_SF.clusterCoef <- transitivity(RICH18_SFTable, type="global") #cluster coefficient
RICH18_SF.degreeCent <- centralization.degree(RICH18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_SFftn <- as.network.matrix(RICH18_SFft)
RICH18_SF.netDensity <- network.density(RICH18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_SF.entropy <- entropy(RICH18_SFft) #entropy

RICH18_SF.netMx <- cbind(RICH18_SF.netMx, RICH18_SF.clusterCoef, RICH18_SF.degreeCent$centralization,
                         RICH18_SF.netDensity, RICH18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
RICH18_TFg2 <- data.frame(RICH18_TF)
RICH18_TFg2 <- RICH18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_TFg2$player1
player2vector <- RICH18_TFg2$player2
RICH18_TFg3 <- RICH18_TFg2
RICH18_TFg3$p1inp2vec <- is.element(RICH18_TFg3$player1, player2vector)
RICH18_TFg3$p2inp1vec <- is.element(RICH18_TFg3$player2, player1vector)

addPlayer1 <- RICH18_TFg3[ which(RICH18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_TFg3[ which(RICH18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_TFg2 <- rbind(RICH18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
RICH18_TFft <- ftable(RICH18_TFg2$player1, RICH18_TFg2$player2)
RICH18_TFft2 <- as.matrix(RICH18_TFft)
numRows <- nrow(RICH18_TFft2)
numCols <- ncol(RICH18_TFft2)
RICH18_TFft3 <- RICH18_TFft2[c(2:numRows) , c(2:numCols)]
RICH18_TFTable <- graph.adjacency(RICH18_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(RICH18_TFTable, vertex.label = V(RICH18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
RICH18_TF.clusterCoef <- transitivity(RICH18_TFTable, type="global") #cluster coefficient
RICH18_TF.degreeCent <- centralization.degree(RICH18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_TFftn <- as.network.matrix(RICH18_TFft)
RICH18_TF.netDensity <- network.density(RICH18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_TF.entropy <- entropy(RICH18_TFft) #entropy

RICH18_TF.netMx <- cbind(RICH18_TF.netMx, RICH18_TF.clusterCoef, RICH18_TF.degreeCent$centralization,
                         RICH18_TF.netDensity, RICH18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************

round = 18
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
RICH18_SAMg2 <- data.frame(RICH18_SAM)
RICH18_SAMg2 <- RICH18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_SAMg2$player1
player2vector <- RICH18_SAMg2$player2
RICH18_SAMg3 <- RICH18_SAMg2
RICH18_SAMg3$p1inp2vec <- is.element(RICH18_SAMg3$player1, player2vector)
RICH18_SAMg3$p2inp1vec <- is.element(RICH18_SAMg3$player2, player1vector)

addPlayer1 <- RICH18_SAMg3[ which(RICH18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_SAMg3[ which(RICH18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_SAMg2 <- rbind(RICH18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
RICH18_SAMft <- ftable(RICH18_SAMg2$player1, RICH18_SAMg2$player2)
RICH18_SAMft2 <- as.matrix(RICH18_SAMft)
numRows <- nrow(RICH18_SAMft2)
numCols <- ncol(RICH18_SAMft2)
RICH18_SAMft3 <- RICH18_SAMft2[c(2:numRows) , c(2:numCols)]
RICH18_SAMTable <- graph.adjacency(RICH18_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(RICH18_SAMTable, vertex.label = V(RICH18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
RICH18_SAM.clusterCoef <- transitivity(RICH18_SAMTable, type="global") #cluster coefficient
RICH18_SAM.degreeCent <- centralization.degree(RICH18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_SAMftn <- as.network.matrix(RICH18_SAMft)
RICH18_SAM.netDensity <- network.density(RICH18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_SAM.entropy <- entropy(RICH18_SAMft) #entropy

RICH18_SAM.netMx <- cbind(RICH18_SAM.netMx, RICH18_SAM.clusterCoef, RICH18_SAM.degreeCent$centralization,
                          RICH18_SAM.netDensity, RICH18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
RICH18_TAMg2 <- data.frame(RICH18_TAM)
RICH18_TAMg2 <- RICH18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_TAMg2$player1
player2vector <- RICH18_TAMg2$player2
RICH18_TAMg3 <- RICH18_TAMg2
RICH18_TAMg3$p1inp2vec <- is.element(RICH18_TAMg3$player1, player2vector)
RICH18_TAMg3$p2inp1vec <- is.element(RICH18_TAMg3$player2, player1vector)

addPlayer1 <- RICH18_TAMg3[ which(RICH18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_TAMg3[ which(RICH18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_TAMg2 <- rbind(RICH18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
RICH18_TAMft <- ftable(RICH18_TAMg2$player1, RICH18_TAMg2$player2)
RICH18_TAMft2 <- as.matrix(RICH18_TAMft)
numRows <- nrow(RICH18_TAMft2)
numCols <- ncol(RICH18_TAMft2)
RICH18_TAMft3 <- RICH18_TAMft2[c(2:numRows) , c(2:numCols)]
RICH18_TAMTable <- graph.adjacency(RICH18_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(RICH18_TAMTable, vertex.label = V(RICH18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
RICH18_TAM.clusterCoef <- transitivity(RICH18_TAMTable, type="global") #cluster coefficient
RICH18_TAM.degreeCent <- centralization.degree(RICH18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_TAMftn <- as.network.matrix(RICH18_TAMft)
RICH18_TAM.netDensity <- network.density(RICH18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_TAM.entropy <- entropy(RICH18_TAMft) #entropy

RICH18_TAM.netMx <- cbind(RICH18_TAM.netMx, RICH18_TAM.clusterCoef, RICH18_TAM.degreeCent$centralization,
                          RICH18_TAM.netDensity, RICH18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
RICH18_SDMg2 <- data.frame(RICH18_SDM)
RICH18_SDMg2 <- RICH18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_SDMg2$player1
player2vector <- RICH18_SDMg2$player2
RICH18_SDMg3 <- RICH18_SDMg2
RICH18_SDMg3$p1inp2vec <- is.element(RICH18_SDMg3$player1, player2vector)
RICH18_SDMg3$p2inp1vec <- is.element(RICH18_SDMg3$player2, player1vector)

addPlayer1 <- RICH18_SDMg3[ which(RICH18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_SDMg3[ which(RICH18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_SDMg2 <- rbind(RICH18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
RICH18_SDMft <- ftable(RICH18_SDMg2$player1, RICH18_SDMg2$player2)
RICH18_SDMft2 <- as.matrix(RICH18_SDMft)
numRows <- nrow(RICH18_SDMft2)
numCols <- ncol(RICH18_SDMft2)
RICH18_SDMft3 <- RICH18_SDMft2[c(2:numRows) , c(2:numCols)]
RICH18_SDMTable <- graph.adjacency(RICH18_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(RICH18_SDMTable, vertex.label = V(RICH18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
RICH18_SDM.clusterCoef <- transitivity(RICH18_SDMTable, type="global") #cluster coefficient
RICH18_SDM.degreeCent <- centralization.degree(RICH18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_SDMftn <- as.network.matrix(RICH18_SDMft)
RICH18_SDM.netDensity <- network.density(RICH18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_SDM.entropy <- entropy(RICH18_SDMft) #entropy

RICH18_SDM.netMx <- cbind(RICH18_SDM.netMx, RICH18_SDM.clusterCoef, RICH18_SDM.degreeCent$centralization,
                          RICH18_SDM.netDensity, RICH18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
RICH18_TDMg2 <- data.frame(RICH18_TDM)
RICH18_TDMg2 <- RICH18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_TDMg2$player1
player2vector <- RICH18_TDMg2$player2
RICH18_TDMg3 <- RICH18_TDMg2
RICH18_TDMg3$p1inp2vec <- is.element(RICH18_TDMg3$player1, player2vector)
RICH18_TDMg3$p2inp1vec <- is.element(RICH18_TDMg3$player2, player1vector)

addPlayer1 <- RICH18_TDMg3[ which(RICH18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_TDMg3[ which(RICH18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_TDMg2 <- rbind(RICH18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
RICH18_TDMft <- ftable(RICH18_TDMg2$player1, RICH18_TDMg2$player2)
RICH18_TDMft2 <- as.matrix(RICH18_TDMft)
numRows <- nrow(RICH18_TDMft2)
numCols <- ncol(RICH18_TDMft2)
RICH18_TDMft3 <- RICH18_TDMft2[c(2:numRows) , c(2:numCols)]
RICH18_TDMTable <- graph.adjacency(RICH18_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(RICH18_TDMTable, vertex.label = V(RICH18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
RICH18_TDM.clusterCoef <- transitivity(RICH18_TDMTable, type="global") #cluster coefficient
RICH18_TDM.degreeCent <- centralization.degree(RICH18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_TDMftn <- as.network.matrix(RICH18_TDMft)
RICH18_TDM.netDensity <- network.density(RICH18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_TDM.entropy <- entropy(RICH18_TDMft) #entropy

RICH18_TDM.netMx <- cbind(RICH18_TDM.netMx, RICH18_TDM.clusterCoef, RICH18_TDM.degreeCent$centralization,
                          RICH18_TDM.netDensity, RICH18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
RICH18_SDg2 <- data.frame(RICH18_SD)
RICH18_SDg2 <- RICH18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_SDg2$player1
player2vector <- RICH18_SDg2$player2
RICH18_SDg3 <- RICH18_SDg2
RICH18_SDg3$p1inp2vec <- is.element(RICH18_SDg3$player1, player2vector)
RICH18_SDg3$p2inp1vec <- is.element(RICH18_SDg3$player2, player1vector)

addPlayer1 <- RICH18_SDg3[ which(RICH18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_SDg3[ which(RICH18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_SDg2 <- rbind(RICH18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
RICH18_SDft <- ftable(RICH18_SDg2$player1, RICH18_SDg2$player2)
RICH18_SDft2 <- as.matrix(RICH18_SDft)
numRows <- nrow(RICH18_SDft2)
numCols <- ncol(RICH18_SDft2)
RICH18_SDft3 <- RICH18_SDft2[c(2:numRows) , c(2:numCols)]
RICH18_SDTable <- graph.adjacency(RICH18_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(RICH18_SDTable, vertex.label = V(RICH18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
RICH18_SD.clusterCoef <- transitivity(RICH18_SDTable, type="global") #cluster coefficient
RICH18_SD.degreeCent <- centralization.degree(RICH18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_SDftn <- as.network.matrix(RICH18_SDft)
RICH18_SD.netDensity <- network.density(RICH18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_SD.entropy <- entropy(RICH18_SDft) #entropy

RICH18_SD.netMx <- cbind(RICH18_SD.netMx, RICH18_SD.clusterCoef, RICH18_SD.degreeCent$centralization,
                         RICH18_SD.netDensity, RICH18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************

round = 18
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
RICH18_TDg2 <- data.frame(RICH18_TD)
RICH18_TDg2 <- RICH18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_TDg2$player1
player2vector <- RICH18_TDg2$player2
RICH18_TDg3 <- RICH18_TDg2
RICH18_TDg3$p1inp2vec <- is.element(RICH18_TDg3$player1, player2vector)
RICH18_TDg3$p2inp1vec <- is.element(RICH18_TDg3$player2, player1vector)

addPlayer1 <- RICH18_TDg3[ which(RICH18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_TDg3[ which(RICH18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_TDg2 <- rbind(RICH18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
RICH18_TDft <- ftable(RICH18_TDg2$player1, RICH18_TDg2$player2)
RICH18_TDft2 <- as.matrix(RICH18_TDft)
numRows <- nrow(RICH18_TDft2)
numCols <- ncol(RICH18_TDft2)
RICH18_TDft3 <- RICH18_TDft2[c(2:numRows) , c(2:numCols)]
RICH18_TDTable <- graph.adjacency(RICH18_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(RICH18_TDTable, vertex.label = V(RICH18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
RICH18_TD.clusterCoef <- transitivity(RICH18_TDTable, type="global") #cluster coefficient
RICH18_TD.degreeCent <- centralization.degree(RICH18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_TDftn <- as.network.matrix(RICH18_TDft)
RICH18_TD.netDensity <- network.density(RICH18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_TD.entropy <- entropy(RICH18_TDft) #entropy

RICH18_TD.netMx <- cbind(RICH18_TD.netMx, RICH18_TD.clusterCoef, RICH18_TD.degreeCent$centralization,
                         RICH18_TD.netDensity, RICH18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************

round = 18
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
RICH18_QTg2 <- data.frame(RICH18_QT)
RICH18_QTg2 <- RICH18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH18_QTg2$player1
player2vector <- RICH18_QTg2$player2
RICH18_QTg3 <- RICH18_QTg2
RICH18_QTg3$p1inp2vec <- is.element(RICH18_QTg3$player1, player2vector)
RICH18_QTg3$p2inp1vec <- is.element(RICH18_QTg3$player2, player1vector)

addPlayer1 <- RICH18_QTg3[ which(RICH18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH18_QTg3[ which(RICH18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH18_QTg2 <- rbind(RICH18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
RICH18_QTft <- ftable(RICH18_QTg2$player1, RICH18_QTg2$player2)
RICH18_QTft2 <- as.matrix(RICH18_QTft)
numRows <- nrow(RICH18_QTft2)
numCols <- ncol(RICH18_QTft2)
RICH18_QTft3 <- RICH18_QTft2[c(2:numRows) , c(2:numCols)]
RICH18_QTTable <- graph.adjacency(RICH18_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(RICH18_QTTable, vertex.label = V(RICH18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
RICH18_QT.clusterCoef <- transitivity(RICH18_QTTable, type="global") #cluster coefficient
RICH18_QT.degreeCent <- centralization.degree(RICH18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH18_QTftn <- as.network.matrix(RICH18_QTft)
RICH18_QT.netDensity <- network.density(RICH18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH18_QT.entropy <- entropy(RICH18_QTft) #entropy

RICH18_QT.netMx <- cbind(RICH18_QT.netMx, RICH18_QT.clusterCoef, RICH18_QT.degreeCent$centralization,
                         RICH18_QT.netDensity, RICH18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH18_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "STK"
KIoutcome = "Goal_F"
STK18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
STK18_Gg2 <- data.frame(STK18_G)
STK18_Gg2 <- STK18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_Gg2$player1
player2vector <- STK18_Gg2$player2
STK18_Gg3 <- STK18_Gg2
STK18_Gg3$p1inp2vec <- is.element(STK18_Gg3$player1, player2vector)
STK18_Gg3$p2inp1vec <- is.element(STK18_Gg3$player2, player1vector)

addPlayer1 <- STK18_Gg3[ which(STK18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_Gg3[ which(STK18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_Gg2 <- rbind(STK18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
STK18_Gft <- ftable(STK18_Gg2$player1, STK18_Gg2$player2)
STK18_Gft2 <- as.matrix(STK18_Gft)
numRows <- nrow(STK18_Gft2)
numCols <- ncol(STK18_Gft2)
STK18_Gft3 <- STK18_Gft2[c(2:numRows) , c(2:numCols)]
STK18_GTable <- graph.adjacency(STK18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(STK18_GTable, vertex.label = V(STK18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
STK18_G.clusterCoef <- transitivity(STK18_GTable, type="global") #cluster coefficient
STK18_G.degreeCent <- centralization.degree(STK18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_Gftn <- as.network.matrix(STK18_Gft)
STK18_G.netDensity <- network.density(STK18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_G.entropy <- entropy(STK18_Gft) #entropy

STK18_G.netMx <- cbind(STK18_G.netMx, STK18_G.clusterCoef, STK18_G.degreeCent$centralization,
                       STK18_G.netDensity, STK18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Behind_F"
STK18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
STK18_Bg2 <- data.frame(STK18_B)
STK18_Bg2 <- STK18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_Bg2$player1
player2vector <- STK18_Bg2$player2
STK18_Bg3 <- STK18_Bg2
STK18_Bg3$p1inp2vec <- is.element(STK18_Bg3$player1, player2vector)
STK18_Bg3$p2inp1vec <- is.element(STK18_Bg3$player2, player1vector)

addPlayer1 <- STK18_Bg3[ which(STK18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_Bg3[ which(STK18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_Bg2 <- rbind(STK18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
STK18_Bft <- ftable(STK18_Bg2$player1, STK18_Bg2$player2)
STK18_Bft2 <- as.matrix(STK18_Bft)
numRows <- nrow(STK18_Bft2)
numCols <- ncol(STK18_Bft2)
STK18_Bft3 <- STK18_Bft2[c(2:numRows) , c(2:numCols)]
STK18_BTable <- graph.adjacency(STK18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(STK18_BTable, vertex.label = V(STK18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
STK18_B.clusterCoef <- transitivity(STK18_BTable, type="global") #cluster coefficient
STK18_B.degreeCent <- centralization.degree(STK18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_Bftn <- as.network.matrix(STK18_Bft)
STK18_B.netDensity <- network.density(STK18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_B.entropy <- entropy(STK18_Bft) #entropy

STK18_B.netMx <- cbind(STK18_B.netMx, STK18_B.clusterCoef, STK18_B.degreeCent$centralization,
                       STK18_B.netDensity, STK18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Stoppage_F"
STK18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
STK18_SFg2 <- data.frame(STK18_SF)
STK18_SFg2 <- STK18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_SFg2$player1
player2vector <- STK18_SFg2$player2
STK18_SFg3 <- STK18_SFg2
STK18_SFg3$p1inp2vec <- is.element(STK18_SFg3$player1, player2vector)
STK18_SFg3$p2inp1vec <- is.element(STK18_SFg3$player2, player1vector)

addPlayer1 <- STK18_SFg3[ which(STK18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_SFg3[ which(STK18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_SFg2 <- rbind(STK18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
STK18_SFft <- ftable(STK18_SFg2$player1, STK18_SFg2$player2)
STK18_SFft2 <- as.matrix(STK18_SFft)
numRows <- nrow(STK18_SFft2)
numCols <- ncol(STK18_SFft2)
STK18_SFft3 <- STK18_SFft2[c(2:numRows) , c(2:numCols)]
STK18_SFTable <- graph.adjacency(STK18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(STK18_SFTable, vertex.label = V(STK18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
STK18_SF.clusterCoef <- transitivity(STK18_SFTable, type="global") #cluster coefficient
STK18_SF.degreeCent <- centralization.degree(STK18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_SFftn <- as.network.matrix(STK18_SFft)
STK18_SF.netDensity <- network.density(STK18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_SF.entropy <- entropy(STK18_SFft) #entropy

STK18_SF.netMx <- cbind(STK18_SF.netMx, STK18_SF.clusterCoef, STK18_SF.degreeCent$centralization,
                        STK18_SF.netDensity, STK18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Turnover_F"
STK18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
STK18_TFg2 <- data.frame(STK18_TF)
STK18_TFg2 <- STK18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_TFg2$player1
player2vector <- STK18_TFg2$player2
STK18_TFg3 <- STK18_TFg2
STK18_TFg3$p1inp2vec <- is.element(STK18_TFg3$player1, player2vector)
STK18_TFg3$p2inp1vec <- is.element(STK18_TFg3$player2, player1vector)

addPlayer1 <- STK18_TFg3[ which(STK18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_TFg3[ which(STK18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_TFg2 <- rbind(STK18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
STK18_TFft <- ftable(STK18_TFg2$player1, STK18_TFg2$player2)
STK18_TFft2 <- as.matrix(STK18_TFft)
numRows <- nrow(STK18_TFft2)
numCols <- ncol(STK18_TFft2)
STK18_TFft3 <- STK18_TFft2[c(2:numRows) , c(2:numCols)]
STK18_TFTable <- graph.adjacency(STK18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(STK18_TFTable, vertex.label = V(STK18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
STK18_TF.clusterCoef <- transitivity(STK18_TFTable, type="global") #cluster coefficient
STK18_TF.degreeCent <- centralization.degree(STK18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_TFftn <- as.network.matrix(STK18_TFft)
STK18_TF.netDensity <- network.density(STK18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_TF.entropy <- entropy(STK18_TFft) #entropy

STK18_TF.netMx <- cbind(STK18_TF.netMx, STK18_TF.clusterCoef, STK18_TF.degreeCent$centralization,
                        STK18_TF.netDensity, STK18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
STK18_SAMg2 <- data.frame(STK18_SAM)
STK18_SAMg2 <- STK18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_SAMg2$player1
player2vector <- STK18_SAMg2$player2
STK18_SAMg3 <- STK18_SAMg2
STK18_SAMg3$p1inp2vec <- is.element(STK18_SAMg3$player1, player2vector)
STK18_SAMg3$p2inp1vec <- is.element(STK18_SAMg3$player2, player1vector)

addPlayer1 <- STK18_SAMg3[ which(STK18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_SAMg3[ which(STK18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_SAMg2 <- rbind(STK18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
STK18_SAMft <- ftable(STK18_SAMg2$player1, STK18_SAMg2$player2)
STK18_SAMft2 <- as.matrix(STK18_SAMft)
numRows <- nrow(STK18_SAMft2)
numCols <- ncol(STK18_SAMft2)
STK18_SAMft3 <- STK18_SAMft2[c(2:numRows) , c(2:numCols)]
STK18_SAMTable <- graph.adjacency(STK18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(STK18_SAMTable, vertex.label = V(STK18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
STK18_SAM.clusterCoef <- transitivity(STK18_SAMTable, type="global") #cluster coefficient
STK18_SAM.degreeCent <- centralization.degree(STK18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_SAMftn <- as.network.matrix(STK18_SAMft)
STK18_SAM.netDensity <- network.density(STK18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_SAM.entropy <- entropy(STK18_SAMft) #entropy

STK18_SAM.netMx <- cbind(STK18_SAM.netMx, STK18_SAM.clusterCoef, STK18_SAM.degreeCent$centralization,
                         STK18_SAM.netDensity, STK18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Turnover_AM"
STK18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
STK18_TAMg2 <- data.frame(STK18_TAM)
STK18_TAMg2 <- STK18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_TAMg2$player1
player2vector <- STK18_TAMg2$player2
STK18_TAMg3 <- STK18_TAMg2
STK18_TAMg3$p1inp2vec <- is.element(STK18_TAMg3$player1, player2vector)
STK18_TAMg3$p2inp1vec <- is.element(STK18_TAMg3$player2, player1vector)

addPlayer1 <- STK18_TAMg3[ which(STK18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_TAMg3[ which(STK18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_TAMg2 <- rbind(STK18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
STK18_TAMft <- ftable(STK18_TAMg2$player1, STK18_TAMg2$player2)
STK18_TAMft2 <- as.matrix(STK18_TAMft)
numRows <- nrow(STK18_TAMft2)
numCols <- ncol(STK18_TAMft2)
STK18_TAMft3 <- STK18_TAMft2[c(1:numRows) , c(1:numCols)]
STK18_TAMTable <- graph.adjacency(STK18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(STK18_TAMTable, vertex.label = V(STK18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
STK18_TAM.clusterCoef <- transitivity(STK18_TAMTable, type="global") #cluster coefficient
STK18_TAM.degreeCent <- centralization.degree(STK18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_TAMftn <- as.network.matrix(STK18_TAMft)
STK18_TAM.netDensity <- network.density(STK18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_TAM.entropy <- entropy(STK18_TAMft) #entropy

STK18_TAM.netMx <- cbind(STK18_TAM.netMx, STK18_TAM.clusterCoef, STK18_TAM.degreeCent$centralization,
                         STK18_TAM.netDensity, STK18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
STK18_SDMg2 <- data.frame(STK18_SDM)
STK18_SDMg2 <- STK18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_SDMg2$player1
player2vector <- STK18_SDMg2$player2
STK18_SDMg3 <- STK18_SDMg2
STK18_SDMg3$p1inp2vec <- is.element(STK18_SDMg3$player1, player2vector)
STK18_SDMg3$p2inp1vec <- is.element(STK18_SDMg3$player2, player1vector)

addPlayer1 <- STK18_SDMg3[ which(STK18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_SDMg3[ which(STK18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_SDMg2 <- rbind(STK18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
STK18_SDMft <- ftable(STK18_SDMg2$player1, STK18_SDMg2$player2)
STK18_SDMft2 <- as.matrix(STK18_SDMft)
numRows <- nrow(STK18_SDMft2)
numCols <- ncol(STK18_SDMft2)
STK18_SDMft3 <- STK18_SDMft2[c(2:numRows) , c(2:numCols)]
STK18_SDMTable <- graph.adjacency(STK18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(STK18_SDMTable, vertex.label = V(STK18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
STK18_SDM.clusterCoef <- transitivity(STK18_SDMTable, type="global") #cluster coefficient
STK18_SDM.degreeCent <- centralization.degree(STK18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_SDMftn <- as.network.matrix(STK18_SDMft)
STK18_SDM.netDensity <- network.density(STK18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_SDM.entropy <- entropy(STK18_SDMft) #entropy

STK18_SDM.netMx <- cbind(STK18_SDM.netMx, STK18_SDM.clusterCoef, STK18_SDM.degreeCent$centralization,
                         STK18_SDM.netDensity, STK18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "STK"
KIoutcome = "Turnover_DM"
STK18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
STK18_TDMg2 <- data.frame(STK18_TDM)
STK18_TDMg2 <- STK18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_TDMg2$player1
player2vector <- STK18_TDMg2$player2
STK18_TDMg3 <- STK18_TDMg2
STK18_TDMg3$p1inp2vec <- is.element(STK18_TDMg3$player1, player2vector)
STK18_TDMg3$p2inp1vec <- is.element(STK18_TDMg3$player2, player1vector)

addPlayer1 <- STK18_TDMg3[ which(STK18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_TDMg3[ which(STK18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_TDMg2 <- rbind(STK18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
STK18_TDMft <- ftable(STK18_TDMg2$player1, STK18_TDMg2$player2)
STK18_TDMft2 <- as.matrix(STK18_TDMft)
numRows <- nrow(STK18_TDMft2)
numCols <- ncol(STK18_TDMft2)
STK18_TDMft3 <- STK18_TDMft2[c(2:numRows) , c(2:numCols)]
STK18_TDMTable <- graph.adjacency(STK18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(STK18_TDMTable, vertex.label = V(STK18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
STK18_TDM.clusterCoef <- transitivity(STK18_TDMTable, type="global") #cluster coefficient
STK18_TDM.degreeCent <- centralization.degree(STK18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_TDMftn <- as.network.matrix(STK18_TDMft)
STK18_TDM.netDensity <- network.density(STK18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_TDM.entropy <- entropy(STK18_TDMft) #entropy

STK18_TDM.netMx <- cbind(STK18_TDM.netMx, STK18_TDM.clusterCoef, STK18_TDM.degreeCent$centralization,
                         STK18_TDM.netDensity, STK18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Stoppage_D"
STK18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
STK18_SDg2 <- data.frame(STK18_SD)
STK18_SDg2 <- STK18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_SDg2$player1
player2vector <- STK18_SDg2$player2
STK18_SDg3 <- STK18_SDg2
STK18_SDg3$p1inp2vec <- is.element(STK18_SDg3$player1, player2vector)
STK18_SDg3$p2inp1vec <- is.element(STK18_SDg3$player2, player1vector)

addPlayer1 <- STK18_SDg3[ which(STK18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_SDg3[ which(STK18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_SDg2 <- rbind(STK18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
STK18_SDft <- ftable(STK18_SDg2$player1, STK18_SDg2$player2)
STK18_SDft2 <- as.matrix(STK18_SDft)
numRows <- nrow(STK18_SDft2)
numCols <- ncol(STK18_SDft2)
STK18_SDft3 <- STK18_SDft2[c(2:numRows) , c(2:numCols)]
STK18_SDTable <- graph.adjacency(STK18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(STK18_SDTable, vertex.label = V(STK18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
STK18_SD.clusterCoef <- transitivity(STK18_SDTable, type="global") #cluster coefficient
STK18_SD.degreeCent <- centralization.degree(STK18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_SDftn <- as.network.matrix(STK18_SDft)
STK18_SD.netDensity <- network.density(STK18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_SD.entropy <- entropy(STK18_SDft) #entropy

STK18_SD.netMx <- cbind(STK18_SD.netMx, STK18_SD.clusterCoef, STK18_SD.degreeCent$centralization,
                        STK18_SD.netDensity, STK18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "Turnover_D"
STK18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
STK18_TDg2 <- data.frame(STK18_TD)
STK18_TDg2 <- STK18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_TDg2$player1
player2vector <- STK18_TDg2$player2
STK18_TDg3 <- STK18_TDg2
STK18_TDg3$p1inp2vec <- is.element(STK18_TDg3$player1, player2vector)
STK18_TDg3$p2inp1vec <- is.element(STK18_TDg3$player2, player1vector)

addPlayer1 <- STK18_TDg3[ which(STK18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_TDg3[ which(STK18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_TDg2 <- rbind(STK18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
STK18_TDft <- ftable(STK18_TDg2$player1, STK18_TDg2$player2)
STK18_TDft2 <- as.matrix(STK18_TDft)
numRows <- nrow(STK18_TDft2)
numCols <- ncol(STK18_TDft2)
STK18_TDft3 <- STK18_TDft2[c(2:numRows) , c(2:numCols)]
STK18_TDTable <- graph.adjacency(STK18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(STK18_TDTable, vertex.label = V(STK18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
STK18_TD.clusterCoef <- transitivity(STK18_TDTable, type="global") #cluster coefficient
STK18_TD.degreeCent <- centralization.degree(STK18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_TDftn <- as.network.matrix(STK18_TDft)
STK18_TD.netDensity <- network.density(STK18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_TD.entropy <- entropy(STK18_TDft) #entropy

STK18_TD.netMx <- cbind(STK18_TD.netMx, STK18_TD.clusterCoef, STK18_TD.degreeCent$centralization,
                        STK18_TD.netDensity, STK18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
STK18_QTg2 <- data.frame(STK18_QT)
STK18_QTg2 <- STK18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK18_QTg2$player1
player2vector <- STK18_QTg2$player2
STK18_QTg3 <- STK18_QTg2
STK18_QTg3$p1inp2vec <- is.element(STK18_QTg3$player1, player2vector)
STK18_QTg3$p2inp1vec <- is.element(STK18_QTg3$player2, player1vector)

addPlayer1 <- STK18_QTg3[ which(STK18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK18_QTg3[ which(STK18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK18_QTg2 <- rbind(STK18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
STK18_QTft <- ftable(STK18_QTg2$player1, STK18_QTg2$player2)
STK18_QTft2 <- as.matrix(STK18_QTft)
numRows <- nrow(STK18_QTft2)
numCols <- ncol(STK18_QTft2)
STK18_QTft3 <- STK18_QTft2[c(2:numRows) , c(2:numCols)]
STK18_QTTable <- graph.adjacency(STK18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(STK18_QTTable, vertex.label = V(STK18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
STK18_QT.clusterCoef <- transitivity(STK18_QTTable, type="global") #cluster coefficient
STK18_QT.degreeCent <- centralization.degree(STK18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK18_QTftn <- as.network.matrix(STK18_QTft)
STK18_QT.netDensity <- network.density(STK18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK18_QT.entropy <- entropy(STK18_QTft) #entropy

STK18_QT.netMx <- cbind(STK18_QT.netMx, STK18_QT.clusterCoef, STK18_QT.degreeCent$centralization,
                        STK18_QT.netDensity, STK18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK18_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************

round = 18
teamName = "SYD"
KIoutcome = "Goal_F"
SYD18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
SYD18_Gg2 <- data.frame(SYD18_G)
SYD18_Gg2 <- SYD18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_Gg2$player1
player2vector <- SYD18_Gg2$player2
SYD18_Gg3 <- SYD18_Gg2
SYD18_Gg3$p1inp2vec <- is.element(SYD18_Gg3$player1, player2vector)
SYD18_Gg3$p2inp1vec <- is.element(SYD18_Gg3$player2, player1vector)

addPlayer1 <- SYD18_Gg3[ which(SYD18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_Gg3[ which(SYD18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_Gg2 <- rbind(SYD18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
SYD18_Gft <- ftable(SYD18_Gg2$player1, SYD18_Gg2$player2)
SYD18_Gft2 <- as.matrix(SYD18_Gft)
numRows <- nrow(SYD18_Gft2)
numCols <- ncol(SYD18_Gft2)
SYD18_Gft3 <- SYD18_Gft2[c(2:numRows) , c(2:numCols)]
SYD18_GTable <- graph.adjacency(SYD18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(SYD18_GTable, vertex.label = V(SYD18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
SYD18_G.clusterCoef <- transitivity(SYD18_GTable, type="global") #cluster coefficient
SYD18_G.degreeCent <- centralization.degree(SYD18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_Gftn <- as.network.matrix(SYD18_Gft)
SYD18_G.netDensity <- network.density(SYD18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_G.entropy <- entropy(SYD18_Gft) #entropy

SYD18_G.netMx <- cbind(SYD18_G.netMx, SYD18_G.clusterCoef, SYD18_G.degreeCent$centralization,
                       SYD18_G.netDensity, SYD18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "SYD"
KIoutcome = "Behind_F"
SYD18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
SYD18_Bg2 <- data.frame(SYD18_B)
SYD18_Bg2 <- SYD18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_Bg2$player1
player2vector <- SYD18_Bg2$player2
SYD18_Bg3 <- SYD18_Bg2
SYD18_Bg3$p1inp2vec <- is.element(SYD18_Bg3$player1, player2vector)
SYD18_Bg3$p2inp1vec <- is.element(SYD18_Bg3$player2, player1vector)

addPlayer1 <- SYD18_Bg3[ which(SYD18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_Bg3[ which(SYD18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_Bg2 <- rbind(SYD18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
SYD18_Bft <- ftable(SYD18_Bg2$player1, SYD18_Bg2$player2)
SYD18_Bft2 <- as.matrix(SYD18_Bft)
numRows <- nrow(SYD18_Bft2)
numCols <- ncol(SYD18_Bft2)
SYD18_Bft3 <- SYD18_Bft2[c(2:numRows) , c(2:numCols)]
SYD18_BTable <- graph.adjacency(SYD18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(SYD18_BTable, vertex.label = V(SYD18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
SYD18_B.clusterCoef <- transitivity(SYD18_BTable, type="global") #cluster coefficient
SYD18_B.degreeCent <- centralization.degree(SYD18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_Bftn <- as.network.matrix(SYD18_Bft)
SYD18_B.netDensity <- network.density(SYD18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_B.entropy <- entropy(SYD18_Bft) #entropy

SYD18_B.netMx <- cbind(SYD18_B.netMx, SYD18_B.clusterCoef, SYD18_B.degreeCent$centralization,
                       SYD18_B.netDensity, SYD18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
SYD18_SFg2 <- data.frame(SYD18_SF)
SYD18_SFg2 <- SYD18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_SFg2$player1
player2vector <- SYD18_SFg2$player2
SYD18_SFg3 <- SYD18_SFg2
SYD18_SFg3$p1inp2vec <- is.element(SYD18_SFg3$player1, player2vector)
SYD18_SFg3$p2inp1vec <- is.element(SYD18_SFg3$player2, player1vector)

addPlayer1 <- SYD18_SFg3[ which(SYD18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_SFg3[ which(SYD18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_SFg2 <- rbind(SYD18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
SYD18_SFft <- ftable(SYD18_SFg2$player1, SYD18_SFg2$player2)
SYD18_SFft2 <- as.matrix(SYD18_SFft)
numRows <- nrow(SYD18_SFft2)
numCols <- ncol(SYD18_SFft2)
SYD18_SFft3 <- SYD18_SFft2[c(2:numRows) , c(2:numCols)]
SYD18_SFTable <- graph.adjacency(SYD18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(SYD18_SFTable, vertex.label = V(SYD18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
SYD18_SF.clusterCoef <- transitivity(SYD18_SFTable, type="global") #cluster coefficient
SYD18_SF.degreeCent <- centralization.degree(SYD18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_SFftn <- as.network.matrix(SYD18_SFft)
SYD18_SF.netDensity <- network.density(SYD18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_SF.entropy <- entropy(SYD18_SFft) #entropy

SYD18_SF.netMx <- cbind(SYD18_SF.netMx, SYD18_SF.clusterCoef, SYD18_SF.degreeCent$centralization,
                        SYD18_SF.netDensity, SYD18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
SYD18_TFg2 <- data.frame(SYD18_TF)
SYD18_TFg2 <- SYD18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_TFg2$player1
player2vector <- SYD18_TFg2$player2
SYD18_TFg3 <- SYD18_TFg2
SYD18_TFg3$p1inp2vec <- is.element(SYD18_TFg3$player1, player2vector)
SYD18_TFg3$p2inp1vec <- is.element(SYD18_TFg3$player2, player1vector)

addPlayer1 <- SYD18_TFg3[ which(SYD18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_TFg3[ which(SYD18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_TFg2 <- rbind(SYD18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
SYD18_TFft <- ftable(SYD18_TFg2$player1, SYD18_TFg2$player2)
SYD18_TFft2 <- as.matrix(SYD18_TFft)
numRows <- nrow(SYD18_TFft2)
numCols <- ncol(SYD18_TFft2)
SYD18_TFft3 <- SYD18_TFft2[c(2:numRows) , c(2:numCols)]
SYD18_TFTable <- graph.adjacency(SYD18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(SYD18_TFTable, vertex.label = V(SYD18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
SYD18_TF.clusterCoef <- transitivity(SYD18_TFTable, type="global") #cluster coefficient
SYD18_TF.degreeCent <- centralization.degree(SYD18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_TFftn <- as.network.matrix(SYD18_TFft)
SYD18_TF.netDensity <- network.density(SYD18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_TF.entropy <- entropy(SYD18_TFft) #entropy

SYD18_TF.netMx <- cbind(SYD18_TF.netMx, SYD18_TF.clusterCoef, SYD18_TF.degreeCent$centralization,
                        SYD18_TF.netDensity, SYD18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
SYD18_SAMg2 <- data.frame(SYD18_SAM)
SYD18_SAMg2 <- SYD18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_SAMg2$player1
player2vector <- SYD18_SAMg2$player2
SYD18_SAMg3 <- SYD18_SAMg2
SYD18_SAMg3$p1inp2vec <- is.element(SYD18_SAMg3$player1, player2vector)
SYD18_SAMg3$p2inp1vec <- is.element(SYD18_SAMg3$player2, player1vector)

addPlayer1 <- SYD18_SAMg3[ which(SYD18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_SAMg3[ which(SYD18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_SAMg2 <- rbind(SYD18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
SYD18_SAMft <- ftable(SYD18_SAMg2$player1, SYD18_SAMg2$player2)
SYD18_SAMft2 <- as.matrix(SYD18_SAMft)
numRows <- nrow(SYD18_SAMft2)
numCols <- ncol(SYD18_SAMft2)
SYD18_SAMft3 <- SYD18_SAMft2[c(2:numRows) , c(2:numCols)]
SYD18_SAMTable <- graph.adjacency(SYD18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(SYD18_SAMTable, vertex.label = V(SYD18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
SYD18_SAM.clusterCoef <- transitivity(SYD18_SAMTable, type="global") #cluster coefficient
SYD18_SAM.degreeCent <- centralization.degree(SYD18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_SAMftn <- as.network.matrix(SYD18_SAMft)
SYD18_SAM.netDensity <- network.density(SYD18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_SAM.entropy <- entropy(SYD18_SAMft) #entropy

SYD18_SAM.netMx <- cbind(SYD18_SAM.netMx, SYD18_SAM.clusterCoef, SYD18_SAM.degreeCent$centralization,
                         SYD18_SAM.netDensity, SYD18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
SYD18_TAMg2 <- data.frame(SYD18_TAM)
SYD18_TAMg2 <- SYD18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_TAMg2$player1
player2vector <- SYD18_TAMg2$player2
SYD18_TAMg3 <- SYD18_TAMg2
SYD18_TAMg3$p1inp2vec <- is.element(SYD18_TAMg3$player1, player2vector)
SYD18_TAMg3$p2inp1vec <- is.element(SYD18_TAMg3$player2, player1vector)

addPlayer1 <- SYD18_TAMg3[ which(SYD18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_TAMg3[ which(SYD18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_TAMg2 <- rbind(SYD18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
SYD18_TAMft <- ftable(SYD18_TAMg2$player1, SYD18_TAMg2$player2)
SYD18_TAMft2 <- as.matrix(SYD18_TAMft)
numRows <- nrow(SYD18_TAMft2)
numCols <- ncol(SYD18_TAMft2)
SYD18_TAMft3 <- SYD18_TAMft2[c(2:numRows) , c(2:numCols)]
SYD18_TAMTable <- graph.adjacency(SYD18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(SYD18_TAMTable, vertex.label = V(SYD18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
SYD18_TAM.clusterCoef <- transitivity(SYD18_TAMTable, type="global") #cluster coefficient
SYD18_TAM.degreeCent <- centralization.degree(SYD18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_TAMftn <- as.network.matrix(SYD18_TAMft)
SYD18_TAM.netDensity <- network.density(SYD18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_TAM.entropy <- entropy(SYD18_TAMft) #entropy

SYD18_TAM.netMx <- cbind(SYD18_TAM.netMx, SYD18_TAM.clusterCoef, SYD18_TAM.degreeCent$centralization,
                         SYD18_TAM.netDensity, SYD18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
SYD18_SDMg2 <- data.frame(SYD18_SDM)
SYD18_SDMg2 <- SYD18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_SDMg2$player1
player2vector <- SYD18_SDMg2$player2
SYD18_SDMg3 <- SYD18_SDMg2
SYD18_SDMg3$p1inp2vec <- is.element(SYD18_SDMg3$player1, player2vector)
SYD18_SDMg3$p2inp1vec <- is.element(SYD18_SDMg3$player2, player1vector)

addPlayer1 <- SYD18_SDMg3[ which(SYD18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_SDMg3[ which(SYD18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_SDMg2 <- rbind(SYD18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
SYD18_SDMft <- ftable(SYD18_SDMg2$player1, SYD18_SDMg2$player2)
SYD18_SDMft2 <- as.matrix(SYD18_SDMft)
numRows <- nrow(SYD18_SDMft2)
numCols <- ncol(SYD18_SDMft2)
SYD18_SDMft3 <- SYD18_SDMft2[c(2:numRows) , c(2:numCols)]
SYD18_SDMTable <- graph.adjacency(SYD18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(SYD18_SDMTable, vertex.label = V(SYD18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
SYD18_SDM.clusterCoef <- transitivity(SYD18_SDMTable, type="global") #cluster coefficient
SYD18_SDM.degreeCent <- centralization.degree(SYD18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_SDMftn <- as.network.matrix(SYD18_SDMft)
SYD18_SDM.netDensity <- network.density(SYD18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_SDM.entropy <- entropy(SYD18_SDMft) #entropy

SYD18_SDM.netMx <- cbind(SYD18_SDM.netMx, SYD18_SDM.clusterCoef, SYD18_SDM.degreeCent$centralization,
                         SYD18_SDM.netDensity, SYD18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
SYD18_TDMg2 <- data.frame(SYD18_TDM)
SYD18_TDMg2 <- SYD18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_TDMg2$player1
player2vector <- SYD18_TDMg2$player2
SYD18_TDMg3 <- SYD18_TDMg2
SYD18_TDMg3$p1inp2vec <- is.element(SYD18_TDMg3$player1, player2vector)
SYD18_TDMg3$p2inp1vec <- is.element(SYD18_TDMg3$player2, player1vector)

addPlayer1 <- SYD18_TDMg3[ which(SYD18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_TDMg3[ which(SYD18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_TDMg2 <- rbind(SYD18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
SYD18_TDMft <- ftable(SYD18_TDMg2$player1, SYD18_TDMg2$player2)
SYD18_TDMft2 <- as.matrix(SYD18_TDMft)
numRows <- nrow(SYD18_TDMft2)
numCols <- ncol(SYD18_TDMft2)
SYD18_TDMft3 <- SYD18_TDMft2[c(2:numRows) , c(2:numCols)]
SYD18_TDMTable <- graph.adjacency(SYD18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(SYD18_TDMTable, vertex.label = V(SYD18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
SYD18_TDM.clusterCoef <- transitivity(SYD18_TDMTable, type="global") #cluster coefficient
SYD18_TDM.degreeCent <- centralization.degree(SYD18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_TDMftn <- as.network.matrix(SYD18_TDMft)
SYD18_TDM.netDensity <- network.density(SYD18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_TDM.entropy <- entropy(SYD18_TDMft) #entropy

SYD18_TDM.netMx <- cbind(SYD18_TDM.netMx, SYD18_TDM.clusterCoef, SYD18_TDM.degreeCent$centralization,
                         SYD18_TDM.netDensity, SYD18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
SYD18_SDg2 <- data.frame(SYD18_SD)
SYD18_SDg2 <- SYD18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_SDg2$player1
player2vector <- SYD18_SDg2$player2
SYD18_SDg3 <- SYD18_SDg2
SYD18_SDg3$p1inp2vec <- is.element(SYD18_SDg3$player1, player2vector)
SYD18_SDg3$p2inp1vec <- is.element(SYD18_SDg3$player2, player1vector)

addPlayer1 <- SYD18_SDg3[ which(SYD18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_SDg3[ which(SYD18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_SDg2 <- rbind(SYD18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
SYD18_SDft <- ftable(SYD18_SDg2$player1, SYD18_SDg2$player2)
SYD18_SDft2 <- as.matrix(SYD18_SDft)
numRows <- nrow(SYD18_SDft2)
numCols <- ncol(SYD18_SDft2)
SYD18_SDft3 <- SYD18_SDft2[c(2:numRows) , c(2:numCols)]
SYD18_SDTable <- graph.adjacency(SYD18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(SYD18_SDTable, vertex.label = V(SYD18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
SYD18_SD.clusterCoef <- transitivity(SYD18_SDTable, type="global") #cluster coefficient
SYD18_SD.degreeCent <- centralization.degree(SYD18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_SDftn <- as.network.matrix(SYD18_SDft)
SYD18_SD.netDensity <- network.density(SYD18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_SD.entropy <- entropy(SYD18_SDft) #entropy

SYD18_SD.netMx <- cbind(SYD18_SD.netMx, SYD18_SD.clusterCoef, SYD18_SD.degreeCent$centralization,
                        SYD18_SD.netDensity, SYD18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
SYD18_TDg2 <- data.frame(SYD18_TD)
SYD18_TDg2 <- SYD18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_TDg2$player1
player2vector <- SYD18_TDg2$player2
SYD18_TDg3 <- SYD18_TDg2
SYD18_TDg3$p1inp2vec <- is.element(SYD18_TDg3$player1, player2vector)
SYD18_TDg3$p2inp1vec <- is.element(SYD18_TDg3$player2, player1vector)

addPlayer1 <- SYD18_TDg3[ which(SYD18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_TDg3[ which(SYD18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_TDg2 <- rbind(SYD18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
SYD18_TDft <- ftable(SYD18_TDg2$player1, SYD18_TDg2$player2)
SYD18_TDft2 <- as.matrix(SYD18_TDft)
numRows <- nrow(SYD18_TDft2)
numCols <- ncol(SYD18_TDft2)
SYD18_TDft3 <- SYD18_TDft2[c(2:numRows) , c(2:numCols)]
SYD18_TDTable <- graph.adjacency(SYD18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(SYD18_TDTable, vertex.label = V(SYD18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
SYD18_TD.clusterCoef <- transitivity(SYD18_TDTable, type="global") #cluster coefficient
SYD18_TD.degreeCent <- centralization.degree(SYD18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_TDftn <- as.network.matrix(SYD18_TDft)
SYD18_TD.netDensity <- network.density(SYD18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_TD.entropy <- entropy(SYD18_TDft) #entropy

SYD18_TD.netMx <- cbind(SYD18_TD.netMx, SYD18_TD.clusterCoef, SYD18_TD.degreeCent$centralization,
                        SYD18_TD.netDensity, SYD18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
SYD18_QTg2 <- data.frame(SYD18_QT)
SYD18_QTg2 <- SYD18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD18_QTg2$player1
player2vector <- SYD18_QTg2$player2
SYD18_QTg3 <- SYD18_QTg2
SYD18_QTg3$p1inp2vec <- is.element(SYD18_QTg3$player1, player2vector)
SYD18_QTg3$p2inp1vec <- is.element(SYD18_QTg3$player2, player1vector)

addPlayer1 <- SYD18_QTg3[ which(SYD18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD18_QTg3[ which(SYD18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD18_QTg2 <- rbind(SYD18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
SYD18_QTft <- ftable(SYD18_QTg2$player1, SYD18_QTg2$player2)
SYD18_QTft2 <- as.matrix(SYD18_QTft)
numRows <- nrow(SYD18_QTft2)
numCols <- ncol(SYD18_QTft2)
SYD18_QTft3 <- SYD18_QTft2[c(2:numRows) , c(2:numCols)]
SYD18_QTTable <- graph.adjacency(SYD18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(SYD18_QTTable, vertex.label = V(SYD18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
SYD18_QT.clusterCoef <- transitivity(SYD18_QTTable, type="global") #cluster coefficient
SYD18_QT.degreeCent <- centralization.degree(SYD18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD18_QTftn <- as.network.matrix(SYD18_QTft)
SYD18_QT.netDensity <- network.density(SYD18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD18_QT.entropy <- entropy(SYD18_QTft) #entropy

SYD18_QT.netMx <- cbind(SYD18_QT.netMx, SYD18_QT.clusterCoef, SYD18_QT.degreeCent$centralization,
                        SYD18_QT.netDensity, SYD18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD18_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "WB"
KIoutcome = "Goal_F"
WB18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
WB18_Gg2 <- data.frame(WB18_G)
WB18_Gg2 <- WB18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_Gg2$player1
player2vector <- WB18_Gg2$player2
WB18_Gg3 <- WB18_Gg2
WB18_Gg3$p1inp2vec <- is.element(WB18_Gg3$player1, player2vector)
WB18_Gg3$p2inp1vec <- is.element(WB18_Gg3$player2, player1vector)

addPlayer1 <- WB18_Gg3[ which(WB18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_Gg3[ which(WB18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_Gg2 <- rbind(WB18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
WB18_Gft <- ftable(WB18_Gg2$player1, WB18_Gg2$player2)
WB18_Gft2 <- as.matrix(WB18_Gft)
numRows <- nrow(WB18_Gft2)
numCols <- ncol(WB18_Gft2)
WB18_Gft3 <- WB18_Gft2[c(2:numRows) , c(2:numCols)]
WB18_GTable <- graph.adjacency(WB18_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(WB18_GTable, vertex.label = V(WB18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
WB18_G.clusterCoef <- transitivity(WB18_GTable, type="global") #cluster coefficient
WB18_G.degreeCent <- centralization.degree(WB18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_Gftn <- as.network.matrix(WB18_Gft)
WB18_G.netDensity <- network.density(WB18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_G.entropy <- entropy(WB18_Gft) #entropy

WB18_G.netMx <- cbind(WB18_G.netMx, WB18_G.clusterCoef, WB18_G.degreeCent$centralization,
                      WB18_G.netDensity, WB18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "WB"
KIoutcome = "Behind_F"
WB18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
WB18_Bg2 <- data.frame(WB18_B)
WB18_Bg2 <- WB18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_Bg2$player1
player2vector <- WB18_Bg2$player2
WB18_Bg3 <- WB18_Bg2
WB18_Bg3$p1inp2vec <- is.element(WB18_Bg3$player1, player2vector)
WB18_Bg3$p2inp1vec <- is.element(WB18_Bg3$player2, player1vector)

addPlayer1 <- WB18_Bg3[ which(WB18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_Bg3[ which(WB18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_Bg2 <- rbind(WB18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
WB18_Bft <- ftable(WB18_Bg2$player1, WB18_Bg2$player2)
WB18_Bft2 <- as.matrix(WB18_Bft)
numRows <- nrow(WB18_Bft2)
numCols <- ncol(WB18_Bft2)
WB18_Bft3 <- WB18_Bft2[c(2:numRows) , c(2:numCols)]
WB18_BTable <- graph.adjacency(WB18_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(WB18_BTable, vertex.label = V(WB18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
WB18_B.clusterCoef <- transitivity(WB18_BTable, type="global") #cluster coefficient
WB18_B.degreeCent <- centralization.degree(WB18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_Bftn <- as.network.matrix(WB18_Bft)
WB18_B.netDensity <- network.density(WB18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_B.entropy <- entropy(WB18_Bft) #entropy

WB18_B.netMx <- cbind(WB18_B.netMx, WB18_B.clusterCoef, WB18_B.degreeCent$centralization,
                      WB18_B.netDensity, WB18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************
#NA

round = 18
teamName = "WB"
KIoutcome = "Stoppage_F"
WB18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
WB18_SFg2 <- data.frame(WB18_SF)
WB18_SFg2 <- WB18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_SFg2$player1
player2vector <- WB18_SFg2$player2
WB18_SFg3 <- WB18_SFg2
WB18_SFg3$p1inp2vec <- is.element(WB18_SFg3$player1, player2vector)
WB18_SFg3$p2inp1vec <- is.element(WB18_SFg3$player2, player1vector)

addPlayer1 <- WB18_SFg3[ which(WB18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_SFg3[ which(WB18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_SFg2 <- rbind(WB18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
WB18_SFft <- ftable(WB18_SFg2$player1, WB18_SFg2$player2)
WB18_SFft2 <- as.matrix(WB18_SFft)
numRows <- nrow(WB18_SFft2)
numCols <- ncol(WB18_SFft2)
WB18_SFft3 <- WB18_SFft2[c(2:numRows) , c(2:numCols)]
WB18_SFTable <- graph.adjacency(WB18_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(WB18_SFTable, vertex.label = V(WB18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
WB18_SF.clusterCoef <- transitivity(WB18_SFTable, type="global") #cluster coefficient
WB18_SF.degreeCent <- centralization.degree(WB18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_SFftn <- as.network.matrix(WB18_SFft)
WB18_SF.netDensity <- network.density(WB18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_SF.entropy <- entropy(WB18_SFft) #entropy

WB18_SF.netMx <- cbind(WB18_SF.netMx, WB18_SF.clusterCoef, WB18_SF.degreeCent$centralization,
                       WB18_SF.netDensity, WB18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************

round = 18
teamName = "WB"
KIoutcome = "Turnover_F"
WB18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
WB18_TFg2 <- data.frame(WB18_TF)
WB18_TFg2 <- WB18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_TFg2$player1
player2vector <- WB18_TFg2$player2
WB18_TFg3 <- WB18_TFg2
WB18_TFg3$p1inp2vec <- is.element(WB18_TFg3$player1, player2vector)
WB18_TFg3$p2inp1vec <- is.element(WB18_TFg3$player2, player1vector)

addPlayer1 <- WB18_TFg3[ which(WB18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB18_TFg3[ which(WB18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_TFg2 <- rbind(WB18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
WB18_TFft <- ftable(WB18_TFg2$player1, WB18_TFg2$player2)
WB18_TFft2 <- as.matrix(WB18_TFft)
numRows <- nrow(WB18_TFft2)
numCols <- ncol(WB18_TFft2)
WB18_TFft3 <- WB18_TFft2[c(2:numRows) , c(2:numCols)]
WB18_TFTable <- graph.adjacency(WB18_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(WB18_TFTable, vertex.label = V(WB18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
WB18_TF.clusterCoef <- transitivity(WB18_TFTable, type="global") #cluster coefficient
WB18_TF.degreeCent <- centralization.degree(WB18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_TFftn <- as.network.matrix(WB18_TFft)
WB18_TF.netDensity <- network.density(WB18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_TF.entropy <- entropy(WB18_TFft) #entropy

WB18_TF.netMx <- cbind(WB18_TF.netMx, WB18_TF.clusterCoef, WB18_TF.degreeCent$centralization,
                       WB18_TF.netDensity, WB18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************

round = 18
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
WB18_SAMg2 <- data.frame(WB18_SAM)
WB18_SAMg2 <- WB18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_SAMg2$player1
player2vector <- WB18_SAMg2$player2
WB18_SAMg3 <- WB18_SAMg2
WB18_SAMg3$p1inp2vec <- is.element(WB18_SAMg3$player1, player2vector)
WB18_SAMg3$p2inp1vec <- is.element(WB18_SAMg3$player2, player1vector)

addPlayer1 <- WB18_SAMg3[ which(WB18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_SAMg3[ which(WB18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_SAMg2 <- rbind(WB18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
WB18_SAMft <- ftable(WB18_SAMg2$player1, WB18_SAMg2$player2)
WB18_SAMft2 <- as.matrix(WB18_SAMft)
numRows <- nrow(WB18_SAMft2)
numCols <- ncol(WB18_SAMft2)
WB18_SAMft3 <- WB18_SAMft2[c(2:numRows) , c(2:numCols)]
WB18_SAMTable <- graph.adjacency(WB18_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(WB18_SAMTable, vertex.label = V(WB18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
WB18_SAM.clusterCoef <- transitivity(WB18_SAMTable, type="global") #cluster coefficient
WB18_SAM.degreeCent <- centralization.degree(WB18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_SAMftn <- as.network.matrix(WB18_SAMft)
WB18_SAM.netDensity <- network.density(WB18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_SAM.entropy <- entropy(WB18_SAMft) #entropy

WB18_SAM.netMx <- cbind(WB18_SAM.netMx, WB18_SAM.clusterCoef, WB18_SAM.degreeCent$centralization,
                        WB18_SAM.netDensity, WB18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "WB"
KIoutcome = "Turnover_AM"
WB18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
WB18_TAMg2 <- data.frame(WB18_TAM)
WB18_TAMg2 <- WB18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_TAMg2$player1
player2vector <- WB18_TAMg2$player2
WB18_TAMg3 <- WB18_TAMg2
WB18_TAMg3$p1inp2vec <- is.element(WB18_TAMg3$player1, player2vector)
WB18_TAMg3$p2inp1vec <- is.element(WB18_TAMg3$player2, player1vector)

addPlayer1 <- WB18_TAMg3[ which(WB18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_TAMg3[ which(WB18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_TAMg2 <- rbind(WB18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
WB18_TAMft <- ftable(WB18_TAMg2$player1, WB18_TAMg2$player2)
WB18_TAMft2 <- as.matrix(WB18_TAMft)
numRows <- nrow(WB18_TAMft2)
numCols <- ncol(WB18_TAMft2)
WB18_TAMft3 <- WB18_TAMft2[c(2:numRows) , c(2:numCols)]
WB18_TAMTable <- graph.adjacency(WB18_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(WB18_TAMTable, vertex.label = V(WB18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
WB18_TAM.clusterCoef <- transitivity(WB18_TAMTable, type="global") #cluster coefficient
WB18_TAM.degreeCent <- centralization.degree(WB18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_TAMftn <- as.network.matrix(WB18_TAMft)
WB18_TAM.netDensity <- network.density(WB18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_TAM.entropy <- entropy(WB18_TAMft) #entropy

WB18_TAM.netMx <- cbind(WB18_TAM.netMx, WB18_TAM.clusterCoef, WB18_TAM.degreeCent$centralization,
                        WB18_TAM.netDensity, WB18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
WB18_SDMg2 <- data.frame(WB18_SDM)
WB18_SDMg2 <- WB18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_SDMg2$player1
player2vector <- WB18_SDMg2$player2
WB18_SDMg3 <- WB18_SDMg2
WB18_SDMg3$p1inp2vec <- is.element(WB18_SDMg3$player1, player2vector)
WB18_SDMg3$p2inp1vec <- is.element(WB18_SDMg3$player2, player1vector)

addPlayer1 <- WB18_SDMg3[ which(WB18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_SDMg3[ which(WB18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_SDMg2 <- rbind(WB18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
WB18_SDMft <- ftable(WB18_SDMg2$player1, WB18_SDMg2$player2)
WB18_SDMft2 <- as.matrix(WB18_SDMft)
numRows <- nrow(WB18_SDMft2)
numCols <- ncol(WB18_SDMft2)
WB18_SDMft3 <- WB18_SDMft2[c(2:numRows) , c(2:numCols)]
WB18_SDMTable <- graph.adjacency(WB18_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(WB18_SDMTable, vertex.label = V(WB18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
WB18_SDM.clusterCoef <- transitivity(WB18_SDMTable, type="global") #cluster coefficient
WB18_SDM.degreeCent <- centralization.degree(WB18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_SDMftn <- as.network.matrix(WB18_SDMft)
WB18_SDM.netDensity <- network.density(WB18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_SDM.entropy <- entropy(WB18_SDMft) #entropy

WB18_SDM.netMx <- cbind(WB18_SDM.netMx, WB18_SDM.clusterCoef, WB18_SDM.degreeCent$centralization,
                        WB18_SDM.netDensity, WB18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "WB"
KIoutcome = "Turnover_DM"
WB18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
WB18_TDMg2 <- data.frame(WB18_TDM)
WB18_TDMg2 <- WB18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_TDMg2$player1
player2vector <- WB18_TDMg2$player2
WB18_TDMg3 <- WB18_TDMg2
WB18_TDMg3$p1inp2vec <- is.element(WB18_TDMg3$player1, player2vector)
WB18_TDMg3$p2inp1vec <- is.element(WB18_TDMg3$player2, player1vector)

addPlayer1 <- WB18_TDMg3[ which(WB18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_TDMg3[ which(WB18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_TDMg2 <- rbind(WB18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
WB18_TDMft <- ftable(WB18_TDMg2$player1, WB18_TDMg2$player2)
WB18_TDMft2 <- as.matrix(WB18_TDMft)
numRows <- nrow(WB18_TDMft2)
numCols <- ncol(WB18_TDMft2)
WB18_TDMft3 <- WB18_TDMft2[c(2:numRows) , c(2:numCols)]
WB18_TDMTable <- graph.adjacency(WB18_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(WB18_TDMTable, vertex.label = V(WB18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
WB18_TDM.clusterCoef <- transitivity(WB18_TDMTable, type="global") #cluster coefficient
WB18_TDM.degreeCent <- centralization.degree(WB18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_TDMftn <- as.network.matrix(WB18_TDMft)
WB18_TDM.netDensity <- network.density(WB18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_TDM.entropy <- entropy(WB18_TDMft) #entropy

WB18_TDM.netMx <- cbind(WB18_TDM.netMx, WB18_TDM.clusterCoef, WB18_TDM.degreeCent$centralization,
                        WB18_TDM.netDensity, WB18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "WB"
KIoutcome = "Stoppage_D"
WB18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
WB18_SDg2 <- data.frame(WB18_SD)
WB18_SDg2 <- WB18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_SDg2$player1
player2vector <- WB18_SDg2$player2
WB18_SDg3 <- WB18_SDg2
WB18_SDg3$p1inp2vec <- is.element(WB18_SDg3$player1, player2vector)
WB18_SDg3$p2inp1vec <- is.element(WB18_SDg3$player2, player1vector)

addPlayer1 <- WB18_SDg3[ which(WB18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_SDg3[ which(WB18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_SDg2 <- rbind(WB18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
WB18_SDft <- ftable(WB18_SDg2$player1, WB18_SDg2$player2)
WB18_SDft2 <- as.matrix(WB18_SDft)
numRows <- nrow(WB18_SDft2)
numCols <- ncol(WB18_SDft2)
WB18_SDft3 <- WB18_SDft2[c(2:numRows) , c(2:numCols)]
WB18_SDTable <- graph.adjacency(WB18_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(WB18_SDTable, vertex.label = V(WB18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
WB18_SD.clusterCoef <- transitivity(WB18_SDTable, type="global") #cluster coefficient
WB18_SD.degreeCent <- centralization.degree(WB18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_SDftn <- as.network.matrix(WB18_SDft)
WB18_SD.netDensity <- network.density(WB18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_SD.entropy <- entropy(WB18_SDft) #entropy

WB18_SD.netMx <- cbind(WB18_SD.netMx, WB18_SD.clusterCoef, WB18_SD.degreeCent$centralization,
                       WB18_SD.netDensity, WB18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************
#NA

round = 18
teamName = "WB"
KIoutcome = "Turnover_D"
WB18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
WB18_TDg2 <- data.frame(WB18_TD)
WB18_TDg2 <- WB18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_TDg2$player1
player2vector <- WB18_TDg2$player2
WB18_TDg3 <- WB18_TDg2
WB18_TDg3$p1inp2vec <- is.element(WB18_TDg3$player1, player2vector)
WB18_TDg3$p2inp1vec <- is.element(WB18_TDg3$player2, player1vector)

addPlayer1 <- WB18_TDg3[ which(WB18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_TDg3[ which(WB18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_TDg2 <- rbind(WB18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
WB18_TDft <- ftable(WB18_TDg2$player1, WB18_TDg2$player2)
WB18_TDft2 <- as.matrix(WB18_TDft)
numRows <- nrow(WB18_TDft2)
numCols <- ncol(WB18_TDft2)
WB18_TDft3 <- WB18_TDft2[c(2:numRows) , c(2:numCols)]
WB18_TDTable <- graph.adjacency(WB18_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(WB18_TDTable, vertex.label = V(WB18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
WB18_TD.clusterCoef <- transitivity(WB18_TDTable, type="global") #cluster coefficient
WB18_TD.degreeCent <- centralization.degree(WB18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_TDftn <- as.network.matrix(WB18_TDft)
WB18_TD.netDensity <- network.density(WB18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_TD.entropy <- entropy(WB18_TDft) #entropy

WB18_TD.netMx <- cbind(WB18_TD.netMx, WB18_TD.clusterCoef, WB18_TD.degreeCent$centralization,
                       WB18_TD.netDensity, WB18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
WB18_QTg2 <- data.frame(WB18_QT)
WB18_QTg2 <- WB18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB18_QTg2$player1
player2vector <- WB18_QTg2$player2
WB18_QTg3 <- WB18_QTg2
WB18_QTg3$p1inp2vec <- is.element(WB18_QTg3$player1, player2vector)
WB18_QTg3$p2inp1vec <- is.element(WB18_QTg3$player2, player1vector)

addPlayer1 <- WB18_QTg3[ which(WB18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB18_QTg3[ which(WB18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB18_QTg2 <- rbind(WB18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
WB18_QTft <- ftable(WB18_QTg2$player1, WB18_QTg2$player2)
WB18_QTft2 <- as.matrix(WB18_QTft)
numRows <- nrow(WB18_QTft2)
numCols <- ncol(WB18_QTft2)
WB18_QTft3 <- WB18_QTft2[c(2:numRows) , c(2:numCols)]
WB18_QTTable <- graph.adjacency(WB18_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(WB18_QTTable, vertex.label = V(WB18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
WB18_QT.clusterCoef <- transitivity(WB18_QTTable, type="global") #cluster coefficient
WB18_QT.degreeCent <- centralization.degree(WB18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB18_QTftn <- as.network.matrix(WB18_QTft)
WB18_QT.netDensity <- network.density(WB18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB18_QT.entropy <- entropy(WB18_QTft) #entropy

WB18_QT.netMx <- cbind(WB18_QT.netMx, WB18_QT.clusterCoef, WB18_QT.degreeCent$centralization,
                       WB18_QT.netDensity, WB18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB18_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 18
##

#ROUND 18, Goal***************************************************************
#NA

round = 18
teamName = "WCE"
KIoutcome = "Goal_F"
WCE18_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Goal with weighted edges
WCE18_Gg2 <- data.frame(WCE18_G)
WCE18_Gg2 <- WCE18_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_Gg2$player1
player2vector <- WCE18_Gg2$player2
WCE18_Gg3 <- WCE18_Gg2
WCE18_Gg3$p1inp2vec <- is.element(WCE18_Gg3$player1, player2vector)
WCE18_Gg3$p2inp1vec <- is.element(WCE18_Gg3$player2, player1vector)

addPlayer1 <- WCE18_Gg3[ which(WCE18_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_Gg3[ which(WCE18_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_Gg2 <- rbind(WCE18_Gg2, addPlayers)

#ROUND 18, Goal graph using weighted edges
WCE18_Gft <- ftable(WCE18_Gg2$player1, WCE18_Gg2$player2)
WCE18_Gft2 <- as.matrix(WCE18_Gft)
numRows <- nrow(WCE18_Gft2)
numCols <- ncol(WCE18_Gft2)
WCE18_Gft3 <- WCE18_Gft2[c(2:numRows) , c(2:numCols)]
WCE18_GTable <- graph.adjacency(WCE18_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Goal graph=weighted
plot.igraph(WCE18_GTable, vertex.label = V(WCE18_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Goal calulation of network metrics
#igraph
WCE18_G.clusterCoef <- transitivity(WCE18_GTable, type="global") #cluster coefficient
WCE18_G.degreeCent <- centralization.degree(WCE18_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_Gftn <- as.network.matrix(WCE18_Gft)
WCE18_G.netDensity <- network.density(WCE18_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_G.entropy <- entropy(WCE18_Gft) #entropy

WCE18_G.netMx <- cbind(WCE18_G.netMx, WCE18_G.clusterCoef, WCE18_G.degreeCent$centralization,
                       WCE18_G.netDensity, WCE18_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_G.netMx) <- varnames

#ROUND 18, Behind***************************************************************
#NA

round = 18
teamName = "WCE"
KIoutcome = "Behind_F"
WCE18_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, Behind with weighted edges
WCE18_Bg2 <- data.frame(WCE18_B)
WCE18_Bg2 <- WCE18_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_Bg2$player1
player2vector <- WCE18_Bg2$player2
WCE18_Bg3 <- WCE18_Bg2
WCE18_Bg3$p1inp2vec <- is.element(WCE18_Bg3$player1, player2vector)
WCE18_Bg3$p2inp1vec <- is.element(WCE18_Bg3$player2, player1vector)

addPlayer1 <- WCE18_Bg3[ which(WCE18_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_Bg3[ which(WCE18_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_Bg2 <- rbind(WCE18_Bg2, addPlayers)

#ROUND 18, Behind graph using weighted edges
WCE18_Bft <- ftable(WCE18_Bg2$player1, WCE18_Bg2$player2)
WCE18_Bft2 <- as.matrix(WCE18_Bft)
numRows <- nrow(WCE18_Bft2)
numCols <- ncol(WCE18_Bft2)
WCE18_Bft3 <- WCE18_Bft2[c(2:numRows) , c(2:numCols)]
WCE18_BTable <- graph.adjacency(WCE18_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 18, Behind graph=weighted
plot.igraph(WCE18_BTable, vertex.label = V(WCE18_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, Behind calulation of network metrics
#igraph
WCE18_B.clusterCoef <- transitivity(WCE18_BTable, type="global") #cluster coefficient
WCE18_B.degreeCent <- centralization.degree(WCE18_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_Bftn <- as.network.matrix(WCE18_Bft)
WCE18_B.netDensity <- network.density(WCE18_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_B.entropy <- entropy(WCE18_Bft) #entropy

WCE18_B.netMx <- cbind(WCE18_B.netMx, WCE18_B.clusterCoef, WCE18_B.degreeCent$centralization,
                       WCE18_B.netDensity, WCE18_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_B.netMx) <- varnames

#ROUND 18, FWD Stoppage**********************************************************

round = 18
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE18_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Stoppage with weighted edges
WCE18_SFg2 <- data.frame(WCE18_SF)
WCE18_SFg2 <- WCE18_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_SFg2$player1
player2vector <- WCE18_SFg2$player2
WCE18_SFg3 <- WCE18_SFg2
WCE18_SFg3$p1inp2vec <- is.element(WCE18_SFg3$player1, player2vector)
WCE18_SFg3$p2inp1vec <- is.element(WCE18_SFg3$player2, player1vector)

addPlayer1 <- WCE18_SFg3[ which(WCE18_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_SFg3[ which(WCE18_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_SFg2 <- rbind(WCE18_SFg2, addPlayers)

#ROUND 18, FWD Stoppage graph using weighted edges
WCE18_SFft <- ftable(WCE18_SFg2$player1, WCE18_SFg2$player2)
WCE18_SFft2 <- as.matrix(WCE18_SFft)
numRows <- nrow(WCE18_SFft2)
numCols <- ncol(WCE18_SFft2)
WCE18_SFft3 <- WCE18_SFft2[c(2:numRows) , c(2:numCols)]
WCE18_SFTable <- graph.adjacency(WCE18_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Stoppage graph=weighted
plot.igraph(WCE18_SFTable, vertex.label = V(WCE18_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Stoppage calulation of network metrics
#igraph
WCE18_SF.clusterCoef <- transitivity(WCE18_SFTable, type="global") #cluster coefficient
WCE18_SF.degreeCent <- centralization.degree(WCE18_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_SFftn <- as.network.matrix(WCE18_SFft)
WCE18_SF.netDensity <- network.density(WCE18_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_SF.entropy <- entropy(WCE18_SFft) #entropy

WCE18_SF.netMx <- cbind(WCE18_SF.netMx, WCE18_SF.clusterCoef, WCE18_SF.degreeCent$centralization,
                        WCE18_SF.netDensity, WCE18_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_SF.netMx) <- varnames

#ROUND 18, FWD Turnover**********************************************************
#NA

round = 18
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE18_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, FWD Turnover with weighted edges
WCE18_TFg2 <- data.frame(WCE18_TF)
WCE18_TFg2 <- WCE18_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_TFg2$player1
player2vector <- WCE18_TFg2$player2
WCE18_TFg3 <- WCE18_TFg2
WCE18_TFg3$p1inp2vec <- is.element(WCE18_TFg3$player1, player2vector)
WCE18_TFg3$p2inp1vec <- is.element(WCE18_TFg3$player2, player1vector)

addPlayer1 <- WCE18_TFg3[ which(WCE18_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_TFg3[ which(WCE18_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_TFg2 <- rbind(WCE18_TFg2, addPlayers)

#ROUND 18, FWD Turnover graph using weighted edges
WCE18_TFft <- ftable(WCE18_TFg2$player1, WCE18_TFg2$player2)
WCE18_TFft2 <- as.matrix(WCE18_TFft)
numRows <- nrow(WCE18_TFft2)
numCols <- ncol(WCE18_TFft2)
WCE18_TFft3 <- WCE18_TFft2[c(2:numRows) , c(2:numCols)]
WCE18_TFTable <- graph.adjacency(WCE18_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, FWD Turnover graph=weighted
plot.igraph(WCE18_TFTable, vertex.label = V(WCE18_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, FWD Turnover calulation of network metrics
#igraph
WCE18_TF.clusterCoef <- transitivity(WCE18_TFTable, type="global") #cluster coefficient
WCE18_TF.degreeCent <- centralization.degree(WCE18_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_TFftn <- as.network.matrix(WCE18_TFft)
WCE18_TF.netDensity <- network.density(WCE18_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_TF.entropy <- entropy(WCE18_TFft) #entropy

WCE18_TF.netMx <- cbind(WCE18_TF.netMx, WCE18_TF.clusterCoef, WCE18_TF.degreeCent$centralization,
                        WCE18_TF.netDensity, WCE18_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_TF.netMx) <- varnames

#ROUND 18, AM Stoppage**********************************************************
#NA

round = 18
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE18_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Stoppage with weighted edges
WCE18_SAMg2 <- data.frame(WCE18_SAM)
WCE18_SAMg2 <- WCE18_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_SAMg2$player1
player2vector <- WCE18_SAMg2$player2
WCE18_SAMg3 <- WCE18_SAMg2
WCE18_SAMg3$p1inp2vec <- is.element(WCE18_SAMg3$player1, player2vector)
WCE18_SAMg3$p2inp1vec <- is.element(WCE18_SAMg3$player2, player1vector)

addPlayer1 <- WCE18_SAMg3[ which(WCE18_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_SAMg3[ which(WCE18_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_SAMg2 <- rbind(WCE18_SAMg2, addPlayers)

#ROUND 18, AM Stoppage graph using weighted edges
WCE18_SAMft <- ftable(WCE18_SAMg2$player1, WCE18_SAMg2$player2)
WCE18_SAMft2 <- as.matrix(WCE18_SAMft)
numRows <- nrow(WCE18_SAMft2)
numCols <- ncol(WCE18_SAMft2)
WCE18_SAMft3 <- WCE18_SAMft2[c(2:numRows) , c(2:numCols)]
WCE18_SAMTable <- graph.adjacency(WCE18_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Stoppage graph=weighted
plot.igraph(WCE18_SAMTable, vertex.label = V(WCE18_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Stoppage calulation of network metrics
#igraph
WCE18_SAM.clusterCoef <- transitivity(WCE18_SAMTable, type="global") #cluster coefficient
WCE18_SAM.degreeCent <- centralization.degree(WCE18_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_SAMftn <- as.network.matrix(WCE18_SAMft)
WCE18_SAM.netDensity <- network.density(WCE18_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_SAM.entropy <- entropy(WCE18_SAMft) #entropy

WCE18_SAM.netMx <- cbind(WCE18_SAM.netMx, WCE18_SAM.clusterCoef, WCE18_SAM.degreeCent$centralization,
                         WCE18_SAM.netDensity, WCE18_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_SAM.netMx) <- varnames

#ROUND 18, AM Turnover**********************************************************

round = 18
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE18_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, AM Turnover with weighted edges
WCE18_TAMg2 <- data.frame(WCE18_TAM)
WCE18_TAMg2 <- WCE18_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_TAMg2$player1
player2vector <- WCE18_TAMg2$player2
WCE18_TAMg3 <- WCE18_TAMg2
WCE18_TAMg3$p1inp2vec <- is.element(WCE18_TAMg3$player1, player2vector)
WCE18_TAMg3$p2inp1vec <- is.element(WCE18_TAMg3$player2, player1vector)

addPlayer1 <- WCE18_TAMg3[ which(WCE18_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_TAMg3[ which(WCE18_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_TAMg2 <- rbind(WCE18_TAMg2, addPlayers)

#ROUND 18, AM Turnover graph using weighted edges
WCE18_TAMft <- ftable(WCE18_TAMg2$player1, WCE18_TAMg2$player2)
WCE18_TAMft2 <- as.matrix(WCE18_TAMft)
numRows <- nrow(WCE18_TAMft2)
numCols <- ncol(WCE18_TAMft2)
WCE18_TAMft3 <- WCE18_TAMft2[c(2:numRows) , c(2:numCols)]
WCE18_TAMTable <- graph.adjacency(WCE18_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, AM Turnover graph=weighted
plot.igraph(WCE18_TAMTable, vertex.label = V(WCE18_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, AM Turnover calulation of network metrics
#igraph
WCE18_TAM.clusterCoef <- transitivity(WCE18_TAMTable, type="global") #cluster coefficient
WCE18_TAM.degreeCent <- centralization.degree(WCE18_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_TAMftn <- as.network.matrix(WCE18_TAMft)
WCE18_TAM.netDensity <- network.density(WCE18_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_TAM.entropy <- entropy(WCE18_TAMft) #entropy

WCE18_TAM.netMx <- cbind(WCE18_TAM.netMx, WCE18_TAM.clusterCoef, WCE18_TAM.degreeCent$centralization,
                         WCE18_TAM.netDensity, WCE18_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_TAM.netMx) <- varnames

#ROUND 18, DM Stoppage**********************************************************

round = 18
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE18_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Stoppage with weighted edges
WCE18_SDMg2 <- data.frame(WCE18_SDM)
WCE18_SDMg2 <- WCE18_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_SDMg2$player1
player2vector <- WCE18_SDMg2$player2
WCE18_SDMg3 <- WCE18_SDMg2
WCE18_SDMg3$p1inp2vec <- is.element(WCE18_SDMg3$player1, player2vector)
WCE18_SDMg3$p2inp1vec <- is.element(WCE18_SDMg3$player2, player1vector)

addPlayer1 <- WCE18_SDMg3[ which(WCE18_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_SDMg3[ which(WCE18_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_SDMg2 <- rbind(WCE18_SDMg2, addPlayers)

#ROUND 18, DM Stoppage graph using weighted edges
WCE18_SDMft <- ftable(WCE18_SDMg2$player1, WCE18_SDMg2$player2)
WCE18_SDMft2 <- as.matrix(WCE18_SDMft)
numRows <- nrow(WCE18_SDMft2)
numCols <- ncol(WCE18_SDMft2)
WCE18_SDMft3 <- WCE18_SDMft2[c(2:numRows) , c(2:numCols)]
WCE18_SDMTable <- graph.adjacency(WCE18_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Stoppage graph=weighted
plot.igraph(WCE18_SDMTable, vertex.label = V(WCE18_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Stoppage calulation of network metrics
#igraph
WCE18_SDM.clusterCoef <- transitivity(WCE18_SDMTable, type="global") #cluster coefficient
WCE18_SDM.degreeCent <- centralization.degree(WCE18_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_SDMftn <- as.network.matrix(WCE18_SDMft)
WCE18_SDM.netDensity <- network.density(WCE18_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_SDM.entropy <- entropy(WCE18_SDMft) #entropy

WCE18_SDM.netMx <- cbind(WCE18_SDM.netMx, WCE18_SDM.clusterCoef, WCE18_SDM.degreeCent$centralization,
                         WCE18_SDM.netDensity, WCE18_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_SDM.netMx) <- varnames

#ROUND 18, DM Turnover**********************************************************

round = 18
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE18_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, DM Turnover with weighted edges
WCE18_TDMg2 <- data.frame(WCE18_TDM)
WCE18_TDMg2 <- WCE18_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_TDMg2$player1
player2vector <- WCE18_TDMg2$player2
WCE18_TDMg3 <- WCE18_TDMg2
WCE18_TDMg3$p1inp2vec <- is.element(WCE18_TDMg3$player1, player2vector)
WCE18_TDMg3$p2inp1vec <- is.element(WCE18_TDMg3$player2, player1vector)

addPlayer1 <- WCE18_TDMg3[ which(WCE18_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE18_TDMg3[ which(WCE18_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_TDMg2 <- rbind(WCE18_TDMg2, addPlayers)

#ROUND 18, DM Turnover graph using weighted edges
WCE18_TDMft <- ftable(WCE18_TDMg2$player1, WCE18_TDMg2$player2)
WCE18_TDMft2 <- as.matrix(WCE18_TDMft)
numRows <- nrow(WCE18_TDMft2)
numCols <- ncol(WCE18_TDMft2)
WCE18_TDMft3 <- WCE18_TDMft2[c(2:numRows) , c(2:numCols)]
WCE18_TDMTable <- graph.adjacency(WCE18_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 18, DM Turnover graph=weighted
plot.igraph(WCE18_TDMTable, vertex.label = V(WCE18_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, DM Turnover calulation of network metrics
#igraph
WCE18_TDM.clusterCoef <- transitivity(WCE18_TDMTable, type="global") #cluster coefficient
WCE18_TDM.degreeCent <- centralization.degree(WCE18_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_TDMftn <- as.network.matrix(WCE18_TDMft)
WCE18_TDM.netDensity <- network.density(WCE18_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_TDM.entropy <- entropy(WCE18_TDMft) #entropy

WCE18_TDM.netMx <- cbind(WCE18_TDM.netMx, WCE18_TDM.clusterCoef, WCE18_TDM.degreeCent$centralization,
                         WCE18_TDM.netDensity, WCE18_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_TDM.netMx) <- varnames

#ROUND 18, D Stoppage**********************************************************
#NA

round = 18
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE18_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Stoppage with weighted edges
WCE18_SDg2 <- data.frame(WCE18_SD)
WCE18_SDg2 <- WCE18_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_SDg2$player1
player2vector <- WCE18_SDg2$player2
WCE18_SDg3 <- WCE18_SDg2
WCE18_SDg3$p1inp2vec <- is.element(WCE18_SDg3$player1, player2vector)
WCE18_SDg3$p2inp1vec <- is.element(WCE18_SDg3$player2, player1vector)

addPlayer1 <- WCE18_SDg3[ which(WCE18_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_SDg3[ which(WCE18_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_SDg2 <- rbind(WCE18_SDg2, addPlayers)

#ROUND 18, D Stoppage graph using weighted edges
WCE18_SDft <- ftable(WCE18_SDg2$player1, WCE18_SDg2$player2)
WCE18_SDft2 <- as.matrix(WCE18_SDft)
numRows <- nrow(WCE18_SDft2)
numCols <- ncol(WCE18_SDft2)
WCE18_SDft3 <- WCE18_SDft2[c(2:numRows) , c(2:numCols)]
WCE18_SDTable <- graph.adjacency(WCE18_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Stoppage graph=weighted
plot.igraph(WCE18_SDTable, vertex.label = V(WCE18_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Stoppage calulation of network metrics
#igraph
WCE18_SD.clusterCoef <- transitivity(WCE18_SDTable, type="global") #cluster coefficient
WCE18_SD.degreeCent <- centralization.degree(WCE18_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_SDftn <- as.network.matrix(WCE18_SDft)
WCE18_SD.netDensity <- network.density(WCE18_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_SD.entropy <- entropy(WCE18_SDft) #entropy

WCE18_SD.netMx <- cbind(WCE18_SD.netMx, WCE18_SD.clusterCoef, WCE18_SD.degreeCent$centralization,
                        WCE18_SD.netDensity, WCE18_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_SD.netMx) <- varnames

#ROUND 18, D Turnover**********************************************************

round = 18
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE18_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, D Turnover with weighted edges
WCE18_TDg2 <- data.frame(WCE18_TD)
WCE18_TDg2 <- WCE18_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_TDg2$player1
player2vector <- WCE18_TDg2$player2
WCE18_TDg3 <- WCE18_TDg2
WCE18_TDg3$p1inp2vec <- is.element(WCE18_TDg3$player1, player2vector)
WCE18_TDg3$p2inp1vec <- is.element(WCE18_TDg3$player2, player1vector)

addPlayer1 <- WCE18_TDg3[ which(WCE18_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_TDg3[ which(WCE18_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_TDg2 <- rbind(WCE18_TDg2, addPlayers)

#ROUND 18, D Turnover graph using weighted edges
WCE18_TDft <- ftable(WCE18_TDg2$player1, WCE18_TDg2$player2)
WCE18_TDft2 <- as.matrix(WCE18_TDft)
numRows <- nrow(WCE18_TDft2)
numCols <- ncol(WCE18_TDft2)
WCE18_TDft3 <- WCE18_TDft2[c(2:numRows) , c(2:numCols)]
WCE18_TDTable <- graph.adjacency(WCE18_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, D Turnover graph=weighted
plot.igraph(WCE18_TDTable, vertex.label = V(WCE18_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, D Turnover calulation of network metrics
#igraph
WCE18_TD.clusterCoef <- transitivity(WCE18_TDTable, type="global") #cluster coefficient
WCE18_TD.degreeCent <- centralization.degree(WCE18_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_TDftn <- as.network.matrix(WCE18_TDft)
WCE18_TD.netDensity <- network.density(WCE18_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_TD.entropy <- entropy(WCE18_TDft) #entropy

WCE18_TD.netMx <- cbind(WCE18_TD.netMx, WCE18_TD.clusterCoef, WCE18_TD.degreeCent$centralization,
                        WCE18_TD.netDensity, WCE18_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_TD.netMx) <- varnames

#ROUND 18, End of Qtr**********************************************************
#NA

round = 18
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE18_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 18, End of Qtr with weighted edges
WCE18_QTg2 <- data.frame(WCE18_QT)
WCE18_QTg2 <- WCE18_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE18_QTg2$player1
player2vector <- WCE18_QTg2$player2
WCE18_QTg3 <- WCE18_QTg2
WCE18_QTg3$p1inp2vec <- is.element(WCE18_QTg3$player1, player2vector)
WCE18_QTg3$p2inp1vec <- is.element(WCE18_QTg3$player2, player1vector)

addPlayer1 <- WCE18_QTg3[ which(WCE18_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE18_QTg3[ which(WCE18_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE18_QTg2 <- rbind(WCE18_QTg2, addPlayers)

#ROUND 18, End of Qtr graph using weighted edges
WCE18_QTft <- ftable(WCE18_QTg2$player1, WCE18_QTg2$player2)
WCE18_QTft2 <- as.matrix(WCE18_QTft)
numRows <- nrow(WCE18_QTft2)
numCols <- ncol(WCE18_QTft2)
WCE18_QTft3 <- WCE18_QTft2[c(2:numRows) , c(2:numCols)]
WCE18_QTTable <- graph.adjacency(WCE18_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 18, End of Qtr graph=weighted
plot.igraph(WCE18_QTTable, vertex.label = V(WCE18_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE18_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 18, End of Qtr calulation of network metrics
#igraph
WCE18_QT.clusterCoef <- transitivity(WCE18_QTTable, type="global") #cluster coefficient
WCE18_QT.degreeCent <- centralization.degree(WCE18_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE18_QTftn <- as.network.matrix(WCE18_QTft)
WCE18_QT.netDensity <- network.density(WCE18_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE18_QT.entropy <- entropy(WCE18_QTft) #entropy

WCE18_QT.netMx <- cbind(WCE18_QT.netMx, WCE18_QT.clusterCoef, WCE18_QT.degreeCent$centralization,
                        WCE18_QT.netDensity, WCE18_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE18_QT.netMx) <- varnames
