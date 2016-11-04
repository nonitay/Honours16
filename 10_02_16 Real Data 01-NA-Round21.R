#####
#10-02-16- Real data 21
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
ADEL21_Gg2 <- data.frame(ADEL21_G)
ADEL21_Gg2 <- ADEL21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_Gg2$player1
player2vector <- ADEL21_Gg2$player2
ADEL21_Gg3 <- ADEL21_Gg2
ADEL21_Gg3$p1inp2vec <- is.element(ADEL21_Gg3$player1, player2vector)
ADEL21_Gg3$p2inp1vec <- is.element(ADEL21_Gg3$player2, player1vector)

addPlayer1 <- ADEL21_Gg3[ which(ADEL21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_Gg3[ which(ADEL21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_Gg2 <- rbind(ADEL21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
ADEL21_Gft <- ftable(ADEL21_Gg2$player1, ADEL21_Gg2$player2)
ADEL21_Gft2 <- as.matrix(ADEL21_Gft)
numRows <- nrow(ADEL21_Gft2)
numCols <- ncol(ADEL21_Gft2)
ADEL21_Gft3 <- ADEL21_Gft2[c(2:numRows) , c(2:numCols)]
ADEL21_GTable <- graph.adjacency(ADEL21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(ADEL21_GTable, vertex.label = V(ADEL21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
ADEL21_G.clusterCoef <- transitivity(ADEL21_GTable, type="global") #cluster coefficient
ADEL21_G.degreeCent <- centralization.degree(ADEL21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_Gftn <- as.network.matrix(ADEL21_Gft)
ADEL21_G.netDensity <- network.density(ADEL21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_G.entropy <- entropy(ADEL21_Gft) #entropy

ADEL21_G.netMx <- cbind(ADEL21_G.netMx, ADEL21_G.clusterCoef, ADEL21_G.degreeCent$centralization,
                        ADEL21_G.netDensity, ADEL21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
ADEL21_Bg2 <- data.frame(ADEL21_B)
ADEL21_Bg2 <- ADEL21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_Bg2$player1
player2vector <- ADEL21_Bg2$player2
ADEL21_Bg3 <- ADEL21_Bg2
ADEL21_Bg3$p1inp2vec <- is.element(ADEL21_Bg3$player1, player2vector)
ADEL21_Bg3$p2inp1vec <- is.element(ADEL21_Bg3$player2, player1vector)

addPlayer1 <- ADEL21_Bg3[ which(ADEL21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_Bg3[ which(ADEL21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_Bg2 <- rbind(ADEL21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
ADEL21_Bft <- ftable(ADEL21_Bg2$player1, ADEL21_Bg2$player2)
ADEL21_Bft2 <- as.matrix(ADEL21_Bft)
numRows <- nrow(ADEL21_Bft2)
numCols <- ncol(ADEL21_Bft2)
ADEL21_Bft3 <- ADEL21_Bft2[c(2:numRows) , c(2:numCols)]
ADEL21_BTable <- graph.adjacency(ADEL21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(ADEL21_BTable, vertex.label = V(ADEL21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
ADEL21_B.clusterCoef <- transitivity(ADEL21_BTable, type="global") #cluster coefficient
ADEL21_B.degreeCent <- centralization.degree(ADEL21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_Bftn <- as.network.matrix(ADEL21_Bft)
ADEL21_B.netDensity <- network.density(ADEL21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_B.entropy <- entropy(ADEL21_Bft) #entropy

ADEL21_B.netMx <- cbind(ADEL21_B.netMx, ADEL21_B.clusterCoef, ADEL21_B.degreeCent$centralization,
                        ADEL21_B.netDensity, ADEL21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
ADEL21_SFg2 <- data.frame(ADEL21_SF)
ADEL21_SFg2 <- ADEL21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_SFg2$player1
player2vector <- ADEL21_SFg2$player2
ADEL21_SFg3 <- ADEL21_SFg2
ADEL21_SFg3$p1inp2vec <- is.element(ADEL21_SFg3$player1, player2vector)
ADEL21_SFg3$p2inp1vec <- is.element(ADEL21_SFg3$player2, player1vector)

addPlayer1 <- ADEL21_SFg3[ which(ADEL21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_SFg3[ which(ADEL21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_SFg2 <- rbind(ADEL21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
ADEL21_SFft <- ftable(ADEL21_SFg2$player1, ADEL21_SFg2$player2)
ADEL21_SFft2 <- as.matrix(ADEL21_SFft)
numRows <- nrow(ADEL21_SFft2)
numCols <- ncol(ADEL21_SFft2)
ADEL21_SFft3 <- ADEL21_SFft2[c(2:numRows) , c(2:numCols)]
ADEL21_SFTable <- graph.adjacency(ADEL21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(ADEL21_SFTable, vertex.label = V(ADEL21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
ADEL21_SF.clusterCoef <- transitivity(ADEL21_SFTable, type="global") #cluster coefficient
ADEL21_SF.degreeCent <- centralization.degree(ADEL21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_SFftn <- as.network.matrix(ADEL21_SFft)
ADEL21_SF.netDensity <- network.density(ADEL21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_SF.entropy <- entropy(ADEL21_SFft) #entropy

ADEL21_SF.netMx <- cbind(ADEL21_SF.netMx, ADEL21_SF.clusterCoef, ADEL21_SF.degreeCent$centralization,
                         ADEL21_SF.netDensity, ADEL21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
ADEL21_TFg2 <- data.frame(ADEL21_TF)
ADEL21_TFg2 <- ADEL21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_TFg2$player1
player2vector <- ADEL21_TFg2$player2
ADEL21_TFg3 <- ADEL21_TFg2
ADEL21_TFg3$p1inp2vec <- is.element(ADEL21_TFg3$player1, player2vector)
ADEL21_TFg3$p2inp1vec <- is.element(ADEL21_TFg3$player2, player1vector)

addPlayer1 <- ADEL21_TFg3[ which(ADEL21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_TFg3[ which(ADEL21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_TFg2 <- rbind(ADEL21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
ADEL21_TFft <- ftable(ADEL21_TFg2$player1, ADEL21_TFg2$player2)
ADEL21_TFft2 <- as.matrix(ADEL21_TFft)
numRows <- nrow(ADEL21_TFft2)
numCols <- ncol(ADEL21_TFft2)
ADEL21_TFft3 <- ADEL21_TFft2[c(2:numRows) , c(2:numCols)]
ADEL21_TFTable <- graph.adjacency(ADEL21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(ADEL21_TFTable, vertex.label = V(ADEL21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
ADEL21_TF.clusterCoef <- transitivity(ADEL21_TFTable, type="global") #cluster coefficient
ADEL21_TF.degreeCent <- centralization.degree(ADEL21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_TFftn <- as.network.matrix(ADEL21_TFft)
ADEL21_TF.netDensity <- network.density(ADEL21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_TF.entropy <- entropy(ADEL21_TFft) #entropy

ADEL21_TF.netMx <- cbind(ADEL21_TF.netMx, ADEL21_TF.clusterCoef, ADEL21_TF.degreeCent$centralization,
                         ADEL21_TF.netDensity, ADEL21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
ADEL21_SAMg2 <- data.frame(ADEL21_SAM)
ADEL21_SAMg2 <- ADEL21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_SAMg2$player1
player2vector <- ADEL21_SAMg2$player2
ADEL21_SAMg3 <- ADEL21_SAMg2
ADEL21_SAMg3$p1inp2vec <- is.element(ADEL21_SAMg3$player1, player2vector)
ADEL21_SAMg3$p2inp1vec <- is.element(ADEL21_SAMg3$player2, player1vector)

addPlayer1 <- ADEL21_SAMg3[ which(ADEL21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_SAMg3[ which(ADEL21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_SAMg2 <- rbind(ADEL21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
ADEL21_SAMft <- ftable(ADEL21_SAMg2$player1, ADEL21_SAMg2$player2)
ADEL21_SAMft2 <- as.matrix(ADEL21_SAMft)
numRows <- nrow(ADEL21_SAMft2)
numCols <- ncol(ADEL21_SAMft2)
ADEL21_SAMft3 <- ADEL21_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL21_SAMTable <- graph.adjacency(ADEL21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(ADEL21_SAMTable, vertex.label = V(ADEL21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
ADEL21_SAM.clusterCoef <- transitivity(ADEL21_SAMTable, type="global") #cluster coefficient
ADEL21_SAM.degreeCent <- centralization.degree(ADEL21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_SAMftn <- as.network.matrix(ADEL21_SAMft)
ADEL21_SAM.netDensity <- network.density(ADEL21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_SAM.entropy <- entropy(ADEL21_SAMft) #entropy

ADEL21_SAM.netMx <- cbind(ADEL21_SAM.netMx, ADEL21_SAM.clusterCoef, ADEL21_SAM.degreeCent$centralization,
                          ADEL21_SAM.netDensity, ADEL21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
ADEL21_TAMg2 <- data.frame(ADEL21_TAM)
ADEL21_TAMg2 <- ADEL21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_TAMg2$player1
player2vector <- ADEL21_TAMg2$player2
ADEL21_TAMg3 <- ADEL21_TAMg2
ADEL21_TAMg3$p1inp2vec <- is.element(ADEL21_TAMg3$player1, player2vector)
ADEL21_TAMg3$p2inp1vec <- is.element(ADEL21_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL21_TAMg3[ which(ADEL21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL21_TAMg2 <- rbind(ADEL21_TAMg2, addPlayer2)

#ROUND 21, AM Turnover graph using weighted edges
ADEL21_TAMft <- ftable(ADEL21_TAMg2$player1, ADEL21_TAMg2$player2)
ADEL21_TAMft2 <- as.matrix(ADEL21_TAMft)
numRows <- nrow(ADEL21_TAMft2)
numCols <- ncol(ADEL21_TAMft2)
ADEL21_TAMft3 <- ADEL21_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL21_TAMTable <- graph.adjacency(ADEL21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(ADEL21_TAMTable, vertex.label = V(ADEL21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
ADEL21_TAM.clusterCoef <- transitivity(ADEL21_TAMTable, type="global") #cluster coefficient
ADEL21_TAM.degreeCent <- centralization.degree(ADEL21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_TAMftn <- as.network.matrix(ADEL21_TAMft)
ADEL21_TAM.netDensity <- network.density(ADEL21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_TAM.entropy <- entropy(ADEL21_TAMft) #entropy

ADEL21_TAM.netMx <- cbind(ADEL21_TAM.netMx, ADEL21_TAM.clusterCoef, ADEL21_TAM.degreeCent$centralization,
                          ADEL21_TAM.netDensity, ADEL21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
ADEL21_SDMg2 <- data.frame(ADEL21_SDM)
ADEL21_SDMg2 <- ADEL21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_SDMg2$player1
player2vector <- ADEL21_SDMg2$player2
ADEL21_SDMg3 <- ADEL21_SDMg2
ADEL21_SDMg3$p1inp2vec <- is.element(ADEL21_SDMg3$player1, player2vector)
ADEL21_SDMg3$p2inp1vec <- is.element(ADEL21_SDMg3$player2, player1vector)

addPlayer1 <- ADEL21_SDMg3[ which(ADEL21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_SDMg3[ which(ADEL21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_SDMg2 <- rbind(ADEL21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
ADEL21_SDMft <- ftable(ADEL21_SDMg2$player1, ADEL21_SDMg2$player2)
ADEL21_SDMft2 <- as.matrix(ADEL21_SDMft)
numRows <- nrow(ADEL21_SDMft2)
numCols <- ncol(ADEL21_SDMft2)
ADEL21_SDMft3 <- ADEL21_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL21_SDMTable <- graph.adjacency(ADEL21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(ADEL21_SDMTable, vertex.label = V(ADEL21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
ADEL21_SDM.clusterCoef <- transitivity(ADEL21_SDMTable, type="global") #cluster coefficient
ADEL21_SDM.degreeCent <- centralization.degree(ADEL21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_SDMftn <- as.network.matrix(ADEL21_SDMft)
ADEL21_SDM.netDensity <- network.density(ADEL21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_SDM.entropy <- entropy(ADEL21_SDMft) #entropy

ADEL21_SDM.netMx <- cbind(ADEL21_SDM.netMx, ADEL21_SDM.clusterCoef, ADEL21_SDM.degreeCent$centralization,
                          ADEL21_SDM.netDensity, ADEL21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
ADEL21_TDMg2 <- data.frame(ADEL21_TDM)
ADEL21_TDMg2 <- ADEL21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_TDMg2$player1
player2vector <- ADEL21_TDMg2$player2
ADEL21_TDMg3 <- ADEL21_TDMg2
ADEL21_TDMg3$p1inp2vec <- is.element(ADEL21_TDMg3$player1, player2vector)
ADEL21_TDMg3$p2inp1vec <- is.element(ADEL21_TDMg3$player2, player1vector)

addPlayer1 <- ADEL21_TDMg3[ which(ADEL21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_TDMg3[ which(ADEL21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_TDMg2 <- rbind(ADEL21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
ADEL21_TDMft <- ftable(ADEL21_TDMg2$player1, ADEL21_TDMg2$player2)
ADEL21_TDMft2 <- as.matrix(ADEL21_TDMft)
numRows <- nrow(ADEL21_TDMft2)
numCols <- ncol(ADEL21_TDMft2)
ADEL21_TDMft3 <- ADEL21_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL21_TDMTable <- graph.adjacency(ADEL21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(ADEL21_TDMTable, vertex.label = V(ADEL21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
ADEL21_TDM.clusterCoef <- transitivity(ADEL21_TDMTable, type="global") #cluster coefficient
ADEL21_TDM.degreeCent <- centralization.degree(ADEL21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_TDMftn <- as.network.matrix(ADEL21_TDMft)
ADEL21_TDM.netDensity <- network.density(ADEL21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_TDM.entropy <- entropy(ADEL21_TDMft) #entropy

ADEL21_TDM.netMx <- cbind(ADEL21_TDM.netMx, ADEL21_TDM.clusterCoef, ADEL21_TDM.degreeCent$centralization,
                          ADEL21_TDM.netDensity, ADEL21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
ADEL21_SDg2 <- data.frame(ADEL21_SD)
ADEL21_SDg2 <- ADEL21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_SDg2$player1
player2vector <- ADEL21_SDg2$player2
ADEL21_SDg3 <- ADEL21_SDg2
ADEL21_SDg3$p1inp2vec <- is.element(ADEL21_SDg3$player1, player2vector)
ADEL21_SDg3$p2inp1vec <- is.element(ADEL21_SDg3$player2, player1vector)

addPlayer1 <- ADEL21_SDg3[ which(ADEL21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_SDg3[ which(ADEL21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_SDg2 <- rbind(ADEL21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
ADEL21_SDft <- ftable(ADEL21_SDg2$player1, ADEL21_SDg2$player2)
ADEL21_SDft2 <- as.matrix(ADEL21_SDft)
numRows <- nrow(ADEL21_SDft2)
numCols <- ncol(ADEL21_SDft2)
ADEL21_SDft3 <- ADEL21_SDft2[c(2:numRows) , c(2:numCols)]
ADEL21_SDTable <- graph.adjacency(ADEL21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(ADEL21_SDTable, vertex.label = V(ADEL21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
ADEL21_SD.clusterCoef <- transitivity(ADEL21_SDTable, type="global") #cluster coefficient
ADEL21_SD.degreeCent <- centralization.degree(ADEL21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_SDftn <- as.network.matrix(ADEL21_SDft)
ADEL21_SD.netDensity <- network.density(ADEL21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_SD.entropy <- entropy(ADEL21_SDft) #entropy

ADEL21_SD.netMx <- cbind(ADEL21_SD.netMx, ADEL21_SD.clusterCoef, ADEL21_SD.degreeCent$centralization,
                         ADEL21_SD.netDensity, ADEL21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
ADEL21_TDg2 <- data.frame(ADEL21_TD)
ADEL21_TDg2 <- ADEL21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_TDg2$player1
player2vector <- ADEL21_TDg2$player2
ADEL21_TDg3 <- ADEL21_TDg2
ADEL21_TDg3$p1inp2vec <- is.element(ADEL21_TDg3$player1, player2vector)
ADEL21_TDg3$p2inp1vec <- is.element(ADEL21_TDg3$player2, player1vector)

addPlayer1 <- ADEL21_TDg3[ which(ADEL21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_TDg3[ which(ADEL21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_TDg2 <- rbind(ADEL21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
ADEL21_TDft <- ftable(ADEL21_TDg2$player1, ADEL21_TDg2$player2)
ADEL21_TDft2 <- as.matrix(ADEL21_TDft)
numRows <- nrow(ADEL21_TDft2)
numCols <- ncol(ADEL21_TDft2)
ADEL21_TDft3 <- ADEL21_TDft2[c(2:numRows) , c(2:numCols)]
ADEL21_TDTable <- graph.adjacency(ADEL21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(ADEL21_TDTable, vertex.label = V(ADEL21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
ADEL21_TD.clusterCoef <- transitivity(ADEL21_TDTable, type="global") #cluster coefficient
ADEL21_TD.degreeCent <- centralization.degree(ADEL21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_TDftn <- as.network.matrix(ADEL21_TDft)
ADEL21_TD.netDensity <- network.density(ADEL21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_TD.entropy <- entropy(ADEL21_TDft) #entropy

ADEL21_TD.netMx <- cbind(ADEL21_TD.netMx, ADEL21_TD.clusterCoef, ADEL21_TD.degreeCent$centralization,
                         ADEL21_TD.netDensity, ADEL21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
ADEL21_QTg2 <- data.frame(ADEL21_QT)
ADEL21_QTg2 <- ADEL21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL21_QTg2$player1
player2vector <- ADEL21_QTg2$player2
ADEL21_QTg3 <- ADEL21_QTg2
ADEL21_QTg3$p1inp2vec <- is.element(ADEL21_QTg3$player1, player2vector)
ADEL21_QTg3$p2inp1vec <- is.element(ADEL21_QTg3$player2, player1vector)

addPlayer1 <- ADEL21_QTg3[ which(ADEL21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL21_QTg3[ which(ADEL21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL21_QTg2 <- rbind(ADEL21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
ADEL21_QTft <- ftable(ADEL21_QTg2$player1, ADEL21_QTg2$player2)
ADEL21_QTft2 <- as.matrix(ADEL21_QTft)
numRows <- nrow(ADEL21_QTft2)
numCols <- ncol(ADEL21_QTft2)
ADEL21_QTft3 <- ADEL21_QTft2[c(2:numRows) , c(2:numCols)]
ADEL21_QTTable <- graph.adjacency(ADEL21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(ADEL21_QTTable, vertex.label = V(ADEL21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
ADEL21_QT.clusterCoef <- transitivity(ADEL21_QTTable, type="global") #cluster coefficient
ADEL21_QT.degreeCent <- centralization.degree(ADEL21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL21_QTftn <- as.network.matrix(ADEL21_QTft)
ADEL21_QT.netDensity <- network.density(ADEL21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL21_QT.entropy <- entropy(ADEL21_QTft) #entropy

ADEL21_QT.netMx <- cbind(ADEL21_QT.netMx, ADEL21_QT.clusterCoef, ADEL21_QT.degreeCent$centralization,
                         ADEL21_QT.netDensity, ADEL21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL21_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "BL"
KIoutcome = "Goal_F"
BL21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
BL21_Gg2 <- data.frame(BL21_G)
BL21_Gg2 <- BL21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_Gg2$player1
player2vector <- BL21_Gg2$player2
BL21_Gg3 <- BL21_Gg2
BL21_Gg3$p1inp2vec <- is.element(BL21_Gg3$player1, player2vector)
BL21_Gg3$p2inp1vec <- is.element(BL21_Gg3$player2, player1vector)

addPlayer1 <- BL21_Gg3[ which(BL21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_Gg3[ which(BL21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_Gg2 <- rbind(BL21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
BL21_Gft <- ftable(BL21_Gg2$player1, BL21_Gg2$player2)
BL21_Gft2 <- as.matrix(BL21_Gft)
numRows <- nrow(BL21_Gft2)
numCols <- ncol(BL21_Gft2)
BL21_Gft3 <- BL21_Gft2[c(2:numRows) , c(2:numCols)]
BL21_GTable <- graph.adjacency(BL21_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(BL21_GTable, vertex.label = V(BL21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
BL21_G.clusterCoef <- transitivity(BL21_GTable, type="global") #cluster coefficient
BL21_G.degreeCent <- centralization.degree(BL21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_Gftn <- as.network.matrix(BL21_Gft)
BL21_G.netDensity <- network.density(BL21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_G.entropy <- entropy(BL21_Gft) #entropy

BL21_G.netMx <- cbind(BL21_G.netMx, BL21_G.clusterCoef, BL21_G.degreeCent$centralization,
                      BL21_G.netDensity, BL21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************

round = 21
teamName = "BL"
KIoutcome = "Behind_F"
BL21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
BL21_Bg2 <- data.frame(BL21_B)
BL21_Bg2 <- BL21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_Bg2$player1
player2vector <- BL21_Bg2$player2
BL21_Bg3 <- BL21_Bg2
BL21_Bg3$p1inp2vec <- is.element(BL21_Bg3$player1, player2vector)
BL21_Bg3$p2inp1vec <- is.element(BL21_Bg3$player2, player1vector)

addPlayer1 <- BL21_Bg3[ which(BL21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL21_Bg3[ which(BL21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_Bg2 <- rbind(BL21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
BL21_Bft <- ftable(BL21_Bg2$player1, BL21_Bg2$player2)
BL21_Bft2 <- as.matrix(BL21_Bft)
numRows <- nrow(BL21_Bft2)
numCols <- ncol(BL21_Bft2)
BL21_Bft3 <- BL21_Bft2[c(2:numRows) , c(2:numCols)]
BL21_BTable <- graph.adjacency(BL21_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(BL21_BTable, vertex.label = V(BL21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
BL21_B.clusterCoef <- transitivity(BL21_BTable, type="global") #cluster coefficient
BL21_B.degreeCent <- centralization.degree(BL21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_Bftn <- as.network.matrix(BL21_Bft)
BL21_B.netDensity <- network.density(BL21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_B.entropy <- entropy(BL21_Bft) #entropy

BL21_B.netMx <- cbind(BL21_B.netMx, BL21_B.clusterCoef, BL21_B.degreeCent$centralization,
                      BL21_B.netDensity, BL21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "BL"
KIoutcome = "Stoppage_F"
BL21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
BL21_SFg2 <- data.frame(BL21_SF)
BL21_SFg2 <- BL21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_SFg2$player1
player2vector <- BL21_SFg2$player2
BL21_SFg3 <- BL21_SFg2
BL21_SFg3$p1inp2vec <- is.element(BL21_SFg3$player1, player2vector)
BL21_SFg3$p2inp1vec <- is.element(BL21_SFg3$player2, player1vector)

addPlayer1 <- BL21_SFg3[ which(BL21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_SFg3[ which(BL21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_SFg2 <- rbind(BL21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
BL21_SFft <- ftable(BL21_SFg2$player1, BL21_SFg2$player2)
BL21_SFft2 <- as.matrix(BL21_SFft)
numRows <- nrow(BL21_SFft2)
numCols <- ncol(BL21_SFft2)
BL21_SFft3 <- BL21_SFft2[c(2:numRows) , c(2:numCols)]
BL21_SFTable <- graph.adjacency(BL21_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(BL21_SFTable, vertex.label = V(BL21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
BL21_SF.clusterCoef <- transitivity(BL21_SFTable, type="global") #cluster coefficient
BL21_SF.degreeCent <- centralization.degree(BL21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_SFftn <- as.network.matrix(BL21_SFft)
BL21_SF.netDensity <- network.density(BL21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_SF.entropy <- entropy(BL21_SFft) #entropy

BL21_SF.netMx <- cbind(BL21_SF.netMx, BL21_SF.clusterCoef, BL21_SF.degreeCent$centralization,
                       BL21_SF.netDensity, BL21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "BL"
KIoutcome = "Turnover_F"
BL21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
BL21_TFg2 <- data.frame(BL21_TF)
BL21_TFg2 <- BL21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_TFg2$player1
player2vector <- BL21_TFg2$player2
BL21_TFg3 <- BL21_TFg2
BL21_TFg3$p1inp2vec <- is.element(BL21_TFg3$player1, player2vector)
BL21_TFg3$p2inp1vec <- is.element(BL21_TFg3$player2, player1vector)

addPlayer1 <- BL21_TFg3[ which(BL21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_TFg3[ which(BL21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_TFg2 <- rbind(BL21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
BL21_TFft <- ftable(BL21_TFg2$player1, BL21_TFg2$player2)
BL21_TFft2 <- as.matrix(BL21_TFft)
numRows <- nrow(BL21_TFft2)
numCols <- ncol(BL21_TFft2)
BL21_TFft3 <- BL21_TFft2[c(2:numRows) , c(2:numCols)]
BL21_TFTable <- graph.adjacency(BL21_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(BL21_TFTable, vertex.label = V(BL21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
BL21_TF.clusterCoef <- transitivity(BL21_TFTable, type="global") #cluster coefficient
BL21_TF.degreeCent <- centralization.degree(BL21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_TFftn <- as.network.matrix(BL21_TFft)
BL21_TF.netDensity <- network.density(BL21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_TF.entropy <- entropy(BL21_TFft) #entropy

BL21_TF.netMx <- cbind(BL21_TF.netMx, BL21_TF.clusterCoef, BL21_TF.degreeCent$centralization,
                       BL21_TF.netDensity, BL21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
BL21_SAMg2 <- data.frame(BL21_SAM)
BL21_SAMg2 <- BL21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_SAMg2$player1
player2vector <- BL21_SAMg2$player2
BL21_SAMg3 <- BL21_SAMg2
BL21_SAMg3$p1inp2vec <- is.element(BL21_SAMg3$player1, player2vector)
BL21_SAMg3$p2inp1vec <- is.element(BL21_SAMg3$player2, player1vector)

addPlayer1 <- BL21_SAMg3[ which(BL21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_SAMg3[ which(BL21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_SAMg2 <- rbind(BL21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
BL21_SAMft <- ftable(BL21_SAMg2$player1, BL21_SAMg2$player2)
BL21_SAMft2 <- as.matrix(BL21_SAMft)
numRows <- nrow(BL21_SAMft2)
numCols <- ncol(BL21_SAMft2)
BL21_SAMft3 <- BL21_SAMft2[c(2:numRows) , c(2:numCols)]
BL21_SAMTable <- graph.adjacency(BL21_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(BL21_SAMTable, vertex.label = V(BL21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
BL21_SAM.clusterCoef <- transitivity(BL21_SAMTable, type="global") #cluster coefficient
BL21_SAM.degreeCent <- centralization.degree(BL21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_SAMftn <- as.network.matrix(BL21_SAMft)
BL21_SAM.netDensity <- network.density(BL21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_SAM.entropy <- entropy(BL21_SAMft) #entropy

BL21_SAM.netMx <- cbind(BL21_SAM.netMx, BL21_SAM.clusterCoef, BL21_SAM.degreeCent$centralization,
                        BL21_SAM.netDensity, BL21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "BL"
KIoutcome = "Turnover_AM"
BL21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
BL21_TAMg2 <- data.frame(BL21_TAM)
BL21_TAMg2 <- BL21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_TAMg2$player1
player2vector <- BL21_TAMg2$player2
BL21_TAMg3 <- BL21_TAMg2
BL21_TAMg3$p1inp2vec <- is.element(BL21_TAMg3$player1, player2vector)
BL21_TAMg3$p2inp1vec <- is.element(BL21_TAMg3$player2, player1vector)

addPlayer1 <- BL21_TAMg3[ which(BL21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL21_TAMg3[ which(BL21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_TAMg2 <- rbind(BL21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
BL21_TAMft <- ftable(BL21_TAMg2$player1, BL21_TAMg2$player2)
BL21_TAMft2 <- as.matrix(BL21_TAMft)
numRows <- nrow(BL21_TAMft2)
numCols <- ncol(BL21_TAMft2)
BL21_TAMft3 <- BL21_TAMft2[c(2:numRows) , c(2:numCols)]
BL21_TAMTable <- graph.adjacency(BL21_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(BL21_TAMTable, vertex.label = V(BL21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
BL21_TAM.clusterCoef <- transitivity(BL21_TAMTable, type="global") #cluster coefficient
BL21_TAM.degreeCent <- centralization.degree(BL21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_TAMftn <- as.network.matrix(BL21_TAMft)
BL21_TAM.netDensity <- network.density(BL21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_TAM.entropy <- entropy(BL21_TAMft) #entropy

BL21_TAM.netMx <- cbind(BL21_TAM.netMx, BL21_TAM.clusterCoef, BL21_TAM.degreeCent$centralization,
                        BL21_TAM.netDensity, BL21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
BL21_SDMg2 <- data.frame(BL21_SDM)
BL21_SDMg2 <- BL21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_SDMg2$player1
player2vector <- BL21_SDMg2$player2
BL21_SDMg3 <- BL21_SDMg2
BL21_SDMg3$p1inp2vec <- is.element(BL21_SDMg3$player1, player2vector)
BL21_SDMg3$p2inp1vec <- is.element(BL21_SDMg3$player2, player1vector)

addPlayer1 <- BL21_SDMg3[ which(BL21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_SDMg3[ which(BL21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_SDMg2 <- rbind(BL21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
BL21_SDMft <- ftable(BL21_SDMg2$player1, BL21_SDMg2$player2)
BL21_SDMft2 <- as.matrix(BL21_SDMft)
numRows <- nrow(BL21_SDMft2)
numCols <- ncol(BL21_SDMft2)
BL21_SDMft3 <- BL21_SDMft2[c(2:numRows) , c(2:numCols)]
BL21_SDMTable <- graph.adjacency(BL21_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(BL21_SDMTable, vertex.label = V(BL21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
BL21_SDM.clusterCoef <- transitivity(BL21_SDMTable, type="global") #cluster coefficient
BL21_SDM.degreeCent <- centralization.degree(BL21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_SDMftn <- as.network.matrix(BL21_SDMft)
BL21_SDM.netDensity <- network.density(BL21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_SDM.entropy <- entropy(BL21_SDMft) #entropy

BL21_SDM.netMx <- cbind(BL21_SDM.netMx, BL21_SDM.clusterCoef, BL21_SDM.degreeCent$centralization,
                        BL21_SDM.netDensity, BL21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "BL"
KIoutcome = "Turnover_DM"
BL21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
BL21_TDMg2 <- data.frame(BL21_TDM)
BL21_TDMg2 <- BL21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_TDMg2$player1
player2vector <- BL21_TDMg2$player2
BL21_TDMg3 <- BL21_TDMg2
BL21_TDMg3$p1inp2vec <- is.element(BL21_TDMg3$player1, player2vector)
BL21_TDMg3$p2inp1vec <- is.element(BL21_TDMg3$player2, player1vector)

addPlayer1 <- BL21_TDMg3[ which(BL21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_TDMg3[ which(BL21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_TDMg2 <- rbind(BL21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
BL21_TDMft <- ftable(BL21_TDMg2$player1, BL21_TDMg2$player2)
BL21_TDMft2 <- as.matrix(BL21_TDMft)
numRows <- nrow(BL21_TDMft2)
numCols <- ncol(BL21_TDMft2)
BL21_TDMft3 <- BL21_TDMft2[c(2:numRows) , c(2:numCols)]
BL21_TDMTable <- graph.adjacency(BL21_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(BL21_TDMTable, vertex.label = V(BL21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
BL21_TDM.clusterCoef <- transitivity(BL21_TDMTable, type="global") #cluster coefficient
BL21_TDM.degreeCent <- centralization.degree(BL21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_TDMftn <- as.network.matrix(BL21_TDMft)
BL21_TDM.netDensity <- network.density(BL21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_TDM.entropy <- entropy(BL21_TDMft) #entropy

BL21_TDM.netMx <- cbind(BL21_TDM.netMx, BL21_TDM.clusterCoef, BL21_TDM.degreeCent$centralization,
                        BL21_TDM.netDensity, BL21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "BL"
KIoutcome = "Stoppage_D"
BL21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
BL21_SDg2 <- data.frame(BL21_SD)
BL21_SDg2 <- BL21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_SDg2$player1
player2vector <- BL21_SDg2$player2
BL21_SDg3 <- BL21_SDg2
BL21_SDg3$p1inp2vec <- is.element(BL21_SDg3$player1, player2vector)
BL21_SDg3$p2inp1vec <- is.element(BL21_SDg3$player2, player1vector)

addPlayer1 <- BL21_SDg3[ which(BL21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_SDg3[ which(BL21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_SDg2 <- rbind(BL21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
BL21_SDft <- ftable(BL21_SDg2$player1, BL21_SDg2$player2)
BL21_SDft2 <- as.matrix(BL21_SDft)
numRows <- nrow(BL21_SDft2)
numCols <- ncol(BL21_SDft2)
BL21_SDft3 <- BL21_SDft2[c(2:numRows) , c(2:numCols)]
BL21_SDTable <- graph.adjacency(BL21_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(BL21_SDTable, vertex.label = V(BL21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
BL21_SD.clusterCoef <- transitivity(BL21_SDTable, type="global") #cluster coefficient
BL21_SD.degreeCent <- centralization.degree(BL21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_SDftn <- as.network.matrix(BL21_SDft)
BL21_SD.netDensity <- network.density(BL21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_SD.entropy <- entropy(BL21_SDft) #entropy

BL21_SD.netMx <- cbind(BL21_SD.netMx, BL21_SD.clusterCoef, BL21_SD.degreeCent$centralization,
                       BL21_SD.netDensity, BL21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************

round = 21
teamName = "BL"
KIoutcome = "Turnover_D"
BL21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
BL21_TDg2 <- data.frame(BL21_TD)
BL21_TDg2 <- BL21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_TDg2$player1
player2vector <- BL21_TDg2$player2
BL21_TDg3 <- BL21_TDg2
BL21_TDg3$p1inp2vec <- is.element(BL21_TDg3$player1, player2vector)
BL21_TDg3$p2inp1vec <- is.element(BL21_TDg3$player2, player1vector)

addPlayer1 <- BL21_TDg3[ which(BL21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_TDg3[ which(BL21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_TDg2 <- rbind(BL21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
BL21_TDft <- ftable(BL21_TDg2$player1, BL21_TDg2$player2)
BL21_TDft2 <- as.matrix(BL21_TDft)
numRows <- nrow(BL21_TDft2)
numCols <- ncol(BL21_TDft2)
BL21_TDft3 <- BL21_TDft2[c(2:numRows) , c(2:numCols)]
BL21_TDTable <- graph.adjacency(BL21_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(BL21_TDTable, vertex.label = V(BL21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
BL21_TD.clusterCoef <- transitivity(BL21_TDTable, type="global") #cluster coefficient
BL21_TD.degreeCent <- centralization.degree(BL21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_TDftn <- as.network.matrix(BL21_TDft)
BL21_TD.netDensity <- network.density(BL21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_TD.entropy <- entropy(BL21_TDft) #entropy

BL21_TD.netMx <- cbind(BL21_TD.netMx, BL21_TD.clusterCoef, BL21_TD.degreeCent$centralization,
                       BL21_TD.netDensity, BL21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
BL21_QTg2 <- data.frame(BL21_QT)
BL21_QTg2 <- BL21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL21_QTg2$player1
player2vector <- BL21_QTg2$player2
BL21_QTg3 <- BL21_QTg2
BL21_QTg3$p1inp2vec <- is.element(BL21_QTg3$player1, player2vector)
BL21_QTg3$p2inp1vec <- is.element(BL21_QTg3$player2, player1vector)

addPlayer1 <- BL21_QTg3[ which(BL21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL21_QTg3[ which(BL21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL21_QTg2 <- rbind(BL21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
BL21_QTft <- ftable(BL21_QTg2$player1, BL21_QTg2$player2)
BL21_QTft2 <- as.matrix(BL21_QTft)
numRows <- nrow(BL21_QTft2)
numCols <- ncol(BL21_QTft2)
BL21_QTft3 <- BL21_QTft2[c(2:numRows) , c(2:numCols)]
BL21_QTTable <- graph.adjacency(BL21_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(BL21_QTTable, vertex.label = V(BL21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
BL21_QT.clusterCoef <- transitivity(BL21_QTTable, type="global") #cluster coefficient
BL21_QT.degreeCent <- centralization.degree(BL21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL21_QTftn <- as.network.matrix(BL21_QTft)
BL21_QT.netDensity <- network.density(BL21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL21_QT.entropy <- entropy(BL21_QTft) #entropy

BL21_QT.netMx <- cbind(BL21_QT.netMx, BL21_QT.clusterCoef, BL21_QT.degreeCent$centralization,
                       BL21_QT.netDensity, BL21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL21_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************
#NA

round = 21
teamName = "CARL"
KIoutcome = "Goal_F"
CARL21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
CARL21_Gg2 <- data.frame(CARL21_G)
CARL21_Gg2 <- CARL21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_Gg2$player1
player2vector <- CARL21_Gg2$player2
CARL21_Gg3 <- CARL21_Gg2
CARL21_Gg3$p1inp2vec <- is.element(CARL21_Gg3$player1, player2vector)
CARL21_Gg3$p2inp1vec <- is.element(CARL21_Gg3$player2, player1vector)

addPlayer1 <- CARL21_Gg3[ which(CARL21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_Gg3[ which(CARL21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_Gg2 <- rbind(CARL21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
CARL21_Gft <- ftable(CARL21_Gg2$player1, CARL21_Gg2$player2)
CARL21_Gft2 <- as.matrix(CARL21_Gft)
numRows <- nrow(CARL21_Gft2)
numCols <- ncol(CARL21_Gft2)
CARL21_Gft3 <- CARL21_Gft2[c(2:numRows) , c(2:numCols)]
CARL21_GTable <- graph.adjacency(CARL21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(CARL21_GTable, vertex.label = V(CARL21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
CARL21_G.clusterCoef <- transitivity(CARL21_GTable, type="global") #cluster coefficient
CARL21_G.degreeCent <- centralization.degree(CARL21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_Gftn <- as.network.matrix(CARL21_Gft)
CARL21_G.netDensity <- network.density(CARL21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_G.entropy <- entropy(CARL21_Gft) #entropy

CARL21_G.netMx <- cbind(CARL21_G.netMx, CARL21_G.clusterCoef, CARL21_G.degreeCent$centralization,
                        CARL21_G.netDensity, CARL21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "CARL"
KIoutcome = "Behind_F"
CARL21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
CARL21_Bg2 <- data.frame(CARL21_B)
CARL21_Bg2 <- CARL21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_Bg2$player1
player2vector <- CARL21_Bg2$player2
CARL21_Bg3 <- CARL21_Bg2
CARL21_Bg3$p1inp2vec <- is.element(CARL21_Bg3$player1, player2vector)
CARL21_Bg3$p2inp1vec <- is.element(CARL21_Bg3$player2, player1vector)

addPlayer1 <- CARL21_Bg3[ which(CARL21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_Bg3[ which(CARL21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_Bg2 <- rbind(CARL21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
CARL21_Bft <- ftable(CARL21_Bg2$player1, CARL21_Bg2$player2)
CARL21_Bft2 <- as.matrix(CARL21_Bft)
numRows <- nrow(CARL21_Bft2)
numCols <- ncol(CARL21_Bft2)
CARL21_Bft3 <- CARL21_Bft2[c(2:numRows) , c(2:numCols)]
CARL21_BTable <- graph.adjacency(CARL21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(CARL21_BTable, vertex.label = V(CARL21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
CARL21_B.clusterCoef <- transitivity(CARL21_BTable, type="global") #cluster coefficient
CARL21_B.degreeCent <- centralization.degree(CARL21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_Bftn <- as.network.matrix(CARL21_Bft)
CARL21_B.netDensity <- network.density(CARL21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_B.entropy <- entropy(CARL21_Bft) #entropy

CARL21_B.netMx <- cbind(CARL21_B.netMx, CARL21_B.clusterCoef, CARL21_B.degreeCent$centralization,
                        CARL21_B.netDensity, CARL21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
CARL21_SFg2 <- data.frame(CARL21_SF)
CARL21_SFg2 <- CARL21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_SFg2$player1
player2vector <- CARL21_SFg2$player2
CARL21_SFg3 <- CARL21_SFg2
CARL21_SFg3$p1inp2vec <- is.element(CARL21_SFg3$player1, player2vector)
CARL21_SFg3$p2inp1vec <- is.element(CARL21_SFg3$player2, player1vector)

addPlayer1 <- CARL21_SFg3[ which(CARL21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_SFg3[ which(CARL21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_SFg2 <- rbind(CARL21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
CARL21_SFft <- ftable(CARL21_SFg2$player1, CARL21_SFg2$player2)
CARL21_SFft2 <- as.matrix(CARL21_SFft)
numRows <- nrow(CARL21_SFft2)
numCols <- ncol(CARL21_SFft2)
CARL21_SFft3 <- CARL21_SFft2[c(2:numRows) , c(2:numCols)]
CARL21_SFTable <- graph.adjacency(CARL21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(CARL21_SFTable, vertex.label = V(CARL21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
CARL21_SF.clusterCoef <- transitivity(CARL21_SFTable, type="global") #cluster coefficient
CARL21_SF.degreeCent <- centralization.degree(CARL21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_SFftn <- as.network.matrix(CARL21_SFft)
CARL21_SF.netDensity <- network.density(CARL21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_SF.entropy <- entropy(CARL21_SFft) #entropy

CARL21_SF.netMx <- cbind(CARL21_SF.netMx, CARL21_SF.clusterCoef, CARL21_SF.degreeCent$centralization,
                         CARL21_SF.netDensity, CARL21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
CARL21_TFg2 <- data.frame(CARL21_TF)
CARL21_TFg2 <- CARL21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_TFg2$player1
player2vector <- CARL21_TFg2$player2
CARL21_TFg3 <- CARL21_TFg2
CARL21_TFg3$p1inp2vec <- is.element(CARL21_TFg3$player1, player2vector)
CARL21_TFg3$p2inp1vec <- is.element(CARL21_TFg3$player2, player1vector)

addPlayer1 <- CARL21_TFg3[ which(CARL21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_TFg3[ which(CARL21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_TFg2 <- rbind(CARL21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
CARL21_TFft <- ftable(CARL21_TFg2$player1, CARL21_TFg2$player2)
CARL21_TFft2 <- as.matrix(CARL21_TFft)
numRows <- nrow(CARL21_TFft2)
numCols <- ncol(CARL21_TFft2)
CARL21_TFft3 <- CARL21_TFft2[c(2:numRows) , c(2:numCols)]
CARL21_TFTable <- graph.adjacency(CARL21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(CARL21_TFTable, vertex.label = V(CARL21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
CARL21_TF.clusterCoef <- transitivity(CARL21_TFTable, type="global") #cluster coefficient
CARL21_TF.degreeCent <- centralization.degree(CARL21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_TFftn <- as.network.matrix(CARL21_TFft)
CARL21_TF.netDensity <- network.density(CARL21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_TF.entropy <- entropy(CARL21_TFft) #entropy

CARL21_TF.netMx <- cbind(CARL21_TF.netMx, CARL21_TF.clusterCoef, CARL21_TF.degreeCent$centralization,
                         CARL21_TF.netDensity, CARL21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
CARL21_SAMg2 <- data.frame(CARL21_SAM)
CARL21_SAMg2 <- CARL21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_SAMg2$player1
player2vector <- CARL21_SAMg2$player2
CARL21_SAMg3 <- CARL21_SAMg2
CARL21_SAMg3$p1inp2vec <- is.element(CARL21_SAMg3$player1, player2vector)
CARL21_SAMg3$p2inp1vec <- is.element(CARL21_SAMg3$player2, player1vector)

addPlayer1 <- CARL21_SAMg3[ which(CARL21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_SAMg3[ which(CARL21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_SAMg2 <- rbind(CARL21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
CARL21_SAMft <- ftable(CARL21_SAMg2$player1, CARL21_SAMg2$player2)
CARL21_SAMft2 <- as.matrix(CARL21_SAMft)
numRows <- nrow(CARL21_SAMft2)
numCols <- ncol(CARL21_SAMft2)
CARL21_SAMft3 <- CARL21_SAMft2[c(2:numRows) , c(2:numCols)]
CARL21_SAMTable <- graph.adjacency(CARL21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(CARL21_SAMTable, vertex.label = V(CARL21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
CARL21_SAM.clusterCoef <- transitivity(CARL21_SAMTable, type="global") #cluster coefficient
CARL21_SAM.degreeCent <- centralization.degree(CARL21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_SAMftn <- as.network.matrix(CARL21_SAMft)
CARL21_SAM.netDensity <- network.density(CARL21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_SAM.entropy <- entropy(CARL21_SAMft) #entropy

CARL21_SAM.netMx <- cbind(CARL21_SAM.netMx, CARL21_SAM.clusterCoef, CARL21_SAM.degreeCent$centralization,
                          CARL21_SAM.netDensity, CARL21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
CARL21_TAMg2 <- data.frame(CARL21_TAM)
CARL21_TAMg2 <- CARL21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_TAMg2$player1
player2vector <- CARL21_TAMg2$player2
CARL21_TAMg3 <- CARL21_TAMg2
CARL21_TAMg3$p1inp2vec <- is.element(CARL21_TAMg3$player1, player2vector)
CARL21_TAMg3$p2inp1vec <- is.element(CARL21_TAMg3$player2, player1vector)

addPlayer1 <- CARL21_TAMg3[ which(CARL21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_TAMg3[ which(CARL21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_TAMg2 <- rbind(CARL21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
CARL21_TAMft <- ftable(CARL21_TAMg2$player1, CARL21_TAMg2$player2)
CARL21_TAMft2 <- as.matrix(CARL21_TAMft)
numRows <- nrow(CARL21_TAMft2)
numCols <- ncol(CARL21_TAMft2)
CARL21_TAMft3 <- CARL21_TAMft2[c(2:numRows) , c(2:numCols)]
CARL21_TAMTable <- graph.adjacency(CARL21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(CARL21_TAMTable, vertex.label = V(CARL21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
CARL21_TAM.clusterCoef <- transitivity(CARL21_TAMTable, type="global") #cluster coefficient
CARL21_TAM.degreeCent <- centralization.degree(CARL21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_TAMftn <- as.network.matrix(CARL21_TAMft)
CARL21_TAM.netDensity <- network.density(CARL21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_TAM.entropy <- entropy(CARL21_TAMft) #entropy

CARL21_TAM.netMx <- cbind(CARL21_TAM.netMx, CARL21_TAM.clusterCoef, CARL21_TAM.degreeCent$centralization,
                          CARL21_TAM.netDensity, CARL21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
CARL21_SDMg2 <- data.frame(CARL21_SDM)
CARL21_SDMg2 <- CARL21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_SDMg2$player1
player2vector <- CARL21_SDMg2$player2
CARL21_SDMg3 <- CARL21_SDMg2
CARL21_SDMg3$p1inp2vec <- is.element(CARL21_SDMg3$player1, player2vector)
CARL21_SDMg3$p2inp1vec <- is.element(CARL21_SDMg3$player2, player1vector)

addPlayer1 <- CARL21_SDMg3[ which(CARL21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL21_SDMg3[ which(CARL21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_SDMg2 <- rbind(CARL21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
CARL21_SDMft <- ftable(CARL21_SDMg2$player1, CARL21_SDMg2$player2)
CARL21_SDMft2 <- as.matrix(CARL21_SDMft)
numRows <- nrow(CARL21_SDMft2)
numCols <- ncol(CARL21_SDMft2)
CARL21_SDMft3 <- CARL21_SDMft2[c(2:numRows) , c(2:numCols)]
CARL21_SDMTable <- graph.adjacency(CARL21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(CARL21_SDMTable, vertex.label = V(CARL21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
CARL21_SDM.clusterCoef <- transitivity(CARL21_SDMTable, type="global") #cluster coefficient
CARL21_SDM.degreeCent <- centralization.degree(CARL21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_SDMftn <- as.network.matrix(CARL21_SDMft)
CARL21_SDM.netDensity <- network.density(CARL21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_SDM.entropy <- entropy(CARL21_SDMft) #entropy

CARL21_SDM.netMx <- cbind(CARL21_SDM.netMx, CARL21_SDM.clusterCoef, CARL21_SDM.degreeCent$centralization,
                          CARL21_SDM.netDensity, CARL21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
CARL21_TDMg2 <- data.frame(CARL21_TDM)
CARL21_TDMg2 <- CARL21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_TDMg2$player1
player2vector <- CARL21_TDMg2$player2
CARL21_TDMg3 <- CARL21_TDMg2
CARL21_TDMg3$p1inp2vec <- is.element(CARL21_TDMg3$player1, player2vector)
CARL21_TDMg3$p2inp1vec <- is.element(CARL21_TDMg3$player2, player1vector)

addPlayer1 <- CARL21_TDMg3[ which(CARL21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_TDMg3[ which(CARL21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_TDMg2 <- rbind(CARL21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
CARL21_TDMft <- ftable(CARL21_TDMg2$player1, CARL21_TDMg2$player2)
CARL21_TDMft2 <- as.matrix(CARL21_TDMft)
numRows <- nrow(CARL21_TDMft2)
numCols <- ncol(CARL21_TDMft2)
CARL21_TDMft3 <- CARL21_TDMft2[c(2:numRows) , c(2:numCols)]
CARL21_TDMTable <- graph.adjacency(CARL21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(CARL21_TDMTable, vertex.label = V(CARL21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
CARL21_TDM.clusterCoef <- transitivity(CARL21_TDMTable, type="global") #cluster coefficient
CARL21_TDM.degreeCent <- centralization.degree(CARL21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_TDMftn <- as.network.matrix(CARL21_TDMft)
CARL21_TDM.netDensity <- network.density(CARL21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_TDM.entropy <- entropy(CARL21_TDMft) #entropy

CARL21_TDM.netMx <- cbind(CARL21_TDM.netMx, CARL21_TDM.clusterCoef, CARL21_TDM.degreeCent$centralization,
                          CARL21_TDM.netDensity, CARL21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
CARL21_SDg2 <- data.frame(CARL21_SD)
CARL21_SDg2 <- CARL21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_SDg2$player1
player2vector <- CARL21_SDg2$player2
CARL21_SDg3 <- CARL21_SDg2
CARL21_SDg3$p1inp2vec <- is.element(CARL21_SDg3$player1, player2vector)
CARL21_SDg3$p2inp1vec <- is.element(CARL21_SDg3$player2, player1vector)

addPlayer1 <- CARL21_SDg3[ which(CARL21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_SDg3[ which(CARL21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_SDg2 <- rbind(CARL21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
CARL21_SDft <- ftable(CARL21_SDg2$player1, CARL21_SDg2$player2)
CARL21_SDft2 <- as.matrix(CARL21_SDft)
numRows <- nrow(CARL21_SDft2)
numCols <- ncol(CARL21_SDft2)
CARL21_SDft3 <- CARL21_SDft2[c(2:numRows) , c(2:numCols)]
CARL21_SDTable <- graph.adjacency(CARL21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(CARL21_SDTable, vertex.label = V(CARL21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
CARL21_SD.clusterCoef <- transitivity(CARL21_SDTable, type="global") #cluster coefficient
CARL21_SD.degreeCent <- centralization.degree(CARL21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_SDftn <- as.network.matrix(CARL21_SDft)
CARL21_SD.netDensity <- network.density(CARL21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_SD.entropy <- entropy(CARL21_SDft) #entropy

CARL21_SD.netMx <- cbind(CARL21_SD.netMx, CARL21_SD.clusterCoef, CARL21_SD.degreeCent$centralization,
                         CARL21_SD.netDensity, CARL21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
CARL21_TDg2 <- data.frame(CARL21_TD)
CARL21_TDg2 <- CARL21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_TDg2$player1
player2vector <- CARL21_TDg2$player2
CARL21_TDg3 <- CARL21_TDg2
CARL21_TDg3$p1inp2vec <- is.element(CARL21_TDg3$player1, player2vector)
CARL21_TDg3$p2inp1vec <- is.element(CARL21_TDg3$player2, player1vector)

addPlayer1 <- CARL21_TDg3[ which(CARL21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_TDg3[ which(CARL21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_TDg2 <- rbind(CARL21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
CARL21_TDft <- ftable(CARL21_TDg2$player1, CARL21_TDg2$player2)
CARL21_TDft2 <- as.matrix(CARL21_TDft)
numRows <- nrow(CARL21_TDft2)
numCols <- ncol(CARL21_TDft2)
CARL21_TDft3 <- CARL21_TDft2[c(2:numRows) , c(2:numCols)]
CARL21_TDTable <- graph.adjacency(CARL21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(CARL21_TDTable, vertex.label = V(CARL21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
CARL21_TD.clusterCoef <- transitivity(CARL21_TDTable, type="global") #cluster coefficient
CARL21_TD.degreeCent <- centralization.degree(CARL21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_TDftn <- as.network.matrix(CARL21_TDft)
CARL21_TD.netDensity <- network.density(CARL21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_TD.entropy <- entropy(CARL21_TDft) #entropy

CARL21_TD.netMx <- cbind(CARL21_TD.netMx, CARL21_TD.clusterCoef, CARL21_TD.degreeCent$centralization,
                         CARL21_TD.netDensity, CARL21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************

round = 21
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
CARL21_QTg2 <- data.frame(CARL21_QT)
CARL21_QTg2 <- CARL21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL21_QTg2$player1
player2vector <- CARL21_QTg2$player2
CARL21_QTg3 <- CARL21_QTg2
CARL21_QTg3$p1inp2vec <- is.element(CARL21_QTg3$player1, player2vector)
CARL21_QTg3$p2inp1vec <- is.element(CARL21_QTg3$player2, player1vector)

addPlayer1 <- CARL21_QTg3[ which(CARL21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL21_QTg3[ which(CARL21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL21_QTg2 <- rbind(CARL21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
CARL21_QTft <- ftable(CARL21_QTg2$player1, CARL21_QTg2$player2)
CARL21_QTft2 <- as.matrix(CARL21_QTft)
numRows <- nrow(CARL21_QTft2)
numCols <- ncol(CARL21_QTft2)
CARL21_QTft3 <- CARL21_QTft2[c(2:numRows) , c(2:numCols)]
CARL21_QTTable <- graph.adjacency(CARL21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(CARL21_QTTable, vertex.label = V(CARL21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
CARL21_QT.clusterCoef <- transitivity(CARL21_QTTable, type="global") #cluster coefficient
CARL21_QT.degreeCent <- centralization.degree(CARL21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL21_QTftn <- as.network.matrix(CARL21_QTft)
CARL21_QT.netDensity <- network.density(CARL21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL21_QT.entropy <- entropy(CARL21_QTft) #entropy

CARL21_QT.netMx <- cbind(CARL21_QT.netMx, CARL21_QT.clusterCoef, CARL21_QT.degreeCent$centralization,
                         CARL21_QT.netDensity, CARL21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL21_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "COLL"
KIoutcome = "Goal_F"
COLL21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
COLL21_Gg2 <- data.frame(COLL21_G)
COLL21_Gg2 <- COLL21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_Gg2$player1
player2vector <- COLL21_Gg2$player2
COLL21_Gg3 <- COLL21_Gg2
COLL21_Gg3$p1inp2vec <- is.element(COLL21_Gg3$player1, player2vector)
COLL21_Gg3$p2inp1vec <- is.element(COLL21_Gg3$player2, player1vector)

addPlayer1 <- COLL21_Gg3[ which(COLL21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_Gg3[ which(COLL21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_Gg2 <- rbind(COLL21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
COLL21_Gft <- ftable(COLL21_Gg2$player1, COLL21_Gg2$player2)
COLL21_Gft2 <- as.matrix(COLL21_Gft)
numRows <- nrow(COLL21_Gft2)
numCols <- ncol(COLL21_Gft2)
COLL21_Gft3 <- COLL21_Gft2[c(2:numRows) , c(2:numCols)]
COLL21_GTable <- graph.adjacency(COLL21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(COLL21_GTable, vertex.label = V(COLL21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
COLL21_G.clusterCoef <- transitivity(COLL21_GTable, type="global") #cluster coefficient
COLL21_G.degreeCent <- centralization.degree(COLL21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_Gftn <- as.network.matrix(COLL21_Gft)
COLL21_G.netDensity <- network.density(COLL21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_G.entropy <- entropy(COLL21_Gft) #entropy

COLL21_G.netMx <- cbind(COLL21_G.netMx, COLL21_G.clusterCoef, COLL21_G.degreeCent$centralization,
                        COLL21_G.netDensity, COLL21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "COLL"
KIoutcome = "Behind_F"
COLL21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
COLL21_Bg2 <- data.frame(COLL21_B)
COLL21_Bg2 <- COLL21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_Bg2$player1
player2vector <- COLL21_Bg2$player2
COLL21_Bg3 <- COLL21_Bg2
COLL21_Bg3$p1inp2vec <- is.element(COLL21_Bg3$player1, player2vector)
COLL21_Bg3$p2inp1vec <- is.element(COLL21_Bg3$player2, player1vector)

addPlayer1 <- COLL21_Bg3[ which(COLL21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_Bg3[ which(COLL21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_Bg2 <- rbind(COLL21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
COLL21_Bft <- ftable(COLL21_Bg2$player1, COLL21_Bg2$player2)
COLL21_Bft2 <- as.matrix(COLL21_Bft)
numRows <- nrow(COLL21_Bft2)
numCols <- ncol(COLL21_Bft2)
COLL21_Bft3 <- COLL21_Bft2[c(2:numRows) , c(2:numCols)]
COLL21_BTable <- graph.adjacency(COLL21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(COLL21_BTable, vertex.label = V(COLL21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
COLL21_B.clusterCoef <- transitivity(COLL21_BTable, type="global") #cluster coefficient
COLL21_B.degreeCent <- centralization.degree(COLL21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_Bftn <- as.network.matrix(COLL21_Bft)
COLL21_B.netDensity <- network.density(COLL21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_B.entropy <- entropy(COLL21_Bft) #entropy

COLL21_B.netMx <- cbind(COLL21_B.netMx, COLL21_B.clusterCoef, COLL21_B.degreeCent$centralization,
                        COLL21_B.netDensity, COLL21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
COLL21_SFg2 <- data.frame(COLL21_SF)
COLL21_SFg2 <- COLL21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_SFg2$player1
player2vector <- COLL21_SFg2$player2
COLL21_SFg3 <- COLL21_SFg2
COLL21_SFg3$p1inp2vec <- is.element(COLL21_SFg3$player1, player2vector)
COLL21_SFg3$p2inp1vec <- is.element(COLL21_SFg3$player2, player1vector)

addPlayer1 <- COLL21_SFg3[ which(COLL21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_SFg3[ which(COLL21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_SFg2 <- rbind(COLL21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
COLL21_SFft <- ftable(COLL21_SFg2$player1, COLL21_SFg2$player2)
COLL21_SFft2 <- as.matrix(COLL21_SFft)
numRows <- nrow(COLL21_SFft2)
numCols <- ncol(COLL21_SFft2)
COLL21_SFft3 <- COLL21_SFft2[c(2:numRows) , c(2:numCols)]
COLL21_SFTable <- graph.adjacency(COLL21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(COLL21_SFTable, vertex.label = V(COLL21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
COLL21_SF.clusterCoef <- transitivity(COLL21_SFTable, type="global") #cluster coefficient
COLL21_SF.degreeCent <- centralization.degree(COLL21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_SFftn <- as.network.matrix(COLL21_SFft)
COLL21_SF.netDensity <- network.density(COLL21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_SF.entropy <- entropy(COLL21_SFft) #entropy

COLL21_SF.netMx <- cbind(COLL21_SF.netMx, COLL21_SF.clusterCoef, COLL21_SF.degreeCent$centralization,
                         COLL21_SF.netDensity, COLL21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
COLL21_TFg2 <- data.frame(COLL21_TF)
COLL21_TFg2 <- COLL21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_TFg2$player1
player2vector <- COLL21_TFg2$player2
COLL21_TFg3 <- COLL21_TFg2
COLL21_TFg3$p1inp2vec <- is.element(COLL21_TFg3$player1, player2vector)
COLL21_TFg3$p2inp1vec <- is.element(COLL21_TFg3$player2, player1vector)

addPlayer1 <- COLL21_TFg3[ which(COLL21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_TFg3[ which(COLL21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_TFg2 <- rbind(COLL21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
COLL21_TFft <- ftable(COLL21_TFg2$player1, COLL21_TFg2$player2)
COLL21_TFft2 <- as.matrix(COLL21_TFft)
numRows <- nrow(COLL21_TFft2)
numCols <- ncol(COLL21_TFft2)
COLL21_TFft3 <- COLL21_TFft2[c(2:numRows) , c(2:numCols)]
COLL21_TFTable <- graph.adjacency(COLL21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(COLL21_TFTable, vertex.label = V(COLL21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
COLL21_TF.clusterCoef <- transitivity(COLL21_TFTable, type="global") #cluster coefficient
COLL21_TF.degreeCent <- centralization.degree(COLL21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_TFftn <- as.network.matrix(COLL21_TFft)
COLL21_TF.netDensity <- network.density(COLL21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_TF.entropy <- entropy(COLL21_TFft) #entropy

COLL21_TF.netMx <- cbind(COLL21_TF.netMx, COLL21_TF.clusterCoef, COLL21_TF.degreeCent$centralization,
                         COLL21_TF.netDensity, COLL21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
COLL21_SAMg2 <- data.frame(COLL21_SAM)
COLL21_SAMg2 <- COLL21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_SAMg2$player1
player2vector <- COLL21_SAMg2$player2
COLL21_SAMg3 <- COLL21_SAMg2
COLL21_SAMg3$p1inp2vec <- is.element(COLL21_SAMg3$player1, player2vector)
COLL21_SAMg3$p2inp1vec <- is.element(COLL21_SAMg3$player2, player1vector)

addPlayer1 <- COLL21_SAMg3[ which(COLL21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_SAMg3[ which(COLL21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_SAMg2 <- rbind(COLL21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
COLL21_SAMft <- ftable(COLL21_SAMg2$player1, COLL21_SAMg2$player2)
COLL21_SAMft2 <- as.matrix(COLL21_SAMft)
numRows <- nrow(COLL21_SAMft2)
numCols <- ncol(COLL21_SAMft2)
COLL21_SAMft3 <- COLL21_SAMft2[c(2:numRows) , c(2:numCols)]
COLL21_SAMTable <- graph.adjacency(COLL21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(COLL21_SAMTable, vertex.label = V(COLL21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
COLL21_SAM.clusterCoef <- transitivity(COLL21_SAMTable, type="global") #cluster coefficient
COLL21_SAM.degreeCent <- centralization.degree(COLL21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_SAMftn <- as.network.matrix(COLL21_SAMft)
COLL21_SAM.netDensity <- network.density(COLL21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_SAM.entropy <- entropy(COLL21_SAMft) #entropy

COLL21_SAM.netMx <- cbind(COLL21_SAM.netMx, COLL21_SAM.clusterCoef, COLL21_SAM.degreeCent$centralization,
                          COLL21_SAM.netDensity, COLL21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
COLL21_TAMg2 <- data.frame(COLL21_TAM)
COLL21_TAMg2 <- COLL21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_TAMg2$player1
player2vector <- COLL21_TAMg2$player2
COLL21_TAMg3 <- COLL21_TAMg2
COLL21_TAMg3$p1inp2vec <- is.element(COLL21_TAMg3$player1, player2vector)
COLL21_TAMg3$p2inp1vec <- is.element(COLL21_TAMg3$player2, player1vector)

addPlayer1 <- COLL21_TAMg3[ which(COLL21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_TAMg3[ which(COLL21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_TAMg2 <- rbind(COLL21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
COLL21_TAMft <- ftable(COLL21_TAMg2$player1, COLL21_TAMg2$player2)
COLL21_TAMft2 <- as.matrix(COLL21_TAMft)
numRows <- nrow(COLL21_TAMft2)
numCols <- ncol(COLL21_TAMft2)
COLL21_TAMft3 <- COLL21_TAMft2[c(2:numRows) , c(2:numCols)]
COLL21_TAMTable <- graph.adjacency(COLL21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(COLL21_TAMTable, vertex.label = V(COLL21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
COLL21_TAM.clusterCoef <- transitivity(COLL21_TAMTable, type="global") #cluster coefficient
COLL21_TAM.degreeCent <- centralization.degree(COLL21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_TAMftn <- as.network.matrix(COLL21_TAMft)
COLL21_TAM.netDensity <- network.density(COLL21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_TAM.entropy <- entropy(COLL21_TAMft) #entropy

COLL21_TAM.netMx <- cbind(COLL21_TAM.netMx, COLL21_TAM.clusterCoef, COLL21_TAM.degreeCent$centralization,
                          COLL21_TAM.netDensity, COLL21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
COLL21_SDMg2 <- data.frame(COLL21_SDM)
COLL21_SDMg2 <- COLL21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_SDMg2$player1
player2vector <- COLL21_SDMg2$player2
COLL21_SDMg3 <- COLL21_SDMg2
COLL21_SDMg3$p1inp2vec <- is.element(COLL21_SDMg3$player1, player2vector)
COLL21_SDMg3$p2inp1vec <- is.element(COLL21_SDMg3$player2, player1vector)

addPlayer1 <- COLL21_SDMg3[ which(COLL21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_SDMg3[ which(COLL21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_SDMg2 <- rbind(COLL21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
COLL21_SDMft <- ftable(COLL21_SDMg2$player1, COLL21_SDMg2$player2)
COLL21_SDMft2 <- as.matrix(COLL21_SDMft)
numRows <- nrow(COLL21_SDMft2)
numCols <- ncol(COLL21_SDMft2)
COLL21_SDMft3 <- COLL21_SDMft2[c(2:numRows) , c(2:numCols)]
COLL21_SDMTable <- graph.adjacency(COLL21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(COLL21_SDMTable, vertex.label = V(COLL21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
COLL21_SDM.clusterCoef <- transitivity(COLL21_SDMTable, type="global") #cluster coefficient
COLL21_SDM.degreeCent <- centralization.degree(COLL21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_SDMftn <- as.network.matrix(COLL21_SDMft)
COLL21_SDM.netDensity <- network.density(COLL21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_SDM.entropy <- entropy(COLL21_SDMft) #entropy

COLL21_SDM.netMx <- cbind(COLL21_SDM.netMx, COLL21_SDM.clusterCoef, COLL21_SDM.degreeCent$centralization,
                          COLL21_SDM.netDensity, COLL21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
COLL21_TDMg2 <- data.frame(COLL21_TDM)
COLL21_TDMg2 <- COLL21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_TDMg2$player1
player2vector <- COLL21_TDMg2$player2
COLL21_TDMg3 <- COLL21_TDMg2
COLL21_TDMg3$p1inp2vec <- is.element(COLL21_TDMg3$player1, player2vector)
COLL21_TDMg3$p2inp1vec <- is.element(COLL21_TDMg3$player2, player1vector)

addPlayer1 <- COLL21_TDMg3[ which(COLL21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_TDMg3[ which(COLL21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_TDMg2 <- rbind(COLL21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
COLL21_TDMft <- ftable(COLL21_TDMg2$player1, COLL21_TDMg2$player2)
COLL21_TDMft2 <- as.matrix(COLL21_TDMft)
numRows <- nrow(COLL21_TDMft2)
numCols <- ncol(COLL21_TDMft2)
COLL21_TDMft3 <- COLL21_TDMft2[c(2:numRows) , c(2:numCols)]
COLL21_TDMTable <- graph.adjacency(COLL21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(COLL21_TDMTable, vertex.label = V(COLL21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
COLL21_TDM.clusterCoef <- transitivity(COLL21_TDMTable, type="global") #cluster coefficient
COLL21_TDM.degreeCent <- centralization.degree(COLL21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_TDMftn <- as.network.matrix(COLL21_TDMft)
COLL21_TDM.netDensity <- network.density(COLL21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_TDM.entropy <- entropy(COLL21_TDMft) #entropy

COLL21_TDM.netMx <- cbind(COLL21_TDM.netMx, COLL21_TDM.clusterCoef, COLL21_TDM.degreeCent$centralization,
                          COLL21_TDM.netDensity, COLL21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
COLL21_SDg2 <- data.frame(COLL21_SD)
COLL21_SDg2 <- COLL21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_SDg2$player1
player2vector <- COLL21_SDg2$player2
COLL21_SDg3 <- COLL21_SDg2
COLL21_SDg3$p1inp2vec <- is.element(COLL21_SDg3$player1, player2vector)
COLL21_SDg3$p2inp1vec <- is.element(COLL21_SDg3$player2, player1vector)

addPlayer1 <- COLL21_SDg3[ which(COLL21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_SDg3[ which(COLL21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_SDg2 <- rbind(COLL21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
COLL21_SDft <- ftable(COLL21_SDg2$player1, COLL21_SDg2$player2)
COLL21_SDft2 <- as.matrix(COLL21_SDft)
numRows <- nrow(COLL21_SDft2)
numCols <- ncol(COLL21_SDft2)
COLL21_SDft3 <- COLL21_SDft2[c(2:numRows) , c(2:numCols)]
COLL21_SDTable <- graph.adjacency(COLL21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(COLL21_SDTable, vertex.label = V(COLL21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
COLL21_SD.clusterCoef <- transitivity(COLL21_SDTable, type="global") #cluster coefficient
COLL21_SD.degreeCent <- centralization.degree(COLL21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_SDftn <- as.network.matrix(COLL21_SDft)
COLL21_SD.netDensity <- network.density(COLL21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_SD.entropy <- entropy(COLL21_SDft) #entropy

COLL21_SD.netMx <- cbind(COLL21_SD.netMx, COLL21_SD.clusterCoef, COLL21_SD.degreeCent$centralization,
                         COLL21_SD.netDensity, COLL21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
COLL21_TDg2 <- data.frame(COLL21_TD)
COLL21_TDg2 <- COLL21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_TDg2$player1
player2vector <- COLL21_TDg2$player2
COLL21_TDg3 <- COLL21_TDg2
COLL21_TDg3$p1inp2vec <- is.element(COLL21_TDg3$player1, player2vector)
COLL21_TDg3$p2inp1vec <- is.element(COLL21_TDg3$player2, player1vector)

addPlayer1 <- COLL21_TDg3[ which(COLL21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_TDg3[ which(COLL21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_TDg2 <- rbind(COLL21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
COLL21_TDft <- ftable(COLL21_TDg2$player1, COLL21_TDg2$player2)
COLL21_TDft2 <- as.matrix(COLL21_TDft)
numRows <- nrow(COLL21_TDft2)
numCols <- ncol(COLL21_TDft2)
COLL21_TDft3 <- COLL21_TDft2[c(2:numRows) , c(2:numCols)]
COLL21_TDTable <- graph.adjacency(COLL21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(COLL21_TDTable, vertex.label = V(COLL21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
COLL21_TD.clusterCoef <- transitivity(COLL21_TDTable, type="global") #cluster coefficient
COLL21_TD.degreeCent <- centralization.degree(COLL21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_TDftn <- as.network.matrix(COLL21_TDft)
COLL21_TD.netDensity <- network.density(COLL21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_TD.entropy <- entropy(COLL21_TDft) #entropy

COLL21_TD.netMx <- cbind(COLL21_TD.netMx, COLL21_TD.clusterCoef, COLL21_TD.degreeCent$centralization,
                         COLL21_TD.netDensity, COLL21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
COLL21_QTg2 <- data.frame(COLL21_QT)
COLL21_QTg2 <- COLL21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL21_QTg2$player1
player2vector <- COLL21_QTg2$player2
COLL21_QTg3 <- COLL21_QTg2
COLL21_QTg3$p1inp2vec <- is.element(COLL21_QTg3$player1, player2vector)
COLL21_QTg3$p2inp1vec <- is.element(COLL21_QTg3$player2, player1vector)

addPlayer1 <- COLL21_QTg3[ which(COLL21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL21_QTg3[ which(COLL21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL21_QTg2 <- rbind(COLL21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
COLL21_QTft <- ftable(COLL21_QTg2$player1, COLL21_QTg2$player2)
COLL21_QTft2 <- as.matrix(COLL21_QTft)
numRows <- nrow(COLL21_QTft2)
numCols <- ncol(COLL21_QTft2)
COLL21_QTft3 <- COLL21_QTft2[c(2:numRows) , c(2:numCols)]
COLL21_QTTable <- graph.adjacency(COLL21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(COLL21_QTTable, vertex.label = V(COLL21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
COLL21_QT.clusterCoef <- transitivity(COLL21_QTTable, type="global") #cluster coefficient
COLL21_QT.degreeCent <- centralization.degree(COLL21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL21_QTftn <- as.network.matrix(COLL21_QTft)
COLL21_QT.netDensity <- network.density(COLL21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL21_QT.entropy <- entropy(COLL21_QTft) #entropy

COLL21_QT.netMx <- cbind(COLL21_QT.netMx, COLL21_QT.clusterCoef, COLL21_QT.degreeCent$centralization,
                         COLL21_QT.netDensity, COLL21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL21_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "ESS"
KIoutcome = "Goal_F"
ESS21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
ESS21_Gg2 <- data.frame(ESS21_G)
ESS21_Gg2 <- ESS21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_Gg2$player1
player2vector <- ESS21_Gg2$player2
ESS21_Gg3 <- ESS21_Gg2
ESS21_Gg3$p1inp2vec <- is.element(ESS21_Gg3$player1, player2vector)
ESS21_Gg3$p2inp1vec <- is.element(ESS21_Gg3$player2, player1vector)

addPlayer1 <- ESS21_Gg3[ which(ESS21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_Gg3[ which(ESS21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_Gg2 <- rbind(ESS21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
ESS21_Gft <- ftable(ESS21_Gg2$player1, ESS21_Gg2$player2)
ESS21_Gft2 <- as.matrix(ESS21_Gft)
numRows <- nrow(ESS21_Gft2)
numCols <- ncol(ESS21_Gft2)
ESS21_Gft3 <- ESS21_Gft2[c(2:numRows) , c(2:numCols)]
ESS21_GTable <- graph.adjacency(ESS21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(ESS21_GTable, vertex.label = V(ESS21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
ESS21_G.clusterCoef <- transitivity(ESS21_GTable, type="global") #cluster coefficient
ESS21_G.degreeCent <- centralization.degree(ESS21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_Gftn <- as.network.matrix(ESS21_Gft)
ESS21_G.netDensity <- network.density(ESS21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_G.entropy <- entropy(ESS21_Gft) #entropy

ESS21_G.netMx <- cbind(ESS21_G.netMx, ESS21_G.clusterCoef, ESS21_G.degreeCent$centralization,
                       ESS21_G.netDensity, ESS21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "ESS"
KIoutcome = "Behind_F"
ESS21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
ESS21_Bg2 <- data.frame(ESS21_B)
ESS21_Bg2 <- ESS21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_Bg2$player1
player2vector <- ESS21_Bg2$player2
ESS21_Bg3 <- ESS21_Bg2
ESS21_Bg3$p1inp2vec <- is.element(ESS21_Bg3$player1, player2vector)
ESS21_Bg3$p2inp1vec <- is.element(ESS21_Bg3$player2, player1vector)

addPlayer1 <- ESS21_Bg3[ which(ESS21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_Bg3[ which(ESS21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_Bg2 <- rbind(ESS21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
ESS21_Bft <- ftable(ESS21_Bg2$player1, ESS21_Bg2$player2)
ESS21_Bft2 <- as.matrix(ESS21_Bft)
numRows <- nrow(ESS21_Bft2)
numCols <- ncol(ESS21_Bft2)
ESS21_Bft3 <- ESS21_Bft2[c(2:numRows) , c(2:numCols)]
ESS21_BTable <- graph.adjacency(ESS21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(ESS21_BTable, vertex.label = V(ESS21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
ESS21_B.clusterCoef <- transitivity(ESS21_BTable, type="global") #cluster coefficient
ESS21_B.degreeCent <- centralization.degree(ESS21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_Bftn <- as.network.matrix(ESS21_Bft)
ESS21_B.netDensity <- network.density(ESS21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_B.entropy <- entropy(ESS21_Bft) #entropy

ESS21_B.netMx <- cbind(ESS21_B.netMx, ESS21_B.clusterCoef, ESS21_B.degreeCent$centralization,
                       ESS21_B.netDensity, ESS21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
ESS21_SFg2 <- data.frame(ESS21_SF)
ESS21_SFg2 <- ESS21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_SFg2$player1
player2vector <- ESS21_SFg2$player2
ESS21_SFg3 <- ESS21_SFg2
ESS21_SFg3$p1inp2vec <- is.element(ESS21_SFg3$player1, player2vector)
ESS21_SFg3$p2inp1vec <- is.element(ESS21_SFg3$player2, player1vector)

addPlayer1 <- ESS21_SFg3[ which(ESS21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_SFg3[ which(ESS21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_SFg2 <- rbind(ESS21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
ESS21_SFft <- ftable(ESS21_SFg2$player1, ESS21_SFg2$player2)
ESS21_SFft2 <- as.matrix(ESS21_SFft)
numRows <- nrow(ESS21_SFft2)
numCols <- ncol(ESS21_SFft2)
ESS21_SFft3 <- ESS21_SFft2[c(2:numRows) , c(2:numCols)]
ESS21_SFTable <- graph.adjacency(ESS21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(ESS21_SFTable, vertex.label = V(ESS21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
ESS21_SF.clusterCoef <- transitivity(ESS21_SFTable, type="global") #cluster coefficient
ESS21_SF.degreeCent <- centralization.degree(ESS21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_SFftn <- as.network.matrix(ESS21_SFft)
ESS21_SF.netDensity <- network.density(ESS21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_SF.entropy <- entropy(ESS21_SFft) #entropy

ESS21_SF.netMx <- cbind(ESS21_SF.netMx, ESS21_SF.clusterCoef, ESS21_SF.degreeCent$centralization,
                        ESS21_SF.netDensity, ESS21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
ESS21_TFg2 <- data.frame(ESS21_TF)
ESS21_TFg2 <- ESS21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_TFg2$player1
player2vector <- ESS21_TFg2$player2
ESS21_TFg3 <- ESS21_TFg2
ESS21_TFg3$p1inp2vec <- is.element(ESS21_TFg3$player1, player2vector)
ESS21_TFg3$p2inp1vec <- is.element(ESS21_TFg3$player2, player1vector)

addPlayer1 <- ESS21_TFg3[ which(ESS21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS21_TFg3[ which(ESS21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_TFg2 <- rbind(ESS21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
ESS21_TFft <- ftable(ESS21_TFg2$player1, ESS21_TFg2$player2)
ESS21_TFft2 <- as.matrix(ESS21_TFft)
numRows <- nrow(ESS21_TFft2)
numCols <- ncol(ESS21_TFft2)
ESS21_TFft3 <- ESS21_TFft2[c(2:numRows) , c(2:numCols)]
ESS21_TFTable <- graph.adjacency(ESS21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(ESS21_TFTable, vertex.label = V(ESS21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
ESS21_TF.clusterCoef <- transitivity(ESS21_TFTable, type="global") #cluster coefficient
ESS21_TF.degreeCent <- centralization.degree(ESS21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_TFftn <- as.network.matrix(ESS21_TFft)
ESS21_TF.netDensity <- network.density(ESS21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_TF.entropy <- entropy(ESS21_TFft) #entropy

ESS21_TF.netMx <- cbind(ESS21_TF.netMx, ESS21_TF.clusterCoef, ESS21_TF.degreeCent$centralization,
                        ESS21_TF.netDensity, ESS21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
ESS21_SAMg2 <- data.frame(ESS21_SAM)
ESS21_SAMg2 <- ESS21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_SAMg2$player1
player2vector <- ESS21_SAMg2$player2
ESS21_SAMg3 <- ESS21_SAMg2
ESS21_SAMg3$p1inp2vec <- is.element(ESS21_SAMg3$player1, player2vector)
ESS21_SAMg3$p2inp1vec <- is.element(ESS21_SAMg3$player2, player1vector)

addPlayer1 <- ESS21_SAMg3[ which(ESS21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_SAMg3[ which(ESS21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_SAMg2 <- rbind(ESS21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
ESS21_SAMft <- ftable(ESS21_SAMg2$player1, ESS21_SAMg2$player2)
ESS21_SAMft2 <- as.matrix(ESS21_SAMft)
numRows <- nrow(ESS21_SAMft2)
numCols <- ncol(ESS21_SAMft2)
ESS21_SAMft3 <- ESS21_SAMft2[c(2:numRows) , c(2:numCols)]
ESS21_SAMTable <- graph.adjacency(ESS21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(ESS21_SAMTable, vertex.label = V(ESS21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
ESS21_SAM.clusterCoef <- transitivity(ESS21_SAMTable, type="global") #cluster coefficient
ESS21_SAM.degreeCent <- centralization.degree(ESS21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_SAMftn <- as.network.matrix(ESS21_SAMft)
ESS21_SAM.netDensity <- network.density(ESS21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_SAM.entropy <- entropy(ESS21_SAMft) #entropy

ESS21_SAM.netMx <- cbind(ESS21_SAM.netMx, ESS21_SAM.clusterCoef, ESS21_SAM.degreeCent$centralization,
                         ESS21_SAM.netDensity, ESS21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
ESS21_TAMg2 <- data.frame(ESS21_TAM)
ESS21_TAMg2 <- ESS21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_TAMg2$player1
player2vector <- ESS21_TAMg2$player2
ESS21_TAMg3 <- ESS21_TAMg2
ESS21_TAMg3$p1inp2vec <- is.element(ESS21_TAMg3$player1, player2vector)
ESS21_TAMg3$p2inp1vec <- is.element(ESS21_TAMg3$player2, player1vector)

addPlayer1 <- ESS21_TAMg3[ which(ESS21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_TAMg3[ which(ESS21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_TAMg2 <- rbind(ESS21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
ESS21_TAMft <- ftable(ESS21_TAMg2$player1, ESS21_TAMg2$player2)
ESS21_TAMft2 <- as.matrix(ESS21_TAMft)
numRows <- nrow(ESS21_TAMft2)
numCols <- ncol(ESS21_TAMft2)
ESS21_TAMft3 <- ESS21_TAMft2[c(2:numRows) , c(2:numCols)]
ESS21_TAMTable <- graph.adjacency(ESS21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(ESS21_TAMTable, vertex.label = V(ESS21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
ESS21_TAM.clusterCoef <- transitivity(ESS21_TAMTable, type="global") #cluster coefficient
ESS21_TAM.degreeCent <- centralization.degree(ESS21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_TAMftn <- as.network.matrix(ESS21_TAMft)
ESS21_TAM.netDensity <- network.density(ESS21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_TAM.entropy <- entropy(ESS21_TAMft) #entropy

ESS21_TAM.netMx <- cbind(ESS21_TAM.netMx, ESS21_TAM.clusterCoef, ESS21_TAM.degreeCent$centralization,
                         ESS21_TAM.netDensity, ESS21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
ESS21_SDMg2 <- data.frame(ESS21_SDM)
ESS21_SDMg2 <- ESS21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_SDMg2$player1
player2vector <- ESS21_SDMg2$player2
ESS21_SDMg3 <- ESS21_SDMg2
ESS21_SDMg3$p1inp2vec <- is.element(ESS21_SDMg3$player1, player2vector)
ESS21_SDMg3$p2inp1vec <- is.element(ESS21_SDMg3$player2, player1vector)

addPlayer1 <- ESS21_SDMg3[ which(ESS21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_SDMg3[ which(ESS21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_SDMg2 <- rbind(ESS21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
ESS21_SDMft <- ftable(ESS21_SDMg2$player1, ESS21_SDMg2$player2)
ESS21_SDMft2 <- as.matrix(ESS21_SDMft)
numRows <- nrow(ESS21_SDMft2)
numCols <- ncol(ESS21_SDMft2)
ESS21_SDMft3 <- ESS21_SDMft2[c(2:numRows) , c(2:numCols)]
ESS21_SDMTable <- graph.adjacency(ESS21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(ESS21_SDMTable, vertex.label = V(ESS21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
ESS21_SDM.clusterCoef <- transitivity(ESS21_SDMTable, type="global") #cluster coefficient
ESS21_SDM.degreeCent <- centralization.degree(ESS21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_SDMftn <- as.network.matrix(ESS21_SDMft)
ESS21_SDM.netDensity <- network.density(ESS21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_SDM.entropy <- entropy(ESS21_SDMft) #entropy

ESS21_SDM.netMx <- cbind(ESS21_SDM.netMx, ESS21_SDM.clusterCoef, ESS21_SDM.degreeCent$centralization,
                         ESS21_SDM.netDensity, ESS21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
ESS21_TDMg2 <- data.frame(ESS21_TDM)
ESS21_TDMg2 <- ESS21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_TDMg2$player1
player2vector <- ESS21_TDMg2$player2
ESS21_TDMg3 <- ESS21_TDMg2
ESS21_TDMg3$p1inp2vec <- is.element(ESS21_TDMg3$player1, player2vector)
ESS21_TDMg3$p2inp1vec <- is.element(ESS21_TDMg3$player2, player1vector)

addPlayer1 <- ESS21_TDMg3[ which(ESS21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_TDMg3[ which(ESS21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_TDMg2 <- rbind(ESS21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
ESS21_TDMft <- ftable(ESS21_TDMg2$player1, ESS21_TDMg2$player2)
ESS21_TDMft2 <- as.matrix(ESS21_TDMft)
numRows <- nrow(ESS21_TDMft2)
numCols <- ncol(ESS21_TDMft2)
ESS21_TDMft3 <- ESS21_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS21_TDMTable <- graph.adjacency(ESS21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(ESS21_TDMTable, vertex.label = V(ESS21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
ESS21_TDM.clusterCoef <- transitivity(ESS21_TDMTable, type="global") #cluster coefficient
ESS21_TDM.degreeCent <- centralization.degree(ESS21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_TDMftn <- as.network.matrix(ESS21_TDMft)
ESS21_TDM.netDensity <- network.density(ESS21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_TDM.entropy <- entropy(ESS21_TDMft) #entropy

ESS21_TDM.netMx <- cbind(ESS21_TDM.netMx, ESS21_TDM.clusterCoef, ESS21_TDM.degreeCent$centralization,
                         ESS21_TDM.netDensity, ESS21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
ESS21_SDg2 <- data.frame(ESS21_SD)
ESS21_SDg2 <- ESS21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_SDg2$player1
player2vector <- ESS21_SDg2$player2
ESS21_SDg3 <- ESS21_SDg2
ESS21_SDg3$p1inp2vec <- is.element(ESS21_SDg3$player1, player2vector)
ESS21_SDg3$p2inp1vec <- is.element(ESS21_SDg3$player2, player1vector)

addPlayer1 <- ESS21_SDg3[ which(ESS21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_SDg3[ which(ESS21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_SDg2 <- rbind(ESS21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
ESS21_SDft <- ftable(ESS21_SDg2$player1, ESS21_SDg2$player2)
ESS21_SDft2 <- as.matrix(ESS21_SDft)
numRows <- nrow(ESS21_SDft2)
numCols <- ncol(ESS21_SDft2)
ESS21_SDft3 <- ESS21_SDft2[c(2:numRows) , c(2:numCols)]
ESS21_SDTable <- graph.adjacency(ESS21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(ESS21_SDTable, vertex.label = V(ESS21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
ESS21_SD.clusterCoef <- transitivity(ESS21_SDTable, type="global") #cluster coefficient
ESS21_SD.degreeCent <- centralization.degree(ESS21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_SDftn <- as.network.matrix(ESS21_SDft)
ESS21_SD.netDensity <- network.density(ESS21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_SD.entropy <- entropy(ESS21_SDft) #entropy

ESS21_SD.netMx <- cbind(ESS21_SD.netMx, ESS21_SD.clusterCoef, ESS21_SD.degreeCent$centralization,
                        ESS21_SD.netDensity, ESS21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
ESS21_TDg2 <- data.frame(ESS21_TD)
ESS21_TDg2 <- ESS21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_TDg2$player1
player2vector <- ESS21_TDg2$player2
ESS21_TDg3 <- ESS21_TDg2
ESS21_TDg3$p1inp2vec <- is.element(ESS21_TDg3$player1, player2vector)
ESS21_TDg3$p2inp1vec <- is.element(ESS21_TDg3$player2, player1vector)

addPlayer1 <- ESS21_TDg3[ which(ESS21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_TDg3[ which(ESS21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_TDg2 <- rbind(ESS21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
ESS21_TDft <- ftable(ESS21_TDg2$player1, ESS21_TDg2$player2)
ESS21_TDft2 <- as.matrix(ESS21_TDft)
numRows <- nrow(ESS21_TDft2)
numCols <- ncol(ESS21_TDft2)
ESS21_TDft3 <- ESS21_TDft2[c(2:numRows) , c(2:numCols)]
ESS21_TDTable <- graph.adjacency(ESS21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(ESS21_TDTable, vertex.label = V(ESS21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
ESS21_TD.clusterCoef <- transitivity(ESS21_TDTable, type="global") #cluster coefficient
ESS21_TD.degreeCent <- centralization.degree(ESS21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_TDftn <- as.network.matrix(ESS21_TDft)
ESS21_TD.netDensity <- network.density(ESS21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_TD.entropy <- entropy(ESS21_TDft) #entropy

ESS21_TD.netMx <- cbind(ESS21_TD.netMx, ESS21_TD.clusterCoef, ESS21_TD.degreeCent$centralization,
                        ESS21_TD.netDensity, ESS21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
ESS21_QTg2 <- data.frame(ESS21_QT)
ESS21_QTg2 <- ESS21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS21_QTg2$player1
player2vector <- ESS21_QTg2$player2
ESS21_QTg3 <- ESS21_QTg2
ESS21_QTg3$p1inp2vec <- is.element(ESS21_QTg3$player1, player2vector)
ESS21_QTg3$p2inp1vec <- is.element(ESS21_QTg3$player2, player1vector)

addPlayer1 <- ESS21_QTg3[ which(ESS21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS21_QTg3[ which(ESS21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS21_QTg2 <- rbind(ESS21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
ESS21_QTft <- ftable(ESS21_QTg2$player1, ESS21_QTg2$player2)
ESS21_QTft2 <- as.matrix(ESS21_QTft)
numRows <- nrow(ESS21_QTft2)
numCols <- ncol(ESS21_QTft2)
ESS21_QTft3 <- ESS21_QTft2[c(2:numRows) , c(2:numCols)]
ESS21_QTTable <- graph.adjacency(ESS21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(ESS21_QTTable, vertex.label = V(ESS21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
ESS21_QT.clusterCoef <- transitivity(ESS21_QTTable, type="global") #cluster coefficient
ESS21_QT.degreeCent <- centralization.degree(ESS21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS21_QTftn <- as.network.matrix(ESS21_QTft)
ESS21_QT.netDensity <- network.density(ESS21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS21_QT.entropy <- entropy(ESS21_QTft) #entropy

ESS21_QT.netMx <- cbind(ESS21_QT.netMx, ESS21_QT.clusterCoef, ESS21_QT.degreeCent$centralization,
                        ESS21_QT.netDensity, ESS21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS21_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "FRE"
KIoutcome = "Goal_F"
FRE21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
FRE21_Gg2 <- data.frame(FRE21_G)
FRE21_Gg2 <- FRE21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_Gg2$player1
player2vector <- FRE21_Gg2$player2
FRE21_Gg3 <- FRE21_Gg2
FRE21_Gg3$p1inp2vec <- is.element(FRE21_Gg3$player1, player2vector)
FRE21_Gg3$p2inp1vec <- is.element(FRE21_Gg3$player2, player1vector)

addPlayer1 <- FRE21_Gg3[ which(FRE21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_Gg3[ which(FRE21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_Gg2 <- rbind(FRE21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
FRE21_Gft <- ftable(FRE21_Gg2$player1, FRE21_Gg2$player2)
FRE21_Gft2 <- as.matrix(FRE21_Gft)
numRows <- nrow(FRE21_Gft2)
numCols <- ncol(FRE21_Gft2)
FRE21_Gft3 <- FRE21_Gft2[c(2:numRows) , c(2:numCols)]
FRE21_GTable <- graph.adjacency(FRE21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(FRE21_GTable, vertex.label = V(FRE21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
FRE21_G.clusterCoef <- transitivity(FRE21_GTable, type="global") #cluster coefficient
FRE21_G.degreeCent <- centralization.degree(FRE21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_Gftn <- as.network.matrix(FRE21_Gft)
FRE21_G.netDensity <- network.density(FRE21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_G.entropy <- entropy(FRE21_Gft) #entropy

FRE21_G.netMx <- cbind(FRE21_G.netMx, FRE21_G.clusterCoef, FRE21_G.degreeCent$centralization,
                       FRE21_G.netDensity, FRE21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Behind_F"
FRE21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
FRE21_Bg2 <- data.frame(FRE21_B)
FRE21_Bg2 <- FRE21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_Bg2$player1
player2vector <- FRE21_Bg2$player2
FRE21_Bg3 <- FRE21_Bg2
FRE21_Bg3$p1inp2vec <- is.element(FRE21_Bg3$player1, player2vector)
FRE21_Bg3$p2inp1vec <- is.element(FRE21_Bg3$player2, player1vector)

addPlayer1 <- FRE21_Bg3[ which(FRE21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_Bg3[ which(FRE21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_Bg2 <- rbind(FRE21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
FRE21_Bft <- ftable(FRE21_Bg2$player1, FRE21_Bg2$player2)
FRE21_Bft2 <- as.matrix(FRE21_Bft)
numRows <- nrow(FRE21_Bft2)
numCols <- ncol(FRE21_Bft2)
FRE21_Bft3 <- FRE21_Bft2[c(2:numRows) , c(2:numCols)]
FRE21_BTable <- graph.adjacency(FRE21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(FRE21_BTable, vertex.label = V(FRE21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
FRE21_B.clusterCoef <- transitivity(FRE21_BTable, type="global") #cluster coefficient
FRE21_B.degreeCent <- centralization.degree(FRE21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_Bftn <- as.network.matrix(FRE21_Bft)
FRE21_B.netDensity <- network.density(FRE21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_B.entropy <- entropy(FRE21_Bft) #entropy

FRE21_B.netMx <- cbind(FRE21_B.netMx, FRE21_B.clusterCoef, FRE21_B.degreeCent$centralization,
                       FRE21_B.netDensity, FRE21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
FRE21_SFg2 <- data.frame(FRE21_SF)
FRE21_SFg2 <- FRE21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_SFg2$player1
player2vector <- FRE21_SFg2$player2
FRE21_SFg3 <- FRE21_SFg2
FRE21_SFg3$p1inp2vec <- is.element(FRE21_SFg3$player1, player2vector)
FRE21_SFg3$p2inp1vec <- is.element(FRE21_SFg3$player2, player1vector)

addPlayer1 <- FRE21_SFg3[ which(FRE21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_SFg3[ which(FRE21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_SFg2 <- rbind(FRE21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
FRE21_SFft <- ftable(FRE21_SFg2$player1, FRE21_SFg2$player2)
FRE21_SFft2 <- as.matrix(FRE21_SFft)
numRows <- nrow(FRE21_SFft2)
numCols <- ncol(FRE21_SFft2)
FRE21_SFft3 <- FRE21_SFft2[c(2:numRows) , c(2:numCols)]
FRE21_SFTable <- graph.adjacency(FRE21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(FRE21_SFTable, vertex.label = V(FRE21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
FRE21_SF.clusterCoef <- transitivity(FRE21_SFTable, type="global") #cluster coefficient
FRE21_SF.degreeCent <- centralization.degree(FRE21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_SFftn <- as.network.matrix(FRE21_SFft)
FRE21_SF.netDensity <- network.density(FRE21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_SF.entropy <- entropy(FRE21_SFft) #entropy

FRE21_SF.netMx <- cbind(FRE21_SF.netMx, FRE21_SF.clusterCoef, FRE21_SF.degreeCent$centralization,
                        FRE21_SF.netDensity, FRE21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
FRE21_TFg2 <- data.frame(FRE21_TF)
FRE21_TFg2 <- FRE21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_TFg2$player1
player2vector <- FRE21_TFg2$player2
FRE21_TFg3 <- FRE21_TFg2
FRE21_TFg3$p1inp2vec <- is.element(FRE21_TFg3$player1, player2vector)
FRE21_TFg3$p2inp1vec <- is.element(FRE21_TFg3$player2, player1vector)

addPlayer1 <- FRE21_TFg3[ which(FRE21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_TFg3[ which(FRE21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_TFg2 <- rbind(FRE21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
FRE21_TFft <- ftable(FRE21_TFg2$player1, FRE21_TFg2$player2)
FRE21_TFft2 <- as.matrix(FRE21_TFft)
numRows <- nrow(FRE21_TFft2)
numCols <- ncol(FRE21_TFft2)
FRE21_TFft3 <- FRE21_TFft2[c(2:numRows) , c(2:numCols)]
FRE21_TFTable <- graph.adjacency(FRE21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(FRE21_TFTable, vertex.label = V(FRE21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
FRE21_TF.clusterCoef <- transitivity(FRE21_TFTable, type="global") #cluster coefficient
FRE21_TF.degreeCent <- centralization.degree(FRE21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_TFftn <- as.network.matrix(FRE21_TFft)
FRE21_TF.netDensity <- network.density(FRE21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_TF.entropy <- entropy(FRE21_TFft) #entropy

FRE21_TF.netMx <- cbind(FRE21_TF.netMx, FRE21_TF.clusterCoef, FRE21_TF.degreeCent$centralization,
                        FRE21_TF.netDensity, FRE21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
FRE21_SAMg2 <- data.frame(FRE21_SAM)
FRE21_SAMg2 <- FRE21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_SAMg2$player1
player2vector <- FRE21_SAMg2$player2
FRE21_SAMg3 <- FRE21_SAMg2
FRE21_SAMg3$p1inp2vec <- is.element(FRE21_SAMg3$player1, player2vector)
FRE21_SAMg3$p2inp1vec <- is.element(FRE21_SAMg3$player2, player1vector)

addPlayer1 <- FRE21_SAMg3[ which(FRE21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_SAMg3[ which(FRE21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_SAMg2 <- rbind(FRE21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
FRE21_SAMft <- ftable(FRE21_SAMg2$player1, FRE21_SAMg2$player2)
FRE21_SAMft2 <- as.matrix(FRE21_SAMft)
numRows <- nrow(FRE21_SAMft2)
numCols <- ncol(FRE21_SAMft2)
FRE21_SAMft3 <- FRE21_SAMft2[c(2:numRows) , c(2:numCols)]
FRE21_SAMTable <- graph.adjacency(FRE21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(FRE21_SAMTable, vertex.label = V(FRE21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
FRE21_SAM.clusterCoef <- transitivity(FRE21_SAMTable, type="global") #cluster coefficient
FRE21_SAM.degreeCent <- centralization.degree(FRE21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_SAMftn <- as.network.matrix(FRE21_SAMft)
FRE21_SAM.netDensity <- network.density(FRE21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_SAM.entropy <- entropy(FRE21_SAMft) #entropy

FRE21_SAM.netMx <- cbind(FRE21_SAM.netMx, FRE21_SAM.clusterCoef, FRE21_SAM.degreeCent$centralization,
                         FRE21_SAM.netDensity, FRE21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
FRE21_TAMg2 <- data.frame(FRE21_TAM)
FRE21_TAMg2 <- FRE21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_TAMg2$player1
player2vector <- FRE21_TAMg2$player2
FRE21_TAMg3 <- FRE21_TAMg2
FRE21_TAMg3$p1inp2vec <- is.element(FRE21_TAMg3$player1, player2vector)
FRE21_TAMg3$p2inp1vec <- is.element(FRE21_TAMg3$player2, player1vector)

addPlayer1 <- FRE21_TAMg3[ which(FRE21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_TAMg3[ which(FRE21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_TAMg2 <- rbind(FRE21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
FRE21_TAMft <- ftable(FRE21_TAMg2$player1, FRE21_TAMg2$player2)
FRE21_TAMft2 <- as.matrix(FRE21_TAMft)
numRows <- nrow(FRE21_TAMft2)
numCols <- ncol(FRE21_TAMft2)
FRE21_TAMft3 <- FRE21_TAMft2[c(2:numRows) , c(2:numCols)]
FRE21_TAMTable <- graph.adjacency(FRE21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(FRE21_TAMTable, vertex.label = V(FRE21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
FRE21_TAM.clusterCoef <- transitivity(FRE21_TAMTable, type="global") #cluster coefficient
FRE21_TAM.degreeCent <- centralization.degree(FRE21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_TAMftn <- as.network.matrix(FRE21_TAMft)
FRE21_TAM.netDensity <- network.density(FRE21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_TAM.entropy <- entropy(FRE21_TAMft) #entropy

FRE21_TAM.netMx <- cbind(FRE21_TAM.netMx, FRE21_TAM.clusterCoef, FRE21_TAM.degreeCent$centralization,
                         FRE21_TAM.netDensity, FRE21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
FRE21_SDMg2 <- data.frame(FRE21_SDM)
FRE21_SDMg2 <- FRE21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_SDMg2$player1
player2vector <- FRE21_SDMg2$player2
FRE21_SDMg3 <- FRE21_SDMg2
FRE21_SDMg3$p1inp2vec <- is.element(FRE21_SDMg3$player1, player2vector)
FRE21_SDMg3$p2inp1vec <- is.element(FRE21_SDMg3$player2, player1vector)

addPlayer1 <- FRE21_SDMg3[ which(FRE21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_SDMg3[ which(FRE21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_SDMg2 <- rbind(FRE21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
FRE21_SDMft <- ftable(FRE21_SDMg2$player1, FRE21_SDMg2$player2)
FRE21_SDMft2 <- as.matrix(FRE21_SDMft)
numRows <- nrow(FRE21_SDMft2)
numCols <- ncol(FRE21_SDMft2)
FRE21_SDMft3 <- FRE21_SDMft2[c(2:numRows) , c(2:numCols)]
FRE21_SDMTable <- graph.adjacency(FRE21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(FRE21_SDMTable, vertex.label = V(FRE21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
FRE21_SDM.clusterCoef <- transitivity(FRE21_SDMTable, type="global") #cluster coefficient
FRE21_SDM.degreeCent <- centralization.degree(FRE21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_SDMftn <- as.network.matrix(FRE21_SDMft)
FRE21_SDM.netDensity <- network.density(FRE21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_SDM.entropy <- entropy(FRE21_SDMft) #entropy

FRE21_SDM.netMx <- cbind(FRE21_SDM.netMx, FRE21_SDM.clusterCoef, FRE21_SDM.degreeCent$centralization,
                         FRE21_SDM.netDensity, FRE21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
FRE21_TDMg2 <- data.frame(FRE21_TDM)
FRE21_TDMg2 <- FRE21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_TDMg2$player1
player2vector <- FRE21_TDMg2$player2
FRE21_TDMg3 <- FRE21_TDMg2
FRE21_TDMg3$p1inp2vec <- is.element(FRE21_TDMg3$player1, player2vector)
FRE21_TDMg3$p2inp1vec <- is.element(FRE21_TDMg3$player2, player1vector)

addPlayer1 <- FRE21_TDMg3[ which(FRE21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_TDMg3[ which(FRE21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_TDMg2 <- rbind(FRE21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
FRE21_TDMft <- ftable(FRE21_TDMg2$player1, FRE21_TDMg2$player2)
FRE21_TDMft2 <- as.matrix(FRE21_TDMft)
numRows <- nrow(FRE21_TDMft2)
numCols <- ncol(FRE21_TDMft2)
FRE21_TDMft3 <- FRE21_TDMft2[c(2:numRows) , c(2:numCols)]
FRE21_TDMTable <- graph.adjacency(FRE21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(FRE21_TDMTable, vertex.label = V(FRE21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
FRE21_TDM.clusterCoef <- transitivity(FRE21_TDMTable, type="global") #cluster coefficient
FRE21_TDM.degreeCent <- centralization.degree(FRE21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_TDMftn <- as.network.matrix(FRE21_TDMft)
FRE21_TDM.netDensity <- network.density(FRE21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_TDM.entropy <- entropy(FRE21_TDMft) #entropy

FRE21_TDM.netMx <- cbind(FRE21_TDM.netMx, FRE21_TDM.clusterCoef, FRE21_TDM.degreeCent$centralization,
                         FRE21_TDM.netDensity, FRE21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
FRE21_SDg2 <- data.frame(FRE21_SD)
FRE21_SDg2 <- FRE21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_SDg2$player1
player2vector <- FRE21_SDg2$player2
FRE21_SDg3 <- FRE21_SDg2
FRE21_SDg3$p1inp2vec <- is.element(FRE21_SDg3$player1, player2vector)
FRE21_SDg3$p2inp1vec <- is.element(FRE21_SDg3$player2, player1vector)

addPlayer1 <- FRE21_SDg3[ which(FRE21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_SDg3[ which(FRE21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_SDg2 <- rbind(FRE21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
FRE21_SDft <- ftable(FRE21_SDg2$player1, FRE21_SDg2$player2)
FRE21_SDft2 <- as.matrix(FRE21_SDft)
numRows <- nrow(FRE21_SDft2)
numCols <- ncol(FRE21_SDft2)
FRE21_SDft3 <- FRE21_SDft2[c(2:numRows) , c(2:numCols)]
FRE21_SDTable <- graph.adjacency(FRE21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(FRE21_SDTable, vertex.label = V(FRE21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
FRE21_SD.clusterCoef <- transitivity(FRE21_SDTable, type="global") #cluster coefficient
FRE21_SD.degreeCent <- centralization.degree(FRE21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_SDftn <- as.network.matrix(FRE21_SDft)
FRE21_SD.netDensity <- network.density(FRE21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_SD.entropy <- entropy(FRE21_SDft) #entropy

FRE21_SD.netMx <- cbind(FRE21_SD.netMx, FRE21_SD.clusterCoef, FRE21_SD.degreeCent$centralization,
                        FRE21_SD.netDensity, FRE21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
FRE21_TDg2 <- data.frame(FRE21_TD)
FRE21_TDg2 <- FRE21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_TDg2$player1
player2vector <- FRE21_TDg2$player2
FRE21_TDg3 <- FRE21_TDg2
FRE21_TDg3$p1inp2vec <- is.element(FRE21_TDg3$player1, player2vector)
FRE21_TDg3$p2inp1vec <- is.element(FRE21_TDg3$player2, player1vector)

addPlayer1 <- FRE21_TDg3[ which(FRE21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_TDg3[ which(FRE21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_TDg2 <- rbind(FRE21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
FRE21_TDft <- ftable(FRE21_TDg2$player1, FRE21_TDg2$player2)
FRE21_TDft2 <- as.matrix(FRE21_TDft)
numRows <- nrow(FRE21_TDft2)
numCols <- ncol(FRE21_TDft2)
FRE21_TDft3 <- FRE21_TDft2[c(2:numRows) , c(2:numCols)]
FRE21_TDTable <- graph.adjacency(FRE21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(FRE21_TDTable, vertex.label = V(FRE21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
FRE21_TD.clusterCoef <- transitivity(FRE21_TDTable, type="global") #cluster coefficient
FRE21_TD.degreeCent <- centralization.degree(FRE21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_TDftn <- as.network.matrix(FRE21_TDft)
FRE21_TD.netDensity <- network.density(FRE21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_TD.entropy <- entropy(FRE21_TDft) #entropy

FRE21_TD.netMx <- cbind(FRE21_TD.netMx, FRE21_TD.clusterCoef, FRE21_TD.degreeCent$centralization,
                        FRE21_TD.netDensity, FRE21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
FRE21_QTg2 <- data.frame(FRE21_QT)
FRE21_QTg2 <- FRE21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE21_QTg2$player1
player2vector <- FRE21_QTg2$player2
FRE21_QTg3 <- FRE21_QTg2
FRE21_QTg3$p1inp2vec <- is.element(FRE21_QTg3$player1, player2vector)
FRE21_QTg3$p2inp1vec <- is.element(FRE21_QTg3$player2, player1vector)

addPlayer1 <- FRE21_QTg3[ which(FRE21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE21_QTg3[ which(FRE21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE21_QTg2 <- rbind(FRE21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
FRE21_QTft <- ftable(FRE21_QTg2$player1, FRE21_QTg2$player2)
FRE21_QTft2 <- as.matrix(FRE21_QTft)
numRows <- nrow(FRE21_QTft2)
numCols <- ncol(FRE21_QTft2)
FRE21_QTft3 <- FRE21_QTft2[c(2:numRows) , c(2:numCols)]
FRE21_QTTable <- graph.adjacency(FRE21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(FRE21_QTTable, vertex.label = V(FRE21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
FRE21_QT.clusterCoef <- transitivity(FRE21_QTTable, type="global") #cluster coefficient
FRE21_QT.degreeCent <- centralization.degree(FRE21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE21_QTftn <- as.network.matrix(FRE21_QTft)
FRE21_QT.netDensity <- network.density(FRE21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE21_QT.entropy <- entropy(FRE21_QTft) #entropy

FRE21_QT.netMx <- cbind(FRE21_QT.netMx, FRE21_QT.clusterCoef, FRE21_QT.degreeCent$centralization,
                        FRE21_QT.netDensity, FRE21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE21_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
GCFC21_Gg2 <- data.frame(GCFC21_G)
GCFC21_Gg2 <- GCFC21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_Gg2$player1
player2vector <- GCFC21_Gg2$player2
GCFC21_Gg3 <- GCFC21_Gg2
GCFC21_Gg3$p1inp2vec <- is.element(GCFC21_Gg3$player1, player2vector)
GCFC21_Gg3$p2inp1vec <- is.element(GCFC21_Gg3$player2, player1vector)

addPlayer1 <- GCFC21_Gg3[ which(GCFC21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC21_Gg3[ which(GCFC21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_Gg2 <- rbind(GCFC21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
GCFC21_Gft <- ftable(GCFC21_Gg2$player1, GCFC21_Gg2$player2)
GCFC21_Gft2 <- as.matrix(GCFC21_Gft)
numRows <- nrow(GCFC21_Gft2)
numCols <- ncol(GCFC21_Gft2)
GCFC21_Gft3 <- GCFC21_Gft2[c(2:numRows) , c(2:numCols)]
GCFC21_GTable <- graph.adjacency(GCFC21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(GCFC21_GTable, vertex.label = V(GCFC21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
GCFC21_G.clusterCoef <- transitivity(GCFC21_GTable, type="global") #cluster coefficient
GCFC21_G.degreeCent <- centralization.degree(GCFC21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_Gftn <- as.network.matrix(GCFC21_Gft)
GCFC21_G.netDensity <- network.density(GCFC21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_G.entropy <- entropy(GCFC21_Gft) #entropy

GCFC21_G.netMx <- cbind(GCFC21_G.netMx, GCFC21_G.clusterCoef, GCFC21_G.degreeCent$centralization,
                        GCFC21_G.netDensity, GCFC21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
GCFC21_Bg2 <- data.frame(GCFC21_B)
GCFC21_Bg2 <- GCFC21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_Bg2$player1
player2vector <- GCFC21_Bg2$player2
GCFC21_Bg3 <- GCFC21_Bg2
GCFC21_Bg3$p1inp2vec <- is.element(GCFC21_Bg3$player1, player2vector)
GCFC21_Bg3$p2inp1vec <- is.element(GCFC21_Bg3$player2, player1vector)

addPlayer1 <- GCFC21_Bg3[ which(GCFC21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_Bg3[ which(GCFC21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_Bg2 <- rbind(GCFC21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
GCFC21_Bft <- ftable(GCFC21_Bg2$player1, GCFC21_Bg2$player2)
GCFC21_Bft2 <- as.matrix(GCFC21_Bft)
numRows <- nrow(GCFC21_Bft2)
numCols <- ncol(GCFC21_Bft2)
GCFC21_Bft3 <- GCFC21_Bft2[c(2:numRows) , c(2:numCols)]
GCFC21_BTable <- graph.adjacency(GCFC21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(GCFC21_BTable, vertex.label = V(GCFC21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
GCFC21_B.clusterCoef <- transitivity(GCFC21_BTable, type="global") #cluster coefficient
GCFC21_B.degreeCent <- centralization.degree(GCFC21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_Bftn <- as.network.matrix(GCFC21_Bft)
GCFC21_B.netDensity <- network.density(GCFC21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_B.entropy <- entropy(GCFC21_Bft) #entropy

GCFC21_B.netMx <- cbind(GCFC21_B.netMx, GCFC21_B.clusterCoef, GCFC21_B.degreeCent$centralization,
                        GCFC21_B.netDensity, GCFC21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
GCFC21_SFg2 <- data.frame(GCFC21_SF)
GCFC21_SFg2 <- GCFC21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_SFg2$player1
player2vector <- GCFC21_SFg2$player2
GCFC21_SFg3 <- GCFC21_SFg2
GCFC21_SFg3$p1inp2vec <- is.element(GCFC21_SFg3$player1, player2vector)
GCFC21_SFg3$p2inp1vec <- is.element(GCFC21_SFg3$player2, player1vector)

addPlayer1 <- GCFC21_SFg3[ which(GCFC21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_SFg3[ which(GCFC21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_SFg2 <- rbind(GCFC21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
GCFC21_SFft <- ftable(GCFC21_SFg2$player1, GCFC21_SFg2$player2)
GCFC21_SFft2 <- as.matrix(GCFC21_SFft)
numRows <- nrow(GCFC21_SFft2)
numCols <- ncol(GCFC21_SFft2)
GCFC21_SFft3 <- GCFC21_SFft2[c(2:numRows) , c(2:numCols)]
GCFC21_SFTable <- graph.adjacency(GCFC21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(GCFC21_SFTable, vertex.label = V(GCFC21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
GCFC21_SF.clusterCoef <- transitivity(GCFC21_SFTable, type="global") #cluster coefficient
GCFC21_SF.degreeCent <- centralization.degree(GCFC21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_SFftn <- as.network.matrix(GCFC21_SFft)
GCFC21_SF.netDensity <- network.density(GCFC21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_SF.entropy <- entropy(GCFC21_SFft) #entropy

GCFC21_SF.netMx <- cbind(GCFC21_SF.netMx, GCFC21_SF.clusterCoef, GCFC21_SF.degreeCent$centralization,
                         GCFC21_SF.netDensity, GCFC21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
GCFC21_TFg2 <- data.frame(GCFC21_TF)
GCFC21_TFg2 <- GCFC21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_TFg2$player1
player2vector <- GCFC21_TFg2$player2
GCFC21_TFg3 <- GCFC21_TFg2
GCFC21_TFg3$p1inp2vec <- is.element(GCFC21_TFg3$player1, player2vector)
GCFC21_TFg3$p2inp1vec <- is.element(GCFC21_TFg3$player2, player1vector)

addPlayer1 <- GCFC21_TFg3[ which(GCFC21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC21_TFg3[ which(GCFC21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_TFg2 <- rbind(GCFC21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
GCFC21_TFft <- ftable(GCFC21_TFg2$player1, GCFC21_TFg2$player2)
GCFC21_TFft2 <- as.matrix(GCFC21_TFft)
numRows <- nrow(GCFC21_TFft2)
numCols <- ncol(GCFC21_TFft2)
GCFC21_TFft3 <- GCFC21_TFft2[c(2:numRows) , c(2:numCols)]
GCFC21_TFTable <- graph.adjacency(GCFC21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(GCFC21_TFTable, vertex.label = V(GCFC21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
GCFC21_TF.clusterCoef <- transitivity(GCFC21_TFTable, type="global") #cluster coefficient
GCFC21_TF.degreeCent <- centralization.degree(GCFC21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_TFftn <- as.network.matrix(GCFC21_TFft)
GCFC21_TF.netDensity <- network.density(GCFC21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_TF.entropy <- entropy(GCFC21_TFft) #entropy

GCFC21_TF.netMx <- cbind(GCFC21_TF.netMx, GCFC21_TF.clusterCoef, GCFC21_TF.degreeCent$centralization,
                         GCFC21_TF.netDensity, GCFC21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
GCFC21_SAMg2 <- data.frame(GCFC21_SAM)
GCFC21_SAMg2 <- GCFC21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_SAMg2$player1
player2vector <- GCFC21_SAMg2$player2
GCFC21_SAMg3 <- GCFC21_SAMg2
GCFC21_SAMg3$p1inp2vec <- is.element(GCFC21_SAMg3$player1, player2vector)
GCFC21_SAMg3$p2inp1vec <- is.element(GCFC21_SAMg3$player2, player1vector)

addPlayer1 <- GCFC21_SAMg3[ which(GCFC21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_SAMg3[ which(GCFC21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_SAMg2 <- rbind(GCFC21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
GCFC21_SAMft <- ftable(GCFC21_SAMg2$player1, GCFC21_SAMg2$player2)
GCFC21_SAMft2 <- as.matrix(GCFC21_SAMft)
numRows <- nrow(GCFC21_SAMft2)
numCols <- ncol(GCFC21_SAMft2)
GCFC21_SAMft3 <- GCFC21_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC21_SAMTable <- graph.adjacency(GCFC21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(GCFC21_SAMTable, vertex.label = V(GCFC21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
GCFC21_SAM.clusterCoef <- transitivity(GCFC21_SAMTable, type="global") #cluster coefficient
GCFC21_SAM.degreeCent <- centralization.degree(GCFC21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_SAMftn <- as.network.matrix(GCFC21_SAMft)
GCFC21_SAM.netDensity <- network.density(GCFC21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_SAM.entropy <- entropy(GCFC21_SAMft) #entropy

GCFC21_SAM.netMx <- cbind(GCFC21_SAM.netMx, GCFC21_SAM.clusterCoef, GCFC21_SAM.degreeCent$centralization,
                          GCFC21_SAM.netDensity, GCFC21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
GCFC21_TAMg2 <- data.frame(GCFC21_TAM)
GCFC21_TAMg2 <- GCFC21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_TAMg2$player1
player2vector <- GCFC21_TAMg2$player2
GCFC21_TAMg3 <- GCFC21_TAMg2
GCFC21_TAMg3$p1inp2vec <- is.element(GCFC21_TAMg3$player1, player2vector)
GCFC21_TAMg3$p2inp1vec <- is.element(GCFC21_TAMg3$player2, player1vector)

addPlayer1 <- GCFC21_TAMg3[ which(GCFC21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_TAMg3[ which(GCFC21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_TAMg2 <- rbind(GCFC21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
GCFC21_TAMft <- ftable(GCFC21_TAMg2$player1, GCFC21_TAMg2$player2)
GCFC21_TAMft2 <- as.matrix(GCFC21_TAMft)
numRows <- nrow(GCFC21_TAMft2)
numCols <- ncol(GCFC21_TAMft2)
GCFC21_TAMft3 <- GCFC21_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC21_TAMTable <- graph.adjacency(GCFC21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(GCFC21_TAMTable, vertex.label = V(GCFC21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
GCFC21_TAM.clusterCoef <- transitivity(GCFC21_TAMTable, type="global") #cluster coefficient
GCFC21_TAM.degreeCent <- centralization.degree(GCFC21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_TAMftn <- as.network.matrix(GCFC21_TAMft)
GCFC21_TAM.netDensity <- network.density(GCFC21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_TAM.entropy <- entropy(GCFC21_TAMft) #entropy

GCFC21_TAM.netMx <- cbind(GCFC21_TAM.netMx, GCFC21_TAM.clusterCoef, GCFC21_TAM.degreeCent$centralization,
                          GCFC21_TAM.netDensity, GCFC21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
GCFC21_SDMg2 <- data.frame(GCFC21_SDM)
GCFC21_SDMg2 <- GCFC21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_SDMg2$player1
player2vector <- GCFC21_SDMg2$player2
GCFC21_SDMg3 <- GCFC21_SDMg2
GCFC21_SDMg3$p1inp2vec <- is.element(GCFC21_SDMg3$player1, player2vector)
GCFC21_SDMg3$p2inp1vec <- is.element(GCFC21_SDMg3$player2, player1vector)

addPlayer1 <- GCFC21_SDMg3[ which(GCFC21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_SDMg3[ which(GCFC21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_SDMg2 <- rbind(GCFC21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
GCFC21_SDMft <- ftable(GCFC21_SDMg2$player1, GCFC21_SDMg2$player2)
GCFC21_SDMft2 <- as.matrix(GCFC21_SDMft)
numRows <- nrow(GCFC21_SDMft2)
numCols <- ncol(GCFC21_SDMft2)
GCFC21_SDMft3 <- GCFC21_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC21_SDMTable <- graph.adjacency(GCFC21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(GCFC21_SDMTable, vertex.label = V(GCFC21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
GCFC21_SDM.clusterCoef <- transitivity(GCFC21_SDMTable, type="global") #cluster coefficient
GCFC21_SDM.degreeCent <- centralization.degree(GCFC21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_SDMftn <- as.network.matrix(GCFC21_SDMft)
GCFC21_SDM.netDensity <- network.density(GCFC21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_SDM.entropy <- entropy(GCFC21_SDMft) #entropy

GCFC21_SDM.netMx <- cbind(GCFC21_SDM.netMx, GCFC21_SDM.clusterCoef, GCFC21_SDM.degreeCent$centralization,
                          GCFC21_SDM.netDensity, GCFC21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
GCFC21_TDMg2 <- data.frame(GCFC21_TDM)
GCFC21_TDMg2 <- GCFC21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_TDMg2$player1
player2vector <- GCFC21_TDMg2$player2
GCFC21_TDMg3 <- GCFC21_TDMg2
GCFC21_TDMg3$p1inp2vec <- is.element(GCFC21_TDMg3$player1, player2vector)
GCFC21_TDMg3$p2inp1vec <- is.element(GCFC21_TDMg3$player2, player1vector)

addPlayer1 <- GCFC21_TDMg3[ which(GCFC21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_TDMg3[ which(GCFC21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_TDMg2 <- rbind(GCFC21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
GCFC21_TDMft <- ftable(GCFC21_TDMg2$player1, GCFC21_TDMg2$player2)
GCFC21_TDMft2 <- as.matrix(GCFC21_TDMft)
numRows <- nrow(GCFC21_TDMft2)
numCols <- ncol(GCFC21_TDMft2)
GCFC21_TDMft3 <- GCFC21_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC21_TDMTable <- graph.adjacency(GCFC21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(GCFC21_TDMTable, vertex.label = V(GCFC21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
GCFC21_TDM.clusterCoef <- transitivity(GCFC21_TDMTable, type="global") #cluster coefficient
GCFC21_TDM.degreeCent <- centralization.degree(GCFC21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_TDMftn <- as.network.matrix(GCFC21_TDMft)
GCFC21_TDM.netDensity <- network.density(GCFC21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_TDM.entropy <- entropy(GCFC21_TDMft) #entropy

GCFC21_TDM.netMx <- cbind(GCFC21_TDM.netMx, GCFC21_TDM.clusterCoef, GCFC21_TDM.degreeCent$centralization,
                          GCFC21_TDM.netDensity, GCFC21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
GCFC21_SDg2 <- data.frame(GCFC21_SD)
GCFC21_SDg2 <- GCFC21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_SDg2$player1
player2vector <- GCFC21_SDg2$player2
GCFC21_SDg3 <- GCFC21_SDg2
GCFC21_SDg3$p1inp2vec <- is.element(GCFC21_SDg3$player1, player2vector)
GCFC21_SDg3$p2inp1vec <- is.element(GCFC21_SDg3$player2, player1vector)

addPlayer1 <- GCFC21_SDg3[ which(GCFC21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_SDg3[ which(GCFC21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_SDg2 <- rbind(GCFC21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
GCFC21_SDft <- ftable(GCFC21_SDg2$player1, GCFC21_SDg2$player2)
GCFC21_SDft2 <- as.matrix(GCFC21_SDft)
numRows <- nrow(GCFC21_SDft2)
numCols <- ncol(GCFC21_SDft2)
GCFC21_SDft3 <- GCFC21_SDft2[c(2:numRows) , c(2:numCols)]
GCFC21_SDTable <- graph.adjacency(GCFC21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(GCFC21_SDTable, vertex.label = V(GCFC21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
GCFC21_SD.clusterCoef <- transitivity(GCFC21_SDTable, type="global") #cluster coefficient
GCFC21_SD.degreeCent <- centralization.degree(GCFC21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_SDftn <- as.network.matrix(GCFC21_SDft)
GCFC21_SD.netDensity <- network.density(GCFC21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_SD.entropy <- entropy(GCFC21_SDft) #entropy

GCFC21_SD.netMx <- cbind(GCFC21_SD.netMx, GCFC21_SD.clusterCoef, GCFC21_SD.degreeCent$centralization,
                         GCFC21_SD.netDensity, GCFC21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
GCFC21_TDg2 <- data.frame(GCFC21_TD)
GCFC21_TDg2 <- GCFC21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_TDg2$player1
player2vector <- GCFC21_TDg2$player2
GCFC21_TDg3 <- GCFC21_TDg2
GCFC21_TDg3$p1inp2vec <- is.element(GCFC21_TDg3$player1, player2vector)
GCFC21_TDg3$p2inp1vec <- is.element(GCFC21_TDg3$player2, player1vector)

addPlayer1 <- GCFC21_TDg3[ which(GCFC21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_TDg3[ which(GCFC21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_TDg2 <- rbind(GCFC21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
GCFC21_TDft <- ftable(GCFC21_TDg2$player1, GCFC21_TDg2$player2)
GCFC21_TDft2 <- as.matrix(GCFC21_TDft)
numRows <- nrow(GCFC21_TDft2)
numCols <- ncol(GCFC21_TDft2)
GCFC21_TDft3 <- GCFC21_TDft2[c(2:numRows) , c(2:numCols)]
GCFC21_TDTable <- graph.adjacency(GCFC21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(GCFC21_TDTable, vertex.label = V(GCFC21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
GCFC21_TD.clusterCoef <- transitivity(GCFC21_TDTable, type="global") #cluster coefficient
GCFC21_TD.degreeCent <- centralization.degree(GCFC21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_TDftn <- as.network.matrix(GCFC21_TDft)
GCFC21_TD.netDensity <- network.density(GCFC21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_TD.entropy <- entropy(GCFC21_TDft) #entropy

GCFC21_TD.netMx <- cbind(GCFC21_TD.netMx, GCFC21_TD.clusterCoef, GCFC21_TD.degreeCent$centralization,
                         GCFC21_TD.netDensity, GCFC21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
GCFC21_QTg2 <- data.frame(GCFC21_QT)
GCFC21_QTg2 <- GCFC21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC21_QTg2$player1
player2vector <- GCFC21_QTg2$player2
GCFC21_QTg3 <- GCFC21_QTg2
GCFC21_QTg3$p1inp2vec <- is.element(GCFC21_QTg3$player1, player2vector)
GCFC21_QTg3$p2inp1vec <- is.element(GCFC21_QTg3$player2, player1vector)

addPlayer1 <- GCFC21_QTg3[ which(GCFC21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC21_QTg3[ which(GCFC21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC21_QTg2 <- rbind(GCFC21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
GCFC21_QTft <- ftable(GCFC21_QTg2$player1, GCFC21_QTg2$player2)
GCFC21_QTft2 <- as.matrix(GCFC21_QTft)
numRows <- nrow(GCFC21_QTft2)
numCols <- ncol(GCFC21_QTft2)
GCFC21_QTft3 <- GCFC21_QTft2[c(2:numRows) , c(2:numCols)]
GCFC21_QTTable <- graph.adjacency(GCFC21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(GCFC21_QTTable, vertex.label = V(GCFC21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
GCFC21_QT.clusterCoef <- transitivity(GCFC21_QTTable, type="global") #cluster coefficient
GCFC21_QT.degreeCent <- centralization.degree(GCFC21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC21_QTftn <- as.network.matrix(GCFC21_QTft)
GCFC21_QT.netDensity <- network.density(GCFC21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC21_QT.entropy <- entropy(GCFC21_QTft) #entropy

GCFC21_QT.netMx <- cbind(GCFC21_QT.netMx, GCFC21_QT.clusterCoef, GCFC21_QT.degreeCent$centralization,
                         GCFC21_QT.netDensity, GCFC21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC21_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
GEEL21_Gg2 <- data.frame(GEEL21_G)
GEEL21_Gg2 <- GEEL21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_Gg2$player1
player2vector <- GEEL21_Gg2$player2
GEEL21_Gg3 <- GEEL21_Gg2
GEEL21_Gg3$p1inp2vec <- is.element(GEEL21_Gg3$player1, player2vector)
GEEL21_Gg3$p2inp1vec <- is.element(GEEL21_Gg3$player2, player1vector)

addPlayer1 <- GEEL21_Gg3[ which(GEEL21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_Gg3[ which(GEEL21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_Gg2 <- rbind(GEEL21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
GEEL21_Gft <- ftable(GEEL21_Gg2$player1, GEEL21_Gg2$player2)
GEEL21_Gft2 <- as.matrix(GEEL21_Gft)
numRows <- nrow(GEEL21_Gft2)
numCols <- ncol(GEEL21_Gft2)
GEEL21_Gft3 <- GEEL21_Gft2[c(2:numRows) , c(2:numCols)]
GEEL21_GTable <- graph.adjacency(GEEL21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(GEEL21_GTable, vertex.label = V(GEEL21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
GEEL21_G.clusterCoef <- transitivity(GEEL21_GTable, type="global") #cluster coefficient
GEEL21_G.degreeCent <- centralization.degree(GEEL21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_Gftn <- as.network.matrix(GEEL21_Gft)
GEEL21_G.netDensity <- network.density(GEEL21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_G.entropy <- entropy(GEEL21_Gft) #entropy

GEEL21_G.netMx <- cbind(GEEL21_G.netMx, GEEL21_G.clusterCoef, GEEL21_G.degreeCent$centralization,
                        GEEL21_G.netDensity, GEEL21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
GEEL21_Bg2 <- data.frame(GEEL21_B)
GEEL21_Bg2 <- GEEL21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_Bg2$player1
player2vector <- GEEL21_Bg2$player2
GEEL21_Bg3 <- GEEL21_Bg2
GEEL21_Bg3$p1inp2vec <- is.element(GEEL21_Bg3$player1, player2vector)
GEEL21_Bg3$p2inp1vec <- is.element(GEEL21_Bg3$player2, player1vector)

addPlayer1 <- GEEL21_Bg3[ which(GEEL21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_Bg3[ which(GEEL21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_Bg2 <- rbind(GEEL21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
GEEL21_Bft <- ftable(GEEL21_Bg2$player1, GEEL21_Bg2$player2)
GEEL21_Bft2 <- as.matrix(GEEL21_Bft)
numRows <- nrow(GEEL21_Bft2)
numCols <- ncol(GEEL21_Bft2)
GEEL21_Bft3 <- GEEL21_Bft2[c(2:numRows) , c(2:numCols)]
GEEL21_BTable <- graph.adjacency(GEEL21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(GEEL21_BTable, vertex.label = V(GEEL21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
GEEL21_B.clusterCoef <- transitivity(GEEL21_BTable, type="global") #cluster coefficient
GEEL21_B.degreeCent <- centralization.degree(GEEL21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_Bftn <- as.network.matrix(GEEL21_Bft)
GEEL21_B.netDensity <- network.density(GEEL21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_B.entropy <- entropy(GEEL21_Bft) #entropy

GEEL21_B.netMx <- cbind(GEEL21_B.netMx, GEEL21_B.clusterCoef, GEEL21_B.degreeCent$centralization,
                        GEEL21_B.netDensity, GEEL21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
GEEL21_SFg2 <- data.frame(GEEL21_SF)
GEEL21_SFg2 <- GEEL21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_SFg2$player1
player2vector <- GEEL21_SFg2$player2
GEEL21_SFg3 <- GEEL21_SFg2
GEEL21_SFg3$p1inp2vec <- is.element(GEEL21_SFg3$player1, player2vector)
GEEL21_SFg3$p2inp1vec <- is.element(GEEL21_SFg3$player2, player1vector)

addPlayer1 <- GEEL21_SFg3[ which(GEEL21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_SFg3[ which(GEEL21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_SFg2 <- rbind(GEEL21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
GEEL21_SFft <- ftable(GEEL21_SFg2$player1, GEEL21_SFg2$player2)
GEEL21_SFft2 <- as.matrix(GEEL21_SFft)
numRows <- nrow(GEEL21_SFft2)
numCols <- ncol(GEEL21_SFft2)
GEEL21_SFft3 <- GEEL21_SFft2[c(2:numRows) , c(2:numCols)]
GEEL21_SFTable <- graph.adjacency(GEEL21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(GEEL21_SFTable, vertex.label = V(GEEL21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
GEEL21_SF.clusterCoef <- transitivity(GEEL21_SFTable, type="global") #cluster coefficient
GEEL21_SF.degreeCent <- centralization.degree(GEEL21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_SFftn <- as.network.matrix(GEEL21_SFft)
GEEL21_SF.netDensity <- network.density(GEEL21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_SF.entropy <- entropy(GEEL21_SFft) #entropy

GEEL21_SF.netMx <- cbind(GEEL21_SF.netMx, GEEL21_SF.clusterCoef, GEEL21_SF.degreeCent$centralization,
                         GEEL21_SF.netDensity, GEEL21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
GEEL21_TFg2 <- data.frame(GEEL21_TF)
GEEL21_TFg2 <- GEEL21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_TFg2$player1
player2vector <- GEEL21_TFg2$player2
GEEL21_TFg3 <- GEEL21_TFg2
GEEL21_TFg3$p1inp2vec <- is.element(GEEL21_TFg3$player1, player2vector)
GEEL21_TFg3$p2inp1vec <- is.element(GEEL21_TFg3$player2, player1vector)

addPlayer1 <- GEEL21_TFg3[ which(GEEL21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_TFg3[ which(GEEL21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_TFg2 <- rbind(GEEL21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
GEEL21_TFft <- ftable(GEEL21_TFg2$player1, GEEL21_TFg2$player2)
GEEL21_TFft2 <- as.matrix(GEEL21_TFft)
numRows <- nrow(GEEL21_TFft2)
numCols <- ncol(GEEL21_TFft2)
GEEL21_TFft3 <- GEEL21_TFft2[c(2:numRows) , c(2:numCols)]
GEEL21_TFTable <- graph.adjacency(GEEL21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(GEEL21_TFTable, vertex.label = V(GEEL21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
GEEL21_TF.clusterCoef <- transitivity(GEEL21_TFTable, type="global") #cluster coefficient
GEEL21_TF.degreeCent <- centralization.degree(GEEL21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_TFftn <- as.network.matrix(GEEL21_TFft)
GEEL21_TF.netDensity <- network.density(GEEL21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_TF.entropy <- entropy(GEEL21_TFft) #entropy

GEEL21_TF.netMx <- cbind(GEEL21_TF.netMx, GEEL21_TF.clusterCoef, GEEL21_TF.degreeCent$centralization,
                         GEEL21_TF.netDensity, GEEL21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
GEEL21_SAMg2 <- data.frame(GEEL21_SAM)
GEEL21_SAMg2 <- GEEL21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_SAMg2$player1
player2vector <- GEEL21_SAMg2$player2
GEEL21_SAMg3 <- GEEL21_SAMg2
GEEL21_SAMg3$p1inp2vec <- is.element(GEEL21_SAMg3$player1, player2vector)
GEEL21_SAMg3$p2inp1vec <- is.element(GEEL21_SAMg3$player2, player1vector)

addPlayer1 <- GEEL21_SAMg3[ which(GEEL21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_SAMg3[ which(GEEL21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_SAMg2 <- rbind(GEEL21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
GEEL21_SAMft <- ftable(GEEL21_SAMg2$player1, GEEL21_SAMg2$player2)
GEEL21_SAMft2 <- as.matrix(GEEL21_SAMft)
numRows <- nrow(GEEL21_SAMft2)
numCols <- ncol(GEEL21_SAMft2)
GEEL21_SAMft3 <- GEEL21_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL21_SAMTable <- graph.adjacency(GEEL21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(GEEL21_SAMTable, vertex.label = V(GEEL21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
GEEL21_SAM.clusterCoef <- transitivity(GEEL21_SAMTable, type="global") #cluster coefficient
GEEL21_SAM.degreeCent <- centralization.degree(GEEL21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_SAMftn <- as.network.matrix(GEEL21_SAMft)
GEEL21_SAM.netDensity <- network.density(GEEL21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_SAM.entropy <- entropy(GEEL21_SAMft) #entropy

GEEL21_SAM.netMx <- cbind(GEEL21_SAM.netMx, GEEL21_SAM.clusterCoef, GEEL21_SAM.degreeCent$centralization,
                          GEEL21_SAM.netDensity, GEEL21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
GEEL21_TAMg2 <- data.frame(GEEL21_TAM)
GEEL21_TAMg2 <- GEEL21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_TAMg2$player1
player2vector <- GEEL21_TAMg2$player2
GEEL21_TAMg3 <- GEEL21_TAMg2
GEEL21_TAMg3$p1inp2vec <- is.element(GEEL21_TAMg3$player1, player2vector)
GEEL21_TAMg3$p2inp1vec <- is.element(GEEL21_TAMg3$player2, player1vector)

addPlayer1 <- GEEL21_TAMg3[ which(GEEL21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_TAMg3[ which(GEEL21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_TAMg2 <- rbind(GEEL21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
GEEL21_TAMft <- ftable(GEEL21_TAMg2$player1, GEEL21_TAMg2$player2)
GEEL21_TAMft2 <- as.matrix(GEEL21_TAMft)
numRows <- nrow(GEEL21_TAMft2)
numCols <- ncol(GEEL21_TAMft2)
GEEL21_TAMft3 <- GEEL21_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL21_TAMTable <- graph.adjacency(GEEL21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(GEEL21_TAMTable, vertex.label = V(GEEL21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
GEEL21_TAM.clusterCoef <- transitivity(GEEL21_TAMTable, type="global") #cluster coefficient
GEEL21_TAM.degreeCent <- centralization.degree(GEEL21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_TAMftn <- as.network.matrix(GEEL21_TAMft)
GEEL21_TAM.netDensity <- network.density(GEEL21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_TAM.entropy <- entropy(GEEL21_TAMft) #entropy

GEEL21_TAM.netMx <- cbind(GEEL21_TAM.netMx, GEEL21_TAM.clusterCoef, GEEL21_TAM.degreeCent$centralization,
                          GEEL21_TAM.netDensity, GEEL21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
GEEL21_SDMg2 <- data.frame(GEEL21_SDM)
GEEL21_SDMg2 <- GEEL21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_SDMg2$player1
player2vector <- GEEL21_SDMg2$player2
GEEL21_SDMg3 <- GEEL21_SDMg2
GEEL21_SDMg3$p1inp2vec <- is.element(GEEL21_SDMg3$player1, player2vector)
GEEL21_SDMg3$p2inp1vec <- is.element(GEEL21_SDMg3$player2, player1vector)

addPlayer1 <- GEEL21_SDMg3[ which(GEEL21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_SDMg3[ which(GEEL21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_SDMg2 <- rbind(GEEL21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
GEEL21_SDMft <- ftable(GEEL21_SDMg2$player1, GEEL21_SDMg2$player2)
GEEL21_SDMft2 <- as.matrix(GEEL21_SDMft)
numRows <- nrow(GEEL21_SDMft2)
numCols <- ncol(GEEL21_SDMft2)
GEEL21_SDMft3 <- GEEL21_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL21_SDMTable <- graph.adjacency(GEEL21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(GEEL21_SDMTable, vertex.label = V(GEEL21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
GEEL21_SDM.clusterCoef <- transitivity(GEEL21_SDMTable, type="global") #cluster coefficient
GEEL21_SDM.degreeCent <- centralization.degree(GEEL21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_SDMftn <- as.network.matrix(GEEL21_SDMft)
GEEL21_SDM.netDensity <- network.density(GEEL21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_SDM.entropy <- entropy(GEEL21_SDMft) #entropy

GEEL21_SDM.netMx <- cbind(GEEL21_SDM.netMx, GEEL21_SDM.clusterCoef, GEEL21_SDM.degreeCent$centralization,
                          GEEL21_SDM.netDensity, GEEL21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************
#NA

round = 21
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
GEEL21_TDMg2 <- data.frame(GEEL21_TDM)
GEEL21_TDMg2 <- GEEL21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_TDMg2$player1
player2vector <- GEEL21_TDMg2$player2
GEEL21_TDMg3 <- GEEL21_TDMg2
GEEL21_TDMg3$p1inp2vec <- is.element(GEEL21_TDMg3$player1, player2vector)
GEEL21_TDMg3$p2inp1vec <- is.element(GEEL21_TDMg3$player2, player1vector)

addPlayer1 <- GEEL21_TDMg3[ which(GEEL21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_TDMg3[ which(GEEL21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_TDMg2 <- rbind(GEEL21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
GEEL21_TDMft <- ftable(GEEL21_TDMg2$player1, GEEL21_TDMg2$player2)
GEEL21_TDMft2 <- as.matrix(GEEL21_TDMft)
numRows <- nrow(GEEL21_TDMft2)
numCols <- ncol(GEEL21_TDMft2)
GEEL21_TDMft3 <- GEEL21_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL21_TDMTable <- graph.adjacency(GEEL21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(GEEL21_TDMTable, vertex.label = V(GEEL21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
GEEL21_TDM.clusterCoef <- transitivity(GEEL21_TDMTable, type="global") #cluster coefficient
GEEL21_TDM.degreeCent <- centralization.degree(GEEL21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_TDMftn <- as.network.matrix(GEEL21_TDMft)
GEEL21_TDM.netDensity <- network.density(GEEL21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_TDM.entropy <- entropy(GEEL21_TDMft) #entropy

GEEL21_TDM.netMx <- cbind(GEEL21_TDM.netMx, GEEL21_TDM.clusterCoef, GEEL21_TDM.degreeCent$centralization,
                          GEEL21_TDM.netDensity, GEEL21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
GEEL21_SDg2 <- data.frame(GEEL21_SD)
GEEL21_SDg2 <- GEEL21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_SDg2$player1
player2vector <- GEEL21_SDg2$player2
GEEL21_SDg3 <- GEEL21_SDg2
GEEL21_SDg3$p1inp2vec <- is.element(GEEL21_SDg3$player1, player2vector)
GEEL21_SDg3$p2inp1vec <- is.element(GEEL21_SDg3$player2, player1vector)

addPlayer1 <- GEEL21_SDg3[ which(GEEL21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_SDg3[ which(GEEL21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_SDg2 <- rbind(GEEL21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
GEEL21_SDft <- ftable(GEEL21_SDg2$player1, GEEL21_SDg2$player2)
GEEL21_SDft2 <- as.matrix(GEEL21_SDft)
numRows <- nrow(GEEL21_SDft2)
numCols <- ncol(GEEL21_SDft2)
GEEL21_SDft3 <- GEEL21_SDft2[c(2:numRows) , c(2:numCols)]
GEEL21_SDTable <- graph.adjacency(GEEL21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(GEEL21_SDTable, vertex.label = V(GEEL21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
GEEL21_SD.clusterCoef <- transitivity(GEEL21_SDTable, type="global") #cluster coefficient
GEEL21_SD.degreeCent <- centralization.degree(GEEL21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_SDftn <- as.network.matrix(GEEL21_SDft)
GEEL21_SD.netDensity <- network.density(GEEL21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_SD.entropy <- entropy(GEEL21_SDft) #entropy

GEEL21_SD.netMx <- cbind(GEEL21_SD.netMx, GEEL21_SD.clusterCoef, GEEL21_SD.degreeCent$centralization,
                         GEEL21_SD.netDensity, GEEL21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
GEEL21_TDg2 <- data.frame(GEEL21_TD)
GEEL21_TDg2 <- GEEL21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_TDg2$player1
player2vector <- GEEL21_TDg2$player2
GEEL21_TDg3 <- GEEL21_TDg2
GEEL21_TDg3$p1inp2vec <- is.element(GEEL21_TDg3$player1, player2vector)
GEEL21_TDg3$p2inp1vec <- is.element(GEEL21_TDg3$player2, player1vector)

addPlayer1 <- GEEL21_TDg3[ which(GEEL21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_TDg3[ which(GEEL21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_TDg2 <- rbind(GEEL21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
GEEL21_TDft <- ftable(GEEL21_TDg2$player1, GEEL21_TDg2$player2)
GEEL21_TDft2 <- as.matrix(GEEL21_TDft)
numRows <- nrow(GEEL21_TDft2)
numCols <- ncol(GEEL21_TDft2)
GEEL21_TDft3 <- GEEL21_TDft2[c(2:numRows) , c(2:numCols)]
GEEL21_TDTable <- graph.adjacency(GEEL21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(GEEL21_TDTable, vertex.label = V(GEEL21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
GEEL21_TD.clusterCoef <- transitivity(GEEL21_TDTable, type="global") #cluster coefficient
GEEL21_TD.degreeCent <- centralization.degree(GEEL21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_TDftn <- as.network.matrix(GEEL21_TDft)
GEEL21_TD.netDensity <- network.density(GEEL21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_TD.entropy <- entropy(GEEL21_TDft) #entropy

GEEL21_TD.netMx <- cbind(GEEL21_TD.netMx, GEEL21_TD.clusterCoef, GEEL21_TD.degreeCent$centralization,
                         GEEL21_TD.netDensity, GEEL21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
GEEL21_QTg2 <- data.frame(GEEL21_QT)
GEEL21_QTg2 <- GEEL21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL21_QTg2$player1
player2vector <- GEEL21_QTg2$player2
GEEL21_QTg3 <- GEEL21_QTg2
GEEL21_QTg3$p1inp2vec <- is.element(GEEL21_QTg3$player1, player2vector)
GEEL21_QTg3$p2inp1vec <- is.element(GEEL21_QTg3$player2, player1vector)

addPlayer1 <- GEEL21_QTg3[ which(GEEL21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL21_QTg3[ which(GEEL21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL21_QTg2 <- rbind(GEEL21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
GEEL21_QTft <- ftable(GEEL21_QTg2$player1, GEEL21_QTg2$player2)
GEEL21_QTft2 <- as.matrix(GEEL21_QTft)
numRows <- nrow(GEEL21_QTft2)
numCols <- ncol(GEEL21_QTft2)
GEEL21_QTft3 <- GEEL21_QTft2[c(2:numRows) , c(2:numCols)]
GEEL21_QTTable <- graph.adjacency(GEEL21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(GEEL21_QTTable, vertex.label = V(GEEL21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
GEEL21_QT.clusterCoef <- transitivity(GEEL21_QTTable, type="global") #cluster coefficient
GEEL21_QT.degreeCent <- centralization.degree(GEEL21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL21_QTftn <- as.network.matrix(GEEL21_QTft)
GEEL21_QT.netDensity <- network.density(GEEL21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL21_QT.entropy <- entropy(GEEL21_QTft) #entropy

GEEL21_QT.netMx <- cbind(GEEL21_QT.netMx, GEEL21_QT.clusterCoef, GEEL21_QT.degreeCent$centralization,
                         GEEL21_QT.netDensity, GEEL21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL21_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************
#NA

round = 21
teamName = "GWS"
KIoutcome = "Goal_F"
GWS21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
GWS21_Gg2 <- data.frame(GWS21_G)
GWS21_Gg2 <- GWS21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_Gg2$player1
player2vector <- GWS21_Gg2$player2
GWS21_Gg3 <- GWS21_Gg2
GWS21_Gg3$p1inp2vec <- is.element(GWS21_Gg3$player1, player2vector)
GWS21_Gg3$p2inp1vec <- is.element(GWS21_Gg3$player2, player1vector)

addPlayer1 <- GWS21_Gg3[ which(GWS21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_Gg3[ which(GWS21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_Gg2 <- rbind(GWS21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
GWS21_Gft <- ftable(GWS21_Gg2$player1, GWS21_Gg2$player2)
GWS21_Gft2 <- as.matrix(GWS21_Gft)
numRows <- nrow(GWS21_Gft2)
numCols <- ncol(GWS21_Gft2)
GWS21_Gft3 <- GWS21_Gft2[c(1:numRows) , c(1:numCols)]
GWS21_GTable <- graph.adjacency(GWS21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(GWS21_GTable, vertex.label = V(GWS21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
GWS21_G.clusterCoef <- transitivity(GWS21_GTable, type="global") #cluster coefficient
GWS21_G.degreeCent <- centralization.degree(GWS21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_Gftn <- as.network.matrix(GWS21_Gft)
GWS21_G.netDensity <- network.density(GWS21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_G.entropy <- entropy(GWS21_Gft) #entropy

GWS21_G.netMx <- cbind(GWS21_G.netMx, GWS21_G.clusterCoef, GWS21_G.degreeCent$centralization,
                       GWS21_G.netDensity, GWS21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "GWS"
KIoutcome = "Behind_F"
GWS21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
GWS21_Bg2 <- data.frame(GWS21_B)
GWS21_Bg2 <- GWS21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_Bg2$player1
player2vector <- GWS21_Bg2$player2
GWS21_Bg3 <- GWS21_Bg2
GWS21_Bg3$p1inp2vec <- is.element(GWS21_Bg3$player1, player2vector)
GWS21_Bg3$p2inp1vec <- is.element(GWS21_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS21_Bg3[ which(GWS21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS21_Bg2 <- rbind(GWS21_Bg2, addPlayer2)

#ROUND 21, Behind graph using weighted edges
GWS21_Bft <- ftable(GWS21_Bg2$player1, GWS21_Bg2$player2)
GWS21_Bft2 <- as.matrix(GWS21_Bft)
numRows <- nrow(GWS21_Bft2)
numCols <- ncol(GWS21_Bft2)
GWS21_Bft3 <- GWS21_Bft2[c(1:numRows) , c(2:numCols)]
GWS21_BTable <- graph.adjacency(GWS21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(GWS21_BTable, vertex.label = V(GWS21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
GWS21_B.clusterCoef <- transitivity(GWS21_BTable, type="global") #cluster coefficient
GWS21_B.degreeCent <- centralization.degree(GWS21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_Bftn <- as.network.matrix(GWS21_Bft)
GWS21_B.netDensity <- network.density(GWS21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_B.entropy <- entropy(GWS21_Bft) #entropy

GWS21_B.netMx <- cbind(GWS21_B.netMx, GWS21_B.clusterCoef, GWS21_B.degreeCent$centralization,
                       GWS21_B.netDensity, GWS21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
GWS21_SFg2 <- data.frame(GWS21_SF)
GWS21_SFg2 <- GWS21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_SFg2$player1
player2vector <- GWS21_SFg2$player2
GWS21_SFg3 <- GWS21_SFg2
GWS21_SFg3$p1inp2vec <- is.element(GWS21_SFg3$player1, player2vector)
GWS21_SFg3$p2inp1vec <- is.element(GWS21_SFg3$player2, player1vector)

addPlayer1 <- GWS21_SFg3[ which(GWS21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_SFg3[ which(GWS21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_SFg2 <- rbind(GWS21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
GWS21_SFft <- ftable(GWS21_SFg2$player1, GWS21_SFg2$player2)
GWS21_SFft2 <- as.matrix(GWS21_SFft)
numRows <- nrow(GWS21_SFft2)
numCols <- ncol(GWS21_SFft2)
GWS21_SFft3 <- GWS21_SFft2[c(2:numRows) , c(2:numCols)]
GWS21_SFTable <- graph.adjacency(GWS21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(GWS21_SFTable, vertex.label = V(GWS21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
GWS21_SF.clusterCoef <- transitivity(GWS21_SFTable, type="global") #cluster coefficient
GWS21_SF.degreeCent <- centralization.degree(GWS21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_SFftn <- as.network.matrix(GWS21_SFft)
GWS21_SF.netDensity <- network.density(GWS21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_SF.entropy <- entropy(GWS21_SFft) #entropy

GWS21_SF.netMx <- cbind(GWS21_SF.netMx, GWS21_SF.clusterCoef, GWS21_SF.degreeCent$centralization,
                        GWS21_SF.netDensity, GWS21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
GWS21_TFg2 <- data.frame(GWS21_TF)
GWS21_TFg2 <- GWS21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_TFg2$player1
player2vector <- GWS21_TFg2$player2
GWS21_TFg3 <- GWS21_TFg2
GWS21_TFg3$p1inp2vec <- is.element(GWS21_TFg3$player1, player2vector)
GWS21_TFg3$p2inp1vec <- is.element(GWS21_TFg3$player2, player1vector)

addPlayer1 <- GWS21_TFg3[ which(GWS21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_TFg3[ which(GWS21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_TFg2 <- rbind(GWS21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
GWS21_TFft <- ftable(GWS21_TFg2$player1, GWS21_TFg2$player2)
GWS21_TFft2 <- as.matrix(GWS21_TFft)
numRows <- nrow(GWS21_TFft2)
numCols <- ncol(GWS21_TFft2)
GWS21_TFft3 <- GWS21_TFft2[c(2:numRows) , c(2:numCols)]
GWS21_TFTable <- graph.adjacency(GWS21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(GWS21_TFTable, vertex.label = V(GWS21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
GWS21_TF.clusterCoef <- transitivity(GWS21_TFTable, type="global") #cluster coefficient
GWS21_TF.degreeCent <- centralization.degree(GWS21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_TFftn <- as.network.matrix(GWS21_TFft)
GWS21_TF.netDensity <- network.density(GWS21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_TF.entropy <- entropy(GWS21_TFft) #entropy

GWS21_TF.netMx <- cbind(GWS21_TF.netMx, GWS21_TF.clusterCoef, GWS21_TF.degreeCent$centralization,
                        GWS21_TF.netDensity, GWS21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
GWS21_SAMg2 <- data.frame(GWS21_SAM)
GWS21_SAMg2 <- GWS21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_SAMg2$player1
player2vector <- GWS21_SAMg2$player2
GWS21_SAMg3 <- GWS21_SAMg2
GWS21_SAMg3$p1inp2vec <- is.element(GWS21_SAMg3$player1, player2vector)
GWS21_SAMg3$p2inp1vec <- is.element(GWS21_SAMg3$player2, player1vector)

addPlayer1 <- GWS21_SAMg3[ which(GWS21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_SAMg3[ which(GWS21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_SAMg2 <- rbind(GWS21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
GWS21_SAMft <- ftable(GWS21_SAMg2$player1, GWS21_SAMg2$player2)
GWS21_SAMft2 <- as.matrix(GWS21_SAMft)
numRows <- nrow(GWS21_SAMft2)
numCols <- ncol(GWS21_SAMft2)
GWS21_SAMft3 <- GWS21_SAMft2[c(2:numRows) , c(2:numCols)]
GWS21_SAMTable <- graph.adjacency(GWS21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(GWS21_SAMTable, vertex.label = V(GWS21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
GWS21_SAM.clusterCoef <- transitivity(GWS21_SAMTable, type="global") #cluster coefficient
GWS21_SAM.degreeCent <- centralization.degree(GWS21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_SAMftn <- as.network.matrix(GWS21_SAMft)
GWS21_SAM.netDensity <- network.density(GWS21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_SAM.entropy <- entropy(GWS21_SAMft) #entropy

GWS21_SAM.netMx <- cbind(GWS21_SAM.netMx, GWS21_SAM.clusterCoef, GWS21_SAM.degreeCent$centralization,
                         GWS21_SAM.netDensity, GWS21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
GWS21_TAMg2 <- data.frame(GWS21_TAM)
GWS21_TAMg2 <- GWS21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_TAMg2$player1
player2vector <- GWS21_TAMg2$player2
GWS21_TAMg3 <- GWS21_TAMg2
GWS21_TAMg3$p1inp2vec <- is.element(GWS21_TAMg3$player1, player2vector)
GWS21_TAMg3$p2inp1vec <- is.element(GWS21_TAMg3$player2, player1vector)

addPlayer1 <- GWS21_TAMg3[ which(GWS21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS21_TAMg3[ which(GWS21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_TAMg2 <- rbind(GWS21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
GWS21_TAMft <- ftable(GWS21_TAMg2$player1, GWS21_TAMg2$player2)
GWS21_TAMft2 <- as.matrix(GWS21_TAMft)
numRows <- nrow(GWS21_TAMft2)
numCols <- ncol(GWS21_TAMft2)
GWS21_TAMft3 <- GWS21_TAMft2[c(2:numRows) , c(2:numCols)]
GWS21_TAMTable <- graph.adjacency(GWS21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(GWS21_TAMTable, vertex.label = V(GWS21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
GWS21_TAM.clusterCoef <- transitivity(GWS21_TAMTable, type="global") #cluster coefficient
GWS21_TAM.degreeCent <- centralization.degree(GWS21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_TAMftn <- as.network.matrix(GWS21_TAMft)
GWS21_TAM.netDensity <- network.density(GWS21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_TAM.entropy <- entropy(GWS21_TAMft) #entropy

GWS21_TAM.netMx <- cbind(GWS21_TAM.netMx, GWS21_TAM.clusterCoef, GWS21_TAM.degreeCent$centralization,
                         GWS21_TAM.netDensity, GWS21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
GWS21_SDMg2 <- data.frame(GWS21_SDM)
GWS21_SDMg2 <- GWS21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_SDMg2$player1
player2vector <- GWS21_SDMg2$player2
GWS21_SDMg3 <- GWS21_SDMg2
GWS21_SDMg3$p1inp2vec <- is.element(GWS21_SDMg3$player1, player2vector)
GWS21_SDMg3$p2inp1vec <- is.element(GWS21_SDMg3$player2, player1vector)

addPlayer1 <- GWS21_SDMg3[ which(GWS21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_SDMg3[ which(GWS21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_SDMg2 <- rbind(GWS21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
GWS21_SDMft <- ftable(GWS21_SDMg2$player1, GWS21_SDMg2$player2)
GWS21_SDMft2 <- as.matrix(GWS21_SDMft)
numRows <- nrow(GWS21_SDMft2)
numCols <- ncol(GWS21_SDMft2)
GWS21_SDMft3 <- GWS21_SDMft2[c(2:numRows) , c(2:numCols)]
GWS21_SDMTable <- graph.adjacency(GWS21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(GWS21_SDMTable, vertex.label = V(GWS21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
GWS21_SDM.clusterCoef <- transitivity(GWS21_SDMTable, type="global") #cluster coefficient
GWS21_SDM.degreeCent <- centralization.degree(GWS21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_SDMftn <- as.network.matrix(GWS21_SDMft)
GWS21_SDM.netDensity <- network.density(GWS21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_SDM.entropy <- entropy(GWS21_SDMft) #entropy

GWS21_SDM.netMx <- cbind(GWS21_SDM.netMx, GWS21_SDM.clusterCoef, GWS21_SDM.degreeCent$centralization,
                         GWS21_SDM.netDensity, GWS21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
GWS21_TDMg2 <- data.frame(GWS21_TDM)
GWS21_TDMg2 <- GWS21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_TDMg2$player1
player2vector <- GWS21_TDMg2$player2
GWS21_TDMg3 <- GWS21_TDMg2
GWS21_TDMg3$p1inp2vec <- is.element(GWS21_TDMg3$player1, player2vector)
GWS21_TDMg3$p2inp1vec <- is.element(GWS21_TDMg3$player2, player1vector)

addPlayer1 <- GWS21_TDMg3[ which(GWS21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_TDMg3[ which(GWS21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_TDMg2 <- rbind(GWS21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
GWS21_TDMft <- ftable(GWS21_TDMg2$player1, GWS21_TDMg2$player2)
GWS21_TDMft2 <- as.matrix(GWS21_TDMft)
numRows <- nrow(GWS21_TDMft2)
numCols <- ncol(GWS21_TDMft2)
GWS21_TDMft3 <- GWS21_TDMft2[c(2:numRows) , c(2:numCols)]
GWS21_TDMTable <- graph.adjacency(GWS21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(GWS21_TDMTable, vertex.label = V(GWS21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
GWS21_TDM.clusterCoef <- transitivity(GWS21_TDMTable, type="global") #cluster coefficient
GWS21_TDM.degreeCent <- centralization.degree(GWS21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_TDMftn <- as.network.matrix(GWS21_TDMft)
GWS21_TDM.netDensity <- network.density(GWS21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_TDM.entropy <- entropy(GWS21_TDMft) #entropy

GWS21_TDM.netMx <- cbind(GWS21_TDM.netMx, GWS21_TDM.clusterCoef, GWS21_TDM.degreeCent$centralization,
                         GWS21_TDM.netDensity, GWS21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
GWS21_SDg2 <- data.frame(GWS21_SD)
GWS21_SDg2 <- GWS21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_SDg2$player1
player2vector <- GWS21_SDg2$player2
GWS21_SDg3 <- GWS21_SDg2
GWS21_SDg3$p1inp2vec <- is.element(GWS21_SDg3$player1, player2vector)
GWS21_SDg3$p2inp1vec <- is.element(GWS21_SDg3$player2, player1vector)

addPlayer1 <- GWS21_SDg3[ which(GWS21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_SDg3[ which(GWS21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_SDg2 <- rbind(GWS21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
GWS21_SDft <- ftable(GWS21_SDg2$player1, GWS21_SDg2$player2)
GWS21_SDft2 <- as.matrix(GWS21_SDft)
numRows <- nrow(GWS21_SDft2)
numCols <- ncol(GWS21_SDft2)
GWS21_SDft3 <- GWS21_SDft2[c(2:numRows) , c(2:numCols)]
GWS21_SDTable <- graph.adjacency(GWS21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(GWS21_SDTable, vertex.label = V(GWS21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
GWS21_SD.clusterCoef <- transitivity(GWS21_SDTable, type="global") #cluster coefficient
GWS21_SD.degreeCent <- centralization.degree(GWS21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_SDftn <- as.network.matrix(GWS21_SDft)
GWS21_SD.netDensity <- network.density(GWS21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_SD.entropy <- entropy(GWS21_SDft) #entropy

GWS21_SD.netMx <- cbind(GWS21_SD.netMx, GWS21_SD.clusterCoef, GWS21_SD.degreeCent$centralization,
                        GWS21_SD.netDensity, GWS21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************

round = 21
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
GWS21_TDg2 <- data.frame(GWS21_TD)
GWS21_TDg2 <- GWS21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_TDg2$player1
player2vector <- GWS21_TDg2$player2
GWS21_TDg3 <- GWS21_TDg2
GWS21_TDg3$p1inp2vec <- is.element(GWS21_TDg3$player1, player2vector)
GWS21_TDg3$p2inp1vec <- is.element(GWS21_TDg3$player2, player1vector)

addPlayer1 <- GWS21_TDg3[ which(GWS21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS21_TDg3[ which(GWS21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_TDg2 <- rbind(GWS21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
GWS21_TDft <- ftable(GWS21_TDg2$player1, GWS21_TDg2$player2)
GWS21_TDft2 <- as.matrix(GWS21_TDft)
numRows <- nrow(GWS21_TDft2)
numCols <- ncol(GWS21_TDft2)
GWS21_TDft3 <- GWS21_TDft2[c(2:numRows) , c(2:numCols)]
GWS21_TDTable <- graph.adjacency(GWS21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(GWS21_TDTable, vertex.label = V(GWS21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
GWS21_TD.clusterCoef <- transitivity(GWS21_TDTable, type="global") #cluster coefficient
GWS21_TD.degreeCent <- centralization.degree(GWS21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_TDftn <- as.network.matrix(GWS21_TDft)
GWS21_TD.netDensity <- network.density(GWS21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_TD.entropy <- entropy(GWS21_TDft) #entropy

GWS21_TD.netMx <- cbind(GWS21_TD.netMx, GWS21_TD.clusterCoef, GWS21_TD.degreeCent$centralization,
                        GWS21_TD.netDensity, GWS21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
GWS21_QTg2 <- data.frame(GWS21_QT)
GWS21_QTg2 <- GWS21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS21_QTg2$player1
player2vector <- GWS21_QTg2$player2
GWS21_QTg3 <- GWS21_QTg2
GWS21_QTg3$p1inp2vec <- is.element(GWS21_QTg3$player1, player2vector)
GWS21_QTg3$p2inp1vec <- is.element(GWS21_QTg3$player2, player1vector)

addPlayer1 <- GWS21_QTg3[ which(GWS21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS21_QTg3[ which(GWS21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS21_QTg2 <- rbind(GWS21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
GWS21_QTft <- ftable(GWS21_QTg2$player1, GWS21_QTg2$player2)
GWS21_QTft2 <- as.matrix(GWS21_QTft)
numRows <- nrow(GWS21_QTft2)
numCols <- ncol(GWS21_QTft2)
GWS21_QTft3 <- GWS21_QTft2[c(2:numRows) , c(2:numCols)]
GWS21_QTTable <- graph.adjacency(GWS21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(GWS21_QTTable, vertex.label = V(GWS21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
GWS21_QT.clusterCoef <- transitivity(GWS21_QTTable, type="global") #cluster coefficient
GWS21_QT.degreeCent <- centralization.degree(GWS21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS21_QTftn <- as.network.matrix(GWS21_QTft)
GWS21_QT.netDensity <- network.density(GWS21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS21_QT.entropy <- entropy(GWS21_QTft) #entropy

GWS21_QT.netMx <- cbind(GWS21_QT.netMx, GWS21_QT.clusterCoef, GWS21_QT.degreeCent$centralization,
                        GWS21_QT.netDensity, GWS21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS21_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************
#NA

round = 21
teamName = "HAW"
KIoutcome = "Goal_F"
HAW21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
HAW21_Gg2 <- data.frame(HAW21_G)
HAW21_Gg2 <- HAW21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_Gg2$player1
player2vector <- HAW21_Gg2$player2
HAW21_Gg3 <- HAW21_Gg2
HAW21_Gg3$p1inp2vec <- is.element(HAW21_Gg3$player1, player2vector)
HAW21_Gg3$p2inp1vec <- is.element(HAW21_Gg3$player2, player1vector)

addPlayer1 <- HAW21_Gg3[ which(HAW21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_Gg3[ which(HAW21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_Gg2 <- rbind(HAW21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
HAW21_Gft <- ftable(HAW21_Gg2$player1, HAW21_Gg2$player2)
HAW21_Gft2 <- as.matrix(HAW21_Gft)
numRows <- nrow(HAW21_Gft2)
numCols <- ncol(HAW21_Gft2)
HAW21_Gft3 <- HAW21_Gft2[c(2:numRows) , c(2:numCols)]
HAW21_GTable <- graph.adjacency(HAW21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(HAW21_GTable, vertex.label = V(HAW21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
HAW21_G.clusterCoef <- transitivity(HAW21_GTable, type="global") #cluster coefficient
HAW21_G.degreeCent <- centralization.degree(HAW21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_Gftn <- as.network.matrix(HAW21_Gft)
HAW21_G.netDensity <- network.density(HAW21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_G.entropy <- entropy(HAW21_Gft) #entropy

HAW21_G.netMx <- cbind(HAW21_G.netMx, HAW21_G.clusterCoef, HAW21_G.degreeCent$centralization,
                       HAW21_G.netDensity, HAW21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "HAW"
KIoutcome = "Behind_F"
HAW21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
HAW21_Bg2 <- data.frame(HAW21_B)
HAW21_Bg2 <- HAW21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_Bg2$player1
player2vector <- HAW21_Bg2$player2
HAW21_Bg3 <- HAW21_Bg2
HAW21_Bg3$p1inp2vec <- is.element(HAW21_Bg3$player1, player2vector)
HAW21_Bg3$p2inp1vec <- is.element(HAW21_Bg3$player2, player1vector)

addPlayer1 <- HAW21_Bg3[ which(HAW21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW21_Bg2 <- rbind(HAW21_Bg2, addPlayer1)

#ROUND 21, Behind graph using weighted edges
HAW21_Bft <- ftable(HAW21_Bg2$player1, HAW21_Bg2$player2)
HAW21_Bft2 <- as.matrix(HAW21_Bft)
numRows <- nrow(HAW21_Bft2)
numCols <- ncol(HAW21_Bft2)
HAW21_Bft3 <- HAW21_Bft2[c(2:numRows) , c(1:numCols)]
HAW21_BTable <- graph.adjacency(HAW21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(HAW21_BTable, vertex.label = V(HAW21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
HAW21_B.clusterCoef <- transitivity(HAW21_BTable, type="global") #cluster coefficient
HAW21_B.degreeCent <- centralization.degree(HAW21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_Bftn <- as.network.matrix(HAW21_Bft)
HAW21_B.netDensity <- network.density(HAW21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_B.entropy <- entropy(HAW21_Bft) #entropy

HAW21_B.netMx <- cbind(HAW21_B.netMx, HAW21_B.clusterCoef, HAW21_B.degreeCent$centralization,
                       HAW21_B.netDensity, HAW21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
HAW21_SFg2 <- data.frame(HAW21_SF)
HAW21_SFg2 <- HAW21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_SFg2$player1
player2vector <- HAW21_SFg2$player2
HAW21_SFg3 <- HAW21_SFg2
HAW21_SFg3$p1inp2vec <- is.element(HAW21_SFg3$player1, player2vector)
HAW21_SFg3$p2inp1vec <- is.element(HAW21_SFg3$player2, player1vector)

addPlayer1 <- HAW21_SFg3[ which(HAW21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_SFg3[ which(HAW21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_SFg2 <- rbind(HAW21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
HAW21_SFft <- ftable(HAW21_SFg2$player1, HAW21_SFg2$player2)
HAW21_SFft2 <- as.matrix(HAW21_SFft)
numRows <- nrow(HAW21_SFft2)
numCols <- ncol(HAW21_SFft2)
HAW21_SFft3 <- HAW21_SFft2[c(2:numRows) , c(2:numCols)]
HAW21_SFTable <- graph.adjacency(HAW21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(HAW21_SFTable, vertex.label = V(HAW21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
HAW21_SF.clusterCoef <- transitivity(HAW21_SFTable, type="global") #cluster coefficient
HAW21_SF.degreeCent <- centralization.degree(HAW21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_SFftn <- as.network.matrix(HAW21_SFft)
HAW21_SF.netDensity <- network.density(HAW21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_SF.entropy <- entropy(HAW21_SFft) #entropy

HAW21_SF.netMx <- cbind(HAW21_SF.netMx, HAW21_SF.clusterCoef, HAW21_SF.degreeCent$centralization,
                        HAW21_SF.netDensity, HAW21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
HAW21_TFg2 <- data.frame(HAW21_TF)
HAW21_TFg2 <- HAW21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_TFg2$player1
player2vector <- HAW21_TFg2$player2
HAW21_TFg3 <- HAW21_TFg2
HAW21_TFg3$p1inp2vec <- is.element(HAW21_TFg3$player1, player2vector)
HAW21_TFg3$p2inp1vec <- is.element(HAW21_TFg3$player2, player1vector)

addPlayer1 <- HAW21_TFg3[ which(HAW21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_TFg3[ which(HAW21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_TFg2 <- rbind(HAW21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
HAW21_TFft <- ftable(HAW21_TFg2$player1, HAW21_TFg2$player2)
HAW21_TFft2 <- as.matrix(HAW21_TFft)
numRows <- nrow(HAW21_TFft2)
numCols <- ncol(HAW21_TFft2)
HAW21_TFft3 <- HAW21_TFft2[c(2:numRows) , c(2:numCols)]
HAW21_TFTable <- graph.adjacency(HAW21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(HAW21_TFTable, vertex.label = V(HAW21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
HAW21_TF.clusterCoef <- transitivity(HAW21_TFTable, type="global") #cluster coefficient
HAW21_TF.degreeCent <- centralization.degree(HAW21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_TFftn <- as.network.matrix(HAW21_TFft)
HAW21_TF.netDensity <- network.density(HAW21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_TF.entropy <- entropy(HAW21_TFft) #entropy

HAW21_TF.netMx <- cbind(HAW21_TF.netMx, HAW21_TF.clusterCoef, HAW21_TF.degreeCent$centralization,
                        HAW21_TF.netDensity, HAW21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
HAW21_SAMg2 <- data.frame(HAW21_SAM)
HAW21_SAMg2 <- HAW21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_SAMg2$player1
player2vector <- HAW21_SAMg2$player2
HAW21_SAMg3 <- HAW21_SAMg2
HAW21_SAMg3$p1inp2vec <- is.element(HAW21_SAMg3$player1, player2vector)
HAW21_SAMg3$p2inp1vec <- is.element(HAW21_SAMg3$player2, player1vector)

addPlayer1 <- HAW21_SAMg3[ which(HAW21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_SAMg3[ which(HAW21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_SAMg2 <- rbind(HAW21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
HAW21_SAMft <- ftable(HAW21_SAMg2$player1, HAW21_SAMg2$player2)
HAW21_SAMft2 <- as.matrix(HAW21_SAMft)
numRows <- nrow(HAW21_SAMft2)
numCols <- ncol(HAW21_SAMft2)
HAW21_SAMft3 <- HAW21_SAMft2[c(2:numRows) , c(2:numCols)]
HAW21_SAMTable <- graph.adjacency(HAW21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(HAW21_SAMTable, vertex.label = V(HAW21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
HAW21_SAM.clusterCoef <- transitivity(HAW21_SAMTable, type="global") #cluster coefficient
HAW21_SAM.degreeCent <- centralization.degree(HAW21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_SAMftn <- as.network.matrix(HAW21_SAMft)
HAW21_SAM.netDensity <- network.density(HAW21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_SAM.entropy <- entropy(HAW21_SAMft) #entropy

HAW21_SAM.netMx <- cbind(HAW21_SAM.netMx, HAW21_SAM.clusterCoef, HAW21_SAM.degreeCent$centralization,
                         HAW21_SAM.netDensity, HAW21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
HAW21_TAMg2 <- data.frame(HAW21_TAM)
HAW21_TAMg2 <- HAW21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_TAMg2$player1
player2vector <- HAW21_TAMg2$player2
HAW21_TAMg3 <- HAW21_TAMg2
HAW21_TAMg3$p1inp2vec <- is.element(HAW21_TAMg3$player1, player2vector)
HAW21_TAMg3$p2inp1vec <- is.element(HAW21_TAMg3$player2, player1vector)

addPlayer1 <- HAW21_TAMg3[ which(HAW21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_TAMg3[ which(HAW21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_TAMg2 <- rbind(HAW21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
HAW21_TAMft <- ftable(HAW21_TAMg2$player1, HAW21_TAMg2$player2)
HAW21_TAMft2 <- as.matrix(HAW21_TAMft)
numRows <- nrow(HAW21_TAMft2)
numCols <- ncol(HAW21_TAMft2)
HAW21_TAMft3 <- HAW21_TAMft2[c(2:numRows) , c(2:numCols)]
HAW21_TAMTable <- graph.adjacency(HAW21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(HAW21_TAMTable, vertex.label = V(HAW21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
HAW21_TAM.clusterCoef <- transitivity(HAW21_TAMTable, type="global") #cluster coefficient
HAW21_TAM.degreeCent <- centralization.degree(HAW21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_TAMftn <- as.network.matrix(HAW21_TAMft)
HAW21_TAM.netDensity <- network.density(HAW21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_TAM.entropy <- entropy(HAW21_TAMft) #entropy

HAW21_TAM.netMx <- cbind(HAW21_TAM.netMx, HAW21_TAM.clusterCoef, HAW21_TAM.degreeCent$centralization,
                         HAW21_TAM.netDensity, HAW21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
HAW21_SDMg2 <- data.frame(HAW21_SDM)
HAW21_SDMg2 <- HAW21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_SDMg2$player1
player2vector <- HAW21_SDMg2$player2
HAW21_SDMg3 <- HAW21_SDMg2
HAW21_SDMg3$p1inp2vec <- is.element(HAW21_SDMg3$player1, player2vector)
HAW21_SDMg3$p2inp1vec <- is.element(HAW21_SDMg3$player2, player1vector)

addPlayer1 <- HAW21_SDMg3[ which(HAW21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_SDMg3[ which(HAW21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_SDMg2 <- rbind(HAW21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
HAW21_SDMft <- ftable(HAW21_SDMg2$player1, HAW21_SDMg2$player2)
HAW21_SDMft2 <- as.matrix(HAW21_SDMft)
numRows <- nrow(HAW21_SDMft2)
numCols <- ncol(HAW21_SDMft2)
HAW21_SDMft3 <- HAW21_SDMft2[c(2:numRows) , c(2:numCols)]
HAW21_SDMTable <- graph.adjacency(HAW21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(HAW21_SDMTable, vertex.label = V(HAW21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
HAW21_SDM.clusterCoef <- transitivity(HAW21_SDMTable, type="global") #cluster coefficient
HAW21_SDM.degreeCent <- centralization.degree(HAW21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_SDMftn <- as.network.matrix(HAW21_SDMft)
HAW21_SDM.netDensity <- network.density(HAW21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_SDM.entropy <- entropy(HAW21_SDMft) #entropy

HAW21_SDM.netMx <- cbind(HAW21_SDM.netMx, HAW21_SDM.clusterCoef, HAW21_SDM.degreeCent$centralization,
                         HAW21_SDM.netDensity, HAW21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
HAW21_TDMg2 <- data.frame(HAW21_TDM)
HAW21_TDMg2 <- HAW21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_TDMg2$player1
player2vector <- HAW21_TDMg2$player2
HAW21_TDMg3 <- HAW21_TDMg2
HAW21_TDMg3$p1inp2vec <- is.element(HAW21_TDMg3$player1, player2vector)
HAW21_TDMg3$p2inp1vec <- is.element(HAW21_TDMg3$player2, player1vector)

addPlayer1 <- HAW21_TDMg3[ which(HAW21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_TDMg3[ which(HAW21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_TDMg2 <- rbind(HAW21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
HAW21_TDMft <- ftable(HAW21_TDMg2$player1, HAW21_TDMg2$player2)
HAW21_TDMft2 <- as.matrix(HAW21_TDMft)
numRows <- nrow(HAW21_TDMft2)
numCols <- ncol(HAW21_TDMft2)
HAW21_TDMft3 <- HAW21_TDMft2[c(2:numRows) , c(2:numCols)]
HAW21_TDMTable <- graph.adjacency(HAW21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(HAW21_TDMTable, vertex.label = V(HAW21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
HAW21_TDM.clusterCoef <- transitivity(HAW21_TDMTable, type="global") #cluster coefficient
HAW21_TDM.degreeCent <- centralization.degree(HAW21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_TDMftn <- as.network.matrix(HAW21_TDMft)
HAW21_TDM.netDensity <- network.density(HAW21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_TDM.entropy <- entropy(HAW21_TDMft) #entropy

HAW21_TDM.netMx <- cbind(HAW21_TDM.netMx, HAW21_TDM.clusterCoef, HAW21_TDM.degreeCent$centralization,
                         HAW21_TDM.netDensity, HAW21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
HAW21_SDg2 <- data.frame(HAW21_SD)
HAW21_SDg2 <- HAW21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_SDg2$player1
player2vector <- HAW21_SDg2$player2
HAW21_SDg3 <- HAW21_SDg2
HAW21_SDg3$p1inp2vec <- is.element(HAW21_SDg3$player1, player2vector)
HAW21_SDg3$p2inp1vec <- is.element(HAW21_SDg3$player2, player1vector)

addPlayer1 <- HAW21_SDg3[ which(HAW21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_SDg3[ which(HAW21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_SDg2 <- rbind(HAW21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
HAW21_SDft <- ftable(HAW21_SDg2$player1, HAW21_SDg2$player2)
HAW21_SDft2 <- as.matrix(HAW21_SDft)
numRows <- nrow(HAW21_SDft2)
numCols <- ncol(HAW21_SDft2)
HAW21_SDft3 <- HAW21_SDft2[c(2:numRows) , c(2:numCols)]
HAW21_SDTable <- graph.adjacency(HAW21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(HAW21_SDTable, vertex.label = V(HAW21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
HAW21_SD.clusterCoef <- transitivity(HAW21_SDTable, type="global") #cluster coefficient
HAW21_SD.degreeCent <- centralization.degree(HAW21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_SDftn <- as.network.matrix(HAW21_SDft)
HAW21_SD.netDensity <- network.density(HAW21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_SD.entropy <- entropy(HAW21_SDft) #entropy

HAW21_SD.netMx <- cbind(HAW21_SD.netMx, HAW21_SD.clusterCoef, HAW21_SD.degreeCent$centralization,
                        HAW21_SD.netDensity, HAW21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
HAW21_TDg2 <- data.frame(HAW21_TD)
HAW21_TDg2 <- HAW21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_TDg2$player1
player2vector <- HAW21_TDg2$player2
HAW21_TDg3 <- HAW21_TDg2
HAW21_TDg3$p1inp2vec <- is.element(HAW21_TDg3$player1, player2vector)
HAW21_TDg3$p2inp1vec <- is.element(HAW21_TDg3$player2, player1vector)

addPlayer1 <- HAW21_TDg3[ which(HAW21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_TDg3[ which(HAW21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_TDg2 <- rbind(HAW21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
HAW21_TDft <- ftable(HAW21_TDg2$player1, HAW21_TDg2$player2)
HAW21_TDft2 <- as.matrix(HAW21_TDft)
numRows <- nrow(HAW21_TDft2)
numCols <- ncol(HAW21_TDft2)
HAW21_TDft3 <- HAW21_TDft2[c(2:numRows) , c(2:numCols)]
HAW21_TDTable <- graph.adjacency(HAW21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(HAW21_TDTable, vertex.label = V(HAW21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
HAW21_TD.clusterCoef <- transitivity(HAW21_TDTable, type="global") #cluster coefficient
HAW21_TD.degreeCent <- centralization.degree(HAW21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_TDftn <- as.network.matrix(HAW21_TDft)
HAW21_TD.netDensity <- network.density(HAW21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_TD.entropy <- entropy(HAW21_TDft) #entropy

HAW21_TD.netMx <- cbind(HAW21_TD.netMx, HAW21_TD.clusterCoef, HAW21_TD.degreeCent$centralization,
                        HAW21_TD.netDensity, HAW21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
HAW21_QTg2 <- data.frame(HAW21_QT)
HAW21_QTg2 <- HAW21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW21_QTg2$player1
player2vector <- HAW21_QTg2$player2
HAW21_QTg3 <- HAW21_QTg2
HAW21_QTg3$p1inp2vec <- is.element(HAW21_QTg3$player1, player2vector)
HAW21_QTg3$p2inp1vec <- is.element(HAW21_QTg3$player2, player1vector)

addPlayer1 <- HAW21_QTg3[ which(HAW21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW21_QTg3[ which(HAW21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW21_QTg2 <- rbind(HAW21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
HAW21_QTft <- ftable(HAW21_QTg2$player1, HAW21_QTg2$player2)
HAW21_QTft2 <- as.matrix(HAW21_QTft)
numRows <- nrow(HAW21_QTft2)
numCols <- ncol(HAW21_QTft2)
HAW21_QTft3 <- HAW21_QTft2[c(2:numRows) , c(2:numCols)]
HAW21_QTTable <- graph.adjacency(HAW21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(HAW21_QTTable, vertex.label = V(HAW21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
HAW21_QT.clusterCoef <- transitivity(HAW21_QTTable, type="global") #cluster coefficient
HAW21_QT.degreeCent <- centralization.degree(HAW21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW21_QTftn <- as.network.matrix(HAW21_QTft)
HAW21_QT.netDensity <- network.density(HAW21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW21_QT.entropy <- entropy(HAW21_QTft) #entropy

HAW21_QT.netMx <- cbind(HAW21_QT.netMx, HAW21_QT.clusterCoef, HAW21_QT.degreeCent$centralization,
                        HAW21_QT.netDensity, HAW21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW21_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "MELB"
KIoutcome = "Goal_F"
MELB21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
MELB21_Gg2 <- data.frame(MELB21_G)
MELB21_Gg2 <- MELB21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_Gg2$player1
player2vector <- MELB21_Gg2$player2
MELB21_Gg3 <- MELB21_Gg2
MELB21_Gg3$p1inp2vec <- is.element(MELB21_Gg3$player1, player2vector)
MELB21_Gg3$p2inp1vec <- is.element(MELB21_Gg3$player2, player1vector)

addPlayer1 <- MELB21_Gg3[ which(MELB21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_Gg3[ which(MELB21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_Gg2 <- rbind(MELB21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
MELB21_Gft <- ftable(MELB21_Gg2$player1, MELB21_Gg2$player2)
MELB21_Gft2 <- as.matrix(MELB21_Gft)
numRows <- nrow(MELB21_Gft2)
numCols <- ncol(MELB21_Gft2)
MELB21_Gft3 <- MELB21_Gft2[c(2:numRows) , c(2:numCols)]
MELB21_GTable <- graph.adjacency(MELB21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(MELB21_GTable, vertex.label = V(MELB21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
MELB21_G.clusterCoef <- transitivity(MELB21_GTable, type="global") #cluster coefficient
MELB21_G.degreeCent <- centralization.degree(MELB21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_Gftn <- as.network.matrix(MELB21_Gft)
MELB21_G.netDensity <- network.density(MELB21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_G.entropy <- entropy(MELB21_Gft) #entropy

MELB21_G.netMx <- cbind(MELB21_G.netMx, MELB21_G.clusterCoef, MELB21_G.degreeCent$centralization,
                        MELB21_G.netDensity, MELB21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************

round = 21
teamName = "MELB"
KIoutcome = "Behind_F"
MELB21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
MELB21_Bg2 <- data.frame(MELB21_B)
MELB21_Bg2 <- MELB21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_Bg2$player1
player2vector <- MELB21_Bg2$player2
MELB21_Bg3 <- MELB21_Bg2
MELB21_Bg3$p1inp2vec <- is.element(MELB21_Bg3$player1, player2vector)
MELB21_Bg3$p2inp1vec <- is.element(MELB21_Bg3$player2, player1vector)

addPlayer1 <- MELB21_Bg3[ which(MELB21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_Bg3[ which(MELB21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_Bg2 <- rbind(MELB21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
MELB21_Bft <- ftable(MELB21_Bg2$player1, MELB21_Bg2$player2)
MELB21_Bft2 <- as.matrix(MELB21_Bft)
numRows <- nrow(MELB21_Bft2)
numCols <- ncol(MELB21_Bft2)
MELB21_Bft3 <- MELB21_Bft2[c(2:numRows) , c(2:numCols)]
MELB21_BTable <- graph.adjacency(MELB21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(MELB21_BTable, vertex.label = V(MELB21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
MELB21_B.clusterCoef <- transitivity(MELB21_BTable, type="global") #cluster coefficient
MELB21_B.degreeCent <- centralization.degree(MELB21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_Bftn <- as.network.matrix(MELB21_Bft)
MELB21_B.netDensity <- network.density(MELB21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_B.entropy <- entropy(MELB21_Bft) #entropy

MELB21_B.netMx <- cbind(MELB21_B.netMx, MELB21_B.clusterCoef, MELB21_B.degreeCent$centralization,
                        MELB21_B.netDensity, MELB21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
MELB21_SFg2 <- data.frame(MELB21_SF)
MELB21_SFg2 <- MELB21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_SFg2$player1
player2vector <- MELB21_SFg2$player2
MELB21_SFg3 <- MELB21_SFg2
MELB21_SFg3$p1inp2vec <- is.element(MELB21_SFg3$player1, player2vector)
MELB21_SFg3$p2inp1vec <- is.element(MELB21_SFg3$player2, player1vector)

addPlayer1 <- MELB21_SFg3[ which(MELB21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB21_SFg3[ which(MELB21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_SFg2 <- rbind(MELB21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
MELB21_SFft <- ftable(MELB21_SFg2$player1, MELB21_SFg2$player2)
MELB21_SFft2 <- as.matrix(MELB21_SFft)
numRows <- nrow(MELB21_SFft2)
numCols <- ncol(MELB21_SFft2)
MELB21_SFft3 <- MELB21_SFft2[c(2:numRows) , c(2:numCols)]
MELB21_SFTable <- graph.adjacency(MELB21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(MELB21_SFTable, vertex.label = V(MELB21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
MELB21_SF.clusterCoef <- transitivity(MELB21_SFTable, type="global") #cluster coefficient
MELB21_SF.degreeCent <- centralization.degree(MELB21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_SFftn <- as.network.matrix(MELB21_SFft)
MELB21_SF.netDensity <- network.density(MELB21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_SF.entropy <- entropy(MELB21_SFft) #entropy

MELB21_SF.netMx <- cbind(MELB21_SF.netMx, MELB21_SF.clusterCoef, MELB21_SF.degreeCent$centralization,
                         MELB21_SF.netDensity, MELB21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
MELB21_TFg2 <- data.frame(MELB21_TF)
MELB21_TFg2 <- MELB21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_TFg2$player1
player2vector <- MELB21_TFg2$player2
MELB21_TFg3 <- MELB21_TFg2
MELB21_TFg3$p1inp2vec <- is.element(MELB21_TFg3$player1, player2vector)
MELB21_TFg3$p2inp1vec <- is.element(MELB21_TFg3$player2, player1vector)

addPlayer1 <- MELB21_TFg3[ which(MELB21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_TFg3[ which(MELB21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_TFg2 <- rbind(MELB21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
MELB21_TFft <- ftable(MELB21_TFg2$player1, MELB21_TFg2$player2)
MELB21_TFft2 <- as.matrix(MELB21_TFft)
numRows <- nrow(MELB21_TFft2)
numCols <- ncol(MELB21_TFft2)
MELB21_TFft3 <- MELB21_TFft2[c(2:numRows) , c(2:numCols)]
MELB21_TFTable <- graph.adjacency(MELB21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(MELB21_TFTable, vertex.label = V(MELB21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
MELB21_TF.clusterCoef <- transitivity(MELB21_TFTable, type="global") #cluster coefficient
MELB21_TF.degreeCent <- centralization.degree(MELB21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_TFftn <- as.network.matrix(MELB21_TFft)
MELB21_TF.netDensity <- network.density(MELB21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_TF.entropy <- entropy(MELB21_TFft) #entropy

MELB21_TF.netMx <- cbind(MELB21_TF.netMx, MELB21_TF.clusterCoef, MELB21_TF.degreeCent$centralization,
                         MELB21_TF.netDensity, MELB21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
MELB21_SAMg2 <- data.frame(MELB21_SAM)
MELB21_SAMg2 <- MELB21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_SAMg2$player1
player2vector <- MELB21_SAMg2$player2
MELB21_SAMg3 <- MELB21_SAMg2
MELB21_SAMg3$p1inp2vec <- is.element(MELB21_SAMg3$player1, player2vector)
MELB21_SAMg3$p2inp1vec <- is.element(MELB21_SAMg3$player2, player1vector)

addPlayer1 <- MELB21_SAMg3[ which(MELB21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_SAMg3[ which(MELB21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_SAMg2 <- rbind(MELB21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
MELB21_SAMft <- ftable(MELB21_SAMg2$player1, MELB21_SAMg2$player2)
MELB21_SAMft2 <- as.matrix(MELB21_SAMft)
numRows <- nrow(MELB21_SAMft2)
numCols <- ncol(MELB21_SAMft2)
MELB21_SAMft3 <- MELB21_SAMft2[c(2:numRows) , c(2:numCols)]
MELB21_SAMTable <- graph.adjacency(MELB21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(MELB21_SAMTable, vertex.label = V(MELB21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
MELB21_SAM.clusterCoef <- transitivity(MELB21_SAMTable, type="global") #cluster coefficient
MELB21_SAM.degreeCent <- centralization.degree(MELB21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_SAMftn <- as.network.matrix(MELB21_SAMft)
MELB21_SAM.netDensity <- network.density(MELB21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_SAM.entropy <- entropy(MELB21_SAMft) #entropy

MELB21_SAM.netMx <- cbind(MELB21_SAM.netMx, MELB21_SAM.clusterCoef, MELB21_SAM.degreeCent$centralization,
                          MELB21_SAM.netDensity, MELB21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
MELB21_TAMg2 <- data.frame(MELB21_TAM)
MELB21_TAMg2 <- MELB21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_TAMg2$player1
player2vector <- MELB21_TAMg2$player2
MELB21_TAMg3 <- MELB21_TAMg2
MELB21_TAMg3$p1inp2vec <- is.element(MELB21_TAMg3$player1, player2vector)
MELB21_TAMg3$p2inp1vec <- is.element(MELB21_TAMg3$player2, player1vector)

addPlayer1 <- MELB21_TAMg3[ which(MELB21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_TAMg3[ which(MELB21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_TAMg2 <- rbind(MELB21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
MELB21_TAMft <- ftable(MELB21_TAMg2$player1, MELB21_TAMg2$player2)
MELB21_TAMft2 <- as.matrix(MELB21_TAMft)
numRows <- nrow(MELB21_TAMft2)
numCols <- ncol(MELB21_TAMft2)
MELB21_TAMft3 <- MELB21_TAMft2[c(2:numRows) , c(2:numCols)]
MELB21_TAMTable <- graph.adjacency(MELB21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(MELB21_TAMTable, vertex.label = V(MELB21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
MELB21_TAM.clusterCoef <- transitivity(MELB21_TAMTable, type="global") #cluster coefficient
MELB21_TAM.degreeCent <- centralization.degree(MELB21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_TAMftn <- as.network.matrix(MELB21_TAMft)
MELB21_TAM.netDensity <- network.density(MELB21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_TAM.entropy <- entropy(MELB21_TAMft) #entropy

MELB21_TAM.netMx <- cbind(MELB21_TAM.netMx, MELB21_TAM.clusterCoef, MELB21_TAM.degreeCent$centralization,
                          MELB21_TAM.netDensity, MELB21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
MELB21_SDMg2 <- data.frame(MELB21_SDM)
MELB21_SDMg2 <- MELB21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_SDMg2$player1
player2vector <- MELB21_SDMg2$player2
MELB21_SDMg3 <- MELB21_SDMg2
MELB21_SDMg3$p1inp2vec <- is.element(MELB21_SDMg3$player1, player2vector)
MELB21_SDMg3$p2inp1vec <- is.element(MELB21_SDMg3$player2, player1vector)

addPlayer1 <- MELB21_SDMg3[ which(MELB21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_SDMg3[ which(MELB21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_SDMg2 <- rbind(MELB21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
MELB21_SDMft <- ftable(MELB21_SDMg2$player1, MELB21_SDMg2$player2)
MELB21_SDMft2 <- as.matrix(MELB21_SDMft)
numRows <- nrow(MELB21_SDMft2)
numCols <- ncol(MELB21_SDMft2)
MELB21_SDMft3 <- MELB21_SDMft2[c(2:numRows) , c(2:numCols)]
MELB21_SDMTable <- graph.adjacency(MELB21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(MELB21_SDMTable, vertex.label = V(MELB21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
MELB21_SDM.clusterCoef <- transitivity(MELB21_SDMTable, type="global") #cluster coefficient
MELB21_SDM.degreeCent <- centralization.degree(MELB21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_SDMftn <- as.network.matrix(MELB21_SDMft)
MELB21_SDM.netDensity <- network.density(MELB21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_SDM.entropy <- entropy(MELB21_SDMft) #entropy

MELB21_SDM.netMx <- cbind(MELB21_SDM.netMx, MELB21_SDM.clusterCoef, MELB21_SDM.degreeCent$centralization,
                          MELB21_SDM.netDensity, MELB21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
MELB21_TDMg2 <- data.frame(MELB21_TDM)
MELB21_TDMg2 <- MELB21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_TDMg2$player1
player2vector <- MELB21_TDMg2$player2
MELB21_TDMg3 <- MELB21_TDMg2
MELB21_TDMg3$p1inp2vec <- is.element(MELB21_TDMg3$player1, player2vector)
MELB21_TDMg3$p2inp1vec <- is.element(MELB21_TDMg3$player2, player1vector)

addPlayer1 <- MELB21_TDMg3[ which(MELB21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_TDMg3[ which(MELB21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_TDMg2 <- rbind(MELB21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
MELB21_TDMft <- ftable(MELB21_TDMg2$player1, MELB21_TDMg2$player2)
MELB21_TDMft2 <- as.matrix(MELB21_TDMft)
numRows <- nrow(MELB21_TDMft2)
numCols <- ncol(MELB21_TDMft2)
MELB21_TDMft3 <- MELB21_TDMft2[c(2:numRows) , c(2:numCols)]
MELB21_TDMTable <- graph.adjacency(MELB21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(MELB21_TDMTable, vertex.label = V(MELB21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
MELB21_TDM.clusterCoef <- transitivity(MELB21_TDMTable, type="global") #cluster coefficient
MELB21_TDM.degreeCent <- centralization.degree(MELB21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_TDMftn <- as.network.matrix(MELB21_TDMft)
MELB21_TDM.netDensity <- network.density(MELB21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_TDM.entropy <- entropy(MELB21_TDMft) #entropy

MELB21_TDM.netMx <- cbind(MELB21_TDM.netMx, MELB21_TDM.clusterCoef, MELB21_TDM.degreeCent$centralization,
                          MELB21_TDM.netDensity, MELB21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
MELB21_SDg2 <- data.frame(MELB21_SD)
MELB21_SDg2 <- MELB21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_SDg2$player1
player2vector <- MELB21_SDg2$player2
MELB21_SDg3 <- MELB21_SDg2
MELB21_SDg3$p1inp2vec <- is.element(MELB21_SDg3$player1, player2vector)
MELB21_SDg3$p2inp1vec <- is.element(MELB21_SDg3$player2, player1vector)

addPlayer1 <- MELB21_SDg3[ which(MELB21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_SDg3[ which(MELB21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_SDg2 <- rbind(MELB21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
MELB21_SDft <- ftable(MELB21_SDg2$player1, MELB21_SDg2$player2)
MELB21_SDft2 <- as.matrix(MELB21_SDft)
numRows <- nrow(MELB21_SDft2)
numCols <- ncol(MELB21_SDft2)
MELB21_SDft3 <- MELB21_SDft2[c(2:numRows) , c(2:numCols)]
MELB21_SDTable <- graph.adjacency(MELB21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(MELB21_SDTable, vertex.label = V(MELB21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
MELB21_SD.clusterCoef <- transitivity(MELB21_SDTable, type="global") #cluster coefficient
MELB21_SD.degreeCent <- centralization.degree(MELB21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_SDftn <- as.network.matrix(MELB21_SDft)
MELB21_SD.netDensity <- network.density(MELB21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_SD.entropy <- entropy(MELB21_SDft) #entropy

MELB21_SD.netMx <- cbind(MELB21_SD.netMx, MELB21_SD.clusterCoef, MELB21_SD.degreeCent$centralization,
                         MELB21_SD.netDensity, MELB21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
MELB21_TDg2 <- data.frame(MELB21_TD)
MELB21_TDg2 <- MELB21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_TDg2$player1
player2vector <- MELB21_TDg2$player2
MELB21_TDg3 <- MELB21_TDg2
MELB21_TDg3$p1inp2vec <- is.element(MELB21_TDg3$player1, player2vector)
MELB21_TDg3$p2inp1vec <- is.element(MELB21_TDg3$player2, player1vector)

addPlayer1 <- MELB21_TDg3[ which(MELB21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_TDg3[ which(MELB21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_TDg2 <- rbind(MELB21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
MELB21_TDft <- ftable(MELB21_TDg2$player1, MELB21_TDg2$player2)
MELB21_TDft2 <- as.matrix(MELB21_TDft)
numRows <- nrow(MELB21_TDft2)
numCols <- ncol(MELB21_TDft2)
MELB21_TDft3 <- MELB21_TDft2[c(2:numRows) , c(2:numCols)]
MELB21_TDTable <- graph.adjacency(MELB21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(MELB21_TDTable, vertex.label = V(MELB21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
MELB21_TD.clusterCoef <- transitivity(MELB21_TDTable, type="global") #cluster coefficient
MELB21_TD.degreeCent <- centralization.degree(MELB21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_TDftn <- as.network.matrix(MELB21_TDft)
MELB21_TD.netDensity <- network.density(MELB21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_TD.entropy <- entropy(MELB21_TDft) #entropy

MELB21_TD.netMx <- cbind(MELB21_TD.netMx, MELB21_TD.clusterCoef, MELB21_TD.degreeCent$centralization,
                         MELB21_TD.netDensity, MELB21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
MELB21_QTg2 <- data.frame(MELB21_QT)
MELB21_QTg2 <- MELB21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB21_QTg2$player1
player2vector <- MELB21_QTg2$player2
MELB21_QTg3 <- MELB21_QTg2
MELB21_QTg3$p1inp2vec <- is.element(MELB21_QTg3$player1, player2vector)
MELB21_QTg3$p2inp1vec <- is.element(MELB21_QTg3$player2, player1vector)

addPlayer1 <- MELB21_QTg3[ which(MELB21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB21_QTg3[ which(MELB21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB21_QTg2 <- rbind(MELB21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
MELB21_QTft <- ftable(MELB21_QTg2$player1, MELB21_QTg2$player2)
MELB21_QTft2 <- as.matrix(MELB21_QTft)
numRows <- nrow(MELB21_QTft2)
numCols <- ncol(MELB21_QTft2)
MELB21_QTft3 <- MELB21_QTft2[c(2:numRows) , c(2:numCols)]
MELB21_QTTable <- graph.adjacency(MELB21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(MELB21_QTTable, vertex.label = V(MELB21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
MELB21_QT.clusterCoef <- transitivity(MELB21_QTTable, type="global") #cluster coefficient
MELB21_QT.degreeCent <- centralization.degree(MELB21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB21_QTftn <- as.network.matrix(MELB21_QTft)
MELB21_QT.netDensity <- network.density(MELB21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB21_QT.entropy <- entropy(MELB21_QTft) #entropy

MELB21_QT.netMx <- cbind(MELB21_QT.netMx, MELB21_QT.clusterCoef, MELB21_QT.degreeCent$centralization,
                         MELB21_QT.netDensity, MELB21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB21_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
NMFC21_Gg2 <- data.frame(NMFC21_G)
NMFC21_Gg2 <- NMFC21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_Gg2$player1
player2vector <- NMFC21_Gg2$player2
NMFC21_Gg3 <- NMFC21_Gg2
NMFC21_Gg3$p1inp2vec <- is.element(NMFC21_Gg3$player1, player2vector)
NMFC21_Gg3$p2inp1vec <- is.element(NMFC21_Gg3$player2, player1vector)

addPlayer1 <- NMFC21_Gg3[ which(NMFC21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_Gg3[ which(NMFC21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_Gg2 <- rbind(NMFC21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
NMFC21_Gft <- ftable(NMFC21_Gg2$player1, NMFC21_Gg2$player2)
NMFC21_Gft2 <- as.matrix(NMFC21_Gft)
numRows <- nrow(NMFC21_Gft2)
numCols <- ncol(NMFC21_Gft2)
NMFC21_Gft3 <- NMFC21_Gft2[c(2:numRows) , c(2:numCols)]
NMFC21_GTable <- graph.adjacency(NMFC21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(NMFC21_GTable, vertex.label = V(NMFC21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
NMFC21_G.clusterCoef <- transitivity(NMFC21_GTable, type="global") #cluster coefficient
NMFC21_G.degreeCent <- centralization.degree(NMFC21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_Gftn <- as.network.matrix(NMFC21_Gft)
NMFC21_G.netDensity <- network.density(NMFC21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_G.entropy <- entropy(NMFC21_Gft) #entropy

NMFC21_G.netMx <- cbind(NMFC21_G.netMx, NMFC21_G.clusterCoef, NMFC21_G.degreeCent$centralization,
                        NMFC21_G.netDensity, NMFC21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************

round = 21
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
NMFC21_Bg2 <- data.frame(NMFC21_B)
NMFC21_Bg2 <- NMFC21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_Bg2$player1
player2vector <- NMFC21_Bg2$player2
NMFC21_Bg3 <- NMFC21_Bg2
NMFC21_Bg3$p1inp2vec <- is.element(NMFC21_Bg3$player1, player2vector)
NMFC21_Bg3$p2inp1vec <- is.element(NMFC21_Bg3$player2, player1vector)

addPlayer1 <- NMFC21_Bg3[ which(NMFC21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC21_Bg3[ which(NMFC21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_Bg2 <- rbind(NMFC21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
NMFC21_Bft <- ftable(NMFC21_Bg2$player1, NMFC21_Bg2$player2)
NMFC21_Bft2 <- as.matrix(NMFC21_Bft)
numRows <- nrow(NMFC21_Bft2)
numCols <- ncol(NMFC21_Bft2)
NMFC21_Bft3 <- NMFC21_Bft2[c(2:numRows) , c(2:numCols)]
NMFC21_BTable <- graph.adjacency(NMFC21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(NMFC21_BTable, vertex.label = V(NMFC21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
NMFC21_B.clusterCoef <- transitivity(NMFC21_BTable, type="global") #cluster coefficient
NMFC21_B.degreeCent <- centralization.degree(NMFC21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_Bftn <- as.network.matrix(NMFC21_Bft)
NMFC21_B.netDensity <- network.density(NMFC21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_B.entropy <- entropy(NMFC21_Bft) #entropy

NMFC21_B.netMx <- cbind(NMFC21_B.netMx, NMFC21_B.clusterCoef, NMFC21_B.degreeCent$centralization,
                        NMFC21_B.netDensity, NMFC21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
NMFC21_SFg2 <- data.frame(NMFC21_SF)
NMFC21_SFg2 <- NMFC21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_SFg2$player1
player2vector <- NMFC21_SFg2$player2
NMFC21_SFg3 <- NMFC21_SFg2
NMFC21_SFg3$p1inp2vec <- is.element(NMFC21_SFg3$player1, player2vector)
NMFC21_SFg3$p2inp1vec <- is.element(NMFC21_SFg3$player2, player1vector)

addPlayer1 <- NMFC21_SFg3[ which(NMFC21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_SFg3[ which(NMFC21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_SFg2 <- rbind(NMFC21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
NMFC21_SFft <- ftable(NMFC21_SFg2$player1, NMFC21_SFg2$player2)
NMFC21_SFft2 <- as.matrix(NMFC21_SFft)
numRows <- nrow(NMFC21_SFft2)
numCols <- ncol(NMFC21_SFft2)
NMFC21_SFft3 <- NMFC21_SFft2[c(2:numRows) , c(2:numCols)]
NMFC21_SFTable <- graph.adjacency(NMFC21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(NMFC21_SFTable, vertex.label = V(NMFC21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
NMFC21_SF.clusterCoef <- transitivity(NMFC21_SFTable, type="global") #cluster coefficient
NMFC21_SF.degreeCent <- centralization.degree(NMFC21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_SFftn <- as.network.matrix(NMFC21_SFft)
NMFC21_SF.netDensity <- network.density(NMFC21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_SF.entropy <- entropy(NMFC21_SFft) #entropy

NMFC21_SF.netMx <- cbind(NMFC21_SF.netMx, NMFC21_SF.clusterCoef, NMFC21_SF.degreeCent$centralization,
                         NMFC21_SF.netDensity, NMFC21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
NMFC21_TFg2 <- data.frame(NMFC21_TF)
NMFC21_TFg2 <- NMFC21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_TFg2$player1
player2vector <- NMFC21_TFg2$player2
NMFC21_TFg3 <- NMFC21_TFg2
NMFC21_TFg3$p1inp2vec <- is.element(NMFC21_TFg3$player1, player2vector)
NMFC21_TFg3$p2inp1vec <- is.element(NMFC21_TFg3$player2, player1vector)

addPlayer1 <- NMFC21_TFg3[ which(NMFC21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_TFg3[ which(NMFC21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_TFg2 <- rbind(NMFC21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
NMFC21_TFft <- ftable(NMFC21_TFg2$player1, NMFC21_TFg2$player2)
NMFC21_TFft2 <- as.matrix(NMFC21_TFft)
numRows <- nrow(NMFC21_TFft2)
numCols <- ncol(NMFC21_TFft2)
NMFC21_TFft3 <- NMFC21_TFft2[c(2:numRows) , c(2:numCols)]
NMFC21_TFTable <- graph.adjacency(NMFC21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(NMFC21_TFTable, vertex.label = V(NMFC21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
NMFC21_TF.clusterCoef <- transitivity(NMFC21_TFTable, type="global") #cluster coefficient
NMFC21_TF.degreeCent <- centralization.degree(NMFC21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_TFftn <- as.network.matrix(NMFC21_TFft)
NMFC21_TF.netDensity <- network.density(NMFC21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_TF.entropy <- entropy(NMFC21_TFft) #entropy

NMFC21_TF.netMx <- cbind(NMFC21_TF.netMx, NMFC21_TF.clusterCoef, NMFC21_TF.degreeCent$centralization,
                         NMFC21_TF.netDensity, NMFC21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
NMFC21_SAMg2 <- data.frame(NMFC21_SAM)
NMFC21_SAMg2 <- NMFC21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_SAMg2$player1
player2vector <- NMFC21_SAMg2$player2
NMFC21_SAMg3 <- NMFC21_SAMg2
NMFC21_SAMg3$p1inp2vec <- is.element(NMFC21_SAMg3$player1, player2vector)
NMFC21_SAMg3$p2inp1vec <- is.element(NMFC21_SAMg3$player2, player1vector)

addPlayer1 <- NMFC21_SAMg3[ which(NMFC21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_SAMg3[ which(NMFC21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_SAMg2 <- rbind(NMFC21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
NMFC21_SAMft <- ftable(NMFC21_SAMg2$player1, NMFC21_SAMg2$player2)
NMFC21_SAMft2 <- as.matrix(NMFC21_SAMft)
numRows <- nrow(NMFC21_SAMft2)
numCols <- ncol(NMFC21_SAMft2)
NMFC21_SAMft3 <- NMFC21_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC21_SAMTable <- graph.adjacency(NMFC21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(NMFC21_SAMTable, vertex.label = V(NMFC21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
NMFC21_SAM.clusterCoef <- transitivity(NMFC21_SAMTable, type="global") #cluster coefficient
NMFC21_SAM.degreeCent <- centralization.degree(NMFC21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_SAMftn <- as.network.matrix(NMFC21_SAMft)
NMFC21_SAM.netDensity <- network.density(NMFC21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_SAM.entropy <- entropy(NMFC21_SAMft) #entropy

NMFC21_SAM.netMx <- cbind(NMFC21_SAM.netMx, NMFC21_SAM.clusterCoef, NMFC21_SAM.degreeCent$centralization,
                          NMFC21_SAM.netDensity, NMFC21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
NMFC21_TAMg2 <- data.frame(NMFC21_TAM)
NMFC21_TAMg2 <- NMFC21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_TAMg2$player1
player2vector <- NMFC21_TAMg2$player2
NMFC21_TAMg3 <- NMFC21_TAMg2
NMFC21_TAMg3$p1inp2vec <- is.element(NMFC21_TAMg3$player1, player2vector)
NMFC21_TAMg3$p2inp1vec <- is.element(NMFC21_TAMg3$player2, player1vector)

addPlayer1 <- NMFC21_TAMg3[ which(NMFC21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_TAMg3[ which(NMFC21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_TAMg2 <- rbind(NMFC21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
NMFC21_TAMft <- ftable(NMFC21_TAMg2$player1, NMFC21_TAMg2$player2)
NMFC21_TAMft2 <- as.matrix(NMFC21_TAMft)
numRows <- nrow(NMFC21_TAMft2)
numCols <- ncol(NMFC21_TAMft2)
NMFC21_TAMft3 <- NMFC21_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC21_TAMTable <- graph.adjacency(NMFC21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(NMFC21_TAMTable, vertex.label = V(NMFC21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
NMFC21_TAM.clusterCoef <- transitivity(NMFC21_TAMTable, type="global") #cluster coefficient
NMFC21_TAM.degreeCent <- centralization.degree(NMFC21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_TAMftn <- as.network.matrix(NMFC21_TAMft)
NMFC21_TAM.netDensity <- network.density(NMFC21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_TAM.entropy <- entropy(NMFC21_TAMft) #entropy

NMFC21_TAM.netMx <- cbind(NMFC21_TAM.netMx, NMFC21_TAM.clusterCoef, NMFC21_TAM.degreeCent$centralization,
                          NMFC21_TAM.netDensity, NMFC21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
NMFC21_SDMg2 <- data.frame(NMFC21_SDM)
NMFC21_SDMg2 <- NMFC21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_SDMg2$player1
player2vector <- NMFC21_SDMg2$player2
NMFC21_SDMg3 <- NMFC21_SDMg2
NMFC21_SDMg3$p1inp2vec <- is.element(NMFC21_SDMg3$player1, player2vector)
NMFC21_SDMg3$p2inp1vec <- is.element(NMFC21_SDMg3$player2, player1vector)

addPlayer1 <- NMFC21_SDMg3[ which(NMFC21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_SDMg3[ which(NMFC21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_SDMg2 <- rbind(NMFC21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
NMFC21_SDMft <- ftable(NMFC21_SDMg2$player1, NMFC21_SDMg2$player2)
NMFC21_SDMft2 <- as.matrix(NMFC21_SDMft)
numRows <- nrow(NMFC21_SDMft2)
numCols <- ncol(NMFC21_SDMft2)
NMFC21_SDMft3 <- NMFC21_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC21_SDMTable <- graph.adjacency(NMFC21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(NMFC21_SDMTable, vertex.label = V(NMFC21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
NMFC21_SDM.clusterCoef <- transitivity(NMFC21_SDMTable, type="global") #cluster coefficient
NMFC21_SDM.degreeCent <- centralization.degree(NMFC21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_SDMftn <- as.network.matrix(NMFC21_SDMft)
NMFC21_SDM.netDensity <- network.density(NMFC21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_SDM.entropy <- entropy(NMFC21_SDMft) #entropy

NMFC21_SDM.netMx <- cbind(NMFC21_SDM.netMx, NMFC21_SDM.clusterCoef, NMFC21_SDM.degreeCent$centralization,
                          NMFC21_SDM.netDensity, NMFC21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
NMFC21_TDMg2 <- data.frame(NMFC21_TDM)
NMFC21_TDMg2 <- NMFC21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_TDMg2$player1
player2vector <- NMFC21_TDMg2$player2
NMFC21_TDMg3 <- NMFC21_TDMg2
NMFC21_TDMg3$p1inp2vec <- is.element(NMFC21_TDMg3$player1, player2vector)
NMFC21_TDMg3$p2inp1vec <- is.element(NMFC21_TDMg3$player2, player1vector)

addPlayer1 <- NMFC21_TDMg3[ which(NMFC21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_TDMg3[ which(NMFC21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_TDMg2 <- rbind(NMFC21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
NMFC21_TDMft <- ftable(NMFC21_TDMg2$player1, NMFC21_TDMg2$player2)
NMFC21_TDMft2 <- as.matrix(NMFC21_TDMft)
numRows <- nrow(NMFC21_TDMft2)
numCols <- ncol(NMFC21_TDMft2)
NMFC21_TDMft3 <- NMFC21_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC21_TDMTable <- graph.adjacency(NMFC21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(NMFC21_TDMTable, vertex.label = V(NMFC21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
NMFC21_TDM.clusterCoef <- transitivity(NMFC21_TDMTable, type="global") #cluster coefficient
NMFC21_TDM.degreeCent <- centralization.degree(NMFC21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_TDMftn <- as.network.matrix(NMFC21_TDMft)
NMFC21_TDM.netDensity <- network.density(NMFC21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_TDM.entropy <- entropy(NMFC21_TDMft) #entropy

NMFC21_TDM.netMx <- cbind(NMFC21_TDM.netMx, NMFC21_TDM.clusterCoef, NMFC21_TDM.degreeCent$centralization,
                          NMFC21_TDM.netDensity, NMFC21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
NMFC21_SDg2 <- data.frame(NMFC21_SD)
NMFC21_SDg2 <- NMFC21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_SDg2$player1
player2vector <- NMFC21_SDg2$player2
NMFC21_SDg3 <- NMFC21_SDg2
NMFC21_SDg3$p1inp2vec <- is.element(NMFC21_SDg3$player1, player2vector)
NMFC21_SDg3$p2inp1vec <- is.element(NMFC21_SDg3$player2, player1vector)

addPlayer1 <- NMFC21_SDg3[ which(NMFC21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_SDg3[ which(NMFC21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_SDg2 <- rbind(NMFC21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
NMFC21_SDft <- ftable(NMFC21_SDg2$player1, NMFC21_SDg2$player2)
NMFC21_SDft2 <- as.matrix(NMFC21_SDft)
numRows <- nrow(NMFC21_SDft2)
numCols <- ncol(NMFC21_SDft2)
NMFC21_SDft3 <- NMFC21_SDft2[c(2:numRows) , c(2:numCols)]
NMFC21_SDTable <- graph.adjacency(NMFC21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(NMFC21_SDTable, vertex.label = V(NMFC21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
NMFC21_SD.clusterCoef <- transitivity(NMFC21_SDTable, type="global") #cluster coefficient
NMFC21_SD.degreeCent <- centralization.degree(NMFC21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_SDftn <- as.network.matrix(NMFC21_SDft)
NMFC21_SD.netDensity <- network.density(NMFC21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_SD.entropy <- entropy(NMFC21_SDft) #entropy

NMFC21_SD.netMx <- cbind(NMFC21_SD.netMx, NMFC21_SD.clusterCoef, NMFC21_SD.degreeCent$centralization,
                         NMFC21_SD.netDensity, NMFC21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
NMFC21_TDg2 <- data.frame(NMFC21_TD)
NMFC21_TDg2 <- NMFC21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_TDg2$player1
player2vector <- NMFC21_TDg2$player2
NMFC21_TDg3 <- NMFC21_TDg2
NMFC21_TDg3$p1inp2vec <- is.element(NMFC21_TDg3$player1, player2vector)
NMFC21_TDg3$p2inp1vec <- is.element(NMFC21_TDg3$player2, player1vector)

addPlayer1 <- NMFC21_TDg3[ which(NMFC21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_TDg3[ which(NMFC21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_TDg2 <- rbind(NMFC21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
NMFC21_TDft <- ftable(NMFC21_TDg2$player1, NMFC21_TDg2$player2)
NMFC21_TDft2 <- as.matrix(NMFC21_TDft)
numRows <- nrow(NMFC21_TDft2)
numCols <- ncol(NMFC21_TDft2)
NMFC21_TDft3 <- NMFC21_TDft2[c(2:numRows) , c(2:numCols)]
NMFC21_TDTable <- graph.adjacency(NMFC21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(NMFC21_TDTable, vertex.label = V(NMFC21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
NMFC21_TD.clusterCoef <- transitivity(NMFC21_TDTable, type="global") #cluster coefficient
NMFC21_TD.degreeCent <- centralization.degree(NMFC21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_TDftn <- as.network.matrix(NMFC21_TDft)
NMFC21_TD.netDensity <- network.density(NMFC21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_TD.entropy <- entropy(NMFC21_TDft) #entropy

NMFC21_TD.netMx <- cbind(NMFC21_TD.netMx, NMFC21_TD.clusterCoef, NMFC21_TD.degreeCent$centralization,
                         NMFC21_TD.netDensity, NMFC21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
NMFC21_QTg2 <- data.frame(NMFC21_QT)
NMFC21_QTg2 <- NMFC21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC21_QTg2$player1
player2vector <- NMFC21_QTg2$player2
NMFC21_QTg3 <- NMFC21_QTg2
NMFC21_QTg3$p1inp2vec <- is.element(NMFC21_QTg3$player1, player2vector)
NMFC21_QTg3$p2inp1vec <- is.element(NMFC21_QTg3$player2, player1vector)

addPlayer1 <- NMFC21_QTg3[ which(NMFC21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC21_QTg3[ which(NMFC21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC21_QTg2 <- rbind(NMFC21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
NMFC21_QTft <- ftable(NMFC21_QTg2$player1, NMFC21_QTg2$player2)
NMFC21_QTft2 <- as.matrix(NMFC21_QTft)
numRows <- nrow(NMFC21_QTft2)
numCols <- ncol(NMFC21_QTft2)
NMFC21_QTft3 <- NMFC21_QTft2[c(2:numRows) , c(2:numCols)]
NMFC21_QTTable <- graph.adjacency(NMFC21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(NMFC21_QTTable, vertex.label = V(NMFC21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
NMFC21_QT.clusterCoef <- transitivity(NMFC21_QTTable, type="global") #cluster coefficient
NMFC21_QT.degreeCent <- centralization.degree(NMFC21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC21_QTftn <- as.network.matrix(NMFC21_QTft)
NMFC21_QT.netDensity <- network.density(NMFC21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC21_QT.entropy <- entropy(NMFC21_QTft) #entropy

NMFC21_QT.netMx <- cbind(NMFC21_QT.netMx, NMFC21_QT.clusterCoef, NMFC21_QT.degreeCent$centralization,
                         NMFC21_QT.netDensity, NMFC21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC21_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "PORT"
KIoutcome = "Goal_F"
PORT21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
PORT21_Gg2 <- data.frame(PORT21_G)
PORT21_Gg2 <- PORT21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_Gg2$player1
player2vector <- PORT21_Gg2$player2
PORT21_Gg3 <- PORT21_Gg2
PORT21_Gg3$p1inp2vec <- is.element(PORT21_Gg3$player1, player2vector)
PORT21_Gg3$p2inp1vec <- is.element(PORT21_Gg3$player2, player1vector)

addPlayer1 <- PORT21_Gg3[ which(PORT21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT21_Gg3[ which(PORT21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_Gg2 <- rbind(PORT21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
PORT21_Gft <- ftable(PORT21_Gg2$player1, PORT21_Gg2$player2)
PORT21_Gft2 <- as.matrix(PORT21_Gft)
numRows <- nrow(PORT21_Gft2)
numCols <- ncol(PORT21_Gft2)
PORT21_Gft3 <- PORT21_Gft2[c(2:numRows) , c(2:numCols)]
PORT21_GTable <- graph.adjacency(PORT21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(PORT21_GTable, vertex.label = V(PORT21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
PORT21_G.clusterCoef <- transitivity(PORT21_GTable, type="global") #cluster coefficient
PORT21_G.degreeCent <- centralization.degree(PORT21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_Gftn <- as.network.matrix(PORT21_Gft)
PORT21_G.netDensity <- network.density(PORT21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_G.entropy <- entropy(PORT21_Gft) #entropy

PORT21_G.netMx <- cbind(PORT21_G.netMx, PORT21_G.clusterCoef, PORT21_G.degreeCent$centralization,
                        PORT21_G.netDensity, PORT21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "Behind_F"
PORT21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
PORT21_Bg2 <- data.frame(PORT21_B)
PORT21_Bg2 <- PORT21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_Bg2$player1
player2vector <- PORT21_Bg2$player2
PORT21_Bg3 <- PORT21_Bg2
PORT21_Bg3$p1inp2vec <- is.element(PORT21_Bg3$player1, player2vector)
PORT21_Bg3$p2inp1vec <- is.element(PORT21_Bg3$player2, player1vector)

addPlayer1 <- PORT21_Bg3[ which(PORT21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_Bg3[ which(PORT21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_Bg2 <- rbind(PORT21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
PORT21_Bft <- ftable(PORT21_Bg2$player1, PORT21_Bg2$player2)
PORT21_Bft2 <- as.matrix(PORT21_Bft)
numRows <- nrow(PORT21_Bft2)
numCols <- ncol(PORT21_Bft2)
PORT21_Bft3 <- PORT21_Bft2[c(2:numRows) , c(2:numCols)]
PORT21_BTable <- graph.adjacency(PORT21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(PORT21_BTable, vertex.label = V(PORT21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
PORT21_B.clusterCoef <- transitivity(PORT21_BTable, type="global") #cluster coefficient
PORT21_B.degreeCent <- centralization.degree(PORT21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_Bftn <- as.network.matrix(PORT21_Bft)
PORT21_B.netDensity <- network.density(PORT21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_B.entropy <- entropy(PORT21_Bft) #entropy

PORT21_B.netMx <- cbind(PORT21_B.netMx, PORT21_B.clusterCoef, PORT21_B.degreeCent$centralization,
                        PORT21_B.netDensity, PORT21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
PORT21_SFg2 <- data.frame(PORT21_SF)
PORT21_SFg2 <- PORT21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_SFg2$player1
player2vector <- PORT21_SFg2$player2
PORT21_SFg3 <- PORT21_SFg2
PORT21_SFg3$p1inp2vec <- is.element(PORT21_SFg3$player1, player2vector)
PORT21_SFg3$p2inp1vec <- is.element(PORT21_SFg3$player2, player1vector)

addPlayer1 <- PORT21_SFg3[ which(PORT21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT21_SFg3[ which(PORT21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_SFg2 <- rbind(PORT21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
PORT21_SFft <- ftable(PORT21_SFg2$player1, PORT21_SFg2$player2)
PORT21_SFft2 <- as.matrix(PORT21_SFft)
numRows <- nrow(PORT21_SFft2)
numCols <- ncol(PORT21_SFft2)
PORT21_SFft3 <- PORT21_SFft2[c(2:numRows) , c(2:numCols)]
PORT21_SFTable <- graph.adjacency(PORT21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(PORT21_SFTable, vertex.label = V(PORT21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
PORT21_SF.clusterCoef <- transitivity(PORT21_SFTable, type="global") #cluster coefficient
PORT21_SF.degreeCent <- centralization.degree(PORT21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_SFftn <- as.network.matrix(PORT21_SFft)
PORT21_SF.netDensity <- network.density(PORT21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_SF.entropy <- entropy(PORT21_SFft) #entropy

PORT21_SF.netMx <- cbind(PORT21_SF.netMx, PORT21_SF.clusterCoef, PORT21_SF.degreeCent$centralization,
                         PORT21_SF.netDensity, PORT21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************

round = 21
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
PORT21_TFg2 <- data.frame(PORT21_TF)
PORT21_TFg2 <- PORT21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_TFg2$player1
player2vector <- PORT21_TFg2$player2
PORT21_TFg3 <- PORT21_TFg2
PORT21_TFg3$p1inp2vec <- is.element(PORT21_TFg3$player1, player2vector)
PORT21_TFg3$p2inp1vec <- is.element(PORT21_TFg3$player2, player1vector)

addPlayer1 <- PORT21_TFg3[ which(PORT21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_TFg3[ which(PORT21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_TFg2 <- rbind(PORT21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
PORT21_TFft <- ftable(PORT21_TFg2$player1, PORT21_TFg2$player2)
PORT21_TFft2 <- as.matrix(PORT21_TFft)
numRows <- nrow(PORT21_TFft2)
numCols <- ncol(PORT21_TFft2)
PORT21_TFft3 <- PORT21_TFft2[c(2:numRows) , c(2:numCols)]
PORT21_TFTable <- graph.adjacency(PORT21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(PORT21_TFTable, vertex.label = V(PORT21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
PORT21_TF.clusterCoef <- transitivity(PORT21_TFTable, type="global") #cluster coefficient
PORT21_TF.degreeCent <- centralization.degree(PORT21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_TFftn <- as.network.matrix(PORT21_TFft)
PORT21_TF.netDensity <- network.density(PORT21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_TF.entropy <- entropy(PORT21_TFft) #entropy

PORT21_TF.netMx <- cbind(PORT21_TF.netMx, PORT21_TF.clusterCoef, PORT21_TF.degreeCent$centralization,
                         PORT21_TF.netDensity, PORT21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
PORT21_SAMg2 <- data.frame(PORT21_SAM)
PORT21_SAMg2 <- PORT21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_SAMg2$player1
player2vector <- PORT21_SAMg2$player2
PORT21_SAMg3 <- PORT21_SAMg2
PORT21_SAMg3$p1inp2vec <- is.element(PORT21_SAMg3$player1, player2vector)
PORT21_SAMg3$p2inp1vec <- is.element(PORT21_SAMg3$player2, player1vector)

addPlayer1 <- PORT21_SAMg3[ which(PORT21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_SAMg3[ which(PORT21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_SAMg2 <- rbind(PORT21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
PORT21_SAMft <- ftable(PORT21_SAMg2$player1, PORT21_SAMg2$player2)
PORT21_SAMft2 <- as.matrix(PORT21_SAMft)
numRows <- nrow(PORT21_SAMft2)
numCols <- ncol(PORT21_SAMft2)
PORT21_SAMft3 <- PORT21_SAMft2[c(2:numRows) , c(2:numCols)]
PORT21_SAMTable <- graph.adjacency(PORT21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(PORT21_SAMTable, vertex.label = V(PORT21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
PORT21_SAM.clusterCoef <- transitivity(PORT21_SAMTable, type="global") #cluster coefficient
PORT21_SAM.degreeCent <- centralization.degree(PORT21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_SAMftn <- as.network.matrix(PORT21_SAMft)
PORT21_SAM.netDensity <- network.density(PORT21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_SAM.entropy <- entropy(PORT21_SAMft) #entropy

PORT21_SAM.netMx <- cbind(PORT21_SAM.netMx, PORT21_SAM.clusterCoef, PORT21_SAM.degreeCent$centralization,
                          PORT21_SAM.netDensity, PORT21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
PORT21_TAMg2 <- data.frame(PORT21_TAM)
PORT21_TAMg2 <- PORT21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_TAMg2$player1
player2vector <- PORT21_TAMg2$player2
PORT21_TAMg3 <- PORT21_TAMg2
PORT21_TAMg3$p1inp2vec <- is.element(PORT21_TAMg3$player1, player2vector)
PORT21_TAMg3$p2inp1vec <- is.element(PORT21_TAMg3$player2, player1vector)

addPlayer1 <- PORT21_TAMg3[ which(PORT21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT21_TAMg2 <- rbind(PORT21_TAMg2, addPlayer1)

#ROUND 21, AM Turnover graph using weighted edges
PORT21_TAMft <- ftable(PORT21_TAMg2$player1, PORT21_TAMg2$player2)
PORT21_TAMft2 <- as.matrix(PORT21_TAMft)
numRows <- nrow(PORT21_TAMft2)
numCols <- ncol(PORT21_TAMft2)
PORT21_TAMft3 <- PORT21_TAMft2[c(2:numRows) , c(1:numCols)]
PORT21_TAMTable <- graph.adjacency(PORT21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(PORT21_TAMTable, vertex.label = V(PORT21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
PORT21_TAM.clusterCoef <- transitivity(PORT21_TAMTable, type="global") #cluster coefficient
PORT21_TAM.degreeCent <- centralization.degree(PORT21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_TAMftn <- as.network.matrix(PORT21_TAMft)
PORT21_TAM.netDensity <- network.density(PORT21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_TAM.entropy <- entropy(PORT21_TAMft) #entropy

PORT21_TAM.netMx <- cbind(PORT21_TAM.netMx, PORT21_TAM.clusterCoef, PORT21_TAM.degreeCent$centralization,
                          PORT21_TAM.netDensity, PORT21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
PORT21_SDMg2 <- data.frame(PORT21_SDM)
PORT21_SDMg2 <- PORT21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_SDMg2$player1
player2vector <- PORT21_SDMg2$player2
PORT21_SDMg3 <- PORT21_SDMg2
PORT21_SDMg3$p1inp2vec <- is.element(PORT21_SDMg3$player1, player2vector)
PORT21_SDMg3$p2inp1vec <- is.element(PORT21_SDMg3$player2, player1vector)

addPlayer1 <- PORT21_SDMg3[ which(PORT21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_SDMg3[ which(PORT21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_SDMg2 <- rbind(PORT21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
PORT21_SDMft <- ftable(PORT21_SDMg2$player1, PORT21_SDMg2$player2)
PORT21_SDMft2 <- as.matrix(PORT21_SDMft)
numRows <- nrow(PORT21_SDMft2)
numCols <- ncol(PORT21_SDMft2)
PORT21_SDMft3 <- PORT21_SDMft2[c(2:numRows) , c(2:numCols)]
PORT21_SDMTable <- graph.adjacency(PORT21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(PORT21_SDMTable, vertex.label = V(PORT21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
PORT21_SDM.clusterCoef <- transitivity(PORT21_SDMTable, type="global") #cluster coefficient
PORT21_SDM.degreeCent <- centralization.degree(PORT21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_SDMftn <- as.network.matrix(PORT21_SDMft)
PORT21_SDM.netDensity <- network.density(PORT21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_SDM.entropy <- entropy(PORT21_SDMft) #entropy

PORT21_SDM.netMx <- cbind(PORT21_SDM.netMx, PORT21_SDM.clusterCoef, PORT21_SDM.degreeCent$centralization,
                          PORT21_SDM.netDensity, PORT21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
PORT21_TDMg2 <- data.frame(PORT21_TDM)
PORT21_TDMg2 <- PORT21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_TDMg2$player1
player2vector <- PORT21_TDMg2$player2
PORT21_TDMg3 <- PORT21_TDMg2
PORT21_TDMg3$p1inp2vec <- is.element(PORT21_TDMg3$player1, player2vector)
PORT21_TDMg3$p2inp1vec <- is.element(PORT21_TDMg3$player2, player1vector)

addPlayer1 <- PORT21_TDMg3[ which(PORT21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_TDMg3[ which(PORT21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_TDMg2 <- rbind(PORT21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
PORT21_TDMft <- ftable(PORT21_TDMg2$player1, PORT21_TDMg2$player2)
PORT21_TDMft2 <- as.matrix(PORT21_TDMft)
numRows <- nrow(PORT21_TDMft2)
numCols <- ncol(PORT21_TDMft2)
PORT21_TDMft3 <- PORT21_TDMft2[c(2:numRows) , c(2:numCols)]
PORT21_TDMTable <- graph.adjacency(PORT21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(PORT21_TDMTable, vertex.label = V(PORT21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
PORT21_TDM.clusterCoef <- transitivity(PORT21_TDMTable, type="global") #cluster coefficient
PORT21_TDM.degreeCent <- centralization.degree(PORT21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_TDMftn <- as.network.matrix(PORT21_TDMft)
PORT21_TDM.netDensity <- network.density(PORT21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_TDM.entropy <- entropy(PORT21_TDMft) #entropy

PORT21_TDM.netMx <- cbind(PORT21_TDM.netMx, PORT21_TDM.clusterCoef, PORT21_TDM.degreeCent$centralization,
                          PORT21_TDM.netDensity, PORT21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
PORT21_SDg2 <- data.frame(PORT21_SD)
PORT21_SDg2 <- PORT21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_SDg2$player1
player2vector <- PORT21_SDg2$player2
PORT21_SDg3 <- PORT21_SDg2
PORT21_SDg3$p1inp2vec <- is.element(PORT21_SDg3$player1, player2vector)
PORT21_SDg3$p2inp1vec <- is.element(PORT21_SDg3$player2, player1vector)

addPlayer1 <- PORT21_SDg3[ which(PORT21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_SDg3[ which(PORT21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_SDg2 <- rbind(PORT21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
PORT21_SDft <- ftable(PORT21_SDg2$player1, PORT21_SDg2$player2)
PORT21_SDft2 <- as.matrix(PORT21_SDft)
numRows <- nrow(PORT21_SDft2)
numCols <- ncol(PORT21_SDft2)
PORT21_SDft3 <- PORT21_SDft2[c(2:numRows) , c(2:numCols)]
PORT21_SDTable <- graph.adjacency(PORT21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(PORT21_SDTable, vertex.label = V(PORT21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
PORT21_SD.clusterCoef <- transitivity(PORT21_SDTable, type="global") #cluster coefficient
PORT21_SD.degreeCent <- centralization.degree(PORT21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_SDftn <- as.network.matrix(PORT21_SDft)
PORT21_SD.netDensity <- network.density(PORT21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_SD.entropy <- entropy(PORT21_SDft) #entropy

PORT21_SD.netMx <- cbind(PORT21_SD.netMx, PORT21_SD.clusterCoef, PORT21_SD.degreeCent$centralization,
                         PORT21_SD.netDensity, PORT21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
PORT21_TDg2 <- data.frame(PORT21_TD)
PORT21_TDg2 <- PORT21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_TDg2$player1
player2vector <- PORT21_TDg2$player2
PORT21_TDg3 <- PORT21_TDg2
PORT21_TDg3$p1inp2vec <- is.element(PORT21_TDg3$player1, player2vector)
PORT21_TDg3$p2inp1vec <- is.element(PORT21_TDg3$player2, player1vector)

addPlayer1 <- PORT21_TDg3[ which(PORT21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_TDg3[ which(PORT21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_TDg2 <- rbind(PORT21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
PORT21_TDft <- ftable(PORT21_TDg2$player1, PORT21_TDg2$player2)
PORT21_TDft2 <- as.matrix(PORT21_TDft)
numRows <- nrow(PORT21_TDft2)
numCols <- ncol(PORT21_TDft2)
PORT21_TDft3 <- PORT21_TDft2[c(2:numRows) , c(2:numCols)]
PORT21_TDTable <- graph.adjacency(PORT21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(PORT21_TDTable, vertex.label = V(PORT21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
PORT21_TD.clusterCoef <- transitivity(PORT21_TDTable, type="global") #cluster coefficient
PORT21_TD.degreeCent <- centralization.degree(PORT21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_TDftn <- as.network.matrix(PORT21_TDft)
PORT21_TD.netDensity <- network.density(PORT21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_TD.entropy <- entropy(PORT21_TDft) #entropy

PORT21_TD.netMx <- cbind(PORT21_TD.netMx, PORT21_TD.clusterCoef, PORT21_TD.degreeCent$centralization,
                         PORT21_TD.netDensity, PORT21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
PORT21_QTg2 <- data.frame(PORT21_QT)
PORT21_QTg2 <- PORT21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT21_QTg2$player1
player2vector <- PORT21_QTg2$player2
PORT21_QTg3 <- PORT21_QTg2
PORT21_QTg3$p1inp2vec <- is.element(PORT21_QTg3$player1, player2vector)
PORT21_QTg3$p2inp1vec <- is.element(PORT21_QTg3$player2, player1vector)

addPlayer1 <- PORT21_QTg3[ which(PORT21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT21_QTg3[ which(PORT21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT21_QTg2 <- rbind(PORT21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
PORT21_QTft <- ftable(PORT21_QTg2$player1, PORT21_QTg2$player2)
PORT21_QTft2 <- as.matrix(PORT21_QTft)
numRows <- nrow(PORT21_QTft2)
numCols <- ncol(PORT21_QTft2)
PORT21_QTft3 <- PORT21_QTft2[c(2:numRows) , c(2:numCols)]
PORT21_QTTable <- graph.adjacency(PORT21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(PORT21_QTTable, vertex.label = V(PORT21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
PORT21_QT.clusterCoef <- transitivity(PORT21_QTTable, type="global") #cluster coefficient
PORT21_QT.degreeCent <- centralization.degree(PORT21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT21_QTftn <- as.network.matrix(PORT21_QTft)
PORT21_QT.netDensity <- network.density(PORT21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT21_QT.entropy <- entropy(PORT21_QTft) #entropy

PORT21_QT.netMx <- cbind(PORT21_QT.netMx, PORT21_QT.clusterCoef, PORT21_QT.degreeCent$centralization,
                         PORT21_QT.netDensity, PORT21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT21_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "RICH"
KIoutcome = "Goal_F"
RICH21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
RICH21_Gg2 <- data.frame(RICH21_G)
RICH21_Gg2 <- RICH21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_Gg2$player1
player2vector <- RICH21_Gg2$player2
RICH21_Gg3 <- RICH21_Gg2
RICH21_Gg3$p1inp2vec <- is.element(RICH21_Gg3$player1, player2vector)
RICH21_Gg3$p2inp1vec <- is.element(RICH21_Gg3$player2, player1vector)

addPlayer1 <- RICH21_Gg3[ which(RICH21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH21_Gg3[ which(RICH21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_Gg2 <- rbind(RICH21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
RICH21_Gft <- ftable(RICH21_Gg2$player1, RICH21_Gg2$player2)
RICH21_Gft2 <- as.matrix(RICH21_Gft)
numRows <- nrow(RICH21_Gft2)
numCols <- ncol(RICH21_Gft2)
RICH21_Gft3 <- RICH21_Gft2[c(2:numRows) , c(2:numCols)]
RICH21_GTable <- graph.adjacency(RICH21_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(RICH21_GTable, vertex.label = V(RICH21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
RICH21_G.clusterCoef <- transitivity(RICH21_GTable, type="global") #cluster coefficient
RICH21_G.degreeCent <- centralization.degree(RICH21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_Gftn <- as.network.matrix(RICH21_Gft)
RICH21_G.netDensity <- network.density(RICH21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_G.entropy <- entropy(RICH21_Gft) #entropy

RICH21_G.netMx <- cbind(RICH21_G.netMx, RICH21_G.clusterCoef, RICH21_G.degreeCent$centralization,
                        RICH21_G.netDensity, RICH21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "RICH"
KIoutcome = "Behind_F"
RICH21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
RICH21_Bg2 <- data.frame(RICH21_B)
RICH21_Bg2 <- RICH21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_Bg2$player1
player2vector <- RICH21_Bg2$player2
RICH21_Bg3 <- RICH21_Bg2
RICH21_Bg3$p1inp2vec <- is.element(RICH21_Bg3$player1, player2vector)
RICH21_Bg3$p2inp1vec <- is.element(RICH21_Bg3$player2, player1vector)

addPlayer1 <- RICH21_Bg3[ which(RICH21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_Bg3[ which(RICH21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_Bg2 <- rbind(RICH21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
RICH21_Bft <- ftable(RICH21_Bg2$player1, RICH21_Bg2$player2)
RICH21_Bft2 <- as.matrix(RICH21_Bft)
numRows <- nrow(RICH21_Bft2)
numCols <- ncol(RICH21_Bft2)
RICH21_Bft3 <- RICH21_Bft2[c(2:numRows) , c(2:numCols)]
RICH21_BTable <- graph.adjacency(RICH21_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(RICH21_BTable, vertex.label = V(RICH21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
RICH21_B.clusterCoef <- transitivity(RICH21_BTable, type="global") #cluster coefficient
RICH21_B.degreeCent <- centralization.degree(RICH21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_Bftn <- as.network.matrix(RICH21_Bft)
RICH21_B.netDensity <- network.density(RICH21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_B.entropy <- entropy(RICH21_Bft) #entropy

RICH21_B.netMx <- cbind(RICH21_B.netMx, RICH21_B.clusterCoef, RICH21_B.degreeCent$centralization,
                        RICH21_B.netDensity, RICH21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
RICH21_SFg2 <- data.frame(RICH21_SF)
RICH21_SFg2 <- RICH21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_SFg2$player1
player2vector <- RICH21_SFg2$player2
RICH21_SFg3 <- RICH21_SFg2
RICH21_SFg3$p1inp2vec <- is.element(RICH21_SFg3$player1, player2vector)
RICH21_SFg3$p2inp1vec <- is.element(RICH21_SFg3$player2, player1vector)

addPlayer1 <- RICH21_SFg3[ which(RICH21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_SFg3[ which(RICH21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_SFg2 <- rbind(RICH21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
RICH21_SFft <- ftable(RICH21_SFg2$player1, RICH21_SFg2$player2)
RICH21_SFft2 <- as.matrix(RICH21_SFft)
numRows <- nrow(RICH21_SFft2)
numCols <- ncol(RICH21_SFft2)
RICH21_SFft3 <- RICH21_SFft2[c(2:numRows) , c(2:numCols)]
RICH21_SFTable <- graph.adjacency(RICH21_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(RICH21_SFTable, vertex.label = V(RICH21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
RICH21_SF.clusterCoef <- transitivity(RICH21_SFTable, type="global") #cluster coefficient
RICH21_SF.degreeCent <- centralization.degree(RICH21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_SFftn <- as.network.matrix(RICH21_SFft)
RICH21_SF.netDensity <- network.density(RICH21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_SF.entropy <- entropy(RICH21_SFft) #entropy

RICH21_SF.netMx <- cbind(RICH21_SF.netMx, RICH21_SF.clusterCoef, RICH21_SF.degreeCent$centralization,
                         RICH21_SF.netDensity, RICH21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************
#NA

round = 21
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
RICH21_TFg2 <- data.frame(RICH21_TF)
RICH21_TFg2 <- RICH21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_TFg2$player1
player2vector <- RICH21_TFg2$player2
RICH21_TFg3 <- RICH21_TFg2
RICH21_TFg3$p1inp2vec <- is.element(RICH21_TFg3$player1, player2vector)
RICH21_TFg3$p2inp1vec <- is.element(RICH21_TFg3$player2, player1vector)

addPlayer1 <- RICH21_TFg3[ which(RICH21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_TFg3[ which(RICH21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_TFg2 <- rbind(RICH21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
RICH21_TFft <- ftable(RICH21_TFg2$player1, RICH21_TFg2$player2)
RICH21_TFft2 <- as.matrix(RICH21_TFft)
numRows <- nrow(RICH21_TFft2)
numCols <- ncol(RICH21_TFft2)
RICH21_TFft3 <- RICH21_TFft2[c(2:numRows) , c(2:numCols)]
RICH21_TFTable <- graph.adjacency(RICH21_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(RICH21_TFTable, vertex.label = V(RICH21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
RICH21_TF.clusterCoef <- transitivity(RICH21_TFTable, type="global") #cluster coefficient
RICH21_TF.degreeCent <- centralization.degree(RICH21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_TFftn <- as.network.matrix(RICH21_TFft)
RICH21_TF.netDensity <- network.density(RICH21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_TF.entropy <- entropy(RICH21_TFft) #entropy

RICH21_TF.netMx <- cbind(RICH21_TF.netMx, RICH21_TF.clusterCoef, RICH21_TF.degreeCent$centralization,
                         RICH21_TF.netDensity, RICH21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
RICH21_SAMg2 <- data.frame(RICH21_SAM)
RICH21_SAMg2 <- RICH21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_SAMg2$player1
player2vector <- RICH21_SAMg2$player2
RICH21_SAMg3 <- RICH21_SAMg2
RICH21_SAMg3$p1inp2vec <- is.element(RICH21_SAMg3$player1, player2vector)
RICH21_SAMg3$p2inp1vec <- is.element(RICH21_SAMg3$player2, player1vector)

addPlayer1 <- RICH21_SAMg3[ which(RICH21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_SAMg3[ which(RICH21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_SAMg2 <- rbind(RICH21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
RICH21_SAMft <- ftable(RICH21_SAMg2$player1, RICH21_SAMg2$player2)
RICH21_SAMft2 <- as.matrix(RICH21_SAMft)
numRows <- nrow(RICH21_SAMft2)
numCols <- ncol(RICH21_SAMft2)
RICH21_SAMft3 <- RICH21_SAMft2[c(2:numRows) , c(2:numCols)]
RICH21_SAMTable <- graph.adjacency(RICH21_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(RICH21_SAMTable, vertex.label = V(RICH21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
RICH21_SAM.clusterCoef <- transitivity(RICH21_SAMTable, type="global") #cluster coefficient
RICH21_SAM.degreeCent <- centralization.degree(RICH21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_SAMftn <- as.network.matrix(RICH21_SAMft)
RICH21_SAM.netDensity <- network.density(RICH21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_SAM.entropy <- entropy(RICH21_SAMft) #entropy

RICH21_SAM.netMx <- cbind(RICH21_SAM.netMx, RICH21_SAM.clusterCoef, RICH21_SAM.degreeCent$centralization,
                          RICH21_SAM.netDensity, RICH21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
RICH21_TAMg2 <- data.frame(RICH21_TAM)
RICH21_TAMg2 <- RICH21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_TAMg2$player1
player2vector <- RICH21_TAMg2$player2
RICH21_TAMg3 <- RICH21_TAMg2
RICH21_TAMg3$p1inp2vec <- is.element(RICH21_TAMg3$player1, player2vector)
RICH21_TAMg3$p2inp1vec <- is.element(RICH21_TAMg3$player2, player1vector)

addPlayer1 <- RICH21_TAMg3[ which(RICH21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_TAMg3[ which(RICH21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_TAMg2 <- rbind(RICH21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
RICH21_TAMft <- ftable(RICH21_TAMg2$player1, RICH21_TAMg2$player2)
RICH21_TAMft2 <- as.matrix(RICH21_TAMft)
numRows <- nrow(RICH21_TAMft2)
numCols <- ncol(RICH21_TAMft2)
RICH21_TAMft3 <- RICH21_TAMft2[c(2:numRows) , c(2:numCols)]
RICH21_TAMTable <- graph.adjacency(RICH21_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(RICH21_TAMTable, vertex.label = V(RICH21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
RICH21_TAM.clusterCoef <- transitivity(RICH21_TAMTable, type="global") #cluster coefficient
RICH21_TAM.degreeCent <- centralization.degree(RICH21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_TAMftn <- as.network.matrix(RICH21_TAMft)
RICH21_TAM.netDensity <- network.density(RICH21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_TAM.entropy <- entropy(RICH21_TAMft) #entropy

RICH21_TAM.netMx <- cbind(RICH21_TAM.netMx, RICH21_TAM.clusterCoef, RICH21_TAM.degreeCent$centralization,
                          RICH21_TAM.netDensity, RICH21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
RICH21_SDMg2 <- data.frame(RICH21_SDM)
RICH21_SDMg2 <- RICH21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_SDMg2$player1
player2vector <- RICH21_SDMg2$player2
RICH21_SDMg3 <- RICH21_SDMg2
RICH21_SDMg3$p1inp2vec <- is.element(RICH21_SDMg3$player1, player2vector)
RICH21_SDMg3$p2inp1vec <- is.element(RICH21_SDMg3$player2, player1vector)

addPlayer1 <- RICH21_SDMg3[ which(RICH21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_SDMg3[ which(RICH21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_SDMg2 <- rbind(RICH21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
RICH21_SDMft <- ftable(RICH21_SDMg2$player1, RICH21_SDMg2$player2)
RICH21_SDMft2 <- as.matrix(RICH21_SDMft)
numRows <- nrow(RICH21_SDMft2)
numCols <- ncol(RICH21_SDMft2)
RICH21_SDMft3 <- RICH21_SDMft2[c(2:numRows) , c(2:numCols)]
RICH21_SDMTable <- graph.adjacency(RICH21_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(RICH21_SDMTable, vertex.label = V(RICH21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
RICH21_SDM.clusterCoef <- transitivity(RICH21_SDMTable, type="global") #cluster coefficient
RICH21_SDM.degreeCent <- centralization.degree(RICH21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_SDMftn <- as.network.matrix(RICH21_SDMft)
RICH21_SDM.netDensity <- network.density(RICH21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_SDM.entropy <- entropy(RICH21_SDMft) #entropy

RICH21_SDM.netMx <- cbind(RICH21_SDM.netMx, RICH21_SDM.clusterCoef, RICH21_SDM.degreeCent$centralization,
                          RICH21_SDM.netDensity, RICH21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
RICH21_TDMg2 <- data.frame(RICH21_TDM)
RICH21_TDMg2 <- RICH21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_TDMg2$player1
player2vector <- RICH21_TDMg2$player2
RICH21_TDMg3 <- RICH21_TDMg2
RICH21_TDMg3$p1inp2vec <- is.element(RICH21_TDMg3$player1, player2vector)
RICH21_TDMg3$p2inp1vec <- is.element(RICH21_TDMg3$player2, player1vector)

addPlayer1 <- RICH21_TDMg3[ which(RICH21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_TDMg3[ which(RICH21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_TDMg2 <- rbind(RICH21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
RICH21_TDMft <- ftable(RICH21_TDMg2$player1, RICH21_TDMg2$player2)
RICH21_TDMft2 <- as.matrix(RICH21_TDMft)
numRows <- nrow(RICH21_TDMft2)
numCols <- ncol(RICH21_TDMft2)
RICH21_TDMft3 <- RICH21_TDMft2[c(2:numRows) , c(2:numCols)]
RICH21_TDMTable <- graph.adjacency(RICH21_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(RICH21_TDMTable, vertex.label = V(RICH21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
RICH21_TDM.clusterCoef <- transitivity(RICH21_TDMTable, type="global") #cluster coefficient
RICH21_TDM.degreeCent <- centralization.degree(RICH21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_TDMftn <- as.network.matrix(RICH21_TDMft)
RICH21_TDM.netDensity <- network.density(RICH21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_TDM.entropy <- entropy(RICH21_TDMft) #entropy

RICH21_TDM.netMx <- cbind(RICH21_TDM.netMx, RICH21_TDM.clusterCoef, RICH21_TDM.degreeCent$centralization,
                          RICH21_TDM.netDensity, RICH21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
RICH21_SDg2 <- data.frame(RICH21_SD)
RICH21_SDg2 <- RICH21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_SDg2$player1
player2vector <- RICH21_SDg2$player2
RICH21_SDg3 <- RICH21_SDg2
RICH21_SDg3$p1inp2vec <- is.element(RICH21_SDg3$player1, player2vector)
RICH21_SDg3$p2inp1vec <- is.element(RICH21_SDg3$player2, player1vector)

addPlayer1 <- RICH21_SDg3[ which(RICH21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_SDg3[ which(RICH21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_SDg2 <- rbind(RICH21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
RICH21_SDft <- ftable(RICH21_SDg2$player1, RICH21_SDg2$player2)
RICH21_SDft2 <- as.matrix(RICH21_SDft)
numRows <- nrow(RICH21_SDft2)
numCols <- ncol(RICH21_SDft2)
RICH21_SDft3 <- RICH21_SDft2[c(2:numRows) , c(2:numCols)]
RICH21_SDTable <- graph.adjacency(RICH21_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(RICH21_SDTable, vertex.label = V(RICH21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
RICH21_SD.clusterCoef <- transitivity(RICH21_SDTable, type="global") #cluster coefficient
RICH21_SD.degreeCent <- centralization.degree(RICH21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_SDftn <- as.network.matrix(RICH21_SDft)
RICH21_SD.netDensity <- network.density(RICH21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_SD.entropy <- entropy(RICH21_SDft) #entropy

RICH21_SD.netMx <- cbind(RICH21_SD.netMx, RICH21_SD.clusterCoef, RICH21_SD.degreeCent$centralization,
                         RICH21_SD.netDensity, RICH21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
RICH21_TDg2 <- data.frame(RICH21_TD)
RICH21_TDg2 <- RICH21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_TDg2$player1
player2vector <- RICH21_TDg2$player2
RICH21_TDg3 <- RICH21_TDg2
RICH21_TDg3$p1inp2vec <- is.element(RICH21_TDg3$player1, player2vector)
RICH21_TDg3$p2inp1vec <- is.element(RICH21_TDg3$player2, player1vector)

addPlayer1 <- RICH21_TDg3[ which(RICH21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_TDg3[ which(RICH21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_TDg2 <- rbind(RICH21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
RICH21_TDft <- ftable(RICH21_TDg2$player1, RICH21_TDg2$player2)
RICH21_TDft2 <- as.matrix(RICH21_TDft)
numRows <- nrow(RICH21_TDft2)
numCols <- ncol(RICH21_TDft2)
RICH21_TDft3 <- RICH21_TDft2[c(2:numRows) , c(2:numCols)]
RICH21_TDTable <- graph.adjacency(RICH21_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(RICH21_TDTable, vertex.label = V(RICH21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
RICH21_TD.clusterCoef <- transitivity(RICH21_TDTable, type="global") #cluster coefficient
RICH21_TD.degreeCent <- centralization.degree(RICH21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_TDftn <- as.network.matrix(RICH21_TDft)
RICH21_TD.netDensity <- network.density(RICH21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_TD.entropy <- entropy(RICH21_TDft) #entropy

RICH21_TD.netMx <- cbind(RICH21_TD.netMx, RICH21_TD.clusterCoef, RICH21_TD.degreeCent$centralization,
                         RICH21_TD.netDensity, RICH21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
RICH21_QTg2 <- data.frame(RICH21_QT)
RICH21_QTg2 <- RICH21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH21_QTg2$player1
player2vector <- RICH21_QTg2$player2
RICH21_QTg3 <- RICH21_QTg2
RICH21_QTg3$p1inp2vec <- is.element(RICH21_QTg3$player1, player2vector)
RICH21_QTg3$p2inp1vec <- is.element(RICH21_QTg3$player2, player1vector)

addPlayer1 <- RICH21_QTg3[ which(RICH21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH21_QTg3[ which(RICH21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH21_QTg2 <- rbind(RICH21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
RICH21_QTft <- ftable(RICH21_QTg2$player1, RICH21_QTg2$player2)
RICH21_QTft2 <- as.matrix(RICH21_QTft)
numRows <- nrow(RICH21_QTft2)
numCols <- ncol(RICH21_QTft2)
RICH21_QTft3 <- RICH21_QTft2[c(2:numRows) , c(2:numCols)]
RICH21_QTTable <- graph.adjacency(RICH21_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(RICH21_QTTable, vertex.label = V(RICH21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
RICH21_QT.clusterCoef <- transitivity(RICH21_QTTable, type="global") #cluster coefficient
RICH21_QT.degreeCent <- centralization.degree(RICH21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH21_QTftn <- as.network.matrix(RICH21_QTft)
RICH21_QT.netDensity <- network.density(RICH21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH21_QT.entropy <- entropy(RICH21_QTft) #entropy

RICH21_QT.netMx <- cbind(RICH21_QT.netMx, RICH21_QT.clusterCoef, RICH21_QT.degreeCent$centralization,
                         RICH21_QT.netDensity, RICH21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH21_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Goal_F"
STK21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
STK21_Gg2 <- data.frame(STK21_G)
STK21_Gg2 <- STK21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_Gg2$player1
player2vector <- STK21_Gg2$player2
STK21_Gg3 <- STK21_Gg2
STK21_Gg3$p1inp2vec <- is.element(STK21_Gg3$player1, player2vector)
STK21_Gg3$p2inp1vec <- is.element(STK21_Gg3$player2, player1vector)

addPlayer1 <- STK21_Gg3[ which(STK21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_Gg3[ which(STK21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_Gg2 <- rbind(STK21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
STK21_Gft <- ftable(STK21_Gg2$player1, STK21_Gg2$player2)
STK21_Gft2 <- as.matrix(STK21_Gft)
numRows <- nrow(STK21_Gft2)
numCols <- ncol(STK21_Gft2)
STK21_Gft3 <- STK21_Gft2[c(2:numRows) , c(2:numCols)]
STK21_GTable <- graph.adjacency(STK21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(STK21_GTable, vertex.label = V(STK21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
STK21_G.clusterCoef <- transitivity(STK21_GTable, type="global") #cluster coefficient
STK21_G.degreeCent <- centralization.degree(STK21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_Gftn <- as.network.matrix(STK21_Gft)
STK21_G.netDensity <- network.density(STK21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_G.entropy <- entropy(STK21_Gft) #entropy

STK21_G.netMx <- cbind(STK21_G.netMx, STK21_G.clusterCoef, STK21_G.degreeCent$centralization,
                       STK21_G.netDensity, STK21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Behind_F"
STK21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
STK21_Bg2 <- data.frame(STK21_B)
STK21_Bg2 <- STK21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_Bg2$player1
player2vector <- STK21_Bg2$player2
STK21_Bg3 <- STK21_Bg2
STK21_Bg3$p1inp2vec <- is.element(STK21_Bg3$player1, player2vector)
STK21_Bg3$p2inp1vec <- is.element(STK21_Bg3$player2, player1vector)

addPlayer1 <- STK21_Bg3[ which(STK21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_Bg3[ which(STK21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_Bg2 <- rbind(STK21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
STK21_Bft <- ftable(STK21_Bg2$player1, STK21_Bg2$player2)
STK21_Bft2 <- as.matrix(STK21_Bft)
numRows <- nrow(STK21_Bft2)
numCols <- ncol(STK21_Bft2)
STK21_Bft3 <- STK21_Bft2[c(2:numRows) , c(2:numCols)]
STK21_BTable <- graph.adjacency(STK21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(STK21_BTable, vertex.label = V(STK21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
STK21_B.clusterCoef <- transitivity(STK21_BTable, type="global") #cluster coefficient
STK21_B.degreeCent <- centralization.degree(STK21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_Bftn <- as.network.matrix(STK21_Bft)
STK21_B.netDensity <- network.density(STK21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_B.entropy <- entropy(STK21_Bft) #entropy

STK21_B.netMx <- cbind(STK21_B.netMx, STK21_B.clusterCoef, STK21_B.degreeCent$centralization,
                       STK21_B.netDensity, STK21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Stoppage_F"
STK21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
STK21_SFg2 <- data.frame(STK21_SF)
STK21_SFg2 <- STK21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_SFg2$player1
player2vector <- STK21_SFg2$player2
STK21_SFg3 <- STK21_SFg2
STK21_SFg3$p1inp2vec <- is.element(STK21_SFg3$player1, player2vector)
STK21_SFg3$p2inp1vec <- is.element(STK21_SFg3$player2, player1vector)

addPlayer1 <- STK21_SFg3[ which(STK21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_SFg3[ which(STK21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_SFg2 <- rbind(STK21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
STK21_SFft <- ftable(STK21_SFg2$player1, STK21_SFg2$player2)
STK21_SFft2 <- as.matrix(STK21_SFft)
numRows <- nrow(STK21_SFft2)
numCols <- ncol(STK21_SFft2)
STK21_SFft3 <- STK21_SFft2[c(2:numRows) , c(2:numCols)]
STK21_SFTable <- graph.adjacency(STK21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(STK21_SFTable, vertex.label = V(STK21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
STK21_SF.clusterCoef <- transitivity(STK21_SFTable, type="global") #cluster coefficient
STK21_SF.degreeCent <- centralization.degree(STK21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_SFftn <- as.network.matrix(STK21_SFft)
STK21_SF.netDensity <- network.density(STK21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_SF.entropy <- entropy(STK21_SFft) #entropy

STK21_SF.netMx <- cbind(STK21_SF.netMx, STK21_SF.clusterCoef, STK21_SF.degreeCent$centralization,
                        STK21_SF.netDensity, STK21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Turnover_F"
STK21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
STK21_TFg2 <- data.frame(STK21_TF)
STK21_TFg2 <- STK21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_TFg2$player1
player2vector <- STK21_TFg2$player2
STK21_TFg3 <- STK21_TFg2
STK21_TFg3$p1inp2vec <- is.element(STK21_TFg3$player1, player2vector)
STK21_TFg3$p2inp1vec <- is.element(STK21_TFg3$player2, player1vector)

addPlayer1 <- STK21_TFg3[ which(STK21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_TFg3[ which(STK21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_TFg2 <- rbind(STK21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
STK21_TFft <- ftable(STK21_TFg2$player1, STK21_TFg2$player2)
STK21_TFft2 <- as.matrix(STK21_TFft)
numRows <- nrow(STK21_TFft2)
numCols <- ncol(STK21_TFft2)
STK21_TFft3 <- STK21_TFft2[c(2:numRows) , c(2:numCols)]
STK21_TFTable <- graph.adjacency(STK21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(STK21_TFTable, vertex.label = V(STK21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
STK21_TF.clusterCoef <- transitivity(STK21_TFTable, type="global") #cluster coefficient
STK21_TF.degreeCent <- centralization.degree(STK21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_TFftn <- as.network.matrix(STK21_TFft)
STK21_TF.netDensity <- network.density(STK21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_TF.entropy <- entropy(STK21_TFft) #entropy

STK21_TF.netMx <- cbind(STK21_TF.netMx, STK21_TF.clusterCoef, STK21_TF.degreeCent$centralization,
                        STK21_TF.netDensity, STK21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
STK21_SAMg2 <- data.frame(STK21_SAM)
STK21_SAMg2 <- STK21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_SAMg2$player1
player2vector <- STK21_SAMg2$player2
STK21_SAMg3 <- STK21_SAMg2
STK21_SAMg3$p1inp2vec <- is.element(STK21_SAMg3$player1, player2vector)
STK21_SAMg3$p2inp1vec <- is.element(STK21_SAMg3$player2, player1vector)

addPlayer1 <- STK21_SAMg3[ which(STK21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_SAMg3[ which(STK21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_SAMg2 <- rbind(STK21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
STK21_SAMft <- ftable(STK21_SAMg2$player1, STK21_SAMg2$player2)
STK21_SAMft2 <- as.matrix(STK21_SAMft)
numRows <- nrow(STK21_SAMft2)
numCols <- ncol(STK21_SAMft2)
STK21_SAMft3 <- STK21_SAMft2[c(2:numRows) , c(2:numCols)]
STK21_SAMTable <- graph.adjacency(STK21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(STK21_SAMTable, vertex.label = V(STK21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
STK21_SAM.clusterCoef <- transitivity(STK21_SAMTable, type="global") #cluster coefficient
STK21_SAM.degreeCent <- centralization.degree(STK21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_SAMftn <- as.network.matrix(STK21_SAMft)
STK21_SAM.netDensity <- network.density(STK21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_SAM.entropy <- entropy(STK21_SAMft) #entropy

STK21_SAM.netMx <- cbind(STK21_SAM.netMx, STK21_SAM.clusterCoef, STK21_SAM.degreeCent$centralization,
                         STK21_SAM.netDensity, STK21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "STK"
KIoutcome = "Turnover_AM"
STK21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
STK21_TAMg2 <- data.frame(STK21_TAM)
STK21_TAMg2 <- STK21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_TAMg2$player1
player2vector <- STK21_TAMg2$player2
STK21_TAMg3 <- STK21_TAMg2
STK21_TAMg3$p1inp2vec <- is.element(STK21_TAMg3$player1, player2vector)
STK21_TAMg3$p2inp1vec <- is.element(STK21_TAMg3$player2, player1vector)

addPlayer1 <- STK21_TAMg3[ which(STK21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_TAMg3[ which(STK21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_TAMg2 <- rbind(STK21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
STK21_TAMft <- ftable(STK21_TAMg2$player1, STK21_TAMg2$player2)
STK21_TAMft2 <- as.matrix(STK21_TAMft)
numRows <- nrow(STK21_TAMft2)
numCols <- ncol(STK21_TAMft2)
STK21_TAMft3 <- STK21_TAMft2[c(2:numRows) , c(2:numCols)]
STK21_TAMTable <- graph.adjacency(STK21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(STK21_TAMTable, vertex.label = V(STK21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
STK21_TAM.clusterCoef <- transitivity(STK21_TAMTable, type="global") #cluster coefficient
STK21_TAM.degreeCent <- centralization.degree(STK21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_TAMftn <- as.network.matrix(STK21_TAMft)
STK21_TAM.netDensity <- network.density(STK21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_TAM.entropy <- entropy(STK21_TAMft) #entropy

STK21_TAM.netMx <- cbind(STK21_TAM.netMx, STK21_TAM.clusterCoef, STK21_TAM.degreeCent$centralization,
                         STK21_TAM.netDensity, STK21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
STK21_SDMg2 <- data.frame(STK21_SDM)
STK21_SDMg2 <- STK21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_SDMg2$player1
player2vector <- STK21_SDMg2$player2
STK21_SDMg3 <- STK21_SDMg2
STK21_SDMg3$p1inp2vec <- is.element(STK21_SDMg3$player1, player2vector)
STK21_SDMg3$p2inp1vec <- is.element(STK21_SDMg3$player2, player1vector)

addPlayer1 <- STK21_SDMg3[ which(STK21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_SDMg3[ which(STK21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_SDMg2 <- rbind(STK21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
STK21_SDMft <- ftable(STK21_SDMg2$player1, STK21_SDMg2$player2)
STK21_SDMft2 <- as.matrix(STK21_SDMft)
numRows <- nrow(STK21_SDMft2)
numCols <- ncol(STK21_SDMft2)
STK21_SDMft3 <- STK21_SDMft2[c(2:numRows) , c(2:numCols)]
STK21_SDMTable <- graph.adjacency(STK21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(STK21_SDMTable, vertex.label = V(STK21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
STK21_SDM.clusterCoef <- transitivity(STK21_SDMTable, type="global") #cluster coefficient
STK21_SDM.degreeCent <- centralization.degree(STK21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_SDMftn <- as.network.matrix(STK21_SDMft)
STK21_SDM.netDensity <- network.density(STK21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_SDM.entropy <- entropy(STK21_SDMft) #entropy

STK21_SDM.netMx <- cbind(STK21_SDM.netMx, STK21_SDM.clusterCoef, STK21_SDM.degreeCent$centralization,
                         STK21_SDM.netDensity, STK21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "STK"
KIoutcome = "Turnover_DM"
STK21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
STK21_TDMg2 <- data.frame(STK21_TDM)
STK21_TDMg2 <- STK21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_TDMg2$player1
player2vector <- STK21_TDMg2$player2
STK21_TDMg3 <- STK21_TDMg2
STK21_TDMg3$p1inp2vec <- is.element(STK21_TDMg3$player1, player2vector)
STK21_TDMg3$p2inp1vec <- is.element(STK21_TDMg3$player2, player1vector)

addPlayer1 <- STK21_TDMg3[ which(STK21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_TDMg3[ which(STK21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_TDMg2 <- rbind(STK21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
STK21_TDMft <- ftable(STK21_TDMg2$player1, STK21_TDMg2$player2)
STK21_TDMft2 <- as.matrix(STK21_TDMft)
numRows <- nrow(STK21_TDMft2)
numCols <- ncol(STK21_TDMft2)
STK21_TDMft3 <- STK21_TDMft2[c(2:numRows) , c(2:numCols)]
STK21_TDMTable <- graph.adjacency(STK21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(STK21_TDMTable, vertex.label = V(STK21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
STK21_TDM.clusterCoef <- transitivity(STK21_TDMTable, type="global") #cluster coefficient
STK21_TDM.degreeCent <- centralization.degree(STK21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_TDMftn <- as.network.matrix(STK21_TDMft)
STK21_TDM.netDensity <- network.density(STK21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_TDM.entropy <- entropy(STK21_TDMft) #entropy

STK21_TDM.netMx <- cbind(STK21_TDM.netMx, STK21_TDM.clusterCoef, STK21_TDM.degreeCent$centralization,
                         STK21_TDM.netDensity, STK21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Stoppage_D"
STK21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
STK21_SDg2 <- data.frame(STK21_SD)
STK21_SDg2 <- STK21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_SDg2$player1
player2vector <- STK21_SDg2$player2
STK21_SDg3 <- STK21_SDg2
STK21_SDg3$p1inp2vec <- is.element(STK21_SDg3$player1, player2vector)
STK21_SDg3$p2inp1vec <- is.element(STK21_SDg3$player2, player1vector)

addPlayer1 <- STK21_SDg3[ which(STK21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_SDg3[ which(STK21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_SDg2 <- rbind(STK21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
STK21_SDft <- ftable(STK21_SDg2$player1, STK21_SDg2$player2)
STK21_SDft2 <- as.matrix(STK21_SDft)
numRows <- nrow(STK21_SDft2)
numCols <- ncol(STK21_SDft2)
STK21_SDft3 <- STK21_SDft2[c(2:numRows) , c(2:numCols)]
STK21_SDTable <- graph.adjacency(STK21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(STK21_SDTable, vertex.label = V(STK21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
STK21_SD.clusterCoef <- transitivity(STK21_SDTable, type="global") #cluster coefficient
STK21_SD.degreeCent <- centralization.degree(STK21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_SDftn <- as.network.matrix(STK21_SDft)
STK21_SD.netDensity <- network.density(STK21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_SD.entropy <- entropy(STK21_SDft) #entropy

STK21_SD.netMx <- cbind(STK21_SD.netMx, STK21_SD.clusterCoef, STK21_SD.degreeCent$centralization,
                        STK21_SD.netDensity, STK21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "Turnover_D"
STK21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
STK21_TDg2 <- data.frame(STK21_TD)
STK21_TDg2 <- STK21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_TDg2$player1
player2vector <- STK21_TDg2$player2
STK21_TDg3 <- STK21_TDg2
STK21_TDg3$p1inp2vec <- is.element(STK21_TDg3$player1, player2vector)
STK21_TDg3$p2inp1vec <- is.element(STK21_TDg3$player2, player1vector)

addPlayer1 <- STK21_TDg3[ which(STK21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_TDg3[ which(STK21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_TDg2 <- rbind(STK21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
STK21_TDft <- ftable(STK21_TDg2$player1, STK21_TDg2$player2)
STK21_TDft2 <- as.matrix(STK21_TDft)
numRows <- nrow(STK21_TDft2)
numCols <- ncol(STK21_TDft2)
STK21_TDft3 <- STK21_TDft2[c(2:numRows) , c(2:numCols)]
STK21_TDTable <- graph.adjacency(STK21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(STK21_TDTable, vertex.label = V(STK21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
STK21_TD.clusterCoef <- transitivity(STK21_TDTable, type="global") #cluster coefficient
STK21_TD.degreeCent <- centralization.degree(STK21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_TDftn <- as.network.matrix(STK21_TDft)
STK21_TD.netDensity <- network.density(STK21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_TD.entropy <- entropy(STK21_TDft) #entropy

STK21_TD.netMx <- cbind(STK21_TD.netMx, STK21_TD.clusterCoef, STK21_TD.degreeCent$centralization,
                        STK21_TD.netDensity, STK21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
STK21_QTg2 <- data.frame(STK21_QT)
STK21_QTg2 <- STK21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK21_QTg2$player1
player2vector <- STK21_QTg2$player2
STK21_QTg3 <- STK21_QTg2
STK21_QTg3$p1inp2vec <- is.element(STK21_QTg3$player1, player2vector)
STK21_QTg3$p2inp1vec <- is.element(STK21_QTg3$player2, player1vector)

addPlayer1 <- STK21_QTg3[ which(STK21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK21_QTg3[ which(STK21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK21_QTg2 <- rbind(STK21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
STK21_QTft <- ftable(STK21_QTg2$player1, STK21_QTg2$player2)
STK21_QTft2 <- as.matrix(STK21_QTft)
numRows <- nrow(STK21_QTft2)
numCols <- ncol(STK21_QTft2)
STK21_QTft3 <- STK21_QTft2[c(2:numRows) , c(2:numCols)]
STK21_QTTable <- graph.adjacency(STK21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(STK21_QTTable, vertex.label = V(STK21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
STK21_QT.clusterCoef <- transitivity(STK21_QTTable, type="global") #cluster coefficient
STK21_QT.degreeCent <- centralization.degree(STK21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK21_QTftn <- as.network.matrix(STK21_QTft)
STK21_QT.netDensity <- network.density(STK21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK21_QT.entropy <- entropy(STK21_QTft) #entropy

STK21_QT.netMx <- cbind(STK21_QT.netMx, STK21_QT.clusterCoef, STK21_QT.degreeCent$centralization,
                        STK21_QT.netDensity, STK21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK21_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************
#NA

round = 21
teamName = "SYD"
KIoutcome = "Goal_F"
SYD21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
SYD21_Gg2 <- data.frame(SYD21_G)
SYD21_Gg2 <- SYD21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_Gg2$player1
player2vector <- SYD21_Gg2$player2
SYD21_Gg3 <- SYD21_Gg2
SYD21_Gg3$p1inp2vec <- is.element(SYD21_Gg3$player1, player2vector)
SYD21_Gg3$p2inp1vec <- is.element(SYD21_Gg3$player2, player1vector)

addPlayer1 <- SYD21_Gg3[ which(SYD21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_Gg3[ which(SYD21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_Gg2 <- rbind(SYD21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
SYD21_Gft <- ftable(SYD21_Gg2$player1, SYD21_Gg2$player2)
SYD21_Gft2 <- as.matrix(SYD21_Gft)
numRows <- nrow(SYD21_Gft2)
numCols <- ncol(SYD21_Gft2)
SYD21_Gft3 <- SYD21_Gft2[c(2:numRows) , c(2:numCols)]
SYD21_GTable <- graph.adjacency(SYD21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(SYD21_GTable, vertex.label = V(SYD21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
SYD21_G.clusterCoef <- transitivity(SYD21_GTable, type="global") #cluster coefficient
SYD21_G.degreeCent <- centralization.degree(SYD21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_Gftn <- as.network.matrix(SYD21_Gft)
SYD21_G.netDensity <- network.density(SYD21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_G.entropy <- entropy(SYD21_Gft) #entropy

SYD21_G.netMx <- cbind(SYD21_G.netMx, SYD21_G.clusterCoef, SYD21_G.degreeCent$centralization,
                       SYD21_G.netDensity, SYD21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "SYD"
KIoutcome = "Behind_F"
SYD21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
SYD21_Bg2 <- data.frame(SYD21_B)
SYD21_Bg2 <- SYD21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_Bg2$player1
player2vector <- SYD21_Bg2$player2
SYD21_Bg3 <- SYD21_Bg2
SYD21_Bg3$p1inp2vec <- is.element(SYD21_Bg3$player1, player2vector)
SYD21_Bg3$p2inp1vec <- is.element(SYD21_Bg3$player2, player1vector)

addPlayer1 <- SYD21_Bg3[ which(SYD21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_Bg3[ which(SYD21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_Bg2 <- rbind(SYD21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
SYD21_Bft <- ftable(SYD21_Bg2$player1, SYD21_Bg2$player2)
SYD21_Bft2 <- as.matrix(SYD21_Bft)
numRows <- nrow(SYD21_Bft2)
numCols <- ncol(SYD21_Bft2)
SYD21_Bft3 <- SYD21_Bft2[c(2:numRows) , c(2:numCols)]
SYD21_BTable <- graph.adjacency(SYD21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(SYD21_BTable, vertex.label = V(SYD21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
SYD21_B.clusterCoef <- transitivity(SYD21_BTable, type="global") #cluster coefficient
SYD21_B.degreeCent <- centralization.degree(SYD21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_Bftn <- as.network.matrix(SYD21_Bft)
SYD21_B.netDensity <- network.density(SYD21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_B.entropy <- entropy(SYD21_Bft) #entropy

SYD21_B.netMx <- cbind(SYD21_B.netMx, SYD21_B.clusterCoef, SYD21_B.degreeCent$centralization,
                       SYD21_B.netDensity, SYD21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
SYD21_SFg2 <- data.frame(SYD21_SF)
SYD21_SFg2 <- SYD21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_SFg2$player1
player2vector <- SYD21_SFg2$player2
SYD21_SFg3 <- SYD21_SFg2
SYD21_SFg3$p1inp2vec <- is.element(SYD21_SFg3$player1, player2vector)
SYD21_SFg3$p2inp1vec <- is.element(SYD21_SFg3$player2, player1vector)

addPlayer1 <- SYD21_SFg3[ which(SYD21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_SFg3[ which(SYD21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_SFg2 <- rbind(SYD21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
SYD21_SFft <- ftable(SYD21_SFg2$player1, SYD21_SFg2$player2)
SYD21_SFft2 <- as.matrix(SYD21_SFft)
numRows <- nrow(SYD21_SFft2)
numCols <- ncol(SYD21_SFft2)
SYD21_SFft3 <- SYD21_SFft2[c(2:numRows) , c(2:numCols)]
SYD21_SFTable <- graph.adjacency(SYD21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(SYD21_SFTable, vertex.label = V(SYD21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
SYD21_SF.clusterCoef <- transitivity(SYD21_SFTable, type="global") #cluster coefficient
SYD21_SF.degreeCent <- centralization.degree(SYD21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_SFftn <- as.network.matrix(SYD21_SFft)
SYD21_SF.netDensity <- network.density(SYD21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_SF.entropy <- entropy(SYD21_SFft) #entropy

SYD21_SF.netMx <- cbind(SYD21_SF.netMx, SYD21_SF.clusterCoef, SYD21_SF.degreeCent$centralization,
                        SYD21_SF.netDensity, SYD21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************
#NA

round = 21
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
SYD21_TFg2 <- data.frame(SYD21_TF)
SYD21_TFg2 <- SYD21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_TFg2$player1
player2vector <- SYD21_TFg2$player2
SYD21_TFg3 <- SYD21_TFg2
SYD21_TFg3$p1inp2vec <- is.element(SYD21_TFg3$player1, player2vector)
SYD21_TFg3$p2inp1vec <- is.element(SYD21_TFg3$player2, player1vector)

addPlayer1 <- SYD21_TFg3[ which(SYD21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_TFg3[ which(SYD21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_TFg2 <- rbind(SYD21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
SYD21_TFft <- ftable(SYD21_TFg2$player1, SYD21_TFg2$player2)
SYD21_TFft2 <- as.matrix(SYD21_TFft)
numRows <- nrow(SYD21_TFft2)
numCols <- ncol(SYD21_TFft2)
SYD21_TFft3 <- SYD21_TFft2[c(2:numRows) , c(2:numCols)]
SYD21_TFTable <- graph.adjacency(SYD21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(SYD21_TFTable, vertex.label = V(SYD21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
SYD21_TF.clusterCoef <- transitivity(SYD21_TFTable, type="global") #cluster coefficient
SYD21_TF.degreeCent <- centralization.degree(SYD21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_TFftn <- as.network.matrix(SYD21_TFft)
SYD21_TF.netDensity <- network.density(SYD21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_TF.entropy <- entropy(SYD21_TFft) #entropy

SYD21_TF.netMx <- cbind(SYD21_TF.netMx, SYD21_TF.clusterCoef, SYD21_TF.degreeCent$centralization,
                        SYD21_TF.netDensity, SYD21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
SYD21_SAMg2 <- data.frame(SYD21_SAM)
SYD21_SAMg2 <- SYD21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_SAMg2$player1
player2vector <- SYD21_SAMg2$player2
SYD21_SAMg3 <- SYD21_SAMg2
SYD21_SAMg3$p1inp2vec <- is.element(SYD21_SAMg3$player1, player2vector)
SYD21_SAMg3$p2inp1vec <- is.element(SYD21_SAMg3$player2, player1vector)

addPlayer1 <- SYD21_SAMg3[ which(SYD21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_SAMg3[ which(SYD21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_SAMg2 <- rbind(SYD21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
SYD21_SAMft <- ftable(SYD21_SAMg2$player1, SYD21_SAMg2$player2)
SYD21_SAMft2 <- as.matrix(SYD21_SAMft)
numRows <- nrow(SYD21_SAMft2)
numCols <- ncol(SYD21_SAMft2)
SYD21_SAMft3 <- SYD21_SAMft2[c(2:numRows) , c(2:numCols)]
SYD21_SAMTable <- graph.adjacency(SYD21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(SYD21_SAMTable, vertex.label = V(SYD21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
SYD21_SAM.clusterCoef <- transitivity(SYD21_SAMTable, type="global") #cluster coefficient
SYD21_SAM.degreeCent <- centralization.degree(SYD21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_SAMftn <- as.network.matrix(SYD21_SAMft)
SYD21_SAM.netDensity <- network.density(SYD21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_SAM.entropy <- entropy(SYD21_SAMft) #entropy

SYD21_SAM.netMx <- cbind(SYD21_SAM.netMx, SYD21_SAM.clusterCoef, SYD21_SAM.degreeCent$centralization,
                         SYD21_SAM.netDensity, SYD21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
SYD21_TAMg2 <- data.frame(SYD21_TAM)
SYD21_TAMg2 <- SYD21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_TAMg2$player1
player2vector <- SYD21_TAMg2$player2
SYD21_TAMg3 <- SYD21_TAMg2
SYD21_TAMg3$p1inp2vec <- is.element(SYD21_TAMg3$player1, player2vector)
SYD21_TAMg3$p2inp1vec <- is.element(SYD21_TAMg3$player2, player1vector)

addPlayer1 <- SYD21_TAMg3[ which(SYD21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_TAMg3[ which(SYD21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_TAMg2 <- rbind(SYD21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
SYD21_TAMft <- ftable(SYD21_TAMg2$player1, SYD21_TAMg2$player2)
SYD21_TAMft2 <- as.matrix(SYD21_TAMft)
numRows <- nrow(SYD21_TAMft2)
numCols <- ncol(SYD21_TAMft2)
SYD21_TAMft3 <- SYD21_TAMft2[c(2:numRows) , c(2:numCols)]
SYD21_TAMTable <- graph.adjacency(SYD21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(SYD21_TAMTable, vertex.label = V(SYD21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
SYD21_TAM.clusterCoef <- transitivity(SYD21_TAMTable, type="global") #cluster coefficient
SYD21_TAM.degreeCent <- centralization.degree(SYD21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_TAMftn <- as.network.matrix(SYD21_TAMft)
SYD21_TAM.netDensity <- network.density(SYD21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_TAM.entropy <- entropy(SYD21_TAMft) #entropy

SYD21_TAM.netMx <- cbind(SYD21_TAM.netMx, SYD21_TAM.clusterCoef, SYD21_TAM.degreeCent$centralization,
                         SYD21_TAM.netDensity, SYD21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************

round = 21
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
SYD21_SDMg2 <- data.frame(SYD21_SDM)
SYD21_SDMg2 <- SYD21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_SDMg2$player1
player2vector <- SYD21_SDMg2$player2
SYD21_SDMg3 <- SYD21_SDMg2
SYD21_SDMg3$p1inp2vec <- is.element(SYD21_SDMg3$player1, player2vector)
SYD21_SDMg3$p2inp1vec <- is.element(SYD21_SDMg3$player2, player1vector)

addPlayer1 <- SYD21_SDMg3[ which(SYD21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_SDMg3[ which(SYD21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_SDMg2 <- rbind(SYD21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
SYD21_SDMft <- ftable(SYD21_SDMg2$player1, SYD21_SDMg2$player2)
SYD21_SDMft2 <- as.matrix(SYD21_SDMft)
numRows <- nrow(SYD21_SDMft2)
numCols <- ncol(SYD21_SDMft2)
SYD21_SDMft3 <- SYD21_SDMft2[c(2:numRows) , c(2:numCols)]
SYD21_SDMTable <- graph.adjacency(SYD21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(SYD21_SDMTable, vertex.label = V(SYD21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
SYD21_SDM.clusterCoef <- transitivity(SYD21_SDMTable, type="global") #cluster coefficient
SYD21_SDM.degreeCent <- centralization.degree(SYD21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_SDMftn <- as.network.matrix(SYD21_SDMft)
SYD21_SDM.netDensity <- network.density(SYD21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_SDM.entropy <- entropy(SYD21_SDMft) #entropy

SYD21_SDM.netMx <- cbind(SYD21_SDM.netMx, SYD21_SDM.clusterCoef, SYD21_SDM.degreeCent$centralization,
                         SYD21_SDM.netDensity, SYD21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************
#NA

round = 21
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
SYD21_TDMg2 <- data.frame(SYD21_TDM)
SYD21_TDMg2 <- SYD21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_TDMg2$player1
player2vector <- SYD21_TDMg2$player2
SYD21_TDMg3 <- SYD21_TDMg2
SYD21_TDMg3$p1inp2vec <- is.element(SYD21_TDMg3$player1, player2vector)
SYD21_TDMg3$p2inp1vec <- is.element(SYD21_TDMg3$player2, player1vector)

addPlayer1 <- SYD21_TDMg3[ which(SYD21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_TDMg3[ which(SYD21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_TDMg2 <- rbind(SYD21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
SYD21_TDMft <- ftable(SYD21_TDMg2$player1, SYD21_TDMg2$player2)
SYD21_TDMft2 <- as.matrix(SYD21_TDMft)
numRows <- nrow(SYD21_TDMft2)
numCols <- ncol(SYD21_TDMft2)
SYD21_TDMft3 <- SYD21_TDMft2[c(2:numRows) , c(2:numCols)]
SYD21_TDMTable <- graph.adjacency(SYD21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(SYD21_TDMTable, vertex.label = V(SYD21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
SYD21_TDM.clusterCoef <- transitivity(SYD21_TDMTable, type="global") #cluster coefficient
SYD21_TDM.degreeCent <- centralization.degree(SYD21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_TDMftn <- as.network.matrix(SYD21_TDMft)
SYD21_TDM.netDensity <- network.density(SYD21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_TDM.entropy <- entropy(SYD21_TDMft) #entropy

SYD21_TDM.netMx <- cbind(SYD21_TDM.netMx, SYD21_TDM.clusterCoef, SYD21_TDM.degreeCent$centralization,
                         SYD21_TDM.netDensity, SYD21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
SYD21_SDg2 <- data.frame(SYD21_SD)
SYD21_SDg2 <- SYD21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_SDg2$player1
player2vector <- SYD21_SDg2$player2
SYD21_SDg3 <- SYD21_SDg2
SYD21_SDg3$p1inp2vec <- is.element(SYD21_SDg3$player1, player2vector)
SYD21_SDg3$p2inp1vec <- is.element(SYD21_SDg3$player2, player1vector)

addPlayer1 <- SYD21_SDg3[ which(SYD21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_SDg3[ which(SYD21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_SDg2 <- rbind(SYD21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
SYD21_SDft <- ftable(SYD21_SDg2$player1, SYD21_SDg2$player2)
SYD21_SDft2 <- as.matrix(SYD21_SDft)
numRows <- nrow(SYD21_SDft2)
numCols <- ncol(SYD21_SDft2)
SYD21_SDft3 <- SYD21_SDft2[c(2:numRows) , c(2:numCols)]
SYD21_SDTable <- graph.adjacency(SYD21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(SYD21_SDTable, vertex.label = V(SYD21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
SYD21_SD.clusterCoef <- transitivity(SYD21_SDTable, type="global") #cluster coefficient
SYD21_SD.degreeCent <- centralization.degree(SYD21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_SDftn <- as.network.matrix(SYD21_SDft)
SYD21_SD.netDensity <- network.density(SYD21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_SD.entropy <- entropy(SYD21_SDft) #entropy

SYD21_SD.netMx <- cbind(SYD21_SD.netMx, SYD21_SD.clusterCoef, SYD21_SD.degreeCent$centralization,
                        SYD21_SD.netDensity, SYD21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
SYD21_TDg2 <- data.frame(SYD21_TD)
SYD21_TDg2 <- SYD21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_TDg2$player1
player2vector <- SYD21_TDg2$player2
SYD21_TDg3 <- SYD21_TDg2
SYD21_TDg3$p1inp2vec <- is.element(SYD21_TDg3$player1, player2vector)
SYD21_TDg3$p2inp1vec <- is.element(SYD21_TDg3$player2, player1vector)

addPlayer1 <- SYD21_TDg3[ which(SYD21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_TDg3[ which(SYD21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_TDg2 <- rbind(SYD21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
SYD21_TDft <- ftable(SYD21_TDg2$player1, SYD21_TDg2$player2)
SYD21_TDft2 <- as.matrix(SYD21_TDft)
numRows <- nrow(SYD21_TDft2)
numCols <- ncol(SYD21_TDft2)
SYD21_TDft3 <- SYD21_TDft2[c(2:numRows) , c(2:numCols)]
SYD21_TDTable <- graph.adjacency(SYD21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(SYD21_TDTable, vertex.label = V(SYD21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
SYD21_TD.clusterCoef <- transitivity(SYD21_TDTable, type="global") #cluster coefficient
SYD21_TD.degreeCent <- centralization.degree(SYD21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_TDftn <- as.network.matrix(SYD21_TDft)
SYD21_TD.netDensity <- network.density(SYD21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_TD.entropy <- entropy(SYD21_TDft) #entropy

SYD21_TD.netMx <- cbind(SYD21_TD.netMx, SYD21_TD.clusterCoef, SYD21_TD.degreeCent$centralization,
                        SYD21_TD.netDensity, SYD21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************

round = 21
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
SYD21_QTg2 <- data.frame(SYD21_QT)
SYD21_QTg2 <- SYD21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD21_QTg2$player1
player2vector <- SYD21_QTg2$player2
SYD21_QTg3 <- SYD21_QTg2
SYD21_QTg3$p1inp2vec <- is.element(SYD21_QTg3$player1, player2vector)
SYD21_QTg3$p2inp1vec <- is.element(SYD21_QTg3$player2, player1vector)

addPlayer1 <- SYD21_QTg3[ which(SYD21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD21_QTg3[ which(SYD21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD21_QTg2 <- rbind(SYD21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
SYD21_QTft <- ftable(SYD21_QTg2$player1, SYD21_QTg2$player2)
SYD21_QTft2 <- as.matrix(SYD21_QTft)
numRows <- nrow(SYD21_QTft2)
numCols <- ncol(SYD21_QTft2)
SYD21_QTft3 <- SYD21_QTft2[c(2:numRows) , c(2:numCols)]
SYD21_QTTable <- graph.adjacency(SYD21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(SYD21_QTTable, vertex.label = V(SYD21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
SYD21_QT.clusterCoef <- transitivity(SYD21_QTTable, type="global") #cluster coefficient
SYD21_QT.degreeCent <- centralization.degree(SYD21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD21_QTftn <- as.network.matrix(SYD21_QTft)
SYD21_QT.netDensity <- network.density(SYD21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD21_QT.entropy <- entropy(SYD21_QTft) #entropy

SYD21_QT.netMx <- cbind(SYD21_QT.netMx, SYD21_QT.clusterCoef, SYD21_QT.degreeCent$centralization,
                        SYD21_QT.netDensity, SYD21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD21_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "WB"
KIoutcome = "Goal_F"
WB21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
WB21_Gg2 <- data.frame(WB21_G)
WB21_Gg2 <- WB21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_Gg2$player1
player2vector <- WB21_Gg2$player2
WB21_Gg3 <- WB21_Gg2
WB21_Gg3$p1inp2vec <- is.element(WB21_Gg3$player1, player2vector)
WB21_Gg3$p2inp1vec <- is.element(WB21_Gg3$player2, player1vector)

addPlayer1 <- WB21_Gg3[ which(WB21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_Gg3[ which(WB21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_Gg2 <- rbind(WB21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
WB21_Gft <- ftable(WB21_Gg2$player1, WB21_Gg2$player2)
WB21_Gft2 <- as.matrix(WB21_Gft)
numRows <- nrow(WB21_Gft2)
numCols <- ncol(WB21_Gft2)
WB21_Gft3 <- WB21_Gft2[c(2:numRows) , c(2:numCols)]
WB21_GTable <- graph.adjacency(WB21_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(WB21_GTable, vertex.label = V(WB21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
WB21_G.clusterCoef <- transitivity(WB21_GTable, type="global") #cluster coefficient
WB21_G.degreeCent <- centralization.degree(WB21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_Gftn <- as.network.matrix(WB21_Gft)
WB21_G.netDensity <- network.density(WB21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_G.entropy <- entropy(WB21_Gft) #entropy

WB21_G.netMx <- cbind(WB21_G.netMx, WB21_G.clusterCoef, WB21_G.degreeCent$centralization,
                      WB21_G.netDensity, WB21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************
#NA

round = 21
teamName = "WB"
KIoutcome = "Behind_F"
WB21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
WB21_Bg2 <- data.frame(WB21_B)
WB21_Bg2 <- WB21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_Bg2$player1
player2vector <- WB21_Bg2$player2
WB21_Bg3 <- WB21_Bg2
WB21_Bg3$p1inp2vec <- is.element(WB21_Bg3$player1, player2vector)
WB21_Bg3$p2inp1vec <- is.element(WB21_Bg3$player2, player1vector)

addPlayer1 <- WB21_Bg3[ which(WB21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_Bg3[ which(WB21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_Bg2 <- rbind(WB21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
WB21_Bft <- ftable(WB21_Bg2$player1, WB21_Bg2$player2)
WB21_Bft2 <- as.matrix(WB21_Bft)
numRows <- nrow(WB21_Bft2)
numCols <- ncol(WB21_Bft2)
WB21_Bft3 <- WB21_Bft2[c(2:numRows) , c(2:numCols)]
WB21_BTable <- graph.adjacency(WB21_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(WB21_BTable, vertex.label = V(WB21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
WB21_B.clusterCoef <- transitivity(WB21_BTable, type="global") #cluster coefficient
WB21_B.degreeCent <- centralization.degree(WB21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_Bftn <- as.network.matrix(WB21_Bft)
WB21_B.netDensity <- network.density(WB21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_B.entropy <- entropy(WB21_Bft) #entropy

WB21_B.netMx <- cbind(WB21_B.netMx, WB21_B.clusterCoef, WB21_B.degreeCent$centralization,
                      WB21_B.netDensity, WB21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************

round = 21
teamName = "WB"
KIoutcome = "Stoppage_F"
WB21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
WB21_SFg2 <- data.frame(WB21_SF)
WB21_SFg2 <- WB21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_SFg2$player1
player2vector <- WB21_SFg2$player2
WB21_SFg3 <- WB21_SFg2
WB21_SFg3$p1inp2vec <- is.element(WB21_SFg3$player1, player2vector)
WB21_SFg3$p2inp1vec <- is.element(WB21_SFg3$player2, player1vector)

addPlayer1 <- WB21_SFg3[ which(WB21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_SFg3[ which(WB21_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_SFg2 <- rbind(WB21_SFg2, addPlayers)

#ROUND 21, FWD Stoppage graph using weighted edges
WB21_SFft <- ftable(WB21_SFg2$player1, WB21_SFg2$player2)
WB21_SFft2 <- as.matrix(WB21_SFft)
numRows <- nrow(WB21_SFft2)
numCols <- ncol(WB21_SFft2)
WB21_SFft3 <- WB21_SFft2[c(2:numRows) , c(2:numCols)]
WB21_SFTable <- graph.adjacency(WB21_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(WB21_SFTable, vertex.label = V(WB21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
WB21_SF.clusterCoef <- transitivity(WB21_SFTable, type="global") #cluster coefficient
WB21_SF.degreeCent <- centralization.degree(WB21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_SFftn <- as.network.matrix(WB21_SFft)
WB21_SF.netDensity <- network.density(WB21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_SF.entropy <- entropy(WB21_SFft) #entropy

WB21_SF.netMx <- cbind(WB21_SF.netMx, WB21_SF.clusterCoef, WB21_SF.degreeCent$centralization,
                       WB21_SF.netDensity, WB21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************
#NA

round = 21
teamName = "WB"
KIoutcome = "Turnover_F"
WB21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
WB21_TFg2 <- data.frame(WB21_TF)
WB21_TFg2 <- WB21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_TFg2$player1
player2vector <- WB21_TFg2$player2
WB21_TFg3 <- WB21_TFg2
WB21_TFg3$p1inp2vec <- is.element(WB21_TFg3$player1, player2vector)
WB21_TFg3$p2inp1vec <- is.element(WB21_TFg3$player2, player1vector)

addPlayer1 <- WB21_TFg3[ which(WB21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_TFg3[ which(WB21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_TFg2 <- rbind(WB21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
WB21_TFft <- ftable(WB21_TFg2$player1, WB21_TFg2$player2)
WB21_TFft2 <- as.matrix(WB21_TFft)
numRows <- nrow(WB21_TFft2)
numCols <- ncol(WB21_TFft2)
WB21_TFft3 <- WB21_TFft2[c(2:numRows) , c(2:numCols)]
WB21_TFTable <- graph.adjacency(WB21_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(WB21_TFTable, vertex.label = V(WB21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
WB21_TF.clusterCoef <- transitivity(WB21_TFTable, type="global") #cluster coefficient
WB21_TF.degreeCent <- centralization.degree(WB21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_TFftn <- as.network.matrix(WB21_TFft)
WB21_TF.netDensity <- network.density(WB21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_TF.entropy <- entropy(WB21_TFft) #entropy

WB21_TF.netMx <- cbind(WB21_TF.netMx, WB21_TF.clusterCoef, WB21_TF.degreeCent$centralization,
                       WB21_TF.netDensity, WB21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************

round = 21
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
WB21_SAMg2 <- data.frame(WB21_SAM)
WB21_SAMg2 <- WB21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_SAMg2$player1
player2vector <- WB21_SAMg2$player2
WB21_SAMg3 <- WB21_SAMg2
WB21_SAMg3$p1inp2vec <- is.element(WB21_SAMg3$player1, player2vector)
WB21_SAMg3$p2inp1vec <- is.element(WB21_SAMg3$player2, player1vector)

addPlayer1 <- WB21_SAMg3[ which(WB21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_SAMg3[ which(WB21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_SAMg2 <- rbind(WB21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
WB21_SAMft <- ftable(WB21_SAMg2$player1, WB21_SAMg2$player2)
WB21_SAMft2 <- as.matrix(WB21_SAMft)
numRows <- nrow(WB21_SAMft2)
numCols <- ncol(WB21_SAMft2)
WB21_SAMft3 <- WB21_SAMft2[c(2:numRows) , c(2:numCols)]
WB21_SAMTable <- graph.adjacency(WB21_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(WB21_SAMTable, vertex.label = V(WB21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
WB21_SAM.clusterCoef <- transitivity(WB21_SAMTable, type="global") #cluster coefficient
WB21_SAM.degreeCent <- centralization.degree(WB21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_SAMftn <- as.network.matrix(WB21_SAMft)
WB21_SAM.netDensity <- network.density(WB21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_SAM.entropy <- entropy(WB21_SAMft) #entropy

WB21_SAM.netMx <- cbind(WB21_SAM.netMx, WB21_SAM.clusterCoef, WB21_SAM.degreeCent$centralization,
                        WB21_SAM.netDensity, WB21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************

round = 21
teamName = "WB"
KIoutcome = "Turnover_AM"
WB21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
WB21_TAMg2 <- data.frame(WB21_TAM)
WB21_TAMg2 <- WB21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_TAMg2$player1
player2vector <- WB21_TAMg2$player2
WB21_TAMg3 <- WB21_TAMg2
WB21_TAMg3$p1inp2vec <- is.element(WB21_TAMg3$player1, player2vector)
WB21_TAMg3$p2inp1vec <- is.element(WB21_TAMg3$player2, player1vector)

addPlayer1 <- WB21_TAMg3[ which(WB21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_TAMg3[ which(WB21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_TAMg2 <- rbind(WB21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
WB21_TAMft <- ftable(WB21_TAMg2$player1, WB21_TAMg2$player2)
WB21_TAMft2 <- as.matrix(WB21_TAMft)
numRows <- nrow(WB21_TAMft2)
numCols <- ncol(WB21_TAMft2)
WB21_TAMft3 <- WB21_TAMft2[c(2:numRows) , c(2:numCols)]
WB21_TAMTable <- graph.adjacency(WB21_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(WB21_TAMTable, vertex.label = V(WB21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
WB21_TAM.clusterCoef <- transitivity(WB21_TAMTable, type="global") #cluster coefficient
WB21_TAM.degreeCent <- centralization.degree(WB21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_TAMftn <- as.network.matrix(WB21_TAMft)
WB21_TAM.netDensity <- network.density(WB21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_TAM.entropy <- entropy(WB21_TAMft) #entropy

WB21_TAM.netMx <- cbind(WB21_TAM.netMx, WB21_TAM.clusterCoef, WB21_TAM.degreeCent$centralization,
                        WB21_TAM.netDensity, WB21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
WB21_SDMg2 <- data.frame(WB21_SDM)
WB21_SDMg2 <- WB21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_SDMg2$player1
player2vector <- WB21_SDMg2$player2
WB21_SDMg3 <- WB21_SDMg2
WB21_SDMg3$p1inp2vec <- is.element(WB21_SDMg3$player1, player2vector)
WB21_SDMg3$p2inp1vec <- is.element(WB21_SDMg3$player2, player1vector)

addPlayer1 <- WB21_SDMg3[ which(WB21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_SDMg3[ which(WB21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_SDMg2 <- rbind(WB21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
WB21_SDMft <- ftable(WB21_SDMg2$player1, WB21_SDMg2$player2)
WB21_SDMft2 <- as.matrix(WB21_SDMft)
numRows <- nrow(WB21_SDMft2)
numCols <- ncol(WB21_SDMft2)
WB21_SDMft3 <- WB21_SDMft2[c(2:numRows) , c(2:numCols)]
WB21_SDMTable <- graph.adjacency(WB21_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(WB21_SDMTable, vertex.label = V(WB21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
WB21_SDM.clusterCoef <- transitivity(WB21_SDMTable, type="global") #cluster coefficient
WB21_SDM.degreeCent <- centralization.degree(WB21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_SDMftn <- as.network.matrix(WB21_SDMft)
WB21_SDM.netDensity <- network.density(WB21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_SDM.entropy <- entropy(WB21_SDMft) #entropy

WB21_SDM.netMx <- cbind(WB21_SDM.netMx, WB21_SDM.clusterCoef, WB21_SDM.degreeCent$centralization,
                        WB21_SDM.netDensity, WB21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "WB"
KIoutcome = "Turnover_DM"
WB21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
WB21_TDMg2 <- data.frame(WB21_TDM)
WB21_TDMg2 <- WB21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_TDMg2$player1
player2vector <- WB21_TDMg2$player2
WB21_TDMg3 <- WB21_TDMg2
WB21_TDMg3$p1inp2vec <- is.element(WB21_TDMg3$player1, player2vector)
WB21_TDMg3$p2inp1vec <- is.element(WB21_TDMg3$player2, player1vector)

addPlayer1 <- WB21_TDMg3[ which(WB21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_TDMg3[ which(WB21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_TDMg2 <- rbind(WB21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
WB21_TDMft <- ftable(WB21_TDMg2$player1, WB21_TDMg2$player2)
WB21_TDMft2 <- as.matrix(WB21_TDMft)
numRows <- nrow(WB21_TDMft2)
numCols <- ncol(WB21_TDMft2)
WB21_TDMft3 <- WB21_TDMft2[c(2:numRows) , c(2:numCols)]
WB21_TDMTable <- graph.adjacency(WB21_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(WB21_TDMTable, vertex.label = V(WB21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
WB21_TDM.clusterCoef <- transitivity(WB21_TDMTable, type="global") #cluster coefficient
WB21_TDM.degreeCent <- centralization.degree(WB21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_TDMftn <- as.network.matrix(WB21_TDMft)
WB21_TDM.netDensity <- network.density(WB21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_TDM.entropy <- entropy(WB21_TDMft) #entropy

WB21_TDM.netMx <- cbind(WB21_TDM.netMx, WB21_TDM.clusterCoef, WB21_TDM.degreeCent$centralization,
                        WB21_TDM.netDensity, WB21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "WB"
KIoutcome = "Stoppage_D"
WB21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
WB21_SDg2 <- data.frame(WB21_SD)
WB21_SDg2 <- WB21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_SDg2$player1
player2vector <- WB21_SDg2$player2
WB21_SDg3 <- WB21_SDg2
WB21_SDg3$p1inp2vec <- is.element(WB21_SDg3$player1, player2vector)
WB21_SDg3$p2inp1vec <- is.element(WB21_SDg3$player2, player1vector)

addPlayer1 <- WB21_SDg3[ which(WB21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_SDg3[ which(WB21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_SDg2 <- rbind(WB21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
WB21_SDft <- ftable(WB21_SDg2$player1, WB21_SDg2$player2)
WB21_SDft2 <- as.matrix(WB21_SDft)
numRows <- nrow(WB21_SDft2)
numCols <- ncol(WB21_SDft2)
WB21_SDft3 <- WB21_SDft2[c(2:numRows) , c(2:numCols)]
WB21_SDTable <- graph.adjacency(WB21_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(WB21_SDTable, vertex.label = V(WB21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
WB21_SD.clusterCoef <- transitivity(WB21_SDTable, type="global") #cluster coefficient
WB21_SD.degreeCent <- centralization.degree(WB21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_SDftn <- as.network.matrix(WB21_SDft)
WB21_SD.netDensity <- network.density(WB21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_SD.entropy <- entropy(WB21_SDft) #entropy

WB21_SD.netMx <- cbind(WB21_SD.netMx, WB21_SD.clusterCoef, WB21_SD.degreeCent$centralization,
                       WB21_SD.netDensity, WB21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************

round = 21
teamName = "WB"
KIoutcome = "Turnover_D"
WB21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
WB21_TDg2 <- data.frame(WB21_TD)
WB21_TDg2 <- WB21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_TDg2$player1
player2vector <- WB21_TDg2$player2
WB21_TDg3 <- WB21_TDg2
WB21_TDg3$p1inp2vec <- is.element(WB21_TDg3$player1, player2vector)
WB21_TDg3$p2inp1vec <- is.element(WB21_TDg3$player2, player1vector)

addPlayer1 <- WB21_TDg3[ which(WB21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_TDg3[ which(WB21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_TDg2 <- rbind(WB21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
WB21_TDft <- ftable(WB21_TDg2$player1, WB21_TDg2$player2)
WB21_TDft2 <- as.matrix(WB21_TDft)
numRows <- nrow(WB21_TDft2)
numCols <- ncol(WB21_TDft2)
WB21_TDft3 <- WB21_TDft2[c(2:numRows) , c(2:numCols)]
WB21_TDTable <- graph.adjacency(WB21_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(WB21_TDTable, vertex.label = V(WB21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
WB21_TD.clusterCoef <- transitivity(WB21_TDTable, type="global") #cluster coefficient
WB21_TD.degreeCent <- centralization.degree(WB21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_TDftn <- as.network.matrix(WB21_TDft)
WB21_TD.netDensity <- network.density(WB21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_TD.entropy <- entropy(WB21_TDft) #entropy

WB21_TD.netMx <- cbind(WB21_TD.netMx, WB21_TD.clusterCoef, WB21_TD.degreeCent$centralization,
                       WB21_TD.netDensity, WB21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
WB21_QTg2 <- data.frame(WB21_QT)
WB21_QTg2 <- WB21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB21_QTg2$player1
player2vector <- WB21_QTg2$player2
WB21_QTg3 <- WB21_QTg2
WB21_QTg3$p1inp2vec <- is.element(WB21_QTg3$player1, player2vector)
WB21_QTg3$p2inp1vec <- is.element(WB21_QTg3$player2, player1vector)

addPlayer1 <- WB21_QTg3[ which(WB21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB21_QTg3[ which(WB21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB21_QTg2 <- rbind(WB21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
WB21_QTft <- ftable(WB21_QTg2$player1, WB21_QTg2$player2)
WB21_QTft2 <- as.matrix(WB21_QTft)
numRows <- nrow(WB21_QTft2)
numCols <- ncol(WB21_QTft2)
WB21_QTft3 <- WB21_QTft2[c(2:numRows) , c(2:numCols)]
WB21_QTTable <- graph.adjacency(WB21_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(WB21_QTTable, vertex.label = V(WB21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
WB21_QT.clusterCoef <- transitivity(WB21_QTTable, type="global") #cluster coefficient
WB21_QT.degreeCent <- centralization.degree(WB21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB21_QTftn <- as.network.matrix(WB21_QTft)
WB21_QT.netDensity <- network.density(WB21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB21_QT.entropy <- entropy(WB21_QTft) #entropy

WB21_QT.netMx <- cbind(WB21_QT.netMx, WB21_QT.clusterCoef, WB21_QT.degreeCent$centralization,
                       WB21_QT.netDensity, WB21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB21_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 21
##

#ROUND 21, Goal***************************************************************

round = 21
teamName = "WCE"
KIoutcome = "Goal_F"
WCE21_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Goal with weighted edges
WCE21_Gg2 <- data.frame(WCE21_G)
WCE21_Gg2 <- WCE21_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_Gg2$player1
player2vector <- WCE21_Gg2$player2
WCE21_Gg3 <- WCE21_Gg2
WCE21_Gg3$p1inp2vec <- is.element(WCE21_Gg3$player1, player2vector)
WCE21_Gg3$p2inp1vec <- is.element(WCE21_Gg3$player2, player1vector)

addPlayer1 <- WCE21_Gg3[ which(WCE21_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_Gg3[ which(WCE21_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_Gg2 <- rbind(WCE21_Gg2, addPlayers)

#ROUND 21, Goal graph using weighted edges
WCE21_Gft <- ftable(WCE21_Gg2$player1, WCE21_Gg2$player2)
WCE21_Gft2 <- as.matrix(WCE21_Gft)
numRows <- nrow(WCE21_Gft2)
numCols <- ncol(WCE21_Gft2)
WCE21_Gft3 <- WCE21_Gft2[c(2:numRows) , c(2:numCols)]
WCE21_GTable <- graph.adjacency(WCE21_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Goal graph=weighted
plot.igraph(WCE21_GTable, vertex.label = V(WCE21_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Goal calulation of network metrics
#igraph
WCE21_G.clusterCoef <- transitivity(WCE21_GTable, type="global") #cluster coefficient
WCE21_G.degreeCent <- centralization.degree(WCE21_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_Gftn <- as.network.matrix(WCE21_Gft)
WCE21_G.netDensity <- network.density(WCE21_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_G.entropy <- entropy(WCE21_Gft) #entropy

WCE21_G.netMx <- cbind(WCE21_G.netMx, WCE21_G.clusterCoef, WCE21_G.degreeCent$centralization,
                       WCE21_G.netDensity, WCE21_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_G.netMx) <- varnames

#ROUND 21, Behind***************************************************************

round = 21
teamName = "WCE"
KIoutcome = "Behind_F"
WCE21_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, Behind with weighted edges
WCE21_Bg2 <- data.frame(WCE21_B)
WCE21_Bg2 <- WCE21_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_Bg2$player1
player2vector <- WCE21_Bg2$player2
WCE21_Bg3 <- WCE21_Bg2
WCE21_Bg3$p1inp2vec <- is.element(WCE21_Bg3$player1, player2vector)
WCE21_Bg3$p2inp1vec <- is.element(WCE21_Bg3$player2, player1vector)

addPlayer1 <- WCE21_Bg3[ which(WCE21_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE21_Bg3[ which(WCE21_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_Bg2 <- rbind(WCE21_Bg2, addPlayers)

#ROUND 21, Behind graph using weighted edges
WCE21_Bft <- ftable(WCE21_Bg2$player1, WCE21_Bg2$player2)
WCE21_Bft2 <- as.matrix(WCE21_Bft)
numRows <- nrow(WCE21_Bft2)
numCols <- ncol(WCE21_Bft2)
WCE21_Bft3 <- WCE21_Bft2[c(2:numRows) , c(2:numCols)]
WCE21_BTable <- graph.adjacency(WCE21_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 21, Behind graph=weighted
plot.igraph(WCE21_BTable, vertex.label = V(WCE21_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, Behind calulation of network metrics
#igraph
WCE21_B.clusterCoef <- transitivity(WCE21_BTable, type="global") #cluster coefficient
WCE21_B.degreeCent <- centralization.degree(WCE21_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_Bftn <- as.network.matrix(WCE21_Bft)
WCE21_B.netDensity <- network.density(WCE21_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_B.entropy <- entropy(WCE21_Bft) #entropy

WCE21_B.netMx <- cbind(WCE21_B.netMx, WCE21_B.clusterCoef, WCE21_B.degreeCent$centralization,
                       WCE21_B.netDensity, WCE21_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_B.netMx) <- varnames

#ROUND 21, FWD Stoppage**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE21_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Stoppage with weighted edges
WCE21_SFg2 <- data.frame(WCE21_SF)
WCE21_SFg2 <- WCE21_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_SFg2$player1
player2vector <- WCE21_SFg2$player2
WCE21_SFg3 <- WCE21_SFg2
WCE21_SFg3$p1inp2vec <- is.element(WCE21_SFg3$player1, player2vector)
WCE21_SFg3$p2inp1vec <- is.element(WCE21_SFg3$player2, player1vector)

addPlayer1 <- WCE21_SFg3[ which(WCE21_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE21_SFg2 <- rbind(WCE21_SFg2, addPlayer1)

#ROUND 21, FWD Stoppage graph using weighted edges
WCE21_SFft <- ftable(WCE21_SFg2$player1, WCE21_SFg2$player2)
WCE21_SFft2 <- as.matrix(WCE21_SFft)
numRows <- nrow(WCE21_SFft2)
numCols <- ncol(WCE21_SFft2)
WCE21_SFft3 <- WCE21_SFft2[c(2:numRows) , c(1:numCols)]
WCE21_SFTable <- graph.adjacency(WCE21_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Stoppage graph=weighted
plot.igraph(WCE21_SFTable, vertex.label = V(WCE21_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Stoppage calulation of network metrics
#igraph
WCE21_SF.clusterCoef <- transitivity(WCE21_SFTable, type="global") #cluster coefficient
WCE21_SF.degreeCent <- centralization.degree(WCE21_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_SFftn <- as.network.matrix(WCE21_SFft)
WCE21_SF.netDensity <- network.density(WCE21_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_SF.entropy <- entropy(WCE21_SFft) #entropy

WCE21_SF.netMx <- cbind(WCE21_SF.netMx, WCE21_SF.clusterCoef, WCE21_SF.degreeCent$centralization,
                        WCE21_SF.netDensity, WCE21_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_SF.netMx) <- varnames

#ROUND 21, FWD Turnover**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE21_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, FWD Turnover with weighted edges
WCE21_TFg2 <- data.frame(WCE21_TF)
WCE21_TFg2 <- WCE21_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_TFg2$player1
player2vector <- WCE21_TFg2$player2
WCE21_TFg3 <- WCE21_TFg2
WCE21_TFg3$p1inp2vec <- is.element(WCE21_TFg3$player1, player2vector)
WCE21_TFg3$p2inp1vec <- is.element(WCE21_TFg3$player2, player1vector)

addPlayer1 <- WCE21_TFg3[ which(WCE21_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_TFg3[ which(WCE21_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_TFg2 <- rbind(WCE21_TFg2, addPlayers)

#ROUND 21, FWD Turnover graph using weighted edges
WCE21_TFft <- ftable(WCE21_TFg2$player1, WCE21_TFg2$player2)
WCE21_TFft2 <- as.matrix(WCE21_TFft)
numRows <- nrow(WCE21_TFft2)
numCols <- ncol(WCE21_TFft2)
WCE21_TFft3 <- WCE21_TFft2[c(2:numRows) , c(2:numCols)]
WCE21_TFTable <- graph.adjacency(WCE21_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, FWD Turnover graph=weighted
plot.igraph(WCE21_TFTable, vertex.label = V(WCE21_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, FWD Turnover calulation of network metrics
#igraph
WCE21_TF.clusterCoef <- transitivity(WCE21_TFTable, type="global") #cluster coefficient
WCE21_TF.degreeCent <- centralization.degree(WCE21_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_TFftn <- as.network.matrix(WCE21_TFft)
WCE21_TF.netDensity <- network.density(WCE21_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_TF.entropy <- entropy(WCE21_TFft) #entropy

WCE21_TF.netMx <- cbind(WCE21_TF.netMx, WCE21_TF.clusterCoef, WCE21_TF.degreeCent$centralization,
                        WCE21_TF.netDensity, WCE21_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_TF.netMx) <- varnames

#ROUND 21, AM Stoppage**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE21_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Stoppage with weighted edges
WCE21_SAMg2 <- data.frame(WCE21_SAM)
WCE21_SAMg2 <- WCE21_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_SAMg2$player1
player2vector <- WCE21_SAMg2$player2
WCE21_SAMg3 <- WCE21_SAMg2
WCE21_SAMg3$p1inp2vec <- is.element(WCE21_SAMg3$player1, player2vector)
WCE21_SAMg3$p2inp1vec <- is.element(WCE21_SAMg3$player2, player1vector)

addPlayer1 <- WCE21_SAMg3[ which(WCE21_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_SAMg3[ which(WCE21_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_SAMg2 <- rbind(WCE21_SAMg2, addPlayers)

#ROUND 21, AM Stoppage graph using weighted edges
WCE21_SAMft <- ftable(WCE21_SAMg2$player1, WCE21_SAMg2$player2)
WCE21_SAMft2 <- as.matrix(WCE21_SAMft)
numRows <- nrow(WCE21_SAMft2)
numCols <- ncol(WCE21_SAMft2)
WCE21_SAMft3 <- WCE21_SAMft2[c(2:numRows) , c(2:numCols)]
WCE21_SAMTable <- graph.adjacency(WCE21_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Stoppage graph=weighted
plot.igraph(WCE21_SAMTable, vertex.label = V(WCE21_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Stoppage calulation of network metrics
#igraph
WCE21_SAM.clusterCoef <- transitivity(WCE21_SAMTable, type="global") #cluster coefficient
WCE21_SAM.degreeCent <- centralization.degree(WCE21_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_SAMftn <- as.network.matrix(WCE21_SAMft)
WCE21_SAM.netDensity <- network.density(WCE21_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_SAM.entropy <- entropy(WCE21_SAMft) #entropy

WCE21_SAM.netMx <- cbind(WCE21_SAM.netMx, WCE21_SAM.clusterCoef, WCE21_SAM.degreeCent$centralization,
                         WCE21_SAM.netDensity, WCE21_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_SAM.netMx) <- varnames

#ROUND 21, AM Turnover**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE21_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, AM Turnover with weighted edges
WCE21_TAMg2 <- data.frame(WCE21_TAM)
WCE21_TAMg2 <- WCE21_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_TAMg2$player1
player2vector <- WCE21_TAMg2$player2
WCE21_TAMg3 <- WCE21_TAMg2
WCE21_TAMg3$p1inp2vec <- is.element(WCE21_TAMg3$player1, player2vector)
WCE21_TAMg3$p2inp1vec <- is.element(WCE21_TAMg3$player2, player1vector)

addPlayer1 <- WCE21_TAMg3[ which(WCE21_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_TAMg3[ which(WCE21_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_TAMg2 <- rbind(WCE21_TAMg2, addPlayers)

#ROUND 21, AM Turnover graph using weighted edges
WCE21_TAMft <- ftable(WCE21_TAMg2$player1, WCE21_TAMg2$player2)
WCE21_TAMft2 <- as.matrix(WCE21_TAMft)
numRows <- nrow(WCE21_TAMft2)
numCols <- ncol(WCE21_TAMft2)
WCE21_TAMft3 <- WCE21_TAMft2[c(2:numRows) , c(2:numCols)]
WCE21_TAMTable <- graph.adjacency(WCE21_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, AM Turnover graph=weighted
plot.igraph(WCE21_TAMTable, vertex.label = V(WCE21_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, AM Turnover calulation of network metrics
#igraph
WCE21_TAM.clusterCoef <- transitivity(WCE21_TAMTable, type="global") #cluster coefficient
WCE21_TAM.degreeCent <- centralization.degree(WCE21_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_TAMftn <- as.network.matrix(WCE21_TAMft)
WCE21_TAM.netDensity <- network.density(WCE21_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_TAM.entropy <- entropy(WCE21_TAMft) #entropy

WCE21_TAM.netMx <- cbind(WCE21_TAM.netMx, WCE21_TAM.clusterCoef, WCE21_TAM.degreeCent$centralization,
                         WCE21_TAM.netDensity, WCE21_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_TAM.netMx) <- varnames

#ROUND 21, DM Stoppage**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE21_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Stoppage with weighted edges
WCE21_SDMg2 <- data.frame(WCE21_SDM)
WCE21_SDMg2 <- WCE21_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_SDMg2$player1
player2vector <- WCE21_SDMg2$player2
WCE21_SDMg3 <- WCE21_SDMg2
WCE21_SDMg3$p1inp2vec <- is.element(WCE21_SDMg3$player1, player2vector)
WCE21_SDMg3$p2inp1vec <- is.element(WCE21_SDMg3$player2, player1vector)

addPlayer1 <- WCE21_SDMg3[ which(WCE21_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_SDMg3[ which(WCE21_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_SDMg2 <- rbind(WCE21_SDMg2, addPlayers)

#ROUND 21, DM Stoppage graph using weighted edges
WCE21_SDMft <- ftable(WCE21_SDMg2$player1, WCE21_SDMg2$player2)
WCE21_SDMft2 <- as.matrix(WCE21_SDMft)
numRows <- nrow(WCE21_SDMft2)
numCols <- ncol(WCE21_SDMft2)
WCE21_SDMft3 <- WCE21_SDMft2[c(2:numRows) , c(2:numCols)]
WCE21_SDMTable <- graph.adjacency(WCE21_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Stoppage graph=weighted
plot.igraph(WCE21_SDMTable, vertex.label = V(WCE21_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Stoppage calulation of network metrics
#igraph
WCE21_SDM.clusterCoef <- transitivity(WCE21_SDMTable, type="global") #cluster coefficient
WCE21_SDM.degreeCent <- centralization.degree(WCE21_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_SDMftn <- as.network.matrix(WCE21_SDMft)
WCE21_SDM.netDensity <- network.density(WCE21_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_SDM.entropy <- entropy(WCE21_SDMft) #entropy

WCE21_SDM.netMx <- cbind(WCE21_SDM.netMx, WCE21_SDM.clusterCoef, WCE21_SDM.degreeCent$centralization,
                         WCE21_SDM.netDensity, WCE21_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_SDM.netMx) <- varnames

#ROUND 21, DM Turnover**********************************************************

round = 21
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE21_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, DM Turnover with weighted edges
WCE21_TDMg2 <- data.frame(WCE21_TDM)
WCE21_TDMg2 <- WCE21_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_TDMg2$player1
player2vector <- WCE21_TDMg2$player2
WCE21_TDMg3 <- WCE21_TDMg2
WCE21_TDMg3$p1inp2vec <- is.element(WCE21_TDMg3$player1, player2vector)
WCE21_TDMg3$p2inp1vec <- is.element(WCE21_TDMg3$player2, player1vector)

addPlayer1 <- WCE21_TDMg3[ which(WCE21_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_TDMg3[ which(WCE21_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_TDMg2 <- rbind(WCE21_TDMg2, addPlayers)

#ROUND 21, DM Turnover graph using weighted edges
WCE21_TDMft <- ftable(WCE21_TDMg2$player1, WCE21_TDMg2$player2)
WCE21_TDMft2 <- as.matrix(WCE21_TDMft)
numRows <- nrow(WCE21_TDMft2)
numCols <- ncol(WCE21_TDMft2)
WCE21_TDMft3 <- WCE21_TDMft2[c(2:numRows) , c(2:numCols)]
WCE21_TDMTable <- graph.adjacency(WCE21_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 21, DM Turnover graph=weighted
plot.igraph(WCE21_TDMTable, vertex.label = V(WCE21_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, DM Turnover calulation of network metrics
#igraph
WCE21_TDM.clusterCoef <- transitivity(WCE21_TDMTable, type="global") #cluster coefficient
WCE21_TDM.degreeCent <- centralization.degree(WCE21_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_TDMftn <- as.network.matrix(WCE21_TDMft)
WCE21_TDM.netDensity <- network.density(WCE21_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_TDM.entropy <- entropy(WCE21_TDMft) #entropy

WCE21_TDM.netMx <- cbind(WCE21_TDM.netMx, WCE21_TDM.clusterCoef, WCE21_TDM.degreeCent$centralization,
                         WCE21_TDM.netDensity, WCE21_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_TDM.netMx) <- varnames

#ROUND 21, D Stoppage**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE21_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Stoppage with weighted edges
WCE21_SDg2 <- data.frame(WCE21_SD)
WCE21_SDg2 <- WCE21_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_SDg2$player1
player2vector <- WCE21_SDg2$player2
WCE21_SDg3 <- WCE21_SDg2
WCE21_SDg3$p1inp2vec <- is.element(WCE21_SDg3$player1, player2vector)
WCE21_SDg3$p2inp1vec <- is.element(WCE21_SDg3$player2, player1vector)

addPlayer1 <- WCE21_SDg3[ which(WCE21_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_SDg3[ which(WCE21_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_SDg2 <- rbind(WCE21_SDg2, addPlayers)

#ROUND 21, D Stoppage graph using weighted edges
WCE21_SDft <- ftable(WCE21_SDg2$player1, WCE21_SDg2$player2)
WCE21_SDft2 <- as.matrix(WCE21_SDft)
numRows <- nrow(WCE21_SDft2)
numCols <- ncol(WCE21_SDft2)
WCE21_SDft3 <- WCE21_SDft2[c(2:numRows) , c(2:numCols)]
WCE21_SDTable <- graph.adjacency(WCE21_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Stoppage graph=weighted
plot.igraph(WCE21_SDTable, vertex.label = V(WCE21_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Stoppage calulation of network metrics
#igraph
WCE21_SD.clusterCoef <- transitivity(WCE21_SDTable, type="global") #cluster coefficient
WCE21_SD.degreeCent <- centralization.degree(WCE21_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_SDftn <- as.network.matrix(WCE21_SDft)
WCE21_SD.netDensity <- network.density(WCE21_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_SD.entropy <- entropy(WCE21_SDft) #entropy

WCE21_SD.netMx <- cbind(WCE21_SD.netMx, WCE21_SD.clusterCoef, WCE21_SD.degreeCent$centralization,
                        WCE21_SD.netDensity, WCE21_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_SD.netMx) <- varnames

#ROUND 21, D Turnover**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE21_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, D Turnover with weighted edges
WCE21_TDg2 <- data.frame(WCE21_TD)
WCE21_TDg2 <- WCE21_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_TDg2$player1
player2vector <- WCE21_TDg2$player2
WCE21_TDg3 <- WCE21_TDg2
WCE21_TDg3$p1inp2vec <- is.element(WCE21_TDg3$player1, player2vector)
WCE21_TDg3$p2inp1vec <- is.element(WCE21_TDg3$player2, player1vector)

addPlayer1 <- WCE21_TDg3[ which(WCE21_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_TDg3[ which(WCE21_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_TDg2 <- rbind(WCE21_TDg2, addPlayers)

#ROUND 21, D Turnover graph using weighted edges
WCE21_TDft <- ftable(WCE21_TDg2$player1, WCE21_TDg2$player2)
WCE21_TDft2 <- as.matrix(WCE21_TDft)
numRows <- nrow(WCE21_TDft2)
numCols <- ncol(WCE21_TDft2)
WCE21_TDft3 <- WCE21_TDft2[c(2:numRows) , c(2:numCols)]
WCE21_TDTable <- graph.adjacency(WCE21_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, D Turnover graph=weighted
plot.igraph(WCE21_TDTable, vertex.label = V(WCE21_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, D Turnover calulation of network metrics
#igraph
WCE21_TD.clusterCoef <- transitivity(WCE21_TDTable, type="global") #cluster coefficient
WCE21_TD.degreeCent <- centralization.degree(WCE21_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_TDftn <- as.network.matrix(WCE21_TDft)
WCE21_TD.netDensity <- network.density(WCE21_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_TD.entropy <- entropy(WCE21_TDft) #entropy

WCE21_TD.netMx <- cbind(WCE21_TD.netMx, WCE21_TD.clusterCoef, WCE21_TD.degreeCent$centralization,
                        WCE21_TD.netDensity, WCE21_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_TD.netMx) <- varnames

#ROUND 21, End of Qtr**********************************************************
#NA

round = 21
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE21_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 21, End of Qtr with weighted edges
WCE21_QTg2 <- data.frame(WCE21_QT)
WCE21_QTg2 <- WCE21_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE21_QTg2$player1
player2vector <- WCE21_QTg2$player2
WCE21_QTg3 <- WCE21_QTg2
WCE21_QTg3$p1inp2vec <- is.element(WCE21_QTg3$player1, player2vector)
WCE21_QTg3$p2inp1vec <- is.element(WCE21_QTg3$player2, player1vector)

addPlayer1 <- WCE21_QTg3[ which(WCE21_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE21_QTg3[ which(WCE21_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE21_QTg2 <- rbind(WCE21_QTg2, addPlayers)

#ROUND 21, End of Qtr graph using weighted edges
WCE21_QTft <- ftable(WCE21_QTg2$player1, WCE21_QTg2$player2)
WCE21_QTft2 <- as.matrix(WCE21_QTft)
numRows <- nrow(WCE21_QTft2)
numCols <- ncol(WCE21_QTft2)
WCE21_QTft3 <- WCE21_QTft2[c(2:numRows) , c(2:numCols)]
WCE21_QTTable <- graph.adjacency(WCE21_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 21, End of Qtr graph=weighted
plot.igraph(WCE21_QTTable, vertex.label = V(WCE21_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE21_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 21, End of Qtr calulation of network metrics
#igraph
WCE21_QT.clusterCoef <- transitivity(WCE21_QTTable, type="global") #cluster coefficient
WCE21_QT.degreeCent <- centralization.degree(WCE21_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE21_QTftn <- as.network.matrix(WCE21_QTft)
WCE21_QT.netDensity <- network.density(WCE21_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE21_QT.entropy <- entropy(WCE21_QTft) #entropy

WCE21_QT.netMx <- cbind(WCE21_QT.netMx, WCE21_QT.clusterCoef, WCE21_QT.degreeCent$centralization,
                        WCE21_QT.netDensity, WCE21_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE21_QT.netMx) <- varnames
